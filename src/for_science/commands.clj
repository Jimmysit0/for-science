;
; Copyright © 2021 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns for-science.commands
  (:require [clojure.core                 :as c]
            [clojure.string               :as s]
            [clojure.math                 :as m]   ; Ignore clj-kondo warning: this is required for sci initialisation
            [clojure.tools.logging        :as log]
            [sci.core                     :as sci]
            [sci.impl.utils               :as sciiu]
            [embroidery.api               :as e]
            [rencg.api                    :as re]
            [discljord.messaging          :as msg]
            [slash.command                :as cmd]
            [slash.command.structure      :as stc]
            [slash.component.structure    :as cmps]
            [for-science.components       :as cmp]))

(def ^:private default-timeout-in-sec 2)
(def ^:private maximum-output-length  512)

(def ^:private clojure-code-fence-regex #"(?is)```(?:(?:clojure|clj)\s+)?(?<source>.*?)```")

; See https://github.com/babashka/sci/issues/952
; From https://github.com/babashka/sci.configs/blob/main/src/sci/configs/clojure_1_11.cljc
(def ^:private clojure-core-namespace-extras-1-11
  {'abs           (sci/copy-var c/abs           sciiu/clojure-core-ns)
   'NaN?          (sci/copy-var c/NaN?          sciiu/clojure-core-ns)
   'infinite?     (sci/copy-var c/infinite?     sciiu/clojure-core-ns)
   'parse-double  (sci/copy-var c/parse-double  sciiu/clojure-core-ns)
   'parse-long    (sci/copy-var c/parse-long    sciiu/clojure-core-ns)
   'parse-boolean (sci/copy-var c/parse-boolean sciiu/clojure-core-ns)
   'parse-uuid    (sci/copy-var c/parse-uuid    sciiu/clojure-core-ns)
   'random-uuid   (sci/copy-var c/random-uuid   sciiu/clojure-core-ns)
   'update-keys   (sci/copy-var c/update-keys   sciiu/clojure-core-ns)
   'update-vals   (sci/copy-var c/update-vals   sciiu/clojure-core-ns)
   'iteration     (sci/copy-var c/iteration     sciiu/clojure-core-ns)})

(def ^:private math       (sci/create-ns 'clojure.math))
(def ^:private math-ns    (sci/copy-ns clojure.math math))
(def ^:private namespaces {'clojure.core clojure-core-namespace-extras-1-11 'clojure.math math-ns})

(def ^:private code-prefix "(use 'clojure.repl)")                ; Prefix to all code evaluation
(def ^:private sci-ctx     (sci/init {:namespaces namespaces}))  ; sci context

(defn- truncate-string
  "Truncates a string to our maximum allowed output length, appending"
  [^String s]
  (when s
    (if (>= (count s) maximum-output-length)
      (str (subs s 0 maximum-output-length) "...and more")
      s)))

(defn- eval-clj
  "Evaluates the given Clojure code, with a timeout on execution (default is 2 seconds). Result is a map which may contain these keys:

  :output any output sent to stdout or stderr
  :result the last result returned by the evaluated code
  :error  an error (either a string or a Throwable), if an error occurred"
  ([code] (eval-clj code default-timeout-in-sec))
  ([code timeout-in-sec]
   (when code
     (log/debug "Evaluating Clojure forms:" code)
     (let [result (try
                    (let [f           (e/future*
                                       (try
                                         (let [sw     (java.io.StringWriter.)
                                               result (sci/binding [sci/out sw
                                                                    sci/err sw]
                                                        (truncate-string (pr-str (sci/eval-string* sci-ctx (str code-prefix "\n" code)))))]    ; Make sure we stringify the result inside sci/binding, to force de-lazying of the result of evaluating code
                                           (merge {:result result}
                                                  (when-let [output (when-not (s/blank? (str sw)) (truncate-string (str sw)))] {:output output})))
                                         (catch Throwable t
                                           {:error t})))
                          eval-result (deref f
                                             (* 1000 timeout-in-sec)
                                             {:error (str "Execution terminated after " timeout-in-sec "s.")})]
                      (when-not (future-done? f) (future-cancel f))
                      eval-result)
                    (catch Throwable t
                      {:error t}))]
       (log/debug "Result:" result)
       result))))

(def ^:private embed-color 0x4e87e6)
(def ^:private error-color 0xFF0000)
(def ^:private ephemeral   64)   

(def ^:private logo-url "https://thumb.wikimedia.org/wikipedia/commons/thumb/5/5d/Clojure_logo.svg/250px-Clojure_logo.svg.png")

(defn- embed-template
  "Base for every embed we send: blue bar, footer with the bot's name and logo, and a timestamp."
  []
  {:color     embed-color
   :footer    {:text "for-science" :icon_url logo-url}
   :timestamp (str (java.time.Instant/now))})

(def commands 
  [(stc/command
    "clj"
    "Evaluates Clojure code"
    :options
    [(stc/option "code" "The Clojure code to evaluate" :string :required true)])
   (stc/command
    "move"
    "Moves a conversation to the specified channel"
    :options
    [(stc/option "channel" "The channel to continue the conversation in" :channel
                 :channel-types [:guild-text]
                 :required true)])])

(defn- extract 
  [text]
  (s/trim
   (if-let [clojure-snippets (re/re-seq clojure-code-fence-regex text)]
     (s/join "\n" (filter #(not (s/blank? %)) (map #(get % "source") clojure-snippets)))
     text)))

(defn- format-eval-result
  [eval-result]
  (if (:error eval-result)
    (str "```\n⚠️ " (:error eval-result) "\n```")
    (str (when (:output eval-result) (str "Output:\n```\n" (:output eval-result) "\n```\n"))
         "Result:\n```clojure\n" (:result eval-result) "\n```")))

(cmd/defhandler clj-command
  ["clj"]
  {:keys [id token]}
  [code]
  (let [conn     @cmp/connection
        ; evaluation can take up to a couple of seconds, but discord requires an answer within 3s, so we send a
        ; "thinking..." response and then edit it with the actual result
        ; TODO: check if whats above is actually needed xd
        response (msg/create-interaction-response! conn id token 5)]
    (future
      (try
        (let [eval-result (eval-clj (extract code))]
          @(msg/edit-original-interaction-response!
            conn
            cmp/application-id
            token
            :embeds [(assoc (embed-template) :description (format-eval-result eval-result))]))
        (catch Throwable t
          (log/error t "Failed to evaluate or respond to /clj"))))
    response))

(defn- message-url
  [guild-id channel-id message-id]
  (str "https://discord.com/channels/" guild-id "/" channel-id "/" message-id))

(defn- channel-link
  [channel-id]
  (str "<#" channel-id ">"))

(defn- reply-privately!
  [conn id token text]
  (msg/create-interaction-response!
   conn id token 4
   :data {:embeds [(assoc (embed-template) :description text :color error-color)]
          :flags  ephemeral}))

(cmd/defhandler move-command
  ["move"]
  {:keys [id token channel-id guild-id]}
  [channel]
  (let [conn @cmp/connection]
    (cond
      (nil? guild-id)
      (reply-privately! conn id token "This command can only be used in a server channel.")

      (= channel channel-id)
      (reply-privately! conn id token "Can't move the conversation to the same channel.")

      :else
      (let [target-message-id  (:id @(msg/create-message!
                                      conn channel
                                      :embed (assoc (embed-template)
                                                    :description (str "Continuing the conversation from " (channel-link channel-id) "..."))))
            target-message-url (message-url guild-id channel target-message-id)
            response           (msg/create-interaction-response!
                                conn id token 4
                                :data {:embeds     [(assoc (embed-template)
                                                           :description (str "Let's continue this conversation in " (channel-link channel) " ([link](" target-message-url "))"))]
                                       :components [(cmps/action-row
                                                     (cmps/link-button target-message-url :label "Link"))]}) 
            source-message-id  (:id @(msg/get-original-interaction-response! conn cmp/application-id token))
            source-message-url (message-url guild-id channel-id source-message-id)]
        @(msg/edit-message!
          conn channel target-message-id
          :embed (assoc (embed-template)
                        :description (str "Continuing the conversation from " (channel-link channel-id) " ([link](" source-message-url "))..."))
          :components [(cmps/action-row
                        (cmps/link-button source-message-url :label "Link"))])
        response))))

(cmd/defpaths command-paths
  clj-command
  move-command)