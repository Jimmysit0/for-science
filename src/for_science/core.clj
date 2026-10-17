;
; Copyright © 2021 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns for-science.core
  "Entry point: connects to Discord, registers the slash commands and routes interactions to their handlers."
  (:require [clojure.core.async     :as a]
            [clojure.tools.logging  :as log]
            [discljord.connections  :as con]
            [discljord.events       :as dsc-evts]
            [discljord.messaging    :as msg]
            [slash.core             :as slash]
            [slash.gateway          :as gtw]
            [for-science.commands   :as cmds]
            [for-science.components :as cmp])
  (:gen-class))

(defmulti event-handler (fn [type _data] type))

(defmethod event-handler :default [_ _])

(defmethod event-handler :ready
  [_ data]
  (log/info "connected as user" (get-in data [:user :id]))
  (let [result @(msg/bulk-overwrite-global-application-commands! @cmp/connection cmp/application-id cmds/commands)]
    (log/info "registered" (count cmds/commands) "slash commands:" (pr-str (map :name result)))))

(defmethod event-handler :interaction-create
  [_ data]
  (slash/route-interaction
    (assoc gtw/gateway-defaults :application-command cmds/command-paths)
    data))

(defn -main
  [& _args]
  (when-not cmp/application-id
    (throw (ex-info "Missing required environment variable: APPLICATION_ID" {})))
  (let [event-channel (a/chan (a/buffer 100)) 
        bot-channel   (con/connect-bot! (cmp/token) event-channel :intents #{})]
    (try
      (dsc-evts/message-pump! event-channel event-handler)  
      (finally
        (msg/stop-connection! @cmp/connection)
        (con/disconnect-bot! bot-channel)
        (a/close! event-channel)))))
