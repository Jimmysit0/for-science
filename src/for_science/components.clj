;
; Copyright © 2021 Peter Monks
;
; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; SPDX-License-Identifier: MPL-2.0
;

(ns for-science.components
  "Shared state: configuration (from environment variables) and the Discord REST connection."
  (:require [discljord.messaging :as msg]))

(defn- required-env
  [name]
  (or (not-empty (System/getenv name))
      (throw (ex-info (str "Missing required environment variable: " name) {:variable name}))))

(defn token          [] (required-env "DISCORD_API_TOKEN"))
(def application-id (System/getenv "APPLICATION_ID"))
(defonce connection (delay (msg/start-connection! (token))))
