(ns cthulu-server.main
  (:gen-class)
  (:require [org.httpkit.server :refer [run-server
                                        as-channel
                                        ;on-close
                                        ;on-receive
                                        send!]]
            [cthulu-server.request-handler :refer [handler]]
            [cthulu-server.game-handler :refer [player-leave!]]))

(defonce server-atom (atom nil))

(defn start! []
  (reset! server-atom
          (run-server (fn [request]
                        (as-channel request
                          {:on-open    (fn [channel] (println channel "connected"))
                           :on-close   (fn [channel status]
                                         (println "channel closed: " status)
                                         (player-leave! channel))
                           :on-receive (fn [channel message-str]
                                         (let [to-send (handler channel message-str)]
                                           (println "to-send:")
                                           (println (map second to-send))
                                           (run! (fn [[ch response]]
                                                   (send! ch (str response)))
                                                 to-send)))}))
                      {:port 8901})))

(defn stop! []
  (let [server-stop-fn (deref server-atom)]
    (server-stop-fn :timeout 100)
    (reset! server-atom nil)))

(defn -main
  []
  (start!))

(comment
  (start!)
  (stop!)
  )