(ns cthulu-server.request-handler
  (:require ;[clojure.data.json :refer [write-str read-json]]
            [cthulu-server.core :refer [game-ended?]]
            [cthulu-server.game-handler :refer [create-game!
                                                player-join!
                                                reveal-card!
                                                clear-game!]]))

(defn- adapt-game-state-for-player
  "Strips off game data that the player should not know about."
  [game-state player-id]
  (if (game-ended? game-state)
    (assoc game-state :player-id-in-turn -1) ; if set to nil client will do wrong
    (-> game-state
        (dissoc :reshuffle-pile)
        (update :players (fn [players]
                           (map (fn [player]
                                  (if (= player-id (:id player))
                                    player
                                    (let [player (update player :cards (fn [cards] (map #(assoc % :entity :unknown) cards)))]
                                      (if (= player-id (:reveal-role-to-player player))
                                        player
                                        (dissoc player :role)))))
                                players))))))

(defn prepare-game-state-for-all-channels
  [state]
  (map (fn [[channel {player-id :id}]]
         [channel (-> state
                      (dissoc :clients)
                      (update :game-state adapt-game-state-for-player player-id)
                      (assoc :client-id player-id))])
       (:clients state)))

(defn handle-received-data
  [channel data]
  (condp = (:type data)
    "set-name"
    (let [new-state  (player-join! channel (:content data))
          client-map (:clients new-state)
          joined-active-game? (< (get-in client-map [channel :id])
                                 (count (get-in new-state [:game-state :players])))]
      (if joined-active-game?
        (prepare-game-state-for-all-channels new-state)
        (->> (keys client-map)
             (map (fn [ch] [ch {:players   (vals client-map)
                                :client-id (get-in client-map [ch :id])}])))))

    "start-game"
    (prepare-game-state-for-all-channels (create-game!))
    
    "reveal-card"
    (let [content (:content data)
          new-state (reveal-card! (:player-id content) (:target-player-id content) (:card-id content))]
      (prepare-game-state-for-all-channels new-state))
    
    "clear-game"
    (let [{clients :clients} (clear-game!)]
      ;(println clients)
      (map (fn [[ch {id :id}]] [ch {:players   (vals clients)
                                    :client-id id}])
           clients))))

(defn handler
  [channel received-message-str]
  (->> (read-string received-message-str)
       (handle-received-data channel)))
