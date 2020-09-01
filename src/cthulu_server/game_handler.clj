(ns cthulu-server.game-handler
  (:require [cthulu-server.core :refer [create-game
                                        reveal-card
                                        get-card]]))

(defonce state-atom (atom {:game-state nil
                           :action-log []
                           :clients    {}}))
(defn reset-state!
 []
 (reset! state-atom {:game-state nil
                     :action-log []
                     :clients    {}}))

(defn clear-game!
  []
  (swap! state-atom (fn [prev-state] {:game-state nil
                                      :action-log []
                                      :clients    (:clients prev-state)})))

(defn create-game!
  []
  (let [seed (rand-int Integer/MAX_VALUE)]
    (swap! state-atom
           (fn [prev-state]
             (let [players (vals (:clients prev-state))]
               (-> prev-state
                   (assoc :game-state (create-game players [:insanitys-grasp :paranoia :evil-presence] seed))
                   (assoc :action-log [])))))))

(defn player-join!
  [channel name]
  (let [new-state (swap! state-atom
                         (fn [prev-state]
                           (let [taken-ids (->> (:clients prev-state)
                                                (vals)
                                                (map :id))
                                 id-of-new-player (->> (range)
                                                       (drop-while #(some #{%} taken-ids))
                                                       (first))]
                             (assoc-in prev-state [:clients channel] {:id id-of-new-player :name name}))))]
    new-state))

(defn player-leave!
  [channel]
  (swap! state-atom update :clients #(dissoc % channel)))

(defn reveal-card!
  [player-id target-player-id card-id]
  (swap! state-atom
        (fn [prev-state]
          (let [card (get-card (:game-state prev-state) target-player-id card-id)]
            (-> prev-state
                (update :action-log conj
                        {:acting-player-id     player-id
                         :target-player-id     target-player-id
                         :revealed-card-id     (:id card)
                         :revealed-card-entity (:entity card)
                         :round                (:round (:game-state prev-state))})
                (update :game-state reveal-card player-id target-player-id card-id))))))