(ns cthulu-server.core
  (:require [clojure.test :refer [is]]
            [clojure.set :refer [difference]]
            [ysera.random :refer [shuffle-with-seed
                                  get-random-int]]))

(def valid-objects-of-power #{:insanitys-grasp :paranoia :evil-presence :private-eye})

(defn- potential-number-of-investigators
  [number-of-players]
  (condp >= number-of-players
    4 3
    6 4
    7 5
    9 6
    7))

(defn- potential-number-of-cultists
  [number-of-players]
  (condp >= number-of-players
    6 2
    9 3
    4))

(defn- identity-list
  [number-of-players]
  (concat (repeat (potential-number-of-investigators number-of-players) :investigator)
          (repeat (potential-number-of-cultists number-of-players) :cultist)))


(defn- shuffle-cards-and-card-ids
  [seed cards]
  {:test (fn []
           (is (= (second (shuffle-cards-and-card-ids 0 [{:entity :futile :id 2}
                                                         {:entity :elder-sign :id 0}
                                                         {:entity :cthulu :id 5}]))
                  [{:entity :futile :id 5}
                   {:entity :cthulu :id 2}
                   {:entity :elder-sign :id 0}])))}
  (let [[seed shuffled-cards] (shuffle-with-seed seed cards)
        [seed shuffled-ids] (shuffle-with-seed seed (map :id shuffled-cards))
        shuffled-cards-and-card-ids (map-indexed (fn [i card]
                                                   (assoc card :id (nth shuffled-ids i)))
                                                 shuffled-cards)]
    [seed shuffled-cards-and-card-ids]))

(defn generate-shuffled-deck
  {:test (fn []
           (is (= (->> (generate-shuffled-deck 0 4 [])
                       (second)
                       (filter #(= :futile (:entity %)))
                       (count))
                  15))
           (is (= (->> (generate-shuffled-deck 0 4 [:insanitys-grasp])
                       (second)
                       (filter #(= :insanitys-grasp (:entity %)))
                       (count))
                  1))
           (is (= (->> (generate-shuffled-deck 0 4 [:insanitys-grasp])
                       (second)
                       (count))
                  20)))}
  [seed number-of-players objects-of-power]
  (let [full-deck (concat
                   (repeat number-of-players {:entity :elder-sign})
                   (repeat (- (dec (* 4 number-of-players)) (count objects-of-power)) {:entity :futile})
                   (map #(-> {:entity %}) objects-of-power)
                   [{:entity :cthulu}])]
    (shuffle-cards-and-card-ids seed (map-indexed #(assoc %2 :id %1) full-deck))))

(defn create-game
  "Creates a game of Don't Mess With Cthulu.\nExpects a list of player maps: {:name \"Name\" :id id}"
  ([players]
   (create-game players [] 0))
  ([players objects-of-power]
   (create-game players objects-of-power 0))
  ([players objects-of-power seed]
   (when-not (empty? (difference (into #{} objects-of-power) valid-objects-of-power))
     (throw (AssertionError. "Invalid objects of power.")))
   (let [number-of-players (count players)
         [seed shuffled-deck] (generate-shuffled-deck seed number-of-players objects-of-power)
         [seed shuffled-identity-list] (shuffle-with-seed seed (identity-list number-of-players))
         [seed starting-player-id] (get-random-int seed number-of-players)
         hands (partition 5 shuffled-deck)]
     {:players           (sort-by
                          :id
                          (map-indexed (fn [i player]
                                         (-> player
                                             (assoc :role (nth shuffled-identity-list i))
                                             (assoc :cards (nth hands i))))
                                       players))
      :revealed-cards    []
      :reshuffle-pile    []
      :active-powers     []
      :player-id-in-turn starting-player-id
      :round             1
      :round-action      1
      :seed              seed})))

(defn- get-player
  [state player-id]
  (->> (:players state)
       (filter #(= player-id (:id %)))
       (first)))

(defn- update-player
  [state player-id update-fn]
  (update state :players (fn [players]
                           (map
                            #(if (= player-id (:id %)) (update-fn %) %)
                            players))))

(defn get-card
  [state player-id card-id]
  (first (filter
          #(= card-id (:id %))
          (:cards (get-player state player-id)))))

(defn- remove-card-from-player ; depends on ids starting from 0 and being in order
  [state player-id card-id]
  (-> state
      (update :players #(apply vector %))
      (update-in [:players player-id :cards]
                 (fn [cards]
                   (remove #(= card-id (:id %))
                           cards)))))

(defn game-ended?
  [{round :round
    revealed-cards :revealed-cards
    players :players}]
  (or (= round 5)
      (= (count players) (count (filter #(= :elder-sign (:entity %)) revealed-cards)))
      (boolean (seq (filter #(= :cthulu (:entity %)) revealed-cards)))))

(defn- maybe-redistribute-unrevealed-cards
  [state]
  (if (game-ended? state)
    state
    (let [unrevealed-cards (concat (:reshuffle-pile state) (mapcat :cards (:players state)))
          [seed shuffled-cards] (shuffle-with-seed (:seed state) unrevealed-cards)
          partitioned-cards (partition (- 6 (:round state)) (map-indexed #(assoc %2 :id %1) shuffled-cards))]
      (-> state
          (assoc :seed seed)
          (update :players #(map-indexed (fn [i player]
                                           (assoc player :cards (nth partitioned-cards i)))
                                         %))))))

(defn- update-round
  [state]
  (if (< (:round-action state)
         (count (:players state)))
    (update state :round-action inc)
    (-> state
        (update :round inc)
        (assoc :round-action 1)
        (assoc :active-powers [])
        (maybe-redistribute-unrevealed-cards))))

(defn- update-player-id-in-turn
  [state player-id]
  (if (some #(= :paranoia %) (:active-powers state))
    state
    (assoc state :player-id-in-turn player-id)))

(defn- activate-power
  [state investigator-id target-player-id target-card-entity]
  ; round is updated just before this function is called,
  ; so we need to make sure relevant powers don't activate when a new round starts
  (condp = target-card-entity
    :private-eye (update-player state target-player-id #(assoc % :reveal-role-to-player investigator-id))
    (if (= 1 (:round-action state))
      state
      (condp = target-card-entity
        :paranoia (update state :active-powers conj :paranoia)
        :evil-presence (-> state
                           (update :reshuffle-pile concat (:cards (get-player state target-player-id)))
                           (update-player target-player-id #(assoc % :cards [])))
        state))))

(defn reveal-card
  {:test (fn []
           (is (= (-> (create-game [{:name "Miguel" :id 0} {:id 1 :name "Lina"} {:id 2 :name "Louise"}])
                      (reveal-card 1 0 9)
                      (:revealed-cards))
                  [{:entity :futile :from-player-id 0}]))
           (is (= (-> (create-game [{:name "Miguel" :id 0} {:id 1 :name "Lina"} {:id 2 :name "Louise"}])
                      (reveal-card 1 0 9)
                      (:player-id-in-turn))
                  0))
           (is (= (-> (create-game [{:name "Miguel" :id   0} {:id   1 :name "Lina"} {:id   2 :name "Louise"}]
                                   [:insanitys-grasp :paranoia])
                      (reveal-card 1 2 2)
                      (reveal-card 2 1 1)
                      (reveal-card 2 1 4)
                      (:player-id-in-turn))
                  1))
           (is (= (-> (create-game [{:name "Miguel" :id   0} {:id   1 :name "Lina"} {:id   2 :name "Louise"}]
                                   [:insanitys-grasp :paranoia])
                      (reveal-card 1 0 9)
                      (reveal-card 0 1 1)
                      (reveal-card 1 2 2)
                      (reveal-card 2 0 0)
                      (:player-id-in-turn))
                  0))
           (is (= (-> (create-game [{:name "Miguel" :id   0} {:id   1 :name "Lina"}]
                                   [:evil-presence])
                      (reveal-card 1 0 1)
                      (get-player 0)
                      (:cards))
                  []))
           (is (= (as-> (create-game [{:name "Miguel" :id   0} {:id   1 :name "Lina"}]
                                     [:evil-presence]) $
                    (reveal-card $ 1 0 1)
                    (reveal-card $ 0 1 0)
                    (:players $)
                    (map #(count (:cards %)) $))
                  [4 4])))}
  [state player-id target-player-id card-id]
  (when-not (= player-id (:player-id-in-turn state))
    (throw (AssertionError. (str "Player with id " player-id " is not in turn!"))))
  (let [revealed-card (get-card state target-player-id card-id)]
    (-> state
        (remove-card-from-player target-player-id card-id)
        (update :revealed-cards conj (-> (dissoc revealed-card :id)
                                         (assoc :from-player-id target-player-id)))
        (update-round)
        (update-player-id-in-turn target-player-id)
        (activate-power player-id target-player-id (:entity revealed-card)))))

(comment
  (-> (create-game [{:name "Miguel" :id 0}
                    {:id 1 :name "Lina"}
                    {:id 2 :name "Louise"}]
                   [:insanitys-grasp :paranoia])
      (reveal-card 1 0 3)
      (reveal-card 0 1 13)
      (maybe-redistribute-unrevealed-cards))
  )
