(ns ygo.core
  (:require [ygo.cards :as cards]
            [clojure.string :as str]))

(defn subsi? [s sub]
  (let [re (re-pattern (str ".*" (str/lower-case sub) ".*"))]
    (re-matches re (str/lower-case s))))

(defn find-by-name [name-fragment]
  (filter #(subsi? (:Name %) name-fragment ) cards/cards))

(defn find-by-id [id]
  (first (filter #(= id (:Id %)) cards/cards)))

(defn fusion* 
  [{id1 :Id fusions :Fusions :as card1}
  {id2 :Id :as card2}]
  (->> fusions
    (filter (fn [{:keys [_card1 _card2]}]
              (and (= _card1 id1) (= _card2 id2))))
    (map (fn [{:keys [_result]} ] (find-by-id _result)))
    (first)))

(defn fusion [card1 card2]
  (or (fusion* card1 card2) (fusion* card2 card1)))  

(defn shallow-fusions [cards]
  (for [i (range (count cards))
        :let [a (first (drop i cards))]
        j (range (inc i) (count cards))
        :let [b (first (drop j cards))
              fusioned (fusion a b)]
        :when fusioned]
    [a b fusioned]))

(defn- remove-one [card coll]
  (let [[n m] (split-with #(= card %) coll )]
    (concat n (rest m))))

(defn deep-fusions [cards]
  (mapcat 
    (fn [[a b fusioned :as fusioned-ab]]
      (let [new-cards (as-> cards cards
                            (remove-one a cards) 
                            (remove-one b cards)
                            (conj cards fusioned))]
        (conj (deep-fusions new-cards) fusioned-ab)))
    (shallow-fusions cards)))

(defn all-fusions [hand board]
  (reduce
    (fn [fusions [_ _ hand-card :as hand-fusion]]
      (concat fusions 
              [hand-fusion]
              (for [board-card board
                    :let [board-fusion (fusion board-card hand-card)]
                    :when board-fusion]
                board-fusion)))
    []
    (deep-fusions hand)))

