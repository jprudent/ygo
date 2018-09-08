(ns ygo.core
  (:require [ygo.cards :as cards]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn subsi? [s sub]
  (let [re (re-pattern (str ".*" (str/lower-case sub) ".*"))]
    (re-matches re (str/lower-case s))))

(defn exact-match? [sub {:keys [Name]}] (= (str/lower-case Name) sub))

(defn find-by-name [name-fragment]
 (let [partial-match (filter #(subsi? (:Name %) name-fragment ) cards/cards)]
   (or (not-empty (filter (partial exact-match? name-fragment) partial-match))
       partial-match)))

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

(defn- remove-one [card coll]
  (let [[n m] (split-with #(not= card %) coll )]
    (concat n (rest m))))

(defn fusions-for [with-card cards]
  (println "fusion for" (:Name with-card) (map :Name (remove-one with-card cards)))
  (println "org" (map :Name cards))
  (for [card (remove-one with-card cards)
        :let [fusioned (fusion with-card card)]
        :when fusioned]
    [with-card card fusioned]))

;;todo use fusions fora
(defn shallow-fusions [cards]
  (for [i (range (count cards))
        :let [a (first (drop i cards))]
        j (range (inc i) (count cards))
        :let [b (first (drop j cards))
              fusioned (fusion a b)]
        :when fusioned]
    [a b fusioned]))

(defn merge-fusion [cards [a b fusioned :as fusioned-ab]]
  (as-> cards cards
    (remove-one a cards) 
    (remove-one b cards)
    (conj cards fusioned)))


(defn deep-fusions 
  ([cards] (deep-fusions {} cards nil))
  ([acc cards with-card]
  (println "cards" (map :Name cards))
  (println "with" (:Name with-card))
  (assoc acc :fusion-children 
    (map
      (fn [[_ _ fusioned :as fusioned-ab]]
        (println "fusion" (map :Name fusioned-ab))
        (let [new-cards (merge-fusion cards fusioned-ab)]
          (deep-fusions {:fusion fusioned-ab} new-cards fusioned)))
    (if with-card 
        (fusions-for with-card cards)
        (shallow-fusions cards))))))

(defn- compute-board-fusions [fusions board]
  (reduce
    (fn [fusions [_ _ hand-card :as hand-fusion]]
      (concat fusions 
              (for [board-card board
                    :let [board-fusion (fusion board-card hand-card)]
                    :when board-fusion]
                [board-card hand-card board-fusion])))
    []
    fusions))

(defn power [[_ _ fusioned]] 
  (+ (:Attack fusioned) (:Defense fusioned)))

(def sort-by-power (partial sort-by power))

(defn all-fusions [hand board]
  (let [hand-fusions (deep-fusions {} hand nil)
        all-hand-fusions (concat hand-fusions 
                                (map #(vector nil nil %) hand))
        board-fusions (compute-board-fusions all-hand-fusions board)]
    (into { :hand hand
            :board board
            :board-fusions board-fusions}
          hand-fusions)))

(defn card? [x] (and (map? x) (contains? x :Attack)))

(defn transform-cards [cards f]
  (clojure.walk/postwalk 
    (fn [x] (if (card? x) (f x) x))
    cards))

(defn the-only [cs]
  (if (= 1 (count cs))
    (first cs)
    (throw (ex-info (str "not unique" {:cards (transform-cards cs :Name)}){:cards cs}))))

(defn c [hand board]
 (let [hand (map (comp the-only find-by-name) hand)
       board (map (comp the-only find-by-name) board)]
       (-> (all-fusions hand board)
	(transform-cards (juxt :Name :Attack :Defense))
	(pp/pprint))))
