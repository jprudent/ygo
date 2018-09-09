(ns ygo.sugar
  (:require [ygo.core :as ygo]
            [cljs-node-io.core :as io :refer [slurp spit]]
            [cljs.reader :as reader]
            [clojure.walk :as walk]
            [clojure.pprint :as pp]))

(defonce session (atom nil))
(def filename "/tmp/ygo.session")

(defn start-session []
  (->> (slurp filename)
       (reader/read-string)
       (reset! session)))

(defn new-session []
  (reset! session
  {:fusions {}
   :hand []
   :board []
   :history []}))

(defn the-only [cs]
  (if (= 1 (count cs))
    (first cs)
    (throw (ex-info (str "not unique" {:cards (ygo/transform-cards cs :Name)}){:cards cs}))))

(defn humanly [cards] (ygo/transform-cards cards (juxt :Name :Attack :Defense)))

(defn print-session []
 (->> @session 
      (humanly)
      (pp/pprint)))

(defn update-session []
  (println "update session")
  (swap! session 
    (fn [{:keys [hand board] :as session}]
      (-> session 
          (assoc :fusions (ygo/all-fusions hand board)
          #_(update :history conj (dissoc session :history))))))
  (print-session))
  
(defn defcards [hand board]
 (let [hand (map (comp the-only ygo/find-by-name) hand)
       board (map (comp the-only ygo/find-by-name) board)]
       (reset! session {:hand hand :board board})
       (update-session)))

(defn find-fusion-node [session fusion-id]
  (->> (tree-seq :fusion-children (comp not-empty :fusion-children) (:fusions session))
       (filter #(= fusion-id (:fusion-id %)))
       (first)))

(defn remove-hand-cards [session fusion-id]
  (if fusion-id
    (let [fusion-node (find-fusion-node session fusion-id)
          {:keys [fusion parent-fusion-id]} fusion-node
          [a b fusioned] fusion]
      (recur (update session :hand #(->> % (ygo/remove-one a) (ygo/remove-one b)))
             parent-fusion-id))
    session))
       
(defn fuse [fusion-id]
  (swap! session
    (fn [session]
      (if-let [{:keys [fusion]} (find-fusion-node session fusion-id)]
         (do (let [[a b fusioned] fusion]
	       (-> session
	           (remove-hand-cards fusion-id)
                   (update :board conj fusioned)))
             (update-session))
         (do (println "fusion not found") session)))))

(defn +h [& name-fragments]
  (when-let [+hand-cards (not-empty (map (comp the-only ygo/find-by-name) name-fragments))]
    (do (swap! session update :hand concat +hand-cards)
        (update-session))))


