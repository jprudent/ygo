(ns ygo.core-test
    (:require [ygo.core :as sut]
        [clojure.test :as t]))

(def dragon-piper (sut/find-by-id 40))
;; +
(def hibikime (sut/find-by-id 649))
;; =
(def flame-swordsman (sut/find-by-id 15))
;; +
(def blue-winged-clown (sut/find-by-id 636))
;; =
(def crimson-sunbird (sut/find-by-id 467))

;; blue-winged-clown + flame-viper = crimson-sunbird
(def flame-viper (sut/find-by-id 644))

;; white dragon has no fusions
(def white-dragon (sut/find-by-id 1))

(t/deftest find-by-name-test
    (t/is (= [dragon-piper] (sut/find-by-name "dragon piper"))))

(t/deftest find-by-id-test
    (t/is (= "Blue-eyes White Dragon" (:Name (sut/find-by-id 1))))
    (t/is (= "Magician of Black Chaos" (:Name (sut/find-by-id 722)))
    (t/is (nil? (sut/find-by-id 4242)))))

(t/deftest fusion-test
        (t/is (nil? (sut/fusion white-dragon dragon-piper)))
        (t/is (= flame-swordsman (sut/fusion dragon-piper hibikime)))
        (t/is (= crimson-sunbird (sut/fusion flame-swordsman blue-winged-clown))))

(t/deftest shallow-fusions-test
    (t/is (empty? (sut/shallow-fusions [])))
    (t/is (empty? (sut/shallow-fusions [dragon-piper])))
    (t/is (= [[dragon-piper hibikime flame-swordsman]]
             (sut/shallow-fusions [dragon-piper hibikime])))
    (t/is (= [[dragon-piper hibikime flame-swordsman]
              [dragon-piper blue-winged-clown crimson-sunbird]]
             (sut/shallow-fusions [dragon-piper 
                               hibikime 
                               blue-winged-clown]))))

(def empty-deep-fusion {:fusion-children []})
(t/deftest deep-fusion-test
    (t/is (= empty-deep-fusion (sut/deep-fusions [])))
    (t/is (= empty-deep-fusion (sut/deep-fusions [white-dragon])))
    (t/is (= empty-deep-fusion (sut/deep-fusions [white-dragon dragon-piper])))
    (t/is (=  {:fusion-children [{:fusion [dragon-piper hibikime flame-swordsman]
                                  :fusion-children []}]}
             (sut/deep-fusions [dragon-piper
                                hibikime])))
    (t/is (= {:fusion-children [{:fusion [dragon-piper hibikime flame-swordsman]
                                 :fusion-children [{:fusion [flame-swordsman blue-winged-clown crimson-sunbird]
                                                    :fusion-children []}]}
                                {:fusion [dragon-piper blue-winged-clown crimson-sunbird]
                                 :fusion-children []}]}
             (sut/deep-fusions [dragon-piper hibikime blue-winged-clown]))))

(t/deftest all-fusions-test
  (let [r (sut/all-fusions [dragon-piper hibikime blue-winged-clown]
                           [flame-viper])]
    (t/is (= {:hand [dragon-piper hibikime blue-winged-clown]
              :board [flame-viper]
              :board-fusions [[flame-viper hibikime flame-swordsman]
                              [flame-viper blue-winged-clown crimson-sunbird]]
              :fusion-children [{:fusion [dragon-piper hibikime flame-swordsman]
                                 :fusion-children [{:fusion [flame-swordsman blue-winged-clown crimson-sunbird]
                                                    :fusion-children []}]}
                                {:fusion [dragon-piper blue-winged-clown crimson-sunbird]
                                 :fusion-children []}]}
        (sut/all-fusions [dragon-piper hibikime blue-winged-clown]
             [flame-viper])))))