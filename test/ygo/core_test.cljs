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

(t/deftest all-fusions-test
    (t/is (empty? (sut/all-fusions [])))
    (t/is (empty? (sut/all-fusions [dragon-piper])))
    (t/is (= [[dragon-piper hibikime flame-swordsman]]
             (sut/all-fusions [dragon-piper hibikime])))
    (t/is (= [[dragon-piper hibikime flame-swordsman]
              [dragon-piper blue-winged-clown crimson-sunbird]]
             (sut/all-fusions [dragon-piper 
                               hibikime 
                               blue-winged-clown]))))