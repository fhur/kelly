(ns kelly.random
  (:require
    [medley.core :refer [map-vals]])
  (:import (java.util Random)))

(def random (new Random 0xc0de))



(defn toss-coin
  "Tosses a coin with the given probability of winning.

  Returns -1 for a loss and +1 for a win."
  [& {:keys [p when-win when-loss] :or {when-win true when-loss false}}]
  (let [limit 1e6
        cut-off (* p limit)
        rnd (.nextInt random limit)]

    (if (<= rnd cut-off)
      when-win
      when-loss)))

(defn create-single-value-history-store
  []
  {:initial identity
   :current-value identity
   :appender (fn [_ new-value] new-value)})

(defn create-vector-history-store
  []
  {:initial vector
   :current-value peek
   :appender conj})

(let [n 1e1]
  (->> (map (fn [_] (toss-coin :p 0.5)) (range))
       (take n)
       (group-by identity)
       ;(map-vals #(/ (count %) n))
       (println)))