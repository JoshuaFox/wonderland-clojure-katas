(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn distance [^String w1 ^String w2]
  (if
    (not= (count w1) (count w2)) ##Inf
                                 (count (filter #(not= (nth w1 %) (nth w2 %)) (range (count w1))))))

(def get-neighbors (memoize (fn [word]
                              (filter (fn [other-word] (= (distance word other-word) 1)) words))))

(defn lazy-contains? [col key]
  (some #{key} col))

(defn doublets
  ([word1 word2 chain]
   (let [neighbors (get-neighbors word2)]
     (cond
       (lazy-contains? neighbors word1)
       (conj chain word1)
       :else
       (for [w neighbors :when (not (lazy-contains? chain w))]
         (doublets word1 w (conj chain w))))))
  ([word1 word2]
   (doublets word1 word2 (list word2))))

