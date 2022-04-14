(set! *warn-on-reflection* true)
(ns alphabet-cipher.coder
  (:require [clojure.string :refer [join]])
  (:import (clojure.lang PersistentVector)))

(defn- rotate ^Integer [^Integer n ^String s]
  (lazy-cat (drop n s) (take n s)))

(defn- char-idx ^Integer [^Character c]
  "Numerical value for char based on its distance from a"
  (assert (and (<= (int c) (int \z)) (>= (int c) (int \a))) "We only support lowercase ASCII")
  (- (int c) (int \a)))

(defn- nth-char-in-alphabet ^Character [^Integer i]
  "Char based on its distance from a (zero-based)"
  (assert (and (<= i 26) (>= i 0)) (str "We only support lowercase ASCII: Received " i))
  (let [char-int (+ (int \a) i)]
    (char char-int)))

(defn- ^PersistentVector alphabet-as-list
  ([] (let [ord-a (int \a)
            ord-z (int \z)
            chars (map (comp str char) (range ord-a (inc ord-z)))]
        chars))
  ([^Character c] (let [a-to-z (alphabet-as-list)]
                    (vec (rotate (char-idx c) a-to-z)))))

(defn- repeat-str-to-match-length ^String [^String repeat-this ^String match-this]
  (assert (< 0 (count repeat-this)) "Cannot repeat empty string to a given length")
  (assert (< 0 (count match-this)) "Cannot repeat a string out to length 0")
  (let [multiplier (int (Math/ceil (double (/ (count match-this) (count repeat-this)))))
        repeated-but-maybe-too-long (apply str (repeat multiplier repeat-this))]
    (subs repeated-but-maybe-too-long 0 (count match-this))))

(defn- zip-together [keyword message]
  (let [keyword-repeated (repeat-str-to-match-length keyword message)]
    (map vector keyword-repeated message)))

(defn is-repetition [string pfx-len]
  (let [pfx (subs string 0 pfx-len)
        repeated (repeat-str-to-match-length pfx string)]
    (= repeated string)) )

(defn shortest-repeated-pfx [string]
  (let [len-repeated-pfxs (filter (fn [pfx-len] (is-repetition string pfx-len))
                                  (range 1 (count string)))
        len-pfx (apply min len-repeated-pfxs)]
    (subs string 0, len-pfx)))

(defn encode [keyword message]
  (let [keyword-repeated (repeat-str-to-match-length keyword message)
        zip (map vector keyword-repeated message)
        encoded-list (map (fn [[key-char plain-char]] (nth (alphabet-as-list key-char) (char-idx plain-char)))
                          zip)]
    (join "" encoded-list)))


(defn decode [keyword message]
  (let [ zip (zip-together keyword message)
        encoded-list (map (fn [[key-char enc-char]]
                            (let [alphabet (alphabet-as-list key-char) idx (.indexOf alphabet (str enc-char))]
                              (nth-char-in-alphabet idx))) zip)]
    (join "" encoded-list)))


(defn decipher [cipher message]
  (let [repeated-key (decode message cipher)]
    (shortest-repeated-pfx repeated-key)))
