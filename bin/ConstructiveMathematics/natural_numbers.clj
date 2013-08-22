(ns ConstructiveMathematics.natural-numbers
  (:refer-clojure :exclude [compare]) ; suppress the shadowing warning
  (:require [clojure.core :as core]) ; allow to still reach clojure.core/compare through core/compare
 )

(defn successor-of [n] {:predecessor n})
(defn predecessor-of [n] (:predecessor n))

(def one (successor-of nil))
(def two (successor-of one))
(def three (successor-of two))
(def four (successor-of three))
; and so on

(defn one? [n] (nil? (predecessor-of n)))

(defn compare [n1 n2]
 (cond
   (and (one? n1) (one? n2)) :equal
    (one? n1) :less-than
    (one? n2) :greater-than
    :else (compare (predecessor-of n1) (predecessor-of n2))))

; convenience functions
(defn equal-to [n1 n2] (= :equal (compare n1 n2)))
(defn less-than [n1 n2] (= :less-than (compare n1 n2)))
(defn less-than-or-equal-to [n1 n2] (not= :greater-than (compare n1 n2)))

(defn add [n1 n2]
  (if (one? n1)
    (successor-of n2)
    (add (predecessor-of n1) (successor-of n2))))

(defn multiply [n1 n2]
    (if (one? n1)
      n2
      (add n2 (multiply (predecessor-of n1) n2))))

(defn all-naturals-from [n]
  (lazy-cat [n] (all-naturals-from (successor-of n))))

(def all-naturals (all-naturals-from one))

(defn try-subtract [n1 n2]
  (cond
    (one? n1) nil
    (one? n2) (predecessor-of n1)
    :else (try-subtract (predecessor-of n1) (predecessor-of n2))))

(defn subtract [n1 n2]
  (let [ts (try-subtract n1 n2)]
    (if (nil? ts) (throw (Exception. "impossible subtraction!")) ts)))