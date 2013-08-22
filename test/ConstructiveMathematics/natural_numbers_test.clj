(ns ConstructiveMathematics.natural-numbers-test
  (:refer-clojure :exclude [compare])
  (:require [clojure.core :as core]) ; allow to still reach clojure.core/compare through core/compare
  (:require [ConstructiveMathematics.test-helpers :as test-helpers)  
  (:use clojure.test
        ConstructiveMathematics.natural-numbers))

(defn to-int32 [n]
  (if (one? n) 1 (+ 1 (predecessor-of n))))

(defn to-string [n]
  (str (to-int32 n)))

(defn from-int32 [i]
  (cond 
    (<= i 0) (throw (Exception. "natural number must be greater than 0"))
    (= i 1) one
    :else (successor-of (from-int32 (- i 1)))))

(deftest equal-to-test
   (is (equal-to one one))
   (is (equal-to three three))
   (is (not (equal-to one three))))

(deftest less-than-test
  (is (not (less-than one one)))
  (is (less-than one three))
  (is (not (less-than three one))))

(deftest less-than-or-equal-to-test
  (is (less-than-or-equal-to one one))
  (is (less-than-or-equal-to one three))
  (is (not (less-than-or-equal-to three one))))

(deftest add-test
  (is (equal-to two (add one one)))
  (is (equal-to three (add one two)))
  (is (equal-to three (add two one)))
  (is (equal-to (from-int32 150) (add (from-int32 50) (from-int32 100)))))

(deftest multiply-test
  (is (equal-to one (multiply one one)))
  (is (equal-to four (multiply two two)))
  (is (equal-to (from-int32 42) (multiply (from-int32 6) (from-int32 7)))))

(def first1000 (into [] (take 1000 all-naturals)))
(def fifty-seven (from-int32 57))

(deftest all-naturals-test
  (is (every? #(test-helpers/unique-up-to-i first1000 %) (range 1000)))
  (is (some #(equal-to % fifty-seven) all-naturals)))
  