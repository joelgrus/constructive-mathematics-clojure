(ns ConstructiveMathematics.integers-test
  (:refer-clojure :exclude [compare max min zero? range]) ; suppress the shadowing warning
  (:require [clojure.core :as core]) ; allow to still reach clojure.core/compare through core/compare
  (:require [ConstructiveMathematics.natural-numbers-test :as nn-test])
  (:require [ConstructiveMathematics.test-helpers :as test-helpers])
  (:use clojure.test
        ConstructiveMathematics.integers))

(defn to-int32 [i]
  (let [[sign n] (sign-n i)]
    (case sign
      :positive (nn-test/to-int32 n)
      :zero 0
      :negative (- (nn-test/to-int32 n)))))

(defn to-string [i]
  (str (to-int32 i)))

(defn from-int32 [int32]
  (cond
    (= int32 0) zero
    (> int32 0) (positive (nn-test/from-int32 int32))
    (< int32 0) (negative (nn-test/from-int32 (- int32)))))

(deftest equal-to-test
   (is (equal-to one one))
   (is (equal-to three three))
   (is (equal-to zero zero))
   (is (equal-to minus-one minus-one))
   (is (not (equal-to one three)))
   (is (not (equal-to one minus-one))))

(deftest less-than-test
  (is (not (less-than one one)))
  (is (less-than one three))
  (is (not (less-than three one)))
  (is (less-than minus-one one)))

(deftest add-test
  (is (equal-to two (add one one)))
  (is (equal-to zero (add one minus-one)))
  (is (equal-to two (add two zero)))
  (is (equal-to (from-int32 150) (add (from-int32 50) (from-int32 100))))
  (is (equal-to (from-int32 50) (add (from-int32 100) (from-int32 -50)))))

(deftest multiply-test
  (is (equal-to one (multiply one one)))
  (is (equal-to four (multiply two two)))
  (is (equal-to four (multiply minus-two minus-two)))
  (is (equal-to (from-int32 -6) (multiply two (negate three))))
  (is (equal-to (from-int32 42) (multiply (from-int32 6) (from-int32 7)))))

(def first1000 (into [] (take 1000 all-integers)))
(def fifty-seven (from-int32 57))
(def minus-one-seventy-three (from-int32 -173))

(deftest all-integers-test
  (is (every? #(test-helpers/unique-up-to-i first1000 %) (core/range 1000)))
  (is (some #(equal-to % fifty-seven) all-integers))
  (is (some #(equal-to % minus-one-seventy-three) all-integers)))

(deftest subtract-test
  (is (equal-to zero (subtract one one)))
  (is (equal-to minus-one (subtract two three)))
  (is (equal-to two (subtract three one)))
  (is (equal-to (from-int32 50) (subtract (from-int32 100) (from-int32 50))))
  (is (equal-to (from-int32 -50) (subtract (from-int32 50) (from-int32 100)))))