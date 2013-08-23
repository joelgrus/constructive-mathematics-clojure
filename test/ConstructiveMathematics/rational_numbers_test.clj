(ns ConstructiveMathematics.rational-numbers-test

  (:refer-clojure :exclude [compare max min zero? range numerator denominator]) ; suppress the shadowing warning
  (:require [clojure.core :as core]) ; allow to still reach clojure.core/compare through core/compare
  (:require [ConstructiveMathematics.natural-numbers-test :as nn-test])
  (:require [ConstructiveMathematics.integers-test :as i-test])
  (:require [ConstructiveMathematics.test-helpers :as test-helpers])
  (:use clojure.test
        ConstructiveMathematics.rational-numbers))
  
  (defn from-int32s [n d]
    (rational (i-test/from-int32 n) (i-test/from-int32 d)))
  
  (defn to-int32s [r]
    [(i-test/to-int32 (numerator r)) (i-test/to-int32 (denominator r))])
  
  (defn to-string [r]
    (let [[n d] (to-int32s r)]
      (str n " / " d)))
  
  (def one-half (from-int32s 1 2))
  (def two-fourths (from-int32s 2 4))
  (def three-fourths (from-int32s 3 4))
  (def minus-one-half (from-int32s -1 2))
  (def one-minus-half (from-int32s 1 -2))
  
  (deftest equal-to-test
    (is (equal-to one-half two-fourths))
    (is (equal-to one (from-int32s 10 10)))
    (is (equal-to minus-one-half one-minus-half))
    (is (not (equal-to one-half three-fourths))))
  
  (deftest less-than-test
    (is (less-than one-half three-fourths))
    (is (not (less-than one-half two-fourths)))
    (is (less-than three-fourths one))
    (is (less-than minus-one-half zero))
    (is (not (less-than one three-fourths))))  