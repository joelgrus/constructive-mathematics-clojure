(ns ConstructiveMathematics.real-numbers-test
  (:refer-clojure :exclude [compare max min zero? range numerator denominator]) ; suppress the shadowing warning
  (:require [clojure.core :as core]) ; allow to still reach clojure.core/compare through core/compare
  (:require [ConstructiveMathematics.natural-numbers-test :as nn-test])
  (:require [ConstructiveMathematics.integers-test :as i-test])
  (:require [ConstructiveMathematics.test-helpers :as test-helpers])
  (:use clojure.test
        ConstructiveMathematics.real-numbers))


;(deftest square-root-of-two-test
;  (is (equal-to two (multiply square-root-of-two square-root-of-two))))