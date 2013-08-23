(ns ConstructiveMathematics.gaussian-integers)

(require '[ConstructiveMathematics.natural-numbers :as natural-numbers])
(require '[ConstructiveMathematics.integers :as integers])

(defn gaussian [r i] {:real r, :imaginary i})

(defn real-part [g] (:real g))
(defn imaginary-part [g] (:imaginary g))

(defn r-i [g] [(real-part g) (imaginary-part g)])

(def zero (gaussian integers/zero integers/zero))
(def one (gaussian integers/one integers/zero))
(def i (gaussian integers/zero integers/one))

(defn equal-to [g1 g2]
  (and
    (integers/equal-to (real-part g1) (real-part g2))
    (integers/equal-to (imaginary-part g1) (imaginary-part g2))))

(defn add [g1 g2]
  (let [[r1 i1] (r-i g1)
        [r2 i2] (r-i g2)]
    (gaussian (integers/add r1 r2) (integers/add i1 i2))))

(defn multiply [g1 g2]
  (let [[r1 i1] (r-i g1)
        [r2 i2] (r-i g2)]
    (gaussian (integers/subtract (integers/multiply r1 r2) (integers/multiply i1 i2))
              (integers/add (integers/multiply r1 i2) (integers/multiply r2 i1)))))

(defn norm [g]
  (reduce integers/add (map integers/square (r-i g))))

(defn unit? [g]
  (integers/equal-to integers/one (norm g)))

(defn conjugate [g]
  (let [[r i] (r-i g)]
    (gaussian r (integers/negate i))))

(defn try-divide-by-int [g int]
  (let [r' (integers/try-divide (real-part g) int)
        i' (integers/try-divide (imaginary-part g) int)]
    (and r' i' (gaussian r' i'))))

(defn try-divide [g1 g2]
  (try-divide-by-int (multiply g1 (conjugate g2)) (norm g2)))

(defn divide [g1 g2]
  (or (try-divide g1 g2) (throw (Exception. "not divisible"))))

(def divisible? try-divide) ; will be truthy if try-divide succeeds

(defn gaussians-with-magnitudes [i1 i2]
  (case [(integers/zero? i1) (integers/zero? i2)]
    [true true] (gaussian integers/zero integers/zero)
    [true false] [(gaussian integers/zero i2) (gaussian integers/zero (integers/negate i2))]
    [false true] [(gaussian i1 integers/zero) (gaussian (integers/negate i1) integers/zero)]
    [false false] [(gaussian i1 i2)
                   (gaussian (integers/negate i1) i2)
                   (gaussian i1 (integers/negate i2))
                   (gaussian (integers/negate i1) (integers/negate i2))]))


(defn indexes-of-norm [i]
  (if (integers/negative? i) []
    (for [i1 (integers/range integers/zero i)
          i2 (integers/range integers/zero i)
          :when (integers/equal-to i (integers/add (integers/square i1) (integers/square i2)))]
          [i1 i2])))

(defn gaussians-of-norm [i]
  (if (integers/zero? i)
    [zero]
    (for [[i1 i2] (indexes-of-norm i)
          g (gaussians-with-magnitudes i1 i2)]
      g)))


(gaussians-of-norm integers/zero)

(def all-gaussian-integers
  (->> integers/all-integers
    (filter #(not (integers/negative? %)))
    (mapcat gaussians-of-norm)))


(defn prime? [g]
  (let [n (norm g)]
    (and (integers/less-than integers/one n)
         (->> (integers/range integers/two (integers/predecessor-of n))
              (mapcat gaussians-of-norm)
              (not-any? #(divisible? g %))))))

(def all-gaussian-primes
  (filter prime? all-gaussian-integers))

