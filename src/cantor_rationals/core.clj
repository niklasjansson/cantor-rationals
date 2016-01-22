(ns cantor-rationals.core)

;Stolen from http://rosettacode.org/wiki/Greatest_common_divisor#Clojure
(defn gcd
  "(gcd a b) computes the greatest common divisor of a and b."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn gen-rationals []
  (flatten (for [i (map inc (range))]
    (for [j (range 1 i)
          :let [k (- i j)]]
      (/ j k)))))

(defn gen-distinct-rationals []
  (flatten (for [i (map inc (range))]
    (for [j (range 1 i)
          :let [k (- i j)]
          :when (= 1 (gcd i k))]
      (/ j k)))))

(defn gen-distinct-rationals-including-negative []
  (concat [0] (interleave (gen-distinct-rationals) (map - (gen-distinct-rationals)))))

;(take 100 (gen-distinct-rationals-including-negative))

