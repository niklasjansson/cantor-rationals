(ns cantor-rationals.core
  (:require [clojure.math.numeric-tower :as math]))

(defn gen-rationals []
  (flatten (for [i (map inc (range))]
    (for [j (range 1 i)
          :let [k (- i j)]]
      (/ j k)))))

(defn gen-distinct-rationals []
  (flatten (for [i (map inc (range))]
    (for [j (range 1 i)
          :let [k (- i j)]
          :when (= 1 (math/gcd i k))]
      (/ j k)))))

(defn gen-distinct-rationals-including-negative []
  (concat [0] (interleave (gen-distinct-rationals) (map - (gen-distinct-rationals)))))

(defn length-in-base [n b]
  (loop [c n
         i 0]
    (if (zero? c)
      i
      (recur (quot c b) (inc i)))))

(defn rational-from-repeating-decimal
  "(rational-from-repeating-decimal a b) returns the fraction represented by the decimal number a.(b)"
  [a b]
  ( let [base 10
         length-b (count (str b))
         factor (math/expt 10 length-b)
         l (- (+ (* a factor) b) a)]
    (/ l (dec factor))))

(defn rational-from-repeating
  [a b base]
  ( let [
         length-b (count (str b)) ; in base
         factor (math/expt base length-b)
         l (- (+ (* a factor) b) a)]
    (/ l (dec factor))))


(rational-from-repeating-decimal 2 718281)

(take 100 (gen-distinct-rationals-including-negative))

