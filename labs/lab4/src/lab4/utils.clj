(ns lab4.utils
  (:require [lab4.atoms :as atoms]
            [lab4.operations :as operations]))

(defn get-next [coll]
  (let [res (rest coll)]
    (if (= (count res) 1)
      (first res)
      res)))

(defn l-contains? [coll key]
  (boolean (some #(= % key) coll)))

(defn atom?
  "Check if atom"
  [exp]
  (or (atoms/constant? exp) (atoms/variable? exp)))

(defn primitive?
  "Check if either atom? or negation of atom"
  [exp]
  (or (atom? exp) (and (operations/negation? exp) (atom? (get-next exp)))))

(defn primitive-or-conj-of-primitives?
  "Check if either primiteve or conjunction of primitives"
  [exp]
  (or
   (primitive? exp)
   (and
    (operations/conjunction? exp)
    (every? primitive-or-conj-of-primitives? (rest exp)))))

;should pass either some or every?
(defn collapse-constants
  "Collapse constant in @exp on @pred"
  [exp pred]
  (let [c (filter atoms/constant? (rest exp))]
    (if (> (count c) 0)
      (let [cc (atoms/constant (boolean (pred true? (map #(atoms/constant-value %) c))))]
        (cons (first exp) (cons cc (filter #(not (atoms/constant? %)) (rest exp)))))
      exp)))

(defn inside-out
  "Pushes 1-level deep same operations into current operation: (conj a b (conj c d)) => (conj a b c d)"
  [cmp? exp]
  (cons (first exp)
        (concat
         (filter #(not (cmp? %)) (rest exp)) ;filter not (first exp)
         (apply concat (map #(rest %) (filter #(cmp? %) (rest exp)))) ;find 1-level nested same (first exp) operands 
         )))
