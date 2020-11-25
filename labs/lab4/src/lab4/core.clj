(ns lab4.core
  (:require [lab4.atoms :as atoms]
            [lab4.translator :as translator]
            [lab4.operations :as operations]
            [lab4.assigner :as assigner]))

; This file contains the demo code

(defn xor [exp1 exp2]
  (list ::xor exp1 exp2))

(defn xor? [exp]
  (= (first exp) ::xor))

(defn xor-tranlator [t exp]
  (let [a (nth exp 1)
        b (nth exp 2)]

    (translator/translate-simplify
     (operations/disjunction
      (operations/conjunction (operations/negation a) b)
      (operations/conjunction a (operations/negation b))) t)))

(defn nor [exp1 exp2]
  (list ::nor exp1 exp2))

(defn nor? [exp]
  (= (first exp) ::nor))

(defn nor-tranlator [t exp]
  (let [a (nth exp 1)
        b (nth exp 2)]

    (translator/translate-simplify
     (operations/conjunction
      (operations/negation a)
      (operations/negation b)) t)))

(let [new-rules [[xor? xor-tranlator] [nor? nor-tranlator]]
      wiki-sample (operations/disjunction
                   (atoms/variable :A)
                   (operations/conjunction
                    (atoms/variable :B)
                    (operations/disjunction (atoms/variable :C) (atoms/variable :D))))]

  (translator/translate-simplify (xor (atoms/constant false) (atoms/constant true)) new-rules)
  (translator/translate-simplify (nor (atoms/constant false) (atoms/constant true)) new-rules)
  (translator/translate-simplify wiki-sample)
  (translator/translate-simplify (assigner/assign-value :A (atoms/constant true) wiki-sample)))