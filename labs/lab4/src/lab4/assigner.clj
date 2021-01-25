(ns lab4.assigner
  (:require [lab4.atoms :as atoms]
            [lab4.utils :as utils]))

(defn assign-value
  "Assign value to variable 
   @var-name - name of the defined variable
   @var-value - new value of this variable
   @exp - expression containing the variable"
  [var-name var-value exp]
  {:pre [(and
          (keyword? var-name)
          (or (atoms/constant? var-value) (atoms/variable? var-value)))]}
  (cond
    (and (atoms/variable? exp) (= var-name (atoms/variable-name exp)))
    var-value

    (utils/atom? exp)
    exp

    :else
    (cons (first exp) (map #(assign-value var-name var-value %) (rest exp)))))
