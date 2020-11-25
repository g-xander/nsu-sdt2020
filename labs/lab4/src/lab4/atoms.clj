(ns lab4.atoms)

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::variable name))

(defn variable? [exp]
  (= (first exp) ::variable))

(defn variable-name [exp]
  (second exp))

(defn same-variable? [exp1 exp2]
  (and
   (variable? exp1) (variable? exp2))
  (= (variable-name exp1) (variable-name exp2)))

(defn constant [value]
  {:pre [(boolean? value)]}
  (list ::constant value))

(defn constant? [exp]
  (= (first exp) ::constant))

(defn constant-value [exp]
  (second exp))
