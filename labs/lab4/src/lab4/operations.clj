(ns lab4.operations)

(defn conjunction
  "logical and"
  [exp & exp-rest]
  (cons ::conjunction (cons exp exp-rest)))

(defn conjunction?
  "logical and?"
  [exp]
  (= (first exp) ::conjunction))

(defn disjunction
  "logical or"
  [exp & exp-rest]
  (cons ::disjunction (cons exp exp-rest)))

(defn disjunction?
  "logical or?"
  [exp]
  (= (first exp) ::disjunction))

(defn negation
  "logical negation"
  [exp]
  (list ::negation exp))

(defn negation?
  "logical negation?"
  [exp]
  (= (first exp) ::negation))