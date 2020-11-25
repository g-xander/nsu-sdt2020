(ns lab4.translator
  (:require [lab4.atoms :as atoms]
            [lab4.utils :as utils]
            [lab4.operations :as operations]))

(declare translator-translate)
(declare push-negations)
(declare distribution-law)
(declare simplify)

(def translator-rules
  ;list of rules of kind [test action]
  ;it's possible to extend them...
  (list
   [#(atoms/variable? %) #(identity %2)]
   [#(atoms/constant? %) #(identity %2)]
   [#(operations/conjunction? %)
    (fn [tt exp] (apply operations/conjunction (map #((partial translator-translate tt) %) (utils/get-next exp))))]
   [#(operations/disjunction? %)
    (fn [tt exp] (apply operations/disjunction (map #((partial translator-translate tt) %) (utils/get-next exp))))]
   [#(operations/negation? %)
    (fn [tt exp] (operations/negation ((partial translator-translate tt) (utils/get-next exp))))]))


(defn ^:private translator-translate
  "private. looking up a rule for transformation"
  [transform-table exp]
  ((some (fn [rule]
           (if ((first rule) exp)
             (second rule)
             false))
         transform-table) transform-table exp))

(defmulti ^:private push-negations "pushing negations inside expression @exp"
  (fn [exp] (if (operations/negation? exp)
              (first (second exp))
              :default)))

;case (not (not exp)) => exp
;example (negation (negation (var a))) => 
(defmethod ^:private push-negations :lab4.operations/negation [exp]
  (push-negations (first (rest (first (rest exp))))))
  ;; (push-negations (apply concat (rest (apply concat (rest exp))))))

;case (not (conj a b)) => (disj (not a) (not b))
(defmethod ^:private push-negations :lab4.operations/conjunction [exp]
  (apply operations/disjunction (map #(push-negations (operations/negation %)) (rest (first (rest exp))))))
  ;; (apply operations/disjunction (map #(push-negations (operations/negation %)) (rest (apply concat (rest exp))))))

;case (not (disj a b)) => (conj (not a) (not b))
(defmethod ^:private push-negations :lab4.operations/disjunction [exp]
  (apply operations/conjunction (map #(push-negations (operations/negation %)) (rest (first (rest exp))))))

;case default (not (var|const))
(defmethod ^:private push-negations :default [exp]
  (cond
    (operations/negation? exp)
    (let [operand (first (rest exp))]
      (if (atoms/constant? operand)
        (atoms/constant (not (atoms/constant-value operand)))
        (operations/negation operand)))

    (utils/atom? exp)
    (if (atoms/constant? exp)
      (atoms/constant (not (atoms/constant-value exp)))
      exp)

    :else exp))

(defmulti ^:private distribution-law "private. applying distribution law to @exp"
  (fn [exp] (first exp)))

(defmethod ^:private distribution-law :default [exp]
  exp)

(defmethod ^:private distribution-law :lab4.operations/disjunction [exp]
  (apply operations/disjunction (map distribution-law (rest exp))))

(defmethod ^:private distribution-law :lab4.operations/negation [exp]
  (operations/negation (distribution-law (rest exp))))

(defmethod ^:private distribution-law :lab4.operations/conjunction [exp]
  (cond
    (every? utils/primitive-or-conj-of-primitives? (rest exp))
    exp

    :else
    (let [disjunction-part (first (filter operations/disjunction? (rest exp)))
          leftover (remove #(= % disjunction-part) (rest exp))]

      (apply operations/disjunction (map #(apply operations/conjunction % leftover)
                                         (rest disjunction-part))))))

; simplify the expression
(defmulti ^:private simplify "private. simplifying the @exp"
  (fn [exp] (first exp)))

; pass by default
(defmethod ^:private simplify :default [exp]
  exp)

(defmethod ^:private simplify :lab4.operations/conjunction [exp]
  (let [exp (utils/collapse-constants (distinct (utils/inside-out operations/conjunction? exp)) every?)
        exp-1 (nth exp 1)
        exp-rest (drop 2 exp)
        true-const (atoms/constant true)
        false-const (atoms/constant false)]

    (cond
      (utils/l-contains? exp false-const)
      false-const

      (empty? exp-rest)
      (simplify exp-1)

      ;true constant is first in exp apply conj to whats left
      (and (= exp-1 true-const) (seq exp-rest))
      (simplify (if (= (count exp-rest) 1)
                  (first exp-rest)
                  (apply operations/conjunction exp-rest)))

      :else (apply operations/conjunction (map simplify (rest exp))))))

(defmethod ^:private simplify :lab4.operations/disjunction [exp]
  (let [exp (utils/collapse-constants (distinct (utils/inside-out operations/disjunction? exp)) some)
        exp-1  (nth exp 1)
        exp-rest (drop 2 exp)
        true-const (atoms/constant true)
        false-const (atoms/constant false)]

    (cond
      (utils/l-contains? exp true-const)
      true-const

      (empty? exp-rest)
      (simplify exp-1)

      ;false constant is first in exp apply disj to whats left
      (and (= exp-1 false-const) (seq exp-rest))
      (simplify (if (= (count exp-rest) 1)
                  (first exp-rest)
                  (apply operations/disjunction exp-rest)))

      :else (apply operations/disjunction (map simplify (rest exp))))))

(defn ^:private simplify-runner
  "private. Running simplifications on @exp until no longer possible to simplify"
  ([exp] (simplify-runner exp (simplify exp)))
  ([exp prev-result]
   (let [curr-result (simplify prev-result)]
     (if (= curr-result prev-result)
       curr-result
       (simplify-runner exp curr-result)))))

(defn translate
  "Translates expression @exp to DNF form
   @exp - expression for transformation
   @user-rules - vector of additional rules of kind [pred action]"
  [exp & [user-rules]]
  (->>
   exp
   (translator-translate (concat translator-rules user-rules))
   (push-negations)
   (distribution-law)))

(defn translate-simplify
  "Translates expression to DNF form, also simplifies it
   @exp - expression for transformation
   @user-rules - vector of additional rules of kind [pred action]"
  [exp & [user-rules]]
  (->>
   exp
   (translator-translate (concat translator-rules user-rules))
   (push-negations)
   (distribution-law)
   (simplify-runner)))
