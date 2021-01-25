(ns lab4.operations-test
  (:require [clojure.test :refer :all]
            [lab4.atoms :as atoms]
            [lab4.operations :as operations]))

(deftest conjunction-test
  (testing "conjunctions"
    (let [foo (atoms/variable :foo)
          bar (atoms/variable :bar)]
      
      (is (operations/conjunction? (operations/conjunction foo bar)))
      (is (not (operations/conjunction? foo))))))

(deftest disjunction-test
  (testing "disjunctions"
    (let [foo (atoms/variable :foo)
          bar (atoms/variable :bar)]

      (is (operations/disjunction? (operations/disjunction foo bar)))
      (is (not (operations/disjunction? foo))))))

(deftest negation-test
  (testing "negations"
    (let [foo (atoms/variable :foo)]

      (is (operations/negation? (operations/negation foo)))
      (is (not (operations/negation? foo))))))