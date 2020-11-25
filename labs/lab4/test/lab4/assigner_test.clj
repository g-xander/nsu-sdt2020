(ns lab4.assigner-test
  (:require [clojure.test :refer :all]
            [lab4.atoms :as atoms]
            [lab4.operations :as operations]
            [lab4.translator :as translator]
            [lab4.assigner :as assigner]))

(deftest assign-value
  (testing "assignment"
    (let [exp (atoms/variable :foo)
          bar (operations/disjunction (atoms/constant true) (atoms/variable :bar) (atoms/constant false))]

      (is (= (assigner/assign-value :foo (atoms/constant true) exp) (atoms/constant true)))
      (is (= (assigner/assign-value :foo (atoms/constant false) exp) (atoms/constant false)))
      (is (not (= (assigner/assign-value :foo (atoms/constant true) exp) (atoms/constant false))))
      (is (= 
           (assigner/assign-value :bar (atoms/variable :qux) bar)
           (operations/disjunction (atoms/constant true) (atoms/variable :qux) (atoms/constant false)))))))