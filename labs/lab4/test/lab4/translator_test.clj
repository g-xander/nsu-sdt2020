(ns lab4.translator-test
  (:require [clojure.test :refer :all]
            [lab4.atoms :as atoms]
            [lab4.operations :as operations]
            [lab4.translator :as translator]))

(deftest push-negations
  (testing "(not (not A)) => A"
    (let [foo (operations/negation (operations/negation (atoms/variable :foo)))
          bar (atoms/variable :foo)
          baz (operations/negation foo)
          f #'translator/push-negations]

      (is (= (f foo) bar))
      (is (not (= (f baz) bar)))))
  
  (testing "(not (not (not A))) => not A"
    (let [foo (operations/negation (operations/negation (operations/negation (atoms/variable :foo))))
          bar (operations/negation (atoms/variable :foo))
          f #'translator/push-negations]

      (is (= (f foo) bar))))

  (testing "(not (conj a b)) => (disj (not a) (not b))"
    (let [foo (atoms/variable :a)
          bar (atoms/variable :b)
          baz (operations/negation (operations/conjunction foo bar))
          qux (operations/disjunction (operations/negation foo) (operations/negation bar))
          f #'translator/push-negations]

      (is (= (f baz) qux))))

  (testing "(not (disj a b)) => (conj (not a) (not b))"
    (let [foo (atoms/variable :a)
          bar (atoms/variable :b)
          baz (operations/negation (operations/disjunction foo bar))
          qux (operations/conjunction (operations/negation foo) (operations/negation bar))
          f #'translator/push-negations]

      (is (= (f baz) qux))))

  (testing "(disj a b) => (disj a b)"
    (let [foo (atoms/variable :a)
          bar (atoms/variable :b)
          baz (operations/disjunction foo bar)
          f #'translator/push-negations]

      (is (= (f baz) baz)))))

(deftest distribution-law
  (testing "(A and (B or C)) => (A and B) or (A and C)"
    (let [f #'translator/distribution-law
          foo (atoms/variable :a)
          bar (atoms/variable :b)
          baz (atoms/variable :c)
          qux (operations/conjunction foo (operations/disjunction bar baz))
          quux (operations/disjunction (operations/conjunction bar foo) (operations/conjunction baz foo))]

      (is (= (f qux) quux))))

  (testing "(A or B) and C => (A and C) or (B and C)"
    (let [f #'translator/distribution-law
          foo (atoms/variable :a)
          bar (atoms/variable :b)
          baz (atoms/variable :c)
          qux (operations/conjunction (operations/disjunction foo bar) baz)
          quux (operations/disjunction (operations/conjunction foo baz) (operations/conjunction bar baz))]

      (is (= (f qux) quux)))))

(deftest simplify
  (testing "(conj var_a var_a) => a"
    (let [f #'translator/simplify
          foo (operations/conjunction (atoms/variable :a) (atoms/variable :a))
          bar (atoms/variable :a)]

      (is (= (f foo) bar))))

  (testing "(false and false ) => false | (false and true) => false | (true and false) => false | (true and true) => true"
    (let [f #'translator/simplify]

      (is (= (f (operations/conjunction (atoms/constant false) (atoms/constant false))) (atoms/constant false)))
      (is (= (f (operations/conjunction (atoms/constant false) (atoms/constant true))) (atoms/constant false)))
      (is (= (f (operations/conjunction (atoms/constant true) (atoms/constant false))) (atoms/constant false)))
      (is (= (f (operations/conjunction (atoms/constant true) (atoms/constant true))) (atoms/constant true)))))

  (testing "(conj var_a false) => false | (conj false var_a) => false"
    (let [f #'translator/simplify
          foo (operations/conjunction (atoms/variable :a) (atoms/constant false))
          bar (operations/conjunction (atoms/constant false) (atoms/variable :a))
          baz (atoms/constant false)]

      (is (= (f foo) baz))
      (is (= (f bar) baz))))

  (testing "(conj var_a true) => var_a | (conj true var_a) => true"
    (let [f #'translator/simplify
          baz (atoms/variable :a)
          foo (operations/conjunction baz (atoms/constant true))
          bar (operations/conjunction (atoms/constant true) baz)]

      (is (= (f foo) baz))
      (is (= (f bar) baz))))

  (testing "(disj var_a var_a) => a"
    (let [f #'translator/simplify
          foo (operations/disjunction (atoms/variable :a) (atoms/variable :a))
          bar (atoms/variable :a)]

      (is (= (f foo) bar))))

  (testing "(false or false ) => false | (false or true) => true | (true or false) => true | (true or true) => true"
    (let [f #'translator/simplify]

      (is (= (f (operations/disjunction (atoms/constant false) (atoms/constant false))) (atoms/constant false)))
      (is (= (f (operations/disjunction (atoms/constant false) (atoms/constant true))) (atoms/constant true)))
      (is (= (f (operations/disjunction (atoms/constant true) (atoms/constant false))) (atoms/constant true)))
      (is (= (f (operations/disjunction (atoms/constant true) (atoms/constant true))) (atoms/constant true)))))

  (testing "(disj var_a true) => true | (disj true var_a) => true"
    (let [f #'translator/simplify
          foo (operations/disjunction (atoms/variable :a) (atoms/constant true))
          bar (operations/disjunction (atoms/constant true) (atoms/variable :a))
          baz (atoms/constant true)]

      (is (= (f foo) baz))
      (is (= (f bar) baz))))

  (testing "(disj var_a false) => var_a | (disj false var_a) => true"
    (let [f #'translator/simplify-runner
          baz (atoms/variable :a)
          foo (operations/disjunction baz (atoms/constant false))
          bar (operations/disjunction (atoms/constant false) baz)]

      (is (= (f foo) baz))
      (is (= (f bar) baz)))))

(deftest translate
  (testing "A or (B and (C or D)) => (A or (B and C) or (B and D))")
  (let [a (atoms/variable :a)
        b (atoms/variable :b)
        c (atoms/variable :c)
        d (atoms/variable :d)
        exp (operations/disjunction
             a
             (operations/conjunction
              b
              (operations/disjunction c d)))
        res (operations/disjunction
             a
             (operations/conjunction c b)
             (operations/conjunction d b))]

    (is (= res (translator/translate-simplify exp))))

  (testing "not (A or B) => not A and not B")
  (let [a (atoms/variable :a)
        b (atoms/variable :b)
        exp (operations/negation
             (operations/disjunction a b))
        res (operations/conjunction (operations/negation a) (operations/negation b))]

    (is (= res (translator/translate-simplify exp))))

  (testing "not ((not X or Y) or not (not Y or Z)) => (X and not Y) or (X and not Y and Z)")
  (let [x (atoms/variable :x)
        y (atoms/variable :y)
        z (atoms/variable :z)
        exp (operations/negation
             (operations/disjunction
              (operations/disjunction
               (operations/negation x)
               y)
               (operations/negation
                (operations/disjunction
                 (operations/negation y)
                 z))))
        res (operations/disjunction
             (operations/conjunction (operations/negation y) x)
             (operations/conjunction z x (operations/negation y)))]

    (is (= res (translator/translate-simplify exp)))))
