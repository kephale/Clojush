(ns examples.arithmetic-exp
  (:require [clojush]
	    [clojure.contrib.string :as string]
	    [clojure.set :as set])
  (:use [clojush]))

(def string-literals #{"(" ")" "+" "-"})

(defn eval-subexpr [lst]
;;  (println lst)
  (cond (string? lst) (try (read-string lst)
			   (eval-subexpr (read-string lst))
			   (catch Exception e (count (string/split #" " lst))))
	(and (symbol? lst) (not (contains? string-literals (str lst)))) 10
	(number? lst) 0
	(empty? lst) 5
	(reduce #(or %1 %2) (map seq? lst)) (reduce + (conj (map eval-subexpr (filter seq? lst))
							    (eval-subexpr (remove seq? lst))))
	:else (try (eval lst) 0 (catch Exception e 1))))

(define-registered make-int-str
  (fn [state]
    (let [i (stack-ref :integer 0 state)]
      (if (= i :no-stack-item)
	state
	(push-item (str i) :string (pop-item :integer state))))))

(define-registered make-float-str
  (fn [state]
    (let [i (stack-ref :float 0 state)]
      (if (= i :no-stack-item)
	state
	(push-item (str i) :string (pop-item :float state))))))

(define-registered group-left
  (fn [state]
    (push-item "(" :string state)))

(define-registered group-right
  (fn [state]
    (push-item ")" :string state)))

(define-registered space
  (fn [state]
    (push-item " " :string state)))

(define-registered add
  (fn [state]
    (push-item "+" :string state)))

(define-registered subtract
  (fn [state]
    (push-item "-" :string state)))

(defn arithmetic-err [program]
  (let [state (run-push program (make-push-state))
	expr (:string state)]
    (println expr (type expr) (boolean (seq expr)) (reduce str expr))
    (if (seq expr)
      (do
	 (+ (eval-subexpr (reduce str expr))
	    (if (= 0 (count (set/intersection string-literals
					      (set expr))))
	      5 0)))
      50)))

(pushgp :error-function (fn [program] (list (arithmetic-err program)))
	:evalpush-limit 100
	:population-size 200
	:max-generations 50)