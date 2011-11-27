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
  (let [state (run-push program (push-item true :boolean (make-push-state)))
	expr (:string state)
	input1 "(+ 3 4 (- 1 2) (- 90 34))"
	input2 "(+ (- 3 4) 10 (- 1 2) (- 90 34))"
	input3 "(+ - 3)"
	input4  "(10)"]
    (if (seq expr)
      (do
	(+ (eval-subexpr (reduce str expr))
	   (- (count string-literals)
	      (count (set/intersection string-literals (set expr))))
	   (reduce +
		   (for [input (list [input1 62] [input2 64] [input3 :no-stack-item] [input4 :no-stack-item])]
		     (if (= (top-item :integer
				      (run-push program
						(push-item (input 0) :string
							   (push-item false :boolean (make-push-state)))))
			    (input 1))
		       0 7)))))
      50)))

(pushgp :error-function (fn [program] (map arithmetic-err (repeat 10 program)))
;; 	:atom-generators (conj (seq @registered-instructions)
;; 			       (tag-instruction-erc [:exec])
	;; 			       (tagged-instruction-erc))
	:final-report-simplifications 0
	:report-simplifications 0
	:simplification-probability 0
	:reproduction-simplifications 0
	:evalpush-limit 500
	:max-points 150
	:population-size 1000
	:max-generations 1001)