(ns examples.grammar
  (:require [clojush :exclude '-main]
	    [clojure.string :as string])
  (:use [clojush :exclude '-main]))

(defn get-stack-instructions [instlist stack]
  (filter #(= (string/replace-first (str stack) #":" "")
	      (first (string/split (str %) #"_")))
	  (seq instlist)))

(defn get-type-instructions [instlist type]
  (filter #(= (str type)
	      (second (string/split (str %) #"_")))
	  (seq instlist)))

;; lists get pushed onto string stack
			       
(def language-1 '(("1" "11" "111" "1111" "11111" "111111" "1111111" "11111111" "1111111111")
		  ("0" "00" "10" "01" "011" "110" "0000" "11111110" "10111111")))
(def language-2 '(("10" "1010" "101010" "10101010" "10101010101010" "10101010101010101010")
		  ("1" "0" "11" "00" "01" "101" "100" "1001010" "10110" "110101010")))
(def language-3 '(("1" "0" "01" "11" "00" "100" "110" "111" "000" "100100" "110000011100001" "111101100010011100")
		  ("10" "101" "010" "1010" "1110" "1011" "10001" "111010" "1001000" "11111000" "0111001101" "11011100110")))
(def language-4 '(("1" "0" "10" "01" "00" "100100" "001111110100" "0100100100" "11100" "010")
		  ("000" "11000" "0001" "000000000" "11111000011" "1101010000010111" "1010010001" "0000" "00000")))
(def language-5 '(("11" "00" "1001" "0101" "1010" "1000111101" "1001100001111010" "111111" "0000")
		  ("0" "111" "011" "000000000" "1000" "01" "10" "1110010100" "010111111110" "0001" "011")))
(def language-6 '(("10" "01" "1100" "101010" "111" "000000" "10111" "0111101111" "100100100")
		  ("1" "0" "11" "00" "101" "011" "11001" "1111" "00000000" "010111" "10111101111" "1001001001")))
(def language-7 '(("1" "0" "10" "01" "11111" "000" "00110011" "0101" "0000100001111" "00100" "011111011111" "00")
		  ("1010" "00110011000" "0101010101" "1011010" "10101" "010100" "101001" "100100110101")))     

(defn grammar-fitness [examples output]
  (fn [program]
    (loop [err (transient [])
	   ex examples
	   ans output]
      (if (empty? ex)
	(reduce + (persistent! err))
	(recur (conj! err (if (= (top-item :boolean
					   (run-push program
						     (loop [tokens (reverse (map str (first ex)))
							    state (make-push-state)]
						       (if (empty? tokens)
							 state
							 (recur (rest tokens)
								(push-item (first tokens) :string state))))))
				 (first ans))
			    0 1))
	       (rest ex)
	       (rest ans))))))

(define-registered accept-reject
  (fn [state]
    (push-item (and (= (pop-item :boolean state) true)
		    (empty? (:string state)))
	       :boolean state)))

(define-registered is-one
  (fn [state]
    (push-item (= "1" (top-item :string state)) :boolean state)))

(define-registered is-zero
  (fn [state]
    (push-item (= "0" (top-item :string state)) :boolean state)))

(define-registered more-input?
  (fn [state]
    (push-item (empty? (:string state)) :boolean state)))

(defn -main [& args]
  (let [argmap (apply hash-map
		    (map read-string
			 (drop-while #(not= (first %) \:) args)))
	atom-generators '(accept-reject is-one is-zero more-input?
					string_eq string_dup string_pop
					exec_y exec_k exec_pop exec_if)
	error-fn (fn [program]
		   (list
		    (let [pos ((grammar-fitness (first (deref (ns-resolve 'examples.grammar (:language argmap))))
						(repeat true)) program)
			  neg ((grammar-fitness (second (deref (ns-resolve 'examples.grammar (:language argmap))))
						(repeat false)) program)]
		      (if (or (= pos 0) (= neg 0))
			100
			(+ pos neg)))))]
    (pushgp-map (-> argmap
		    (assoc :atom-generators (if (:use-tags argmap)
					      (conj atom-generators
						    (tagged-instruction-erc)
						    (tag-instruction-erc [:exec]))
					      (conj atom-generators 'exec_noop 'exec_noop)))
		    (assoc :error-function error-fn))))		    
  (System/exit 0))