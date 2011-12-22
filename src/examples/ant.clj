;; ant.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Lee Spector, lspector@hampshire.edu, 2010

(ns examples.ant
  (:require [clojure.contrib.string :as string])
  (:use [clojush :exclude [-main]]
	[clojure.contrib.math]
	[local-file]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artificial Ant problem from Koza, via Luke, with the Santa Fe and Los Altos
;; trails. Several calls to pushgp are included, commented out -- uncomment
;; one to run it. The code below is in 3 sections: 1) Push-independent code
;; to implement artificial ant environments; 2) Push/PushGP-specific ant code;
;; 3) code for actual runs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Push-independent artificial ant code

(defn read-grid
  "Reads an ant map file of the format used in Sean Luke's ECJ system's artificial
ant examples. The argument should be a path to the file. This returns a vector of
rows, each of which is a vector of numbers (one per column), with a 1 for food and
0 for anything else."
  [map-path]
  (let [lines (string/split-lines (slurp (file* map-path)))
        [rows columns] (read-string (format "[%s]" (nth lines 0)))]
    (vec (for [r (range 1 (inc rows))]
           (if (> r (dec (count lines))) 
             (vec (repeat columns 0))
             (vec (for [c (range columns)]
                    (if (>= c (count (nth lines r))) 
                      0 
                      (if (= \# (nth (nth lines r) c)) 1 0)))))))))
        
(defstruct ant-state :grid :eaten :row :column :orientation :steps :limit)

(def santafe-grid (read-grid "src/examples/santafe.trl"))
(def losaltos-grid (read-grid "src/examples/losaltos.trl"))

(defn new-santafe-state
  "Returns a new ant-state initialized for the santafe trail problem."
  []
  (let [g santafe-grid]
    (struct-map ant-state 
      :grid g 
      :eaten #{} 
      :row 0 
      :column 0 
      :max-row (count g)
      :max-column (count (first g))
      :orientation :east
      :steps 0
      :limit 600)))

(defn new-losaltos-state
  "Returns a new ant-state initialized for the santafe trail problem."
  []
  (let [g losaltos-grid]
    (struct-map ant-state 
      :grid g 
      :eaten #{} 
      :row 0 
      :column 0 
      :max-row (count g)
      :max-column (count (first g))
      :orientation :east
      :steps 0
      :limit 3000)))

(defn left-in
  "Returns a copy of the given ant-state with the ant having made a left turn."
  [ant-state]
  (if (< (:steps ant-state) (:limit ant-state))
    (-> ant-state
      (assoc :orientation (get {:east :north, :north :west, :west :south, :south :east}
                            (:orientation ant-state)))
      (assoc :steps (inc (:steps ant-state))))
    ant-state))

(defn right-in
  "Returns a copy of the given ant-state with the ant having made a right turn."
  [ant-state]
  (if (< (:steps ant-state) (:limit ant-state))
    (-> ant-state
      (assoc :orientation (get {:east :south, :south :west, :west :north, :north :east}
                            (:orientation ant-state)))
      (assoc :steps (inc (:steps ant-state))))
    ant-state))

(defn loc-ahead
  "Returns a [row column] vector for the location ahead of the ant in the given state."
  [ant-state]
  [(mod (case (:orientation ant-state)
          :south (inc (:row ant-state)) 
          :north (dec (:row ant-state))
          (:row ant-state))
     (:max-row ant-state))
   (mod (case (:orientation ant-state)
          :east (inc (:column ant-state))
          :west (dec (:column ant-state))
          (:column ant-state))
     (:max-column ant-state))])

(defn food-ahead-in
  "Returns true if there is uneaten food in front of the ant in the given state,
or false otherwise."
  [ant-state]
  (let [[row col] (loc-ahead ant-state)]
    (and (= 1 (nth (nth (:grid ant-state) row) col))
      (not (contains? (:eaten ant-state) [row col])))))

(defn move-in
  "Returns a copy of the given ant-state with the ant having moved one step forward."
  [ant-state]
  (if (< (:steps ant-state) (:limit ant-state))
    (let [[new-row new-column] (loc-ahead ant-state)]
      (-> ant-state
        (assoc :steps (inc (:steps ant-state)))
        (assoc :row new-row)
        (assoc :column new-column)
        (assoc :eaten (if (= 1 (nth (nth (:grid ant-state) new-row) new-column))
                        (conj (:eaten ant-state) [new-row new-column]) ;; set, won't add twice
                        (:eaten ant-state)))))
    ant-state))

;; a little test of the movement functions
#_(let [s (-> (new-santafe-state) 
          (move-in) 
          (move-in) 
          (move-in) 
          (move-in) ;; bad move
          (right-in)
          (right-in)
          (move-in)
          (left-in)
          (move-in))]
  (println (assoc s :grid nil))) ;; blank out the grid so it's easier to see the rest
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Push/PushGP-specific ant code

(define-registered left 
  (fn [state]
    (if (empty? (:auxiliary state))
      state
      (let [astate (stack-ref :auxiliary 0 state)]
	(->> state
	     (pop-item :auxiliary)
	     (push-item (left-in astate) :auxiliary))))))

(define-registered right 
  (fn [state]
    (if (empty? (:auxiliary state))
      state
      (let [astate (stack-ref :auxiliary 0 state)]
	(->> state
	     (pop-item :auxiliary)
	     (push-item (right-in astate) :auxiliary))))))

(define-registered move 
  (fn [state]
    (if (empty? (:auxiliary state))
      state
      (let [astate (stack-ref :auxiliary 0 state)]
	(->> state
	     (pop-item :auxiliary)
	     (push-item (move-in astate) :auxiliary))))))

;; a test of the movement instructions
#_(run-push '(1 1 integer_add move move move move right right move left move) 
  (push-item (new-santafe-state) :auxiliary (make-push-state)))

(define-registered if_food_ahead
  (fn [state]
    (if (or (empty? (:auxiliary state))
	    (empty? (rest (:exec state))))
      state
      (let [if-clause (first (:exec state))
            else-clause (second (:exec state))]
        (push-item (if (food-ahead-in (stack-ref :auxiliary 0 state))
                     if-clause
                     else-clause)
          :exec
          (pop-item :exec (pop-item :exec state)))))))

#_(defn run-ant-push-pgm-to-limit
  "Returns the ant-state. Aborts re-running if no action executed."
  [pgm ant-state]
  (loop [s ant-state]
    (if (>= (:steps s) (:limit s))
      s
      (let [result (stack-ref :auxiliary 0
                     (run-push pgm (push-item s :auxiliary (make-push-state))))]
        (if (= (:steps result) (:steps s)) ;; no action was executed
          s
          (recur result))))))

(defn run-ant-program
  "Returns the ant state after running the program."
  [p ant-state]
  (top-item :auxiliary
	    (run-push p (push-item ant-state :auxiliary (make-push-state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code for actual runs

;; Code to conduct a run on the santafe trail problem with just the ant-specific instructions.
;; Note that the santafe trail includes 89 pellets of food.
#_(pushgp
  :error-function (fn [program]
                    (doall
                      (list (- 89
                              (count (:eaten (run-ant-push-pgm-to-limit program 
                                               (new-santafe-state))))))))
  :atom-generators (list 'left 'right 'move 'if_food_ahead)
  :mutation-probability 0.3
  :crossover-probability 0.3
  :simplification-probability 0.3
  :reproduction-simplifications 10
  :max-points 100)

;; An evolved solution: ((right) (if_food_ahead move) (right right if_food_ahead move right) move)

;; Code to conduct a pushgp run on the santafe trail problem with additional instructions too
#_(pushgp
  :error-function (fn [program]
                    (doall
                      (list (- 89
                              (count (:eaten (run-ant-push-pgm-to-limit program 
                                               (new-santafe-state))))))))
  :atom-generators (concat (registered-for-type :integer :include-randoms false)
                     (registered-for-type :exec :include-randoms false)
                     (registered-for-type :boolean  :include-randoms false) 
                     '(left right move if_food_ahead))
  :mutation-probability 0.3
  :crossover-probability 0.3
  :simplification-probability 0.3
  :reproduction-simplifications 10
  :max-points 100)

;; An evolved solution: ((move exec_dup) ((((right (((if_food_ahead ((exec_stackdepth (exec_y exec_shove move exec_rot integer_pop))) integer_min)) (exec_do*times))))) (right)))

;; Code to conduct a pushgp run on the santafe trail problem with the ant-specific 
;; instructions but also with decimation.
#_(pushgp
  :error-function (fn [program]
                    (doall
                      (list (- 89
                              (count (:eaten (run-ant-push-pgm-to-limit program 
                                               (new-santafe-state))))))))
  :atom-generators '(left right move if_food_ahead)
  :mutation-probability 0.3
  :crossover-probability 0.3
  :simplification-probability 0.3
  :max-points 100
  :tournament-size 1
  :decimation-ratio 0.1
  :decimation-tournament-size 2)

;; An evolved solution: (if_food_ahead (if_food_ahead) (right right) (if_food_ahead move left move left))

;; Code to conduct a run on the losaltos trail problem with just the ant-specific 
;; instructions. Note that the losaltos trail includes 156 pellets of food. (Koza 
;; says 157 but Luke's file only includes 156!) More steps are allowed (which
;; is specified in the definition of new-losaltos-state above), and we also increase
;; the population size and the evalpush-limit.
#_(pushgp
  :error-function (fn [program]
                    (doall
                      (list (- 156
                              (count (:eaten (run-ant-push-pgm-to-limit program 
                                               (new-losaltos-state))))))))
  :atom-generators (list 'left 'right 'move 'if_food_ahead)
  :mutation-probability 0.3
  :crossover-probability 0.3
  :simplification-probability 0.3
  :max-points 100
  :population-size 5000
  :evalpush-limit 10000)

;; An evolved solution: ((move left) (if_food_ahead) if_food_ahead ((left) if_food_ahead if_food_ahead left) (if_food_ahead (right if_food_ahead) if_food_ahead) ((move (left if_food_ahead)) move (if_food_ahead) right) left)

(defn -main [& args]
  (let [argmap (apply hash-map
		      (map read-string
			   (drop-while #(not= (first %) \:) args)))
;	argmap (zipmap (map #(keyword (reduce str (drop 2 %))) (take-nth 2 args))
;		       (map read-string (take-nth 2 (drop 1 args))))
        atom-generators (concat
                         (when (:use-tagged argmap) (list (tagged-instruction-erc)))
			 (when (:use-tag argmap) (list (tag-instruction-erc [:exec])))
			 (when (:use-tagdo argmap) (list (tagdo-instruction-erc [:exec])))
			 (when (and (:use-padding argmap) (not (:use-tags argmap))) (repeat 2 'exec_noop))    
			 (list 'left 'right 'move 'if_food_ahead))
	trail-name (or (:trail-name argmap) "santafe"); or losaltos
	error-fn (if (= "santafe" trail-name)
		   (fn [program] (doall (list (- 89 (count (:eaten (run-ant-program program (new-santafe-state))))))))
		   (fn [program] (doall (list (- 156 (count (:eaten (run-ant-program program (new-losaltos-state)))))))))
	args (-> argmap
		 (assoc :mutation-probability (or (:mutation-probability argmap) 0.45))
		 (assoc :crossover-probability (or (:crossover-probability argmap) 0.45))
		 (assoc :simplification-probability (or (:simplification-probability argmap) 0))
                 (assoc :max-points (or (:max-points argmap) (if (= "santafe" trail-name) 890 1560)))
		 (assoc :evalpush-limit (or (:evalpush-limit argmap) (if (= "santafe" trail-name) 890 1560)))
		 (assoc :error-function error-fn)
		 (assoc :atom-generators atom-generators))]
    (println "trail-name =" trail-name)
    (pushgp-map args))
  (System/exit 0))
