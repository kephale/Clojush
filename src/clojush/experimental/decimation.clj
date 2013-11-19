(ns clojush.experimental.decimation
  (:use [clojush.random]))

(defn decimate
  "Returns the subset of the provided population remaining after sufficiently many
   elimination tournaments to reach the provided target-size."
  [population target-size tournament-size radius]
  (let [popsize (count population)]
    (if (<= popsize target-size)
      population
      (recur (let [tournament-index-set 
                   (let [first-location (lrand-int popsize)]
                     (cons first-location
                           (doall
                             (for [_ (range (dec tournament-size))]
                               (if (zero? radius)
                                 (lrand-int popsize)
                                 (mod (+ first-location (- (lrand-int (+ 1 (* radius 2))) radius))
                                      popsize))))))
                   victim-index
                   (reduce (fn [i1 i2] 
                             (if (> (:total-error (nth population i1))
                                    (:total-error (nth population i2)))
                               i1 
                               i2))
                           tournament-index-set)]
               (vec (concat (subvec population 0 victim-index)
                            (subvec population (inc victim-index)))))
             target-size tournament-size radius))))

(defn pareto-tournament
  "Return the index of an individual that lost a pareto tournament, nil if there is no loser."
  [population radius]
  (let [loc1 (lrand-int (count population))
        loc2 (if (zero? radius)
               (lrand-int (count population))
	       (mod (+ loc1 (- (lrand-int (+ 1 (* radius 2))) radius))
                    (count population)))
        errs1 (:errors (nth population loc1))
        errs2 (:errors (nth population loc2))
        loc1-dominates? (and (reduce #(and %1 %2) (map <= errs1 errs2))
	                     (reduce #(or %1 %2) (map < errs1 errs2)))
        loc2-dominates? (and (reduce #(and %1 %2) (map <= errs2 errs1))
                             (reduce #(or %1 %2) (map < errs2 errs1)))]
    (cond loc1-dominates? loc1
          loc2-dominates? loc2
          :else nil)))

(defn pareto-decimate
  "Returns the subset of the provided population remaining after sufficiently many
elimination tournaments to reach near provided target-size. This is approximate
because there is a possibility that even the entire population is non-dominated."
  [population target-size tournament-size radius]
  (loop [population (vec population)
         attempts 0]
    (if (or (<= (count population) target-size)
            (> attempts 5))
      population
      (let [victims (keep identity
                          (apply pcalls (repeat (- (count population) target-size)
                                                #(pareto-tournament population radius))))]
        (if (empty? victims)
          (recur population (inc attempts))
          (recur (vec (keep identity (apply assoc
                                            (cons population
                                                  (interleave victims
                                                              (repeat (count victims) nil)))))) 0))))))
