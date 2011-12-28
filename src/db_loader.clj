(ns db_loader
  (:import [java.io File RandomAccessFile]
	   [java.text SimpleDateFormat FieldPosition]
	   [java.util Date])
  (:require [clojure.contrib.string :as s]
	    [clojure.contrib.duck-streams :as ds]
	    [clojure.contrib.seq-utils :as su]))

;; print to csv
;; need to synchronize across multiple users/accesses
;; add check for existence of data directory; create if not there
;; load config file - to take args from command line?
;; create ~/.db_config if it does not already exist
(def debug (atom true))
(def id (atom nil))
(def user (atom nil))
(def data_dir (atom nil))
(def problem_data (atom nil)) ;; hashmap of problem names to ids
(def experiments [:id :user :rundate :problem_name :problem_id :clojush_version])
(def experiment [:id :parameter :value])
(def generations [:id :gennum :parameter :value])
(def summary [:id :successp :maxgen])
(def separator (System/getProperty "file.separator"))

(defn from-symbol [symb]
  (deref (ns-resolve 'db_loader symb)))

(defn load-config [config-file-name]
  (let [config (if (.exists (File. config-file-name))
		 (read-string (slurp config-file-name))
		 {})]
    (reset! id (:id config 0))
    (reset! user (:user config (System/getProperty "user.name")))
    (reset! data_dir (:data_dir config (str (System/getProperty "user.home") separator "data_dir" separator)))
    (reset! problem_data (:problem_data config {:none -1}))))
	    
(defn save-config [config-file-name]
  (spit config-file-name
	{:id (inc @id)
	 :user @user
	 :data_dir @data_dir
	 :problem_data @problem_data}))
	 
(defn write-experiments [filename problemname experiment-data]
  (ds/with-out-append-writer
    (str @data_dir "experiments.csv")
    (println (str @id ",")
	     (str @user ",")
	     (str (.toString (.format (SimpleDateFormat. "EEE, d MMM yyyy HH:mm:ss Z")
				      (Date. (.lastModified (File. filename)))
				      (StringBuffer.)
				      (FieldPosition. 0))))
	     (str problemname)
	     (str (or (get @problem_data problemname)
		      (get (swap! problem_data assoc problemname (inc (apply max (vals @problem_data)))) problemname)))
	     (str (second (su/find-first #(= (first %) "Clojush version") experiment-data))))))

(defn write-experiment [filename experiment-data]
  (ds/with-out-append-writer
    (str @data_dir "experiment.csv")
    (doseq [[param value] (filter #(= 2 (count %)) experiment-data)]
      (println (str @id ",")
	       (str param ",")
	       (str value)))))
	     
(defn write-generations [generation-data]
  (let [generations (map (fn [generation]
			   (let [gen-num (re-find #"\d+$" (su/find-first #(re-find #"-\*-" %) generation))]
			     (list gen-num generation)))
			 generation-data)]
    (ds/with-out-append-writer
      (str @data_dir "generations.csv")
      (doseq [[gen-num gen-data] generations]
	(doseq [[param value] (filter #(= 2 (count %)) (map #(s/split #": " %) gen-data))]
	  (println (str @id ",")
		   (str gen-num ",")
		   (str param ",")
		   (str value)))))))

(defn write-summary [summary-data]
  (let [final-gen (su/find-first #(re-find #"[SUCCESS|FAILURE] at generation \d+" %) summary-data)]
    (ds/with-out-append-writer
      (str @data_dir "summary.csv")
      (println (str @id ",")
	       (str (boolean (re-find #"SUCCESS" final-gen)) ",")
	       (str (re-find #"\d+$" final-gen)) ;; not sure if the log with FAILURE has generation in the same line
	       ))))
					 
(defn rcons [thing lst]
  (reverse (cons thing (reverse lst))))

(defn seq-split [re some-seq]
  "Returns a list of lists, split and grouped by re. e.g. (\"---\" \"param1 = val1\" \"param2 = val2\" \"---\" \"param3 = val3\" etc.)
becomes ((\"param1 = val1\" \"param2 = val2\") (\"param3 = val3\")...)"
  (lazy-seq
   (loop [old-seq some-seq
	  new-seq ()
	  unit ()]
     (cond (empty? old-seq) (if (empty? unit) new-seq (rcons unit new-seq))
	   (re-find re (first old-seq)) (recur (rest old-seq) (if (empty? unit) new-seq (rcons unit new-seq)) ())
	   :else (recur (rest old-seq) new-seq (rcons (first old-seq) unit))))))
		    
(defn parse-log [filename problemname]
  (let [partition-data (seq-split #";+$" (ds/read-lines filename))
	experiment-data (map #(s/split #" = " %) (filter #(re-find #" = " %) (first partition-data)))
	generation-data (filter (fn [g] (su/find-first #(re-find #"-\*-" %) g))
				(rest (butlast partition-data)))
	]
    (when @debug
      (println (first experiment-data) (last experiment-data)
	       (first generation-data) (last generation-data))
      (println (str @data_dir "experiments"))
      (println (set (map count experiment-data)))
      )
    (write-experiments filename problemname experiment-data)
    (write-experiment filename experiment-data)
    (write-generations generation-data)
    (write-summary (last partition-data))
    ))
  
(defn -main [& args]
  (try
    (let [strargs (drop-while #(not= (first %) \:) (map str args))
	  argmap (zipmap (map read-string (take-nth 2 strargs))
			 (take-nth 2 (rest strargs)))]
      (loop [success false]
	(when-not success
	  (recur (try
		   (let [config-file-name (str (System/getProperty "user.home") separator ".db_config")
			 channel (.getChannel (RandomAccessFile. (File. config-file-name) "rw"))
			 lock (.lock channel)]
		     (load-config config-file-name)
		     (save-config config-file-name)
		     (.release lock)
		     (.close channel))
		   true
		   (catch Exception e false)))))
      (when (empty? argmap)
	(println "Please supply argmap with at least :filename")
	(System/exit 1))
      (doseq [csv '(experiments experiment generations summary)]
	(let [file (File. (str @data_dir csv ".csv"))]
	  (when (not (.exists file))
	    (ds/make-parents file)
	    (ds/spit file (reduce str (concat (butlast (interleave (map name (from-symbol csv)) (repeat ",")))
					      (list "\n")))))))
      (println argmap)
      (parse-log (:filename argmap) (:problemname argmap (re-find #"\/?(\w+_?)+_\d+" (:filename argmap)))))
    (catch Exception e
      (.printStackTrace e)
      (println "Note: file names must use the escape character for unix/mac separators"))))