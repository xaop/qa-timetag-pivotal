(ns timetag-pivotal.main
  (require
   [clojure.data.csv :as csv]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [timetag-pivotal.querying :as q]
   [timetag-pivotal.storysource :as ss]
   [timetag-pivotal.storysourcefactory :as ssf]))

(defn read-csv [r]
  "reads csv from reader"
  (with-open [reader r]
    (doall
     (csv/read-csv r))))


(defn header [csv-data]
  (first csv-data))

(defn body [csv-data]
  (rest csv-data))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (header csv-data)
            (map keyword) 
            repeat)
       (body csv-data)))


(def project-key (keyword "Project"))
(def username-key (keyword "User Name"))
(def useremail-key (keyword "User Email"))
(def category-key (keyword "Category"))
(def entry-key (keyword "Entry"))
(def date-key (keyword "Date"))
(def start-key (keyword "Start"))
(def end-key (keyword "End"))
(def duration-key (keyword "Duration (minutes)"))
(def total-key (keyword "Total per Project (minutes)"))

(defn entry-duration [entry]
  (-> entry
      (duration-key)
      (Float/parseFloat)
      (int)))

(defn entry-duration-divided-by-stories [entry]
  (let [duration (entry-duration entry)
        no-stories (count (:pivotal entry))]
    (if (= no-stories 0)
      duration
      (/ duration no-stories))))

(defn entry-get-stories [entry]
  (letfn [(process-match [match res]
            (if (empty? match)
              res
              (let [next-match (re-find #",?\s*(\d+)(\D.*)?" match)]
                (recur (last next-match) (conj res (second next-match))))))]
    (let [text (entry-key entry)
          matches (re-find #"\[(\s*(\d+)((,|\s)\s*(\d+))*)\s*\]" text)]
      (if matches
        (process-match (second matches) '())))))

(defn entries->project-map
  "takes as input a csv-map and returns a map where the keys are projects and the values are entries with a :stories key"
  [csv-map]
  (letfn [(process-project [project-entries]
            (map
             (fn [entry]
               (let [stories (entry-get-stories entry)]
                 (assoc entry :stories stories)))
             project-entries))]
    (let [grouped (group-by project-key csv-map)]
      (reduce
       (fn [res key]
         (assoc res key
                (process-project (grouped key))))
       {}
       (keys grouped)))))
        

(defn available-project-names [csv-map]
  (set
   (map project-key csv-map)))

(defn entry-group-time-worked
  ([entry-group]
   (entry-group-time-worked
    entry-group
    (fn [res entry]
      (+ res (entry-duration entry)))))
  ([entry-group adder]
   (reduce
    (fn [res key]
      (let [entries (entry-group key)]
        (assoc
         res key
         (reduce
          adder 0
          entries))))
    {}
    (keys entry-group))))

(defn pm-all-entries [pm]
  (flatten (vals pm)))
     

;;pivotal integration





(defn entries-process-story-entries [entries]
  (let [total-time (int (apply + (map entry-duration-divided-by-stories entries)))
        user-time (entry-group-time-worked
                   (group-by username-key entries)
                   (fn [res entry]
                     (+ res (entry-duration-divided-by-stories entry))))]
    {:total-time total-time
     :user-time user-time}))

(defn entries-group-by-story [entries]
  (reduce
   (fn [res entry]
     (let [stories (:stories entry)]
       (if stories
         (reduce
          (fn [m id]
            (update-in
             m
             [id]
             conj
             entry))
          res
          stories)
         (update-in res [:no-story] conj entry))))
   {}
   entries))


    

(defn pm-process-projects [pm]
  "takes as input a map with the keys the different projects
   and values the lists of different pivotalized entries of that project"
  (reduce
   (fn [res project]
     (let [entries (pm project)]
       (assoc res project (entries-group-by-story entries))))
   {}
   (keys pm)))





(defn process-results [processed storysource]
  (letfn [(process-result [m story-id]
            (let [entries (get m story-id)
                  val (entries-process-story-entries entries)
                  total (:total-time val)
                  users (:user-time val)]
              (ss/process-entries storysource entries story-id total users)))]
    (let [filtered-entries (ss/verify-project-map storysource processed)
          no-story (get processed :no-story)] ;;we are missing wrongly written pivotals
      {:story
       (map
        (fn [story-id]
          (process-result filtered-entries story-id))
        (keys filtered-entries))
       :no-story
       (list (process-result processed :no-story))})))
        
(def project-map
  {:kd4dm-august {:type :pivotal :name "UCB KD4DM Release 2 -August" :pivotal "2088138"}
   :kd4dm {:type :pivotal :name "KD4DM r1.0" :pivotal "2088138" }})

(defn process-projects [csv-map project-map]
  (letfn [(process-projects-intern [project]
            (let [project-settings (project-map project)
                  project-name (:name project-settings)
                  pivotal-id (:pivotal project-settings)
                  project-entries (filter (fn [entry] (= (project-key entry) project-name)) csv-map)
                  storysource (ssf/create-storysource project-settings)
                  pm (entries->project-map project-entries)
                  processed (pm-process-projects pm)
                  results
                  (first
                   (map
                    (fn [project]
                      (let [results (get processed project)]
                        (process-results results storysource)))
                    (keys processed)))
                  post-process (ss/post-process-results storysource results)]
              post-process))]
    (reduce
     (fn [res project-key]
       (assoc res project-key (process-projects-intern project-key)))
     {}
     (keys project-map))))


(defn generate-csv [writer results keys]
  (let [header (map name keys)
        result-keyed (map
                      #(select-keys % (vec keys))
                      results)]
    (csv/write-csv
     writer
     (apply
      vector
      (vec header)
      (map
       (fn [res]
         (vec (vals res)))
       result-keyed)))))


(defn generate-json-string [results]
  (json/write-str
   results
   :key-fn name))

(defn generate-json [results output]
  (with-open [writer output]
    (.write writer (generate-json-string results))))
          


(defn generate-csvs [results path]
  (let [projects (keys results)]
    (doall
     (map (fn [key]
            (with-open [writer (io/writer (str path "/" (name key)  ".csv"))]
              (generate-csv writer (get results key) '(:pivotal :total-time))))
          projects))))

  
(defn process-csv [input output]
  (let [csv-map (csv-data->maps (read-csv input))
        results (process-projects csv-map project-map)]
    (generate-json results output)
    results))
        

