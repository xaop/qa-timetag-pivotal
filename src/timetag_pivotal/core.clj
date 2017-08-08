(ns timetag-pivotal.core
  (require
   [clojure.data.csv :as csv]
   [clj-http.client :as client]
   [clojure.data.json :as json]
   [clojure.java.io :as io]))

(defn read-csv [location]
  "reads csv from location"
  (with-open [reader (io/reader location)]
    (doall
     (csv/read-csv reader))))


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

(defn entry-get-pivotals [entry]
  (letfn [(process-match [match res]
            (if (empty? match)
              res
              (let [next-match (re-find #",?\s*(\d+)(\D.*)?" match)]
                (recur (last next-match) (conj res (second next-match))))))]
    (let [text (entry-key entry)
          matches (re-find #"\[(\s*(\d+)((,|\s)\s*(\d+))*)\s*\]" text)]
      (if matches
        (process-match (second matches) '())))))

(defn entries->pivotal-map
  "takes as input a csv-map and returns a map where the keys are projects and the values are entries with a :pivotal key"
  [csv-map]
  (letfn [(process-project [project-entries]
            (map
             (fn [entry]
               (let [pivotals (entry-get-pivotals entry)]
                 (assoc entry :pivotal pivotals)))
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
(defrecord Pivotal [api-token project-id base-url])
(def pivotal-base-url "https://www.pivotaltracker.com/services/v5/projects/") ;;"/" at the end is required
(defn pivotal [api-token project-id]
  (Pivotal. api-token project-id pivotal-base-url))


(defn pivotal-generic-get [pivotal request]
  (let [res
        (client/get
         (str (:base-url pivotal)
              (:project-id pivotal)
              "/"
              request)
         {:headers {:x-trackertoken (:api-token pivotal)}})]
    (json/read-str (:body res))))


(defn pivotal-generic-post [pivotal request data]
  (client/post
   (str (:base-url pivotal)
        (:project-id pivotal)
        "/"
        request)
   {:headers
    {:x-trackertoken (:api-token pivotal)
     :content-type "application/json"}
    :body data}))



(defn pivotal-get-stories [pivotal]
  (pivotal-generic-get pivotal "stories"))

(defn pivotal-get-project-story-ids [pivotal]
  (let [stories (pivotal-get-stories pivotal)]
    (map str (map (fn [story] (story "id")) stories))))

(defn pivotal-add-comment [pivotal story-id text]
  (pivotal-generic-post
   pivotal
   (str "stories/" test-story-id "/comments")
   (json/write-str {:text text})))





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
     (let [stories (:pivotal entry)]
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
         (update-in res [:no-pivotal] conj entry))))
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


(defn create-pivotal-comment [total-time users-time]
  (apply
   str "Total time: " total-time "\n\n"
   (map
    (fn [user]
      (let [time (users-time user)]
        (str user ": " time " minutes\n")))
    (keys users-time))))



(defn process-results [processed pivotal]
  (letfn [(process-result [m story-id]
            (let [entries (get m story-id)
                  val (entries-process-story-entries entries)
                  total (:total-time val)
                  users (:user-time val)]
              (json/write-str {:pivotal story-id
                               :total-time total
                               :user-time users})))]
    (let [existing-pivotals (pivotal-get-project-story-ids pivotal)
          filtered-proc (select-keys processed existing-pivotals)]
      (map
       (fn [story-id]
         (process-result filtered-proc story-id))
       (keys filtered-proc)))))
        
(def pivotal-token "457413bf8b11103b57fa029cd23218d7")
(def project-map
  {:kd4dm {:name "UCB KD4DM Release 2 -August" :pivotal "2088138"}})

(defn process-projects [csv-map project-map]
  (letfn [(process-projects [project]
            (let [project-settings (project-map project)
                  project-name (:name project-settings)
                  pivotal-id (:pivotal project-settings)
                  pivotal-token (or (:pivotal-token project-settings) pivotal-token)
                  project-entries (filter (fn [entry] (= (project-key entry) project-name)) csv-map)
                  pivotalized (entries->pivotal-map project-entries)
                  pivo (pivotal pivotal-token project-id)
                  processed (pm-process-projects pivotalized)]
              (reduce
               (fn [res project]
                 (let [results (get processed project)]
                   (process-results results pivo)))
               {}
               (keys processed))))]
    (reduce
     (fn [res project-key]
       (assoc res project-key (process-projects project-key)))
     {}
     project-map)))
