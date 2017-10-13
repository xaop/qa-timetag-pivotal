(ns timetag-pivotal.pivotal
  (require
   [clj-http.client :as client]
   [clojure.data.json :as json]
   [timetag-pivotal.storysource :as ss]))

(def pivotal-base-url "https://www.pivotaltracker.com/services/v5/projects/") ;;"/" at the end is required
(def pivotal-token "457413bf8b11103b57fa029cd23218d7")

(declare verify-project-map-intern process-entries-intern)
(defrecord PivotalSource [api-token project-id base-url]
  ss/StorySource
  (verify-project-map [pivotal pm]
    (verify-project-map-intern pivotal pm))
  (process-entries [pivotal entries story-id total-time user-time]
    (process-entries-intern pivotal entries story-id total-time user-time))
  (post-process-results [pivotal results]
    results))



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

(defn pivotal-story-id [story]
  (str (story "id")))

(defn pivotal-get-project-story-ids [pivotal]
  (let [stories (pivotal-get-stories pivotal)]
    (map pivotal-story-id stories)))

(defn pivotal-add-comment [pivotal story-id text]
  (pivotal-generic-post
   pivotal
   (str "stories/" story-id "/comments")
   (json/write-str {:text text})))


(defn create-pivotal-comment [total-time users-time]
  (apply
   str "Total time: " total-time "\n\n"
   (map
    (fn [user]
      (let [time (users-time user)]
        (str user ": " time " minutes\n")))
    (keys users-time))))

(defn verify-project-map-intern [pivotal pm]
  (let [ids (map pivotal-story-id  (:stories pivotal))]
    (select-keys pm ids)))

(defn process-entries-intern [pivotal entries story-id total-time user-time]
  {:pivotal story-id
   :total-time total-time
   :total-hours (/ total-time 60)
   :user-time user-time})

(defn pivotal [entry]
  (let [pivo (PivotalSource. (or (:api-token entry) pivotal-token) (:pivotal entry) pivotal-base-url)]
    (assoc pivo :stories (pivotal-get-stories pivo))))
