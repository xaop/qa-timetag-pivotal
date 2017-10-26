(ns timetag-pivotal.pivotal
  (require
   [clj-http.client :as client]
   [clojure.data.json :as json]
   [timetag-pivotal.storysource :as ss]))

(def ^:const +pivotal-base-url+ "https://www.pivotaltracker.com/services/v5/projects/") ;;"/" at the end is required
(def ^:const +pivotal-token+ "457413bf8b11103b57fa029cd23218d7")

(declare pivotal-get-stories pivotal-story-id)
(defrecord PivotalSource [api-token project-id base-url]
  ss/StorySource
  (get-stories [ss]
    (pivotal-get-stories ss))
  (get-story-id [ss story]
    (pivotal-story-id story)))


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


(defn pivotal [entry]
  (let [pivo (PivotalSource. (or (:api-token entry) +pivotal-token+) (:pivotal entry) +pivotal-base-url+)]
    (assoc pivo :stories (pivotal-get-stories pivo))))
