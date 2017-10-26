(ns timetag-pivotal.trello
  (require
    [clj-http.client :as client]
    [clojure.data.json :as json]
    [timetag-pivotal.storysource :as ss]))


(def ^:const +trello-key+ "f114d0190e1aaf054c13a3bd536c8b07")
(def ^:const +trello-token+ "732082a2c387f260cc3f5fbf3605df3bc0347611f6fb3903168a8ce788ee9a66")
(def ^:const +trello-base-url+ "https://api.trello.com/1/boards/")


(declare trello-get-stories trello-story-id)
(defrecord TrelloSource [api-token api-key base-url board-id]
  ss/StorySource
  (get-stories [ss]
    (trello-get-stories ss))
  (get-story-id [ss story]
    (trello-story-id story)))
  
(defn trello-generic-get [trello request]
  (let [res
        (client/get
         (str (:base-url trello)
              (:board-id trello)
              "/"
              request)
         {:query-params {:key (:api-key trello) :token (:api-token trello)}})]
    (json/read-json (:body res))))

(defn trello-get-stories [trello]
  (trello-generic-get trello "cards"))

(defn trello-story-id [story]
  (str (story :idShort)))

(def test-trello
  {:api-token +trello-token+
   :api-key +trello-key+
   :board-id "LJ6MIM1n"})


(defn trello [entry]
  (let [trel (TrelloSource.
              (or (:api-token entry) +trello-token+)
              (or (:api-key entry) +trello-key+)
              (or (:trello entry) +trello-base-url+)
              (:board-id entry))]
    (assoc trel :stories (trello-get-stories trel))))
