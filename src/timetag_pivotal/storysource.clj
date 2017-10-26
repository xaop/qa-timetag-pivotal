(ns timetag-pivotal.storysource)

(defprotocol StorySource
  ;;  (verify-project-map [source pm] "verifies that the entry exists in source")
  ;; (process-entries [source entries story-id total-time user-time] "processes all entries of a single id")
  ;;  (post-process-results [source results] )
  (get-stories [ss] )
  (get-story-id [ss story] )
  )

(defn get-story-ids [ss]
  (map #(get-story-id ss %) (get-stories ss)))


(defmulti process-entries (fn [ss entries story-id total-time user-time] (type ss)))

(defmethod process-entries :default [ss entries story-id total-time user-time]
  {:story story-id
   :total-time total-time
   :total-hours (/ total-time 60)
   :user-time user-time})

(defmulti verify-project-map (fn [ss pm] (class ss)))

(defmethod verify-project-map :default [ss pm]
  (select-keys pm (get-story-ids ss)))


(defmulti post-process-results (fn [ss results] (type ss)))

(defmethod post-process-results :default [ss results]
  results)
