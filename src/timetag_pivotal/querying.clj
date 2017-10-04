(ns timetag-pivotal.querying)


(defn all-projects [results]
  (keys results))

(defn project-pivotal-entries [results project]
  (:pivotal (results project)))

(defn project-no-pivotal-entries [results project]
  (:no-pivotal (results project)))

(defn all-pivotal-entries [results]
  (map
   #(project-pivotal-entries results %)
   (all-projects results)))

(defn all-no-pivotal-entries [results]
   (mapcat
   #(project-no-pivotal-entries results %)
   (all-projects results)))

(defn all-entries [results]
  (concat
   (all-pivotal-entries results)
   (all-no-pivotal-entries results)))

(defn find-pivotal [results pivo-id]
  (first (filter #(= (:pivotal %) pivo-id) (all-entries results))))


