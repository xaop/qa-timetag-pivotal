(ns timetag-pivotal.core
  (:require
   [timetag-pivotal.main :as main]
   [clojure.tools.cli :refer [parse-opts])
   (:gen-class)))


(def cli-options
  [["-h" "--help"]
   ["-i" "--input CSV" "Timetag CSV"
    :id :input]
   ["-o" "--out FILE" "Output"
    :id :output]])



(defn input-reader [location]
  (if location
    (clojure.java.io/reader location)
    (java.io.BufferedReader. *in*)))

(defn output-reader [location]
  (if location
    (clojure.java.io.writer location)
    *out*))

  
(defn -main [& args]
  (let [opts (parse-opts args cli-options)
        input (input-reader (:input opts))
        output (output-writer (:output opts))
        results (doall (main/process-input input))]
    results))
