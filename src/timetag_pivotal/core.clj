(ns timetag-pivotal.core
  (:require
   [timetag-pivotal.main :as main]
   [clojure.tools.cli :refer [parse-opts]]
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

(defn output-writer [location]
  (if location
    (clojure.java.io/writer location)
    *out*))

  
(defn -main [& args]
  (let [opts (parse-opts args cli-options)
        options (:options opts)
        input (input-reader (:input options))
        output (output-writer (:output options))]
    (if (:help options)
      (:summary opts)
      (do
        (doall (main/process-csv input output))
        (System/exit 0)))))
