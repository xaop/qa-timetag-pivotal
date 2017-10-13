(ns timetag-pivotal.storysourcefactory
  (:require
   [timetag-pivotal.pivotal :as pivo]))

;;we are Java now bois!

(defmulti create-storysource (fn [entry] (:type entry)))
(defmethod create-storysource :pivotal [entry]
  (pivo/pivotal entry))
    

