(ns timetag-pivotal.storysourcefactory
  (:require
   [timetag-pivotal.pivotal :as pivo]
   [timetag-pivotal.trello :as trello]))

;;we are Java now bois!

(defmulti create-storysource (fn [entry] (keyword (:type entry))))
(defmethod create-storysource :pivotal [entry]
  (pivo/pivotal entry))
    
(defmethod create-storysource :trello [entry]
  (trello/trello entry))

