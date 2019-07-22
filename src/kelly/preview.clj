(ns kelly.preview
  (:require [oz.core :as oz]))


(defonce ^:dynamic *is-server-running* (atom false))


(defn preview [view]
  (time
    (with-precision 2
      (println "starting kelly" *is-server-running*)
      (when-not @*is-server-running*
        (oz/start-plot-server!)
        (reset! *is-server-running* true))

      (println "render-main: start")
      (oz/view! view)
      (println "render-main: end"))))
