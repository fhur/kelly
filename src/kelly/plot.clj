(ns kelly.plot
  (:require
    [medley.core :refer [map-vals]]
    [kelly.ts :as ts]))

(defn- get-schema
  [plot]
  (let [[_vega-light schema] plot]
    schema))

(defn- get-plot-values
  [plot]
  (-> (get-schema plot) :data :values))



(defn histogram
  [values & {:keys [width step title]
             :or {width 600
                  step 1
                  title "x"}}]

  [:vega-lite
   {:data {:values (map (fn [x] {:x x}) values)}
    :mark "bar"
    :title title
    :width width
    :encoding {:x {:field "x"
                   :title title
                   :type "quantitative"
                   :bin (if step {:step step}
                                 true)}
               :y {:type "quantitative"
                   :aggregate "count"}}}])

(defn join-histograms
  [histograms & {:keys [width step title]}]
  (let [field :title
        values
          (->> histograms
               (map (fn [plot]
                      (let [schema (get-schema plot)
                            vega-light-values (get-plot-values plot)
                            histogram-title (get schema field)]
                        (map #(assoc % field histogram-title) vega-light-values))))
             (apply concat))
        values-by-field
          (->> values
               (group-by field)
               (map-vals #(-> % :x ts/average)))]
    [:div
     [:div "Averages"
      [:ul
       (for [[field-name average] values-by-field]
         [:li field-name ": " average])]]
     [:vega-lite
      {:data {:values values}
       :mark "bar"
       :title title
       :width width
       :encoding
         {:row {:field (name field)
                :type "nominal"}
          :x {:field "x"
              :title title
              :type "quantitative"
              :bin (if step {:step step}
                            true)}
          :y {:type "quantitative"
              :aggregate "count"
              :stack false}
          :color {:type "nominal"
                  :field (name field)}
          :opacity {:value 0.5}}}]]))

(defn time-series
  [values & {:keys [width x y] :or {width 600}}]
  (assert (keyword? x))
  (assert (keyword? y))
  [:vega-lite
    {:width width
     :data {:values values}
     :mark {:type "line", :point true},
     :encoding {:x {:field (name x)
                    :type "quantitative"},
                :y {:field (name y)
                    :type "quantitative"}}}])


(defn values->time-series
  [values & {:keys [width x y]
             :or {width 600 x :step y :value}}]
  (-> (map-indexed
        (fn [index value]
         (hash-map x index y value))
        values)
      (time-series :x x :y y :width width)))


(defn format-% [number]
  (format "%.2f%%" (* 100 number)))
