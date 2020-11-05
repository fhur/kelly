(ns kelly.sims.sp500
  (:require
    [kelly.random :as rnd]
    [kelly.ts.plot :as plot]
    [kelly.ts.finance :refer [future-value]]
    [kelly.random :refer [random-float]]
    [kelly.ts :as ts]
    [kelly.preview :refer [preview]]
    [medley.core :refer [map-vals]]))

(defn repeatedly-fast
  [^Integer n func]
  (loop [^Integer n n
         result (transient [])]
    (if (<= n 0)
      (persistent! result)
      (recur
        (dec n)
        (conj! result (func))))))


(defn add-black-swans
  "Reads a number of tail events from `numbers` and based on that tail sample,
  it will return a new distribution with additional tail events as specified by
  `num-tail-events-to-add`."
  [num-tail-events-sample num-tail-events-to-add numbers]

  (let [sorted-numbers (sort numbers)
        tail (take num-tail-events-sample sorted-numbers)
        tail-generator! #(random-float (first tail) (last tail))]

    (->> (repeatedly-fast num-tail-events-to-add tail-generator!)
         (into numbers))))


(defn plot-sp500-histogram
  [{:keys [period-to-period-length
           from-year
           to-year]}]
  (let [ts (ts/parse-shiller-sp500 {:from-year from-year :to-year to-year})
        percentages (->> (ts/period-to-period-price-deltas ts {:periods period-to-period-length})
                         (add-black-swans 5 5)
                         (map #(* 100 %)))
        expected-value (ts/average percentages)]
    [:div
     [:vega-lite (plot/histogram percentages
                   :width 600
                   :step 1
                   :title (str "SP500 " period-to-period-length "-month returns from " from-year " - " to-year))]
     [:p "Expected Value: " expected-value "%"]]))


(defn run-sp500-kelly-simulation
  [{:keys [^Double fraction history-store initial-cash]}]

  (let [
        create-initial-history (:initial history-store)
        history-store-appender (:appender history-store)
        history-store-current-value (:current-value history-store)
        period-to-period-length 1
        years-to-invest 20
        transaction-cost 5
        total-periods-to-invest (* (/ 12 period-to-period-length) years-to-invest)
        ts (ts/parse-shiller-sp500 {:from-year 1990 :to-year 2007})
        deltas (->> (ts/period-to-period-price-deltas ts {:periods period-to-period-length})
                    (add-black-swans 2 5))]


    (loop [history (create-initial-history initial-cash)
           step 0]
      (if (or (>= step total-periods-to-invest)
              (<= (history-store-current-value history)
                  (* 0.7 initial-cash)))
        history

        (let [current-cash (double (history-store-current-value history))
              delta (double (rand-nth deltas))
              cash-to-invest-pre (* fraction current-cash)
              cash-to-invest-post (* (+ 1 delta) cash-to-invest-pre)
              static-cash (- current-cash cash-to-invest-pre transaction-cost)
              resulting-cash (+ static-cash (* cash-to-invest-post))]

          (recur (history-store-appender history (double resulting-cash))
                 (inc step)))))))

(defn probability-less-than-value
  [value points]
  (float (/ (->> points
                 (filter #(<= % value))
                 (count))
            (count points))))

(defn probability-greater-than-value
  [value points]
  (float (/ (->> points
                 (filter #(>= % value))
                 (count))
            (count points))))




(defn run-find-optimal-fraction []
  (->> (range 0.05 1 0.10)
       (pmap (fn [fraction]
               (println "Processing:" fraction)
               (let [initial-cash 10000
                     run-sim #(run-sp500-kelly-simulation
                                {:fraction fraction
                                 :history-store (rnd/create-single-value-history-store)
                                 :initial-cash initial-cash})
                     num-iterations 1000
                     resulting-cashes (repeatedly-fast num-iterations run-sim)]
                 (println "Done with: " fraction)
                 {:probability-of-market-returns
                    (probability-greater-than-value
                      (future-value {:present-value initial-cash
                                     :interest-rate 0.04
                                     :periods 20})
                      resulting-cashes)
                  :probability-of-worse-than-initial-cash
                    (probability-less-than-value
                      initial-cash
                      resulting-cashes)
                  :probability-of-worse-than-t-bill
                    (probability-less-than-value
                      (future-value {:present-value initial-cash
                                     :interest-rate 0.025
                                     :periods 20})
                      resulting-cashes)
                  :fraction fraction})))
       (into [])))

(defn plot-sp500-kelly-simulation
  [{:keys [fraction]}]
  (let [initial-cash 10000
        run-sim #(run-sp500-kelly-simulation
                   {:fraction fraction
                    :history-store %
                    :initial-cash initial-cash})
        points-in-histogram 1000
        resulting-cashes (->> #(run-sim (rnd/create-single-value-history-store))
                             (repeatedly-fast points-in-histogram)
                             (vec))
        t-bill-returns (future-value {:present-value initial-cash
                                      :interest-rate 0.025
                                      :periods 20})]
    [:div
     [:h2 "Fraction: " (plot/format-% fraction)]
     [:vega-lite (plot/values->time-series
                   (run-sim (rnd/create-vector-history-store))
                   :x :step
                   :y :cash)]

     [:p "Distribution of Returns"]
     [:vega-lite (plot/histogram
                   resulting-cashes
                   :step 2000
                   :title "Returns")]

     [:p "Probability of doing worse than the initial investment amount"]]
    (let [optimal-fraction-results (run-find-optimal-fraction)]
      [:div
       [:vega-lite
        (plot/time-series
          optimal-fraction-results
          :x :fraction
          :y :probability-of-market-returns)]
       [:vega-lite
        (plot/time-series
          optimal-fraction-results
          :x :fraction
          :y :probability-of-worse-than-initial-cash)]
       [:vega-lite
        (plot/time-series
          optimal-fraction-results
          :x :fraction
          :y :probability-of-worse-than-t-bill)]])))







(defn render-main []
  (let [edge 0.03]
    [:div
     [:h1 "Explorations on the Kelly Criterion: SP500"]

     [:p "Let's continue now our exploration of the Kelly Criterion with a more practical application: the returns of the SP500"]
     [:p "In order to create a proper simulation we will need to simulate some sort of bet "]

     (let [period-to-period-length 1
           initial-year 1965]
       [:div
         [:h2 "Estimating an Edge"]
         [:p "Let's first examine the distribution of the SP500's " period-to-period-length "-month returns, by looking at three distinct periods."]
         (plot-sp500-histogram {:from-year 1990
                                :to-year 2007
                                :period-to-period-length period-to-period-length})
         #_(plot-sp500-histogram {:from-year (+ initial-year 10)
                                   :to-year (+ initial-year 30)
                                   :period-to-period-length period-to-period-length})
         #_(plot-sp500-histogram {:from-year (+ initial-year 20)
                                    :to-year (+ initial-year 40)
                                    :period-to-period-length period-to-period-length})

         [:p "From this limited dataset we can see that there does seem to be an edge in the markets, which is related to the whole idea of index investing being the 'best' form of investment. We can also see that the edge seems to be consistently positive."]

         [:h2 "The Experiment"]
         [:p "Let's setup our experiment as follows:"]
         [:ul
          [:li "Every round we take a random " period-to-period-length "-month return from the distribution. "]
          [:li "If the return is positive we 'sell' our position and buy again, thus increasing our exposure by re-balancing"]
          [:li "If the return is negative we 'sell' our position and buy again, thus decreasing our exposure by re-balancing"]

          (plot-sp500-kelly-simulation {:fraction 0.6})]])]))



(let [result (time (render-main))]
  (println "Sending off to preview...")
  (preview result))