(ns kelly.ts
  (:require
    [ts.csv :as csv]
    [clojure.pprint :refer [pprint]])
  (:import (java.time LocalDate)))


(defn parse-dollars
  [string]
  (int (* 100 (Double/parseDouble string))))

(defmacro defmemoized
  [fn-name args & body]
  `(def ~fn-name (memoize (fn ~fn-name ~args ~@body))))

(defmemoized parse-shiller-sp500 [{:keys [^Integer from-year ^Integer to-year]}]
  (->> (csv/parse
         "shiller-sp-500.csv"
         [{:id     :ts/date
           :column 0
           :parser #(LocalDate/parse %)}
          {:id     :ts/price
           :column 1
           :parser parse-dollars}
          {:id     :dividend
           :column 7
           :parser parse-dollars}])
       (filter (fn [ts]
                 (and (.isAfter (:ts/date ts) (LocalDate/of from-year 1 1))
                      (not (.isAfter (:ts/date ts) (LocalDate/of to-year 1 1))))))))

(defn period-to-period-price-deltas
  [ts {:keys [periods]}]
  (map (fn [left right]
         (double (/ (- (:ts/price right)
                       (:ts/price left))
                    (:ts/price left))))
       ts (drop periods ts)))

(defn average [nums]
  (double (/ (reduce + nums)
             (count nums))))



