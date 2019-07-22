(ns kelly.sims.coin-toss
  (:require
    [kelly.random :as rnd]
    [kelly.preview :refer [preview]]
    [medley.core :refer [map-vals]]
    [ts.plot :as plot]))

(defn simulate-biased-coin
  [{:keys [edge minimum-bet starting-cash bet-size stop-at]}]
  (loop [history [starting-cash]]

    (let [remaining-cash (peek history)
          bet-amount (* bet-size remaining-cash)]

      (if (or (<= remaining-cash minimum-bet)
              (>= remaining-cash stop-at)
              (> (count history) 1000))

        history

        (let [amount-if-won (+ remaining-cash bet-amount)
              amount-if-loss (- remaining-cash bet-amount)]
          (recur
            (conj history
                  (rnd/toss-coin
                    :p (+ 0.5 edge)
                    :when-win amount-if-won
                    :when-loss amount-if-loss))))))))





(defn format-percentage [number]
  (format "%.2f%%" (* 100 number)))


(defn view-biased-coin-simulation
  [{:keys [edge starting-cash bet-size stop-at minimum-bet]}]
  (let [run-sim
        #(simulate-biased-coin
            {:edge edge
             :minimum-bet minimum-bet
             :starting-cash starting-cash
             :bet-size bet-size
             :stop-at stop-at})]
    [:div
     [:ul
      [:li "Edge: " (format-percentage edge)]
      [:li "Starting Cash: " starting-cash]
      [:li "Bet Size: " (format-percentage bet-size) " of remaining cash"]
      [:li "Stop At: " stop-at]]

     [:vega-lite (plot/values->time-series (run-sim) :x :step :y :cash)]

     [:vega-lite (plot/histogram (->> (repeatedly 1000 run-sim)
                                      (map #(-> % peek)))
                                 :title "Remaining Cash"
                                 :width 600
                                 :step (/ starting-cash 2))]]))




(defn render-main []
  (let [edge 0.05
        minimum-bet 100
        starting-cash 1e4
        stop-at 1e5]
    [:div
      [:h1 "Explorations on the Kelly Criterion"]

      [:p "The Kelly Criterion is an attempt to solve the question of what fraction of your total capital should be put at risk in order to maximize long term survival and capital growth."]

      [:h2 "Simplest Kelly"]
      [:p "Let us begin our exploration of the Kelly Criterion with the most basic formulation."]

      [:p "Imagine there is an (oddly generous) casino that offers a bet with a " (format-percentage (+ 0.5 edge)) " odds of doubling, meaning you have an edge of " (format-percentage edge) ". A positive edge means that you can make money in the long term thanks to the law of large numbers, but how can you use this edge to maximise your profits while avoiding ruin?"]

     [:p "Let's try to explore this problem empirically, by running monte carlo simulations. We will assume that the player starts with a fixed amount and will stop once he gets to the goal or goes bankrupt."

      [:h3 "Attempt #1: bet 50%"]
      [:p "Betting all your money seems risky already from the start, but lets run a simulation to see how well we can do."]
      (view-biased-coin-simulation {:edge edge
                                    :minimum-bet minimum-bet
                                    :starting-cash starting-cash
                                    :bet-size 0.5
                                    :stop-at stop-at})

      [:p "Not surprisingly it seems to always end up in ruin."]

      [:h3 "Attempt #2: bet 10%"]
      [:p "Let's try a more conservative approach now, but betting 10% instead."]
      (view-biased-coin-simulation {:edge edge
                                    :minimum-bet minimum-bet
                                    :starting-cash starting-cash
                                    :bet-size 0.10
                                    :stop-at stop-at})
      [:p "If you run this simulation a few times you will notice that its quite volatile. Sometimes we do end up at our goal but very often it ends up in ruin."]

      [:h3 "Attempt #3: bet 5%"]
      [:p "This option seems to consistently make decent profits."]
      (view-biased-coin-simulation {:edge edge
                                    :minimum-bet minimum-bet
                                    :starting-cash starting-cash
                                    :bet-size 0.05
                                    :stop-at stop-at})

      [:h3 "Attempt #4: bet 2.5%"]
      [:p "This option seems to consistently make decent profits."]
      (view-biased-coin-simulation {:edge edge
                                    :minimum-bet minimum-bet
                                    :starting-cash starting-cash
                                    :bet-size 0.025
                                    :stop-at stop-at})

      [:h3 "Attempt #5: bet 1%"]
      [:p "It doesn't end up in ruin, but it also never reaches our goal. Safe but unprofitable."]
      (view-biased-coin-simulation {:edge edge
                                    :minimum-bet minimum-bet
                                    :starting-cash starting-cash
                                    :bet-size 0.01
                                    :stop-at stop-at})

      [:h2 "What does Kelly Recommend?"]

      [:p "Kelly's formula produces the optimal wager at every bet and can be summarized by edge/odds. Applying this formula to our example we get edge=0.05"]]]))


(preview (render-main))