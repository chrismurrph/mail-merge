(ns letter.common
  (:require [common.utils :as u]
            [common.common :as cc]))

(defn create-addr-rows [l r]
  (let [len (max (count l) (count r))]
    (for [n (range len)]
      [(get l n) "" (get r n)])))

(defn create-addrs [left-address right-address]
  (u/pp [left-address right-address])
  (let [table (into [:table {:header       [[:cell {:colspan 1 :align :left}] [:cell {:colspan 1 :align :center}] [:cell {:colspan 1 :align :right}]]
                             :border-width 0
                             :border       false
                             :cell-border  false
                             :spacing      -5}
                     ] (create-addr-rows left-address right-address))]
    [:paragraph {:indent cc/indent} table]))
