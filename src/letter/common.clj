(ns letter.common
  (:require [common.utils :as u]))

(defn create-addr-rows [l r]
  (let [len (max (count l) (count r))]
    (for [n (range len)]
      [(get l n) "" (get r n)])))

(defn create-addrs [left-address right-address]
  (u/pp [left-address right-address])
  (into [:table {:header       [[:cell {:colspan 1 :align :left}] [:cell {:colspan 1 :align :center}] [:cell {:colspan 1 :align :right}]]
                 :border-width 0
                 :border       false
                 :cell-border  false
                 :spacing      -7}
         ] (create-addr-rows left-address right-address)))
