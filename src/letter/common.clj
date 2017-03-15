(ns letter.common)

(defn create-spaced-paragraph [text]
  [:paragraph
   [:chunk text]
   [:spacer]])

(defn create-close-paragraph [text]
  [:paragraph
   [:chunk text]])

(defn create-table []
  [:table {:header [[:cell {:colspan 1 :align :left}] [:cell {:colspan 1 :align :center}] [:cell {:colspan 1 :align :right}]]
           :border-width 0
           :border false
           :cell-border false
           :spacing -7}
   ["foo" "" "bar"]
   ["foo1" "" "bar1"]
   ["foo2" "" "bar2"]])
