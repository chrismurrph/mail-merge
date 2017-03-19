(ns common.common)

(defn create-spaced-paragraph [text]
  [:paragraph
   [:chunk text]
   [:spacer]])

(defn create-close-paragraph [text]
  [:paragraph
   [:chunk text]])
