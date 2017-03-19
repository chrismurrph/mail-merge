(ns cv.common
  (:require [clojure.java.io :as io]))

(defn image-here [image-file-name scale]
  (let [img (-> image-file-name io/resource)]
    [:image {:scale scale
             :align :center
             :valign :middle}
     img]))

(defn flattened-image-here [image-file-name scale]
  (let [img (-> image-file-name io/resource)]
    [:image {:xscale (/ scale 90)
             :yscale (/ scale 130)
             :align  :center
             :valign :middle}
     img]))
