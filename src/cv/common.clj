(ns cv.common
  (:require [clojure.java.io :as io]))

(defn image-here
  ([image-file-name scale x y]
   (let [img (-> image-file-name io/resource)]
     [:chunk {:x x :y y} [:image {:scale  scale
                                   :align  :center
                                   :valign :middle}
                           img]]))
  ([image-file-name scale]
    (image-here image-file-name scale 0 -2))
  ([image-file-name scale x]
   (image-here image-file-name scale x -2)))

(defn flattened-image-here [image-file-name scale]
  (let [img (-> image-file-name io/resource)]
    [:image {:xscale (/ scale 90)
             :yscale (/ scale 90)
             :align  :center
             :valign :middle}
     img]))
