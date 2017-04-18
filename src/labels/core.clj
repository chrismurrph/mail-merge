(ns labels.core
  (:require [clojure.java.io :as io])
  (:import (cljpdf.text.pdf PdfWriter ColumnText)
           (cljpdf.text Document Phrase Element)))

(def a4-cm-width 21.0)
(def a4-cm-height 29.7)
(def cms-point 0.0352778)
(def points-height (/ a4-cm-height cms-point))

;; The page for label printing is defined in centimetres, yet this api requires points.
;; Also this api requires points to be cartesian (y is going up from 0), yet label printing
;; is y going down the page.
(defn cms->pts [[x-cm y-cm]]
  [(/ x-cm cms-point) (- points-height (/ y-cm cms-point))])

;; Print some text at a coordinate position, where coords are in cms
(defn print-at-cms [canvas]
  (fn [text [x y :as coord]]
    (let [[x-pts y-pts] (cms->pts coord)]
      (ColumnText/showTextAligned canvas, Element/ALIGN_LEFT, text, x-pts, y-pts, 0))))

(def page-top-margin 1.3)
(def page-side-margin 0.64)
(def label-width 6.4)
(def label-height 3.39)
(def vertical-pitch 3.39)
(def horizontal-pitch 6.65)
(def label-indent 0.4)
(def next-line-space 0.3)

;; Label printing is (in our case addresses) to positions in a grid.
;; Given [0 0], the top left label, and the number of lines that need to be printed,
;; this function will return a vector of [x y], one for each line.
;; So this function ensures that the address is vertically centered within the bounds of the label.
;; Well possibly - for now we will just put it in the top left corner: see label-indent
(defn print-at-grid-pos [canvas]
  (let [print-text (print-at-cms canvas)]
    (fn [text-lines [grid-x grid-y]]
      (assert (vector? text-lines))
      (let [
            ;; top left corner of label
            label-x (+ page-side-margin (* grid-x horizontal-pitch))
            label-y (+ page-top-margin (* grid-y vertical-pitch))
            x (+ label-indent label-x)
            y (+ label-indent label-y)
            coords (map (juxt (fn [n] n) (fn [_] x) (fn [n] (* y n))) (range 0 (count text-lines)))
            ]
        (doseq [coord coords]
          (println coord))))))

(defn x-1 []
  (let [os (io/output-stream "labels.pdf")
        doc (Document.)
        writer (PdfWriter/getInstance doc os)
        hello (Phrase. "hello doggy world")
        hello-again (Phrase. "hello again")]
    (.open doc)
    (let [canvas (.getDirectContentUnder writer)
          print-text (print-at-cms canvas)]
      (.setCompressionLevel writer 0)
      (print-text hello [1 1])
      (print-text hello-again [2 2])
      (.close doc))))

(defn x-2 []
  (cms->pts [1 1]))