(ns labels.core
  (:require [clojure.java.io :as io]
            [common.utils :as u]
            [labels.pages-spec :as p])
  (:import (cljpdf.text.pdf PdfWriter ColumnText PdfContentByte)
           (cljpdf.text Document Phrase Element)))

(def postfix "senators")

(def output-dir "output/labels")
(def a4-cm-width 21.0)
(def a4-cm-height 29.7)
(def cm->point 0.0352778)
(def points-height (/ a4-cm-height cm->point))

;; The page for label printing is defined in centimetres, yet this api requires points.
;; Also this api requires points to be cartesian (y is going up from 0), yet label printing
;; is y going down the page.
(defn cms->pts [[x-cm y-cm]]
  [(/ x-cm cm->point) (- points-height (/ y-cm cm->point))])

;; Print some text at a coordinate position, where coords are in cms
(defn print-at-cms [canvas]
  (fn [text [x y :as coord]]
    (let [[x-pts y-pts] (cms->pts coord)]
      (ColumnText/showTextAligned canvas, Element/ALIGN_LEFT, text, x-pts, y-pts, 0))))

;; These all in cms. Use to call print-at-cms
(def page-spec p/L7519)
(def label-indent 0.4)
(def next-line-space 0.45)

;; Label printing is (in our case addresses) to positions in a grid.
;; Given [0 0], the top left label, and the number of lines that need to be printed,
;; this function will return a vector of [x y], one for each line.
;; So this function ensures that the address is vertically centered within the bounds of the label.
;; Well possibly - for now we will just put it in the top left corner: see label-indent
(defn print-at-grid-pos [^PdfWriter writer]
  (let [canvas (.getDirectContentUnder writer)
        print-text! (print-at-cms canvas)
        {:keys [page-top-margin page-side-margin vertical-pitch horizontal-pitch]} page-spec]
    (fn [text-lines [grid-x grid-y]]
      (assert (vector? text-lines))
      (let [
            ;; top left corner of label
            label-x (+ page-side-margin (* grid-x horizontal-pitch))
            label-y (+ page-top-margin (* grid-y vertical-pitch))
            x (+ label-indent label-x)
            y (+ label-indent label-y)
            coords (map (juxt identity
                              (fn [_] (u/round3 x))
                              (fn [n] (u/round3 (+ y (* n next-line-space)))))
                        (range 0 (count text-lines)))]
        (doseq [[n x y] coords]
          ;(println "printing at" x y)
          (print-text! (nth text-lines n) [x y]))))))

(defn ->Phrase [^String text]
  (Phrase. text))

(defn print-one-label [printer lines]
  (let [phrases (mapv ->Phrase lines)]
    (printer phrases)))

(defn print-to-file [file-name]
  (let [os (io/output-stream (str output-dir "/" file-name))
        doc (Document.)
        ^PdfWriter writer (PdfWriter/getInstance doc os)
        _ (.open doc)
        printer (print-at-grid-pos writer)]
    (.setCompressionLevel writer 0)
    (fn [labels]
      (let [phrases (mapv ->Phrase (first labels))]
        (printer phrases [0 0])
        (.close doc)))))

(defn x-1 []
  (let [file-name "labels.pdf"
        texts ["hello doggy world 3" "hello doggy world 4"]]
    ((print-to-file file-name) [texts])))

(defn x-2 []
  (let [os (io/output-stream (str output-dir "/" "labels.pdf"))
        doc (Document.)
        writer (PdfWriter/getInstance doc os)
        _ (println (type writer))
        hello (Phrase. "hello doggy world")
        hello-again (Phrase. "hello again")]
    (.open doc)
    (let [canvas (.getDirectContentUnder writer)
          print-text (print-at-cms canvas)]
      (.setCompressionLevel writer 0)
      (print-text hello [1 1])
      (print-text hello-again [2 2])
      (.close doc))))

(defn x-3 []
  (cms->pts [1 1]))