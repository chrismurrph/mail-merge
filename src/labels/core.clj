(ns labels.core
  (:require [clojure.java.io :as io]
            [common.utils :as u]
            [labels.pages-spec :as p]
            [common.common :as cc])
  (:import (cljpdf.text.pdf PdfWriter ColumnText PdfContentByte)
           (cljpdf.text Document Element Chunk Phrase)))

(def postfix "senators")
(def addresses-file-name "mm/Senators.txt")

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

;; These all in cms. Use to call print-at-cms
(def page-spec p/L7519)
(def labels-per-page
  (let [{:keys [labels-across labels-down]} page-spec]
    (* labels-across labels-down)))
(def label-indent 0.4)
(def next-line-space 0.45)
(def max-line-width
  (let [{:keys [label-width]} page-spec]
    (/ (- label-width (* 2 label-indent)) cm->point)))
;(println "MAX" max-line-width)

;; Print some text at a coordinate position, where coords are in cms
(defn print-at-cms [canvas]
  (fn [^Chunk text [x y :as coord]]
    (let [[x-pts y-pts] (cms->pts coord)
          content (.getContent text)
          width (.getWidthPoint text)]
      ;; When too wide means s/have fixed earlier (when had hash)
      (assert (<= width max-line-width) (str "Content too wide: <" content ">"))
      (ColumnText/showTextAligned canvas, Element/ALIGN_LEFT, (Phrase. text), x-pts, y-pts, 0))))

;; Label printing is (in our case addresses) to positions in a grid.
;; Given [0 0], the top left label, and the number of lines that need to be printed,
;; this function will return a vector of [x y], one for each line.
;; So this function ensures that the address is vertically centered within the bounds of the label.
;; Well possibly - for now we will just put it in the top left corner: see label-indent
(defn print-at-grid-pos [^PdfWriter writer]
  (let [canvas (.getDirectContentUnder writer)
        print-text! (print-at-cms canvas)
        {:keys [page-top-margin page-side-margin vertical-pitch horizontal-pitch]} page-spec]
    (fn [[grid-x grid-y] text-lines]
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

(defn ->Chunk [^String text]
  (Chunk. text))

(defn create-positioned-labels [labels]
  (let [{:keys [labels-across labels-down]} page-spec]
    (assert (<= (count labels) labels-per-page))
    (let [page-positions (for [y (range labels-down)
                               x (range labels-across)]
                           [x y])]
      (map vector page-positions labels))))

(defn print-to-file [file-name]
  (let [os (io/output-stream file-name)
        doc (Document.)
        ^PdfWriter writer (PdfWriter/getInstance doc os)
        _ (.open doc)
        printer! (print-at-grid-pos writer)]
    (.setCompressionLevel writer 0)
    (fn [grid-pos-labels]
      (let [stream (mapcat (fn [{:keys [grid-pos label]}]
                             (let [chunks (mapv ->Chunk label)]
                               [[grid-pos chunks]])) grid-pos-labels)]
        (doseq [[pos chunk] stream]
          (printer! pos chunk))
        (.close doc)))))

(defn print-single-page-pdf! [file-name labels]
  (let [printer! (print-to-file file-name)
        texts (->> labels
                   create-positioned-labels
                   (mapv (fn [[pos label]] {:grid-pos pos :label label})))]
    ;(println texts)
    (printer! texts)))

(defn create-name-string [contact]
  (let [{:keys [title first-name second-name]} contact]
    (str title " " first-name " " second-name)))

(defn make-address-label [contact]
  (let [first-line (create-name-string contact)
        {:keys [street-address po-box-address]} contact
        address (if po-box-address po-box-address street-address)]
    (into [first-line] address)))

(defn all-adderss-labels []
  (let [get-contacts-fn (partial cc/get-contacts cc/make-address)
        labels (->> addresses-file-name
                    u/file-name->lines
                    get-contacts-fn
                    (map make-address-label))]
    labels))

;;
;; May create lots of files depending on how many labels-per-page there are
;;
(defn gen-label-files! [all-labels]
  (let [llabels (->> all-labels
                     (partition-all labels-per-page)
                     (map-indexed vector))]
    (doseq [[idx labels] llabels]
      ;(println labels)
      (print-single-page-pdf! (u/make-filename idx output-dir postfix "pdf") labels))))
