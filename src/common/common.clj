(ns common.common
  (:require [clojure.java.io :as io]
            [common.utils :as u]))

;;
;; Going to turn the chunk that has the word in it into three chunks.
;; Find idx of chunk and also idx of character in the text of the chunk.
;; Are gonna return [:paragraph (update chunks idx f)],
;; where f
;;
;; [:chunk {:style :italic} "small chunk of text"]
(defn italicize [[kw & chunks :as paragraph] word]
  (assert (= kw :paragraph))
  paragraph)

(defn make-italicized-chunk [text]
  [:chunk {:style :italic} text])

(defn default-text->chunks [text]
  [[:chunk text]])

;; 51,102,187
(def anchor-attributes-1 {:style {:style :underline
                                  :color [51 102 187]}})
(def anchor-attributes-2 {:color [51 102 187]})
(def anchor-attributes anchor-attributes-1)

(defn anchor-text->chunk [link-fn]
  (fn [text]
    ;(println "anchor-text->chunks for" text)
    [:anchor (assoc anchor-attributes :target (link-fn text)) text]))

(defn create-spaced-paragraph
  ([text]
    (create-spaced-paragraph text default-text->chunks))
  ([text->chunks text]
   (conj (into [:paragraph] (text->chunks text)) [:spacer])))

(def bigger 11)

(defn insert-heading [text n]
  (fn [paragraphs]
    ;(println paragraphs)
    (assert (vector? paragraphs))
    (u/insert-at n [:heading {:style {:size bigger}} text]
                 paragraphs)))

(defn insert-image [image-file-name {:keys [n xscale yscale]}]
  (fn [paragraphs]
    ;(println paragraphs)
    (assert (vector? paragraphs))
    (u/insert-at n [:image {:xscale xscale
                            :yscale yscale
                            :align  :center}
                    (-> image-file-name io/resource)]
                 paragraphs)))

(defn create-close-paragraph [text]
  [:paragraph
   [:chunk text]])
