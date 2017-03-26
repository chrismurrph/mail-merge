(ns common.common
  (:require [clojure.java.io :as io]
            [common.utils :as u]
            [clojure.string :as s]))

(defn word-in-text->chunks [[{:keys [search-word op]} & tail] text]
  (if search-word
    (if-let [idx (s/index-of text search-word)]
      (let [before (subs text 0 idx)
            after (subs text (+ idx (count search-word)))]
        (concat (conj (word-in-text->chunks tail before) (op search-word)) (word-in-text->chunks tail after)))
      (word-in-text->chunks tail text))
    [[:chunk text]]))

(defn word->paragraph [{:keys [search-word op]} text]
  (if (= text search-word)
    [:paragraph (op search-word)]
    [:paragraph text]))

(defn make-italicized-chunk [text]
  [:chunk {:style :italic} text])

(defn default-text->chunks [text]
  [[:chunk text]])

(def anchor-attributes {:style {:style :underline
                                :color [51 102 187]}})

(defn anchor-text->anchor [link-fn]
  (fn [text]
    [:anchor (assoc anchor-attributes :target (link-fn text)) text]))

(defn create-spaced-paragraph
  ([text]
   (create-spaced-paragraph text default-text->chunks))
  ([text->chunks text]
   (conj (into [:paragraph] (text->chunks text)) [:spacer])))

(def bigger 11)

(defn insert-heading [text n]
  (fn [paragraphs]
    (assert (vector? paragraphs))
    ;; TODO
    ;; Can't seem to get a font that is a particular size and underlined
    ;; I can do bold or underlined on their own, but both are too big
    (u/insert-at n [:heading {:style {:size bigger}} text]
                 paragraphs)))

(defn insert-image [image-file-name {:keys [n xscale yscale]}]
  (fn [paragraphs]
    (assert (vector? paragraphs))
    (u/insert-at n [:image {:xscale xscale
                            :yscale yscale
                            :align  :center}
                    (-> image-file-name io/resource)]
                 paragraphs)))
