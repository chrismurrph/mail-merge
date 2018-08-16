(ns common.common
  (:require [clojure.java.io :as io]
            [common.utils :as u]
            [clojure.string :as s]
            [common.dev :as dev]))

(defn split-by-commas [addr]
  (assert addr (str "Must have address to split by commas"))
  (mapv s/trim (s/split addr #",")))

(defn get-contacts [make-address-fn address-lines]
  (->> address-lines
       (partition-by #(= "" %))
       (remove #(or (= ["" ""] %) (= [""] %)))
       (map make-address-fn)))

(defn make-address [[title first-name second-name position state-party street-addr po-box-addr :as in]]
  (assert street-addr (str "No street-addr for: <" in ">"))
  (let [res-1 {:title          title
               :first-name     first-name
               :second-name    second-name
               :state-party    state-party
               :street-address (split-by-commas street-addr)
               }
        res-2 (if (= "." position)
                res-1
                (assoc res-1 :position position))
        res-3 (if (= "." po-box-addr)
                res-2
                (do
                  (assert po-box-addr (str "No po-box-addr for: " first-name " " second-name))
                  (assoc res-2 :po-box-address (split-by-commas po-box-addr))))]
    res-3))

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

(def indent 5)
(def narrow-indent 35)

(defn create-spaced-paragraph-traditional
  ([text]
   (create-spaced-paragraph-traditional default-text->chunks text))
  ([text->chunks text]
   (conj (into [:paragraph {:indent indent}] (text->chunks text)) [:spacer])))

(defn create-spaced-paragraph-narrow
  ([text]
   (create-spaced-paragraph-narrow default-text->chunks text))
  ([text->chunks text]
   (conj (into [:paragraph {:indent-left  narrow-indent
                            :indent-right narrow-indent
                            :align  :justified}] (text->chunks text)) [:spacer])))

(def bigger 11)
(def smaller 9)

(defn heading-traditional [text]
  [:heading {:style {:size bigger} :indent indent} text])

(defn heading-narrow [text]
  [:heading {:style {:size bigger} :indent narrow-indent} text])

;;
;; x to be inserted at n in vector v
;;
(defn insert-at [n x v]
  (vec (concat (subvec v 0 n) (vector x) (subvec v n))))

;; TODO
;; Can't seem to get a font that is a particular size and underlined
;; I can do bold or underlined on their own, but both are too big

(defn insert-heading-narrow [text n]
  (fn [paragraphs]
    (assert (vector? paragraphs))
    (assert (<= n (count paragraphs)) (str "inserting at " n " when have only " (count paragraphs) " paragraphs"))
    (->> paragraphs
         (insert-at n (heading-narrow text)))))

(defn insert-page-break-heading-narrow [text n]
  (fn [paragraphs]
    (assert (vector? paragraphs))
    (->> paragraphs
         (insert-at n (heading-narrow text))
         (insert-at n [:pagebreak]))))

(defn insert-page-break-heading [text n]
  (fn [paragraphs]
    (assert (vector? paragraphs))
    (->> paragraphs
         (insert-at n (heading-traditional text))
         (insert-at n [:pagebreak]))))

(defn lines->paragraph-chunks [text-lines]
  (-> (for [line text-lines]
        [:chunk {:size smaller} (str line "\n")])
      vec
      (conj [:spacer])))

;;
;; So this one takes lines so we can limit each line's length.
;;
(defn insert-cation-paragraph-1 [caption-file-name {:keys [n indent align]}]
  (fn [paragraphs]
    (assert (vector? paragraphs))
    (let [text-lines (->> caption-file-name
                          u/file-name->lines)]
      (insert-at n (into [:paragraph
                          {:align   align
                           :indent  indent
                           :leading 14}]
                         (lines->paragraph-chunks text-lines))
                 paragraphs))))

(defn insert-cation-paragraph-2 [caption-file-name {:keys [n indent align]}]
  (fn [paragraphs]
    (assert (vector? paragraphs))
    (let [text (->> caption-file-name
                    u/file-name->lines
                    first)]
      (insert-at n [:paragraph
                    {:align        align
                     :indent-left  indent
                     :indent-right indent
                     :leading      14
                     :spacing-after 20}
                    [:chunk {:size smaller} text]]
                 paragraphs))))

(defn insert-image [image-file-name {:keys [n xscale yscale caption]}]
  (fn [paragraphs]
    (assert (vector? paragraphs))
    (insert-at n [:paragraph
                  {:align :center}
                  [:image {:xscale xscale
                           :yscale yscale}
                   (-> image-file-name io/resource)]
                  [:chunk {:size smaller} caption]
                  [:spacer]]
               paragraphs)))

(defn insert-many [many-of existing]
  (vec (concat many-of existing)))
