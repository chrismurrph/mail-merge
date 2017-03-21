(ns common.common)

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

(defn create-spaced-paragraph [text]
  [:paragraph
   [:chunk text]
   [:spacer]])

(defn create-close-paragraph [text]
  [:paragraph
   [:chunk text]])
