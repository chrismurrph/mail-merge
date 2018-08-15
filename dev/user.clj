(ns user
  (:require [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [clojure.stacktrace]))

;;
;;
(defn reset []
  ;(use 'clojure.stacktrace)
  (refresh))


;;
;;
(defn stop-reset []
  (reset))
