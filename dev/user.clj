(ns user
  (:require [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [clojure.stacktrace :refer [print-stack-trace]]))

;;
;;
(defn reset []
  ;(use 'clojure.stacktrace)
  (refresh))


;;
;;
(defn stop-reset []
  (reset))
