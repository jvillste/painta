(ns painta.command-line
  (:require [clojure.java.shell :as shell]))

(defn run-command [& args]
  (let [result (apply shell/sh args)]
    
    (when (or (not= 0 (:exit result))
              #_(not= "" (:err result)))
      (throw (ex-info (:err result) result)))

    (:out result)))
