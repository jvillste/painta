(ns painta.ffmpeg
  (:require [painta.command-line :as command-line]
            (flow-gl.graphics [buffered-image :as buffered-image]))
  (:import [java.nio.file Files FileSystems]))

(defn extract-frame [source position]
  (let [temp-file-name (.getPath (FileSystems/getDefault)
                                 "out.png"
                                 (into-array String []))]
    (Files/deleteIfExists temp-file-name)
    (command-line/run-command "ffmpeg"
                              "-i"
                              source
                              "-ss"
                              position
                              "-vframes"
                              "1"
                              (.toString temp-file-name))
    (let [result (buffered-image/create-from-file (.toString temp-file-name))]
      (Files/deleteIfExists temp-file-name)
      result)))
