(ns cljnes.core
  (:require [bytegeist.bytegeist :as g]
            [clojure.core.specs.alpha :as s]))


(def header-spec
  (g/spec
   [:map
    [:NES [:string {:length 4}]]
    [:PRG :byte]
    [:CHR :byte]
    [:flags6 :byte]
    [:flags7 :byte]
    [:flags8 :byte]
    [:flags9 :byte]
    [:flags10 :byte]
    [:filler [:bytes {:length 3}]]]))

(defn byte-to-flags
  [ibyte kws]
  (let [keyword-list (map keyword kws)
        bits (map #(bit-test ibyte %) [0 1 2 3 4 5 6 7])
        retmap (apply assoc {}
                      (interleave keyword-list bits))]
    retmap))


(def ines-spec
  (g/spec [:map
           [:header {:inline true} header-spec]
           [:TRAINER [:bytes {:length 512}]]]))

