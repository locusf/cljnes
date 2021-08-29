(ns cljnes.core
  (:require [bytegeist.bytegeist :as g]))


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
        bits (map #(bit-test ibyte %) [7 6 5 4 3 2 1 0])]
        (apply assoc {}
                      (interleave keyword-list bits))))
(defn wrap-header [header list]
(byte-to-flags ((:body header) (:flag header)) list))
(defmulti asBits :flag)
(defmethod asBits :flags6 [header]
  (wrap-header header [:d3 :d2 :d1 :d0 :fourscreen :trainer :battery :mirroring]))
(defmethod asBits :flags7 [header]
  (wrap-header header [:d7 :d6 :d5 :d4 :nes1 :nes2 :playchoice :unisystem]))
(defmethod asBits :flags8 [header]
  (wrap-header header [:p7 :p6 :p5 :p4 :p3 :p2 :p1 :p0]))
(defmethod asBits :flags9 [header]
  (wrap-header header [:r7 :r6 :r5 :r4 :r3 :r2 :r1 :tv]))
(defmethod asBits :flags10 [header]
  (wrap-header header [:bconflict :prgram :z1 :z2 :tv1 :tv0]))
(defmethod asBits :default [_] nil)
(defn asBody [flag body] {:body body :flag flag})
(def ines-spec
  (g/spec [:map
           [:header {:inline true} header-spec]
           [:TRAINER [:bytes {:length 512}]]]))

