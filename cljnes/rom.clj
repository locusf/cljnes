(ns cljnes.rom
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
  (wrap-header header [:pram7 :pram6 :pram5 :pram4 :pram3 :pram2 :pram1 :p0]))
(defmethod asBits :flags9 [header]
  (wrap-header header [:r7 :r6 :r5 :r4 :r3 :r2 :r1 :tv]))
(defmethod asBits :flags10 [header]
  (wrap-header header [:bconflict :prgram :z1 :z2 :tv1 :tv0]))
(defmethod asBits :default [_] nil)
(defn asBody [flag body] {:body body :flag flag})
(defn select-values [map ks]
  (reduce #(conj %1 (map %2)) [] ks))
(defn mapper-bits [rom] (select-values rom [:d0 :d1 :d2 :d3 :d4 :d5 :d6 :d7]))

(defmulti mapper
  (fn [body]
    (let [mapper-indexes (doall (keep-indexed (fn [i v] (if (true? v) i nil)) (mapper-bits body)))]
    (reduce + (map #(bit-set 0 %) mapper-indexes))
    )))
  (defmethod mapper 1
    [_] "MAPPER 1")
  (defmethod mapper 2
    [_] "MAPPER 2")
  (defmethod mapper 3
    [_] "MAPPER 3")


(def ines-spec
  (g/spec [:map
           [:header {:inline true} header-spec]
           [:TRAINER [:bytes {:length 512}]]]))
