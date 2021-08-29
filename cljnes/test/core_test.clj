(ns cljnes.core-test
  (:require [clojure.test :refer [testing deftest is]]
            [cljnes.core :refer [header-spec ines-spec byte-to-flags]]
            [clojure.java.io :refer [input-stream copy]]
            [bytegeist.bytegeist :as g])

  (:import (java.io ByteArrayOutputStream)
           (io.netty.buffer Unpooled)))
(def fullBuf (with-open [xin (input-stream "test.nes")
                         xout (ByteArrayOutputStream.)]
               (copy xin xout)
               (lazy-seq (byte-array (.toByteArray xout)))))
(def ines-header
  (g/read header-spec (Unpooled/wrappedBuffer (byte-array (take 16 fullBuf)))))

(def ines-body
  (g/read ines-spec (Unpooled/wrappedBuffer (byte-array (doall fullBuf)))))

(deftest rom-file-header
  (testing "ROM file header"
    (is (= "NES\u001a" (ines-header :NES)) "reads a rom file header, first string NES")
    (is (= 2 (ines-header :PRG)) "reads the size of the PRG")
    (is (= 1 (ines-header :CHR)) "reads the CHRROM size")
    (is (= "110001" (Integer/toBinaryString (ines-header :flags6))) "has a value for flags6")))


(def flags6
  (byte-to-flags (ines-header :flags6) [:mirroring :battery :trainer :fourscreen :d0 :d1 :d2 :d3]))

(deftest rom-header-flags
  (testing "ROM header flags")
  (is (= true (flags6 :mirroring))))

(deftest rom-body-reader
  (testing "ROM PRGROM reader")
  (is (= "NES\u001a" (ines-body :NES)))
  (is (seq (ines-body :TRAINER))))
