(ns vigenere-cipher.core-test
  (:require [clojure.test :refer :all]
            [vigenere-cipher.core :refer :all]))

(deftest test-char-encode-decode
  (testing "Encode Decode Char"
    (is (= (encode-char (first alphabet)) 0)
        "First char encodes to 0")
    (is (= (decode-char 0) (first alphabet))
        "0 decodes to first char")
    (is (and (map #(= %1 (decode-char (encode-char %1))) alphabet))
        "Each character's encoding decodes to itself")))

(def ts "she sells sea shells on the sea shore")

(deftest test-string-encode-decode
  (testing "Encode Decode String"
    (let [test-en-de-code #(= (decode-string (encode-string %1))
                              %1)]
      (is (= (count (encode-string "")) 0)
          "Empty string has empty encoding")
      (is (test-en-de-code ""),
          "Encoded empty string decodes to itself")
      (is (test-en-de-code alphabet),
             "Encoded alphabet decodes to itself")
      (is (test-en-de-code ts)
          "Encoded test string decodes to itself"))))

(deftest test-encrypt-decrypt
  (testing "Encrypt Decrypt"
    (let [test-en-de-crypt (fn [s k] (= (decrypt (encrypt s k) k)
                                        s))]
      (is (test-en-de-crypt "" "a"),
          "Encrypted empty string decrypts to itself with a")
      (is (test-en-de-crypt ts "a"),
          "Encrypted ts string decrypts to itself with a")
      (is (test-en-de-crypt alphabet "a"),
          "Encrypted alphabet string decrypts to itself with a")
      (is (test-en-de-crypt "" ts),
          "Encrypted empty string decrypts to itself with ts")
      (is (test-en-de-crypt ts ts),
          "Encrypted ts string decrypts to itself with ts")
      (is (test-en-de-crypt alphabet ts),
          "Encrypted alphabet string decrypts to itself with ts")
      (is (test-en-de-crypt "" alphabet),
          "Encrypted empty string decrypts to itself with alphabet")
      (is (test-en-de-crypt alphabet alphabet),
          "Encrypted alphabet string decrypts to itself with alphabet")
      (is (test-en-de-crypt alphabet alphabet),
          "Encrypted alphabet string decrypts to itself with alphabet"))))
