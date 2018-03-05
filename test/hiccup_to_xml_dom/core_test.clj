(ns hiccup-to-xml-dom.core-test
  (:require [clojure.test :refer :all]
            [hiccup-to-xml-dom.core :refer :all]
            [clojure.data :refer [diff]]
            [clojure.xml :refer [parse]])
  (:import [java.io File FileOutputStream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using FOP documents as an example of valid XML to generate.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest dom-test
  (testing "can create a single xml dom node with no attributes & no content"
    (is (= (serialize-dom (->dom [:fo:box] :document-namespace "http://www.w3.org/1999/XSL/Format"))
           "<fo:box xmlns:fo=\"http://www.w3.org/1999/XSL/Format\"/>")))

  (testing "can create a single xml dom node with no attributes"
    (is (= (serialize-dom (->dom [:fo:box "garland texas"] :document-namespace "http://www.w3.org/1999/XSL/Format"))
           "<fo:box xmlns:fo=\"http://www.w3.org/1999/XSL/Format\">garland texas</fo:box>")))

  (testing "can create a single xml dom node"
    (is (= (serialize-dom (->dom [:fo:box {:keep-together.within-page "always"} "garland texas"]
                                 :document-namespace "http://www.w3.org/1999/XSL/Format"))
           "<fo:box xmlns:fo=\"http://www.w3.org/1999/XSL/Format\" keep-together.within-page=\"always\">garland texas</fo:box>")))

  (testing "can create nested dom node with no attributes"
    (is (= (serialize-dom (->dom [:fo:root [:fo:layout-master-set "f"]]
                                 :document-namespace "http://www.w3.org/1999/XSL/Format"))
           "<fo:root xmlns:fo=\"http://www.w3.org/1999/XSL/Format\"><fo:layout-master-set>f</fo:layout-master-set></fo:root>")))

  (testing "can create nested dom node with attributes on child"
    (is (= (serialize-dom
            (->dom [:fo:root [:fo:layout-master-set {:d "dork"} "f"]]
                   :document-namespace "http://www.w3.org/1999/XSL/Format"))
           "<fo:root xmlns:fo=\"http://www.w3.org/1999/XSL/Format\"><fo:layout-master-set d=\"dork\">f</fo:layout-master-set></fo:root>")))

  (testing "can create nested dom node with attributes on parent"
    (is (= (serialize-dom
            (->dom [:fo:root {:d "dork"} [:fo:layout-master-set "f"]]
                   :document-namespace "http://www.w3.org/1999/XSL/Format"))
           "<fo:root xmlns:fo=\"http://www.w3.org/1999/XSL/Format\" d=\"dork\"><fo:layout-master-set>f</fo:layout-master-set></fo:root>")))
  (testing "can create nested dom node with no namespace"
    (is (= (serialize-dom
            (->dom [:Employees
                    [:Employee
                     [:FirstName "John"]
                     [:MiddleNames "James"]
                     [:LastName "Smith"]
                     [:DateOfBirth "1980-01-01"]
                     [:HomeAddress
                      [:StreetAddress "12 Main Street"]
                      [:City "Perth"]
                      [:State "LA"]
                      [:Zip "70805"]]]]))
           "<Employees><Employee><FirstName>John</FirstName><MiddleNames>James</MiddleNames><LastName>Smith</LastName><DateOfBirth>1980-01-01</DateOfBirth><HomeAddress><StreetAddress>12 Main Street</StreetAddress><City>Perth</City><State>LA</State><Zip>70805</Zip></HomeAddress></Employee></Employees>"))))

