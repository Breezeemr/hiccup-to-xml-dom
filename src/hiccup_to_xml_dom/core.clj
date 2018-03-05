(ns hiccup-to-xml-dom.core
  (:import [javax.xml.parsers DocumentBuilder DocumentBuilderFactory]))

;; (def doc-ns "http://www.w3.org/1999/XSL/Format")

(defn serialize-dom [dom]
  (let [ls-impl (.. dom (getOwnerDocument) (getImplementation) (getFeature "LS" "3.0"))
        serializer (.createLSSerializer ls-impl)]
    (.. serializer (getDomConfig) (setParameter "xml-declaration" false))
    (.writeToString serializer dom)))

(defn- create-elem-ns [doc doc-ns tag attrs content]
  (let [el (. doc (createElementNS doc-ns (name tag)))
        content (if (seq? content)
                  content
                  (list content))]
    (doseq [[k v] attrs]
      (.setAttributeNS el nil (name k) v))
    (doseq [item content]
      (cond
        (string? item) (.appendChild
                        el
                        (.. el (getOwnerDocument) (createTextNode item)))
        (not (nil? item)) (.appendChild el item)))
    el))

(defn- element-compile-strategy
  "Returns the compilation strategy to use for a given element."
  [[doc tag attrs & content :as element] document-namespace]
  (cond
    (not-any? coll? (rest element)) ::all-literal ; e.g. [:span "foo"]
    (and (keyword? tag) (map? attrs)) ::literal-tag-and-attributes     ; e.g. [:span {} x]
    :else ::default))                      ; e.g. [x]

(defmulti #^{:private true} compile-element element-compile-strategy)
(defmethod compile-element ::all-literal
  [[doc tag & content] document-namespace]
  (create-elem-ns doc document-namespace tag {} content))
(defmethod compile-element ::literal-tag-and-attributes
  [[doc tag attrs & content] document-namespace]
  (create-elem-ns doc document-namespace tag attrs
    (map #(if (vector? %)
            (compile-element (into [doc] %) document-namespace)
           %)
      content)))
(defmethod compile-element ::default
  [[doc tag & else] document-namespace]
  (compile-element
   (into [doc tag]
         (for [e else]
           (if (vector? e)
             (compile-element (into [doc] e) document-namespace)
             e)))
   document-namespace))

(defn- make-document-builder []
  (let [dbf (DocumentBuilderFactory/newInstance)
        _ (.setNamespaceAware dbf true)
        db (.newDocumentBuilder dbf)]
    db))

(defn ->dom [content & {:keys [document-namespace] :or {document-namespace ""}}]
  (let [db (make-document-builder)
        doc-root (.newDocument db)]
    (compile-element (into [doc-root] content) document-namespace)))
