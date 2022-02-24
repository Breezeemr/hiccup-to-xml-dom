(ns hiccup-to-xml-dom.core
  (:import [javax.xml.parsers DocumentBuilder DocumentBuilderFactory]
           (java.io StringWriter)
           (javax.xml.transform.dom DOMSource)
           (javax.xml.transform TransformerFactory)
           (javax.xml.transform.stream StreamResult)
           (org.w3c.dom Document)
           (org.w3c.dom.ls LSSerializer DOMImplementationLS)
           (com.sun.org.apache.xml.internal.serializer.dom3 LSSerializerImpl)))

(defprotocol AsElements
  (as-elements [expr doc doc-ns] "Return a seq of elements represented by an expression."))

;; (def doc-ns "http://www.w3.org/1999/XSL/Format")
(defn element* [tag attrs content doc doc-ns]
  (let [el (. doc (createElementNS doc-ns (name tag)))
        as-el-wrapper #(as-elements % doc doc-ns)]
    (doseq [[k v] attrs]
      (.setAttributeNS el (namespace k) (name k) v))
    (transduce
      (map identity)
      (fn
        ([acc] acc)
        ([acc item]
         (cond
           (string? item) (.appendChild
                            el
                            (.createTextNode doc item))
           (not (nil? item)) (.appendChild el item))))
      nil
      (as-elements content doc doc-ns))
    el))

(defn sexp-element [tag attrs child doc doc-ns]
  (cond
    ;(= :-cdata tag) (cdata (first child))
    ;(= :-comment tag) (xml-comment (first child))
    :else (element* tag attrs child  doc doc-ns)))

(extend-protocol AsElements
  clojure.lang.IPersistentVector
  (as-elements [v doc doc-ns]
    (let [[tag & [attrs & after-attrs :as content]] v
          [attrs content] (if (map? attrs)
                            [(into {} (for [[k v] attrs]
                                        [k (str v)]))
                             after-attrs]
                            [{} content])]
      [(sexp-element tag attrs content  doc doc-ns)]))

  clojure.lang.ISeq
  (as-elements [s doc doc-ns]
    (let [as-el-wrapper #(as-elements % doc doc-ns)]
      (mapcat as-el-wrapper s)))

  ;clojure.lang.Keyword
  ;(as-elements [k doc doc-ns]
  ;  [(element* k nil nil doc doc-ns)])

  java.lang.String
  (as-elements [s doc doc-ns]
    [s])

  nil
  (as-elements [_ doc doc-ns] nil)

  java.lang.Object
  (as-elements [o doc doc-ns]
    [(str o)]))

(defn sexps-as-fragment
  "Convert a compact prxml/hiccup-style data structure into the more formal
   tag/attrs/content format. A seq of elements will be returned, which may
   not be suitable for immediate use as there is no root element. See also
   sexp-as-element.

   The format is [:tag-name attr-map? content*]. Each vector opens a new tag;
   seqs do not open new tags, and are just used for inserting groups of elements
   into the parent tag. A bare keyword not in a vector creates an empty element.

   To provide XML conversion for your own data types, extend the AsElements
   protocol to them."
  ([doc doc-ns] nil)
  ([doc doc-ns sexp]
   (as-elements sexp doc doc-ns)))

(defn sexp-as-element
  "Convert a single sexp into an Element"
  [doc doc-ns sexp]
  (let [[root & more] (sexps-as-fragment doc doc-ns sexp)]
    (when more
      (throw
        (IllegalArgumentException.
          "Cannot have multiple root elements; try creating a fragment instead")))
    root))


(defn  serialize-dom [^Document dom]
  (let [ls-impl ^DOMImplementationLS (-> dom
                                         (.getOwnerDocument)
                                         (.getImplementation)
                                         (.getFeature "LS" "3.0"))
        serializer ^LSSerializer (.createLSSerializer ls-impl)]
    (-> serializer .getDomConfig (.setParameter "xml-declaration" false))
    (.writeToString serializer dom)))

(defn serialize-dom2 [dom]
  (let [source (DOMSource. dom)
        sw     (StringWriter.)
        sr     (StreamResult. sw)
        txformer (-> (TransformerFactory/newDefaultInstance)
                 .newTransformer)]
    (.transform txformer source sr)
    (str sw)))

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

(defn ^Document ->dom2 [content & {:keys [document-namespace] :or {document-namespace ""}}]
  (let [db ^DocumentBuilder (make-document-builder)
        doc-root (.newDocument db)]
    (.appendChild doc-root (sexp-as-element doc-root document-namespace content))
    doc-root))
