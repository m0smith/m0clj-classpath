(ns m0clj-classpath.tools
  (:use [clojure.string :only (split)]))

;;;Search a ZIP file: From http://stackoverflow.com/a/5428265/850252
;; Maybe use a ZipInputStream

;; (java.lang.System/getProperty "sun.boot.class.path")

;;#'user/filenames-in-zip
;;user> (def z "/media/xubuntu/D8D3-1C07/.m2/repository/clojurewerkz/archimedes/1.0.0-alpha5/archimedes-1.0.0-alpha5.jar")
;;#'user/z
;;user> (filenames-in-zip z)


(def m0clj-resource-map (atom {} ))

(def m0clj-excludes 
  (atom
   [
    #"^com[\\/]sun"
    #"^sun"
    #"~$"
    ]))

(defprotocol EntryHandler
  (entry-name [entry]))

(defrecord FileEntry [base file]
  EntryHandler 
  (entry-name [e] 
    (let [b (.getPath base)
          f (.getPath file)]
      (.replaceAll (.replaceAll (.replace f b "") "[\\\\]" "/") "^/" ""))))

(defrecord NewZipEntry [base file]
  EntryHandler 
  (entry-name [e] (.getName file)))

(extend-type java.net.URI
  EntryHandler 
  (entry-name [e] (println "Unknown entry:" e)))

(defn m0clj-filenames-in-zip [filename]
  (let [z (java.util.zip.ZipFile. filename)] 
    (map #(NewZipEntry. filename %) (enumeration-seq (.entries z)))))

(defn m0clj-files-in [d]
  (map (fn [f] (FileEntry. d f))(filter #(not (.isDirectory %))(file-seq d))))

(defn m0clj-type-of [^java.net.URI uri]
  (let [f (java.io.File. uri)
        p (.getPath f)]
    (cond
     (not (.exists f)) []
     (.isDirectory f) (m0clj-files-in f)
     (.endsWith p "jar") (m0clj-filenames-in-zip f)
     (.endsWith p "zip") (m0clj-filenames-in-zip f)
     :else [uri])))

(defn m0clj-classpath-uris-via-classloader [obj] 
  (let [cl (.. obj getClass getClassLoader)]
    (if cl
       (map #(.toURI %) (. cl getURLs))
       [])))

(defn m0clj-split-classpath [path]
  (let [sep (java.io.File/pathSeparator)]
    (split path (re-pattern sep))))

(defn m0clj-classpath-uris-via-path [path]
  (map #(.toURI (java.io.File. %)) (m0clj-split-classpath path)))

(defn m0clj-classpath-uris-via-system-property [prop-name]
  (m0clj-classpath-uris-via-path (System/getProperty prop-name)))

(defn m0clj-classpath-uris [] 
  (set
   (concat 
    (m0clj-classpath-uris-via-classloader clojure-version)
    (m0clj-classpath-uris-via-system-property "sun.boot.class.path")
    (m0clj-classpath-uris-via-system-property "java.class.path"))))

(defn m0clj-resources-map [] 
  (reduce #(update-in %1 [(entry-name %2)] conj %2) {} 
          (mapcat m0clj-type-of (m0clj-classpath-uris))))

(defn camel-case-pattern [pattern]
  (let [any (str \. \*)
        empty-str (str)
        non-upper (str \( \[ \^ \A \- \Z \] \* \) )
        upper (str \( \[ \A \- \Z \] \[ \^ \A \- \Z \] \* \) )
        lo (first (re-find (re-pattern non-upper) pattern))
        prefix (if (= lo empty-str ) empty-str (str any lo))]
    (re-pattern (str prefix any
                     (apply str (map #(str (first %1) %2) 
                                     (re-seq (re-pattern upper) pattern) (repeat any )))))))

(defn camel-case? [p s]
  (re-find (camel-case-pattern p) s))

(defn m0clj-exclude? [p]
  (not-any? #(re-find % p) @m0clj-excludes))

(defn m0clj-init []
  (reset! m0clj-resource-map (m0clj-resources-map))
  true)

(defn m0clj-resource-search [p]
  (filter #(m0clj-exclude? (first %)) 
          (filter #(camel-case? p (first %)) @m0clj-resource-map)))

(defn m0clj-class-search [p]
  (filter #(.endsWith (first %) ".class") (m0clj-resource-search p)))

(defn m0clj-path-to-full-class [p]
  (.. p (replaceAll "/" ".") (replaceAll ".class$" "")))


(defn which [class-name]
  (let [clazz (Class/forName class-name)
        resource-name (str "/" (.replace class-name "." "/") ".class")]
    (.getResource clazz resource-name)))


