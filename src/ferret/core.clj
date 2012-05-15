
(ns ferret.core
  (:gen-class)
  (:use [clojure.java.io]
        [clojure.contrib.io :only [delete-file-recursively]]
        [clojure.contrib.string :only [escape]]
        [clojure.contrib.command-line]
        [clojure.walk :only [macroexpand-all]]
        [org.bituf.clj-stringtemplate])
  (:require [clojure.zip :as zip])
  (:use [ferret.template] :reload-all)
  (:import (org.apache.commons.io FileUtils)
           (java.io BufferedReader StringReader InputStreamReader)))


;; I/O

(defn read-from-url [f]
  (with-open [in (.getResourceAsStream (ClassLoader/getSystemClassLoader) f)
              rdr (BufferedReader. (InputStreamReader. in))]
    (apply str (interpose \newline (line-seq rdr)))))

(defn copy-to-solution [fin fout]
  (FileUtils/copyURLToFile (ClassLoader/getSystemResource fin) (file fout)))

(defn init-solution-dir []
  (doto (file "./solution/")
    (delete-file-recursively true)
    (.mkdir))
  (copy-to-solution "ferret.h" "./solution/ferret.h"))

(defn write-to-solution [s f]
  (FileUtils/writeStringToFile (file (str "./solution/" f)) s))

(defn append-to! [r ks v]
  (dosync 
   (let [cv (reduce (fn[h v] (v h)) @r ks)]
     (alter r assoc-in ks (conj cv v)))))





















(defn to-str? [f]
  (or (true? f) (false? f) (symbol? f)))

(defn is-special-form? [s f]
  (and (seq? f)
       (= (first f) s)))




(defmethod emit :to-str [form state] (str "VAR("form ")"))

(defmethod emit :char [form state] (str "VAR('" form "')"))

(defmethod emit :string [form state] (str "INVOKE(list,"
                                          (apply str (interpose \, (map #(emit % state) (reverse form))))  ")"))

(defmethod emit :nil [form state] "VAR()")

(defmethod emit :keyword [form state]
           (str "VAR(new ferret::Keyword(" (reduce (fn[h v] (+ h (int v))) 0 (str form))"))"))

(defmethod emit :number [form state]
           (str "VAR("form (if (float? form) "f") ")"))

(defmethod emit :sequence [[fn & args] state]
           (invoke-lambda (emit fn state) (map #(emit % state) args)))

(defmethod emit 'define_var [[_ name form] state]
           (str "VAR " name " = " (emit form state)))

(defmethod emit 'native_declare [[_ declaration] state]
           (append-to! state [:native-declarations] declaration) "")

(defmethod emit 'lambda_object [[_ name & env] state]
           (new-lambda name (filter #(not (= '& %)) env)))

(defmethod emit 'define_lambda [[_ name env args & body] state]
           (let [body (if (string? (first body))
                        ["VAR __result" body "__result"]
                        (map #(emit % state) body))
                 env (filter #(not (= '& %)) env)
                 reg-args (take-while #(not (= '& %)) args)
                 va-args (if (some #{'&} args)
                           (let [arg (last args)]
                             (str "VAR " arg " = "
                              (reduce (fn[h v]
                                       (str "SEQUENCE(" h ")->rest()"))
                                     "_args_" (range (count reg-args))) ";\n")))]
             (append-to! state [:lambdas] {:name name :env env :args reg-args :var_args va-args :body body}) ""))

(defmethod emit 'if [[_ cond t f] state]
           (let [cond (emit cond state)
                 t (emit t state)
                 f (if (nil? f) "VAR()" (emit f state))]
             (if-statement cond t f)))

(defmethod emit 'reduce [[_ & args] state]
           (if (= 2 (count args))
             (let [[f s] args]
               (str "(SEQUENCE(" (emit s state) ")->reduce(" (emit f state) "))"))
             (let [[f v s] args]
               (str "(SEQUENCE(" (emit s state) ")->reduce(" (emit f state) " , " (emit v state) "))"))))

(defmethod emit 'def [[_ name & form] state]
           (append-to! state [:symbol-table] name)
           (str name " = " (apply str (map #(emit % state) form))))



(defn compile->cpp [form]
  (init-solution-dir)
  (let [source (emit-source form)]
    (write-to-solution (solution-template source) "solution.cpp")))

(defn -main [& args]
  (with-command-line args
    "Ferret"
    [[input in "File to compile"]]
    (let [f (read-string (str \( (FileUtils/readFileToString (file input)) \)))]
      (compile->cpp f))))
