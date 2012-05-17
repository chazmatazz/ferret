
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
  (copy-to-solution "ferret.h" "./solution/ferret.h")
  (copy-to-solution "Makefile" "./solution/Makefile"))

(defn write-to-solution [s f]
  (FileUtils/writeStringToFile (file (str "./solution/" f)) s))

(defn append-to! [r ks v]
  (dosync
   (let [cv (reduce (fn[h v] (v h)) @r ks)]
     (alter r assoc-in ks (conj cv v)))))

(defn morph-form [tree pred f]
    (loop [loc (zip/seq-zip tree)]
      (if (zip/end? loc)
        (zip/root loc)
        (recur
         (zip/next
          (if (pred (zip/node loc))
            (zip/replace loc (f (zip/node loc)))
            loc))))))

  (defn remove-form [tree pred]
    (loop [loc (zip/seq-zip tree)]
      (if (zip/end? loc)
        (zip/root loc)
        (recur
         (zip/next
          (if (pred (zip/node loc))
            (zip/remove loc)
            loc))))))

  (defn is-form? [& s]
    (fn [f]
      (and (seq? f)
           (some true? (map #(= % (first f)) s)))))

(defn dispatch-reader-macro [ch fun]
    (let [dm (.get
              (doto (.getDeclaredField clojure.lang.LispReader "dispatchMacros")
                (.setAccessible true))
              nil)]
      (aset dm (int ch) fun)))

  (defn native-string [rdr letter-u]
    (loop [s (str )
           p \space
           c (char (.read rdr))]
      (if (and (= c \#) (= p \>))
        s
        (recur (str s p) c (char (.read rdr))))))

  (dispatch-reader-macro \< native-string)

(defn expand-macros [form]
    (let [macros (->> (read-string (str \( (read-from-url "runtime.clj") \)))
                      ;;get built in macros
                      (filter (is-form? 'defmacro))
                      ;;merge user defined macros
                      (concat (filter (is-form? 'defmacro) form)))
          form (remove-form form (is-form? 'defmacro))
          temp-ns (gensym)]

      (create-ns temp-ns)
      (binding [*ns* (the-ns temp-ns)]
        (refer 'clojure.core :exclude (concat (map second macros) ['fn 'let 'def]))
        (use 'clojure.contrib.macro-utils)
        (doseq [m macros]
          (eval m)))

      (let [form (morph-form form
                             (apply is-form? (map second macros))
                             (fn [f]
                               (binding [*ns* (the-ns temp-ns)]
                                 (macroexpand-all f))))]
        (remove-ns temp-ns)
        form)))

(defn add-built-in
    ([form]
       (let [built-in (->> (read-string (str \( (read-from-url "runtime.clj") \)))
                           (filter (is-form? 'defn))
                           (reduce (fn[h v] (assoc h (second v) v)) {}))
             fns (ref {'list (built-in 'list)})
             form (add-built-in form built-in fns)]
         (concat (vals @fns) form)))
    ([form built-in fns]
       (morph-form form symbol?
                   #(do (if-let [f (built-in %)]
                          (when (not (@fns %))
                            (do (dosync (alter fns assoc % f))
                                (add-built-in
                                 (expand-macros (drop 3 f))
                                 built-in fns)))) %))))

(defn vector->list [form]
    (morph-form form vector? #(reverse (into '() %))))

(defn let->fn [form]
    (morph-form form
                (is-form? 'let)
                (fn [[_ bindings & body]]
                  (let [bindings (partition 2 bindings)
                        vars (flatten (map first bindings))
                        defs (map #(cons 'define-var %) bindings)
                        body-fn (cons (concat ['fn vars] body) vars)]
                    (list (concat ['fn []] defs [body-fn]))))))

 (defn do->fn [form]
    (morph-form form
                (is-form? 'do)
                #(list (concat ['fn []] (rest %)))))

(defn lambda-defined? [fns env args body]
    (let [f (concat [env args] body)
          name (reduce (fn[h v]
                         (let [[_ n & r] v]
                           (if (= r f) n))) nil @fns)]
      (when name
        (apply list 'lambda-object name env))))

  (defn define-lambda [fns env args body]
    (let [n (gensym)]
      (dosync (alter fns conj (concat ['define-lambda n env args] body)))
      (apply list 'lambda-object n env)))

  (defn closure-conversion
    ([form]
       (let [fns (ref [])
             form (closure-conversion form fns)]
         (vector->list (concat @fns form))))
    ([form fns & env]
       (morph-form form
                   (is-form? 'fn)
                   (fn [[_ args & body]]
                     (let [env (if (nil? env) '() (first env))
                           body (closure-conversion body fns (concat args env))]
                       (if-let [n (lambda-defined? fns env args body)]
                         n
                         (define-lambda fns env args body)))))))

 (defn symbol-conversion [form]
    (let [c (comp #(symbol (escape {\- \_ \* "_star_" \+ "_plus_" \/ "_slash_"
                                    \< "_lt_" \> "_gt_" \= "_eq_" \? "_QMARK_"}
                                   (str %)))
                  #(cond (= 'not %) '_not_
                         :default %))]
      (morph-form form symbol? c)))

(defn process [form]
    (->> (expand-macros form)
         (add-built-in)
         (expand-macros)
         (vector->list)
         (let->fn)
         (do->fn)
         (closure-conversion)
         (symbol-conversion)
         (vector->list)))

(defn to-str? [f]
  (or (true? f) (false? f) (symbol? f)))

(defn is-special-form? [s f]
  (and (seq? f)
       (= (first f) s)))

(defmulti emit (fn [form _]
                   (cond (is-special-form? 'define_lambda form) 'define_lambda
                         (is-special-form? 'lambda_object form) 'lambda_object
                         (is-special-form? 'define_var form) 'define_var
                         (is-special-form? 'native_declare form) 'native_declare
                         (is-special-form? 'if form) 'if
                         (is-special-form? 'def form) 'def
                         (is-special-form? 'reduce form) 'reduce
                         (to-str? form) :to-str
                         (keyword? form) :keyword
                         (number? form) :number
                         (nil? form) :nil
                         (char? form) :char
                         (string? form) :string
                         (seq? form) :sequence)))

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

(defn emit-source [form]
    (let [state (ref {:lambdas [] :symbol-table #{} :native-declarations []})
          body (doall (map #(emit % state) (process form)))]
      (assoc @state :body body)))

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
