
(defmacro not= [& test]
  (list 'not (cons '= `( ~@test))))

(defmacro when [test & body]
  (list 'if test (cons 'do body)))

(defmacro while [test & body]
  (list '_while_ (list 'fn [] test) (cons 'fn `( [] ~@body))))

(defmacro forever [& body]
  (cons 'while `(true  ~@body)))

(defmacro and
  ([] true)
  ([x] x)
  ([x & next]
     (list 'if x `(and ~@next) false)))

(defmacro or
  ([] nil)
  ([x] x)
  ([x & next]
     (list 'if x x `(or ~@next))))

(defmacro cond
  [& clauses]
    (when clauses
      (list 'if (first clauses)
            (if (next clauses)
                (second clauses)
                (throw (IllegalArgumentException.
                         "cond requires an even number of forms")))
            (cons 'cond (next (next clauses))))))

(defn not [x]
  #<
  if (OBJECT(x)->getType() != BOOLEAN_TYPE)
    return false;
  __result = !BOOLEAN(x)->asBool();
  >#)

(defn nil? [x] "__result = (x.get() == NULL)")

(defn empty? [x] "__result = SEQUENCE(x)->isEmpty();")

(defn list [& xs] "__result = xs;")


(defn rest [x] "__result = SEQUENCE(x)->rest();")

(defn cons [x seq] "__result = (SEQUENCE(seq)->clone(),x);")

(defn _while_ [pred fn]
  #<
  while(BOOLEAN(INVOKE(pred))->asBool() == true)
      INVOKE(fn);
  >#)

(defmacro dotimes [binding & body]
  (list '_dotimes_ (second binding) (cons 'fn `( [~(first binding)] ~@body))))

(defn _dotimes_ [t f] "for(int i = 0; i < INTEGER(t)->intValue(); i++) INVOKE(f,i);")

(defn apply [f args] "__result = LAMBDA(f)->invoke(args);")

(defn integer? [x] "__result = (OBJECT(x)->getType() == INTEGER_TYPE);")

(defn float? [x] "__result = (OBJECT(x)->getType() == FLOAT_TYPE);")

(defn char? [x] "__result = (OBJECT(x)->getType() == CHARACTER_TYPE);")

(defn list? [x] "__result = (OBJECT(x)->getType() == LIST_TYPE);")

(defn print [& more]
  (dotimes [i (count more)]
    #<
    SEQUENCE(more)->nth(INTEGER(i)->intValue()).toOutputStream();
    fprintf(OUTPUT_STREAM, " ");
    >#))

(defn newline []
  #<
  fprintf(OUTPUT_STREAM, "\n");
  >#)


(defn + [& xs]
  (reduce (fn[h v]
            #<
            switch(OBJECT(h)->getType()) {
                case INTEGER_TYPE:
                    if (OBJECT(v)->getType() == INTEGER_TYPE) {
                        __result = INTEGER(h)->intValue() + INTEGER(v)->intValue();
                        break;
                    }
                case FLOAT_TYPE:
                    __result = GETFLOAT(h) + GETFLOAT(v);
            }
            >#) 0 xs))

(defn * [& xs]
  (reduce (fn[h v]
            #<
            switch(OBJECT(h)->getType()) {
                case INTEGER_TYPE:
                    if (OBJECT(v)->getType() == INTEGER_TYPE) {
                        __result = INTEGER(h)->intValue() * INTEGER(v)->intValue();
                        break;
                    }
                case FLOAT_TYPE:
                    __result = GETFLOAT(h) * GETFLOAT(v);
            }
            >#) 1 xs))

(defn - [& xs]
  (if (= (count xs) 1)
    (* -1 (first xs))
    (reduce (fn[h v]
              #<
            switch(OBJECT(h)->getType()) {
                case INTEGER_TYPE:
                    if (OBJECT(v)->getType() == INTEGER_TYPE) {
                        __result = INTEGER(h)->intValue() - INTEGER(v)->intValue();
                        break;
                    }
                case FLOAT_TYPE:
                    __result = GETFLOAT(h) - GETFLOAT(v);
            }
              >#) (first xs) (rest xs))))

(defn / [& xs]
  (if (= (count xs) 1)
    (apply / (cons 1 xs))
    (reduce (fn[h v]
              #<
            switch(OBJECT(h)->getType()) {
                case INTEGER_TYPE:
                    if (OBJECT(v)->getType() == INTEGER_TYPE) {
                        __result = INTEGER(h)->intValue() / INTEGER(v)->intValue();
                        break;
                    }
                case FLOAT_TYPE:
                    __result = GETFLOAT(h) / GETFLOAT(v);
            }
              >#) (first xs) (rest xs))))

(defn = [a & more]
  (if (empty? more)
    true
    (and ((fn [a b] "__result = OBJECT(a)->equals(b)") a (first more))
         (apply = more))))

(defn < [a & more]
  (if (empty? more)
    true
    (and ((fn [a b]
            #<
            switch(OBJECT(a)->getType()) {
                case INTEGER_TYPE:
                    if (OBJECT(b)->getType() == INTEGER_TYPE) {
                        __result = (INTEGER(a)->intValue() < INTEGER(b)->intValue());
                        break;
                    }
                case FLOAT_TYPE:
                    __result = (GETFLOAT(a) < GETFLOAT(b));
            }
            >#) a (first more))
         (apply < more))))

(defn > [a & more]
  (if (empty? more)
    true
    (and ((fn [a b]
            #<
            switch(OBJECT(a)->getType()) {
                case INTEGER_TYPE:
                    if (OBJECT(b)->getType() == INTEGER_TYPE) {
                        __result = (INTEGER(a)->intValue() > INTEGER(b)->intValue());
                        break;
                    }
                case FLOAT_TYPE:
                    __result = (GETFLOAT(a) > GETFLOAT(b));
            }
            >#) a (first more))
         (apply > more))))

(defn >= [a & more]
  (if (empty? more)
    true
    (and ((fn [a b]
            #<
            switch(OBJECT(a)->getType()) {
                case INTEGER_TYPE:
                    if (OBJECT(b)->getType() == INTEGER_TYPE) {
                        __result = (INTEGER(a)->intValue() >= INTEGER(b)->intValue());
                        break;
                    }
                case FLOAT_TYPE:
                    __result = (GETFLOAT(a) >= GETFLOAT(b));
            }
            >#) a (first more))
         (apply >= more))))

(defn <= [a & more]
  (if (empty? more)
    true
    (and ((fn [a b]
            #<
            switch(OBJECT(a)->getType()) {
                case INTEGER_TYPE:
                    if (OBJECT(b)->getType() == INTEGER_TYPE) {
                        __result = (INTEGER(a)->intValue() <= INTEGER(b)->intValue());
                        break;
                    }
                case FLOAT_TYPE:
                    __result = (GETFLOAT(a) <= GETFLOAT(b));
            }
            >#) a (first more))
         (apply <= more))))

(defn conj [coll & xs]
  (reduce (fn[h v] (cons v h)) (if (nil? coll) (list) coll) xs))

(defn inc [x]
  (+ x 1))

(defn dec [x]
  (- x 1))

(defn pos? [x]
  (> x 0))

(defn neg? [x]
  (< x 0))

(defn zero? [x]
  (= x 0))

(defn count [s]
  (reduce (fn [h v] (inc h)) 0 s))

(defn reverse [s]
  (reduce conj (list) s))

;;Arduino

(defn pin-mode [pin mode]
  #<
  if (KEYWORD(mode)->id == 618)
    pinMode(INTEGER(pin)->intValue(), INPUT);
  else
    pinMode(INTEGER(pin)->intValue(), OUTPUT);
  >#)

(defn digital-write [pin mode]
  #<
  if (KEYWORD(mode)->id == 474)
    digitalWrite(INTEGER(pin)->intValue(), HIGH);
  else
    digitalWrite(INTEGER(pin)->intValue(), LOW);
  >#)

(defn sleep [timeout] "::delay(INTEGER(timeout)->intValue());")
