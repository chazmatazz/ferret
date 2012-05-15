
(ns ferret.template
    (:use org.bituf.clj-stringtemplate)
    (:use [clojure.contrib.seq :only [indexed]]))
  
  (defn new-lambda [n e]
    (let [view (create-view "FN($name$$env:{,$it$}$)")]
      (fill-view! view "name" n)
      (fill-view! view "env" e)
      (render-view view)))
  
  (defn invoke-lambda [n args]
    (let [view (create-view "INVOKE($lambda$, $args:{$it$} ;separator=\",\"$)")]
      (fill-view! view "lambda" n)
      (fill-view! view "args" (reverse args))
      (render-view view)))
  
  (defn if-statement [cond t f]
    (apply str "(BOOLEAN(" cond ")->asBool() ? (VAR)" t " : (VAR)" f ")"))
  
  ;;
  ;; Solution Template
  ;;
  
  (defn declare-lambdas [lambdas]
    (let [view (create-view
                "
$lambdas: {lambda|
    class $lambda.name$ : public Lambda{

    $lambda.env:{VAR $it$;} ;separator=\"\n\"$

    public:

      $lambda.name$ ($lambda.env:{VAR $it$} ;separator=\",\"$){
        $lambda.env:{this->$it$ = $it$;} ;separator=\"\n\"$
      }

      VAR invoke (VAR _args_){

        $lambda.args:{args |
          VAR $last(args)$ = SEQUENCE(_args_)->nth($first(args)$); }
          ;separator=\"\n\"$

        $lambda.var_args$

        $trunc(lambda.body):{$it$;} ;separator=\"\n\"$
        return $last(lambda.body):{ $it$;} ;separator=\"\n\"$
      }

      int getType(){ return LAMBDA_TYPE;}
      VAR equals(VAR o){ return false; }
      VAR toOutputStream(){ fprintf(OUTPUT_STREAM, \"%s\", \"$lambda.name$\"); return VAR();}
    };
}$
                ")]
      (fill-view! view "lambdas" (map #(let [args (:args %)]
                                         (assoc % :args (indexed args))) lambdas))
      (render-view view)))
  
  
  (defn solution-template [source]
    (let [{:keys [body lambdas symbol-table native-declarations]} source
          view (create-view
                "
  #include \"ferret.h\"
  
  $native_declarations:{$it$} ;separator=\"\n\"$
  
  $symbols:{VAR $it$;} ;separator=\"\n\"$
  
  namespace ferret{
    $lambdas:{$it$} ;separator=\"\n\"$
  }
  
  int main(void){
    INIT_ENV
    $body:{$it$;} ;separator=\"\n\"$
    return 0;
  }
                ")]
      (fill-view! view "body" (filter #(not (empty? %)) body))
      (fill-view! view "lambdas" (declare-lambdas lambdas))
      (fill-view! view "symbols" symbol-table)
      (fill-view! view "native_declarations" native-declarations)
      (render-view view)))
