(require 'clojure.string)

(ns logical-interpreter
  (:require [core :refer :all]
            [consultas :refer :all]))

(defn resolver-consulta [hechos-normalizados reglas-normalizadas consulta-normalizada]
      ;;Determina si la consulta corresponde a una regla y la devuelve
      (def regla-correspondiente-consulta (obtener-regla reglas-normalizadas consulta-normalizada))
     
      (if regla-correspondiente-consulta
      ;;Es una consulta a una regla
      (obtener-verdad-regla hechos-normalizados regla-correspondiente-consulta consulta-normalizada)
      ;;Es una consulta a un hecho
      (obtener-verdad-hecho hechos-normalizados consulta-normalizada)
  )
)


(defn evaluate-query [base-de-datos consulta]
  ;;Normaliza todas las entradas para simplificar el analisis dentro de los algoritmos
  (def base-de-datos-normalizada (formatear-entrada base-de-datos))
  (def consulta-normalizada (formatear-entrada consulta))
  (def hechos-normalizados (obtener-hechos base-de-datos-normalizada))
  (def reglas-normalizadas (obtener-reglas base-de-datos-normalizada))
  
  (if (= (+ (count hechos-normalizados) (count reglas-normalizadas)) (count (clojure.string/split base-de-datos-normalizada #"\.")))
    (if (validar-consulta consulta-normalizada) 
        (resolver-consulta hechos-normalizados reglas-normalizadas consulta-normalizada)
        nil
    )
  ;;La base de datos tenia errores (estaba incompleta)
  nil)
)


