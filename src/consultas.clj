(require 'clojure.string)
(ns consultas
  (:require [core :refer :all]))

;;Devuelve una lista de todos los hechos de la base de datos
(defn obtener-hechos [base-de-datos-normalizada]
  (apply list (filter es-hecho (clojure.string/split base-de-datos-normalizada #"\.")))
  )

;;Devuelve una lista de todas las reglas de la base de datos
(defn obtener-reglas [base-de-datos-normalizada]
  (apply list (filter es-regla (clojure.string/split base-de-datos-normalizada #"\.")))
  )

;;Devuelve una lista con los nombres de los parametros de una determinada regla
(defn obtener-parametros-regla [regla-normalizada]
  (clojure.string/split (second (re-matches #"^[^(]*\(([^)]*)\).*$" regla-normalizada)) #",")
  )

;;Devuelve una lista con los valores de los parametros de una determinada consulta
(defn obtener-parametros-consulta [consulta-normalizada]
  (obtener-parametros-regla consulta-normalizada)
)

;;Devuelve el nombre de la regla
(defn obtener-nombre-regla [regla-normalizada]
  (second (re-matches #"^([^(]*)\([^)]*\).*$" regla-normalizada))
  )

;;Devuelve el nombre de la consulta
(defn obtener-nombre-consulta [regla-normalizada]
  (obtener-nombre-regla regla-normalizada)
  )

;;Dada una consulta a un hecho verifica su condicion de verdad (si esta en la lista es verdadera, sino falsa)
(defn obtener-verdad-hecho [hechos hecho-normalizado]
   (if (some #{hecho-normalizado} hechos)
      true
      false)
  )

;;Obtiene los hechos que componen a una determinada regla
(defn obtener-hechos-regla [regla-normalizada]
  (clojure.string/split (clojure.string/replace (second (re-matches #"^.*:-(.*)$" regla-normalizada)) #"\)," ") ") #" ")
  )

;;Verifica si una consulta es igual a una determinada regla en base al nombre y cantidad de parametros (soporta sobrecarga por numero de parametros)
(defn validar-equivalencia-regla-consulta [regla-normalizada consulta-normalizada]
  (def nombre-regla (obtener-nombre-regla regla-normalizada))
  (def nombre-consulta (obtener-nombre-consulta consulta-normalizada))
  (def cantidad-parametros-regla (count (obtener-parametros-regla regla-normalizada)))
  (def cantidad-parametros-consulta (count (obtener-parametros-consulta consulta-normalizada)))

  (if (and (= cantidad-parametros-regla cantidad-parametros-consulta) (= nombre-regla nombre-consulta))
      true
      false
      )
  )

;;Obtiene la regla correspondiente a la consulta (si es que fuera una regla)
(defn obtener-regla [reglas consulta-normalizada]
  (some (fn [regla] (if (validar-equivalencia-regla-consulta consulta-normalizada regla) regla false))  reglas)
)

;;Funcion auxiliar recursiva para 'reemplazar-parametros-regla'
(defn reemplazar-parametros-regla-recursivo [regla-normalizada-a-reemplazar mapa-equivalencia-parametros]
  (if (not= (count mapa-equivalencia-parametros) 0)
      (clojure.string/replace (reemplazar-parametros-regla-recursivo regla-normalizada-a-reemplazar (drop 1 mapa-equivalencia-parametros)) (re-pattern (first (first mapa-equivalencia-parametros))) (second (first mapa-equivalencia-parametros)))
      regla-normalizada-a-reemplazar
  )
)

;;Usando "reemplazar-parametros-regla-recursivo" reemplaza los valores de las variables en una regla
(defn reemplazar-parametros-regla [regla-normalizada regla-normalizada-a-reemplazar]
    "regla-normalizada es la consulta a resolver y regla-normalizada-a-reemplazar es la regla generica definida en la base de datos"
    (def lista-parametros-regla (obtener-parametros-consulta regla-normalizada))
    (def lista-parametros-regla-a-reemplazar (obtener-parametros-regla regla-normalizada-a-reemplazar))
    (def mapa-equivalencia-parametros (zipmap lista-parametros-regla-a-reemplazar lista-parametros-regla))
    
    (reemplazar-parametros-regla-recursivo regla-normalizada-a-reemplazar mapa-equivalencia-parametros)
  )

;;Funcion auxiliar recursiva para 'obtener-verdad-regla'
(defn obtener-verdad-regla-recursiva [hechos-normalizados lista-hechos]
  (if (not= (count lista-hechos) 0)
      (and (obtener-verdad-regla-recursiva hechos-normalizados (drop 1 lista-hechos)) (obtener-verdad-hecho hechos-normalizados (first lista-hechos)))
      true
  )
)

;;Obtiene la condicion de verdad de una determinada regla usando 'obtener-verdad-regla-recursiva'
(defn obtener-verdad-regla [hechos-normalizados regla-correspondiente-consulta consulta-normalizada]
  (def lista-hechos (obtener-hechos-regla (reemplazar-parametros-regla consulta-normalizada regla-correspondiente-consulta)))
  (obtener-verdad-regla-recursiva hechos-normalizados lista-hechos)
  )