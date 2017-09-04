(require 'clojure.string)
(ns core)

;;Determina si una entrada de la base de datos es una regla
(defn es-regla [regla-normalizada]
  (if (re-matches #"^[0-9a-zA-Z]{1,}\(([0-9a-zA-Z]{1,}, *)*([0-9a-zA-Z]){1,}\) *:- *([0-9a-zA-Z]{1,}\(([0-9a-zA-Z]{1,}, *)*([0-9a-zA-Z]){1,}\), *)*([0-9a-zA-Z]{1,}\(([0-9a-zA-Z]{1,}, *)*([0-9a-zA-Z]){1,}\) *)$" regla-normalizada)
      true
      false
  )
)

;;Determina si una entrada de la base de datos es un hecho
(defn es-hecho [hecho-normalizado]
  (if (re-matches #"^[0-9a-zA-Z]{1,}\(([0-9a-zA-Z]{1,}, *)*([0-9a-zA-Z]){1,}\)$" hecho-normalizado)
      true
      false
  )
)

;;Valida el formato de una consulta que es de la forma 'nombre(parametros separados por ,)'
(defn validar-consulta [consulta-normalizada]
  (es-hecho consulta-normalizada)
)

;;Acondiciona (normaliza) la entrada a una forma comun para asi facilitar los algoritmos mas internos a la resolucion de la consulta 
(defn formatear-entrada [entrada]
  (clojure.string/replace (clojure.string/replace (clojure.string/replace entrada #"\n" "") #"\t" "") #" " "")
)