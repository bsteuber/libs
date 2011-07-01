(ns libs.validate)

(def email-regex #"^(?i)[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$")

(defn validate-email [email]
  (when-not (re-matches email-regex email)
    :invalid-email))
