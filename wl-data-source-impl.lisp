
(in-package :ulubis)

(def-wl-callback offer (client data-source (mime-type :string))
  (format t "Mime-type offered: ~A~%" mime-type))

(defimplementation wl-data-source ()
  ((:offer offer))
  ())
