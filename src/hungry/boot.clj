(ns hungry.boot
  (:require [overtone.core :refer :all :exclude [tap]]))

(comment
  osample/reload-all-samples

  (server-status)
  (kill-server)
  (boot-internal-server)
  (reset! connected :no)

  @osample/loaded-samples*
  )


(defonce connected (atom :no))
(when-not (= @connected :happy-hacking)
  (reset! connected (connect-external-server 57110)))
