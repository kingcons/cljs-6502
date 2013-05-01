(ns clj-6502.core
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]))

(defroutes app-routes
  (GET "/" [] "You are now running cljs-6502!")
  (route/resources "/")
  (route/not-found "404 - Page not found"))

(def handler (handler/site app-routes))
