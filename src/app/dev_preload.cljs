(ns app.dev-preload
  "Dev-only preload: enables spec assertions and instrumentation.
   Not included in production builds."
  (:require
   [cljs.spec.alpha :as s]))

(s/check-asserts true)

(js/console.log "[dev] Spec assertions enabled")
