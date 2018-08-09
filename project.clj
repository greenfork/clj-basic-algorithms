(defproject clj-basic-algorithms "0.1.0-SNAPSHOT"
  :description "Basic algorithms written in Clojure with <3"
  :url "https://github.com/greenfork/clj-basic-algorithms"
  :license {:name "Unlicense"
            :url "http://unlicense.org/"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [criterium "0.4.4"]]
  :main ^:skip-aot clj-basic-algorithms.main
  :test-paths ["src/clj_basic_algorithms"]
  :monkeypatch-clojure-test false
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}})
