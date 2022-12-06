(defproject aoc-2022 "2022.1.0-SNAPSHOT"
  :description "Advent of code - 2022"
  :url "https://github.com/einars/aoc-2022"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.trace "0.7.11"]
                 ]
  :main ^:skip-aot aoc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
