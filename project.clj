(defproject mail-merge "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [clj-pdf "2.2.17"]
                 [org.clojure/tools.namespace "0.2.11"]]

  :main ^:skip-aot parser.core
  :target-path "target/%s"

  :clean-targets ^{:protect false} ["target"]
  :source-paths ["dev" "src"]

  :profiles {:dev {
                   :repl-options {:init-ns          user
                                  :port             7001}
                   :env          {:dev true}
                   :dependencies [[org.clojure/test.check "0.9.0"]
                                  [binaryage/devtools "0.5.2" :exclusions [environ]]
                                  [org.clojure/tools.namespace "0.2.11"]]}}
  )
