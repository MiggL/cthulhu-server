(defproject cthulu-server "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.json "1.0.0"]
                 [ysera "2.0.2"]
                 [http-kit "2.4.0"]]
  :main ^:skip-aot cthulu-server.main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
