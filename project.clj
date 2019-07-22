(defproject kelly "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [
                 [ts "0.1.0-SNAPSHOT"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.reader "1.3.1"]
                 [medley "1.2.0"]
                 [metasoarous/oz "1.5.6"]]

  :repl-options {:init-ns kelly.core})
