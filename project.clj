(defproject cljas "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :repositories {"freehep-maven" "http://java.freehep.org/maven2"}
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
                 [org.freehep/freehep-jaida "3.3.0-11-SNAPSHOT"]
                 [org.freehep/freehep-graphicsio-pdf "2.1.2-SNAPSHOT"]
		 [org.freehep/jas-plotter "2.2.6"]]
  :dev-dependencies [[leiningen-run "0.3"]
                     [autodoc "0.7.0"]
                     [leiningen/lein-swank "1.1.0"]]
  :main cljas.core)
