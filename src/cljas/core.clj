(ns cljas.core
  (:gen-class)
  (:import (hep.aida IAnalysisFactory)
	   (java.util Random)))

(defn collect-randoms [n]
  (let [r (Random.)]
    (loop [i n
	   coll []]
      (cond (zero? i) coll
            true (recur (- i 1)
                        (conj coll (. r nextGaussian)))))))

(defn fillhisto! [h coll]
  (loop [remaining-coll coll]
    (cond (empty? remaining-coll) true
	  true (do (. h fill (first remaining-coll))
		   (recur (rest remaining-coll))))))

(defn -main [& argv]
  (let [randoms (collect-randoms 1000)
        af (IAnalysisFactory/create)
        tf (. af createTreeFactory)
        tree (. tf create)
        hf (. af createHistogramFactory tree)
        histo (. hf createHistogram1D "My first histo" 100 -5.0 5.0)
        pf (. af createPlotterFactory)
        plotter (. pf create "My very first plotter")]
    (do
      (. plotter show)
      (fillhisto! histo randoms) 
      (. plotter createRegions 1 1)
      (. (. plotter region 0) plot histo))))

