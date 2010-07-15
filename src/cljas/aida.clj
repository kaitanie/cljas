(ns cljas.aida
  (:gen-class)
  (:import (hep.aida IAnalysisFactory)
	   (java.util Random)))

(defn plot [plotter plottable]
  (let [regions (* (:max-x plotter) (:max-y plotter))
	next-region (fn [current]
		      (if (= current (- regions 1)) 0
			  (+ current 1)))
	region (. (:plotter plotter) region @(:current-region plotter))]
    (do
      (. region plot plottable)
      (dosync (alter (:current-region plotter) next-region)))))

;; Public API
(defn seq->array [coll]
  (let [element-type (type (first coll))
	can-convert-to-array (fn [c]
			       (and (or (list? c) (vector? c))
				    (every? #(= (type %) element-type)
					    c)))]
    (if (can-convert-to-array coll)
      (let [array (make-array element-type (count coll))]
	(loop [i 0]
	  (cond (< i (count coll)) array
		true (do (aset array i (nth coll i))
			 (recur (+ i 1)))))))
    (do
      (println "Not a sequence!")
      (make-array element-type 0))))

(defn collect-randoms [n]
  (let [r (Random.)]
    (loop [i n
	   coll []]
      (cond (zero? i) coll
            true (recur (- i 1)
                        (conj coll (. r nextGaussian)))))))

(defn histo1d-fill!
  "Fill 1D histogram (causes side-effects!)"
  ([h x]
     (. h fill x))
  ([h x weight]
     (. h fill x weight)))

(defn make-analysis
  "Creates an AIDA context"
  []
  (let [af (IAnalysisFactory/create)
	tf (. af createTreeFactory)
	pf (. af createPlotterFactory)]
    {:item "AIDA context"
     :analysis-factory af
     :tree-factory tf
     :plotter-factory pf}))

(defn tree-init-factories [aida-context tree]
  (let [af (:analysis-factory aida-context)
	hf (. af createHistogramFactory (:tree tree))
	tupleF (. af createTupleFactory (:tree tree))
	entries {:histogram-factory hf
		 :tuple-factory tupleF}]
    (conj tree entries)))

(defn make-tree
  "Creates a new in-memory or persistent tree"
  ([aida-context]
     (let [tf (:tree-factory aida-context)
	   tree (. tf create)]
       (tree-init-factories aida-context
			    {:item "AIDA Tree"
			     :file "in-memory tree"
			     :tree tree})))
  ([aida-context file]
     (let [tf (:tree-factory aida-context)
	   tree (. tf create file)]
       (tree-init-factories aida-context
			    {:item "AIDA Tree"
			     :file file
			     :tree tree}))))

(defn make-histogram1d [tree name nbins xmin xmax]
  (let [hf (:histogram-factory tree)]
    (. hf createHistogram1D name nbins xmin xmax)))

(defn make-logx-histogram1d [tree name xmin xmax]
  )

(defn make-datapoint-set [tree x-coll y-coll]
  )

(defn make-plotter
  ([aida-context]
     (let [plotter-factory (:plotter-factory aida-context)
	   plotter (. plotter-factory create)]
       (. plotter createRegions 1 1)
       {:nx 1
	:ny 1
	:current-region (ref 0)
	:widget plotter}))
  ([aida-context nx ny]
     (let [plotter-factory (:plotter-factory aida-context)
	   plotter (. plotter-factory create)]
       (. plotter createRegions nx ny)
       {:nx nx
	:ny ny
	:current-region (ref 0)
	:widget plotter})))

(defn plot
  ([plotter plotter-region plottable]
     (let [widget (:widget plotter)
	   reg (. widget region plotter-region)]
       (. reg plot plottable)))
  ([plotter plottable]
     (let [region-id (:current-region plotter)
	   widget (:widget plotter)]
       (plot plotter @(:current-region plotter) plottable))))

(defn fillhisto! [h coll]
  (loop [remaining-coll coll]
    (cond (empty? remaining-coll) true
	  true (do (. h fill (first remaining-coll))
		   (recur (rest remaining-coll))))))

(defn aida-example []
  (let [aida (make-analysis)
	tree (make-tree aida)
	plotter (make-plotter aida)
	rndm (collect-randoms 10000)
	rndm2 (collect-randoms 12000)
	h (make-histogram1d tree "Fewer entries" 100 -5.0 5.0)
	h2 (make-histogram1d tree "More entries" 100 -5.0 5.0)]
    (. (:widget plotter) show)
    (fillhisto! h rndm)
    (fillhisto! h2 rndm2)
    (plot plotter h2)
    (plot plotter h)))

