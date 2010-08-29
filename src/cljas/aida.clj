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
(defn make-gaussian-generator []
  (let [rndm (Random.)]
    (repeatedly #(.nextGaussian rndm))))

(defn collect-randoms [n]
  (let [r (Random.)
	r-fn #(.nextGaussian r)]
    (repeatedly n r-fn)))

;; (defn collect-randoms [n]
;;   (let [r (Random.)]
;;     (loop [i n
;; 	   coll []]
;;       (cond (zero? i) coll
;;             true (recur (- i 1)
;;                         (conj coll (. r nextGaussian)))))))

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
	   tree (. tf create file "xml" false true)]
       (tree-init-factories aida-context
			    {:item "AIDA Tree"
			     :file file
			     :tree tree})))
  ([aida-context file & options]
     (let [tf (:tree-factory aida-context)]
       (cond (and (contains? options :read-only)
		  (not (contains? :create-new)))
	     (let [tree (. tf create file "xml" true false)]
	       (tree-init-factories aida-context
				    {:item "AIDA tree (read-only)"
				     :file file
				     :tree tree}))
	     (and (contains? options :create-new)
		  (not (contains? :read-only)))
	     (let [tree (. tf create file "xml" false true)]
	       (tree-init-factories aida-context
				    {:item "AIDA tree (new-file)"
				     :file file
				     :tree tree}))
	     true (println (str "Unknown options: " options))))))

(defn make-existing-tree [aida-context filename]
  (let [tf (:tree-factory aida-context)
	tree (. tf create filename "xml" true false)]
    (tree-init-factories aida-context
			 {:item "AIDA tree (read-only)"
			  :file filename
			  :tree tree})))

(defn make-persistent-tree [aida-context filename]
  (let [tf (:tree-factory aida-context)
	tree (. tf create filename "xml" false true)]
    (tree-init-factories aida-context
			 {:item "AIDA tree"
			  :file filename
			  :tree tree})))

(defn tuple-project-histo1d [tuple predicate evaluator histo]
  (let [mass-column (. tuple findColumn "A")
	charge-column (. tuple findColumn "Z")
	energy-column (. tuple findColumn "kinE")
	theta-column (. tuple findColumn "theta")]
    (do
      (. tuple start)
      (loop []
	(cond (. tuple next) (do
			       (let [mass (. tuple getInt mass-column)
				     charge (. tuple getInt charge-column)
				     energy (. tuple getFloat energy-column)
				     theta (. tuple getFloat theta-column)]
				 (if (predicate mass charge energy theta)
				   (histo1d-fill! histo (evaluator mass
								   charge
								   energy
								   theta))))
			       (recur))
	      true nil)))))

;; Selector: {:histo h :predicate p :evaluator e}
(defn tuple-project-histo1d-list! [tuple selectors]
  (let [mass-column (. tuple findColumn "A")
	charge-column (. tuple findColumn "Z")
	energy-column (. tuple findColumn "kinE")
	theta-column (. tuple findColumn "theta")]
    (do
      (. tuple start)
      (loop []
	(cond (. tuple next) (do
			       (let [mass (int (. tuple getInt mass-column))
				     charge (int (. tuple getInt charge-column))
				     energy (float (. tuple getFloat energy-column))
				     theta (float (. tuple getFloat theta-column))]
				 (doseq [selector selectors]
				   (let [h (:histogram selector)
					 p (:predicate selector)
					 e (:evaluator selector)]
				     (if (p mass charge energy theta)
				       (histo1d-fill! h (e mass charge energy theta))))))
			       (recur))
	      true nil)))))

(defn commit-tree [tree]
  (. (:tree tree) commit))

(defn close-tree [tree]
  (. (:tree tree) close))

(defn make-histogram1d [tree name nbins xmin xmax]
  (let [hf (:histogram-factory tree)]
    (. hf createHistogram1D name nbins xmin xmax)))

(defn calculate-log-binning [nbins xmin xmax]
  (let [l-xmin (Math/log xmin)
	l-xmax (Math/log xmax)
	fact (/ (- l-xmax l-xmin) nbins)
	edge-fn (fn [bin] (Math/exp (+ l-xmin (* fact bin))))]
    (map edge-fn (range (+ nbins 1)))))

(defn make-histogram1d-logx [tree name nbins xmin xmax]
  (let [edges (calculate-log-binning nbins xmin xmax)
	edges-array (double-array edges)
	hf (:histogram-factory tree)]
    (. hf createHistogram1D name name edges-array)))

(defn make-datapoint-set [tree x-coll y-coll]
  )

(defn region-apply-default-style [plotter-region]
  (let [region-style (.style plotter-region)]
    (doto region-style
      (.. dataStyle fillStyle (setVisible false))
      (.. dataStyle lineStyle (setVisible true))
      (.. dataStyle markerStyle (setVisible false))
      (.. dataStyle errorBarStyle (setVisible false))
      (.. dataStyle lineStyle (setColor "black"))
      (.. legendBoxStyle textStyle (setFont "Courier"))
      (.. legendBoxStyle textStyle (setFontSize 16))
      (.. statisticsBoxStyle textStyle (setFont "Courier"))
      (.. statisticsBoxStyle textStyle (setFontSize 16))
      (.. xAxisStyle labelStyle (setFont "Courier"))
      (.. xAxisStyle labelStyle (setFontSize 16))
      (.. xAxisStyle tickLabelStyle (setFont "Courier"))
      (.. xAxisStyle tickLabelStyle (setFontSize 16))
      (.. yAxisStyle labelStyle (setFont "Courier"))
      (.. yAxisStyle labelStyle (setFontSize 16))
      (.. yAxisStyle tickLabelStyle (setFont "Courier"))
      (.. yAxisStyle tickLabelStyle (setFontSize 16))
      )))

;;(defn plotter-apply-default-style! [plotter]
;;  (let [widget (:widget plotter)
	

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
;;	:plotter-style (ref (plotter-default-style))
	:current-region (ref 0)
	:widget plotter})))

(defn plot
  ([plotter plotter-region plottable]
     (let [widget (:widget plotter)
	   reg (. widget region plotter-region)]
       (. reg plot plottable)))
  ([plotter plotter-region plottable style]
     (let [widget (:widget plotter)
	   reg (.region widget plotter-region)]
       (. reg plot plottable style)))
  ([plotter plottable]
     (let [region-id (:current-region plotter)
	   widget (:widget plotter)]
       (plot plotter @(:current-region plotter) plottable))))

(defn tree-find-object [tree object-name]
  (let [t (:tree tree)
	o (.find t object-name)]
    o))

(defn fillhisto! [h coll]
  (loop [remaining-coll coll]
    (cond (empty? remaining-coll) true
	  true (do (. h fill (first remaining-coll))
		   (recur (rest remaining-coll))))))

;; ;; (defrecord Analysis [^hep.aida.IAnalysisFactory analysis-factory
;; ;; 		     ^hep.aida.ITreeFactory tree-factory
;; ;; 		     ^hep.aida.IPlotterFactory plotter-factory])
;; (defrecord Analysis [analysis-factory tree-factory plotter-factory])

;; (defn make-analysis []
;;   (let [af (IAnalysisFactory/create)
;; 	tf (.createTreeFactory af)
;; 	pf (.createPlotterFactory af)]
;;     (Analysis. af tf pf)))

;; (defrecord Tree [aida-tree
;; 		 histogram-factory
;; 		 tuple-factory
;; 		 in-memory?
;; 		 filename])

;; (defn make-tree
;;   ([analysis]
;;      (let [af (analysis :analysis-factory)
;; 	   t (. af
     
;;      (Tree. (create-tree analysis)

(defn aida-example []
  (let [aida (make-analysis)
	tree (make-tree aida)
	plotter (make-plotter aida)
	rndm (collect-randoms 12000)
	rndm2 (collect-randoms 12000)
	rndm3 (collect-randoms 30000)
	h (make-histogram1d tree "Fewer entries" 100 -5.0 5.0)
	h2 (make-histogram1d tree "More entries" 100 -5.0 5.0)
	h3 (make-histogram1d tree "Even more entries" 100 -5.0 5.0)]
    (. (:widget plotter) show)
    (fillhisto! h rndm)
    (fillhisto! h2 rndm2)
    (fillhisto! h3 rndm3)
    (plot plotter h3)
    (plot plotter h2)
    (plot plotter h)))

(use 'clojure.contrib.profile)

(defn example-analysis [aida output-tree tuple]
  (let [h1 (make-histogram1d output-tree
			     "Proton energies"
			     100 0.0 1500.0)
	h2 (make-histogram1d output-tree
			     "Alpha energies"
			     100 0.0 4000.0)
	h3 (make-histogram1d output-tree
			     "Helium isotopes"
			     10 0.5 10.5)
	h4 (make-histogram1d output-tree
			     "Neutron energies"
			     100 0.0 1500.0)
	proton? (fn [A Z E theta]
		  (let [A (int A)
			Z (int Z)
			E (float E)
			theta (float theta)]
		    (and (= A 1) (= Z 1))))
	neutron? (fn [A Z E theta]
		   (let [A (int A)
			 Z (int Z)
			 E (float E)
			 theta (float theta)]
		     (and (= A 1) (= Z 0))))
	alpha? (fn [A Z E theta]
		 (let [A (int A)
		       Z (int Z)
		       E (float E)
		       theta (float theta)]
		   (and (= A 4) (= Z 2))))
	helium? (fn [A Z E theta]
		  (let [A (int A)
			Z (int Z)
			E (float E)
			theta (float theta)]
		    (= Z 2)))
	energy-eval (fn [A Z E theta]
		      (let [A (int A)
			    Z (int Z)
			    E (float E)
			    theta (float theta)]
			E))
	mass-eval (fn [A Z E theta]
		    (let [A (int A)
			  Z (int Z)
			  E (float E)
			  theta (float theta)]
		      A))
	selectors [{:histogram h1
		    :predicate proton?
		    :evaluator energy-eval}
		   {:histogram h2
		    :predicate alpha?
		    :evaluator energy-eval}
		   {:histogram h3
		    :predicate helium?
		    :evaluator mass-eval}
		   {:histogram h4
		    :predicate neutron?
		    :evaluator energy-eval}]
	plotter (make-plotter aida 2 2)
	sty (region-apply-default-style (.region (:widget plotter) 0))]
    (do
      (.show (:widget plotter))
      (plot plotter 0 h1 sty)
      (plot plotter 1 h2 sty)
      (plot plotter 2 h3 sty)
      (plot plotter 3 h4 sty)
      (tuple-project-histo1d-list! tuple selectors))))