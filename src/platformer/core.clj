(ns platformer.core
  (:use [clojure.contrib.duck-streams :only [pwd]])
  (:import [javax.swing JFrame JLabel JPanel]
	   [javax.imageio ImageIO]
	   [java.awt.image BufferedImage]
	   [java.awt.event ActionListener]
	   [java.awt GridLayout Graphics Graphics2D Color Dimension BasicStroke]
	   [java.awt.geom AffineTransform Ellipse2D$Double]))

(def *width* 400)
(def *height* 400)

(defrecord position-rec
  [x y])

(defn position [x y]
  (position-rec. x y))

(defprotocol game-object
  (game-position [obj]))

(defrecord circle-rec
  [pos radius]

  game-object
  (game-position [obj]
    (:pos obj)))

(defn circle [& {:keys [pos radius] :or {radius 5}}]
  (circle-rec. pos radius))

;; the world is an ordered array of layers that are drawn bottom to
;; top and apply their influences from bottom to top
(def *world* (atom []))

(defn- make-layer [n]
  (into [] (map (fn [_] (circle :pos (position (rand *width*)
					       (rand *height*))))
		(range n))))

(defn- make-world [old l]
  (into [] (map (fn [_] (make-layer 5)) (range l))))

(defn populate-sample-world []
  (swap! *world* make-world 3))

;; bound dynamically to the current graphics context
(def *g* nil)

(defmulti draw-object type)

(defmethod draw-object circle-rec [obj]
  (let [pos (game-position obj)
	rad (:radius obj)]
    (.draw *g* (Ellipse2D$Double. (:x pos) (:y pos) rad rad))))

(defrecord animator
  [enabled
  clock
  function
  sleep])

(defn make-animator [& {:keys [enabled clock function sleep]
			:or {enabled true
			     clock (System/currentTimeMillis)
			     function identity
			     sleep 100}}]
  (animator. enabled clock function sleep))

(defn- step-animator [animator agent]
  (if (:enabled animator)
    (let [start (System/currentTimeMillis)
	  dt (- start (:clock animator))
	  dt-secs (/ dt 1000)]

      ((:function animator) dt animator)

      (let [anim-time (- (System/currentTimeMillis) start)
	    sleep-time (max 0 (- (:sleep animator) anim-time))]
	(Thread/sleep sleep-time))

      (send-off agent #'step-animator agent)
      (conj animator {:clock start}))
    animator))

(defn- stop-animation [animator]
  (conj animator {:enabled false}))

(defn- start-animation [animator agent panel]
  (send-off agent #'step-animator agent)
  (conj animator {:enabled true :panel panel}))

(defn update-world [dt animator]
  (.repaint (:panel animator)))

(def *animator* (agent (make-animator :function #'update-world)))

(defn start-animator [panel]
  (send-off *animator* start-animation *animator* panel))

(defn stop-animator []
  (send-off *animator* stop-animation))

(defn draw-world [^Graphics2D g]
  (binding [*g* g]
    (doseq [layer @*world*
	    object layer]
      (draw-object object))))

(defn- box-panel []
  (proxy [JPanel] []
    (getPreferredSize [] (Dimension. *width* *height*))

    (paint [^Graphics2D g]
      (proxy-super paint g)
      (let [size (.getSize this)
	    width (.getWidth size)
	    height (.getHeight size)]

	(doto g
	  (.setStroke (BasicStroke. 1))
	  (.setColor Color/gray)
	  (#'draw-world))))))

(defn box []
  (let [frame (JFrame. "My Box")
	hello (JLabel. "Hello World")
	panel (box-panel)]
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    (start-animator panel)))

(defn get-resource [file]
  (println "getting resource " file)
  (let [loader (clojure.lang.RT/baseLoader)]
    (.getResourceAsStream loader file)))

(defn load-img [stream]
  ^BufferedImage (ImageIO/read stream))

(defn img-size  [^BufferedImage img]
  (list (.getWidth img) (.getHeight img)))

(defn draw-img [^Graphics2D g img pos]
  (let [^ImageObserver obs nil
	^double x (:x pos)
	^double y (:y pos)]
    (.drawImage g img x y obs)))

(def *resources* (atom {}))

(defn- cute [img]
  (load-img (get-resource (str "cute/" img))))

(defn load-resources []
  (swap! *resources*
    (fn [_] {:brown (cute "Brown Block.png")
	     :dirt (cute "Dirt Block.png")
	     :stone (cute "Stone Block.png")
	     :chest (cute "Chest Closed.png")
	     :shadow-east (cute "Shadow East.png")
	     :shadow-north-east (cute "Shadow North East.png")
	     :shadow-north-west (cute "Shadow North West.png")
	     :shadow-north (cute "Shadow North.png")
	     :shadow-side-west (cute "Shadow Side West.png")
	     :shadow-south-east (cute "Shadow South East.png")
	     :shadow-south-west (cute "Shadow South West.png")
	     :shadow-south (cute "Shadow South.png")
	     :shadow-west (cute "Shadow West.png")})))


(defn zip [& seqs]
  (apply map list seqs))

(defmacro doseq-idx [[[var idx] sequence] & body]
  `(let [seq2# (zip ~sequence (range (count ~sequence)))]
     (dotimes [item# seq2#]
       (let [[~var ~idx] item#]
	 ~@body))))

(def *scene*
     [[[:stone :stone :brown]
       [:stone :brown :brown :brown]
       [:stone :dirt :dirt]]
      [[:none :none :chest]
       [:none :none :none]
       [:none :dirt :dirt]]])

;; plane y spacing is 82
;; stacked y spacing is 40
;; draw order switched
(def *x-space* 100)
(def *y-space* 42)
(def *y-plane-space* 82)

(defn maybe-index [obj index]
  (if (and obj
	   (>= index 0)
	   (< index (count obj)))
    (obj index)))

(defn scene-index [scene pos]
  (let [[layeri rowi coli] pos]
    (maybe-index (maybe-index (maybe-index scene layeri) rowi) coli)))

(defn make-scene-index [layer row column]
  [layer row column])

(defn tile-add [pos offset]
  (into [] (map + pos offset)))

(defn shadow-with [shadow-type & tests]
  (fn [scene pos]
    (let [tests-pass (reduce #(and %1 (%2 scene pos)) true tests)]
      (if tests-pass
	shadow-type
	nil))))

(defn present [offset]
  (fn [scene tgt]
    (let [tile (scene-index scene (tile-add tgt offset))]
      (and tile (not (= tile :none))))))

(defn absent [offset]
  (fn [scene tgt]
    (let [tile (scene-index scene (tile-add tgt offset))]
      (or (not tile) (= tile :none)))))

(def *shadow-types*
     [(shadow-with :shadow-south-east
		   (present [1 1 1])
		   (absent [1 0 1]))
      (shadow-with :shadow-east
		   (present [1 0 1]))
      (shadow-with :shadow-north-east
		   (present [1 -1 1])
		   (absent [1 -1 0])
		   (absent [1 0 1]))
      (shadow-with :shadow-side-west
		   (present [0 1 -1])
		   (absent [0 1 0]))
      (shadow-with :shadow-south
		   (present [1 1 0]))
      (shadow-with :shadow-north
		   (present [1 -1 0]))
      (shadow-with :shadow-south-west
		   (present [1 1 -1])
		   (absent [1 0 -1]))
      (shadow-with :shadow-west
		   (present [1 0 -1]))
      (shadow-with :shadow-north-west
		   (present [1 -1 -1])
		   (absent [1 -1 0])
		   (absent [1 0 -1]))])

(defn shadow-types [scene pos]
  (reduce #(if-let [result (%2 scene pos)]
	     (conj %1 result)
	     %1)
	  [] *shadow-types*))

(defn draw-tile [^Graphics2D g key pos]
  (if-let [img (key @*resources*)]
    (let [[layeri rowi coli] pos
	  y-start (- 50 (* *y-space* layeri))
	  y (+ y-start (* *y-plane-space* rowi))
	  x (* *x-space* coli)]
      (draw-img g img (position x y)))))

(defn draw-world [^Graphics2D g]
  (doseq [[layer layeri] (zip *scene* (range (count *scene*)))
	  [row rowi] (zip layer (range (count layer)))
	  [img-key coli] (zip row (range (count row)))]

    (let [pos (make-scene-index layeri rowi coli)]
      (draw-tile g img-key pos)
      (if-let [shadows (shadow-types *scene* (make-scene-index layeri rowi coli))]
	(doseq [shadow shadows]
	  (draw-tile g shadow pos))))))
