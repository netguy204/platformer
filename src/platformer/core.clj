(ns platformer.core
  (:use [clojure.contrib.duck-streams :only [pwd]])
  (:import [javax.swing JFrame JLabel JPanel]
	   [javax.imageio ImageIO]
	   [java.awt.image BufferedImage ImageObserver]
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
	x (:x pos)
	y (:y pos)]
    (.drawImage g ^BufferedImage img (int x) (int y) obs)))

(def *resources* (atom {}))

(defn load-img-compat [resource]
  (let [img (load-img resource)
	width (.getWidth img)
	height (.getHeight img)
	img2 (-> @*panel*
		 (.getGraphicsConfiguration)
		 (.createCompatibleImage width height (.getTransparency img)))
	g (.getGraphics img2)]
    (.drawImage g img 0 0 nil)
    img2))

(defn- cute [img & {:keys [properties] :or {properties [:shadow]}}]
  {:img (load-img-compat (get-resource (str "cute/" img)))
   :properties properties})

(defn load-resources []
  (swap! *resources*
    (fn [_] {:brown (cute "Brown Block.png")
	     :dirt (cute "Dirt Block.png")
	     :stone (cute "Stone Block.png")
	     :chest (cute "Chest Closed.png")
	     :tree-short (cute "Tree Short.png" :properties [:noshadow])
	     :wood (cute "Wood Block.png")
	     :wall (cute "Wall Block.png")
	     :grass (cute "Grass Block.png")
	     :character-boy (cute "Character Boy.png" :properties [:noshadow])
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
     [[[:grass :grass :grass :grass]
       [:stone :stone :brown :grass]
       [:stone :brown :brown :brown]
       [:stone :dirt :dirt]]
      [[:none :none :none :none]
       [:tree-short :none :chest]
       [:none :none :none :character-boy]
       [:none :dirt :dirt]]])

(def *panel* (atom nil))

;; plane y spacing is 82
;; stacked y spacing is 40
;; draw order switched
(def *x-space* 100)
(def *y-space* 40)
(def *y-plane-space* 80)

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
	shadow-type))))

(defn shadower? [tile]
  (if tile
    (let [res (tile @*resources*)]
      (and res (:properties res) (some #{:shadow} (:properties res))))))

(defn present [offset]
  (fn [scene tgt]
    (let [tile (scene-index scene (tile-add tgt offset))]
      (shadower? tile))))

(defn absent [offset]
  (fn [scene tgt]
    (let [tile (scene-index scene (tile-add tgt offset))]
      (not (shadower? tile)))))

(def uncovered (absent [1 0 0]))

(def *shadow-types*
     [(shadow-with :shadow-south-east
		   uncovered
		   (present [1 1 1])
		   (absent [1 0 1]))
      (shadow-with :shadow-east
		   uncovered
		   (present [1 0 1]))
      (shadow-with :shadow-north-east
		   uncovered
		   (present [1 -1 1])
		   (absent [1 -1 0])
		   (absent [1 0 1]))
      (shadow-with :shadow-side-west
		   (present [0 1 -1])
		   (absent [0 1 0]))
      (shadow-with :shadow-south
		   uncovered
		   (present [1 1 0]))
      (shadow-with :shadow-north
		   uncovered
		   (present [1 -1 0]))
      (shadow-with :shadow-south-west
		   uncovered
		   (present [1 1 -1])
		   (absent [1 0 -1]))
      (shadow-with :shadow-west
		   uncovered
		   (present [1 0 -1]))
      (shadow-with :shadow-north-west
		   uncovered
		   (present [1 -1 -1])
		   (absent [1 -1 0])
		   (absent [1 0 -1]))])

(defn shadow-types [scene pos]
  (reduce #(if-let [result (%2 scene pos)]
	     (conj %1 result)
	     %1)
	  [] *shadow-types*))

;; x increases to the right, y increases towards the back, z increases
;; as we go up. coordinates are normalized so that a typical brick is
;; 1x1x1
(defrecord position3d
  [x y z])

(defn p3d- [p1 p2]
  (let [{x1 :x y1 :y z1 :z} p1
	{x2 :x y2 :y z2 :z} p2]
    (position3d. (- x1 x2) (- y1 y2) (- z1 z2))))

(defn p3d+ [p1 p2]
  (let [{x1 :x y1 :y z1 :z} p1
	{x2 :x y2 :y z2 :z} p2]
    (position3d. (+ x1 x2) (+ y1 y2) (+ z1 z2))))

;; position of the camera in world coordinates the origin of world
;; coordinates is the bottom back left surface of the top left tile in
;; the first layer
;;
;; the camera object is the 3d point that should be centered in the
;; window when the world is projected to 2d
;;
(def *camera* (position3d. 1.4 -1.5 0))

(defn w3d-to-sc2d [pos3d]
  (let [{xc :x yc :y zc :z} (p3d- pos3d *camera*)
	y0 (* *y-space* zc)
	y (+ y0 (* *y-plane-space* yc))
	x (* *x-space* xc)]
    (position x y)))

(defn sc2d-to-g2d [pos]
  "convert from a frame where (0,0) is the center of the screen and y
increases as we go up to the frame demanded by Graphics2d"
  (let [{:keys [x y]} pos
	x0 (/ *width* 2)
	y0 (/ *height* 2)]

    (position (+ x x0)
	      (+ (- y) y0))))

(defn tc-to-te [pos]
  "convert a point from a frame with the origin at the center of the
tile to a frame with the origin at the top left of the tile"
  (let [mcx (- 101 (/ *x-space* 2))
	mcy (- 171 (/ *y-plane-space* 2) (/ *y-space* 2))
	{:keys [x y]} pos]
    (position (- x mcx) (- y mcy))))

(defn draw-tile [^Graphics2D g key pos]
  (if-let [img (key @*resources*)]
    (let [p2d (tc-to-te
	       (sc2d-to-g2d
		(w3d-to-sc2d pos)))]
		
      (draw-img g (:img img) p2d))))

(defn- draw-order-pos3d [p1 p2]
  (let [{y1 :y z1 :z} p1
	{y2 :y z2 :z} p2]
    (and (> y1 y2) (< z1 z2))))

(defn execute-draw [^Graphics2D g commands]
  (let [{:keys [background active overlay]} commands]
    ;; draw background objects in order given
    (doseq [[pos tile] background]
      (draw-tile g tile pos))

    ;; sort-then-draw the active tiles
    (let [active (sort-by first draw-order-pos3d active)]
      (doseq [[pos & tiles] active]
	(doseq [tile tiles]
	  (draw-tile g tile pos))))

    ;; ignore overlay
    ))

(defn scene-to-commands [scene]
  (for [[layer layeri] (zip *scene* (range (count scene)))
	[row rowi] (zip layer (range (count layer)))
	[img-key coli] (zip row (range (count row)))]
    (concat
     (list (position3d. coli (- rowi) layeri) img-key)
     (if (shadower? img-key)
       (shadow-types scene (make-scene-index layeri rowi coli))
       nil))))

(defn- decr-all [coll]
  (map #(- % 1) coll))

(defn generate-background-shadow-commands [scene]
  (let [ground (first scene)
	grows (count ground)
	gcols (apply max (map count ground))]
    (for [rii (decr-all (range (+ grows 2)))
	  cii (decr-all (range (+ gcols 2)))
	  shadow (shadow-types scene (make-scene-index -1 rii cii))]
      (list (position3d. cii (- rii) -1)
	    shadow))))

(defn draw-world [^Graphics2D g]
  (execute-draw g {:background (generate-background-shadow-commands *scene*)
		   :active (scene-to-commands *scene*)}))
		
(defn- box-panel []
  (proxy [JPanel] []
    (getPreferredSize [] (Dimension. *width* *height*))

    (paint [^Graphics2D g]
      (proxy-super paint g)
      (let [size (.getSize this)]
	(binding [*width* (.getWidth size)
		  *height* (.getHeight size)]

	  (doto g
	    (.setStroke (BasicStroke. 1))
	    (.setColor Color/gray)
	    (#'draw-world)))))))

(defn box []
  (let [frame (JFrame. "My Box")
	hello (JLabel. "Hello World")
	panel (box-panel)]
    
    (swap! *panel* (fn [_] panel))
    
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    (start-animator panel)))

(defn -main [& args]
  (box)
  (load-resources))

