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
			     sleep 10}}]
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
    (println "hello1" img)
    (.drawImage g img x y obs)))

(def *resources* (atom {}))

(defn- cute [img]
  (load-img (get-resource (str "cute/" img))))

(defn load-resources []
  (swap! *resources*
    (fn [_] {:brown (cute "Brown Block.png")
	     :dirt (cute "Dirt Block.png")})))


;; plane y spacing is 82
;; stacked y spacing is 40
;; draw order switched
(def *x-space* 100)
(def *y-space* 40)

(defn draw-world [^Graphics2D g]
  (dotimes [x 5]
    (dotimes [y 5]
      (draw-img g (:dirt @*resources*)
		(position (+ 10 (* x *x-space*)) (- 200 (* y *y-space*)))))))
