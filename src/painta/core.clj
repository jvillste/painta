(ns painta.core
  (:require [clojure.spec.test :as spec-test]
            [clojure.spec :as spec]
            (fungl [atom-registry :as atom-registry]
                   [application :as application]
                   [layouts :as layouts]
                   [layout :as layout]
                   [cache :as cache]
                   [handler :as handler])
            (flow-gl.gui
             [scene-graph :as scene-graph]
             [keyboard :as keyboard]
             [visuals :as visuals]
             [quad-renderer :as quad-renderer]
             [render-target-renderer :as render-target-renderer]
             [animation :as animation]
             [stateful :as stateful])
            (flow-gl.opengl.jogl [opengl :as opengl]
                                 [quad :as quad]
                                 [texture :as texture]
                                 [render-target :as render-target])
            (flow-gl.graphics [font :as font]
                              [buffered-image :as buffered-image]
                              [text :as text])
            [painta.ffmpeg :as ffmpeg])
  (:import  [javax.imageio ImageIO]
            [java.io File]
            [java.awt.event KeyEvent])
  (:use clojure.test))

(def fragment-shader-source "
  #version 140

  uniform int number_of_points;

  uniform sampler2D texture;

  uniform vec2 points[50];

  uniform vec4 paint_color;

  in vec2 texture_coordinate;

  out vec4 outColor;

  // from https://www.shadertoy.com/view/4dfXDn
  float distance_to_line(vec2 p, vec2 start, vec2 end, float width)
  {
	vec2 dir = start - end;
	float lngth = length(dir);
	dir /= lngth;
	vec2 proj = max(0.0, min(lngth, dot((start - p), dir))) * dir;
	return length( (start - p) - proj ) - (width / 2.0);
  }

  void main() {
    float distance = 1;
    for (int i = 0; i < (number_of_points -1); i++){
      distance = min(distance, distance_to_line(texture_coordinate, points[i], points[i+1], 0.001) / 0.005);
    }
    distance = min(1,distance);

     vec2 flipped_texture_coordinate = vec2(texture_coordinate.x, 1.0 - texture_coordinate.y);
     vec4 texture_color = texture(texture, flipped_texture_coordinate);

     outColor = (texture_color * distance) + (paint_color * (1.0 - distance));


    //outColor = vec4(0,0,0, distance); //max( 0.0, 1.0 - distance));
  }
  ")

(def diff-fragment-shader-source "
  #version 140

  uniform sampler2D target_texture;
  uniform sampler2D source_texture;
  uniform float target_scale;
  uniform float target_x;
  uniform float target_y;

  in vec2 texture_coordinate;

  out vec4 outColor;

  void main() {

      vec2 flipped_texture_coordinate = vec2(texture_coordinate.x, 1.0 - texture_coordinate.y);

     // outColor = vec4(1,0,0, abs(texture(target_texture, texture_coordinate).a - texture(source_texture, flipped_texture_coordinate).a));

  //  vec4 difference = texture(target_texture, texture_coordinate) - texture(source_texture, flipped_texture_coordinate);
  
  //  outColor = vec4(1,0,0, abs( (difference.r + difference.g + difference.b) / 3.0));
  vec2 target_coordinates = vec2(target_scale * texture_coordinate.x   - target_x, target_scale * texture_coordinate.y  - target_y);
  vec4 target_color = vec4(1,1,1,1);
  if(target_coordinates.x >= 0.0  && target_coordinates.x <= 1.0 && target_coordinates.y >= 0.0  && target_coordinates.y <= 1.0){
    target_color = texture(target_texture, target_coordinates);
  }
//   vec4 target_color = texture(target_texture, texture_coordinate);
  vec4 source_color = texture(source_texture, flipped_texture_coordinate);

  outColor = (target_color / 2.0) + (source_color / 2.0);
  

  }
  ")

(defn create-render-target [gl width height]
  (let [render-target (render-target/create width height gl)]
    (render-target/render-to render-target gl
                             (opengl/clear gl 1 1 1 1))
    render-target))

(defn canvas-state-atom-specification [gl width height]
  {:create (fn []
             {:source (create-render-target gl width height)
              :target (create-render-target gl width height)})
   :delete (fn [state-atom]
             (render-target/delete (:source @state-atom) gl)
             (render-target/delete (:target @state-atom) gl))})

(defn swap-and-return-old-and-new! [atom f & args]
  (loop []
    (let [old @atom
          new (apply f old args)]
      (if (compare-and-set! atom old new)
        [old new]
        (recur)))))


(defn diff-view [target-buffered-image canvas-state-id target-scale target-x target-y diff-width diff-height]
  {:width diff-width
   :height diff-height
   :render (fn [scene-graph gl]
             (let [render-target-renderer-atom (atom-registry/get! [:diff diff-width diff-height]  (render-target-renderer/atom-specification gl))
                   canvas-state-atom (atom-registry/get! canvas-state-id)]
               (render-target-renderer/render render-target-renderer-atom gl scene-graph
                                              (fn []
                                                (opengl/clear gl 1 1 1 1)
                                                (when canvas-state-atom
                                                  (quad/draw gl
                                                             ["target_texture" (cache/call-with-key! texture/create-for-buffered-image
                                                                                                     target-buffered-image
                                                                                                     target-buffered-image
                                                                                                     gl)
                                                              "source_texture" (:texture (:target @canvas-state-atom))]
                                                             [:1f "target_scale" target-scale
                                                              :1f "target_x" target-x
                                                              :1f "target_y" target-y]
                                                             (cache/call-with-key! quad/create-program
                                                                                   diff-fragment-shader-source
                                                                                   diff-fragment-shader-source
                                                                                   gl)
                                                             0 0
                                                             diff-width
                                                             diff-height
                                                             diff-width
                                                             diff-height))))))})

#_(def target-buffered-image (buffered-image/create-from-file #_"pumpkin.png" "/Users/jukka/Pictures/how-to-draw-a-dragon-from-skyrim-step-9_1_000000153036_5.gif"))

#_(def target-buffered-image
    (let [original-image (ImageIO/read (File. "/Users/jukka/Pictures/how-to-draw-a-dragon-from-skyrim-step-9_1_000000153036_5.gif"))
          size (max (.getWidth original-image) (.getHeight original-image))
          new-image (buffered-image/create size
                                           size)]
      (.drawImage (buffered-image/get-graphics new-image)
                  original-image
                  nil
                  0
                  0)
      new-image))

(defn resize-max [original-image maximum-size]
  (let [original-width (.getWidth original-image)
        original-height (.getHeight original-image)
        maximum-size (max maximum-size
                          original-width
                          original-height)
        aspect-ratio (/ original-width
                        original-height)
        width (if (> original-width
                     original-height)
                maximum-size
                (* maximum-size aspect-ratio))
        height (if (> original-width
                      original-height)
                 (/ maximum-size aspect-ratio)
                 maximum-size)
        new-image (buffered-image/create maximum-size maximum-size)]
    (.drawImage (buffered-image/get-graphics new-image)
                original-image
                0
                0
                width
                height
                nil)
    new-image))

(def target-buffered-image (resize-max (ffmpeg/extract-frame "/Users/jukka/Pictures/video/2016-04-15.12.58.20_eb239942895b57b8773875a12e6fbe26.mp4"
                                                             "00:00:01")
                                       2000))

#_(def target-buffered-image #_(buffered-image/create-from-file "pumpkin.png")
    (let [font (font/create "LiberationSans-Regular.ttf" 100)
          text "O"
          color [0 0 0 255]
          size (max 1
                    (font/width font text)
                    (font/height font))
          buffered-image (buffered-image/create size size)]
      (buffered-image/clear buffered-image 255 255 255 255)
      (text/draw (buffered-image/get-graphics buffered-image)
                 color
                 font
                 text)
      buffered-image))



(defn create-events-set [& events]
  (apply sorted-set-by (fn [a b]
                         (compare (:time a)
                                  (:time b)))
         events))

(defn partition-on [predicate xs]
  (loop [xs xs
         partitions []
         partition []]
    (if-let [x (first xs)]
      (if (predicate x)
        (recur (rest xs)
               (if (empty? partition)
                 partitions
                 (conj partitions partition))
               [x])
        (recur (rest xs)
               partitions
               (conj partition x)))
      (if (empty? partition)
        partitions
        (conj partitions partition)))))

(deftest partition-on-test
  (is (= [[1 1] [2 1 1 1] [2]]
         (partition-on even? [1 1 2 1 1 1 2])))

  (is (= [[2 1] [2 1 1] [2]]
         (partition-on even? [2 1 2 1 1 2])))

  (is (= []
         (partition-on even? []))))

(defn strokes [events last-painted-event]
  (->> (if last-painted-event
         (subseq events >= last-painted-event)
         events)
       (partition-on (fn [event]
                       (= :start-stroke
                          (:type event))))))

(deftest strokes-test
  (let [stroke-1 [{:time 1 :type :start-stroke}
                  {:time 2 :type :draw-stroke}
                  {:time 3 :type :draw-stroke}
                  {:time 4 :type :end-stroke}]
        stroke-2 [{:time 10 :type :start-stroke}
                  {:time 11 :type :draw-stroke}
                  {:time 12 :type :end-stroke}]]
    (is (= [[{:time 1, :type :start-stroke}
             {:time 2, :type :draw-stroke}
             {:time 3, :type :draw-stroke}
             {:time 4, :type :end-stroke}]]
           (strokes (apply create-events-set stroke-1)
                    nil)))

    (is (= [stroke-1
            stroke-2]
           (strokes (apply create-events-set (concat stroke-1
                                                     stroke-2))
                    nil)))

    (is (= [[{:time 3, :type :draw-stroke}
             {:time 4, :type :end-stroke}]]
           (strokes (apply create-events-set stroke-1)
                    {:time 3 :type :draw-stroke})))

    (is (= [[{:time 3 :type :draw-stroke}]]
           (strokes (create-events-set {:time 3, :type :draw-stroke})
                    {:time 3 :type :draw-stroke})))

    (is (= []
           (strokes (create-events-set)
                    {:time 3 :type :draw-stroke})))))


(defn draw-stroke [events canvas-state-atom gl]
  (let [width (:width (:target @canvas-state-atom))
        height (:height (:target @canvas-state-atom))
        coordinates (apply concat (map (fn [event]
                                         [(float (/ (:x event)
                                                    width))
                                          (float (/ (:y event)
                                                    height))])
                                       (filter (fn [event]
                                                 (not= :end-stroke (:type event)))
                                               events)))]
    (render-target/render-to (:target @canvas-state-atom) gl
                             (let [program (cache/call-with-key! quad/create-program
                                                                 fragment-shader-source
                                                                 fragment-shader-source
                                                                 gl)]
                               (quad/draw gl
                                          ["texture" (:texture (:source @canvas-state-atom))]
                                          [:2fv "points" coordinates
                                           :1i "number_of_points" (/ (count coordinates)
                                                                     2)
                                           :4f "paint_color" (:color (first events))]
                                          
                                          program
                                          0 0
                                          width
                                          height
                                          width
                                          height)))

    (swap! canvas-state-atom (fn [state]
                               (assoc state
                                      :source (:target state)
                                      :target (:source state))))))

(handler/def-handler-creator create-canvas-renderer [events canvas-state-id] [scene-graph gl]
  (let [canvas-state-atom (atom-registry/get! canvas-state-id
                                              (canvas-state-atom-specification gl
                                                                               (:width scene-graph)
                                                                               (:height scene-graph)))
        strokes (strokes events (:last-painted-event @canvas-state-atom))]
    
    (doseq [stroke strokes]
      (when (not= [(:last-painted-event @canvas-state-atom)]
                  stroke)
        
        (draw-stroke (if (= 1 (count stroke))
                       [(first stroke)
                        (first stroke)]
                       stroke)
                     canvas-state-atom
                     gl)))

    (swap! canvas-state-atom assoc :last-painted-event (last (last strokes)))

    (assoc (select-keys scene-graph [:x :y :width :height])
           :texture-id (:texture (:source  @canvas-state-atom))
           :texture-hash (hash scene-graph))))

(handler/def-handler-creator create-canvas-mouse-event-handler [event-state-atom] [node event]
  (when-let [paint-event (case (:type event)
                           :mouse-pressed {:type :start-stroke
                                           :color (:paint-color @event-state-atom)
                                           :x (:local-x event)
                                           :y (:local-y event)
                                           :time (:time event)}

                           :mouse-dragged {:type :draw-stroke
                                           :color (:paint-color @event-state-atom)
                                           :x (:local-x event)
                                           :y (:local-y event)
                                           :time (:time event)}

                           :mouse-released {:type :end-stroke
                                            :x (:local-x event)
                                            :y (:local-y event)
                                            :time (:time event)}

                           nil)]
    (swap! event-state-atom update :events conj paint-event))
  event)

(def space 32)
(def d-key-code 68)

(defn pressed? [event key-code]
  (and (= key-code (:key-code event))
       (= :key-pressed (:type event))
       (not (:is-auto-repeat event))))

(defn released? [event key-code]
  (and (= key-code (:key-code event))
       (= :key-released (:type event))
       (not (:is-auto-repeat event))))


(defn toggle-on-key [state key-code key event]
  (if (pressed? event key-code)
    (assoc state key true)
    (if (released? event key-code)
      (assoc state key false)
      state)))


(handler/def-handler-creator create-keyboard-event-handler [event-state-atom] [event]
  (when (pressed? event space)
    (swap! event-state-atom assoc :paint-color [1 1 1 1]))

  (when (released? event space)
    (swap! event-state-atom assoc :paint-color [0 0 0 1]))

  (swap! event-state-atom
         (fn [state]
           (-> state
               (toggle-on-key KeyEvent/VK_D :show-diff event)
               (toggle-on-key KeyEvent/VK_M :move event)
               (toggle-on-key KeyEvent/VK_S :scale event)))))

(defn with-borders [child]
  (layouts/box 10 (visuals/rectangle [0 0 0 255] 20 20)
               child))

(defn scale-quad [scale quad]
  (-> quad
      (update :width * scale)
      (update :height * scale)
      (update :x * scale)
      (update :y * scale)))

(defn zoom-pane [id width height scale x y child]
  {:children [(layouts/with-preferred-size child)]
   :id id
   :width width
   :height height
   :render (fn [scene-graph gl]
             (let [width (:width scene-graph)
                   height (:height scene-graph)
                   render-target-renderer-atom (atom-registry/get! [id :render-target-renderer] (render-target-renderer/atom-specification gl))
                   render-target-atom (atom-registry/get! [id :render-target width height] (render-target/atom-specification width height gl))
                   quad-renderer-atom (atom-registry/get! [id :quad-renderer] (quad-renderer/atom-specification gl))
                   quads (map (fn [quad]
                                (-> quad
                                    (update :x + x)
                                    (update :y + y)))
                              (filter (partial scene-graph/in-region? x y width height)
                                      (map (partial scale-quad scale)
                                           (scene-graph/leaf-nodes scene-graph))))]
               (render-target/render-to @render-target-atom gl
                                        (let [{:keys [width height]} (opengl/size gl)]
                                          (opengl/clear gl 1 1 1 1)
                                          (swap! quad-renderer-atom
                                                 quad-renderer/draw
                                                 quads 
                                                 width
                                                 height
                                                 gl)))
               {:x (:x scene-graph)
                :y (:y scene-graph)
                :width width
                :height height
                :texture-id (:texture @render-target-atom)
                :texture-hash (hash quads)}))})
(defn mouse-movement [event-state event]
  {:x (- (or (:previous-mouse-x event-state)
             (:local-x event))
         (:local-x event))
   :y (- (or (:previous-mouse-y event-state)
             (:local-y event))
         (:local-y event))})

(handler/def-handler-creator create-target-mouse-event-handler [event-state-atom] [node event]

  (when (:local-x event)
    (let [movement (mouse-movement @event-state-atom event)]
      (when (and (:move @event-state-atom)
                 (= :mouse-dragged (:type event)))
        (swap! event-state-atom (fn [state]
                                  (-> state
                                      (update :target-x (fnil + 0 0) (- (:x movement)))
                                      (update :target-y (fnil + 0 0) (- (:y movement)))))))

      (when (and (:scale @event-state-atom)
                 (= :mouse-dragged (:type event)))
        (swap! event-state-atom (fn [state]
                                  (-> state
                                      (update :target-scale (fnil + 0 0) (* 0.01 (- (:y movement)))))))))

    (swap! event-state-atom (fn [state]
                              (assoc state
                                     :previous-mouse-x (:local-x event)
                                     :previous-mouse-y (:local-y event)))))
  
  event)


(defn create-scene-graph [width height]
  (let [target-width 300 #_(.getWidth target-buffered-image)
        target-height 300 #_(.getHeight target-buffered-image) 
        canvas-width 300
        canvas-height 300
        event-state-atom (atom-registry/get! :state {:create (fn [] {:events (create-events-set)
                                                                     :paint-color [0 0 0 1]
                                                                     :target-x 0
                                                                     :target-y 0
                                                                     :target-scale 0.2})})
        event-state @event-state-atom
        target-zoom-pane (fn [id content]
                           (cache/call! zoom-pane
                                        id
                                        target-width
                                        target-height
                                        (:target-scale event-state)
                                        (:target-x event-state)
                                        (:target-y event-state)
                                        content))
        
        canvas-state-id [:canvas-state canvas-width canvas-height]]
    (prn (:target-x event-state) (:target-scale event-state))
    (animation/swap-state! animation/set-wake-up 1000)
    (keyboard/set-focused-event-handler! (create-keyboard-event-handler event-state-atom))
    (-> {:x 10
         :y 10
         :width width
         :height height
         :children [(assoc (visuals/rectangle [255 255 255 255] 0 0)
                           :width width
                           :height height)
                    (layouts/vertically-with-margin 10
                                                    (with-borders
                                                      (assoc (target-zoom-pane :target-zoom-pane
                                                                               (visuals/image target-buffered-image))

                                                             :mouse-event-handler (create-target-mouse-event-handler event-state-atom)))
                                                    (with-borders
                                                      (if true #_(:show-diff @event-state-atom)
                                                          (diff-view target-buffered-image
                                                                     canvas-state-id
                                                                     (:target-scale event-state)
                                                                     (/ (:target-x event-state)
                                                                            target-width)
                                                                     (/ (:target-y event-state)
                                                                            target-height)
                                                                     canvas-width
                                                                     canvas-height)))
                                                    (with-borders
                                                      {:x 0
                                                       :y 0
                                                       :width canvas-width
                                                       :height canvas-width
                                                       :id :canvas
                                                       :mouse-event-handler (create-canvas-mouse-event-handler event-state-atom)
                                                       :render (create-canvas-renderer (:events @event-state-atom) canvas-state-id)}))]}
        (application/do-layout width height))))

(defn start []
  (.start (Thread. (fn []
                     (application/start-window #'create-scene-graph)))))

