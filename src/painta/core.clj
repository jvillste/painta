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
                                 [window :as window]
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

(def draw-stroke-fragment-shader-source "
  #version 140

  uniform int number_of_points;
  uniform sampler2D texture;
  uniform vec2 points[50];
  uniform vec3 paint_color;
  uniform float line_width;

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
      distance = min(distance, distance_to_line(texture_coordinate, points[i], points[i+1], line_width) / 0.005);
    }
    distance = min(1,distance);

     vec2 flipped_texture_coordinate = vec2(texture_coordinate.x, 1.0 - texture_coordinate.y);
     vec4 texture_color = texture(texture, flipped_texture_coordinate);
  
     outColor = texture_color * distance + vec4(paint_color * (1.0 - distance), (1.0 - distance));
     // outColor = vec4((texture_color * distance * texture_color.a).rgb + (paint_color * (1.0 - distance) * (1.0 - texture_color.a)), max((1.0 - distance), texture_color.a));
  //     outColor.a = (1.0 - distance);
  //     outColor.a = 1.0;


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
  uniform float target_width;
  uniform float target_height;
  uniform float diff_width;
  uniform float diff_height;

  in vec2 texture_coordinate;

  out vec4 outColor;

  void main() {

      vec2 flipped_texture_coordinate = vec2(texture_coordinate.x, 1.0 - texture_coordinate.y);

     // outColor = vec4(1,0,0, abs(texture(target_texture, texture_coordinate).a - texture(source_texture, flipped_texture_coordinate).a));

  //  vec4 difference = texture(target_texture, texture_coordinate) - texture(source_texture, flipped_texture_coordinate);
  
  //  outColor = vec4(1,0,0, abs( (difference.r + difference.g + difference.b) / 3.0));
  vec2 target_coordinates = vec2((texture_coordinate.x / target_scale  - target_x),
                                  (texture_coordinate.y * diff_height - target_y * target_height) / target_height / target_scale);

  vec4 target_color = vec4(1,1,1,1);
  if(target_coordinates.x >= 0.0  && target_coordinates.x <= 1.0 && target_coordinates.y >= 0.0  && target_coordinates.y <= 1.0){
    target_color = texture(target_texture, target_coordinates);
  }
  // target_color = texture(target_texture, texture_coordinate);
  vec4 source_color = texture(source_texture, flipped_texture_coordinate);

  outColor = (target_color / 2.0) + (source_color / 2.0);
  

  }
  ")

(def grid-fragment-shader-source "
  #version 140

  uniform float x1;
  uniform float dx;
  uniform float y1;
  uniform float dy;
  uniform float line_width;
  uniform int highlight_x;
  uniform int highlight_y;

  in vec2 texture_coordinate;

  out vec4 outColor;

  void main() {
  float half_line_width = line_width / 2.0;
  float modulus_x = mod(texture_coordinate.x - x1 , dx);
  float modulus_y = mod(texture_coordinate.y - y1 , dy);
    if(   (modulus_x >= 0 && modulus_x <= half_line_width) || (dx - modulus_x >= 0 && dx - modulus_x <= half_line_width)
       || (modulus_y >= 0 && modulus_y <= half_line_width) || (dy - modulus_y >= 0 && dy - modulus_y <= half_line_width)){
      outColor = vec4(0,0,0,0.8);
    }else{
      if(highlight_x == int(floor((texture_coordinate.x - modulus_x) / dx))
      && highlight_y == int(floor((texture_coordinate.y - modulus_y) / dy))){
        outColor = vec4(0,0,0,0);
      } else {
        outColor = vec4(0.5,0,0,0.4);
      }
    }
  }
  ")

(defn create-render-target [gl width height]
  (let [render-target (render-target/create width height gl)]
    (render-target/render-to render-target gl
                             (opengl/clear gl 1 1 1 0))
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
                                                              :1f "target_y" target-y
                                                              :1f "target_width" (.getWidth target-buffered-image)
                                                              :1f "target_height" (.getHeight target-buffered-image)
                                                              :1f "diff_width" diff-width
                                                              :1f "diff_height" diff-height]
                                                             
                                                             (cache/call-with-key! quad/create-program
                                                                                   diff-fragment-shader-source
                                                                                   diff-fragment-shader-source
                                                                                   gl)
                                                             0 0
                                                             diff-width
                                                             diff-height
                                                             diff-width
                                                             diff-height))))))})


(defn grid-view [id x1 y1 x2 y2 highlight_x highlight_y width height]
  {:width width
   :height height
   :render (fn [scene-graph gl]
             (let [render-target-atom (atom-registry/get! [id :render-target width height] (render-target/atom-specification width height gl))]
               (render-target/render-to @render-target-atom gl
                                        (opengl/clear gl 1 1 1 0)
                                        (quad/draw gl
                                                   []
                                                   [:1f "x1" x1
                                                    :1f "dx" (-  x2 x1) 
                                                    :1f "y1" y1
                                                    :1f "dy" (-  y2 y1) 
                                                    :1f "line_width" (/ 1.1 width)
                                                    :1i "highlight_x" highlight_x
                                                    :1i "highlight_y" highlight_y]
                                                   
                                                   (cache/call-with-key! quad/create-program
                                                                         grid-fragment-shader-source
                                                                         grid-fragment-shader-source
                                                                         gl)
                                                   0 0
                                                   width
                                                   height
                                                   width
                                                   height))
               #_(visuals/image target-buffered-image)
               {:x (:x scene-graph)
                :y (:y scene-graph)
                :width width
                :height height
                :texture-id (:texture @render-target-atom)
                :texture-hash  (rand 10000) #_(hash [x1 y1 x2 y2 width height grid-fragment-shader-source])}))})

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

(defonce target-buffered-image (resize-max (buffered-image/create-from-file "/Users/jukka/Downloads/horse-02.jpg" #_"/Users/jukka/Downloads/Screen Shot 2017-04-29 at 05.06.00.png")
                                           #_(ffmpeg/extract-frame "/Users/jukka/Pictures/video/2016-04-15.12.58.20_eb239942895b57b8773875a12e6fbe26.mp4"
                                                                   "00:00:01")
                                           300))


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
                                                                 draw-stroke-fragment-shader-source
                                                                 draw-stroke-fragment-shader-source
                                                                 gl)]
                               (quad/draw gl
                                          ["texture" (:texture (:source @canvas-state-atom))]
                                          [:2fv "points" coordinates
                                           :1i "number_of_points" (/ (count coordinates)
                                                                     2)
                                           :3f "paint_color" (:color (first events))
                                           :1f "line_width" (/ (:line-width (first events))
                                                               width)]
                                          
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

(defn mouse-movement [event-state event]
  {:x (- (or (:previous-mouse-x event-state)
             (:local-x event))
         (:local-x event))
   :y (- (or (:previous-mouse-y event-state)
             (:local-y event))
         (:local-y event))})

(defn set-highlight [event-state event width height]

  (let [cell-width (- (:grid-x2 event-state)
                      (:grid-x1 event-state))
        cell-height (- (:grid-y2 event-state)
                       (:grid-y1 event-state))]
    

    (assoc event-state
           :highlight-x (Math/floor (/ (- (/ (:local-x event)
                                             width)
                                          (mod (:grid-x1 event-state)
                                               cell-width))
                                       
                                       cell-width))
           :highlight-y (Math/floor (/ (- (/ (:local-y event)
                                             height)
                                          (mod (:grid-y1 event-state)
                                               cell-height))
                                       
                                       cell-height)))))

(handler/def-handler-creator create-canvas-mouse-event-handler [event-state-atom width height] [node event]
  (when-let [paint-event (case (:type event)
                           :mouse-pressed {:type :start-stroke
                                           :color (:paint-color @event-state-atom)
                                           :line-width (:line-width @event-state-atom)
                                           :x (:local-x event)
                                           :y (:local-y event)
                                           :time (:time event)}

                           :mouse-dragged {:type :draw-stroke
                                           :color (:paint-color @event-state-atom)
                                           :line-width (:line-width @event-state-atom)
                                           :x (:local-x event)
                                           :y (:local-y event)
                                           :time (:time event)}

                           :mouse-released {:type :end-stroke
                                            :x (:local-x event)
                                            :y (:local-y event)
                                            :time (:time event)}

                           nil)]
    (swap! event-state-atom update :events conj paint-event))
  
  (when (:local-x event)
    (when (and (:scale @event-state-atom)
               (= :mouse-dragged (:type event)))
      (swap! event-state-atom (fn [state]
                                (let [scale-delta (* 0.01
                                                     (- (:y (mouse-movement @event-state-atom event))))]
                                  (-> state
                                      (update :canvas-scale
                                              (fnil + 0 0)
                                              scale-delta))))))
    (swap! event-state-atom (fn [state]
                              (-> state
                                  (assoc :previous-mouse-x (:local-x event)
                                         :previous-mouse-y (:local-y event))
                                  (set-highlight event width height)))))
  
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

(defn toggle-premanently-on-key [state key-code key event]
  (if (pressed? event key-code)
    (update state key not)
    state))


(handler/def-handler-creator create-keyboard-event-handler [event-state-atom] [event]
  (when (pressed? event space)
    (swap! event-state-atom assoc
           :paint-color [1 1 1]
           :line-width 10))

  (when (released? event space)
    (swap! event-state-atom assoc
           :paint-color [0 0 0]
           :line-width 1))

  (swap! event-state-atom
         (fn [state]
           (-> state
               (toggle-premanently-on-key KeyEvent/VK_D :show-diff event)
               (toggle-on-key KeyEvent/VK_M :move event)
               (toggle-on-key KeyEvent/VK_S :scale event)
               (toggle-premanently-on-key KeyEvent/VK_G :show-grid event)))))

(defn with-borders [child]
  (layouts/box 10 (visuals/rectangle [0 0 0 255] 20 20)
               child))

(defn scale-quad [scale quad]
  (-> quad
      (update :width * scale)
      (update :width long)
      (update :height * scale)
      (update :height long)
      (update :x * scale)
      (update :y * scale)))

(defn zoom-pane [id width height scale x y child]
  (let [content-width 1000
        pixel-size (* (/ width
                         content-width)
                      scale)
        x-in-pixels (* x width)
        y-in-pixels (* y width)]
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
                                      (update :x + x-in-pixels)
                                      (update :y + y-in-pixels)))
                                (filter (partial scene-graph/in-region? x-in-pixels y-in-pixels width height)
                                        (map (partial scale-quad pixel-size)
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
                  :texture-hash (hash quads)}))}))






(handler/def-handler-creator create-target-mouse-event-handler
  [event-state-atom
   target-width
   target-height]
  [node event]

  (when (:local-x event)
    (when (= :mouse-pressed (:type event))
      (swap! event-state-atom assoc
             :mouse-pressed-x (/ (:local-x event)
                                 target-width)
             :mouse-pressed-y (/ (:local-y event)
                                 target-height)))
    
    (let [movement (mouse-movement @event-state-atom event)]
      (when (and (:move @event-state-atom)
                 (= :mouse-dragged (:type event)))
        (swap! event-state-atom (fn [state]
                                  (-> state
                                      (update :target-x (fnil + 0.0 0.0) (- (/ (:x movement)
                                                                               target-width)))
                                      (update :target-y (fnil + 0.0 0.0) (- (/ (:y movement)
                                                                               target-height)))))))

      (when (and (:scale @event-state-atom)
                 (= :mouse-dragged (:type event)))
        (swap! event-state-atom (fn [state]
                                  (let [scale-delta (* 0.01 (- (:y movement)))]
                                    (prn (:mouse-pressed-x state))
                                    (-> state
                                        (update :target-scale (fnil + 0 0) scale-delta)
                                        (update :target-x (fn [target-x]
                                                            (- target-x
                                                               (* (:mouse-pressed-x state)
                                                                  scale-delta))))
                                        (update :target-y (fn [target-y]
                                                            (- target-y
                                                               (* (:mouse-pressed-y state)
                                                                  scale-delta))))))))))

    (swap! event-state-atom (fn [state]
                              (-> state
                                  (assoc :previous-mouse-x (:local-x event)
                                         :previous-mouse-y (:local-y event))
                                  (set-highlight event target-width target-height)))))
  
  event)


(defn create-scene-graph [width height]
  (let [target-width 300 #_(.getWidth target-buffered-image)
        target-height target-width #_(.getHeight target-buffered-image) 
        canvas-width 700
        canvas-height canvas-width

        event-state-atom (atom-registry/get! :state {:create (fn [] {:events (create-events-set)
                                                                     :paint-color [0 0 0]
                                                                     :target-x 0.0
                                                                     :target-y 0.0
                                                                     :line-width 1
                                                                     :target-scale 1.0
                                                                     :canvas-scale 1.0
                                                                     :grid-x1 0.1
                                                                     :grid-y1 0.1
                                                                     :grid-x2 0.3
                                                                     :grid-y2 0.3
                                                                     :highlight-x 0
                                                                     :highlight-y 0})})
        event-state @event-state-atom
        scale (* (:target-scale event-state)
                 (:canvas-scale event-state))
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
                                                      (layouts/superimpose
                                                       (assoc (cache/call! zoom-pane
                                                                           :target-zoom-pane
                                                                           target-width
                                                                           target-height
                                                                           scale
                                                                           (:target-x event-state)
                                                                           (:target-y event-state)
                                                                           (visuals/image target-buffered-image))


                                                              :mouse-event-handler (create-target-mouse-event-handler event-state-atom
                                                                                                                      target-width
                                                                                                                      target-height))
                                                       (grid-view :target-grid
                                                                  (:grid-x1 event-state)
                                                                  (:grid-y1 event-state)
                                                                  (:grid-x2 event-state)
                                                                  (:grid-y2 event-state)
                                                                  (:highlight-x event-state)
                                                                  (:highlight-y event-state)
                                                                  target-width
                                                                  target-height)))
                                                    (with-borders
                                                      (layouts/superimpose
                                                       (assoc (visuals/rectangle [255 255 255 255]
                                                                                 #_[0 0 0 255]
                                                                                 0 0)
                                                              :width canvas-width
                                                              :height canvas-height)

                                                       (if (:show-diff @event-state-atom)
                                                         (cache/call! zoom-pane
                                                                      :diff-zoom-pane
                                                                      canvas-width
                                                                      canvas-height
                                                                      scale
                                                                      (:target-x event-state)
                                                                      (:target-y event-state)
                                                                      (visuals/image target-buffered-image))
                                                         nil
                                                         
                                                         #_(diff-view target-buffered-image
                                                                      canvas-state-id
                                                                      (:target-scale event-state)
                                                                      (:target-x event-state)
                                                                      (:target-y event-state)
                                                                      canvas-width
                                                                      canvas-height))
                                                       (assoc {:x 0
                                                               :y 0
                                                               :width canvas-width
                                                               :height canvas-width
                                                               :id :canvas
                                                               :render (create-canvas-renderer (:events @event-state-atom) canvas-state-id)}
                                                              #_(cache/call! zoom-pane
                                                                             :canvas-zoom-pane
                                                                             canvas-width
                                                                             canvas-height
                                                                             1.0 #_(:canvas-scale event-state)
                                                                             0
                                                                             0
                                                                             #_(visuals/image target-buffered-image)
                                                                             {:x 0
                                                                              :y 0
                                                                              :width canvas-width
                                                                              :height canvas-width
                                                                              :id :canvas
                                                                              :render (create-canvas-renderer (:events @event-state-atom) canvas-state-id)})
                                                              :mouse-event-handler (create-canvas-mouse-event-handler event-state-atom
                                                                                                                      canvas-width
                                                                                                                      canvas-height))
                                                       
                                                       
                                                       (when (:show-grid event-state)
                                                         (grid-view :target-grid
                                                                    (:grid-x1 event-state)
                                                                    (:grid-y1 event-state)
                                                                    (:grid-x2 event-state)
                                                                    (:grid-y2 event-state)
                                                                    (:highlight-x event-state)
                                                                    (:highlight-y event-state)
                                                                    canvas-width
                                                                    canvas-height)))))]}
        (application/do-layout width height))))

(defn start []
  (.start (Thread. (fn []
                     (application/start-window  #'create-scene-graph
                                                :window (window/create 1000 1100
                                                                       :close-automatically true) )))))

