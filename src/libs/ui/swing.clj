(ns libs.ui.swing
  (:use (clojure.contrib miglayout)
        (libs args imperative translate ui))
  (:require [clojure.java.io :as io]
            [libs.generic    :as g]
            [libs.java.meta  :as m])
  (:import (clojure.lang IPersistentMap Sequential)
           (java.awt BorderLayout
                     Component
                     Dimension
                     Event
                     GridLayout)
           (java.awt.event ActionListener
                           KeyAdapter
                           KeyEvent
                           MouseAdapter)
           (javax.swing AbstractButton
                        BorderFactory
                        Box
                        BoxLayout
                        ButtonGroup
                        DefaultListModel
                        ImageIcon
                        JButton
                        JCheckBox
                        JComboBox
                        JComponent
                        JDialog
                        JFrame
                        JPanel
                        JLabel
                        JList
                        JOptionPane
                        JPasswordField
                        JProgressBar
                        JRadioButton
                        JSeparator
                        JScrollPane
                        JSplitPane
                        JTabbedPane
                        JTextArea
                        JTextField
                        ListCellRenderer
                        UIManager)
           (javax.swing.event DocumentListener
                              PopupMenuListener)
           (javax.swing.text JTextComponent)))

(g/def-backend
  :swing
  label               JLabel
  button              JButton
  text-field          JTextField
  text-area           JTextArea
  password-field      JPasswordField
  check-box           JCheckBox
  combo-box           JComboBox
  icon                ::icon
  progress-bar        JProgressBar
  options             ::options
  form                ::form
  window              JFrame
  panel               JPanel
  dialog              JDialog
  tabs                JTabbedPane
  rigid-area          ::rigid-area
  splitter            JSplitPane)

(defmethod g/set [JComponent :title]
  [o _ text]
  (.setText o (translate text)))

(defmethod g/set [JComponent :children]
  [o _ children]
  (doseq [child children]
    (.add o child)))

(defn layout [elements]
  (apply miglayout (panel) elements))

(defmethod g/set [JTabbedPane :tabs]
  [o _ kvs]
  (doseq [[name panel] (partition 2 kvs)]
    (.addTab o (translate name) panel)))

(defmethod g/as [::rigid-area IPersistentMap]
  [_ {:keys [width height]}]
  (Box/createRigidArea (Dimension. width height)))

(defmethod g/set [JComponent :border]
  [o _ borders]
  (let [[top left bottom right] (if (number? borders)
                                  (repeat borders)
                                  (map borders [:top :left :right :bottom]))]
    (.setBorder o
                (BorderFactory/createEmptyBorder top left bottom right))))

(defmethod g/set [JComponent :titled-border]
  [o _ text]
  (.setBorder o (BorderFactory/createTitledBorder (translate text))))

(defmethod g/as [::horizontal Sequential]
  [_ {:keys [children]}]
  (layout children))

(defmethod g/as [::vertical Sequential]
  [_ {:keys [children]}]
  (layout (list* :layout
                 :flowy
                 children)))

(defmethod g/as [::grid Sequential]
  [_ {:keys [columns children]}]
  (layout (list* :layout [:wrap columns] children)))

(defmethod g/set [JSplitPane :orientation]
  [o _ orientation]
  (.setOrientation o (case orientation
                           :vertical   JSplitPane/VERTICAL_SPLIT
                           :horizontal JSplitPane/HORIZONTAL_SPLIT)))

(defmethod g/set [JSplitPane :top]
  [o _ elt]
  (.setTopComponent o elt))

(defmethod g/set [JSplitPane :bottom]
  [o _ elt]
  (.setBottomComponent o elt))

(defmethod g/set [JSplitPane :left]
  [o _ elt]
  (.setLeftComponent o elt))

(defmethod g/set [JSplitPane :right]
  [o _ elt]
  (.setRightComponent o elt))

(defmethod g/set [JComponent :text]
  [o _ text]
  (.setText o (translate text)))

(defmethod g/set [JComponent :tooltip]
  [o _ tooltip]
  (.setToolTipText o (translate tooltip)))

(defn add-action-handler [o handler]
  (.addActionListener o
                      (reify ActionListener
                             (actionPerformed
                              [_ evt]
                              (handler evt)))))

(defmethod g/on [JComponent :action]
  [o _ handler]
  (add-action-handler o handler))

(defmethod g/on [JTextComponent :change]
  [o _ handler]
  (.. o
      getDocument
      (addDocumentListener
       (reify DocumentListener
              (changedUpdate
               [_ evt]
               (handler evt))
              (insertUpdate
               [_ evt]
               (handler evt))
              (removeUpdate
               [_ evt]
               (handler evt))))))

(defmethod g/on [JComponent :popup]
  [o _  {:keys [hide show cancel]}]
  (.addPopupMenuListener
   o
   (reify PopupMenuListener
          (popupMenuCanceled            [_ evt] (when cancel (cancel evt)))
          (popupMenuWillBecomeVisible   [_ evt] (when show   (show   evt)))
          (popupMenuWillBecomeInvisible [_ evt] (when hide   (hide   evt))))))

(defmethod g/on [JComponent :click]
  [o _ handler]
  (.addMouseListener
   o
   (proxy [MouseAdapter] []
     (mouseClicked [evt]
                   (handler evt)))))

(defmethod g/on [JComponent :double-click]
  [o _ handler]
  (g/on o :click
        (fn [evt]
          (when (= (.getClickCount evt)
                   2)
            (handler evt)))))

(defmethod g/on [JComponent :right-click]
  [o _ handler]
  (.addMouseListener
   o
   (proxy [MouseAdapter] []
     (mousePressed [evt]
                   (when (.isPopupTrigger evt)
                     (handler evt)))
     (mouseReleased [evt]
                    (when (.isPopupTrigger evt)
                      (handler evt))))))

(defmethod g/set [JComponent :enabled?]
  [o _ enabled?]
  (.setEnabled o (boolean enabled?)))

(defmethod g/set [JComponent :line-wrap]
  [o _ wrap]
  (.setLineWrap o (boolean wrap))
  (.setWrapStyleWord o (= :words wrap)))

(defmethod g/set [JComponent :selected?]
  [o _ selected?]
  (.setSelected o (boolean selected?)))

(defmethod g/get [Object :enabled?]
  [_ _]
  false)

(defmethod g/get [nil :enabled?]
  [_ _]
  false)

;; Hack for not calling :action handlers
;; when changing items or value programmatically
(def disabled-combo-boxes (atom #{}))

(defmethod g/on [JComboBox :action]
  [o _ handler]
  (add-action-handler
   o
   #(when-not (@disabled-combo-boxes o)
      (handler %))))

(defmethod g/set [JComboBox :value]
  [o _ value]
  (swap! disabled-combo-boxes conj o)
  (.setSelectedItem o value)
  (swap! disabled-combo-boxes disj o))

(defmethod g/set [JComboBox :items]
  [o _ items]
  (swap! disabled-combo-boxes conj o)
  (.removeAllItems o)
  (doseq [i items]
    (.addItem o i))
  (.setSelectedIndex o 0)
  (swap! disabled-combo-boxes disj o))

(defmethod g/set [JComponent :icon]
  [o _ source]
  (.setIcon (ImageIcon. (io/resource source))))

(defmethod g/as [::icon Sequential]
  [_ args]
  (let [[{:keys [source]}
         args]               (parse-options [:source] args)
         lbl (apply label :icon source args)]
    (m/assoc-meta! lbl :type ::icon)
    lbl))

(defmethod g/get [::options :value]
  [o _]
  @(:value-atom (m/meta o)))

(defmethod g/as [::options Sequential]
  [_ args]
  (let [[{:keys [format-fn init layout items]
          :or {format-fn str
               layout    :horizontal}}
         args]         (parse-options [:format-fn :init :layout :items]
                                      args)
         group         (ButtonGroup.)
         current-value (atom init)
         buttons       (for [i items]
                         (g/make JRadioButton
                               :text (format-fn i)
                               :selected (= i init)
                               :on {:click (fn [_] (reset! current-value i))}))
         inner-panel   (case layout
                             :horizontal (apply horizontal buttons)
                             :vertical   (apply vertical   buttons))
         outer-panel   (JPanel.)]
    (doseq [b buttons]
      (.add group b))
    (.add outer-panel inner-panel)
    (m/assoc-meta! outer-panel
                   :type ::options
                   :value-atom current-value)
    outer-panel))

(defmethod g/get [::form :value]
  [o _]
  (let [m (:form-mapping (m/meta o))]
    (zipmap (keys m)
            (map #(get % :value)
                 (vals m)))))

(defmethod g/as [::form Sequential]
  [_ args]
  (let [[{:keys [keys items]}
         args] (parse-options [:keys :items]
                              args)
         form (grid :rows 2
                 :items (mapcat (fn [k v]
                                  [(label :text k)
                                   v])
                                keys
                                items))]
    (m/assoc-meta! form
                   :type         ::form
                   :form-mapping (zipmap keys items))
    form))

(defn scrollable [o]
  (JScrollPane. o))

(defn get-ui-color [descr-string]
  (UIManager/getColor descr-string))

(defn make-enable-renderer
  "A list cell renderer that greys out items not enabled.
   :separator is also accepted instead of an item."
  []
  (let [lbl (doto (label "")
              (.setOpaque true)
              (set :border 1))]
    (reify ListCellRenderer
           (getListCellRendererComponent
            [_ list value index selected? focus?]
            (if (= value :separator)
              (JSeparator. JSeparator/HORIZONTAL)
              (let [[bg fg] (if-not (get :enabled? value)
                              [(.getBackground list)
                               (get-ui-color "Label.disabledForeground")]
                              (if selected?
                                [(.getSelectionBackground list)
                                 (.getSelectionForeground list)]
                                [(.getBackground list)
                                 (.getForeground list)]))]
                (doto lbl
                  (.setBackground bg)
                  (.setForeground fg)
                  (.setFont (.getFont list))
                  (.setText (if value
                              (str value)
                              "")))))))))

(defn close-frame [frame]
  (.setVisible frame false)
  (.dispose frame))

(defmethod g/set [JFrame :open]
  [o _ open?]
  (when open?
    (.pack o)
    (.setVisible o true)))

(defn key-name [key-event]
  (str (.getKeyModifiers key-event)
       (.getKeyText key-event)))

(defn combined-key-handler [key-handlers]
  (let [key-handled? (set (keys key-handlers))]
    (proxy [KeyAdapter] []
      (keyPressed
       [key-event]
       (let [key (key-name key-event)]
         (when (key-handled? key)
           ((key-handlers key) key-event)))))))

(defmethod g/on [JComponent :key]
  [o _ handlers]
  (.addKeyListener o (combined-key-handler handlers)))

(defmethod g/on [JFrame :key-in-child]
  [o _ handlers]
  (let [handler (combined-key-handler handlers)
        rec-add (fn rec-add [component]
                  (.addKeyListener component handler)
                  (doseq [elt (.getComponents component)]
                    (rec-add elt)))]
    (rec-add (.getContentPane o))))

(defmethod g/set [JComponent :min-size]
  [o _ [width height]]
  (.setMinimumSize o (Dimension. width height)))

(defmethod g/set [JComponent :pref-size]
  [o _ [width height]]
  (.setPreferredSize o (Dimension. width height)))

(defmethod g/set [JComponent :max-size]
  [o _ [width height]]
  (.setMaximumSize o (Dimension. width height)))

(defmethod g/set [JComponent :size]
  [o _ sz]
  (g/set-all o {:min-size  sz
                :pref-size sz
                :max-size  sz}))

(defn show-message [msg]
  (javax.swing.JOptionPane/showMessageDialog
   nil
   msg))

