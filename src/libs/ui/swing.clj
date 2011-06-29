(ns libs.ui.swing
  (:use (clojure.contrib miglayout)
        (libs args imperative predicates translate)
        [libs.generic :only [conf config on set-all]]
        (libs.java reflect))
  (:require [clojure.java.io :as io]
            [libs.generic    :as g]
            [libs.java.meta  :as m])
  (:import (clojure.lang IPersistentMap Sequential)
           (java.awt BorderLayout
                     Component
                     Dimension
                     Event
                     Window)
           (java.awt.event ActionListener
                           KeyAdapter
                           KeyEvent
                           MouseAdapter
                           WindowAdapter)
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
                        JMenu
                        JMenuItem
                        JOptionPane
                        JPasswordField
                        JPopupMenu
                        JProgressBar
                        JRadioButton
                        JSeparator
                        JScrollPane
                        JSplitPane
                        JTabbedPane
                        JTextArea
                        JTextField
                        ListCellRenderer
                        SwingUtilities
                        UIManager)
           (javax.swing.event DocumentListener
                              ListSelectionListener
                              PopupMenuListener)
           (javax.swing.text JTextComponent)))

(defn window [& args]
  (config (JFrame.) args))

(defn dialog [& args]
  (with-options [[parent] args]
    (let [o (if parent
              (JDialog. parent)
              (JDialog.))]
      (config o args))))

(defn button [& args]
  (config (JButton.) args))

(defn check-box [& args]
  (config (JCheckBox.) args))

(defn combo-box [& args]
  (config (JComboBox.) args))

(defn label [& args]
  (config (JLabel.) args))

(defn list-box [& args]
  (config (JList.) args))

(defn menu [& args]
  (config (JMenu.) args))

(defn panel [& args]
  (config (JPanel.) args))

(defn password-field [& args]
  (config (JPasswordField.) args))

(defn popup-menu [& args]
  (config (JPopupMenu.) args))

(defn progress-bar [& args]
  (config (JProgressBar.) args))

(defn splitter [& args]
  (config (JSplitPane.) args))

(defn tabs [& args]
  (config (JTabbedPane.) args))

(defn text-area [& args]
  (config (JTextArea.) args))

(defn text-field [& args]
  (config (JTextField.) args))

(defn layout [elements]
  (apply miglayout (panel) elements))

(defn horizontal [& args]
  (with-options [[content] args]
    (layout content)))

(defn vertical [& args]
  (with-options [[content] args]
    (layout (list* :layout
                   :flowy
                   content))))

(defn ask-user
  ([text] (ask-user :Question text))
  ([title text]
     (= JOptionPane/YES_OPTION
        (JOptionPane/showConfirmDialog
         nil
         (translate text)
         (translate title)
         JOptionPane/YES_NO_OPTION
         JOptionPane/QUESTION_MESSAGE))))

(defn message [text]
  (JOptionPane/showMessageDialog
   nil
   (translate text)))

(defn rigid-area [width height]
  (Box/createRigidArea (Dimension. width height)))

(defn grid [& args]
  (with-options [[columns content] args]
    (layout (list* :layout [:wrap columns] content))))

(defn icon [& args]
  (with-options [[content] args]
    (let [lbl (apply label :icon content args)]
      (m/assoc-meta! lbl :type ::icon)
      lbl)))


(defn form [& args]
  (with-options [[content] args]
    (let [form (->> content
                    (partition 2)
                    (mapcat (fn [[k v]]
                              [(label k)
                               v]))
                    (grid :columns 2))]
      (m/assoc-meta! form
                     :type         ::form
                     :form-mapping (apply hash-map content))
      form)))

(defn scrollable [o]
  (JScrollPane. o))

(defn close [o]
  (conf o :close true))

(defn open [o]
  (conf o :open true))


(defmacro invoke-later [& body]
  `(SwingUtilities/invokeLater
    (fn []
      ~@body)))

(doseq [widget-type [Component
                     ::ask-user
                     ::form
                     ::grid
                     ::horizontal
                     ::icon
                     ::message
                     ::options
                     ::rigid-area
                     ::vertical]]
  (derive widget-type ::swing))

(derive JMenu ::menu)
(derive JPopupMenu ::menu)

(defmethod g/set [::swing :content]
  [o _ x]
  (cond ((or? sequential? nil?) x) (g/set o :children x)
        ((or? keyword? string?) x) (g/set o :text x)
        (ifn? x)                   (on o :action x)
        :else                      (g/set o :value x)))

(defmethod g/set [ButtonGroup :content]
  [o _ buttons]
  (doseq [b buttons]
    (.add o b)))

(defmethod g/set [Component :title]
  [o _ text]
  (.setTitle o (translate text)))

(defmethod g/set [Component :text]
  [o _ text]
  (.setText o (translate text)))

(defmethod g/set [Window :text]
  [o _ text]
  (.setTitle o (translate text)))

(defmethod g/set [Component :tooltip]
  [o _ tooltip]
  (.setToolTipText o (translate tooltip)))

(defmethod g/set [Component :children]
  [o _ children]
  (doseq [child children]
    (.add o child)))

(defmethod g/set [JTabbedPane :children]
  [o _ kvs]
  (doseq [[name panel] (partition 2 kvs)]
    (.addTab o (translate name) panel)))

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

(defn add-action-handler [o handler]
  (.addActionListener o
                      (reify ActionListener
                             (actionPerformed
                              [_ evt]
                              (handler evt)))))

(defmethod g/on [JComponent :action]
  [o _ handler]
  (add-action-handler o handler))

(defmethod g/get [JTextComponent :value]
  [o _]
  (.getText o))

(defmethod g/set [JTextComponent :value]
  [o _ val]
  (.setText o val))

(defmethod g/set [JCheckBox :value]
  [o _ val]
  (.setSelected o val))

(defmethod g/get [JCheckBox :value]
  [o _]
  (.isSelected o val))

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

(defmethod g/on [JList :change]
  [o _ handler]
  (.addListSelectionListener
   o
   (reify ListSelectionListener
          (valueChanged [_ evt]
                        (handler evt)))))

(defn menu-item [text handler]
  (g/make JMenuItem
        :text text
        handler))

(defmethod g/set [::menu :children]
  [o _ entries]
  (doseq [e (->> entries
                 (partition 2)
                 (map menu-item))]
    (.add o e)))

(defmethod g/set [::swing :popup-menu]
  [o _ menu]
  (g/on o :right-click (fn [e] (.show menu
                                     (.getComponent e)
                                     (.getX e)
                                     (.getY e)))))

(defmethod g/set [::swing :background]
  [o _ color]
  (when color
    (.setBackground o color)))

(defmethod g/set [::swing :foreground]
  [o _ color]
  (when color
    (.setForeground o color)))

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

(defmethod g/get [JComboBox :value]
  [o _]
  (.getSelectedItem o))

(defmethod g/get [JList :value]
  [o _]
  (.getSelectedValue o))

(defmethod g/get [JList :values]
  [o _]
  (.getSelectedValues o))

(defmethod g/set [JComboBox :value]
  [o _ value]
  (swap! disabled-combo-boxes conj o)
  (.setSelectedItem o value)
  (swap! disabled-combo-boxes disj o))

(defmethod g/set [JList :value]
  [o _ value]
  (.setSelectedValue o value true))

(derive JList ::list)
(derive JComboBox ::list)

(defmethod g/get [::list :children]
  [o _]
  (let [model (.getModel o)]
    (doall (for [i (range (.getSize model))]
             (.getElementAt model i)))))

(defmethod g/set [JComboBox :children]
  [o _ items]
  (swap! disabled-combo-boxes conj o)
  (.removeAllItems o)
  (doseq [i items]
    (.addItem o i))
  (.setSelectedIndex o 0)
  (swap! disabled-combo-boxes disj o))

(defmethod g/set [JList :children]
  [o _ items]
  (.setListData o (to-array items)))

(defmethod g/set [JComponent :icon]
  [o _ source]
  (.setIcon o (ImageIcon. (io/resource source))))

(defmethod g/get [::options :value]
  [o _]
  @(:value-atom (m/meta o)))

(defn options [& args]
  (with-options [[format-fn init layout content] args]
    (let [format-fn (or format-fn str)
          layout    (or layout :horizontal)
          current-value (atom init)
          buttons       (for [i content]
                          (g/make JRadioButton
                                  :text (format-fn i)
                                  :selected (= i init)
                                  :on {:click (fn [_] (reset! current-value i))}))
          group         (g/make ButtonGroup buttons)
          inner-panel   (case layout
                              :horizontal (horizontal buttons)
                              :vertical   (vertical   buttons))
          outer-panel   (panel [inner-panel])]
      (m/assoc-meta! outer-panel
                     :type ::options
                     :value-atom current-value)
      outer-panel)))

(defmethod g/get1 ::form
  [o key]
  (-> o
      m/meta
      :form-mapping
      key
      (g/get :value)))

(defmethod g/get [::form :value]
  [o _]
  (let [m (:form-mapping (m/meta o))]
    (zipmap (keys m)
            (map #(get % :value)
                 (vals m)))))

(defn get-ui-color [descr-string]
  (UIManager/getColor descr-string))

(defn make-enable-renderer
  "A list cell renderer that greys out items not enabled.
   :separator is also accepted instead of an item."
  []
  (let [lbl (label :text ""
                   :opaque true
                   :border 1)]
    (reify ListCellRenderer
           (getListCellRendererComponent
            [_ list value index selected? focus?]
            (if (= value :separator)
              (JSeparator. JSeparator/HORIZONTAL)
              (let [[bg fg] (if-not (g/get value :enabled?)
                              [(.getBackground list)
                               (get-ui-color "Label.disabledForeground")]
                              (if selected?
                                [(.getSelectionBackground list)
                                 (.getSelectionForeground list)]
                                [(.getBackground list)
                                 (.getForeground list)]))]
                (conf lbl
                      :background bg
                      :foreground fg
                      :font (g/get list :font)
                      :text (str value))
                lbl))))))


(defmethod g/set [Window :open]
  [o _ open?]
  (when open?
    (.pack o)
    (.setVisible o true)))

(defmethod g/set [Window :close]
  [o _ close?]
  (when close?
    (.setVisible o false)
    (.dispose o)))

(defmethod g/on [Window :closing]
  [o _ handler]
  (.addWindowListener o (proxy [WindowAdapter] []
                            (windowClosing [evt] (handler evt)))))

(defmethod config Window
  [o args]
  (with-options [[open] args]
    (let [o (set-all o args)]
      (g/set o :open open)
      o)))

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

(defmethod g/on [Window :key-in-child]
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
  (set-all o {:min-size  sz
              :pref-size sz
              :max-size  sz}))