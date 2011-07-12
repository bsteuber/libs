(ns libs.swing
  (:refer-clojure :exclude [get set])
  (:use (clojure.contrib miglayout)
        (libs args
              debug
              fn
              generic
              log
              predicates
              translate)
        (libs.java reflect))
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]
            [libs.java.meta  :as m])
  (:import (clojure.lang Keyword
                         Fn
                         IFn
                         IPersistentMap)
           (java.awt BorderLayout
                     Component
                     Container
                     Dimension
                     Event
                     Window)
           (java.awt.event ActionListener
                           KeyAdapter
                           KeyEvent
                           MouseAdapter
                           WindowAdapter)
           (javax.swing AbstractAction
                        AbstractButton
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
                        JTable
                        JTextArea
                        JTextField
                        KeyStroke
                        ListCellRenderer
                        ListSelectionModel
                        RowSorter$SortKey
                        RowFilter
                        SortOrder
                        SwingUtilities
                        ToolTipManager
                        UIManager)
           (javax.swing.event DocumentListener
                              ListSelectionListener
                              PopupMenuListener)
           (javax.swing.table AbstractTableModel
                              DefaultTableModel
                              DefaultTableCellRenderer)
           (javax.swing.text JTextComponent)))

(derive JMenu      ::menu)
(derive JPopupMenu ::menu)

(defmacro invoke-later [& body]
  `(SwingUtilities/invokeLater
    (fn []
      ~@body)))

(defn window [& args]
  (config (JFrame.) args))

(deff dialog [parent & other-args]
  (let [o (if parent
            (JDialog. parent)
            (JDialog.))]
    (config o other-args)))

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

(defn popup-menu [& args]
  (config (JPopupMenu.) args))

(defn progress-bar [& args]
  (config (JProgressBar.) args))

(defn splitter [& args]
  (config (JSplitPane.) args))

(defn tabs [& args]
  (config (JTabbedPane.) args))

(defn text-area [& args]
  (config (JTextArea. 3 15) args))

(defn text-field [& args]
  (config (JTextField. 15) args))

(defn password-field [& args]
  (config (JPasswordField. 15) args))

(defn layout [elements]
  (apply miglayout (JPanel.) elements))

(deff horizontal [args]
  (layout args))

(deff vertical [args]
  (layout (list* :layout
                 :flowy
                 args)))

(deff ask-user [title args]
  (let [title  (or title :question)
        [text] args]
    (= JOptionPane/YES_OPTION
        (JOptionPane/showConfirmDialog
         nil
         (translate text)
         (translate title)
         JOptionPane/YES_NO_OPTION
         JOptionPane/QUESTION_MESSAGE))))

(deff message [icon title type args]
  (let [type     (or type :plain)
        [text]   args
        msg-type (case type
                       :error    JOptionPane/ERROR_MESSAGE
                       :info     JOptionPane/INFORMATION_MESSAGE
                       :warn     JOptionPane/WARNING_MESSAGE
                       :question JOptionPane/QUESTION_MESSAGE
                       :plain    JOptionPane/PLAIN_MESSAGE)]
    (JOptionPane/showMessageDialog
     nil
     (translate text)
     (translate title)
     msg-type
     icon)))

(deff rigid-area [args]
  (let [[width height] args]
    (Box/createRigidArea (Dimension. width height))))

(deff grid [columns args]
  (layout (list* :layout [:wrap columns] args)))

(deff form [args]
  (let [form (->> args
                  (partition 2)
                  (mapcat (fn [[k v]]
                            [(label k)
                             v]))
                  (list* :column "[]20[]")
                  (grid :columns 2))]
    (m/assoc-meta! form
                   :type         ::form
                   :form-mapping (apply hash-map args))
    form))

(deff options [format-fn init layout args]
  (let [format-fn     (or format-fn translate)
        layout        (or layout :horizontal)
        current-value (atom init)
        buttons       (for [o args]
                        (conf (JRadioButton.)
                                :text (format-fn o)
                                :selected (= o init)
                                :on {:click #(reset! current-value o)}))
        group         (make ButtonGroup buttons)
        inner-panel   (case layout
                            :horizontal (horizontal (list* :layout "ins 0"
                                                           buttons))
                            :vertical   (vertical buttons))
        outer-panel   (panel inner-panel)]
    (m/assoc-meta! outer-panel
                   :type ::options
                   :value-atom current-value)
    outer-panel))

(defn scrollable [o]
  (JScrollPane. o))

(defn close [o]
  (conf o :close true))

(defn open [o]
  (conf o :open true))

(defmethod set [Object :args]
  [o _ args]
  (doseq [arg args]
    (put o arg)))

(defmethod put [Component Object]
  [o val]
  (set o :value val))

(defmethod put [Container Component]
  [o inner]
  (.add o inner))

(defmethod put [ButtonGroup Component]
  [o inner]
  (.add o inner))

(defmethod put [Component String]
  [o text]
  (set o :text text))

(defmethod put [Component Keyword]
  [o text]
  (set o :text text))

(defmethod put [Component Fn]
  [o f]
  (on o :action f))

(defmethod set [Component :title]
  [o _ text]
  (.setTitle o (translate text)))

(defmethod set [Component :text]
  [o _ text]
  (.setText o (translate text))
  (when (translatable? (descriptive text))
    (set o :tooltip (descriptive text))))

(defmethod set [Window :text]
  [o _ text]
  (.setTitle o (translate text)))

(defmethod set [Component :tooltip]
  [o _ tooltip]
  (.setToolTipText o (translate tooltip)))

(defmethod set [JTabbedPane :args]
  [o _ kvs]
  (doseq [[name panel] (partition 2 kvs)]
    (.addTab o (translate name) panel)))

(defmethod set [Component :border]
  [o _ borders]
  (let [[top left bottom right] (if (number? borders)
                                  (repeat borders)
                                  (map borders [:top :left :right :bottom]))]
    (.setBorder o
                (BorderFactory/createEmptyBorder top left bottom right))))

(defmethod set [Component :titled-border]
  [o _ text]
  (.setBorder o (BorderFactory/createTitledBorder (translate text))))

(defmethod set [JSplitPane :orientation]
  [o _ orientation]
  (.setOrientation o (case orientation
                           :vertical   JSplitPane/VERTICAL_SPLIT
                           :horizontal JSplitPane/HORIZONTAL_SPLIT)))

(defmethod set [JSplitPane :top]
  [o _ elt]
  (.setTopComponent o elt))

(defmethod set [JSplitPane :bottom]
  [o _ elt]
  (.setBottomComponent o elt))

(defmethod set [JSplitPane :left]
  [o _ elt]
  (.setLeftComponent o elt))

(defmethod set [JSplitPane :right]
  [o _ elt]
  (.setRightComponent o elt))

(defn add-action-handler [o handler]
  (.addActionListener o
                      (reify ActionListener
                             (actionPerformed
                              [_ evt]
                              (handler evt)))))

(defmethod on [Component :action]
  [o _ handler]
  (add-action-handler o (as-handler handler)))

(defmethod get [JTextComponent :value]
  [o _]
  (.getText o))

(defmethod set [JTextComponent :value]
  [o _ val]
  (.setText o (str val)))

(defmethod set [JCheckBox :value]
  [o _ val]
  (.setSelected o val))

(defmethod get [JCheckBox :value]
  [o _]
  (.isSelected o val))

(defmethod on [JTextComponent :change]
  [o _ handler]

  (with-handlers [handler]
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
                 (handler evt)))))))

(defmethod on [JList :change]
  [o _ handler]
  (with-handlers [handler]
    (.addListSelectionListener
     o
     (reify ListSelectionListener
            (valueChanged [_ evt]
                          (handler evt))))))

(defn menu-item [text handler]
  (make JMenuItem
        :text text
        (as-handler handler)))

(defmethod set [::menu :args]
  [o _ entries]
  (doseq [e (->> entries
                 (partition 2)
                 (map #(apply menu-item %)))]
    (.add o e)))

(defmethod set [Component :popup-menu]
  [o _ menu]
  (on o :right-click (fn [e] (.show menu
                                    (.getComponent e)
                                    (.getX e)
                                    (.getY e)))))

(defmethod set [Component :background]
  [o _ color]
  (when color
    (.setBackground o color)))

(defmethod set [Component :foreground]
  [o _ color]
  (when color
    (.setForeground o color)))

(defmethod on [Component :popup]
  [o _  {:keys [hide show cancel]}]
  (with-handlers [hide show cancel]
    (.addPopupMenuListener
     o
     (reify PopupMenuListener
            (popupMenuCanceled            [_ evt] (when cancel (cancel evt)))
            (popupMenuWillBecomeVisible   [_ evt] (when show   (show   evt)))
            (popupMenuWillBecomeInvisible [_ evt] (when hide   (hide   evt)))))))

(defmethod on [Component :click]
  [o _ handler]
  (with-handlers [handler]
    (.addMouseListener
     o
     (proxy [MouseAdapter] []
       (mouseClicked [evt]
                     (handler evt))))))

(defmethod on [Component :double-click]
  [o _ handler]
  (with-handlers [handler]
    (on o :click
        (fn [evt]
          (when (= (.getClickCount evt)
                   2)
            (handler evt))))))

(defmethod on [Component :right-click]
  [o _ handler]
  (with-handlers [handler]
    (.addMouseListener
     o
     (proxy [MouseAdapter] []
       (mousePressed [evt]
                     (when (.isPopupTrigger evt)
                       (handler evt)))
       (mouseReleased [evt]
                      (when (.isPopupTrigger evt)
                        (handler evt)))))))

(defmethod set [Component :enabled?]
  [o _ enabled?]
  (.setEnabled o (boolean enabled?)))

(defmethod set [Component :line-wrap]
  [o _ wrap]
  (.setLineWrap o (boolean wrap))
  (.setWrapStyleWord o (= :words wrap)))

(defmethod set [Component :selected?]
  [o _ selected?]
  (.setSelected o (boolean selected?)))

(defmethod get [Object :enabled?]
  [_ _]
  false)

(defmethod get [nil :enabled?]
  [_ _]
  false)

;; Hack for not calling :action handlers
;; when changing items or value programmatically
(def disabled-combo-boxes (atom #{}))

(defmethod on [JComboBox :action]
  [o _ handler]
  (with-handlers [handler]
    (add-action-handler
     o
     #(when-not (@disabled-combo-boxes o)
        (handler %)))))

(defmethod get [JComboBox :value]
  [o _]
  (.getSelectedItem o))

(defmethod get [JList :value]
  [o _]
  (.getSelectedValue o))

(defmethod get [JList :values]
  [o _]
  (.getSelectedValues o))

(defmethod set [JComboBox :value]
  [o _ value]
  (swap! disabled-combo-boxes conj o)
  (.setSelectedItem o value)
  (swap! disabled-combo-boxes disj o))

(defmethod set [JList :value]
  [o _ value]
  (.setSelectedValue o value true))

(derive JList ::list)
(derive JComboBox ::list)

(defmethod get [::list :items]
  [o _]
  (let [model (.getModel o)]
    (doall (for [i (range (.getSize model))]
             (.getElementAt model i)))))

(defmethod put [JComboBox Object]
  [o val]
  (swap! disabled-combo-boxes conj o)
  (.addItem o val)
  (swap! disabled-combo-boxes disj o))

(prefer-method put
               [JComboBox Object]
               [Component String])

(prefer-method put
               [JComboBox Object]
               [Component Keyword])

(prefer-method put
               [Component Fn]
               [JComboBox Object])

(defmethod set [JComboBox :items]
  [o _ items]
  (swap! disabled-combo-boxes conj o)
  (.removeAllItems o)
  (doseq [i items]
    (.addItem o i))
  (.setSelectedIndex o 0)
  (swap! disabled-combo-boxes disj o))

(defmethod set [JList :items]
  [o _ items]
  (.setListData o (to-array items)))

(defn icon [path]
  (if-let [res (io/resource path)]
    (ImageIcon. res)
    (warn "can't find icon:" path)))

(defmethod get [::options :value]
  [o _]
  @(:value-atom (m/meta o)))

(defmethod get1 ::form
  [o key]
  (-> o
      m/meta
      :form-mapping
      key
      (get :value)))

(defmethod get [::form :value]
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
              (let [[bg fg] (if-not (get value :enabled?)
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
                      :font (get list :font)
                      :text value)))))))

(defn make-icon-renderer
  "A list cell renderer that also shows icons"
  [value->icon]
  (let [lbl (label ""
                   :opaque true
                   :border 1)]
    (reify ListCellRenderer
           (getListCellRendererComponent
            [_ list val index selected? focus?]
            (conf lbl
                  :background (if selected?
                                (.getSelectionBackground list)
                                (.getBackground list))
                  :foreground (if selected?
                                (.getSelectionForeground list)
                                (.getForeground list))
                  ;; :font (.getFont list)
                  :icon (value->icon val)
                  :text val)))))

(defmethod set [Window :open]
  [o _ open?]
  (when open?
    (.pack o)
    (.setVisible o true)))

(defmethod set [Window :close]
  [o _ close?]
  (when close?
    (.setVisible o false)
    (.dispose o)))

(defmethod on [Window :closing]
  [o _ handler]
  (with-handlers [handler]
    (.addWindowListener o (proxy [WindowAdapter] []
                            (windowClosing [evt] (handler evt))))))

(defn as-action [f]
  (proxy [AbstractAction] []
    (actionPerformed [e] (f e))))

(defn add-key-handler [win key handler]
  (with-handlers [handler]
    (let [key-id     (->> (name key)
                          str/upper-case
                          (str "VK_")
                          (static-field KeyEvent))
          key-stroke (KeyStroke/getKeyStroke key-id 0)
          root (.getRootPane win)]
      (.. root
          (getInputMap JComponent/WHEN_IN_FOCUSED_WINDOW)
          (put key-stroke (name key)))
      (.. root
          (getActionMap)
          (put (name key) (as-action handler))))))

(defmethod on [Component :key]
  [o _ handlers]
  (doseq [[key handler] handlers]
    (add-key-handler o key handler)))

(defmethod set [Component :min-size]
  [o _ [width height]]
  (.setMinimumSize o (Dimension. width height)))

(defmethod set [Component :pref-size]
  [o _ [width height]]
  (.setPreferredSize o (Dimension. width height)))

(defmethod set [Component :max-size]
  [o _ [width height]]
  (.setMaximumSize o (Dimension. width height)))

(defmethod set [Component :size]
  [o _ sz]
  (set-all o {:min-size  sz
              :pref-size sz
              :max-size  sz}))

(deff ok-cancel-dialog [args on-ok open validator & other-args]
  (let [d             (apply dialog other-args)
        validator     (or validator (fn []))
        on-ok         (or on-ok (fn []))
        ok-button     (button #(if-let [error (validator)]
                                 (message :type :error
                                          error)
                                 (do (close d)
                                     (on-ok)))
                              :ok)
        cancel-button (button #(close d)
                              :cancel)]
    (.. d getRootPane (setDefaultButton ok-button))
    (conf d
          :on {:key {:escape #(doto cancel-button
                                .requestFocusInWindow
                                (.doClick 100))}}
          (vertical (concat args
                            [(horizontal [ok-button [:tag :ok]
                                          cancel-button [:tag :cancel]])
                             :align :center]))
          :open open)))

(defn make-row-model [data-ref column-model p-key]
  (proxy [AbstractTableModel] []
    (getRowCount
      []
      (try (count @data-ref)
           (catch Exception e (error e))))
    (getColumnName
      [col]
      (try (:caption (column-model col))
           (catch Exception e (error e))))
    (getColumnCount
      []
      (try (count column-model)
           (catch Exception e (error e))))
    (getColumnClass
      [col]
      (try (:class (column-model col))
           (catch Exception e (error e))))
    (getValueAt
      [row col]
      (try (let [key (:key (column-model col))]
             (key (nth (vals @data-ref) row)))
           (catch Exception e (error e))))))

(defn set-regex-filter [table regex]
  (try
    (.. table
        getRowSorter
        (setRowFilter (RowFilter/regexFilter regex 0)))
    (catch java.util.regex.PatternSyntaxException e)))

(defn make-table-cell-renderer [render]
  (proxy [DefaultTableCellRenderer] []
    (setValue [val]
      (render this val))))

(defn mouse-evt->row-col [table evt]
  (let [pt (.getPoint evt)
        row (.rowAtPoint table pt)
        col (.columnAtPoint table pt)]
    (debug "mouse-evt-rowcol" row col)
    (when-not (or (= -1 row)
                  (= -1 col))
      [(.convertRowIndexToModel    table row)
       (.convertColumnIndexToModel table col)])))

(defn make-table-model [{:keys [columns]}]
  (let [classes     (vec (map :class columns))
        table-model (proxy [DefaultTableModel] []
                      (getColumnClass [col]
                        (classes col)))]
    (doseq [col columns]
      (->> col
           :key
           translate
           (.addColumn table-model)))
    table-model))

(defn add-table-row [table record]
  (invoke-later
   (let [keys (:keys (m/meta table))
         ;; records don't implement IFn, so no (map record keys)
         row   (to-array (map #(% record) keys))]
     (.. table getModel (addRow row)))))

(defn remove-table-row [table id]
  (invoke-later
   (let [model      (.getModel table)
         get-row-id (:get-row-id (m/meta table))
         row-number (some #(when (= (get-row-id %)
                                    id)
                             %)
                          (range (.getRowCount model)))]
     (.removeRow model row-number))))

(defn clear-table [table]
  (invoke-later
   (.. table
       getModel
       getDataVector
       removeAllElements)))

(defn table [model & {:keys [tool-tip-generator
                             on-right-click
                             on-double-click]}]
  (let [table-model (make-table-model model)
        table (if tool-tip-generator
                (proxy [JTable] [table-model]
                  (getToolTipText [evt]
                    (let [[row col] (mouse-evt->row-col this evt)]
                      (when (and row col)
                        (tool-tip-generator table-model row col)))))
                (JTable. table-model))
        {columns     :columns
         sort-keys   :sort
         primary-key :primary} model
        column-keys (map :key columns)
        key->index (into {}
                         (map-indexed (fn [id col]
                                        [(:key col) id])
                                      columns))
        sort-order {:asc  SortOrder/ASCENDING
                    :desc SortOrder/DESCENDING}
        get-row-id  (fn [row-number]
                      ;; todo primary key of multiple columns
                      (.getValueAt table-model
                                   row-number
                                   (key->index primary-key)))]
    (m/assoc-meta! table
                   :keys       column-keys
                   :get-row-id get-row-id)
    (doseq [{:keys [key width renderer]} columns]
      (let [col (.getColumn table (translate key))]
        (when renderer
          (.setCellRenderer col renderer))
        (when width
          (.setPreferredWidth col width))))
    (when on-double-click
      (on table :double-click (fn [evt]
                                (when-let [cell (mouse-evt->row-col table evt)]
                                  (on-double-click cell)))))
    (when on-right-click
      (on table :right-click (fn [evt]
                               (when-let [cell (mouse-evt->row-col table evt)]
                                  (on-right-click cell)))))
    (doto table
      (.setShowVerticalLines false)
      (.setShowHorizontalLines false)
      (.setAutoCreateRowSorter true)
      (.setFillsViewportHeight true)
      (.setSelectionMode ListSelectionModel/SINGLE_SELECTION))
    (when sort-keys (.. table
                        getRowSorter
                        (setSortKeys (for [[key order] sort-keys]
                                       (RowSorter$SortKey. (key->index key)
                                                           (sort-order order))))))
    table))

(defn set-native-look []
  (invoke-later
   (UIManager/setLookAndFeel
    (if (= "Linux" (System/getProperty "os.name"))
      "com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel"
      (UIManager/getSystemLookAndFeelClassName)))))
