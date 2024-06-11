(in-package #:liminality)

(defun ecs-debug-update-tree-view (data interface)
  (setf (capi:tree-view-roots (entity-tree-pane interface))
        (apply #'get-entities-in-system
               (capi:choice-selected-items (components-pane interface)))))

(capi:define-interface ecs-debug-interface ()
  ()
  (:panes
   (repl capi:listener-pane
         :visible-min-width 300
         :visible-min-height 300
         :width 300
         :height 300)
   (entity-tree capi:tree-view
                :visible-min-width '(character 30)
                :accessor entity-tree-pane)
   (components capi:list-panel
               :accessor components-pane
               :items (loop for component across *components*
                            collect (name component))
               :print-function 'string-downcase
               :selection-callback #'ecs-debug-update-tree-view
               :interaction :multiple-selection
               :visible-min-height '(character 8)
               :visible-min-width '(character 30)))
  (:layouts
   (main-layout capi:column-layout
                '(ecs-row repl-row))
   (ecs-row capi:row-layout
            '(components entity-tree))
   (repl-row capi:row-layout
             '(repl)))
  (:default-initargs :title "ECS Display"))

(defun ecs-debug-update-tree-view (data interface)
  (setf (capi:tree-view-roots (component-tree-pane interface))
        (get-entity-components (capi:choice-selected-item (entities-pane interface)))))

(capi:define-interface ecs-interface ()
  ()
  (:panes
   (entities capi:list-panel
             :accessor entities-pane
             :items (loop for entities across *entities* collect entities)
             :selection-callback #'ecs-debug-update-tree-view
             :interaction :single-selection
             :visible-min-height '(character 8)
             :visible-min-width '(character 30))
   (component-tree capi:tree-view
                   :visible-min-width '(character 30)
                   :children-function #'(lambda (component)
                                          (when (eq 'component (type-of (get-component component)))
                                            (loop for slot in (hcl:class-direct-slots (find-class component))
                                                  collect (let ((name (hcl:slot-definition-name slot))
                                                                (comp (funcall component *player*)))
                                                            (list name
                                                                  (if (slot-boundp comp name)
                                                                      (slot-value comp name)
                                                                      "unbound"))))))
                   :accessor component-tree-pane)
   (repl capi:listener-pane
         :visible-min-width 300
         :visible-min-height 300
         :width 300
         :height 300))
  (:layouts
   (main-layout capi:column-layout
                '(ecs-row repl-row))
   (ecs-row capi:row-layout
            '(entities component-tree))
   (repl-row capi:row-layout
             '(repl)))
  (:default-initargs :title "ECS Display"))
