(ns jsparse.core
  (require [clojure.java.io :as io])
  (import [org.mozilla.javascript Node Token Parser CompilerEnvirons]
          [org.mozilla.javascript.ast
            Scope Symbol FunctionNode VariableInitializer VariableDeclaration Name
            ArrayLiteral StringLiteral ObjectLiteral InfixExpression FunctionCall
            ExpressionStatement IfStatement WithStatement
            WhileLoop ReturnStatement Jump AstRoot Comment ScriptNode]))

(set! *warn-on-reflection* true)

(defrecord AstNode [op value line symbols children])

(defprotocol NodeValue
  (get-value [this]))

(defn type-keyword
  [type-id]
  (keyword (Token/typeToName type-id)))

(defprotocol NodeToFeature
  (node->map [this]))

(declare symbol-table)

(defn- base-map
  [^Node node]
  (map->AstNode
   {:op (type-keyword (.getType node))
    :value (get-value node)
    :line (if (instance? ScriptNode node) [(.getBaseLineno ^ScriptNode node) (.getEndLineno ^ScriptNode node)] [(.getLineno node) (.getLineno node)])
    :symbols (if (instance? Scope node) (symbol-table node) [])
    :children (map node->map node)}))

(extend-protocol NodeToFeature
  AstRoot
  (node->map
    [node]
    (let [res (base-map node)
          comments (map node->map (or (.getComments node) []))]
      (assoc res :children (concat (:children res) comments))))
  FunctionNode
  (node->map
    [node]
    (assoc (base-map node) :children [(node->map (.getBody node))]))
  VariableInitializer
  (node->map
    [node]
    (assoc (base-map node)
      :children [(node->map (.getTarget node)) (node->map (.getInitializer node))]))
  VariableDeclaration
  (node->map
    [node]
    (assoc (base-map node)
      :children (map node->map (.getVariables node))))
  ArrayLiteral
  (node->map
    [node]
    (assoc (base-map node)
      :children (map node->map (.getElements node))))
  ObjectLiteral
  (node->map
    [node]
    (assoc (base-map node)
      :children (map node->map (.getElements node))))
  InfixExpression
  (node->map
    [node]
    (assoc (base-map node)
      :children [(node->map (.getLeft node)) (node->map (.getRight node))]))
  FunctionCall
  (node->map
    [node]
    (assoc (base-map node)
      :children (cons (node->map (.getTarget node)) (map node->map (.getArguments node)))))
  ExpressionStatement
  (node->map
    [node]
    (assoc (base-map node) :children [(node->map (.getExpression node))]))
  IfStatement
  (node->map
    [node]
    (assoc (base-map node)
      :children [(node->map (.getCondition node))
                 (node->map (.getThenPart node))
                 (node->map (.getElsePart node))]))
  WithStatement
  (node->map
    [node]
    (assoc (base-map node)
      :children [(node->map (.getExpression node)) (node->map (.getStatement node))]))
  WhileLoop
  (node->map
    [node]
    (assoc (base-map node)
      :children [(node->map (.getCondition node)) (node->map (.getBody node))]))
  ReturnStatement
  (node->map
    [node]
    (assoc (base-map node)
      :children [(node->map (.getReturnValue node))]))
  Jump
  (node->map
    [node]
    (let [child (case (.getType node)
                  Token/CONTINUE (node->map (.getJumpStatement node))
                  Token/BREAK (node->map (.getJumpStatement node))
                  Token/SWITCH (node->map (.getDefault node))
                  Token/TRY (node->map (.getFinally node))
                  Token/LABEL (node->map (.getLoop node))
                  Token/LOOP (node->map (.getContinue node))
                  nil)]
      (if child
        (assoc (base-map node) :children [child])
        (base-map node))))
  Node
  (node->map
    [node]
    (base-map node))
  nil
  (node->map
    [node] nil))

(defn symbol-table
  [^Scope scope]
  (let [symbol-table (.getSymbolTable scope)]
    (if (nil? symbol-table)
      []
      (map (fn [[a b]] b)
        (sort-by first
          (for [[k ^Symbol sym] (.entrySet symbol-table)]
            [(.getIndex sym) {:value (.getName sym) :op (type-keyword (.getDeclType sym))}]))))))

(extend-protocol NodeValue
  Name
  (get-value [this]
    (.getIdentifier this))
  StringLiteral
  (get-value [this]
    (.getValue this))
  Comment
  (get-value [this]
    (.getValue this))
  Node
  (get-value [this]
    (case (.getType this)
      Token/NUMBER (.getDouble this)
      Token/STRING (.getString this)
      ;; else 
      nil)))

(defn parse-js
  ([^java.io.Reader inp-reader ^String source-uri line-no]
    (let [env (CompilerEnvirons.)]
      (.setRecordingLocalJsDocComments env true)
      (.setAllowSharpComments env true)
      (.setRecordingComments env true)
      (assoc (node->map (.parse (Parser. env) inp-reader source-uri ^int line-no)) :root true)))
  ([inp-reader source-uri]
    (parse-js inp-reader source-uri 0))
  ([inp-reader]
    (parse-js inp-reader ".")))
