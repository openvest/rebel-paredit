(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]
            [clojure.string :as str]))

(def micro-ver 60 #_(-> (b/process {:command-args (str/split "git rev-list --count master..HEAD" #" +") :out :capture})
                  :out
                  (str/trim)))
(def minor-ver 2 #_(-> (b/process {:command-args (str/split "git rev-list --count master" #" +") :out :capture})
                   :out
                   (str/trim)))

(def lib 'com.openvest/repl-balance)
(def version (format "0.%s.%s" minor-ver micro-ver))
(def main 'repl-balance.main)
(def class-dir "target/classes")
;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn test
 "Run all the tests."
 [opts]
  (let [basis    (b/create-basis {:aliases [:test]})
        cmds     (b/java-command
                  {:basis     basis
                   :main      'clojure.main
                   :main-args ["-m" "kaocha.runner"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)

(defn- pom-template [version]
  [[:description "Clojure command line repl with paredit.
  Uses jline3, originally forked from rebel-readline."]
   [:url "https://github.com/openvest/repl-balance"]
   [:licenses
    [:license
     [:name "Eclipse Public License"]
     [:url "http://www.eclipse.org/legal/epl-v10.html"]]]
   [:developers
    [:developer
     [:name "Phil Cooper"]]]
   [:scm
    [:url "https://github.com/openvest/repl-balance"]
    [:connection "scm:git:https://github.com/openvest/repl-balance.git"]
    [:developerConnection "scm:git:ssh:git@github.com:openvest/repl-balance.git"]
    [:tag (str "v" version)]]])

(defn- jar-opts [opts]
  (assoc opts
          :lib lib   :version version
          :jar-file  (format "target/%s-%s.jar" lib version)
          :basis     (b/create-basis {})
          :class-dir class-dir
          :target    "target"
          :src-dirs  ["src"]
          :pom-data  (pom-template version)))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar
  "Run the CI pipeline of tests (and build the JAR)."
  [opts]
  ;(test opts)
  (clean opts)
  (let [opts (jar-opts opts)]
    (println "\nWriting pom.xml...")
    (b/write-pom opts)

    (println "\nCopying source...")
    (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})

    (println "\nBuilding JAR..." (:jar-file opts))
    (b/jar opts))
  opts)


(defn install
  "Install the JAR locally."
  [opts]
  (let [opts (jar-opts opts)]
    (b/install opts))
  opts)

(defn deploy
  "Deploy the JAR to Clojars."
  [opts]
  (let [{:keys [jar-file] :as opts} (jar-opts opts)]
    (dd/deploy {:installer :remote :artifact (b/resolve-path jar-file)
                :pom-file (b/pom-path (select-keys opts [:lib :class-dir]))}))
  opts)
;; End of Build configuration

(defn echo-opts [opts] (println "\nopts:") (print opts))
