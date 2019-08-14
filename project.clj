(def JVMOPTS
  "Per os jvm options. Options common to all cases go under
  `:any`. Options specific to one OS go under the key returned by
  `leiningen.core.eval/get-os` for that system. Temporarily disabled
  options can be kept under `:disabled`."
  {:any
   ["-Xms512m" "-Xmx1g"                 ; Minimum and maximum sizes of the heap
    "-XX:+CMSConcurrentMTEnabled"       ; Enable multi-threaded concurrent gc work (ParNewGC)
    "-XX:MaxGCPauseMillis=20"           ; Specify a target of 20ms for max gc pauses
    "-XX:MaxNewSize=257m"               ; Specify the max and min size of the new
    "-XX:NewSize=256m"                  ;  generation to be small
    "-XX:+UseTLAB"                      ; Uses thread-local object allocation blocks. This
                                        ;  improves concurrency by reducing contention on
                                        ;  the shared heap lock.
    "-XX:MaxTenuringThreshold=0"]       ; Makes the full NewSize available to every NewGC
                                        ;  cycle, and reduces the pause time by not
                                        ;  evaluating tenured objects. Technically, this
                                        ;  setting promotes all live objects to the older
                                        ;  generation, rather than copying them.
   :disabled
   ["-XX:ConcGCThreads=2"               ; Use 2 threads with concurrent gc collections
    "-XX:TieredCompilation"             ; JVM7 - combine both client and server compilation
                                        ;  strategies
    "-XX:CompileThreshold=1"            ; JIT each function after one execution
    "-XX:+PrintGC"                      ; Print GC info to stdout
    "-XX:+PrintGCDetails"               ;  - with details
    "-XX:+PrintGCTimeStamps"]})         ;  - and timestamps


(defproject hungry "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [overtone "0.10.6"]
                 [leipzig "0.10.0"]
                 [seesaw "1.5.0"]]
  :jvm-opts ^:replace ["-Xms512m" "-Xmx1g"                 ; Minimum and maximum sizes of the heap
                       "-XX:+CMSConcurrentMTEnabled"       ; Enable multi-threaded concurrent gc work (ParNewGC)
                       "-XX:MaxGCPauseMillis=20"           ; Specify a target of 20ms for max gc pauses
                       "-XX:MaxNewSize=257m"               ; Specify the max and min size of the new
                       "-XX:NewSize=256m"                  ;  generation to be small
                       "-XX:+UseTLAB"                      ; Uses thread-local object allocation blocks. This
                                                           ;  improves concurrency by reducing contention on
                                                           ;  the shared heap lock.
                       "-XX:MaxTenuringThreshold=0"]
  :repl-options {:init-ns hungry.core})
