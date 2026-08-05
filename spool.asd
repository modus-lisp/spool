;;;; spool.asd — a podcast client in pure Common Lisp.

(asdf:defsystem :spool
  :description "Podcast subscriptions, feed parsing, an episode cache, and playback
that hands out a reed source thunk — so the same object plays into a glass mixer, an
RTP sender, or a file.  Feeds are fetched over weft's pure-CL TLS and parsed by a
feed-shaped XML reader; audio is decoded incrementally by reed.  No FFI."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("reed" "weft/fetch")
  :serial t
  :components ((:module "src" :serial t
                :components ((:file "packages")
                             (:file "xml")
                             (:file "feed")
                             (:file "library")
                             (:file "play")))))

(asdf:defsystem :spool/glass
  :description "spool's audio into a glass session mixer: the box plays the show, and
every listener the session has — a VNC viewer, a WebRTC peer — hears it."
  :depends-on ("spool" "glass/audio")
  :serial t
  :components ((:module "src" :serial t :components ((:file "glass")))))

(asdf:defsystem :spool/app
  :description "The CLIM application: subscriptions, episodes and a transport, as a
window in the glass desktop."
  :depends-on ("spool/glass" "mcclim")
  :serial t
  :components ((:module "src" :serial t :components ((:file "app")))))
