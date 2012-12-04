(defsystem :fast-io
  :description "Alternative I/O mechanism to a stream or vector"
  :author "Ryan Pavlik"
  :license "NewBSD"
  :version "1.0"

  :depends-on (:alexandria :trivial-gray-streams :static-vectors)

  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "types")
   (:file "io")
   (:file "gray")))
