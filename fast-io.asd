(defsystem :fast-io
  :description "Alternative I/O mechanism to a stream or vector"
  :author "Ryan Pavlik"
  :license "NewBSD"

  :depends-on (:alexandria :trivial-gray-streams)

  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "types")
   (:file "io")
   (:file "gray")))
