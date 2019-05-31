;;;; lightpuzzle.asd

(asdf:defsystem #:lightpuzzle
  :description "Describe lightpuzzle here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "lightpuzzle")))
