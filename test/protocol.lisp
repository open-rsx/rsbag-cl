;;;; protocol.lisp --- Unit tests for the client-facing protocol.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.test)

(deftestsuite protocol-root (root)
  ()
  (:function
   (pathname/existing ()
     (asdf:system-relative-pathname
      (asdf:find-system :cl-rsbag-test)
      "test/data/minimal.tide")))
  (:function
   (namestring/existing ()
     (pathname/existing)))
  (:function
   (stream ()
     (open (pathname/existing)
           :element-type '(unsigned-byte 8)
           :direction    :input)))
  (:documentation
   "Unit test for the client-facing protocol."))

(addtest (protocol-root
          :documentation
          "Test case for the `open-bag' function.")
  open-bag/valid

  (ensure-cases (args)
      `((,(namestring/existing) :direction :input)
        (,(pathname/existing)   :direction :input)
        (,(namestring/existing) :direction :input :backend :tide)
        (,(pathname/existing)   :direction :input :backend :tide)
        (,(stream)              :direction :input :backend :tide))
    (handler-bind
        ((open-error #'continue))
      (let ((bag (apply #'open-bag args)))
        (close bag)))))

(addtest (protocol-root
          :documentation
          "Test cases for which the `open-bag' function has to signal
errors.")
  open-bag/invalid

  (ensure-cases (args)
      `(;; :backend, :direction missing
        (,(stream))
        ;; :backend missing
        (,(stream)              :direction :input)
        ;; :direction missing
        (,(stream)              :backend :tide)
        ;; invalid direction
        (,(namestring/existing) :direction :invalid :backend :tide)
        (,(pathname/existing)   :direction :invalid :backend :tide)
        (,(stream)              :direction :invalid :backend :tide)
        ;; invalid backend
        (,(namestring/existing) :direction :input :backend :no-such-backend)
        (,(pathname/existing)   :direction :input :backend :no-such-backend)
        (,(stream)              :direction :input :backend :no-such-backend)
        ;; file exists
        (,(namestring/existing) :direction :output :backend :tide)
        (,(pathname/existing)   :direction :output :backend :tide)
        ;; cannot specify flush strategy for input direction
        (,(namestring/existing) :direction :input :flush-strategy :some-strategy)
        (,(pathname/existing)   :direction :input :flush-strategy :some-strategy)
        (,(stream)              :direction :input :backend :tide :flush-strategy :some-strategy)
        ;; invalid flush strategy
        (,(namestring/existing) :direction :io :flush-strategy :no-such-strategy)
        (,(pathname/existing)   :direction :io :flush-strategy :no-such-strategy)
        (,(stream)              :direction :io :backend :tide :flush-strategy :no-such-strategy))

    (ensure-condition 'error
      (apply #'open-bag args))))
