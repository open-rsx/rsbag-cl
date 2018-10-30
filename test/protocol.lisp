;;;; protocol.lisp --- Unit tests for the client-facing protocol.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.test)

(deftestsuite protocol-root (root)
  ()
  (:function
   (pathname/existing ()
     (asdf:system-relative-pathname
      (asdf:find-system :rsbag/test)
      "test/data/minimal.mock")))
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
        (,(namestring/existing) :direction :input :backend :mock)
        (,(pathname/existing)   :direction :input :backend :mock)
        (,(stream)              :direction :input :backend :mock))
    (close (apply #'open-bag args))))

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
        (,(stream)              :backend :mock)
        ;; invalid direction
        (,(namestring/existing) :direction :invalid :backend :mock)
        (,(pathname/existing)   :direction :invalid :backend :mock)
        (,(stream)              :direction :invalid :backend :mock)
        ;; invalid backend
        (,#P"name-but-no-type"  :direction :input)
        (,(namestring/existing) :direction :input :backend :no-such-backend)
        (,(pathname/existing)   :direction :input :backend :no-such-backend)
        (,(stream)              :direction :input :backend :no-such-backend)
        ;; file exists
        (,(namestring/existing) :direction :output :backend :mock)
        (,(pathname/existing)   :direction :output :backend :mock)
        ;; cannot specify flush strategy for input direction
        (,(namestring/existing) :direction :input :flush-strategy :some-strategy)
        (,(pathname/existing)   :direction :input :flush-strategy :some-strategy)
        (,(stream)              :direction :input :backend :mock :flush-strategy :some-strategy)
        ;; invalid flush strategy
        (,(namestring/existing) :direction :io :flush-strategy :no-such-strategy)
        (,(pathname/existing)   :direction :io :flush-strategy :no-such-strategy)
        (,(stream)              :direction :io :backend :mock :flush-strategy :no-such-strategy))

    (ensure-condition 'error
      (apply #'open-bag args))))
