;;;; reloading.lisp --- Support for image dumping and reloading.
;;;;
;;;; Copyright (C) 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag)

;; Make sure that the rsbag threadpool is shutdown when dumping an
;; image and restarted when restoring an image.

(uiop:register-image-dump-hook 'stop-threadpool nil)
(uiop:register-image-restore-hook 'start-threadpool nil)
