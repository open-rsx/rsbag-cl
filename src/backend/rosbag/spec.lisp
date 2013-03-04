;;;; spec.lisp ---
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Note
;;;;
;;;; Documentation strings have been taken from the Rosbag and ROS
;;;; documentation obtained from
;;;; http://www.ros.org/wiki/Bags/Format/2.0 and
;;;; http://www.ros.org/wiki/ROS/Connection%20Header.

(cl:in-package #:rsbag.backend.rosbag)

;;; Record class registry

(defvar *record-classes* (make-hash-table)
  "Maps record opcodes to record class names.")

(defun find-record-class (opcode)
  (or (gethash opcode *record-classes*)
      (error "~@<No record class for opcode ~D.~@:>" opcode)))

(defun (setf find-record-class) (new-value opcode)
  (setf (gethash opcode *record-classes*) new-value))

;;;

(defvar +header/2.0+ (concatenate
                      'simple-octet-vector
                      (map 'list #'char-code "#ROSBAG V2.0") '(10))
  "TODO(jmoringe): document")

;;; Record definitions

(define-record (bag-header :opcode #x03)
  (index-position   (unsigned-byte 64)
                    :id "index_pos"
                    :documentation
                    "File offset of first record after the chunk
                     section.")
  (connection-count (unsigned-byte 32)
                    :id "conn_count"
                    :documentation
                    "Number of unique connections contained in the
                     file.")
  (chunk-count      (unsigned-byte 32)
                    :id "chunk_count"
                    :documentation
                    "Number of chunk records in the file.")
  (:documentation
   "Stores information about the entire bag, such as the offset to the
    first index data record, and the number of chunks and connections.

    The bag header record occurs once in the file as the first record.

    The bag header record is padded out by filling data with ASCII
    space characters (0x20) so that additional information can be
    added after the bag file is recorded. Currently, this padding is
    such that the header is 4096 bytes long."))

(define-record (chunk :opcode #x05)
  (compression string
               :documentation
               "Compression type for the data

                The supported compression values are \"none\" and
                \"bz2\". The compressed size of the chunk can be found
                in the data_len field of the record.")
  (size        (unsigned-byte 32)
               :documentation
               "size in bytes of the uncompressed chunk")
  (data-length (unsigned-byte 32)
               :id "data_len"
               :documentation
               "size in bytes of the uncompressed chunk")
  (&data       (:compressed compression (:repeated :detect))
               :documentation
               "The data for a chunk record consists of message data
                and connection records, compressed using the method
                specified in the chunk record header.")
  (:documentation
   "Stores (possibly compressed) connection and message records."))

(define-record (connection-header)
  (type               string
                      :documentation
                      "Message type.")
  (topic              string
                      :documentation
                      "Name of the topic the subscriber is connecting
                       to.")
  (md5sum             string
                      :documentation
                      "md5sum of the message type.")
  (message-definition string
                      :id "message_definition"
                      :documentation
                      "Full text of message definition (output of
                       gendeps --cat).")
  (caller-id          string
                      :id "callerid"
                      :documentation
                      "Name of node sending data.")
  (latching           string
                      :documentation
                      "Publisher is in latching mode (i.e. sends the
                       last value published to new subscribers).")
  (:documentation
   "Stores the header of a ROS connection, including topic name and
    full text of the message definition.

    Two topic fields exist (in the record and connection
    headers). This is because messages can be written to the bag file
    on a topic different from where they were originally published."))

;; TODO include these?
;; service: name of service the client is calling
;; error: human-readable error message if the connection is not successful
;; persistent: sent from a service client to a service. If '1', keep connection open for multiple requests.
;; tcp_nodelay: sent from subscriber to publisher. If '1', publisher will set TCP_NODELAY on socket if possible

(defmethod unpack :around ((source simple-array) (object connection-header)
                           &optional start)
  (let+ (((&values value length)
          (call-next-method source object (- start 4))))
    (values value (- length 4))))

(define-record (connection :opcode #x07)
  (id    (unsigned-byte 32)
         :id "conn"
         :documentation
         "Unique connection ID.")
  (topic string
         :documentation
         "Topic on which the messages are stored.")
  (&data connection-header
         :documentation
         "The data consists of a string containing the connection
          header in the same format as a bag record header. The
          following fields must appear in the connection header:
          topic, type, md5sum, message_definition. Optional fields
          include: callerid, latching.")
  (:documentation
   "Stores the header of a ROS connection, including topic name and
    full text of the message definition.

    Two topic fields exist (in the record and connection
    headers). This is because messages can be written to the bag file
    on a topic different from where they were originally published."))

(define-record (message-data :opcode #x02)
  (connection  (unsigned-byte 32)
               :id "conn"
               :documentation
               "ID for connection on which message arrived.")
  (time        (unsigned-byte 64)
               :documentation
               "Time at which the message was received.")
  (&data       :blob
               :documentation
               "The data in these records is the serialized message
                data in the ROS serialization format.")
  (:documentation
   "Stores the serialized message data (which can be zero-length) with
    the ID of the connection."))

(define-record (index-data :opcode #x04)
  (version    (unsigned-byte 32)
              :id "ver"
              :documentation
              "Index data record version.")
  (connection (unsigned-byte 32)
              :id "conn"
              :documentation
              "Connection ID.")
  (count      (unsigned-byte 32)
              :documentation
              "Number of messages on CONNECTION in the preceding
               chunk.")
  (&data      :blob #+no (:versioned
                (1 (:repeated index-data-body/v1)))
              :documentation
              "The data in these records depends on the version in the
               header.")
  (:documentation
   "Stores an index of messages in a single connection of the
    preceding chunk."))

;; TODO these entries seem to be 12 byte instead of 20. why?
(define-record (index-data-body/v1)
  (time         (unsigned-byte 64)
                :documentation
                "Time at which the message was received.")
  (chunk-offset (unsigned-byte 64)
                :id "chunk_pos"
                :documentation
                "Chunk record offset.")
  (offset       (unsigned-byte 32)
                :documentation
                "Offset of message data record in uncompressed chunk
                 data.")
  (:documentation
   "The current version is version 1, which consists of count
    repeating occurrences of timestamps, chunk record offsets and
    message offsets:"))

(define-record (chunk-info :opcode #x06)
  (version      (unsigned-byte 32)
                :id "ver"
                :documentation
                "Chunk info record version.")
  (chunk-offset (unsigned-byte 64)
                :id "chunk_pos"
                :documentation
                "Offset of the chunk record.")
  (start-time   (unsigned-byte 64)
                :id "start_time"
                :documentation
                "Timestamp of earliest message in the chunk.")
  (end-time     (unsigned-byte 64)
                :id "end_time"
                :documentation
                "Timestamp of latest message in the chunk.")
  (count        (unsigned-byte 32)
                :documentation
                "Number of connections in the chunk.")
  (&data        :blob #+no (:versioned
                  (1 (:repeated chunk-info-body/v1)))
                :documentation
                "The data in these records depends on the version in
                 the header. The current version is version 1, which
                 consists of count repeating occurrences of connection
                 ID's and message counts:")
  (:documentation
   "Stores information about messages in a chunk."))

(define-record (chunk-info-body/v1)
  (connection (unsigned-byte 32)
              :id "con"
              :documentation
              "Connection id.")
  (count      (unsigned-byte 32)
              :documentation
              "Number of messages that arrived on this connection in
               the chunk."))
