;;;; spec.lisp --- Based on TIDE log file format specification.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.backend.tidelog)

(define-element (tide)
  (version-major (unsigned-byte 8)
                 :documentation
                 "Major version of the TIDE format used.")
  (version-minor (unsigned-byte 8)
                 :documentation
                 "Minor version of the TIDE format used.")
  (num-channels  (unsigned-byte 32)
                 :documentation
                 "Number of channels of data in the file.")
  (num-chunks    (unsigned-byte 32)
                 :documentation
                 "Number of chunk blocks that are in the file.")
  (:toplevel? t)
  (:documentation
   "Header block of TIDELog file. Implementations are required to
support any TIDE file with the same major version as the
implementation, although not all features supported by the file may be
available.

Note: num-channels and num-chunks are not used or written by RSBag and
will hopefully be removed from the specification."))

(define-element (chan)
  (id            (unsigned-byte 32)
                 :documentation
                 "Channel identification, used to link entries to the
channel.")
  (name          (:string (unsigned-byte 8))
                 :documentation
                 "Name of the channel, as a character string.")
  (type          (:string (unsigned-byte 32))
                 :documentation
                 "String describing the type of the format.")
  (source-name   (:string (unsigned-byte 32))
                 :documentation
                 "Human-readable source description.")
  (source-config (:string (unsigned-byte 32))
                 :documentation
                 "Raw data describing the source of this channel's
data.")
  (format        (:string (unsigned-byte 32))
                 :documentation
                 "Raw data describing the format of the channel's
data.")
  (:toplevel? t)
  (:documentation
 "The CHAN block stores the meta-data for one channel of data.

A channel stores a set of data entries of a single type, indexed by
time. The channel is identified internally within the file by its
unique identification, stored in the `id' field. The channel may be
identified externally by its id or by its unique human-readable name,
stored in the `name' field.

The `type' field indicates the type of source and format in use by
this channel. It should indicate both the connection/transport type
and serialization type, and will vary by these. For example, a logging
tool for the OpenRTM-aist architecture may specify \"openrtm-cdr\" or
\"openrtm-ros\" to indicate that the source connection was an
OpenRTM-aist port using either the CDR serialisation or the ROS
serialisation scheme.

The `source-name' field provides a human-readable form of the source
information. This allows introspection tools to describe the file
contents more completely. For example, this field could contain the
human-readable path of a ROS topic that provided the data.

The `source-config' field provides space for describing the source of
the channel's data in a machine-readable form. Typically this will
describe the connection that was recorded. For example, an
implementation for ROS may store the connection header in this field,
while an implementation for OpenRTM-aist may store the name of the
source port and the port's properties.

The `format' field provides space for describing the serialized
data. It is intended that this field contain enough information to
deserialize the data stored in the file, such that it can be
reconstructed later without any extra information from external to the
file. For example, a ROS implementation may store the message
description in this field."))

(define-element (indx)
  (channel-id (unsigned-byte 32)
              :documentation
              "Points to the channel this block indexes.")
  (count      (unsigned-byte 32)
              :documentation
              "Number of indices in this block.")
  (entries    (:repeated count index-entry))
  (:toplevel? t)
  (:documentation
   "The INDX block provides an index for random-access in time to the
data of one channel stored in the file. It links timestamps with the
data values stored in the chunk blocks.

This block has a fixed-length section consisting of the `channel-id'
and `count' fields and a variable-length section consisting of COUNT
`index-entry' elements."))

(define-element (index-entry)
  (chunk-id  (unsigned-byte 32)
             :documentation
             "Points to the chunk this entry is stored in.")
  (timestamp (unsigned-byte 64)
             :documentation
             "Timestamp of the entry.")
  (offset    (unsigned-byte 64)
             :documentation
             "Offset (in bytes) in the chunk's uncompressed data of
the entry.")
  (:documentation
   "The offset points to the specific position in the data of the
entry relative to the start of its chunk. If the chunk is compressed,
the offset refers to the data once it has been uncompressed."))

(define-element (chnk)
  (chunk-id    (unsigned-byte 32)
               :documentation
               "Chunk identification, used to link entries to chunks.")
  (count       (unsigned-byte 32)
               :documentation
               "Number of entries in this chunk.")
  (start       (unsigned-byte 64)
               :documentation
               "Timestamp of the first entry in this chunk.")
  (end         (unsigned-byte 64)
               :documentation
               "Timestamp of the last entry in this chunk.")
  (compression (unsigned-byte 8)
               :documentation
               "Indicates the compression used on the entries.

The value must be one of the following values:
0: No compression.
1: gzip compression.
2: bzip2 compression.")
  (entries     (:repeated count chunk-entry))
  (:toplevel? t)
  (:documentation
   "Each CHNK block stores a collection of recorded data items.

This block has a fixed-length section consisting of the `chunk-id',
`count', `start', `end' and `compression' fields and a variable-length
section consisting of COUNT `chunk-entry' elements."))

(define-element (chunk-entry)
  (channel-id (unsigned-byte 32)
              :documentation
              "Points to the channel this entry belongs to.")
  (timestamp  (unsigned-byte 64)
              :documentation
              "Timestamp of the entry.")
  (size       (unsigned-byte 32)
              :documentation
              "Size of the following serialised data.")
  (entry      (:blob size)
              :documentation
              "Serialized entry data.")
  (:documentation
   "A CHUNK-ENTRY block stores a single data item that was recorded on
a particular channel and a particular point in time."))
