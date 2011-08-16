;;; spec.lisp --- Based on TIDE log file format specification.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsbag.backend.tidelog)

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
  (:documentation
   "Implementations are REQUIRED to support any TIDE file with the
same major version as the implementation, although not all features
supported by the file may be available."))

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
(:documentation
 "The ``CHAN`` block stores the meta-data for ONE channel of data.

A channel stores a set of data entries of a single type, indexed by
time. The channel is identified internally in the TIDE file by its
unique identification, stored in the ``ID`` field. The channel may be
identified externally by its ID or by its unique human-readable name,
stored in the ``Name`` field.

The ``Type`` field indicates the type of source and format in use by
this channel. It should indicate both the connection/transport type
and serialization type, and will vary by these. For example, a logging
tool for the OpenRTM-aist architecture may specify ``openrtm-cdr`` or
``openrtm-ros`` to indicate that the source connection was an
OpenRTM-aist port using either the CDR serialisation or the ROS
serialisation scheme.

The ``Source string`` field provides a human-readable form of the
source information. This allows TIDE file introspection tools to
describe the file contents more completely. For example, this field
could contain the human-readable path of a ROS topic that provided the
data.

The ``Source`` field provides space for describing the source of the
channel's data in a machine-readable form. Typically this will
describe the connection that was recorded. For example, an
implementation for ROS may store the connection header in this field,
while an implementation for OpenRTM-aist may store the name of the
source port and the port's properties.

The ``Format`` field provides space for describing the serialised
data. It is intended that this field contain enough information to
deserialise the data stored in the file, such that it can be
reconstructed later without any extra information from external to the
TIDE file. For example, a ROS implementation may store the message
description in this field."))

(define-element (indx)
  (channel-id (unsigned-byte 32)
	      :documentation
	      "Points to the channel this block indexes.")
  (count      (unsigned-byte 32)
	      :documentation
	      "Number of indices in this block.")
  (entries    (:repeated count index-entry))
  (:documentation
   "The ``INDX`` block provides an index for random-access in time to
the data of one channel stored in the file. It links time stamps with
the data values stored in the chunk blocks.

This block has a fixed-length section and a variable-length
section."))

(define-element (index-entry)
  (chunk-id  (unsigned-byte 32)
	     :documentation
	     "Points to the chunk this entry is stored in.")
  (timestamp (unsigned-byte 64)
	     :documentation
	     "Time stamp of the entry.")
  (offset    (unsigned-byte 64)
	     :documentation
	     "Offset (in bytes) in the chunk's uncompressed data of
the entry.")
  (:documentation
   "The variable-length section contains the indices. It has exactly
``Count`` occurances of the following fields:

The offset points to the specific position in the data of the entry
relative to the start of its chunk. If the chunk is compressed, the
offset refers to the data once it has been uncompressed."))

(define-element (chnk)
  (chunk-id    (unsigned-byte 32)
	       :documentation
	       "Chunk identification, used to link entries to chunks.")
  (count       (unsigned-byte 32)
	       :documentation
	       "Number of entries in this chunk.")
  (start       (unsigned-byte 64)
	       :documentation
	       "Time stamp of the first entry in this chunk.")
  (end         (unsigned-byte 64)
	       :documentation
	       "Time stamp of the last entry in this chunk.")
  (compression (unsigned-byte 8)
	       :documentation
	       "Indicates the compression used on the entries.

The value of the ``Compression`` field must be one of the following
values:

0
No compression.

1
gzip compression.

2
bzip2 compression.")
  (entries     (:repeated count chunk-entry))
  (:documentation
   "``CHNK`` blocks store the recorded data entries.

This block has a fixed-length section and a variable-length
section."))

(define-element (chunk-entry)
  (channel-id (unsigned-byte 32)
	      :documentation
	      "Points to the channel this entry belongs to.")
  (timestamp  (unsigned-byte 64)
	      :documentation
	      "Time stamp of the entry.")
  (size       (unsigned-byte 32)
	      :documentation
	      "Size of the following serialised data.")
  (entry      (:blob size)
	      :documentation
	      "Serialized entry data."))
