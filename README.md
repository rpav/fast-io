# fast-io

```lisp
(deftype octet '(unsigned-byte 8))
(deftype octet-vector '(simple-array octet (*)))
```

Fast-io is about improving performance to octet-vectors and octet
streams (though primarily the former, while wrapping the latter).
Imagine we're creating messages for the network. If we try and fill an
octet-vector with 50 bytes, 50000 times, here are the results (SBCL
1.0.57):

<table>
<tr>
  <th></th>
  <th align=right><tt>vector-push-extend</tt>:</th>
  <th align=right><tt>flexi-streams</tt>:</th>
  <th align=right><tt>fast-io</tt>:</th>
</tr>
<tr>
  <td align=right>Time:</td>
  <td align=right>0.767s</td>
  <td align=right>2.545s</td>
  <td align=right>0.090s</td>
</tr>
<tr>
  <td align=right>Bytes consed:</td>
  <td align=right>104,778,352</td>
  <td align=right>274,452,768</td>  
  <td align=right>18,373,904</td>
</tr>
</table>

(See `t/benchmarks.lisp` for the exact code used.)

It *should* be surprising that it takes a nontrivial effort to achieve
relatively decent performance to octet-vectors, but probably isn't.
However, fast-io provides a relatively straightforward interface for
reading and writing either a stream or a vector:

```lisp
;;; Write a byte or sequence, optionally to a stream:

(with-fast-output (buffer [STREAM])
  (fast-write-byte BYTE buffer))

(with-fast-output (buffer [STREAM])
  (fast-write-sequence OCTET-VECTOR buffer [START [END]]))

;;; Read from a vector or stream:

(with-fast-input (buffer VECTOR [STREAM])
  (fast-read-byte buffer))

(with-fast-input (buffer VECTOR [STREAM])
  (let ((vec (make-octet-vector N)))
    (fast-read-sequence vec buffer [START [END]])))
```

## Multi-byte and Endianness

Fast-io provides a host of read and write functions for big- and
little-endian reads, in the following forms:

* `write[u]{8,16,32,64,128}{-be,-le}`: E.g., `(write32-be VALUE
  BUFFER)` will write the specified 32-bit value to the specified
  buffer with a *big-endian* layout.  Likewise, `(writeu16-le VALUE
  BUFFER)` will write an *unsigned* 16-bit value in *little-endian*
  layout.

* `read[u]{8,16,32,64,128}{-be,-le}`: Similarly, `(read64-le BUFFER)`
  will read a 64-bit value from the buffer with little-endian layout.

## Streams

Obviously, the above API isn't built around Lisp streams, or even
gray-streams.  However, fast-io provides a small wrapper using
`trivial-gray-streams`, and supports `{WRITE,READ}-SEQUENCE`:

```lisp
(let ((stream (make-instance 'fast-io:fast-output-stream)))
  (write-sequence (fast-io:octets-from '(1 2 3 4)) stream))
```

Both `fast-input-stream` and `fast-output-stream` support backing a
stream, much like using the plain fast-io buffers.  However, using the
gray-streams interface is a 3-4x as slow as using the buffers alone.
Simple benchmarks show the gray-streams interface writing 1M 50-byte
vectors in about 1.7s, whereas simply using buffers is about 0.8s.
Consing remains similar between the two.
