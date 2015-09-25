R7RS cryptographic library
==========================

Aeolus is an R7RS cryptographic library. The goal for this library is that
providing pure R7RS cryptographic library which can be used to implement
secure protocols such as TLS or SSH.

Status
------

This library is before alpha status. The API would most likely change
in the near future.

How to use
----------

See [tests/test-des.scm](tests/test-des.scm).


Requirements
------------

This library requires the following SRFI:

- SRFI-60, SRFI-33 or R6RS
- SRFI-64 for testing

Consideration
-------------

Even though the library is aming to be a cryptographic library, its security
is not secure enough. For example, key information will remain on memory
until GC is invoked.

Supporting implementations
--------------------------

The library is tested on the following R7RS implementations:

- Sagittarius Scheme 0.6.8
- Gauche 0.9.4
- Chibi Scheme 0.7.3

Copyright and license
---------------------

Copyright 2015 Takashi Kato. Code released under the BSD-style license.
