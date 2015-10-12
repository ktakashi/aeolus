(import (scheme base)
	(scheme write)
	(aeolus digest)
	(aeolus digest sha1)
	(aeolus-test))

(test-begin "Digest")
#;(print #u8(#xa9 #x99 #x3e #x36 #x47 #x06 #x81 #x6a #xba #x3e
	   #x25 #x71 #x78 #x50 #xc2 #x6c #x9c #xd0 #xd8 #x9d))
;;(print (bytevector->digest SHA1 (string->utf8 "abc")))
(test-equal "SHA1 abc" #u8(#xa9 #x99 #x3e #x36 #x47 #x06 #x81 #x6a #xba #x3e
			   #x25 #x71 #x78 #x50 #xc2 #x6c #x9c #xd0 #xd8 #x9d)
	    (bytevector->digest SHA1 (string->utf8 "abc")))

(test-equal "SHA1 abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
	    #u8(#x84 #x98 #x3E #x44 #x1C #x3B #xD2 #x6E #xBA #xAE
		#x4A #xA1 #xF9 #x51 #x29 #xE5 #xE5 #x46 #x70 #xF1)
	    (bytevector->digest SHA1
		(string->utf8 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")))

(test-equal "SHA1 abc start end"
	    #u8(#xa9 #x99 #x3e #x36 #x47 #x06 #x81 #x6a #xba #x3e
		#x25 #x71 #x78 #x50 #xc2 #x6c #x9c #xd0 #xd8 #x9d)
	    (bytevector->digest SHA1 
		(string->utf8 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
		0 3))


(test-end)
