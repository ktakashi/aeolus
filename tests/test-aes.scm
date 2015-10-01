(import (scheme base)
	(scheme cxr)
	(aeolus cipher)
	(aeolus cipher aes)
	(aeolus modes ecb)
	(aeolus modes cbc)
	(aeolus modes ctr)
	(aeolus modes parameters)
	(aeolus-test)
	(extra-aes-vectors))

(test-begin "AES")

(test-assert "cipher?"
	     (cipher? 
	      (make-cipher AES #u8(1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6) mode-ecb)))

;; from 
;; http://www.inconteam.com/software-development/41-encryption/55-aes-test-vectors
(define test-aes-vectors
  '(
    ;; ken-len key plain cipher
    #(16 #x2b7e151628aed2a6abf7158809cf4f3c #x6bc1bee22e409f96e93d7e117393172a #x3ad77bb40d7a3660a89ecaf32466ef97)
    #(16 #x2b7e151628aed2a6abf7158809cf4f3c #xae2d8a571e03ac9c9eb76fac45af8e51 #xf5d3d58503b9699de785895a96fdbaaf)
    #(16 #x2b7e151628aed2a6abf7158809cf4f3c #x30c81c46a35ce411e5fbc1191a0a52ef #x43b1cd7f598ece23881b00e3ed030688)
    #(16 #x2b7e151628aed2a6abf7158809cf4f3c #xf69f2445df4f9b17ad2b417be66c3710 #x7b0c785e27e8ad3f8223207104725dd4)
    #(24 #x8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b #x6bc1bee22e409f96e93d7e117393172a #xbd334f1d6e45f25ff712a214571fa5cc)
    #(24 #x8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b #xae2d8a571e03ac9c9eb76fac45af8e51 #x974104846d0ad3ad7734ecb3ecee4eef)
    #(24 #x8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b #x30c81c46a35ce411e5fbc1191a0a52ef #xef7afd2270e2e60adce0ba2face6444e)
    #(24 #x8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b #xf69f2445df4f9b17ad2b417be66c3710 #x9a4b41ba738d6c72fb16691603c18e0e)
    #(32 #x603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4 #x6bc1bee22e409f96e93d7e117393172a #xf3eed1bdb5d2a03c064b5a7e3db181f8)
    #(32 #x603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4 #xae2d8a571e03ac9c9eb76fac45af8e51 #x591ccb10d410ed26dc5ba74a31362870)
    #(32 #x603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4 #x30c81c46a35ce411e5fbc1191a0a52ef #xb6ed21b99ca6f4f9f153e7b1beafed1d)
    #(32 #x603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4 #xf69f2445df4f9b17ad2b417be66c3710 #x23304b7a39f9f3ff067d8d8f9e24ecc7)
    ))

(define (test-aes-ecb vec)
  (let ((key (integer->bytevector (vector-ref vec 1) (vector-ref vec 0)))
	(pt (integer->bytevector (vector-ref vec 2) 16))
	(ct (integer->bytevector (vector-ref vec 3) 16)))
    (define cipher (make-cipher AES key mode-ecb))

    (cipher-encrypt cipher pt)
    (test-equal (string-append "encrypt: (" 
			       (number->string (vector-ref vec 0))
			       ") "
			       (number->string (vector-ref vec 2) 16)
			       " -> "
			       (number->string (vector-ref vec 3) 16))
		  ct
		  (cipher-encrypt cipher pt))
    (cipher-decrypt cipher ct)
    (test-equal (string-append "decrypt: (" 
			       (number->string (vector-ref vec 0))
			       ") "
			       (number->string (vector-ref vec 3) 16)
			       " -> "
			       (number->string (vector-ref vec 2) 16))
		pt
		(cipher-decrypt cipher ct))))

(for-each test-aes-ecb test-aes-vectors)
(for-each test-aes-ecb extra-aes-ecb-vectors)

(define test-aes-ctr-vectors
  '(;; key-len key iv plain cipher1 ... plainn  ciphern
    (16
     #x2b7e151628aed2a6abf7158809cf4f3c 
     #xf0f1f2f3f4f5f6f7f8f9fafbfcfdfeff 
     (#x6bc1bee22e409f96e93d7e117393172a #x874d6191b620e3261bef6864990db6ce)
     (#xae2d8a571e03ac9c9eb76fac45af8e51 #x9806f66b7970fdff8617187bb9fffdff)
     (#x30c81c46a35ce411e5fbc1191a0a52ef #x5ae4df3edbd5d35e5b4f09020db03eab)
     (#xf69f2445df4f9b17ad2b417be66c3710 #x1e031dda2fbe03d1792170a0f3009cee))
    (24 
     #x8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b 
     #xf0f1f2f3f4f5f6f7f8f9fafbfcfdfeff
     (#x6bc1bee22e409f96e93d7e117393172a #x1abc932417521ca24f2b0459fe7e6e0b)
     (#xae2d8a571e03ac9c9eb76fac45af8e51 #x090339ec0aa6faefd5ccc2c6f4ce8e94)
     (#x30c81c46a35ce411e5fbc1191a0a52ef #x1e36b26bd1ebc670d1bd1d665620abf7)
     (#xf69f2445df4f9b17ad2b417be66c3710 #x4f78a7f6d29809585a97daec58c6b050))
    (32 
     #x603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4
     #xf0f1f2f3f4f5f6f7f8f9fafbfcfdfeff
     (#x6bc1bee22e409f96e93d7e117393172a #x601ec313775789a5b7a7f504bbf3d228)
     (#xae2d8a571e03ac9c9eb76fac45af8e51 #xf443e3ca4d62b59aca84e990cacaf5c5)
     (#x30c81c46a35ce411e5fbc1191a0a52ef #x2b0930daa23de94ce87017ba2d84988d)
     (#xf69f2445df4f9b17ad2b417be66c3710 #xdfc9c58db67aada613c2dd08457941a6))
    ))

(define (test-aes-ctr lis)
  (let ((key (integer->bytevector (cadr lis) (car lis)))
	(iv (integer->bytevector (caddr lis) 16))
	(v* (cdddr lis)))

    (define enc-cipher 
      (make-cipher AES key mode-ctr (make-iv-paramater iv)))
    (define dec-cipher 
      (make-cipher AES key mode-ctr (make-iv-paramater iv)))
    
    (for-each (lambda (pt&ct)
		(let ((pt (integer->bytevector (car pt&ct) 16))
		      (ct (integer->bytevector (cadr pt&ct) 16)))
		  (test-equal (string-append "CTR encrypt: (" 
					     (number->string (car lis))
					     ") "
					     (number->string (car pt&ct) 16)
					     " -> "
					     (number->string (cadr pt&ct) 16))
			      ct
			      (cipher-encrypt enc-cipher pt))
		  (test-equal (string-append "CTR decrypt: (" 
					     (number->string (car lis))
					     ") "
					     (number->string (car pt&ct) 16)
					     " -> "
					     (number->string (cadr pt&ct) 16))
			      pt
			      (cipher-decrypt dec-cipher ct))))
	      v*)))

(for-each test-aes-ctr test-aes-ctr-vectors)


(define (test-rfc3686 i vec)
  (let ((key (vector-ref vec 0))
	(iv (vector-ref vec 1))
	(nonce (vector-ref vec 2))
	(plain (vector-ref vec 3))
	(cipher (vector-ref vec 4)))
    (define enc-c (make-cipher AES key mode-ctr
			       (make-composite-parameter
				(make-rfc3686-parameter iv nonce))))
    (define dec-c (make-cipher AES key mode-ctr
			       (make-composite-parameter
				(make-rfc3686-parameter iv nonce))))
    (test-equal (format "AES-CTR encrypt (~a)" i) 
		cipher (cipher-encrypt enc-c plain))
    (test-equal (format "AES-CTR decrypt (~a)" i) 
		plain (cipher-encrypt dec-c cipher))))

(define test-rfc3686-vector
  '(;; key iv nonce plain cipher
    #(#vu8(#xAE #x68 #x52 #xF8 #x12 #x10 #x67 #xCC #x4B #xF7 #xA5 #x76 #x55 #x77 #xF3 #x9E)
      #vu8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
      #vu8(#x00 #x00 #x00 #x30)
      #vu8(#x53 #x69 #x6E #x67 #x6C #x65 #x20 #x62 #x6C #x6F #x63 #x6B #x20 #x6D #x73 #x67)
      #vu8(#xE4 #x09 #x5D #x4F #xB7 #xA7 #xB3 #x79 #x2D #x61 #x75 #xA3 #x26 #x13 #x11 #xB8))
    #(#vu8(#x7E #x24 #x06 #x78 #x17 #xFA #xE0 #xD7 #x43 #xD6 #xCE #x1F #x32 #x53 #x91 #x63)
      #vu8(#xC0 #x54 #x3B #x59 #xDA #x48 #xD9 #x0B)
      #vu8(#x00 #x6C #xB6 #xDB)
      #vu8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F
           #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F)
      #vu8(#x51 #x04 #xA1 #x06 #x16 #x8A #x72 #xD9 #x79 #x0D #x41 #xEE #x8E #xDA #xD3 #x88
           #xEB #x2E #x1E #xFC #x46 #xDA #x57 #xC8 #xFC #xE6 #x30 #xDF #x91 #x41 #xBE #x28))
    #(#vu8(#x76 #x91 #xBE #x03 #x5E #x50 #x20 #xA8 #xAC #x6E #x61 #x85 #x29 #xF9 #xA0 #xDC)
      #vu8(#x27 #x77 #x7F #x3F #x4A #x17 #x86 #xF0)
      #vu8(#x00 #xE0 #x01 #x7B)
      #vu8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F
           #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F
	   #x20 #x21 #x22 #x23)
      #vu8(#xC1 #xCF #x48 #xA8 #x9F #x2F #xFD #xD9 #xCF #x46 #x52 #xE9 #xEF #xDB #x72 #xD7
           #x45 #x40 #xA4 #x2B #xDE #x6D #x78 #x36 #xD5 #x9A #x5C #xEA #xAE #xF3 #x10 #x53
	   #x25 #xB2 #x07 #x2F))
    #(#vu8(#x16 #xAF #x5B #x14 #x5F #xC9 #xF5 #x79 #xC1 #x75 #xF9 #x3E #x3B #xFB #x0E #xED
           #x86 #x3D #x06 #xCC #xFD #xB7 #x85 #x15)
      #vu8(#x36 #x73 #x3C #x14 #x7D #x6D #x93 #xCB)
      #vu8(#x00 #x00 #x00 #x48)
      #vu8(#x53 #x69 #x6E #x67 #x6C #x65 #x20 #x62 #x6C #x6F #x63 #x6B #x20 #x6D #x73 #x67)
      #vu8(#x4B #x55 #x38 #x4F #xE2 #x59 #xC9 #xC8 #x4E #x79 #x35 #xA0 #x03 #xCB #xE9 #x28))
    #(#vu8(#x7C #x5C #xB2 #x40 #x1B #x3D #xC3 #x3C #x19 #xE7 #x34 #x08 #x19 #xE0 #xF6 #x9C
           #x67 #x8C #x3D #xB8 #xE6 #xF6 #xA9 #x1A)
      #vu8(#x02 #x0C #x6E #xAD #xC2 #xCB #x50 #x0D)
      #vu8(#x00 #x96 #xB0 #x3B)
      #vu8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F
           #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F)
      #vu8(#x45 #x32 #x43 #xFC #x60 #x9B #x23 #x32 #x7E #xDF #xAA #xFA #x71 #x31 #xCD #x9F
	   #x84 #x90 #x70 #x1C #x5A #xD4 #xA7 #x9C #xFC #x1F #xE0 #xFF #x42 #xF4 #xFB #x00))
    #(#vu8(#x02 #xBF #x39 #x1E #xE8 #xEC #xB1 #x59 #xB9 #x59 #x61 #x7B #x09 #x65 #x27 #x9B
	   #xF5 #x9B #x60 #xA7 #x86 #xD3 #xE0 #xFE)
      #vu8(#x5C #xBD #x60 #x27 #x8D #xCC #x09 #x12)
      #vu8(#x00 #x07 #xBD #xFD)
      #vu8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F
	   #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F
	   #x20 #x21 #x22 #x23)
      #vu8(#x96 #x89 #x3F #xC5 #x5E #x5C #x72 #x2F #x54 #x0B #x7D #xD1 #xDD #xF7 #xE7 #x58
	   #xD2 #x88 #xBC #x95 #xC6 #x91 #x65 #x88 #x45 #x36 #xC8 #x11 #x66 #x2F #x21 #x88
	   #xAB #xEE #x09 #x35))
    #(#vu8(#x77 #x6B #xEF #xF2 #x85 #x1D #xB0 #x6F #x4C #x8A #x05 #x42 #xC8 #x69 #x6F #x6C
	   #x6A #x81 #xAF #x1E #xEC #x96 #xB4 #xD3 #x7F #xC1 #xD6 #x89 #xE6 #xC1 #xC1 #x04)
      #vu8(#xDB #x56 #x72 #xC9 #x7A #xA8 #xF0 #xB2)
      #vu8(#x00 #x00 #x00 #x60)
      #vu8(#x53 #x69 #x6E #x67 #x6C #x65 #x20 #x62 #x6C #x6F #x63 #x6B #x20 #x6D #x73 #x67)
      #vu8(#x14 #x5A #xD0 #x1D #xBF #x82 #x4E #xC7 #x56 #x08 #x63 #xDC #x71 #xE3 #xE0 #xC0))
    #(#vu8(#xF6 #xD6 #x6D #x6B #xD5 #x2D #x59 #xBB #x07 #x96 #x36 #x58 #x79 #xEF #xF8 #x86
	   #xC6 #x6D #xD5 #x1A #x5B #x6A #x99 #x74 #x4B #x50 #x59 #x0C #x87 #xA2 #x38 #x84)
      #vu8(#xC1 #x58 #x5E #xF1 #x5A #x43 #xD8 #x75)
      #vu8(#x00 #xFA #xAC #x24)
      #vu8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F
           #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F)
      #vu8(#xF0 #x5E #x23 #x1B #x38 #x94 #x61 #x2C #x49 #xEE #x00 #x0B #x80 #x4E #xB2 #xA9
	   #xB8 #x30 #x6B #x50 #x8F #x83 #x9D #x6A #x55 #x30 #x83 #x1D #x93 #x44 #xAF #x1C))
  #(#vu8(#xFF #x7A #x61 #x7C #xE6 #x91 #x48 #xE4 #xF1 #x72 #x6E #x2F #x43 #x58 #x1D #xE2
	 #xAA #x62 #xD9 #xF8 #x05 #x53 #x2E #xDF #xF1 #xEE #xD6 #x87 #xFB #x54 #x15 #x3D)
    #vu8(#x51 #xA5 #x1D #x70 #xA1 #xC1 #x11 #x48)
    #vu8(#x00 #x1C #xC5 #xB7)
    #vu8(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F
         #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F
	 #x20 #x21 #x22 #x23)
    #vu8(#xEB #x6C #x52 #x82 #x1D #x0B #xBB #xF7 #xCE #x75 #x94 #x46 #x2A #xCA #x4F #xAA
         #xB4 #x07 #xDF #x86 #x65 #x69 #xFD #x07 #xF4 #x8C #xC0 #xB5 #x83 #xD6 #x07 #x1F
	 #x1E #xC0 #xE6 #xB8))
  ))

(let loop ((i 1) (v test-rfc3686-vector))
  (unless (null? v)
    (test-rfc3686 i (car v))
    (loop (+ i 1) (cdr v))))


(test-end)
