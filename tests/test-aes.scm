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

(test-end)
