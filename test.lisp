(defpackage :rerout-test
  (:use :cl :parachute))
(in-package :rerout-test)

(defun test-route (router method url
                   expected-result
                   expected-parameters
                   expected-canonical)
  (multiple-value-bind (result parameters canonical)
      (rerout:route router method url)
    (if expected-result
        (is equal expected-result result)
        (false result))
    (if expected-parameters
        (is equal expected-parameters parameters)
        (false parameters))
    (if expected-canonical
        (is equal expected-canonical canonical)
        (false canonical))))

(rerout:define-router simple-root ()
  (:get "/" "get")
  (:post "/" "post"))

(define-test "Simple root, GET"
  (test-route 'simple-root :get "/"
              "get"
              nil
              nil)
  (test-route 'simple-root :get "/a"
              nil
              nil
              nil))

(define-test "Simple root, POST"
  (test-route 'simple-root :post "/"
              "post"
              nil
              nil)
  (test-route 'simple-root :post "/a"
              nil
              nil
              nil))

(rerout:define-router simple-parts ()
  (:get "/a/b/c" "a")
  (:get "/b/c" "b")
  (:get "/c" "c"))

(define-test "Simple parts, length 3"
  (test-route 'simple-parts :get "/a/b/c"
              "a"
              nil
              nil)
  (test-route 'simple-parts :get "/a"
              nil
              nil
              nil)
  (test-route 'simple-parts :get "/a/b"
              nil
              nil
              nil))

(define-test "Simple parts, length 2"
  (test-route 'simple-parts :get "/b/c"
              "b"
              nil
              nil)
  (test-route 'simple-parts :get "/b"
              nil
              nil
              nil))

(define-test "Simple parts, length 1"
  (test-route 'simple-parts :get "/c"
              "c"
              nil
              nil)
  (test-route 'simple-parts :get "/"
              nil
              nil
              nil))

(rerout:define-router simple-colons ()
  (:get "/:a" "/:a")
  (:get "/a/:a" "/a/:a")
  (:get "/:a/a" "/:a/a"))

(define-test "Simple colons, length 1"
  (test-route 'simple-colons :get "/"
              nil
              nil
              nil)
  (test-route 'simple-colons :get "/test"
              "/:a"
              '(:a "test")
              nil))

(define-test "Simple colons, length 2"
  (test-route 'simple-colons :get "/a/test"
              "/a/:a"
              '(:a "test")
              nil)
  (test-route 'simple-colons :get "/test/a"
              "/:a/a"
              '(:a "test")
              nil))

(rerout:define-router simple-splat ()
  (:get "/a/*a" "a")
  (:get "/*b" "b"))

(define-test "Simple splat, length 2"
  (test-route 'simple-splat :get "/a/foo"
              "a"
              '(:a "foo")
              nil)
  (test-route 'simple-splat :get "/a/foo/"
              "a"
              '(:a "foo/")
              nil))

(define-test "Simple splat, length 1"
  (test-route 'simple-splat :get "/b/foo"
              "b"
              '(:b "b/foo")
              nil)
  (test-route 'simple-splat :get "/foo/bar/baz/"
              "b"
              '(:b "foo/bar/baz/")
              nil))

(rerout:define-router canonicalization ()
  (:get "/a/a" "a without")
  (:get "/b/b/" "b with")
  (:get "/c/:c" "c without")
  (:get "/d/:d/" "d with")
  (:get "/e/*e" "e without"))

(define-test "Canonicalization, plain without final /"
  (test-route 'canonicalization :get "/a/a"
              "a without"
              nil
              nil)
  (test-route 'canonicalization :get "/a/a/"
              "a without"
              nil
              "/a/a"))

(define-test "Canonicalization, plain with final /"
  (test-route 'canonicalization :get "/b/b/"
              "b with"
              nil
              nil)
  (test-route 'canonicalization :get "/b/b"
              "b with"
              nil
              "/b/b/"))

(define-test "Canonicalization, colon without final /"
  (test-route 'canonicalization :get "/c/foo"
              "c without"
              '(:c "foo")
              nil)
  (test-route 'canonicalization :get "/c/foo/"
              "c without"
              '(:c "foo")
              "/c/foo"))

(define-test "Canonicalization, colon with final /"
  (test-route 'canonicalization :get "/d/foo/"
              "d with"
              '(:d "foo")
              nil)
  (test-route 'canonicalization :get "/d/foo"
              "d with"
              '(:d "foo")
              "/d/foo/"))

(define-test "Canonicalization, splat"
  (test-route 'canonicalization :get "/e/foo"
              "e without"
              '(:e "foo")
              nil)
  (test-route 'canonicalization :get "/e/foo/"
              "e without"
              '(:e "foo/")
              nil))
