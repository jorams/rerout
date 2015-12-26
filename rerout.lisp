;;;; This file includes the source code for Rerout, a simple HTTP routing
;;;; library. The interface of Rerout consists of a macro, DEFINE-ROUTER, and a
;;;; function, ROUTE. Most of the code in this file exists to support
;;;; DEFINE-ROUTER.

(defpackage :rerout
  (:use :cl :cl-ppcre :alexandria)
  (:export #:define-router
           #:route))
(in-package :rerout)

;;; Route URLs ----------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((colon-regex (create-scanner
                      (concatenate 'string
                                   ":([^/]+?)"            ; name
                                   "(?:(?:\\{(.+?)\\}))?" ; optional regex
                                   "(?:(?=/)|$)"          ; end of match
                                   )))
        (splat-regex (create-scanner
                      (concatenate 'string
                                   "\\*(.*?)"         ; name
                                   "(?:(\\{.+?\\}))?" ; optional regex
                                   "$"                ; end of match
                                   ))))
    (defun parse-parameter (parameter)
      "Parse a matched variable in the regex pattern, returning a list of three
elements.

The first element will be a two-element list containing the parameter's name as
a keyword and as a symbol in the current package.

The second element will be a regular expression fragment to match on the
segment.

The third element will be a boolean value indicating if the parameter is a
wildcard parameter."
      (register-groups-bind (name regex)
          ((case (char parameter 0)
             (#\: colon-regex)
             (#\* splat-regex))
           parameter)
        (let ((keyword (intern (string-upcase name) :keyword))
              (symbol (intern (string-upcase name))))
          (list (list keyword symbol)
                (format nil
                        (ecase (char parameter 0)
                          (#\: "(~A)(?=/|$)")
                          (#\* "(~A)$"))
                        (or regex ".*?"))
                (char= #\* (char parameter 0)))))))


  (defun parse-route-url (url)
    (let ((parts (split "([:\\*][^/]+?(?:(?=/)|$))" url :with-registers-p t)))
      ;; Splitting the URL into different parts results in a list of alternating
      ;; plain URL segments and parameters. The plain parts need to be escaped
      ;; and collected into a regular expression, interleaved with appropriate
      ;; regular expressions for the parameters.
      ;;
      ;; At the same time we also want a FORMAT pattern we can fill for reverse
      ;; routing. We do this by collecting the plain parts and interleaving them
      ;; with ~A format directives.
      (multiple-value-bind (regex-parts template-parts parameters wildcardp)
          ;; WILDCARDP is used for error checking in the following loop, but it
          ;; is also used later, when determining if a request needs a trailing
          ;; slash or not. URL templates ending in a wildcard parameter don't
          ;; have a canonical answer to whether or not a request URL needs a
          ;; trailing slash.
          (loop with parameters = ()
                with wildcardp = nil
                for parameterp = nil then (not parameterp)
                for part in parts
                for (keyword+symbol regex wildcard-found)
                  = (when parameterp
                      (parse-parameter part))
                when wildcardp
                  do (error "Wildcard not at end of URL!")
                when wildcard-found
                  do (setf wildcardp t)
                unless parameterp
                  collect (quote-meta-chars part) into regex-parts
                  and collect part into template-parts
                else
                  collect regex into regex-parts
                  and collect "~A" into template-parts
                  and do (pushnew keyword+symbol
                                  parameters
                                  :test #'equal)
                finally (return (values regex-parts
                                        template-parts
                                        (reverse parameters)
                                        wildcardp)))
        (values (format nil "^~{~A~}~A"
                        regex-parts
                        (cond
                          ;; Wildcards don't care about trailing slashes.
                          (wildcardp "$")
                          ;; If the template ends in a /, it should be made
                          ;; optional
                          ((ends-with #\/ (lastcar regex-parts))
                           "?$")
                          ;; If the template doesn't end in a / we add an
                          ;; optional one to it.
                          (t "/?$")))
                (format nil "~{~A~}" template-parts)
                parameters
                wildcardp)))))


;;; Route specifications ------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-route-test (method-sym url-sym
                          method regex wildcardp
                          result template parameters)
    "Generate a piece of code that determines whether a method and URL match a
route, and if so, returns (to the block NIL) three values: The result object for
the route, a property list for the route's parameters, and an optional canonical
version of the request URL to which the user should be redirected."
    `(when ,(if (consp method)
                `(member ,method-sym ',method)
                `(eq ,method-sym ,method))
       (register-groups-bind ,(mapcar #'second parameters)
           (,regex ,url-sym)
         (return
           (values ',result
                   (list ,@(apply #'append parameters))
                   ;; If the template doesn't end in a wildcard and the request
                   ;; URL differs from the template in whether it has a trailing
                   ;; slash, we generate a URL for the user to be redirected to.
                   ,(unless wildcardp
                      (if (ends-with #\/ template)
                          `(unless (ends-with #\/ ,url-sym)
                             (format nil "~A/" ,url-sym))
                          `(when (ends-with #\/ ,url-sym)
                             (subseq ,url-sym 0 (1- (length ,url-sym)))))))))))

  (defun make-url-generator (template parameters)
    `(format nil ,template ,@(mapcar (lambda (parameter)
                                       `(quri:url-encode
                                         (princ-to-string
                                          ,parameter)))
                                     parameters)))

  (defun parse-route (method-sym url-sym prefix route)
    (destructuring-bind (method url result &optional generator-name)
        route
      (multiple-value-bind (regex template parameters wildcardp)
          (parse-route-url (concatenate 'string prefix url))
        (list (make-route-test method-sym url-sym
                               method regex wildcardp
                               result template parameters)
              (when generator-name
                `(defun ,generator-name
                     ,(when parameters
                        (cons '&key (mapcar #'second parameters)))
                   ,(make-url-generator template
                                        (mapcar #'second parameters))))))))

  (defun parse-include-route (method-sym url-sym route)
    (list
     `(let ((values (multiple-value-list
                     (funcall (get ',(second route) 'router)
                              ,method-sym
                              ,url-sym))))
        (when (first values) (return (values-list values)))))
    ;; Note that, unlike PARSE-ROUTE, this function doesn't ever return a URL
    ;; generator.
    ))


;;; The router ----------------------------------------------------------------

(defmacro define-router (name (&key (prefix "")) &body routes)
  "Define a router for the symbol NAME.

PREFIX should be a string that is automatically prepended to every route's
template.

ROUTES is a list containing route definitions. They normally look like this:

  (<method(s)> <template> <result> [generator-name])

METHOD is a keyword like :GET or :POST, or a list of such keywords. The route
will only respond to an HTTP method supplied here.

TEMPLATE is a string representing the template for URLs for the route to respond
to. The template can contain parameters, in the form of `:param-name`, that will
match in the request URL until the next '/'. It can also end with a parameter in
the form of `*param-name`. Parameters of this sort will match until the end of
the request URL. If the URL matches, the values for these parameters are
collected and returned by ROUTE as a property list.

If desired, it's also possible to specify the regular expression used by a
parameter by placing it within {} after the parameter name. This should be done
with caution.

RESULT is an (unevaluated) value returned by ROUTE when the route matches. A
good use for this would be a symbol representing a function name.

GENERATOR-NAME, if supplied, should be a symbol. A function will be defined with
that name that can be called with keyword arguments for every parameter in the
URL. It will place the supplied values at the right place in the template (after
URL-encoding them) and return the resulting URL.


A route definition can also be of the form

  (:include <symbol>)

In that case the router will call the router for SYMBOL before moving on to the
next route.


Example:

  (define-router api (:prefix \"/api\")
    (:get \"/:namespace/search/*terms\" api-example-url url/api-example-url))

  (define-router site ()
    (:include api)
    ((:get :post) \"/\" index url/index)
    ;; Note the regular expression between {}
    (:get \"/:example{[^/]+}\" example))

  (url/api-example-url :namespace \"example\" :terms \"example search terms\")
  ;; => \"/api/example/search/example%20search%20terms\"

  (route 'site :get \"/api/example-2/search/multiple+terms/with/slashes\")
  ;; => API-EXAMPLE-URL
  ;; => (:NAMESPACE \"example-2\" :TERMS \"multiple+terms/with/slashes\")
  ;; => NIL

  (route 'site :post \"/\")
  ;; => INDEX
  ;; => NIL
  ;; => NIL
"
  (let ((method-sym (gensym "METHOD"))
        (url-sym (gensym "URL"))
        generator-items
        test-items)
    (loop for route in routes
          for (tester generator) = (if (eq (first route) :include)
                                       (parse-include-route method-sym
                                                            url-sym
                                                            route)
                                       (parse-route method-sym
                                                    url-sym
                                                    prefix
                                                    route))
          when generator
            collect generator into generators
          collect tester into testers
          finally (setf generator-items generators
                        test-items testers))
    `(progn ,@generator-items
            (setf (get ',name 'router)
                  (lambda (,method-sym ,url-sym)
                    (block nil
                      ,@test-items)))
            ',name)))

(defun route (router method url)
  "Pass METHOD and URL through the router named by ROUTER.

ROUTER should be a symbol on which a router has previously been defined using
DEFINE-ROUTER. Note that here it has to be quoted.

METHOD should be a keyword like :GET or :POST, representing the HTTP method used
in a request.

URL should be a string representing the path of an HTTP request.

ROUTE returns three values:

- The first value is an object specified in the call to DEFINE-ROUTER as the
  result of a route. If no matching route was found, this is NIL.

- The second value is a property list linking the route's parameters (as
  keywords) to their values in the request URL.

- The third value is either NIL or a string representing the canonical path of
  the route. The \"canonical\" path, if returned, will differ from the supplied
  request path in whether or not it has a trailing slash. It is recommended that
  you redirect the user there.

Examples:

  (route 'router-symbol :get \"/\")
  (route 'router-symbol :post \"/some/url\")
"
  (funcall (get router 'router) method url))
