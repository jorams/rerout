# Rerout

Rerout is a simple HTTP routing library. It supports simple segment capture,
wildcard capture at the end of the URL, and canonicalization of the use of a
forward slash at the end of a URL. It doesn't care about the way you handle its
results, instead just returning a user-specified value for the matchd route.

## Example

```lisp
(define-router site ()
  ((:get :post) "/" index url/index)
  (:get "/hello/:name" get-form)
  (:post "/hello/:name" post-form)
  (:get "/some-directory/" some-directory))

(route 'site :get "/")
;; => INDEX
;; => NIL
;; => NIL

(route 'site :post "/hello/foo")
;; => POST-FORM
;; => (:name "foo")
;; => NIL

(route 'site :get "/some-directory")
;; => SOME-DIRECTORY
;; => NIL
;; => "/some-directory/"
```

## Dependencies

- [CL-PPCRE](http://weitz.de/cl-ppcre/) (BSD)
- [QURI](https://github.com/fukamachi/quri) (BSD)

The tests also use
[Parachute](https://shinmera.github.io/parachute/) (Artistic 2.0)

## Installation

Rerout is not yet in Quicklisp, so you will have to download it to somewhere
ASDF can find it. After that, you can load it using Quicklisp:

```lisp
(ql:quickload :rerout)
```

## Usage

The API consists of two parts: The macro `DEFINE-ROUTER` and the function
`ROUTE`.

### DEFINE-ROUTER (macro)

```lisp
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
    ((:get :post) \"/\" index url/index))

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
" ...)
```

### ROUTE (function)

```lisp
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
  ...)
```

## License

MIT. See `LICENSE`.
