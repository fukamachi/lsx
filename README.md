# LSX

[![Quicklisp dist](http://quickdocs.org/badge/lsx.svg)](http://quickdocs.org/lsx/)
[![Build Status](https://travis-ci.org/fukamachi/lsx.svg?branch=master)](https://travis-ci.org/fukamachi/lsx)
[![Coverage Status](https://coveralls.io/repos/fukamachi/lsx/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/lsx)

Embeddable HTML templating engine with [JSX](https://reactjs.org/docs/introducing-jsx.html)-like syntax.

## Usage

```common-lisp
(ql:quickload '(:lsx :local-time))

(lsx:enable-lsx-syntax)

<br/>
;=> #<LSX/HTML:ELEMENT br {1003F98BF3}>

(lsx:render-object <br/> t)
;-> <br>
;=> NIL

(lsx:render-object <a href="/hello">Say Hello</a> t)
;-> <a href="/hello">Say Hello</a>
;=> NIL


;;
;; Embed Lisp code in {}

(lsx:render-object <a href="/hello">Say Hello at {(local-time:now)}</a> t)
;-> <a href="/hello">Say Hello at 2018-09-14T05:04:55.009102+09:00</a>
;=> NIL
```

### Defining custom tags

```common-lisp
(lsx:deftag welcome (&key name)
  <h1>{name}</h1>)

<welcome name="fukamachi"></welcome>
;=> #<WELCOME {10028D74D3}>

(lsx:render-object <welcome name="fukamachi" /> t)
;-> <h1>fukamachi</h1>
;=> NIL
```

### Loading from file

```common-lisp
;; example.lsx
(lambda (&key (name "Guest"))
<html>
  <head>
    <title>Welcome {name}</title>
  </head>
  <body>
    <div id="main"><h1>Hello</h1><p><a href="/entries">Show Entries</a></p></div>
  </body>
</html>)
```

```common-lisp
(lsx:read-lsx-file #P"example.lsx")
;=> #<FUNCTION (LAMBDA (&KEY :NAME) :IN "~/Programs/lib/lsx/example.lsx") {1005E72B5B}>

(lsx:render-object (funcall * :name "fukamachi") t)
;-> <!DOCTYPE html>
;   <html>
;     <head>
;       <title>Welcome fukamachi</title>
;     </head>
;     <body>
;       <div id="main"><h1>Hello</h1><p><a href="/entries">Show Entries</a></p></div>
;     </body>
;   </html>
;=> NIL
```

## How it works

LSX syntax is implemented as reader macro. It's able to see how it's expanded with quoting.

```common-lisp
'<br/>
;=> (LSX/TAG:H 'BR (LIST))

'<a href="/hello">Say Hello</a>
;=> (LSX/TAG:H 'A (LIST (CONS "href" "/hello")) (LIST "Say Hello"))

'<a href="/hello">Say Hello at {(local-time:now)}</a>
;=> (LSX/TAG:H 'A (LIST (CONS "href" "/hello")) (LIST "Say Hello at " (LAMBDA () (LOCAL-TIME:NOW))))
```

`h` is a function to make an element. It takes a single required argument, a `tag-name` as a string, and 2 optional arguments, attributes as an association list and children as a list of elements.

```common-lisp
;; Same as <br/>
(lsx:h "br")
;=> #<LSX/HTML:ELEMENT br {10033183D3}>

(lsx:h "a" '(("href" . "/hello")) '("Say Hello"))
;=> #<LSX/HTML:ELEMENT a {100331D823}>

(lsx:h "a" '(("href" . "/hello")) (list "Say Hello at " (lambda () (local-time:now))))
```

## See Also

- [Introducing JSX](https://reactjs.org/docs/introducing-jsx.html)
- [JSX In Depth](https://reactjs.org/docs/jsx-in-depth.html)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2018 Eitaro Fukamachi

## License

Licensed under the BSD 2-Clause License.
