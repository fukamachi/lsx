# LSX

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
(defclass welcome (lsx:component)
  ((name :initarg :name)))

(defmethod lsx:render-object ((object welcome) stream)
  (lsx:render-object <h1>{(slot-value object 'name)}</h1> stream))

<welcome name="fukamachi"></welcome>
;=> #<WELCOME {10028D74D3}>

(lsx:render-object <welcome name="fukamachi"></welcome> t)
;-> <h1>fukamachi</h1>
;=> NIL
```

### Loading from file

```common-lisp
(uiop:with-output-file (out #P"example.lsx" :if-exists :supersede)
  (princ "<div id=\"main\"><h1>Hello</h1><p><a href=\"/entries\">Show Entries</a></p></div>" out))

(lsx:read-lsx-file #P"example.lsx")
;=> #<LSX/HTML::ELEMENT div {100485C1A3}>
```

## See Also

- [Introducing JSX](https://reactjs.org/docs/introducing-jsx.html)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2018 Eitaro Fukamachi

## License

Licensed under the BSD 2-Clause License.
