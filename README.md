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
(lsx:defcomponent welcome ()
  (name)
  (:render <h1>{name}</h1>))

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
  (list
<!doctype html>
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
;-> <!doctype html>
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

## See Also

- [Introducing JSX](https://reactjs.org/docs/introducing-jsx.html)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2018 Eitaro Fukamachi

## License

Licensed under the BSD 2-Clause License.
