# LSX

Embeddable HTML templating engine with JSX-like syntax.

## Usage

```common-lisp
(ql:quickload '(:lsx :local-time))

(lsx:enable-lsx-syntax)

<br/>
;=> #<LSX/HTML::ELEMENT br {1003F98BF3}>

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


;;
;; Loading from LSX file

(uiop:with-output-file (out #P"example.lsx" :if-exists :supersede)
  (princ "<div id=\"main\"><h1>Hello</h1><p><a href=\"/entries\">Show Entries</a></p></div>" out))

(lsx:read-lsx-file #P"example.lsx")
;=> #<LSX/HTML::ELEMENT div {100485C1A3}>
```
