# heist-stlc

Heist templates as a surface syntax for simply-typed lambda calculus


Some terms:

```html
<lc:lit type="Int">5</lc>
```

```html
<lc:succ type="Int -> Int"/>
```

```html
<lc:showtime type="UTCTime -> Html"/>
```

```html
<lc:fun type="{myname :: Html, mydate :: UTCTime} -> Html">
  Hello <lc:myname type="Html"/>.
  It&lapos;s <lc:apply template="showtime"><mydate/></lc>.
</lc>
```

```html
<lc type="Html"/>
<apply template="succ">
  <apply template="five"/>
</apply>
```
