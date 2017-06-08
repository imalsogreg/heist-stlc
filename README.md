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

## How is [heist](https://hackage.haskell.org/package/heist) like a programming language?

We can bind values to variables

```html
...
<bind tag="myvar">
  <p>Hello</p>
</bind>
```

We have abstraction, because a template with `<apply-content/>` is like a function `:: Html -> Html`. We can apply that function to an argument with the `<apply>` splice:

myfun.tpl
```html
<div class="example function">
  <apply-content/>
</div>
```

myapp.tpl
```html
<apply template="myfun">
  <img class="the-argument" src="something.jpg"/>
</apply>
```


## How does heist differ from lambda calculus?

There are two different ways of passing arguments to applied templates: `<apply-content/>` and named variables. This is another way of achieving function application:

myfun.tpl
```html
<div class="example function">
  <freevar1/>
  <freevar2/>
</div>
```

myapp.tpl
```html
<apply template="myfun">
  <bind tag="freevar1">
    <p>Val1</p>
  </bind>
  <bind tag="freevar2">
    <p>Val2</p>
  </bind>
</apply>
```

The syntax of `heist` favors literal html. Embedding lambda terms in heist templates probably requires namespacing a few special html tags for application and abstraction.
