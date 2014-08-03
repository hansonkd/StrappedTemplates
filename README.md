Strapped Templates
=================

General Purpose Templates in Haskell.

Objective
=========

The objective of this project is to build an easy-to-use, flexible, general purpose, performant templating language. Strapped is general purpose and is not necessarily geared towards HTML. For example, this README and cabal file is written in Strapped (see `benchmarks/strapped_templates/README.strp` and `examples/make_readme.hs`)

Quick Start
===========

Strapped templates need two things to render. A `TemplateStore` and an `InputBucket m`. 

`TemplateStore` is just function that take a String and returns a template.

`InputBucket m` is a list of maps that get iterated through to find variables.

The easiest way to create a template store is to use one of the built in functions. The `templateStoreFromDirectory` loads and parses the templates upfront. The TemplateStore datatype is flexible enough (executes in `IO`) that you could add a dynamically loading TemplateStore if you wanted.

```haskell
import Control.Monad.IO.Class
import qualified Blaze.ByteString.Builder as B
import qualified Data.Text.Lazy as T
import Data.Time

import Text.Strapped

makeBucket :: Integer -> InputBucket IO
makeBucket i = bucketFromList 
      [ ("is", List $ map (LitVal . LitInteger) [1..i])
      , ("ioTime", Func (\_ -> (liftIO $ getCurrentTime) >>= (\c -> return $ LitText $ T.pack $ show c)))
      ]

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory "benchmarks/strapped_templates" ".strp"
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render (putStore store defaultConfig) (makeBucket 2) "base_simple.strp"
      either (print) (print . B.toByteString) rendered
```

Template Lanaguage Features
===========================

### Functions
Strapped lets you build and pass functions in your monad context.

A function takes a list of `Literal` and produces a Literal in an `ErrorT m` context.


```haskell
instance Renderable UTCTime where
  renderOutput _ c = showToBuilder c

instance Renderable NominalDiffTime where
  renderOutput _ c = showToBuilder c

bucket = bucketFromList [
          ("ioTime", Func (\_ -> (liftIO $ getCurrentTime) >>= (\c -> return $ LitDyn $ c) )),
          ("diffTime", Func diffTime),
        ]

diffTime (LitList ((LitDyn a):(LitDyn b):_)) = do
  case do {t1 <- cast a; t2 <- cast b; return (t1, t2)} of
    Just (time1, time2) -> return $ LitDyn (diffUTCTime time1 time2)
    Nothing -> return $ LitText $ T.pack "Only UTCTimes Please..."
```


```html
<h1>${ ioTime }<h1>
Diff ${ diffTime [ioTime, ioTime]}
```


Retults in:

```html
<h1>2014-02-24 02:02:35.191664 UTC</h1>
Diff -0.000004s
```

### For loops

For loops are easy to do:

```haskell 
bucket :: MonadIO m => InputBucket m
bucket = bucketFromList [
          ("is", List $ map (LitVal . LitInteger) [1..5]),
        ]
```


```html
<ul>
  {$ for i in is $}
  <li> ${ i } </li>
  {$ endfor $}
</ul>
```



### Control Parsing

You can use the `{$ raw $}` tag to prevent the parser from parsing a part of the file.


```html
{$ raw $} ${ thisWontBeEvaluated } 
```
{$ endraw $}

Or you can use the `{$ comment $}` tag to skip over that part of the file altogether.


```html
{$ comment $} This wont show. ${ thisWontShowAtAll } neither will this. {$ endcomment $}
```




### In template declarations

At the start of every template, block, or forloop, you can define template variables with a let tag.


```html
{$ let time = ioTime $}

${ time }

<ul>
  {$ for i in is $}
  {$ let val = someFunc i $}
  <li> ${ val } </li>
  {$ endfor $}
</ul>
```



### Includes

You can easily include other templates.

includes.strp
```html 
Other Template
```


```html
This is a template that calls another.
The other template: {$ include includes.strp $}
```


Result:

```html
This is a template that calls another.
The other template: Other Template
```


### Inheritance

Any block, forloop, or template can inherit from another template. This allows you to specify a base template with `block` tags that will get filled by content defined in `isblock`. If an `inherits` tag is encountered, only `isblock` tags will be allowed at that level. 


base.strp
```html
This is a base template
${ time }
{$ block title $} Default Title {$ endblock $}
{$ block body $} Default Body {$ endblock $}
```



```html
{$ let time = ioTime $}
{$ inherits base.strp $}

{$ isblock title $}Block title!!{$ endisblock $}
{$ isblock title $}Block Body!!{$ endisblock $}
```


Here is an example of using inheritence at different levels:


```html
{$ let time = ioTime $}

{$ inherits base.strp $}

{$ isblock title $}Outside of loop{$ endisblock $}

{$ isblock body $}
  {$ for i in is $}
    {$ inherits base.strp $}
    {$ isblock title $}Inside loop{$ endisblock $}
    {$ isblock body $}
      Called from for loop ${ i }
    {$ endisblock $}
  {$ endfor $}
{$ endisblock $}
```


Speed
=====

Strapped preloads and tokenizes your templates before render time. This results in overall good performance. It is about as fast as Blaze-Html in normal linear templates. Agravating the garbage collection and doing large loops inside loops slows it down about 2x slower than Blaze-Html, which is still pretty fast. It is significantly (orders of magnitude) faster than interpreted templates like Django, Interpreted-Heist and Hastache. The part that seems to slow it down the most is variable lookup.

I haven't spent spent much time optimizing so there is still room for improvement. Feel free to run the benchmarks or optimize them more.