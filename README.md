Strapped Templates
=================

General Purpose Templates in Haskell.

Objective
=========

The objective of this project is to build an easy-to-use, flexible, general purpose templating language. 

Strapped isn't necessarily geared towards HTML. For example, this README and cabal file is written in Strapped (see `examples/templates/README.strp` and `examples/make_readme.hs`).

As of `0.2` Strapped now accepts custom parsers and template tags as part of the `StrappedConfig` object. 

Quick Start
===========

### Syntax
Strapped renders variables to text with this tag:

`${ _ }`


Strapped includes these tag blocks to control flow:

 `{$ inherits _ $}` `{$ let _ = _ $}` `{$ includes _ $}` `{$ for _ in _ $}{$ endfor $}` `{$ if _ $}{$ endif $}` `{% block _ $}{$ endblock $}` `{$ comment $}{$ endcomment $}` `{$ raw $}{$ endraw $}`

### Expressions
Strapped gives you a limited number of expressions you can define in your template. Improving this is a core focus of future releases.

Currently expressions include Bools, Lists, 0 arity functions, single arity functions, strings, ints, floats. 


```
${ takesAList ["string", 1, 1.0, [True, zeroArityFunc, lookupVar], (someFunc False)] }
```


## Example

Here is an example of the tags being used together:

```html
{$ inherits base.strp $}

{$ isblock body $}

An IO function to find the current time: ${ ioTime }

{$ if is_truthy $}
    {$ inherits base.strp $}
    {$ isblock body $}
        Any block level can inherit from another template and override blocks.
    {$ endisblock $}
{$ else $}
    Don't show me.
{$ endif $}

Taken from an includes:
{$ include includes/includes.strp $}

Lets count...
{$ for i in is $}
    ${ i }
{$ endfor $}

{$ endisblock $}
```

### Rendering

Strapped templates need two things to render. A `TemplateStore` and an `InputBucket m`. 

`TemplateStore` is just function that take a String and returns a template.

`InputBucket m` is a list of maps that get iterated through to find variables.

The easiest way to create a template store is to use one of the built in functions. The `templateStoreFromDirectory` loads and parses the templates upfront. The TemplateStore datatype is flexible enough (executes in `IO`) that you could add a dynamically loading TemplateStore if you wanted.

```haskell
import Control.Monad.IO.Class
import qualified Blaze.ByteString.Builder as B
import qualified Data.Text as T
import Data.Time

import Text.Strapped

makeBucket :: Integer -> InputBucket IO
makeBucket i = bucketFromList 
      [ ("is", lit $ map (LitInteger) [1..i])
      , ("is_truthy", lit i)
      , ("ioTime", Func (\_ -> (liftIO $ getCurrentTime) >>= (\c -> return $ LitText $ T.pack $ show c)))
      ]

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory defaultConfig "examples/templates" ".strp"
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



### Parsing Control

You can use the `{$ raw $}` tag to prevent the parser from parsing a part of the file.


```html
{$ raw $} ${ thisWontBeEvaluated } {$ endraw $}
```


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


Extending the template language with custom parsers and blocks
==============================================================

Strapped lets you define custom parsers to use to parse and render. You have to define both how to parse your data out of a template with Parsec and how to process it when it is rendered.

Theoretically this means you can build an entirely new template syntax within Strapped.

```haskell

import Blaze.ByteString.Builder.Char8
import Data.Monoid ((<>))
import Data.Text as T
import Text.Strapped
import Text.Strapped.Render
import Text.Strapped.Parser
import Text.Parsec
import Text.Parsec.String

data MyData = MyData ParsedExpression
    deriving (Show)

-- | Process MyData in the RenderT monad when it is rendered.
instance Block MyData where
    process (MyData ex) = do
        config <- getConfig
        lit <- reduceExpression ex
        return $ fromString "MyData ->" <> (renderOutput config lit) <> fromString "<- MyData"

-- | Parse out MyData using Parsec
parseMyData :: ParserM MyData
parseMyData = do
    tagStart >> spaces >> (string "MyData") >> spaces >> parseExpression (spaces >> tagEnd) >>= (return . MyData)

main :: IO ()
main = do
    
    case templateStoreFromList config [("mydata", "testing... {$ MyData lookup $}")] of
        Left e -> print e
        Right ts -> do
            r <- render (config {templateStore = ts}) (varBucket "lookup" $ LitVal $ LitText $ T.pack "Cooolio") "mydata"
            print r
    where config = defaultConfig {customParsers = [BlockParser parseMyData]}
```



Speed
=====

Speed has taken a backseat to usability at this point in development.