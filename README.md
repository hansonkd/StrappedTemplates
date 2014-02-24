Strapped Templates
=================

General Purpose Templates in Haskell.

Objective
=========

The objective of this project is to build an easy-to-use, flexible, general purpose, performant templating language.

Quick Start
===========

Strapped templates need two things to render. A `TemplateStore` and an `InputBucket m`. 

`TemplateStore` and `InputBucket m` are both just functions that take a String and returns templates or inputs.

The easiest way to create a template store is to use one of the built in functions. The `templateStoreFromDirectory` loads and parses the templates upfront. The TemplateStore datatype is flexible enough (executes in `IO`) that you could add a dynamically loading TemplateStore if you wanted.

```haskell
import Control.Monad.IO.Class
import qualified Blaze.ByteString.Builder as B
import qualified Data.Text.Lazy as T
import Data.Time

import Text.Strapped

makeBucket :: Int -> InputBucket IO
makeBucket i = bucket
  where bucket "render_size" = Just $ LitVal $ LitInt i
        bucket "is" = Just $ List $ map (LitVal . LitInt) [1..i]
        bucket "ioTime" = Just $ Func (\_ -> (liftIO $ getCurrentTime) >>= (\c -> return $ LitText $ T.pack $ show c) )
        bucket _ = Nothing

main :: IO ()
main = do
  tmpls <- templateStoreFromDirectory "benchmarks/strapped_templates" ".strp"
  case tmpls of
    Left err -> print err
    Right store -> do
      rendered <- render (defaultConfig {templateStore = store}) (makeBucket 2) "big.strp"
      either (print) (print . B.toByteString) rendered
```

Template Lanaguage Features
===========================

### Functions
Strapped lets you build and pass functions in your monad context.

A function takes a list of `Input m` and produces a Literal in an `ErrorT m` context.

```haskell 
bucket :: MonadIO m => InputBucket m
bucket "ioTime" = Just $ Func (\_ -> (liftIO $ getCurrentTime) >>= (\c -> return $ packInput $ show c) )
bucket _        = Nothing
```

```html
<h1>@{ ioTime }<h1>
```

Retults in:

```html
<h1>2014-02-24 02:02:35.191664 UTC</h1>
```

### For loops

For loops are easy to do:

```haskell 
bucket :: MonadIO m => InputBucket m
bucket "is" = Just $ List $ map (LitVal . LitInt) [1..5]
bucket _    = Nothing
```

```html
<ul>
  {@ for i in is @}
  <li> @{ i } </li>
  {@ endfor @}
</ul>
```

### In template declarations

At the start of every template, block, or forloop, you can do stuff like this:

```html
{@ let time = ioTime @}

@{ time }

<ul>
  {@ for i in is @}
  {@ let val = someFunc i @}
  <li> @{ val } </li>
  {@ endfor @}
</ul>

```

### Includes

You can easily call other in other templates.

includes.strp
```html 
Other Template
```

```html
This is a template that calls another.
The other template: {@ include includes.strp @}
```

Result:

```html
This is a template that calls another.
The other template: Other Template
```


### Extends

You can call an "extend" which extends the current "block context" and declarations into another template. If you have an includes, it must come at the beginning of your document (after `let` declerations). 
Everything in that template after the first includes statement will be ignored, execpt for `isblock` tags. 

base.strp
```html
This is a base template
@{ time }
{@ block title @} Default Title {@ endblock @}
Footer.
```

```html
{@ let time = ioTime @}
{@ extends base.strp @}
This will be ignored.
{@ isblock title @}Block title!!{@ endisblock @}
```

Speed
=====

Strapped preloads and tokenizes your templates before render time. This results in overall good performance. It is about as fast as Blaze-Html in normal linear templates. Agravating the garbage collection and doing a nested loop slows it down about 2x slower than Blaze-Html, which is still pretty fast. It is significantly (orders of magnitude) faster than interpreted templates like Interpreted-Heist and Hastache. 

I haven't spent spent much time optimizing so there is still room for improvement. Feel free to run the benchmarks or optimize them more.