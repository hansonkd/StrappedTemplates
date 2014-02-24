StrappedTemplates
=================

General Purpose Templates in Haskell.

Objective
=========

The objective of this project is to build an easy to use, flexible, general purpose, performant templating language.

Template Lanaguage Features
===========================

### Functions
Strapped lets you build and pass functions in your monad context.

```haskell 
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

For loops are easy to do 

Speed
=====

Strap preloads and tokenizes your templates before render time. This results in overall good performance. It is about 2x slower than Blaze-Html and about 1000x faster than Interprted Heist. Feel free to checkout the 