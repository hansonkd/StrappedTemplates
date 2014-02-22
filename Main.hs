import Control.Monad

import Strapped.Parser

templates :: [String]
templates = 
  [ "Blah blah blah"
  , "${ some_var } string ${ other_var } s"
  , "{$ block body $}{$ endblock $}"
  , "{$ block body $}${ hello }{$ endblock $}"
  , "{$ block body $}{$ {$ endblock $}"
  , "{$ block body $}{$ endblock $} "
  , "{$ block body $} {} { ${ }{ ${{} cool } {$ block inner_body $} Hello: ${ hello }{$ endblock $} body string {$ endblock $}"
  , "{$ block body $} {} ${ cool } {$ block inner_body $} Hello: ${ hello }{$ endblock $}{$ endblock $} ldsakjf"
  , "{$ block body $} {$ for b in bs $} ${ b } {$ endfor $}{$ endblock $} "
  , "{$ for i in is $} ${ i } {$ endfor $}"
  , "{}{}  { { }}"
  ]

main :: IO ()
main = do
  forM_ (zip [1..] $ map parseTemplate templates) (\(i, r) -> print i >> print r)