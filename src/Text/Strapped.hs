module Text.Strapped
  ( -- * Rendering
    render
  , defaultConfig
  , showToBuilder
    -- ** Controlling variables 
  , combineBuckets
  , varBucket
  , bucketLookup
    -- * Parsing
  , parseTemplate
    -- * TemplateLoading
  , templateStoreFromList
  , templateStoreFromDirectory
  , module Text.Strapped.Types
  ) where
  
import Text.Strapped.Types
import Text.Strapped.Render
import Text.Strapped.Parser
import Text.Strapped.Utils