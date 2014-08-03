module Text.Strapped
  ( -- * Rendering
    render
  , defaultConfig
  , showToBuilder
    -- ** Controlling variables 
  , combineBuckets
  , varBucket
  , bucketLookup
  , bucketFromList
  , emptyBucket
    -- * Parsing
  , parseTemplate
    -- * TemplateLoading
  , templateStoreFromList
  , templateStoreFromDirectory
  , putStore
  , module Text.Strapped.Types
  ) where
  
import Text.Strapped.Types
import Text.Strapped.Render
import Text.Strapped.Parser
import Text.Strapped.Utils