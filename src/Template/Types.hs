module Template.Types where

import Data.Text (Text)


{-- TODO:
 * Add:
  - tree of blocks (Logic, Textual),
  - origin of content
  - format of output (html, amp, xml)
  - parent pointer
  - children templates
  - ? compiled logic ?
-}

data Template = Template {
    origin :: FilePath
  }
