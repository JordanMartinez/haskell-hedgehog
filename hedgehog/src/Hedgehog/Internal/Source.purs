module Hedgehog.Internal.Source (
    LineNo(..)
  , ColumnNo(..)
  , Span(..)
  , getCaller

  -- * Re-exports from "GHC.Stack"
  , CallStack
  , HasCallStack
  , callStack
  , withFrozenCallStack
  ) where

-- TODO: determine how to convert this API into PureScript API
import GHC.Stack (CallStack, HasCallStack, SrcLoc(..))
import GHC.Stack (callStack, getCallStack, withFrozenCallStack)

newtype LineNo = LineNo Int
derive newtype instance eqLineNo :: Eq LineNo
derive newtype instance ordLineNo :: Ord LineNo
derive newtype instance enumLineNo :: Enum LineNo
-- TODO: Num, Real, and Integral instances

newtype ColumnNo = ColumnNo Int
derive newtype instance eqColumnNo :: Eq ColumnNo
derive newtype instance ordColumnNo :: Ord ColumnNo
derive newtype instance enumColumnNo :: Enum ColumnNo
-- TODO: Num, Real, and Integral instances

type Span =
  { file :: FilePath
  , startLine :: LineNo
  , startColumn :: ColumnNo
  , endLine :: LineNo
  , endColumn :: ColumnNo
  }

getCaller :: CallStack -> Maybe Span
getCaller stack =
  case getCallStack stack of
    Nil ->
      Nothing
    Cons _ (Cons x _) ->
      Just
        { file: srcLocFile x
        , startLine: fromIntegral $ srcLocStartLine x
        , startColumn: fromIntegral $ srcLocStartCol x
        , endLine: fromIntegral $ srcLocEndLine x
        , endColumn: fromIntegral $ srcLocEndCol x
        }

------------------------------------------------------------------------
-- Show instances

-- Jordan's note: Show instance isn't needed as Record has Show instance already
-- instance Show Span where
--   showsPrec p (Span file sl sc el ec) =
--     showParen (p > 10) $
--       showString "Span " .
--       showsPrec 11 file .
--       showChar ' ' .
--       showsPrec 11 sl .
--       showChar ' ' .
--       showsPrec 11 sc .
--       showChar ' ' .
--       showsPrec 11 el .
--       showChar ' ' .
--       showsPrec 11 ec

instance showLineNo :: Show LineNo where
  show (LineNo x) = "LineNo(" <> show x <> ")"

instance showColumnNo :: Show ColumnNo where
  show (ColumnNo x) = "ColumnNo(" <> show x <> ")"
