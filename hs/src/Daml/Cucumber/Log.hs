module Daml.Cucumber.Log where

import Control.Monad.Log
import Control.Monad.Log.Colors (colorize)
import Control.Monad.Reader
import Data.String (IsString)
import Data.Text (Text)
import Prettyprinter

type Log m a = LoggingT (WithSeverity Text) m a

askLogHandler :: Monad m => Log m (Handler m (WithSeverity Text))
askLogHandler = LoggingT ask

-- | Note that we use the `Notice` constructor to represent output that
-- we want to pass through to stdout without the severity prefix or re-coloring
renderLogMessage :: (Monoid a, Pretty a, IsString a) => WithSeverity a -> Doc ann
renderLogMessage msg = case msgSeverity msg of
  Notice -> pretty $ discardSeverity msg
  _ -> renderWithSeverity' pretty $ colorize msg
  where
    renderWithSeverity' :: (a -> Doc ann) -> (WithSeverity a -> Doc ann)
    renderWithSeverity' k (WithSeverity u a) =
      brackets (pretty $ sev u) <+> align (k a)
    sev :: Severity -> String
    sev = \case
      Informational -> "Info"
      a -> show a
