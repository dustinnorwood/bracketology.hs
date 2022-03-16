{-# LANGUAGE FlexibleContexts, OverloadedStrings, PackageImports, StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}
module Backend.Errors
  ( ErrorBody(..)
  , AppErrorsT
  , (??)
  , failedValidation
  , forbidden
  , internalServerError
  , internalServerErrorShow
  , notAuthorized
  , notFound
  , runAppErrorsT
  ) where

import Control.Exception
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class
import Data.Aeson           (ToJSON, encode)
import Data.Text            (Text, pack)
import "servant-snap" Servant (ServantErr (..), err401, err403, err404, err500, errBody)
import Snap.Core            (MonadSnap)

import Common.Api.Errors

newtype AppException = AppException ServantErr
  deriving (Show, Exception)

throwAppException :: MonadIO m => ServantErr -> m a
throwAppException = liftIO . throwIO . AppException

type AppErrorsT m = ExceptT ServantErr m

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

(??) :: Applicative m => Maybe a -> e -> ExceptT e m a
(??) a e = ExceptT (pure $ note e a)

runAppErrorsT :: MonadSnap m => AppErrorsT m a -> m a
runAppErrorsT = (either throwAppException pure  =<<) . runExceptT

notAuthorized :: ServantErr
notAuthorized =
  err401 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = "Not Authorized"
        , errors = Nothing
        }

forbidden :: ServantErr
forbidden =
  err403 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = "Forbidden"
        , errors = Nothing
        }

notFound :: Text -> ServantErr
notFound resourceName =
  err404 {errBody = encode body}
    where
      body :: ErrorBody ()
      body = ErrorBody
        { message = resourceName <> " not found"
        , errors = Nothing
        }

failedValidation :: ToJSON failures => failures -> ServantErr
failedValidation failures =
  ServantErr
    { errHTTPCode = 422
    , errReasonPhrase = "Unprocessable Entity"
    , errBody = encode (body failures)
    , errHeaders = []
    }
    where
      body :: failures -> ErrorBody failures
      body fs = ErrorBody
        { message = "Failed validation"
        , errors = Just fs
        }

internalServerError :: Text -> ServantErr
internalServerError msg =
  err500 {errBody = encode body}
    where
      body :: ErrorBody [Text]
      body = ErrorBody
        { message = "Internal server error"
        , errors = Just [msg]
        }

internalServerErrorShow :: Show e => Text -> e -> ServantErr
internalServerErrorShow msg e =
  err500 {errBody = encode body}
    where
      body :: ErrorBody [Text]
      body = ErrorBody
        { message = "Internal server error"
        , errors = Just [msg, pack . show $ e]
        }