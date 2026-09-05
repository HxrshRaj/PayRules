{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | An HTTP front end for the engine: a demo UI at @GET /@, plus
-- @GET /healthz@ and @POST /authorize@. Thin on purpose — the UI is a single
-- static page (embedded into the binary at compile time, so the release
-- binaries and the Docker image need nothing extra on disk); the API
-- decodes JSON, calls 'PayRules.Wire.authorize', and encodes the result. The
-- engine itself is untouched by any of this.
module Main (main) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy     as LBS
import           Data.FileEmbed           (embedFile)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Time                (getCurrentTime)
import           Network.HTTP.Media       ((//), (/:))
import           Network.Wai.Handler.Warp (run)
import           Servant
import           System.Environment       (lookupEnv)
import           Text.Read                (readMaybe)

import           PayRules.Wire            (AuthRequest, AuthResponse, authorize)

-- ---------------------------------------------------------------------------
-- A minimal HTML content type, so the demo page can be served through the
-- same 'Get' combinator as everything else.
-- ---------------------------------------------------------------------------

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML T.Text where
  mimeRender _ = LBS.fromStrict . TE.encodeUtf8

-- | The demo page, baked into the binary at compile time (path is relative to
-- the package root, where stack\/cabal always run the build from). Embedded
-- as raw bytes and decoded as UTF-8 explicitly -- 'Data.FileEmbed's
-- 'Data.FileEmbed.embedStringFile' instead reads through GHC's default
-- 'String' text decoding, which on Windows follows the system codepage and
-- silently mangles non-ASCII characters (an em dash came out as \"ΓÇö\").
indexHtml :: T.Text
indexHtml = TE.decodeUtf8 $(embedFile "server/static/index.html")

-- ---------------------------------------------------------------------------
-- API
-- ---------------------------------------------------------------------------

type API =
       Get '[HTML] T.Text                                  -- GET /  : the demo UI
  :<|> "healthz"   :> Get  '[JSON] Value
  :<|> "authorize" :> ReqBody '[JSON] AuthRequest :> Post '[JSON] AuthResponse

api :: Proxy API
api = Proxy

server :: Server API
server = index :<|> healthz :<|> authorizeHandler
  where
    index :: Handler T.Text
    index = pure indexHtml

    healthz :: Handler Value
    healthz = pure (object ["status" .= ("ok" :: String)])

    authorizeHandler :: AuthRequest -> Handler AuthResponse
    authorizeHandler req = do
      now <- liftIO getCurrentTime
      case authorize now req of
        Right resp -> pure resp
        Left err   -> throwError err400
          { errBody    = encode (object ["error" .= err])
          , errHeaders = [("Content-Type", "application/json")]
          }

main :: IO ()
main = do
  port <- maybe 8080 id . (>>= readMaybe) <$> lookupEnv "PORT"
  putStrLn ("PayRules API + demo UI listening on :" <> show port)
  run port (serve api server)
