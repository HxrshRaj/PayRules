{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | An HTTP front end for the engine: @GET /healthz@ and @POST /authorize@.
-- Thin on purpose — it decodes JSON, calls 'PayRules.Wire.authorize', and
-- encodes the result. The engine is untouched.
module Main (main) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson              (Value, encode, object, (.=))
import           Data.Time               (getCurrentTime)
import           Network.Wai.Handler.Warp (run)
import           Servant                 -- re-exports Proxy, Handler, throwError, err400, ...
import           System.Environment      (lookupEnv)
import           Text.Read               (readMaybe)

import           PayRules.Wire           (AuthRequest, AuthResponse, authorize)

type API =
       "healthz"   :> Get  '[JSON] Value
  :<|> "authorize" :> ReqBody '[JSON] AuthRequest :> Post '[JSON] AuthResponse

api :: Proxy API
api = Proxy

server :: Server API
server = healthz :<|> authorizeHandler
  where
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
  putStrLn ("PayRules API listening on :" <> show port)
  run port (serve api server)
