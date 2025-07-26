{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}


module WebServer.Servant where

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (runContT, ContT (..), Cont)
import Control.Monad.Except (ExceptT, MonadError, withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Int as DI
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.List.NonEmpty (NonEmpty (..))

import Data.Aeson (FromJSON (..), ToJSON (..), Value)

import Network.Wai.Handler.Warp as Wrp
import Network.Wai.Parse (setMaxRequestKeyLength, defaultParseRequestBodyOptions)
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Middleware.RequestLogger (logStdout)

-- Incompatible with aeson-2
-- import Network.Wai.Middleware.Servant.Errors (errorMw)

import GHC.Generics
import GHC.Stack (HasCallStack)
import Servant as Srv
import Servant.API.Generic
import Servant.Auth.Server (Auth, AuthResult (..), BasicAuth, BasicAuthCfg, CookieSettings (CookieSettings, cookieIsSecure)
                  , IsSecure (NotSecure), FromBasicAuthData, JWT, JWTSettings, FromJWT (..), ToJWT (..), cookieIsSecure
                  , defaultCookieSettings, defaultJWTSettings )
import qualified Servant.Auth.Server as Sauth
import Servant.Multipart (defaultMultipartOptions, MultipartOptions (..), Tmp)
import Servant.Server.Generic (AsServerT, genericServerT)

import System.Posix.Signals as Sgnl

import WebServer.JWT (readJWK)
import WebServer.CorsPolicy (setCorsPolicy)
import Options.Runtime as Ropt

setupWai :: Int -> IO () -> Settings
setupWai port shutdownCallback =
  Wrp.setPort port . Wrp.setGracefulShutdownTimeout (Just 5) . Wrp.setInstallShutdownHandler shutdownHandler
    -- . setBeforeMainLoop showBanner
    $ Wrp.defaultSettings
  where
    showBanner =
      putStrLn $ "@[setupWai] using port: " <> show port
    shutdownHandler closeSocket = do
      void $ installHandler Sgnl.sigTERM (Catch $ shutdownCallback >> closeSocket) Nothing
      void $ installHandler Sgnl.sigINT (Catch $ shutdownCallback >> closeSocket) Nothing


fakeContFct :: [a] -> IO Int
fakeContFct l = return (length l)
fakeEndFct :: Int -> IO ()
fakeEndFct aNum = pure ()

listen :: HasCallStack => Ropt.RunOptions -> IO ()
listen rtOpts = do
  let
    fakeContT = ContT $ bracket (fakeContFct "dummy.") fakeEndFct
  runContT fakeContT finalAction
  where
  finalAction dummy = do
    let shutdownCallback = putStrLn "@[Servant.run] Terminating..."
        settings = setupWai rtOpts.serverPort shutdownCallback
    webHandling <- runAPI rtOpts
    putStrLn $ "@[listen] final listen on port " <> show rtOpts.serverPort
    Wrp.runSettings settings webHandling


runAPI ::  Ropt.RunOptions -> IO Application
runAPI rtOpts = do
  myKey <- maybe (pure Nothing) (fmap Just . readJWK) rtOpts.jwkConfFile

  let
    cookieCfg = defaultCookieSettings { cookieIsSecure = NotSecure }
    jwtDefSettings = Sauth.defaultJWTSettings <$> myKey
    -- FOr file upload support, will be used later:
    multipartOpts :: MultipartOptions Tmp
    multipartOpts = (defaultMultipartOptions (Proxy :: Proxy Tmp)) {
          generalOptions = setMaxRequestKeyLength 512 defaultParseRequestBodyOptions
      }

    runContext = cookieCfg :. jwtDefSettings :. multipartOpts :. EmptyContext
    runCtxtProxy = Proxy :: Proxy '[CookieSettings, Maybe JWTSettings, BasicAuthCfg]

    corsMiddleware  = rtOpts.corsPolicy >>= (Just <$> setCorsPolicy)

    -- TODO: add errorMw @JSON @'["message", "status"] when Servant.Errors is compatible with aeson-2.
    -- TODO: enable corsMiddleware based on rtOpts.
    middlewares = linkUp $ id :| [ logStdout ]

    appEnv = AppEnv { jwtSettings = jwtDefSettings, rtOptions = rtOpts }
    server = hoistServerWithContext serverApiProxy runCtxtProxy (toHandler appEnv) serverApiT

  pure $ middlewares $ serveWithContext serverApiProxy runContext server
  where
    linkUp :: NonEmpty (a -> a) -> a -> a
    linkUp = foldr1 (.)

-- Route definitions:
type ServerApi = ToServantApi ServerRoutes


data ServerRoutes route = ServerRoutes {
    -- TODO: find why the authenticated route creates a non-instantiated [] BasicAuth...
    -- authenticated :: route :- Auth '[Maybe JWT, Sauth.BasicAuth] SessionContext :> ToServantApi AuthenticatedRoutes
    anonymous :: route :- ToServantApi AnonymousRoutes
  }
  deriving stock (Generic)


data AuthenticatedRoutes route = AuthenticatedRoutes {
    getPage :: route :- ToServantApi GetPageRoutes
  }
  deriving stock (Generic)


data AnonymousRoutes route = AnonymousRoutes {
    login :: route :- "inlogin" :> ReqBody '[JSON] LoginForm :> Post '[JSON] LoginResult
    , anonGetPage :: route :- "site" :> Capture "path" String :> Get '[HTML] RawHtml
    , homePage :: route :- Get '[HTML] RawHtml
  }
  deriving stock (Generic)


data GetPageRoutes route = GetPageRoutes {
    getPage :: route :- "private" :> Capture "path" String :> Get '[HTML] RawHtml
  }
  deriving stock (Generic)


-- Handler definitions:
serverApiProxy :: Proxy ServerApi
serverApiProxy = Proxy


serverApiT :: ToServant ServerRoutes (AsServerT WebApp)
serverApiT =
  genericServerT $ ServerRoutes {
    -- authenticated = authHandlers
    anonymous = anonHandlers
  }


authHandlers :: AuthResult SessionContext -> ToServant AuthenticatedRoutes (AsServerT WebApp)
authHandlers authResult =
  genericServerT $ AuthenticatedRoutes {
    getPage = getPageHandler authResult
  }


getPageHandler :: AuthResult SessionContext -> String -> WebApp RawHtml
getPageHandler authResult pageUrl =
  pure $ RawHtml "<html><head><title>TEST</title></head><body>TEST</body></html>"



anonHandlers :: ToServant AnonymousRoutes (AsServerT WebApp)
anonHandlers =
  genericServerT $ AnonymousRoutes {
    login = loginHandler
    , anonGetPage = anonGetPageHandler
    , homePage = homePageHandler
  }


loginHandler :: LoginForm -> WebApp LoginResult
loginHandler form@LoginForm {..} = do
  pure $ LoginResult {
      context = SessionContext 1
      , jwt = decodeUtf8 . LBS.toStrict $ "<fake-jwt"
    }


anonGetPageHandler :: String -> WebApp RawHtml
anonGetPageHandler pageUrl = do
  let
    tmpStr = encodeUtf8 . DT.pack $ pageUrl
  pageContent <- liftIO $ LBS.readFile ("/tmp/" <>pageUrl)
  pure . RawHtml $ "<html><head><title>TEST</title></head><body>TEST: " <> LBS.toStrict pageContent <> "</body></html>"

homePageHandler :: WebApp RawHtml
homePageHandler =
  pure . RawHtml $ "<html><head><title>DANIELL</title></head><body>Welcome to DANIELL, the site starts at <a href='/site/landing.html'>Landing</a><br/><br/>For more information, consult <a href='https://whatsupfudd.com'>FUDD</a></body></html>"


data LoginResult = LoginResult {
    context :: !SessionContext
    , jwt :: !DT.Text
  }
  deriving stock Generic
  deriving anyclass (ToJSON)

data LoginForm = LoginForm {
  username :: !DT.Text
  , password :: !DT.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HTML = HTML
newtype RawHtml = RawHtml { rawContent :: ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = LBS.fromStrict . rawContent


data SessionContext = SessionContext {
    sessionID :: DI.Int32
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

instance ToJWT SessionContext
instance FromJWT SessionContext
type instance BasicAuthCfg = Srv.BasicAuthData -> IO (AuthResult SessionContext)
instance FromBasicAuthData SessionContext where
  fromBasicAuthData authData authCheckFun = authCheckFun authData


data ServerApiError
  = NotImplemented
  | UnexpectedError DT.Text
  | NotAuthorized DT.Text
  | Unaccessible
  | NotFound DT.Text
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)


toServerError :: ServerApiError -> ServerError
toServerError err =
  case err of
    Unaccessible -> err401 { errBody = "Resource not accessible" }
    NotImplemented -> err500 { errBody = "NotImplemented" }
    UnexpectedError x -> err500 { errBody = LBS.fromStrict . encodeUtf8 $ x }
    NotAuthorized x -> err401 { errBody = LBS.fromStrict . encodeUtf8 $ x }
    NotFound x -> err404 { errBody = LBS.fromStrict . encodeUtf8 $ x }


data AppEnv = AppEnv {
    jwtSettings :: Maybe JWTSettings
  , rtOptions :: Ropt.RunOptions
  }


newtype WebApp a = WebApp {
    runApp :: ReaderT AppEnv (ExceptT ServerApiError IO) a
  }
  deriving newtype (
    Functor, Applicative, Monad, MonadMask, MonadCatch, MonadThrow
    , MonadReader AppEnv, MonadIO, MonadError ServerApiError
  )


-- | Natural transformations between 'App' and 'Handler' monads
toHandler :: AppEnv -> WebApp a -> Srv.Handler a
toHandler e =
  Handler . withExceptT toServerError . flip runReaderT e . runApp


fromAuthResult :: Show a => AuthResult a -> WebApp a
fromAuthResult (Authenticated uid) = return uid
fromAuthResult x =
  throwError . NotAuthorized . DT.pack . show $ x
