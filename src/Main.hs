module Main where

import Import

import Network.AWS
import Network.AWS.EC2
import Control.Lens

data MyTag = MyTag { myName :: Text, myValue :: Text } deriving (Eq, Show)
simpleTag
  :: TagDescription
  -> MyTag
simpleTag td =
  MyTag
  { myName = td^.tdKey
  , myValue = td^.tdValue
  }

newtype MyInstanceId = MyInstanceId { unInstanceId :: Text } deriving (Eq, Show)
newtype MyInstanceRep = MyInstanceRep { unInstanceRep :: (MyInstanceId, [MyTag])} deriving (Eq, Show)

prettyPrintInstance
  :: MyInstanceRep
  -> Text
prettyPrintInstance instanceRep =
  let
    (instId, tags) = unInstanceRep instanceRep
    ppTag MyTag{..} =
      "  Name: " <> myName <> "\n" <>
      "  Value: " <> myValue <> "\n"
  in "Instance ID: " <> unInstanceId instId <> "\n"
  <> mconcat (map ppTag tags)

main :: IO ()
main = do
  putStrLn "Welcome to janitor"

  args <- getArgs
  case map utf8bs args of
    [accessKey, secretKey] -> runWithCreds $
      FromKeys (AccessKey accessKey) (SecretKey secretKey)
    [accessKey, secretKey, sessionKey] -> runWithCreds $
      FromSession (AccessKey accessKey) (SecretKey secretKey) (SessionToken sessionKey)
    otherwise ->
      putStrLn $ "Error: either run with 'janitor <accesskey> <secretkey>' or 'janitor <accesskey> <secretkey> <sessiontoken>'"

runWithCreds
  :: Credentials
  -> IO ()
runWithCreds creds =
  let
    regions =
      [ Ireland
      , Frankfurt
      , Tokyo
      , Singapore
      , Sydney
      --, Beijing
      , NorthVirginia
      , NorthCalifornia
      , Oregon
      , SaoPaulo
      ]
  in forM_ regions $ \region -> do
    putStrLn $ "Checking for instances in " <> tshow region
    instances <- instancesForRegion creds region
    forM_ instances $ putStrLn . prettyPrintInstance

instancesForRegion
  :: (MonadIO m, MonadCatch m, MonadBaseControl IO m)
  => Credentials
  -> Region
  -> m [MyInstanceRep]
instancesForRegion creds region = do
  env <- newEnv region creds

  (runResourceT . runAWS env) $ do
    r <- send describeInstances

    let
      instances = r ^. dirsReservations . to ((=<<) $ view rInstances)
      responseStatus = r ^. dirsResponseStatus
      maybeNextToken = r ^. dirsNextToken

    -- TODO: Actually keep on getting more instances here
    when (isJust maybeNextToken) $ putStrLn "Got a next token"

    forM instances $ tagsForInstance

tagsForInstance
  :: (MonadAWS m)
  => Instance
  -> m MyInstanceRep
tagsForInstance singleInstance =
  let
    instanceId = singleInstance^.insInstanceId

    instanceFilters = [resourceTypeFilter, resourceIdFilter]
    resourceTypeFilter = filter' "resource-type"
      & fValues .~ ["instance"]
    resourceIdFilter = filter' "resource-id"
      & fValues .~ [instanceId]
    tagReq = describeTags & dtFilters .~ instanceFilters

    tagsFrom tagResponse = tagResponse^.dtrsTags . to (map simpleTag)

    tagRep tags = MyInstanceRep (MyInstanceId instanceId, tagsFrom tags)
  in tagRep <$> send tagReq

