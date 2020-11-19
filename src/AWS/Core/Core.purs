module AWS.Core where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.DateTime (DateTime)
import Data.Either (Either, either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Newtype (class Newtype)
import Effect.Exception (Error, error)
import Simple.JSON (class WriteForeign, class ReadForeign)
import Data.Maybe (Maybe)

--
newtype AccessKeyId
  = AccessKeyId String

derive instance ntAccessKeyId :: Newtype AccessKeyId _

derive newtype instance wfAccessKeyId :: WriteForeign AccessKeyId

newtype Region
  = Region String

derive instance ntRegion :: Newtype Region _

derive newtype instance wfRegion :: WriteForeign Region

newtype SecretAccessKey
  = SecretAccessKey String

derive instance ntSecretAccessKey :: Newtype SecretAccessKey _

derive newtype instance wfSecretAccessKey :: WriteForeign SecretAccessKey

newtype SessionToken
  = SessionToken String

derive instance ntSessionToken :: Newtype SessionToken _

derive newtype instance wfSessionToken :: WriteForeign SessionToken

type Credentials
  = { accessKeyId :: AccessKeyId
    , secretAccessKey :: SecretAccessKey
    , sessionToken :: SessionToken
    }

newtype Arn
  = Arn String

derive instance ntArn :: Newtype Arn _

newtype ExternalId
  = ExternalId String

derive instance ntExternalId :: Newtype ExternalId _

type Instance
  = { id :: InstanceId, "type" :: InstanceType }

newtype InstanceId
  = InstanceId String

derive instance ntInstanceId :: Newtype InstanceId _

derive newtype instance showInstanceId :: Show InstanceId

newtype InstanceType
  = InstanceType String

derive instance ntInstanceType :: Newtype InstanceType _

derive newtype instance showInstanceType :: Show InstanceType

formatted :: DateTime -> Either String String
formatted d = formatDateTime "YYYY-MM-DD" d

raiseEither :: forall m r. MonadThrow Error m => Either String r -> m r
raiseEither = either (error >>> throwError) pure

newtype NextPageToken
  = NextPageToken String

derive instance ntNextPageToken :: Newtype NextPageToken _

derive newtype instance showNextPageToken :: Show NextPageToken

derive newtype instance wfNextPageToken :: WriteForeign NextPageToken

derive newtype instance rfNextPageToken :: ReadForeign NextPageToken

type GroupDefinition
  = { key :: Maybe String }

type DateInterval
  = { start :: DateTime, end :: DateTime }

type MetricValue
  = { amount :: Maybe String }

type Metric
  = { unblendedCost :: Maybe MetricValue }

type Group
  = { keys :: Maybe (Array String), metrics :: Maybe Metric }

type ResultByTime
  = { timePeriod :: Maybe DateInterval, groups :: Maybe (Array Group) }

type CostAndUsage
  = { resultsByTime :: Maybe (Array ResultByTime)
    , groupDefinitions :: Maybe (Array GroupDefinition)
    , nextPageToken :: Maybe NextPageToken
    }
