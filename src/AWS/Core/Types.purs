module AWS.Core.Types where

import Prelude (class Show)
import Data.Newtype (class Newtype)
import Simple.JSON (class WriteForeign, class ReadForeign)
import Data.Maybe (Maybe)
import Data.DateTime (DateTime)

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

type BasicClientPropsR r
  = ( accessKeyId :: Maybe AccessKeyId
    , secretAccessKey :: Maybe SecretAccessKey
    , region :: Maybe Region
    , sessionToken :: Maybe SessionToken
    | r
    )

type DefaultClientPropsR
  = BasicClientPropsR ()

type DefaultClientProps
  = Record DefaultClientPropsR

newtype NextPageToken
  = NextPageToken String

derive instance ntNextPageToken :: Newtype NextPageToken _

derive newtype instance showNextPageToken :: Show NextPageToken

derive newtype instance wfNextPageToken :: WriteForeign NextPageToken

derive newtype instance rfNextPageToken :: ReadForeign NextPageToken

newtype Key
  = Key String

derive instance ntKey :: Newtype Key _

derive newtype instance showKey :: Show Key

derive newtype instance wfKey :: WriteForeign Key

derive newtype instance rfKey :: ReadForeign Key

newtype Amount
  = Amount String

derive instance ntAmount :: Newtype Amount _

derive newtype instance showAmount :: Show Amount

derive newtype instance wfAmount :: WriteForeign Amount

derive newtype instance rfAmount :: ReadForeign Amount

type GroupDefinition
  = { key :: Maybe Key }

type DateInterval
  = { start :: DateTime, end :: DateTime }

type MetricValue
  = { amount :: Maybe Amount }

type Metric
  = { unblendedCost :: Maybe MetricValue }

type Group
  = { keys :: Array Key, metrics :: Maybe Metric }

type ResultByTime
  = { timePeriod :: Maybe DateInterval, groups :: Array Group }

type CostAndUsage
  = { resultsByTime :: Array ResultByTime
    , groupDefinitions :: Array GroupDefinition
    , nextPageToken :: Maybe NextPageToken
    }
