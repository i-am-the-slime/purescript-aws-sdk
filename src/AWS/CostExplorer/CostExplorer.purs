module AWS.CostExplorer where

import Prelude
import AWS.Core.Client (makeClientHelper, makeDefaultClient)
import AWS.Core.Util (raiseEither, toIso8601Date)
import AWS.Core.Types (CostAndUsage, DateInterval, DefaultClientProps, DefaultClientPropsR, Group, GroupDefinition, MetricValue, NextPageToken(..), ResultByTime, Key(..), Amount(..), Metric)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.DateTime (DateTime)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, null)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Justifill.Fillable (class FillableFields)
import Justifill.Justifiable (class JustifiableFields)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Data.Formatter.DateTime (unformatDateTime)
import Data.Either (hush, Either)

foreign import data CE :: Type

foreign import newCE :: Foreign -> (Effect CE)

makeClient ::
  forall t4 t5 t6 t7 t8.
  RowToList t6 t5 =>
  FillableFields t5 () t6 =>
  Union
    t8
    t6
    DefaultClientPropsR =>
  RowToList t7 t4 => JustifiableFields t4 t7 () t8 => Record t7 -> Effect CE
makeClient r = ((makeDefaultClient r :: DefaultClientProps)) # makeClientHelper newCE

-- https://github.com/aws/aws-sdk-js/blob/dabf8b11e6e0d61d4dc2ab62717b8735fb8b29e4/clients/costexplorer.d.ts#L649
type InternalGetCostAndUsageResponse
  = { "ResultsByTime" :: Nullable (Array InternalResultByTime)
    , "GroupDefinitions" :: Nullable (Array InternalGroupDefinition)
    , "NextPageToken" :: Nullable (String)
    }

toCostAndUsage :: InternalGetCostAndUsageResponse -> CostAndUsage
toCostAndUsage internal =
  { resultsByTime: toMaybe internal."ResultsByTime" <#> (\internalResults -> internalResults <#> (toResultByTime))
  , groupDefinitions: toMaybe internal."GroupDefinitions" <#> (\internalGD -> internalGD <#> (toGroupDefinition))
  , nextPageToken: toMaybe internal."NextPageToken" <#> NextPageToken
  }

type InternalGroupDefinition
  = { "Key" :: Nullable String }

toGroupDefinition :: InternalGroupDefinition -> GroupDefinition
toGroupDefinition internal = { key: toMaybe internal."Key" <#> Key }

type InternalResultByTime
  = { "TimePeriod" :: Nullable InternalDateInterval, "Groups" :: Nullable (Array InternalGroup) }

toResultByTime :: InternalResultByTime -> ResultByTime
toResultByTime internal =
  { timePeriod: toMaybe internal."TimePeriod" >>= toDateInterval
  , groups: toMaybe internal."Groups" <#> (\internalGroups -> internalGroups <#> (toGroup))
  }

type InternalDateInterval
  = { "Start" :: String, "End" :: String }

parseDateTime :: String -> Either String DateTime
parseDateTime = unformatDateTime "YYYY-MM-DD"

toDateInterval :: InternalDateInterval -> Maybe DateInterval
toDateInterval internal =
  hush
    $ do
        start <- parseDateTime internal."Start"
        end <- parseDateTime internal."End"
        pure { start, end }

type InternalGroup
  = { "Keys" :: Nullable (Array String), "Metrics" :: Nullable InternalMetric }

toGroup :: InternalGroup -> Group
toGroup internal =
  { keys: toMaybe internal."Keys" <#> (\keys -> keys <#> Key)
  , metrics: toMaybe internal."Metrics" <#> toMetric
  }

type InternalMetric
  = { "UnblendedCost" :: Nullable InternalMetricValue }

toMetric :: InternalMetric -> Metric
toMetric internal = { unblendedCost: toMaybe internal."UnblendedCost" <#> toMetricValue }

type InternalMetricValue
  = { "Amount" :: Nullable String }

toMetricValue :: InternalMetricValue -> MetricValue
toMetricValue internal = { amount: toMaybe internal."Amount" <#> Amount }

foreign import getCostAndUsageImpl :: Fn2 CE InternalGetCostAndUsageParams (Effect (Promise InternalGetCostAndUsageResponse))

-- https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/CostExplorer.html#getCostAndUsage-property
type InternalGetCostAndUsageParams
  = { "Granularity" :: String
    , "GroupBy" :: Array { "Key" :: String, "Type" :: String }
    , "Metrics" :: Array String
    , "TimePeriod" :: { "End" :: String, "Start" :: String }
    , "NextPageToken" :: Nullable String
    }

getCostAndUsage ::
  forall a.
  CE ->
  { start :: DateTime, end :: DateTime | a } ->
  Aff CostAndUsage
getCostAndUsage ce range = do
  start <- raiseEither $ toIso8601Date range.start
  end <- raiseEither $ toIso8601Date range.end
  internalResp <- _getCostAndUsage ce start end null
  pure (toCostAndUsage internalResp)

_getCostAndUsage ::
  CE ->
  String ->
  String ->
  Nullable String ->
  Aff InternalGetCostAndUsageResponse
_getCostAndUsage ce start end nextPageToken = do
  result <-
    Promise.toAffE
      $ runFn2 getCostAndUsageImpl ce
          { "TimePeriod":
              { "Start": start
              , "End": end
              }
          , "Granularity": "DAILY"
          , "GroupBy":
              [ { "Key": "SERVICE"
                , "Type": "DIMENSION"
                }
              , { "Key": "USAGE_TYPE"
                , "Type": "DIMENSION"
                }
              ]
          , "Metrics": [ "UnblendedCost" ]
          , "NextPageToken": nextPageToken
          }
  pure result
