module Main (main) where

import Test.Hspec (hspec)

import qualified CollisionSpec
import qualified RulesSpec
import qualified ParserSpec
import qualified SaveSpec
import qualified Parallel.PropertySpec
import qualified StateSyncSpec
import qualified ServerSpec
import qualified NetworkSpec
import qualified InputSpec
import qualified DescribableSpec
import qualified ClockSpec
import qualified MechanicsSpec
import qualified HpSpec
import qualified MapIdSpec
import qualified LineSpec
import qualified WorkerSpec
import qualified MathSpec
import qualified EventsSpec

main :: IO ()
main = hspec $ do
  CollisionSpec.spec
  RulesSpec.spec
  ParserSpec.spec
  SaveSpec.spec
  Parallel.PropertySpec.spec
  StateSyncSpec.spec
  ServerSpec.spec
  NetworkSpec.spec
  InputSpec.spec
  DescribableSpec.spec
  ClockSpec.spec
  MechanicsSpec.spec
  HpSpec.spec
  MapIdSpec.spec
  LineSpec.spec
  WorkerSpec.spec
  MathSpec.spec
  EventsSpec.spec
