module LineSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS

import Net.Line (scanBuffer)
import Model.Error (GameError(..))
import Model.Config (maxLineBytes)

spec :: Spec
spec = describe "TCP line length limit" $ do

  it "accepts a normal line" $
    scanBuffer maxLineBytes (BS.pack "STATS\n")
      `shouldBe` Right (Just "STATS")

  it "accepts exactly 1024 bytes" $
    scanBuffer maxLineBytes (BS.pack (replicate 1024 'A' ++ "\n"))
      `shouldBe` Right (Just (replicate 1024 'A'))

  it "rejects 1025 bytes" $
    scanBuffer maxLineBytes (BS.pack (replicate 1025 'A' ++ "\n"))
      `shouldBe` Left (LineTooLong 1024)

  it "waits when newline has not arrived" $
    scanBuffer maxLineBytes (BS.pack "RESULT 1 P1")
      `shouldBe` Right Nothing

  it "rejects oversized data without newline" $
    scanBuffer maxLineBytes (BS.pack (replicate 1025 'A'))
      `shouldBe` Left (LineTooLong 1024)

  it "removes CR before returning the line" $
    scanBuffer maxLineBytes (BS.pack "STATS\r\n")
      `shouldBe` Right (Just "STATS")

  it "checks only the first line" $
    scanBuffer maxLineBytes (BS.pack ("STATS\n" ++ replicate 2000 'A'))
      `shouldBe` Right (Just "STATS")