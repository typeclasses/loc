module Main (main) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Loc
import Data.Loc.Area qualified as Area
import Data.Loc.Internal.Prelude
import Data.Loc.List.OneToTwo qualified as OneToTwo
import Data.Loc.List.ZeroToTwo qualified as ZeroToTwo
import Data.Loc.Loc qualified as Loc
import Data.Loc.Pos qualified as Pos
import Data.Loc.Span qualified as Span
import Gen qualified
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog
import Prelude (Num (..), fromInteger, ($!))

main :: IO ()
main = hspec do
  posSpec
  locSpec
  spanSpec
  areaSpec

posSpec :: SpecWith ()
posSpec = describe "Pos" do
  specify "fromInteger" $ (fromInteger 3 :: Pos) == 3
  specify "fromInteger underflow" $ (return $! (fromInteger 0 :: Pos)) `shouldThrow` (== Underflow)
  specify "2 + 3" $ (2 + 3 :: Pos) == 5
  specify "3 - 2" $ (3 - 2 :: Pos) == 1
  specify "(-) underflow" $ (return $! (3 - 3 :: Pos)) `shouldThrow` (== Underflow)
  specify "2 * 3" $ (2 * 3 :: Pos) == 6
  specify "negate underflow" $ (return $! (negate 3 :: Pos)) `shouldThrow` (== Underflow)
  specify "toEnum" $ (toEnum 3 :: Pos) == 3
  specify "toEnum underflow" $ (return $! (toEnum 0 :: Pos)) `shouldThrow` (== Underflow)
  specify "fromEnum" $ fromEnum (3 :: Pos) == 3
  specify "show 1" $ Pos.posShowsPrec minPrec 1 "" == "1"
  specify "show 42" $ Pos.posShowsPrec minPrec 42 "" == "42"
  specify "read 1" $ readPrec_to_S Pos.posReadPrec minPrec "1" == [(1, "")]
  specify "read 42" $ readPrec_to_S Pos.posReadPrec minPrec "42" == [(42, "")]
  specify "read 0" $ readPrec_to_S Pos.posReadPrec minPrec "0" == []
  specify "read -1" $ readPrec_to_S Pos.posReadPrec minPrec "-1" == []

locSpec :: SpecWith ()
locSpec = describe "Loc" do
  specify "read and show" $ hedgehog do
    x <- forAll Gen.loc'
    read (show x) === x

  specify "read and show examples" $ hedgehog do
    show Loc.origin === "1:1"
    Loc.locShowsPrec minPrec (loc 3 14) "" === "3:14"
    readPrec_to_S Loc.locReadPrec minPrec "3:14" === [(read "3:14", "")]

spanSpec :: SpecWith ()
spanSpec = describe "Span" do
  specify "joinAsc" $ hedgehog do
    spans <- forAll (Gen.list (Range.linear 1 10) Gen.span')
    areaSpansAsc (foldMap spanArea spans) === Span.joinAsc (List.sort spans)

  specify "read and show" $ hedgehog do
    x <- forAll Gen.span'
    read (show x) === x

  describe "read and show examples" do
    specify "show 3:14-6:5" $ Span.spanShowsPrec minPrec (Span.fromTo (read "3:14") (read "6:5")) "" == "3:14-6:5"
    specify "read 3:14-6:5" $ show (readPrec_to_S Span.spanReadPrec minPrec "3:14-6:5") == "[(3:14-6:5,\"\")]"
    specify "read 6:5-3:14" $ show (readPrec_to_S Span.spanReadPrec minPrec "6:5-3:14") == "[(3:14-6:5,\"\")]"
    specify "read 6:5-6:5" $ readPrec_to_S Span.spanReadPrec minPrec "6:5-6:5" == []

  describe "lines" do
    specify "one" $ NonEmpty.toList (Span.lines (read "2:6-2:10")) == [2]
    specify "many" $ NonEmpty.toList (Span.lines (read "2:6-8:4")) == [2, 3, 4, 5, 6, 7, 8]

  describe "overlapping" do
    specify "if only touching, no" $ not $ Span.overlapping (read "1:5-1:8") (read "1:8-1:12")
    specify "on a single line" $ Span.overlapping (read "1:5-1:9") (read "1:8-1:12")
    specify "one contained within another" $ Span.overlapping (read "1:5-1:15") (read "1:6-1:10")
    specify "same span" $ hedgehog do
      x <- forAll Gen.span'
      assert (Span.overlapping x x)

  describe "linesOverlapping" do
    specify "same span" $ hedgehog do
      x <- forAll Gen.span'
      assert (Span.linesOverlapping x x)
    specify "multi line" $ Span.linesOverlapping (read "1:1-1:2") (read "1:1-2:1")
    specify "no" $ not $ Span.linesOverlapping (read "1:1-1:2") (read "2:1-2:2")

  describe "touching" do
    specify "same span" $ hedgehog do
      x <- forAll Gen.span'
      assert (Span.touching x x)
    specify "barely" $ Span.touching (read "1:1-1:2") (read "1:2-1:3")
    specify "overlapping" $ Span.touching (read "1:1-1:2") (read "1:1-1:3")
    specify "no" $ not $ Span.touching (read "1:1-1:2") (read "1:3-1:4")

  describe "join" do
    specify "touching" $ Span.join (read "1:1-1:2") (read "1:2-1:3") == read "1:1-1:3"
    specify "overlapping" $ Span.join (read "1:1-1:2") (read "1:1-1:3") == read "1:1-1:3"

  describe "addition" do
    specify "example 1" $ read "1:1-1:2" Span.+ read "1:2-1:3" == OneToTwo.One (read "1:1-1:3")
    specify "example 2" $ read "1:1-1:2" Span.+ read "1:1-3:1" == OneToTwo.One (read "1:1-3:1")
    specify "example 3" $ read "1:1-1:2" Span.+ read "1:1-11:1" == OneToTwo.One (read "1:1-11:1")
    specify "example 4" $ read "1:1-1:2" Span.+ read "2:1-2:5" == OneToTwo.Two (read "1:1-1:2") (read "2:1-2:5")
    specify "example 5" $ read "2:1-2:5" Span.+ read "1:1-1:2" == OneToTwo.Two (read "2:1-2:5") (read "1:1-1:2")

  describe "subtraction" do
    specify "x - x" $ hedgehog do
      x <- forAll Gen.span'
      x Span.- x === ZeroToTwo.Zero
    specify "example 1" $ read "2:5-4:1" Span.- read "2:9-3:5" == ZeroToTwo.Two (read "2:5-2:9") (read "3:5-4:1")
    specify "example 2" $ read "2:5-4:1" Span.- read "2:5-3:5" == ZeroToTwo.One (read "3:5-4:1")
    specify "example 3" $ read "2:5-4:1" Span.- read "2:2-3:5" == ZeroToTwo.One (read "3:5-4:1")
    specify "example 4" $ read "2:5-4:1" Span.- read "2:2-4:4" == ZeroToTwo.Zero
    specify "example 5" $ read "1:1-8:1" Span.- read "1:2-8:1" == ZeroToTwo.One (read "1:1-1:2")

areaSpec :: SpecWith ()
areaSpec = describe "Area" do
  specify "add mempty = id for a single span" $ hedgehog do
    a <- forAll Gen.span'
    spanArea a Area.+ mempty === spanArea a

  specify "subtract mempty = id for a single span" $ hedgehog do
    a <- forAll Gen.span'
    spanArea a Area.- mempty === spanArea a

  specify "addition is commutative" $ hedgehog do
    a <- forAll Gen.area'
    b <- forAll Gen.area'
    a Area.+ b === b Area.+ a

  specify "add mempty = id" $ hedgehog do
    a <- forAll Gen.area'
    a Area.+ mempty === a

  specify "subtract mempty = id" $ hedgehog do
    a <- forAll Gen.area'
    a Area.- mempty === a

  specify "addition and subtraction" $ hedgehog do
    a <- forAll Gen.area'
    b <- forAll Gen.area'
    c <- forAll Gen.area'
    a Area.- b Area.- c === a Area.- (b Area.+ c)

  specify "addSpan" $ hedgehog do
    a <- forAll Gen.area'
    s <- forAll Gen.span'
    Area.addSpan s a === areaUnion (spanArea s) a

  specify "fromTo mempty 1" $ hedgehog do
    x <- forAll Gen.loc'
    y <- forAll Gen.loc'
    (Area.fromTo x y == mempty) === (x == y)

  specify "fromTo mempty 2" $ hedgehog do
    x <- forAll Gen.loc'
    Area.fromTo x x === mempty

  specify "read and show" $ hedgehog do
    x <- forAll Gen.area'
    read (show x) === x

  specify "read and show example 1" $ hedgehog do
    let x = show (readPrec_to_S Area.areaReadPrec minPrec "[]")
    x === "[([],\"\")]"

  specify "read and show example 2" $ hedgehog do
    x <- forAll (Gen.element ["[3:2-5:5,8:3-11:4]", "[3:2-5:5,11:4-8:3]"])
    let y = show (readPrec_to_S Area.areaReadPrec minPrec x)
    y === "[([3:2-5:5,8:3-11:4],\"\")]"

  specify "read and show example 3" $ hedgehog do
    let x = show (readPrec_to_S Area.areaReadPrec minPrec "[3:2-5:5,8:3-8:3]")
    x === "[]"

  specify "constructed from a single span" $ hedgehog do
    let x = read "4:5-6:3"
    spanArea x === read "[4:5-6:3]"

  specify "converted to a span, maybe" $ hedgehog do
    Area.areaSpan mempty === Nothing
    Area.areaSpan (read "[3:4-7:2]") === Just (read "3:4-7:2")
    Area.areaSpan (read "[3:4-7:2,15:6-17:9]") === Just (read "3:4-17:9")

  specify "converted to a list of spans" $ hedgehog do
    Area.spansAsc mempty === []
    Area.spansAsc (read "[3:4-7:2,15:6-17:9]") === [read "3:4-7:2", read "15:6-17:9"]

  specify "spanCount" $ hedgehog do
    Area.spanCount mempty === 0
    Area.spanCount (read "[3:4-7:2]") === 1
    Area.spanCount (read "[3:4-7:2,15:6-17:9]") === 2

  specify "firstSpan" $ hedgehog do
    Area.firstSpan mempty === Nothing
    Area.firstSpan (read "[3:4-7:2]") === Just (read "3:4-7:2")
    Area.firstSpan (read "[3:4-7:2,15:6-17:9]") === Just (read "3:4-7:2")

  specify "lastSpan" $ hedgehog do
    Area.lastSpan mempty === Nothing
    Area.lastSpan (read "[3:4-7:2]") === Just (read "3:4-7:2")
    Area.lastSpan (read "[3:4-7:2,15:6-17:9]") === Just (read "15:6-17:9")

  specify "start" $ hedgehog do
    Area.start mempty === Nothing
    Area.start (read "[3:4-7:2]") === Just (read "3:4")
    Area.start (read "[3:4-7:2,15:6-17:9]") === Just (read "3:4")

  specify "end" $ hedgehog do
    Area.end mempty === Nothing
    Area.end (read "[3:4-7:2]") === Just (read "7:2")
    Area.end (read "[3:4-7:2,15:6-17:9]") === Just (read "17:9")

  specify "addition examples" $ hedgehog do
    read "[1:1-1:2]" Area.+ mempty === read "[1:1-1:2]"
    read "[1:1-1:2]" Area.+ read "[1:2-1:3]" === read "[1:1-1:3]"
    read "[1:1-1:2]" Area.+ read "[1:1-3:1]" === read "[1:1-3:1]"
    read "[1:1-1:2]" Area.+ read "[1:1-11:1]" === read "[1:1-11:1]"
    read "[1:1-3:1,6:1-6:2]" Area.+ read "[1:1-6:1]" === read "[1:1-6:2]"
    read "[1:1-3:1]" Area.+ read "[5:1-6:2]" === read "[1:1-3:1,5:1-6:2]"

  specify "addSpan examples" $ hedgehog do
    Area.addSpan (read "1:1-6:1") (read "[1:1-3:1,6:1-6:2]") === read "[1:1-6:2]"
