{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text formatting of 'Double's.
--
-- In particular, the library provides functionality to calculate and display a fixed number of <https://en.wikipedia.org/wiki/Significant_figures significant figures> for a variety of different number formatting styles.
--
--
-- Some similar libraries that may be better suited for different use cases include:
--
-- Flexible formatters. These libraries provide more flexibility around formatting options, but do not have a concept of significance:
--
-- - <https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-Printf.html Text.Printf> and <https://hackage.haskell.org/package/base-4.16.0.0/docs/Numeric.html#v:showFFloat Numeric> in base.
-- - <https://hackage.haskell.org/package/formatting Formatting>
-- - <https://hackage.haskell.org/package/vformat-0.9.0.0 vformat: A Python str.format() like formatter>
--
-- <https://hackage.haskell.org/package/text-format text-format> has similar functionality but is not native haskell and I wanted to do some tweaking to defaults. It's probably safer and faster.
--
-- <https://hackage.haskell.org/package/rounded rounded> seems to be much more about doing computation taking rounding into account, compared with the much simpler task of pretty printing a number.
--
-- This library could have just provided an ability to compute a significant figure version of a number and then use these other libraries, but the round trip (from Double to SigFig to Double) introduces errors (eg the least significant figure goes from being a '4' to a '3999999' via float maths).
module Data.FormatN
  ( -- * Usage
    -- $usage

    -- * FormatN
    FormatN (..),
    defaultFormatN,
    formatN,
    formatNs,
    precision,

    -- * SigFig
    SigFig (..),
    SigFigSign (..),
    toSigFig,
    fromSigFig,

    -- * formatters
    fixed,
    expt,
    decimal,
    prec,
    comma,
    dollar,
    percent,
    showOr,
  )
where

import Data.Bifunctor
import Data.Bool
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as Text
import GHC.Generics hiding (prec)
import Numeric
import Prelude

-- $setup
-- >>> import Data.FormatN
-- >>> xs = [(-1),0,1,1.01,1.02,1.1,1.2]
--
-- >>> fixed (Just 2) <$> xs
-- ["-1.00","0.00","1.00","1.01","1.02","1.10","1.20"]
--
-- >>> decimal (Just 2) <$> xs
-- ["-1.0","0.0","1.0","1.0","1.0","1.1","1.2"]
--
-- >>> comma (Just 3) <$> xs
-- ["-1.00","0.00","1.00","1.01","1.02","1.10","1.20"]
--
-- comma (Just 3) . (1e3*) <$> xs
-- ["-1,000","0.00","1,000","1,010","1,020","1,100","1,200"]
--
-- comma (Just 3) . (1e-3*) <$> xs
-- ["-0.00100","0.00","0.00100","0.00101","0.00102","0.00110","0.00120"]
--
-- >>> comma (Just 3) . (1e-6*) <$> xs
-- ["-1.00e-6","0.00","1.00e-6","1.01e-6","1.02e-6","1.10e-6","1.20e-6"]
--
-- >>> comma (Just 2) . (1e3*) <$> xs
-- ["-1,000","0.0","1,000","1,000","1,000","1,100","1,200"]
--
-- >>> precision comma (Just 2) $ (1e3*) <$> [0,1,1.01,1.02,1.1,1.2]
-- ["0.00","1,000","1,010","1,020","1,100","1,200"]

-- $usage
-- >>> import Data.FormatN
-- >>> xs = [(-1),0,1,1.01,1.02,1.1,1.2]
-- >>> fixed (Just 2) <$> xs
-- ["-1.00","0.00","1.00","1.01","1.02","1.10","1.20"]
--
-- >>> decimal (Just 2) <$> xs
-- ["-1.0","0.0","1.0","1.0","1.0","1.1","1.2"]
--
-- formatn is used in the <https://hackage.haskell.org/package/chart-svg chart-svg> library to automate consistent number formatting across different scales.
--
-- >>> comma (Just 3) <$> xs
-- ["-1.00","0.00","1.00","1.01","1.02","1.10","1.20"]
--
-- >>> comma (Just 3) . (1e3*) <$> xs
-- ["-1,000","0.00","1,000","1,010","1,020","1,100","1,200"]
--
-- >>> comma (Just 3) . (1e-3*) <$> xs
-- ["-0.00100","0.00","0.00100","0.00101","0.00102","0.00110","0.00120"]
--
-- >>> comma (Just 3) . (1e-6*) <$> xs
-- ["-1.00e-6","0.00","1.00e-6","1.01e-6","1.02e-6","1.10e-6","1.20e-6"]
--
-- Using significant figures actually changes numbers - numbers that were slightly different end up being (and looking like) the same. 'precision' increases the number of significant figures to get around this.
--
-- >>> comma (Just 2) . (1e3*) <$> xs
-- ["-1,000","0.0","1,000","1,000","1,000","1,100","1,200"]
--
-- >>> precision comma (Just 2) $ (1e3*) <$> [0,1,1.01,1.02,1.1,1.2]
-- ["0.00","1,000","1,010","1,020","1,100","1,200"]
--
-- Also note the clunkiness of the treatment of zero. It is problematic to default format zero consistently.

-- | Wrapper for the various formatting options.
--
-- Nothing in the context of these constructors means do not perform and significant figure adjustments to the numbers (or decimal figures with respect to FormatFixed).
data FormatN
  = FormatFixed (Maybe Int)
  | FormatDecimal (Maybe Int)
  | FormatComma (Maybe Int)
  | FormatExpt (Maybe Int)
  | FormatPrec (Maybe Int)
  | FormatDollar (Maybe Int)
  | FormatPercent (Maybe Int)
  | FormatNone
  deriving (Eq, Show, Generic)

-- | The official format
--
-- >>> defaultFormatN
-- FormatComma (Just 2)
defaultFormatN :: FormatN
defaultFormatN = FormatComma (Just 2)

-- | run a 'FormatN'
--
-- >>> formatN defaultFormatN 1234
-- "1,200"
formatN :: FormatN -> Double -> Text
formatN (FormatFixed n) x = fixed n x
formatN (FormatDecimal n) x = decimal n x
formatN (FormatPrec n) x = prec n x
formatN (FormatComma n) x = comma n x
formatN (FormatExpt n) x = expt n x
formatN (FormatDollar n) x = dollar n x
formatN (FormatPercent n) x = percent n x
formatN FormatNone x = pack (show x)

-- | Format to x decimal places with no significant figure rounding.
--
-- >>> fixed (Just 2) 100
-- "100.00"
--
-- >>> fixed (Just 2) 0.001
-- "0.00"
fixed :: Maybe Int -> Double -> Text
fixed n x = pack $ showFFloat n x ""

-- | Decomposition of a Double into the components that are needed to determine significant figure formatting.
--
-- eliding type changes, the relationship between a Double and a SigFig is:
--
-- \[
--   x == sign * figures * 10^{exponent}
-- \]
data SigFig = SigFig
  { -- | sign
    sign :: SigFigSign,
    -- | significant figures expressed as an Integer
    figures :: Integer,
    -- | the power of 10 exponent given figures.
    exponent :: Int
  }
  deriving (Eq, Show)

-- | Sign component
data SigFigSign = SigFigNeg | SigFigPos deriving (Eq, Show)

sfsign :: SigFigSign -> String
sfsign s = bool "" "-" (s == SigFigNeg)

-- | convert from a Double to a 'SigFig'
--
-- >>> toSigFig (Just 2) 1234
-- SigFig {sign = SigFigPos, figures = 12, exponent = 2}
--
-- prop> \x -> let (SigFig s fs e) = toSigFig Nothing x in let x' = ((if (s==SigFigNeg) then (-1.0) else 1.0) * fromIntegral fs * 10.0**fromIntegral e) in (x==0 || abs (x/x'-1) < 1e-6)
toSigFig :: Maybe Int -> Double -> SigFig
toSigFig n x = SigFig s fs' expo
  where
    (s, (floatfs, floate)) = bool (SigFigPos, floatToDigits 10 x) (SigFigNeg, floatToDigits 10 (-x)) (x < 0)
    -- floatToDigits 10 0 == ([0],0) floatToDigits 10 1 == ([1],1)
    floate' = bool floate (floate + 1) (x == 0)
    nsig = fromMaybe (length floatfs) n
    -- pad with extra zeros if less figures than requested
    (floatfs', e) =
      bool
      (floatfs, floate' - length floatfs)
      (floatfs <> replicate (nsig - length floatfs) 0, floate' - nsig)
      (length floatfs < nsig)
    (fs0, fs1) = splitAt nsig floatfs'
    -- reconstitute number to get rounding right at the least significance point
    fs =
      round $
        (fromIntegral $ foldl' (\x' a -> x' * 10 + a) 0 fs0 :: Double)
          + fromIntegral (foldl' (\x' a -> x' * 10 + a) 0 fs1) / (10.0 ^ (length fs1 :: Int))
    -- rounding can bump significant figures by 1 eg 99(.9999) ==> 100
    (fs', expo) =
      bool
      (fs, e + length floatfs' - nsig)
      (fs `div` 10, e + length floatfs' - nsig + 1)
      (length (show fs) > nsig)

-- | convert from a 'SigFig' to a Double
--
-- >>> fromSigFig (SigFig SigFigPos 12 2)
-- 1200.0
--
-- @fromSigFig . toSigFig Nothing@ may not be isomorphic
fromSigFig :: SigFig -> Double
fromSigFig (SigFig s fs e) = bool 1 (-1) (s == SigFigNeg) * fromIntegral fs * 10 ** fromIntegral e

exptR :: SigFig -> Text
exptR (SigFig s i e) = pack $ sfsign s <> i'' <> "e" <> show e'
  where
    i''
      | length i' == 1 = i'
      | otherwise = take 1 i' <> "." <> drop 1 i'
    i' = show i
    e' = e + length i' - 1

-- | Format in exponential style, maybe with significant figure rounding.
--
-- >>> expt Nothing 1245
-- "1.245e3"
--
-- >>> expt (Just 3) 1245
-- "1.24e3"
--
-- >>> expt (Just 3) 0.1245
-- "1.24e-1"
expt :: Maybe Int -> Double -> Text
expt n x = exptR (toSigFig n x)

decimalR :: SigFig -> Text
decimalR (SigFig s xs e) = pack $ sfsign s <> t
  where
    xs' = show xs
    nsf = length xs'
    extrasf = bool (-(e + nsf)) (-(e + nsf)) (xs == 0)
    oversf = length xs' + e
    t
      | e >= 0 = xs' <> replicate e '0'
      | e <= -nsf = "0." <> replicate extrasf '0' <> xs'
      | otherwise = take oversf xs' <> "." <> drop oversf xs'

-- | Format in decimal style, and maybe round to n significant figures.
--
-- >>> decimal Nothing 1.2345e-2
-- "0.012345"
--
-- >>> decimal (Just 2) 0.012345
-- "0.012"
--
-- >>> decimal (Just 2) 12345
-- "12000"
decimal :: Maybe Int -> Double -> Text
decimal n x = decimalR (toSigFig n x)

-- | Format between 0.001 and 1,000,000 using decimal style and exponential style outside this range.
--
-- >>> prec (Just 2) 0.00234
-- "0.0023"
--
-- >>> prec (Just 2) 0.000023
-- "2.3e-5"
--
-- >>> prec (Just 2) 123
-- "120"
--
-- >>> prec (Just 2) 123456
-- "120000"
--
-- >>> prec (Just 2) 1234567
-- "1.2e6"
prec :: Maybe Int -> Double -> Text
prec n x = bool (go x) ("-" <> go (-x)) (x < 0)
  where
    go x' =
      bool
        ( bool
            (decimal n x')
            (expt n x')
            (x' > 1e6)
        )
        (expt n x')
        (x' < 0.001 && (x /= 0))

-- | Format using comma separators for numbers above 1,000 but below 1 million, otherwise use prec style.
--
-- >>> comma (Just 3) 1234
-- "1,230"
comma :: Maybe Int -> Double -> Text
comma n x
  | x < 0 = "-" <> comma n (-x)
  | x < 1000 || x > 1e6 = prec n x
  | otherwise = addcomma (prec n x)
  where
    addcomma =
      uncurry (<>)
        . first (Text.reverse . Text.intercalate "," . Text.chunksOf 3 . Text.reverse)
        . Text.breakOn "."

-- | Format as dollars, always using comma notation
--
-- >>> dollar (Just 3) 1234
-- "$1,230"
--
-- >>> dollar (Just 2) 0.01234
-- "$0.012"
dollar :: Maybe Int -> Double -> Text
dollar n x
  | x < 0 = "-" <> dollar n (-x)
  | otherwise = "$" <> comma n x

-- | Format as a percentage using decimal style.
--
-- >>> percent (Just 2) 0.001234
-- "0.12%"
percent :: Maybe Int -> Double -> Text
percent n x = (<> "%") $ decimal n (100 * x)

precision_ :: (Maybe Int -> Double -> Text) -> Int -> [Double] -> [Text]
precision_ f n0 xs =
  precLoop n0 xs
  where
    precLoop n xs' =
      let s = f (Just n) <$> xs'
       in bool (precLoop (1 + n) xs') s (s == nubOrd s || n > 4)

-- | Provide formatted text for a list of numbers so that they are just distinguished.
--
-- For example, __@precision comma (Just 2)@__ means use as much significant figures as is needed for the numbers to be distinguished on rendering, but with at least 2 significant figures.
--
-- The difference between this and __@fmap (comma (Just 2))@__ can be seen in these examples:
--
-- >>> precision comma (Just 2) [0,1,1.01,1.02,1.1,1.2]
-- ["0.00","1.00","1.01","1.02","1.10","1.20"]
--
-- >>> fmap (comma (Just 2)) [0,1,1.01,1.02,1.1,1.2]
-- ["0.0","1.0","1.0","1.0","1.1","1.2"]
precision :: (Maybe Int -> Double -> Text) -> Maybe Int -> [Double] -> [Text]
precision f n xs =
  case n of
    Nothing -> f Nothing <$> xs
    Just n' -> precision_ f n' xs

-- | Consistently format a list of numbers via using 'precision'.
--
-- >>> formatNs defaultFormatN [0,1,1.01,1.02,1.1,1.2]
-- ["0.00","1.00","1.01","1.02","1.10","1.20"]
formatNs :: FormatN -> [Double] -> [Text]
formatNs (FormatFixed n) xs = precision fixed n xs
formatNs (FormatDecimal n) xs = precision decimal n xs
formatNs (FormatPrec n) xs = precision prec n xs
formatNs (FormatComma n) xs = precision comma n xs
formatNs (FormatExpt n) xs = precision expt n xs
formatNs (FormatDollar n) xs = precision dollar n xs
formatNs (FormatPercent n) xs = precision percent n xs
formatNs FormatNone xs = pack . show <$> xs

-- | Format with the shorter of show and formatN.
showOr :: FormatN -> Double -> Text
showOr f x = bool (bool f' (pack s') (Text.length (pack s') < Text.length f')) "0" (x < 1e-6 && x > -1e-6)
  where
    f' = formatN f x
    s' = show x
