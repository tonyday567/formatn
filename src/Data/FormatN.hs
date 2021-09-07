{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Formatting of 'Double's.
module Data.FormatN
  ( FormatN (..),
    defaultFormatN,
    fromFormatN,
    toFormatN,
    fixed,
    decimal,
    prec,
    comma,
    expt,
    dollar,
    formatN,
    precision,
    formatNs,
    roundSig,
    percent,
  )
where

import Data.Bifunctor
import Data.Containers.ListUtils (nubOrd)
import Data.Generics.Labels ()
import Data.Scientific
import Data.String
import Data.Text (Text, pack)
import qualified Data.Text as Text
import NumHask.Prelude hiding (prec)

-- | Wrapper for the various formatting options.
--
-- >>> defaultFormatN
-- FormatComma 2
data FormatN
  = FormatFixed Int
  | FormatDecimal Int
  | FormatComma Int
  | FormatExpt Int
  | FormatPrec Int
  | FormatDollar Int
  | FormatPercent Int
  | FormatNone
  deriving (Eq, Show, Generic)

-- | The official format
defaultFormatN :: FormatN
defaultFormatN = FormatComma 2

-- | make text
formatN :: FormatN -> Double -> Text
formatN (FormatFixed n) x = fixed n x
formatN (FormatDecimal n) x = decimal n x
formatN (FormatPrec n) x = prec n x
formatN (FormatComma n) x = comma n x
formatN (FormatExpt n) x = expt n x
formatN (FormatDollar n) x = dollar n x
formatN (FormatPercent n) x = percent n x
formatN FormatNone x = pack (show x)

-- | type textifier
fromFormatN :: (IsString s) => FormatN -> s
fromFormatN (FormatFixed _) = "Fixed"
fromFormatN (FormatDecimal _) = "Decimal"
fromFormatN (FormatComma _) = "Comma"
fromFormatN (FormatExpt _) = "Expt"
fromFormatN (FormatPrec _) = "Prec"
fromFormatN (FormatDollar _) = "Dollar"
fromFormatN (FormatPercent _) = "Percent"
fromFormatN FormatNone = "None"

-- | type readifier
toFormatN :: (Eq s, IsString s) => s -> Int -> FormatN
toFormatN "Fixed" n = FormatFixed n
toFormatN "Decimal" n = FormatDecimal n
toFormatN "Comma" n = FormatComma n
toFormatN "Expt" n = FormatExpt n
toFormatN "Prec" n = FormatPrec n
toFormatN "Dollar" n = FormatDollar n
toFormatN "Percent" n = FormatPercent n
toFormatN "None" _ = FormatNone
toFormatN _ _ = FormatNone

-- | to x decimal places
--
-- >>> fixed 2 1
-- "1.00"
--
-- >>> fixed 2 0.001
-- "0.00"
fixed :: Int -> Double -> Text
fixed x n = pack $ formatScientific Fixed (Just x) (fromFloatDigits n)

-- | scientific exponential
--
-- >>> expt 2 1234
-- "1.23e3"
expt :: Int -> Double -> Text
expt x n = pack $ formatScientific Exponent (Just x) (fromFloatDigits n)

-- | round to n significant figures
--
-- >>> roundSig 2 1234
-- 1230.0
--
-- >>> roundSig 2 0.001234
-- 1.23e-3
roundSig :: Int -> Double -> Scientific
roundSig n x = scientific r' (e - length ds0)
  where
    (ds, e) = toDecimalDigits $ fromFloatDigits x
    (ds0, ds1) = splitAt (n + 1) ds
    r =
      (fromIntegral $ foldl' (\x a -> x * 10 + a) 0 ds0 :: Double)
        + fromIntegral (foldl' (\x a -> x * 10 + a) 0 ds1) / (10.0 ^ (length ds1 :: Int))
    r' = round r :: Integer

-- | format numbers between 0.001 and 1,000,000 using digit and comma notation and exponential outside this range, with x significant figures.
--
-- >>> prec 1 0.00234
-- "0.0023"
--
-- >>> prec 1 0.000023
-- "2.3e-5"
--
-- >>> prec 1 123
-- "120"
--
-- >>> prec 1 123456
-- "120000"
--
-- >>> prec 1 1234567
-- "1.2e6"
prec :: Int -> Double -> Text
prec n x = case compare x zero of
  LT -> "-" <> prec n (-x)
  EQ -> "0"
  GT ->
    bool
      ( bool
          (decimal n (toRealFloat x'))
          (expt n x)
          (x > 1e6)
      )
      (expt n x)
      (x < 0.001)
  where
    x' = maybe fromFloatDigits roundSig (Just n) x

-- | round to n significant figures and always use decimal notation
--
-- >>> decimal 2 0.000001234
-- "0.00000123"
--
-- >>> decimal 2 1234567
-- "1230000"
decimal :: Int -> Double -> Text
decimal n x = x''
  where
    x' = pack $ formatScientific Fixed Nothing $ maybe fromFloatDigits roundSig (Just n) x
    x'' = (\x -> bool x' (fst x) (snd x == ".0")) $ Text.breakOn "." x'

-- | add commas format for numbers above 1,000 but below 1 million, otherwise use prec.
--
-- >>> comma 2 1234
-- "1,230"
comma :: Int -> Double -> Text
comma n x
  | x < 0 = "-" <> comma n (-x)
  | x < 1000 || x > 1e6 = prec n x
  | otherwise = addcomma (prec n x)
  where
    addcomma x = uncurry (<>) . first (Text.reverse . Text.intercalate "," . Text.chunksOf 3 . Text.reverse) . Text.breakOn "." $ x

-- | dollars and cents, always decimal notation
--
-- >>> dollar 2 1234
-- "$1,230"
--
-- >>> dollar 2 0.01234
-- "$0.0123"
dollar :: Int -> Double -> Text
dollar n x
  | x < 0 = "-" <> dollar n (-x)
  | otherwise = "$" <> comma n x

-- | fixed percent, always decimal notation
--
-- >>> percent 2 0.001234
-- "0.123%"
percent :: Int -> Double -> Text
percent n x = (<> "%") $ decimal n (100 * x)

-- | Provide formatted text for a list of numbers so that they are just distinguished.  'precision commas 2 ticks' means use as much precision as is needed for them to be distinguished, but with at least 2 significant figures.
precision :: (Int -> Double -> Text) -> Int -> [Double] -> [Text]
precision f n0 xs =
  precLoop f n0 xs
  where
    precLoop f' n xs' =
      let s = f' n <$> xs'
       in if s == nubOrd s || n > 4
            then s
            else precLoop f' (n + 1) xs'

-- | Consistently format a list of doubles.
formatNs :: FormatN -> [Double] -> [Text]
formatNs (FormatFixed n) xs = precision fixed n xs
formatNs (FormatDecimal n) xs = precision decimal n xs
formatNs (FormatPrec n) xs = precision prec n xs
formatNs (FormatComma n) xs = precision comma n xs
formatNs (FormatExpt n) xs = precision expt n xs
formatNs (FormatDollar n) xs = precision dollar n xs
formatNs (FormatPercent n) xs = precision percent n xs
formatNs FormatNone xs = pack . show <$> xs
