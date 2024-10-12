
# formatn 

[![img](https://img.shields.io/hackage/v/formatn.svg)](https://hackage.haskell.org/package/formatn) [![img](https://github.com/tonyday567/formatn/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/formatn/actions?query=workflow%3Ahaskell-ci)

A library for support of:

-   significant figure rounding of numbers, and
-   common number formatting patterns.

<a id="org95ded84"></a>

# Usage

    :reload
    :set prompt "> "
    
    :set -Wno-type-defaults
    :set -XOverloadedStrings
    import Data.FormatN
    
    xs = [(-1),0,0.1,1,1.01,1.02,1.1,1.2]
    
    fixed (Just 2) <$> xs
    decimal (Just 2) <$> xs
    formats False False (const DecimalStyle) (Just 2) xs
    formats False True (const DecimalStyle) (Just 2) xs
    distinguish 4 False True (const DecimalStyle) (Just 2) xs

    Ok, 14 modules loaded.
    > >
    >
    > ["-1.00","0.00","0.10","1.00","1.01","1.02","1.10","1.20"]
    ["-1.0","0.0","0.10","1.0","1.0","1.0","1.1","1.2"]
    ["-1.00","0.00","0.10","1.00","1.00","1.00","1.10","1.20"]
    ["-1.0","0.0","0.1","1.0","1.0","1.0","1.1","1.2"]
    ["-1.00","0.00","0.10","1.00","1.01","1.02","1.10","1.20"]


<a id="orgb82f98d"></a>

# getting 0 right


<a id="orgaf42992"></a>

## fixed

    fixed (Just 3) <$> [0, 0.5, 1.5, 2.0]

    ["0.000","0.500","1.500","2.000"]

fixed defaults to representing Doubles in the same manner as Haskell does ie with a trailing &rsquo;.0&rsquo;

    fixed Nothing <$> [0, 0.5, 1.5, 2]

    ["0.0","0.5","1.5","2.0"]


<a id="org1b53172"></a>

## expt

    expt Nothing 0

    0e0

    expt (Just 2) 1
    expt (Just 2) 0

    1.0e0
    0.0e0


<a id="org90e32fb"></a>

## decimal

    decimal Nothing 0
    decimal (Just 1) 0
    decimal (Just 3) 0

    0
    0
    0.00


<a id="orgbe4b502"></a>

## percent

percent is a little weird any way you cut it.

    percent decimalSF (Just 4) 0
    percent decimalSF (Just 4) 1

    0.0%
    100.0%


<a id="org549dfe5"></a>

## inclusion in lists

    formats True False commaPrecStyle Nothing [0, 0.5, 1.5, 2]
    formats True False commaPrecStyle (Just 1) [0, 1, 10]
    formats True False commaPrecStyle (Just 1) [0, 0.5, 1.5, 2]
    formats True False commaPrecStyle (Just 2) [0, 0.5, 1.5, 2]
    formats True True commaPrecStyle (Just 6) [0, 0.5, 1.5, 2]

    ["0.0","0.5","1.5","2.0"]
    [" 0"," 1","10"]
    ["0.0","0.5","2.0","2.0"]
    ["0.00","0.50","1.50","2.00"]
    ["0.0","0.5","1.5","2.0"]

