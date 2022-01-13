formatn
===

[![Build
Status](https://travis-ci.org/tonyday567/formatn.svg)](https://travis-ci.org/tonyday567/formatn)
[![Hackage](https://img.shields.io/hackage/v/formatn.svg)](https://hackage.haskell.org/package/formatn)

This package provides support for common number formatting patterns.

Usage
===
    
``` haskell
-- >>> import Data.FormatN
-- >>> xs = [(-1),0,1,1.01,1.02,1.1,1.2]
-- >>> fixed (Just 2) <$> xs
-- ["-1.00","0.00","1.00","1.01","1.02","1.10","1.20"]
--
-- >>> comma (Just 3) <$> xs
-- ["-1.00","0.00","1.00","1.01","1.02","1.10","1.20"]
--
-- >>> comma (Just 3) . (1e3*) <$> xs
-- ["-1,000","0.00","1,000","1,010","1,020","1,100","1,200"]
```

