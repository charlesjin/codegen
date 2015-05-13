-- param module
-- builds an optimal set of params
-- given the input of a param

module Param where

  import Data.List
  import Data.List.Split

  data Params = Params {base :: Int, 
                        offset :: Int, 
                        sign :: Sign, 
                        rep :: [Int],
                        len :: Int,
                        opt :: Bool}

  data Sign = Positive | Negative

-- the prime is encoded as 2^base + sign * offeset
-- rep is a least-to-most significant breakdown of how to represent
-- elements in the field (this might be the hardest part)
-- length is just the number of elements in rep
--
-- we assume that the sign is always negative for now

  genParams :: String -> String -> String -> String -> Params
  genParams b o r f =
    let r'  = map read (splitOn " " r)
        l   = length r'
        f'  = if (isInfixOf "--unopt" f || isInfixOf "-u" f)
              then False
              else True
    in Params { base=read b,
                offset=read o,
                sign=Negative,
                rep=r',
                len=l,
                opt=f' }

  genSampleParams :: String -> Params
  genSampleParams s = 
    let o   = if (isInfixOf "--unopt" s || isInfixOf "-u" s)
              then False
              else True
        w   = if (isInfixOf "--weird" s || isInfixOf "-w" s)
              then True
              else False
    in if (isInfixOf "25519" s)
       then gen25519 o w
       else (if isInfixOf "7615" s
             then gen7615 o w
             else (if isInfixOf "1271" s
                   then gen1271 o w
                   else error "Sorry, do not recognize that input."))

  gen25519 :: Bool -> Bool -> Params
  gen25519 o w =
    Params {  base=255,
              offset=19,
              sign=Negative,
              rep=[26,25,26,25,26,25,26,25,26,25],
              len=10,
              opt=o }

  gen7615 :: Bool -> Bool -> Params
  gen7615 o w =
    let r = if w
            then [26,26,24]
            else [26,25,25]
    in Params { base=76,
                offset=15,
                sign=Negative,
                rep=r,
                len=3,
                opt=o }

  gen1271 :: Bool -> Bool -> Params
  gen1271 o w =
    let r = if w
            then [26,26,26,25,24]
            else [26,25,26,25,25]
    in Params { base=127,
                offset=1,
                sign=Negative,
                rep=[26,26,26,25,24],
                len=5,
                opt=o }



