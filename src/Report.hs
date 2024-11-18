{-# LANGUAGE RecordWildCards, TupleSections #-}

module Report(
    reportText,
    reportHTML
    ) where

import Data.List.Extra
import Data.Tree
import Data.Char
import Data.Hashable
import qualified Data.Set as Set
import Numeric.Extra
import Util
import Type


presort :: [Tree Val] -> [Tree Val]
presort =
    sortOn (negate . timeInh . rootLabel) .
    fmapForest (sortOn (negate . timeTot . rootLabel))


reportHTML :: [Tree Val] -> [String]
reportHTML vals =
    let vals2 = presort vals
        links = Set.fromList $ map (name . rootLabel) vals2
        anchor x = "<a id='" ++ show (hash x) ++ "'></a>"
    in ((css ++ ["<pre>"]) ++) $ (++ ["</pre>"]) $ intercalate [""] $
        (" TOT   INH   IND" : map (showHTML links . (0,) . rootLabel) (take 25 vals2)) :
        [ anchor (name $ rootLabel x) :
          map (showHTML $ Set.delete (name $ rootLabel x) links) (flatten $ fmapTreeDepth (,) x)
        | x <- vals2]

showHTML :: Set.Set String -> (Int, Val) -> String
showHTML xs (indent, Val{..}) =
    (col $ intercalate "  " [showDouble timeTot, showDouble timeInh, showDouble timeInd]) ++
    "  " ++
    spc ++ (if name `Set.member` xs then link else colouredName) ++
    col (" (" ++ show entries ++ ")")
    where
        link = "<a href='#" ++ show (hash name) ++ "'>" ++ colouredName ++ "</a>"
        colouredName = col name
        col = colourText indent
        spc = replicate (indent * 2) ' '

colourText :: Int -> String -> String
colourText i text =
    "<span class=\"" ++ cssColourClass i ++ "\">" ++ text ++ "</span>"

css :: [String]
css =
    ["<style>", "body {background-color: white;}"] ++
    (defineColourClass <$> [0 .. noCssColours - 1]) ++
    ["</style>"]
    where
    defineColourClass i =
        '.' : cssColourClass i ++ " {color: " ++ cssColour i ++ ";}"

cssColourClass :: Int -> String
cssColourClass i = "c" ++ show (rem i noCssColours)

noCssColours :: Int
noCssColours = 4

cssColour :: Int -> String
cssColour 0 = "black"
cssColour 1 = "#e6194B"
cssColour 2 = "#4363d8"
cssColour 3 = "#f58231"

reportText :: [Tree Val] -> [String]
reportText vals =
    let vals2 = presort vals
        indent i x = x{name = replicate (i*2) ' ' ++ name x}
    in intercalate ["",""] $
        (" TOT   INH   IND" : map (showText . rootLabel) (take 25 vals2)) :
        [map showText $ flatten $ fmapTreeDepth indent x | x <- vals2]

showText :: Val -> String
showText Val{..} = intercalate "  "
    [showDouble timeTot, showDouble timeInh, showDouble timeInd, name ++ " (" ++ show entries ++ ")"]

showDouble :: Double -> String
showDouble x = case showDP 1 x of
    "0.0" -> "   -"
    "100.0" -> "99.9" -- avoid making the column bigger for a corner case
    ['0','.',x] -> [' ',' ','.',x]
    x -> replicate (4 - length x) ' ' ++ x
