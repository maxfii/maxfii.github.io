{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Typst.Regex
  ( RE (..),
    RegexMatch (..),
    replaceRegex,
    splitRegex,
    makeLiteralRE,
    makeRE,
    match,
    matchAll,
    -- re-export
    extract,
  )
where

import qualified Data.Array as Array
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Text.Regex.TDFA (Regex, extract)
import qualified Text.Regex.TDFA as TDFA
import qualified Text.Regex.TDFA.Text as TDFA

-- import Debug.Trace

-- | A regular expression. Note that typst-hs does not use the same Regex engine
-- as Typst. See issue [#28](https://github.com/jgm/typst-hs/issues/28).
data RE = RE !Text !Regex
  deriving (Typeable)

instance Eq RE where
  RE t1 _ == RE t2 _ = t1 == t2

instance Ord RE where
  compare (RE t1 _) (RE t2 _) = compare t1 t2

instance Show RE where
  show (RE t _) = "/" <> T.unpack t <> "/"

data RegexMatch = RegexMatch
  { matchStart :: Int,
    matchEnd :: Int,
    matchText :: Text,
    matchCaptures :: [Text]
  }
  deriving (Eq, Ord, Typeable)

replaceRegex :: RE -> Maybe Int -> (RegexMatch -> Text) -> Text -> Text
replaceRegex (RE _ re) mbCount replaceFn strIn =
  let matches = maybe id take mbCount $ TDFA.matchAll re strIn
      getCaptures m =
        map
          (\(off, len) -> extract (off, len) strIn)
          (drop 1 (Array.elems m))
      go i [] = T.drop i strIn
      go i (m : rest) =
        seq i $
          let (off, len) = m Array.! 0
           in ( if off > i
                  then slice i (off - i) strIn
                  else mempty
              )
                <> replaceFn
                  RegexMatch
                    { matchStart = off,
                      matchEnd = off + len,
                      matchText = extract (off, len) strIn,
                      matchCaptures = getCaptures m
                    }
                <> go (off + len) rest
      slice pos len = T.take len . T.drop pos
   in go 0 matches

makeRE :: MonadFail m => Text -> m RE
makeRE t =
  RE t'
    <$> either
      fail
      pure
      (TDFA.compile compopts TDFA.defaultExecOpt t')
  where
    (caseSensitive, t') =
      if "(?i)" `T.isPrefixOf` t
        then (False, T.pack . go False . T.unpack $ T.drop 4 t)
        else (True, T.pack . go False . T.unpack $ t)
    compopts = TDFA.defaultCompOpt {TDFA.caseSensitive = caseSensitive}

    -- Handle things not supported in TFFA posix regexes, e.g. \d \w \s, +, ?
    -- Note that we have to track whether we're in a character class, because
    -- the expansions will be different in that case.  The first
    -- parameter of `go` is True if in a character class.
    go _ [] = []
    go True (']' : cs) = ']' : go False cs
    go False ('[' : cs) = '[' :
      case cs of
        '^':']':ds -> '^' : ']' : go True ds
        '^':'\\':']':ds -> '^' : ']' : go True ds
        ']':ds -> ']' : go True ds
        '\\':']':ds -> ']' : go True ds
        _ -> go True cs
    go False ('?' : cs) = "{0,1}" ++ go False cs
    go False ('+' : cs) = "{1,}" ++ go False cs
    go inCharClass ('\\' : c : cs)
      = let f = if inCharClass
                   then id
                   else \x -> "[" ++ x ++ "]"
            r = case c of
                  'd' -> f "[:digit:]"
                  'D' -> f "^[:digit:]"
                  's' -> f "[:space:]"
                  'S' -> f "^[:space:]"
                  'w' -> f "[:word:]"
                  'W' -> f "^[:word:]"
                  _ -> ['\\', c]
        in r ++ go inCharClass cs
    go inCharClass (c : cs) = c : go inCharClass cs

match :: TDFA.RegexContext Regex source target => RE -> source -> target
match (RE _ re) t = TDFA.match re t

matchAll :: TDFA.RegexLike Regex source => RE -> source -> [TDFA.MatchArray]
matchAll (RE _ re) t = TDFA.matchAll re t

makeLiteralRE :: MonadFail m => Text -> m RE
makeLiteralRE t
  | T.null t = makeRE ".{0,0}" -- experimentally behaves as typst does
  | otherwise = makeRE $ T.foldl go mempty t
  where
    go acc c = if isSpecial c then acc <> T.pack ['\\', c] else T.snoc acc c
    isSpecial c = c `elem` (".*?+(){}[]|\\^$" :: [Char])

-- from regex-compat but for Text
splitRegex :: RE -> Text -> [Text]
splitRegex (RE _ delim) strIn =
  let matches = map (Array.! 0) (TDFA.matchAll delim strIn)
      go _i str [] = str : []
      go i str ((off, len) : rest) =
        let i' = off + len
            firstline = T.take (off - i) str
            remainder = T.drop (i' - i) str
         in seq i' $
              if T.null remainder
                then [firstline, ""]
                else firstline : go i' remainder rest
   in go 0 strIn matches
