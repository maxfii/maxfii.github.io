{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad (forM_)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Digest.Pure.MD5 (md5)
import Data.List (groupBy, intercalate, isSuffixOf, sortOn)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Ord (Down(..), comparing)
import Data.Text qualified as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime, utctDay)
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Debug.Trace
import Hakyll hiding (host)
import Redirects
import System.FilePath.Posix (takeBaseName, takeDirectory, (</>), splitPath, joinPath, dropExtension)
import Text.Pandoc
import Text.Pandoc.Citeproc
import Text.Pandoc.Highlighting (pygments, styleToCss, zenburn)
import Text.Pandoc.Options
import Text.Pandoc.Walk (walkM)

import GHC.Generics(Generic)
import System.Environment.Blank (getEnv)
import System.Posix.Files (fileExist)
import Debug.Trace
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as H
import Data.String (fromString)
import Network.Wreq qualified as W
import Data.Aeson
import Data.Aeson.KeyMap qualified as JK
import Control.Lens ((^.))
import Data.Vector (Vector, fromList)
import Data.Vector qualified as V
import Data.Vector.Algorithms qualified as VAlg
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IO.Encoding qualified as E
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Ord qualified as Ord

host :: String
-- host = "https://jezenthomas.com"
host = "https://maxfii.github.io"

postsPattern :: Pattern
postsPattern = "posts/*/*/*"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
  { feedTitle = "jezenthomas.com"
  , feedDescription = "Thoughts on Haskell, Business, Unix, and more."
  , feedAuthorName = "Jezen Thomas"
  , feedAuthorEmail = "jezen@jezenthomas.com"
  , feedRoot = host
  }

copyFiles :: [Pattern]
copyFiles =
  [ "static/**"
  , "404.html"
  , "robots.txt"
  , "doom.gif"
  ]

styleSheets :: [FilePath]
styleSheets =
  [ "css/normalize.css"
  , "css/default.css"
  , "css/header.css"
  , "css/syntax.css"
  , "css/spirograph.css"
  ]

type WMPost = String

data StoredMentions = SM {
    likes :: Map WMPost (Vector Value),
    replies :: Map WMPost (Vector Value),
    reposts :: Map WMPost (Vector Value)
} deriving (Show, Generic)

instance ToJSON StoredMentions where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON StoredMentions

emptySM :: StoredMentions
emptySM = SM mempty mempty mempty

newtype GetWM = GetWM { unGetWM :: StoredMentions } deriving (Show)

instance FromJSON GetWM where
    parseJSON = withObject "webmention" $ \v -> do
        cs <- v .: "children"
        case cs of
            Array vec -> pure . GetWM $ SM (foldToMap likes) (foldToMap replies) (foldToMap reposts) where

                foldToMap objs = Map.fromList $ foldl go mempty (V.groupBy gb objs) where
                    -- go :: [(WMPost, Vector Value)] -> Vector Value -> [(WMPost, Vector Value)]
                    go wmmap ls
                        | V.null ls = wmmap
                        | otherwise = case V.head ls of
                            Object o -> case JK.lookup "wm-target" o of
                                Just (String t) -> (drop 8 $ T.unpack t, ls) : wmmap
                                _ -> wmmap
                            _ -> wmmap

                gb (Object kmap1) (Object kmap2) = fromMaybe False $ (==)
                    <$> JK.lookup "wm-target" kmap1
                    <*> JK.lookup "wm-target" kmap2
                gb _ _ = False

                (likes, rest) = V.partition (isType "like-of") vec
                (replies, reposts) = V.partition (isType "in-reply-to") rest

                isType type_ (Object kmap) = maybe False (type_ ==) (JK.lookup "wm-property" kmap)
                isType _ _ = False

            _ -> fail "failed to parse chidren as an array"


renderRepost (Object kmap) = do
  Object author <- JK.lookup "author" kmap
  String authorPhotoUrl <- JK.lookup "photo" author
  String authorUrl <- JK.lookup "url" author
  pure . renderHtml $ H.img ! H.src (fromString $ T.unpack $ authorPhotoUrl) ! H.alt "like image"

renderReply (Object kmap) = do
  String (T.unpack -> url) <- JK.lookup "url" kmap
  String (T.unpack -> published) <- JK.lookup "published" kmap
  Object author <- JK.lookup "author" kmap
  Object content <- JK.lookup "content" kmap
  String (T.unpack -> chtml) <- JK.lookup "html" content
  String (T.unpack -> ctext) <- JK.lookup "text" content
  String (T.unpack -> authorPhotoUrl) <- JK.lookup "photo" author
  String (T.unpack -> authorName) <- JK.lookup "name" author
  String (T.unpack -> authorUrl) <- JK.lookup "url" author
  pure . renderHtml $
      H.div ! H.class_ "mention" $ do
          H.a ! H.class_ "mention__authorImageLink" ! H.href (fromString url) $
              H.img ! H.class_ "mention_authorLink" ! H.src (fromString authorPhotoUrl) ! H.alt (fromString authorName)
          H.div ! H.class_ "mention__authorLink" $ do
              H.strong (H.a ! H.href (fromString authorUrl) $ (fromString authorName))
              H.div ! H.class_ "mention__content" $ fromString chtml
              H.small $ H.a ! H.href (fromString url) $ fromString published

renderLike (Object kmap) = do
  String (T.unpack -> url) <- JK.lookup "url" kmap
  Object author <- JK.lookup "author" kmap
  String authorPhotoUrl <- JK.lookup "photo" author
  String (T.unpack -> authorPhotoUrl) <- JK.lookup "photo" author
  String (T.unpack -> authorName) <- JK.lookup "name" author
  pure . renderHtml $ H.a ! H.class_ "like" ! H.href (fromString url) $
      H.img ! H.class_ "like__image" ! H.src (fromString authorPhotoUrl) ! H.alt (fromString authorName)

main = do
    E.setLocaleEncoding E.utf8
    hmain

hmain :: IO ()
hmain = hakyll $ do


  newMentions <- preprocess $ do
    webementionIoToken <- getEnv "WMTOKEN"
    putStrLn $ "Warning: toker is this" <> show webementionIoToken
    case webementionIoToken of
      Nothing -> do
        putStrLn $ "Warning: no webmention.io token found"
        putStrLn $ "Warning: please set WMToken environmental variable"
        pure emptySM
      Just token -> do

        response <- W.get $ "https://webmention.io/api/mentions.jf2?domain=maxfii.github.io&token=" <> token

        case response ^. W.responseStatus . W.statusCode of
          200 -> case fmap unGetWM . eitherDecode $ response ^. W.responseBody of
              Right sm@(SM _ _ _) -> pure sm
              Left err -> do
                putStrLn $ "Error: decoding webmentions. aeson error: "
                    <> show err
                pure $ SM mempty mempty mempty

          bad -> do
            putStrLn $ "Error: fetching webmentions. status code: " <> show bad
            pure emptySM

  mentions <- preprocess $ fileExist "webmentions.json" >>= \case
    True -> do
      SM oldLikes oldReplies oldReposts <- fromMaybe emptySM . decodeStrict <$> B.readFile "webmentions.json"

      let SM newLikes newReplies newReposts = newMentions
          result = SM (merge newLikes oldLikes) (merge newReplies oldReplies)
            (merge oldReposts oldReposts)

          merge = Map.unionWith checkIds

          checkIds :: Vector Value -> Vector Value -> Vector Value
          checkIds vec1 vec2 = VAlg.nubBy comparison (vec1 <> vec2)

          comparison v1@(Object o1) v2@(Object o2) =
            fromMaybe (Ord.compare (encode v1) (encode v2)) $
                Ord.compare <$> JK.lookup "wm-id" o1 <*> JK.lookup "wm-id" o2

          comparison o1 o2 = (Ord.compare (encode o1) (encode o2))

      encodeFile "webmentions.json" result
      pure result

    False -> do
      encodeFile "webmentions.json" newMentions
      pure newMentions

  compiledStylesheetPath <- preprocess $ do
    styles <- mapM readFile styleSheets
    let h = md5 $ fromStrict $ pack $ compressCss $ mconcat styles
    pure $ "css/" <> show h <> ".css"

  let cssPathCtx = constField "cssPath" compiledStylesheetPath

  forM_ copyFiles $ \ptrn ->
    match ptrn $ do
      route idRoute
      compile copyFileCompiler

  match "css/*" $ route idRoute >> compile compressCssCompiler

  create [fromFilePath compiledStylesheetPath] $ do
    route idRoute
    compile $ do
      styles <- mapM (load . fromFilePath) styleSheets
      let ctx = listField "styles" pageCtx (pure styles)
      makeItem ""
        >>= loadAndApplyTemplate "templates/empty.html" ctx

  let postCtx =  postSlugField "slug"
              <> pageCtx
              <> cssPathCtx

  let utcCtx = field "utcOrdinal" getItemUTCOrdinal
            <> field "utcDay" getItemUTCDay
            <> field "utcMonth" getItemUTCMonth
            <> field "utcYear" getItemUTCYear

  postsMeta <- getAllMetadata "posts/*/*/*"

  version "redirects" $ createRedirects redirects

  match "posts/*/*/*" $ do
    route postCleanRoute
    dep <- makePatternDependency "css/*"
    rulesExtraDependencies [dep] $ compile $ do
      ident <- getUnderlying

      let wmreplies = listField "replies" (field "repl" (return . itemBody))
            (traverse (makeItem . fromMaybe "" . renderReply)
                (V.toList . fromMaybe V.empty . Map.lookup target $ replies mentions))

          wmlikes = listField "likes" (field "like" (return . itemBody))
            (traverse (makeItem . fromMaybe "" . renderLike)
                (V.toList . fromMaybe V.empty . Map.lookup target $ likes mentions))

          wmreposts = listField "reposts" (field "repo" (return . itemBody))
            (traverse (makeItem . fromMaybe "" . renderRepost)
                (V.toList . fromMaybe V.empty . Map.lookup target $ reposts mentions))

          ctx = postCtx <> utcCtx <> wmreplies <> wmlikes
          cleanTarget = dropExtension . joinPath . drop 1 . splitPath . toFilePath
          target = drop 8 host </> cleanTarget ident <> "/"

          count = fmap $ foldl (\acc _ -> acc+1) 0

      -- debugCompiler $ "LOOK: reading mentions: " <> (show . Map.lookup "maxfii.github.io/2023/11/stubbing-io-in-yesod/" $ replies mentions)
      if target == "maxfii.github.io/2023/11/stubbing-io-in-yesod/"
        then do
            debugCompiler $ "LOOK: counting replies: " <> (show . count . Map.lookup target $ replies mentions)
            debugCompiler $ "LOOK: counting likes: " <> (show . count . Map.lookup target $ likes mentions)
            debugCompiler $ "LOOK: counting reposts: " <> (show . count . Map.lookup target $ reposts mentions)
        else pure ()
      debugCompiler $ "LOOK: reading target: " <> target


      blogCompiler
        >>= loadAndApplyTemplate "templates/post-content.html" postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" (postCtx <> boolField "page-blog" (const True))
        >>= cleanIndexUrls

  match "index.html" $ do
    route idRoute
    dep <- makePatternDependency "css/*"
    rulesExtraDependencies [dep] $ compile $ do
      let ctx =  constField "title" "Jezen Thomas | Haskell, Unix, Minimalism, and Entrepreneurship."
              <> postCtx
              <> boolField "page-home" (const True)
              <> cssPathCtx

      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  create ["about/index.html"] $ do
    route idRoute
    compile $ makeItem $ Redirect "/"

  create ["posts/index.html"] $ do
    route idRoute
    dep <- makePatternDependency "css/*"
    rulesExtraDependencies [dep] $ compile $ do
      posts <- recentFirst =<< loadAll "posts/*/*/*"
      let ctx =  constField "title" "All Posts | Jezen Thomas"
              <> boolField "page-blog" (const True)
              <> publishedGroupField "years" posts (postCtx <> utcCtx)
              <> cssPathCtx
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" ctx
        >>= loadAndApplyTemplate "templates/post-content.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= cleanIndexUrls

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*/*/*"

      let allPosts = return posts
      let sitemapCtx = mconcat
                       [ listField "entries" pageCtx allPosts
                       , constField "host" host
                       , defaultContext
                       ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
        >>= cleanIndexHtmls

  create ["feed.xml"] $ do
         route   idRoute
         compile $ do
           let feedCtx = pageCtx <> bodyField "description"
           posts <- fmap (take 10) . recentFirst =<<
                   loadAllSnapshots "posts/*/*/*" "content"
           renderRss myFeedConfiguration feedCtx posts
             >>= cleanIndexHtmls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
pageCtx :: Context String
pageCtx = mconcat
  [ modificationTimeField "mtime" "%U"
  , constField "host" host
  , dateField "date" "%B %e, %Y"
  , defaultContext
  ]

blogCompiler :: Compiler (Item String)
blogCompiler =
  pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    pygmentsHighlight
  where
  pygmentsHighlight :: Pandoc -> Compiler Pandoc
  pygmentsHighlight = walkM \case
    CodeBlock (_, listToMaybe -> mbLang, _) (T.unpack -> body) -> do
      let lang = T.unpack (fromMaybe "text" mbLang)
      RawBlock "html" . T.pack <$> callPygs lang body
    block -> pure block
  callPygs :: String -> String -> Compiler String
  callPygs lang = unixFilter "pygmentize" [ "-l", lang, "-f", "html" ]

-- custom routes
--------------------------------------------------------------------------------
postCleanRoute :: Routes
postCleanRoute = cleanRoute
 `composeRoutes` gsubRoute "(posts|drafts)/" (const "")

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      takeDirectory p </> takeBaseName p </> "index.html"
        where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll ptrn replacement)
  where
    ptrn = "/index.html"
    replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

-- utils
--------------------------------------------------------------------------------
postSlugField :: String -> Context a
postSlugField key = field key $ return . baseName
  where baseName = takeBaseName . toFilePath . itemIdentifier

publishedGroupField ::
     String           -- name
  -> [Item String]    -- posts
  -> Context String   -- Post context
  -> Context String   -- output context
publishedGroupField name posts postContext = listField name groupCtx $ do
    traverse extractTime posts
      >>= mapM makeItem . fmap merge . groupByYear
    where
      groupCtx = field "year" (return . show . getYear . fst . itemBody)
                  <> listFieldWith "posts" postContext (return . snd . itemBody)

      merge :: [(UTCTime, Item a)] -> (UTCTime, [Item a])
      merge gs = (fst (head gs), snd <$> sortByTime gs)

      groupByYear :: [(UTCTime, Item a)] -> [[(UTCTime, Item a)]]
      groupByYear = groupBy (\(a, _) (b, _) -> getYear a == getYear b)

      sortByTime :: [(UTCTime, a)] -> [(UTCTime, a)]
      sortByTime = sortOn (Down . fst)

      getYear :: UTCTime -> Integer
      getYear time = year
        where (year, _, _) = (toGregorian . utctDay) time

      extractTime :: Item a -> Compiler (UTCTime, Item a)
      extractTime item = getItemUTC defaultTimeLocale (itemIdentifier item)
        >>= \time -> pure (time, item)

getItemUTCDay :: Item String -> Compiler String
getItemUTCDay item = do
  utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
  pure $ formatTime defaultTimeLocale "%e" utc

getItemUTCMonth :: Item String -> Compiler String
getItemUTCMonth item = do
  utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
  pure $ formatTime defaultTimeLocale "%B" utc

getItemUTCYear :: Item String -> Compiler String
getItemUTCYear item = do
  utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
  pure $ formatTime defaultTimeLocale "%Y" utc

getItemUTCOrdinal :: Item String -> Compiler String
getItemUTCOrdinal item = do
  utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
  pure $ ordinalSuffix utc
  where
  ordinalSuffix :: UTCTime -> String
  ordinalSuffix t =
    let dayOfMonth = (\(_, _, a) -> a) (toGregorian (utctDay t))
        suffix n
          | n `elem` [1,21,31] = "st"
          | n `elem` [2,22]    = "nd"
          | n `elem` [3,23]    = "rd"
          | otherwise          = "th"
    in suffix dayOfMonth
