{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             RecordWildCards, NamedFieldPuns,
             TypeApplications, GeneralizedNewtypeDeriving #-}

-- | Copyright (c) 2021 Gavin Falconer
-- Maintainer: Gavin Falconer <gavin [at] expressivelogic [dot] net>
-- License :  BSD-3

module Main where

import           Control.Exception
import           Control.Monad (void)
import           Data.Char (isAsciiUpper)
import           Data.List (nub, sortOn)
import           Data.Maybe (catMaybes)
import qualified Data.Ord as Ord (Down(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as LBT
import           Options.Applicative
import           System.Environment (getProgName)
import qualified Text.Casing as TC
import           Text.Shakespeare.Text (ToText(..), sbt, st)

import qualified Data.GI.Base.BasicTypes as B (GType, UnexpectedNullPointerReturn)
import qualified GI.GObject.Objects.Object as GObject
import qualified GI.Vips as V

import Introspect


-- |Command line arguments
data Options = Options
  { headerFile   :: Maybe FilePath
  , genCommand   :: Command
  }

newtype Command = Command { runCommand :: [Nickname] -> IO () }

argParse :: Parser Options
argParse = Options
  <$> option (Just <$> str)
       ( long "header"
      <> short 'f'
      <> value Nothing
      <> metavar "<file>"
      <> help "Prefix the generated output with the content of <file>" )
  <*> subparser
      ( command "types" (info typesCommand ( progDesc "Generate content for Introspection/Operations.hs" ))
     <> command "ops" (info opsCommand ( progDesc "Generate content for Operations.hs" ))
     <> command "args" (info argsCommand (progDesc "Generate content for Arguments.hs"))
     <> command "results" (info resultsCommand (progDesc "Generate content for Results.hs"))
      )

typesCommand :: Parser Command
typesCommand = pure (Command printTypes)

opsCommand :: Parser Command
opsCommand = pure (Command printOps)

argsCommand :: Parser Command
argsCommand = pure (Command printArgs)

resultsCommand :: Parser Command
resultsCommand = pure (Command printResults)

main :: IO ()
main = runApp =<< execParser opts
  where
    opts = info (argParse <**> helper)
      (  fullDesc
      <> header "hvips-gen - generate hvips code by introspection of libvips" )

runApp :: Options -> IO ()
runApp options = do
  header' <- readHeader . headerFile $ options
  initVips
  version' <- V.versionString
  ops <- findOps
  T.putStrLn header'
  T.putStrLn $ versionInfo version'
  flip runCommand ops . genCommand $ options
  shutdownVips
    where
      initVips = void . V.init . T.pack =<< getProgName
      findOps = validNicknames =<< allNicknames =<< listVipsOperations
      shutdownVips = V.shutdown

versionInfo :: T.Text -> T.Text
versionInfo v =
  [sbt|--
      |-- The following code has been automatically generated using hvips-gen,
      |-- from libvips #{v}
      |--
      |]

printTypes :: [Nickname] -> IO ()
printTypes ops = sequence_ $ printType <$> ops

printOps :: [Nickname] -> IO ()
printOps ops = sequence_ $ printOp <$> ops

printArgs :: [Nickname] -> IO ()
printArgs ops = do
  args' <- uniqueArgs ops
  sequence_ $ printArg <$> args'

printResults :: [Nickname] -> IO ()
printResults ops = do
  results' <- uniqueOutputs ops
  sequence_ $ printOutput <$> results'
  sequence_ $ printOutputResultType <$> ops

readHeader :: Maybe String -> IO T.Text
readHeader Nothing = return ""
readHeader (Just f) = T.readFile f


printType :: Nickname -> IO ()
printType n = do
  args <- lookupArgs n
  T.putStrLn ""
  T.putStr $ operationType n args
  T.putStr $ operationResultType n args
  T.putStr $ operationArgs n args

printOp :: Nickname -> IO ()
printOp n = do
  args <- lookupArgs n
  description <- lookupDescription n
  T.putStrLn ""
  T.putStr $ operationFn n description args

printArg :: VipsOperationArgInfo -> IO ()
printArg a = do
  T.putStrLn ""
  T.putStr $ setArg a

printOutput :: VipsOperationArgInfo -> IO ()
printOutput a = do
  T.putStrLn ""
  T.putStr $ getOutput a

printOutputResultType :: Nickname -> IO ()
printOutputResultType n = do
  args <- lookupArgs n
  T.putStr $ outputResultType n args

lookupOp :: Nickname -> IO (Maybe Nickname)
lookupOp (Nickname n) = do
  result <- try ( do
    op <- V.operationNew n
    f <- V.operationGetFlags op
    GObject.objectUnref op
    return (Nickname n, f)
    ) :: IO (Either B.UnexpectedNullPointerReturn (Nickname, [V.OperationFlags]))
  case result of
    Left _ -> return Nothing
    Right (nick, flags)  -> return $ unlessDeprecated nick flags

lookupDescription :: Nickname -> IO T.Text
lookupDescription n = do
  op <- V.operationNew . unNickname $ n
  desc <- V.objectGetDescription op
  GObject.objectUnref op
  return desc

lookupArgs :: Nickname -> IO ArgList
lookupArgs n = do
  op <- V.operationNew . unNickname $ n
  args <- listVipsOperationArgs op
  GObject.objectUnref op
  return args

uniqueArgs :: [Nickname] -> IO ArgList
uniqueArgs = uniqueArgs' inputs

uniqueOutputs :: [Nickname] -> IO ArgList
uniqueOutputs = uniqueArgs' outputs

uniqueArgs' :: (ArgList -> ArgList) -> [Nickname] -> IO ArgList
uniqueArgs' f ns = do
  args <- mconcat $ lookupArgs <$> ns
  let ordered = OrderedArg <$> f args
  return $ fmap fromOrdered . Set.toList . Set.fromList $ ordered

newtype OrderedArg = OrderedArg { fromOrdered :: VipsOperationArgInfo }
instance Eq OrderedArg where
  x == y = name' x == name' y
    where
      name' = toArgname . fromOrdered
instance Ord OrderedArg where
  x <= y = name' x <= name' y
    where
      name' = toArgname . fromOrdered

unlessDeprecated :: Nickname -> [V.OperationFlags] -> Maybe Nickname
unlessDeprecated n f = if isDeprecated then Nothing else Just n
  where
    isDeprecated = V.OperationFlagsDeprecated `elem` f


--
-- Operation parsing
--
synonyms :: [Nickname]        -- ^synonyms for 'missing' operations to be explicitly added
synonyms = Nickname <$> ["crop"]

nicknames :: [B.GType] -> IO [Nickname]
nicknames ts = sequence $ nick <$> ts
  where
    nick n = Nickname <$> V.nicknameFind n

allNicknames :: [B.GType] -> IO [Nickname]
allNicknames ts = do
  ns <- nicknames ts
  return $ ns ++ synonyms

validNicknames :: [Nickname] -> IO [Nickname]
validNicknames ns = fmap catMaybes $ sequence $ validate <$> nub ns
  where
    validate = lookupOp

newtype Nickname = Nickname { unNickname :: T.Text }
  deriving (Eq, Ord, Show)
newtype Typename = Typename { unTypename :: T.Text }
  deriving (Eq, Ord, Show)
newtype Funcname = Funcname { unFuncname :: T.Text }
  deriving (Eq, Ord, Show)
newtype Argname = Argname { unArgname :: T.Text }
  deriving (Eq, Ord, Show)
newtype Outname = Outname { unOutname :: T.Text }
  deriving (Eq, Ord, Show)
instance ToText Nickname where
  toText = LBT.fromText . unNickname
instance ToText Typename where
  toText = LBT.fromText . unTypename
instance ToText Funcname where
  toText = LBT.fromText . unFuncname
instance ToText Argname where
  toText = LBT.fromText . unArgname
instance ToText Outname where
  toText = LBT.fromText . unOutname

toTypename :: Nickname -> Typename
toTypename = Typename . T.pack . TC.pascal . T.unpack . unNickname

toFuncname :: Nickname -> Funcname
toFuncname (Nickname "case") = Funcname "vipsCase"
toFuncname x = Funcname . T.pack . TC.camel . T.unpack . unNickname $ x

toArgname :: VipsOperationArgInfo -> Argname
toArgname VipsOperationArgInfo { name = "in", typename = "VipsArrayImage" } = Argname "imgs"
toArgname VipsOperationArgInfo { name = "in", typename = "VipsImage" } = Argname "img"
toArgname a = toArgname' . T.unpack . name $ a

toArgname' :: String -> Argname
toArgname' a@(x:_)
  | isAsciiUpper x = Argname . T.pack . (<> "_") . TC.camel $ a
  | otherwise = Argname . T.pack . (<> "'") . TC.camel $ a
toArgname' [] = Argname ""

toOutname :: VipsOperationArgInfo -> Outname
toOutname VipsOperationArgInfo { name = "out", typename = "VipsImage" } = Outname "outImg"
toOutname a = Outname $ T.pack "out" <> outname' a
  where
    outname' = T.pack . TC.pascal . T.unpack . name

toOutname' :: Nickname -> Outname
toOutname' n = Outname $ T.pack "out" <> name'
  where
    name' = unTypename . toTypename $ n

{-
Sample output:
type Invert = VipsOp "invert" GV.Image
instance HasArgument Invert "in" GV.Image
instance HasOutput Invert "out" GV.Image
or:
type System = VipsOp "system" SystemResult
data SystemResult = SystemResult { out = Maybe GV.Image, log = Maybe T.Text }
instance HasArgument System "cmd-format" T.Text
instance HasOutput System "out" GV.Image
instance HasOutput System "log" T.Text
-}

type ArgList = [VipsOperationArgInfo]

operationType :: Nickname -> ArgList -> T.Text
operationType n args =
  [sbt|type #{t} = VipsOp "#{n}" #{operationResult' n args}|]
    where
      t = toTypename n

operationArgs :: Nickname -> ArgList -> T.Text
operationArgs n args = inputs' <> outputs'
  where
    inputs' = operationInputs n args
    outputs' = operationOutputs n args

operationResultType :: Nickname -> ArgList -> T.Text
operationResultType n args = operationResultType'' (toTypename n) (outputs args)

operationResultType'' :: Typename -> ArgList -> T.Text
operationResultType'' _ [] = ""
operationResultType'' _ [_] = ""
operationResultType'' n args =
  [sbt|data #{t} = #{t} { #{fields} }|]
    where
      t = unTypename n <> "Result"
      fields = T.intercalate ", " $ operationResultField <$> args

operationResultField :: VipsOperationArgInfo -> T.Text
operationResultField a =
  [st|#{fieldname a} :: Maybe #{toHaskell (typename a)}|]
    where
      fieldname x = fmap tr $ T.unpack . name $ x
      tr '-' = '_'
      tr x = x

outputResultType :: Nickname -> ArgList -> T.Text
outputResultType n args = outputResultType' (toTypename n) (outputs args)

outputResultType' :: Typename -> ArgList -> T.Text
outputResultType' _ [] = ""
outputResultType' _ [_] = ""
outputResultType' n args =
  [sbt|
      |out#{t} :: V.VipsOp l #{t} -> V.VipsOp l #{t}
      |out#{t} = V.setOutput $ \opResult' -> do
      |  #{fields}
      |  return $ Just #{t} { #{assignFields} }
      |]
    where
      t = unTypename n <> "Result"
      fields = T.intercalate "\n|  " $ outputResultField' <$> args
      assignFields = T.intercalate ", " $ outputAssignField' <$> args

outputResultField' :: VipsOperationArgInfo -> T.Text
outputResultField' a =
  [st|#{n}' <- V.getProperty "#{n}" opResult' :: (VipsIO (Maybe #{t}))|]
    where
      n = fmap tr $ T.unpack . name $ a
      t = toHaskell (typename a)
      tr '-' = '_'
      tr x = x

outputAssignField' :: VipsOperationArgInfo -> T.Text
outputAssignField' a =
  [st|#{n} = #{n}'|]
    where
      n = fmap tr $ T.unpack . name $ a
      tr '-' = '_'
      tr x = x

operationResult' :: Nickname -> ArgList -> T.Text
operationResult' n args = operationResult'' (toTypename n) (outputs args)

operationResult'' :: Typename -> ArgList -> T.Text
operationResult'' _ [] = "()"
operationResult'' _ [x] = toHaskell . typename $ x
operationResult'' n _ = unTypename n <> "Result"

operationFn :: Nickname -> T.Text -> ArgList -> T.Text
operationFn n desc args =
  [sbt|-- |#{desc}
      |#{funcname'} :: #{params'} #{result'}
      |#{funcname'} #{params''} = vipsOp (Lookup :: Nickname "#{n}") & inputs & outputs
      |  where
      |    inputs = #{inputs'}
      |    outputs = #{output'}
      |]
    where
      funcname' = toFuncname n
      result' = toTypename n
      params' = operationFnParams' $ params args
      params'' = operationFnParams'' $ params args
      inputs' = operationFnInputs' $ params args
      output' = operationFnOutput' n $ outputs args

operationFnParams' :: ArgList -> T.Text
operationFnParams' [] = ""
operationFnParams' args = T.intercalate " -> " params' <> " -> "
  where
    params' = toHaskell . typename <$> args

operationFnParams'' :: ArgList -> T.Text
operationFnParams'' args = T.intercalate " " params'
  where
    params' = toText' <$> take count ['a'..'z']
    toText' = T.pack . (:[])
    count = length args

operationFnInputs' :: ArgList -> T.Text
operationFnInputs' [] = "id"
operationFnInputs' args = T.intercalate " . " args'
  where
    args' = map (\(x,y) -> "V." <> x <> " " <> y) $ zip argNames' paramNames'
    argNames' = unArgname . toArgname <$> args
    paramNames' = toText' <$> take count ['a' .. 'z']
    toText' = T.pack . (:[])
    count = length args

operationFnOutput' :: Nickname -> ArgList -> T.Text
operationFnOutput' n args = T.pack "V." <> operationFnOutput'' n args

operationFnOutput'' :: Nickname -> ArgList -> T.Text
operationFnOutput'' _ [] = "void"
operationFnOutput'' _ [x] = unOutname . toOutname $ x
operationFnOutput'' n _ = (<> "Result") . unOutname . toOutname' $ n

operationInputs :: Nickname -> ArgList -> T.Text
operationInputs n = operationArgs' n "HasArgument" inputs

operationOutputs :: Nickname -> ArgList -> T.Text
operationOutputs n = operationArgs' n "HasOutput" outputs

operationArgs' :: Nickname -> T.Text -> (ArgList -> ArgList) -> ArgList -> T.Text
operationArgs' n inst f args = T.intercalate "" $ arg <$> f args
  where
    arg a = [sbt|instance #{inst} #{t} "#{name a}" #{toHaskell (typename a)}#{blurb' a}|]
    t = toTypename n

blurb' :: VipsOperationArgInfo -> T.Text
blurb' a = case blurb a of
  Nothing -> ""
  Just b -> [st|		-- ^#{b}|]

params :: ArgList -> ArgList
params = sortOn (Ord.Down . priority) . filter (isParam . argumentFlags)
  where
    isParam flags = V.ArgumentFlagsInput `elem` flags &&
                    V.ArgumentFlagsRequired `elem` flags &&
                    V.ArgumentFlagsDeprecated `notElem` flags

inputs :: ArgList -> ArgList
inputs = sortOn priority . filter (isInput . argumentFlags)
  where
    isInput flags = V.ArgumentFlagsInput `elem` flags &&
                    V.ArgumentFlagsDeprecated `notElem` flags

outputs :: ArgList -> ArgList
outputs = sortOn priority . filter (isOutput . argumentFlags)
  where
    isOutput flags = V.ArgumentFlagsOutput `elem` flags &&
                     V.ArgumentFlagsDeprecated `notElem` flags

toHaskell :: String -> T.Text
toHaskell "VipsTarget" = "VipsTarget"
toHaskell "VipsSource" = "VipsSource"
toHaskell ('V' : 'i' : 'p' : 's' : restOfString) = "GV." <> T.pack restOfString
toHaskell "gint" = "Int32"
toHaskell "guint64" = "Word64"
toHaskell "gboolean" = "Bool"
toHaskell "gdouble" = "Double"
toHaskell "gchararray" = "T.Text"
toHaskell t@(_:_) = "<UNRECOGNISED TYPE: " <> T.pack t
toHaskell [] = "<EMPTY TYPE>"


setArg :: VipsOperationArgInfo -> T.Text
setArg a =
  [sbt|#{argname} :: (HasArgument a "#{name'}" #{t}) => #{t} -> a -> a
      |#{argname} = set (Set :: Argument "#{name'}")
      |]
    where
      name' = name a
      argname = toArgname a
      t = argTypename argname

argTypename :: Argname -> T.Text
argTypename (Argname "imgs") = "GV.ArrayImage"
argTypename (Argname "img") = "GV.Image"
argTypename _ = "b"

getOutput :: VipsOperationArgInfo -> T.Text
getOutput a =
  [sbt|#{resultName} :: (Result a ~ #{t}, HasOutput a "#{name'}" #{t}) => a -> a
      |#{resultName} = get (Get :: Argument "#{name'}")
      |]
    where
      name' = name a
      resultName = toOutname a
      t = toHaskell . typename $ a
