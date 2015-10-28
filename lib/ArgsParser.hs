module ArgsParser (Action(..), classify) where

import           Control.Applicative                                ((<|>), liftA2)
import           Data.Maybe                                         (fromMaybe)

data Action
  -- | Invoke a distributed compilation with input, output, and other arguments.
  = DistributedCompilation String String [String]
  -- | Invoke local compiler with the provided arguments.
  | InvokeLocal [String]
  deriving (Eq, Show)

data ArgumentType
  -- | The argument might be a key, e.g. "-o", with "o" stored in the constructor.
  = Key String
  -- | The argument is not a key but a value to some key or standalone.
  | Value
  deriving (Eq, Ord, Show)

type Args = [(ArgumentType, String)]

-- | Classify the provided arguments into either a distributed computation,
--   or a computation evaluated locally.
classify :: [String] -> Action
classify []   = InvokeLocal []
classify args = fromMaybe (InvokeLocal args) (constructDistributed (parse args))

-- | Try to construct the necessary parameters of a distributed computation,
--   given a list of arguments.
--
--   Requires the following to be present:
--   * -c
--   * -o output_filename
--   * input filename appearing after stripping the arguments above, in either 
--     leading or trailing position of the argument list.
--     It should not have a leading "-".
--   
--   Will return the processed action with an argument list stripped of the flags above.
constructDistributed :: Args -> Maybe (Action)
constructDistributed args = do
  -- strip compilation flag
  (_, args')            <- extractPredicates [matchFlag "c"] args

  -- locate output file
  ([_, (_, output)], args'') <- extractPredicates [matchFlag "o", matchVal] args'

  -- extract input filename as a Value field in either leading or trailing position
  (input, processedArgs) <- consider args'' <|> (do
    (x, xs) <- consider (reverse args'')
    return $ (x, reverse xs)
    )

  return $ DistributedCompilation input output (map snd processedArgs)
  where
  matchFlag ref = \(kind, _) -> kind == Key ref
  matchVal      = \(kind, val) -> kind == Value

  consider ((Value, val):xs) = Just (val, xs)
  consider _                 = Nothing

-- | Try to extract a subsequence as indicated by the element-wise predicates.
--   Will return Nothing if the whole sequence was not found somewhere.
--   Assumes that a partial match indicates the start of a sequence.
extractPredicates :: [(a -> Bool)] -> [a] -> Maybe ([a], [a])
extractPredicates [] xs = Just ([], xs)
extractPredicates _ []  = Nothing
extractPredicates (p:ps) (x:xs)
  | p x       = fmap (\(sub, other) -> (x:sub, other)) (extractPredicates ps xs)
  | otherwise = fmap (\(sub, other) -> (sub, x:other)) (extractPredicates (p:ps) xs)

-- | Parse a list of arguments into tagged representation.
parse :: [String] -> Args
parse []                  = []
parse (f@('-':'-':flag):xs) = (Key flag, f)  : parse xs
parse (f@('-':flag):xs)     = (Key flag, f)  : parse xs
parse (val:xs)            = (Value, val) : parse xs
