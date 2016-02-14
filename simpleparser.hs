module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val


spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x

showIndented :: Int -> LispVal -> String
showIndented x (List contents) = let spcs  = take x $ repeat ' ' 
                                     conts = map ( showIndented (x + 2) ) contents
                                 in
                                  "\n" ++ spcs ++ "(" ++ unwords conts ++ "\n" ++ spcs ++ ")"
showIndented x (DottedList head tail) = let spcs = take x $ repeat ' '
                                            hd = map (showIndented (x + 2)) head
                                            tl = showIndented (x + 2) tail
                                 in
                                  "\n" ++ spcs ++ "(" ++ unwords hd ++ "\n" ++ spcs ++ " . " ++ tl ++ ")"
showIndented x y = showVal y


showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unpackNum :: LispVal -> Maybe Integer
unpackNum (Number n) = Just n
unpackNum _ = Nothing

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b
unpackBool _ = False 

unpackStr :: LispVal -> String
unpackStr (String s) = s
unpackStr _ = ""
 
getNumBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
getNumBinop op params = Number $ foldl1 op $ map unpackNum params

getBoolBinop :: (LispVal -> a) -> (a -> a -> Bool) -> [LispVal] -> LispVal
getBoolBinop unpacker op args = Bool $ left `op` right
                                where   left = unpacker $ args !! 0
                                        right = unpacker $ args !! 1 

-- Get boolean operations from operator list
numBoolBinop = getBoolBinop unpackNum
strBoolBinop  = getBoolBinop unpackStr
boolBoolBinop = getBoolBinop unpackBool



apply :: String -> [LispVal] -> LispVal
apply func args = case lookup func operators of 
                        Nothing -> Bool False
                        Just f -> f args

-- Alternative version of the above.
-- apply func args = maybe (Bool False) ($ args) $ lookup func operators 


instance Show LispVal where show = showIndented 0 

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(DottedList _ _) = val
eval val@(Atom _) = val
eval (DottedList x (List y)) = eval (List (x ++ y))
eval (DottedList x (DottedList y z)) = eval (DottedList (x ++ y) z)
eval (List [Atom "quote", val]) = val
eval (List [Atom "if", pred, conseq, alt]) = 
        case eval pred of 
             Bool False -> eval alt
             Bool True  -> eval conseq
-- Evaluate a list of arguments recursively and apply the operator 
-- to each list item in the sub lists. "map" runs eval on each element in the list "args".
eval (List (Atom func : args )) = apply func $ map eval args
eval val@(List _) = val

car :: [LispVal] -> LispVal
car [List (x : xs)] = x
car [DottedList (x : xs) _] = x

cdr :: [LispVal] -> LispVal
cdr [List (x : xs)] = List xs
cdr [DottedList [_] x] = x
cdr [DottedList (_ : xs) x] = DottedList xs x

cons :: [LispVal] -> LispVal
cons [x1, List []] = List [x1]
cons [x, List xs] = List $ x : xs
cons [x, DottedList xs xlast] = DottedList (x : xs) xlast
cons [x1, x2] = DottedList [x1] x2


operators = [ ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("-",getNumBinop(-)),
              ("+",getNumBinop(+)),
              ("*",getNumBinop(*)),
              ("/",getNumBinop div),
              ("mod", getNumBinop mod),
              ("quotient", getNumBinop quot),
              ("remainder", getNumBinop rem),
              ("car",car),
              ("cdr",cdr),
              ("cons", cons)]


-- Old function for evaluating 
-- eval (List (Atom a : x : y : [])) = (case lookup a operators of
--                                       Nothing -> Bool False 
--                                    Just a -> Number (a (getLispNum $ eval x) (getLispNum $ eval y)))



