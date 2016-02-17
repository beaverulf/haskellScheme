module Main where
import Control.Monad
import Control.Monad.Error
import System.IO
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> evalAndPrint $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"

-- Defining error handling
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- IMPLEMENTING REPL.
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val


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

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "bool" $ String n
                             else return $ fst $ parsed !! 0
unpackBool (List [n]) = unpackBool n
unpackBool notBool     = throwError $ TypeMismatch "bool" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Bool s) = return $ show s
unpackStr (Number s) = return $ show s
unpackStr (List [n]) = unpackStr n
unpackStr notStr     = throwError $ TypeMismatch "string" notStr

 
getNumBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
getNumBinop op           []  = throwError $ NumArgs 2 []
getNumBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
getNumBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
-- getNumBinop op params = return $ Number $ foldl1 op $ mapM unpackNum params

getBoolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
getBoolBinop unpacker op args = do left <- unpacker $ args !! 0
                                   right <- unpacker $ args !! 1 
                                   return $ Bool $ left `op` right
--getBoolBinop unpacker op args = return $ Bool $ left `op` right
--                                where   left = unpacker $ args !! 0
--                                        right = unpacker $ args !! 1 

-- Get boolean operations from operator list
numBoolBinop = getBoolBinop unpackNum
strBoolBinop  = getBoolBinop unpackStr
boolBoolBinop = getBoolBinop unpackBool

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = case lookup func operators of
                       Just p -> p args

-- Alternative version of the above.
-- apply func args = maybe (Bool False) ($ args) $ lookup func operators 

instance Show LispVal where show = showIndented 0 

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (DottedList x (List y)) = eval (List (x ++ y))
eval (DottedList x (DottedList y z)) = eval (DottedList (x ++ y) z)
eval val@(DottedList _ _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
    do p <- eval pred
       case p of 
             Bool False -> eval alt
             otherwise -> eval conseq
-- Evaluate a list of arguments recursively and apply the operator 
-- to each list item in the sub lists. "map" runs eval on each element in the list "args".
--eval (List (Atom func : args )) = apply func $ mapM eval args
eval (List (Atom func : args )) = mapM eval args >>= apply func
eval val@(List _) = return val
--eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return $ x
car [DottedList (x : xs) _] = return $ x

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return $ x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2

operators :: [(String, ([LispVal] -> ThrowsError LispVal))]
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



