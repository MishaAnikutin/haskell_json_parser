{-# LANGUAGE OverloadedStrings #-}

import Data.Map.Strict (Map)
import Data.Char (chr)

-- example.json засовываем в лексер
-- лексер возвращает список лексем
-- лексемы засовываем в токенизатор
-- Токенизатор возвращает список токенов 
-- Список токенов засовываем в Parser
-- Парсер возвращает JSON


--------------------------------- ЛЕКСЕР ---------------------------------
isWhiteSpace :: Char -> Bool
isWhiteSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

isSymbol :: Char -> Bool
isSymbol c = c == ':' || c == '{' || c == '}' || c == '[' || c == ']' || c == ';' || c == ','

-- Добавим разделитель, по которому будем делать split
-- будем использовать символ разделения ASCII, 
-- такой точно не попадется в обычном тексте.
sep = chr 0x1F

wrap :: Char -> String
wrap word = [sep, word, sep]
-- Вставляем разделитель.
-- Либо оборачиваем символ вокруг разделителя
-- Либо пропускаем пробел/перенос строки
-- Либо это литерал или строка в ковычках, которую 
-- оставляем как есть и работаем дальше
insertSep :: Bool -> String -> String
insertSep _ "" = ""
insertSep False ('"' : rest) = sep : '"' : insertSep True rest  -- Открываем кавычку
insertSep True  ('"' : rest) = '"' : sep : insertSep False rest -- Закрываем кавычку
insertSep True  ('\\' : x : xs) = '\\' : x : insertSep True xs
insertSep True  (c : rest) = c : insertSep True rest  -- Все символы внутри строки
insertSep False (c : rest)
  | isSymbol c     = wrap c ++ insertSep False rest  -- Оборачиваем символ разделителями
  | isWhiteSpace c = insertSep False rest            -- Пропускаем пробелы
  | otherwise      = c : insertSep False rest        -- Все остальные символы

preprocess :: String -> String
preprocess jsonString = insertSep False jsonString

-- Делим строчку на две: первая, которая до разделителя
-- и вторая (все остальное). Повторяем рекурсивно
split :: String -> [String]
split [] = []
split jsonString = 
  let (x, y) = break (== sep) jsonString in x : split (drop 1 y)

-- Лексер разделяет строчку по лексемам и убирает пустые значения
type Lexem = String
lexer :: String -> [Lexem]
lexer jsonString = filter (not . null) $ split $ preprocessedString 
  where preprocessedString = insertSep False jsonString


--------------------------------- ТОКЕНИЗАТОР ---------------------------------
data Token 
  = LeftBrace      -- {
  | RightBrace     -- }
  | LeftBracket    -- [
  | RightBracket   -- ]
  | Comma          -- ,  
  | Colon          -- :
  | StringToken String
  | NumberToken Integer
  deriving(Show, Eq)


tokenMap :: String -> Token
tokenMap "{" = LeftBrace
tokenMap "}" = RightBrace
tokenMap "[" = LeftBracket
tokenMap "]" = RightBracket
tokenMap "," = Comma
tokenMap ":" = Colon
tokenMap ('"' : xs) = StringToken (toString xs)
tokenMap xs         = NumberToken (read xs)
tokenMap ('-' : xs) = NumberToken (read xs)

toString :: String -> String
toString [] = ""
toString ('"' : xs)        = toString xs
toString ('\\' : '"' : xs) = '"' : toString xs
toString (x : xs)          = x : toString xs

-- Принимает на вход список лексем, возвращает список токенов
tokenize :: [Lexem] -> [Token]
tokenize lexems = map tokenMap lexems

--------------------------------- ПАРСЕР ---------------------------------

data JSON 
  = JsonNumber Integer
  | JsonString String
  | JsonArray [JSON]
  | JsonMap [(String, JSON)]
  deriving (Show, Eq)
  
  
-- Возвращает сбалансированную последовательность токенов
-- внутри скобок одного уровня вложенности
takeWhileBalanced :: [Token] -> [Token]
takeWhileBalanced [] = []
takeWhileBalanced (LeftBrace : xs) = takeWhileBrace 1 xs
takeWhileBalanced (LeftBracket : xs) = takeWhileBracket 1 xs
takeWhileBalanced xs = xs

-- Возвращает список токенов этого уровня вложенности до RightBrace
takeWhileBrace :: Integer -> [Token] -> [Token]
takeWhileBrace n [] = []
takeWhileBrace n (LeftBrace : xs) = LeftBrace : takeWhileBrace (n + 1) xs
takeWhileBrace 1 (RightBrace : xs) = []
takeWhileBrace n (RightBrace : xs) = RightBrace : takeWhileBrace (n - 1) xs
takeWhileBrace n (x : xs) = x : takeWhileBrace n xs

-- Возвращает список токенов этого уровня вложенности до RightBracket
takeWhileBracket :: Integer -> [Token] -> [Token]
takeWhileBracket 0 xs = []
takeWhileBracket n [] = []
takeWhileBracket n (LeftBracket : xs) = LeftBracket : takeWhileBracket (n + 1) xs
takeWhileBracket 1 (RightBracket : xs) = []
takeWhileBracket n (RightBracket : xs) = RightBracket : takeWhileBracket (n - 1) xs
takeWhileBracket n (x : xs) = x : takeWhileBracket n xs

-- Аналогично, только дропает
dropWhileBalanced :: [Token] -> [Token]
dropWhileBalanced [] = []
dropWhileBalanced (LeftBrace : xs) = dropWhileBrace 1 xs
dropWhileBalanced (LeftBracket : xs) = dropWhileBracket 1 xs
dropWhileBalanced xs = xs

dropWhileBrace :: Integer -> [Token] -> [Token]
dropWhileBrace 0 xs = xs
dropWhileBrace n [] = []
dropWhileBrace n (LeftBrace : xs) = dropWhileBrace (n + 1) xs
dropWhileBrace 1 (RightBrace : xs) = xs
dropWhileBrace n (RightBrace : xs) = dropWhileBrace (n - 1) xs
dropWhileBrace n (x : xs) = dropWhileBrace n xs

dropWhileBracket :: Integer -> [Token] -> [Token]
dropWhileBracket 0 xs = xs
dropWhileBracket n [] = []
dropWhileBracket n (LeftBracket : xs) = dropWhileBracket (n + 1) xs
dropWhileBracket 1 (RightBracket : xs) = xs
dropWhileBracket n (RightBracket : xs) = dropWhileBracket (n - 1) xs
dropWhileBracket n (x : xs) = dropWhileBracket n xs


parser :: [Token] -> JSON
parser [] = error "Ошибка парсера: пустая строка"
parser (LeftBrace : xs) = -- Если открывающая фигурная скобочка 
                          -- то парсим значения до её уровня вложенности
  JsonMap (parseMap (takeWhileBalanced (LeftBrace:xs)))  
  
parser (LeftBracket : xs) = -- Если открывающая квадратная скобочка
                            -- то парсим значения до её уровня вложенности
  JsonArray (parseArray (takeWhileBalanced (LeftBracket:xs)))

parser (StringToken s : xs) = JsonString s  
parser (NumberToken n : xs) = JsonNumber n

parser (RightBrace : _)   = error "Ошибка парсера: открывающая скобка без закрывающей"
parser (RightBracket : _) = error "Ошибка парсера: открывающая фигурная скобка без закрывающей"
parser (Comma : _) = error "Ошибка парсера: запятая вне скобок"
parser (Colon : _) = error "Ошибка парсера: двоеточие вне фигурных скобок"


type JsonMap = [(String, JSON)]
parseMap :: [Token] -> JsonMap
parseMap [] = []
parseMap (Comma : xs)     = parseMap xs 
parseMap (LeftBrace : xs) = 
  parseMap (takeWhileBalanced (LeftBrace:xs))
  
parseMap (StringToken s : Colon : LeftBrace : xs) = 
  -- "ключ": {
  -- Преобразовываем в JsonMap до LeftBrace, обьединенный с 
  -- спаршенным списком токенов, начиная после закрывающейся скобки
  (s, JsonMap (parseMap (takeWhileBalanced (LeftBrace:xs)))) : parseMap (dropWhileBalanced (LeftBrace:xs))

parseMap (StringToken s : Colon : LeftBracket : xs) = 
  -- "ключ": [
  -- Преобразовываем в JsonMap с JsonArray до LeftBrace, обьединенный с 
  -- спаршенным списком токенов, начиная после закрывающейся скобки
    (s, JsonArray (parseArray (takeWhileBalanced (LeftBracket:xs)))) : parseMap (dropWhileBalanced (LeftBracket:xs))

parseMap (StringToken s : Colon : x : xs) = -- "ключ": значение
  (s, parser [x]) : parseMap xs
  
parseMap (RightBrace : _) = error "Ошибка парсера: '}' без '{' "
parseMap (x : _) = error ("Ошибка парсера: " ++ show x ++ " - не валидный токен")

type JsonArray = [JSON]
parseArray :: [Token] -> JsonArray
parseArray [] = []
parseArray (Comma : xs)       = parseArray xs
parseArray (LeftBracket : xs) = parseArray (takeWhileBalanced (LeftBracket:xs))
parseArray (LeftBrace : xs)   = JsonMap(parseMap (takeWhileBalanced (LeftBrace:xs))) : parseArray (dropWhileBalanced (LeftBrace:xs))
parseArray (RightBracket : _) = error "Ошибка парсера: ']' без '['"
parseArray (x : xs) = parser [x] : parseArray xs

main = do
    let inputJson = "{\"abc\": 2, \"3\": [\"4\", \"5\", \"6\"]}"
    putStrLn "Изначальная строчка JSON:"
    print inputJson
    
    let tokens = tokenize $ lexer inputJson
    putStrLn "\nПосле токенизации:"
    print tokens
    
    let json = parser tokens
    putStrLn "\nПосле парсера:"
    print json
  
