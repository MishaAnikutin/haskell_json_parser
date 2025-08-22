# Парсер JSON-а на Haskell

Игрушечный парсер JSON написанный на Haskell, который преобразует строку JSON в обьект языка. 

Умеет немного:
- Целые числа (без экспоненциальной записи)
- Строки
- Списки
- Словари

## Структура парсера:
```haskell
-- json строчку засовываем в лексер

type Lexem = String
lexer :: String -> [Lexem]

-- лексемы засовываем в токенизатор
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

tokenize :: [Lexem] -> [Token]

-- Список токенов засовываем в Parser, который возвращате в JSON
data JSON 
  = JsonNumber Integer
  | JsonString String
  | JsonArray [JSON]
  | JsonMap [(String, JSON)]
  deriving (Show, Eq)

parse :: [Token] -> JSON
```

Итоговая функция получения JSON выглядит так:
```haskell
parseJSON :: String -> JSON
parseJSON inputJson = parse $ tokenize $ lexer inputJson
```

## Алгоритм:
1. Лексер предобрабатывает строку, разделяет её на леммы, убирает пропуски и пробелы:
```haskell
-- реализация insertSep и split
-- ...

lexer :: String -> [Lexem]
lexer jsonString = filter (not . null) $ split $ preprocessedString 
  where preprocessedString = insertSep False jsonString
```

2. Токенизатор маппит лексемы на токены:
```haskell
tokenMap :: String -> Token
-- реализация маппера
-- ...

tokenize :: [Lexem] -> [Token]
tokenize lexems = map tokenMap lexems
```

3. Парсер строит JSON из токенов по алгоритму рекурсивного спуска. Тип `JSON` рекурсивный, поэтому функция `parse` также красиво реализуется рекурсивно:

При парсинге JSON-а список токенов делится на токены текущего уровня вложенности и остальных, каждые парсятся и результат обьеденяется
```haskell
-- Если открывающая фигурная скобочка то парсим значения до её уровня вложенности 
parser (LeftBrace : xs) =
  JsonMap (parseMap (takeWhileBalanced (LeftBrace:xs)))

-- Если открывающая квадратная скобочка то парсим значения до её уровня вложенности
parser (LeftBracket : xs) =
  JsonArray (parseArray (takeWhileBalanced (LeftBracket:xs)))
```

С отдельной реализацией парсинга списков и словарей:

```haskell
parseMap :: [Token] -> JsonMap
parseArray :: [Token] -> JsonArray
```

Результат обьединяется на моменте парсинга паттернов. Для словаря:
```haskell
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
```

Для списка:
```haskell
parseArray (LeftBracket : xs) = parseArray (takeWhileBalanced (LeftBracket:xs))
parseArray (LeftBrace : xs)   = JsonMap(parseMap (takeWhileBalanced (LeftBrace:xs))) : parseArray (dropWhileBalanced (LeftBrace:xs))
```

Который итогом приходит в конечные реализации атомарных типов:
```haskell
parser (StringToken s : xs) = JsonString s  
parser (NumberToken n : xs) = JsonNumber n
```

В который без проблем можно добавить реализации для новых типов или расширения текущих


