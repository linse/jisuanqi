-- ┌─[linse@yolocat]─[~/test]
-- └──╼ ghc -o calc own_combi.hs
-- [1 of 1] Compiling Main             ( own_combi.hs, own_combi.o )
-- Linking calc ...
-- ┌─[linse@yolocat]─[~/test]
-- └──╼ ./calc
-- type ':q' to quit.
-- 二乘以八
-- 二乘以八等于十六

type Parser a = String -> Maybe (a, String) 

dict = [('零', 0) ,('〇', 0) ,('一', 1) ,('二', 2) ,('三', 3) ,('四', 4) ,('五', 5) ,('六', 6) ,('七', 7) ,('八', 8) ,('九', 9), ('十',10), ('百',100)]

-- parse 1 character
char :: Parser Char
char [] = Nothing
char (x:xs) = Just(x, xs)

-- parse a chinese digit
cndigit :: Parser Char
cndigit = char <== isCNdigit
  where isCNdigit c = elem c $ map fst dict

-- parse a string literal
literal :: String -> Parser String
literal s [] = Nothing
literal s xs = if (head==s) then Just(head, tail)
                            else Just("", xs)
  where (head,tail) = splitAt (length s) xs 

-- parse and evaluate the following terminal symbols
digit = cndigit ==> cnDigitToInt
  where cnDigitToInt d = head [snd x | x <- dict, fst x==d]
add  = literal "加"   ==> (\_ -> (+))
subt = literal "减"   ==> (\_ -> (-))
mult = literal "乘以" ==> (\_ -> (*))
divi = literal "除以" ==> (\_ -> div)
tens = literal "十"   ==> (\_ -> t)
  where t a b = a * 10 + b
hndrds = literal "百" ==> (\_ -> h)
  where h a b = a * 100 + b

-- inject a 1 into the parse in case people underspecify tens
inject1 :: Parser Integer
inject1 s = Just (1, s)

-- the grammar
expression = number <:> add  <::> number |||
             number <:> subt <::> number |||
             number <:> mult <::> number |||
             number <:> divi <::> number |||
             number
number = digit <:> (hndrds <::> (digit <:> tens <::> digit)) |||
         digit <:> tens <::> digit |||
         digit |||
         inject1 <:> tens <::> digit -- inject 1 for underspecified tens

-- run the calculator as a repl
main = putStrLn "type ':q' to quit." >> repl
 where 
  repl = do
         input <- getLine
         let result = printNum $ eval input
         if ((== ":q") input)
           then return ()
           else putStrLn (input ++ "等于" ++ result) >> repl

-- call the grammar to parse and evaluate
eval s = case expression s of
   Just (n,[]) -> n
   Just (n,xs) -> error $ "Could not parse input, got stuck on "++xs
   Nothing -> error $ "Could not parse input at all "++s

-- reencode a number into chinese
printNum x = h' ++ t' ++ o
 where o  = if ((h'/="" || t'/="") && tRest == 0) then "" else [intToCNDigit tRest] 
       h' = printDigit h "百"
       t' = printDigit t "十"
       printDigit d c | d==0 = ""
                      | d==1 && c=="百" = '一':c
                      | d==1 = c
                      | otherwise = intToCNDigit d:c
       (t, tRest) = divMod hRest 10
       (h, hRest) = divMod x 100
       intToCNDigit d =  head [fst x | x <- dict, snd x==d]

-- the combinators:

-- return the result of the parser only if
-- it also satisfies the given predicate - like `with` in ADP
infix 7 <== 
(<==) :: Parser a -> (a -> Bool) -> Parser a 
(m <== p) s = case m s of 
        Nothing     -> Nothing 
        Just(a,s)  -> if p a then Just(a,s) else Nothing

-- alternative/or combinator
infixl 3 |||
(|||) :: Parser a -> Parser a -> Parser a 
(m ||| n) s = case m s of 
    Nothing -> n s 
    ms -> ms

-- next/sequence combinator
-- reverse polish notation needs just this:
-- testRPExp = digit <:> (digit <:> add)
infixl 6 <:>
(<:>) :: Parser a -> Parser (a -> b) -> Parser b
(m <:> n) s = case m s of
    Nothing -> Nothing
    Just (a, s') -> case n s' of
        Nothing -> Nothing
        Just (b, s2) -> Just (b a, s2)

-- next/sequence combinator, right side
-- infix notation needs this second combinator:
-- testExp = digit <:> add <::> digit
infixl 5 <::>
(<::>) :: Parser (a->b) -> Parser a -> Parser b
(m <::> n) s = case m s of
    Nothing -> Nothing
    Just (a, s') -> case n s' of
        Nothing -> Nothing
        Just (b, s2) -> Just (a b, s2)

-- evaluate a parse result!!
infixl 5 ==>
(==>) :: Parser a -> (a -> b) -> Parser b
(m ==> n) s = case m s of
    Nothing -> Nothing
    Just (a, s') -> Just (n a, s')
