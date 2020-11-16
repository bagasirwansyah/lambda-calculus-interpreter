# Tugas 5 Pemrograman Fungsional:

**Bagas Irwansyah**
**1606880730**

A simple REPL for lambda calculus.

This repo is forked from Magesh Kumar in Github

[Repo Magesh Kumar / lambda-calculus-interpreter](https://github.com/Ema93sh/lambda-calculus-interpreter)

## Run

To run the REPL, just type in:

    cabal run

## Addition to the Code

### The main requirement:
1. Use numeral to evaluate the lambda expression

### My aditional work:
1. Simple conditional statement
2. The number can be more than 9
3. The multiplication operator can be operated infix
4. The '/' and '&' operator can be operated infix

Here are my additional code, suplementary from Magesh Kumar.

```bash
(!>) = drop
(<!) = flip take
```
This sugar code was taken from [StackOverflow](https://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python)

```bash
translateInput :: String -> String
translateInput str
    | '*' `elem` str                              = convertInput (rewriteStar (map (\x -> x+1) (elemIndices '*' str)) str)
    | True `elem` (map (\x -> x `elem` "/&") str) = convertInput (rewriteBool (map (\x -> x+1) (elemIndices '&' str)) (map (\x -> x+1) (elemIndices '/' str)) str)
    | otherwise                                   = convertInput str
```

This code will take the first input, and check wether the symbol '\*', '/' and '&' is used, as my code enables them to be used as infix operator, I would decode the input into normal equation '3\*2' -> '*3(2)'.

```bash
where rewriteStar :: [Int] -> String -> String
rewriteStar _      ""  = ""
rewriteStar []     str = str
rewriteStar (x:xs) str = "*" ++ (0 !> str <! (x-1)) ++ "(" ++(rewriteStar (map (\a -> a - x) xs) (x !> str <! length str)) + ")"

rewriteBool :: [Int] -> [Int] -> String -> String
rewriteBool _      _      ""  = ""
rewriteBool []     []     str = str
rewriteBool (x:xs) []     str = "&" ++ (0 !> str <! (x-1)) ++ "(" ++ (rewriteBool (map (\a -> a - x) xs) [] (x !> str <! length str)) ++ ")"
rewriteBool []     (y:ys) str = "/" ++ (0 !> str <! (y-1)) ++ "(" ++ (rewriteBool [] (map (\a -> a - y) ys) (y !> str <! length str)) ++ ")"
rewriteBool (x:xs) (y:ys) str
    | x < y     = "&" ++ (0 !> str <! (x-1)) ++ "(" ++ (rewriteBool (map (\a -> a - x) xs) (map (\a -> a - x) (y:ys)) (x !> str <! length str)) ++ ")"
    | otherwise = "/" ++ (0 !> str <! (y-1)) ++ "(" ++ (rewriteBool (map (\a -> a - y) (x:xs)) (map (\a -> a - y) ys) (y !> str <! length str)) ++ ")"
```

These are the code I used to decode the infix operator as I said above.
example "1\*2\*3\*4\*5"<br/>
will be "*1(2\*3\*4\*5)"<br/>
will be "*1(*2(3\*4\*5))"<br/>
will be "*1(*2(*3(4\*5)))"<br/>
will be "*1(*2(*3(*4(5))))"<br/>

example "F/T&T/F"<br/>
will be "/F(T&T/F)"<br/>
will be "/F(&T(T/F))"<br/>
will be "/F(&T(/T(F)))"<br/>

```bash
onvertInput :: String -> String
convertInput []           = []
convertInput (x:xs) 
    | x == '^'            = convertInput aksen "" xs
    | x `elem` ['0'..'9'] = "\\"
    | x == '+'            = "(λwyx.y(wyx))"        ++ convertInput xs
    | x == '*'            = "(λxyz.x(yz))"         ++ convertInput xs
    | x == 'T'            = "(λxy.x)"              ++ convertInput xs
    | x == 'F'            = "(λxy.y)"              ++ convertInput xs
    | x == '&'            = "(λxy.xy(λuv.v))"      ++ convertInput xs
    | x == '/'            = "(λxy.x(λuv.u)y)"      ++ convertInput xs
    | x == '~'            = "(λx.x(λuv.v)(λab.a))" ++ convertInput xs
    | otherwise           = [x]                    ++ convertInput xs

        
    where convertInput aksen :: String -> String -> String
          convertInput aksen []  []      = []
          convertInput aksen str []      = convertNumeral (read str :: Int)
          convertInput aksen str (x:xs)
              | x `elem` ['0'..'9'] = convertInput aksen (str ++ [x]) xs
              | otherwise           = convertNumeral (read str :: Int) ++ convertInput (x:xs)
```

This is the main code to decode the symbol and numeral into lambda expression, the symbol is decoded easily as shown. But for the numeral to be able to go beyond 9, I need a unique symbol for numeral so instead of using '6', we used '^6'. And in convertInput' (aksen) there was when I call the function to decode the numeral into lambda expression.

```bash
convertNumeral :: Int -> String
convertNumeral x = "(λsz." ++ (concat . replicate x) "s(" ++ "z" ++ (concat . replicate (x + 1)) ")"
```

This is how I convert numeral into lambda expression, I use replicate to write over and over until, equivalent to the numeral.

## How to use it

These input are expected by the program
```bash
Use \ or λ before a function

ex: \xy.x or λxy.y
```
Number
```bash
Numeral can be used beyond 9, but needed to be included the symbol '^' before.

ex: ^9 or ^10 or ^0 or ^1000000000
```

Addition
```bash
To use addition, use the symbol + in between the numbers

ex: ^9+^8 or ^11+^2
```

Multiplication
```bash
To use multiplication, use the symbol * in between the numbers

ex: ^0*^1 or ^2*^19
```

Addition + Multiplication
```bash
To use both addition and multiplication, use it wisely by wrapping the addition in parentheses.
ALWAYS wrap the addition in parentheses.

ex: ^8*(^10+^1)*^0 or (^8+^2)*(^9+^1)
wrong ex: (^8*^1)+^5 or ^1+^2*^3+^4*^5
```

True and False
```bash
You can use true or false by simply typing T or F

ex: T or F
```

Or operator
```bash
To use or, use the symbol / in between the conditionals (T or F)

ex: T/F or F/F/T
```

And operator
```bash
To use and, use the symbol & in between the conditionals (T or F)

ex: F&T or T&T&F
```

Not operator
```bash
To use not operator simply use the symbol ~ followed by conditional
If the conditional is a statement, rather than a normal F or T, use ~()

ex: ~F or ~T or ~(T/T&F)
```

All conditional operator
```bash
Make sure to make yourself clear when using all conditional operator.

ex: ~T/F means you want to (~T) / F
ex: ~(T/F) means you want to ~ (T / F)
et cetera
```