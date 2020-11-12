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

I'm implementing the use of numeral and simple conditional statement

```bash
convertInput :: String -> String
convertInput []     = []
convertInput (x:xs) 
        | x `elem` ['0'..'9'] = convertNumeral x       ++ convertInput xs
        | x == '+'            = "(λwyx.y(wyx))"        ++ convertInput xs
        | x == '*'            = "(λxyz.x(yz))"         ++ convertInput xs
        | x == 'T'            = "(λxy.x)"              ++ convertInput xs
        | x == 'F'            = "(λxy.y)"              ++ convertInput xs
        | x == '&'            = "(λxy.xy(λuv.v))"      ++ convertInput xs
        | x == '/'            = "(λxy.x(λuv.u)y)"      ++ convertInput xs
        | x == '~'            = "(λx.x(λuv.v)(λab.a))" ++ convertInput xs
        | otherwise           = [x]                    ++ convertInput xs

convertNumeral :: Char -> String
convertNumeral x = "(λsz." ++ (concat . replicate (digitToInt x)) "s(" ++ "z" ++ (concat . replicate (digitToInt x)) ")"
```

In there I did some simple tas of converting some symbol into lambda expression.
I'm also use specialized numeral converter making use of concat and replicate.

**Possible update:**
Number more than 9 and many more to come.
Be Right Back!!

## How to use it

These input are expected by the program
```bash
Use \ or λ before a function

ex: \xy.x or λxy.y
```
Number
```bash
Number can be used are 0-9 more than that is not yet possible
```

Increment
```bash
To use increment, use the symbol + in between the numbers

ex: 9+8 or 1+2
```

Multiplication
```bash
To use multiplication, use the symbol * FOLLOWED by two numbers
Attention: Do not use * in between the numbers

ex: *12 or *08
```

True and False
```bash
You can use true or False by simply typing T or F

ex: T or F
```

Or operator
```bash
To use or operator use the symbol / FOLLOWED by two conditional
Attention: Conditional being T or F
Attention: Do not use / in between the conditionals

ex: /TF or /FF
```

And operator
```bash
To use or operator use the symbol & FOLLOWED by two conditional
Attention: Conditional being T or F
Attention: Do not use & in between the conditionals

ex: &FT or &TT
```

Not operator
```bash
To use not operator simply use the symbol ~ followed by conditional

ex: ~F or ~T
```