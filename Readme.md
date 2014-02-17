This is a solution to http://codegolf.stackexchange.com/questions/20957/

Here is a solution in Haskell with custom made parser combinators. No library is required, all pure functional power. :)

The grammar in the middle of the file describes the structure of the expressions that will be parsed. The grammar itself is the parser, and it is build from terminal parsers, with parser combinators to put them in sequence (`<:>`,`<::>`) or as alternatives (`|||`). Each of the terminal parsers parses a certain string or character, such as an operator or a digit. The evaluation combinator (`==>`) then transforms and calculates the expression.

Some inspiration I found 
[in this blog][1], but mainly I learned this when I worked with a DSL embedded in Haskell that uses the same approach ([ADP, Algebraic Dynamic Programming][2], I wrote a typechecker for it), here we can even solve optimization problems with dynamic programming!
Also there is a nice paper by G.Hutton about parsing with combinators in Haskell.

This was a really fun problem, thanks for suggesting it!

  [1]: http://alephnullplex.appspot.com/blog/view/2010/01/12/lbach-1-introduction
  [2]: http://bibiserv.techfak.uni-bielefeld.de/adp/
  [3]: http://eprints.nottingham.ac.uk/221/1/parsing.pdf

┌─[linse@yolocat]─[~/jìsuànqì]
└──╼ ghc 计算器.hs && ./计算器
[1 of 1] Compiling Main             ( 计算器.hs, 计算器.o )
Linking 计算器 ...
type ':q' to quit.
二乘以八
二乘以八等于十六
:q
