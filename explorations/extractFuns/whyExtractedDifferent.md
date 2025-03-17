#

Why is the size of a function different from 
(size of extracted functions + size of reduced function after extraction)?

One reason is that we are remove `fun = function()...` calls so 
we are removing the = and left and right hand side.
When we compute the size of the extracted functions, we won't see the `fun` and `=`.
So we have lost 2 elements for every extracted functions.


But that wasn't enough.

It turns out that 

1. when there are multiple levels of nested named functions within the top-level function
    we don't recursively extract them (yet?!) and so if we have the  situation below, 
	 we get g and h as the extracted functions, but g will continue to contain h.
	 Accordingly, the number of terms in g and h double counts those in h.
	 This is why are calculations for back and then diff were always negative, i.e., more in the
    `reduced + sum(extracted) + 2*legnth(extracted)`.
```
f = function() {
     g = function() {
        h = function() {
		
		}
		...
	 }
	 ...
}
```
	
2. There were bugs in the mkModifyCodeWalker() which we fixed given this long debugging
   investigation.
    + See toolsExtract.R




##

Moved from tools_package.md and written before we solved the issue and 
noted it above.

The size of the original function
is not necessarily the same size 
as the number of terms in the modified function and the sum of the number of terms in the 
```
cbind(d$numTerms - d$sizeNestedFunctions, d$afterExtractedNumTerms)
```
```
     [,1] [,2]
[1,]  200   56
[2,] 3193 3072
[3,] 1256 1206
[4,]  441  376
[5,] 1757 1702
[6,]  441  390
```
This is in part due to the `var <- function() {...}` being removed and the `var` and `<-` are not
count. So for every named nested functions (the one we take out), we remove three terms
and only count


cbind(d$numTerms - d$sizeNestedFunctions - 2*d$numFunctions, d$afterExtractedNumTerms)


v = "makeJSS"
library(codetools)
# The nested functions
ctr = CodeAnalysis:::mkCounter ()
invisible(lapply(nfns[[v]], walkCode, ctr))

# the original version
ctr2 = CodeAnalysis:::mkCounter()
invisible(walkCode(tfns[[v]], ctr2))
length(ctr2$.els())

# the reduced/extracted version
ctr3 = CodeAnalysis:::mkCounter()
invisible(walkCode(ex[[v]]$newFun, ctr3))
length(ctr3$.els())





getTerms = function(code) { ctr = CodeAnalysis:::mkCounter(); walkCode(code, ctr); ctr$.els()}

getTerms(quote(function() a))
```
[[1]]
`function`

[[2]]
NULL

[[3]]
a

[[4]]
function() a
```


getTerms(function() a)

```
[[1]]
NULL

[[2]]
a

```


#



f = function(x, y) {  a = 1;  b = function(c, d = 2) { c }; b(a) }

fex = extractFunctions(f)

numTerms(f) -               16
numTerms(fex$new)            8
numTerms(fex$nested[[1]])    6


g = function(x, y) {  a = 1;  b = function(c, d = 2) { c }; o = function() 10; b(a) }

gex = extractFunctions(g)
numTerms(g) -               22
numTerms(gex$new)            8
numTerms(gex$nested[[1]])    6 4 = 10


#


numTerms(f)
ctr4 = CodeAnalysis:::mkCounter ()
walkCode(f, ctr4); ctr4$.els()



[[1]]


[[2]]


[[3]]
`{`

[[4]]
`=`

[[5]]
a

[[6]]
[1] 1

[[7]]
`=`

[[8]]
b

[[9]]
`function`

[[10]]


[[11]]
[1] 2

[[12]]
`{`

[[13]]
c

[[14]]
function(c, d = 2) { c }

[[15]]
b

[[16]]
a










fex = extractFunctions(f)
ctr4 = CodeAnalysis:::mkCounter ()
walkCode(fex$new, ctr4); ctr4$.els()

[[1]]


[[2]]


[[3]]
`{`

[[4]]
`=`

[[5]]
a

[[6]]
[1] 1

[[7]]
b

[[8]]
a


This corresponds to 
+ the 2 parameters
+ {
+ a = 1
+ b(a)


#


fex = extractFunctions(f)
ctr4 = CodeAnalysis:::mkCounter ()
walkCode(fex$nested[[1]],
ctr4);
ctr4$.els()

[[1]]
`function`

[[2]]


[[3]]
[1] 2

[[4]]
`{`

[[5]]
c

[[6]]
function(c, d = 2) { c }


This corresponds to 9..14 in the original function.


