f = function(x) { y = 1; ans = foo(); ans}
g = function(){ ans = foo();  y}
g2 = function(){ ans = foo();  invisible(y)}
g2 = function(){ ans = foo();  invisible(y)}

f2 = function(x) { y = 1; ans = foo(); invisible(ans)}
f3 = function(x) { y = 1; ans = foo(); return(ans)}
f4 = function(x) { y = 1; ans = foo(); return(invisible(ans))}
f5 = function(x) { y = 1; ans = foo(); invisible(return(ans))}
f6 = function(x) { y = 1; ans = foo(); bar(ans)}

