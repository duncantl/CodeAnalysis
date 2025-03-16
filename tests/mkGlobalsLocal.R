library(CodeAnalysis)

#XXXX remove if(FALSE) when fix function.
if(FALSE) {
f = function(x) {  y = x + a; z = 2*a ; return(z + y)}
fn = mkGlobalsLocal(f)

# Both references to a changed to .a

f = function(x) {  y = x + a; z = 2*a ; return(z + y + a)}
fn = mkGlobalsLocal(f)

}


