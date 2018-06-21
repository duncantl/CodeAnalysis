R programmers typically develop functions incrementally by evaluating expressions at the R prompt
and then adding the code to a function.  These functions sometimes use non-local/global variables,
either intentionally or because the author forgot to add these as parameters for the function.
Similarly, in section ??, we saw how we sometimes want to extract functions defined within the body
of a primary function and define them separately.  However, these often directly access non-local
variables within the enclosing function.  So when we extract and define these functions separately
from the enclosing function, we have to identify non-local variables and define corresponding
parameters for each of these functions and then change the calls to these functions to include the
non-local variables.

The function `mkGlobalsLocal()` analyses one or more functions
and 
+ identifies the non-local variables used in each function;
+ adds parameters to each of these functions, with the global variable as the default value for the parameter;
+ changes calls to these functions in other (stage 2) functions to add the global variables as arguments in the calls;
+ iterates to determine if these stage 2functions also now refer  to global variables.

