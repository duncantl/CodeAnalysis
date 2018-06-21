In topics.md, this corresponds to 
   Refactoring functions to smaller, more modular functions. Detect blocks
   of expressions that have direct data dependence and make these into
   functions.
   

The idea is that we have a function that really consists of two or more separate tasks
and then combines the results from these two  to return the result
```
function(...)
{
   # expressions for task 1
   
   a = 
   b = 
   c = 
   
   # expressions for task 2
   
   x = 
   y = 
   z = 

   foo(c, z)
}
```
We could rewrite this as
```
function(....)
{
     c = task1()
	 z = task2()
	 
	 foo(c, z)
}
```
This makes the code clearer, more reusable and we can test the two functions task1 separately
and easily provide alternative implementation for either.
