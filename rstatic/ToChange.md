The following are functions that have calls to rstatic functions that
are not also in base or in CodeAnalysis.

The rstatic functions called are 
+ as_language
+ children
+ `children<-`
+ find_nodes
+ is_symbol
+ match_call
+ replace_nodes
+ rstatic::arg_index
+ rstatic::arg_value
+ rstatic::as_language
+ rstatic::find_nodes
+ rstatic::Symbol
+ rstatic::to_ast
+ to_ast
+ where_is


Also look for $fn, $value, ....

"Symbol"
"ListOfCalls"


# Exported functions that use rstatic functions but not called (5)
+ [test] findCallsParam
   + callsFunParam.R
   + changed
+ √ [removed] rlangType
   + not used. Could be removed.
   + removed
+ [test] findNSE
   + NSE.R
   + not called by anything
   + changed
+ [test] getFilesRead
   + readFile.R
   + not called within package
+ [test] findUsedOptions
   + options.R
   + not called within package
   + see tests/options.R for test cases
   
# Exported functions that use rstatic functions (2)
+ [test] isCallTo
   + callsTo.R
   + remove rstatic part.

+ [test] getURLs
   + freeVariables.R
   + changed to use findLiterals() and then filter.

+ [test] isFunAssign
    + freeVariables.R
    + removed the  rstatic if part
+ [test] isIfFalse
    + freeVariables.R
    + removed the  rstatic if part

# Not exported but called (13)

But could be called by a function that is not itself called or exported.

+ [removed] asFunction
   + used in countNestedFunctionLevel
   + not any longer.
+ [remove] asScript
   + used in findLiteralValue
+ [remove] asToplevelExpr
   + used in findLiteralValue
+ [test] countNestedFunctionLevel
   + Moved these three to findLiteralValue.
   + These walk back up the AST.  So need to reimplement.

+ collectRemoveFun
+ removeFromBody
    + both related to extractFunctions.
+ renameVarFun
    + used in changeParamName
+ changeParamName
    + used in addParams()

+ checkVariableDependency
    + used in checkLoopDepend
+ checkUnique
    + checkLoopDepend.R
	+ but only called from checkLoopDepend() which is not exported.


+ passGlobals <<<<<<<<
   + passGlobals.R
   + rewrites language objects to rename variable
   + used in mkGlobalsLocal()
   
+ updateCallsFun
    + used in passGlobals

+ [test] getUsedOptionName
+ [false positive] findMissingVariables
    + accesses defs$variable but that is okay.

+ [test] varAppears
   + use getAllSymbols()
+ [test] findAssignsOverVar
   + use findAssignsTo
+ [test] findAllUpdates
   + use findAssignsTo
+ [removed] paramValue
   + calls findLiteralValue()
   + checks if is(, "Literal")
   + called from getCallParam() (freeVariables.R) and getS3ClassInfo() (2 times in
     packageAnalysis.R)
	  + getS3ClassInfo not used apparently. See S3Assigns.R  Probably more robust.

+ [removed] usedAsFunction
    + freeVariables.R
	+ removed
    + use findCallsParam() instead.
+ [test] getCallParam
    + freeVariables.R
    + removed rstatic code from function
+ [test] getAssignedVars
   + ifAssignment.R
   + uses findAssignsTo()
   + ¿¿¿  Do we want to complex = TRUE/FALSE?
+ [test & fix] usesDotsI
    + usesDots.R
	+ determine whether to include list(...) as a separate element or only in assignments or calls.
	  e.g., a = list(...) or foo(list(...))
	


# Not called and not exported.  (17)

So enclosed in `if(FALSE)`

+ findLiteralValue
   + freeVariables.R
   + for constant propagation
+ independentUpdate
   + checkLoopDepend.R
+ checkLoopDepend
   + checkLoopDepend.R
+ extractFunctions
+ [test] findFunctions
+ [test] findSuperAssignments
   + converted to use findAssignsTo
+ [test] usedInCode
+ [removed] getSymbolValues
+ [removed] isParamUsedAsFun
   + freeVariables.R
   + removed
+ [test] getGraphicsDeviceCalls

+ √ ifAssignments
    + ifAssignment.R
	+ not exported
	+ not called

+ [removed] astnumTerms
   + packageAnalysis.R
   + numTerms version in same file after this.

+ [Moved] to rstatic/funs.R
   + dropNotRunCode.R6
   + isS3ClassSetNode
   + findS3ClassDefs.ASTNode
   + findS3ClassDefs.expression = findS3ClassDefs.function

+ √ usesDots
+ [removed] isUsedOption


###################################

## From findRstatic.R

+ √ findCallsParam
   + callsFunParam.R
+ √ isCallTo
+ √ rlangType
   + not used, just exported
+ √ getURLs
   + freeVariables.R
+ findNSE
+ findUsedOptions
+ getFilesRead
+ isComplexAssignTo
+ mkGlobalsLocal
+ mkCallWalkerPred
+ freeVariables
+ returnsFunction
+ findCallsTo
+ findLoops
+ findAssignsTo
+ findLiterals
+ S3Assignments
+ numNestedLoops
+ getGlobals
+ isLocalVar
   + notParam causes  isParameter() to be called.
+ √ numCalls
   + Fine.
+ getScriptGlobals
+ missingScriptVars
