# what about none standard generic calls
isS4Generic = function(x) if(is.function(x)) length(findCallsTo(body(x), "standardGeneric")) > 0 else FALSE
isS3Generic = function(x) if(is.function(x)) length(findCallsTo(body(x), "UseMethod")) > 0 else FALSE

s4Generics =
function(env, asNames = TRUE)
    genericFuns(env, asNames, isS4Generic)

s3Generics =
function(env, asNames = TRUE)
  genericFuns(env, asNames, isS3Generic)

genericFuns =
function(env, asNames = TRUE, predicate = isS3Generic)            
{
    if(is.character(env))
	   env = getNamespace(gsub("^package:", "", env))
	   
    generic = sapply(as.list(env, TRUE), predicate)
	# table(generic)
    if(asNames)
    	names(generic)[generic]
    else
        generic	  
}
