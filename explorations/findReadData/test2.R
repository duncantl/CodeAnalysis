# Run from within EgSourceFiles/../ directory, i.e. the same as this test2.R file.
#
# This is almost the same as test1.R but is different in that we
# start from the directory above the top-level file being "lexically sourced"
# and we provide the starting directory in the call to substSource().
#
library(rstatic)
source("findReadDataCalls.R")
topLevelFile = "EgSourceFiles/src6.R"
src1 = to_ast(parse(topLevelFile))
src1
src2 = substSource(copy(src1), recursive = TRUE, dir = dirname(topLevelFile))
src2





