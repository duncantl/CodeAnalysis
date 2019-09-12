# Run from within EgSourceFiles/ directory
#
# This shows recursive lexical source()'ing with subdirectories,
# i.e. the top-level file source()s a second that is in a different directory D and
# that file sources() a third file that is also in D.
# So the 1st source() specifies a directory, but the other two are relative.
#
#  src6.R
#    SubDir/src_sub2.R
#       SubDir/src_sub.R
#
#
library(rstatic)
code = "../findReadDataCalls.R"
if(file.exists("EgSourceFiles")) # !file.exists(code) && file.exists(basename(code)))
    stop("Need to run this from EgSourceFiles")

source(code)

src1 = to_ast(parse("src6.R"))
src1
src2 = substSource(copy(src1), recursive = TRUE)
src2





