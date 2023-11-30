source("source2.R")
source("source3.R")

if(FALSE) {
    source("source4.R")
}


if(TRUE) {
    source("source5.R")
}

if(FALSE) {
    source("source6.R")
} else
    source("source7.R")

if(FALSE) {
    source("source8.R")
} else if(foo() > 0) {
    source("source9.R")
} else
    source("source10.R")

