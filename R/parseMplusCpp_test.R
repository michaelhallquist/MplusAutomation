library(Rcpp)
library(microbenchmark)

# Load the function
sourceCpp("parseMplusCpp.cpp")

# Read 
outfile <- readLines("parseMplusCpp.out")

# Benchmark the C++ function against stringsplit. Keep in mind that the C++ 
# function also trims whitespace and skips multiple occasions. On my system,
# the cpp function is 7.5 times faster than this single step of the R-parser.

# Unit: milliseconds
# expr       min        lq      mean    median        uq      max neval
# lapply(outfile, strsplit, "\\\\s+?") 273.78144 312.12733 418.43595 359.79135 483.57866 922.3221   100
# parseMplusCpp(outfile)  35.87438  43.63656  56.35262  51.61543  66.52329 210.4740   100

microbenchmark(
  lapply(outfile, strsplit, "\\s+?"),
  parseMplusCpp(outfile)
)

# This is the raw output of the cpp parser:
results <- parseMplusCpp(outfile)

# This returned list is easy to work with in R, and it shouldn't be too hard to
# update the current parser functions to work with it. However, additional speed
# improvements can be gained by intelligently delegating additional tasks to 
# c++. Which tasks these should be is a matter of discussion and performance
# profiling.

# Some initial ideas:
# 1. Make the first element of the returned list a vector of the row lengths of
# the returned elements. This will allow you to quickly grab relevant rows of
# known length that follow the line number of a given header.
# 2. Have C++ put together subsequent lines of identical length into a matrix
# (although this will skip single-line parts of the output and covariance 
# matrices)
# 3. Have C++ only parse sections that start with a header passed to the 
# parseMplusCpp() function, akin to the way things are currently done in R,
# but without regular expressions (single-line, literal matches only). Parsed
# lines are piped to a named list element. Encountering a relevant header
# toggles a bool that enables the parsing syntax; encountering the next header
# will either start piping subsequent lines to another named outlist element, or
# toggle the bool to FALSE until the next relevant header is encountered.