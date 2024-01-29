get_mplus_file <- function(fname, mplus_version="8.10") {
  d <- testthat::test_path("mplus_ug", mplus_version)
  stopifnot(dir.exists(d))
  f <- file.path(d, fname)
  f_bz <- paste0(f, ".bz2")
  f_gz <- paste0(f, ".gz")
  f_xz <- paste0(f, ".xz")

  if (file.exists(f)) {
    return(f)
  } else if (file.exists(f_bz)) {
    return(decompress(f_bz))
  } else if (file.exists(f_gz)) {
    return(decompress(f_gz))
  } else if (file.exists(f_xz)) {
    return(decompress(f_xz))
  } else {
    stop("Cannot locate file: ", fname)
  }
}

# adapted from https://github.com/HenrikBengtsson/R.utils/blob/develop/R/compressFile.R
decompress <- function(filename, dest=NULL) {
  BFR.SIZE <- 1e7

  stopifnot(file.exists(filename))
  if (grepl("\\.bz2$", filename)) FUN <- bzfile
  else if (grepl("\\.gz$", filename)) FUN <- gzfile
  else if (grepl("\\.xz$", filename)) FUN <- xzfile
  else stop("Cannot sort out compressed file extension from: ", filename)
  
  inn <- FUN(filename, open="rb")
  on.exit(if (!is.null(inn)) close(inn))
  
  if (is.null(dest)) {
    ext <- sub(".*(\\.[^.]+)\\.(gz|xz|bz2)$", "\\1", filename, perl=TRUE)
    dest <- tempfile(fileext = ext)
  }
  outComplete <- FALSE
  out <- file(dest, open="wb")
  on.exit({
    if (!is.null(out)) close(out)
    # Remove incomplete file?
    if (!outComplete) file.remove(destnameT)
  }, add=TRUE)
  
  # Process
  nbytes <- 0
  repeat {
    bfr <- readBin(inn, what=raw(0L), size=1L, n=BFR.SIZE)
    n <- length(bfr)
    if (n == 0L) break
    nbytes <- nbytes + n
    writeBin(bfr, con=out, size=1L)
    bfr <- NULL  # Not needed anymore
  }
  outComplete <- TRUE
  close(out)
  out <- NULL
  return(dest)
}

