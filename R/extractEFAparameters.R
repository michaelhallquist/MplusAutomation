#' Extract the model parameters from an EFA Mplus model output
#'
#' @param outfiletext character vector of Mplus output file being processed
#' @param filename name of the output file
#' @return list of parsed EFA parameters
extractEFAparameters <- function(outfiletext, filename) {
  
  #get pre-parsed header lines so that we demarcate the section boundaries properly
  h <- attr(outfiletext, "headerlines")
  
  EFA_HEADER          <- paste(
    c("^(",
      "EXPLORATORY FACTOR ANALYSIS WITH [1-9][0-9]* FACTOR\\(S\\):|",
      "EXPLORATORY FACTOR ANALYSIS WITH \\d+ WITHIN FACTOR\\(S\\) AND \\d+ BETWEEN FACTOR\\(S\\):|",
      "EXPLORATORY FACTOR ANALYSIS WITH \\d+ WITHIN FACTOR\\(S\\) AND UNRESTRICTED BETWEEN COVARIANCE:|",
      "EXPLORATORY FACTOR ANALYSIS WITH UNRESTRICTED WITHIN COVARIANCE AND \\d+ BETWEEN FACTOR\\(S\\):",
      ")"
    ), collapse="")

  header_lines <- grep(EFA_HEADER, outfiletext)
  
  n_factors <- get_efa_nfactors(outfiletext[header_lines])
  if (nrow(n_factors) == 2L) {
    n_factors <- sapply(1:ncol(n_factors), function(x) {
      paste0("wi", n_factors["wi",x], "_", "bw", n_factors["bw",x])
    })
  } else {
    n_factors <- paste0("f", n_factors)
  }
  
  complete_params <- list()
  
  last_boundary <- h[ h > header_lines[length(header_lines)] ]
  if (outfiletext[last_boundary[1L]] == "MODEL FIT INFORMATION") {
    last_boundary <- last_boundary[2L] #skip model fit as boundary, since that is still part of the section
  } else { last_boundary <- last_boundary[1L] } #next substantive section
  
  for(i in seq_along(header_lines)) {
    search_init <- header_lines[i]
    search_end  <- ifelse(i != length(header_lines),  header_lines[i + 1] - 1, last_boundary - 1)
    
    #full output of this efa section
    lines_to_search <- outfiletext[search_init:search_end]
    
    #not the most elegant solution, but we need to deal with the fact that multilevel EFA 
    #returns two independent sections for WITHIN and BETWEEN results. To keep these clear, add
    #a suffix to the list element denoting the model so that the user knows which section we're parsing.
    is_wi <- any(grepl("WITHIN LEVEL RESULTS", lines_to_search))
    is_bw <- any(grepl("BETWEEN LEVEL RESULTS", lines_to_search))
    mlefa_suffix <- ""
    if (is_wi || is_bw) { 
      if (is_wi) { 
        mlefa_suffix <- "_WITHIN" 
      } else if (is_bw) {
        mlefa_suffix <- "_BETWEEN" 
      }
      if (is_wi && is_bw) { stop("EFA section contains both within and between level results. Don't know how to proceed.") }
    }
    
    loadings_est <- matrixExtract(lines_to_search, "[A-z\\-]+ ROTATED LOADINGS\\s*.*", filename, expect_sig=TRUE)
    loadings_sig <- attr(loadings_est, "sig")
    attr(loadings_est, "sig") <- NULL
    corrs_est <- matrixExtract(lines_to_search, "[A-z\\-]+ FACTOR CORRELATIONS\\s*.*", filename, expect_sig=TRUE)
    corrs_sig <- attr(corrs_est, "sig")
    attr(corrs_est, "sig") <- NULL
    residuals_est <- matrixExtract(lines_to_search, "ESTIMATED RESIDUAL VARIANCES", filename)
    loadings_se <- matrixExtract(lines_to_search, "S\\.E\\. [A-z\\-]+ ROTATED LOADINGS", filename)
    corrs_se <- matrixExtract(lines_to_search, "S\\.E\\. [A-z\\-]+ FACTOR CORRELATIONS\\s*.*", filename)
    residuals_se <- matrixExtract(lines_to_search, "S\\.E\\. ESTIMATED RESIDUAL VARIANCES", filename)
    loadings_est_se <- matrixExtract(lines_to_search, "Est\\./S\\.E\\. [A-z\\-]+ ROTATED LOADINGS", filename)
    corrs_est_se <- matrixExtract(lines_to_search, "Est\\./S\\.E\\. [A-z\\-]+ FACTOR CORRELATIONS\\s*.*", filename)
    residuals_est_se <- matrixExtract(lines_to_search, "Est\\./S\\.E\\. ESTIMATED RESIDUAL VARIANCES", filename)
    loadings_structure <- matrixExtract(lines_to_search, "FACTOR STRUCTURE", filename)
    
    parameters <- list(
      loadings  = list(
        estimates = loadings_est,
        sig       = loadings_sig,
        std_errs  = loadings_se,
        est_se    = loadings_est_se,
        structure = loadings_structure),
      corrs     = list(
        estimates = corrs_est,
        sig       = corrs_sig,
        std_errs  = corrs_se,
        est_se    = corrs_est_se
      ),
      residuals   = list(
        estimates = residuals_est,
        std_errs  = residuals_se,
        est_se    = residuals_est_se
      )
    )
    
    complete_params <- append(complete_params, list(parameters))
    names(complete_params)[length(complete_params)] <- paste0(n_factors[i], mlefa_suffix)
  }
  
  result <- list(efa = complete_params)
  
  return(result)
}
