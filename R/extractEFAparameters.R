#' Extract the model parameters from an EFA Mplus model output
#'
#' @param outfiletext character vector of Mplus output file being processed
#' @param filename name of the output file
#' @return list of parameters
extractEFAparameters <- function(outfiletext, filename) {
  
  EFA_HEADER          <-
    "^EXPLORATORY FACTOR ANALYSIS WITH [1-9][0-9]* FACTOR\\(S\\):"
  LOADINGS_HEADER			<- "^ *[A-Z]* ROTATED LOADINGS"
  CORRS_HEADER				<- "^ *[A-Z]* FACTOR CORRELATIONS"
  RESIDUALS_HEADER		<- "^ *ESTIMATED RESIDUAL VARIANCES$"
  LOADINGS_SE_HEADER	<- "^ *S.E. [A-Z]* ROTATED LOADINGS$"
  CORRS_SE_HEADER			<- "^ *S.E. [A-Z]* FACTOR CORRELATIONS"
  RESIDUALS_SE_HEADER	<- "^ *S.E. ESTIMATED RESIDUAL VARIANCES$"
  
  
  FACTOR_HEADER_PREFFIX  <- "factor_"
  N_FACTORS_NAME_PREFFIX <- "efa_factors_"
  
  
  POS_INT_NUMBER <- "[1-9][0-9]*"
  OR_OPERATOR <- "|"
  MPLUS_VAR_NAME <- "[a-zA-Z][a-zA-Z0-9_]*"
  MPLUS_POS_PARAM <- "[01]\\.[0-9]{3}"
  MPLUS_MINUS_SIGN <- "-?"
  MPLUS_PARAM <- paste0(MPLUS_MINUS_SIGN, MPLUS_POS_PARAM)
  MPLUS_SIGNIFICANT <- "\\*"
  MPLUS_PARAM_AND_SIG <- paste0(MPLUS_PARAM, MPLUS_SIGNIFICANT, "?")
  MPLUS_ONE <- "1\\.000"
  MPLUS_ONE_OR_PARAM <- paste0(MPLUS_ONE, OR_OPERATOR, MPLUS_PARAM)
  MPLUS_ONE_OR_PARAM_AND_SIG <- paste0(
    "(",
    MPLUS_ONE,
    OR_OPERATOR,
    MPLUS_PARAM,
    ")",
    MPLUS_SIGNIFICANT,
    "?"
  )
  MPLUS_ONE_OR_POS_PARAM <- paste0(MPLUS_ONE, OR_OPERATOR, MPLUS_POS_PARAM)
  
  
  tryCatch(
    {
      header_lines <- grep(EFA_HEADER, outfiletext)
      
      n_factors <- as.numeric(
        regmatches(
          outfiletext[header_lines],
          regexpr(POS_INT_NUMBER, outfiletext[header_lines])
        )
      )
      
      complete_params <- list()
      
      for(i in seq_along(header_lines)) {
        
        factor_headers <- paste0(FACTOR_HEADER_PREFFIX, 1:n_factors[i])
        
        search_init <- header_lines[i]
        search_end  <- if (i != length(header_lines)) {
          
          header_lines[i + 1]
          
        } else {
          
          length(outfiletext) 
        }

        lines_to_search <- outfiletext[search_init:search_end]
        
        
        loadings_header_lines			<- grep(LOADINGS_HEADER,     lines_to_search)
        corrs_header_lines				<- grep(CORRS_HEADER,        lines_to_search)
        residuals_header_lines		<- grep(RESIDUALS_HEADER,    lines_to_search)
        loadings_se_header_lines	<- grep(LOADINGS_SE_HEADER,  lines_to_search)
        corrs_se_header_lines			<- grep(CORRS_SE_HEADER,     lines_to_search)
        residuals_se_header_lines	<- grep(RESIDUALS_SE_HEADER, lines_to_search)
        
        ## Loadings:
        for(j in seq_along(loadings_header_lines)) {
          
          header_line    <- lines_to_search[loadings_header_lines[j] + 1]
          factor_indices <- as.numeric(
            regmatches(
              header_line,
              gregexpr(POS_INT_NUMBER, header_line)
            )[[1]]
          )
          
          search_init <- loadings_header_lines[j] + 3
          search_end  <- if (j != length(loadings_header_lines)) {
            
            loadings_header_lines[j + 1] - 3
            
          } else {
            
            corrs_header_lines - 3
          }
            
          loadings_lines <- lines_to_search[search_init:search_end]
          
          if(j == 1) {
            
            item_names <- regmatches(
              loadings_lines,
              regexpr(MPLUS_VAR_NAME, loadings_lines)
            )
            loading_est <- loading_sig <- matrix(
              ncol     = n_factors[i],
              nrow     = length(item_names),
              dimnames = list(NULL, factor_headers)
            )
          }
          
          loading_vals <- regmatches(
            loadings_lines,
            gregexpr(MPLUS_PARAM_AND_SIG, loadings_lines)
          )
          loading_vals_num <- sapply(
            loading_vals,
            function(param) {
              
              as.numeric(regmatches(param, regexpr(MPLUS_PARAM, param)))
            }
          )
          loading_est[, factor_indices] <- t(loading_vals_num)

          loading_vals_sig <- t(
            sapply(
              loading_vals,
              function(param) grepl(MPLUS_SIGNIFICANT, param))
          )
          loading_sig[, factor_indices] <- loading_vals_sig
        }
        
        loading_est <- data.frame(variable = item_names, loading_est)
        loading_sig <- data.frame(variable = item_names, loading_sig)
        
        # Correlations:
        corr_est <- corr_sig <- matrix(
          ncol     = n_factors[i],
          nrow     = n_factors[i],
          dimnames = list(factor_headers, factor_headers)
        )
        
        for(j in seq_along(corrs_header_lines)) {
          
          header_line    <- lines_to_search[corrs_header_lines[j] + 1]
          factor_indices <- as.numeric(
            regmatches(
              header_line,
              gregexpr(POS_INT_NUMBER, header_line)
            )[[1]]
          )
          
          search_init <- corrs_header_lines[j] + 3
          search_end  <- if (j == length(corrs_header_lines)) {
            
            residuals_header_lines[1] - 3
            
          } else {
            
            corrs_header_lines[j + 1] - 3
          }

          corrs_lines <- lines_to_search[search_init:search_end]
          
          corr_vals <- regmatches(
            corrs_lines,
            gregexpr(MPLUS_ONE_OR_PARAM_AND_SIG, corrs_lines)
          )
          
          corr_vals_num <- lapply(
            corr_vals,
            function(param) {
              
              as.numeric(regmatches(param, regexpr(MPLUS_ONE_OR_PARAM, param)))
            }
          )
          corr_vals_sig <- lapply(
            corr_vals,
            function(param) grepl(MPLUS_SIGNIFICANT, param)
          )
          
          init_factor <- min(factor_indices)
          end_factor  <- max(factor_indices)
          
          for(factor in seq_along(corr_vals_sig)) {
            
            current_factor <- init_factor + factor - 1
            
            corr_est[
              current_factor,
              init_factor:min(current_factor, end_factor)
            ] <- corr_vals_num[[factor]]
            
            corr_sig[
              current_factor,
              init_factor:min(current_factor, end_factor)
            ] <- corr_vals_sig[[factor]]
          }
        }
        
        # The diagonal estimates of the correlation matrix should not have a
        #   significance value at all
        diag(corr_sig) <- NA

        # Residuals:
        residuals_est        <- numeric(length(item_names))
        names(residuals_est) <- item_names
        
        for(j in seq_along(residuals_header_lines)) {
          
          item_headers_line <- lines_to_search[residuals_header_lines[j] + 1]
          item_headers      <- regmatches(
            item_headers_line,
            gregexpr(MPLUS_VAR_NAME, item_headers_line)
          )[[1]]
          
          residuals_line <- lines_to_search[residuals_header_lines[j] + 3]
          
          residuals_est[item_headers] <- as.numeric(
            regmatches(
              residuals_line,
              gregexpr(MPLUS_ONE_OR_POS_PARAM, residuals_line)
            )[[1]]
          )
        }
        
        # Loadings' estimation errors:
        loadings_se <- matrix(
          ncol     = n_factors[i],
          nrow     = length(item_names),
          dimnames = list(NULL, factor_headers)
        )
        
        for(j in seq_along(loadings_se_header_lines)) {
          
          header_line    <- lines_to_search[loadings_se_header_lines[j] + 1]
          factor_indices <- as.numeric(
            regmatches(
              header_line,
              gregexpr(POS_INT_NUMBER, header_line)
            )[[1]]
          )
          
          search_init <- loadings_se_header_lines[j] + 3
          search_end  <- if (j == length(loadings_se_header_lines)) {
            
            corrs_se_header_lines - 3
            
          } else {
            
            loadings_se_header_lines[j + 1] - 3
          }
          
          loadings_lines <- lines_to_search[search_init:search_end]
          
          loadings_se_vals <- regmatches(
            loadings_lines,
            gregexpr(MPLUS_POS_PARAM, loadings_lines)
          )
          loadings_se[, factor_indices] <- t(
            sapply(loadings_se_vals, as.numeric)
          )
        }
        
        loadings_se <- data.frame(variable = item_names, loadings_se)
        
        # Correlations' estimation errors:
        corrs_se <- matrix(
          ncol     = n_factors[i],
          nrow     = n_factors[i],
          dimnames = list(factor_headers, factor_headers)
        )
        
        for(j in seq_along(corrs_se_header_lines)) {
          
          header_line    <- lines_to_search[corrs_se_header_lines[j] + 1]
          factor_indices <- as.numeric(
            regmatches(
              header_line,
              gregexpr(POS_INT_NUMBER, header_line)
            )[[1]]
          )
          
          search_init <- corrs_se_header_lines[j] + 3
          search_end  <- if (j == length(corrs_se_header_lines)) {
            
            residuals_se_header_lines[1] - 3
            
          } else {
            
            corrs_se_header_lines[j + 1] - 3
          }
          
          corrs_lines <- lines_to_search[search_init:search_end]
          
          corr_vals <- regmatches(
            corrs_lines,
            gregexpr(MPLUS_POS_PARAM, corrs_lines)
          )
          corr_vals_num <- lapply(corr_vals, as.numeric)
          
          init_factor <- min(factor_indices)
          end_factor  <- max(factor_indices)
          
          for(factor in seq_along(corr_vals_num)) {
            
            current_factor <- init_factor + factor - 1
            
            corrs_se[
              current_factor,
              init_factor:min(current_factor, end_factor)
            ] <- corr_vals_num[[factor]]
          }
        }
        
        # Residuals' estimation errors:
        residuals_se        <- numeric(length(item_names))
        names(residuals_se) <- item_names
        
        for(j in seq_along(residuals_se_header_lines)) {
          
          item_headers_line <- lines_to_search[residuals_se_header_lines[j] + 1]
          item_headers      <- regmatches(
            item_headers_line,
            gregexpr(MPLUS_VAR_NAME, item_headers_line)
          )[[1]]
          
          residuals_line <- lines_to_search[residuals_se_header_lines[j] + 3]
          
          residuals_se[item_headers] <- as.numeric(
            regmatches(
              residuals_line,
              gregexpr(MPLUS_POS_PARAM, residuals_line)
            )[[1]]
          )
        }
        
        parameters <- list(
          loadings  = list(
            estimates = loading_est,
            sig       = loading_sig,
            std_errs  = loadings_se),
            corrs     = list(
              estimates = corr_est,
              sig       = corr_sig,
              std_errs  = corrs_se
            ),
          residuals   = list(
            estimates = residuals_est,
            std_errs  = residuals_se
          )
        )
        
        complete_params <- append(complete_params, list(parameters))
        names(complete_params)[length(complete_params)] <- paste0(
          N_FACTORS_NAME_PREFFIX,
          n_factors[i]
        )
      }
      
      result <- list(efa = complete_params)
    }
  )
  
  return(result)
}
