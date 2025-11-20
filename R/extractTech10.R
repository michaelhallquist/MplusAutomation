#' Extract Technical 10 matrix from Mplus
#'
#' Function that extracts the Tech10 matrix
#'
#' @param outfiletext the text of the output file
#' @param filename The name of the file
#' @return An empty list
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
extractTech10 <- function(outfiletext, filename) {
  tech10Section <- getSection("TECHNICAL 10 OUTPUT", outfiletext)
  if (is.null(tech10Section)) return(list()) #no tech10 output
  
  # tech10_start <- grep("^\\s*TECHNICAL 10 OUTPUT\\s*$", outfiletext, perl = TRUE)
  # if (length(tech10_start) == 0) return(list()) # no tech10 output
  # 
  # tech_headers <- grep("^\\s*TECHNICAL \\d+ OUTPUT", outfiletext, perl = TRUE)
  # next_header <- tech_headers[tech_headers > tech10_start[1]]
  # tech10_end <- if (length(next_header) > 0) min(next_header) - 1 else length(outfiletext)
  # 
  # tech10_text <- outfiletext[tech10_start[1]:tech10_end]
  # tech10_text <- parse_into_sections(tech10_text)
  
  patternHeader <- "(LATENT CLASS )?PATTERN ([\\d\\w#_]+\\s*)+"
  tech10Headers <- c(
    "RESPONSE PATTERNS",
    "RESPONSE PATTERN FREQUENCIES AND CHI-SQUARE CONTRIBUTIONS",
    "MOST FREQUENT RESPONSE PATTERNS AND CHI-SQUARE CONTRIBUTIONS",
    "UNIVARIATE MODEL FIT INFORMATION",
    "UNIVARIATE DISTRIBUTION FIT",
    "BIVARIATE MODEL FIT INFORMATION",
    "UNIVARIATE DISTRIBUTION FIT FOR CLASS \\d+",
    "BIVARIATE DISTRIBUTIONS FIT FOR CLASS \\d+",
    paste0("BIVARIATE DISTRIBUTIONS FIT FOR ", patternHeader),
    paste0("UNIVARIATE DISTRIBUTION FIT FOR ", patternHeader)
  )
      #"MODEL FIT INFORMATION FOR .*", ")

  log_parse_issue <- function(area, err) {
    message("Problem parsing TECH10 ", area, " in ", filename, ": ", conditionMessage(err))
    NULL
  }
  
  normalize_pattern_label <- function(label) {
    label <- gsub("\\s*\\(.*\\)\\s*$", "", label, perl = TRUE)
    gsub("\\s+", ".", trimSpace(label), perl = TRUE)
  }

  parse_univariate_distribution <- function(section) {
    if (is.null(section) || length(section) == 0) return(NULL)

    section <- section[!grepl("^\\s*$", section, perl = TRUE)]
    # Variable lines look like "     U1" and are followed by "Category" rows
    var_lines <- grep("^\\s{5}\\S+", section, perl = TRUE)
    if (length(var_lines) == 0) return(NULL)

    res <- tryCatch({
      plyr::ldply(seq_along(var_lines), function(i) {
        v.line <- var_lines[i]
        end <- ifelse(i == length(var_lines), length(section), var_lines[i + 1] - 1)
        var <- trimSpace(section[v.line])
        lines.idxs <- (v.line + 1):end
        cat_lines <- grep("^\\s+Category \\d+", section[lines.idxs], perl = TRUE)
        plyr::ldply(cat_lines, function(cl) {
          vals <- unlist(strsplit(trimSpace(section[lines.idxs[cl]]), "\\s+", perl = TRUE))
          if (length(vals) < 4) return(NULL)
          data.frame(
            variable = var,
            category = suppressWarnings(as.numeric(vals[2])),
            observed = suppressWarnings(as.numeric(vals[3])),
            estimated = suppressWarnings(as.numeric(vals[4])),
            resid = suppressWarnings(as.numeric(ifelse(length(vals) >= 5, vals[5], NA))),
            stand_resid = suppressWarnings(as.numeric(ifelse(length(vals) >= 6, vals[6], NA))),
            stringsAsFactors = FALSE
          )
        })
      })
    }, error = function(e) log_parse_issue("univariate distribution", e))

    if (is.null(res) || nrow(res) == 0) return(NULL)
    rownames(res) <- NULL
    res
  }

  univar_cat <- NULL
  univar_stats <- NULL
  univar_overall_df <- NULL
  univar_distribution <- NULL
  univar_class_df <- NULL
  univar_pattern_df <- NULL
  bivarFitData <- NULL
  bivarFitStats <- NULL
  bivar_class_df <- NULL
  bivar_pattern_df <- NULL

  # Pre-compute header locations inside the TECH10 section so we can slice repeated subsections reliably
  header_patterns <- paste0("^\\s*", gsub(" ", "\\\\s+", tech10Headers, perl = TRUE), "\\s*$")
  tech10_sub_headers <- sort(unlist(lapply(header_patterns, function(pat) grep(pat, tech10Section, perl = TRUE))))

  next_header_end <- function(idx) {
    nh <- tech10_sub_headers[tech10_sub_headers > idx]
    if (length(nh) == 0) length(tech10Section) else nh[1] - 1L
  }

  # Univariate model fit information (overall)
  univar_section <- getSection("^\\s*UNIVARIATE MODEL FIT INFORMATION\\s*$", tech10Section, headers = tech10Headers)
  if (!is.null(univar_section) && length(univar_section) > 0) {
    overall_lines <- grep("^\\s+Overall", univar_section, perl = TRUE)

    if (length(overall_lines) > 0) {
      univar_overall_df <- plyr::ldply(univar_section[overall_lines], function(line) {
        pearson <- regexec("^\\s+Overall Univariate (Pearson|Log-Likelihood) Chi-Square\\s+(\\S+)", line, perl = TRUE)
        sig_resid <- regexec("^\\s+Overall Number of Significant Standardized Residuals\\s+(\\S+)", line, perl = TRUE)

        m <- regmatches(line, pearson)[[1]]
        if (length(m) > 0) {
          return(data.frame(stat = m[2], value = m[3], stringsAsFactors = FALSE))
        }

        m <- regmatches(line, sig_resid)[[1]]
        if (length(m) > 0) {
          return(data.frame(stat = "Significant", value = m[2], stringsAsFactors = FALSE))
        }
        NULL
      })
    }

    # Identify lines that start a variable; drop header text and any trailing overall lines
    var.lines <- grep("^\\s{5}\\S+\\s*$", univar_section, perl = TRUE)
    if (length(var.lines) > 0) {
      upper_bound <- if (length(overall_lines) > 0) min(overall_lines) - 1 else length(univar_section)
      trimmed <- univar_section[min(var.lines):upper_bound]
      var.lines <- grep("^\\s{5}\\S+\\s*$", trimmed, perl = TRUE)

      univar_cat <- tryCatch({
        plyr::ldply(seq_along(var.lines), function(i) {
          v.line <- var.lines[i]
          end <- ifelse(i == length(var.lines), length(trimmed), var.lines[i + 1] - 1)
          lines.idxs <- (v.line + 1):end
          var <- trimSpace(trimmed[v.line])

          cat.lines <- grep("^\\s+Category \\d+", trimmed[lines.idxs], perl = TRUE)
          plyr::ldply(cat.lines, function(cl) {
            vals <- unlist(strsplit(trimSpace(trimmed[lines.idxs[cl]]), "\\s+", perl = TRUE))
            if (length(vals) < 5) return(NULL)
            data.frame(var = var, category = paste(vals[1], vals[2]), h1 = vals[3], h0 = vals[4], z = vals[5], stringsAsFactors = FALSE)
          })
        })
      }, error = function(e) log_parse_issue("univariate model fit information", e))

      univar_stat_rows <- tryCatch({
        plyr::ldply(seq_along(var.lines), function(i) {
          v.line <- var.lines[i]
          end <- ifelse(i == length(var.lines), length(trimmed), var.lines[i + 1] - 1)
          lines.idxs <- (v.line + 1):end
          var <- trimSpace(trimmed[v.line])

          stat.lines <- trimmed[lines.idxs]
          plyr::ldply(list(
            list("Univariate Pearson Chi-Square\\s+(\\S+)", "Pearson"),
            list("Univariate Log-Likelihood Chi-Square\\s+(\\S+)", "Log-Likelihood"),
            list("Number of Significant Standardized Residuals\\s+(\\S+)", "Significant")
          ), function(pat) {
            m <- regmatches(stat.lines, regexec(pat[[1]], stat.lines, perl = TRUE))
            m <- Filter(function(x) length(x) > 0, m)
            if (length(m) == 0) return(NULL)
            data.frame(var = var, stat = pat[[2]], value = m[[1]][2], stringsAsFactors = FALSE)
          })
        })
      }, error = function(e) log_parse_issue("univariate chi-square statistics", e))

      if (!is.null(univar_stat_rows) && nrow(univar_stat_rows) > 0) {
        univar_stats <- tryCatch({
          reshape(univar_stat_rows, idvar = "var", timevar = "stat", direction = "wide",
                  varying = c("Pearson", "Log-Likelihood", "Significant"))
        }, error = function(e) log_parse_issue("univariate chi-square statistics", e))
        if (!is.null(univar_stats)) attr(univar_stats, "reshapeWide") <- NULL
      }
    }
  }

  # Overall univariate distribution (if present)
  univar_distribution <- parse_univariate_distribution(getSection("^\\s*UNIVARIATE DISTRIBUTION FIT\\s*$", tech10Section, headers = tech10Headers))

  # Univariate distribution fit for each class
  classHeaders <- grep("^\\s*UNIVARIATE DISTRIBUTION FIT FOR CLASS \\d+\\s*$", tech10Section, perl = TRUE)
  if (length(classHeaders) > 0) {
    classNames <- gsub("^\\s*UNIVARIATE DISTRIBUTION FIT FOR CLASS (\\d+)\\s*$", "\\1", tech10Section[classHeaders], perl = TRUE)
    class_frames <- vector("list", length(classNames))

    for (i in seq_along(classHeaders)) {
      start_idx <- classHeaders[i] + 1L
      end_idx <- next_header_end(classHeaders[i])
      if (start_idx > length(tech10Section)) next
      thisSub <- tech10Section[start_idx:end_idx]
      parsed <- parse_univariate_distribution(thisSub)
      if (is.null(parsed)) next
      lc <- suppressWarnings(as.integer(classNames[i]))
      if (is.na(lc)) lc <- classNames[i]
      parsed$LatentClass <- lc
      class_frames[[i]] <- parsed
    }

    class_frames <- Filter(Negate(is.null), class_frames)
    if (length(class_frames) > 0) {
      univar_class_df <- do.call(rbind, class_frames)
      rownames(univar_class_df) <- NULL
    }
  }

  bivarFit <- getSection("^\\s*BIVARIATE MODEL FIT INFORMATION\\s*$", tech10Section, headers = tech10Headers)

  if (!is.null(bivarFit) && length(bivarFit) > 0) {
    # Skip header lines
    if (length(bivarFit) > 6) bivarFit <- bivarFit[6:length(bivarFit)] else bivarFit <- bivarFit

    # Find the lines numbers with the variable name pairs
    #  (and add location for the end of text, for the ldply below)
    var.lines <- c(grep("^\\s{5}\\S{1,8}\\s+\\S{1,8}", bivarFit, perl = TRUE), length(bivarFit))

    # Iterate over each set of variable pairs to extract values
    if (length(var.lines) > 1) {
      res <- tryCatch({
        plyr::ldply(seq_len(length(var.lines) - 1), function(i) {
          # Get start and end line numbers for this pair
          v.line <- var.lines[i]
          end <- var.lines[i + 1] - 1

          # Split the var names into a vector
          vars <- unlist(strsplit(trimSpace(bivarFit[v.line]), "\\s+", perl = TRUE))
          if (length(vars) < 2) return(NULL)

          lines.idxs <- (v.line + 1):end
          lines.idxs <- lines.idxs[lines.idxs <= length(bivarFit)]

          # Parse remaining lines for this variable pair
          plyr::ldply(lines.idxs, function(line) {
            if (grepl("^\\s+Category \\d+\\s+Category \\d+", bivarFit[line], perl = TRUE)) {
              vals <- unlist(strsplit(trimSpace(bivarFit[line]), "\\s{2,}", perl = TRUE))
              if (length(vals) < 5) return(NULL)
              c(vars, vals)
            } else if (grepl("^\\s+Bivariate (Pearson|Log-Likelihood) Chi-Square", bivarFit[line], perl = TRUE)) {
              m <- unlist(regmatches(bivarFit[line], regexec("Bivariate (Pearson|Log-Likelihood) Chi-Square\\s+(\\S+)", bivarFit[line], perl = TRUE)))
              if (length(m) < 3) return(NULL)
              c(vars, "Summary", m[2:3], NA, NA)
            } else if (grepl("^\\s+Number of Significant Standardized Residuals", bivarFit[line], perl = TRUE)) {
              m <- unlist(regmatches(bivarFit[line], regexec("Number of Significant Standardized Residuals\\s+(\\S+)", bivarFit[line], perl = TRUE)))
              if (length(m) < 2) return(NULL)
              c(vars, "Summary", "Significant", m[2], NA, NA)
            }
          })
        })
      }, error = function(e) log_parse_issue("bivariate model fit information", e))
    } else res <- NULL

    if (!is.null(res) && nrow(res) > 0) {
      # Split out the "summary" lines from the bivariate data
      bivarFitData <- res[res$V3 != "Summary", , drop = FALSE]
      bivarFitStats <- res[res$V3 == "Summary", c("V1", "V2", "V4", "V5"), drop = FALSE]

      if (nrow(bivarFitData) > 0) {
        names(bivarFitData) <- c("var1", "var2", "cat1", "cat2", "h1", "h0", "z")
        rownames(bivarFitData) <- NULL
      } else bivarFitData <- NULL

      if (nrow(bivarFitStats) > 0) {
        names(bivarFitStats) <- c("var1", "var2", "stat", "value")
        bivarFitStats <- tryCatch({
          reshape(bivarFitStats, idvar = c("var1", "var2"), timevar = "stat", direction = "wide"
                  , varying = c("Pearson", "Log-Likelihood", "Significant"))
        }, error = function(e) log_parse_issue("bivariate chi-square statistics", e))
        if (!is.null(bivarFitStats)) {
          rownames(bivarFitStats) <- NULL
          attr(bivarFitStats, "reshapeWide") <- NULL
        }
      } else bivarFitStats <- NULL
    }
  }

  # Bivariate distributions for each class
  bivar_class_headers <- grep("^\\s*BIVARIATE DISTRIBUTIONS FIT FOR CLASS \\d+\\s*$", tech10Section, perl = TRUE)
  if (length(bivar_class_headers) > 0) {
    classNames <- gsub("^\\s*BIVARIATE DISTRIBUTIONS FIT FOR CLASS (\\d+)\\s*$", "\\1", tech10Section[bivar_class_headers], perl = TRUE)
    class_frames <- vector("list", length(classNames))

    for (i in seq_along(bivar_class_headers)) {
      sub_section <- getSection(paste0("^\\s*BIVARIATE DISTRIBUTIONS FIT FOR CLASS ", classNames[i], "\\s*$"), tech10Section, headers = tech10Headers)
      if (is.null(sub_section) || length(sub_section) == 0) next

      sub_section <- sub_section[!grepl("^\\s*$", sub_section, perl = TRUE)]
      # Drop the column header row if present
      sub_section <- sub_section[!grepl("^\\s*Variable\\s+Variable", sub_section, perl = TRUE)]

      var.lines <- c(grep("^\\s{5}\\S{1,8}\\s+\\S{1,8}", sub_section, perl = TRUE), length(sub_section) + 1L)

      class_df <- plyr::ldply(seq_len(length(var.lines) - 1L), function(j) {
        v.line <- var.lines[j]
        end <- var.lines[j + 1] - 1
        vars <- unlist(strsplit(trimSpace(sub_section[v.line]), "\\s+", perl = TRUE))

        plyr::ldply((v.line + 1):end, function(line_idx) {
          vals <- strsplit(trimSpace(sub_section[line_idx]), "\\s{2,}", perl = TRUE)[[1]]
          if (length(vals) < 6) return(NULL)
          data.frame(
            var1 = vars[1], var2 = vars[2],
            cat1 = vals[1], cat2 = vals[2],
            observed = as.numeric(vals[3]),
            estimated = as.numeric(vals[4]),
            resid = as.numeric(vals[5]),
            stand_resid = as.numeric(vals[6]),
            stringsAsFactors = FALSE
          )
        })
      })

      if (nrow(class_df) > 0) {
        rownames(class_df) <- NULL
        lc <- suppressWarnings(as.integer(classNames[i]))
        if (is.na(lc)) lc <- classNames[i]
        class_df$LatentClass <- lc
        class_frames[[i]] <- class_df
      }
    }

    class_frames <- Filter(Negate(is.null), class_frames)
    if (length(class_frames) > 0) {
      bivar_class_df <- do.call(rbind, class_frames)
      rownames(bivar_class_df) <- NULL
    }
  }
  
  # Univariate distributions for latent class patterns
  pattern_headers <- grep("^\\s*UNIVARIATE DISTRIBUTION FIT FOR (?:LATENT CLASS )?PATTERN ([\\d\\w#_]+\\s*)+\\s*$", tech10Section, perl = TRUE)
  if (length(pattern_headers) > 0) {
    patternNames <- sub("^\\s*UNIVARIATE DISTRIBUTION FIT FOR (?:LATENT CLASS )?PATTERN\\s+", "", tech10Section[pattern_headers], perl = TRUE)
    patternNames <- normalize_pattern_label(patternNames)
    pattern_frames <- vector("list", length(pattern_headers))
    
    for (i in seq_along(pattern_headers)) {
      start_idx <- pattern_headers[i] + 1L
      end_idx <- next_header_end(pattern_headers[i])
      if (start_idx > length(tech10Section)) next
      thisSub <- tech10Section[start_idx:end_idx]
      parsed <- parse_univariate_distribution(thisSub)
      if (is.null(parsed)) next
      parsed$LatentClassPattern <- patternNames[i]
      pattern_frames[[i]] <- parsed
    }
    
    pattern_frames <- Filter(Negate(is.null), pattern_frames)
    if (length(pattern_frames) > 0) {
      univar_pattern_df <- do.call(rbind, pattern_frames)
      rownames(univar_pattern_df) <- NULL
    }
  }
  
  # Bivariate distributions for latent class patterns
  bivar_pattern_headers <- grep("^\\s*BIVARIATE DISTRIBUTIONS FIT FOR (?:LATENT CLASS )?PATTERN ([\\d\\w#_]+\\s*)+\\s*$", tech10Section, perl = TRUE)
  if (length(bivar_pattern_headers) > 0) {
    rawNames <- sub("^\\s*BIVARIATE DISTRIBUTIONS FIT FOR (?:LATENT CLASS )?PATTERN\\s+", "", tech10Section[bivar_pattern_headers], perl = TRUE)
    patternNames <- normalize_pattern_label(rawNames)
    pattern_frames <- vector("list", length(patternNames))
    
    for (i in seq_along(bivar_pattern_headers)) {
      start_idx <- bivar_pattern_headers[i] + 1L
      end_idx <- next_header_end(bivar_pattern_headers[i])
      if (start_idx > length(tech10Section)) next
      sub_section <- tech10Section[start_idx:end_idx]
      
      sub_section <- sub_section[!grepl("^\\s*$", sub_section, perl = TRUE)]
      sub_section <- sub_section[!grepl("^\\s*Variable\\s+Variable", sub_section, perl = TRUE)]
      
      var.lines <- c(grep("^\\s{5}\\S{1,8}\\s+\\S{1,8}", sub_section, perl = TRUE), length(sub_section) + 1L)
      
      class_df <- plyr::ldply(seq_len(length(var.lines) - 1L), function(j) {
        v.line <- var.lines[j]
        end <- var.lines[j + 1] - 1
        vars <- unlist(strsplit(trimSpace(sub_section[v.line]), "\\s+", perl = TRUE))
        
        plyr::ldply((v.line + 1):end, function(line_idx) {
          vals <- strsplit(trimSpace(sub_section[line_idx]), "\\s{2,}", perl = TRUE)[[1]]
          if (length(vals) < 6) return(NULL)
          data.frame(
            var1 = vars[1], var2 = vars[2],
            cat1 = vals[1], cat2 = vals[2],
            observed = as.numeric(vals[3]),
            estimated = as.numeric(vals[4]),
            resid = as.numeric(vals[5]),
            stand_resid = as.numeric(vals[6]),
            stringsAsFactors = FALSE
          )
        })
      })
      
      if (nrow(class_df) > 0) {
        rownames(class_df) <- NULL
        class_df$LatentClassPattern <- patternNames[i]
        pattern_frames[[i]] <- class_df
      }
    }
    
    pattern_frames <- Filter(Negate(is.null), pattern_frames)
    if (length(pattern_frames) > 0) {
      bivar_pattern_df <- do.call(rbind, pattern_frames)
      rownames(bivar_pattern_df) <- NULL
    }
  }

  components <- list(
    bivar_model_fit_info = bivarFitData,
    bivar_chi_square = bivarFitStats,
    bivar_class = bivar_class_df,
    bivar_pattern = bivar_pattern_df,
    univar_model_fit_info = univar_cat,
    univar_chi_square = univar_stats,
    univar_overall = univar_overall_df,
    univar_distribution = univar_distribution,
    univar_class = univar_class_df,
    univar_pattern = univar_pattern_df
  )
  Filter(Negate(is.null), components)
}
