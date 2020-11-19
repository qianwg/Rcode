################################
# CATEGORICAL SUMMARY FUNCTION #
################################
cat.var <- function(var, 
                    strat      = NULL, 
                    dec        = 2, 
                    rownames   = as.vector(levels(as.factor(var))),
                    header     = deparse(substitute(var)),
                    ptype      = "None",
                    pname      = TRUE,
                    cat.rmstat = "None",
                    vspace     = TRUE,
                    output     = "plain") {
  
  #~~~~~~~~~~~#
  # No strata #
  #~~~~~~~~~~~#
  # Construct categorical summary 
  # Column percents only when strat=NULL
  if (is.null(strat)) {
    
    # Total counts and missings
    n <- length(which(is.na(var) == FALSE))
    tot <- as.matrix(table(var))
    tot[] <- paste0(tot, 
                    paste0(' (', 
                           format(round((tot / colSums(tot)) * 100, dec), 
                                  nsmall = dec), 
                           '%)')
                    )
    miss <- length(which(is.na(var) == T))
    
    # Combine total count (%) and missing values into a data frame
    out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 1), 
                                   n, 
                                   rep(NA, length(levels(as.factor(strat))) + 1),
                                   tot, 
                                   miss),
                             row.names = NULL),
                  as.character)
    
    # Paste a column of row headers and replace NA with '' for printing
    out <- cbind(as.vector(c(paste(header, '     '), '  Count', '  (%)',  
                             paste0('  ', rownames), '  Missing')), 
                 replace(out, is.na(out), ''))
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 'Overall')
  }
  
  #~~~~~~~~#
  # Strata #
  #~~~~~~~~#
  # Construct categorical summary
  # Row and column percents as default
  else if (!is.null(strat) & !('col' %in% cat.rmstat) & !('row' %in% cat.rmstat)) {
    cat <- as.matrix(table(var, as.factor(strat)))
    tot <- as.matrix(apply(cat, 1, sum))
    
    # Counts
    n <- as.matrix(apply(cat, 2, sum))
    n[] <- paste0(n,
                  paste0(' (',
                         format(round((n / sum(n)) * 100, dec),
                                nsmall = dec),
                         '%)')
                  )
    n <- cbind(t(n), apply(tot, 2, sum))
    
    # Stratified summary: count (row %)(col %)
    cat[] <- paste0(cat, 
                    paste0(' (', 
                           format(round((cat / rowSums(cat)) * 100, dec), 
                                  nsmall = dec),
                           '%)',
                           paste0(' (', 
                                  format(round(t(t(cat) / colSums(cat)) * 100, dec), 
                                         nsmall = dec),
                                  '%)')
                          )
                        )
    
    # Overall summary: count (row %)(col %)
    tot[] <- paste0(tot, 
                    paste0(' (', 
                           format(round((tot / rowSums(tot)) * 100, dec), 
                                  nsmall = dec), 
                           '%)',
                           paste0(' (', 
                                  format(round((tot / colSums(tot)) * 100, dec), 
                                         nsmall = dec), 
                                  '%)')
                          )
                        )
    
    # Aggregate missings
    miss <- c(aggregate(var, 
                        list(strat), 
                        function(x) {sum(is.na(x))})[, -1],
                        sum(aggregate(var, list(strat), 
                        function(x) {sum(is.na(x))})[, -1])
              )
    
    # cbind stratified and overall summaries
    # rbind counts and missings to create complete summary
    out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 1),
                                   n,
                                   rep(NA, length(levels(as.factor(strat))) + 1),
                                   cbind(cat, tot), 
                                   miss),
                             row.names = NULL),
                  as.character)
    out <- cbind(as.vector(c(paste(header, '     '), 
                             '  Count (%)', 
                             '  (Row %)(Col %)', 
                             paste0('  ', rownames), 
                             '  Missing')), 
                 replace(out, is.na(out), ''))
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 
                       as.vector(levels(as.factor(strat))), 
                       'Overall')
  }
  
  # Row percents only ('col' %in% cat.rmstat)
  # count (row %)
  else if (!is.null(strat) & ('col' %in% cat.rmstat)) {
    cat <- as.matrix(table(var, as.factor(strat)))
    tot <- as.matrix(apply(cat, 1, sum))
    
    # Counts
    n <- as.matrix(apply(cat, 2, sum))
    n[] <- paste0(n,
                  paste0(' (',
                         format(round((n / sum(n)) * 100, dec),
                                nsmall = dec), 
                         '%)'
                         )
                        )
    n <- cbind(t(n), apply(tot, 2, sum))
    
    # Stratified summary
    cat[] <- paste0(cat, 
                    paste0(' (', 
                           format(round((cat / rowSums(cat)) * 100, dec), 
                                  nsmall = dec), 
                           '%)'
                           )
                          )
    
    # Overall summary
    tot[] <- paste0(tot, 
                    paste0(' (', 
                           format(round((tot / rowSums(tot)) * 100, dec), 
                                  nsmall=dec), 
                           '%)'
                           )
                          )
    
    # Aggregate missings
    miss <- c(aggregate(var, 
                        list(strat), 
                        function(x) {sum(is.na(x))})[,-1],
                        sum(aggregate(var, list(strat), 
                        function(x) {sum(is.na(x))})[,-1])
              )
    
    # cbind stratified and overall summaries
    # rbind counts and missings to create complete summary
    out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 1),
                                   n,
                                   rep(NA, length(levels(as.factor(strat))) + 1),
                                   cbind(cat, tot), 
                                   miss),
                             row.names=NULL),
                  as.character)
    out <- cbind(as.vector(c(paste(header, '     '), 
                             '  Count (%)', 
                             '  (Row %)', 
                             paste0('  ', rownames), 
                             '  Missing')), 
                 replace(out, is.na(out), ''))
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 
                       as.vector(levels(as.factor(strat))), 
                       'Overall')
  }
  
  # Column percents only ('row' %in% cat.rmstat)
  # count (col %)
  else if (!is.null(strat) & ('row' %in% cat.rmstat)) {
    cat <- as.matrix(table(var, as.factor(strat)))
    tot <- as.matrix(apply(cat, 1, sum))
    
    # Counts
    n <- as.matrix(apply(cat, 2, sum))
    n[] <- paste0(n,
                  paste0(' (',
                         format(round((n / sum(n)) * 100, dec),
                                nsmall = dec), 
                         '%)')
                  )
    n <- cbind(t(n), apply(tot, 2, sum))
    
    # Stratified summary
    cat[] <- paste0(cat, 
                    paste0(' (', 
                           format(round(t(t(cat) / colSums(cat)) * 100, dec), 
                                  nsmall = dec), '%)'
                           )
                          )
    
    # Overall summary
    tot[] <- paste0(tot, 
                    paste0(' (', 
                           format(round((tot / colSums(tot)) * 100, dec), 
                                  nsmall = dec), '%)'
                           )
                          )
    
    # Missings
    miss <- c(aggregate(var, list(strat), function(x) {sum(is.na(x))})[, -1],
              sum(aggregate(var, list(strat), function(x) {sum(is.na(x))})[, -1]))
    
    # cbind stratified and overall summaries
    # rbind counts and missings to create complete summary
    out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 1),
                                   n,
                                   rep(NA, length(levels(as.factor(strat))) + 1),
                                   cbind(cat, tot), 
                                   miss),
                             row.names = NULL),
                  as.character)
    out <- cbind(as.vector(c(paste(header, '     '), 
                             '  Count (%)', 
                             '  (Col %)', 
                             paste0('  ', rownames), 
                             '  Missing')), 
                 replace(out, is.na(out), ''))
    rownames(out) <- NULL
    colnames(out) <- c('Variable',
                       as.vector(levels(as.factor(strat))), 
                       'Overall')
  } 
  
  # Include p-values without printing the name of the test
  if (pname == FALSE) {
    p <- c(stat.col(var, strat, ptype, pname = FALSE, output = output), 
           rep(NA, length(levels(as.factor(var))) + 3))
    out <- cbind(out, p)
    out <- replace(out, is.na(out), '')
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 
                       as.vector(levels(as.factor(strat))), 
                       'Overall', 
                       'p-value')
  }
  
  # Print the name of the test used beneath each p-value
  else if (pname == TRUE & !('count' %in% cat.rmstat)) {
    p <- c(stat.col(var, strat, ptype, pname = TRUE, output = output), 
           rep(NA, length(levels(as.factor(var))) + 2))
    out <-  cbind(out, p)
    out <- replace(out, is.na(out), '')
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 
                       as.vector(levels(as.factor(strat))), 
                       'Overall', 
                       'p-value')
  }
  
  # Print the name of the test used beneath each p-value when 
  # 'count' %in% cat.rmstat
  else if (pname == TRUE & ('count' %in% cat.rmstat)) {
    p <- c(stat.col(var, strat, ptype, pname = TRUE, output = output), 
           rep(NA, length(levels(as.factor(var))) + 2))
    p <- replace(p, 3, p[2])
    out <-  cbind(out, p)
    out <- replace(out, is.na(out), '')
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 
                       as.vector(levels(as.factor(strat))), 
                       'Overall', 
                       'p-value')
  }
  
  # Remove missing values row if requested
  if ('miss' %in% cat.rmstat) {
    out <- out[-dim(out)[1], ]
  }
  
  # Remove count (%) row if requested
  if (is.null(strat) & ('count' %in% cat.rmstat)) {
    out <- out[-c(2,3), ]
  }
  else if (!is.null(strat) & ('count' %in% cat.rmstat)) {
    out <- out[-2, ]
  } 
  out[grepl("NA", out)] <- "-"
  out[grepl("NaN", out)] <- "-"
  out[grepl("Inf", out)] <- "-"
  
  # Add vertical spacing between variables for plain and HTML output
  if (vspace == TRUE & output == "plain") {
    out <- rbind(out, rep(" ", dim(out)[2]))
  }
  if (vspace == TRUE & output == "html") {
    out <- rbind(out, rep("&nbsp;", dim(out)[2]))
  }
  return(data.frame(out))
}

###############################
# CONTINUOUS SUMMARY FUNCTION #
###############################

cont.var <- function(var, 
                     strat       = NULL, 
                     dec         = 2, 
                     header      = deparse(substitute(var)), 
                     ptype       = 'None',
                     pname       = TRUE,
                     cont.rmstat = 'None',
                     vspace      = TRUE,
                     output      = "plain") {
  
  #~~~~~~~~~~~#
  # No strata #
  #~~~~~~~~~~~#
  # Continuous summary with no strata
  if (is.null(strat)) {
    
    # Counts
    n.tot <- length(which(is.na(var) == FALSE))
    
    # Mean (standard deviation)
    mean.sd.tot <- paste(
                      format(
                        round(mean(var, na.rm = TRUE), dec), 
                      nsmall = dec), 
                      paste('(', 
                        format(
                          round(sd(var, na.rm = TRUE), dec), 
                        nsmall = dec), ')', 
                      sep='')
                      )
    
    # Median (interquartile range)
    med.iqr.tot <- paste(
                      format(
                        round(median(var, na.rm = TRUE), dec), 
                      nsmall = dec), 
                      paste('(', 
                        format(
                          round(IQR(var, na.rm = TRUE), dec), 
                        nsmall = dec), ')', 
                      sep = '')
                      )
    
    # 25th percentile, 75th percentile
    q1.q3.tot <- paste(
                    format(
                      round(quantile(var, 0.25, na.rm = TRUE), dec), 
                    nsmall = 2), ', ',
                    format(
                      round(quantile(var, 0.75, na.rm = TRUE), dec)), 
                    sep = '')
    
    # Minimum, maximum
    min.max.tot <- paste(
                      format(
                        round(min(var, na.rm = TRUE), dec), 
                        nsmall = dec), ', ',
                      format(
                        round(max(var, na.rm = TRUE), 2)), 
                      sep = '')
    
    # Missings
    miss.tot <- length(which(is.na(var) == TRUE))
    
    # rbind summary statistics
    # cbind row headers and replace NAs with '' for printing
    out <- sapply(data.frame(rbind(NA, 
                                   n.tot,
                                   mean.sd.tot, 
                                   med.iqr.tot, 
                                   q1.q3.tot, 
                                   min.max.tot, 
                                   miss.tot)),
                  as.character)
    out <- cbind(as.vector(c(paste(header, '     '),  
                             '   Count', 
                             '   Mean (SD)', 
                             '   Median (IQR)', 
                             '   Q1, Q3', 
                             '   Min, Max', 
                             '   Missing')), 
                 replace(out, is.na(out), ''))
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 'Overall')
  }
  
  #~~~~~~~~#
  # Strata #
  #~~~~~~~~#
  # Continuous summary with strata
  else {
    if (!is.null(strat)) {
      
      # Mean (standard deviation)
      mean.sd <- format(
                    round(aggregate(var, 
                                    list(strat), 
                                    mean, 
                                    na.rm = TRUE)[, -1], 
                          dec), 
                 nsmall = dec)
      mean.sd[] <- paste0(mean.sd, 
                          paste0(' (', 
                                 (format(
                                   round(aggregate(var, 
                                                   list(strat), 
                                                   sd, 
                                                   na.rm = TRUE)[, -1], 
                                         dec), 
                                   nsmall = dec)),
                                 ')')
                          )
      
      # Median (interquartile range)
      med.iqr <- format(
                    round(aggregate(var, 
                                    list(strat), 
                                    median, 
                                    na.rm = TRUE)[, -1], 
                          dec), 
                 nsmall = dec)
      med.iqr[] <- paste0(med.iqr, 
                          paste0(' (', 
                                 (format(
                                   round(aggregate(var, 
                                                   list(strat), 
                                                   IQR, 
                                                   na.rm = TRUE)[, -1], 
                                         dec), 
                                   nsmall = dec)), 
                                 ')')
                          )
      
      # 25th percentile, 75th percentile
      q1.q3 <- format(
                  round(aggregate(var, 
                                  list(strat), 
                                  quantile, 
                                  0.25, 
                                  na.rm=T)[,-1], 
                        dec), 
               nsmall=dec)
      q1.q3[] <- paste0(q1.q3, 
                        paste0(', ', 
                               (format(
                                 round(aggregate(var, 
                                                 list(strat), 
                                                 quantile, 
                                                 0.75, 
                                                 na.rm = TRUE)[, -1], 
                                       dec), 
                                 nsmall=dec))
                          )
                        )
      
      # Minimum, maximum
      min.max <- format(
                    round(aggregate(var, 
                                    list(strat), 
                                    min, 
                                    na.rm=T)[,-1], 
                          dec), 
                  nsmall=dec)
      min.max[] <- paste0(min.max, 
                          paste0(', ', 
                            (format(
                                round(aggregate(var, 
                                                list(strat), 
                                                max, 
                                                na.rm=T)[,-1],
                                      dec), 
                                   nsmall=dec))
                                  )
                                )
      
      # Missings
      miss <- aggregate(var, 
                        list(strat), 
                        function(x) {sum(is.na(x))})[,-1]
      
      # Counts
      n <- as.vector(aggregate(var, 
                               list(strat), 
                               length)[,-1]) - as.vector(miss)
      
      # Stratified summary
      cont <- rbind(n, 
                    mean.sd, 
                    med.iqr, 
                    q1.q3, 
                    min.max, 
                    miss)
      
      # Overall summaries for total column
      n.tot <- sum(n)
      
      # Overall mean (standard deviation)
      mean.sd.tot <- paste(
                        format(
                          round(mean(var[!is.na(strat)], na.rm = TRUE), dec), 
                        nsmall = dec), 
                        paste('(', 
                          format(
                            round(sd(var[!is.na(strat)], na.rm = TRUE), dec), 
                          nsmall = dec), ')', 
                        sep = '')
                        )
      
      # Overall median (interquartile range)
      med.iqr.tot <- paste(
                        format(
                          round(median(var[!is.na(strat)], na.rm = TRUE), dec), 
                        nsmall = dec), 
                        paste('(', 
                          format(
                            round(IQR(var[!is.na(strat)], na.rm = TRUE), dec), 
                          nsmall = dec), ')', 
                        sep = '')
                        )
      
      # Overall 25th percentile, 75th percentile
      q1.q3.tot <- paste(
                      format(
                        round(quantile(var[!is.na(strat)], 0.25, na.rm = TRUE), dec), 
                      nsmall = dec), ', ',
                      format(
                        round(quantile(var[!is.na(strat)], 0.75, na.rm = TRUE), dec),
                      nsmall = dec), 
                    sep = '') 
      
      # Overall minimum, maximum
      min.max.tot <- paste(
                        format(
                          round(min(var[!is.na(strat)], na.rm = TRUE), dec), 
                        nsmall = dec), ', ',
                        format(
                          round(max(var[!is.na(strat)], na.rm = TRUE), dec),
                        nsmall = dec), 
                      sep = '')
      
      # Overall missings
      miss.tot = sum(miss)
      
      # Total summary column
      tot <- rbind(n.tot, 
                   mean.sd.tot, 
                   med.iqr.tot, 
                   q1.q3.tot, 
                   min.max.tot, 
                   miss.tot)
      
      # cbind stratified and total columns
      # rbind counts and missings to create complete summary
      out <- sapply(data.frame(rbind(rep(NA, length(levels(as.factor(strat))) + 1),
                                     cbind(cont, tot))),
                    as.character)
      # cbind vector of row headers and replace NAs with '' to show up as a blank cell when printed
      out <- cbind(as.vector(c(paste(header, '     '), 
                               '   Count', 
                               '   Mean (SD)', 
                               '   Median (IQR)', 
                               '   Q1, Q3', 
                               '   Min, Max', 
                               '   Missing')), 
                   replace(out, is.na(out), ''))
      rownames(out) <- NULL
      colnames(out) <- c('Variable', 
                         as.vector(levels(as.factor(strat))), 
                         'Overall')
    }
  }
  
  # Print p-values without the name of the test used
  if (pname == FALSE) {
    p <- c(stat.col(var, strat, ptype, pname = FALSE, output = output), rep(NA, 6))
    out <- cbind(out, p)
    out <- replace(out, is.na(out), '')
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 
                       as.vector(levels(as.factor(strat))), 
                       'Overall', 
                       'p-value')    
    }
  
  # Print p-values with the name of the test used (default)
  else if (pname == TRUE & !('count' %in% cont.rmstat)) {
    p <- c(stat.col(var, strat, ptype, pname = TRUE, output = output), rep(NA, 5))
    out <-  cbind(out, p)
    out <- replace(out, is.na(out), '')
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 
                       as.vector(levels(as.factor(strat))), 
                       'Overall', 
                       'p-value')
    }
  
  # Print p-values with the name of the test used if 'count' %in% cont.rmstat
  else if (pname == TRUE & ('count' %in% cont.rmstat)) {
    p <- c(stat.col(var, strat, ptype, pname = TRUE, output = output), rep(NA, 5))
    p <- replace(p, 3, p[2])
    out <-  cbind(out, p)
    out <- replace(out, is.na(out), '')
    rownames(out) <- NULL
    colnames(out) <- c('Variable', 
                       as.vector(levels(as.factor(strat))), 
                       'Overall', 
                       'p-value')
    }
  
  # Remove summary statistics specified in cont.rmstat
  if (!(any(cont.rmstat == 'None'))) {
    out <- out[-((which(c('count', 
                          'meansd', 
                          'mediqr', 
                          'q1q3', 
                          'minmax', 
                          'miss') 
                        %in% cont.rmstat)) + 1), ]
    }
  
  # Replace uncalculated quantities with dashes
  out[grepl("NA", out)] <- "-"
  out[grepl("NaN", out)] <- "-"
  out[grepl("Inf", out)] <- "-"
  
  # Add vertical spacing between variables in plain and HTML output
  if (vspace == TRUE & output == "plain") {
    out <- rbind(out, rep(" ", dim(out)[2]))
    }
  if (vspace == TRUE & output == "html") {
    out <- rbind(out, rep("&nbsp;", dim(out)[2]))
    }
  return(data.frame(out))
}

################
# LaTeX OUTPUT #
################
out.latex <- function(tab, 
                      colnames     = NULL, 
                      header.style = "bold", 
                      factor.style = "bold", 
                      stat.style   = "plain") {
  named <- as.vector(tab[,1])
  
  # Tag statistic names and variable names and levels separately for 
  # different formatting
  # Formatting discrepancies can arise if factor levels have names
  # that include the name of one of the statistics below
  tags <- grepl('^ ', named)
  tags2 <- (grepl('Count', named, fixed = TRUE) | 
              grepl('%', named, fixed = TRUE) | 
              grepl('Missing', named, fixed = T) == TRUE | 
              grepl('Mean', named, fixed = TRUE) |
              grepl('Median', named, fixed = TRUE) | 
              grepl('Q1', named, fixed = TRUE) |
              grepl('Min', named, fixed = TRUE))
  
  # Apply style to variable headers
  if (header.style == "bold") {
    named <- ifelse(tags == FALSE, 
                    paste("\\textbf{", named, "}", sep = ""), 
                    named)
    }
    else if (header.style == "italic") {
      named <- ifelse(tags == FALSE, 
                      paste("\\textit{", named, "}", sep = ""), 
                      named)
      }
      else if (header.style == "bolditalic") {
        named <- ifelse(tags == FALSE, 
                        paste("\\textbf{\\textit{", named, "}}", sep = ""), 
                        named)
        }
        else if (header.style == "plain") {}
  
  # Apply style to factor levels
  if (factor.style == "bold") {
    named <- ifelse((tags == TRUE & tags2 == FALSE), 
                    paste("\\textbf{", named, "}", sep = ""), 
                    named)
    }
    else if (factor.style == "italic") {
      named <- ifelse((tags == TRUE & tags2 == FALSE), 
                      paste("\\textit{", named, "}", sep = ""), 
                      named)
      }
      else if (factor.style == "bolditalic") {
        named <- ifelse((tags == TRUE & tags2 == FALSE), 
                        paste("\\textbf{\\textit{", named, "}}", sep = ""), 
                        named)
        }
        else if (factor.style == "plain") {}
  
  # Apply style to statistics
  if (stat.style == "bold") {
    named <- ifelse(tags2 == TRUE, 
                    paste("\\textbf{", named, "}", sep = ""), 
                    named)
    }
    else if (stat.style == "italic") {
      named <- ifelse(tags2 == TRUE, 
                      paste("\\textit{", named, "}", sep = ""), 
                      named)
      }
      else if (stat.style == "bolditalic") {
        named <- ifelse(tags2 == TRUE, 
                        paste("\\textbf{\\textit{", named, "}}", sep = ""), 
                        named)
        }
        else if (stat.style == "plain") {}
  #named <- ifelse(tags2==F,
  #                paste("\\textbf{", named, "}", sep=''),
  #                named)
  named <- c(ifelse(tags == F,
              paste("\\vspace*{0.1cm}", 
              paste("\\", "\\", sep = ''), 
              named),
              paste("\\hskip .5cm", named, sep = ' '))
             )
  
  output <- apply(cbind(named, tab[,2:dim(tab)[2]]), 
                  2, 
                  function(x) gsub('%', '\\\\%', x)
                  )
  output <- gsub('<', '\\textless ', output)
  output <- gsub('>', '\\textgreater ', output)
  colnames(output) <- colnames
  
  print(xtable(output, 
               align = paste(c('l', 
                               'l', 
                               rep('r', dim(output)[2] - 1)), 
                             collapse='')), 
               type = "latex", 
               sanitize.text.function = function(x){x}, 
               include.rownames = F)
}

###############
# HTML OUTPUT #
###############
# For categorical variables, formatting may be a problem if categories 
# have similar names to available summary statistics
out.html <- function (tab, 
                      colnames, 
                      stripe       = TRUE, 
                      stripe.col   ='#F7F7F7', 
                      header.style = "bold", 
                      factor.style = "bold", 
                      stat.style   = "plain",
                      caption, 
                      footer, 
                      tspanner, 
                      n.tspanner, 
                      cgroup, 
                      n.cgroup, 
                      col.columns = "none", 
                      nowrap      = TRUE,
                      vspace      = TRUE) {
  
  # Define the column of row names
  named <- as.vector(tab[, 1])
  
  # Identify names with whitespace before text as TRUE
  # =>variable names are FALSE
  tags <- grepl("^ ", named)
  
  # Identify strings with summary stat name components
  tags2 <- (grepl("Count", named, fixed = TRUE) | 
              grepl("%", named, fixed = TRUE) | 
              grepl("Missing", named, fixed = TRUE) == TRUE | 
              grepl("Mean", named, fixed = TRUE) | 
              grepl("Median", named, fixed = TRUE) | 
              grepl("Q1", named, fixed = TRUE) | 
              grepl("Min", named, fixed = TRUE))
  
  # Indent all row names except variable name by 3 spaces
  named <- ifelse(tags == TRUE, 
                  paste("&nbsp;", "&nbsp;", "&nbsp;", named, sep = " "), 
                  named)
  
  # Apply style to variable headers
  if (header.style == "bold") {
    named <- ifelse(tags == FALSE, 
                    paste("<b>", named, "</b>", sep = ""), 
                    named)
    }
    else if (header.style == "italic") {
      named <- ifelse(tags == FALSE, 
                      paste("<i>", named, "</i>", sep = ""), 
                      named)
      }
      else if (header.style == "bolditalic") {
        named <- ifelse(tags == FALSE, 
                        paste("<b><i>", named, "</i></b>", sep = ""), 
                        named)
        }
        else if (header.style == "plain") {}
  
  # Apply style to factor levels
  if (factor.style == "bold") {
    named <- ifelse((tags == TRUE & tags2 == FALSE), 
                     paste("<b>", named, "</b>", sep = ""), 
                     named)
    }
    else if (factor.style == "italic") {
      named <- ifelse((tags == TRUE & tags2 == FALSE), 
                       paste("<i>", named, "</i>", sep = ""), 
                       named)
      }
      else if (factor.style == "bolditalic") {
        named <- ifelse((tags == TRUE & tags2 == FALSE), 
                         paste("<b><i>", named, "</i></b>", sep = ""), 
                         named)
        }
        else if (factor.style == "plain") {}
  
  # Apply style to statistics
  if (stat.style == "bold") {
    named <- ifelse(tags2 == TRUE, 
                    paste("<b>", named, "</b>", sep = ""), 
                    named)
    }
    else if (stat.style == "italic") {
      named <- ifelse(tags2 == TRUE, 
                      paste("<i>", named, "</i>", sep = ""), 
                      named)
      }
      else if (stat.style == "bolditalic") {
        named <- ifelse(tags2 == TRUE, 
                        paste("<b><i>", named, "</i></b>", sep = ""), 
                        named)
        }
        else if (stat.style == "plain") {}
  
  output <- cbind(named, as.vector(tab[, 2:dim(tab)[2]]))
  
  # Option for light zebra striping of every other variable (default)
  if (stripe == FALSE) {   
    # Left justify row names; right justify all other columns
    # Use css.cell to add whitespace between columns
    return(htmlTable(as.matrix(output), 
                     rnames      = F, 
                     header      = colnames, 
                     align       = c("l", rep("r", ncol(output) - 1)), 
                     css.cell    = "padding-left: .2em; padding-right: 2em;",
                     caption     = caption,
                     tfoot       = footer,
                     tspanner    = tspanner,
                     n.tspanner  = n.tspanner,
                     cgroup      = cgroup,
                     n.cgroup    = n.cgroup,
                     col.columns = col.columns))
  }
  
  # Option to remove zebra striping for an all-white background
  else if (stripe == TRUE) {
    
    # Get row lengths for each variable group in the table
    if (vspace == FALSE) {
      v <- (rle(tags)$length)[c(FALSE, TRUE)] + 1
      }
      else if (vspace == TRUE) {
        v <- (rle(tags)$length)[c(FALSE, TRUE)] + 1
        }
    
    # Specify colors for striping
    color <- c('#FFFFFF', stripe.col)
    
    # Left justify row names; right justify all other columns
    # Use css.cell to add whitespace between columns
    return(htmlTable(as.matrix(output), 
                     rnames=FALSE, 
                     header=colnames, 
                     col.rgroup  = unlist(mapply(rep, x=color, times=v), 
                                          use.names=FALSE),
                     align       = c("l", rep("r", ncol(output) - 1)), 
                     css.cell    = "padding-left: .2em; padding-right: 2em;",
                     caption     = caption,
                     tfoot       = footer,
                     tspanner    = tspanner,
                     n.tspanner  = n.tspanner,
                     cgroup      = cgroup,
                     n.cgroup    = n.cgroup,
                     col.columns = col.columns))    
  }
  
  # Include options here to replace symbols with HTML tags and prevent
  # text wrapping to the next line within the table
  #if (nowrap==TRUE) {
  #Replace symbols with HTML entities
  #htmltab <- gsub('<', '&lt;', htmltab)
  #htmltab <- gsub('>', '&gt;', htmltab)
  #Prevent within-cell text wrapping
  #htmltab <- gsub('<td', '<td nowrap="nowrap"; ', htmltab)
  #return(htmltab)
  #}
  #else if (nowrap==FALSE) {
  #Replace symbols with HTML entities
  #htmltab <- gsub('<', '&lt;', htmltab)
  #htmltab <- gsub('>', '&gt;', htmltab)
  #return(htmltab)
  #}
}

#####################
# PLAIN TEXT OUTPUT #
#####################
out.plain <- function(tab, colnames = NULL) {
  output <- cbind(format(as.vector(tab[, 1]), justify = 'left'), 
                  data.frame(tab[, 2:dim(tab)[2]])
                  )
  colnames(output) = colnames
  return(print(output, row.names = FALSE))
}


#############################
# VECTORIZED TABLE FUNCTION #
#############################
make.table <- function(dat,
                       # Categorical variable options
                       cat.varlist  = NULL,
                       cat.header   = names(dat[, cat.varlist]),
                       cat.rownames = lapply(lapply(
                                        dat[, cat.varlist], factor), 
                                        levels),
                       cat.ptype    = "None",
                       
                       # Continuous variable options
                       cont.varlist = NULL, 
                       cont.header  = names(dat[, cont.varlist]),
                       cont.ptype   = "None",
                       
                       # Overall table options
                       strat        = NULL,
                       cat.rmstat   = "None",
                       cont.rmstat  = "None",
                       dec          = 2,
                       pname        = TRUE,
                       colnames     = NULL,
                       output       = "plain",
                       vspace       = TRUE,
                       varorder     = "data",
                       
                       # HTML defaults
                       stripe       = TRUE,
                       stripe.col   = '#F7F7F7',
                       header.style = "bold",
                       factor.style = "bold",
                       stat.style   = "plain",
                       nowrap       = TRUE,
                       
                       # HTML options passed directly to htmlTable
                       caption,
                       footer,
                       tspanner,
                       n.tspanner,
                       cgroup,
                       n.cgroup,
                       col.columns  = "none") {
  
  #----------#
  # Warnings #
  #----------#
  # A data frame must be supplied
  if (missing(dat) | (!is.data.frame(dat))) {
    warning("A data frame object must be provided in dat = .")
  }
  
  # Lists of variables, headers, and rownames must be equal length
  if (!(length(cat.varlist) == length(cat.header))) {
    warning("cat.varlist and cat.header must be the same length.")
  }
  
  if (!(length(cat.varlist) == length(cat.rownames))) {
    warning("cat.varlist and cat.rownames must be the same length.")
  }
  
  if (!(length(cont.varlist) == length(cont.header))) {
    warning("cont.varlist and cont.header must be the same length.")
  }
  
  #----------------------------#
  # Re-order data as requested #
  #----------------------------#
  
  #Customize variable re-ordering by variable name
  #if (!is.null(varorder.custom)) {
  #  dat <- dat[c(varorder.custom)]
  #}
  
  #Re-order variables alphabetically
  #if (varorder.abc=TRUE) {
  #  dat <- dat[order(names(dat))]
  #}
  
  #Replace any NaN values with NA and attach data
  #NaNs create formatting problems
  #dat[is.na(dat)] <- NA
  #attach(dat)
  
  #-----------#
  # No strata #
  #-----------#
  if (is.null(strat)) {
    cat.strat = rep(list(strat), length(cat.varlist))
    cont.strat = rep(list(strat), length(cont.varlist))
    strat.miss = 0
    strat.rem = 0
  }
  
  #--------#
  # Strata #
  #--------#
  else if (!is.null(strat)) {
    
    cat.strat = rep(list(
      interaction(
        sapply(strat, 
               FUN = get, 
               pos = dat, 
               simplify = FALSE, 
               USE.NAMES = TRUE))), 
      length(cat.varlist))
    
    cont.strat = rep(list(
      interaction(
        sapply(strat, 
               FUN = get, 
               pos = dat, 
               simplify = FALSE, 
               USE.NAMES = TRUE))), 
      length(cont.varlist))
    
    strat.miss = lapply(
      sapply(strat, 
             FUN = get, 
             pos = dat, 
             simplify = FALSE, 
             USE.NAMES = TRUE), 
      function(x) sum(is.na(x)))
    
    strat.rem = sum(is.na(
      interaction(
        sapply(strat, 
               FUN = get, 
               pos = dat, 
               simplify = FALSE, 
               USE.NAMES = TRUE))))
    }
  
  #----------------------#
  # Removed observations #
  #----------------------#
  if (!is.null(strat) & strat.rem > 0) {
    print(paste("Total observations removed from table:", strat.rem, sep = ' '))
    print("Summary of total missing stratification variable(s):")
    print(strat.miss)
    footer.miss <- paste(strat.rem, 
                         "observations removed due to missing values in 
                         stratifying variable(s)", 
                         sep = ' ')
    }
    else {footer.miss <- paste('')}
  
  #------------------#
  # Categorical only #
  #------------------#
  if (is.null(cont.varlist)) {
    cats <- mapply(cat.var, 
                   var        = sapply(cat.varlist, 
                                       FUN = get, 
                                       pos = dat, 
                                       simplify = FALSE, 
                                       USE.NAMES = TRUE), 
                   strat      = cat.strat, 
                   cat.rmstat = cat.rmstat,
                   dec        = dec, 
                   rownames   = cat.rownames,
                   header     = cat.header,
                   ptype      = cat.ptype,
                   pname      = pname,
                   vspace     = TRUE,
                   SIMPLIFY   = FALSE)
    
    # Order variables in the table as requested
    # By variable order in dataset
    if (varorder == "data") {
      tab <- do.call(rbind, 
                     (c(cats))[order(
                       match(names(c(cats)), names(dat))
                     )])
      }
    # Alphabetically
      else if (varorder == "abc") {
        tab <- do.call(rbind, 
                       (c(cats))[order(
                         match(names(c(cats)), sort(names(dat)))
                       )])
        }
    
    # Remove p-value column if no p-values are calculated
    if (all(cat.ptype == "None")) { 
      tab <- tab[, -dim(tab)[2]]
    }
    
    # Replace uncalculated values with dashes
    tab[grepl("NaN", tab)] <- "-"
    tab[grepl("NA", tab)] <- "-"
    tab[grepl("-Inf", tab)] <- "-"
  }
  
  #-----------------#
  # Continuous only #
  #-----------------#
  else if (is.null(cat.varlist)) {
    conts <- mapply(cont.var, 
                    var         = sapply(cont.varlist, 
                                         FUN = get, 
                                         pos = dat, 
                                         simplify = F, 
                                         USE.NAMES = T),
                    strat       = cont.strat,
                    cont.rmstat = cont.rmstat,
                    dec         = dec,
                    header      = cont.header,
                    ptype       = cont.ptype,
                    pname       = pname,
                    vspace      = TRUE,
                    SIMPLIFY    = FALSE)
    
    # Order variables in the table as requested
    # By variable order in dataset
    if (varorder == "data") {
      tab <- do.call(rbind, 
                     (c(conts))[order(
                       match(names(c(conts)), names(dat))
                     )])
      }
    # Alphabetically
      else if (varorder == "abc") {
        tab <- do.call(rbind, 
                      (c(conts))[order(
                         match(names(c(conts)), sort(names(dat)))
                       )])
        }
    
    # Remove p-value column if none are calculated
    if (all(cont.ptype == "None")) { 
      tab <- tab[, -dim(tab)[2]]
    }
    
    # Replace uncalculated values with hyphens
    tab[grepl("NaN", tab)] <- "-"
    tab[grepl("NA", tab)] <- "-"
    tab[grepl("-Inf", tab)] <- "-"
  }
  
  #----------------------------#
  # Categorical and continuous #
  #----------------------------#
  else {
    # Apply cat.var function to list of categorical variables
    cats <- mapply(cat.var, 
                   var        = sapply(cat.varlist, 
                                       FUN = get, 
                                       pos = dat, 
                                       simplify = FALSE, 
                                       USE.NAMES = TRUE), 
                   strat      = cat.strat, 
                   cat.rmstat = cat.rmstat,
                   dec        = dec, 
                   rownames   = cat.rownames,
                   header     = cat.header,
                   ptype      = cat.ptype,
                   pname      = pname,
                   vspace     = TRUE,
                   SIMPLIFY   = FALSE)
    
    # Apply cont.var function to list of continuous variables
    conts <- mapply(cont.var, 
                    var         = sapply(cont.varlist, 
                                         FUN = get, 
                                         pos = dat, 
                                         simplify = FALSE, 
                                         USE.NAMES = TRUE),
                    strat       = cont.strat,
                    cont.rmstat = cont.rmstat,
                    dec         = dec,
                    header      = cont.header,
                    ptype       = cont.ptype,
                    pname       = pname,
                    vspace      = TRUE,
                    SIMPLIFY    = FALSE)
    
    # Order variables in the table as requested
    # By variable order in dataset
    if (varorder == "data") {
      tab <- do.call(rbind, 
                     (c(cats, conts))[order(
                       match(names(c(cats, conts)), names(dat))
                     )])
      }
    # Alphabetically
      else if (varorder == "abc") {
        tab <- do.call(rbind, 
                      (c(cats, conts))[order(
                        match(names(c(cats, conts)), sort(names(dat)))
                      )])
        }
    
    # Remove p-value column if no p-values are calculated
    if (all(cat.ptype == "None" & all(cont.ptype == "None"))) { 
      tab <- tab[, -dim(tab)[2]]
    }
    
    # Replace uncalculated values with hyphens
    tab[grepl("NaN", tab)] <- "-"
    tab[grepl("NA", tab)] <- "-"
    tab[grepl("-Inf", tab)] <- "-"
  }
  
  # Define column names
  if (!is.null(colnames)) {
    colnames <- colnames
    }
    else if (is.null(colnames)) {
      colnames <- colnames(tab)
      }
  
  # Define output type
  if (output == 'plain') {
    out.plain(tab, 
              colnames = colnames)
    }
    else if (output == 'html') {
      out.html(tab, 
               colnames     = colnames, 
               stripe       = stripe, 
               stripe.col   = stripe.col,
               header.style = header.style, 
               factor.style = factor.style,
               stat.style   = stat.style, 
               caption,
               footer,
               tspanner,
               n.tspanner,
               cgroup,
               n.cgroup,
               col.columns  = col.columns,
               nowrap       = nowrap)
        }
        else if (output == 'latex') {
          out.latex(tab, 
                    colnames     = colnames, 
                    header.style = header.style, 
                    factor.style = factor.style, 
                    stat.style   = stat.style)
          } 
        }

################################
# MINIMAL INPUT TABLE FUNCTION #
################################

quick.table <- function(dat,
                        strat      = NULL,
                        dec        = 2,
                        colnames   = NULL,
                        output     = 'plain',
                        
                        #Factor limit for variable classification
                        classlim   = 6,
                        
                        #HTML output options
                        stripe     = TRUE,
                        stripe.col = '#F7F7F7'            
)

{
  #----------#
  # Warnings #
  #----------#
  if (missing(dat)) {
    warning('A data frame must be provided in dat=.')
  }
  
  # Remove any variables with the same name as a base R function
#   if ((any(c(names(dat)) %in% ls('package:base'))) == TRUE) {
#     warning('Variables cannot share the names of any base R functions.')
#     warning(print(paste('Removed variable(s):', 
#                         names(dat)[which(c(names(dat)) %in% ls('package:base'))],
#                         sep=' ')))
#     dat <- dat[, -which(c(names(dat)) %in% ls('package:base'))]
#   }
  
  #-------------------------------#
  # Classify continuous variables #
  #-------------------------------#
  # Get all numeric variables with equal or more than classification 
  # limit (default 6) levels when factored
  cont <- dat[, sapply(dat, is.numeric)]
  cont <- dat[, sapply(dat, function(x) length(levels(as.factor(x))) >= classlim)]
  
  if (nrow(cont) > 0 & ncol(cont) > 0) {
    print('The following variables are summarized as continuous:')
  }
  cont.varlist <- dput(colnames(cont))
  
  #--------------------------------#
  # Classify categorical variables #
  #--------------------------------#
  # All variables that are not continuous; non-numeric or numeric with
  # < classlim factorable levels
  cat <- dat[, -which(c(names(dat)) %in% c(names(cont)))]
  
  # Remove variables from table if specified as stratifying variables
  if (length(strat) > 0) {
    cat <- cat[, -which(c(names(cat)) %in% strat)]
  }  
  
  # Print summary of variable classification
  if (nrow(cat) > 0 & ncol(cat) > 0) {
    print('The following variables are summarized as categorical:')
  }
  cat.varlist <- dput(colnames(cat))
  
  make.table(dat          = dat,
             
             # Categorical variable options
             cat.varlist  = cat.varlist,
             cat.header   = names(dat[, cat.varlist]),
             cat.rownames = lapply(lapply(dat[, cat.varlist], factor), levels),
             
             # Continuous variable options
             cont.varlist = cont.varlist,
             cont.header  = names(dat[, cont.varlist]),
             
             # Basic table options
             strat        = strat,
             colnames     = colnames,
             output       = output,
             stripe       = stripe,
             stripe.col   = stripe.col)   
            }

#######################
# STATISTICAL TESTING #
#######################
stat.col <- function(var, 
                     strat, 
                     ptype, 
                     pname = TRUE,
                     output) {
  
  #------------------#
  # One sample tests #
  #------------------#
  # One-sample t-test, mean = 0
  if (ptype == 't.oneway') {
    p <- t.test(var, mu = 0)$p.value
    }
  # One-sample median test, median = 0
    else if (ptype == 'wilcox.oneway') {
      p <- wilcox.test(var, mu = 0)$p.value
      }
  # Binomial test, proportion = 0.5
      else if (ptype == 'prop.oneway') {
        p <- prop.test(sum(var, na.rm = TRUE), 
                       sum(!is.na(var)), p = 0.5)$p.value
        }
  
  #--------------------#
  # Independent groups #
  #--------------------#
  # Chi-square test
  else if (ptype == 'chisq') {
    p <- chisq.test(var, strat)$p.value
    }
  # Fisher's exact test
    else if (ptype == 'fisher') {
      p <- fisher.test(var, strat)$p.value
      }
  # t-test
      else if (ptype == 'ttest') {
        p <- t.test(var[strat == levels(as.factor(strat))[1]], 
                    var[strat == levels(as.factor(strat))[2]])$p.value
        }
  # One-way ANOVA
        else if (ptype == 'anova') {
          p <- summary(aov(var ~ strat))[[1]][["Pr(>F)"]][[1]]
          }
  # Kruskal-Wallis
          else if (ptype == 'kruskal') {
            p <- kruskal.test(var, strat)$p.value
            }
  # Wilcoxon rank-sum test
            else if (ptype == 'wilcox') {
              p <- wilcox.test(var[strat == levels(as.factor(strat))[1]], 
                               var[strat == levels(as.factor(strat))[2]])$p.value
              }
  
  #------------------#
  # Dependent groups #
  #------------------#
  # Paired t-test
  else if (ptype == 'ttest.pair') {
    p <- t.test(var[strat == levels(as.factor(strat))[1]], 
                var[strat == levels(as.factor(strat))[2]],
                paired = TRUE)$p.value
    }
  # Wilcoxon signed-rank test
    else if (ptype == 'wilcox.pair') {
      p <- wilcox.test(var[strat == levels(as.factor(strat))[1]], 
                      var[strat == levels(as.factor(strat))[2]],
                      paired = TRUE)$p.value
      }
  # McNemar's test
      else if (ptype == 'mcnemar') {
        p <- mcnemar.test(var, strat)$p.value
        }
  
  #------------#
  # No p-value #
  #------------#
  # Print a placeholder dash if no p-value is calculated
  else if (ptype == "None") {
    p <- "-"
    }
  
  #------------#
  # Formatting #
  #------------#
  # Format p-values for consistency across output platforms
  if (p >= 0.001 & p <= 0.999) {
    p <- format(round(p, 3), nsmall = 3)
    }
    else if (p == "-") {
      p <- "-"
      }
      else if (p < 0.001 & output %in% c("plain", "latex")) {
        p <- "<0.001"
        }
        else if (p < 0.001 & output == "html") {
          p <- "&lt;0.001"
          }
          else if (p > 0.999 & output %in% c("plain", "latex")) { 
            p <- ">0.999"
            }
            else if (p > 0.999 & output == "html") {
              p <- "&gt;0.999"
              }
  
  
  # Print name of test or statistic if requested
  if (pname == TRUE) {
    if (ptype == 't.oneway') {
      p.col <- c(p, '1-sample t-test')
      }
      else if (ptype == 'wilcox.oneway') {
        p.col <- c(p, '1-sample median')
        }
        else if (ptype == 'prop.oneway') {
          p.col <- c(p, 'Binomial test')
          }
          else if (ptype == 'chisq') {
            p.col <- c(p, 'Chi-square')
            }
            else if (ptype == 'fisher') {
              p.col <- c(p, 'Fisher exact')
              }
              else if (ptype == 'ttest') {
                p.col <- c(p, 't-test')
                }
                else if (ptype == 'anova') {
                  p.col <- c(p, '1-way ANOVA')
                  }
                  else if (ptype == 'kruskal') {
                    p.col <- c(p, 'Kruskal-Wallis')
                    }
                    else if (ptype == 'wilcox') {
                      p.col <- c(p, 'Wilcoxon rank-sum')
                      }
                      else if (ptype == 'ttest.pair') {
                        p.col <- c(p, 'Paired t-test')
                        }
                        else if (ptype == 'wilcox.pair') {
                          p.col <- c(p, 'Wilcoxon signed-rank')
                          }
                          else if (ptype == 'mcnemar') {
                            p.col <- c(p, 'McNemar')
                            }
                            else if (ptype=="None") {
                              if (output == "plain") {
                                p.col <- c(p, "-")
                              }
                              else if (output == "html") {
                                p.col <- c(p, "&nbsp;")
                              }
                              else if (output == "latex") {
                                p.col <- c(p, "~")
                              }
                            }
                          }
    else {p.col <- p}
  return(p.col)
}  