getMostInfluentialVars <- function(similarity_threshold, svd_out, perc_explained){
  vars <- list()
  require("Hmisc")
  for (i in 1:length(perc_explained)){
    v_abs <- abs(svd_out$v[,i])
    maxContributor <- which.max(v_abs)
    similarSizedContributors <- which(v_abs >= v_abs[maxContributor]*similarity_threshold)
    if (any(similarSizedContributors %nin% maxContributor)){
      maxContributor <- similarSizedContributors[order(v_abs[similarSizedContributors], decreasing=TRUE)]
    }
    vars[[length(vars) + 1]] <- maxContributor
  }
  return(vars)
}


GetName <- function(vars, perc_index, varnames){
  unlist(lapply(vars[1:perc_index], FUN=function(x){
    ret <- paste(varnames[x], collapse="\n")
    return(ret)
  }))
}

Plot_SVD <- function(svd_out, perc_index, threhold, plot_threshold){
  cols_expl <- which(cumsum(perc_explained) <= threhold)
  bar_count <- perc_index
  if (bar_count > length(perc_explained)){
    bar_count <- length(perc_explained)
    plot_percent <- perc_explained
  }else{
    plot_percent <- rep(NA, times=bar_count)
    plot_percent <- perc_explained[perc_explained >= plot_threshold]
    plot_percent[bar_count] <- sum(perc_explained[perc_explained < plot_threshold]) 
  }
  
  # Create transition colors
  selected_colors <- colorRampPalette(c("darkgreen", "#FFFFFF"))(bar_count+2)[cols_expl]
  nonselected_colors <- colorRampPalette(c("darkgrey", "#FFFFFF"))(bar_count+2)[(max(cols_expl)+1):bar_count]
  
  names <- unlist(lapply(vars[1:perc_index], FUN=function(x){
    ret <- paste(varnames[x], collapse="\n")
    return(ret)
  }))
  
  rotation <- 45 + (max(unlist(lapply(vars[1:bar_count], 
                                      function(x) {
                                        min(length(x), 5)
                                      })))-1)*(45/5)
  
  p1 <- barchart(plot_percent * 100 ~ 1:bar_count,
                 horiz=FALSE,
                 ylab="Percentage explained (%D)", 
                 main="SVD - the maximum contributors defined by V column",
                 xlab="Pattern contributing variables",
                 col=c(selected_colors, nonselected_colors),
                 key=list(text=list(c("Selected", "Not selected")), 
                          rectangles=list(col=c("darkgreen", "#777777"))),
                 scales=list(x=list(rot=rotation, labels=names)))
  return(p1)
}


Get_SVD <- function(Matrix_In, similarity_threshold){
  Result <- list()
  SVD <- svd(scale(Matrix_In))
  b_clr <- c("steelblue", "darkred")
  key <- simpleKey(rectangles = TRUE, space = "top", points=FALSE,
                   text=c("Positive", "Negative"))
  key$rectangles$col <- b_clr
  
  perc_explained <- SVD$d^2/sum(SVD$d^2)
  perc_index <- max(which(cumsum(perc_explained) < 0.9))
  
  vars <- getMostInfluentialVars(similarity_threshold, SVD, perc_explained)
  Important_Loc <- GetName(vars, perc_index, names(PCA_Input))
  
  Result[[length(Result)+1]] <- vars
  Result[[length(Result)+1]] <- unlist(strsplit(Important_Loc, "\n"))
  Result[[length(Result)+1]] <- Plot_SVD(SVD, perc_index, threhold = 0.9, plot_threshold = 0.05)
  
  return(Result)
}


