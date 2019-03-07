plot_correlation <- function(data, cohort, covariates){
  
  c.cols <- covariates
  dat <- data
  x <- cohort
  
  M <- cor(
    dat[
      cohort == (x),
      lapply(.SD, as.numeric),
      .SDcols = c(c.cols)
      ], use = "pairwise"
  )
  
  # Plot correlation
  # p.mat <- cor.mtest(dat[
  #   cohort == (x),
  #   lapply(.SD, as.numeric),
  #   .SDcols = c(c.cols)
  #   ])$p
  
  ####################################
  # Alternative Plotting
  
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  
  # arrange data
  M.reorder <- reorder_cormat(M)
  M.upper <- get_upper_tri(M)
  M.upper.m <- melt(M.upper, na.rm = T)
  
  ggheatmap <- ggplot(M.upper.m, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45,
                                     vjust = 1, 
                                     size = 13,
                                     hjust = 1),
          axis.text.y = element_text(size = 13)) +
    coord_fixed()
  
  ggheatmap +
    geom_text(aes(Var2, Var1, label = round(value, 3)), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank()
      )
}