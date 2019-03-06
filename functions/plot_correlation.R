plot_correlation <- function(data, cohort){
  
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
  p.mat <- cor.mtest(dat[
    cohort == (x),
    lapply(.SD, as.numeric),
    .SDcols = c(c.cols)
    ])$p
  
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  # plot
  corrplot(M,
           method = "color",
           col = col(200),
           type = "upper",
           addCoef.col = "black", # Add coefficient of correlation
           tl.col = "black",
           title = paste0("Pairwise Pearsons' correlation of ", x, " metabolites."),
           tl.srt = 45, # Text label color and rotation
           p.mat = p.mat, # Combine with significance
           sig.level = 0.01,
           insig = "blank", # hide correlation coefficient on the principal diagonal
           outline = T,
           diag = FALSE,
           mar = c(0,0,2,0)
  )
}