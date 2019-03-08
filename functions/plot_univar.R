plot_univar <- function(data, rSquaredCol){
  
  setnames(data, old = rSquaredCol, new = "r.squared", skip_absent = T)
  
  ggplot(data,
         aes(fill = cohort,
             y = (r.squared),
             x = factor(term))) +
    geom_boxplot(outlier.alpha = 0.1, lwd=0.4) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    # geom_hline(yintercept = cutoffs, lty = "dotted", col = "red", lwd = .3) +
    coord_flip() +
    theme_minimal() +
    ylab(expression("r"^2)) + 
    theme(axis.text.x = element_text(hjust = 0.5, angle = 0),
          axis.ticks.x = element_line(),
          axis.title.y = element_blank(),
          text = element_text(size=16),
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(),
          panel.grid.major.y = element_blank(),
          panel.grid.major = element_line(size = .2, colour = "grey90")
    )
}
  
  