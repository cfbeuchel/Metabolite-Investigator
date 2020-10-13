plot_multivar <- function(
  data
){
  
  global.min <- data[term.r.squared > 0, min(term.r.squared)]
  data[term.r.squared <= 0, term.r.squared := (global.min)]
  r.squared.cutoff <- unique(data$r.squared.cutoff)
  
  rSquaredCutoff <- r.squared.cutoff
  uni.multi.covar.plot <- ggplot(data,
                                 aes(fill = cohort,
                                     y = term.r.squared,
                                     x = factor(covariate)
                                     # alpha = factor(sig.dummy),
                                     # color = factor(sig.dummy)
                                 )) +
    geom_boxplot(outlier.alpha = 0.5, lwd=0.4) +
    coord_flip() +
    # facet_wrap( ~ assoc,
    #             # scales = "free_x",
    #             labeller = labeller(assoc = labels.assoc)) +
    scale_y_log10(limits = c(10^-6, .5),
                  breaks = trans_breaks("log10",
                                        function(x) 10^x,
                                        n = 5),
                  labels = trans_format("log10",
                                        math_format(10^.x))) + 
    # scale_alpha_manual(values = c("1"=0.6, "0"=1), guide='none') +
    # scale_color_manual(values = c("grey0", "grey25"), guide = F) +
    # geom_hline(data=subset(all.assoc.results.2, assoc=="multi"), aes(yintercept = cutoffs), lty = "dashed", col = "black", lwd = .3) + 
    geom_hline(yintercept = r.squared.cutoff, lty = "dashed", col = "black", lwd = .3) +
    # scale_fill_manual(values = plotting.pal, labels = c("LIFE-Adult", "LIFE-Heart", "Sorb cohort")) +
    theme_minimal() +
    ylab(expression("partial-r"^2)) + 
    theme(axis.text.x = element_text(hjust = .5, angle = 0),
          axis.ticks.x = element_line(), 
          axis.title.y = element_blank(),
          text = element_text(size=18),
          legend.title = element_blank(),
          axis.line.y =  element_blank(),
          axis.line.x =  element_line(),
          panel.border = element_blank(),
          panel.grid.major = element_line(),
          # panel.grid.major.y = element_blank(),
          legend.position = "bottom",
          legend.box.margin=margin(c(0,70,0,0)))
  
  return(uni.multi.covar.plot)
}
