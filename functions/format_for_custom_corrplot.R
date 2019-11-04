format_for_custom_corrplot <- function(
  metabCol,
  covarCol,
  r2Col,
  pCol,
  sigCol,
  dat,
  clustMethod = "ward.D2"
){
  
  
  dat2 <- copy(dat)
  setnames(dat2,
           old = c(metabCol, covarCol, r2Col, pCol, sigCol),
           new = c("metabolite", "covariate", "r.squared", "p.value", "significant"),
           skip_absent = T)
  
  
  dat2[,.N,significant]
  datmax = dat2[,.( maxR2            = max(r.squared, na.rm = T),
                   maxIsSignificant = significant[r.squared==max(r.squared)][1],
                   nStudies         = .N,
                   pcorrMaxR2       = p.value[r.squared==max(r.squared)][1]),
               by = .(metabolite, covariate)]
  datmax
  datmax[nStudies!=3, .N, covariate]
  # hh(datmax)
  datmax_r2 = dcast.data.table(datmax, metabolite ~ covariate, value.var = "maxR2")
  datmax_r2
  datmax_r2_matrix = as.matrix(datmax_r2[,-'metabolite'])
  rownames(datmax_r2_matrix) =  datmax_r2$metabolite
  datmax_pval = dcast.data.table(datmax, metabolite ~ covariate, value.var = "pcorrMaxR2")
  datmax_pval
  datmax_pval_matrix = as.matrix(datmax_pval[,-'metabolite'])
  rownames(datmax_pval_matrix) =  datmax_pval$metabolite
  # hh(datmax_pval_matrix)
  cc <- hclust(dist(t(datmax_r2_matrix)), method = clustMethod)
  # cc$order
  # plot(cc)
  # colnames(datmax_r2_matrix)[cc$order]
  rc <- hclust(dist((datmax_r2_matrix)), method = clustMethod )
  res = c()
  res$r2matrix = t(datmax_r2_matrix[rc$order, cc$order])
  res$pvalmatrix = t(datmax_pval_matrix[rc$order, cc$order])
  return(res)
}
