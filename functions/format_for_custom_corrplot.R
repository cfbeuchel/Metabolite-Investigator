format_for_custom_corrplot <- function(
  metabCol,
  covarCol,
  r2Col,
  pCol,
  dat,
  clustMethod = "ward.D2"
){

    setnames(dat,
             old = c(metabCol, covarCol, r2Col, pCol),
             new = c("metabolite", "covariate", "r.squared", "p.value"),
             skip_absent = T)

    dat[,pHierBonf := toolboxH::addHierarchFDR(pvalues = p.value,
                                     categs = covariate,
                                     fdrmethod_level1 = "bonferroni",
                                     fdrmethod_level2 = "bonferroni",
                                     correctionLevel1 = "listlookup"
    )$fdr_level1]
    
    dat[,significant:=pHierBonf<=0.05]
    dat[,.N,significant]
    datmax = dat[,.( maxR2            = max(r.squared, na.rm = T),
                     maxIsSignificant = significant[r.squared==max(r.squared)][1],
                     nStudies         = .N,
                     pcorrMaxR2       = pHierBonf[r.squared==max(r.squared)][1]),
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





# format_for_custom_corrplot <- function(
#   metabCol,
#   covarCol,
#   r2Col,
#   dat,
#   clustMethod = "ward.D2"){
#   
#   # rename for use in dt
#   setnames(dat,
#            old = c(metabCol, covarCol, r2Col),
#            new = c("metabolite", "covariate", "r.squared"),
#            skip_absent = T)
#   
# 
#   # redundant... but whatever 
#   datmax = dat[,.(maxR2 = max(r.squared, na.rm = T)),
#     by = .(metabolite, covariate)]
#   
#   # cast into matrix
#   datmax_r2 = dcast.data.table(datmax, metabolite ~ covariate, value.var = "maxR2")
#   datmax_r2_matrix = as.matrix(datmax_r2[,-'metabolite'])
#   rownames(datmax_r2_matrix) =  datmax_r2$metabolite
#   
#   # do clustering
#   cc <- hclust(dist(t(datmax_r2_matrix)), method = clustMethod)
#   rc <- hclust(dist((datmax_r2_matrix)), method = clustMethod)
#   
#   # create result output
#   res = c()
#   res$r2matrix = t(datmax_r2_matrix[rc$order, cc$order])
#   
#   return(res)
# }
