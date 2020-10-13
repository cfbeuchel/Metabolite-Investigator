network_plot <- function(assocResults,
                         rSquaredColumn,
                         pColumn,
                         cohort,
                         hierarchicalNetwork = T
                         ){
  
  # pColumn <- "p.hierarchical.bonferroni"
  # my.cohort = "cohort_A"
  p.min <- 0.05
  my.cohort <- cohort
  
  # change name
  setnames(assocResults, "term", "factor", skip_absent = T)
  setnames(assocResults, rSquaredColumn, "r.squared", skip_absent = T)
  
  # create data for plot
  plotdat2 <- assocResults[get(pColumn)<=p.min &
                            cohort==my.cohort, .(cohort, metab, factor, r.squared)]
  
  # melt
  plotdat2m <- melt(plotdat2,
                    id.vars = c("cohort"),
                    measure.vars = c("metab", "factor"))
  
  # get nodes
  knoten2 = unique(plotdat2m[, .(label = value,
                                 group = variable )])
  
  # order nodes
  setorder(knoten2, -group) # damitlegende richtigrum ist
  
  # index
  knoten2[ ,id := 1:.N]
  
  # number factors
  plotdat2[,factor_num := knoten2[match(plotdat2$factor,
                                           knoten2$label), id]]
  plotdat2[,metab_num := knoten2[match(plotdat2$metab,
                                          knoten2$label),id]]
  
  # Hierarchical plot
  ecken2 = rbind(
    unique(
      plotdat2[, .(
        from = factor_num, 
        to = metab_num, 
        color = "orange", 
        title = paste0("<p>",
                       factor,
                       " - ",
                       metab,
                       "<br>Expl.Variance: ",
                       signif(r.squared, 3),
                       " </p>"),
        value = r.squared
      )])) #  #  dashes = F, smooth = T, shadow = T
  
  ecken2$smooth = F
  ecken2$shadow = T
  
  # hierarchicalNetwork
  if(hierarchicalNetwork){
    
  visNetwork(
    knoten2,
    ecken2,
    height = "100%",
    width = "100%") %>%
    visHierarchicalLayout(sortMethod="directed") %>%
    visNodes(font= '18px arial black') %>%
    visLegend() %>%
    visOptions(highlightNearest = T) %>%
    visPhysics(enabled = FALSE) #  %>%  visEdges(arrows = 'to')
    
  } else {
    
  # Nonhierarchical plot
  visNetwork(
    knoten2,
    ecken2,
    height = "100%",
    width = "100%") %>%
    visLayout(randomSeed = 12) %>%
    visNodes(font= '18px arial black') %>%
    visLegend() %>%
    visOptions(highlightNearest = T) %>%
    visPhysics(enabled	 = FALSE) #  %>%  visEdges(arrows = 'to')
  }
}