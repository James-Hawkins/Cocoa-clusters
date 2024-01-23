
#'  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'  The below R script develops clusters of cocoa plots by applying sequentially
#'  factor reduction using 'Multiple Factor Analysis' and hierarchical 
#'  clustering using 'hclust' (Stats package). 
#'  
#'  The script has four main functions which are run sequentially:
#'  
#'  1. 'typology.data' which merges the 'comp' (composite) dataframe including all 
#'  plot level variables derived from a farm-survey (GGS 2021) with 'bioclim.shp', 
#'  a shapefile with spatially explicit climatic variables. 
#'  
#'  2. 'typology.settings' which specifies the main parameters of the clustering, namely the clustering method
#'  and quantity of clusters per production system (see paper)
#
#'  3. 'gen.clusters' conducts the multiple factor analysis and clustering. Each plot id then gets assigned
#'  to a cluster identifier.
#'  
#'  4. 'gen.figures' generates results figures which are presented in the paper.
#'  
#'  Note that the actual script used to derive the survey variables as well as the survey data 
#'  is not included here. This data and code can be obtained from authors upon request. 
#'  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##



typology.data <- function(){
  
  setwd(main.dir) 
  
  # Load bioclimatic data from shapefile
  bioclim <- readOGR("Spatial_data/bioclimatic_data.shp")
  
  # Rename climatic variables
  bioclim$elev <- as.numeric(bioclim$elev1)
  bioclim$temp <- as.numeric(bioclim$temp1)
  bioclim$precip <- as.numeric(bioclim$precip_1)
  bioclim$temp.mmm <- as.numeric(bioclim$temp1_2)
 
  bioclim.df <<- as.data.frame( bioclim)

  # List of all variables used in development of typology
  vr.list <<- c('district',
               #Production characteristics
               'Variety',
               'cc.catg.str',
               'years_since_planted',
               'cc.production.cycle',
               'cm.plot.monocrop.bool',
               # Yield variables
               'cc_yield_fn_Mg_per_ha',
               'yld_pc_attain_plot',
               # Practices
               'weeds_per_year',
               'prunes_per_year',
               'fert_bool',
               'fungi.app.cocoa',
               'insecti.app.cocoa',
               'herbi.app.cocoa',
               'pollination.bool' ,
               'ccoa_spread_pods',
               'ccoa_piled_pods',
               'ccoa_passive_pods',
               'ccoa_apply_mnr_farm',
               'ccoa_apply_mnr_bought',
               'ccoa_apply_mnr_anywhere',
               'ccoa_apply_cmp',
               # Vegetative structure and diversity
               'tree.count.per.ha',
               'tree.config.cocoa',
               'unique.tree.species',
               'unique.shade.tree.species',
               'plot.overstory.crown',
               'other.tree.to.cm.tree.ratio',
               'other.short.tree.to.cm.tree.ratio',
               'shade.tree.to.cm.tree.ratio',
               'plot.quant.short.trees.ha',
               'plot.quant.shade.trees.ha',
               'plot.quant.large.shade.trees.ha',
               'num_other_tree_crops',
               'num_other_root_grain_crops',
               'plot.quant.trees.per.ha.greater.than.50.m',
               'plot.quant.trees.per.ha.greater.than.35.m',
               'plot.quant.trees.per.ha.greater.than.20.m',
               'pct.shade.trees.greater.than.50.m',
               'pct.shade.trees.greater.than.35.m',
               'pct.shade.trees.greater.than.20.m',
               # Diseases
               'detect.bool.blackpod',
               'detect.bool.capsid',
               'detect.bool.stemborer' ,
               'detect.bool.rodents',
               'detect.bool.CSSVD',
               'detect.bool.stemcanker',
               'detect.bool.other',
               'path.contr.cc.vir.org',
               'path.contr.cc.bact.org',
               'path.contr.cc.fung.org',
               'path.contr.cc.insect.org',
               'path.contr.cc.vir.syn',
               'path.contr.cc.bact.syn',
               'path.contr.cc.fung.syn',
               'path.contr.cc.insect.syn',
               'path.contr.tot.org',
               'path.contr.tot.syn',
               'frac.tot.paths.controlled.syn',
               'frac.tot.paths.controlled.org',
               'quant.paths',
               'quant.paths.fungal',
               'quant.paths.viral',
               'quant.paths.bacterial',
               'quant.paths.insect',
               # Inputs
               'total_fert_Nitr_applied_kg_per_ha',
               'total_N_fert_applied_Mg_per_ha',
               'total_non_N_fert_applied_Mg_per_ha',
               'total_fert_applied_Mg_per_ha',
               'total_urea_Nitr_applied_kg_per_ha',
               'total_ammon_Nitr_applied_kg_per_ha',
               # Labour
               'total.hi.labor.days.per.year.per.ha',
               # Profitability
               'total.cocoa.costs.usd.per.ha',
               'gross.revenue.frt.usd.per.ha',
               'gross.revenue.ann.crop.usd.per.ha',
               'gross.revenue.usd.per.ha.frac.frt',
               'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.frt',
               'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ann.crop',
               'lc.net.VOP.1000.usd.per.ha',
               # Agronomic characteristics
               'domn_soil',
               'water.log.bool',
               'water.logged'
   )
   
   

   # Add survey variables to bioclim
   for (id in bioclim$hhID){
     
     # Add variables to bioclim
     for (v in 1:length(vr.list)){
       
        var <- vr.list[v]
        v <- comp[comp$hhID == id ,var ]
        
       if (is.numeric(v)){
         bioclim[bioclim$hhID == id ,var ] <- v
       } else {
         bioclim[bioclim$hhID == id ,var ] <- v
       }
     } 
   } 
  
  # Dataframe with all necessary variables (survey and spatially derived)
  T.df.raw <<- as.data.frame(bioclim)
  
 }
typology.data()

# Define settings used to derive typology  
typology.settings <- function(){
  
  clusters.redefine <<- 1
  
  cluster.method <<- "ward.D2" 
  
  cluster.method.amaz <<-  cluster.method 
  cluster.method.hysh <<-  cluster.method 
  cluster.method.hysun <<- cluster.method 
  
  # Specify number of clusters per production system
  num.clusts.hysh <<- 5
  num.clusts.hysn <<- 5
  num.clusts.amz <<- 4
  
}
typology.settings()
 

gen.clusters <- function(){

  setwd(cc.typ.dir)
 
  T.df <- T.df.raw

 
  amaz.ids <- T.df[ T.df$cc.catg.str == 'Amazonia'
                      &  !is.na(T.df$cc.catg.str)
                   ,'hhID']
  
  hysun.ids <- T.df[  T.df$cc.catg.str == 'Hybrid sun'
                   &  !is.na(T.df$cc.catg.str)
                     ,'hhID']
  
  hysh.ids <- T.df[   T.df$cc.catg.str == 'Hybrid shade'
                    & !is.na(T.df$cc.catg.str)
                 ,'hhID']
  
    hysh.plots <- T.df[ T.df$hhID %in% hysh.ids ,]
    hysun.plots <- T.df[ T.df$hhID %in% hysun.ids ,]
    amaz.plots <- T.df[ T.df$hhID %in% amaz.ids ,]
    
    n.hysh <- nrow(hysh.plots)
    n.hysn <-nrow(hysun.plots)
    n.amz <-nrow(amaz.plots)
    
    n.total <- n.hysn + n.hysh + n.amz

    print(paste('Total number of observations included: ', n.total  ))
    print(paste('For hybrid sun: ',  n.hysn   ))
    print(paste('For hybrid shade: ',  n.hysh   ))
    print(paste('For Amazonia: ',    n.amz  ))
    
    ids.considered <- append(hysh.ids ,hysun.ids  )
    ids.considered <- append(ids.considered ,amaz.ids  )

    comp[comp$hhID %in% ids.considered , 'typology'] <- NA

    # Sun
    mfa.hysun <- hysun.plots[ , c(  'hhID',
                                    'total_N_fert_applied_Mg_per_ha',
                                    'total_non_N_fert_applied_Mg_per_ha',
                                    'tree.count.per.ha',
                                    'num_other_root_grain_crops',
                                    'prunes_per_year',
                                    'weeds_per_year',
                                    'yld_pc_attain_plot',
                                    'precip',
                                    'elev',
                                    'temp1_2',
                                    'fungi.app.cocoa',
                                    'insecti.app.cocoa',
                                    'herbi.app.cocoa',
                                    'pollination.bool' ,
                                    'tree.config.cocoa',
                                    'detect.bool.blackpod',
                                    'detect.bool.capsid',
                                    'detect.bool.stemborer',
                                 'water.logged'
    )]

  
    # Shade
    mfa.hysh <- hysh.plots[ , c('hhID',
                                'total_N_fert_applied_Mg_per_ha',
                                'total_non_N_fert_applied_Mg_per_ha',
                                'tree.count.per.ha',
                                'shade.tree.to.cm.tree.ratio',
                                'other.short.tree.to.cm.tree.ratio',
                                'plot.quant.trees.per.ha.greater.than.50.m',
                                'plot.quant.trees.per.ha.greater.than.35.m',
                                'plot.quant.trees.per.ha.greater.than.20.m',
                                'unique.tree.species', 
                                'num_other_root_grain_crops',
                                'prunes_per_year',
                                'weeds_per_year',
                                'yld_pc_attain_plot',
                                'precip',
                                'elev',
                                'temp1_2',
                                'water.logged',
                                'fungi.app.cocoa',
                                'insecti.app.cocoa',
                                'herbi.app.cocoa',
                                'pollination.bool' ,
                                'detect.bool.blackpod',
                                'detect.bool.capsid',
                                'detect.bool.stemborer'
    )]
    
    
    # Amazonia
    mfa.amaz <- amaz.plots[ , c(  'hhID',
                                  'total_N_fert_applied_Mg_per_ha',
                                  'total_non_N_fert_applied_Mg_per_ha',
                                  'tree.count.per.ha',
                                  'shade.tree.to.cm.tree.ratio',
                                  'plot.quant.trees.per.ha.greater.than.50.m',
                                  'plot.quant.trees.per.ha.greater.than.35.m',
                                  'plot.quant.trees.per.ha.greater.than.20.m',
                                  'unique.tree.species', 
                                  'num_other_root_grain_crops',
                                  'prunes_per_year',
                                  'weeds_per_year',
                                  'yld_pc_attain_plot',
                                  'precip',
                                  'elev',
                                  'temp1_2',
                                  'fungi.app.cocoa',
                                  'insecti.app.cocoa',
                                  'herbi.app.cocoa',
                                  'pollination.bool' ,
                                  'detect.bool.blackpod',
                                  'detect.bool.capsid',
                                  'detect.bool.stemborer',
                                  'water.logged'
    )]
    
    # Define element of vector which represents first qualitative variable 
    hysun.qvars.f.ix <- 11
    amaz.qvars.f.ix <- 16
    hysh.qvars.f.ix <- 17
    
    # Omit observations with NA
    mfa.hysun <-  na.omit(mfa.hysun)
    mfa.hysh <-  na.omit(mfa.hysh)
    mfa.amaz <-  na.omit(mfa.amaz)
    
    # Check for variables that are highly correlated
    cor_matrix.hysh <- cor(mfa.hysh[ ,-c(1)])
    cor_matrix.hysun <- cor(mfa.hysh[ ,-c(1)])
    cor_matrix.amaz <- cor(mfa.amaz[ ,-c(1)])

    # Sun
    mfa.hysun[,c((hysun.qvars.f.ix+1):ncol(mfa.hysun))] <- as.character(mfa.hysun[,c((hysun.qvars.f.ix+1):ncol(mfa.hysun))])
    
    # Amaz
    mfa.amaz[,c((amaz.qvars.f.ix+1):ncol(mfa.amaz))] <- as.character(mfa.amaz[,c((amaz.qvars.f.ix+1):ncol(mfa.amaz))])
    
    # Shade
    mfa.hysh[,c((hysh.qvars.f.ix+1):ncol(mfa.hysh))] <- as.character(mfa.hysh[,c((hysh.qvars.f.ix+1):ncol(mfa.hysh))])
    

    # RUN PCA MIX algorithm
    # Sun
    hysun.pcamix <- PCAmix(X.quanti=mfa.hysun[,c(2:(hysun.qvars.f.ix))],  
                         X.quali=mfa.hysun[,c((hysun.qvars.f.ix+1):ncol(mfa.hysun))], 
                         rename.level=TRUE, 
                         graph=FALSE, 
                         ndim=25)
    
    ## Shade
    hyshade.pcamix <- PCAmix(X.quanti = mfa.hysh[,c(2:hysh.qvars.f.ix)],  
                             X.quali = mfa.hysh[,c((hysh.qvars.f.ix+1) :ncol(mfa.hysh))], 
                             rename.level = TRUE, 
                             graph = FALSE, 
                             ndim = 25 )
    
    # Amazonia
    amaz.pcamix <- PCAmix(X.quanti=mfa.amaz[,c(2:amaz.qvars.f.ix)],  
                          X.quali=mfa.amaz[,c((amaz.qvars.f.ix+1):ncol(mfa.amaz))], 
                          rename.level=TRUE, 
                          graph=FALSE, 
                          ndim=25)
    
   
  # Specify variables used in clustering for each system based on results of PCA mix # 

    hysun.pcamix$eig
    hysun.pcamix$quanti$contrib.pct
    hysun.pcamix$quali$contrib.pct
    
    "
    HYBRID SUN
    First 11 PCs ; 10 - 12 quantitative variable correspondence
    9. max temp
    10. othe fert , tree density , weeding
    11. N fert
    "
    
    # Hybrid sun
    hc.hysun <- hysun.plots[ , c('hhID',
                                 'num_other_root_grain_crops',
                                 'total_N_fert_applied_Mg_per_ha',
                                 'total_non_N_fert_applied_Mg_per_ha',
                                 'yld_pc_attain_plot',
                                 'tree.count.per.ha',
                                 'weeds_per_year',
                                 'temp1_2',
                                 'water.logged',
                                 'fungi.app.cocoa',
                                 'insecti.app.cocoa',
                                 'herbi.app.cocoa',
                                 'pollination.bool' ,
                                 'tree.config.cocoa',
                                 'detect.bool.blackpod',
                                 'detect.bool.capsid',
                                 'detect.bool.stemborer'
    )]
    
    
    hyshade.pcamix$eig
    hyshade.pcamix$quanti$contrib.pct
    hyshade.pcamix$quali$contrib.pct
    
    "
    SHADE
    First 12 PCs ; 8 - 12 quantitative variable correspondence
     8. max temp, altitude
     9. tree count, shade to cc tree
     10. N fert, medium trees
     11. ann crops 
     12. N fert

    "
    hc.hysh <- hysh.plots[ , c('hhID',
                              'yld_pc_attain_plot',
                              'total_N_fert_applied_Mg_per_ha',
                              'tree.count.per.ha',
                              'shade.tree.to.cm.tree.ratio',
                              'plot.quant.trees.per.ha.greater.than.35.m',
                              'num_other_root_grain_crops',
                              'elev',
                              'temp1_2',
                              'water.logged',
                              'fungi.app.cocoa',
                              'insecti.app.cocoa',
                              'herbi.app.cocoa',
                              'pollination.bool' ,
                              'detect.bool.blackpod',
                              'detect.bool.capsid',
                              'detect.bool.stemborer'
    )]
    
    
    # Amazonia
    amaz.pcamix$eig
    amaz.pcamix$quanti$contrib.pct
    amaz.pcamix$quali$contrib.pct
    "
    AMAZONIA
    First 11 PCs ; 8 - 11 quantitative variable correspondence
    8. max temperature, altitude
    9. ann crops
    10. tall trees, n fert
    11. tall trees and short trees , yield
    "
    hc.amaz <- amaz.plots[ , c('hhID',
                               'num_other_root_grain_crops',
                               'plot.quant.trees.per.ha.greater.than.50.m',
                               'plot.quant.trees.per.ha.greater.than.20.m',
                               'total_N_fert_applied_Mg_per_ha',
                               'yld_pc_attain_plot',
                               'elev',
                               'temp1_2',
                               'water.logged',
                               'fungi.app.cocoa',
                               'insecti.app.cocoa',
                               'herbi.app.cocoa',
                               'pollination.bool' ,
                               'tree.config.cocoa',
                               'detect.bool.blackpod',
                               'detect.bool.capsid',
                               'detect.bool.stemborer' 
    )]
    
    # Omit observations that are NA
    hc.hysh <-  na.omit(hc.hysh)
    hc.hysun <-  na.omit( hc.hysun)
    hc.amaz <-  na.omit( hc.amaz)
    
    # Calculate gap statistic for each system 
    # Note this is commented out because it only need be done once to determine number of clusters 
    # (and in subsequent runs of function need not be included)
    # gap_stat.hysh <- clusGap(hc.hysh[,c(2:length(hc.hysh))], FUN = hcut, nstart = 250, K.max = 30, B = 50)
    # gap_stat.hysun <- clusGap(hc.hysun[,c(2:length(hc.hysun))], FUN = hcut, nstart = 250, K.max = 30, B = 50)
    # gap_stat.amaz<- clusGap(hc.amaz[,c(2:length(hc.amaz))], FUN = hcut, nstart = 250, K.max = 30, B = 50)
    
  
    # fviz_gap_stat(gap_stat.hysh)
    # fviz_gap_stat( gap_stat.hysun)
    # fviz_gap_stat(gap_stat.amaz )
    

    # ~~~~~~ CLUSTERING ~~~~~~ #
    
    # Hybrid shade
    d <- daisy(hc.hysh[,c(2:length(hc.hysh))], metric="gower")
    fit <- hclust(d=d, method= cluster.method.hysh)
    

    groups.hysh <- cutree(fit, k = num.clusts.hysh)     
    
    # Amazonia
    d <- daisy(hc.amaz[,c(2:length(hc.amaz))], metric="gower")
    fit <- hclust(d=d, method= cluster.method.amaz)
    
    groups.amaz <- cutree(fit, k=  num.clusts.amz)   
    
    # Hybrid sun
    d <- daisy(hc.hysun[,c(2:length(hc.hysun))], metric="gower")
    fit <- hclust(d=d, method=cluster.method.hysun)
    
    groups.hysun <- cutree(fit, k = num.clusts.hysn )   

    
    # Assign new name for cluster variable
    hc.hysh$typ <- (hc.hysh$cluster)
    hc.hysun$typ <-(hc.hysun$cluster)
    hc.amaz$typ <- (hc.amaz$cluster)
    
    # Column bind cluster variables with cluster assignments
    hc.hysun <- cbind(hc.hysun , cluster = groups.hysun)
    hc.hysh <- cbind(hc.hysh , cluster = groups.hysh)
    hc.amaz <- cbind(hc.amaz , cluster = groups.amaz )
  
    hc.hysh$typ <- as.factor(hc.hysh$cluster)
    hc.hysun$typ <- as.factor(hc.hysun$cluster)
    hc.amaz$typ <- as.factor(hc.amaz$cluster)
    
    # Define the system to which each cluster exists in comp dataframe
    comp[comp$hhID %in% hc.hysh$hhID , 'cc.plot.sys'] <- 'Hybrid shade'
    comp[comp$hhID %in% hc.hysun$hhID , 'cc.plot.sys'] <- 'Hybrid sun'
    comp[comp$hhID %in% hc.amaz$hhID , 'cc.plot.sys'] <- 'Amazonia'
    
    
  # Name production systems (typologies)
    typologies <<- c(
    'Hybrid sun'
     ,'Hybrid shade'
     ,'Amazonia'
     )
  

 # Order clusters in ascending order of yield
  for (T in typologies){
    
    if (T == 'Amazonia') {
      data <- hc.amaz}
    if (T == 'Hybrid shade') {
      data <- hc.hysh}
    if (T == 'Hybrid sun') {
      data <- hc.hysun}
    
    clusters <-   unique(data$typ)
    
    cluster.names <- c()
    cluster.values <- c()
    
      for (c in 1:length(clusters)){
        
        
        ids <- data[data$hhID %in%  data[ data$cluster  == c,'hhID'],'hhID'] 
        
        if (c == 1) {cluster.names <- append (cluster.names , 1)}
        if (c == 2) {cluster.names <- append (cluster.names , 2)}
        if (c == 3) {cluster.names <- append (cluster.names , 3)}
        if (c == 4) {cluster.names <- append (cluster.names , 4)}
        if (c == 5) {cluster.names <- append (cluster.names , 5)}
        if (c == 6) {cluster.names <- append (cluster.names , 6)}
        if (c == 7) {cluster.names <- append (cluster.names , 7)}
      
        variable.to.order <- mean( comp[comp$hhID %in% ids,'yld_pc_attain_plot'] )
         
        if (c == 1) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 2) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 3) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 4) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 5) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 6) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 7) {cluster.values <- append (cluster.values ,  variable.to.order)}
        
         
         cluster.values.ordered <- cluster.values

         cluster.dat <- data.frame(column_1 = cluster.names , column_2 = cluster.values.ordered)
         
         cluster.dat.ordered <- cluster.dat[order(cluster.dat$column_2),]

         if (T == 'Amazonia'){ t.nm.amz <- cluster.dat.ordered[,c(1)]  }
         if (T == 'Hybrid shade'){ t.nm.hy.sh <- cluster.dat.ordered[,c(1)]  }
         if (T == 'Hybrid sun'){ t.nm.hy.sun <- cluster.dat.ordered[,c(1)]  }
      }
   
  }

} # END CLUSTERS CODE

# gen.clusters()




summary(T.df$temp)
summary(T.df$temp.mmm)
summary(T.df$precip)
summary(T.df$elev)




gen.figures <- function(){
  
  setwd(cc.typ.dir)
  
  # Create new variables in comp dataframe related to cluster results
  
  comp[ ,'typ.str']  <- NA
  comp[ ,'typology']  <- NA
  comp[ ,'typ.str.fill']  <- NA
  
  
  # Assign names representing cluster result to each observation of comp dataframe
  for (T in typologies){
    
    current.typology <- T
    
    if (T == 'Amazonia') {
      t.nm <-  t.nm.amz
      num.clusters <- num.clusts.amz
      data <- hc.amaz} else if (T == 'Hybrid shade') {
        t.nm <-  t.nm.hy.sh
        num.clusters <- num.clusts.hysh
        data <- hc.hysh} else if (T == 'Hybrid sun') {
          t.nm <-  t.nm.hy.sun
          data <- hc.hysun
          num.clusters <- num.clusts.hysn
        }
    
    
    comp[comp$hhID %in% data[,    'hhID'] ,   'typology']  <- T
    data[ ,   'typology']  <- T
    
    
    for (i in seq(1:num.clusters))  {
      
      data[data$typ ==   i & !is.na(data$typ)  ,'typ.str']  <-  match(i,t.nm) 
      data[data$typ ==   i & !is.na(data$typ)  ,'typ']  <- i
      
      
      comp[comp$hhID %in% data[data$typ == i,    'hhID'] ,   'typ.str']  <- match(i,t.nm) 
      comp[comp$hhID %in% data[data$typ == i,    'hhID']  ,'typ']  <- i
      
      cluster.name <- str_c('Cluster number ', i)
      
      comp[comp$hhID %in% data[data$typ == i,    'hhID']  ,'typ.str.fill']  <-  cluster.name
      comp[comp$hhID %in% data[data$typ == i,    'hhID']  ,'facet.lab']  <-  'All systems'
      
    }
    
    
  }


# Assign descriptive names for clusters to appear in results figures
ordered.cluster.names <<- c(
  'Fertiliser & High elevation',
  'Fertiliser & Low elevation',
  'Labour & Fertiliser',
  'Weeding & Waterlogged',
  'Low tree density',
  'Cluster num 6',
  'Nutrient deficient', # Hybrid shade #1
  'High tree density',
  'Open & Complex fertilisation',
  'Dense & Complex fertilisation',
  'N-fertilisation & Pollination',
  'Cluster num 12',
  'Nutrient deficient & High elevation', # Amazonia #1
  'Nutrient deficient & Low elevation',
  'N-fertilisation',
  'Fertiliser & Tall canopy',
  'Cluster num 17',
  'Cluster num 18'
)

  
  
##  ~~~~ GENERATE RESULTS FIGURES/TABLES ~~~~ ##
  
# Define typology variable as an ordered factor
ordered.typologies <<- c( 'Hybrid sun' , 'Hybrid shade' ,'Amazonia'  )
comp$typology <- factor(comp$typology  , levels = ordered.typologies)


# Define type variable as an ordered factor
ordered.typ <<- c(
 1,
 2,
 3,
 4,
 5,
 6
)
comp[,'typ.str'] <- factor(comp[,'typ.str'] , levels = ordered.typ)


# Re-define type string variable as an ordered factor
comp[comp$typ.str == 1 & !is.na(comp$typ.str) & comp$typology == 'Hybrid sun' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[1]
comp[comp$typ.str == 2 & !is.na(comp$typ.str) & comp$typology == 'Hybrid sun' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[2]
comp[comp$typ.str == 3 & !is.na(comp$typ.str) & comp$typology == 'Hybrid sun' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[3]
comp[comp$typ.str == 4 & !is.na(comp$typ.str) & comp$typology == 'Hybrid sun' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[4]
comp[comp$typ.str == 5 & !is.na(comp$typ.str) & comp$typology == 'Hybrid sun' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[5]
comp[comp$typ.str == 6 & !is.na(comp$typ.str) & comp$typology == 'Hybrid sun' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[6]

comp[comp$typ.str == 1 & !is.na(comp$typ.str) & comp$typology == 'Hybrid shade' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[7]
comp[comp$typ.str == 2 & !is.na(comp$typ.str) & comp$typology == 'Hybrid shade' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[8]
comp[comp$typ.str == 3 & !is.na(comp$typ.str) & comp$typology == 'Hybrid shade' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[9]
comp[comp$typ.str == 4 & !is.na(comp$typ.str) & comp$typology == 'Hybrid shade' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[10]
comp[comp$typ.str == 5 & !is.na(comp$typ.str) & comp$typology == 'Hybrid shade' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[11]
comp[comp$typ.str == 6 & !is.na(comp$typ.str) & comp$typology == 'Hybrid shade' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[12]

comp[comp$typ.str == 1 & !is.na(comp$typ.str) & comp$typology == 'Amazonia' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[13]
comp[comp$typ.str == 2 & !is.na(comp$typ.str) & comp$typology == 'Amazonia' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[14]
comp[comp$typ.str == 3 & !is.na(comp$typ.str) & comp$typology == 'Amazonia' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[15]
comp[comp$typ.str == 4 & !is.na(comp$typ.str) & comp$typology == 'Amazonia' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[16]
comp[comp$typ.str == 5 & !is.na(comp$typ.str) & comp$typology == 'Amazonia' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[17]
comp[comp$typ.str == 6 & !is.na(comp$typ.str) & comp$typology == 'Amazonia' & !is.na(comp$typology), 'typ.str.fill'] <- ordered.cluster.names[18]


# Define type string variable as an ordered factor
comp$typ.str.fill <- factor(comp$typ.str.fill , levels =   ordered.cluster.names , ordered =  TRUE)

nrow(comp[ !is.na(comp$typ.str.fill) , ])

#' Figures are generated respectively for within (intra) group comparisons
#' (for each cluster in each production system) and then for cross (inter) group
#'  comparisons (comparing production systems). These figures are then merged into 
#'  panelled ggplot figures for each indicator (GHG, VOP, and yield)


# Data prep for intra group figures
fig.intrag.dt.prep <- function(){
  
  # GHG data
  ghg.dat.var.names <- c ('Typology',
                             'Type',
                             'Type.str' , 
                             'Emission.category',
                             'value.mn',
                             'tot.value.mn',
                             'tot.value.sd'
                             )

  
  emis.catg <- c(
     'N2O syn',
    'N2O organic',
    'CO2 seqn. shade',
    'CO2 seqn. cocoa',
    'CO2 seqn. fruit',
    'CO2 seqn. other',
    'CO2 seqn. soil',
    'CH4 cocoa'
  )
  
  
  tot.clusters <- num.clusts.amz + num.clusts.hysn + num.clusts.hysh
  rows <- seq( 1 : ( tot.clusters*length(emis.catg)) )
  
  ghg.dat <- data.frame(  matrix(ncol = length(ghg.dat.var.names) , nrow = length(rows) , dimnames=list(rows  , ghg.dat.var.names )))
  
  # VOP prep=
  vop.dat.var.names <- c ('Typology',
                          'Type',
                          'Type.str' , 
                          'Revenue category',
                          'value.mn',
                          'tot.value.mn',
                          'tot.value.sd'
                          )
  
  rev.catg <- c('Cocoa',
                'Fuelwood',
                'Hardwood lumber',
                'Annual crops',
                'Fruit trees',
                'Other agcommodities',
                'All cost categories'
                )
  
  rows <- seq(1:( tot.clusters * length(rev.catg )))
  
  vop.dat <- data.frame(  matrix(ncol = length(vop.dat.var.names) , nrow = length(rows) , dimnames=list(rows  , vop.dat.var.names )))
  
  vop.var.names <- c('lc.net.VOP.1000.usd.per.ha.frac.rev.basis.cc',
                     'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.fuelw',
                     'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.hardw',
                     'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ann.crop',
                     'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.frt',
                     'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ag.cmd',
                     'total.costs.usd.per.ha'
  )
  

  ghg.row.count <- 1
  vop.row.count <- 1
  
  #' For loop to populate dataframes for VOP and GHG with respective statistics (means, errors)
  #' which are used to generate ggplot figures

  for (t1 in  typologies) {
    
    if (t1 == "Hybrid sun")   {  clust.quant <- num.clusts.hysn}
    if (t1 == "Hybrid shade") {  clust.quant <- num.clusts.hysh}
    if (t1 == "Amazonia")     {  clust.quant <- num.clusts.amz}
    
    for (t2 in 1: clust.quant ){
      
      
      # GHG emissions
      for (cat in emis.catg){
        
     
        if (cat == emis.catg[1] ) {var <- 'lc.N2O.synthetic.total.Mg.CO2eq' }
        if (cat == emis.catg[2] ) {var <- 'lc.N2O.organic.total.Mg.CO2eq' }
        if (cat == emis.catg[3] ) {var <- 'lc.Biomass.CO2.remv.cc.shade.total.Mg.ha.yr'  }
        if (cat == emis.catg[4] ) {var <- 'lc.Biomass.CO2.remv.cc.cocoa.total.Mg.ha.yr' }
        if (cat == emis.catg[5] ) {var <- 'lc.Biomass.CO2.remv.cc.fruit.total.Mg.ha.yr' }
        if (cat == emis.catg[6] ) {var <- 'lc.Biomass.CO2.remv.cc.interc.total.Mg.ha.yr' }
        if (cat == emis.catg[7] ) {var <- 'lc.SOIL.CO2.Mg.CO2eq'   }
        if (cat == emis.catg[8] ) {var <- 'lc.CH4.Mg.pods.Mg.CO2eq'}
        

        # Name emission category (used for group in ggplot)
        ghg.dat[ghg.row.count, 'Emission.category'] <-  cat
        
        ghg.dat[ghg.row.count, 'Typology' ] <- t1
        ghg.dat[ghg.row.count, 'Type' ] <- t2
        
        ghg.dat[ghg.row.count, 'Type.str' ] <- unique(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'typ.str' ] )
        ghg.dat[ghg.row.count, 'Type.str.fill' ] <- unique(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'typ.str.fill' ] )
        
        # Actual value of emission category
        ghg.dat[ghg.row.count, 'value.mn' ] <- mean(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , var] , na.rm = TRUE)
        
        val <- ghg.dat[ghg.row.count, 'value.mn' ]
        
 
        
        #  Aggregated values
       # ghg.dat[ghg.dat$Typology == t1 & ghg.dat$Type == t2 & !is.na(ghg.dat$Type) & !is.na(ghg.dat$Typology ), 'tot.value.mn' ] %+=%  (val)
        
          
       ghg.dat[ghg.row.count, 'tot.value.mn' ] <- mean(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr' ], na.rm = TRUE)

      ghg.dat[ghg.row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr.95pci' ]))
      #ghg.dat[ghg.row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr.sd' ]))
      
        ghg.row.count %+=% 1
        
      }
      
      
      # VOP uncertainty
      for (cat in rev.catg){
        
        
        # var <-  var.names[ var.names == cat ]
        
        if (cat == rev.catg[1]) {var <- vop.var.names[1]  }
        if (cat == rev.catg[2]) {var <- vop.var.names[2] }
        if (cat == rev.catg[3]) {var <- vop.var.names[3] }
        if (cat == rev.catg[4]) {var <- vop.var.names[4] }
        if (cat == rev.catg[5]) {var <- vop.var.names[5] }
        if (cat == rev.catg[6]) {var <- vop.var.names[6] }
        if (cat == rev.catg[7]) {var <- vop.var.names[7] }
        
        vop.dat[vop.row.count, 'Revenue.category'] <-  cat
        
        vop.dat[vop.row.count, 'Typology' ] <- t1
        vop.dat[vop.row.count, 'Type' ] <- t2
        
        vop.dat[vop.row.count, 'Type.str' ] <- unique(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'typ.str' ] )
        vop.dat[vop.row.count, 'Type.str.fill' ] <- unique(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'typ.str.fill' ] )
   
        if ( (cat == rev.catg[1])  |
             (cat == rev.catg[2])  |
             (cat == rev.catg[3])  |
             (cat == rev.catg[4])  |
             (cat == rev.catg[5])  |
             (cat == rev.catg[6]) ) {
       vop.dat[vop.row.count, 'value.mn' ] <- mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , var]))
        }else {
          
          vop.dat[vop.row.count, 'value.mn' ] <- (-1/1000)*mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , var]))
          
        }
        
        
      #  vop.dat[vop.row.count, 'value.sd' ] <-  sd(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , var]))
        
        # Aggregated values
        vop.dat[vop.row.count, 'tot.value.mn' ] <- mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.net.VOP.1000.usd.per.ha']))

       vop.dat[vop.row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.net.VOP.1000.usd.per.ha.95pci']))
       #vop.dat[vop.row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.net.VOP.1000.usd.per.ha.sd']))
       
        vop.row.count %+=% 1
        
      }
    }
  }
  
  
  
  ghg.row.count  <- 1
  vop.row.count <- 1
  
  ghg.dat[, 'tot.value.mn' ] <- 0
  vop.dat[,'tot.value.mn'] <- 0
  
  for (t1 in  typologies) {
    
    if (t1 == "Hybrid sun") {  clust.quant <- num.clusts.hysn}
    if (t1 == "Hybrid shade") {  clust.quant <- num.clusts.hysh}
    if (t1 == "Amazonia") {  clust.quant <- num.clusts.amz}
    
    
    for (t2 in 1:clust.quant){
      for (cat in emis.catg){
      
      val <- ghg.dat[ghg.row.count, 'value.mn' ]
      ghg.dat[ghg.dat$Typology == t1 & ghg.dat$Type == t2 & !is.na(ghg.dat$Type) & !is.na(ghg.dat$Typology ), 'tot.value.mn' ] %+=%  (val)
      ghg.row.count %+=% 1
      }
      for (cat in rev.catg){
        val <-  vop.dat[vop.row.count, 'value.mn' ]
        vop.dat[ vop.dat$Typology == t1 &  vop.dat$Type == t2 & !is.na( vop.dat$Type) & !is.na( vop.dat$Typology ), 'tot.value.mn' ] %+=%  (val)
        vop.row.count %+=% 1
      }
    }
  }
  

  vop.dat[ is.na(vop.dat$tot.value.sd), 'tot.value.sd' ] <- mean(na.omit(vop.dat[,'tot.value.sd']))

  
  # Factor specification
  # GHG
  
  ghg.dat$Typology <- factor( ghg.dat$Typology   , levels= ordered.typologies)
  
  ordered.emission.categories <<- c(         'CO2 seqn. soil',
                                             'CO2 seqn. other',
                                         'CO2 seqn. fruit',
                                         'CO2 seqn. cocoa',
                                         'CO2 seqn. shade',
                                         'CH4 cocoa',
                                         'N2O organic',
                                         'N2O syn'
                                      )
  

  
  ghg.dat$Emission.category <- factor( ghg.dat$Emission.category   ,  ordered.emission.categories)
  
  ghg.dat[,'Type.str'] <- factor(  ghg.dat[,'Type.str'], levels = ordered.typ)
  
  ghg.dat <<-  ghg.dat
  
  
  # VOP
  vop.dat$Typology <- factor( vop.dat$Typology   , levels= ordered.typologies)
  
  ordered.revenue.categories <<- c('Cocoa'
                                   ,'Fruit trees', 
                                   'Annual crops',
                                   'Other agcommodities',
                                   'Fuelwood',
                                   'Hardwood lumber',
                                   'All cost categories')
  
  vop.dat$Revenue.category <- factor( vop.dat$Revenue.category   , ordered.revenue.categories)
  
  
  vop.dat[,'Type.str'] <- factor(  vop.dat[,'Type.str'] , levels = ordered.typ)
  vop.dat <<-  vop.dat
  
 # View(vop.dat)
 # View(ghg.dat)
  
}  
fig.intrag.dt.prep()

# Figure parameters 
fig.params.intrag <- function(){

  bar.chart.border.color <<- 'black'
  bar.chart.border.thickness <<- 0#0.035
  
 bar.width <<- 0.68
 bar.color <<- '#9aabbc'
 bar.color.border <<- 'black'
 
  box.plot.color <<- "#353839"
  fig.yd.bp.thickness <<- 0.25
  fig.yd.bp.fatten <<- 2
 
   box.plot.fill.color <<- '#ABB2B9' #"#a9a9a9"
   
 error.bar.width <<- 0.15
 error.bar.size <<- 0.28
 error.bar.color <<- '#000000'# '#3f5265'  #353839' #   '#fc6c85'
 vop.error.bar.color <<-  error.bar.color
 point.color.fill <<- '#4c4c4c'
 point.color.border <<- error.bar.color
 point.type <<- 22
 point.size.intrag <<- 1.3725
 point.border.thickness <<- 0.02
 
 y.tick.fs <<- 8.5
 x.tick.fs <<- 10.5
 y.tit.sz <<- 10.5
 y.tit.sz.yd <<- 11.5
 
 x.tick.angle <<- 90
 
 facet.tx.size <<- 11.5
 facet.tx.size.yd <<- 10.5
 
 label.fs <<- 11.5
 
 p.mg.left <<- 0.3
 p.mg.right <<- 0.2
 p.mg.top <<- 0.2
 p.mg.bottom <<- -0.35
 
 p.vop.intrag.mg.left <<- 0.3
 p.vop.intrag.mg.right <<- 0.2
 p.vop.intrag.mg.top <<- 0.2
 p.vop.intrag.mg.bottom <<- -0.21
 
 p.ghg.intrag.mg.left <<- 0.3
 p.ghg.intrag.mg.right <<- 0.2
 p.ghg.intrag.mg.top <<- 0.2
 p.ghg.intrag.mg.bottom <<- -.215
 
 p.yd.mg.top <<- 0.2
 p.yd.mg.right <<- 0.2
 p.yd.mg.bottom <<- 0.125
 p.yd.mg.left <<- 0.3
 
 box.error.bar.width <<- 0.215
 
 fig.ghg.y.lim.max <<- 2.25
 fig.ghg.y.lim.min <<- - 10.5
 
 y.max.vop <<- 1.85
 y.min.vop <<- -0.25
   
 intrag.leg.key.h.ghg <<- 0.285
 intrag.leg.key.w.ghg <<- 0.6
 
 intrag.leg.key.h.vop <<- 0.285
 intrag.leg.key.w.vop <<- 0.6
   
 intrag.leg.space.x.ghg <<- 0.2
 intrag.leg.space.y.ghg <<- 0.10
 
 intrag.leg.space.x.vop <<-  intrag.leg.space.x.ghg 
 intrag.leg.space.y.vop <<- intrag.leg.space.y.ghg
   
 intrag.fig.ghg.leg.text.fs <<- 8.5
 intrag.fig.vop.leg.text.fs <<- 8.5
   
 yd.mean.point.size <<- 1.2
 yd.mean.point.color <<- '#566573'
 yd.mean.point.shape <<- 24
 
 # Variable ranges
 #min.x.intrag.ghgr.int <<- 1.15 * min(na.omit(comp$typ.mn.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated) - na.omit(comp$typ.sd.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated))
 #max.x.intrag.ghgr.int <<- 1.15 * (max(na.omit(comp$typ.mn.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)) + max(na.omit(comp$typ.sd.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)))
 #min.x.intrag.ghgr.int <<- 1.15 * (min(na.omit(comp$typ.mn.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)) - min(na.omit(comp$typ.sd.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)))
 
 
 # GHG Figures
 labels_emis_srcs <<- c(   bquote('Soil '*CO[2]*''),
                           bquote('Annual crop '*CO[2]*''),
                          bquote('Fruit tree '*CO[2]*''),
                         bquote('Cocoa tree '*CO[2]*''),
                          bquote('Shade tree '*CO[2]*''),
                         bquote('Cocoa residues '*CH[4]*''),
                      bquote('Organic N  '*N[2]*'O'),
                      bquote('Fertiliser N '*N[2]*'O')
                      
                      )
 
 
 colors_emis_srcs <<- c(  '#cfcfc4' , #'#dcdcdc' , # '#99A6B2',
                        '#D5F5E3',
   '#82E0AA',
                          '#28B463',
                          '#279757',
 '#6495ed' ,  #'#87b5eb' , 
   '#E5981B',
                         '#F5B857'  
 )

 # VOP figure
 labels_rev_srcs <<-  ordered.revenue.categories
 
 colors_rev_srcs <<-  c( '#D77345' ,
                        '#9acd32', 
                                           '#ffef00',
                                           '#FF981B',
                                        '#bfa584'  ,  
                        '#9E8363', 
                        '#989898' 
)
 

 
}
fig.params.intrag()

intrag.figs <- function(){   
  
  fig.barintrag.ghgr.t <- ggplot( data = ghg.dat[!is.na(ghg.dat$Typology),] , aes( fill = Emission.category) )  +
    geom_bar(aes(y = value.mn  , x = Type.str.fill  ) , position="stack", stat="identity" , width = bar.width , colour = NA )+
    geom_errorbar(aes(ymin = tot.value.mn -  tot.value.sd, ymax = tot.value.mn +  tot.value.sd , x = Type.str.fill) , width = error.bar.width , size = error.bar.size , color = error.bar.color
    ) + 
  geom_point( aes( y = tot.value.mn , x = Type.str.fill),stat = "identity",  shape = point.type  , size = point.size.intrag ,color = point.color.border , fill = point.color.fill ,   stroke =  point.border.thickness )  +
    scale_fill_manual(labels = labels_emis_srcs  , values = colors_emis_srcs ) +
    xlab('')  +
    ylab(bquote('Net GHG emissions (Mg  '*CO[2]*'eq '*ha^-1*' '*yr^-1*')'))  +
    facet_grid( cols = vars(Typology) , scales = "free_x", space = "free_x")   +
    coord_cartesian( ylim = c(fig.ghg.y.lim.min, fig.ghg.y.lim.max)) +
    guides(fill = guide_legend(byrow = TRUE))+
    theme(
      # Margin
      plot.margin = unit(c(p.ghg.intrag.mg.top , p.ghg.intrag.mg.right, p.ghg.intrag.mg.bottom ,p.ghg.intrag.mg.left), "cm"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = x.tick.angle, face = 'italic' , vjust = 0.5, hjust=1, size = x.tick.fs),
      axis.text.y = element_blank(), 
       # Legend
      legend.position = "none",
     axis.title.y = element_blank(),
     strip.text.x = element_text(size =  facet.tx.size, face = 'italic' , color = 'black'),
      strip.background = element_rect(color = 'black' , fill='white' ,  size=1.0 , linetype="solid"),
      # Panel
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black",
                                  fill=NA, size=1))
 

  fig.bar.intrag.vop.b <- ggplot( data = vop.dat[ !is.na(vop.dat$Typology), ] ,  aes(y = value.mn  , x = Type.str.fill ) )  +
    geom_bar( aes(  fill = Revenue.category), position =  position_stack(bar.width) , stat="identity", width = bar.width , colour= NA )+
    geom_errorbar(aes(ymin = tot.value.mn -  tot.value.sd , ymax = tot.value.mn +  tot.value.sd ),  width = error.bar.width , size = error.bar.size , color = vop.error.bar.color
    ) +
    geom_point( aes( y = tot.value.mn ),stat = "identity",  shape = point.type  , size = point.size.intrag ,color = point.color.border , fill = point.color.fill ,   stroke =  point.border.thickness )  + # color = point.color.border , fill = point.color.fill )  +
    scale_fill_manual(labels = labels_rev_srcs  , values = colors_rev_srcs )+
    xlab('') +
   facet_grid( cols = vars(Typology) , scales = "free_x", space = "free_x")   +
     scale_y_continuous(
      breaks = seq(0.0, 1.5, by = 0.25))+
    coord_cartesian( ylim = c(y.min.vop, y.max.vop)) +
   guides(fill = guide_legend(byrow = TRUE))+
    theme(
      plot.margin = unit(c(p.vop.intrag.mg.top , p.vop.intrag.mg.right, p.vop.intrag.mg.bottom , p.vop.intrag.mg.left), "cm"),
     # axis.ticks.x = element_blank(),
  #legend.background = element_rect(fill = 'white', size = 0.35, linetype = "solid",  colour = NA),
      axis.ticks.y = element_blank(),
  axis.ticks.x = element_blank(),
      axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, face = 'italic' , hjust=1, size = x.tick.fs),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      # Legend
  legend.position = "none",
       axis.title.y = element_blank(), 
      strip.text.x = element_text(size =  facet.tx.size, color = 'black' , face = 'italic' ),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1))  #810,320
  
  

  fig.intrag.yg.b <- ggplot( data= comp[!is.na(comp$typology)   &  !is.na(comp$typ.str)     ,]  ) +
 stat_boxplot(aes(y = yld_pc_attain_plot  , x = typ.str.fill, group = typ.str.fill ), outlier.shape = NA ,  coef = 10 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 , lwd= fig.yd.bp.thickness , fatten = fig.yd.bp.fatten)+
   stat_boxplot(aes(y = yld_pc_attain_plot  , x = typ.str.fill, group = typ.str.fill ), geom = 'errorbar' , coef = 10 , color = box.plot.color,  alpha= 5 , lwd= fig.yd.bp.thickness , width = box.error.bar.width)+
    #   stat_summary( aes(y = yld_pc_attain_plot  , x = typ.str.fill, group = typ.str.fill ),
    #   geom = "point",  fun.y = "mean", col = "black", size = yd.mean.point.size ,  shape =  yd.mean.point.shape,  fill =  yd.mean.point.color   ) +
    xlab('') +
    coord_cartesian( ylim = c(0.0, 100.0)) +
  ylab('  Percent attainable yield (%)')+
    facet_grid( cols = vars(typology) , scales = "free_x", space = "free_x")   +
theme(
    plot.margin = unit(c(p.mg.top,p.mg.right,-.2,p.mg.left), "cm"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),  axis.text.x = element_blank(),
    axis.text.y = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.y =  element_blank(), 
    strip.text.x = element_text(size =  facet.tx.size.yd, face = 'italic' ,color = 'black'),
  strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )

fig.intrag.yd.act.b <- ggplot( data = comp[!is.na(comp$typology)   &  !is.na(comp$typ.str)   &  !is.na(comp$typ.str.fill)   ,]  ) +
 geom_boxplot(aes(y = cc.yd.lc.mn.Mg.ha  , x = typ.str.fill , group = typ.str.fill) ,  outlier.shape = NA, coef = 5 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 , lwd= fig.yd.bp.thickness , fatten = fig.yd.bp.fatten )+
  stat_boxplot(aes(y = cc.yd.lc.mn.Mg.ha  , x = typ.str.fill, group = typ.str.fill ), geom = 'errorbar' , coef = 10 , color = box.plot.color,  alpha= 5 , lwd= fig.yd.bp.thickness , width = box.error.bar.width) +
  # stat_summary( aes(y = cc.yd.lc.mn.Mg.ha , x = typ.str.fill, group = typ.str.fill ),
  #      geom = "point",  fun.y = "mean", col = "black", size = yd.mean.point.size ,  shape =  yd.mean.point.shape,  fill =  yd.mean.point.color   ) +
  ylab(bquote('Actual yield (Mg  '*ha^-1*' '*yr^-1*')    '))+
  facet_grid( cols = vars(typology) , scales = "free_x", space = "free_x")   +
  scale_y_continuous(breaks = seq(0, 3, by = .5), labels = scales::number_format(accuracy = 0.1))+
  theme(
    plot.margin = unit(c(p.yd.mg.top , p.yd.mg.right , p.yd.mg.bottom , p.yd.mg.left*.9), "cm"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = x.tick.angle , vjust = 0.5, hjust=1, size = x.tick.fs , face = 'italic' ),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.y =  element_blank(), 
    strip.text.x = element_text(size =  facet.tx.size.yd, face = 'italic' ,color = 'black'),
    strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )




fig.intrag.yg <<- annotate_figure(fig.intrag.yg.b ,   fig.lab = "b", fig.lab.pos ="top.left", fig.lab.size = label.fs )

fig.intrag.yd.act <<- annotate_figure(fig.intrag.yd.act.b ,   fig.lab = "d", fig.lab.pos ="top.left", fig.lab.size = label.fs )


fig.barintrag.ghgr.t <<-  annotate_figure(fig.barintrag.ghgr.t,   fig.lab = "b", fig.lab.pos ="top.left", fig.lab.size = label.fs)

fig.bar.intrag.vop  <<- annotate_figure( fig.bar.intrag.vop.b ,   fig.lab = "b", fig.lab.pos ="top.left", fig.lab.size = label.fs)






}
intrag.figs()



fig.bar.intrag.vop
fig.barintrag.ghgr.t 
fig.intrag.yg
fig.intrag.yd.act

# Data prep for inter group figures
fig.interg.dt.prep <- function(){

  
  #Absolute GHG data
  ghg.dat.var.names <- c ('Typology',
                             'Emission.category',
                             'value.mn',
                             'tot.value.mn',
                             'tot.value.sd',
                             'facet.lab')
  
  
  emis.catg <- c('N2O syn',
                 'N2O organic',
                 'CO2 seqn. shade',
                 'CO2 seqn. cocoa',
                 'CO2 seqn. fruit',
                 'CO2 seqn. other',
                 'CO2 seqn. soil',
                 'CH4 cocoa'
                 )
  
  rows <- seq( 1: (3*length(emis.catg)) )
  inter.ghg.dat <- data.frame(  matrix(ncol = length(ghg.dat.var.names) , nrow = length(rows) , dimnames=list(rows  , ghg.dat.var.names )))
  
  row.count <- 1

  inter.ghg.dat[, 'tot.value.mn' ] <- 0
  
  for (t1 in typologies) {
      for (cat in emis.catg){
        
        
        var <-  ghg.ab.dat.var.names[ ghg.ab.dat.var.names == cat ]
        
        if (cat == emis.catg[1] ) {var <- 'lc.N2O.synthetic.total.Mg.CO2eq' }
        if (cat == emis.catg[2] ) {var <- 'lc.N2O.organic.total.Mg.CO2eq' }
        if (cat == emis.catg[3] ) {var <- 'lc.Biomass.CO2.remv.cc.shade.total.Mg.ha.yr' }
        if (cat == emis.catg[4] ) {var <- 'lc.Biomass.CO2.remv.cc.cocoa.total.Mg.ha.yr' }
        if (cat == emis.catg[5] ) {var <- 'lc.Biomass.CO2.remv.cc.fruit.total.Mg.ha.yr' }
        if (cat == emis.catg[6] ) {var <- 'lc.Biomass.CO2.remv.cc.interc.total.Mg.ha.yr' }
        if (cat == emis.catg[7] ) {var <- 'lc.SOIL.CO2.Mg.CO2eq' }
        if (cat == emis.catg[8] ) {var <- 'lc.CH4.Mg.pods.Mg.CO2eq' }
       
        
        # vr <- var.names[]
        
        ids <- comp[comp$typology ==  t1  & !is.na(comp$typology)  , 'hhID']
        
        inter.ghg.dat[row.count, 'Emission.category'] <-  cat
        
        inter.ghg.dat[row.count, 'Typology' ] <- t1

        inter.ghg.dat[row.count, 'value.mn' ] <- mean(comp[comp$hhID %in% ids , var] , na.rm =  TRUE)
        val <-   inter.ghg.dat[row.count, 'value.mn' ]

        #  Aggregated values
       # inter.ghg.ab.dat[row.count, 'tot.value.mn' ] <- mean(comp[comp$hhID %in% ids  , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr' ])
        #inter.ghg.ab.dat[inter.ghg.ab.dat$Typology == t1 & !is.na(inter.ghg.ab.dat$Typology), 'tot.value.mn' ] %+=%   (val )
        
        
        inter.ghg.dat[row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$hhID %in% ids  , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr.95pci' ]))
        #inter.ghg.dat[row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$hhID %in% ids  , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr.sd' ]))
        
        
        inter.ghg.dat[row.count, 'facet.lab' ] <-  'All systems'
        row.count %+=% 1
        
      }
  }
  
  inter.ghg.dat[, 'tot.value.mn' ] <- 0

  ghg.row.count <- 1
  
  for (t1 in typologies) {
    for (cat in emis.catg){
      
      val <-   inter.ghg.dat[ ghg.row.count, 'value.mn' ]
    inter.ghg.dat[inter.ghg.dat$Typology == t1 & !is.na(inter.ghg.dat$Typology), 'tot.value.mn' ] %+=%   (val )
      
      ghg.row.count %+=% 1
    }
  }
  
  
  # Value of production
  vop.dat.var.names <- c ('Typology',
                          'Revenue category',
                          'value.mn',
                          'tot.value.mn',
                          'tot.value.sd',
                          'facet.lab')

  
  
  rows <- seq(1:(3*4))
  
  interg.vop.dat <- data.frame(  matrix(ncol = length(vop.dat.var.names) , nrow = 12 , dimnames=list(rows  , vop.dat.var.names )))
  
  vop.dat$Typology <- factor( vop.dat$Typology   , levels= ordered.typologies)
  

  rev.catg <- c('Cocoa',
                'Fuelwood',
                'Hardwood lumber',
                'Annual crops',
                'Fruit trees',
                'Other agcommodities',
                'Total costs'
  )
  
  
  vop.var.names <- c('lc.net.VOP.1000.usd.per.ha.frac.rev.basis.cc',
                     'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.fuelw',
                     'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.hardw',
                     'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ann.crop',
                     'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.frt',
                     'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ag.cmd',
                     'total.costs.usd.per.ha'
  )
  
  row.count <- 1
  
  for (t1 in typologies) {
      for (cat in rev.catg){
        
        if (cat == rev.catg[1]) {var <- vop.var.names[1]  }
        if (cat == rev.catg[2]) {var <- vop.var.names[2]  }
        if (cat == rev.catg[3]) {var <- vop.var.names[3]  }
        if (cat == rev.catg[4]) {var <- vop.var.names[4]  }
        if (cat == rev.catg[5]) {var <- vop.var.names[5]  }
        if (cat == rev.catg[6]) {var <- vop.var.names[6]  }
        if (cat == rev.catg[7]) {var <- vop.var.names[7]  }
        
        
        ids <- comp[comp$typology ==  t1  & !is.na(comp$typology) & !is.na(comp$typ) , 'hhID']
        
        interg.vop.dat[row.count, 'Revenue.category'] <-  cat
        
        interg.vop.dat[row.count, 'Typology' ] <- t1
        
        if ( (cat == rev.catg[1])  |
             (cat == rev.catg[2])  |
             (cat == rev.catg[3])  |
             (cat == rev.catg[4])  |
             (cat == rev.catg[5])  |
             (cat == rev.catg[6]) ) {
        interg.vop.dat[row.count, 'value.mn' ] <- mean(na.omit(comp[comp$hhID %in% ids, var]))
        } else{
          interg.vop.dat[row.count, 'value.mn' ] <- (-1/1000)* mean(na.omit(comp[comp$hhID %in% ids, var]))
        }

        # Aggregated values
        interg.vop.dat[row.count, 'tot.value.mn' ] <- mean(na.omit(comp[comp$hhID %in% ids, 'lc.net.VOP.1000.usd.per.ha']))
        interg.vop.dat[row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$hhID %in% ids , 'lc.net.VOP.1000.usd.per.ha.95pci']))
        #interg.vop.dat[row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$hhID %in% ids , 'lc.net.VOP.1000.usd.per.ha.sd']))
        
        interg.vop.dat[row.count, 'facet.lab' ] <-  'All systems'
        row.count %+=% 1
        
      }
  }
  
  interg.vop.dat <<- interg.vop.dat
  
  # Factor specification
  # Absolute GHG
  inter.ghg.dat$Typology <- factor( inter.ghg.dat$Typology   , levels= ordered.typologies)
  
 # ordered.emission.categories <<-   ordered.ab.emission.categories
  inter.ghg.dat$Emission.category <- factor( inter.ghg.dat$Emission.category   ,  ordered.emission.categories)
  
  display.names.emission <<-  labels_emis_srcs

  inter.ghg.dat  <<-  inter.ghg.dat
  
  
  # VOP
  ordered.typologies <- c('Hybrid sun' ,  'Hybrid shade' , 'Amazonia' )
  interg.vop.dat$Typology <- factor( interg.vop.dat$Typology   , levels= ordered.typologies)
  
  ordered.revenue.categories <-   ordered.revenue.categories
  
  interg.vop.dat$Revenue.category <- factor( interg.vop.dat$Revenue.category   , ordered.revenue.categories)
  
  interg.vop.dat <<-  interg.vop.dat
  
  
}  
fig.interg.dt.prep()

# Parameters for intergroup figures
fig.params.interg <- function(){
  
  # GGPLOT settings
  # Legend row spacing
  fig.bar.inter.ghg.legend.row.spacing.cm <<- 0.25
  fig.bar.inter.nvp.row.spacing.cm <<- 0.25
  
  bar.interg.legend.left.margin <<- 7
  bar.interg.legend.right.margin <<- 2
  
  fig.bar.interg.vop.p.mg.bottom <<- 1.3
  fig.bar.intergr.ghgr.p.mg.top <<-  p.mg.top
  fig.bar.intergr.ghgr.p.mg.left <<- 0.1
  
  fig.bar.intergr.vop.p.mg.top <<-  fig.bar.intergr.ghgr.p.mg.top
  fig.bar.intergr.vop.p.mg.left <<-   fig.bar.intergr.ghgr.p.mg.left
  
  p.mg.interg.ghg.bottom <<- 3.3
  p.mg.interg.vop.bottom <<- 3.05

  p.interg.yd.mg.top <<- 0.2
  p.interg.yd.mg.right <<- 0.2
  p.interg.yd.mg.bottom  <<- 3.18
  p.interg.yd.mg.left <<- 0.05
  
  p.interg.yg.mg.top <<- 0.2
  p.interg.yg.mg.right <<- 0.2
  p.interg.yg.mg.bottom <<- -0.3
  p.interg.yg.mg.left <<- 0.32
  
  interg.leg.key.h.ghg <<- 0.37
  interg.leg.key.w.ghg <<- 0.5
  interg.leg.key.h.vop <<- 0.37
  interg.leg.key.w.vop <<- 0.6
  
  fig.interg.ghg.x.tick.fs <<- x.tick.fs 
  fig.interg.vop.x.tick.fs <<-  fig.interg.ghg.x.tick.fs 
  
  fig.interg.ghg.y.tick.fs <<- 9
  fig.interg.vop.y.tick.fs <<- 9
  
  fig.interg.ghg.y.tit.fs <<- 11.5
  fig.interg.vop.y.tit.fs <<- 11.5
  
  interg.fig.ghg.leg.text.fs <<- 8
  interg.fig.vop.leg.text.fs <<- 8.5
  
  interg.leg.space.x.vop <<- 0.3
  interg.leg.space.y.vop <<- 0.12
  
  fig.interg.yg.y.tick.fs <<- 7
  fig.interg.yg.y.tit.fs <<- 10.0
  fig.interg.yg.x.tick.fs <<-  fig.interg.ghg.x.tick.fs 
  
  fig.interg.ghg.bar.width <<- 0.65
  fig.interg.vop.bar.width <<- 0.65
  
  fig.interg.yg.bar.width  <<- 0.65
  
  fig.vop.y.lim.min <<-  -0.25
  fig.vop.y.lim.max <<- 1.875

  
  point.size.interg <<- 1.05 * point.size.intrag
  
  facet.tx.size.interg.yg <<- 10.5
  facet.tx.size.interg.yd <<- 10.5
  
  fig.ghg.leg.x.coord <<- 0.5
  fig.ghg.leg.y.coord <<- -0.76
  
  fig.vop.leg.x.coord <<-   .7
  fig.vop.leg.y.coord <<- -0.7
  
}
fig.params.interg()


interg.figs <- function(){
  
  fig.bar.intergr.ghgr.t.b <- ggplot( data = inter.ghg.dat[,]  )  +
    geom_bar(aes(y = value.mn  , x = Typology, fill = Emission.category) , position="stack", stat="identity" , width = fig.interg.ghg.bar.width , colour = NA , size=bar.chart.border.thickness ) +
    geom_point(aes(y = tot.value.mn    , x = Typology),stat = "identity",  shape = point.type, size = point.size.interg  , color = point.color.border , fill=point.color.fill  , stroke =  point.border.thickness)  +
    geom_errorbar(aes(ymin = tot.value.mn -  tot.value.sd, ymax = tot.value.mn +  tot.value.sd , x = Typology) , width= error.bar.width , size = error.bar.size , color = error.bar.color
    ) +
    scale_fill_manual(labels =   display.names.emission , values = colors_emis_srcs ) +
    xlab('') +
    ylab('') +
    scale_y_continuous(
      limits = c( fig.ghg.y.lim.min , fig.ghg.y.lim.max) , breaks = seq(-10.0, 20, by = 2.0), 
      labels = scales::number_format(accuracy = 0.1))  +
    coord_cartesian( ylim = c(fig.ghg.y.lim.min, fig.ghg.y.lim.max)) +
   facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
    ylab(bquote('Net GHG emissions (Mg '*CO[2]*'eq '*ha^-1*' '*yr^-1*')       '))  +
    guides(fill = guide_legend(byrow = TRUE)) +
    theme(
      # Legend
      legend.background = element_rect(fill = 'white', size = 0.35, linetype = "solid",  colour = NA),
      legend.key.height = unit(intrag.leg.key.h.ghg, 'cm'),
      legend.key.width = unit(intrag.leg.key.w.ghg, 'cm'),
      legend.spacing.y = unit(intrag.leg.space.y.ghg, 'cm'),
      legend.spacing.x = unit(intrag.leg.space.x.ghg  , 'cm'),
      legend.position = c(fig.ghg.leg.x.coord , fig.ghg.leg.y.coord),
      legend.margin = margin(1.1 , 1.1 , 1.1 , 1.1) ,
      legend.title = element_blank(),
      legend.text = element_text(size =   interg.fig.ghg.leg.text.fs),
      #
      #legend.text = element_text(size = interg.fig.ghg.leg.text.fs),
      plot.margin = unit(c(fig.bar.intergr.ghgr.p.mg.top , p.mg.right , p.mg.interg.ghg.bottom , fig.bar.intergr.ghgr.p.mg.left), "cm"),
      axis.ticks.x = element_blank(),
      axis.text.y =  element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs),
     axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, hjust=1, face = 'italic' , size = fig.interg.ghg.x.tick.fs ),
    panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
     # legend.position = "none",
      axis.title.y = element_text(size =  y.tit.sz), 
      strip.text.x = element_text(size =  facet.tx.size, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1))

  
  fig.bar.interg.vop.b <- ggplot( data = interg.vop.dat[]  )  +
    geom_bar(aes(y = value.mn  , x = Typology, fill = Revenue.category) , position="stack", stat="identity" , width = fig.interg.vop.bar.width, colour = NA , size = bar.chart.border.thickness) +
     geom_errorbar(aes(ymin = tot.value.mn -  tot.value.sd, ymax = tot.value.mn +  tot.value.sd , x = Typology) ,     width = error.bar.width , size = error.bar.size , color = vop.error.bar.color  ) +
    geom_point(aes(y = tot.value.mn    , x = Typology) , stat = "identity",  shape = point.type, size = point.size.interg , color = point.color.border , fill = point.color.fill ,   stroke =  point.border.thickness)  +
    scale_fill_manual(labels = labels_rev_srcs  , values = colors_rev_srcs) +
    xlab('') +
    ylab('') +
    scale_y_continuous(
      limits = c( y.min.vop,  y.max.vop) ,breaks = seq(0.0, 1.5, by = 0.5))+
  guides(fill = guide_legend(byrow = TRUE)) +
    facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
   ylab(bquote('Value of production (1000 USD  '*ha^-1*' '*yr^-1*')    ')) +
    theme(
   plot.margin = unit(c(fig.bar.intergr.vop.p.mg.top , p.mg.right ,   p.mg.interg.vop.bottom , fig.bar.intergr.vop.p.mg.left), "cm"),
      axis.ticks.x = element_blank(),
     # axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, hjust=1, face = 'italic' ,size = fig.interg.ghg.x.tick.fs) ,
      axis.text.y =  element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
   # Legend
   legend.background = element_rect(fill = 'white', size = 0.35, linetype = "solid",  colour = NA),
   legend.key.height = unit(intrag.leg.key.h.ghg, 'cm'),
   legend.key.width = unit(intrag.leg.key.w.ghg, 'cm'),
   legend.spacing.y = unit(interg.leg.space.y.vop, 'cm'),
   legend.spacing.x = unit(interg.leg.space.x.vop  , 'cm'),
   legend.position = c(fig.vop.leg.x.coord , fig.vop.leg.y.coord),
   legend.margin = margin(1.1 , 1.1 , 1.1 , 1.1) ,
   legend.title = element_blank(),
   legend.text = element_text(size =   interg.fig.vop.leg.text.fs),
   
    # legend.text = element_text(size = intrag.fig.vop.leg.text.fs),
     #legend.key.height = unit(intrag.leg.key.h.vop, 'cm'),
     #legend.key.width = unit(intrag.leg.key.w.vop, 'cm'),
     #legend.spacing.x = unit(intrag.leg.space.x.vop, 'cm'),
     #legend.spacing.y = unit(intrag.leg.space.y.vop, 'cm'),
 #legend.position = "none",
    # legend.title =  element_blank(),
   #legend.background = element_rect(fill=alpha('white', 0.4)),
    # legend.margin = margin(.00005,.00005,.00005,.00005),
    # legend.box.margin = margin(.00005,.00005,.00005,.00005),
   axis.title.y = element_text(size =  y.tit.sz),
 strip.text.x = element_text(size =  facet.tx.size, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1))
    
  fig.interg.yg.b <- ggplot( data= comp[ !is.na(comp$typology) &  !(comp$hhID %in%  hh_exclude.cc.yd.typ )   &  !is.na(comp$typology)    & !is.na(comp$yld_pc_attain_plot) ,]) +
    stat_boxplot(aes(y = yld_pc_attain_plot  , x = typology, group = typology ), geom = 'errorbar' , coef = 10 , color = box.plot.color,  alpha= 5 , lwd= fig.yd.bp.thickness , width = box.error.bar.width)+
    geom_boxplot(aes(y = yld_pc_attain_plot  , x = typology, group = typology), width = fig.interg.yg.bar.width , outlier.shape = NA, coef = 5 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 ,  lwd= fig.yd.bp.thickness , fatten = fig.yd.bp.fatten )+
    #  stat_summary( aes(y = yld_pc_attain_plot  , x = typology, group = typology ),
    #   geom = "point",  fun.y = "mean", col = "black", size = yd.mean.point.size ,  shape =  yd.mean.point.shape,  fill =  yd.mean.point.color   ) +
    xlab('') +
    ylab('')+
    coord_cartesian(ylim=c(0, 100.0)) +
    scale_y_continuous(limits = c(0.0, 100.0) ,breaks = seq(0, 100, by = 25), labels = scales::number_format(accuracy = 1.0))+
  ylab('  Percent attainable yield (%)')+
    facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
    theme(
      plot.margin = unit(c( p.interg.yg.mg.top  , p.interg.yg.mg.right, p.interg.yg.mg.bottom, p.interg.yg.mg.left), "cm"), 
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(vjust = 0.5, hjust=01, size = y.tick.fs),
      axis.text.x = element_blank(),   panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title.y = element_text(size =   fig.interg.yg.y.tit.fs ), 
      strip.text.x = element_text(size =    facet.tx.size.interg.yg, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1)
    )

  fig.interg.yd.act.b <- ggplot( data= comp[ !is.na(comp$typology) &  !(comp$hhID %in%  hh_exclude.cc.yd.typ )   &  !is.na(comp$typology)    & !is.na(comp$cc.yd.lt.mn.Mg.ha) ,]) +
    geom_boxplot(aes(y = cc.yd.lc.mn.Mg.ha   , x = typology, group = typology), width = fig.interg.yg.bar.width ,  outlier.shape = NA, coef = 5 , color = box.plot.color , fill = box.plot.fill.color , alpha= 0.5 ,  lwd= fig.yd.bp.thickness ,  fatten = fig.yd.bp.fatten)+
    #stat_boxplot(aes(y = cc.yd.lc.mn.Mg.ha   , x = typology, group = typology ), geom = 'errorbar' , coef = 10 , color = box.plot.color,  alpha= 5 , lwd= fig.yd.bp.thickness , width = box.error.bar.width)+
    #  stat_summary( aes(y = cc.yd.lc.mn.Mg.ha , x = typology, group = typology  ),
    #   geom = "point",  fun.y = "mean", col = "black", size = yd.mean.point.size ,  shape =  yd.mean.point.shape,  fill =  yd.mean.point.color   ) +
    xlab('') +
    ylab('') +
    coord_cartesian(ylim=c(0,  1.5)) +
    facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
  scale_y_continuous( labels = scales::number_format(accuracy = 0.01))+
    ylab(bquote('Actual yield (Mg  '*ha^-1*' '*yr^-1*')    '))+
    theme(
      plot.margin = unit(c( p.interg.yd.mg.top  , p.interg.yd.mg.right, p.interg.yd.mg.bottom, p.interg.yd.mg.left), "cm"),
      axis.ticks.x = element_blank(),
  axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs , face = 'italic'  ),
      axis.text.x = element_text( face = 'italic' , angle = x.tick.angle , vjust = 0.5, hjust=1, size = fig.interg.yg.x.tick.fs ),
    panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title.y = element_text(size =    fig.interg.yg.y.tit.fs ),
      strip.text.x = element_text(size =    facet.tx.size.interg.yd , face = 'italic' ,color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1)
    )  
  
  # Merge inter and intra group figures into panelled figures
  fig.interg.yg <<- annotate_figure( fig.interg.yg.b,   fig.lab = "a", fig.lab.pos ="top.left", fig.lab.size = label.fs )   
  fig.interg.yd.act <<-  annotate_figure( fig.interg.yd.act.b,   fig.lab = "c", fig.lab.pos ="top.left", fig.lab.size = label.fs )  
  
  fig.bar.intergr.ghgr.t <<- annotate_figure( fig.bar.intergr.ghgr.t.b,   fig.lab = "a", fig.lab.pos ="top.left", fig.lab.size = label.fs)
  fig.bar.interg.vop <<- annotate_figure(fig.bar.interg.vop.b,   fig.lab = "a", fig.lab.pos ="top.left", fig.lab.size = label.fs)
  
  
  fig.yd   <<- plot_grid(    fig.interg.yg , 
                             fig.intrag.yg , 
                             fig.interg.yd.act, 
                            fig.intrag.yd.act , 
                            align = "h", 
                            nrow = 2, 
                            ncol = 2 , 
                            rel_widths = c(26/100, 74/100),
                            rel_heights = c(32/100, 68/100))
  
  fig.vop   <<- plot_grid(   fig.bar.interg.vop, 
                             fig.bar.intrag.vop , 
                            align = "h", 
                            nrow = 1, 
                            ncol = 2 , 
                            rel_widths = c(24/100, 76/100))
  
  fig.ghg   <<- plot_grid(  fig.bar.intergr.ghgr.t, 
                            fig.barintrag.ghgr.t  , 
                                align = "h", 
                                nrow = 1, 
                                ncol = 2 , 
                                rel_widths = c(24/100, 76/100))
  
  ggsave("fig.yd.pdf",   fig.yd    , path = "Figures.out", width=1000, height=700, units="px", scale=2.5)
  
  ggsave("fig.yd.jpeg",   fig.yd    , path = "Figures.out", width=1000, height=700, units="px", scale=2.5)
  
  
  ggsave("fig.vop.pdf",  fig.vop   , path = "Figures.out", width=1250, height=600, units="px", scale=2.5)
  
  ggsave("fig.ghg.pdf",   fig.ghg     , path = "Figures.out", width=1250, height=600, units="px", scale=2.5)
  
  ggsave("fig.ghg.jpeg",  fig.ghg  , path = "Figures.out", width=1250, height= 600, units="px", scale=2.5)
  
  ggsave("fig.vop.jpeg",  fig.vop  , path = "Figures.out", width=1250, height= 600, units="px", scale=2.5)
  
  
}
interg.figs()


fig.ghg   # 950 , 450
fig.vop

fig.yd


fig.bar.intergr.ghgr.t
fig.bar.interg.vop
fig.interg.yg
fig.interg.yd.act 

# Correlation plot

corr.plot.gen <- function(){
  
  
  cor.plot.data.prep.intra <- function(){
    
    
    comp$shade.tree.to.cm.tree.ratio.pct <-  comp$shade.tree.to.cm.tree.ratio * 100
   
    
    comp$elev <- NA
    comp$temp.mmm <- NA
    comp$precip <- NA
    
    for (id in T.df$hhID){
      
      comp[comp$hhID == id , 'elev'] <-  T.df[T.df$hhID == id , 'elev']
      comp[comp$hhID == id , 'temp.mmm'] <-  T.df[T.df$hhID == id , 'temp.mmm']
      comp[comp$hhID == id , 'precip'] <-  T.df[T.df$hhID == id , 'precip']
    }
    comp <<- comp

    
    cor.data.intra.var.names <- c('name' , 
                                  'name.2r',
                                  'typ',
                                  'typology' ,
                                  'variable.cat',
                                  # variable values
                                  'typ.mn' ,
                                  'sys.mn',
                                  'var.v',
                                  'var.s',
                                  'dir.d',
                                  'dir.d.str',
                                  'var.label'
    )
    
    
    cor.data.var.names <<- c('yld_pc_attain_plot' , 
                             'tree.count.per.ha' ,
                             'shade.tree.to.cm.tree.ratio.pct' , 
                             'plot.quant.trees.per.ha.greater.than.20.m' , 
                             'plot.quant.trees.per.ha.greater.than.35.m' ,
                             'plot.quant.trees.per.ha.greater.than.50.m' ,
                             'unique.tree.species' ,
                             'num_other_root_grain_crops' ,
                             
                             'total_N_fert_applied_kg_per_ha' , 
                             'total_non_N_fert_applied_kg_per_ha' ,
                             'prunes_per_year' ,
                             'weeds_per_year' ,
                             'pollination.bool' ,
                             'insecticides.bool' ,
                             'fungicides.bool' ,
                             'herbicides.bool' ,
                             
                             'detect.bool.blackpod' ,
                             'detect.bool.capsid' ,
                             'detect.bool.stemborer' ,
                             'water.log.bool', 
                             'elev',
                            'temp.mmm',
                             'precip'
                             )
    

    
    cor.data.var.labels.old <<- c( '% attainable yield',
                               'Cocoa tree density (trees/ha)',
                               'Shade to cocoa trees',
                               'Short shade trees (%)',
                               'Medium shade trees (/ha)',
                               'Tall shade trees (/ha)',
                               'Unique tree species(/900 m^2)',
                               'Unique crop species (/900 m^2)',
                               
                               'N fertiliser (kg/ha/yr)' , 
                               'Non N-fertiliser (kg/ha/yr)' , 
                               'Pruning frequency (/year)' ,
                               'Weeding frequency  (/year)' ,
                               'Pollination (dummy)' ,
                               'Insecticides (dummy)' ,
                               'Fungicides (dummy)' ,
                               'Herbicides (dummy)' ,
                               
                               'Blackpod (dummy)' ,
                               'Capsid (dummy)' ,
                               'Stemborer (dummy)' ,
                               'Water logged (dummy)' ,
                               'Elevation (m asl)',
                               'Max. temp (Celsius)',
                               'Rainfall (mm/month)'
                               )
    
    cor.data.var.labels <<- c( '% attainable yield',
                               'Cocoa tree density',
                               'Shade to cocoa trees',
                               'Short shade trees',
                               'Medium shade trees',
                               'Tall shade trees',
                               'Unique tree species',
                               'Unique crop species',
                               
                               'N fertiliser' , 
                               'Non N-fertiliser' , 
                               'Pruning frequency' ,
                               'Weeding frequency' ,
                               'Pollination' ,
                               'Insecticides' ,
                               'Fungicides' ,
                               'Herbicides' ,
                               'Blackpod' ,
                               'Capsid' ,
                               'Stemborer' ,
                               'Water logged' ,
                               'Elevation',
                               'Max. temp',
                               'Rainfall'
    )
    
    cor.data.var.labels.2r <<- c( '% attainable\nyield',
                                 'Cocoa tree\ndensity',
                                 'Shade to\ncocoa trees',
                                 'Short\nshade trees',
                                 'Medium\nshade trees',
                                 'Tall shade\ntrees',
                                 'Unique\ntree species',
                                 'Unique\ncrop species',
                                 
                                 'N fertiliser' , 
                                 'Non\nN-fertiliser' , 
                                 'Pruning\nfrequency' ,
                                 'Weeding\nfrequency' ,
                                 'Pollination' ,
                                 'Insecticides' ,
                                 'Fungicides' ,
                                 'Herbicides' ,
                                 'Blackpod' ,
                                 'Capsid' ,
                                 'Stemborer' ,
                                 'Water logged' ,
                                 'Elevation',
                                 'Max. temp',
                                 'Rainfall'
    )
    
    
    cor.data.variable.cats <<- c('Yield',
                                 'Vegetative structure & diversity',
                                 'Vegetative structure & diversity',
                                 'Vegetative structure & diversity',
                                 'Vegetative structure & diversity',
                                 'Vegetative structure & diversity',
                                 'Vegetative structure & diversity',
                                 'Vegetative structure & diversity',
                                 
                                 'Inputs', 
                                 'Inputs', 
                                 'Practices', 
                                 'Practices', 
                                 
                                 'Inputs', 
                                 'Inputs', 
                                 'Inputs', 
                                 'Inputs', 
                                 'Yield constraining factors', 
                                 'Yield constraining factors', 
                                 'Yield constraining factors', 
                                 'Yield constraining factors', 
                                 'Climate conditions',
                                 'Climate conditions',
                                 'Climate conditions'
                                 )
    
    ordered.cluster.names.2r <- c(
      "Fertiliser &\nHigh elevation"   ,
      "Fertiliser &\nLow elevation" ,      
      "Labour &\nFertiliser"  ,
      "Weeding &\nWaterlogged",   
      "Low tree\ndensity"    ,         
      "Cluster num 6"      ,
     "Nutrient\ndeficient"    ,
      "High tree\ndensity"       ,
      "Open & Complex\nfertilisation"  ,
      "Dense & Complex\nfertilisation" ,
      "N-fertilisation\n& Pollination"   ,
      "Cluster num 12"     ,
     "Nutrient deficient\n& High elevation" ,
      "Nutrient deficient\n& Low elevation" ,
      "N-fertilisation"  ,
       "Fertiliser &\nTall canopy"  ,
       "Cluster num 17"       ,
       "Cluster num 18" 
    )
      
    
    num.vars <- length(cor.data.var.names)
    num.typs <- 18
    rows <- seq(1,18 * num.vars,1)
    
    cor.intrag  <-   data.frame(  matrix(ncol = length( cor.data.intra.var.names  ) , nrow = length(rows) , dimnames=list(rows  ,   cor.data.intra.var.names  )))
    
    
    start.hs <- 1
    end.hs <- 6 * length(cor.data.var.names)
    
    start.hsn <-   end.hs +1 
    end.hsn <-  12 * length(cor.data.var.names)
    
    start.az <-  end.hsn +1 
    end.az <- 18 * length(cor.data.var.names)
    
    cor.intrag[seq(  start.hs  : end.hs  ), 'typology'] <- 'Hybrid sun'
    cor.intrag[   seq( start.hsn   ,  end.hsn)  , 'typology'] <- 'Hybrid shade'
    cor.intrag[   seq( start.az   ,    end.az)  , 'typology'] <- 'Amazonia'
    
    row.count <- 1
    circle.size.scale <<- 1
    
    #View(cor.intrag)
    
    #for (t in 1:20){
    # summary(comp[ !is.na(comp$typology) , cor.data.var.names[t] ])
    # }
    
    #summary(comp[ !is.na(comp$typology) , cor.data.var.names[20] ])
    
    for (r in seq(1:(num.typs))  ) { 
      
      for (n in 1:length(cor.data.var.names)) {
        
        cor.intrag[row.count,'typ'] <- r
        cor.intrag[row.count,'name']  <- ordered.cluster.names[r]
        cor.intrag[row.count,'name.2r']  <- ordered.cluster.names.2r[r]
        cor.intrag[row.count,'variable.name']  <-   cor.data.var.names [n]
        cor.intrag[row.count,'variable.label']  <-   cor.data.var.labels[n]
        cor.intrag[row.count,'variable.label.2r']  <-   cor.data.var.labels.2r[n]
        cor.intrag[row.count,'variable.cat']  <-  cor.data.variable.cats[n]
        
        
        
        current.typology <-   cor.intrag[row.count, 'typology']
        
        typ.mn.val <-   mean(na.omit(comp[!is.na(comp$typ.str.fill ) & comp$typ.str.fill  == ordered.cluster.names[r] & !is.na(comp$typ) & comp$typology == current.typology & !is.na(comp$typology)  ,      cor.data.var.names[n] ]))
        
        typ.sd.val  <-  sd(na.omit(comp[!is.na(comp$typ.str.fill ) & comp$typ.str.fill  == ordered.cluster.names[r] & !is.na(comp$typ) & comp$typology == current.typology & !is.na(comp$typology)  ,      cor.data.var.names[n] ]))
        
        
        other.types.str <- unique( comp[comp$typology == current.typology &  comp$typ.str.fill != ordered.cluster.names[r] & !is.na(comp$typ.str.fill), 'typ.str.fill'])
        
        sys.mean.val  <- 0.0001 
        
        sys.mean.val  <- .01 + mean(na.omit(comp[ !is.na(comp$typ) & comp$typ.str.fill  %in% other.types.str & comp$typology == current.typology & !is.na(comp$typology) ,  cor.data.var.names[n]])) 
        
      
        
        
        print(paste('Typ mean is ' , typ.mn.val))
        print(paste('System mean is ' , sys.mean.val))
        
        cor.intrag[row.count, 'typ.mn'] <-  typ.mn.val
        cor.intrag[row.count, 'sys.mn'] <- sys.mean.val
        
         cor.intrag[row.count, 'var.v'] <-  (  typ.mn.val
                -  sys.mean.val ) /    sys.mean.val 
        
        sys.sd.val  <- sd(na.omit(comp[ !is.na(comp$typ) & comp$typ.str.fill  %in% other.types.str & comp$typology == current.typology & !is.na(comp$typology) ,  cor.data.var.names[n]])) 
        
        # Define text/label to use in plot 
        if (  !is.na(typ.mn.val ) & typ.mn.val >= 10) { dec.rd <- 0 } else if (  !is.na(typ.mn.val ) &   typ.mn.val > -100) {
          dec.rd <- 1 } else {dec.rd <- 1}
        mean.val <- round(  typ.mn.val ,  dec.rd ) 
        sd.val <- round(    typ.sd.val , dec.rd )
        
        # If no decimals, add a decimal (to keep consistent)
        if ( !is.na(mean.val) & nchar(mean.val) == 1 ) { mean.val <- str_c( mean.val , '.0')}
        if ( !is.na(   sd.val) & nchar(sd.val) == 1 ) {    sd.val <- str_c(    sd.val , '.0')}
       
        if (mean.val == '0.0' & !is.na(mean.val) & sd.val == '0.0' & !is.na(d.val) ){
          cor.intrag[row.count, 'var.label'] <- '-'
        } else {
      cor.intrag[row.count, 'var.label'] <- str_c(  mean.val,'\n(',  sd.val , ')')
        }
        
        
        
        if ( typ.mn.val >= sys.mean.val & !is.na( sys.mean.val ) & !is.na( typ.mn.val ))  {      cor.intrag[row.count,'dir.d']  <- 1 }  
        if ( typ.mn.val < sys.mean.val & !is.na( sys.mean.val ) & !is.na( typ.mn.val ))  {     cor.intrag[row.count,'dir.d']  <- -1 }  
        
        g1 <- na.omit(comp[comp$typ.str.fill == ordered.cluster.names[r]  & !is.na(comp$typ.str.fill) & comp$typology ==    current.typology, cor.data.var.names[n] ])
        g2 <- na.omit(comp[comp$typ.str.fill != ordered.cluster.names[r]  & !is.na(comp$typ.str.fill) & comp$typology ==    current.typology , cor.data.var.names[n] ])
        
        
        if (length(g1) > 0 & length(g2) > 0  & mean(g1) != mean(g2)){
          
          #  t.test.res <-  t.test(g1 , g2, alternative = "two.sided", var.equal = FALSE)
          
          t.test.res <- wilcox.test(g1, g2, alternative = "two.sided" , var.equal = FALSE)
          
          cor.intrag[row.count, 'var.s']     <- t.test.res$p.value
        } else {
          cor.intrag[row.count, 'var.s']  <-  runif(1,0.3,0.95)
         
        
        }
        
        print(paste(n))
        print(paste('Typology is ' , current.typology))
        row.count <-   row.count + 1 
        
      }
      
      
    }
    
    variable.cats.ordered <<-  c('Yield' , "Vegetative structure & diversity" , "Practices" , "Inputs" , "Yield constraining factors" ,  'Climate conditions' )
    ordered.dir.d <- c('1','-1')
    
    cor.intrag[cor.intrag$dir.d == -1 & !is.na(cor.intrag$dir.d),'dir.d.str']<- '-ve'
    cor.intrag[cor.intrag$dir.d == 1 & !is.na(cor.intrag$dir.d) ,'dir.d.str']<- '+ve'
    
    cor.intrag$typology <- factor( cor.intrag$typology   , levels= ordered.typologies)
    cor.intrag[,'name'] <- factor(  cor.intrag[,'name'] , levels = ordered.cluster.names)
    cor.intrag[,'name.2r'] <- factor(  cor.intrag[,'name.2r'] , levels = ordered.cluster.names.2r)
    cor.intrag[,'variable.name'] <- factor(   cor.intrag[,'variable.name'] , levels =   cor.data.var.names  )
    cor.intrag[,'dir.d'] <- factor(  cor.intrag[,'dir.d'] , levels = ordered.dir.d)
    cor.intrag[,'variable.cat'] <- factor(  cor.intrag[,'variable.cat'] , levels =    variable.cats.ordered )
    cor.intrag[,'variable.label'] <- factor(  cor.intrag[,'variable.label'] , levels =    cor.data.var.labels )
    cor.intrag[,'variable.label.2r'] <- factor(  cor.intrag[,'variable.label.2r'] , levels =    cor.data.var.labels.2r )
    
    cor.intrag$var.label
    
    rc <- 1 
    
    sign.thresh <<- 0.1
    
    cor.intrag[ !is.na(cor.intrag$var.s) , 'var.s.c' ] <- (cor.intrag[  !is.na(cor.intrag$var.s) , 'var.s' ] ) /   sign.thresh
    cor.intrag[ !is.na(cor.intrag$var.s) & cor.intrag$var.s >=   sign.thresh , 'var.s.c' ] <- 1.0
    
    cor.intrag[ !is.na(cor.intrag$var.s.c) & cor.intrag$var.s.c >= 1, 'var.s.c' ] <- 1.0
    
    
    
    
    
    cor.intrag <<-   cor.intrag[!str_detect(cor.intrag$name ,'Cluster') & !is.na(cor.intrag$name)  , ]
    
    cor.intrag$var.sign.label <- round( cor.intrag$var.s , 2)
    cor.intrag[cor.intrag$var.sign.label == 0 , 'var.sign.label' ] <- "<0.01"
    
    cor.intrag <<- cor.intrag
    
  }
  cor.plot.data.prep.intra()
  
  #View(cor.intrag)
  
  cor.plot.data.prep.inter <- function(){
    
    cor.data.inter.var.names <- c(
      'typology' ,
      'variable.cat',
      # variable values
      'Facet.label' , 
      'var.v',
      'var.s',
      'dir.d',
      'dir.d.str',
      'var.label'
    )
    
    
    
    num.vars <- length(cor.data.var.names)
    num.typologies <- 3
    rows <- seq(1,  num.typologies *  num.vars ,1)
    
    cor.interg <-   data.frame(  matrix(ncol = length(cor.data.inter.var.names ) , nrow = length(rows) , dimnames = list(rows  , cor.data.inter.var.names  )))
    
    start.hs <- 1
    end.hs <-  length(cor.data.var.names)
    
    start.hsn <-   end.hs +1 
    end.hsn <-  2 * length(cor.data.var.names)
    
    start.az <-  end.hsn +1 
    end.az <- 3 * length(cor.data.var.names)
    
    cor.interg[seq(  start.hs  : end.hs  ), 'typology'] <- 'Hybrid sun'
    cor.interg[   seq( start.hsn   ,  end.hsn)  , 'typology'] <- 'Hybrid shade'
    cor.interg[   seq( start.az   ,    end.az)  , 'typology'] <- 'Amazonia'
    
    row.count <- 1
    
    for (r in seq(1:(num.typologies))  ) { 
      
      for (n in 1:length(cor.data.var.names)) {
        
        cor.interg[row.count,'variable.name']  <-  cor.data.var.names[n]
        cor.interg[row.count,'variable.label']  <-   cor.data.var.labels[n]
        cor.interg[row.count,'variable.cat']  <-  cor.data.variable.cats[n]
        
        
        
        current.typology <-   cor.interg[row.count, 'typology']
        
        typ.mn.val <-         mean(na.omit(comp[ comp$typology == current.typology & !is.na(comp$typology)  ,      cor.data.var.names[n] ]))
        
        other.typologies.str <- unique( comp[comp$typology != current.typology  & !is.na(comp$typology), 'typology'])
        
        sys.mean.val  <- 0 
        
        for (t in 1:length(   other.typologies.str)) {
          sys.mean.val  %+=% mean(na.omit(comp[ comp$typology  %in%   other.typologies.str & comp$typology != current.typology & !is.na(comp$typology) ,  cor.data.var.names[n]])) 
        }
        
        sys.mean.val <-  sys.mean.val / length( other.typologies.str)
        
        
        sys.sd.val  <- sd(na.omit(comp[ !is.na(comp$typ) & comp$typology != current.typology & !is.na(comp$typology) ,  cor.data.var.names[n]])) 
    
        
        if (  typ.mn.val >= 10) { dec.rd <- 0 } else if (   typ.mn.val > -100) {
          dec.rd <- 1 }
        mn.val <- round(typ.mn.val, dec.rd ) 
        sd.val <- round( sys.sd.val , dec.rd )
        
        # If no decimals, add a decimal (to keep consistent)
        if ( !is.na(mn.val) & nchar(mn.val) == 1 ) { mn.val <- str_c( mn.val , '.0')}
        if ( !is.na(   sd.val) & nchar(sd.val) == 1 ) {    sd.val <- str_c(    sd.val , '.0')}
        
        
        if (mean.val == '0.0' & !is.na(mean.val) & sd.val == '0.0' & !is.na(sd.val) ){
          cor.interg[row.count, 'var.label'] <- '-'
        } else {
          cor.interg[row.count, 'var.label'] <- str_c(mn.val,'\n(',  sd.val , ')')
          
        }
        
        
       
     
        cor.interg[row.count, 'var.v'] <- circle.size.scale *
          (  typ.mn.val
             -  sys.mean.val ) /    sys.mean.val 
        
        if ( typ.mn.val >= sys.mean.val & !is.na( sys.mean.val ) & !is.na( typ.mn.val ))  {      cor.interg[row.count,'dir.d']  <- 1 }  
        if ( typ.mn.val < sys.mean.val & !is.na( sys.mean.val ) & !is.na( typ.mn.val ))  {      cor.interg[row.count,'dir.d']  <- -1 }  
        
        g1 <- na.omit(comp[ comp$typology ==    current.typology & !is.na(comp$typology), cor.data.var.names[n] ])
        g2 <- na.omit(comp[ comp$typology !=    current.typology & !is.na(comp$typology), cor.data.var.names[n] ])
        
        
        if (length(g1) > 0 & length(g2) > 0  & mean(g1) != mean(g2)){
          
          
          t.test.res <- wilcox.test(g1, g2, alternative = "two.sided" , var.equal = FALSE)
          cor.interg[row.count, 'var.s']     <- t.test.res$p.value
          
        } else {
          cor.interg[row.count, 'var.s']  <-  runif(1,0.3,0.95)
          
        }
        
        print(paste(n))
        row.count <-   row.count + 1 
        
      }
      
    }
    
    
    cor.interg[ !is.na(cor.interg$var.s) , 'var.s.c' ] <- (cor.interg[  !is.na(cor.interg$var.s) , 'var.s' ] ) /   sign.thresh
    cor.interg[ !is.na(cor.interg$var.s) & cor.interg$var.s >=   sign.thresh , 'var.s.c' ] <- 1.0
    
    cor.interg[ !is.na(cor.interg$var.s.c) & cor.interg$var.s.c >= 1, 'var.s.c' ] <- 1.0
    
    
    
    
    lev <- c()
    
    for (r in 1:nrow(cor.interg)){
      
      #  if (cor.interg[r,'dir.d'] < 0.0) {   new <- 'dark'}
      #  if (cor.interg[r,'dir.d'] > 0.0) {   new <- 'dark'}
      
      new <- 'dark'
       if (cor.interg[r,'var.s.c']  <= 0.6 & cor.interg[r,'dir.d'] < 0  & !is.na(cor.interg[r,'var.s.c']) & !is.na(cor.interg[r,'dir.d'])) {   new <- 'light'}
      #  if (cor.interg[r,'var.s.c']  <= 0.6 & cor.interg[r,'dir.d'] > 0) {   new <- 'dark'}
     
       lev <- c( lev, new)
      
      
    }
    
    lev <<- lev
    
    
   ordered.dir.d <- c('1','-1')
   #  ordered.dir.d.str <- c('-ve','+ve')
    ordered.dir.d.str <- c('Lower', 'Higher')
    
    cor.interg[cor.interg$dir.d == -1,'dir.d.str']<- 'Lower'
    cor.interg[cor.interg$dir.d == 1,'dir.d.str']<- 'Higher'
    
    cor.interg$typology <- factor( cor.interg$typology   , levels= ordered.typologies)
    cor.interg[,'variable.name'] <- factor(  cor.interg[,'variable.name'] , levels =     cor.data.var.names )
    
    cor.interg[,'variable.label'] <- factor(  cor.interg[,'variable.label'] , levels =     rev(cor.data.var.labels ))
    
    cor.interg[,'dir.d'] <- factor(  cor.interg[,'dir.d'] , levels = ordered.dir.d)
    cor.interg[,'dir.d.str'] <- factor(  cor.interg[,'dir.d.str'] , levels = ordered.dir.d.str)
    cor.interg[,'variable.cat'] <- factor(  cor.interg[,'variable.cat'] , levels =    variable.cats.ordered )
    rc <- 1 
    
    # cor.interg$var.s.r <- as.double(sprintf(1.00000000000000000000 - cor.interg$var.s, fmt = '%#.17f') )
    
    
    # cor.interg[ cor.interg$var.s >= 0.1  & !is.na(cor.interg$var.s) , 'var.s' ] <- 0.1
    
    # cor.interg[ !is.na(cor.interg$var.s.r) , 'var.s.r' ] <- (cor.interg[  !is.na(cor.interg$var.s.r) , 'var.s.r' ] - 0.900) / 0.10000
    #  cor.interg[ cor.interg$var.s.r < 0.0  & !is.na(cor.interg$var.s.r) , 'var.s.r' ] <- 0.0
    
    
    cor.interg$var.sign.label <- round( cor.interg$var.s , 2)
    cor.interg[cor.interg$var.sign.label == 0 , 'var.sign.label' ] <- "<0.01"
    
    cor.interg$Facet.label <- 'All systems'
    
    cor.interg <<- cor.interg
    
    
  }
  cor.plot.data.prep.inter()
  
  
  p.corr.intrag.mg.bottom <- -0.02
  p.corr.intrag.mg.top <- 0.12
  p.corr.intrag.mg.right <- 0.1
  p.corr.intrag.mg.left <- 0.3
  
  p.corr.interg.mg.bottom <- 1.10
  p.corr.interg.mg.top <- 0.12
  p.corr.interg.mg.right <- -.45
  p.corr.interg.mg.left <- 0.16
  
  cor.plot.x.tx.fs <<- 8
  cor.plot.y.tx.fs <<- 7.9
  corr.plot.label.fs <<- 10.9
  corr.plot.base.alpha <- 0.205
  cor.plot.point.shape.id <<- 22
  p.corr.strip.tx.fs <<- 8.4
  p.corr.y.tit.fs <- 11
  cor.plot.main.point.size <<- 7.06
  cor.plot.sm.point.size <<-  8.68
  cor.plot.sm.label.fs <<- 2.12 #1.565
  cor.plot.mn.label.fs <<- 1.8
  cor.plot.text.lh <<- 0.81
  cor.plot.strip.tx.fs.left <<- 8.8
  
  p.corr.strip.tx.angle <<- 29.8
  p.cor.geom.label.text.alpha.level <<- 0.77
  cor.plot.border.thickness <<- 0.66
  
  cor.plot.text.color.light <<- '#FEFE14'
  cor.plot.text.color.dark <<- 'black'
  
  
  cor.plot.text.color.light <<- 'grey'
  cor.plot.text.color.dark <<- 'grey'
  
  cor.plot.leg.key.h <<- 0.001
  cor.plot.leg.key.w  <<- 0.001
  cor.plot.leg.key.size  <<- 4
  
  cor.plot.leg.tx.fs <<- 7
  
  cor.plot.stroke.size <<- 0.0000037
  cor.plot.panel.spaces <<- 0.1
  
  p.cor.fg.y.tx.margin.top <<- 2.9
  p.cor.fg.y.tx.margin.right <<- 2
  p.cor.fg.y.tx.margin.bottom <<- 2.9
  p.cor.fg.y.tx.margin.left <<- 2
  
  cor.plot.leg.pos.y <<- -0.008
  cor.plot.leg.pos.x <<- - 0.060468
  
  dir.d.neg.color <- '#87b5eb'  #   '#9D4DCA' #F46D43'
  dir.d.pos.color <-  '#AEDB2D' # '#ABDDA4'
  
  fig.cor.sm.intrag.b <<- ggplot( data = cor.intrag[ !is.na(cor.intrag$var.v) &  !is.na(cor.intrag$var.s.c) , ] ,aes(  y = as.factor(variable.label.2r) ,  x = as.factor(name.2r)))+
   geom_point(shape = cor.plot.point.shape.id ,  stroke = cor.plot.stroke.size ,   size = cor.plot.sm.point.size , aes( alpha =  var.s.c , y = variable.label.2r ,  fill = factor(dir.d.str)) , stat = 'identity') +
    #  geom_point(shape = cor.plot.point.shape.id,  stroke = cor.plot.stroke.size , alpha =  corr.plot.base.alpha,  size =  cor.plot.point.size , aes(  y =variable.label.2r  ,  fill = factor(dir.d.str)) , stat = 'identity') + 
    scale_alpha_continuous( breaks = c( 0.01 , 0.1 , 0.5 , 0.999  ),
            range = c( 1.0 ,corr.plot.base.alpha ), 
          limits= c( 0.0 , 1.0)
    )  +
    scale_fill_manual(values=c(   dir.d.neg.color , dir.d.pos.color  ))  +
    facet_grid( variable.cat ~ typology      , scales = 'free' ,
                space='free' , 
                switch = "y"  
    )  +
      geom_text(alpha =  p.cor.geom.label.text.alpha.level  , lineheight =   cor.plot.text.lh ,   size =  cor.plot.label.fs ,  aes(y = variable.label.2r , label =  var.label  )  ) +
    scale_y_discrete(position = "right" ,
                     limits = rev ) +
    scale_x_discrete(  ) +
      ylab('       Plot characteristics')  +
       xlab('Systems, clusters             ')  +
    # xlab('')  +
    theme(
      plot.margin = unit(c(p.corr.intrag.mg.top , p.corr.intrag.mg.right , p.corr.intrag.mg.bottom , p.corr.intrag.mg.left), "cm"), 
      # Facets
      strip.background.y =  element_blank(),
      strip.text.y =   element_blank(),
   strip.text.x = element_text( face = 'italic' , margin = margin ( p.cor.fg.y.tx.margin.top , p.cor.fg.y.tx.margin.right, p.cor.fg.y.tx.margin.bottom, p.cor.fg.y.tx.margin.left ),
   size =  p.corr.strip.tx.fs, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.spacing.y = unit(cor.plot.panel.spaces , "lines") , 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = 'transparent'),
      panel.border = element_rect(colour = "black", fill = NA, size= cor.plot.border.thickness ),
      #  axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = cor.plot.x.tx.fs , angle=78 , vjust = .7, hjust=.62 , face = 'italic'),
      axis.text.y = element_text(size = cor.plot.y.tx.fs ), 
      axis.title.y = element_text(size = p.corr.y.tit.fs  ,  vjust = 0, hjust=0.5) , 
      axis.title.x = element_text(size = p.corr.y.tit.fs , hjust=0.5 , vjust = 1),
      #axis.ticks.x = element_blank() ,
      legend.position="none",
      legend.text = element_text(size=7))
  
 
  
  fig.cor.sm.interg.b <<- ggplot( data = cor.interg[ !is.na(cor.interg$var.v) &  !is.na(cor.interg$var.s.c) , ] ,aes(  y = factor(variable.label) ,  x = as.factor(typology)))+
      geom_point(shape = cor.plot.point.shape.id ,  stroke = cor.plot.stroke.size  ,   size = cor.plot.sm.point.size  , aes( alpha =  var.s.c , y = variable.label  ,  fill = factor(dir.d.str)) , stat = 'identity') +
    #  corr.plot.base.alpha  geom_point(shape = cor.plot.point.shape.id,  stroke = cor.plot.stroke.size , alpha =  corr.plot.base.alpha,  size = cor.plot.point.size , aes(  y = variable.label  ,  fill = factor(dir.d.str)) , stat = 'identity') +
    scale_fill_manual(values = c(  dir.d.neg.color , dir.d.pos.color ),
                      name = 'Direction\nof change')  +
    scale_alpha_continuous( name = 'p-value',
                            breaks = c( 0.01  , 0.25   , 0.75 , 1.0  ),
                            range = c( 1.000 ,  corr.plot.base.alpha ), 
                            labels = c('0.01' , '0.025' , '0.075' , '>= 0.1') , 
                            limits= c( 0.0000 , 1.0) 
    ) +
    geom_text(alpha =  p.cor.geom.label.text.alpha.level  , lineheight =   cor.plot.text.lh ,   size =  cor.plot.label.fs ,  aes(y = variable.label , label =  var.label  )  ) +
    scale_color_identity( labels= levels(cor.interg$var.s.c) , 
                          breaks= c(cor.plot.text.color.dark, cor.plot.text.color.light)) +
   facet_grid( variable.cat ~  Facet.label   , scales = 'free' ,  space='free' ,  switch = "y"  , labeller = label_wrap_gen(width=15)) + 
  scale_y_discrete(position = "right"  ) +
    scale_x_discrete(
   ) +
    ylab('')  +
    xlab('')  +
    guides(
      fill = guide_legend( reverse = TRUE , 
                           title.hjust = 0.5 , 
                           override.aes = list(size = cor.plot.leg.key.size*.8) 
                           ,  title="Deviation from\nsystem (a) & \ncluster (b) mean"
                           , order = 1 , nrow = 2 , title.position = 'top'),
      alpha = guide_legend(  title="Statistical\nsignifiance\n(Wilcoxon\nrank test)" ,
                             order=2 , override.aes = list(size = cor.plot.leg.key.size*.8) , nrow = 2 , title.hjust = 0.00 ,title.position = 'left')
    ) +
 theme(
      plot.margin = unit(c(p.corr.interg.mg.top , p.corr.interg.mg.right , p.corr.interg.mg.bottom , p.corr.interg.mg.left), "cm"), 
      strip.text.x.top  = element_text(size =  p.corr.strip.tx.fs , color = 'black' , margin = margin (p.cor.fg.y.tx.margin.top , p.cor.fg.y.tx.margin.right, p.cor.fg.y.tx.margin.bottom, p.cor.fg.y.tx.margin.left)),
      strip.text.y.left = element_text(angle = p.corr.strip.tx.angle  , size = cor.plot.strip.tx.fs.left ),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.spacing.y = unit(cor.plot.panel.spaces , "lines") , 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size = cor.plot.border.thickness),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = cor.plot.x.tx.fs ,   angle=78 , vjust = .7, hjust=.62 , face = 'italic' )
      , axis.text.y = element_blank()
      #axis.ticks.x = element_blank() 
     ,axis.title.x =  element_blank()
      , legend.title = element_text(size = 7.0)
      ,   legend.box="vertical"
      , legend.spacing.y = unit(.1, 'cm')
     , legend.margin = margin(0, 0, 0, 0)  
      , legend.background = element_rect(fill = alpha('white', 0.0))
      , legend.position = c(cor.plot.leg.pos.x , cor.plot.leg.pos.y)
       ,  legend.key = element_rect(fill = "white")
      ,  legend.key.height = unit(cor.plot.leg.key.h , 'cm')
      , legend.key.width = unit(cor.plot.leg.key.w , 'cm')
      ,legend.justification = c("center", "top")
      , legend.text = element_text(size=cor.plot.leg.tx.fs)
    )
  

  
  fig.cor.sm.interg <<- annotate_figure( fig.cor.sm.interg.b ,   fig.lab = "a", fig.lab.pos ="top.left", fig.lab.size = corr.plot.label.fs )
  
  fig.cor.sm.intrag <<- annotate_figure( fig.cor.sm.intrag.b ,   fig.lab = "b", fig.lab.pos ="top.left", fig.lab.size = corr.plot.label.fs )
  
  
  fig.cor.sm.plot   <<- plot_grid(  fig.cor.sm.interg, 
                                 fig.cor.sm.intrag, 
                                 #  align = "v", 
                                 nrow = 1, 
                                 ncol = 2 , 
                                 rel_widths = c(24/100, 64/100)
  )
  
  ggsave("fig.cor.sm.plot.pdf",      fig.cor.sm.plot     , path = "Figures.out", width=790, height= 902, units="px", scale=2.5  )
  ggsave("fig.cor.sm.plot.jpeg",     fig.cor.sm.plot     , path = "Figures.out", width=790, height= 902, units="px", scale=2.5  )
  
  
  p.corr.mn.intrag.mg.bottom <- -0.32
  p.corr.mn.intrag.mg.top <- 0.12
  p.corr.mn.intrag.mg.right <- 0.1
  p.corr.mn.intrag.mg.left <- 0.3
  
  p.corr.mn.interg.mg.bottom <- 1.98
  p.corr.mn.interg.mg.top <- 0.12
  p.corr.mn.interg.mg.right <- -.45
  p.corr.mn.interg.mg.left <- 0.16
  
  cor.plot.mn.leg.pos.x <<-  -.07
  cor.plot.mn.leg.pos.y <<-  -.028
  
  cor.plot.mn.x.tx.fs  <<- 7
  cor.plot.mn.a.x.tx.fs <- 8.6
  cor.plot.mn.b.x.tx.fs  <<- 6.25
  
  cor.plot.mn.leg.key.w <<- .0018
  cor.plot.mn.leg.key.h <<-  0.0018
  
  cor.plot.leg.key.size <<- 4.62
  
  p.corr.mn.leg.tx.fs <<- 7.08
  p.corr.mn.leg.tit.fs <<- 7.768
  

  # MAIN text figure
  fig.cor.mn.intrag.b <<- ggplot( data = cor.intrag[ !is.na(cor.intrag$var.v) &  !is.na(cor.intrag$var.s.c) , ] ,aes(  y = as.factor(variable.label.) ,  x = as.factor(name)))+
    geom_point(shape = cor.plot.point.shape.id ,  stroke = cor.plot.stroke.size ,   size = cor.plot.main.point.size , aes( alpha =  var.s.c , y = variable.label ,  fill = factor(dir.d.str)) , stat = 'identity') +
    # geom_point(shape = cor.plot.point.shape.id,  stroke = cor.plot.stroke.size , alpha =  corr.plot.base.alpha,  size =  cor.plot.point.size , aes(  y =variable.label  ,  fill = factor(dir.d.str)) , stat = 'identity') + 
    scale_alpha_continuous( breaks = c( 0.01 , 0.1 , 0.5 , 0.999  ),
                            range = c( 1.0 , corr.plot.base.alpha ), 
                            limits= c( 0.0 , 1.0)
    )  +
    scale_fill_manual(values=c(   dir.d.neg.color , dir.d.pos.color  ))  +
    facet_grid( variable.cat ~ typology      , scales = 'free' ,
                space='free' , 
                switch = "y"  
    )  +
    geom_text(alpha =  p.cor.geom.label.text.alpha.level  , lineheight =   cor.plot.text.lh ,   size =  cor.plot.mn.label.fs ,  aes(y = as.factor(variable.label) ,  label =  var.sign.label,  x = as.factor(name) )  ) +
    scale_y_discrete(position = "right" ,
                     limits = rev ) +
    scale_x_discrete(  ) +
    ylab('       Plot characteristics')  +
    xlab('')  +
    theme(
      plot.margin = unit(c(p.corr.mn.intrag.mg.top , p.corr.mn.intrag.mg.right , p.corr.mn.intrag.mg.bottom , p.corr.mn.intrag.mg.left), "cm"), 
      # Facets
      strip.background.y =  element_blank(),
      strip.text.y =   element_blank(),
      strip.text.x = element_text( face = 'italic' , margin = margin ( p.cor.fg.y.tx.margin.top , p.cor.fg.y.tx.margin.right, p.cor.fg.y.tx.margin.bottom, p.cor.fg.y.tx.margin.left ),
                                   size =  p.corr.strip.tx.fs, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.spacing.y = unit(cor.plot.panel.spaces , "lines") , 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = 'transparent'),
      panel.border = element_rect(colour = "black", fill = NA, size= cor.plot.border.thickness ),
      #  axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = cor.plot.mn.x.tx.fs , angle=90 , vjust = .7, hjust= 1 , face = 'italic'),
      axis.text.y = element_text(size = cor.plot.y.tx.fs ), 
      axis.title.y = element_text(size = p.corr.y.tit.fs  ,  vjust = 0, hjust=0.5) , 
      axis.title.x = element_text(size = p.corr.y.tit.fs , hjust=0.5 , vjust = 1),
      axis.ticks.x = element_blank() ,
      legend.position="none",
      legend.text = element_text(size=p.corr.mn.leg.tx.fs))
  
  
  
  fig.cor.mn.interg.b <<- ggplot( data = cor.interg[ !is.na(cor.interg$var.v) &  !is.na(cor.interg$var.s.c) , ] ,aes(  y = factor(variable.label) ,  x = as.factor(typology)))+
    geom_point(shape = cor.plot.point.shape.id ,  stroke = cor.plot.stroke.size*0  ,   size = cor.plot.main.point.size  , aes( alpha =  var.s.c , y = variable.label  ,  fill = factor(dir.d.str)) , stat = 'identity') +
    #   geom_point(shape = cor.plot.point.shape.id,  stroke = cor.plot.stroke.size*0 , alpha =  corr.plot.base.alpha,  size = cor.plot.point.size , aes(  y = variable.label  ,  fill = factor(dir.d.str)) , stat = 'identity') +
    scale_fill_manual(values = c(  dir.d.neg.color , dir.d.pos.color ),
                      name = 'Direction\nof change')  +
    scale_alpha_continuous( name = 'p-value',
                            breaks = c( 0.01  , 0.25   , 0.75 , 1.0  ),
                            range = c( 1.000 ,  corr.plot.base.alpha ), 
                            labels = c('p < 0.01' , 'p < 0.025' , 'p < 0.075' , 'p > 0.1') , 
                            limits= c( 0.0000 , 1.0) 
    ) +
    geom_text(alpha =  p.cor.geom.label.text.alpha.level  , lineheight =   cor.plot.text.lh*1 ,   size =  cor.plot.mn.label.fs,  aes(y = as.factor(variable.label) ,  label =  var.sign.label,   x = as.factor(typology) )  ) +
    scale_color_identity( labels= levels(cor.interg$var.s.c) , 
                          breaks= c(cor.plot.text.color.dark, cor.plot.text.color.light)) +
    facet_grid( variable.cat ~  Facet.label   , scales = 'free' ,  space='free' ,  switch = "y"  , labeller = label_wrap_gen(width=15)) + 
    scale_y_discrete(position = "right"  ) +
    scale_x_discrete(
    ) +
    ylab('')  +
    xlab('')  +
    theme(
      plot.margin = unit(c(p.corr.mn.interg.mg.top , p.corr.mn.interg.mg.right , p.corr.mn.interg.mg.bottom , p.corr.mn.interg.mg.left), "cm"), 
      strip.text.x.top  = element_text(size =  p.corr.strip.tx.fs , color = 'black' , margin = margin (p.cor.fg.y.tx.margin.top , p.cor.fg.y.tx.margin.right, p.cor.fg.y.tx.margin.bottom, p.cor.fg.y.tx.margin.left)),
      strip.text.y.left = element_text(angle = p.corr.strip.tx.angle  , size = cor.plot.strip.tx.fs.left ),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.spacing.y = unit(cor.plot.panel.spaces , "lines") , 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size = cor.plot.border.thickness),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size =   cor.plot.mn.a.x.tx.fs ,   angle=90 , vjust = .7, hjust=1 , face = 'italic' )
      , axis.text.y = element_blank()
      , axis.ticks.x = element_blank() 
      ,axis.title.x =  element_blank()
      , legend.title = element_text(size = p.corr.mn.leg.tit.fs )
      ,   legend.box="vertical"
      , legend.margin = margin(0, 0, 0, 0)  
      , legend.background = element_rect(fill = alpha('white', 0.0))
      , legend.position = c(cor.plot.mn.leg.pos.x , cor.plot.mn.leg.pos.y)
       ,  legend.key = element_rect( size = unit(0.015, 'lines') ,  fill = "white")
       ,  legend.key.height = unit(cor.plot.mn.leg.key.h*10 , 'mm')
       , legend.key.width = unit(cor.plot.mn.leg.key.w , 'mm') 
      ,legend.spacing.y = unit(1, 'mm') 
      ,legend.justification = c("center", "top")
       , legend.spacing.x = unit(0.03668, 'mm')
      , legend.text = element_text(size = p.corr.mn.leg.tx.fs) 
    ) +
    guides(
      fill = guide_legend( reverse = TRUE , 
                           title.hjust = 0.37 , 
                           override.aes = list(  size =   cor.plot.leg.key.size) 
                         ,  title="Deviation from\nsystem (a) &\ncluster (b) mean"
                           , order = 1 , nrow = 2 , title.position = 'top'),
      alpha = guide_legend(        title="Statistical\nsignifiance\n(Wilcoxon\nrank test)" 
                                      , override.aes = list(  size =   cor.plot.leg.key.size*.88)
                                   ,order=2 ,
                                   label.theme = element_text(size = p.corr.mn.leg.tx.fs*.78)
                                   ,nrow = 3 , title.hjust = 0.09 ,  title.position = 'left')
    ) 
  
  
  
  fig.cor.mn.interg <<- annotate_figure( fig.cor.mn.interg.b ,   fig.lab = "a", fig.lab.pos ="top.left", fig.lab.size = corr.plot.label.fs )
  
  fig.cor.mn.intrag <<- annotate_figure( fig.cor.mn.intrag.b ,   fig.lab = "b", fig.lab.pos ="top.left", fig.lab.size = corr.plot.label.fs )
  
  
  fig.cor.mn.plot   <<- plot_grid(  fig.cor.mn.interg, 
                                 fig.cor.mn.intrag, 
                                 #  align = "v", 
                                 nrow = 1, 
                                 ncol = 2 , 
                                 rel_widths = c(22/100, 60/100)
  )
  
  
  ggsave("fig.cor.mn.plot.pdf",      fig.cor.mn.plot    , path = "Figures.out", width = 740 , height = 802, units="px", scale=2.5  )
  ggsave("fig.cor.mn.plot.jpeg",      fig.cor.mn.plot   , path = "Figures.out", width = 790, height = 880, units="px", scale=2.5  )
  
}
corr.plot.gen()


fig.cor.plot 





# ~ ~ ADDITIONAL  FIGURES
# ~ Biomass figure

biom.tree.bio <- function(){
  
  
  setwd(cc.typ.dir)
  
View(organics.tree.all)

# TREE BIOMASS FIGURES
organics.tree.all <- organics.tree 

organics.tree.all <-   rbind(organics.tree , c( seq(0,0, length.out = (ncol(organics.tree)))))

mn.cc.biom.Mg <- mean(na.omit(comp[,'Biomass.per.tree.cc.tree.Mg']))
mn.cc.biom.Mg.sd <- .2


cocoa.row.number <- nrow(organics.tree.all)

row.names(organics.tree.all)[cocoa.row.number] <- 'Theobroma cacao'
organics.tree.all[cocoa.row.number,'tree.name'] <- "Cocoa"
organics.tree.all[cocoa.row.number,'sci.name'] <- "Theobroma cacoa"
organics.tree.all[cocoa.row.number,'Tree.type'] <- 'Evergreen'
organics.tree.all[cocoa.row.number,'total.biomass.Mg.per.tree'] <- as.numeric(mn.cc.biom.Mg)
organics.tree.all[cocoa.row.number,'total.volume.m3.sd.frac'] <- as.numeric(mn.cc.biom.Mg.sd )


organics.tree.all$total.biomass.Mg.per.tree <- as.numeric(organics.tree.all$total.biomass.Mg.per.tree)
organics.tree.all$total.biomass.Mg.per.tree.sd.frac <- as.numeric(organics.tree.all$total.volume.m3.sd.frac)
organics.tree.all$total.biomass.Mg.per.tree.sd.abs <- (organics.tree.all$total.biomass.Mg.per.tree.sd.frac * organics.tree.all$total.biomass.Mg.per.tree)

tree.type.order <- c('Evergreen' , 'Deciduous')

max.char <- max(nchar(organics.tree.all[,'sci.name']))
organics.tree.all[,'sci.name'] <-format(organics.tree.all[,'sci.name'], width = max.char, justify = "right") 



organics.tree.all <- organics.tree.all[ order(organics.tree.all$total.biomass.Mg.per.tree),]

organics.tree.all[,'sci.name'] <-  factor(organics.tree.all[,'sci.name'] , levels = organics.tree.all[,'sci.name'])
organics.tree.all[,'Tree.type'] <- factor(organics.tree.all[,'Tree.type'] , levels = tree.type.order)



organics.tree.all[,'y.label'] <- NA
organics.tree.all[,'y.label.v.just'] <- 0
organics.tree.all[,'num.obsvs'] <- 10
organics.tree.all[organics.tree.all$tree.name == 'Edinam','y.label.v.just'] <- 29.7
organics.tree.all[organics.tree.all$tree.name == 'Yellow-wood','y.label.v.just'] <- 18

organics.tree.all[,'num.obsvs.str'] <- NA

for (r in 1:nrow(organics.tree.all)){
  
  if (organics.tree.all[r,'total.biomass.Mg.per.tree'] > 30){
    
    organics.tree.all[r,'y.label'] <-  round(organics.tree.all[r,'total.biomass.Mg.per.tree'] , 1)
    
  }
  
  current.id <-  organics.tree.all[r , 'index']
  current.tree.count.var <- str_c('total_tree_count_id_',  current.id)
  organics.tree.all[r,'num.obsvs'] <- tree.dat[1 , current.tree.count.var]
  organics.tree.all[r,'num.obsvs.str'] <- str_c('n=',  organics.tree.all[r,'num.obsvs'])
    
   
} 
  


# Settings for figure
tree.sizes.x.tick.fs <- 9.5
tree.sizes.bar.width <-  0.4275
tree.sizes.error.bar.width <-  0.24885
tree.sizes.error.bar.size <- 0.37 
tree.sizes.y.tit.fs <- 10.5
tree.sizes.y.ticks.fs <- 9
tree.sizes.error.bar.color <-  error.bar.color
tree.fig.strip.fs <- 12
tree.sizes.bar.color <- box.plot.fill.color

fig.tree.mass.p.mg.top <- 0.2
fig.tree.mass.p.mg.bottom <- -.1
fig.tree.mass.p.mg.right <- 0.1
fig.tree.mass.p.mg.left <- 0.2

fit.tree.mass.y.lab.size <- 2.35
fit.tree.mass.y.lab.padding <- 0.17


fig.tree.mass.b <<- ggplot(organics.tree.all[organics.tree.all$tree.name != 'Cocoa',], aes(x = sci.name, y = total.biomass.Mg.per.tree)) +
  geom_bar(stat="identity", fill = tree.sizes.bar.color, colour =  bar.color.border  , width =  tree.sizes.bar.width ,  position = position_dodge() ,  lwd = fig.yd.bp.thickness )+
  xlab('')+
  geom_text(aes(x = sci.name, y = -.75 , label = num.obsvs.str), 
            colour = 'black' ,
            hjust = 0 ,
            size = 2.8 ,
            angle = 270
            ) +
  #geom_rect(data = organics.tree.all, aes(xmin = sci.name -.4, xmax = sci.name + .4, ymin = 8 - 3, ymax = 8 + 3), fill = "grey80") +
  geom_label(aes(label=y.label), 
            nudge_y = c(-19, -31.405),
             nudge_x = c(-.1,.0), 
           size = fit.tree.mass.y.lab.size , label.padding = unit(fit.tree.mass.y.lab.padding , "lines")) +
 # geom_label(aes(label=num.observations), 
  #           nudge_y = c(-31.5,-19),
   #          nudge_x = c(.00,-.3), 
    #         size = fit.tree.mass.y.lab.size , label.padding = unit(fit.tree.mass.y.lab.padding , "lines"),) +
  geom_errorbar(aes(ymin = total.biomass.Mg.per.tree - (total.biomass.Mg.per.tree.sd.abs  ), ymax = total.biomass.Mg.per.tree +  total.biomass.Mg.per.tree.sd.abs , x = sci.name, group = sci.name) , width=  tree.sizes.error.bar.width , size =   tree.sizes.error.bar.size ,  position=position_dodge()
                ,color = tree.sizes.error.bar.color) +
  ylab(bquote('Tree biomass (Mg  dry mass  '*tree^-1*') ')) +
  facet_wrap( Tree.type  ~ ., ncol = 2, nrow = 1 , scales = 'free_x' )   +
  #force_panelsizes(cols = c(.8, 1)) +
 scale_y_continuous(breaks=seq(0,30,10) , labels = scales::number_format(accuracy = 1.0) 
                  # ,  expand = c(0.5,0.5)
                    ) +
 coord_cartesian(ylim=c(-6,30))+
  scale_x_discrete(limits = rev(levels('sci.name'))) +
                 #  ,   expand = c(0.5,0.5))
  theme(
    plot.margin = unit(c(  fig.tree.mass.p.mg.top ,   
                           fig.tree.mass.p.mg.right,
                           fig.tree.mass.p.mg.bottom,  
                           fig.tree.mass.p.mg.left), "cm"),
  #  axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = .5, hjust=1, size = tree.sizes.y.ticks.fs , face = 'italic'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_text(size =   tree.sizes.y.tit.fs),
    strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
    strip.text.x = element_text(size =  tree.fig.strip.fs, color = 'black')
  , panel.border = element_rect(colour = "black", fill=NA, size=1)
    ) 




ggsave("Fig_tree_mass.jpeg", fig.tree.mass.b  , path = "Figures.out", width=680, height=450, units="px", scale=3)

ggsave("Fig_tree_mass.pdf", fig.tree.mass.b  , path = "Figures.out", width=680, height=450, units="px", scale=3)

getwd()

}
biom.tree.bio()




}



