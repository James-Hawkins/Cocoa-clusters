
#'  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#'  The below R script develops clusters of cocoa plots by applying sequentially
#'  factor reduction using 'Multiple Factor Analysis' (Pages, 2002) and hierarchical 
#'  clustering using R's 'hclust' (Stats package). 
#'  
#'  The script has three functions which are run sequentially:
#'  
#'  1. 'typology.data' which merges the 'comp' (composite) dataframe including all 
#'  plot level variables derived from a farm-survey (GML 2021) with 'bioclim.shp', 
#'  a shapefile with spatially explicit climatic variables (Fick and Hijmans, 2017)
#'  
#'  2. 'typology.settings' which specifies the main parameters of the clustering, namely the clustering method
#'  and quantity of clusters per production system (see paper)
#
#'  3. 'gen.clusters' conducts the multiple factor analysis and clustering. Each plot id then gets assigned
#'  to a cluster identifier.
#'
#'  
#'  Note that the script used to derive the survey variables as well as the survey data 
#'  is not included here. This data and code can be obtained from authors upon request. 
#'  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



# Merge dataset used in the construction of the typology
typology.data <- function(){
  
  setwd(main.dir) 
  
  # Load bioclimatic data from shapefile
  bioclim <- readOGR("Spatial_data/bioclimatic_data.shp")
  
  #' Data derived from external spatial dataset for climatic and elevation variables:
  #' Fick, S.E. & Hijmans, R.J. (2017). WorldClim 2: new 1km spatial resolution climate 
  #' surfaces for global land areas. International Journal of Climatology 37 (12): 4302-4315
  
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

# Define clstering settings used to derive typology  
typology.settings <- function(){
  
  clusters.redefine <<- 1
  
  cluster.method <<- "ward.D2"  # Cluster method based on Ward's minimum variance (see description in paper)
  
  cluster.method.amaz <<-  cluster.method 
  cluster.method.hysh <<-  cluster.method 
  cluster.method.hysun <<- cluster.method 
  
  # Specify number of clusters per production system (see description in paper)
  num.clusts.hysh <<- 5
  num.clusts.hysn <<- 5
  num.clusts.amz <<- 4
  
}

typology.settings()
 
# Code to conduct MFA and assign clusters 
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
    print(paste('For hybrid sun: ', n.hysn))
    print(paste('For hybrid shade: ', n.hysh))
    print(paste('For Amazonia: ', n.amz))
    
    ids.considered <- append(hysh.ids ,hysun.ids  )
    ids.considered <- append(ids.considered ,amaz.ids  )

    comp[comp$hhID %in% ids.considered , 'typology'] <- NA

    
    #' Run Multiple Factor Analysis (PCAmix package in R)
    #' 
    #' Define variables used in multiple factor analysis for each of three production systems
    #' Note: variables used in MFA differ per production system 
    #' (e.g. because Hybrid sun omits select variables depicting quantity/variety of shade trees)
    
    # Hybrid sun
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

  
    # Hybrid shade
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
                                'fungi.app.cocoa',
                                'insecti.app.cocoa',
                                'herbi.app.cocoa',
                                'pollination.bool' ,
                                'detect.bool.blackpod',
                                'detect.bool.capsid',
                                'detect.bool.stemborer',
                                'water.logged'
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
    

    # RUN PCA MIX algorithm for each production system
    # Hybrid sun
    hysun.pcamix <- PCAmix(X.quanti=mfa.hysun[,c(2:(hysun.qvars.f.ix))],  
                         X.quali=mfa.hysun[,c((hysun.qvars.f.ix+1):ncol(mfa.hysun))], 
                         rename.level=TRUE, 
                         graph=FALSE, 
                         ndim=25)
    
    # Hybrid shade
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
    
   
  # Specify variables used in clustering for each system based on results of PCA mix 
    
    # Hybrid sun
    hysun.pcamix$eig
    hysun.pcamix$quanti$contrib.pct
    hysun.pcamix$quali$contrib.pct
    "
    HYBRID SUN
    First 11 Factors chosen based on Cramer's rule ; 9 - 11 quantitative variable correspondence
    9. Max. monthly temperature
    10. Other fertiliser, cocoa tree density, weeding frequency
    11. N fertiliser
    "
   
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
    
    # Hybrid shade
    hyshade.pcamix$eig
    hyshade.pcamix$quanti$contrib.pct
    hyshade.pcamix$quali$contrib.pct
    
    "
    First 12 Factors chosen based on Cramer's rule ; 8 - 12 quantitative variable correspondence
     8. Maximum monthly temperature, elevation
     9. Cocoa tree density, shade to cocoa tree density
     10. N fertiliser, density of medium shade trees
     11. Number of annual crops 
     12. N fertiliser

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
    First 11 Factors chosen based on Cramer's rule ; 8 - 11 quantitative variable correspondence
    8. Maximum monthly temperature, elevation
    9. Annual crops grown
    10. tall trees, n fert
    11. Density of tall trees and short shade trees, cocoa yield
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
    
    
    #' Calculate gap statistic for each system 
  
    #' Gap statistic for 'Hybrid sun'
    gap_stat.hysun <- clusGap(hc.hysun[,c(2:length(hc.hysun))], FUN = hcut, nstart = 250, K.max = 30, B = 50)
    
    #' Gap statistic for 'Hybrid Shade'
    gap_stat.hysh <- clusGap(hc.hysh[,c(2:length(hc.hysh))], FUN = hcut, nstart = 250, K.max = 30, B = 50)
  
    #' Gap statistic for 'Amazonia'
    gap_stat.amaz<- clusGap(hc.amaz[,c(2:length(hc.amaz))], FUN = hcut, nstart = 250, K.max = 30, B = 50)
 
    
    # View results of gap statistics
    fviz_gap_stat( gap_stat.hysun)
    
    fviz_gap_stat(gap_stat.hysh)
    
    fviz_gap_stat(gap_stat.amaz )
    

    #' ~~~~~~ CLUSTERING ~~~~~~ #
    #' Hierarchical clustering performed for each production 
    #' system using 'daisy' (Cluster package version 2.1.6)
    
    
    # Hybrid sun
    d <- daisy(hc.hysun[,c(2:length(hc.hysun))], metric="gower")
    fit <- hclust(d=d, method=cluster.method.hysun)
    
    groups.hysun <- cutree(fit, k = num.clusts.hysn )  
    
    # Hybrid shade
    d <- daisy(hc.hysh[,c(2:length(hc.hysh))], metric="gower")
    fit <- hclust(d=d, method= cluster.method.hysh)
    
    groups.hysh <- cutree(fit, k = num.clusts.hysh)     
    
    # Amazonia
    d <- daisy(hc.amaz[,c(2:length(hc.amaz))], metric="gower")
    fit <- hclust(d=d, method= cluster.method.amaz)
    
    groups.amaz <- cutree(fit, k=  num.clusts.amz) 

    
    # Assign new name for cluster variable
    hc.hysh$typ <- (hc.hysh$cluster)
    hc.hysun$typ <-(hc.hysun$cluster)
    hc.amaz$typ <- (hc.amaz$cluster)
    
    # Column bind cluster variables with cluster assignments
    hc.hysun <- cbind(hc.hysun , cluster = groups.hysun)
    hc.hysh <- cbind(hc.hysh , cluster = groups.hysh)
    hc.amaz <- cbind(hc.amaz , cluster = groups.amaz )
  
    # Re-define typology variable as factor
    hc.hysh$typ <- as.factor(hc.hysh$cluster)
    hc.hysun$typ <- as.factor(hc.hysun$cluster)
    hc.amaz$typ <- as.factor(hc.amaz$cluster)
    
    #' Define the system to which each cluster exists in 'comp' dataframe 
    #' (dataframe used for extended variable definitions)
 
    comp[comp$hhID %in% hc.hysun$hhID , 'cc.plot.sys'] <- 'Hybrid sun'
    comp[comp$hhID %in% hc.hysh$hhID , 'cc.plot.sys'] <- 'Hybrid shade'
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

} 

gen.clusters() 


