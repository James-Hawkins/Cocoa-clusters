
#'  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'  The below R script develops clusters of cocoa plots by applying sequentially
#'  factor reduction using 'Multiple Factor Analysis' followed by hierarchical 
#'  clustering. The script has three main functions which are run sequentially:
#'  
#'  1. 'typology.data' which merges the 'comp' (composite) dataframe including all 
#'  plot level variables derived from a farm-survey (GGS 2021) with 'bioclim.shp', 
#'  a shapefile with spatially explicit climatic variables. 
#'  
#'  2. 'typology.settings' which specifies the main parameters of the clustering, namely the clustering method
#'  and quantity of clusters per production system (see paper)
#
#'  3. 'run.typology' conducts the multiple factor analysis, clustering, and generates 
#'  the resulting tables/figures.
#'  
#'  data.summarize' is used to derive summary statistics from the plot observations
#'  and for which data are summarized in the paper.
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

  # Typology sample selection
  T.df <- T.df[ !is.na(T.df$tree.count.per.ha) & T.df$tree.count.per.ha > 600 & T.df$tree.count.per.ha < 1600,]
  
  T.df <- T.df[ !( !is.na(T.df$gross.revenue.frt.usd.per.ha) & T.df$gross.revenue.frt.usd.per.ha > 800  & T.df$cc.catg.str != 'Hybrid shade')  ,]
  T.df <- T.df[ !( !is.na(T.df$gross.revenue.ann.crop.usd.per.ha) & T.df$gross.revenue.ann.crop.usd.per.ha > 800 & T.df$cc.catg.str != 'Hybrid shade')   ,]
  
  T.df <- T.df[ ( !is.na(T.df$gross.revenue.frt.usd.per.ha) & T.df$gross.revenue.frt.usd.per.ha < 500  )  ,]
  T.df <- T.df[ ( !is.na(T.df$gross.revenue.ann.crop.usd.per.ha) & T.df$gross.revenue.ann.crop.usd.per.ha < 295 )   ,]
  
  T.df <- T.df[ !( !is.na(T.df$gross.revenue.usd.per.ha.frac.frt) & T.df$gross.revenue.usd.per.ha.frac.frt > .340 & T.df$cc.catg.str == 'Hybrid sun')  ,]

  T.df <- T.df[ !is.na(T.df$total.cocoa.costs.usd.per.ha) & T.df$total.cocoa.costs.usd.per.ha < 450 ,]
  
  T.df <- T.df[ !is.na(T.df$yld_pc_attain_plot) & T.df$yld_pc_attain_plot <= 100.000 ,]
  
  T.df <- T.df[ !is.na(T.df$shade.tree.to.cm.tree.ratio) & !(T.df$shade.tree.to.cm.tree.ratio < 0) & !(T.df$shade.tree.to.cm.tree.ratio*100 > 100 ) ,]
  
  
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
    
    n.total <- n.hysn + n.hysh+ n.amz

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
    mfa.hysh <- hysh.plots[ , c(  'hhID',
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
    
    # Define index of vector which represents first qualitative variable 
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
     8. max temp, elev ; almost N fert
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
    First 11 PCs ; 8 - 12 quantitative variable correspondence
    8. TEMP, ELEV
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
    
  
   #  fviz_gap_stat(gap_stat.hysh)
    # fviz_gap_stat( gap_stat.hysun)
   # fviz_gap_stat(gap_stat.amaz )
    

    # ~~~~~~ CLUSTERING ~~~~~~ #
    
    # Hybrid shade
    d <- daisy(hc.hysh[,c(2:length(hc.hysh))], metric="gower")
    fit <- hclust(d=d, method= cluster.method.hysh)
    
   #plot.new()
  #  plot(fit, hang=-1)
    groups.hysh <- cutree(fit, k = num.clusts.hysh)     
 #  rect.hclust(fit, k=5, border="red")
    
    # Amazonia
    d <- daisy(hc.amaz[,c(2:length(hc.amaz))], metric="gower")
    fit <- hclust(d=d, method= cluster.method.amaz)
    
    groups.amaz <- cutree(fit, k=  num.clusts.amz)   
    
    # Hybrid sun
    d <- daisy(hc.hysun[,c(2:length(hc.hysun))], metric="gower")
    fit <- hclust(d=d, method=cluster.method.hysun)
    
    #  plot.new()
    # plot(fit, hang=-1)
    groups.hysun <- cutree(fit, k = num.clusts.hysn )   
    #rect.hclust(fit, k=5, border="red")
    
   # plot.new()
   # plot(fit, hang=-1)
   
   # rect.hclust(fit, k=5, border="red")
    
    
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

  
if (clusters.redefine == 1){
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
}
  
  # Assign descriptive names for clusters to appear in results figures
  ordered.cluster.names <- c(
    'Fertiliser & High altitude',
    'Fertiliser & Low altitude',
    'Labour & Fertiliser',
    'Weeding & Waterlogged',
    'Low tree density',
    'Cluster num 6',
    'Nutrient deficient', # Hybrid shade #1
    'High tree density',
    'Open & Balanced fertilisation',
    'Dense & Balanced fertilisation',
    'N-fertilisation & Pollination',
    'Cluster num 12',
    'Nutrient deficient & High altitude', # Amazonia #1
    'Nutrient deficient & Low altitude',
    'N-fertilisation',
    'Fertiliser & Tall canopy',
    'Cluster num 17',
    'Cluster num 18'
  )
  
  
}

gen.clusters()

gen.figures <- function(){
  
  setwd(cc.typ.dir)
  
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
    'CO2 seqn. other'
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
        #ghg.ab.dat[ghg.ab.dat$Typology == t1 & ghg.ab.dat$Type == t2 & !is.na(ghg.ab.dat$Type) & !is.na(ghg.ab.dat$Typology ), 'tot.value.mn' ] %+=%  (val)
        
          
       ghg.dat[ghg.row.count, 'tot.value.mn' ] <- mean(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr' ], na.rm = TRUE)

        ghg.dat[ghg.row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr.sd' ]))

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
        
        
        vop.dat[vop.row.count, 'value.sd' ] <-  sd(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , var]))
        
        # Aggregated values
        vop.dat[vop.row.count, 'tot.value.mn' ] <- mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.net.VOP.1000.usd.per.ha']))

        vop.dat[vop.row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.net.VOP.1000.usd.per.ha.sd']))

        vop.row.count %+=% 1
        
      }
    }
  }
  
  
  
  ghg.row.count  <- 1
  vop.row.count <- 1
  
 # ghg.dat[, 'tot.value.mn' ] <- 0
#  vop.dat[,'tot.value.mn'] <- 0
  
  for (t1 in  typologies) {
    
    if (t1 == "Hybrid sun") {  clust.quant <- num.clusts.hysn}
    if (t1 == "Hybrid shade") {  clust.quant <- num.clusts.hysh}
    if (t1 == "Amazonia") {  clust.quant <- num.clusts.amz}
    
    
    for (t2 in 1:clust.quant){
      for (cat in emis.catg){
      
      val <- ghg.dat[ghg.row.count, 'value.mn' ]
     # ghg.dat[ghg.dat$Typology == t1 & ghg.dat$Type == t2 & !is.na(ghg.dat$Type) & !is.na(ghg.dat$Typology ), 'tot.value.mn' ] %+=%  (val)
      ghg.row.count %+=% 1
      }
      for (cat in rev.catg){
        val <-  vop.dat[vop.row.count, 'value.mn' ]
       # vop.dat[ vop.dat$Typology == t1 &  vop.dat$Type == t2 & !is.na( vop.dat$Type) & !is.na( vop.dat$Typology ), 'tot.value.mn' ] %+=%  (val)
        vop.row.count %+=% 1
      }
    }
  }
  

  vop.dat[ is.na(vop.dat$tot.value.sd), 'tot.value.sd' ] <- mean(na.omit(vop.dat[,'tot.value.sd']))

  
  # Factor specification
  # GHG
  
  ghg.dat$Typology <- factor( ghg.dat$Typology   , levels= ordered.typologies)
  
  ordered.emission.categories <<- c(  'CO2 seqn. other',
                                         'CO2 seqn. fruit',
                                         'CO2 seqn. cocoa',
                                         'CO2 seqn. shade',
                                         'N2O organic',
                                         'N2O syn' )
  

  
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
 
  fig.ghg.leg.x.coord  <<- 0.19 
  fig.ghg.leg.y.coord <<-  0.309925
  
  fig.vop.leg.x.coord <<- 0.88
  fig.vop.leg.y.coord <<- 0.7
  

 bar.width <<- 0.68
 bar.color <<- '#9aabbc'
 bar.color.border <<- 'black'
 
  box.plot.color <<- "#353839"
  fig.yd.bp.thickness <<- 0.25
  fig.yd.bp.fatten <<- 2
 
   box.plot.fill.color <<- "#a9a9a9"
   
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
 
 x.tick.angle <<- 270.0  
 
 facet.tx.size <<- 11.5
 facet.tx.size.yd <<- facet.tx.size
 
 label.fs <<- 11.5
 
 p.mg.left <<- 0.3
 p.mg.right <<- 0.2
 p.mg.top <<- 0.2
 p.mg.bottom <<- -0.35
 
 box.error.bar.width <<- 0.215
 
 fig.ghg.y.lim.max <<- 1.5
 fig.ghg.y.lim.min <<- -9.5
   
 intrag.leg.key.h.ghg <<- 0.35
 intrag.leg.key.w.ghg <<- 0.35
 
 intrag.leg.key.h.vop <<- 0.3
 intrag.leg.key.w.vop <<- 0.3
   
 intrag.leg.space.x.ghg <<- 0.3
 intrag.leg.space.y.ghg <<- 0.10
 
 intrag.leg.space.x.vop <<-  intrag.leg.space.x.ghg 
 intrag.leg.space.y.vop <<- intrag.leg.space.y.ghg
   
 intrag.fig.ghg.leg.text.fs <<- 8.5
 intrag.fig.vop.leg.text.fs <<- 8.5
   
 # Variable ranges
 #min.x.intrag.ghgr.int <<- 1.15 * min(na.omit(comp$typ.mn.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated) - na.omit(comp$typ.sd.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated))
 #max.x.intrag.ghgr.int <<- 1.15 * (max(na.omit(comp$typ.mn.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)) + max(na.omit(comp$typ.sd.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)))
 #min.x.intrag.ghgr.int <<- 1.15 * (min(na.omit(comp$typ.mn.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)) - min(na.omit(comp$typ.sd.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)))
 
 
 # GHG Figures
 labels_emis_srcs <<- c( bquote('Annual crop '*CO[2]*''),
                          bquote('Fruit tree '*CO[2]*''),
                         bquote('Cocoa tree '*CO[2]*''),
                          bquote('Shade tree '*CO[2]*''),
                      bquote('Organic N  '*N[2]*'O'),
                      bquote('Fertiliser N '*N[2]*'O'))
 
 
 colors_emis_srcs <<- c(  '#D5F5E3',
   '#82E0AA',
                          '#28B463',
                          '#186A3B',
                         '#CA6F1E' , # organic
                         '#F5B041'  # synth 
 )

 # VOP figure
 
 labels_rev_srcs <<-  ordered.revenue.categories
 
 
 colors_rev_srcs <<-  c('#cd754c' , 
                        '#9acd32', 
                                           '#fdee00',
                                           '#ff8c00',
                                           '#c19a6b' , 
                                           '#9E8363' ,
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
    ylab(bquote('Net GHG emissions (Mg   '*CO[2]*'eq '*ha^-1*' '*yr^-1*')'))  +
    facet_grid( cols = vars(Typology) , scales = "free_x", space = "free_x")   +
    scale_y_continuous(
      breaks = seq(-8.0, 0.0, by = 2))+ 
    coord_cartesian( ylim = c(fig.ghg.y.lim.min, fig.ghg.y.lim.max)) +
    guides(fill = guide_legend(byrow = TRUE))+
    theme(
      # Margin
      plot.margin = unit(c(p.mg.top , p.mg.right, p.mg.bottom ,p.mg.left), "cm"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, hjust=0.0, size = x.tick.fs),
      axis.text.y = element_blank(), 
    legend.background = element_rect(fill = 'white', size = 0.35, linetype = "solid",  colour = NA),
       # Legend
      legend.key.height = unit(intrag.leg.key.h.ghg, 'cm'),
      legend.key.width = unit(intrag.leg.key.w.ghg, 'cm'),
      legend.spacing.y = unit(intrag.leg.space.y.ghg, 'cm'),
      legend.spacing.x = unit(intrag.leg.space.x.ghg  , 'cm'),
      legend.position = c(fig.ghg.leg.x.coord , fig.ghg.leg.y.coord),
      legend.margin = margin(1.1 , 1.1 , 1.1 , 1.1) ,
 legend.title = element_blank(),
     axis.title.y = element_blank(),
      legend.text = element_text(size = intrag.fig.ghg.leg.text.fs),
     strip.text.x = element_text(size =  facet.tx.size, color = 'black'),
      strip.background = element_rect(color = 'black' , fill='white' , size=1.0 , linetype="solid"),
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
      breaks = seq(0.0, 2, by = 0.5))+
    coord_cartesian( ylim = c(-0.25, 1.75)) +
   guides(fill = guide_legend(byrow = TRUE))+
    theme(
      plot.margin = unit(c(p.mg.top,p.mg.right, p.mg.bottom ,p.mg.left), "cm"),
      axis.ticks.x = element_blank(),
  legend.background = element_rect(fill = 'white', size = 0.35, linetype = "solid",  colour = NA),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, hjust=0.0, size = x.tick.fs),
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      # Legend
      legend.text = element_text(size = intrag.fig.vop.leg.text.fs),
      legend.key.height = unit(intrag.leg.key.h.vop, 'cm'),
      legend.key.width = unit(intrag.leg.key.w.vop, 'cm'),
      legend.spacing.x = unit(intrag.leg.space.x.vop, 'cm'),
      legend.spacing.y = unit(intrag.leg.space.y.vop, 'cm'),
      legend.margin = margin(1.1,1.1,1.1,1.1),
      legend.key.size = unit(.42, 'cm'),
      legend.title =  element_blank(),
      legend.position = c(fig.vop.leg.x.coord , fig.vop.leg.y.coord),
       axis.title.y = element_blank(), 
      strip.text.x = element_text(size =  facet.tx.size, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1))  #810,320
  

  fig.intrag.yg.b <- ggplot( data= comp[!is.na(comp$typology)   &  !is.na(comp$typ.str)     ,]  ) +
 stat_boxplot(aes(y = yld_pc_attain_plot  , x = typ.str.fill, group = typ.str.fill ), outlier.shape = NA ,  coef = 10 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 , lwd= fig.yd.bp.thickness , fatten = fig.yd.bp.fatten)+
    stat_boxplot(aes(y = yld_pc_attain_plot  , x = typ.str.fill, group = typ.str.fill ), geom = 'errorbar' , coef = 10 , color = box.plot.color,  alpha= 5 , lwd= fig.yd.bp.thickness , width = box.error.bar.width)+
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
    strip.text.x = element_text(size =  facet.tx.size.yd, color = 'black'),
  strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )

fig.intrag.yd.act.b <- ggplot( data = comp[!is.na(comp$typology)   &  !is.na(comp$typ.str)   &  !is.na(comp$typ.str.fill)   ,]  ) +
 geom_boxplot(aes(y = cc.yd.lc.mn.Mg.ha  , x = typ.str.fill , group = typ.str.fill) ,  outlier.shape = NA, coef = 5 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 , lwd= fig.yd.bp.thickness , fatten = fig.yd.bp.fatten )+
  stat_boxplot(aes(y = cc.yd.lc.mn.Mg.ha  , x = typ.str.fill, group = typ.str.fill ), geom = 'errorbar' , coef = 10 , color = box.plot.color,  alpha= 5 , lwd= fig.yd.bp.thickness , width = box.error.bar.width) +
  ylab(bquote('Actual yield (Mg  '*ha^-1*' '*yr^-1*')    '))+
  facet_grid( cols = vars(typology) , scales = "free_x", space = "free_x")   +
  scale_y_continuous(breaks = seq(0, 3, by = .5), labels = scales::number_format(accuracy = 0.1))+
  theme(
    plot.margin = unit(c(p.mg.top,p.mg.right,p.mg.bottom,p.mg.left*.9), "cm"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = x.tick.angle , vjust = 0.5, hjust=0.0, size = x.tick.fs),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.y =  element_blank(), 
    strip.text.x = element_text(size =  facet.tx.size.yd, color = 'black'),
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
                 'CO2 seqn. other')
  
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
        
        # vr <- var.names[]
        
        ids <- comp[comp$typology ==  t1  & !is.na(comp$typology)  , 'hhID']
        
        inter.ghg.dat[row.count, 'Emission.category'] <-  cat
        
        inter.ghg.dat[row.count, 'Typology' ] <- t1

        inter.ghg.dat[row.count, 'value.mn' ] <- mean(comp[comp$hhID %in% ids , var] , na.rm =  TRUE)
        val <-   inter.ghg.dat[row.count, 'value.mn' ]

        #  Aggregated values
       # inter.ghg.ab.dat[row.count, 'tot.value.mn' ] <- mean(comp[comp$hhID %in% ids  , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr' ])
        #inter.ghg.ab.dat[inter.ghg.ab.dat$Typology == t1 & !is.na(inter.ghg.ab.dat$Typology), 'tot.value.mn' ] %+=%   (val )
        
        
        inter.ghg.dat[row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$hhID %in% ids  , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr.sd' ]))
        
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
        interg.vop.dat[row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$hhID %in% ids , 'lc.net.VOP.1000.usd.per.ha.sd']))
        interg.vop.dat[row.count, 'facet.lab' ] <-  'All systems'
        row.count %+=% 1
        
      }
  }
  
  interg.vop.dat <<- interg.vop.dat
  
  # Factor specification
  # Absolute GHG
  inter.ghg.dat$Typology <- factor( inter.ghg.dat$Typology   , levels= ordered.typologies)
  
  ordered.emission.categories <<-   ordered.ab.emission.categories
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
  
  p.mg.interg.ghg.bottom <<- 3
  p.mg.interg.vop.bottom <<- 2.85
  p.mg.interg.yd.bottom <<- 1.25

  p.interg.yd.mg.top <<- 0.2
  p.interg.yd.mg.right <<- 0.2
  p.interg.yd.mg.bottom  <<- 2.4
  p.interg.yd.mg.left <<- -0.05
  
  p.interg.yg.mg.top <<- 0.2
  p.interg.yg.mg.right <<- 0.2
  p.interg.yg.mg.bottom <<- -0.25
  p.interg.yg.mg.left <<- 0.2
  
  interg.leg.key.h.ghg <<- 0.37
  interg.leg.key.w.ghg <<- 0.37
  interg.leg.key.h.vop <<- 0.47
  interg.leg.key.w.vop <<- 0.47
  
  fig.interg.ghg.x.tick.fs <<- x.tick.fs 
  fig.interg.vop.x.tick.fs <<-  fig.interg.ghg.x.tick.fs 
  
  fig.interg.ghg.y.tick.fs <<- 9
  fig.interg.vop.y.tick.fs <<- 9
  
  fig.interg.ghg.y.tit.fs <<- 11.5
  fig.interg.vop.y.tit.fs <<- 11.5
  
  interg.fig.ghg.leg.text.fs <<- 11.5
  interg.fig.vop.leg.text.fs <<- 10.0
  
  fig.interg.yg.y.tick.fs <<- 7
  fig.interg.yg.y.tit.fs <<- 12.5
  fig.interg.yg.x.tick.fs <<-  fig.interg.ghg.x.tick.fs 
  
  fig.interg.ghg.bar.width <<- 0.65
  fig.interg.vop.bar.width <<- 0.65
  
  fig.interg.yg.bar.width  <<- 0.65
  
  fig.vop.y.lim.min <<-  -0.25
  fig.vop.y.lim.max <<- 1.75
  
  fig.vop.leg.x.coord <<- 0.58
  fig.vop.leg.y.coord <<- 0.7
  
  point.size.interg <<- 1.05 * point.size.intrag
  
}
fig.params.interg()



interg.figs <- function(){
  
  fig.bar.intergr.ghgr.t.b <- ggplot( data = inter.ghg.dat[,]  )  +
    geom_bar(aes(y = value.mn  , x = Typology, fill = Emission.category) , position="stack", stat="identity" , width = fig.interg.ghg.bar.width , colour = NA , size=bar.chart.border.thickness ) +
    geom_point(aes(y = tot.value.mn    , x = Typology),stat = "identity",  shape = point.type, size = point.size.interg  , color = point.color.border , fill=point.color.fill  , stroke =  point.border.thickness)  +
    geom_errorbar(aes(ymin = tot.value.mn -  tot.value.sd, ymax = tot.value.mn +  tot.value.sd , x = Typology) , width= error.bar.width , size = error.bar.size , color = error.bar.color
    ) +
    scale_fill_manual(labels = display.names.ab.emission , values = colors_emis_srcs ) +
    xlab('') +
    ylab('') +
    scale_y_continuous(
      limits = c( fig.ghg.y.lim.min , fig.ghg.y.lim.max) ,breaks = seq(-8.0, 0, by = 2.0), 
      labels = scales::number_format(accuracy = 0.1))  +
   facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
    ylab(bquote('Net GHG emissions (Mg   '*CO[2]*'eq '*ha^-1*' '*yr^-1*')     '))  +
    guides(fill = guide_legend(byrow = TRUE)) +
    theme(
      legend.text = element_text(size = interg.fig.ghg.leg.text.fs),
      plot.margin = unit(c(fig.bar.intergr.ghgr.p.mg.top , p.mg.right , p.mg.interg.ghg.bottom , fig.bar.intergr.ghgr.p.mg.left), "cm"),
      axis.ticks.x = element_blank(),
      axis.text.y =  element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs),
     axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, hjust=0.0, size = fig.interg.ghg.x.tick.fs ),
    panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "none",
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
      limits = c(-0.25, 1.75) ,breaks = seq(0.0, 2, by = 0.5))+
  guides(fill = guide_legend(byrow = TRUE)) +
    facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
   ylab(bquote('Value of production (1000 USD  '*ha^-1*' '*yr^-1*')    ')) +
    theme(
   plot.margin = unit(c(fig.bar.intergr.vop.p.mg.top , p.mg.right ,   p.mg.interg.vop.bottom , fig.bar.intergr.vop.p.mg.left), "cm"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, hjust=0.0, size = fig.interg.ghg.x.tick.fs) ,
      axis.text.y =  element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
     legend.text = element_text(size = intrag.fig.vop.leg.text.fs),
     legend.key.height = unit(intrag.leg.key.h.vop, 'cm'),
     legend.key.width = unit(intrag.leg.key.w.vop, 'cm'),
     legend.spacing.x = unit(intrag.leg.space.x.vop, 'cm'),
     legend.spacing.y = unit(intrag.leg.space.y.vop, 'cm'),
 legend.position = "none",
     legend.title =  element_blank(),
   legend.background = element_rect(fill=alpha('white', 0.4)),
     legend.margin = margin(.00005,.00005,.00005,.00005),
     legend.box.margin = margin(.00005,.00005,.00005,.00005),
   axis.title.y = element_text(size =  y.tit.sz),
 strip.text.x = element_text(size =  facet.tx.size, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1))
    
  fig.interg.yg.b <- ggplot( data= comp[ !is.na(comp$typology) &  !(comp$hhID %in%  hh_exclude.cc.yd.typ )   &  !is.na(comp$typology)    & !is.na(comp$yld_pc_attain_plot) ,]) +
    stat_boxplot(aes(y = yld_pc_attain_plot  , x = typology, group = typology ), geom = 'errorbar' , coef = 10 , color = box.plot.color,  alpha= 5 , lwd= fig.yd.bp.thickness , width = box.error.bar.width)+
    geom_boxplot(aes(y = yld_pc_attain_plot  , x = typology, group = typology), width = fig.interg.yg.bar.width , outlier.shape = NA, coef = 5 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 ,  lwd= fig.yd.bp.thickness , fatten = fig.yd.bp.fatten )+
    xlab('') +
    ylab('')+
    coord_cartesian(ylim=c(0, 100.0)) +
    scale_y_continuous(limits = c(0.0, 100.0) ,breaks = seq(0, 100, by = 25), labels = scales::number_format(accuracy = 1.0))+
  ylab('  Percent attainable yield (%)')+
    facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
    theme(
      plot.margin = unit(c( p.interg.yg.mg.top  , p.interg.yg.mg.right, p.interg.yg.mg.bottom, p.interg.yg.mg.left), "cm"), 
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs),
      axis.text.x = element_blank(),   panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title.y = element_text(size =   fig.interg.yg.y.tit.fs ), 
      strip.text.x = element_text(size =  facet.tx.size.yd, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1)
    )

  fig.interg.yd.act.b <- ggplot( data= comp[ !is.na(comp$typology) &  !(comp$hhID %in%  hh_exclude.cc.yd.typ )   &  !is.na(comp$typology)    & !is.na(comp$cc.yd.lt.mn.Mg.ha) ,]) +
    geom_boxplot(aes(y = cc.yd.lc.mn.Mg.ha   , x = typology, group = typology), width = fig.interg.yg.bar.width ,  outlier.shape = NA, coef = 5 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 ,  lwd= fig.yd.bp.thickness ,  fatten = fig.yd.bp.fatten)+
    stat_boxplot(aes(y = cc.yd.lc.mn.Mg.ha   , x = typology, group = typology ), geom = 'errorbar' , coef = 10 , color = box.plot.color,  alpha= 5 , lwd= fig.yd.bp.thickness , width = box.error.bar.width)+
    xlab('') +
    ylab('') +
    coord_cartesian(ylim=c(0,  1.5)) +
    facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
  scale_y_continuous( labels = scales::number_format(accuracy = 0.01))+
    ylab(bquote('Actual yield (Mg  '*ha^-1*' '*yr^-1*')    '))+
    theme(
      plot.margin = unit(c( p.interg.yd.mg.top  , p.interg.yd.mg.right, p.interg.yd.mg.bottom, p.interg.yd.mg.left), "cm"),
      axis.ticks.x = element_blank(),
  axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs),
      axis.text.x = element_text(angle = x.tick.angle , vjust = 0.5, hjust=0, size = fig.interg.yg.x.tick.fs ),
    panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title.y = element_text(size =    fig.interg.yg.y.tit.fs ),
      strip.text.x = element_text(size =  facet.tx.size.yd , color = 'black'),
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


stop()

# Table outputs

tb.typ.agfo <- function(){
  
  typologies <- c(
                'Hybrid sun',
                'Hybrid shade',
                'Amazonia'
                 )
  
  agfor.dat <- data.frame( stringsAsFactors=FALSE)
  agfor.dat <- 0
  
  for (T in typologies){
    
  v.name <- 'typ.str.fill'
  v1 <- 'tree.count.per.ha'  
  v2 <- 'plot.quant.shade.trees.ha'
  v3 <- 'plot.quant.trees.per.ha.greater.than.20.m'
  v4 <- 'plot.quant.trees.per.ha.greater.than.35.m' 
  v5 <- 'plot.quant.trees.per.ha.greater.than.50.m'
  
  v6 <- 'plot.quant.short.trees.ha' 
  v7 <- 'unique.tree.species' 
  v8 <- 'num_other_root_grain_crops'
  

  v.name.dat <- data.frame()
  v0.dat <- data.frame()
  v1.dat <- data.frame()
  v2.dat <- data.frame()
  v3.dat <- data.frame()
  v4.dat <- data.frame()
  v5.dat <- data.frame()
  v6.dat <- data.frame()
  v7.dat <- data.frame()
  v8.dat <- data.frame()
  
  v.name.dat <- 0
  v0.dat <- 0
  v1.dat <- 0
  v2.dat <- 0 
  v3.dat <- 0 
  v4.dat <- 0 
  v5.dat <- 0 
  v6.dat <- 0
  v7.dat <- 0 
  v8.dat <- 0
  
  comp$typ <- as.numeric((comp$typ))
  
  insert <<- " \u00B1 "
  
  if (T == "Hybrid sun") {  clust.quant <- num.clusts.hysn}
  if (T == "Hybrid shade") {  clust.quant <- num.clusts.hysh}
  if (T == "Amazonia") {  clust.quant <- num.clusts.amz}
  

  for ( t in 1:clust.quant ){
    
    tp <- t
    
    ids <- comp[!is.na(comp$typ.str) & comp$typ.str == tp & (comp$typology == T) , 'hhID'] 
    
    # Name of cluster
    name <- unique(comp[(comp$hhID %in% ids ) , v.name] )
    v.name.dat <- append(v.name.dat ,  name )
    
    # Variable 0
    v0 <- nrow( comp[(comp$hhID %in% ids ) , ] )

    v0.dat <- append(v0.dat ,  v0 )
    
    # Variable 1
    v1.m <- mean( na.omit(comp[(comp$hhID %in% ids ) , v1] ))
    v1.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v1] ))
    
    v1.m <- round( v1.m , 0)
    v1.sd <- round( v1.sd ,0) 
      
    v1.d <- str_c(v1.m , insert , v1.sd )
    if (length(ids) == 1) {v1.d <- v1.m  }
    v1.dat <- append(v1.dat ,  v1.d )
    
    # Variable 2
    v2.m <-   mean( na.omit(comp[(comp$hhID %in% ids ), v2] ))
    v2.sd <-    sd( na.omit(comp[(comp$hhID %in% ids ) , v2] ))
    
    v2.m <- round( v2.m , 1)
    v2.sd <- round( v2.sd , 1) 
    
    v2.d <- str_c(v2.m , insert , v2.sd )
    if (length(ids) == 1) {v2.d <- v2.m  }
    v2.dat <- append(v2.dat ,  v2.d )
    
    # Variable 3
    v3.m <-  mean( na.omit(comp[(comp$hhID %in% ids ), v3] ))
    v3.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v3] ))
    
    v3.m <- round( v3.m , 1)
    v3.sd <- round( v3.sd , 1) 
    
    v3.d <- str_c(v3.m , insert , v3.sd )
    if (length(ids) == 1) {v3.d <- v3.m  }
    v3.dat <- append(v3.dat ,  v3.d )
    
    # Variable 4
    v4.m <- mean( na.omit(comp[(comp$hhID %in% ids ), v4] ))
    v4.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v4] ))
    
    v4.m <- round( v4.m , 1)
    v4.sd <- round( v4.sd , 1) 
    
    v4.d <- str_c(v4.m , insert , v4.sd )
    if (length(ids) == 1) {v4.d <- v4.m  }
    v4.dat <- append(v4.dat ,  v4.d )
    
    # Variable 5
    v5.m <- mean( na.omit(comp[(comp$hhID %in% ids ), v5] ))
    v5.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v5] ))
    
    v5.m <- round( v5.m , 1)
    v5.sd <- round( v5.sd , 1) 
    
    v5.d <- str_c(v5.m , insert , v5.sd )
    if (length(ids) == 1) {v5.d <- v5.m  }
    v5.dat <- append(v5.dat ,  v5.d )

    # Variable 6
    v6.m <-  mean( na.omit(comp[(comp$hhID %in% ids ), v6] ))
    v6.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v6] ))
    
    v6.m <- round( v6.m , 1)
    v6.sd <- round( v6.sd , 1) 
    
    v6.d <- str_c(v6.m , insert , v6.sd )
    if (length(ids) == 1) {v6.d <- v6.m  }
    v6.dat <- append(v6.dat ,  v6.d )
    
    # Variable 7
    v7.m <- mean( na.omit(comp[(comp$hhID %in% ids ), v7] ))
    v7.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v7] ))
    
    v7.m <- round( v7.m , 1)
    v7.sd <- round( v7.sd , 1) 
    
    v7.d <- str_c(v7.m , insert , v7.sd )
    if (length(ids) == 1) {v7.d <- v7.m  }
    v7.dat <- append(v7.dat ,  v7.d )
    
    # Variable 8
    v8.m <- mean( na.omit(comp[(comp$hhID %in% ids ), v8] ))
    v8.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v8] ))
    
    v8.m <- round( v8.m , 1)
    v8.sd <- round( v8.sd , 1) 
    
    v8.d <- str_c(v8.m , insert , v8.sd )
    if (length(ids) == 1) {v8.d <- v8.m  }
    v8.dat <- append(v8.dat ,  v8.d )
    
   
    
  }
  
  v.name.dat <- v.name.dat[ -c(1)]
  v0.dat <-  v0.dat[ -c(1)]
  v1.dat <-  v1.dat[ -c(1)]
  v2.dat <-  v2.dat[ -c(1)]
  v3.dat <-  v3.dat[ -c(1)]
  v4.dat <-  v4.dat[ -c(1)]
  v5.dat <-  v5.dat[ -c(1)]
  v6.dat <-  v6.dat[ -c(1)]
  v7.dat <-  v7.dat[ -c(1)]
  v8.dat <-  v8.dat[ -c(1)]
  
  typ.dat <- cbind(
    v.name.dat,
    v0.dat,
    v1.dat,
    v2.dat,
    v3.dat,
    v4.dat,
    v5.dat,
    v6.dat,
    v7.dat,
    v8.dat
  )
  
  agfor.dat <- rbind(  agfor.dat,typ.dat)
  #  View(agfor.dat)
  
  

  }
  
#  agfor.dat <-  na.omit(agfor.dat[])
  agfor.dat <- agfor.dat[-c(1),]

  
  colnames(agfor.dat) = c(
    'Cluster name',
                   'n',
                   'Cocoa tree density (per ha)',
                   'Shade to cocoa trees (%)',   
                   '# trees > 20 m high',
                   '# trees > 35 m high',
                   '# trees > 50 m high',
                   'Other short trees to cocoa (%)',
                   '# tree crops',
                   '# root or grain crops')
  
  path_out = '.\\Figures.out\\'
  fileName = paste(path_out, 'table_typology.agfo.csv',sep = '')
  write.csv.utf8.BOM(  agfor.dat  ,  file =   fileName  ) 
  #  View(agfor.dat)
  

  agfor.dat <<- agfor.dat  
  
  
  
  
  
}
tb.typ.agfo() 


tb.typ.pract <- function(){
  
  typologies <- c( 'Hybrid sun',
    'Hybrid shade',
    'Amazonia'
  )
  
  pract.dat <- data.frame()
  pract.dat <- 0
  
  for (T in typologies){
    
    v.name <- 'typ.str.fill'
    v1 <- 'total_N_fert_applied_kg_per_ha'
    v2 <- 'total_non_N_fert_applied_kg_per_ha'
    v3 <- 'prunes_per_year'
    v4 <- 'weeds_per_year'
    v5 <- 'pollination.bool'
    v6 <- 'insecticides.bool'
    v7 <- 'fungicides.bool'
    v8 <- 'herbicides.bool'
    v9 <- 'detect.bool.blackpod'
    v10 <- 'detect.bool.capsid'
    v11 <- 'detect.bool.stemborer'
    
    v.name.dat <- data.frame()
    v0.dat <- data.frame()
    v1.dat <- data.frame()
    v2.dat <- data.frame()
    v3.dat <- data.frame()
    v4.dat <- data.frame()
    v5.dat <- data.frame()
    v6.dat <- data.frame()
    v7.dat <- data.frame()
    v8.dat <- data.frame()
    v9.dat <- data.frame()
    v10.dat <- data.frame()
    v11.dat <- data.frame()
    
    v.name.dat <- 0
    v0.dat <- 0
    v1.dat <- 0
    v2.dat <- 0 
    v3.dat <- 0 
    v4.dat <- 0 
    v5.dat <- 0 
    v6.dat <- 0
    v7.dat <- 0 
    v8.dat <- 0 
    v9.dat <- 0 
    v10.dat <- 0 
    v11.dat <- 0
    
    comp$typ <- as.numeric((comp$typ))
    
    insert <<- ' \u00B1 '
    
    if (T == "Hybrid sun") {  clust.quant <- num.clusts.hysn}
    if (T == "Hybrid shade") {  clust.quant <- num.clusts.hysh}
    if (T == "Amazonia") {  clust.quant <- num.clusts.amz}
    
    for ( t in 1:clust.quant){
      
      tp <- t
      
      ids <- comp[!is.na(comp$typ.str) & comp$typ.str == tp & (comp$typology == T) , 'hhID'] 

      # Name of cluster
      name <- unique(comp[(comp$hhID %in% ids ) , v.name] )
      v.name.dat <- append(v.name.dat ,  name )
      
      # Number of observations
      v0 <- nrow(comp[(comp$hhID %in% ids ) ,] )
      
      v0.dat <- append(v0.dat ,  v0 )
      
      # Variable 1
      v1.m <- mean( na.omit(comp[(comp$hhID %in% ids ) , v1] ))
      v1.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v1] ))
      
      v1.m <- round( v1.m , 0)
      v1.sd <- round( v1.sd , 0) 
      
      v1.d <- str_c(v1.m , insert , v1.sd )
      if (length(ids) == 1) {v1.d <- v1.m  }
      v1.dat <- append(v1.dat ,  v1.d )
      
      
      # Variable 2
      v2.m <- mean( na.omit(comp[(comp$hhID %in% ids ), v2] ))
      v2.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v2] ))
      
      v2.m <- round( v2.m , 0)
      v2.sd <- round( v2.sd , 0) 
      
      v2.d <- str_c(v2.m , insert , v2.sd )
      if (length(ids) == 1) {v2.d <- v2.m  }
      v2.dat <- append(v2.dat ,  v2.d )
      
      
      # Variable 3
      v3.m <- mean( na.omit(comp[(comp$hhID %in% ids ) , v3] ))
      v3.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v3] ))
      
      v3.m <- round( v3.m , 1)
      v3.sd <- round( v3.sd , 1) 
      
      v3.d <- str_c(v3.m , insert , v3.sd )
      if (length(ids) == 1) {v3.d <- v3.m  }
      v3.dat <- append(v3.dat ,  v3.d )
      
      # Variable 4
      v4.m <- mean( na.omit(comp[(comp$hhID %in% ids ) , v4] ))
      v4.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v4] ))
      
      v4.m <- round( v4.m , 1)
      v4.sd <- round( v4.sd , 1) 
      
      v4.d <- str_c(v4.m , insert , v4.sd )
      if (length(ids) == 1) {v4.d <- v4.m  }
      v4.dat <- append(v4.dat ,  v4.d )
      
      # Variable 5
      v5.m <- 100*mean( na.omit(comp[(comp$hhID %in% ids ) , v5] )) 
      
      v5.m <- round( v5.m , 0)
      
      v5.dat <- append(v5.dat ,  v5.m )
      
      
      # Variable 6
      v6.m <- 100*mean( na.omit(comp[(comp$hhID %in% ids ) , v6] )) 
      
      v6.m <- round( v6.m , 0)
      
      v6.dat <- append(v6.dat ,  v6.m )
      
      
      # Variable 7
      v7.m <- 100*mean( na.omit(comp[(comp$hhID %in% ids ) , v7] )) 
      
      v7.m <- round( v7.m , 0)
      
      v7.dat <- append(v7.dat ,  v7.m )
      
      # Variable 8
      v8.m <- 100*mean( na.omit(comp[(comp$hhID %in% ids ) , v8] )) 
      
      v8.m <- round( v8.m , 0)
        
      v8.dat <- append(v8.dat ,  v8.m )
      
      # Variable 9
      v9.m <- 100*mean( na.omit(comp[(comp$hhID %in% ids ) , v9] )) 
      
      v9.m <- round( v9.m , 0)
      
      v9.dat <- append(v9.dat ,  v9.m )
      
      # Variable 10
      v10.m <- 100*mean( na.omit(comp[(comp$hhID %in% ids ) , v10] )) 
      
      v10.m <- round( v10.m , 0)
      
      v10.dat <- append(v10.dat ,  v10.m )
      
      # Variable 11
      v11.m <- 100*mean( na.omit(comp[(comp$hhID %in% ids ) , v11] )) 
      
      v11.m <- round( v11.m , 0)
      
      v11.dat <- append(v11.dat ,  v11.m )

 
    }
    v.name.dat <- v.name.dat[ -c(1)]  
    v0.dat <-  v0.dat[ -c(1)]
    v1.dat <-  v1.dat[ -c(1)]
    v2.dat <-  v2.dat[ -c(1)]
    v3.dat <-  v3.dat[ -c(1)]
    v4.dat <-  v4.dat[ -c(1)]
    v5.dat <-  v5.dat[ -c(1)]
    v6.dat <-  v6.dat[ -c(1)]
    v7.dat <-  v7.dat[ -c(1)]
    v8.dat <-  v8.dat[ -c(1)]
    v9.dat <-  v9.dat[ -c(1)]
    v10.dat <-  v10.dat[ -c(1)]
    v11.dat <-  v10.dat[ -c(1)]
    
    typ.dat <- cbind(
      v.name.dat,
      v0.dat,
      v1.dat,
      v2.dat,
      v3.dat,
      v4.dat,
      v5.dat,
      v6.dat,
      v7.dat,
      v8.dat,
      v9.dat,
      v10.dat, 
      v11.dat
    )
    
    
    
    pract.dat <- rbind(pract.dat,typ.dat)
    
    #  View(pract.dat)
    
    
    
  }
  
  pract.dat <- na.omit(pract.dat)
  pract.dat <- pract.dat[-c(1),]
  
  
  
  colnames(pract.dat) = c(
    'Cluster',
    'n',
                        'N-fert (kg/ha/yr)',
                        'Non N-fert (kg/ha/yr)',
                        'Pruning (#/yr)',
                        'Weeding (#/yr)',
                        'Pollination (dummy)',
                        'Insecticide (dummy)',
                        'Fungicides (dummy)',
                        'Herbicides (dummy)',
    'Black pod detection rate (%)',
    'Capsid detection rate (%)',
    'Stemborer detection rate (%)'
  )
  
  #pract.dat <<-  na.omit(pract.dat[])

  path_out = '.\\Figures.out\\'
  fileName = paste(path_out, 'table_typology.pract.csv',sep = '')
  write.csv.utf8.BOM(  pract.dat  ,   file = fileName)  # Export
  
}
tb.typ.pract() 


tb.typ.clim <- function(){
  
  typologies <- c(
    'Hybrid sun',
    'Hybrid shade',
    'Amazonia'
  )
  
  cli.dat <- data.frame()
  cli.dat <- 0
  
  for (T in typologies){
    
    v1 <- 'precip'
    v2 <- 'temp.mmm'
    v3 <- 'elev'
    v4 <- 'water.log.bool'
    
    v0.dat <- data.frame()
    v1.dat <- data.frame()
    v2.dat <- data.frame()
    v3.dat <- data.frame()
    v4.dat <- data.frame()

    v0.dat <- 0
    v1.dat <- 0
    v2.dat <- 0 
    v3.dat <- 0 
    v4.dat <- 0 

    
    comp$typ <- as.numeric((comp$typ))
    
    insert <<- " \u00B1 "
    
    if (T == "Hybrid sun") {  clust.quant <- num.clusts.hysn}
    if (T == "Hybrid shade") {  clust.quant <- num.clusts.hysh}
    if (T == "Amazonia") {  clust.quant <- num.clusts.amz}
    
    
    for ( t in 1:clust.quant){
      
      tp <- t
      
      ids <- comp[!is.na(comp$typ.str) & comp$typ.str == tp & (comp$typology == T) , 'hhID'] 
      
      # Variable 0
      v0 <- nrow( comp[(comp$hhID %in% ids ) , ] )
      
      v0.dat <- append(v0.dat ,  v0 )
      
      # Variable 1
      v1.m <- mean( na.omit(T.df[(T.df$hhID %in% ids ) , v1] ))
      v1.sd <- sd( na.omit(T.df[(T.df$hhID %in% ids ) , v1] ))
      
      if (length(ids) == 1) {  v1.m <- T.df[(T.df$hhID %in% ids ) , v1] 
      v1.sd <- 0}
      
      v1.m <- round( v1.m , 1)
      v1.sd <- round( v1.sd , 1) 
      
      v1.d <- str_c(v1.m , insert , v1.sd )
      v1.dat <- append(v1.dat ,  v1.d )
      
      # Variable 2
      v2.m <- mean( na.omit(T.df[(T.df$hhID %in% ids ), v2] ))
      v2.sd <- sd( na.omit(T.df[(T.df$hhID %in% ids ) , v2] ))
      
      if (length(ids) == 1) {  v2.m <- T.df[(T.df$hhID %in% ids ) , v2] 
      v2.sd <- 0}
      
      v2.m <- round( v2.m , 2)
      v2.sd <- round( v2.sd , 2) 
      
      v2.d <- str_c(v2.m , insert , v2.sd )
      v2.dat <- append(v2.dat ,  v2.d )
      
      # Variable 3
      v3.m <- mean( na.omit(T.df[(T.df$hhID %in% ids ) , v3] ))
      v3.sd <- sd( na.omit(T.df[(T.df$hhID %in% ids ) , v3] ))
      
      if (length(ids) == 1) {  v3.m <- T.df[(T.df$hhID %in% ids ) , v3] 
      v3.sd <- 0}
      
      v3.m <- round( v3.m , 0)
      v3.sd <- round( v3.sd , 0) 
      
      v3.d <- str_c(v3.m , insert , v3.sd )
      v3.dat <- append(v3.dat ,  v3.d )
      
      # Variable 4
      v4.m <- 100* mean( na.omit(T.df[(T.df$hhID %in% ids ) , v4] ))
      v4.sd <- 100*sd( na.omit(T.df[(T.df$hhID %in% ids ) , v4] ))
      
      if (length(ids) == 1) {  v4.m <- T.df[(T.df$hhID %in% ids ) , v4] 
      v4.sd <- 0}
      
      v4.m <- round( v4.m , 0)
      v4.sd <- round( v4.sd ,0) 
      
      v4.d <- str_c(v4.m )
      v4.dat <- append(v4.dat ,  v4.d )
 
    }
    
    v1.dat <-  v1.dat[ -c(1)]
    v2.dat <-  v2.dat[ -c(1)]
    v3.dat <-  v3.dat[ -c(1)]
    v4.dat <-  v4.dat[ -c(1)]
    
    dat <- cbind(
      v3.dat,
      v2.dat,
      v1.dat,
      v4.dat
    )
    
    cli.dat <- rbind(cli.dat,dat)
    
  
  }
  
  cli.dat <- cli.dat[-c(1),]
 # cli.dat <- na.omit(cli.dat)
  
  colnames(cli.dat) = c('Altitude',
                        'Max temp',
                        'Rainfall',
                       'Water logging')
  
  
  path_out = '.\\Figures.out\\'
  fileName = paste(path_out, 'table_typology_clim.csv',sep = '')
  write.csv.utf8.BOM(  cli.dat  ,   file =   fileName )  # Export
  #View(cli.dat)
  
}
tb.typ.clim() 


# ~ ~ ADDITIONAL  FIGURES
# ~ Biomass figure

biom.tree.bio <- function(){

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


organics.tree.all <- organics.tree.all[ order(organics.tree.all$total.biomass.Mg.per.tree),]

organics.tree.all[,'sci.name'] <-  factor(organics.tree.all[,'sci.name'] , levels = organics.tree.all[,'sci.name'])
organics.tree.all[,'Tree.type'] <- factor(organics.tree.all[,'Tree.type'] , levels = tree.type.order)

  
# Settings for figure
tree.sizes.x.tick.fs <- 9.5
tree.sizes.bar.width <-  0.4275
tree.sizes.error.bar.width <-  0.24885
tree.sizes.error.bar.size <- 0.37 
tree.sizes.y.tit.fs <- 10.5
tree.sizes.y.ticks.fs <- 9
tree.sizes.error.bar.color <-  error.bar.color
tree.fig.strip.fs <- 10

fig.tree.mass.p.mg.top <- 0.35
fig.tree.mass.p.mg.bottom <- 0.35
fig.tree.mass.p.mg.right <- 0.35
fig.tree.mass.p.mg.left <- 0.25


fig.tree.mass.b <<- ggplot(organics.tree.all, aes(x = sci.name, y = total.biomass.Mg.per.tree)) +
  geom_bar(stat="identity", fill= bar.color, colour =  bar.color.border  , width =  tree.sizes.bar.width ,  position = position_dodge() ,  lwd = fig.yd.bp.thickness )+
  xlab('')+
  geom_errorbar(aes(ymin = total.biomass.Mg.per.tree - (total.biomass.Mg.per.tree.sd.abs  ), ymax = total.biomass.Mg.per.tree +  total.biomass.Mg.per.tree.sd.abs , x = sci.name, group = sci.name) , width=  tree.sizes.error.bar.width , size =   tree.sizes.error.bar.size ,  position=position_dodge()
                ,color = tree.sizes.error.bar.color) +
  ylab(bquote('Mature tree biomass (Mg  '*tree^-1*') ')) +
  facet_wrap( Tree.type  ~ ., ncol = 2, nrow = 1 , scales = 'free_x' )   +
  force_panelsizes(cols = c(.8, 1)) +
  scale_y_continuous(breaks=seq(0,60,10) , labels = scales::number_format(accuracy = 1.0))+
  scale_x_discrete(limits = rev(levels('sci.name')))+
  theme(
    plot.margin = unit(c(  fig.tree.mass.p.mg.top ,   
                           fig.tree.mass.p.mg.right,
                           fig.tree.mass.p.mg.bottom*0 -.4,  
                           fig.tree.mass.p.mg.left), "cm"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 270.0, vjust = 0.5, hjust=0.5, size = tree.sizes.y.ticks.fs , face = 'italic'),
   panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_text(size =   tree.sizes.y.tit.fs),
    strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
    strip.text.x = element_text(size =  tree.fig.strip.fs, color = 'black'),
   panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave("Fig_tree_mass.jpeg", fig.tree.mass.b  , path = "Figures.out", width=600, height=800, units="px", scale=2.5)

}
biom.tree.bio()
# COMPARTMENT SPECIFIC BIOMASS

fig.carbon <- function(){
# Compartment C sequestration

C.remv.sum  <- data.frame()

var.names <- c ('CO2.Mg.ha.yr',
                'compartment',
                'system')

comp[comp$cc.plot.sys == 'Traditional' & !is.na(comp$cc.plot.sys) ,'cc.plot.sys'] <- 'Amazonia'

length.hysn <- nrow((comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Hybrid sun' & !is.na(comp$cc.plot.sys) , ]))
length.hysh <-nrow((comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Hybrid shade' & !is.na(comp$cc.plot.sys) , ]))
length.amaz <-nrow((comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Amazonia' & !is.na(comp$cc.plot.sys) , ]))

total.row <- length.hysn + length.hysh + length.amaz
rows <- seq(1:total.row)

C.remv.sum <- data.frame(  matrix(ncol = length(var.names), nrow = length(rows) , dimnames=list(rows  , var.names  )))


# Cocoa
var <- 'lc.Biomass.CO2.remv.cc.cocoa.total.Mg.ha.yr'

sc.2.min <- length.hysn +1
sc.2.max <- sc.2.min  + length.hysh - 1
sc.3.min <- sc.2.max + 1
sc.3.max <- sc.3.min  + length.amaz - 1

C.remv.sum[1:length.hysn, 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Hybrid sun' & !is.na(comp$cc.plot.sys) , var]
C.remv.sum[sc.2.min:sc.2.max  , 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Hybrid shade' & !is.na(comp$cc.plot.sys) , var]
C.remv.sum[sc.3.min:sc.3.max , 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Amazonia' & !is.na(comp$cc.plot.sys) , var]

C.remv.sum[1:length.hysn, 'compartment'] <- 'Cocoa trees'
C.remv.sum[(sc.2.min ):sc.2.max, 'compartment'] <- 'Cocoa trees'
C.remv.sum[sc.3.min:sc.3.max, 'compartment'] <- 'Cocoa trees'

C.remv.sum[1:length.hysn, 'system'] <- 'Hybrid sun'
C.remv.sum[(sc.2.min ):sc.2.max, 'system'] <- 'Hybrid shade'
C.remv.sum[sc.3.min:sc.3.max, 'system'] <- 'Amazonia'

#View(C.remv.sum)

# Shade
var <- 'lc.Biomass.CO2.remv.cc.shade.total.Mg.ha.yr'
min.hysn <- max.hysn+ 1
min.hysh <- max.hysh  + 1
min.amaz <- max.amaz + 1
max.hysn <- min.hysn + length.hysn -1
max.hysh <- min.hysh + length.hysh - 1
max.amaz <- min.amaz + length.amaz -1


sc.1.min <- sc.3.max  +1
sc.1.max <- sc.1.min  + length.hysn -1
sc.2.min <- sc.1.max + 1
sc.2.max <- sc.2.min  + length.hysh - 1
sc.3.min <- sc.2.max + 1
sc.3.max <- sc.3.min  + length.amaz - 1

C.remv.sum[sc.1.min:sc.1.max, 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Hybrid sun' & !is.na(comp$cc.plot.sys) , var]
C.remv.sum[sc.2.min:sc.2.max, 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Hybrid shade' & !is.na(comp$cc.plot.sys) , var]
C.remv.sum[sc.3.min:sc.3.max, 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Amazonia' & !is.na(comp$cc.plot.sys) , var]

C.remv.sum[sc.1.min:sc.1.max, 'compartment'] <- 'Shade trees'
C.remv.sum[sc.2.min:sc.2.max, 'compartment'] <- 'Shade trees'
C.remv.sum[sc.3.min:sc.3.max, 'compartment'] <- 'Shade trees'

C.remv.sum[sc.1.min:sc.1.max, 'system'] <- 'Hybrid sun'
C.remv.sum[sc.2.min:sc.2.max, 'system'] <- 'Hybrid shade'
C.remv.sum[sc.3.min:sc.3.max, 'system'] <- 'Amazonia'

# Intercrop
var <- 'lc.Biomass.CO2.remv.cc.interc.total.Mg.ha.yr'
min.hysn <- max.hysn +1
min.hysh <- max.hysh+ 1
min.amaz <- max.amaz+ 1
max.hysn <- min.hysn + length.hysn -1
max.hysh <- min.hysh + length.hysh - 1
max.amaz <- min.amaz + length.amaz -1

sc.1.min <- sc.3.max  +1
sc.1.max <- sc.1.min  + length.hysn -1
sc.2.min <- sc.1.max + 1
sc.2.max <- sc.2.min  + length.hysh - 1
sc.3.min <- sc.2.max + 1
sc.3.max <- sc.3.min  + length.amaz - 1

C.remv.sum[sc.1.min:sc.1.max, 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Hybrid sun' & !is.na(comp$cc.plot.sys) , var]
C.remv.sum[sc.2.min:sc.2.max, 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Hybrid shade' & !is.na(comp$cc.plot.sys) , var]
C.remv.sum[sc.3.min:sc.3.max, 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Amazonia' & !is.na(comp$cc.plot.sys) , var]

C.remv.sum[sc.1.min:sc.1.max, 'compartment'] <- 'Annual crops'
C.remv.sum[sc.2.min:sc.2.max, 'compartment'] <- 'Annual crops'
C.remv.sum[sc.3.min:sc.3.max, 'compartment'] <- 'Annual crops'

C.remv.sum[sc.1.min:sc.1.max, 'system'] <- 'Hybrid sun'
C.remv.sum[sc.2.min:sc.2.max, 'system'] <-  'Hybrid shade'
C.remv.sum[sc.3.min:sc.3.max, 'system'] <- 'Amazonia'

# Fruit/other
var <- 'lc.Biomass.CO2.remv.cc.fruit.total.Mg.ha.yr' 
min.hysn <- max.hysn +1
min.hysh <- max.hysh+ 1
min.amaz <- max.amaz+ 1
max.hysn <- min.hysn + length.hysn -1
max.hysh <- min.hysh + length.hysh - 1
max.amaz <- min.amaz + length.amaz -1

sc.1.min <- sc.3.max  +1
sc.1.max <- sc.1.min  + length.hysn -1
sc.2.min <- sc.1.max + 1
sc.2.max <- sc.2.min  + length.hysh - 1
sc.3.min <- sc.2.max + 1
sc.3.max <- sc.3.min  + length.amaz - 1

C.remv.sum[sc.1.min:sc.1.max, 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Hybrid sun' & !is.na(comp$cc.plot.sys) , var]
C.remv.sum[sc.2.min:sc.2.max, 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Hybrid shade' & !is.na(comp$cc.plot.sys) , var]
C.remv.sum[sc.3.min:sc.3.max, 'CO2.Mg.ha.yr'] <- comp[comp$cm_crop == 1 & comp$cc.plot.sys == 'Amazonia' & !is.na(comp$cc.plot.sys) , var]

C.remv.sum[sc.1.min:sc.1.max, 'compartment'] <- 'Other trees'
C.remv.sum[sc.2.min:sc.2.max, 'compartment'] <- 'Other trees'
C.remv.sum[sc.3.min:sc.3.max, 'compartment'] <- 'Other trees'

C.remv.sum[sc.1.min:sc.1.max, 'system'] <- 'Hybrid sun'
C.remv.sum[sc.2.min:sc.2.max, 'system'] <- 'Hybrid shade'
C.remv.sum[sc.3.min:sc.3.max, 'system'] <- 'Amazonia'

ordered.compartments <- c('Annual crops',
                          'Cocoa trees',
                          'Shade trees',
                          'Other trees')

C.remv.sum[ , 'system'] <- factor (C.remv.sum[ , 'system'] , ordered.typologies)
C.remv.sum[ , 'compartment'] <- factor (C.remv.sum[ , 'compartment'] , ordered.compartments)

#View(C.remv.sum)

CO2r.bar.width <- compart.bmass.bar.width


fig.co2r.x.axis.fs <- 9
y.tit.sz <- 10
fig.co2r.facet.tx.size <- 10.5
box.plot.color <<- '#4a646c'
box.plot.fill.color <<- '#a9a9a9'


fig.compart.CO2r <<- ggplot( data = C.remv.sum ,  aes(x = system , y = CO2.Mg.ha.yr  )) +
#  geom_bar(data = C.remv.sum[ !(C.remv.sum$compartments == 'Total') , ]  , stat = "identity", fill = bar.color , colour =  bar.color.border  , width =  CO2r.bar.width ,  position = position_dodge())+
  xlab('')+
  geom_boxplot(data = C.remv.sum  ,aes(y = CO2.Mg.ha.yr    ,  x = system ), outlier.shape = NA, coef = 5 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 , lwd = fig.yd.bp.thickness , fatten = fig.yd.bp.fatten)+
 ylab(bquote('Carbon dioxide removal (Mg  '*CO[2]*' '*ha^-1*' '*yr^-1*')'))+
  facet_wrap( compartment ~ . , ncol = 4, nrow = 1, scales = 'free_x' )   + 
   scale_y_continuous(breaks = seq(-15,0,3),limit = c(-15,0) ,  labels = scales::number_format(accuracy = 1.0))+
  theme(
    plot.margin = unit(c(p.mg.top,p.mg.right,-.3,p.mg.left), "cm"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = x.tick.angle , vjust = 0.5, hjust=0.5, size = fig.co2r.x.axis.fs ),
    # axis.text.x = element_blank(),
    axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = fig.co2r.y.tick.sz ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_text(size =  fig.co2r.y.tit.sz),
    strip.text.x = element_text(size =  facet.tx.size, color = 'black'),
    strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )
fig.compart.CO2r

#fig.compart.CO2r <<-  annotate_figure(fig.compart.CO2r.b,   fig.lab = "b", fig.lab.pos ="top.left", fig.lab.size = fig.lab.fs, fig.lab.face = 'bold')


#fig.bio.1 <<- plot_grid( fig.tree.mass ,   biom.incr ,  align = "v", nrow = 1, ncol = 2 ,  rel_heights = c(50/100,  50/100) , rel_widths = c(60/100,  38/100) )
#fig.bio.2 <<- plot_grid(  fig.compart.bmass , fig.compart.CO2r,  align = "h", nrow = 2, ncol = 1 ,  rel_heights =   c(36/100,  50/100)   )

#ggsave("fig.bio.C.jpeg", fig.bio.2   , path = "Figures.out", width=620, height=750, units="px", scale=2.5)
ggsave("fig.compart.CO2r.jpeg", fig.compart.CO2r   , path = "Figures.out", width=710, height=390, units="px", scale=2.5)

}
fig.carbon()

fig.compart.CO2r 

# Typology data out (Export all typology data as one excel file)

cols.to.include <- vr.list
Typ.data <- comp[!is.na(comp$typ)   , c('typ' ,'typology' , 'hhID' , cols.to.include) ]

climatic.vars <- c('elev',
                   'temp',
                   'precip',
                   'temp.mmm',
                   'cv_precip'
                   )


for (v in climatic.vars){
  
  col.name <- str_c('col.',v)
  new.column <- 0
  
   for (r in 1:nrow(Typ.data)){
     
      cid <- Typ.data[r, 'hhID' ]
      new.column <- append (new.column , bioclim.df[bioclim.df$hhID == cid , v ])
  
   }
  
  new.column <- new.column[-c(1)]
  
  Typ.data <- cbind(Typ.data  , new.column )
  colnames(Typ.data)[ncol((Typ.data))] <- v
  
  
  setwd(main.dir)
}

names(Typ.data)[names(Typ.data) == 'elev'] <- 'Elevation.m.asl'
names(Typ.data)[names(Typ.data) == 'temp'] <- 'Temperature'
names(Typ.data)[names(Typ.data) == 'precip'] <- 'Rainfall'
names(Typ.data)[names(Typ.data) == 'temp.mmm'] <- 'Temp.mean.max.month'
names(Typ.data)[names(Typ.data) == 'cv_precip'] <- 'Rainfall.cv'


path_out = '.\\Figures.out\\'

Typology.data.file.name = paste(path_out,'Typology_data.csv',sep = '')

write.csv(T.df, Typology.data.file.name)

write.csv(Typ.data  , Typology.data.file.name )



comp <<- comp
setwd(main.dir) 

} # END TYPOLOGY CODE
  
gen.figures()
  
  
  
run.typology()


# Type comparisons
fig.intrag.yg
fig.intrag.yd.act


fig.barintrag.ghgr.t   # 810, 320
fig.bar.intrag.vop

fig.yd 

fig.ghg  # ideal dimensions 950, 400
fig.vop # ideal dimensions 860, 390


# System comparisons
fig.interg.yd.act
fig.interg.yg
#fig.bar.intergr.ghgr.t.b
#fig.bar.interg.vop.b


# Allometric results
fig.tree.mass.b

fig.compart.bmass 
fig.compart.CO2r

#Typology.data.file.name = paste(path_out, 'Typology_data.xlsx',sep = '')
#write.xlsx(T.df, Typology.data.file.name )

Typology.data.file.name = paste(path_out, 'Typology_data.csv',sep = '')


write.csv(T.df, Typology.data.file.name )











