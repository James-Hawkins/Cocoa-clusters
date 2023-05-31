

    
typology.settings <- function(){
  
  # Boolean for whether uncertainty quantification is conducted.
  # For time's sake this can be set to zero when conducting model runs
  typ.uncertainty <<- 0
  
  cluster.method <<- "ward.D2" 
  cluster.method <<- "ward.D" 
  cluster.method <<-"complete"
  cluster.method <<-"average"
  cluster.method <<-"mcquitty"
  cluster.method <<-"centroid"
  cluster.method <<-"single"
  
  # Acceptable options are complete, ward/ward.d2 , average
  # Best option is ward.d2
 #cluster.method <<- "ward.D2" 
   method <- cluster.method
  cluster.method.trad <<-  method 
  cluster.method.hysh <<-    method 
  cluster.method.hysun <<-   method 
  
  
  # best methods
  
}
typology.settings()


system.summmary <- function(){
  
  
  farmers <- T.df[ !is.na(T.df$Variety) & !is.na(T.df$plot.quant.shade.trees.ha ) & !(T.df$hhID %in%  hh_exclude.cc.yd.typ) & (T.df$yld_pc_attain_plot <= 100), ]
  

  
  # SUMMARY STATS
  summary(comp[comp$cm_crop == 1 , 'years_since_planted'])
  sd(na.omit(comp[comp$cm_crop == 1 , 'years_since_planted']))
  
  summary(comp[comp$cm_crop == 1 , 'cc_yield_fn_Mg_per_ha'])
  sd(na.omit(comp[comp$cm_crop == 1 , 'cc_yield_fn_Mg_per_ha']))
  
  
  nrow(farmers)
  nrow(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety) & !is.na(farmers$plot.quant.large.shade.trees.ha )& (farmers$plot.quant.large.shade.trees.ha <= thres.shade.trees.ha),])
  nrow(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety) & !is.na(farmers$plot.quant.large.shade.trees.ha ) & (farmers$plot.quant.large.shade.trees.ha > thres.shade.trees.ha),])
  nrow(farmers[farmers$Variety == 'Amazonia' & !is.na(farmers$Variety) & !is.na(farmers$plot.quant.shade.trees.ha ),])
  
  
  tot.sample <- nrow(farmers)
  
  nrow(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety) & farmers$plot.quant.shade.trees.ha <= thres.shade.trees.ha,])  /   tot.sample
  nrow(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety)& farmers$plot.quant.shade.trees.ha > thres.shade.trees.ha,])  /   tot.sample
  nrow(farmers[farmers$Variety == 'Amazonia' & !is.na(farmers$Variety),]) /   tot.sample
  
  nrow(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety) & farmers$plot.quant.shade.trees.ha <= thres.shade.trees.ha,])  
  nrow(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety)& farmers$plot.quant.shade.trees.ha > thres.shade.trees.ha,])  
  nrow(farmers[farmers$Variety == 'Amazonia' & !is.na(farmers$Variety),]) 
  
  
  # variable summary 
  variable <- 'years_since_planted' 
  variable <- 'plot.overstory.crown' 
  variable <- 'years_since_planted'
  variable <- 'cc.production.cycle'
  variable <- 'total_ha_cultv'
  variable <- 'plot.quant.large.shade.trees.ha'
  variable <- 'plot.quant.shade.trees.ha'
  
  # All systems
  summary(farmers[ ,variable])
  sd(na.omit(farmers[ ,variable]))
  

  
  summary(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety)  & farmers$plot.quant.large.shade.trees.ha > thres.shade.trees.ha  ,variable])
  sd(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety)  & farmers$plot.quant.large.shade.trees.ha > thres.shade.trees.ha  ,variable])
  
  summary(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety)  & farmers$plot.quant.large.shade.trees.ha <= thres.shade.trees.ha  ,variable])
  sd(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety)  & farmers$plot.quant.large.shade.trees.ha <= thres.shade.trees.ha  ,variable])
  

  summary(farmers[farmers$Variety == 'Amazonia' & !is.na(farmers$Variety) , variable])
  sd(na.omit(farmers[farmers$Variety == 'Amazonia' & !is.na(farmers$Variety) , variable]))
  
  nrow(farmers[farmers$Variety == 'Amazonia' & !is.na(farmers$Variety) &  farmers$plot.quant.large.shade.trees.ha > 25, ])
  nrow(farmers[farmers$Variety == 'Amazonia' & !is.na(farmers$Variety) &  farmers$plot.quant.large.shade.trees.ha == 0, ])
  
  
  
  # Soils
  nrow(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety)  & farmers$plot.quant.shade.trees.ha > thres.shade.trees.ha & farmers$domn_soil == 1  ,])
  nrow(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety)  & farmers$plot.quant.shade.trees.ha > thres.shade.trees.ha & farmers$domn_soil == 2  ,])
  nrow(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety)  & farmers$plot.quant.shade.trees.ha > thres.shade.trees.ha & farmers$domn_soil == 3  ,])
  nrow(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety)  & farmers$plot.quant.shade.trees.ha > thres.shade.trees.ha & farmers$domn_soil == 4  ,])
  nrow(farmers[farmers$Variety == 'Hybrid' & !is.na(farmers$Variety)  & farmers$plot.quant.shade.trees.ha > thres.shade.trees.ha & farmers$domn_soil == 5  ,])
  
  
  
  # Diseases
  nrow(farmers[ (farmers$detect.bool.CSSVD == 1) ,]) /    nrow(farmers[  ,])
  nrow(farmers[ (farmers$detect.bool.blackpod == 1) ,])/    nrow(farmers[  ,])
  nrow(farmers[ (farmers$detect.bool.capsid == 1) ,])/    nrow(farmers[  ,])
  nrow(farmers[ (farmers$detect.bool.rodents == 1) ,])/    nrow(farmers[  ,])
  nrow(farmers[ (farmers$detect.bool.stemborer == 1) ,])/    nrow(farmers[  ,])
  nrow(farmers[ (farmers$detect.bool.other == 1) ,])/    nrow(farmers[  ,])
  
  
  
  # livestock
  nrow(farmers[farmers$crop_lives == 1 ,])
  nrow(farmers[farmers$ccoa_apply_mnr_anywhere == 1 & !is.na(farmers$ccoa_apply_mnr_anywhere),])/nrow(farmers)
  nrow(farmers[farmers$ccoa_apply_cmp == 1 & !is.na(farmers$ccoa_apply_cmp) ,])/nrow(farmers)
  
  
  
  farmers <- farmers[farmers$crop_lives == 1 , ]
  
  
  summary(farmers[farmers$Variety == 'Amazonia','plot.quant.shade.trees.ha'])
  sd(na.omit(farmers[farmers$Variety == 'Amazonia','plot.quant.shade.trees.ha']))
  
  
  
  
}


typology.data <- function(){
  
  setwd(main.dir) 
  
   bioclim <- readOGR("Spatial_data/bioclimatic_data.shp")
   
  
   # Modify Bioclim variables
   bioclim$elev <- as.numeric(bioclim$elev1)
   bioclim$temp <- as.numeric(bioclim$temp1)
   bioclim$precip <- as.numeric(bioclim$precip_1)
   bioclim$temp.mmm <- as.numeric(bioclim$temp1_2)
   bioclim$cv_precip <- as.numeric(bioclim$cv_precip1)
   
   bioclim.df <<- as.data.frame( bioclim)

   
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
               'plot.overstory.crown',
               'other.tree.to.cm.tree.ratio',
               'shade.tree.to.cm.tree.ratio',
               'plot.quant.shade.trees.ha',
               'plot.quant.large.shade.trees.ha',
               'num_other_tree_crops',
               'num_other_root_grain_crops',
               'plot.quant.trees.per.ha.greater.than.50.m',
               'plot.quant.trees.per.ha.greater.than.35.m',
               'plot.quant.trees.per.ha.greater.than.20.m',
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
               'total_N_fert_applied_Mg_per_ha',
               'total_non_N_fert_applied_Mg_per_ha',
               'total_fert_applied_Mg_per_ha',
               # Labour
               'total.hi.labor.days.per.year.per.ha',
               # GHG emissions variables
               'lc.GHG.remv.cc.Mg.CO2eq.ha.yr',
               'lc.GHG.N2O.Mg.CO2eq',
               'lc.GHG.remv.C.Mg.CO2eq', 
               'lc.N2O.synthetic.total.Mg.CO2eq' ,
               'lc.N2O.organic.shadet.resd.total.Mg.CO2eq' ,
               'lc.N2O.organic.resd.total.Mg.CO2eq' , 
               'lc.N2O.organic.inter.resd.total.Mg.CO2eq' , 
               'lc.N2O.organic.other.total.Mg.CO2eq'  , 
               'lc.Biomass.CO2.remv.cc.shade.total.Mg.ha.yr' , 
               'lc.Biomass.CO2.remv.cc.cocoa.total.Mg.ha.yr' , 
               'lc.Biomass.CO2.remv.cc.fruit.total.Mg.ha.yr' , 
               'lc.Biomass.CO2.remv.cc.interc.total.Mg.ha.yr' ,
               # Profitability
               'lc.net.VOP.1000.usd.per.ha',
                 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.cc',
                  'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ann.crop',
               'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.frt',
               'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ag.cmd',
                  'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.fuelw',
                  'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.hardw',
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
   
   bioclim$yld_pc_attain_plot
   
  T.df <<- as.data.frame(bioclim)
 }
typology.data()
 

run.typology <- function( ){
  
  setwd(cc.typ.dir)
  
    # Typology sample selection
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 #T.df <- T.df[ !is.na(T.df$Biomass.C.dens.total.litter.Mg) & T.df$Biomass.C.dens.total.litter.Mg < 100,]
 # T.df <- T.df[ !is.na(T.df$tree.count.per.ha) & T.df$tree.count.per.ha > 600 & T.df$tree.count.per.ha < 1600,]

  #T.df <- T.df[ !is.na(T.df$lc.N2O.Mg.total.alloc) & T.df$lc.N2O.Mg.total.alloc < .3844 ,]
  
#  T.df <- T.df[ !is.na(T.df$plot.quant.shade.trees.ha) & T.df$plot.quant.shade.trees.ha < 100 ,]
#  T.df <- T.df[ !is.na(T.df$Biomass.C.dens.per.tree.sh.tree.Mg) & T.df$Biomass.C.dens.per.tree.sh.tree.Mg < 5 ,]
  
 # T.df <- T.df[ !is.na(T.df$Biomass.C.dens.interc.litter.Mg) & T.df$Biomass.C.dens.interc.litter.Mg < 16 ,]
  T.df <- T.df[ !is.na(T.df$yld_pc_attain_plot) & T.df$yld_pc_attain_plot <= 100.000 ,]
  
  
  T.df <- T.df[ !is.na(T.df$shade.tree.to.cm.tree.ratio) & !(T.df$shade.tree.to.cm.tree.ratio < 0) & !(T.df$shade.tree.to.cm.tree.ratio*100 > 100 ) ,]
  
  

  amaz.ids <- T.df[  #& !is.na(T.df$Variety) 
                  #  & (T.df$Variety == 'Amazonia') 
                      T.df$cc.catg.str == 'Amazonia'
                    & !is.na(T.df$cc.catg.str)
                    # T.df$Variety  == 'Amazonia' #& !is.na(T.df$plot.quant.large.shade.trees.ha )  
                   # & T.df$plot.quant.large.shade.trees.ha > thres.shade.trees.ha
                   ,'hhID']
  
  hysun.ids <- T.df[  T.df$cc.catg.str == 'Hybrid sun'
                    # & T.df$Variety == 'Hybrid' 
                   &  !is.na(T.df$cc.catg.str)
                    # & T.df$plot.quant.large.shade.trees.ha < thres.shade.trees.ha
                    #   T.df$plot.quant.shade.trees.ha < thres.shade.trees.ha #&
                   # &  T.df$plot.overstory.crown < 2.5
                     ,'hhID']
  
  hysh.ids <- T.df[   T.df$cc.catg.str == 'Hybrid shade'
                    & !is.na(T.df$cc.catg.str)
                  #   & T.df$Variety == 'Hybrid' 
                    # &  T.df$plot.quant.shade.trees.ha >= thres.shade.trees.ha # &
                   # &  T.df$plot.overstory.crown > 2.5
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
    
  #  hist(farmers$plot.quant.shade.trees )
  #  hist(farmers$plot.overstory.crown )
    

  #  farmers <<- na.omit(farmers)
  #  farmers <<- is.numeric(farmers$cm.tree)
    
   # hysh.plots$precip  <- as.numeric(hysh.plots$precip )
  #  hysun.plots$precip  <- as.numeric(farmers$precip)
  #  amaz.plots$precip  <- as.numeric(farmers$precip)
    
    
 # farmers$precip <- as.numeric(farmers$precip)
 # farmers$elev <- as.numeric(farmers$elev)
 # farmers$temp1_2 <- as.numeric(farmers$temp1_2)
    
  
  
 # hysh.plots <- bc.df[ bc.df$hhID %in% hysh.ids ,]
 # hysun.plots <- bc.df[ bc.df$hhID %in% hysun.ids ,]
 # amaz.plots <- bc.df[ bc.df$hhID %in% amaz.plots ,]
  
    # Sun
    mfa.hysun <- hysun.plots[ , c(  'hhID',
                                   'total_N_fert_applied_Mg_per_ha',
                                    'total_non_N_fert_applied_Mg_per_ha',
                                    # 'num_other_tree_crops',
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
                                    'detect.bool.stemborer'
    )]

    # Amazonia
    mfa.amaz <- amaz.plots[ , c(  'hhID',
                                 'total_N_fert_applied_Mg_per_ha',
                                 'total_non_N_fert_applied_Mg_per_ha',
                            'other.tree.to.cm.tree.ratio',
                            'plot.quant.trees.per.ha.greater.than.50.m',
                            'plot.quant.trees.per.ha.greater.than.35.m',
                            'plot.quant.trees.per.ha.greater.than.20.m',
                            'tree.config.cocoa',
                          'num_other_tree_crops',
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
                        'detect.bool.stemborer'
                      # 'domn_soil'
    )]

     
    # Shade
    mfa.hysh <- hysh.plots[ , c(  'hhID',
                               'total_N_fert_applied_Mg_per_ha',
                               'total_non_N_fert_applied_Mg_per_ha',
                               'other.tree.to.cm.tree.ratio',
                              'plot.quant.trees.per.ha.greater.than.50.m',
                              'plot.quant.trees.per.ha.greater.than.35.m',
                               'plot.quant.trees.per.ha.greater.than.20.m',
                               'num_other_tree_crops',
                               'num_other_root_grain_crops',
                               'prunes_per_year',
                               'weeds_per_year',
                               'yld_pc_attain_plot',
                                    'precip',
                               'elev',
                             #  'temp1',
                                  'temp1_2',
                               'fungi.app.cocoa',
                               'insecti.app.cocoa',
                               'herbi.app.cocoa',
                               'pollination.bool' ,
                               'tree.config.cocoa',
                           #    'path.contr.cc.vir.org',
                             #  'path.contr.cc.bact.org',
                             'detect.bool.blackpod',
                             'detect.bool.capsid',
                             'detect.bool.stemborer'
                               #  'path.contr.cc.fung.org'
                               # 'path.contr.cc.insect.org',
                               # 'domn_soil'
    )]
    
   
    
    hysun.qvars.f.ix <- 10
    amaz.qvars.f.ix <- 16
    hysh.qvars.f.ix <- 15
    
    
    mfa.hysun <-  na.omit(mfa.hysun)
    mfa.hysh <-  na.omit(mfa.hysh)
    mfa.amaz <-  na.omit(mfa.amaz)
    
    cor_matrix.hysh <- cor(mfa.hysh[ ,-c(1)])
    cor_matrix.hysun <- cor(mfa.hysh[ ,-c(1)])
    cor_matrix.amaz <- cor(mfa.amaz[ ,-c(1)])

    # Sun
    mfa.hysun[,c((hysun.qvars.f.ix+1):ncol(mfa.hysun))] <- as.character(mfa.hysun[,c((hysun.qvars.f.ix+1):ncol(mfa.hysun))])
    
    # Amaz
    mfa.amaz[,c((amaz.qvars.f.ix+1):ncol(mfa.amaz))] <- as.character(mfa.amaz[,c((amaz.qvars.f.ix+1):ncol(mfa.amaz))])
    
    # Shade
    mfa.hysh[,c((hysh.qvars.f.ix+1):ncol(mfa.hysh))] <- as.character(mfa.hysh[,c((hysh.qvars.f.ix+1):ncol(mfa.hysh))])
    
    # Amazonia
    amaz.pcamix <- PCAmix(X.quanti=mfa.amaz[,c(2:amaz.qvars.f.ix)],  
                          X.quali=mfa.amaz[,c((amaz.qvars.f.ix+1):ncol(mfa.amaz))], 
                          rename.level=TRUE, 
                          graph=FALSE, 
                          ndim=25)
    
    # Sun
    hysun.pcamix <- PCAmix(X.quanti=mfa.hysun[,c(2:(hysun.qvars.f.ix))],  
                         X.quali=mfa.hysun[,c((hysun.qvars.f.ix+1):ncol(mfa.hysun))], 
                         rename.level=TRUE, 
                         graph=FALSE, 
                         ndim=25)
    
    ## shade
    hyshade.pcamix <- PCAmix(X.quanti=mfa.hysh[,c(2:hysh.qvars.f.ix)],  
                             X.quali=mfa.hysh[,c((hysh.qvars.f.ix+1) :ncol(mfa.hysh))], 
                             rename.level=TRUE, 
                             graph=FALSE, 
                             ndim=25)
    
   
    hysun.pcamix$eig
    hysun.pcamix$quanti
    hysun.pcamix$quali
    
    amaz.pcamix$eig
    amaz.pcamix$quanti
    amaz.pcamix$quali
    
    hyshade.pcamix$eig
    hyshade.pcamix$quanti
    hyshade.pcamix$quali
    


    # Hybrid sun
    hc.hysun <- hysun.plots[ , c('hhID',
                                 'num_other_root_grain_crops',
                                 'total_N_fert_applied_Mg_per_ha',
                                 'total_non_N_fert_applied_Mg_per_ha',
                                 'yld_pc_attain_plot',
                                 # 'precip',
                                 'elev',
                                 'temp1_2',
                                 'fungi.app.cocoa',
                                 'insecti.app.cocoa',
                                 'herbi.app.cocoa',
                                 'pollination.bool' ,
                                 'tree.config.cocoa',
                                 'detect.bool.blackpod',
                                 'detect.bool.capsid',
                                 'detect.bool.stemborer'
    )]
    
    # Amazonia
    hc.amaz <- amaz.plots[ , c('hhID',
                               #  'shade.tree.to.cm.tree.ratio',
                               #   'num_other_root_grain_crops',
                               # 'num_other_tree_crops',
                               'other.tree.to.cm.tree.ratio',
                               'total_N_fert_applied_Mg_per_ha',
                               'total_non_N_fert_applied_Mg_per_ha',
                               'yld_pc_attain_plot',
                               'precip',
                               'prunes_per_year',
                               'weeds_per_year',
                               'elev',
                               'temp1_2',
                               'fungi.app.cocoa',
                               'insecti.app.cocoa',
                               'herbi.app.cocoa',
                               'pollination.bool' ,
                               'tree.config.cocoa',
                               'detect.bool.blackpod',
                               'detect.bool.capsid',
                               'detect.bool.stemborer' 
    )]
    
    # Hybrid shaded
    hc.hysh <- hysh.plots[ , c('hhID',
                               'yld_pc_attain_plot',
                              'elev',
                               'temp1_2',
                              'precip',
                             'num_other_root_grain_crops',
                           'total_N_fert_applied_Mg_per_ha',
                             'total_non_N_fert_applied_Mg_per_ha',
                           'plot.quant.trees.per.ha.greater.than.50.m',
                           'plot.quant.trees.per.ha.greater.than.35.m',
                           'plot.quant.trees.per.ha.greater.than.20.m',
                               'prunes_per_year',
                            # 'weeds_per_year',
                           'fungi.app.cocoa',
                           'insecti.app.cocoa',
                           'herbi.app.cocoa',
                           'pollination.bool' ,
                           'tree.config.cocoa',
                           'detect.bool.blackpod',
                           'detect.bool.capsid',
                           'detect.bool.stemborer'
    )]
    
    
    
    

    hc.hysh <-  na.omit(hc.hysh)
    hc.hysun <-  na.omit( hc.hysun)
    hc.amaz <-  na.omit( hc.amaz)
    
    # calculate opt num of clusters
    # calculate gap statistic for each number of clusters (up to 10 clusters)
 #   gap_stat.hysh <- clusGap(hc.hysh[,c(2:length(hc.hysh))], FUN = hcut, nstart = 250, K.max = 30, B = 50)
  # gap_stat.hysun <- clusGap(hc.hysun[,c(2:length(hc.hysun))], FUN = hcut, nstart = 250, K.max = 30, B = 50)
   # gap_stat.amaz<- clusGap(hc.amaz[,c(2:length(hc.amaz))], FUN = hcut, nstart = 250, K.max = 30, B = 50)
    
  
   # fviz_gap_stat(gap_stat.hysh)
  #  fviz_gap_stat( gap_stat.hysun)
  #  fviz_gap_stat(gap_stat.amaz )
    
    
    
    # CLUSTERING 
    num.clusts <- 5
    
    # Shade
    d <- daisy(hc.hysh[,c(2:length(hc.hysh))], metric="gower")
    fit <- hclust(d=d, method= cluster.method.hysh)
    
    plot.new()
  #  plot(fit, hang=-1)
    groups.hysh <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
  #  rect.hclust(fit, k=5, border="red")
    
    # Sun
    d <- daisy(hc.hysun[,c(2:length(hc.hysun))], metric="gower")
    fit <- hclust(d=d, method=cluster.method.hysun)
    
  #  plot.new()
   # plot(fit, hang=-1)
    groups.hysun <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
   #rect.hclust(fit, k=5, border="red")
    
    # Traditional
    d <- daisy(hc.amaz[,c(2:length(hc.amaz))], metric="gower")
    fit <- hclust(d=d, method= cluster.method.trad)
    
   # plot.new()
   # plot(fit, hang=-1)
    groups.amaz <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
   # rect.hclust(fit, k=5, border="red")
    
    
    hc.amaz <- cbind(hc.amaz , cluster = groups.amaz )
    hc.hysun <- cbind(hc.hysun , cluster = groups.hysun)
    hc.hysh <- cbind(hc.hysh , cluster = groups.hysh)
    
  
    hc.hysh$typ <- (hc.hysh$cluster)
    hc.hysun$typ <-(hc.hysun$cluster)
    hc.amaz$typ <- (hc.amaz$cluster)
    
    # Re-assign comp dataframe with typology clusters
    comp[comp$hhID %in% hc.hysh$hhID , 'cc.plot.sys'] <- 'Hybrid shade'
    comp[comp$hhID %in% hc.hysun$hhID , 'cc.plot.sys'] <- 'Hybrid sun'
    comp[comp$hhID %in% hc.amaz$hhID , 'cc.plot.sys'] <- 'Traditional'
    
    
    
    hc.hysh$typ <- as.factor(hc.hysh$cluster)
    hc.hysun$typ <- as.factor(hc.hysun$cluster)
    hc.amaz$typ <- as.factor(hc.amaz$cluster)
        

    
  
         # Variable summary
      #   var <- 'total_N_fert_applied_Mg_per_ha'
       #  var <- 'total_non_N_fert_applied_Mg_per_ha'
       #  var <- 'plot.quant.shade.trees'
       #  var <- 'num_other_crops'
       #  var <- 'path.contr.cc.vir.org'
       #  var <- 'path.contr.cc.bact.org'
         
       #  var <- 'prunes_per_year'
       #  var <- 'weeds_per_year'
       #  var <- 'temp1_2'
         
         
        var <- 'yld_pc_attain_plot'
       # var <- 'elev'
       # var <- 'precip'
       # var <- 'insecti.app.cocoa'
       # var <- 'fungi.app.cocoa'
      #  var <- 'herbi.app.cocoa'
       # var <- 'pollination.bool'
  
      # dat <- hc.hysh 
      #  dat <- hc.trad
        #  dat <- hc.hysun
        
     #   nrow( dat[ dat$cluster==1, ])
       # nrow( dat[ dat$cluster==2, ])
       # nrow( dat[ dat$cluster==3, ])
       # nrow( dat[ dat$cluster==4, ])
       # nrow( dat[ dat$cluster==5, ])
      #  nrow( dat[ dat$cluster==6, ])
        
       #  summary(comp[ comp$hhID %in%  dat[ dat$cluster==1 ,'hhID'],var])
        # summary(comp[ comp$hhID %in%  dat[ dat$cluster==2 ,'hhID'],var])
        # summary(comp[ comp$hhID %in%  dat[ dat$cluster==3 ,'hhID'],var])
        # summary(comp[ comp$hhID %in%  dat[ dat$cluster==4 ,'hhID'],var])
        # summary(comp[ comp$hhID %in%  dat[ dat$cluster==5 ,'hhID'],var])
        # summary(comp[ comp$hhID %in%  dat[ dat$cluster==6 ,'hhID'],var])
         
      
  # Name types
  typologies <<- c(
    'Hybrid sun'
             ,     'Hybrid shade'
           ,    'Amazonia'
    )
  
  vars.stat <- c('cc.yd.lt.mn.Mg.ha',
                 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr',
                 'yld_pc_attain_plot',
                 'lc.net.VOP.1000.usd.per.ha',
                 'Biomass.C.dens.total.Mg.ha',
                 'Biomass.C.dens.cc.tree.Mg',
                 'Biomass.C.dens.sh.tree.Mg',
                 'Biomass.C.dens.sh.litter.Mg',
                 'Biomass.C.dens.cc.litter.Mg',
                 'Biomass.C.dens.interc.litter.Mg'
                 )


 # ORDER CLUSTERS ACCORDING TO YIELD (ASCENDING) 
  for (T in typologies){
    
    if (T == 'Amazonia') {
      data <- hc.amaz}
    if (T == 'Hybrid shade') {
      data <- hc.hysh}
    if (T == 'Hybrid sun') {
      data <- hc.hysun}
    

    clusters <-   unique(data$typ)
    

  #  rm( cluster.names)
  #  rm(  cluster.values.ordered)
    
    cluster.names <- c()
    cluster.values <- c()
    
      for (c in 1:length(clusters)){
        
        
        ids <- data[data$hhID %in%  data[ data$cluster  == c,'hhID'],'hhID'] 
       # print((c==1))
        if (c == 1) {cluster.names <- append (cluster.names , 1)}
        if (c == 2) {cluster.names <- append (cluster.names , 2)}
        if (c == 3) {cluster.names <- append (cluster.names , 3)}
        if (c == 4) {cluster.names <- append (cluster.names , 4)}
        if (c == 5) {cluster.names <- append (cluster.names , 5)}
        if (c == 6) {cluster.names <- append (cluster.names , 6)}
        if (c == 7) {cluster.names <- append (cluster.names , 7)}
      
         variable.to.order <- mean(comp[comp$hhID %in% ids,'yld_pc_attain_plot'])
         #variable.to.order <- median(comp[comp$hhID %in% ids,'yld_pc_attain_plot'])
           
        if (c == 1) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 2) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 3) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 4) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 5) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 6) {cluster.values <- append (cluster.values ,  variable.to.order)}
        if (c == 7) {cluster.values <- append (cluster.values ,  variable.to.order)}
        
         
         cluster.values.ordered <- cluster.values

         cluster.dat <- data.frame(column_1 = cluster.names , column_2 =cluster.values.ordered)
         

         cluster.dat.ordered <- cluster.dat[order(cluster.dat$column_2),]

         if (T == 'Amazonia'){ t.nm.amz <- cluster.dat.ordered[,c(1)]  }
         if (T == 'Hybrid shade'){ t.nm.hy.sh <- cluster.dat.ordered[,c(1)]  }
         if (T == 'Hybrid sun'){ t.nm.hy.sun <- cluster.dat.ordered[,c(1)]  }
      }
   
  }

# assign string names to clusters  
  comp[ ,'typ.str']  <- NA
  comp[ ,'typology']  <- NA
  
  # Name types
  for (T in typologies){
    
    current.typology <- T
    

    
    if (T == 'Amazonia') {
      t.nm <-  t.nm.amz
      data <- hc.amaz} else if (T == 'Hybrid shade') {
      t.nm <-  t.nm.hy.sh
      data <- hc.hysh} else if (T == 'Hybrid sun') {
      t.nm <-  t.nm.hy.sun
      data <- hc.hysun}

  
  
    comp[comp$hhID %in% data[,    'hhID'] ,   'typology']  <- T
    data[ ,   'typology']  <- T
    
    
    
    for (i in seq(1:5))  {
      
      data[data$typ ==   i & !is.na(data$typ)  ,'typ.str']  <-  match(i,t.nm) 
      data[data$typ ==   i & !is.na(data$typ)  ,'typ']  <- i
      
      
        
    comp[comp$hhID %in% data[data$typ == i,    'hhID'] ,   'typ.str']  <- match(i,t.nm) 
    comp[comp$hhID %in% data[data$typ == i,    'hhID']  ,'typ']  <- i
    
    cluster.name <- str_c('Cluster numero ', i)
    #system.name <- T
    
    comp[comp$hhID %in% data[data$typ == i,    'hhID']  ,'typ.str.fill']  <-  cluster.name
    comp[comp$hhID %in% data[data$typ == i,    'hhID']  ,'facet.lab']  <-  'Systems'

    }
    
    if (T == 'Traditional') {
      hc.amaz <-  data } else if (T == 'Hybrid shade') {
      hc.hysh <-  data} else if (T == 'Hybrid sun') {
        hc.hysun <- data }
    
    
    
    
  }

  # Estimate means and standard dev
  "
  for (T in typologies){
  
  current.typology <- T
  
  if (T == 'Amazonia') {
    t.nm <-  t.nm.amz
    data <- hc.amaz}
  if (T == 'Hybrid shade') {
    t.nm <-  t.nm.hy.sh
    data <- hc.hysh}
  if (T == 'Hybrid sun') {
    t.nm <-  t.nm.hy.sun
    data <- hc.hysun}
  
  
  for (v in vars.stat) {

    sd.nm <- str_c('typ.sd.' ,  v)
    mn.nm <- str_c('typ.mn.' ,  v)
    
    comp[comp$typology ==   current.typology & !is.na(comp$typology)  ,sd.nm ]  <- NA
    comp[comp$typology ==   current.typology & !is.na(comp$typology)  ,mn.nm]  <- NA
    
    }
  
  
  # Get statistics per typology and typ 
 for (r in 1:nrow(data)) {   
  
     crID <- data[r , 'hhID']

     typ <- data[data$hhID ==   crID  ,'typ'] 
    
    
   # if (comp[comp$hhID ==   crID  ,'typ'] == 1){
      
      for (v in vars.stat) {
        
        sd.nm <- str_c('typ.sd.' ,  v)
        mn.nm <- str_c('typ.mn.' ,  v)
        
        
        comp[comp$hhID ==   crID  , mn.nm]  <- mean(na.omit( comp[comp$hhID %in% (comp[comp$typology == current.typology & comp$typ == typ & !is.na(comp$typ) & !is.na(comp$typology), 'hhID']) ,v]))
        comp[comp$hhID ==   crID  , sd.nm]  <- .5 *comp[comp$hhID ==   crID  , mn.nm] 
          #sd(na.omit( comp[comp$hhID %in% (comp[comp$typology == current.typology & comp$typ == typ & !is.na(comp$typ) & !is.na(comp$typology) , 'hhID']) ,v]))
        
        
         }
      
      
      

    
  }
 
}  
  "
 

ordered.typologies <<- c( 'Hybrid sun' , 'Hybrid shade' ,'Amazonia'  )
comp$typology <- factor(comp$typology  , levels= ordered.typologies)

 

ordered.typ <<- c(
 1,
 2,
 3,
 4,
 5
)


comp[,'typ.str'] <- factor(comp[,'typ.str'] , levels = ordered.typ)
#comp[,'typ.str'] <- droplevels(comp[,'typ.str'] )

comp[comp$typology=='Hybrid shade' & comp$typ.str ==4 , 'hhID']

#cc.typ <- comp[!is.na(comp$typology),]
#cc.typ <- cc.typ[,c('yld.pc.attainable.adj','typ' ,'typology')]


# Data prep for figures

fig.intrag.dt.prep <- function(){
  
  #Absolute GHG data
  ghg.ab.dat.var.names <- c ('Typology',
                             'Type',
                             'Type.str' , 
                             'Emission.category',
                             'value.mn',
                             'value.sd',
                             'tot.value.mn',
                             'tot.value.var',
                             'tot.value.sd',
                             'tot.value.95pci')

  
  emis.catg <- c(
    'N2O syn',
    'N2O organic',
    'CO2 seqn. shade',
    'CO2 seqn. cocoa',
    'CO2 seqn. fruit',
    'CO2 seqn. other'
  )
  
  
  
  rows <- seq( 1 : (3*5*length(emis.catg)) )
  
  ghg.ab.dat <- data.frame(  matrix(ncol = length(ghg.ab.dat.var.names) , nrow = length(rows) , dimnames=list(rows  , ghg.ab.dat.var.names )))
  
  #VOP prep
  # Value of production
  vop.dat.var.names <- c ('Typology',
                          'Type',
                          'Type.str' , 
                          'Revenue category',
                          'value.mn',
                          'value.sd',
                          'tot.value.mn',
                          'tot.value.var',
                          'tot.value.sd',
                          'tot.value.95pci')
  
  rev.catg <- c('Cocoa',
                'Fuelwood',
                'Hardwood lumber',
                'Annual crops',
                'Fruit trees',
                'Other agrocommodities')
  
  
  rows <- seq(1:(3*5*length(rev.catg )))
  
  vop.dat <- data.frame(  matrix(ncol = length(vop.dat.var.names) , nrow = length(rows) , dimnames=list(rows  , vop.dat.var.names )))
  
  var.names <- c('lc.net.VOP.1000.usd.per.ha.frac.rev.basis.cc',
                 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.fuelw',
                 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.hardw',
                 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ann.crop',
                 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.frt',
                 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ag.cmd'
  )
  
  
  
  ghg.row.count <- 1
  vop.row.count <- 1
  
  ghg.ab.dat[ , 'tot.value.mn' ] <- 0
  

  
  for (t1 in  typologies) {
    
    for (t2 in 1:5){
      
      if (typ.uncertainty == 1) {
        ids.to.include <- comp[comp$typ == t2 & comp$typology == t1 & !is.na(comp$typology) & !is.na(comp$typ) , 'hhID']
        uncert.analysis(comp,ids.to.include)
      }
      
      # Absolute GHG emission uncertainty
      for (cat in emis.catg){
        
        if (cat == emis.catg[1] ) {var <- 'lc.N2O.synthetic.total.Mg.CO2eq' }
        if (cat == emis.catg[2] ) {var <- 'lc.N2O.organic.total.Mg.CO2eq' }
        if (cat == emis.catg[3] ) {var <- 'lc.Biomass.CO2.remv.cc.shade.total.Mg.ha.yr'  }
        if (cat == emis.catg[4] ) {var <- 'lc.Biomass.CO2.remv.cc.cocoa.total.Mg.ha.yr' }
        if (cat == emis.catg[5] ) {var <- 'lc.Biomass.CO2.remv.cc.fruit.total.Mg.ha.yr' }
        if (cat == emis.catg[6] ) {var <- 'lc.Biomass.CO2.remv.cc.interc.total.Mg.ha.yr' }
        
        # vr <- var.names[]
        #   print(paste(var))
        
        ghg.ab.dat[ghg.row.count, 'Emission.category'] <-  cat
        
        ghg.ab.dat[ghg.row.count, 'Typology' ] <- t1
        ghg.ab.dat[ghg.row.count, 'Type' ] <- t2
        
        ghg.ab.dat[ghg.row.count, 'Type.str' ] <- unique(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'typ.str' ] )
        ghg.ab.dat[ghg.row.count, 'Type.str.fill' ] <- unique(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'typ.str.fill' ] )
        
        ghg.ab.dat[ghg.row.count, 'value.mn' ] <- mean(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , var] , na.rm = TRUE)
        
        val <- ghg.ab.dat[ghg.row.count, 'value.mn' ]
        
        ghg.ab.dat[ghg.row.count, 'value.sd' ] <-  sd( comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , var] , na.rm = TRUE)
        
        #  Aggregated values
        #ghg.ab.dat[ghg.ab.dat$Typology == t1 & ghg.ab.dat$Type == t2 & !is.na(ghg.ab.dat$Type) & !is.na(ghg.ab.dat$Typology ), 'tot.value.mn' ] %+=%  (val)
        
          
      #  ghg.ab.dat[ghg.row.count, 'tot.value.mn' ] <- mean(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr' ], na.rm = TRUE)
        ghg.ab.dat[ghg.row.count, 'tot.value.var' ] <- sd(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr' ] , na.rm = TRUE)
        
        ghg.ab.dat[ghg.row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr.sd' ]))
        ghg.ab.dat[ghg.row.count, 'tot.value.95pci' ] <-  mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr.95pci' ]))
        
        ghg.row.count %+=% 1
        
      }
      
      
      # VOP uncertainty
      for (cat in rev.catg){
        
        
        # var <-  var.names[ var.names == cat ]
        
        if (cat == rev.catg[1]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.cc' }
        if (cat == rev.catg[2]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.fuelw' }
        if (cat == rev.catg[3]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.hardw' }
        if (cat == rev.catg[4]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ann.crop' }
        if (cat == rev.catg[5]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.frt' }
        if (cat == rev.catg[6]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ag.cmd' }
        
        
        # vr <- var.names[]
        #   print(paste(var))
        
        vop.dat[vop.row.count, 'Revenue.category'] <-  cat
        
        vop.dat[vop.row.count, 'Typology' ] <- t1
        vop.dat[vop.row.count, 'Type' ] <- t2
        
        vop.dat[vop.row.count, 'Type.str' ] <- unique(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'typ.str' ] )
        vop.dat[vop.row.count, 'Type.str.fill' ] <- unique(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'typ.str.fill' ] )
   
        
        vop.dat[vop.row.count, 'value.mn' ] <- mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , var]))
       
        vop.dat[vop.row.count, 'value.sd' ] <-  sd(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , var]))
        
        # Aggregated values
        vop.dat[vop.row.count, 'tot.value.mn' ] <- mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.net.VOP.1000.usd.per.ha']))
        vop.dat[vop.row.count, 'tot.value.var' ] <- sd(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.net.VOP.1000.usd.per.ha']))
        
        vop.dat[vop.row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.net.VOP.1000.usd.per.ha.sd']))
        vop.dat[vop.row.count, 'tot.value.95pci' ] <-  mean(na.omit(comp[comp$typology ==  t1 & comp$typ == t2  & !is.na(comp$typology) & !is.na(comp$typ) , 'lc.net.VOP.1000.usd.per.ha.95pci']))
        
        vop.row.count %+=% 1
        
      }
    }
  }
  
  
  ghg.row.count  <- 1
  ghg.ab.dat[, 'tot.value.mn' ] <- 0
  for (t1 in  typologies) {
    
    for (t2 in 1:5){
      for (cat in emis.catg){
      
      val <- ghg.ab.dat[ghg.row.count, 'value.mn' ]
      ghg.ab.dat[ghg.ab.dat$Typology == t1 & ghg.ab.dat$Type == t2 & !is.na(ghg.ab.dat$Type) & !is.na(ghg.ab.dat$Typology ), 'tot.value.mn' ] %+=%  (val)
      ghg.row.count %+=% 1
    }
    }
  }
  
  # View(ghg.ab.dat)
  #(-0.856791474
  #-1.384693266
  #-0.436240308
  #-0.366667725)
  
  #  ghg.ab.dat <<- ghg.ab.dat
  # vop.dat <<- vop.dat
  
  
  
  
  
  
  # Factor specification
  #   Absolute GHG
  ghg.ab.dat$Typology <- factor( ghg.ab.dat$Typology   , levels= ordered.typologies)
  
  ordered.ab.emission.categories <<- c(  'CO2 seqn. other',
                                         'CO2 seqn. fruit',
                                         'CO2 seqn. cocoa',
                                         'CO2 seqn. shade',
                                      #   'N2O cocoa litter',
                                        # 'N2O shade litter',   
                                         #'N2O intercrop litter',
                                         'N2O organic',
                                         'N2O syn' )
  
 # emis.catg <- c(
   # 'N2O syn',
    #'N2O organic',
   # 'CO2 seqn. shade',
   # 'CO2 seqn. cocoa',
   # 'CO2 seqn. fruit',
   # 'CO2 seqn. other'
 # )
  
  
  
  ghg.ab.dat$Emission.category <- factor( ghg.ab.dat$Emission.category   ,  ordered.ab.emission.categories)
  
  ghg.ab.dat[,'Type.str'] <- factor(  ghg.ab.dat[,'Type.str'], levels = ordered.typ)
  
  ghg.ab.dat <<-  ghg.ab.dat
  
  
  # VOP
  
  vop.dat$Typology <- factor( vop.dat$Typology   , levels= ordered.typologies)
  
  ordered.revenue.categories <<- c('Cocoa'
                                   ,'Fruit trees', 
                                   'Annual crops',
                                   'Other agrocommodities',
                                   'Fuelwood',
                                   'Hardwood lumber')
  
  
  vop.dat$Revenue.category <- factor( vop.dat$Revenue.category   , ordered.revenue.categories)
  
  
  vop.dat[,'Type.str'] <- factor(  vop.dat[,'Type.str'], levels = ordered.typ)
  
  vop.dat <<-  vop.dat
  
 
  
  
}  
fig.intrag.dt.prep()

fig.settings <- function(){

  bar.chart.border.color <<- 'black'
  bar.chart.border.thickness <<- 0.035
 
  fig.ghg.leg.x.coord  <<- 0.19 
  fig.ghg.leg.y.coord <<-  0.3
  fig.vop.leg.x.coord <<- 0.19
  fig.vop.leg.y.coord <<- 0.7
  
 bar.width <<- 0.6
 bar.color <<- '#9aabbc'
 bar.color.border <<- 'black'
 
  box.plot.color <<- "#000000"
  fig.yd.bp.thickness <<- 0.12
  fig.yd.bp.fatten <<- 1.4
 
   box.plot.fill.color <<- "#a9a9a9"
# bar.widths
 error.bar.width <<- 0.225
 error.bar.size <<- 0.25
 error.bar.color <<- '#3f5265'  #353839' #   '#fc6c85'
 nvp.error.bar.color <<-  error.bar.color
 point.color.fill <<- 'black'
 point.color.border <<- error.bar.color
 point.type <<- '-'
 point.size <<- 3
 
 y.tick.fs <<- 8.5
 x.tick.fs <<- 8.0
 y.tit.sz <<- 9.5
 
 x.tick.angle <<- 270.0  
 
 facet.tx.size <<- 10
 facet.tx.size.yd <<- 9
 
 label.fs <<- 11.5
 
 p.mg.left <<- 0.7
 p.mg.right <<- 0.2
 p.mg.top <<- 0.2
 p.mg.bottom <<- 0.05
 
 
 fig.ghg.y.lim.max <<- 1
 fig.ghg.y.lim.min <<- -14
   
   
 intrag.leg.key.h.ghg <<- 0.36
 intrag.leg.key.w.ghg <<- 0.36
 
 intrag.leg.key.h.vop <<- 0.36
 intrag.leg.key.w.vop <<- 0.36
   
 intrag.leg.space.x.ghg <<- 0.6
 intrag.leg.space.y.ghg <<- 0.175
 
 intrag.leg.space.x.vop <<-  intrag.leg.space.x.ghg 
 intrag.leg.space.y.vop <<- intrag.leg.space.y.ghg
   
 intrag.fig.ghg.leg.text.fs <<- 8.5
 intrag.fig.vop.leg.text.fs <<- 8.5
   
 # Variable ranges
 min.x.intrag.ghgr.int <<- 1.15 * min(na.omit(comp$typ.mn.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated) - na.omit(comp$typ.sd.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated))
 max.x.intrag.ghgr.int <<- 1.15 * (max(na.omit(comp$typ.mn.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)) + max(na.omit(comp$typ.sd.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)))
 min.x.intrag.ghgr.int <<- 1.15 * (min(na.omit(comp$typ.mn.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)) - min(na.omit(comp$typ.sd.lc.GHG.remv.cc.kg.CO2eq.kg.econ.allocated)))
 
 
 # GHG Figures
 labels_emis_srcs <<- c( bquote('Annual crop '*CO[2]*''),
                          bquote('Fruit tree '*CO[2]*''),
                         bquote('Cocoa tree '*CO[2]*''),
                          bquote('Shade tree '*CO[2]*''),
                         
                        # bquote('Cocoa tree biomass '*N[2]*'O'),
                        # bquote('Shade tree biomass '*N[2]*'O'),
                       #  bquote('Other tree/crop '*N[2]*'O'),
                      #   bquote('External organic '*N[2]*'O'),
                      #   bquote('Mineral fertiliser '*N[2]*'O') )
                      bquote('Total synthetic '*N[2]*'O'),
                      bquote('Total organic  '*N[2]*'O')
                      
 
                       #    bquote('Synthetic fertiliser '*N[2]*'O') ,
                        #    bquote('Cocoa tree biomass '*N[2]*'O'),
                      #  bquote('Shade tree biomass '*N[2]*'O'),
                       # bquote('Other tree/crop '*N[2]*'O'),
                      #  bquote('Other organic '*N[2]*'O')
                        
                     #   )
 )
 
 colors_emis_srcs <<- c( '#82E0AA' ,# interc
                             '#2ECC71' , # fruit
                             '#239B56' ,   # cocoa
                             '#196F3D' , # shade
                         '#F5B041',  # organic
                         '#CA6F1E'  # synth
 )

 # VOP figure
 
 labels_rev_srcs <<-  ordered.revenue.categories
 
 
 colors_rev_srcs <<-  c('#cd754c' , 
                        '#9acd32', 
                                           '#fdee00',
                                           '#ff8c00',
                                           '#c19a6b' , 
                                           '#826644'  )

 

 
}
fig.settings()

intrag.figs <- function(){
  
  fig.barintrag.ghgr.t <- ggplot( data = ghg.ab.dat )  +
    geom_bar(aes(y = value.mn  , x = Type.str.fill , fill = Emission.category) , position="stack", stat="identity" , width = bar.width , colour=bar.chart.border.color , size=bar.chart.border.thickness)+
    geom_point(aes(y = tot.value.mn    , x = Type.str.fill),stat = "identity",  shape=point.type, size= point.size , color = point.color.border , fill=point.color.fill )  +
    geom_errorbar(aes(ymin = tot.value.mn -  tot.value.sd, ymax = tot.value.mn +  tot.value.sd , x = Type.str.fill) , width= error.bar.width , size = error.bar.size , color = error.bar.color
    ) +
    scale_fill_manual(labels = labels_emis_srcs  , values = colors_emis_srcs ) +
    xlab('')  +
    ylab(bquote('Net GHG emissions (Mg  '*CO[2]*'eq '*ha^-1*' '*yr^-1*')'))  +
    facet_wrap( Typology ~ . , ncol = 3, nrow = 1 , scales = "free_x")   +
    scale_y_continuous(
      limits = c( fig.ghg.y.lim.min , fig.ghg.y.lim.max) ,breaks = seq(-14.0, 2.0, by = 2), 
      labels = scales::number_format(accuracy = 0.1))  +
   # guides(fill = guide_legend(override.aes = list(colour = "gray", size = .392*2)))+
    #guides(fill = guide_legend(bycol = TRUE))+
    guides(fill = guide_legend(byrow = TRUE))+
    theme(
      # Margin
      plot.margin = unit(c(p.mg.top,p.mg.right, p.mg.bottom ,p.mg.left), "cm"),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, hjust=0.5, size = x.tick.fs),
      axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs),
      # Legend
      legend.key.height = unit(intrag.leg.key.h.ghg, 'cm'),
      legend.key.width = unit(intrag.leg.key.w.ghg, 'cm'),
      legend.spacing.y = unit(intrag.leg.space.y.ghg, 'cm'),
      legend.spacing.x = unit(intrag.leg.space.x.ghg, 'cm'),
      #legend.position = "bottom",
      legend.position = c(fig.ghg.leg.x.coord , fig.ghg.leg.y.coord),
      legend.margin = margin(.00005,.00005,.00005,0.00005),
      legend.box.margin = margin(.00005,.00005,.00005,.00005),
      legend.title = element_blank(),
      legend.text = element_text(intrag.fig.ghg.leg.text.fs),
      axis.title.y = element_text(size =  y.tit.sz, face = "plain"),
      strip.text.x = element_text(size =  facet.tx.size, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      # Panel
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black",
                                  fill=NA, size=1))
  
  fig.bar.intrag.vop.b <- ggplot( data = vop.dat[, ]  )  +
    geom_bar(aes(y = value.mn  , x = Type.str.fill, fill = Revenue.category) , position="stack", stat="identity",  width = bar.width , colour=bar.chart.border.color , size=bar.chart.border.thickness)+
    geom_errorbar(aes(ymin = tot.value.mn -  tot.value.sd, ymax = tot.value.mn +  tot.value.sd , x = Type.str.fill) , width= error.bar.width , size = error.bar.size , color = nvp.error.bar.color
    ) +
    scale_fill_manual(labels = labels_rev_srcs  , values = colors_rev_srcs )+
    xlab('')+
    ylab(bquote('Value of production (1000 USD '*ha^-1*' '*yr^-1*')    '))+
    facet_wrap( Typology ~ . , ncol = 3, nrow = 1 , scales = "free_x")   +
    scale_y_continuous(
      limits = c(0.0, 3.3) ,breaks = seq(0.0, 3.25, by = 0.5),
      labels = scales::number_format(accuracy = 0.1))  +
   # guides(fill = guide_legend(override.aes = list(colour = "gray", size = .392)))+
   guides(fill = guide_legend(byrow = TRUE))+
    theme(
      legend.text = element_text(size = intrag.fig.vop.leg.text.fs),
      plot.margin = unit(c(p.mg.top,p.mg.right, p.mg.bottom ,p.mg.left), "cm"),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, hjust=0.5, size = x.tick.fs),
      axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      # Legend
      legend.key.height = unit(intrag.leg.key.h.vop, 'cm'),
      legend.key.width = unit(intrag.leg.key.w.vop, 'cm'),
      legend.spacing.x = unit(intrag.leg.space.x.vop, 'cm'),
      legend.spacing.y = unit(intrag.leg.space.y.vop, 'cm'),
      #  legend.key.size = unit(.42, 'cm'),
     # legend.position = "bottom",
      legend.position = c(fig.vop.leg.x.coord , fig.vop.leg.y.coord),
      legend.margin = margin(.00005,.00005,.00005,.00005),
      legend.box.margin = margin(.00005,.00005,.00005,.00005),
      legend.title = element_blank(),
      axis.title.y = element_text(size =  y.tit.sz, face = "plain"),
      # axis.text.x=element_blank(),
      strip.text.x = element_text(size =  facet.tx.size, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1))  #810,320
  

  fig.intrag.yg.b <- ggplot( data= comp[!is.na(comp$typology)   &  !is.na(comp$typ.str)     ,]  ) +
  geom_boxplot(aes(y = yld_pc_attain_plot  , x = typ.str , group = typ.str) , outlier.shape = NA, coef = 5 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 , lwd= fig.yd.bp.thickness , fatten = fig.yd.bp.fatten)+
 xlab('') +
    scale_y_continuous(limits = c(0.0, 100.0) ,breaks = seq(0, 100, by = 25), labels = scales::number_format(accuracy = 1.0))+
  ylab('Percent attainable yield (%)')+
  facet_wrap( typology ~ . , ncol =3, nrow = 1, scales = 'free_x' )   + 
 # scale_y_continuous(labels = scales::number_format(accuracy = 1.0))+
  theme(
    plot.margin = unit(c(p.mg.top,p.mg.right,p.mg.bottom,p.mg.left), "cm"),
    axis.ticks.x = element_blank(),
   # axis.text.x = element_text(angle = x.tick.angle , vjust = 0.5, hjust=0.5, size = x.tick.fs),
    axis.text.x = element_blank(),
    axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_text(size =  y.tit.sz),
    strip.text.x = element_text(size =  facet.tx.size.yd, color = 'black'),
  strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )


fig.intrag.yd.act.b <<- ggplot( data= comp[!is.na(comp$typology)   &  !is.na(comp$typ.str)     ,]  ) +
 geom_boxplot(aes(y = cc.yd.lt.mn.Mg.ha  , x = typ.str.fill, group = typ.str.fill) , outlier.shape = NA, coef = 5 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 , lwd= fig.yd.bp.thickness , fatten = fig.yd.bp.fatten )+
  xlab('') +
 #   scale_y_reverse(limits = c(0, 2.5))+
  ylab(' (Mg ha yr)')+
  ylab(bquote('Actual yield (Mg  '*ha^-1*' '*yr^-1*')    '))+
  facet_wrap( typology ~ . , ncol =5, nrow = 1 , scales = 'free_x')   + 
   scale_y_continuous(limits = c(0.0, 3.0) ,breaks = seq(0, 3, by = .5), labels = scales::number_format(accuracy = 0.1))+
  theme(
    plot.margin = unit(c(p.mg.top,p.mg.right,p.mg.bottom,p.mg.left), "cm"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = x.tick.angle , vjust = 0.5, hjust=0.5, size = x.tick.fs),
   # axis.text.x = element_blank(),
    axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_text(size =  y.tit.sz),
    strip.text.x = element_text(size =  facet.tx.size.yd, color = 'black'),
    strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )



fig.intrag.yg <<- annotate_figure(fig.intrag.yg.b ,   fig.lab = "a", fig.lab.pos ="top.left", fig.lab.size = label.fs )

fig.intrag.yd.act <<- annotate_figure(fig.intrag.yd.act.b ,   fig.lab = "c", fig.lab.pos ="top.left", fig.lab.size = label.fs )


fig.barintrag.ghgr.t <<-  annotate_figure(fig.barintrag.ghgr.t,   fig.lab = "a", fig.lab.pos ="top.left", fig.lab.size = label.fs)

fig.bar.intrag.vop  <<- annotate_figure( fig.bar.intrag.vop.b ,   fig.lab = "a", fig.lab.pos ="top.left", fig.lab.size = label.fs)


#fig.intrag.1   <- plot_grid( fig.intrag.yg , fig.intrag.yd.act ,  align = "v", nrow = 1  )


#ggsave("Fig_cc_1.jpeg", fig.intrag.1 ,  path = "Figures.out", width=920, height=320, units="px", scale=2.5)

#ggsave("Fig_ghg_intra.jpeg", fig.barintrag.ghgr.t  ,  path = "Figures.out", width=1222, height=410, units="px", scale=2.5)
#ggsave("Fig_vop_intra.jpeg", fig.bar.intrag.vop  ,  path = "Figures.out", width=1060, height=380, units="px", scale=2.5)




}
intrag.figs()

fig.bar.intrag.vop

fig.barintrag.ghgr.t 


fig.intrag.yg
fig.intrag.yd.act

fig.interg.dt.prep <- function(){

  
  #Absolute GHG data
  ghg.ab.dat.var.names <- c ('Typology',
                             'Emission.category',
                             'value.mn',
                             'value.sd',
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
  
  inter.ghg.ab.dat <- data.frame(  matrix(ncol = length(ghg.ab.dat.var.names) , nrow = length(rows) , dimnames=list(rows  , ghg.ab.dat.var.names )))
  
  row.count <- 1

  inter.ghg.ab.dat[, 'tot.value.mn' ] <- 0
  
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
        
        inter.ghg.ab.dat[row.count, 'Emission.category'] <-  cat
        
        inter.ghg.ab.dat[row.count, 'Typology' ] <- t1

        inter.ghg.ab.dat[row.count, 'value.mn' ] <- mean(comp[comp$hhID %in% ids , var] , na.rm =  TRUE)
        val <-   inter.ghg.ab.dat[row.count, 'value.mn' ]
        inter.ghg.ab.dat[row.count, 'value.sd' ] <-  sd(na.omit(comp[comp$hhID %in% ids , var]))
        
        #  Aggregated values
        #inter.ghg.ab.dat[row.count, 'tot.value.mn' ] <- mean(comp[comp$hhID %in% ids  , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr' ])
        inter.ghg.ab.dat[inter.ghg.ab.dat$Typology == t1 & !is.na(inter.ghg.ab.dat$Typology), 'tot.value.mn' ] %+=%   (val )
        
        
        inter.ghg.ab.dat[row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$hhID %in% ids  , 'lc.GHG.remv.cc.Mg.CO2eq.ha.yr.sd' ]))
        
        inter.ghg.ab.dat[row.count, 'facet.lab' ] <-  'System aggregates'
        row.count %+=% 1
        
        
        
        
      }
  }
  
 # View(inter.ghg.ab.dat)
  inter.ghg.ab.dat[, 'tot.value.mn' ] <- 0
  ghg.row.count <- 1
  
  for (t1 in typologies) {
    for (cat in emis.catg){
      
      val <-   inter.ghg.ab.dat[ ghg.row.count, 'value.mn' ]
    inter.ghg.ab.dat[inter.ghg.ab.dat$Typology == t1 & !is.na(inter.ghg.ab.dat$Typology), 'tot.value.mn' ] %+=%   (val )
      
      ghg.row.count %+=% 1
    }
  }
  
  
 # View(inter.ghg.ab.dat)
  # Value of production
  vop.dat.var.names <- c ('Typology',
                          'Revenue category',
                          'value.mn',
                          'value.sd',
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
                'Other agrocommodities')
  
  var.names <- c('lc.net.VOP.1000.usd.per.ha.frac.rev.basis.cc',
                 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.fuelw',
                 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.hardw',
                 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ann.crop',
                 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.frt',
                 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ag.cmd'
  )
  
  row.count <- 1
  
  for (t1 in typologies) {
      for (cat in rev.catg){
        
        var <-  var.names[ var.names == cat ]
        
        if (cat == rev.catg[1]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.cc' }
        if (cat == rev.catg[2]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.fuelw' }
        if (cat == rev.catg[3]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.hardw' }
        if (cat == rev.catg[4]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ann.crop' }
        if (cat == rev.catg[5]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.frt' }
        if (cat == rev.catg[6]) {var <- 'lc.net.VOP.1000.usd.per.ha.frac.rev.basis.ag.cmd' }
        
        
        
        # vr <- var.names[]
        ids <- comp[comp$typology ==  t1  & !is.na(comp$typology) & !is.na(comp$typ) , 'hhID']
        
        
        interg.vop.dat[row.count, 'Revenue.category'] <-  cat
        
        interg.vop.dat[row.count, 'Typology' ] <- t1
        
        interg.vop.dat[row.count, 'value.mn' ] <- mean(na.omit(comp[comp$hhID %in% ids, var]))
        
        interg.vop.dat[row.count, 'value.sd' ] <-  sd(na.omit(comp[comp$hhID %in% ids , var]))
        
        # Aggregated values
        interg.vop.dat[row.count, 'tot.value.mn' ] <- mean(na.omit(comp[comp$hhID %in% ids, 'lc.net.VOP.1000.usd.per.ha']))
        interg.vop.dat[row.count, 'tot.value.sd' ] <-  mean(na.omit(comp[comp$hhID %in% ids , 'lc.net.VOP.1000.usd.per.ha.sd']))
        interg.vop.dat[row.count, 'facet.lab' ] <-  'System aggregates'
        row.count %+=% 1
        
      }
  }
  
  interg.vop.dat <<- interg.vop.dat
  
  # Factor specification
  #   Absolute GHG
  inter.ghg.ab.dat$Typology <- factor( inter.ghg.ab.dat$Typology   , levels= ordered.typologies)
  
  ordered.ab.emission.categories <<-   ordered.ab.emission.categories
  inter.ghg.ab.dat$Emission.category <- factor( inter.ghg.ab.dat$Emission.category   ,  ordered.ab.emission.categories)
  
  display.names.ab.emission <<-  labels_emis_srcs
  
  #inter.ghg.ab.dat[,'Type.str'] <- factor(  inter.ghg.ab.dat[,'Type.str'], levels = ordered.typ)
  
  inter.ghg.ab.dat  <<-  inter.ghg.ab.dat
  
  
  # VOP
  ordered.typologies <- c('Hybrid sun' ,  'Hybrid shade' , 'Amazonia' )
  interg.vop.dat$Typology <- factor( interg.vop.dat$Typology   , levels= ordered.typologies)
  
  ordered.revenue.categories <-   ordered.revenue.categories
  
  interg.vop.dat$Revenue.category <- factor( interg.vop.dat$Revenue.category   , ordered.revenue.categories)
  
  interg.vop.dat <<-  interg.vop.dat
  
  
}  
fig.interg.dt.prep()



interg.figs <- function(){
  
  fig.bar.inter.ghg.legend.row.spacing.cm <<- 0.25
  fig.bar.inter.nvp.row.spacing.cm <<- 0.25
  
  bar.interg.legend.left.margin <<- 7
  bar.interg.legend.right.margin <<- 2
  
  fig.bar.interg.vop.p.mg.bottom <<- .05
  fig.bar.intergr.ghgr.p.mg.top <<-  p.mg.top
  fig.bar.intergr.ghgr.p.mg.left <<- 0.1
  
  fig.bar.intergr.vop.p.mg.top <<-  fig.bar.intergr.ghgr.p.mg.top
  fig.bar.intergr.vop.p.mg.left <<-   fig.bar.intergr.ghgr.p.mg.left
  
  p.yd.mg.top <<- 0.2
  p.yd.mg.right <<- 0.2
  p.yd.mg.bottom  <<- 0.05
  p.yd.mg.left <<- -.05
  
  interg.leg.key.h.ghg <<- 0.37
  interg.leg.key.w.ghg <<- 0.37
  interg.leg.key.h.vop <<- 0.47
  interg.leg.key.w.vop <<- 0.47
  
  fig.interg.ghg.x.tick.fs <<- x.tick.fs # 11.5 
  fig.interg.vop.x.tick.fs <<-  fig.interg.ghg.x.tick.fs 
  
  fig.interg.ghg.y.tick.fs <<- 9
  fig.interg.vop.y.tick.fs <<- 9
  
  fig.interg.ghg.y.tit.fs <<- 11.5
  fig.interg.vop.y.tit.fs <<- 11.5
  
  interg.fig.ghg.leg.text.fs <<- 10.0
  interg.fig.vop.leg.text.fs <<- 10.0
  
  fig.interg.yg.y.tick.fs <<- 8.5
  fig.interg.yg.y.tit.fs <<- 11.5
  fig.interg.yg.x.tick.fs <<-  fig.interg.ghg.x.tick.fs 
  
  fig.interg.ghg.bar.width <<- 0.65
  fig.interg.vop.bar.width <<- 0.65
  
  fig.interg.yg.bar.width  <<- 0.65
  
  fig.vop.y.lim.min <<-  0
  fig.vop.y.lim.max <<- 1.5


  fig.bar.intergr.ghgr.t.b <<- ggplot( data = inter.ghg.ab.dat[,]  )  +
    geom_bar(aes(y = value.mn  , x = Typology, fill = Emission.category) , position="stack", stat="identity" , width = fig.interg.ghg.bar.width , colour=bar.chart.border.color , size=bar.chart.border.thickness ) +
    geom_point(aes(y = tot.value.mn    , x = Typology),stat = "identity",  shape = point.type, size= point.size , color = point.color.border , fill=point.color.fill ,   position=position_dodge(.9))  +
    geom_errorbar(aes(ymin = tot.value.mn -  tot.value.sd, ymax = tot.value.mn +  tot.value.sd , x = Typology) , width= error.bar.width , size = error.bar.size , color = error.bar.color
    ) +
    scale_fill_manual(labels = display.names.ab.emission , values = colors_emis_srcs ) +
    xlab('') +
    ylab('') +
   facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
    coord_cartesian( ylim = c(fig.ghg.y.lim.min, fig.ghg.y.lim.max)) +
    guides(fill = guide_legend(byrow = TRUE)) +
    theme(
      legend.text = element_text(size = interg.fig.ghg.leg.text.fs),
      plot.margin = unit(c(fig.bar.intergr.ghgr.p.mg.top , p.mg.right , p.mg.bottom , fig.bar.intergr.ghgr.p.mg.left), "cm"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y =  element_blank(),
     axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, hjust=0.5, size = fig.interg.ghg.x.tick.fs),
     # axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = fig.interg.ghg.y.tick.fs),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size =    fig.interg.ghg.y.tit.fs),
      strip.text.x = element_text(size =  facet.tx.size, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1))

  
  fig.bar.interg.vop.b <<- ggplot( data = interg.vop.dat[]  )  +
    geom_bar(aes(y = value.mn  , x = Typology, fill = Revenue.category) , position="stack", stat="identity" , width = fig.interg.vop.bar.width, colour=bar.chart.border.color , size=bar.chart.border.thickness)+
     geom_errorbar(aes(ymin = tot.value.mn -  tot.value.sd, ymax = tot.value.mn +  tot.value.sd , x = Typology) ,  width= error.bar.width , size = error.bar.size , color = nvp.error.bar.color
    ) +
    scale_fill_manual(labels = labels_rev_srcs  , values = colors_rev_srcs) +
    xlab('') +
    ylab('') +
        #scale_y_continuous(
     # limits = c(-.15, 1.5) ,breaks = seq(0, 1.5, by = 0.25),
    #  labels = scales::number_format(accuracy = 0.1))  +
    guides(fill = guide_legend(byrow = TRUE)) +
    facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
    coord_cartesian( ylim = c(fig.vop.y.lim.min, fig.vop.y.lim.max)) +
    theme(
      legend.text = element_text(size = interg.fig.vop.leg.text.fs),
      plot.margin = unit(c(fig.bar.intergr.vop.p.mg.top , p.mg.right , p.mg.bottom , fig.bar.intergr.vop.p.mg.left), "cm"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
     # axis.text.x = element_blank(),
     axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, hjust=0.5, size = fig.interg.ghg.x.tick.fs),
    #  axis.text.x = element_text(angle = x.tick.angle, vjust = 0.5, hjust=0.5, size = fig.interg.vop.x.tick.fs),
     # axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = y.tick.fs),
    axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "none",
      axis.title.y = element_blank(),
      #axis.title.y = element_text(size =  fig.interg.vop.y.tit.fs),
      # axis.text.x=element_blank(),
      strip.text.x = element_text(size =  facet.tx.size, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  fig.interg.yg.b <<- ggplot( data= comp[ !is.na(comp$typology) &  !(comp$hhID %in%  hh_exclude.cc.yd.typ )   &  !is.na(comp$typology)    & !is.na(comp$yld_pc_attain_plot) ,]) +
    geom_boxplot(aes(y = yld_pc_attain_plot  , x = typology, group = typology), width = fig.interg.yg.bar.width , outlier.shape = NA, coef = 5 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 ,  lwd= fig.yd.bp.thickness , fatten = fig.yd.bp.fatten )+
    xlab('') +
    ylab('')+
    coord_cartesian(ylim=c(0, 100.0)) +
     scale_y_continuous( labels = scales::number_format(accuracy = 0.1))+
    facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
    theme(
      plot.margin = unit(c( p.yd.mg.top , p.yd.mg.right , -.25 , p.yd.mg.left ), "cm"),
      axis.ticks.y = element_blank(),  
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
    #  axis.text.x = element_text(angle = x.tick.angle , vjust = 0.5, hjust=0.5, size = fig.interg.yg.x.tick.fs),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title.y = element_text(size =  fig.interg.yg.y.tit.fs),
      strip.text.x = element_text(size =  facet.tx.size.yd, color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1)
    )

  fig.interg.yd.act.b <<- ggplot( data= comp[ !is.na(comp$typology) &  !(comp$hhID %in%  hh_exclude.cc.yd.typ )   &  !is.na(comp$typology)    & !is.na(comp$cc.yd.lt.mn.Mg.ha) ,]) +
    geom_boxplot(aes(y = cc.yd.lt.mn.Mg.ha  , x = typology, group = typology), width = fig.interg.yg.bar.width ,  outlier.shape = NA, coef = 5 , color = box.plot.color, fill = box.plot.fill.color , alpha= 0.5 ,  lwd= fig.yd.bp.thickness ,  fatten = fig.yd.bp.fatten)+
    xlab('') +
    ylab('') +
    coord_cartesian(ylim=c(0,  3.0)) +
    facet_wrap(  facet.lab ~ . , ncol = 1, nrow = 1 , scales = "free_x")   +
  scale_y_continuous( labels = scales::number_format(accuracy = 0.01))+
    theme(
      plot.margin = unit(c(p.yd.mg.top,p.yd.mg.right,p.yd.mg.bottom ,p.yd.mg.left), "cm"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(angle = x.tick.angle , vjust = 0.5, hjust=0.5, size = fig.interg.yg.x.tick.fs),
      # axis.text.x = element_blank(),
     # axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = fig.interg.yg.y.tick.fs),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title.y = element_text(size =   fig.interg.yg.y.tit.fs),
      strip.text.x = element_text(size =  facet.tx.size.yd , color = 'black'),
      strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
      panel.border = element_rect(colour = "black", fill=NA, size=1)
    )  
  

  fig.interg.yg <- annotate_figure( fig.interg.yg.b,   fig.lab = "b", fig.lab.pos ="top.left", fig.lab.size = label.fs )   
  fig.interg.yd.act <-  annotate_figure( fig.interg.yd.act.b,   fig.lab = "d", fig.lab.pos ="top.left", fig.lab.size = label.fs )  
  
  fig.bar.intergr.ghgr.t <<- annotate_figure( fig.bar.intergr.ghgr.t.b,   fig.lab = "b", fig.lab.pos ="top.left", fig.lab.size = label.fs)
  fig.bar.interg.vop <<- annotate_figure(fig.bar.interg.vop.b,   fig.lab = "b", fig.lab.pos ="top.left", fig.lab.size = label.fs)
  
  
  fig.yield   <<- plot_grid(  fig.interg.yg,
                                 fig.interg.yd.act,
                              align = "v", 
                              nrow = 2, 
                              ncol = 1,
                              rel_heights = c(32/100, 42/100)
                              )
  
  
  ggsave("fig.yield.jpeg", fig.yield   , path = "Figures.out", width=610, height=1040, units="px", scale=1.65)

  fig.intrag.yg <<- annotate_figure(fig.intrag.yg.b ,   fig.lab = "a", fig.lab.pos ="top.left", fig.lab.size = label.fs )
  
  fig.intrag.yd.act <<- annotate_figure(fig.intrag.yd.act.b ,   fig.lab = "a", fig.lab.pos ="top.left", fig.lab.size = label.fs )
  
  
  fig.yg   <<- plot_grid(   fig.intrag.yg , 
                            fig.interg.yg , 
                             align = "h", 
                             nrow = 1, 
                             ncol = 2 , 
                             rel_widths = c(78/100, 22/100))
  
  fig.yd   <<- plot_grid(   fig.intrag.yg , 
                            fig.interg.yg , 
                            fig.intrag.yd.act , 
                            fig.interg.yd.act, 
                            align = "h", 
                            nrow = 2, 
                            ncol = 2 , 
                            rel_widths = c(78/100, 22/100),
                            rel_heights = c(40/100, 60/100))
  
  fig.vop   <<- plot_grid(   fig.bar.intrag.vop , 
                             fig.bar.interg.vop, 
                            align = "h", 
                            nrow = 1, 
                            ncol = 2 , 
                            rel_widths = c(78/100, 22/100))
  
  fig.ghg   <<- plot_grid(  fig.barintrag.ghgr.t  , 
                            fig.bar.intergr.ghgr.t, 
                                align = "h", 
                                nrow = 1, 
                                ncol = 2 , 
                                rel_widths = c(78/100, 22/100))
  
  ggsave("fig.yd.jpeg",   fig.yd    , path = "Figures.out", width=800, height=400, units="px", scale=2.5)
  
  ggsave("fig.vop.jpeg",  fig.vop   , path = "Figures.out", width=1350, height=510, units="px", scale=2.5)
  
  ggsave("fig.ghg.jpeg",  fig.ghg  , path = "Figures.out", width=1350, height=510, units="px", scale=2.5)
  
  
  
    
}
interg.figs()


fig.ghg
fig.vop


fig.bar.intergr.ghgr.t
fig.bar.interg.vop

fig.interg.yg.b 
fig.interg.yd.act.b 

# C accounting




# Table outputs

tb.typ.agfo <- function(){
  
  typologies <- c(
                'Hybrid sun',
                'Hybrid shade',
                'Amazonia'
                 )
  
  agfor.dat <- data.frame()
  agfor.dat <- 0
  
  for (T in typologies){
  
  v1 <- 'tree.count.per.ha'  
  v2 <- 'shade.tree.to.cm.tree.ratio'
  v3 <- 'num_other_root_grain_crops'
  v4 <- 'num_other_tree_crops'
  v5 <- 'tree.config.cocoa'
  v6 <- 'tree.config.cocoa'
  v7 <- 'tree.config.cocoa'
  v8 <- 'plot.quant.trees.per.ha.greater.than.20.m'
  v9 <- 'plot.quant.trees.per.ha.greater.than.35.m'
  v10 <- 'plot.quant.trees.per.ha.greater.than.50.m'
  

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
  
  comp$typ <- as.numeric((comp$typ))
  
  insert <<- " \u00B1 "
  

  for ( t in 1:(length(ordered.typ))){
    
    tp <- t
    
    ids <- comp[!is.na(comp$typ) & comp$typ == tp & (comp$typology == T) , 'hhID'] 
    
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
    v2.m <- 100*mean( na.omit(comp[(comp$hhID %in% ids ), v2] ))
    v2.sd <- 100*sd( na.omit(comp[(comp$hhID %in% ids ) , v2] ))
    
    v2.m <- round( v2.m , 0)
    v2.sd <- round( v2.sd , 0) 
    
    v2.d <- str_c(v2.m , insert , v2.sd )
    if (length(ids) == 1) {v2.d <- v2.m  }
    v2.dat <- append(v2.dat ,  v2.d )
    
    # Variable 3
    v3.m <- mean( na.omit(comp[(comp$hhID %in% ids ), v3] ))
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
    v5.m <-  100 *nrow(comp[(comp$hhID %in% ids ) & comp$tree.config.cocoa == 1, ]) / nrow(comp[(comp$hhID %in% ids ) , ])

    v5.m <- round( v5.m , 0)

    v5.dat <- append(v5.dat ,  v5.m )

    # Variable 6
    v6.m <- 100 * nrow(comp[(comp$hhID %in% ids ) & comp$tree.config.cocoa == 2, ]) / nrow(comp[(comp$hhID %in% ids ) , ])
    
    v6.m <- round( v6.m , 0)
    
    v6.dat <- append(v6.dat ,  v6.m )
    
    # Variable 7
    v7.m <- 100 * nrow(comp[(comp$hhID %in% ids ) & comp$tree.config.cocoa == 3, ]) / nrow(comp[(comp$hhID %in% ids ) , ])
    
    v7.m <- round( v7.m , 0)
    
    v7.dat <- append(v7.dat ,  v7.m )
    
    # Variable 8
    v8.m <- mean( na.omit(comp[(comp$hhID %in% ids ), v8] ))
    v8.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v8] ))
    
    v8.m <- round( v8.m , 1)
    v8.sd <- round( v8.sd , 1) 
    
    v8.d <- str_c(v8.m , insert , v8.sd )
    if (length(ids) == 1) {v8.d <- v8.m  }
    v8.dat <- append(v8.dat ,  v8.d )
    
    # Variable 9
    v9.m <- mean( na.omit(comp[(comp$hhID %in% ids ), v9] ))
    v9.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v9] ))
    
    v9.m <- round( v9.m , 1)
    v9.sd <- round( v9.sd , 1) 
    
    v9.d <- str_c(v9.m , insert , v9.sd )
    if (length(ids) == 1) {v9.d <- v9.m  }
    v9.dat <- append(v9.dat ,  v9.d )
    
    # Variable 10
    v10.m <- mean( na.omit(comp[(comp$hhID %in% ids ), v10] ))
    v10.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v10] ))
    
    v10.m <- round( v10.m , 1)
    v10.sd <- round( v10.sd , 1) 
    
    v10.d <- str_c(v10.m , insert , v10.sd )
    if (length(ids) == 1) {v10.d <- v10.m  }
    v10.dat <- append(v10.dat ,  v10.d )
    
    
  }
  
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
  
  typ.dat <- cbind(
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
    v10.dat
  )
  

  
  agfor.dat <- rbind(  agfor.dat,typ.dat)
  #  View(agfor.dat)
  
  

  }
  
#  agfor.dat <-  na.omit(agfor.dat[])
  agfor.dat <- agfor.dat[-c(1),]

  colnames(agfor.dat) = c('n',
                   'Cocoa tree density (per ha)',
                    'Shade to cocoa trees (%)',   
                   '# root or grain crops', 
                   '# tree crops',
                    'Random',
                    'Zoned',
                    'Lined',
                    '# trees > 20 m high',
                    '# trees > 35 m high',
                    '# trees > 50 m high'
  )
  

  path_out = '.\\Figures.out\\'
  fileName = paste(path_out, 'table_typology.agfo.csv',sep = '')
  write.csv(  agfor.dat  ,  file = fileName)  # Export
  #  View(agfor.dat)
  

  agfor.dat <<- agfor.dat  
}
tb.typ.agfo() 


tb.typ.pract <- function(){
  
  typologies <- c( 'Hybrid sun',
    'Amazonia',
    'Hybrid shade'
  )
  
  pract.dat <- data.frame()
  pract.dat <- 0
  
  for (T in typologies){
    
    
    v1 <- 'total_N_fert_applied_kg_per_ha'
    v2 <- 'total_non_N_fert_applied_kg_per_ha'
    v3 <- 'prunes_per_year'
    v4 <- 'weeds_per_year'
    v5 <- 'pollination.bool'
    v6 <- 'insecticides.bool'
    v7 <- 'fungicides.bool'
    v8 <- 'herbicides.bool'
    v9 <- 'path.contr.cc.fung.org'
    v10 <- 'path.contr.cc.bact.org'
    
    v1.dat <- data.frame()
    v2.dat <- data.frame()
    v3.dat <- data.frame()
    v4.dat <- data.frame()
    v5.dat <- data.frame()
    v6.dat <- data.frame()
    v7.dat <- data.frame()
    v8.dat <- data.frame()
    
    v1.dat <- 0
    v2.dat <- 0 
    v3.dat <- 0 
    v4.dat <- 0 
    v5.dat <- 0 
    v6.dat <- 0
    v7.dat <- 0 
    v8.dat <- 0 
    
    
    comp$typ <- as.numeric((comp$typ))
    
    insert <<- ' \u00B1 '
    
    for ( t in 1:(length(ordered.typ))){
      
      tp <- t
      
      ids <- comp[!is.na(comp$typ) & comp$typ == tp & (comp$typology == T) , 'hhID'] 

      # Variable 1
      v1.m <- mean( na.omit(comp[(comp$hhID %in% ids ) , v1] ))
      v1.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v1] ))
      
      v1.m <- round( v1.m , 1)
      v1.sd <- round( v1.sd , 1) 
      
      v1.d <- str_c(v1.m , insert , v1.sd )
      if (length(ids) == 1) {v1.d <- v1.m  }
      v1.dat <- append(v1.dat ,  v1.d )
      
      
      # Variable 2
      v2.m <- mean( na.omit(comp[(comp$hhID %in% ids ), v2] ))
      v2.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v2] ))
      
      v2.m <- round( v2.m , 1)
      v2.sd <- round( v2.sd , 1) 
      
      v2.d <- str_c(v2.m , insert , v2.sd )
      if (length(ids) == 1) {v2.d <- v2.m  }
      v2.dat <- append(v2.dat ,  v2.d )
      
      
      # Variable 3
      v3.m <- mean( na.omit(comp[(comp$hhID %in% ids ) , v3] ))
      v3.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v3] ))
      
      v3.m <- round( v3.m , 2)
      v3.sd <- round( v3.sd , 2) 
      
      v3.d <- str_c(v3.m , insert , v3.sd )
      if (length(ids) == 1) {v3.d <- v3.m  }
      v3.dat <- append(v3.dat ,  v3.d )
      
      # Variable 4
      v4.m <- mean( na.omit(comp[(comp$hhID %in% ids ) , v4] ))
      v4.sd <- sd( na.omit(comp[(comp$hhID %in% ids ) , v4] ))
      
      v4.m <- round( v4.m , 2)
      v4.sd <- round( v4.sd , 2) 
      
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

 
    }
    
    v1.dat <-  v1.dat[ -c(1)]
    v2.dat <-  v2.dat[ -c(1)]
    v3.dat <-  v3.dat[ -c(1)]
    v4.dat <-  v4.dat[ -c(1)]
    v5.dat <-  v5.dat[ -c(1)]
    v6.dat <-  v6.dat[ -c(1)]
    v7.dat <-  v7.dat[ -c(1)]
    v8.dat <-  v8.dat[ -c(1)]
    
    typ.dat <- cbind(
     # v0.dat,
      v1.dat,
      v2.dat,
      v3.dat,
      v4.dat,
      v5.dat,
      v6.dat,
      v7.dat,
      v8.dat
    )
    
    
    
    pract.dat <- rbind(pract.dat,typ.dat)
    
    #  View(pract.dat)
    
    
    
  }
  
  pract.dat <- na.omit(pract.dat)
  pract.dat <- pract.dat[-c(1),]
  
  
  
  colnames(pract.dat) = c(
    #'n',
                        'N-fert (kg/ha/yr)',
                        'Non N-fert (kg/ha/yr)',
                        'Pruning (#/yr)',
                        'Weeding (#/yr)',
                        'Pollination (dummy)',
                        'Insecticide (dummy)',
                        'Fungicides (dummy)',
                        'Herbicides (dummy)'
                     #   'Organic fungal control (dummy)',
                      #  'Organic bacterial control (dummy)'
  )
  
  #pract.dat <<-  na.omit(pract.dat[])

  path_out = '.\\Figures.out\\'
  fileName = paste(path_out, 'table_typology.pract.csv',sep = '')
  write.csv(  pract.dat  ,   file = fileName)  # Export
  
}
tb.typ.pract() 


tb.typ.clim <- function(){
  
  typologies <- c(
    'Hybrid sun',
       'Amazonia',
    'Hybrid shade'
  )
  
  cli.dat <- data.frame()
  cli.dat <- 0
  
  for (T in typologies){
    
    
   
    v1 <- 'precip'
    v2 <- 'temp.mmm'
    v3 <- 'elev'
    #v3 <- 'cv_precip'
    
    
  

    v0.dat <- data.frame()
    v1.dat <- data.frame()
    v2.dat <- data.frame()
    v3.dat <- data.frame()
    v4.dat <- data.frame()
    v5.dat <- data.frame()

    
    v0.dat <- 0
    v1.dat <- 0
    v2.dat <- 0 
    v3.dat <- 0 
    v4.dat <- 0 
    v5.dat <- 0 

    
    
    comp$typ <- as.numeric((comp$typ))
    
    insert <<- " \u00B1 "
    
    for ( t in 1:(length(ordered.typ))){
      
      tp <- t
      
      ids <- comp[!is.na(comp$typ) & comp$typ == tp & (comp$typology == T) , 'hhID'] 
      
      # Variable 0
      v0 <- nrow( comp[(comp$hhID %in% ids ) , ] )
      
      v0.dat <- append(v0.dat ,  v0 )
      
      # Variable 1
      v1.m <- mean( na.omit(T.df[(T.df$hhID %in% ids ) , v1] ))
      v1.sd <- sd( na.omit(T.df[(T.df$hhID %in% ids ) , v1] ))
      
      v1.m <- round( v1.m , 2)
      v1.sd <- round( v1.sd , 2) 
      
      v1.d <- str_c(v1.m , insert , v1.sd )
      v1.dat <- append(v1.dat ,  v1.d )
      
      # Variable 2
      v2.m <- mean( na.omit(T.df[(T.df$hhID %in% ids ), v2] ))
      v2.sd <- sd( na.omit(T.df[(T.df$hhID %in% ids ) , v2] ))
      
      v2.m <- round( v2.m , 2)
      v2.sd <- round( v2.sd , 2) 
      
      v2.d <- str_c(v2.m , insert , v2.sd )
      v2.dat <- append(v2.dat ,  v2.d )
      
      # Variable 3
      v3.m <- mean( na.omit(T.df[(T.df$hhID %in% ids ) , v3] ))
      v3.sd <- sd( na.omit(T.df[(T.df$hhID %in% ids ) , v3] ))
      
      v3.m <- round( v3.m , 2)
      v3.sd <- round( v3.sd , 2) 
      
      v3.d <- str_c(v3.m , insert , v3.sd )
      v3.dat <- append(v3.dat ,  v3.d )
      
      # Variable 4
      v4.m <- mean( na.omit(T.df[(T.df$hhID %in% ids ) , v4] ))
      v4.sd <- sd( na.omit(T.df[(T.df$hhID %in% ids ) , v4] ))
      
      v4.m <- round( v4.m , 2)
      v4.sd <- round( v4.sd , 2) 
      
      v4.d <- str_c(v4.m , insert , v4.sd )
      v4.dat <- append(v4.dat ,  v4.d )
      

      
    }
    
    v1.dat <-  v1.dat[ -c(1)]
    v2.dat <-  v2.dat[ -c(1)]
    v3.dat <-  v3.dat[ -c(1)]
    v4.dat <-  v4.dat[ -c(1)]
    
    dat <- cbind(
      v1.dat,
      v2.dat,
      v4.dat
    )
    
    cli.dat <- rbind(cli.dat,dat)
    
  
  }
  
  cli.dat <- cli.dat[-c(1),]
  cli.dat <- na.omit(cli.dat)
  
  
  colnames(cli.dat) = c('Rainfall',
                        'Max temp',
                        'Altitude')
  
  
  path_out = '.\\Figures.out\\'
  fileName = paste(path_out, 'table_typology_clim.csv',sep = '')
  write.csv(  cli.dat  ,   file =   fileName )  # Export
  #View(cli.dat)
  
}
tb.typ.clim() 




# ~ ~ ADDITIONAL TYPOLOGY SUMMARY FIGURES
# ~ Biomass figure

biom.tree.bio <- function(){

# TREE BIOMASS FIGURES
#organics.tree.all <- data.frame()

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

organics.tree.all 
#colnames(organics.tree)

organics.tree.all$total.biomass.Mg.per.tree <- as.numeric(organics.tree.all$total.biomass.Mg.per.tree)
organics.tree.all$total.biomass.Mg.per.tree.sd.frac <- as.numeric(organics.tree.all$total.volume.m3.sd.frac)

organics.tree.all$total.biomass.Mg.per.tree.sd.abs <- (organics.tree.all$total.biomass.Mg.per.tree.sd.frac * organics.tree.all$total.biomass.Mg.per.tree)

tree.type.order <- c('Evergreen' , 'Deciduous')


organics.tree.all <- organics.tree.all[ order(organics.tree.all$total.biomass.Mg.per.tree),]

organics.tree.all[,'sci.name'] <-  factor(organics.tree.all[,'sci.name'] , levels = organics.tree.all[,'sci.name'])
organics.tree.all[,'Tree.type'] <- factor(organics.tree.all[,'Tree.type'] , levels = tree.type.order)

  
  
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
 # coord_flip() +
  geom_errorbar(aes(ymin = total.biomass.Mg.per.tree - (total.biomass.Mg.per.tree.sd.abs  ), ymax = total.biomass.Mg.per.tree +  total.biomass.Mg.per.tree.sd.abs , x = sci.name, group = sci.name) , width=  tree.sizes.error.bar.width , size =   tree.sizes.error.bar.size ,  position=position_dodge()
                ,color = tree.sizes.error.bar.color) +
  #scale_y_reverse(limits = c(100, 0))+
  ylab(bquote('Mature tree biomass (Mg  '*tree^-1*') ')) +
  facet_wrap( Tree.type  ~ ., ncol = 2, nrow = 1 , scales = 'free_x' )   +
  force_panelsizes(cols = c(.8, 1)) +
  scale_y_continuous(breaks=seq(0,60,10) , labels = scales::number_format(accuracy = 1.0))+
  # labels = scales::number_format(accuracy = 1.0))+
  scale_x_discrete(limits = rev(levels('sci.name')))+
  theme(
    plot.margin = unit(c(  fig.tree.mass.p.mg.top ,   
                           fig.tree.mass.p.mg.right,
                           fig.tree.mass.p.mg.bottom*0 -.4,  
                           fig.tree.mass.p.mg.left), "cm"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 270.0, vjust = 0.5, hjust=0.5, size = tree.sizes.y.ticks.fs , face = 'italic'),
   # axis.text.y = element_text( vjust = 0.5, hjust = 1, size =   tree.sizes.x.tick.fs , face = 'italic'),
  #  axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = tree.sizes.y.ticks.fs ),
   #    axis.title.y = element_text(vjust = 0.5, hjust=0.5, size = tree.sizes.y.tit.fs ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_text(size =   tree.sizes.y.tit.fs),
    strip.background = element_rect(color='black', fill='white', size=1.0, linetype="solid"),
    #   axis.text.x = element_blank(),
     strip.text.x = element_text(size =  tree.fig.strip.fs, color = 'black'),
    #  strip.text.x = element_blank(),
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
box.plot.fill.color <<- '#e5e4e2'


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
    axis.text.x = element_text(angle = x.tick.angle , vjust = 0.5, hjust=0.5, size = fig.co2r.x.axis.fs , face = "italic"),
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


# Export all typology data as one excel file
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

}  # END TYPOLOGY CODE
run.typology()



# Type comparisons
fig.intrag.yg
fig.intrag.yd.act


fig.barintrag.ghgr.t   # 810, 320
fig.bar.intrag.vop

fig.yd 

fig.ghg  # ideal dimensions 860, 390
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


#View(T.df)

# Summary stats by typology and typ
summary(  comp[comp$typ == 1 & comp$typology == current.typology & (comp$yld_pc_attain_plot != Inf) , 'total_N_fert_applied_Mg_per_ha'])
summary(  comp[comp$typ == 2 & comp$typology == current.typology & (comp$yld_pc_attain_plot != Inf) , 'total_N_fert_applied_Mg_per_ha'])
summary(  comp[comp$typ == 3 & comp$typology == current.typology & (comp$yld_pc_attain_plot != Inf) , 'total_N_fert_applied_Mg_per_ha'])
summary(  comp[comp$typ == 4 & comp$typology == current.typology & (comp$yld_pc_attain_plot != Inf) , 'total_N_fert_applied_Mg_per_ha'])
summary(  comp[comp$typ == 5 & comp$typology == current.typology & (comp$yld_pc_attain_plot != Inf) , 'total_N_fert_applied_Mg_per_ha'])





nrow(kmns.data)

View(kmns.data)

cor(na.omit(kmns.data$cc_yield_kg_per_ha) , na.omit(kmns.data$total_fert_Nitr_applied_kg_per_ha))
cor(na.omit(kmns.data$cc_yield_kg_per_ha) , na.omit(kmns.data$total_org_N_applied_kg_per_ha))
cor(na.omit(kmns.data$cc_yield_kg_per_ha) , na.omit(kmns.data$total_N_applied_kg_per_ha))


cor(farmers$cc_yield_kg_per_ha , farmers$total_fert_Nitr_applied_kg_per_ha, method = 'spearman', use="complete.obs" )
cor(farmers$cc_yield_kg_per_ha , farmers$total_org_N_applied_kg_per_ha, method = 'spearman', use="complete.obs" )
cor(farmers$cc_yield_kg_per_ha , farmers$total_N_applied_kg_per_ha, method = 'spearman', use="complete.obs" )



plot(  y = kmns.data$cc_yield_kg_per_ha , x = kmns.data$total_fert_Nitr_applied_kg_per_ha )
abline(lm(kmns.data$cc_yield_kg_per_ha  ~ kmns.data$total_fert_Nitr_applied_kg_per_ha ), col = "red")



plot(  y = kmns.data$cc_yield_kg_per_ha , x = kmns.data$total_org_N_applied_kg_per_ha )
abline(lm( kmns.data$cc_yield_kg_per_ha  ~ kmns.data$total_org_N_applied_kg_per_ha   ), col = "red")

plot(  y = kmns.data$cc_yield_kg_per_ha ,
       x = kmns.data$total_N_applied_kg_per_ha ,
       xlab = 'Total N applied (kg/ha)',
       ylab = 'Yield (kg/ha)')


abline(lm(kmns.data$cc_yield_kg_per_ha  ~ kmns.data$total_N_applied_kg_per_ha   ), col = "red")









