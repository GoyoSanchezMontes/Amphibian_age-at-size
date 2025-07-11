# 1) RUN THE GENERAL MODELS FOR EACH SPECIES

library(nlme)

rm(list = ls())

# Import the dataset, the parameters for each species and the models to test

dataset<-read.csv("Growth_recaptures.csv", header=T)
params<-read.csv("specific_parameters.csv")

model_comparison <- data.frame(matrix(ncol = length(unique(unlist(strsplit(unique(params$models_to_test),split="+",fixed=TRUE))))+2, nrow = length(unique(dataset$species))))
colnames(model_comparison) <- c("Species",unique(unlist(strsplit(unique(params$models_to_test),split="+",fixed=TRUE))),"Bestmodel")
model_comparison$Species<-unique(dataset$species)

for (species in unique(dataset$species)) {
  sp <- species
  data<-subset(dataset,dataset$species==sp)

  # Remove the complete history records of all individuals with 
  # negative growth errors beyond specific threshold or with clear
  # positive mistakes, i.e., recorded growth over a specific threshold
  # during a maximum time span as set in the specific parameters file
   
  data<-data[!data$id %in% unique(data[data$dsvl<params[params$species==sp,]$neg_gr_thr,"id"]), ]
   
  if (sp %in% params[!is.na(params$max_dsvl_thr),"species"]) {
   data<-data[!data$id %in% unique(data[data$dsvl>params[params$species==sp,]$max_dsvl_thr & 
                                        data$d<params[params$species==sp,]$min_temp_thr,"id"]), ]
  }
   
  ####################   Fit the growth models  ################################
  
  rm(list = ls()[sapply(ls(), function(x) inherits(get(x), "nlme"))])

  growth<-groupedData(svl2~svl1|id,data=data)
  
  # Met_size = Size at metamorphosis (mm)
  # tmax = Maximum longevity (in days, better to extend than to be short)
  
  Met_size<-params[params$species==sp,]$Met_size
  tmax<-params[params$species==sp,]$tmax
  Linf_init<-params[params$species==sp,]$Linf_init
  k_init<-params[params$species==sp,]$k_init
  C_init<-params[params$species==sp,]$C_init
  ts_init<-params[params$species==sp,]$ts_init
  
  #######################VON BERTALANFFY#################################

  if ("VB" %in% unlist(strsplit(params[params$species==sp,]$models_to_test,split="+",fixed=T))) {
    VB<-nlme(svl2~Linf-(Linf-svl1)*exp(-k*d),
             fixed=list(Linf~ 1, k ~ 1),
             random=Linf~1,
             data=growth,
             start=c(Linf=Linf_init,k=k_init))
  }  
  
  #######################logistic#################################
  
  if ("Logistic" %in% unlist(strsplit(params[params$species==sp,]$models_to_test,split="+",fixed=T))) {
    Logistic<-nlme(svl2~Linf*svl1/(svl1+(Linf-svl1)*exp(-k*d)),
                   fixed=list(Linf~ 1, k ~ 1),
                   random=Linf~1,
                   data=growth,
                   start=c(Linf=Linf_init,k=k_init))
  }
  
  #######################SCHNUTE#################################
  
  # Initial variables
  
  growth$Met_size<-Met_size
  growth$tmax<-tmax

  if ("Schnute" %in% unlist(strsplit(params[params$species==sp,]$models_to_test,split="+",fixed=T))) {
    Schnute<-nlme(svl2~svl1*exp(-k*d)+(y2-Met_size*exp(-k*(tmax-0)))*(1-exp(-k*d))/(1-exp(-k*(tmax-0))),
                  fixed=list(y2 ~ 1, k ~ 1),
                  random=y2~1,
                  data=growth,
                  start=c(y2=Linf_init,k=k_init))
  }

  #################  Seasonal Oscillatory, Appledoorn, 1987  ##################
  
  if ("Sommers" %in% unlist(strsplit(params[params$species==sp,]$models_to_test,split="+",fixed=T))) {
      Sommers<-nlme(svl2~svl1+(Linf-svl1)*(1-exp(-k*d+(0.5*(C*k*sin((2*pi/365)*(t1-ts)))*pi^-1)-(0.5*(C*k*sin((2*pi/365)*(t2-ts)))*pi^-1))),
                    fixed = list(Linf ~ 1, k ~1, C~1, ts~1),
                    random=Linf~1,
                    data=growth,
                    start=c(Linf=Linf_init,k=k_init,C=C_init,ts=ts_init))
  }

  ###MODEL comparison
  comparison<-do.call(AIC, lapply(ls()[sapply(ls(), function(x) inherits(get(x), "nlme"))], as.symbol))

  for (model in rownames(comparison)) {
    model_comparison[model_comparison$Species==sp,model]<-comparison[model,]$AIC
    model_comparison[model_comparison$Species==sp,"Bestmodel"]<-rownames(comparison[comparison$AIC==min(comparison$AIC),])
  }  
    
}

write.csv(model_comparison, "Model_comparison.csv", row.names=F)




# 2) ADJUST THE PARAMETERS FOR THE SELECTED MODEL FOR EACH SPECIES

##########################  Model selection  ################################

allmodels<-read.csv2("models.csv")
model_selection_table<-data.frame(matrix(ncol = 3, nrow = 0))
colnames(model_selection_table) <- c("Species", "Model", "AIC")

# Create empty dataframes to store the parameters and coefficients
# of each model

model_param_table<-data.frame(matrix(ncol = 10, nrow = 0))
colnames(model_param_table) <- c("Species", "Model", "Parameter", "Value",
                                 "Std.Error", "DF", "t-value", "p-value",
                                 "CI_lower", "CI_upper")

model_coef_table<-data.frame(matrix(ncol = 2, nrow = 0))
colnames(model_coef_table) <- c("Species", "Model")

for (sp in model_comparison$Species) {

  # subset and filter data, deleting unrealistic cases (measurement errors) 
  # and categories with few cases in each species (e.g. juveniles or individuals
  # with unknown sex) 
  
  data<-subset(dataset,dataset$species==sp)
  data<-data[!data$id %in% unique(data[data$dsvl<params[params$species==sp,]$neg_gr_thr,"id"]), ]
    
  if (sp %in% params[!is.na(params$max_dsvl_thr),"species"]) {
   data<-data[!data$id %in% unique(data[data$dsvl>params[params$species==sp,]$max_dsvl_thr & 
                                        data$d<params[params$species==sp,]$min_temp_thr,"id"]), ]
  }
  
  if (params[params$species==sp,]$exclude_juvs=="yes") {
    data<-data[data$sex!="Juvenile", ]
  }

  if (params[params$species==sp,]$exclude_unknown_sex=="yes") {
    data<-data[data$sex!="Unknown", ]
  }

  # Prepare dataset for analysis and load specific parameters
  
  growth<-groupedData(svl2~svl1|id,data=data)

  Met_size<-params[params$species==sp,]$Met_size
  tmax<-params[params$species==sp,]$tmax
  Linf_init<-params[params$species==sp,]$Linf_init
  k_init<-params[params$species==sp,]$k_init
  C_init<-params[params$species==sp,]$C_init
  ts_init<-params[params$species==sp,]$ts_init  
  
  # Code the number of parameters for models according to additive,
  # interaction, sex, population or constant
  
  numsex<-length(unique(growth$sex))
  numpop<-length(unique(growth$pop))
  startparam_code<-data.frame(letter=c("a","i","s","p","c"), 
                              num=c(numsex-1+numpop-1,numsex-1+numpop-1+max(numsex,numpop)-1,numsex-1,numpop-1,0))

  # Build and fit candidate models
  
  bestmodel<-model_comparison[model_comparison$Species==sp,"Bestmodel"]
  candidate_models<-allmodels[grepl(bestmodel, allmodels$model_name),]

  # Remove models with interactions when any of the possible combinations
  # has no cases
  
  if (min(as.data.frame(table(growth$sex,growth$pop))$Freq)<5) {
    candidate_models<-candidate_models[!grepl("_.*i",candidate_models$model_name),]
  }
  
  # In species with only one sampled population, remove all
  # models including factor pop from the candidate models
    
  if (length(unique(growth$pop))<2) {
    candidate_models<-candidate_models[!(grepl("_.*p",candidate_models$model_name) |
                                         grepl("_.*i",candidate_models$model_name) |
                                         grepl("_.*a",candidate_models$model_name)),]
  } 
  
  for (testmodel in candidate_models$model_name) {
    
    fixed_fact<-as.vector(strsplit(candidate_models[candidate_models$model_name==testmodel,"fixed_fact"], split = ",")[[1]])
    start_param<-strsplit(testmodel,split="_")[[1]][2]
    skip_to_next <- FALSE
    
    if (bestmodel=="Sommers") {
        
      fixed_fact_form<-c(as.formula(fixed_fact[1]),as.formula(fixed_fact[2]),
                         as.formula(fixed_fact[3]),as.formula(fixed_fact[4]))
      
      tryCatch(model.n<-nlme(svl2~svl1+(Linf-svl1)*(1-exp(-k*d+(0.5*(C*k*sin((2*pi/365)*(t1-ts)))*pi^-1)-(0.5*(C*k*sin((2*pi/365)*(t2-ts)))*pi^-1))),
                    fixed = fixed_fact_form,
                    random=Linf~1,
                    data=growth,
                    start=c(Linf=Linf_init,rep(0,startparam_code[startparam_code$letter==substring(start_param,1,1),"num"]),
                            k=k_init,rep(0,startparam_code[startparam_code$letter==substring(start_param,2,2),"num"]),
                            C=C_init,rep(0,startparam_code[startparam_code$letter==substring(start_param,3,3),"num"]),
                            ts=ts_init,rep(0,startparam_code[startparam_code$letter==substring(start_param,4,4),"num"]))),
               error = function(e) { skip_to_next <<- TRUE})
      if(skip_to_next) {next}
      
    }

    else if (bestmodel=="Schnute") {
 
      growth$Met_size<-Met_size
      growth$tmax<-tmax
      fixed_fact_form<-c(as.formula(fixed_fact[1]),as.formula(fixed_fact[2]))

      model.n<-nlme(svl2~svl1*exp(-k*d)+(y2-Met_size*exp(-k*(tmax-0)))*(1-exp(-k*d))/(1-exp(-k*(tmax-0))),
                    fixed=fixed_fact_form,
                    random=y2~1,
                    data=growth,
                    start=c(y2=Linf_init,rep(0,startparam_code[startparam_code$letter==substring(start_param,1,1),"num"]),
                            k=k_init,rep(0,startparam_code[startparam_code$letter==substring(start_param,2,2),"num"])))

    }

    else if (bestmodel=="VB") {
      
      fixed_fact_form<-c(as.formula(fixed_fact[1]),as.formula(fixed_fact[2]))
      
      model.n<-nlme(svl2~Linf-(Linf-svl1)*exp(-k*d),
                    fixed=fixed_fact_form,
                    random=Linf~1,
                    data=growth,
                    start=c(Linf=Linf_init,rep(0,startparam_code[startparam_code$letter==substring(start_param,1,1),"num"]),
                            k=k_init,rep(0,startparam_code[startparam_code$letter==substring(start_param,2,2),"num"])))
      
    }

    else if (bestmodel=="Logistic") {
      
      fixed_fact_form<-c(as.formula(fixed_fact[1]),as.formula(fixed_fact[2]))
      
      model.n<-nlme(svl2~Linf*svl1/(svl1+(Linf-svl1)*exp(-k*d)),
                    fixed=fixed_fact_form,
                    random=Linf~1,
                    data=growth,
                    start=c(Linf=Linf_init,rep(0,startparam_code[startparam_code$letter==substring(start_param,1,1),"num"]),
                            k=k_init,rep(0,startparam_code[startparam_code$letter==substring(start_param,2,2),"num"])))
      
    }
    
    newmodel<-data.frame(matrix(ncol = 3, nrow = 1))
    colnames(newmodel) <- c("Species", "Model", "AIC")
    newmodel$Species<-sp
    newmodel$Model<-testmodel
    newmodel$AIC<-AIC(summary(model.n))
    model_selection_table<-rbind(model_selection_table,newmodel)
    
    model_sum<-summary(model.n)
    model_param<-as.data.frame(model_sum$tTable)
    model_param<-cbind(Species=sp, Model=testmodel, 
                       Parameter=row.names(model_param), model_param)
    CI<-intervals(model.n, which = "fixed")
    model_param$CI_lower<-as.data.frame(CI$fixed)$lower
    model_param$CI_upper<-as.data.frame(CI$fixed)$upper
    model_param_table<-rbind(model_param_table,model_param)

    model_coef<-coef(model.n)
    model_coef<-cbind(Species=sp, Model=testmodel,
                      id=rownames(model_coef),model_coef)
    model_coef_table<-merge(x=model_coef_table, y=model_coef, all=TRUE)
    
  }

}

write.csv(model_selection_table, "Model_selection_table.csv", row.names=F)
write.csv(model_param_table, "model_param_table.csv", row.names=F)
write.csv(model_coef_table, "model_coef_table.csv", row.names=F)



# 3) ESTIMATE GROWTH PARAMETERS FOR SAMPLED INDIVIDUALS ACCORDING TO THE
# SELECTED MODEL FOR EACH SPECIES 

for (sp in model_comparison$Species) {
  
  data<-subset(dataset,dataset$species==sp)

  selectedmodel<-model_selection_table[model_selection_table$Species==sp &
                                     model_selection_table$AIC==min(model_selection_table[model_selection_table$Species==sp,"AIC"]),"Model"]

  x<-model_coef_table[model_coef_table$Species==sp & model_coef_table$Model==selectedmodel,]
  x<-x[colSums(!is.na(x)) > 0]

  dat2<-merge(data, x)

  write.csv(dat2, file=paste(gsub(" ","_", sp),"_indiv_growth_param.csv",
                             sep = ""), row.names=F)

}
