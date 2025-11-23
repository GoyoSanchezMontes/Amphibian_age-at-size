# This script encodes visualization for the models built and selected
# for each species in the accompanying script 01_Growth_function_models.R,
# using the output files created therein

# NOTE: the plotting parameters of the graphs (colors, margins, axes, etc.)
# have been defined to suit the study species. The user may want to modify
# them to adjust to other species

library(scales)

rm(list = ls())

spec_params<-read.csv("specific_parameters.csv") 
model_parameters<-read.csv("model_param_table.csv")

allparam_table <- data.frame(matrix(ncol = 19, nrow = 0))
colnames(allparam_table) <- c("Species", "Pop", "Sex", "Model", "Linf",
                              "Linf_lower","Linf_upper", "k", "k_lower",
                              "k_upper","C", "C_lower", "C_upper", "ts",
                              "ts_lower","ts_upper","y2","y2_lower","y2_upper")

for (sp in spec_params$species) {
  
  selectedmodel_dataset<-read.csv(paste(gsub(" ","_", sp),"_indiv_growth_param.csv",sep=""))
  selectedmodel<-unique(selectedmodel_dataset$Model)
  sexes<-sort(unique(selectedmodel_dataset$sex))
  pops<-sort(unique(selectedmodel_dataset$pop))

  model_parameters_sp<-model_parameters[model_parameters$Species==sp & 
                                          model_parameters$Model==selectedmodel,]
  
  myparams<-rep(NA,length(model_parameters_sp$Parameter))

  for (i in 1:length(model_parameters_sp$Parameter)) {
    myparams[i]<-strsplit(model_parameters_sp$Parameter[i],split=".", fixed=TRUE)[[1]][1]
  }
  
  myparams<-unique(myparams)
  
  Pop <- NULL
  for (i in 1:length(pops)) {
    element<-c(rep(pops[i],length(sexes)))
    Pop <- append(Pop, element)   
  }
  
  param_table<-data.frame(Species=sp, Pop=Pop, Sex=rep(sexes,length(pops)),
                          Model=selectedmodel, Linf=NA, Linf_lower=NA, Linf_upper=NA,
                          k=NA, k_lower=NA, k_upper=NA, C=NA, C_lower=NA,
                          C_upper=NA, ts=NA, ts_lower=NA, ts_upper=NA, y2=NA, 
                          y2_lower=NA, y2_upper=NA)
  
  for (p in pops) {
    
    for (s in sexes) {
      
      for (para in myparams) {
        
        param_table[param_table$Pop==p & param_table$Sex==s, para] <-
          sum(model_parameters_sp[grepl(para,model_parameters_sp$Parameter) &
                                    !grepl(sexes[!sexes==s],model_parameters_sp$Parameter) &
                                    !grepl(paste(c(pops[!pops==p],"falsepop"),collapse="|"),model_parameters_sp$Parameter),"Value"])
        
        param_table[param_table$Pop==p & param_table$Sex==s, paste(para,"lower",sep="_")] <-
          sum(model_parameters_sp[grepl(para,model_parameters_sp$Parameter) &
                                    !grepl(sexes[!sexes==s],model_parameters_sp$Parameter) &
                                    !grepl(paste(c(pops[!pops==p],"falsepop"),collapse="|"),model_parameters_sp$Parameter),"CI_lower"])
        
        param_table[param_table$Pop==p & param_table$Sex==s, paste(para,"upper",sep="_")] <-
          sum(model_parameters_sp[grepl(para,model_parameters_sp$Parameter) &
                                    !grepl(sexes[!sexes==s],model_parameters_sp$Parameter) &
                                    !grepl(paste(c(pops[!pops==p],"falsepop"),collapse="|"),model_parameters_sp$Parameter),"CI_upper"])
        
      }
      
    }
    
  }
  
  allparam_table<-rbind(allparam_table,param_table)
  
  
}

write.csv(allparam_table, "allparam_table.csv", row.names=F)



graph_dataset_all <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(graph_dataset_all) <- c("Species", "Pop", "Sex", "Days", "st", "std",
                              "st_lower", "std_lower", "st_upper", "std_upper",
                              "L2", "L2_lower", "L2_upper", "Years")

indiv_observed_data<-data.frame(matrix(ncol = 17, nrow = 0))
colnames(indiv_observed_data) <- c("species","Model","pop","sex","id","date1",
                                   "date2","d","t1","t2","svl1","svl2",
                                   "Linf","k","st","std","age_est")

for (sp in unique(allparam_table$Species)) {
  
  param_table<-allparam_table[allparam_table$Species==sp,]

  selectedmodel<-unique(param_table$Model)
  sexes<-sort(unique(param_table$Sex))
  pops<-sort(unique(param_table$Pop))

  indiv_data<-read.csv(paste(gsub(" ","_", sp),"_indiv_growth_param.csv",sep=""))
  indiv_data$date1<-as.Date(indiv_data$date1,format="%d/%m/%Y")
  indiv_data$date2<-as.Date(indiv_data$date2,format="%d/%m/%Y")
  indiv_data$Linf<-NA
  indiv_data$k<-NA
  indiv_data$st<-NA
  indiv_data$std<-NA
  
  for (p in pops) {
    
    for (s in sexes) {
      
      param_table_pop_sex<-param_table[param_table$Pop==p & param_table$Sex==s,
                                       colSums(is.na(param_table))<nrow(param_table)]
      graph_dataset <- data.frame(matrix(ncol = 14, nrow = spec_params[spec_params$species==sp,"tmax"]+1))
      colnames(graph_dataset) <- colnames(graph_dataset_all)
      graph_dataset$Species<-sp
      graph_dataset$Pop<-p
      graph_dataset$Sex<-s
      graph_dataset$Days<-seq(0,spec_params[spec_params$species==sp,"tmax"])
      myparams<-colnames(param_table_pop_sex)[5:ncol(param_table_pop_sex)]

      Met_size<-spec_params[spec_params$species==sp,"Met_size"]
      tmax<-spec_params[spec_params$species==sp,"tmax"]
      for (para in myparams) {
        assign(para, param_table_pop_sex[,para])
      }
      
      L2<-rep(NA,nrow(graph_dataset))
      L2_lower<-rep(NA,nrow(graph_dataset))
      L2_upper<-rep(NA,nrow(graph_dataset))
      L1 <- Met_size
      L1_lower <- Met_size-1
      L1_upper <- Met_size+1

      if (grepl("Seasonal",selectedmodel)) {

        indiv_data_sex_pop<-indiv_data[indiv_data$pop==p & indiv_data$sex==s,
                                       c("species","Model","pop","sex","id",
                                         "date1","date2","d","t1","t2",
                                         "svl1","svl2",
                                         "Linf","k","st","std")]
        
        if (nrow(indiv_data_sex_pop)>0) {
          indiv_data_sex_pop$k<-k
          indiv_data_sex_pop$Linf<-param_table_pop_sex$Linf
          for (ind in unique(indiv_data_sex_pop$id)) {
            records<-indiv_data_sex_pop[indiv_data_sex_pop$id==ind,]
            records$age_est[1]<-
              ((-log((records$Linf[1]-records$svl1[1])/(records$Linf[1]-Met_size))+records$st[1]-records$std[1])/records$k[1])/365
            if (nrow(records) > 1) {
              for (row in 2:nrow(records)) {
                records$age_est[row]<-records$age_est[row-1]+(records$date1[row]-records$date1[row-1])/365
              }
            }
            indiv_observed_data<-rbind(indiv_observed_data,records)
          }
        }

        for (i in 1:length(L2)) {
          
          graph_dataset$st<-(C*k/(2*pi))*sin(2*pi/365*(graph_dataset$Days - ts))
          graph_dataset$std<-(C*k/(2*pi))*sin(2*pi/365*(graph_dataset$Days+1 - ts))
          graph_dataset$st_lower<-(C_lower*k_lower/(2*pi))*sin(2*pi/365*(graph_dataset$Days - ts_lower))
          graph_dataset$std_lower<-(C_lower*k_lower/(2*pi))*sin(2*pi/365*(graph_dataset$Days+1 - ts_lower))
          graph_dataset$st_upper<-(C_upper*k_upper/(2*pi))*sin(2*pi/365*(graph_dataset$Days - ts_upper))
          graph_dataset$std_upper<-(C_upper*k_upper/(2*pi))*sin(2*pi/365*(graph_dataset$Days+1 - ts_upper))
        
          val <- L1+(Linf-L1)*(1-exp(-k*1+graph_dataset$st[i]-graph_dataset$std[i]))
          L2[i]<-val
          L1<-val
          val_lower <- L1_lower+(Linf_lower-L1_lower)*(1-exp(-k_lower*1+graph_dataset$st_lower[i]-graph_dataset$std_lower[i]))
          L2_lower[i]<-val_lower
          L1_lower<-val_lower
          val_upper <- L1_upper+(Linf_upper-L1_upper)*(1-exp(-k_upper*1+graph_dataset$st_upper[i]-graph_dataset$std_upper[i]))
          L2_upper[i]<-val_upper
          L1_upper<-val_upper

        }
        
      }
        
      else if (grepl("Schnute",selectedmodel)) {
      
        indiv_data_sex_pop<-indiv_data[indiv_data$pop==p & indiv_data$sex==s,
                                       c("species","Model","pop","sex","id",
                                         "date1","date2","d","t1","t2",
                                         "svl1","svl2",
                                         "Linf","k","st","std")]
        
        if (nrow(indiv_data_sex_pop)>0) {
          indiv_data_sex_pop$k<-k
          indiv_data_sex_pop$Linf<-param_table_pop_sex$Linf
          for (ind in unique(indiv_data_sex_pop$id)) {
            records<-indiv_data_sex_pop[indiv_data_sex_pop$id==ind,]
            records$age_est[1]<-
              (-1/records$k[1])*log(1-(log(records$svl1[1])-log(Met_size))/(log(records$Linf[1])-log(Met_size))*(1-exp(1-records$k[1]*tmax)))/365
            if (nrow(records) > 1) {
              for (row in 2:nrow(records)) {
                records$age_est[row]<-records$age_est[row-1]+(records$date1[row]-records$date1[row-1])/365
              }
            }
            indiv_observed_data<-rbind(indiv_observed_data,records)
          }
        }

        for (i in 1:length(L2)) {
          
          T2 <- max(graph_dataset$Days)
          val <- L1+(y2-L1)*(1-exp(-k*graph_dataset$Days[i]))/(1-exp(-k*T2))
          L2[i]<-val
          L1<-val
          val_lower <- L1_lower+(y2_lower-L1_lower)*(1-exp(-k_lower*graph_dataset$Days[i]))/(1-exp(-k_lower*T2))
          L2_lower[i]<-val_lower
          L1_lower<-val_lower
          val_upper <- L1_upper+(y2_upper-L1_upper)*(1-exp(-k_upper*graph_dataset$Days[i]))/(1-exp(-k_upper*T2))
          L2_upper[i]<-val_upper
          L1_upper<-val_upper

        }
        
      }

      else if (grepl("Logistic",selectedmodel)) {
          
        indiv_data_sex_pop<-indiv_data[indiv_data$pop==p & indiv_data$sex==s,
                                       c("species","Model","pop","sex","id",
                                         "date1","date2","d","t1","t2",
                                         "svl1","svl2",
                                         "Linf","k","st","std")]

        if (nrow(indiv_data_sex_pop)>0) {
          indiv_data_sex_pop$k<-k
          indiv_data_sex_pop$Linf<-param_table_pop_sex$Linf
          for (ind in unique(indiv_data_sex_pop$id)) {
            records<-indiv_data_sex_pop[indiv_data_sex_pop$id==ind,]
            records$age_est[1]<-
              (-log(((records$Linf[1]*Met_size/records$svl1[1])-Met_size)/(records$Linf[1]-Met_size))/records$k[1])/365
            if (nrow(records) > 1) {
              for (row in 2:nrow(records)) {
                records$age_est[row]<-records$age_est[row-1]+(records$date1[row]-records$date1[row-1])/365
              }
            }
            indiv_observed_data<-rbind(indiv_observed_data,records)
          }
        }
        
        for (i in 1:length(L2)) {
          
          val <- Linf*L1/(L1+(Linf-L1)*exp(-k*1))
          L2[i]<-val
          L1<-val
          val_lower <- Linf_lower*L1_lower/(L1_lower+(Linf_lower-L1_lower)*exp(-k_lower*1))
          L2_lower[i]<-val_lower
          L1_lower<-val_lower
          val_upper <- Linf_upper*L1_upper/(L1_upper+(Linf_upper-L1_upper)*exp(-k_upper*1))
          L2_upper[i]<-val_upper
          L1_upper<-val_upper

          
        }

      }

      else if (grepl("VB",selectedmodel)) {
          
        indiv_data_sex_pop<-indiv_data[indiv_data$pop==p & indiv_data$sex==s,
                                       c("species","Model","pop","sex","id",
                                         "date1","date2","d","t1","t2",
                                         "svl1","svl2",
                                         "Linf","k","st","std")]
        
        if (nrow(indiv_data_sex_pop)>0) {
          indiv_data_sex_pop$k<-k
          indiv_data_sex_pop$Linf<-param_table_pop_sex$Linf
          for (ind in unique(indiv_data_sex_pop$id)) {
            records<-indiv_data_sex_pop[indiv_data_sex_pop$id==ind,]
            records$age_est[1]<-
              ((-log((records$Linf[1]-records$svl1[1])/(records$Linf[1]-Met_size)))/records$k[1])/365
            if (nrow(records) > 1) {
              for (row in 2:nrow(records)) {
                records$age_est[row]<-records$age_est[row-1]+(records$date1[row]-records$date1[row-1])/365
              }
            }
           indiv_observed_data<-rbind(indiv_observed_data,records)
          }
        }
        
        for (i in 1:length(L2)) {
          
          val <- Linf-(Linf-L1)*exp(-k*1)
          L2[i]<-val
          L1<-val
          val_lower <- Linf_lower-(Linf_lower-L1_lower)*exp(-k_lower*1)
          L2_lower[i]<-val_lower
          L1_lower<-val_lower
          val_upper <- Linf_upper-(Linf_upper-L1_upper)*exp(-k_upper*1)
          L2_upper[i]<-val_upper
          L1_upper<-val_upper
          
        }
        
      }
      
      graph_dataset$L2<-L2
      graph_dataset$L2_lower<-L2_lower
      graph_dataset$L2_upper<-L2_upper
      graph_dataset$Years<-graph_dataset$Days/365
      graph_dataset_all<-rbind(graph_dataset_all,graph_dataset)
      
    }
      
  }

}

write.csv(indiv_observed_data, "indiv_observed_data.csv", row.names=F)
write.csv(graph_dataset_all, "graph_dataset_all.csv", row.names=F)


spec_params<-read.csv("specific_parameters.csv")
age_certain<-read.csv("age_certain.csv")

for (sp in unique(graph_dataset_all$Species)) {

  graph_dataset_sp<-subset(graph_dataset_all,Species==sp)
  indiv_observed_sp<-subset(indiv_observed_data,species==sp)
  age_certain_sp<-subset(age_certain,Species==sp)
  
  for (p in unique(graph_dataset_sp$Pop)) {
    
    png(filename=paste(paste(gsub(" ", "_", sp),"_",sep=""),p,".png",sep=""),
         width = 800, height = 700,)
    
    par(mar = c(5, 5, 4, 2) + 0.1)

    graph_dataset_sp_pop<-subset(graph_dataset_sp,Pop==p)
    indiv_observed_sp_pop<-subset(indiv_observed_sp,pop==p)
    age_certain_sp_pop<-subset(age_certain_sp,Pop==p)
    
    males<-subset(graph_dataset_sp_pop, Sex=="Male")
    females<-subset(graph_dataset_sp_pop, Sex=="Female")
    observed_males<-subset(indiv_observed_sp_pop, sex=="Male")
    observed_females<-subset(indiv_observed_sp_pop, sex=="Female")
    age_certain_males<-subset(age_certain_sp_pop, Sex=="Male")
    age_certain_females<-subset(age_certain_sp_pop, Sex=="Female")
    age_certain_others<-subset(age_certain_sp_pop, Sex!="Male" & Sex!= "Female")
    
    
    plot(males$Years,males$L2, type="l", col="steelblue", lwd=5,
         xlim=c(0,spec_params[spec_params$species==sp,"max_size_age"]),ylim=c(min(graph_dataset_sp_pop$L2),max(graph_dataset_sp_pop$L2_upper)+5),
         bty="L", xlab="Age (years)", ylab="SVL (mm)", xaxt = "n", yaxt = "n",
         main=bquote(italic(.(sp))~.(gsub("_", " ", p))), cex.main=2.5, cex.lab=2)
    axis(1, lwd=1.3,cex.axis=1.5)
    axis(2, las=2, lwd=1.3,cex.axis=1.5)
    for (ind in observed_females$id) {
      lines(observed_females[observed_females$id==ind,]$age_est,observed_females[observed_females$id==ind,]$svl1,
             col=alpha("firebrick",0.1),cex=2,pch=16, lwd=2.5)
    }
    points(observed_females$age_est,observed_females$svl1, col=alpha("firebrick",0.4),cex=2,pch=16)
    for (ind in observed_males$id) {
      lines(observed_males[observed_males$id==ind,]$age_est,observed_males[observed_males$id==ind,]$svl1,
            col=alpha("steelblue",0.1),cex=2,pch=16, lwd=2.5)
    }
    points(observed_males$age_est,observed_males$svl1, col=alpha("steelblue",0.4),cex=2,pch=16)
    lines(males$Years,males$L2, type="l", col="steelblue", lwd=5)
    lines(females$Years,females$L2, type="l", col="firebrick", lwd=5)
    lines(males$Years,males$L2_lower, type="l", col="steelblue", lty=3, lwd=4)
    lines(males$Years,males$L2_upper, type="l", col="steelblue", lty=3, lwd=4)
    lines(females$Years,females$L2_lower, type="l", col="firebrick", lty=3, lwd=4)
    lines(females$Years,females$L2_upper, type="l", col="firebrick", lty=3, lwd=4)
    points(age_certain_others$age,age_certain_others$SVL, col="darkgrey",cex=2,pch=17)
    points(age_certain_females$age,age_certain_females$SVL, col="firebrick1",cex=2,pch=17)
    points(age_certain_males$age,age_certain_males$SVL, col="blue",cex=2,pch=17)
    legend(x="bottomright", legend=c(paste(unique(graph_dataset_sp$Sex),"s",sep=""),"Juveniles/Unknown sex"),
           col=c("firebrick","steelblue","darkgrey"),pch=16, bty="n",cex=2)
    
    dev.off()
    
  }
    
}


####################  Table of size predictions at age  #################################

graph_dataset_all<-read.csv("graph_dataset_all.csv")
allparam_table<-read.csv("allparam_table.csv")
allparam_subset<-allparam_table[c("Species","Pop","Sex","Linf","Linf_lower",
                                  "Linf_upper","k","k_lower","k_upper")]

# We set 10 months as the time lapse until the onset of the first breeding 
# season after the metamorphosis of each individual. Whether the individual
# will have reached maturity or not by that time, we obtain the body size (SVL)
# predicted by the best model for that species at 10 months (c. 300 days), 
# so right after the first winter of the individual

# Similarly we obtain the body size predicted at the time of the second 
# breeding season after metamorphosis: 21 months = 665 days, and
# the same for the 3rd (1030 days), 4th (1395) and 5th (1760) breeding
# seasons

recap_estimates<- graph_dataset_all[graph_dataset_all$Days %in% c(300,665,1030,1395,1760),
                                    c("Species","Pop","Sex","Days","L2","L2_lower","L2_upper")]
colnames(recap_estimates)[colnames(recap_estimates) == 'Days'] <- 'age_days'
colnames(recap_estimates)[colnames(recap_estimates) == 'L2'] <- 'SVL_pred'
colnames(recap_estimates)[colnames(recap_estimates) == 'L2_lower'] <- 'SVL_pred_lower'
colnames(recap_estimates)[colnames(recap_estimates) == 'L2_upper'] <- 'SVL_pred_upper'
recap_estimates<-merge(recap_estimates,allparam_subset, by=c("Species","Pop","Sex"))

for (sp in unique(recap_estimates$Species)) {
  males<-recap_estimates[recap_estimates$Species==sp & recap_estimates$Sex=="Male",c(4:ncol(recap_estimates))]
  rownames(males) <- NULL
  females<-recap_estimates[recap_estimates$Species==sp & recap_estimates$Sex=="Female",c(4:ncol(recap_estimates))]
  rownames(females) <- NULL
  if (identical(males,females)) {
    recap_estimates<-recap_estimates[!(recap_estimates$Species==sp & recap_estimates$Sex=="Male"),]
    recap_estimates[recap_estimates$Species==sp, "Sex"] <- "Male/Female"
  }
}

write.csv(recap_estimates, "recaptures_estimates.csv", row.names=F)
