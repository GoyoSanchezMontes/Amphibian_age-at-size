# This script builds upon files generated in previous analysis of growth 
# models based both on recapture data (recaptures_estimates.csv) and 
# individuals of known age (Knownage_estimates.csv),
# as well as a data file of SVL measurements of marked individuals across
# different years (captures_per_year.csv), including a single SVL
# measurement per year for each individual

# Also, it requires that the user has specified i) the preferred model to
# estimate size at age, either recaptures or knownage and ii) the reference
# population, i.e., the one that shows the most reliable estimates based on 
# previous analysis, in the specific parameters file

# NOTE: the plotting parameters of the graphs (colors, margins, axes, etc.)
# have been defined to suit the study species. The user may want to modify
# them to adjust to other species

library(scales)
library(ggplot2)
require(gridExtra)

rm(list = ls())

# Load the required files

recap_estimates<-read.csv("recaptures_estimates.csv")
Knownage_estimates<-read.csv("Knownage_estimates.csv")
captures<-read.csv("captures_per_year.csv")
param<-read.csv("specific_parameters.csv")

# Ensemble the data frame with the size estimates at 1-5 years for each species

agepred <- data.frame(
  Species = character(),
  Sex = character(),
  age_days = numeric(),
  SVL_pred = numeric(),
  SVL_pred_lower = numeric(),
  SVL_pred_upper = numeric(),
  stringsAsFactors = FALSE
)

# Ensemble the data frame with the asymptotic body size estimates each species

asympt_size <- data.frame(
  Species = rep(unique(param[param$modelpred!="","species"]),2),
  Sex = c(rep("Male",length(unique(param[param$modelpred!="","species"]))),
          rep("Female",length(unique(param[param$modelpred!="","species"])))),
  age_days = "Inf",
  SVL_pred = NA,
  SVL_pred_lower = NA,
  SVL_pred_upper = NA,
  stringsAsFactors = FALSE
)

for (sp in unique(param[param$modelpred!="","species"])) {
  
  if (param[param$species==sp,"modelpred"]=="recaptures") {
    
    agepred<-rbind(agepred,
                   recap_estimates[recap_estimates$Species==sp & recap_estimates$Pop==param[param$species==sp,"refpop"],
                    c("Species","Sex","age_days","SVL_pred",
                      "SVL_pred_lower","SVL_pred_upper")])

    for (s in c("Male", "Female")) {
      asympt_size[asympt_size$Species==sp & asympt_size$Sex==s,"SVL_pred"] <-
        round(unique(recap_estimates[recap_estimates$Species==sp & 
                                 (recap_estimates$Sex==s | recap_estimates$Sex=="Male/Female" | recap_estimates$Sex=="Pooled") &
                                 recap_estimates$Pop==param[param$species==sp,"refpop"],"Linf"]),0)
      asympt_size[asympt_size$Species==sp & asympt_size$Sex==s,"SVL_pred_lower"] <-
        round(unique(recap_estimates[recap_estimates$Species==sp & 
                                 (recap_estimates$Sex==s | recap_estimates$Sex=="Male/Female" | recap_estimates$Sex=="Pooled") &
                                 recap_estimates$Pop==param[param$species==sp,"refpop"],"Linf_lower"]),0)
      asympt_size[asympt_size$Species==sp & asympt_size$Sex==s,"SVL_pred_upper"] <-
        round(unique(recap_estimates[recap_estimates$Species==sp & 
                                 (recap_estimates$Sex==s | recap_estimates$Sex=="Male/Female" | recap_estimates$Sex=="Pooled") &
                                 recap_estimates$Pop==param[param$species==sp,"refpop"],"Linf_upper"]),0)
    } 
    if (length(unique(recap_estimates[recap_estimates$Species==sp,"Sex"]))==1){
      asympt_size[asympt_size$Species==sp,"Sex"]<-unique(recap_estimates[recap_estimates$Species==sp,"Sex"])
    }
  }
  
  else if (param[param$species==sp,"modelpred"]=="knownage") {
    
    agepred<-rbind(agepred,
                   Knownage_estimates[Knownage_estimates$Species==sp,
                       c("Species","Sex","age_days","SVL_pred",
                         "SVL_pred_lower","SVL_pred_upper")])
    
    for (s in c("Male", "Female")) {
      asympt_size[asympt_size$Species==sp & asympt_size$Sex==s,"SVL_pred"] <-
        round(unique(Knownage_estimates[Knownage_estimates$Species==sp & 
                                 (Knownage_estimates$Sex==s | Knownage_estimates$Sex=="Male/Female" | 
                                    Knownage_estimates$Sex=="Pooled"),"Linf"]),0)
      asympt_size[asympt_size$Species==sp & asympt_size$Sex==s,"SVL_pred_lower"] <-
        round(unique(Knownage_estimates[Knownage_estimates$Species==sp & 
                                 (Knownage_estimates$Sex==s | Knownage_estimates$Sex=="Male/Female" | 
                                    Knownage_estimates$Sex=="Pooled"),"Linf_lower"]),0)
      asympt_size[asympt_size$Species==sp & asympt_size$Sex==s,"SVL_pred_upper"] <-
        round(unique(Knownage_estimates[Knownage_estimates$Species==sp & 
                                 (Knownage_estimates$Sex==s | Knownage_estimates$Sex=="Male/Female" | 
                                    Knownage_estimates$Sex=="Pooled"),"Linf_upper"]),0)
    }
    if (length(unique(Knownage_estimates[Knownage_estimates$Species==sp,"Sex"]))==1){
      asympt_size[asympt_size$Species==sp,"Sex"]<-unique(Knownage_estimates[Knownage_estimates$Species==sp,"Sex"])
    }
  }
}

agepred$SVL_pred<-round(agepred$SVL_pred,0)
agepred[agepred$SVL_pred_lower<0,"SVL_pred_lower"]<-0
agepred$SVL_pred_lower<-round(agepred$SVL_pred_lower,0)
agepred$SVL_pred_upper<-round(agepred$SVL_pred_upper,0)
agepred$age_years<-round(agepred$age_days/365,0)
asympt_size$age_years<-"Asymp"
asympt_size<-unique(asympt_size)

asympt_size<-rbind(asympt_size,agepred)


## PLOT (FIG. 2): Age estimates for species

for (sp in unique(asympt_size$Species)) {
  
  assign(paste(gsub(" ", "_", sp),"_plot",sep=""), ggplot(asympt_size[asympt_size$Species==sp,]) +
           aes(x = as.factor(age_years), y = SVL_pred, group = Sex, colour = Sex) +
           geom_point(position=position_dodge(width=0.6), size=3) +
           geom_errorbar(aes(ymin = SVL_pred_lower,ymax = SVL_pred_upper), 
                         position = position_dodge(width=0.6), width=0.5) +
           labs(title=bquote(italic(.(sp))~"("~.(param[param$species==sp,"modelpred"])~")")) + 
           theme(legend.position = "none", axis.text=element_text(size=14),
                 axis.title=element_text(size=17,face="bold"),
                 plot.title = element_text(size = rel(2))) +
           xlab("Estimated age (years)") +
           ylab("SVL (mm)"))
  
}

png(filename="Age_estimates_species.png", width = 800, height = 1200)
grid.arrange(Alytes_cisternasii_plot,Alytes_obstetricans_plot,
             Bufo_spinosus_plot,Epidalea_calamita_plot,
             Hyla_molleri_plot, Pelobates_cultripes_plot,
             Pelophylax_perezi_plot, Pleurodeles_waltl_plot, ncol=2)
dev.off()

pdf("Age_estimates_species.pdf", width = 800, height = 1200)
grid.arrange(Alytes_cisternasii_plot,Alytes_obstetricans_plot,
             Bufo_spinosus_plot,Epidalea_calamita_plot,
             Hyla_molleri_plot, Pelobates_cultripes_plot,
             Pelophylax_perezi_plot, Pleurodeles_waltl_plot, ncol=2)
dev.off()

## PLOT (FIG. 3): Age estimates for individuals

agepred$min_SVL_threshold<-NA
agepred$max_SVL_threshold<-NA
for (sp in unique(agepred$Species)) {
  for (s in unique(agepred[agepred$Species==sp,"Sex"])){
    for (a in 1:4){
      agepred[agepred$Species==sp & agepred$Sex==s & agepred$age_years==a,
              "min_SVL_threshold"] <-
        max(agepred[agepred$Species==sp & agepred$Sex==s & agepred$age_years==a,"SVL_pred_lower"],
            ifelse(a!=1,agepred[agepred$Species==sp & agepred$Sex==s & agepred$age_years==a-1,"SVL_pred_upper"],
                   agepred[agepred$Species==sp & agepred$Sex==s & agepred$age_years==a,"SVL_pred_lower"]))
      agepred[agepred$Species==sp & agepred$Sex==s & agepred$age_years==a,
              "max_SVL_threshold"] <-
        min(agepred[agepred$Species==sp & agepred$Sex==s & agepred$age_years==a,"SVL_pred_upper"],
            agepred[agepred$Species==sp & agepred$Sex==s & agepred$age_years==a+1,"SVL_pred_lower"])
    }
    
  }
}

agepred<-agepred[agepred$age_years<5,]
captures$est_age_years<-NA
for (indiv in unique(captures$ID)) {
  skip_to_next <- FALSE
  tryCatch(captures[captures$ID==indiv & captures$years_from_mark==min(captures[captures$ID==indiv,"years_from_mark"]), "est_age_years"] <-
    agepred[agepred$Species==unique(captures[captures$ID==indiv,"Species"]) &
            (agepred$Sex==unique(captures[captures$ID==indiv,"Sex"]) |
             agepred$Sex=="Pooled" | agepred$Sex=="Male/Female")  &
            agepred$min_SVL_threshold <= captures[captures$ID==indiv & captures$years_from_mark==min(captures[captures$ID==indiv,"years_from_mark"]), "SVL_mm"] &
            agepred$max_SVL_threshold >= captures[captures$ID==indiv & captures$years_from_mark==min(captures[captures$ID==indiv,"years_from_mark"]), "SVL_mm"]  
            ,"age_years"], error = function(e) { skip_to_next <<- TRUE})
  if(skip_to_next) { next }
  else {
    captures[captures$ID==indiv, "est_age_years"] <- 
      captures[captures$ID==indiv & captures$years_from_mark==min(captures[captures$ID==indiv,"years_from_mark"]),"est_age_years"] + 
      captures[captures$ID==indiv,"years_from_mark"] - min(captures[captures$ID==indiv,"years_from_mark"])
  }
}

# Set here the species you want to plot
for (sp in c("Bufo spinosus", "Hyla molleri", "Pelophylax perezi")) {
  
  captures_sp <- captures[captures$Species==sp & !is.na(captures$est_age_years),]
  maxage<-max(captures_sp$est_age_years)
  captures_sp$est_age_years<-as.factor(captures_sp$est_age_years)
  
  if (nrow(captures_sp)>0) {
  
    males <- captures_sp[captures_sp$Sex=="Male",]
    males$est_age_years<-factor(males$est_age_years, levels=c(1:maxage))
    females <- captures_sp[captures_sp$Sex=="Female",]
    females$est_age_years<-factor(females$est_age_years, levels=c(1:maxage))

    assign(paste(gsub(" ", "_", sp),"_plot1",sep=""), ggplot(males) +
      aes(x = est_age_years, y = SVL_mm) +
      geom_violin(data = males[males$years_from_mark==0,], aes(y = SVL_mm),trim=FALSE) +
      geom_line(data = males, aes(y = SVL_mm, group = ID), color=alpha("steelblue",0.3)) +
      geom_jitter(data = males[males$years_from_mark==0,], aes(y = SVL_mm),size=3,
                  shape=16, position=position_jitter(0.05), color="steelblue") +
      geom_jitter(data = males[males$years_from_mark>0,], aes(y = SVL_mm),size=3,
                  shape=16, position=position_jitter(0.02), color=alpha("steelblue",0.3)) +
      theme(legend.position = "none", axis.text=element_text(size=14),
              axis.title=element_text(size=17,face="bold")) +
# Set the coordinates for the name of your species
      annotate("text", x=1.6, y=116, label= sp, fontface = 'italic',, size = 16/.pt) + 
# Set the desired y-scale limits (in our case, (60,120) for Bufo spinosus,
# (30,60) for Hyla molleri and (30,100) for Pelophylax perezi)
      ylim(60,120) +
      scale_x_discrete(drop=FALSE) +
      xlab("Estimated age (years)") +
      ylab("SVL (mm)"))

    assign(paste(gsub(" ", "_", sp),"_plot2",sep=""), ggplot(females) +
      aes(x = est_age_years, y = SVL_mm) +
      geom_violin(data = females[females$years_from_mark==0,], aes(y = SVL_mm),trim=FALSE) +
      geom_line(data = females, aes(y = SVL_mm, group = ID), color=alpha("firebrick",0.3)) +
      geom_jitter(data = females[females$years_from_mark==0,], aes(y = SVL_mm), size=3,
                  shape=16, position=position_jitter(0.05), color="firebrick") +
      geom_jitter(data = females[females$years_from_mark>0,], aes(y = SVL_mm), size=3,
                  shape=16, position=position_jitter(0.02), color=alpha("firebrick",0.3)) +
      theme(legend.position = "none", axis.text=element_text(size=14),
              axis.title=element_text(size=17,face="bold")) +
      # Set the desired y-scale limits (in our case, (60,120) for Bufo spinosus,
      # (30,60) for Hyla molleri and (30,100) for Pelophylax perezi)
      ylim(60,120) +
      scale_x_discrete(drop=FALSE) +
      xlab("Estimated age (years)") +
      ylab(""))
  }
}

png(filename="Age_estimates_individuals.png", width = 800, height = 900)
grid.arrange(Bufo_spinosus_plot1, Bufo_spinosus_plot2,
             Hyla_molleri_plot1, Hyla_molleri_plot2,
             Pelophylax_perezi_plot1, Pelophylax_perezi_plot2, ncol=2)
dev.off()


## PLOT (FIG. 4): Body size distributions per year

captures<-captures[order(captures$Species,captures$year),]

# Set here the species you want to plot
for (sp in c("Hyla molleri", "Pelophylax perezi")) {

  captures_sp <- captures[captures$Species==sp &
                          captures$years_from_mark==0,]
  minyear<-min(captures_sp$year)
  maxyear<-max(captures_sp$year)
  
  males <- captures_sp[captures_sp$Sex=="Male",]
  males$year<-factor(males$year, levels=c(minyear:maxyear))
  males[is.na(males$est_age_years),"est_age_years"]<--9
  females <- captures_sp[captures_sp$Sex=="Female",]
  females$year<-factor(females$year, levels=c(minyear:maxyear))
  females[is.na(females$est_age_years),"est_age_years"]<--9
  
  assign(paste(gsub(" ", "_", sp),"_size_distr_plot1",sep=""), ggplot(males) +
           aes(x = year, y = SVL_mm, group = year) +
           geom_jitter(data = males[males$est_age_years==-9,],
                       aes(x= year, y = SVL_mm), size=3,shape=16, 
                       position=position_jitter(0.1), color=alpha("steelblue",0.3), na.rm = TRUE) +
           geom_violin(data = males[males$est_age_years==1,], aes(x= year, y = SVL_mm),trim=TRUE) +
           geom_violin(data = males[males$est_age_years==2,], aes(x= year, y = SVL_mm),trim=TRUE) +
           geom_jitter(data = males[males$est_age_years==2,], aes(x= year, y = SVL_mm), size=3,shape=16, 
                       position=position_jitter(0.1), color="steelblue", na.rm = TRUE) +
           geom_jitter(data = males[males$est_age_years==1,], aes(x= year, y = SVL_mm), size=3,shape=16, 
                       position=position_jitter(0.1), color="steelblue1", na.rm = TRUE) +
           theme(legend.position = "none", axis.text=element_text(size=14),
                 axis.title=element_text(size=17,face="bold")) +
           scale_x_discrete(drop=FALSE) +
           # Set the y-axis limits for your species. H.molleri (32,60), P. perezi (40,100)
           ylim(40,100) +
           # Set the coordinates for the name of your species
           annotate("text", x=3, y=95, label= sp, fontface = 'italic',, size = 16/.pt) + 
           xlab("Year") +
           ylab("SVL (mm)"))
  
  assign(paste(gsub(" ", "_", sp),"_size_distr_plot2",sep=""), ggplot(females) +
           aes(x = year, y = SVL_mm, group = year) +
           geom_jitter(data = females[females$est_age_years==-9,],
                       aes(x= year, y = SVL_mm), size=3,shape=16, 
                       position=position_jitter(0.1), color=alpha("firebrick",0.3), na.rm = TRUE) +
           geom_violin(data = females[females$est_age_years==1,], aes(x= year, y = SVL_mm),trim=TRUE) +
           geom_violin(data = females[females$est_age_years==2,], aes(x= year, y = SVL_mm),trim=TRUE) +
           geom_jitter(data = females[females$est_age_years==2,], aes(x= year, y = SVL_mm), size=3,shape=16, 
                       position=position_jitter(0.1), color="firebrick", na.rm = TRUE) +
           geom_jitter(data = females[females$est_age_years==1,], aes(x= year, y = SVL_mm), size=3,shape=16, 
                       position=position_jitter(0.1), color="firebrick1", na.rm = TRUE) +
           theme(legend.position = "none", axis.text=element_text(size=14),
                 axis.title=element_text(size=17,face="bold")) +
           scale_x_discrete(drop=FALSE) +
           # Set the y-axis limits for your species. H.molleri (32,60), P. perezi (40,100)
           ylim(40,100) +
           xlab("Year") +
           ylab(""))
  
  
}

png(filename="Body_size_distribs.png", width = 1200, height = 800)
grid.arrange(Hyla_molleri_size_distr_plot1,Hyla_molleri_size_distr_plot2,
             Pelophylax_perezi_size_distr_plot1,Pelophylax_perezi_size_distr_plot2,
             ncol=2)
dev.off()

