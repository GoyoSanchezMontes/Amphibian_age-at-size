# This script builds upon files generated in previous analysis of growth 
# models based both on recapture data (recaptures_estimates.csv) and 
# individuals of known age (Knownage_estimates.csv),
# as well as a data file of SVL measurements of marked individuals across
# different years (captures_per_year.csv)

# Also, it requires that the user has specified i) the preferred model to
# estimate size at age, either recaptures or knownage and ii) the reference
# population, i.e., the one that shows the most reliable estimates based on 
# previous analysis, in the specific parameters file

# NOTE: the plotting parameters of the graphs (colors, margins, axes, etc.)
# have been defined to suit the study species. The user may want to modify
# them to adjust to other species

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
  stringsAsFactors = FALSE
)

for (sp in unique(param[param$modelpred!="","species"])) {
  
  if (param[param$species==sp,"modelpred"]=="recaptures") {
    
    agepred<-rbind(agepred,
                   recap_estimates[recap_estimates$Species==sp & recap_estimates$Pop==param[param$species==sp,"refpop"],
                    c("Species","Sex","age_days","SVL_pred")])
    
  }
  
  else if (param[param$species==sp,"modelpred"]=="knownage") {
    
    agepred<-rbind(agepred,
                   Knownage_estimates[Knownage_estimates$Species==sp,
                       c("Species","Sex","age_days","SVL_pred")])
    
  }
  
}

agepred$lwd<-1
agepred[agepred$age_days %in% c(300,665),"lwd"]<-3
agepred[agepred$Sex=="males","Sex"]<-"Male"
agepred[agepred$Sex=="females","Sex"]<-"Female"

for (sp in unique(agepred$Species)) {
  
  captures_sp <- captures[captures$Species==sp,]
  
  for (p in unique(captures_sp$Pop)) {
    
    captures_sp_pop <- captures_sp[captures_sp$Pop==p,]
    males <- captures_sp_pop[captures_sp_pop$Sex=="Male",]
    females <- captures_sp_pop[captures_sp_pop$Sex=="Female",]
    unknown <- captures_sp_pop[captures_sp_pop$Sex=="Unknown",]

    png(filename=paste(paste("Years_from_mark_",gsub(" ", "_", sp),"_",sep=""),p,".png",sep=""),
        width = 800, height = 700)

    par(mar = c(5, 6, 4, 2) + 0.1)
    plot(males$years_from_mark-0.014*max(captures_sp$years_from_mark),
         males$SVL_mm, type="p", col="steelblue", cex=2,bty="L", 
         xlab="Years from marking", ylab="SVL (mm)", xaxt = "n", yaxt = "n",
         main=bquote(italic(.(sp))~.(gsub("_"," ",p))), cex.main=2.5, 
         cex.lab=2,pch=16,
         xlim=c(-0.5,max(captures_sp$years_from_mark)+0.5),
         ylim=c(25,max(captures_sp$SVL_mm,na.rm=T)+5),
    )
    axis(1, lwd=1.3,cex.axis=1.5)
    axis(2, las=2, lwd=1.3,cex.axis=1.5)
    points(females$years_from_mark+0.014*max(captures_sp$years_from_mark),
           females$SVL_mm,col="firebrick",cex=2,pch=16)
    points(unknown$years_from_mark+0.045*max(captures_sp$years_from_mark),
           unknown$SVL_mm,col="black",cex=2,pch=16)
    
    # We set 10 months as the time lapse until the onset of the first breeding 
    # season after the metamorphosis of each individual. Whether the individual
    # will have reached maturity or not by that time, we obtain the body size (SVL)
    # predicted by the best model for that species at 10 months (c. 300 days), 
    # so right after the first winter of the individual
    
    # Similarly we obtain the body size predicted at the time of the second 
    # breeding season after metamorphosis: 21 months = 665 days, and
    # the same for the 3rd (1030 days), 4th (1395) and 5th (1760) breeding
    # seasons

    for (d in c(300,665,1030,1395,1760)) {
      
      if ("pooled" %in% unique(agepred[agepred$Species==sp,"Sex"])) {
        
        abline(h=agepred[agepred$Species==sp & agepred$age_days==d,"SVL_pred"],
               col="black",lty=1,
               lwd=unique(agepred[agepred$age_days==d,"lwd"]))
        
      } else {
        
        abline(h=agepred[agepred$Species==sp & agepred$Sex=="Male" &
                           agepred$age_days==d,"SVL_pred"],
               col="steelblue",lty=1,
               lwd=unique(agepred[agepred$age_days==d,"lwd"]))
        
        abline(h=agepred[agepred$Species==sp & agepred$Sex=="Female" &
                           agepred$age_days==d,"SVL_pred"],
               col="firebrick",lty=1,
               lwd=unique(agepred[agepred$age_days==d,"lwd"]))
      }
      
    }
        
    dev.off()
    
    png(filename=paste(paste("SVL_first_year_",gsub(" ", "_", sp),"_",sep=""),p,".png",sep=""),
        width = 800, height = 700)
    
    par(mar = c(5, 6, 4, 2) + 0.1)
    plot(males[males$years_from_mark==0,"year"]-
           0.014*(max(captures_sp$year,na.rm=T)-min(captures_sp$year,na.rm=T)),
         males[males$years_from_mark==0,"SVL_mm"], type="p", col="steelblue",
         cex=2,bty="L", xlab="Year", ylab="SVL (mm)", xaxt = "n",yaxt = "n",
         main=bquote(italic(.(sp))~.(gsub("_"," ",p))), cex.main=2.5, 
         cex.lab=2,pch=16,
         xlim=c(min(captures_sp$year,na.rm=T)-0.5,max(captures_sp$year,na.rm=T)+0.5),
         ylim=c(25,max(captures_sp$SVL_mm,na.rm=T)+5),
    )
    axis(1, lwd=1.3,cex.axis=1.5)
    axis(2, las=2, lwd=1.3,cex.axis=1.5)
    points(females[females$years_from_mark==0,"year"]+
             0.014*(max(captures_sp$year,na.rm=T)-min(captures_sp$year,na.rm=T)),
           females[females$years_from_mark==0,"SVL_mm"],col="firebrick",
           cex=2,pch=16)
    points(unknown[unknown$years_from_mark==0,"year"]+
             0.03*(max(captures_sp$year,na.rm=T)-min(captures_sp$year,na.rm=T)),
           unknown[unknown$years_from_mark==0,"SVL_mm"],col="black",
           cex=2,pch=16)
    
    for (d in c(300,665,1030,1395,1760)) {
      
      if ("pooled" %in% unique(agepred[agepred$Species==sp,"Sex"])) {
        
        abline(h=agepred[agepred$Species==sp & agepred$age_days==d,"SVL_pred"],
               col="black",lty=1,
               lwd=unique(agepred[agepred$age_days==d,"lwd"]))
        
      } else {
        
        abline(h=agepred[agepred$Species==sp & agepred$Sex=="Male" &
                           agepred$age_days==d,"SVL_pred"],
               col="steelblue",lty=1,
               lwd=unique(agepred[agepred$age_days==d,"lwd"]))
        
        abline(h=agepred[agepred$Species==sp & agepred$Sex=="Female" &
                           agepred$age_days==d,"SVL_pred"],
               col="firebrick",lty=1,
               lwd=unique(agepred[agepred$age_days==d,"lwd"]))
      }
      
    }
    
    dev.off()
    
  }
  
}

###### Create a table with the minimum sizes observed for each species and sex
###### at the year of first capture of the individual and up to the following
###### 5 years, including sample size

### For this table, we only consider adults (therefore we exclude juveniles)

captures_ad <- captures[captures$Sex!="Juvenile",]

minsizes <- data.frame(
  Species = character(),
  Sex = character(),
  min_SVL_0 = numeric(), N_0 = numeric(), min_SVL_1 = numeric(), N_1 = numeric(),
  min_SVL_2 = numeric(), N_2 = numeric(), min_SVL_3 = numeric(), N_3 = numeric(),
  min_SVL_4 = numeric(), N_4 = numeric(), min_SVL_5 = numeric(), N_5 = numeric(),
  stringsAsFactors = FALSE
)

for (sp in sort(unique(captures_ad$Species))) {
  
  captures_ad_sp <- captures_ad[captures_ad$Species==sp,]
  
  df<-data.frame(
    Species = rep(sp,length(unique(captures_ad_sp$Sex))),
    Sex = sort(unique(captures_ad_sp$Sex)),
    min_SVL_0 = NA, N_0 = NA, min_SVL_1 = NA, N_1 = NA,
    min_SVL_2 = NA, N_2 = NA, min_SVL_3 = NA, N_3 = NA,
    min_SVL_4 = NA, N_4 = NA, min_SVL_5 = NA, N_5 = NA,
    stringsAsFactors = FALSE
  )
  
  for (sex in sort(unique(captures_ad_sp$Sex))) {
    
    captures_ad_sp_sex <- captures_ad_sp[captures_ad_sp$Sex==sex,]
    
    for (yfm in seq(0,5)) {
      
      df[df$Sex==sex,paste("min_SVL_",yfm,sep="")] <- round(min(captures_ad_sp_sex[captures_ad_sp_sex$years_from_mark==yfm,
                                                            "SVL_mm"], na.rm = TRUE),0)
      df[df$Sex==sex,paste("N_",yfm,sep="")] <- length(!is.na(captures_ad_sp_sex[captures_ad_sp_sex$years_from_mark==yfm,
                                                                "SVL_mm"]))
    }
      
  }
  
  minsizes <- rbind(minsizes,df)

}

write.csv(minsizes, "minimum_sizes.csv", row.names=F)
