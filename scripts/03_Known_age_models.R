# Modified from Ogle (2019) FSA: Fisheries Stock Analysis. R package 
# version 0.8.25, available at https://github.com/fishR-Core-Team/FSA.
# This scripts builds models based on body size (in amphibians, 
# snout-to-vent length, SVL) measures of individuals of known age

# NOTE: the plotting parameters of the graphs (colors, margins, axes, etc.)
# have been defined to suit the study species. The user may want to modify
# them to adjust to other species

library(FSA)
library(propagate)

rm(list = ls())

# Load the dataset of measures of individuals of known age

age_certain<-read.csv("age_certain.csv")
age_certain$Date<-as.Date(age_certain$Date,format="%d/%m/%Y")

# Filtering the dataset

# This step removes repeated measures of the same individual (which are not allowed in the
# models), by selecting the largest measure (at the youngest age in the case of tie)
# for each individual

# NOTE: It is assumed that rows with empty ID correspond to different individuals.
# Otherwise, identify repeated individuals by labeling them in the ID column.

age_certain_filtered<-subset(age_certain, ID=="")
age_certain_filtered<-age_certain_filtered[,c("age","SVL","Species","Pop","ID","Sex")]

                                                 
for (id in unique(age_certain[age_certain$ID!="","ID"])) {
  individual<-age_certain[age_certain$ID==id,]
  nextrecord<-individual[which(individual$SVL==max(individual$SVL, na.rm = TRUE))
                         ,c("age","SVL","Species","Pop","ID","Sex","Date")]
  nextrecord<-nextrecord[nextrecord$Date==min(nextrecord$Date),]
  nextrecord<-subset(nextrecord,select=-c(Date))
  age_certain_filtered<-rbind(age_certain_filtered,nextrecord)
}

# We pool data of species with insufficient data for any of the populations,
# as set in the specific parameters file

params<-read.csv("specific_parameters.csv")

for (sp in unique(age_certain_filtered$Species)) {
  if (params[params$species==sp,"knownage_pops"]=="pooled") {
    age_certain_filtered[age_certain_filtered$Species==sp,"Pop"]<-"Pooled"
  }
}

mycol<-data.frame(Sex=c("Female","Juvenile","Male","Unknown"),
                  Col=c("firebrick","darkgrey","steelblue","black"))

predict_age <- data.frame(
  Species = character(),
  Sex = character(),
  age_days = numeric(),
  SVL_pred = numeric(),
  SVL_pred_lower = numeric(),
  SVL_pred_upper = numeric(),
  Linf = numeric(),
  Linf_lower = numeric(),
  Linf_upper = numeric(),
  k = numeric(),
  stringsAsFactors = FALSE
)

for (sp in params[params$knownage_sex!="","species"]) {
  numsex<-unlist(strsplit(params[params$species==sp,"knownage_sex"],"+",fixed=TRUE))

  # We create a dataframe to store SVL predictions for each species
  
  # We set 10 months as the time lapse until the onset of the first breeding 
  # season after the metamorphosis of each individual. Whether the individual
  # will have reached maturity or not by that time, we obtain the body size (SVL)
  # predicted by the best model for that species at 10 months (c. 300 days), 
  # so right after the first winter of the individual
  
  # Similarly we obtain the body size predicted at the time of the second 
  # breeding season after metamorphosis: 21 months = 665 days, and
  # the same for the 3rd (1030 days), 4th (1395) and 5th (1760) breeding
  # seasons
  
  miniset<-data.frame(Species=rep(sp,5*length(numsex)),Sex=rep(numsex,each=5),
                      age_days=rep(c(300,665,1030,1395,1760),length(numsex)),
                      SVL_pred=NA,SVL_pred_lower=NA,SVL_pred_upper=NA,Linf=NA,
                      Linf_lower=NA,Linf_upper=NA,k=NA)
  predict_age<-rbind(predict_age,miniset)
}


for (sp in unique(age_certain_filtered$Species)) {
  
  age_certain_filt_sp<-age_certain_filtered[age_certain_filtered$Species==sp,]
  
  for (p in unique(age_certain_filt_sp$Pop)) {
    
    age_certain_filt_sp_pop<-age_certain_filt_sp[age_certain_filt_sp$Pop==p,]
    
    png(filename=paste("knownage_",paste(gsub(" ", "_", sp),"_",sep=""),p,".png",sep=""),
        width = 800, height = 700,)
    
    par(mar = c(5, 5, 4, 2) + 0.1)
    
    plot(SVL~age,data=age_certain_filt_sp_pop[age_certain_filt_sp_pop$Sex=="Juvenile",],
         xlim=c(0,6),ylim=c(0,max(age_certain_filt_sp_pop$SVL, na.rm=T)+17),
         pch=19,col="darkgrey",cex=2,
         xaxt = "n", yaxt = "n",xlab="Age (years)", ylab="SVL (mm)", bty="L",
         main=bquote(italic(.(sp))~.(gsub("_", " ", p))), cex.main=2.5, cex.lab=2)
    axis(1, lwd=1.3,cex.axis=1.5)
    axis(2, las=2, lwd=1.3,cex.axis=1.5)
    points(age_certain_filt_sp_pop[age_certain_filt_sp_pop$Sex=="Male",]$age,
           age_certain_filt_sp_pop[age_certain_filt_sp_pop$Sex=="Male",]$SVL,
           col="steelblue",cex=2,pch=19)
    points(age_certain_filt_sp_pop[age_certain_filt_sp_pop$Sex=="Female",]$age,
           age_certain_filt_sp_pop[age_certain_filt_sp_pop$Sex=="Female",]$SVL,
           col="firebrick",cex=2,pch=19)
    points(age_certain_filt_sp_pop[age_certain_filt_sp_pop$Sex=="Unknown",]$age,
           age_certain_filt_sp_pop[age_certain_filt_sp_pop$Sex=="Unknown",]$SVL,
           col="black",cex=2,pch=19)
    legend(x="bottomright", legend=unique(sort(age_certain_filt_sp_pop$Sex)),
           col=mycol[mycol$Sex %in% unique(age_certain_filt_sp_pop$Sex),"Col"],
           pch=16, bty="n",cex=2)
    
    for (sex in c("females","males","pooled")) {
      
      if (sex %in% unlist(strsplit(params[params$species==sp,"knownage_sex"],"+",fixed=T))) {
        
        if (sex=="females") {
          subsetdata<-age_certain_filt_sp_pop[(age_certain_filt_sp_pop$Sex=="Female" | 
                                                 age_certain_filt_sp_pop$Sex=="Juvenile"),]
          colsex<-"firebrick"
        }
        
        else if (sex=="males") {
          subsetdata<-age_certain_filt_sp_pop[(age_certain_filt_sp_pop$Sex=="Male" | 
                                                 age_certain_filt_sp_pop$Sex=="Juvenile"),]
          colsex<-"steelblue"
        }
        
        else {
          subsetdata<-age_certain_filt_sp_pop
          colsex<-"black"
        }
        
        subsetdata$age<-round(subsetdata$age)
        vb1 <- vbFuns()
        fit1 <- nls(SVL~vb1(age,Linf,K,t0),data=subsetdata,
                    start=vbStarts(SVL~age,data=subsetdata,type="typical"),
                    control = nls.control(maxiter = 1000))
        Linf <- coef(fit1)[1]
        K <- coef(fit1)[2]
        t0 <- coef(fit1)[3]
        curve(vb1(x,Linf=Linf,K=K,t0=t0),from=0,to=10,lwd=5,add=TRUE,
              col=colsex)

        x1 <- seq(0, 6, length = 100)
        y.pred <- predictNLS(fit1, newdata=data.frame(age=x1), interval="prediction", alpha=0.05, nsim=10000)$summary
        matlines(x1, y.pred[,c("Sim.2.5%", "Sim.97.5%")], col=colsex, lty="dashed", lwd=4)

        Linf.pred <- predictNLS(fit1, newdata=data.frame(age=30), interval="prediction", alpha=0.05, nsim=10000)$summary
        newage <- data.frame(age_days=c(300,665,1030,1395,1760),age=c(0.822,1.822,2.822,3.822,4.822))
        newage$SVL_pred <- predict(fit1, newdata=newage)
        newage.pred <- predictNLS(fit1, newdata=data.frame(age=newage$age), interval="prediction", alpha=0.05, nsim=10000)$summary
        newage$SVL_pred_low <- newage.pred[,c("Sim.2.5%")]
        newage$SVL_pred_up <- newage.pred[,c("Sim.97.5%")]
        predict_age[predict_age$Species==sp & predict_age$Sex==sex,"SVL_pred"]<-newage$SVL_pred
        predict_age[predict_age$Species==sp & predict_age$Sex==sex,"SVL_pred_lower"]<-newage$SVL_pred_low
        predict_age[predict_age$Species==sp & predict_age$Sex==sex,"SVL_pred_upper"]<-newage$SVL_pred_up
        predict_age[predict_age$Species==sp & predict_age$Sex==sex,"Linf"]<-Linf
        predict_age[predict_age$Species==sp & predict_age$Sex==sex,"Linf_lower"]<-Linf.pred[,"Sim.2.5%"]
        predict_age[predict_age$Species==sp & predict_age$Sex==sex,"Linf_upper"]<-Linf.pred[,"Sim.97.5%"]
        predict_age[predict_age$Species==sp & predict_age$Sex==sex,"k"]<-K
      }
      
    }

    dev.off()
    
  }
  
}

write.csv(predict_age, "Knownage_estimates.csv", row.names=F)
