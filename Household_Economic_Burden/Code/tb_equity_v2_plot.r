## Clear workspace
rm(list=ls(all=TRUE))

## Load packages
library(fst)
library(dplyr)
library(tidyr)
library(data.table)
library(tidyverse)
library(RColorBrewer)

## FIGURE 1 - BARPLOTS BY QUINTILE
setwd("C:/Users/aportnoy/BOSTON UNIVERSITY Dropbox/Allison Portnoy/TB/Equity 2.0")
figure <- read.csv("Figure1.csv",check.names=FALSE,header=TRUE)

colors <- brewer.pal(5, "Dark2")

cc <- as.matrix(figure[1:5,2:6])
cc <- cc/1000000

png("Figure1.png",res=300,width=220,height=132,units="mm")

layout(matrix(c(1,2,3,3), ncol=1, byrow=TRUE), heights=c(8, 1))

par(mar=c(2,5,3,0.8))

barplot(cc,main="",ylim=c(0,15),
        col=colors,xlab="",beside=TRUE,ylab="",
        names.arg=c("Poorest","Poorer","Middle","Richer","Richest"),las=1,cex.axis=1,border=NA,cex.lab=1.25)
#mtext("Income quintile",1,3.2)
mtext("Households with catastrophic costs (millions)",2,2.5)

par(mar = c(1, 0, 1, 0))
plot.new()
legend(
  x = "center",
  legend = c("USAID termination",
             "USAID termination + Reduced GF contributions USA alone",
             "USAID termination + terminated GF contributions USA alone",
             "USAID termination + Reduced GF contributions",
             "Elimination of all external TB funding"),
  ncol = 2,                       # all scenarios in one row
  xpd = TRUE,                      # allow drawing outside plot
  inset = 0,                       # no extra inset space
  bty = "n",                       # no box
  #title=expression(bold("Scenario")),
  pch = 15,
  cex = 1.2,
  col = colors,
  pt.cex = 2.2,
  x.intersp = 0.8,                 # reduce gap between symbols and text
  y.intersp = 0.8                  # vertical spacing 
)

dev.off();system(paste("open", "Figure1.png"))


###### Concentration index

setwd("C:/Users/aportnoy/BOSTON UNIVERSITY Dropbox/Allison Portnoy/TB/Equity 2.0/Tables")
quintile <- read.csv("table2_quintile.csv",check.names=FALSE,header=TRUE)

dm <- as.matrix(quintile[1:5,3:7])
nm <- as.matrix(quintile[6:10,3:7])
ind <- as.matrix(quintile[11:15,3:7])
total <- as.matrix(quintile[16:20,3:7])
cc <- as.matrix(quintile[21:25,3:7])

## TB dm 
for (i in 1:5) {
  
  y = c(0,cumsum(dm[i,])/sum(dm[i,]))
  y1 <- (y[-1]+y[-length(y)])/2
  x1 <- rep(0.2,5)
  z = sum(y1*x1)
  C = 1 - z*2
  print(C) 
  
}
# [1] 0.04117091
# [1] 0.04128766
# [1] 0.04105167
# [1] 0.04104294
# [1] 0.04104389

## TB nm 
for (i in 1:5) {
  
  y = c(0,cumsum(nm[i,])/sum(nm[i,]))
  y1 <- (y[-1]+y[-length(y)])/2
  x1 <- rep(0.2,5)
  z = sum(y1*x1)
  C = 1 - z*2
  print(C) 
  
}
# [1] 0.008712317
# [1] 0.008882056
# [1] 0.008882668
# [1] 0.00887381
# [1] 0.008876689

## TB ind 
for (i in 1:5) {
  
  y = c(0,cumsum(ind[i,])/sum(ind[i,]))
  y1 <- (y[-1]+y[-length(y)])/2
  x1 <- rep(0.2,5)
  z = sum(y1*x1)
  C = 1 - z*2
  print(C) 
  
}
# [1] 0.2735353
# [1] 0.2733726
# [1] 0.2734878
# [1] 0.2734974
# [1] 0.2734952

## TB total 
for (i in 1:5) {
  
  y = c(0,cumsum(total[i,])/sum(total[i,]))
  y1 <- (y[-1]+y[-length(y)])/2
  x1 <- rep(0.2,5)
  z = sum(y1*x1)
  C = 1 - z*2
  print(C) 
  
}
# [1] 0.1407572
# [1] 0.1349775
# [1] 0.1361586
# [1] 0.1365948
# [1] 0.1364743

## TB cc 
for (i in 1:5) {
  
  y = c(0,cumsum(cc[i,])/sum(cc[i,]))
  y1 <- (y[-1]+y[-length(y)])/2
  x1 <- rep(0.2,5)
  z = sum(y1*x1)
  C = 1 - z*2
  print(C) 
  
}
# [1] -0.2390796
# [1] -0.2441027
# [1] -0.2441199
# [1] -0.2437963
# [1] -0.2438954

#### FIGURE 2 - CC DISTRIBUTION 

## Create list of countries
setwd("C:/Users/aportnoy/BOSTON UNIVERSITY Dropbox/Allison Portnoy/TB/Equity 2.0/Inputs")
countries <- read.csv("countries_included.csv",header=TRUE,check.names=FALSE)[,2]
countries_all <- read.csv("countries_included.csv",header=TRUE,check.names=FALSE)

## Load % catastrophic
setwd("C:/Users/aportnoy/BOSTON UNIVERSITY Dropbox/Allison Portnoy/TB/Equity 2.0/Inputs")
cc <- read.csv("cc_all.csv",header=TRUE,check.names=FALSE)

### QUINTILE SUMMARY - BASE-CASE SCENARIO

output <- matrix(0,79,5)
colnames(output) <- c("cc_poorest","cc_poorer","cc_middle","cc_richer","cc_richest")

for (i0 in 1:length(countries)) {
  
  i <- countries[i0]
  
  cty_cc <- cc[which(cc$code==i),]
  
  cc_mean <- colMeans(cty_cc[,2:6])
  
  output[i0,] <- cc_mean
  
}

output_all <- cbind(countries_all,output)
setwd("C:/Users/aportnoy/BOSTON UNIVERSITY Dropbox/Allison Portnoy/TB/Equity 2.0")
write.csv(output_all,"cc_summary.csv",row.names=FALSE)

## % catastrophic by country and quintile
quintile_colors <- c("#B2182B","#F4A582","#92C5DE","#2166AC","#053061")

png("Figure2.png",res=300,width=220,height=132,units="mm")

par(mar=c(1, 0.5, 0.5, 1),
    mfrow=c(1,1),
    xpd=TRUE,
    mai=c(1, 0.8, 0.5, 0.1))

plot(output_all$GDPpc,output_all$cc_richest,main="",ylim=c(0,1),col=quintile_colors[1],
     xlab="",ylab="",pch=16,las=1,cex.axis=1,cex.lab=1.25,axes=FALSE)
points(output_all$GDPpc,output_all$cc_richer,pch=16,col=quintile_colors[2])
points(output_all$GDPpc,output_all$cc_middle,pch=16,col=quintile_colors[3])
points(output_all$GDPpc,output_all$cc_poorer,pch=16,col=quintile_colors[4])
points(output_all$GDPpc,output_all$cc_poorest,pch=16,col=quintile_colors[5])

abline(lm(output_all$cc_richest ~ output_all$GDPpc),col=quintile_colors[1],xpd=FALSE,lty=2)
abline(lm(output_all$cc_richer ~ output_all$GDPpc),col=quintile_colors[2],xpd=FALSE,lty=2)
abline(lm(output_all$cc_middle ~ output_all$GDPpc),col=quintile_colors[3],xpd=FALSE,lty=2)
abline(lm(output_all$cc_poorer ~ output_all$GDPpc),col=quintile_colors[4],xpd=FALSE,lty=2)
abline(lm(output_all$cc_poorest ~ output_all$GDPpc),col=quintile_colors[5],xpd=FALSE,lty=2)

axis(side=1,at=c(0,4000,8000,12000,16000),
     labels=c("$0","$4K","$8K","$12K","$16K"),cex.axis=0.7)
axis(side=2,at=c(0,0.2,0.4,0.6,0.8,1.0),
     labels=c("0%","20%","40%","60%","80%","100%"),cex.axis=0.7,las=1)
title(xlab="GDP per capita",ylab="Percentage of TB-affected households experiencing CC\n",line=2,
      cex.lab=0.8)

legend("top",box.col="white",legend=c("Richest","Richer","Middle","Poorer","Poorest"),
       col=quintile_colors,pch=16,cex=0.8,
       horiz=TRUE,inset=c(0,-0.1),pt.cex=1)

dev.off();system(paste("open", "Figure2.png"))

###################### 
## FIGURE 3 - GLOBAL DISTRIBUTION

setwd("C:/Users/aportnoy/BOSTON UNIVERSITY Dropbox/Allison Portnoy/TB/Equity 2.0/Global Dist")

mTrsp <- function(cl,a)  { apply(col2rgb(cl), 2, function(x){ rgb(x[1],x[2],x[3],a,maxColorValue=255)}) }

scenarios <- c("scen_usaid","scen_usaid_gf","scen_usaid_gf_expcut_noNGO",
               "scen_usaid_USA_expcut","scen_usaid_USA")

for (j0 in 1:length(scenarios)) {
  
  j <- scenarios[j0]
  
  dat <- read.csv(paste0("global_dist_",j,".csv"),check.names=FALSE,header=TRUE)
  
  ## Calculate average income
  dat$poorest_avg_income <- dat$gdp_pc_ppp*dat$poorest_share/0.2
  dat$poorer_avg_income  <- dat$gdp_pc_ppp*dat$poorer_share/0.2
  dat$middle_avg_income  <- dat$gdp_pc_ppp*dat$middle_share/0.2
  dat$richer_avg_income  <- dat$gdp_pc_ppp*dat$richer_share/0.2
  dat$richest_avg_income <- dat$gdp_pc_ppp*dat$richest_share/0.2
  
  ## Move to long format
  dat2 <- as.data.frame(cbind(country=rep(dat$country,5),
                              code=rep(dat$code,5),
                              region=rep(dat$region,5),
                              pop=rep(dat$pop,5)/5,
                              quintile=rep(1:5,each=nrow(dat))))
  dat2$av_incm_ppp <- c(dat$poorest_avg_income,dat$poorer_avg_income,
                        dat$middle_avg_income,dat$richer_avg_income,
                        dat$richest_avg_income)
  
  dat2$cc_averted <- c(dat$cc_averted_poorest,dat$cc_averted_poorer,
                          dat$cc_averted_middle,dat$cc_averted_richer,
                          dat$cc_averted_richest)
  
  dat2$cc_averted <- c(dat$cc_poorest,dat$cc_poorer,
                       dat$cc_middle,dat$cc_richer,
                       dat$cc_richest)
  
  dat2$dm_averted <- c(dat$dm_poorest,dat$dm_poorer,
                       dat$dm_middle,dat$dm_richer,
                       dat$dm_richest)
  
  dat2$nm_averted <- c(dat$nm_poorest,dat$nm_poorer,
                       dat$nm_middle,dat$nm_richer,
                       dat$nm_richest)
  
  dat2$ind_averted <- c(dat$ind_poorest,dat$ind_poorer,
                        dat$ind_middle,dat$ind_richer,
                        dat$ind_richest)
  
  dat2$total_averted <- c(dat$total_poorest,dat$total_poorer,
                          dat$total_middle,dat$total_richer,
                          dat$total_richest)
  
  ## Rank by av_incm_ppp (lowest first)
  dat3 <- dat2[order(dat2$av_incm_ppp),]
  
  new_name_1 <- paste0("dat3_",j)
  
  assign(new_name_1,dat3)
  
  # Lets do 20 categories by pop - CC cases
  pop_fract <- cumsum(as.numeric(dat3$pop))/sum(as.numeric(dat3$pop))
  by_20_cc <- data.frame(quantile=1:20,pop=NA,cumulative=NA,cc_averted=NA)
  
  for(i in 1:20){
    tmp <- sum(pop_fract<(0.05*i))
    by_20_cc$cumulative[i] <- sum(dat3$cc_averted[1:tmp])+
      dat3$cc_averted[tmp+1] * ((0.05*i)-pop_fract[tmp])/(pop_fract[tmp+1]-pop_fract[tmp])
    by_20_cc$pop[i] <- sum(as.numeric(dat3$pop)[1:tmp])+
      as.numeric(dat3$pop)[tmp+1] * ((0.05*i)-pop_fract[tmp])/(pop_fract[tmp+1]-pop_fract[tmp])
    
  }
  
  by_20_cc$cc_averted  <- diff(c(0,by_20_cc$cumulative))
  
  new_name_2 <- paste0("by_20_cc_",j)
  
  assign(new_name_2,by_20_cc)
  
}

######################

png("Figure3.png", res = 300, width = 8, height = 10, units = "in")

# Layout for 3 vertically stacked plots
par(mfrow = c(3, 1), 
    mar = c(3.2, 4.8, 2.0, 4.8),  # inner margins: bottom, left, top, right
    oma = c(2, 3, 1, 3),        # outer margins
    xpd = NA)                  # allow plotting outside region (for labels)

########### A. Termination of USAID funding
plot(NA, NA, xlim = c(0, 100), ylim = c(0, max(by_20_cc_scen_usaid_gf$cc_averted)), 
     xlab = "", ylab = "", axes = FALSE)
for (i in 1:20) {
  polygon(c(0.25, 0.25, 4.75, 4.75) + (i - 1) * 5, 
          c(0, by_20_cc_scen_usaid$cc_averted[i], by_20_cc_scen_usaid$cc_averted[i], 0),
          border = FALSE, col = ifelse(i < 5, "red4", "dodgerblue"))
}
axis(2, las = 1, tcl = -0.25, mgp = c(3, 0.6, 0),
     at = seq(0, 7000000, by = 1000000),
     labels = sprintf("%.1f", seq(0, 7, by = 1)))
axis(1, las = 1, tcl = -0.25, mgp = c(3, 0.4, 0))
par(new = TRUE)
plot(cumsum(as.numeric(dat3_scen_usaid$pop)) / sum(as.numeric(dat3_scen_usaid$pop)) * 100,
     cumsum(dat3_scen_usaid$cc_averted) / sum(dat3_scen_usaid$cc_averted) * 100,
     type = "l", axes = FALSE, xlab = "", ylab = "", col = mTrsp(1, 150), lwd = 2)
axis(4, las = 1, tcl = -0.25, mgp = c(3, 0.6, 0))
mtext("A. USAID termination", 3, 1.2, adj = 0, font = 2, cex = 1.2)
box()

########### B. Termination of USAID + GF* funding
plot(NA, NA, xlim = c(0, 100), ylim = c(0, max(by_20_cc_scen_usaid_gf$cc_averted)),
     xlab = "", ylab = "", axes = FALSE)
for (i in 1:20) {
  polygon(c(0.25, 0.25, 4.75, 4.75) + (i - 1) * 5, 
          c(0, by_20_cc_scen_usaid_gf_expcut_noNGO$cc_averted[i], by_20_cc_scen_usaid_gf_expcut_noNGO$cc_averted[i], 0),
          border = FALSE, col = ifelse(i < 5, "red4", "dodgerblue"))
}
axis(2, las = 1, tcl = -0.25, mgp = c(3, 0.6, 0),
     at = seq(0, 7000000, by = 1000000),
     labels = sprintf("%.1f", seq(0, 7, by = 1)))
axis(1, las = 1, tcl = -0.25, mgp = c(3, 0.4, 0))
par(new = TRUE)
plot(cumsum(as.numeric(dat3_scen_usaid_gf_expcut_noNGO$pop)) / sum(as.numeric(dat3_scen_usaid_gf_expcut_noNGO$pop)) * 100,
     cumsum(dat3_scen_usaid_gf_expcut_noNGO$cc_averted) / sum(dat3_scen_usaid_gf_expcut_noNGO$cc_averted) * 100,
     type = "l", axes = FALSE, xlab = "", ylab = "", col = mTrsp(1, 150), lwd = 2)
axis(4, las = 1, tcl = -0.25, mgp = c(3, 0.6, 0))
mtext("B. USAID termination + Reduced GF contributions", 3, 1.2, adj = 0, font = 2, cex = 1.2)

# Add shared axis labels on middle panel only (closer to plots)
mtext("Households experiencing catastrophic costs (millions)", 
      side = 2, line = 3.2, outer = FALSE, cex = 1.2)
mtext("Cumulative percentile of households experiencing catastrophic costs", 
      side = 4, line = 3.2, outer = FALSE, cex = 1.2)

box()

########### C. Termination of USAID + GF funding
plot(NA, NA, xlim = c(0, 100), ylim = c(0, max(by_20_cc_scen_usaid_gf$cc_averted)), 
     xlab = "", ylab = "", axes = FALSE)
for (i in 1:20) {
  polygon(c(0.25, 0.25, 4.75, 4.75) + (i - 1) * 5, 
          c(0, by_20_cc_scen_usaid_gf$cc_averted[i], by_20_cc_scen_usaid_gf$cc_averted[i], 0),
          border = FALSE, col = ifelse(i < 5, "red4", "dodgerblue"))
}
axis(2, las = 1, tcl = -0.25, mgp = c(3, 0.6, 0),
     at = seq(0, 7000000, by = 1000000),
     labels = sprintf("%.1f", seq(0, 7, by = 1)))
axis(1, las = 1, tcl = -0.25, mgp = c(3, 0.4, 0))
par(new = TRUE)
plot(cumsum(as.numeric(dat3_scen_usaid_gf$pop)) / sum(as.numeric(dat3_scen_usaid_gf$pop)) * 100,
     cumsum(dat3_scen_usaid_gf$cc_averted) / sum(dat3_scen_usaid_gf$cc_averted) * 100,
     type = "l", axes = FALSE, xlab = "", ylab = "", col = mTrsp(1, 150), lwd = 2)
axis(4, las = 1, tcl = -0.25, mgp = c(3, 0.6, 0))
mtext("C. Elimination of all external TB funding", 3, 1.2, adj = 0, font = 2, cex = 1.2)
box()

# Shared labels for all plots
mtext("Percentiles of household income", side = 1, outer = TRUE, line = 1.2, cex = 1.2)
#mtext("Households experiencing catastrophic costs (millions)", side = 2, outer = TRUE, line = 1.8, cex = 1.1)
#mtext("Cumulative percentile of households experiencing catastrophic costs", side = 4, outer = TRUE, line = 1.8, cex = 1.1)
dev.off()
system(paste("open", "Figure3.png"))

### Concentration index
# If z = area under the concentration curve,
# then C = 1 - z*2
# negative values --> concentration among poor
# positive values --> concentration among rich

# https://www.worldbank.org/content/dam/Worldbank/document/HDN/Health/HealthEquityCh8.pdf

## USAID scenario
x = c(0,cumsum(as.numeric(dat3_scen_usaid$pop))/sum(as.numeric(dat3_scen_usaid$pop)))
y = c(0,cumsum(dat3_scen_usaid$cc_averted)/sum(dat3_scen_usaid$cc_averted))

y1 <- (y[-1]+y[-length(y)])/2
x1 <- diff(x)
z = sum(y1*x1)
C = 1 - z*2; C
# C = -0.6064182 --> concentration among poor
# % in each quintile
round(diff(c(0,by_20_cc_scen_usaid$cumulative[c(4,8,12,16,20)]))/by_20_cc_scen_usaid$cumulative[20]*100)
# 68 18  7  4  3

## USAID + GF scenario
x = c(0,cumsum(as.numeric(dat3_scen_usaid_gf$pop))/sum(as.numeric(dat3_scen_usaid_gf$pop)))
y = c(0,cumsum(dat3_scen_usaid_gf$cc_averted)/sum(dat3_scen_usaid_gf$cc_averted))

y1 <- (y[-1]+y[-length(y)])/2
x1 <- diff(x)
z = sum(y1*x1)
C = 1 - z*2; C
# C = -0.5277961 --> concentration among poor
# % in each quintile
round(diff(c(0,by_20_cc_scen_usaid_gf$cumulative[c(4,8,12,16,20)]))/by_20_cc_scen_usaid_gf$cumulative[20]*100)
# 59 21 10  7  3

## USAID + GF* scenario
x = c(0,cumsum(as.numeric(dat3_scen_usaid_gf_expcut_noNGO$pop))/sum(as.numeric(dat3_scen_usaid_gf_expcut_noNGO$pop)))
y = c(0,cumsum(dat3_scen_usaid_gf_expcut_noNGO$cc_averted)/sum(dat3_scen_usaid_gf_expcut_noNGO$cc_averted))

y1 <- (y[-1]+y[-length(y)])/2
x1 <- diff(x)
z = sum(y1*x1)
C = 1 - z*2; C
# C = -0.5297153 --> concentration among poor
# % in each quintile
round(diff(c(0,by_20_cc_scen_usaid_gf_expcut_noNGO$cumulative[c(4,8,12,16,20)]))/by_20_cc_scen_usaid_gf_expcut_noNGO$cumulative[20]*100)
# 59 21 11  6  3

## USAID + USA* scenario
x = c(0,cumsum(as.numeric(dat3_scen_usaid_USA_expcut$pop))/sum(as.numeric(dat3_scen_usaid_USA_expcut$pop)))
y = c(0,cumsum(dat3_scen_usaid_USA_expcut$cc_averted)/sum(dat3_scen_usaid_USA_expcut$cc_averted))

y1 <- (y[-1]+y[-length(y)])/2
x1 <- diff(x)
z = sum(y1*x1)
C = 1 - z*2; C
# C = -0.5353915 --> concentration among poor
# % in each quintile
round(diff(c(0,by_20_cc_scen_usaid_USA_expcut$cumulative[c(4,8,12,16,20)]))/by_20_cc_scen_usaid_USA_expcut$cumulative[20]*100)
# 60 21 10  6  3

## USAID + USA scenario
x = c(0,cumsum(as.numeric(dat3_scen_usaid_USA$pop))/sum(as.numeric(dat3_scen_usaid_USA$pop)))
y = c(0,cumsum(dat3_scen_usaid_USA$cc_averted)/sum(dat3_scen_usaid_USA$cc_averted))

y1 <- (y[-1]+y[-length(y)])/2
x1 <- diff(x)
z = sum(y1*x1)
C = 1 - z*2; C
# C = -0.5337115 --> concentration among poor
# % in each quintile
round(diff(c(0,by_20_cc_scen_usaid_USA$cumulative[c(4,8,12,16,20)]))/by_20_cc_scen_usaid_USA$cumulative[20]*100)
# 60 21 10  6  3

######################
## FIGURE S2 - GLOBAL DISTRIBUTION - US scenarios

png("FigureS2.png",res=300,width=8, height=9,units="in")

par(mfrow=2:1,mar=c(2.8,4.5,1.8,3.8))

###########  USAID + USA* scenario
# histogram

plot(NA,NA,xlim=0:1*100,ylim=c(0,max(by_20_cc_scen_usaid_gf_expcut_noNGO$cc_averted)),xlab=NA,ylab=NA,axes=F)
for(i in 1:20){
  polygon(c(0.25,0.25,4.75,4.75)+(i-1)*5,c(0,by_20_cc_scen_usaid_USA_expcut$cc_averted[i],by_20_cc_scen_usaid_USA_expcut$cc_averted[i],0),
          border=F,col=ifelse(i<5,"red4","dodgerblue"))
}

axis(2,las=1,tcl=-.25,mgp=c(3, 0.6, 0),
     at=c(0,500000,1000000,1500000,2000000,2500000,3000000),
     labels=c("0.0","0.5","1.0","1.5","2.0","2.5","3.0"))
axis(1,las=1,tcl=-.25,mgp=c(3, 0.4, 0))

# Concentration curve
par(new = T)
plot(cumsum(as.numeric(dat3_scen_usaid_USA_expcut$pop))/sum(as.numeric(dat3_scen_usaid_USA_expcut$pop))*100,cumsum(dat3_scen_usaid_USA_expcut$cc_averted)/sum(dat3_scen_usaid_gf$cc_averted)*100,type="l",xlab="",
     ylab="",axes=F,col=mTrsp(1,150),lwd=2)
axis(4,las=1,tcl=-.25,mgp=c(3, 0.6, 0))

mtext("Percentiles of household income",1,1.7)
mtext("Households experiencing CC (millions)",2,3.2)
mtext("Cumulative percentile of households experiencing CC",4,2.0)
mtext("A. Termination of USAID + reduced contributions from US to GF",3,0.8,adj=-0.12,font=2,cex=1.2)

box()

###########  USAID + USA* scenario
# histogram

plot(NA,NA,xlim=0:1*100,ylim=c(0,max(by_20_cc_scen_usaid_gf_expcut_noNGO$cc_averted)),xlab=NA,ylab=NA,axes=F)
for(i in 1:20){
  polygon(c(0.25,0.25,4.75,4.75)+(i-1)*5,c(0,by_20_cc_scen_usaid_USA_expcut$cc_averted[i],by_20_cc_scen_usaid_USA_expcut$cc_averted[i],0),
          border=F,col=ifelse(i<5,"red4","dodgerblue"))
}

axis(2,las=1,tcl=-.25,mgp=c(3, 0.6, 0),
     at=c(0,500000,1000000,1500000,2000000,2500000,3000000),
     labels=c("0.0","0.5","1.0","1.5","2.0","2.5","3.0"))
axis(1,las=1,tcl=-.25,mgp=c(3, 0.4, 0))

# Concentration curve
par(new = T)
plot(cumsum(as.numeric(dat3_scen_usaid_USA_expcut$pop))/sum(as.numeric(dat3_scen_usaid_USA_expcut$pop))*100,cumsum(dat3_scen_usaid_USA_expcut$cc_averted)/sum(dat3_scen_usaid_gf$cc_averted)*100,type="l",xlab="",
     ylab="",axes=F,col=mTrsp(1,150),lwd=2)
axis(4,las=1,tcl=-.25,mgp=c(3, 0.6, 0))

mtext("Percentiles of household income",1,1.7)
mtext("Households experiencing CC (millions)",2,3.2)
mtext("Cumulative percentile of households experiencing CC",4,2.0)
mtext("B. Termination of USAID + termination of US contributions to GF",3,0.8,adj=-0.12,font=2,cex=1.2)

box()

dev.off();system(paste("open", "FigureS2.png"))

## FIGURE S1 - country plot

setwd("C:/Users/aportnoy/BOSTON UNIVERSITY Dropbox/Allison Portnoy/TB/Equity 2.0")
cuts <- read.csv("funding_scenario_values.csv",check.names=FALSE,header=TRUE)
cc <- read.csv("output_cc.csv",check.names=FALSE,header=TRUE)

cuts_merge <- merge(cuts,countries_all,by="code")
cuts_long <- cuts_merge %>%
  pivot_longer(cols=starts_with("scen_"),names_to="scenario",values_to="cut")

cc_long <- cc %>%
  pivot_longer(
    cols = matches("^(pop_scen_|cc_scen_)"),      # select all relevant columns
    names_to = c(".value", "scenario"),           # split into variable type and scenario
    names_pattern = "^(pop_scen_|cc_scen_)(.*)$"  # extract scenario name after prefix
  )
colnames(cc_long) <- c("country","code","region","income","GDPpc","scenario","pop","cc")

years <- as.numeric(length(seq(2025,2050,1)))
#cc_long$cc_per <- cc_long$cc / (cc_long$pop*years)

regions <- c("AFR","AMR","EMR","EUR","SEAR","WPR")
region_colors <- c("#1178B4","#FF8856","#DA4C4C","#33B1FF","#9966CC","#38A86F")
names(region_colors) <- regions

png("FigureS1.png",res=300,width=150,height=150,units="mm")

par(mar=c(1, 0.5, 0.5, 1),
    mfrow=c(1,1),
    xpd=TRUE,
    mai=c(1, 0.8, 0.5, 0.1))

#plot(NA,NA,xlim=c(0,1),ylim=range(cc_long$cc_per),xlab="",ylab="",axes=FALSE,main="")
plot(NA,NA,xlim=c(0,1),ylim=range(cc_long$cc),xlab="",ylab="",axes=FALSE,main="")

# Loop through countries and add lines
for (i0 in 1:length(countries)) {
  
  i <- countries[i0]
  cty_cc <- subset(cc_long,code==i)
  cty_cut <- subset(cuts_long,code==i)
  
  # get region color
  region_name <- unique(cty_cc$region)
  line_color <- region_colors[region_name]
  
  # add line connecting all 5 scenario x values for that country
  #lines(cty_cut$cut,cty_cc$cc_per,col=line_color,type="l",lwd=1)
  lines(cty_cut$cut,cty_cc$cc,col=line_color,type="l",lwd=1)
  
}

# Create axes
axis(side=1,at=c(0,0.25,0.5,0.75,1),
     labels=c("0%","25%","50%","75%","100%"),cex.axis=0.7)
# axis(side=2,at=c(0,0.05,0.10,0.15,0.20,0.25),
#      labels=c("0","0.05","0.10","0.15","0.20","0.25"),cex.axis=0.7,las=1)
axis(side=2,at=c(0,0.5,1.0,1.5,2.0,2.5,3.0,3.5),
     labels=c("0%","50%","100%","150%","200%","250%","300%","350%"),cex.axis=0.7,las=1)
title(xlab="Percentage reduction in funding",ylab="Percentage increase in households experiencing CC\n",line=2,
      cex.lab=0.8,font.lab=2)

# Add legend
#legend("topright",legend=regions,col=region_colors,lwd=2,title="Region")

legend("top",box.col="white",legend=regions,
       col=region_colors,pch=16,cex=0.8,
       horiz=TRUE,inset=c(0,-0.1),pt.cex=1)

dev.off();system(paste("open", "FigureS1.png"))
