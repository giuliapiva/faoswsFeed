library(RJDBC)
library(stats4)

#setwd("~/1_Job/1_Work/5_Innovations/7_Use-Elements/Feed/Programming/Data")

'******************************************'
'* IR-Model 6.9                           *'
'* Estimation of intensification rates    *'
'* by combining cross-section micro-data  *'
'* with a pseudo panel, using a fixed     *'
'* effects model.                         *'
'* v.11: Imputation for missing estimates *'
'*       improved.                        *'
'* v.12: Source data updated              *'
'* v.13: Confidence ranges added          *'
'******************************************'

lstyear=2012

'*** Calculate the degree of intensification in production systems ***'
# GLEAM is source data

df1=read.csv("../Data/source/GLEAM_Feed-Comp_Rumin.csv")
df2=df1[,1:6]
df2$IntFactor=df1$ALFALFAH + df1$GRAINSIL + df1$MAIZESIL + df1$FDDRBEET + df1$GRAINS + df1$CORN + df1$MLSOY + df1$MLRAPE + df1$MLCTTN + df1$PKEXP + df1$MZGLTM + df1$MZGLTF + df1$BULP + df1$MOLASSES + df1$GRNBYDRY + df1$BRNBYWET
df2$total=apply(df1[,7:ncol(df1)], 1, FUN=sum)
df2[which(df2$total>0 & df2$total<0.99),]
is.na(df2$IntFactor)=(df2$total==0)

df3=read.csv("../Data/source/GLEAM_Feed-Comp_Monogast.csv")
df4=df3[,1:6]
df4$IntFactor=df3$PULSES + df3$CASSAVA + df3$WHEATS + df3$WHEATN + df3$MAIZES + df3$MAIZEN + df3$BARLEY + df3$MILLET + df3$RICE + df3$SORGHUM + df3$SOY + df3$BNFRUIT + df3$MLSOY + df3$MLOILSDS + df3$MLCTTN + df3$GRNBYDRY + df3$CPULSES + df3$CCASSAVA + df3$CWHEAT + df3$CMAIZE + df3$CBARLEY + df3$CMILLET + df3$CRICE + df3$CSORGHUM + df3$CSOY + df3$CMLSOY + df3$CMLOILSDS + df3$CMLCTTN + df3$PKEXP + df3$MOLASSES + df3$RAPESEED + df3$MLRAPE + df3$SOY.OIL + df3$LIME
df4$total=apply(df3[,7:ncol(df3)], 1, FUN=sum)
df4[which(df4$total>0 & df4$total<0.99),]
is.na(df4$IntFactor)=(df4$total==0)

df5=rbind(df2,df4)[,1:7]
df5=df5[order(df5$ADM0_CODE, df5$AnimGroup, df5$ProdSys),]

df6=df5[which(is.na(df5$IntFactor)==FALSE),]

df7=aggregate(df6[,7], df6[,c(1:3,5)], FUN=mean)
df7=df7[order(df7$ADM0_CODE, df7$AnimGroup, df7$ProdSys),]
names(df7)[5]="IntFactor"

df8=read.csv("../Data/GLEAM_Prodsys-Size.csv")

df9=merge(df7, df8, all.x=TRUE)

df9$IntAnimals=df9$NUMBER.HEADS*df9$IntFactor
names(df9)[4]="GLEAMPS"

gleamps=df9

remove(df1, df2, df3, df4, df5, df6, df7, df8, df9)

psreg=read.csv("../Data/LS-ProdSys_Registry_6-6.csv")
df0=unique(psreg[,c(3,5)])

df1=merge(gleamps, df0, all.x=TRUE)

df2=df1[which(is.na(df1$NUMBER.HEADS)==FALSE & is.na(df1$IntAnimals)==FALSE),]

agg1=aggregate(as.numeric(df2$NUMBER.HEADS), list(df2$ADM0_CODE, df2$AnimGroup, df2$StdPS), FUN=sum)
names(agg1)=c("ADM0_CODE", "AnimGroup", "StdPS", "TotAnimals")

agg2=aggregate(as.numeric(df2$IntAnimals), list(df2$ADM0_CODE, df2$AnimGroup, df2$StdPS), FUN=sum)
names(agg2)=c("ADM0_CODE", "AnimGroup", "StdPS", "IntAnimals")

df3=merge(agg1, agg2)

df3$IR=df3$IntAnimals/df3$TotAnimals

irprosys=df3

'* irprosys: intensification rates by (standardized) production system by country *'
'*           for all countries included in GLEAM                                  *'

remove(df0, df1, df2, df3, agg1, agg2, psreg, gleamps)


'*** Construct the roster for the aggregation among countries ***'

df0=read.csv("../Data/area-advancement_6-8.csv")
df1=read.csv("../Data/WLPS-Countries.csv")[1]
df1$WLPSCty=TRUE

for (i in 1:3) {

  ctyvar=paste("Area_", i, sep="")
  checkvar=paste("WLPSCty_", i, sep="")
  names(df1)=c(ctyvar, checkvar)

  df2=merge(df0, df1, all.x=TRUE)
  
  df0=df2  

}

df2=df0[which(df0$WLPSCty_1 | df0$WLPSCty_2 | df0$WLPSCty_3),]

df3=read.csv("../Data/GLEAM-Countries.csv")
names(df3)[3]="Area_3"

df4=merge(df2, df3, all.x=TRUE)

ctyrost=df4[order(df4$GERegion, df4$Area_1, df4$Area_2, df4$Area_3), c(4:5,3,6,2,7,1,8,12:13)]

'* ctyrost: Table which defines the hierarchy of countries, their assignment *'
'*         to greater regions, and, for the lowest hierarchy level, the     *'
'*         correspondence to the ADM0-codes used by GLEAM. Includes all     *'
'*         countries covered by WLPS.                                       *'

remove(df0, df1, df2, df3, df4, i, ctyvar, checkvar)

write.csv(ctyrost, file="output/check-cty.csv", row.names=FALSE)


'*** Calculate intensification rates in 2005 and aggregation weights ***'

df0=merge(ctyrost, irprosys, all.x=TRUE)

df0[which(df0$Area_3==2),]

write.csv(df0, file="output/check-1.csv", row.names=FALSE)

agg1=aggregate(df0$TotAnimals, list(df0$GERegion, df0$LabGERegion, df0$AnimGroup, df0$StdPS), FUN=sum)
names(agg1)=c("GERegion", "LabGERegion", "AnimGroup", "StdPS", "TotAnimals")

agg2=aggregate(df0$IntAnimals, list(df0$GERegion, df0$AnimGroup, df0$StdPS), FUN=sum)
names(agg2)=c("GERegion", "AnimGroup", "StdPS", "IntAnimals")
nrow(agg2)

df1=merge(agg1, agg2)
df1$IR=df1$IntAnimals/df1$TotAnimals

regpsir=df1[,c(1:3,7)]

'* regpsir: intensification rates of (standardized) production systems by region *'


agg3=aggregate(as.numeric(df0$TotAnimals), list(df0$GERegion, df0$AnimGroup), FUN=sum)
names(agg3)=c("GERegion", "AnimGroup", "RegAnimals")

agg4=aggregate(as.numeric(df0$IntAnimals), list(df0$GERegion, df0$AnimGroup), FUN=sum)
names(agg4)=c("GERegion", "AnimGroup", "RegIntAnimals")

agg5=aggregate(as.numeric(df0$TotAnimals), list(df0$GERegion, df0$Area_1, df0$Area_2, df0$Area_3, df0$LabArea_3, df0$AnimGroup), FUN=sum)
names(agg5)=c("GERegion", "Area_1", "Area_2", "Area_3", "LabArea_3", "AnimGroup", "CtyAnimals")

agg6=aggregate(as.numeric(df0$IntAnimals), list(df0$GERegion, df0$Area_3, df0$AnimGroup), FUN=sum)
names(agg6)=c("GERegion", "Area_3", "AnimGroup", "CtyIntAnimals")
agg6[1:10,]

df3=merge(agg3, agg4)
df4=merge(agg5, agg6)
df5=merge(df4, df3, all.x=TRUE)

df5$IR=df5$CtyIntAnimals/df5$CtyAnimals

df5$TrIR=ifelse(df5$IR>0 & df5$IR<1, log(df5$IR/(1-df5$IR)), NA)
'df5$TrIR=ifelse(df5$CtyIntAnimals>0, log(df5$IR/(1-df5$IR)), log(1/df5$CtyAnimals))'

df5$WgtTr=(df5$CtyAnimals-df5$CtyIntAnimals)/(df5$RegAnimals-df5$RegIntAnimals)

rst=merge(df5, c(1982,1992,2005))
aggrost=rst[,c(1,4,5,3,6,2,14,13)]
names(aggrost)[7]="Year"

ircty=df5[,c(1,3,2,11:12)]
names(ircty)[2]="AREA"


'* aggrost: Roster for the aggregation from the level of countries to regions. Includes the *'
'*          aggregation weights for each country at the lowest hierarchy level.             *'

'* ircty:   Intensification rates and their transformations for each GLEAM country *'

write.csv(ircty, file="output/ircty.csv", row.names=FALSE)

agg7=aggregate(df5$WgtTr^2, list(df5$GERegion, df5$AnimGroup), FUN=sum)
names(agg7)=c("GERegion", "AnimGroup", "SSqrWgt")
agg7$RegrWgt=1/agg7$SSqrWgt
rgrwgt=agg7[,c(1:2,4)]

agg8=aggregate(df5$WgtTr*df5$TrIR, list(df5$GERegion, df5$AnimGroup), FUN=sum)
names(agg8)=c("GERegion", "AnimGroup", "MnTrIR")

df6=merge(df3, agg8)
df6$MnIR=df6$RegIntAnimals/df6$RegAnimals
df6$TrMnIR=log(df6$MnIR/(1-df6$MnIR))
df6$Year=2005
rgir2005=df6[,c(1:2,8,5:7)]

remove(df0, df1, df3, df4, df5, df6, agg1, agg2, agg3, agg4, agg5, agg6, agg7, agg8)

write.csv(regpsir, file="output/regpsir.csv", row.names=FALSE)


'*** Analyse aggegation bias in the transformed IR ***'

plot(rgir2005$MnTrIR, rgir2005$TrMnIR, col=rgir2005$AnimGroup)
'plot(rgir2005$MnTrIR, rgir2005$TrMnIR, col=rgir2005$AnimGroup, xlim=c(-5,5), ylim=c(-5,5))'
abline(a=0, b=1)

df1=rgir2005[which(rgir2005$MnTrIR>-9999 & rgir2005$MnTrIR<9999),]

mod0=lm(df1$MnTrIR ~ df1$TrMnIR)
summary(mod0)
confint(mod0, level=0.95)

diff=df1$MnTrIR-(df1$TrMnIR-0.172)
ttest0=lm(diff~1)
summary(ttest0)

df1[which(diff>0.2 | diff<(-0.2)),]
hist(ircty$TrIR[which(ircty$AnimGroup==1 & ircty$GERegion==6)])

aggbs=data.frame(rgir2005$GERegion, rgir2005$AnimGroup)
aggbs$AggBias_1=ifelse(is.na(rgir2005$MnTrIR)==FALSE, rgir2005$MnTrIR-rgir2005$TrMnIR, NA)
aggbs$AggBias_2=ifelse(is.na(rgir2005$MnTrIR)==FALSE, rgir2005$MnTrIR-rgir2005$TrMnIR, -0.172)
names(aggbs)=c("GERegion", "AnimGroup", "AggBias_1", "AggBias_2")

remove(df1, diff, mod0, ttest0)


'*** Calculate (regional) intensification rates in 1982 and 1992 ***'

df0=read.csv("../Data/WLPS_PS-Structure.csv")

psw1=df0[, c(1:5,6)]
names(psw1)[6]="PSShare"
psw1$WLPSPS=3
psw1$LabWLPSPS="Landless"

psw2=df0[, c(1:5,7)]
names(psw2)[6]="PSShare"
psw2$WLPSPS=2
psw2$LabWLPSPS="Mixed"

psw3=df0[, c(1:5,8)]
names(psw3)[6]="PSShare"
psw3$WLPSPS=1
psw3$LabWLPSPS="Grassland"

psw4=rbind(psw1, psw2, psw3)

psw5=psw4[which(is.na(psw4$GERegion)==FALSE & psw4$Year==1982), c(1:3,7:8,6)]
names(psw5)[6]="PSShare_82"

psw6=psw4[which(is.na(psw4$GERegion)==FALSE & psw4$Year==1992), c(1:3,7:8,6)]
names(psw6)[6]="PSShare_92"

psw7=merge(psw5, psw6)

pswlps=psw7[order(psw7$AnimGroup, psw7$GERegion, psw7$WLPSPS),]

write.csv(pswlps, file="output/WLPS_PS-Composition.csv", row.names=FALSE)

remove(psw1, psw2, psw3, psw4, psw5, psw6, psw7)
 
ps1a12=df0[which(is.element(df0$AnimGroup, c(1,2)) & is.element(df0$GERegion, c(1:7)) ),]
ps1a12$StdPS=1
ps1a12$AnimShare=ifelse(ps1a12$LG>0, ps1a12$LG, 0)

ps2a12=df0[which(is.element(df0$AnimGroup, c(1,2)) & is.element(df0$GERegion, c(1:7)) ),]
ps2a12$StdPS=2
ps2a12$AnimShare=ps2a12$LL+ps2a12$M

ps1a34=df0[which(is.element(df0$AnimGroup, c(3,4)) & is.element(df0$GERegion, c(1:7)) ),]
ps1a34$StdPS=1
ps1a34$AnimShare=ifelse(ps1a34$LG+ps1a34$M>0, ps1a34$LG+ps1a34$M, 0)

ps2a34=df0[which(is.element(df0$AnimGroup, c(3,4)) & is.element(df0$GERegion, c(1:7)) ),]
ps2a34$StdPS=2
ps2a34$AnimShare=ifelse(ps2a34$LL>0, ps2a34$LL, 0)

df1=rbind(ps1a12, ps2a12, ps1a34, ps2a34)[,c(1:5,9:10)]
df1=df1[order(df1$GERegion, df1$AnimGroup, df1$Year, df1$StdPS),]

df2=merge(df1, regpsir, all=TRUE) 
write.csv(df2, file="output/check-2.csv", row.names=FALSE)

agg1=aggregate(df2$AnimShare*df2$IR, list(df2$GERegion, df2$AnimGroup, df2$Year), FUN=sum)
names(agg1)=c("GERegion", "AnimGroup", "Year", "MnIR")

agg1$TrMnIR=log(agg1$MnIR/(1-agg1$MnIR))

macir=rbind(agg1, rgir2005[,c(1:3,5:6)])

macir[order(macir$AnimGroup, macir$GERegion, macir$Year),]

df3=merge(macir, rgrwgt, all.x=TRUE)
df4=merge(df3, aggbs, all.x=TRUE)

df4$AdjTrIR_1=df4$TrMnIR+df4$AggBias_1
df4$AdjTrIR_2=df4$TrMnIR+df4$AggBias_2
df4$AREA=999

df5=df4[,c(1,11,2:3,4,9,6)]
names(df5)[5:6]=c("IR", "TrIR")

df6=ircty
df6$Year=2005
df6$RegrWgt=1

df7=rbind(df5, df6[,c(1:3,6,4:5,7)])
df7=df7[order(df7$AnimGroup, df7$GERegion, df7$Year, df7$AREA),]

areareg=read.csv("../Data/area-regions-registry_6-10.csv")[,c(1,7)]

df8=merge(df7, areareg, all.x=TRUE)

irpnl0=df8[,c(2,1,8,3,4,7,5:6)]

write.csv(irpnl0[order(irpnl0$GERegion, irpnl0$AnimGroup, irpnl0$AREA, irpnl0$Year),], file="output/irpnl0.csv", row.names=FALSE)


'* irpnl#: Panel used for the regressions. More columns will be matched for additional variables. *'

remove(agg1, aggbs, rgrwgt, rgir2005, df0, df1, df2, df3, df4, df5, df6, df7, df8, ps1a12, ps2a12, ps1a34, ps2a34, ircty, macir)


'*** Match independent variables ***'

'* Labour productivity in agriculture, from WDI *'

df0=read.csv("../Data/WDI-Productivity_6-12.csv")

lstcol=5+lstyear-1980

df1=df0[which(as.character(df0[,4])=="EA.PRD.AGRI.KD"), c(2,5:lstcol)]
names(df1)[1]="UNCode"

areareg=read.csv("../Data/area-regions-registry_6-10.csv")[,c(7,1)]
areareg=areareg[which(areareg$AREA!=276),]

df2=merge(df1, areareg, all.x=TRUE)

wblapro=df2

ctyreg=data.frame(wblapro$UNCode, wblapro$AREA)
names(ctyreg)=c("UNCode", "AREA")

remove(df0, df1, df2)

ctyreg[which(is.na(ctyreg$AREA)),]

mn82=apply(wblapro[,3:5], 1, mean, na.rm=TRUE)
lpr82=data.frame(ctyreg, mn82)
names(lpr82)=c("UNCode", "AREA", "LabProd")
lpr82$Year=1982

mn92=apply(wblapro[,13:15], 1, mean, na.rm=TRUE)
lpr92=data.frame(ctyreg, mn92)
names(lpr92)=c("UNCode", "AREA", "LabProd")
lpr92$Year=1992

mn05=apply(wblapro[,26:28], 1, mean, na.rm=TRUE)
lpr05=data.frame(ctyreg, mn05)
names(lpr05)=c("UNCode", "AREA", "LabProd")
lpr05$Year=2005

df0=aggrost
df1=rbind(lpr82, lpr92, lpr05)[,c(2:4)]

remove(mn82, mn92, mn05, lpr82, lpr92, lpr05, ctyreg, lstcol)

for (i in 1:3) {

  ctyvar=paste("Area_", i, sep="")
  objvar=paste("LabProd_", i, sep="")
  names(df1)=c(ctyvar, objvar, "Year")

  df2=merge(df0, df1, all.x=TRUE)
  
  df0=df2  

}

remove(i, ctyvar, objvar)

df2$LabProd=ifelse(is.na(df2$LabProd_3)==FALSE, df2$LabProd_3, ifelse(is.na(df2$LabProd_2)==FALSE, df2$LabProd_2, df2$LabProd_1))

df2$LogLP=log(df2$LabProd)
df3=df2[which(is.nan(df2$LogLP)==FALSE & is.na(df2$LogLP)==FALSE),]

agg1=aggregate(df3$WgtTr, list(df3$AnimGroup, df3$GERegion, df3$Area_3), FUN=length)
names(agg1)=c("AnimGroup", "GERegion", "Area_3", "Obs")

df4=merge(df2, agg1, all.x=TRUE)
df4=df4[order(df4$AnimGroup, df4$GERegion, df4$Area_3, df4$Year),]

df5=df4[which(df4$Obs==3 | (df4$Area_1==228 & df4$Obs==2)),]

write.csv(df5, file="output/check-5.csv", row.names=FALSE)

df6=df5[which(is.element(df5$Year, c(1982, 1992, 2005))),]

agg2=aggregate(df6$WgtTr, list(df6$AnimGroup, df6$GERegion, df6$Year), FUN=sum) 
names(agg2)=c("AnimGroup", "GERegion", "Year", "CovLP")

agg3=aggregate(df6$WgtTr*df6$LogLP, list(df6$AnimGroup, df6$GERegion, df6$Year), FUN=sum) 
names(agg3)=c("AnimGroup", "GERegion", "Year", "SumLogLP")

df7=merge(agg2, agg3)

is.na(df7$SumLogLP[which(df7$GERegion==6 & df7$Year==1982)])=TRUE
df7$LogLP=df7$SumLogLP/df7$CovLP
df7$AREA=999

lpmacro=df7[,c(2,7,1,3,4,6)]

df8=df5[which(df5$Year==2005), c(2,1,3,4,13)]
names(df8)[2]="AREA"
df8$CovLP=NA

df9=rbind(lpmacro, df8[,c(1:4,6,5)])

irpnl1=merge(irpnl0, df9, all.x=TRUE)

write.csv(irpnl1[order(irpnl1$GERegion, irpnl1$AnimGroup, irpnl1$AREA, irpnl1$Year),], file="output/irpnl1.csv", row.names=FALSE)

remove(df0, df1, df2, df3, df4, df5, df6, df7, df8, df9, rst, agg1, agg2, agg3, irpnl0, lpmacro)


'* Livestock Density, from FAOSTAT *'

df0=read.csv("../Data/area-regions-registry_6-10.csv")[,c(11,3:4,9:10)]
ctyreg=unique(df0)
remove(df0)

df0=read.csv("../Data/pmp-series_6-12.csv")
df1=df0[,c(3,9,11)]
names(df1)=c("Country", "Year", "pmp")

df1[1:100,]

df2=merge(ctyreg, df1)


'* Carry forward the last value of each time series until the last year (lastyear)*'

agg1=aggregate(df2$Year, list(df2$Country), FUN=max)
names(agg1)=c("Country", "maxyr")

df3=merge(df2, agg1, all.x=TRUE)

add1=df3[which(df3$Year==df3$maxyr & df3$Year<lstyear & (df3$Year>df3$endyr | is.na(df3$endyr))),]
add0=df3[which(FALSE),]

while (nrow(add1)>0){
  
  add1$Year=add1$Year+1
  
  add2=rbind(add0, add1)
  add0=add2
  
  add3=add1[which(add1$Year<lstyear & (add1$Year>add1$endyr | is.na(add1$endyr))),]
  add1=add3
  
  nrow(add1)
  
}

df4=rbind(df3, add0)

dfpmp=df4[order(df4$Country, df4$Year) ,c(1,4:7)]

remove(df0, df1, df2, df3, df4, agg1, add0, add1, add2, add3)

df1=read.csv("../Data/faostat_animals.csv")[,c(1,3,6:7)]
names(df1)[1:2]=c("AREA", "ITEM")

df2=df1[which(is.element(df1$ITEM, c(866, 976, 1016)) & df1$Year>=1980 & df1$Year<=lstyear),]

animreg=read.csv("../Data/animal-registry.csv")

df3=merge(df2, animreg, all.x=TRUE)

anim0=aggregate(df3[,4], df3[,c(2,3,5)], FUN=sum)
names(anim0)[4]="animals"

areacty=read.csv("../Data/area-regions-registry_6-10.csv")[,c(1,11)]

anim1=merge(anim0, areacty, all.x=TRUE)

anim2=aggregate(anim1$animals, anim1[,c(5,3,2)], FUN=sum, na.rm=TRUE)
names(anim2)[4]="animals"

df4=merge(dfpmp, anim2)

df4$LSDens=df4$animals/df4$pmp

write.csv(df4[order(df4$GERegion, df4$Country, df4$AnimGroup, df4$Year), c(3,1,6,2,5,7,8)], file="output/check_lsd.csv", row.names=FALSE)

df5=df4[,c(3:4,1,6,2,8)]

df6=merge(df5, areacty, all.x=TRUE)

lsdbase=df6[order(df6$AnimGroup, df6$Year, df6$Country, df6$AREA), c(2,1,7,4:6)]

remove(df1, df2, df3, df4, df5, df6, anim0, anim1, anim2, areacty, ctyreg, dfpmp)

df1=lsdbase[which(is.element(lsdbase$Year, c(1982,1992,2005))), c(3:6)]
df0=aggrost

for (i in 1:3) {

  ctyvar=paste("Area_", i, sep="")
  objvar=paste("LSDens_", i, sep="")
  names(df1)=c(ctyvar, "AnimGroup", "Year", objvar)

  df2=merge(df0, df1, all.x=TRUE)

  df0=df2  

}

remove(i, ctyvar, objvar)

df2$LSDens=ifelse(is.na(df2$LSDens_3)==FALSE, df2$LSDens_3, ifelse(is.na(df2$LSDens_2)==FALSE, df2$LSDens_2, df2$LSDens_1))
df2$LogLSDens=log(df2$LSDens)

df3=df2[which(is.na(df2$LogLSDens)==FALSE),]

agg1=aggregate(df3$LSDens, list(df3$Area_3, df3$AnimGroup), FUN=length)
names(agg1)=c("Area_3", "AnimGroup", "Obs")

df4=merge(df2, agg1, all.x=TRUE)

df5=df4[which(df4$Obs==3),]

micro=df5
mic2005=df5[which(df5$Year==2005),c(6,3,1,2,12:13)]

remove(df0, df1, df2, df3, df4, df5, agg1)

agg1=aggregate(micro$WgtTr, list(micro$AnimGroup, micro$GERegion, micro$Year), FUN=sum)
names(agg1)=c("AnimGroup", "GERegion", "Year", "CovLSDens")

agg2=aggregate(micro$WgtTr*micro$LSDens, list(micro$AnimGroup, micro$GERegion, micro$Year), FUN=sum)
names(agg2)=c("AnimGroup", "GERegion", "Year", "SumLSDens")

agg3=aggregate(micro$WgtTr*micro$LogLSDens, list(micro$AnimGroup, micro$GERegion, micro$Year), FUN=sum)
names(agg3)=c("AnimGroup", "GERegion", "Year", "SumLogLSDens")

df1=merge(agg1, agg2)
df2=merge(df1, agg3)

df2$LSDens=df2$SumLSDens/df2$CovLSDens
df2$LogLSDens=df2$SumLogLSDens/df2$CovLSDens
df2$AREA=999

macro=df2[,c(2,9,1,3,4,7:8)]

remove(df1, df2, agg1, agg2, agg3)

mic2005$CovLSDens=NA
names(mic2005)[4]="AREA"

df1=rbind(macro, mic2005[,c(1,4,3,2,7,5:6)])

irpnl2=merge(irpnl1, df1, all.x=TRUE)

write.csv(irpnl2[order(irpnl2$GERegion, irpnl2$AREA, irpnl2$AnimGroup, irpnl2$Year),], file="output/irpnl2.csv", row.names=FALSE)

remove(df1, micro, macro, mic2005, irpnl1)


'* Proportion of Muslims and Hindus, from the Pew Research Center *'

df1=read.csv("../Data/Religions.csv")[,c(1,4,6)]
df0=aggrost

for (i in 1:3) {

  ctyvar=paste("Area_", i, sep="")
  objvar1=paste("Muslim_", i, sep="")
  objvar2=paste("Hindu_", i, sep="")
  names(df1)=c(ctyvar, objvar1, objvar2)

  df2=merge(df0, df1, all.x=TRUE)

  df0=df2  

}

remove(i, ctyvar, objvar1, objvar2)

df2$Muslim=ifelse(is.na(df2$Muslim_3)==FALSE, df2$Muslim_3, ifelse(is.na(df2$Muslim_2)==FALSE, df2$Muslim_2, df2$Muslim_1))
df2$TrMuslim=ifelse(df2$Muslim>0, ifelse(df2$Muslim<1, log(df2$Muslim/(1-df2$Muslim)), log(1/0.001)), log(0.001))
df2$DumMuslim=(df2$Muslim>0.5)*1

df2$Hindu=ifelse(is.na(df2$Hindu_3)==FALSE, df2$Hindu_3, ifelse(is.na(df2$Hindu_2)==FALSE, df2$Hindu_2, df2$Hindu_1))
df2$TrHindu=ifelse(df2$Hindu>0, ifelse(df2$Hindu<1, log(df2$Hindu/(1-df2$Hindu)), log(1/0.001)), log(0.001))
df2$DumHindu=(df2$Hindu>0.5)*1

micro=df2[, c(4,1,6,7,8,16:17,19:20)]
mic2005=df2[which(df2$Year==2005), c(4,1,6,7,8,16:17,19:20)]

remove(df0, df1, df2)

df0=micro[which(is.na(micro$TrMuslim)==FALSE),]

agg1=aggregate(df0$WgtTr, list(df0$AnimGroup, df0$GERegion, df0$Year), FUN=sum)
names(agg1)=c("AnimGroup", "GERegion", "Year", "CovMuslim")

agg2=aggregate(df0$WgtTr*df0$TrMuslim, list(df0$AnimGroup, df0$GERegion, df0$Year), FUN=sum)
names(agg2)=c("AnimGroup", "GERegion", "Year", "SumTrMuslim")

agg3=aggregate(df0$WgtTr*df0$DumMuslim, list(df0$AnimGroup, df0$GERegion, df0$Year), FUN=sum)
names(agg3)=c("AnimGroup", "GERegion", "Year", "SumDumMuslim")

df1=merge(agg1, agg2)
df2=merge(df1, agg3)

df2$TrMuslim=df2$SumTrMuslim/df2$CovMuslim
df2$DumMuslim=df2$SumDumMuslim/df2$CovMuslim
df2$AREA=999

aggmusl=df2[,c(2,9,1,3,4,7:8)]

micmusl=mic2005[,c(1:4,6:7)]
names(micmusl)[2]="AREA"
micmusl$CovMuslim=NA

df3=rbind(aggmusl, micmusl[,c(1:4,7,5:6)])
irpnl3=merge(irpnl2, df3, all.x=TRUE)

remove(df0, df1, df2, df3, agg1, agg2, agg3, aggmusl, micmusl, irpnl2)


df0=micro[which(is.na(micro$TrHindu)==FALSE),]

agg1=aggregate(df0$WgtTr, list(df0$AnimGroup, df0$GERegion, df0$Year), FUN=sum)
names(agg1)=c("AnimGroup", "GERegion", "Year", "CovHindu")

agg2=aggregate(df0$WgtTr*df0$TrHindu, list(df0$AnimGroup, df0$GERegion, df0$Year), FUN=sum)
names(agg2)=c("AnimGroup", "GERegion", "Year", "SumTrHindu")

agg3=aggregate(df0$WgtTr*df0$DumHindu, list(df0$AnimGroup, df0$GERegion, df0$Year), FUN=sum)
names(agg3)=c("AnimGroup", "GERegion", "Year", "SumDumHindu")

df1=merge(agg1, agg2)
df2=merge(df1, agg3)

df2$TrHindu=df2$SumTrHindu/df2$CovHindu
df2$DumHindu=df2$SumDumHindu/df2$CovHindu
df2$AREA=999

agghind=df2[,c(2,9,1,3,4,7:8)]

michind=mic2005[,c(1:4,8:9)]
names(michind)[2]="AREA"
michind$CovHindu=NA

df3=rbind(agghind, michind[,c(1:4,7,5:6)])

irpnl4=merge(irpnl3, df3, all.x=TRUE)

remove(df0, df1, df2, df3, agg1, agg2, agg3, agghind, michind, irpnl3)

remove(micro, mic2005)

write.csv(irpnl4[order(irpnl4$GERegion, irpnl4$AREA, irpnl4$AnimGroup, irpnl4$Year),], file="output/basepanel.csv", row.names=FALSE)


'*** Estimate the coefficients ***'

'* Differencing *'

df0=irpnl4[which(irpnl4$AREA==999),]

agg1=aggregate(df0$TrIR, list(df0$GERegion, df0$AnimGroup), FUN=mean)
names(agg1)=c("GERegion", "AnimGroup", "TMnTrIR")

agg2=aggregate(df0$LogLP, list(df0$GERegion, df0$AnimGroup), FUN=mean, na.rm=TRUE)
names(agg2)=c("GERegion", "AnimGroup", "TMnLogLP")

agg3=aggregate(df0$LogLSDens, list(df0$GERegion, df0$AnimGroup), FUN=mean, na.rm=TRUE)
names(agg3)=c("GERegion", "AnimGroup", "TMnLogLSDens")

agg4=aggregate(df0$DumMuslim, list(df0$GERegion, df0$AnimGroup), FUN=mean, na.rm=TRUE)
names(agg4)=c("GERegion", "AnimGroup", "TMnDumMuslim")

agg5=aggregate(df0$DumHindu, list(df0$GERegion, df0$AnimGroup), FUN=mean, na.rm=TRUE)
names(agg5)=c("GERegion", "AnimGroup", "TMnDumHindu")

df1=merge(irpnl4, agg1, all.x=TRUE)
df2=merge(df1, agg2, all.x=TRUE)
df3=merge(df2, agg3, all.x=TRUE)
df4=merge(df3, agg4, all.x=TRUE)
df5=merge(df4, agg5, all.x=TRUE)

df5$DiffTrIR=df5$TrIR-df5$TMnTrIR
df5$DiffLogLP=df5$LogLP-df5$TMnLogLP
df5$DiffLogLSDens=df5$LogLSDens-df5$TMnLogLSDens
df5$DiffDumMuslim=df5$DumMuslim-df5$TMnDumMuslim
df5$DiffDumHindu=df5$DumHindu-df5$TMnDumHindu

basepnl=df5[,c(1:5,6,7:8,25,9:10,26,11,13,27,14,16,28,17,19,29)]

write.csv(basepnl[order(basepnl$GERegion, basepnl$AnimGroup, basepnl$AREA, basepnl$Year),], file="output/basepnl.csv", row.names=FALSE)

df6=merge(agg1, agg3)
df7=merge(df6, agg2)

sermns=df7

remove(agg1, agg2, agg3, agg4, agg5, df0, df1, df2, df3, df4, df5, df6, df7)


'*** Functions for storing the regression output ***'

regcf=function(regmod){

  cf=coef(summary(regmod))

  res0=vector(length=0)
  cfnm0=vector(length=0)
  lab0=vector(length=0)

  for (i in 1:nrow(cf)) {
 
    res1=c(res0, cf[i,1:3])
    cfnm1=c(cfnm0, row.names(cf)[i], row.names(cf)[i], row.names(cf)[i])
    lab1=c(lab0, "est", "stderr", "tval")

    res0=res1
    cfnm0=cfnm1
    lab0=lab1 

  }

  out=data.frame(cfnm0, lab0, res0)
  names(out)=c("Coeff", "Stat", "Value")
  row.names(out)=NULL

  return(out)

}

regsum=function(regmod, obsunit){

  lab0=c("R2_wgt", "R2_unwgt", "Obs", "Units")
  res0=vector(length=4)

  depvar=regmod$fitted.values+regmod$residuals

  'R2 from the regression (calculated on the means in the pseudo panel)'
  tss=sum((depvar-mean(depvar))^2)
  rss=sum(regmod$residuals^2)
  res0[1]=1-rss/tss

  'Adjusted R2 (as it would be with the micro-data)'
  wtss=sum(regmod$weights*(depvar-mean(depvar))^2)
  wrss=sum(regmod$weights*regmod$residuals^2)
  res0[2]=1-wrss/wtss

  res0[3]=length(regmod$fitted.values[which(is.na(regmod$fitted.values)==FALSE)])
  res0[4]=length(unique(obsunit[which(is.na(regmod$fitted.values)==FALSE)]))

  out=data.frame(lab0, res0)
  names(out)=c("Stat", "Value")
  return(out)

}

basepnl$CompIdx=0.5*(basepnl$DiffLogLP+basepnl$DiffLogLSDens)


'* Cattle *'

'subset=basepnl[which(basepnl$AnimGroup==1 & (is.element(basepnl$Year,c(1982, 1992)) | (basepnl$Year==2005 & basepnl$AREA!=999)) & is.element(basepnl$GERegion,c(1,2,3,4,5,6,7))),]'
subset=basepnl[which(basepnl$AnimGroup==1 & basepnl$AREA==999 & is.element(basepnl$GERegion,c(1,2,3,4,5,6,7))),]

ptype=ifelse(subset$Year==2005, 19,1)

plot(subset$LogLP, subset$TrIR, col=subset$GERegion, pch=ptype)
legend(0.9, -0.7, c("1-SSA", "2-Asia", "3-CSA", "4-WANA", "5-OECD", "6-EE/CIS", "7-oDev"), pch=c(1,1,1), col=c(1,2,3,4,5,6,7))

plot(subset$LogLSDens, subset$TrIR, col=subset$GERegion, pch=ptype)
legend(0.9, -0.7, c("1-SSA", "2-Asia", "3-CSA", "4-WANA", "5-OECD", "6-EE/CIS", "7-oDev"), pch=c(1,1,1), col=c(1,2,3,4,5,6,7))

mod0=lm(DiffTrIR ~ 0+DiffLogLSDens, data=subset, weights=RegrWgt)
summary(mod0)

t0=lm(DiffTrIR ~ 0+DiffLogLSDens+Year, data=subset, weights=RegrWgt)
summary(t0)

t1=lm(DiffTrIR ~ 0+DiffLogLSDens+(Year==2005), data=subset, weights=RegrWgt)
summary(t1)

rgres1=regcf(regmod=mod0)
rgres1$AnimGroup=1

rgsum1=regsum(regmod=mod0, obsunit=subset$GERegion)
rgsum1$AnimGroup=1


'* Sheep and goats *'

unique(basepnl[which(basepnl$IR==0), c(1:2)])

'subset=basepnl[which(basepnl$AnimGroup==2 & (is.element(basepnl$Year,c(1982, 1992)) | (basepnl$Year==2005 & basepnl$AREA!=999)) & is.element(basepnl$GERegion,c(1,2,3,4,5,6,7)) & basepnl$IR>0),]'
subset=basepnl[which(basepnl$AnimGroup==2 & basepnl$AREA==999 & is.element(basepnl$GERegion,c(1,2,3,4,5,6,7)) & basepnl$IR>0),]
'subset=basepnl[which(basepnl$AnimGroup==2 & basepnl$AREA==999 & is.element(basepnl$GERegion,c(1,2,3,4,5,7)) & basepnl$IR>0),]'

subset=subset[order(subset$GERegion, subset$Year),]

'sbs=subset[which(subset$GERegion==6),]
plot(sbs$Year, sbs$DiffTrIR, col=1, type="l", ylim=c(-0.5, 0.5))
lines(sbs$Year, sbs$DiffLogLP, col=2)
lines(sbs$Year, sbs$DiffLogLSDens, col=3)
legend(2000, -0.2, c("IR", "LP", "LSD"), pch=c(1,1,1), col=c(1,2,3))'

subset$AnimGroup[1]

ptype=ifelse(subset$Year==2005, 19,1)

plot(subset$DiffLogLP, subset$DiffTrIR, col=subset$GERegion, pch=ptype)
legend(-0.3, 0.08, c("1-SSA", "2-Asia", "3-CSA", "4-WANA", "5-OECD", "6-EE/CIS", "7-oDev"), pch=c(1,1,1), col=c(1,2,3,4,5,6,7))

plot(subset$DiffLogLSDens, subset$DiffTrIR, col=subset$GERegion, pch=ptype)
legend(0.2, -0.15, c("1-SSA", "2-Asia", "3-CSA", "4-WANA", "5-OECD", "6-EE/CIS", "7-oDev"), pch=c(1,1,1), col=c(1,2,3,4,5,6,7))

mod0=lm(DiffTrIR ~ 0+DiffLogLP, data=subset, weights=RegrWgt)
summary(mod0)

t0=lm(DiffTrIR ~ 0+DiffLogLP+Year, data=subset, weights=RegrWgt)
summary(t0)

t1=lm(DiffTrIR ~ 0+DiffLogLP+(Year==2005), data=subset, weights=RegrWgt)
summary(t1)

rgres2=regcf(regmod=mod0)
rgres2$AnimGroup=2

rgsum2=regsum(regmod=mod0, obsunit=subset$GERegion)
rgsum2$AnimGroup=2


'* Pigs *'

'subset=basepnl[which(basepnl$AnimGroup==3 & (is.element(basepnl$Year,c(1982, 1992)) | (basepnl$Year==2005 & basepnl$AREA!=999)) & is.element(basepnl$GERegion,c(1,2,3,4,5,6,7)) & basepnl$IR>0),]'
subset=basepnl[which(basepnl$AnimGroup==3 & basepnl$AREA==999 & is.element(basepnl$GERegion,c(1,2,3,4,5,6,7)) & basepnl$IR>0),]

subset$AnimGroup[1]

ptype=ifelse(subset$Year==2005, 19,1)

plot(subset$DiffLogLP, subset$DiffTrIR, col=subset$GERegion, pch=ptype)
legend(0.9, -0.7, c("1-SSA", "2-Asia", "3-CSA", "4-WANA", "5-OECD", "6-EE/CIS", "7-oDev"), pch=c(1,1,1), col=c(1,2,3,4,5,6,7))

mod0=lm(DiffTrIR ~ 0+DiffLogLP, data=subset, weights=RegrWgt)
summary(mod0)

t0=lm(DiffTrIR ~ 0+DiffLogLP+Year, data=subset, weights=RegrWgt)
summary(t0)

t1=lm(DiffTrIR ~ 0+DiffLogLP+(Year==2005), data=subset, weights=RegrWgt)
summary(t1)

rgres3=regcf(regmod=mod0)
rgres3$AnimGroup=3

rgsum3=regsum(regmod=mod0, obsunit=subset$GERegion)
rgsum3$AnimGroup=3


'* Poultry *'

'subset=basepnl[which(basepnl$AnimGroup==4 & (is.element(basepnl$Year,c(1982, 1992)) | (basepnl$Year==2005 & basepnl$AREA!=999)) & is.element(basepnl$GERegion,c(1,2,3,4,5,6,7)) & basepnl$IR>0),]'
subset=basepnl[which(basepnl$AnimGroup==4 & basepnl$AREA==999 & is.element(basepnl$GERegion,c(1,2,3,4,5,6,7)) & basepnl$IR>0),]

subset$AnimGroup[1]

ptype=ifelse(subset$Year==2005, 19,1)

plot(subset$LogLP, subset$TrIR, col=subset$GERegion, pch=ptype)
legend(0.9, -0.7, c("1-SSA", "2-Asia", "3-CSA", "4-WANA", "5-OECD", "6-EE/CIS", "7-oDev"), pch=c(1,1,1), col=c(1,2,3,4,5,6,7))

plot(subset$DiffLogLP, subset$DiffTrIR, col=subset$GERegion, pch=ptype)
legend(0.9, -0.7, c("1-SSA", "2-Asia", "3-CSA", "4-WANA", "5-OECD", "6-EE/CIS", "7-oDev"), pch=c(1,1,1), col=c(1,2,3,4,5,6,7))

mod0=lm(DiffTrIR ~ 0+DiffLogLP, data=subset, weights=RegrWgt)
summary(mod0)

t0=lm(DiffTrIR ~ 0+DiffLogLP+Year, data=subset, weights=RegrWgt)
summary(t0)

t1=lm(DiffTrIR ~ 0+DiffLogLP+(Year==2005), data=subset, weights=RegrWgt)
summary(t1)

rgres4=regcf(regmod=mod0)
rgres4$AnimGroup=4

rgsum4=regsum(regmod=mod0, obsunit=subset$GERegion)
rgsum4$AnimGroup=4

remove(subset, mod0, ptype, t0, t1)


'* Merge the regression results to a single parameter table'

df0=rbind(rgres1, rgres2, rgres3, rgres4)

v1=unique(df0$Coeff)
v2=unique(df0$Stat)
v3=unique(df0$AnimGroup)

df1=merge(v3, v1)
names(df1)=c("AnimGroup", "Coeff")

df2=merge(df1, v2)
names(df2)=c("AnimGroup", "Coeff", "Stat")

df3=merge(df2, df0, all.x=TRUE)
df3$Value[which(is.na(df3$Value & df3$Stat=="est"))]=0

param=df3

df4=rbind(rgsum1, rgsum2, rgsum3, rgsum4)
rgdesc=df4[,c(3,1:2)]

remove(v1, v2, v3, df0, df1, df2, df3, df4, rgres1, rgres2, rgres3, rgres4, rgsum1, rgsum2, rgsum3, rgsum4)

write.csv(param, file="output/param.csv", row.names=FALSE)
write.csv(rgdesc, file="output/rgdesc.csv", row.names=FALSE)


'*** Prediction ***'

'* Create the roster for prediction *' 

df0=read.csv("../Data/area-regions-registry_6-10.csv")[,c(9,1:4)]
df1=df0[which(df0$endyr>2010 | is.na(df0$endyr)),]
names(df1)[2]="Area_3"

df2=unique(ctyrost[,c(3,5,7)])
df2[order(df2$Area_3),]

df3=merge(df1, df2, all.x=TRUE)

df4=merge(df3, c(1:4))
names(df4)[8]="AnimGroup"

df5=merge(df4, c(1990:lstyear))
names(df5)[9]="Year"

df6=df5[which((df5$Year>=df5$startyr | is.na(df5$startyr)) & (df5$Year<=df5$endyr | is.na(df5$endyr))), c(2,6:7,1,3,8,9)]

df6$Area_2=ifelse(is.na(df6$Area_2), df6$Area_3, df6$Area_2)
df6$Area_1=ifelse(is.na(df6$Area_1), df6$Area_2, df6$Area_1)

prdrost=df6[order(df6$GERegion, df6$Area_3, df6$AnimGroup, df6$Year),]

remove(df0, df1, df2, df3, df4, df5, df6)


'* Match the data of the independent variables *'

# Livestock Density

df0=prdrost
df1=lsdbase[,c(1,3:6)]

for (i in 1:3) {

  ctyvar=paste("Area_", i, sep="")
  checkvar=paste("LSDens_", i, sep="")
  names(df1)[c(2,5)]=c(ctyvar, checkvar)

  df2=merge(df0, df1, all.x=TRUE)
  
  df0=df2  

}

df2$LSDens=ifelse(is.na(df2$LSDens_3), ifelse(is.na(df2$LSDens_2), df2$LSDens_3 , df2$LSDens_2), df2$LSDens_3)

prdpnl1=df2[,c(1,6,5,4,7,2,3,11)]

prdpnl1$LogLSD=ifelse(prdpnl1$LSDens>0, log(prdpnl1$LSDens), NA)

remove(df0, df1, df2, df3)


# Labour Productivity

ncol(wblapro)

df0=wblapro[, c(lstyear-1980+3, 1990-1980+2)]
names(df0)[2]="LabProd"
df0$Year=1990

for (i in 1991:lstyear){

  df1=wblapro[, c(lstyear-1980+3, i-1980+2)]
  names(df1)[2]="LabProd"
  df1$Year=i

  df2=rbind(df0, df1)

  df0=df2

}


df3=prdpnl1

for (i in 1:3) {

  ctyvar=paste("Area_", i, sep="")
  checkvar=paste("LabProd_", i, sep="")
  names(df2)[1:2]=c(ctyvar, checkvar)

  df4=merge(df3, df2, all.x=TRUE)
  
  df3=df4  

}

df4$LabProd=ifelse(is.na(df4$LabProd_3), ifelse(is.na(df4$LabProd_2), df4$LabProd_3 , df4$LabProd_2), df4$LabProd_3)

prdpnl2=df4[order(df4$Area_3, df4$AnimGroup, df4$Year), c(5,4,3,2,6,7,1,8:9,13)]
prdpnl2$LogLP=ifelse(prdpnl2$LabProd>0, log(prdpnl2$LabProd), NA)

remove(df0, df1, df2, df3, df4, i, ctyvar, checkvar, prdpnl1)


# Variables for the calculation of fixed effects

prdpnl2$LogLSD=ifelse(is.element(prdpnl2$AnimGroup, c(2,3,4)), 999, prdpnl2$LogLSD)
prdpnl2$LogLP=ifelse(is.element(prdpnl2$AnimGroup, c(1)), 999, prdpnl2$LogLP)

df0=merge(prdpnl2, sermns, all.x=TRUE)

df0$TMnLogLSDens=ifelse(is.element(df0$AnimGroup, c(2,3,4)), 999, df0$TMnLogLSDens)
df0$TMnLogLP=ifelse(is.element(df0$AnimGroup, c(1)), 999, df0$TMnLogLP)

df0[1:5,]

df1=prdpnl2[which(prdpnl2$Year==2005), c(4,6,9)]
names(df1)[3]="LogLSD_2005"

df2=prdpnl2[which(prdpnl2$Year==2005), c(4,6,11)]
names(df2)[3]="LogLP_2005"

df3=merge(df0, df1, all.x=TRUE)
df4=merge(df3, df2, all.x=TRUE)

df4=df4[order(df4$GERegion, df4$Area_3, df4$AnimGroup, df4$Year),]

df5=basepnl[which(basepnl$AREA!=999), c(3,2,7:8)]

names(df5)[1]="Area_3"
names(df5)[3]="IR_2005"
names(df5)[4]="TrIR_2005"

df6=merge(df4, df5, all.x=TRUE)

df6[1:5,]

prdpnl3=df6[order(df6$GERegion, df6$Area_3, df6$AnimGroup, df6$Year), c(3,4,5,2,6,1,7,8:18)]

remove(df0, df1, df2, df3, df4, df5, df6, sermns, prdpnl2)


# Parameters

df0=param[which(param$Coeff=="DiffLogLSDens" & param$Stat=="est"), c(1,4)]
names(df0)[2]="CfLogLSD"

df1=param[which(param$Coeff=="DiffLogLP" & param$Stat=="est"), c(1,4)]
names(df1)[2]="CfLogLP"

df2=merge(prdpnl3, df0, all.x=TRUE)
df3=merge(df2, df1, all.x=TRUE)

prdpnl4=df3[order(df3$GERegion, df3$Area_3, df3$AnimGroup, df3$Year),]

write.csv(prdpnl4, file="output/prdpnl4.csv", row.names=FALSE)

remove(df0, df1, df2, df3, prdpnl3)


'*** Calculate (transformed) IRs ***'

prdpnl4$CFixEff=prdpnl4$TrIR_2005-prdpnl4$CfLogLSD*prdpnl4$LogLSD_2005-prdpnl4$CfLogLP*prdpnl4$LogLP_2005
prdpnl4$RFixEff=prdpnl4$TMnTrIR-prdpnl4$CfLogLSD*prdpnl4$TMnLogLSDens-prdpnl4$CfLogLP*prdpnl4$TMnLogLP

write.csv(unique(prdpnl4[,c(1:2,5,21:22)]), file="output/check2.csv", row.names=FALSE)

prdpnl4$TrIR_est1=ifelse(prdpnl4$Year==2005 & is.na(prdpnl4$TrIR_2005)==FALSE, prdpnl4$TrIR_2005, prdpnl4$CFixEff+prdpnl4$CfLogLSD*prdpnl4$LogLSD+prdpnl4$CfLogLP*prdpnl4$LogLP)
prdpnl4$TrIR_est2=ifelse(is.na(prdpnl4$TrIR_est1), prdpnl4$RFixEff+prdpnl4$CfLogLSD*prdpnl4$LogLSD+prdpnl4$CfLogLP*prdpnl4$LogLP, prdpnl4$TrIR_est1)

prdpnl4[which(prdpnl4$Year==2005 & (prdpnl4$TrIR_est1>prdpnl4$TrIR_2005+0.001 | prdpnl4$TrIR_est1<prdpnl4$TrIR_2005-0.001)), c(1,2,5,6,16,20)]


'*** Impute for missing values ***'

prdpnl4$valid=(is.na(prdpnl4$TrIR_est2)==FALSE)*1

agg1=aggregate(prdpnl4$valid, list(prdpnl4$Area_3, prdpnl4$AnimGroup), FUN=sum)
names(agg1)=c("Area_3","AnimGroup","obs")
df1=merge(prdpnl4, agg1, all.x=TRUE)


'Average regional changes'

df2=df1[order(df1$Area_3, df1$AnimGroup, df1$Year),]

lgt=nrow(df2)

df2$lagIR=c(NA, df2$TrIR_est2[1:lgt-1]) 
df2$lagYear=c(NA, df2$Year[1:lgt-1])

df2$diffIR=ifelse(df2$Year>df2$lagYear, df2$TrIR_est2-df2$lagIR, NA)

'Assign to Isreal regional changes in West-Asia/North-America (4) and to South-Africa
regional changes in Sub-Saharan Africa (1)'

df2$GEReg_2=df2$GERegion
df2$GEReg_2[which(df2$Area_3==105)]=4
df2$GEReg_2[which(df2$Area_3==202)]=1

agg2=aggregate(df2$diffIR, list(df2$AnimGroup, df2$Year, df2$GERegion), FUN=mean, na.rm=TRUE)
names(agg2)=c("AnimGroup", "Year", "GEReg_2", "RegChange")

df3=merge(df2, agg2, all.x=TRUE)

df3=df3[order(df3$Area_3, df3$AnimGroup, df3$Year),]

write.csv(df3, file="output/check1.csv", row.names=FALSE)


'Regional averages'

agg3=aggregate(df3$TrIR_est2, list(df3$AnimGroup, df3$Year, df3$GERegion), FUN=mean, na.rm=TRUE)
names(agg3)=c("AnimGroup", "Year", "GERegion", "RegMean")

df4=merge(df3, agg3, all.x=TRUE)

df4$TrIR_est3=ifelse(is.na(df4$TrIR_est2)==FALSE, df4$TrIR_est2, ifelse(df4$obs==0 & df4$Year==2005, df4$RegMean, NA))


'Inter- and extrapolation'

df4=df4[order(df4$Area_3, df4$AnimGroup, df4$Year),]
lgt=nrow(df4)

yrfwd=vector(length=lgt)
valfwd=vector(length=lgt)
regfwd=vector(length=lgt)

yrbwd=vector(length=lgt)
valbwd=vector(length=lgt)
regbwd=vector(length=lgt)

lstyr0=-1
lstval0=-1
regchg0=-1

lstyr1=-1
lstval1=-1
regchg1=0


for(i in 2:lgt){
  
  if(df4$Year[i]<=df4$Year[i-1]){
    lstyr0=-1
    lstval0=-1
    regchg0=0
  }
  
  if (is.na(df4$TrIR_est3[i])==FALSE){
    lstyr0=df4$Year[i]
    lstval0=df4$TrIR_est3[i]
    regchg0=0
  }
  
  if (is.na(df4$TrIR_est3[i])==TRUE){
    yrfwd[i]=lstyr0
    valfwd[i]=lstval0
    regchg0=regchg0+df4$RegChange[i]
    regfwd[i]=regchg0
  }
  
  
  j=lgt-i+1
  
  if(df4$Year[j]>=df4$Year[j+1]){
    lstyr1=-1
    lstval1=-1
    regchg1=0
  }
  
  if (is.na(df4$TrIR_est3[j])==FALSE){
    lstyr1=df4$Year[j]
    lstval1=df4$TrIR_est3[j]
    regchg1=-df4$RegChange[j]
  }
  
  if (is.na(df4$TrIR_est3[j])==TRUE){
    yrbwd[j]=lstyr1
    valbwd[j]=lstval1
    regbwd[j]=regchg1
    regchg1=regchg1-df4$RegChange[j]
  }
  
}

df4$interpol=ifelse(yrfwd>0 & yrbwd>0, valfwd+(df4$Year-yrfwd)*(valbwd-valfwd)/(yrbwd-yrfwd),-9999)
df4$extrapol=ifelse(yrfwd>0 & yrbwd<=0, valfwd+regfwd, ifelse(yrfwd<=0 & yrbwd>0, valbwd+regbwd,-9999))

df4$TrIR_est4=apply(cbind(df4$interpol, df4$extrapol, df4$mnimpute, df4$TrIR_est3), 1, FUN=max, na.rm=TRUE)
is.na(df4$TrIR_est4[which(df4$IR_est4==-9999)])=TRUE

df4$yrfwd=yrfwd
df4$valfwd=valfwd
df4$regfwd=regfwd

df4$yrbwd=yrbwd
df4$valbwd=valbwd
df4$regbwd=regbwd

df4$IR_est=ifelse(is.na(df4$IR_2005)==FALSE & df4$IR_2005==0, 0, exp(df4$TrIR_est4)/(1+exp(df4$TrIR_est4)))

df4$Flag="NA"
df4$Flag[which(df4$extrapol>-9999)]="ben-rgr"
df4$Flag[which(df4$interpol>-9999)]="int-lin"
df4$Flag[which(is.na(df4$TrIR_est4)==FALSE) & df4$obs==0]="ben-rmn"
df4$Flag[which(is.na(df4$TrIR_est2)==FALSE)]="aux-rfe"
df4$Flag[which(is.na(df4$TrIR_est1)==FALSE)]="aux-cfe"
df4$Flag[which(is.na(df4$IR_2005)==FALSE & df4$IR_2005==0)]="ben-rgr"
df4$Flag[which(df4$Year==2005 & is.na(df4$IR_2005)==FALSE)]=" "

write.csv(df4, file="output/IR-Calculation-Check_6-12.csv", row.names=FALSE)

simir=df4[order(df4$AnimGroup, df4$GERegion, df4$Area_3, df4$Year), c(3,5,8,1,2,43:44)]
names(simir)[2]="AREA"
names(simir)[6]="IR"

write.csv(simir, file="trans/IR-estimated_6-12.csv", row.names=FALSE)
