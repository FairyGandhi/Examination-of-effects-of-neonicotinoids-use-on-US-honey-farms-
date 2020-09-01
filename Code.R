setwd("C:/Users/Glenn Family PC/Desktop/Quantitative Methods") 
Honey <- read.csv("HoneyNeonic_v03.csv", header=TRUE)
par(mfrow=c(2,2))
fitreg101 <- lm(Honey$totalprod~Honey$year)
summary(fitreg101)
par(mfrow=c(2,2))
plot(fitreg101)
Honey2 <- aggregate(totalprod ~ year , FUN=sum, data=Honey)
Honey[is.na(Honey[,12]),12] <- 0
Honey[is.na(Honey[,13]),13] <- 0
Honey[is.na(Honey[,14]),14] <- 0
Honey[is.na(Honey[,15]),15] <- 0
Honey[is.na(Honey[,16]),16] <- 0
Honey[is.na(Honey[,17]),17] <- 0

Honey3 <- cbind(Honey[,c(1:11,17)])
Honey4 <- Honey3[Honey3$year>=2015]
Honey3 <- Honey3[Honey3$year>=1995 & Honey3$year<=2015,]
OnlyNeo <- lm(totalprod ~ . - StateName - FIPS -Region , data=Honey3)
summary(OnlyNeo)

Neouse <- aggregate(nAllNeonic~year, FUN=sum, data=Honey)
Neouse <- Neouse[Neouse$year>=1995 & Neouse$year<=2015,]
par(mfrow=c(1,2))
plot(Neouse)
abline(lm(Neouse$nAllNeonic~Neouse$year))
plot(Honey2)
abline(lm(Honey2$totalprod~Honey2$year))
plot(Neouse)
abline(lm(Neouse$nAllNeonic~Neouse$year))
plot(Honey2)
abline(lm(Honey2$totalprod~Honey2$year))


PPLb <- aggregate(priceperlb~Region + year, FUN=mean, data=Honey)


NeoProd <- lm(totalprod ~ . - StateName - FIPS -Region - state - year, data=Honey3)
summary(NeoProd)
Honey3 <- Honey3[Honey3$year>=1995 & Honey3$year<=2015,]
Honey4 <- Honey3[Honey3$year>=1995 & Honey3$year<=2015,]

#Most and Least Neonic used

NeonicState <- aggregate(nAllNeonic ~ state, FUN=mean, data=Honey)
HoneyState <- aggregate(numcol ~ state, FUN=sum, data=Honey)
StateProd <- merge(NeonicState, HoneyState, by="state")
StateProd$AvgUse <- StateProd$nAllNeonic / StateProd$numcol
StateProd <- StateProd[ order(StateProd$AvgUse),]
LargeStateprod <- StateProd[StateProd$numcol>1000000,]

#Comparing Iowa and Wyoming

HoneyIA <- Honey[Honey$state=="IA",]
HoneyWY <- Honey[Honey$state=="WY",]
HoneyprodIA <- aggregate(totalprod ~ year , FUN=sum, data=HoneyIA)
HoneyprodIA <- HoneyprodIA[HoneyprodIA$year>=1995 & HoneyprodIA$year<=2015,]
HoneyprodWY <- aggregate(totalprod ~ year , FUN=sum, data=HoneyWY)
HoneyprodWY <- HoneyprodWY[HoneyprodWY$year>=1995 & HoneyprodWY$year<=2015,]
par(mfrow=c(2,2))
plot(Neouse,xlab="Neonicitinoid Use in America")
abline(lm(Neouse$nAllNeonic~Neouse$year))
plot(Honey2, xlab="Honey Production in America")
abline(lm(Honey2$totalprod~Honey2$year))
plot(HoneyprodIA,xlab="IOWA")
abline(lm(HoneyprodIA$totalprod~HoneyprodIA$year))
plot(HoneyprodWY, xlab="WYOMING")
abline(lm(HoneyprodWY$totalprod~HoneyprodWY$year))
par(mfrow=c(1,1))
plot(HoneyprodIA, pch=20, col="blue")
points(HoneyprodWY, pch=20, col="red")
abline(lm(HoneyprodWY$totalprod~HoneyprodWY$year),col="red")
abline(lm(HoneyprodIA$totalprod~HoneyprodIA$year),col="blue")

#Neonicitinoid Usage in Iowa and Wyoming
HoneyIA <- Honey[Honey$state=="IA",]
HoneyWY <- Honey[Honey$state=="WY",]
NeouseIA <- aggregate(nAllNeonic~year, FUN=sum, data=HoneyIA)
NeouseIA <- NeouseIA[NeouseIA$year>=1995 & NeouseIA$year<=2015,]
NeouseWY <- aggregate(nAllNeonic~year, FUN=sum, data=HoneyWY)
NeouseWY <- NeouseWY[NeouseWY$year>=1995 & NeouseWY$year<=2015,]

#Graphs
par(mfrow=c(2,2))
plot(NeouseIA, pch=20, col="blue", xlab="Neonicitinoid usage in (blue=Iowa) & (red=Wyoming)")
points(NeouseWY, pch=20, col="red")
plot(HoneyprodIA, pch=20, col="blue", xlab="Honey Production in (blue=Iowa) & (red=Wyoming)")
points(HoneyprodWY, pch=20, col="red")
abline(lm(HoneyprodWY$totalprod~HoneyprodWY$year),col="red")
abline(lm(HoneyprodIA$totalprod~HoneyprodIA$year),col="blue")
plot(Neouse,xlab="Neonicitinoid Use in America")
plot(Honey2, xlab="Honey Production in America")
abline(lm(Honey2$totalprod~Honey2$year),col=3)



#Linear modeling of individual states


PredIA <- as.data.frame(Honey3[Honey3$state=="IA",])
PredIA <- as.data.frame(PredIA[,c(2:8,12)])
IAlm <- lm(totalprod ~ nAllNeonic + year , data=PredIA)
summary(IAlm)
PredNE <- as.data.frame(Honey3[Honey3$state=="NE",])
PredNE <- as.data.frame(PredNE[,c(2:8,12)])
NElm <- lm(totalprod ~ nAllNeonic + year , data=PredNE)
summary(NElm)
PredAZ <- as.data.frame(Honey3[Honey3$state=="AZ",])
PredAZ <- as.data.frame(PredAZ[,c(2:8,12)])
AZlm <- lm(totalprod ~ nAllNeonic + year , data=PredAZ)
summary(AZlm)
PredCA <- as.data.frame(Honey3[Honey3$state=="CA",])
PredCA <- as.data.frame(PredCA[,c(2:8,12)])
CAlm <- lm(totalprod ~ nAllNeonic + year  , data=PredCA)
summary(CAlm)
PredMT <- as.data.frame(Honey3[Honey3$state=="MT",])
PredMT <- as.data.frame(PredMT[,c(2:8,12)])
MTlm <- lm(totalprod ~ nAllNeonic + year  , data=PredMT)
summary(MTlm)
PredWY <- as.data.frame(Honey3[Honey3$state=="WY",])
PredWY <- as.data.frame(PredWY[,c(2:8,12)])
WYlm <- lm(totalprod ~ nAllNeonic + year , data=PredWY)
summary(WYlm)
coeftest(WYlm,vcov=vcovHC,type="HC1")
coeftest(MTlm,vcov=vcovHC,type="HC1")
coeftest(CAlm,vcov=vcovHC,type="HC1")
coeftest(AZlm,vcov=vcovHC,type="HC1")
coeftest(NElm,vcov=vcovHC,type="HC1")
coeftest(IAlm,vcov=vcovHC,type="HC1")
coef(summary(IAlm))
coef(summary(NElm))
coef(summary(AZlm))
coef(summary(CAlm))
coef(summary(MTlm))
coef(summary(WYlm))

summary(lm( totalprod ~ priceperlb + year, data=Honey))

par(mfrow=c(1,1))
plot( Honey$year,Honey$priceperlb)

#Pulling out the Tvalues for every large state and plotting them.

LargeTvalues <- rep(0,nrow(LargeStateprod))
for(i in 1:nrow(LargeStateprod)){
  state <- LargeStateprod[i,1]
  Pred <- as.data.frame(Honey3[Honey3$state==state,])
  Pred <- as.data.frame(Pred[,c(2:8,12)])
  Pred <- summary(lm(totalprod ~ nAllNeonic + year , data=Pred))
  Pred2 <- Pred$coefficients[2,3]
  LargeTvalues[i] <- abs(Pred2) 
}
plot(LargeTvalues)

#Pulling out the Tvalues after applying the coeftest for every large state and plotting them.

LargeTvalues2 <- rep(0,nrow(LargeStateprod))
for(i in 1:nrow(LargeStateprod)){
	state <- LargeStateprod[i,1]
	Pred <- as.data.frame(Honey3[Honey3$state==state,])
	Pred <- as.data.frame(Pred[,c(2:8,12)])
	Pred <- lm(totalprod ~ nAllNeonic + year , data=Pred)
	Pred <- coeftest(Pred,vcov=vcovHC,type="HC1")
	Pred <- Pred[2,3]
	LargeTvalues2[i] <- abs(Pred) 
}
plot(LargeTvalues2)


#All graphs down here

par(mfrow=c(2,3))
plot(NeouseIA, pch=20, col="blue", xlab="Neonicitinoid usage in (blue=Iowa) & (red=Wyoming)")
points(NeouseWY, pch=20, col="red")
plot(HoneyprodIA, pch=20, col="blue", xlab="Honey Production in (blue=Iowa) & (red=Wyoming)")
points(HoneyprodWY, pch=20, col="red")
abline(lm(HoneyprodWY$totalprod~HoneyprodWY$year),col="red")
abline(lm(HoneyprodIA$totalprod~HoneyprodIA$year),col="blue")
plot(Neouse,xlab="Neonicitinoid Use in America")
plot(Honey2, xlab="Honey Production in America")
abline(lm(Honey2$totalprod~Honey2$year),col=3)
plot( Honey$year,Honey$priceperlb)

par(mfrow=c(1,1))
plot(LargeTvalues)
abline(h=1.96, col="red")
plot(LargeTvalues2, ylab="T-value", xlab="Top States for Neonicotinoid Use (Least to Greatest)")
abline(h=1.96, col="red")

##Price is not driving production (increasing prices, should increase supply if it did)
par(mfrow=c(1,2))
plot(aggregate(priceperlb~year, FUN=mean, data=Honey), ylab="honey price per pound ($)")
plot(Honey2, ylab="total honey production (pounds)")

plot(aggregate(stocks~year, FUN=mean, data=Honey), ylab="honey stocks (pounds)")

##Time and state fixed effects with regular regression function (not demeaned)
OnlyNeo2 <- lm(totalprod ~ nAllNeonic + priceperlb + state + year -1 , data=Honey3)
summary(OnlyNeo2)

##Time and state fixed effects regressions with demeaned data
library(plm)
library(AER)

OnlyNeo2TF <- plm(totalprod ~ nAllNeonic, 
                  data = Honey3,
                  index = c("state", "year"), 
                  model = "within", 
                  effect = "twoways")
summary(OnlyNeo2TF)
coeftest(OnlyNeo2TF, vcov = vcovHC, type = "HC1")[1,] ##controlling for autocorrelation and heteroskadasticity
coeftest(OnlyNeo2TF, vcov = vcovHC, type = "HC1") ##controlling for autocorrelation and heteroskadasticity
