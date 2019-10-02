#setwd("C:/Jonathan/Dropbox/Admin/UBC/Teaching/BIOL416/Tutorials/T9/mammal extinction")
setwd("~/Documents/Teaching/416_Spring19/Tutorials/Mammal_Extn/")
raw.data <- read.csv("mammal.data.csv") #, stringsAsFactors=F)
MData <- read.csv("mammal.data.csv", stringsAsFactors=F)

specnu <- unique(
  MData$MSW05_Binomial)

#Create a for loop to numberize RL status
for(specnam in 1:length(specnu)){
#  spec.data <- specnu[specnam]
#  species.subset <- subset(MData, X==spec.data)
#  RL<-NULL
  if (MData[specnam,7]=="LC"){MData[specnam, 7]<-0
  } else if (MData[specnam, 7]=="NT") {MData[specnam, 7]<-1
  } else if (MData[specnam, 7]=="VU") {MData[specnam, 7]<-2
  } else if (MData[specnam, 7]=="EN") {MData[specnam, 7]<-3
  } else if (MData[specnam, 7]=="CR") {MData[specnam, 7]<-4
  } else if (MData[specnam, 7]=="EW") {MData[specnam, 7]<-5
  } else if (MData[specnam, 7]=="DD") {MData[specnam, 7]<-NA}
}

#Subset data for carnivora 
subset(MData, MSW05_Order=="Carnivora")
csubset <- subset(MData, MSW05_Order=="Carnivora")

#Create df for carnivora
carnivora.df <- data.frame(
  Species = csubset
)


#Part 1: Carnivora

comparative.data(mphylogeny, carnivora.df, names="Species.MSW05_Binomial")

comparative.data.object.c <- comparative.data(mphylogeny, carnivora.df, names="Species.MSW05_Binomial")
summary(comparative.data.object.c)

#Run the phylogenetic regression
cmodel <- pgls(Species.RedList~log(Species.GR_Area_km2 + Species.AdultBodyMass_g), data = comparative.data.object.c, lambda = 'ML')

class(comparative.data.object.c$data$Species.RedList)

cmodel <- pgls(as.numeric(Species.RedList)~log(Species.GR_Area_km2 + Species.AdultBodyMass_g), data = comparative.data.object.c, lambda = 'ML')





unique(raw.data$RedList)
#DD LC CR NT EN VU EX EW

RL<-NULL
for (i in 1:length(raw.data$RedList)){
if (raw.data$RedList[i]=="LC") RL[i]<-0
if (raw.data$RedList[i]=="NT") RL[i]<-1
if (raw.data$RedList[i]=="VU") RL[i]<-2
if (raw.data$RedList[i]=="EN") RL[i]<-3
if (raw.data$RedList[i]=="CR") RL[i]<-4
if (raw.data$RedList[i]=="EW") RL[i]<-5
if (raw.data$RedList[i]=="EX") RL[i]<-5
if (raw.data$RedList[i]=="DD") RL[i]<-NA
}

data<-cbind(raw.data, as.factor(RL))

Carnivora<-subset(data, MSW05_Order=="Carnivora")
Lagomorpha<-subset(data, MSW05_Order=="Lagomorpha")

lagomorph.model<-lm(RL~log(AdultBodyMass_g)+log(GR_Area_km2),data = Lagomorpha)
carnivore.model<-lm(RL~log(AdultBodyMass_g)+log(GR_Area_km2),data = Carnivora)

###################################################################################
#Phylogenetic regression
###################################################################################
library(ape)
library(caper)

tree<-read.tree("mammal_phylogeny.tre")

z<-comparative.data(tree, data = Carnivora, names="MSW05_Binomial")
carnivore.model<-pgls(as.factor(RL)~log(AdultBodyMass_g)+log(GR_Area_km2),data = z, lambda='ML')
summary(carnivore.model)

z<-comparative.data(tree, data = Lagomorpha, names="MSW05_Binomial")
lagomorph.model<-pgls(RL~log(AdultBodyMass_g)+log(GR_Area_km2),data = z, lambda='ML')
summary(lagomorph.model)

