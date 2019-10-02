setwd("~/Documents/Teaching/416_Spring19/BEFTutorial/")

# First step - open the CSV file
# Things that the students will need to review:
#   header = TRUE
#   setwd() - spend one minute here 

bef.data<-read.csv("BEF2.csv", header=T)
View(bef.data)

# Instructions:
#    For each plot, calculate:
#     1. Species richness - count of how many species are there
# Conceptual steps here?
#     2. 

plots<-unique(bef.data$Plot)

Richness<-NULL
Biomass<-NULL
 
for (i in 1:length(plots)){

sub.data<-subset(bef.data,Plot==plots[i])

species<-sub.data[,3:20]
spp<-colnames(species)[species[1,]==1]


Richness[i]<-length(spp)
Biomass[i]<-mean(sub.data$AbvBioAnnProd)

}

bef.results<-cbind(Richness, Biomass)
row.names(bef.results)<-plots


model.data<-subset(bef.results, Richness>0)

model<-lm(Biomass~log(Richness), data=as.data.frame(model.data))
summary(model)

par(mar=c(5,5,2,2)) 
plot(log(model.data[,"Richness"]), model.data[,"Biomass"], xlab = "Species Richness", ylab = expression(paste("Biomass (g/m"^"2", ")")), cex = 1.25,cex.lab = 1.5, cex.axis = 1.25)
lines(log(model.data[,"Richness"]), predict(model), lwd = 1.5)
