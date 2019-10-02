setwd("C:/Jonathan/Dropbox/Admin/UBC/Teaching/BIOL416/Tutorials/T5")

bef.data<-read.csv("../BEF2.csv", header=T)
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
# plot(bef.results$)
model<-lm(Biomass~log(Richness), data=as.data.frame(model.data))
model2<-lm(Biomass~Richness, data=as.data.frame(model.data))
summary(model)

par(mar=c(5,5,2,2)) 
plot(log(model.data[,"Richness"]), model.data[,"Biomass"], xlab = "log(Species Richness)", ylab = expression(paste("Biomass (g/m"^"2", ")")), cex = 1.25,cex.lab = 1.5, cex.axis = 1.25)
plot(model.data[,"Richness"], model.data[,"Biomass"], xlab = "Species Richness", ylab = expression(paste("Biomass (g/m"^"2", ")")), cex = 1.25,cex.lab = 1.5, cex.axis = 1.25)

lines(log(model.data[,"Richness"]), predict(model), lwd = 1.5)


### Changes:
# Simplify plot
# Need to teach loops
# Introduce alternative ways to subset data - sum 
# for loop - fill up a dataframe, avoid cbind 
 

# Hour for the takl 

# Outcome:

- fitted model
- coefficients
- graph interpretation
- log vs. non-log transformed 

Expected to write:
- before the next tutorial: 1-2 pages showing model output, graph, and some interpretation of what this means
- show potential species richness relationships, they can ID which one they have and provide some ecological explanation
- interpret the output of the LM
- post L+H 2001 paper

### New commands
# for !!!
# subset()
