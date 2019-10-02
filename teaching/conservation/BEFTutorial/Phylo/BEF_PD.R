#!# Feb 11 to-do:
# Email Jonathan - extra species in the phylogeny?
#   Email students from today about the extra species
# Clearer step-by-step instructions?
#   Use terminology like drop.tip
#   Linear model results with summary()


setwd("~/Documents/Teaching/416_Spring19/Tutorials/BEFTutorial/Phylo/")

library(ape)
tree<-read.tree("phylogeny.phy")

bef.data<-read.csv("../BEF2.csv", header=T)
plots<-unique(bef.data$Plot)

#!# My changes
results.df <- data.frame(
  Plots = plots,
  PD = NaN,
  SR = NaN,
  AAB = NaN
)

PD.s<-NULL
PD.t<-NULL
Richness<-NULL
Biomass<-NULL

sub.data <- subset(bef.data, Plot == "X2")
drop.spp <- colnames(sub.data)[
  which(sub.data[1, ]==0)
]

phylo2 <- drop.tip(phy = tree, tip = drop.spp)
plot(phylo2)

for (i in 1:length(plots)){

sub.data<-subset(bef.data,Plot==plots[i])

species<-sub.data[,3:20]
spp<-colnames(species)[species[1,]==1]

Richness[i]<-length(spp)
Biomass[i]<-mean(sub.data$AbvBioAnnProd)

# prune species NOT in the plots form the phylogeny
subtree.s<-drop.tip(tree, tree$tip.label[!tree$tip.label %in% c(spp,"OUTGROUP")])
subtree.t<-drop.tip(time.tree, time.tree$tip.label[!time.tree$tip.label %in% c(spp,"OUTGROUP")])
# note hear that we always include the OUTGROUP in the tree even though it is not included in any of the plots
# this is equivalent to adding a constant to the plot PD.
# It is necessary to do this, otherwise plots with species richness of one
# will crash the calculations of PD because you cannot have a tree with just one taxon (it is a stick!)

#sum edge lengths of pruned tree - this returns the phylogenetic diversity of included species
PD.s[i]<-sum(subtree.s$edge.length)
PD.t[i]<-sum(subtree.t$edge.length)

}

bef.results<-cbind(PD.s, PD.t, Richness, Biomass)
row.names(bef.results)<-plots

#Remove plots with zero species (these still have biomass recorded because not all species were included in this dataframe)
model.data<-subset(bef.results, Richness>0)

model.PD.s<-lm(Biomass~PD.s, data=as.data.frame(model.data))
model.PD.t<-lm(Biomass~PD.t, data=as.data.frame(model.data))
summary(model.PD)

model.Richness<-lm(Biomass~log(Richness), data=as.data.frame(model.data))
summary(model.Richness)

par(mar=c(5,5,2,2))
plot(model.data[,"PD"], model.data[,"Biomass"], xlab = "Phylogenetic Diversity (time)", ylab = expression(paste("Biomass (g/m"^"2", ")")), cex = 1.25,cex.lab = 1.5, cex.axis = 1.25)
lines(model.data[,"PD"], predict(model.PD), lwd = 1.5)

x11()#open new plotting window (Windows OS)
plot(log(model.data[,"Richness"]), model.data[,"Biomass"], xlab = "Species Richness", ylab = expression(paste("Biomass (g/m"^"2", ")")), cex = 1.25,cex.lab = 1.5, cex.axis = 1.25)
lines(log(model.data[,"Richness"]), predict(model.Richness), lwd = 1.5)

x11()#open new plotting window (Windows OS)
plot(bef.results[,"PD"], bef.results[,"Richness"],xlab = "Phylogenetic Diversity", ylab = "Species Richness", cex = 1.25,cex.lab = 1.5, cex.axis = 1.25)

#This shows that the tree is not ultrametric - branch lengths represent molecular substitutions
plot(tree)

#Let's make branch lengths proportional to time
time.tree <- chronoMPL(tree)

#Now redo your analyses - how do your results change?

########################################################################
# bonus points for anybody who identifies problems with this new tree!
# (it includes some negative branch lengths, which are, of course,
# impossible in an evolutionary sense)
########################################################################
