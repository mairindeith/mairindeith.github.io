# Step 1: Read the csv

# These do the same thing:
# bef.data <- read.csv(file.choose())
bef.data <- read.csv("~/Documents/Teaching/416_Spring19/Tutorials/BEFTutorial/BEF2.csv")

# Step 2: Make an X2 dataframe
bef.x2 <- subset(x = bef.data, 
                 Plot == "X2")

# Step 3: Calculate species richness for X2 only
sr.x2 <- sum(bef.x2[1,3:20])
# only first row for first year

# Step 4: Biomass production
ef.x2 <- mean(bef.x2$AbvBioAnnProd)
ef.x2 <- mean(bef.x2[ ,21])

# Step 5: 
for( # counter # ){
  # between {}, code that we want to repeat
  # ...as many lines...
}

# Counter: "repeat the code inside five times"
#   Print: 1, 2, 3, ... 5
for(x in 1:5){
#  print(x)
  print(x*2)
}
# x is my counter variable
#   1:5 = c(1,2,3,4,5)
#   start with x=1, next loop: x=2, stop when x=5

plot.list <- unique(bef.data$Plot)
# This is going to be like our counter variable

results.df <- data.frame(
  Plot = plot.list,
  SR = NaN,
  BP = NaN
)

for(plot.number in 1:length(plot.list)){
  # Find the name of the plot so we can subset to it
  plot.name.tmp <- plot.list[plot.number]
  
  bef.tmp <- subset(x = bef.data, 
                    Plot == plot.name.tmp)
  # calculate SR and Biomass produced for each plot
  sr.tmp <- sum(bef.tmp[1,3:20])      ### calculation for species richness ### 
  bp.tmp <- mean(bef.tmp[,21])        ### calculation for average biomass produced ###
  
  # results.df$SR[plot.number] <- sr.tmp
  # results.df$BP[plot.number] <- bp.tmp
  
  results.df[plot.number,2:3] <- c(sr.tmp, bp.tmp)
}







# Example for loop:
# Start point: 1
# End point: 5
y <- 0

for(x in 3:5){
  y <- y + x
  # print(x)
  print(y)
}

for(x in 1:5){y <- y+x; print(y)}

for(x in 1:5){
  print(x)
}

plot.list <- unique(bef.data$Plot)

# Create a new data frame to store my results
#    because the for loop will re-write them!

results.data <- data.frame(
  PlotID = plot.list,
  SR = NaN,
  Biomass = NaN
)

for(plot.number in 1:length(plot.list)){
  # What is the name of the current plot?
  plot.name <- plot.list[plot.number]
  # Subset the data
  # Calculate SR
  # Calculate average biomass produced
  # Save the results to your results data frame
}

  
  
  # Filter only to the plot that matters at the moment
  bef.tmp <- subset(x = bef.data,
                    Plot == plot.name)
  # ... calculate SR and biomass for each plot 
  
  # SR calculations
  # biomass calcs
  
  results.data$SR[plot.number] <- sr.tmp
  results.data$Biomass[plot.number] <- biomass.tmp
}
