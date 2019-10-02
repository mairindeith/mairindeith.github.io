# BIOL 416 Tutorial 2
x <- 5

# Variables can also be strings
species.name <- "Homo sapiens"

# Variables can also be TRUE/FALSE
am.i.human <- TRUE

# Vectors
# Vectors are like lists of data
vector <- c(1,2,3)
# vector.2 <- c(4,5,6)

# Indexing:
vector
# Use [] to tell R where the variable we want is
vector[2]
# vector.2[2]

# Use the c() (combine) to pull out multiple values in the vector
vector[c(1,3)]

# Give names to the objects in vector
names(vector) <- c("Bruce", "Janet", "Third")
vector

# Pull out values in vector based on their name
# Mairin's a dork
vector["Janet"]

# Either use the location (as a number) or the name to pull out a single value

# 2-dimensional data
# Load in iris dataset
data("iris")
# View the entire dataset
View(iris)
# View the top 6 rows
head(iris)

class(iris)

# Pull out the value in first row, first column
# ROW FIRST, THEN COLUMN
iris[1,1]
# pull out the entire first column
sepal.length <- iris[1:150,1]
# If you leave a blank, R assumes you want all of it
sepal.length <- iris[,1]
first.observation <- iris[1,]
third.observation <- iris[3,]

# use names to pull out columns
sepal.length <- iris$Sepal.Length
# Only want flower #5's sepal length:
sepal.length.5 <- iris$Sepal.Length[5]

# Modifying data
# Make a copy of iris - KEEP THE ORIGINAL DATA UNTOUCHED
iris2 <- iris

# Make a new column - petal length/sepal length
iris2$Petal.Sepal.Ratio <- iris2$Petal.Length/iris2$Sepal.Length
View(iris2)

# R repeats values in a new column
iris2$Zeros <- 0
# iris2$Zeros[4:7] <- 4:7
View(iris2)

# Make a new dataframe "iris3", equal to iris2 EXCEPT FOR COLUMN 7
iris3 <- iris2[,-7]
View(iris3)
# Remove columns by position
iris3 <- iris2[,-c(7,8,9)]
View(iris3)

# Remove columns by name
# iris3 <- iris2[,-c(Zeros, petals2, 2petals)]
# iris3 <- iris2[,-"Zeros"]

# Get rid of the first 5 rows AND columns 7-9
iris4 <- iris2[-c(1:5),-c(7,8,9)]
View(iris4)

# Rewrite the variable "vector" (this deletes the old assignment for vector)
vector <- c(0,2,3)
# ONLY change number at position 1
vector[1] <- 5
vector

# Save new iris4 as a .csv file
write.csv(iris4, "tutorial2iris.csv")
dir()
# Read data from a file, save into the variable "iriscsv"
# iriscsv <- read.csv("Documents/BIOL416/tutorial2iris.csv")

write.csv(x=iris4, file=file.choose())

iriscsv <- read.csv(file.choose())

# Function
# c()

# Calculate the mean value of "vector"
vector <- c(5,2,3)

(vector[1]+vector[2]+vector[3])/3
sum(vector)/3
mean(vector)

# Get help:
?mean
?write.csv

write.csv(x = iris4, file = "tutorial2iris4.csv")
write.csv(file = "iris4.csv")

# How many objects in a vector?
?length
length(vector)

calculate.mean <- function(inputvector){
  sum(inputvector)/length(inputvector)
}

# Making ugly pictures
?plot

plot(x = iris4$Sepal.Length, 
     y = iris4$Petal.Length)

# Make a prettier plot
plot(x = iris4$Sepal.Length, 
     y = iris4$Petal.Length,
     main = "Iris sepal and petal size")

# Make an even prettier plot
plot(x = iris4$Sepal.Length, 
     y = iris4$Petal.Length,
     main = "Iris sepal and petal size",
     ylab = "Petal length (cm)",
     xlab = "Sepal length (cm)")

iris.xlab <- "Sepal length (cm)"

plot(x = iris4$Sepal.Length, 
     y = iris4$Petal.Length,
     main = "Iris sepal and petal size",
     ylab = "Petal length (cm)",
     xlab = "Sepal length (cm)", 
     col = iris4$Species)

# lm() = linear model
?lm
# ~ = tilde

# Find the linear equation that fits Petal.Length 
#    as a function of Sepal.Length
linearmodel.res <- lm(Petal.Length ~ Sepal.Length, data = iris4)

# Add the line on top of the plot
abline(lm(Petal.Length ~ Sepal.Length, data = iris4))

### Notes to self:
# Update the lesson according to the following:

# Include some discussion of working directory!
# setwd()? 

# Show the file system that R works within, do more than just dir()!
# Things that went well:
#    Finding the values in a linear model
#    Class seemed curious about how to go about problem solving (e.g. mean, 
#       using $ and [] notation)

### Naming variables
#    Capitalization matters!
#    Spaces matter!
#    Numbers matter!
#    Difference between "____" and ____
### More discussion of the working space beforehand? Or as we go?
#   In this session, as you go, and this worked out alright (only became confusing re: Files)
### Saving the working space
#   End of lesson
#   Saving individual files with write.csv() should come after functions have been introduced

# Functions before manipulations? Yes. 
# Order: 
#    Objects (0, 1, 2D using iris as a simple example to view)
#    Functions (mean() example over a vector, read.csv(), using help files)
#    Did not cover getting help online by searching for things

# Note: barely covered data classes, it may not be strictly necessary...