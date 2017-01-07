#install.packages('doMC')
#install.packages('reshape2')

# Importing Libraries
library(reshape2)
library(doMC)
registerDoMC()


######  IMPORTATION AND CLEANING  ######
#Just setting up some global variables
data.dir <- '/Users/plablanche/Kaggle_Challenges/Face_Reco_R/'
train.file <- paste0(data.dir, 'training.csv')
test.file <- paste0(data.dir, 'test.csv')

#Reading the training data set
d.train <- read.csv(train.file,stringsAsFactors=F)

#Selecting and removing Image from the data set
im.train <- d.train$Image
d.train$Image <- NULL
head(d.train)


#Looking at the first image
im.train[1]
length(im.train[1])
as.integer(unlist(strsplit(im.train[1]," ")))

# froeach loop transforming all string images in integers
im.train <- foreach(im = im.train, .combine = rbind) %dopar% { as.integer(unlist(strsplit(im," ")))}

# Reading and extracting pictures for the test set
d.test <- read.csv(test.file,stringsAsFactors = F)
im.test <- d.test$Image
d.test$Image <- NULL
im.test <- foreach(im = im.test, .combine = rbind) %dopar% { as.integer(unlist(strsplit(im," ")))}

# Saving everything for future work
save(d.train,im.train,d.test,im.test,file = 'data.Rd')

#Uncomment and execute next line to save time...
#load('data.Rd')


######  VISUALIZATION  #######
# rev = reverse
im <- matrix(data = rev(im.train[1,]), nrow = 96, ncol = 96)
par(pty="s")
image(1:96,1:96,im,col = gray((0:255)/255))

points(96-d.train$left_eye_center_x[1], 96-d.train$left_eye_center_y[1],col='red')
points(96-d.train$right_eye_center_x[1], 96-d.train$right_eye_center_y[1],col='blue')
points(96-d.train$nose_tip_x[1], 96-d.train$nose_tip_y[1],col='green')

# To clear the figure
frame()
par(pty="s")
image(1:96,1:96,im,col = gray((0:255)/255))
# Plot all nose tips
for(i in 1:nrow(d.train)) {
  points(96-d.train$nose_tip_x[i], 96-d.train$nose_tip_y[i], col="green")
}

#find extreme bottom left values
idx <- which.max(d.train$nose_tip_x)
im  <- matrix(data=rev(im.train[idx,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))
points(96-d.train$nose_tip_x[idx], 96-d.train$nose_tip_y[idx], col="green")


######  SOME COMPUTATIONS FOR SUBMISSION  #######
#Mean values na.rm=T to avoid non value
colMeans(d.train,na.rm = T)

#Create pr??dictions ??gal aux moyennes
p <- matrix(data=colMeans(d.train, na.rm=T), nrow=nrow(d.test),
            ncol=ncol(d.train), byrow=T)
colnames(p) <- names(d.train)
predictions <- data.frame(ImageId = 1:nrow(d.test), p)
head(predictions)

#Create table for submission
submission <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
head(submission)

#Create csv file for prediction submission
example.submission <- read.csv(paste0(data.dir, 'IdLookupTable.csv'))
sub.col.names      <- names(example.submission)
example.submission$Location <- NULL
submission <- merge(example.submission, submission, all.x=T, sort=F)
submission <- submission[, sub.col.names]
write.csv(submission, file="submission_means.csv", quote=F, row.names=F)


######  PATCHES FOR EYE PREDICTIONS  #######
coord      <- "left_eye_center"
patch_size <- 10
coord_x <- paste(coord, "x", sep="_")
coord_y <- paste(coord, "y", sep="_")

#hereafter create patches
# each row are the 441 pixels around x and y center (21x21)
# total 7033 rows instead of 7049 because of crop patches
patches <- foreach (i = 1:nrow(d.train), .combine=rbind) %dopar% {
  im  <- matrix(data = im.train[i,], nrow=96, ncol=96)
  x   <- d.train[i, coord_x]
  y   <- d.train[i, coord_y]
  x1  <- (x-patch_size)
  x2  <- (x+patch_size)
  y1  <- (y-patch_size)
  y2  <- (y+patch_size)
  if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
  {
    as.vector(im[x1:x2, y1:y2])
  }
  else
  {
    NULL
  }
}

#Compute the mean of all pacthes
mean.patch <- matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)

image(1:21, 1:21, mean.patch[21:1,21:1], col=gray((0:255)/255))

#Using the mean.patch to find left eyes
search_size <- 2
mean_x <- mean(d.train[, coord_x], na.rm=T)
mean_y <- mean(d.train[, coord_y], na.rm=T)
x1     <- as.integer(mean_x)-search_size
x2     <- as.integer(mean_x)+search_size
y1     <- as.integer(mean_y)-search_size
y2     <- as.integer(mean_y)+search_size

#Grid of x and y around mean x and y value
params <- expand.grid(x = x1:x2, y = y1:y2)
params

im <- matrix(data = im.test[1,], nrow=96, ncol=96)
image(1:96,1:96,im, col=gray((0:255)/255))

#below use cor = correlation coefficient in order to compute the score
r  <- foreach(j = 1:nrow(params), .combine=rbind) %dopar% {
  x     <- params$x[j]
  y     <- params$y[j]
  p     <- im[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
  score <- cor(as.vector(p), as.vector(mean.patch))
  score <- ifelse(is.na(score), 0, score)
  data.frame(x, y, score)
}

#find the highest score
which.max(r$score)
best <- r[which.max(r$score), c("x", "y")]
best
points(best['x'],best['y'],col='green')


######  Splitting the training data set for validation
#  In order to estimate the score by Kaggle
d  <- read.csv(train.file, stringsAsFactors=F)
im <- foreach(im = d$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
d$Image <- NULL
set.seed(0)
idxs     <- sample(nrow(d), nrow(d)*0.8)
d.train  <- d[idxs, ]
d.test   <- d[-idxs, ]
im.train <- im[idxs,]
im.test  <- im[-idxs,]
rm("d", "im")

p <- matrix(data=colMeans(d.train, na.rm=T), nrow=nrow(d.test), ncol=ncol(d.train), byrow=T)
sqrt(mean((d.test-p)^2, na.rm=T))




##############################################################
##############################################################
### PERSONAL ACHIEVEMENTS

image(1:96,1:96,im,col = gray((0:255)/255))
xx <- d.train[1,seq(1,ncol(d.train),2)]
yy <- d.train[1,seq(2,ncol(d.train),2)]
points(96-xx,96-yy,col='green')