rm(list = ls());
library(jpeg);
library(nnet); 
library(ggplot2);
library(R2HTML);

set.seed(1234);


# read all files in folder
fs <- list.files(
  "D:/2015Fall/AIL_Machine_Learning/ML/project1-clustering/mit8-images-64x64", 
  full.name=TRUE);

# color average
colorAvg <- function(img) {
  r <- mean(img[,,1]);
  g <- mean(img[,,2]);
  b <- mean(img[,,3]);
  
  return(c(r, g, b));
}

colorAvg2 <- function(img) {
  mr = nrow(img);
  mc = ncol(img);
  mr <- mr / 2;
  mc <- mc / 2;
  
  a1 <- img[1:mr, 1:mc, ];
  a2 <- img[1:mr + mr, 1:mc, ];
  a3 <- img[1:mr, 1:mc + mc, ];
  a4 <- img[1:mr + mr, 1:mc + mc, ];
  
  v1 <- colorAvg(a1);
  v2 <- colorAvg(a2);
  v3 <- colorAvg(a3);
  v4 <- colorAvg(a4);
  
  m <- rbind(v1, v2, v3, v4);
  
  return(m);   
}

colorAvg3 <- function(img) {
  mr = nrow(img);
  mc = ncol(img);
  mr <- mr / 2;
  mc <- mc / 2;
  
  a1 <- img[1:mr, 1:mc, ];
  a2 <- img[1:mr + mr, 1:mc, ];
  a3 <- img[1:mr, 1:mc + mc, ];
  a4 <- img[1:mr + mr, 1:mc + mc, ];
  
  v1 <- colorAvg2(a1);
  v2 <- colorAvg2(a2);
  v3 <- colorAvg2(a3);
  v4 <- colorAvg2(a4);
  
  m <- rbind(v1, v2, v3, v4);
  
  return(m);   
}

colorAvg123 <- function(img)
{
  m1 <- colorAvg(img);
  m2 <- colorAvg2(img);
  m3 <- colorAvg3(img); 
  m <- rbind(m1, m2, m3);
  
  return(as.vector(m));   
}

colorAvgSpatialTable <- function(files, dpar=21*3)
{
  
  X <- matrix(nrow=length(files), ncol=dpar);
  
  for(k in 1:length(files))
  { 
    ik <- readJPEG(files[k]);
    X[k, ] <- colorAvg123(ik);
  }
  
  return(X);
}

# create data set
X <- colorAvgSpatialTable(fs);
D <- rbind(X);
D <- data.frame(D);

# random centers
randCenters <- function(numOfCenters, numCol) {
  centers <- matrix(nrow = numOfCenters, ncol = numCol);
  for (i in 1:numOfCenters) {
    centers[i, ] = runif(numCol, 0, 1);
  }
  return(centers);
}

# init labels
labelOne <- function(x, centers)
{
  rows <- nrow(centers);
  dist <- 1:rows;
  
  for(k in 1:rows)
  { 
    ck <- centers[k, ];
    dist[k] <- sum(abs(x-ck));
  }
  
  return(which.min(dist));
}


labelAll <- function(x, centers)
{
  rows <- nrow(x);  
  
  labels <- 1:rows;
  for(k in 1:rows) labels[k] <- labelOne(x[k, ], centers);
  
  return(labels);
}

# calculate new centers by average all of the elements in its group
averaging <- function(X, L, numOfCenters) {
  cols = ncol(X);
  rows = nrow(X);
  centers <- matrix(nrow = numOfCenters, ncol = cols, data = 0);
  numOfElements <- rep.int(0, numOfCenters);
  
  for (i in 1:rows) {
    # label of X[i, ]
    li = L[i];
    
    # sum of all X[i, ] has center li
    centers[li, ] <- centers[li, ] + as.numeric(X[i, ]);
    numOfElements[li] <- numOfElements[li] + 1;
  }
  
  for (i in 1:numOfCenters) {
    if (numOfElements[i] > 0) {
      centers[i, ] <- centers[i, ] / numOfElements[i];
    }
  }
  
  return(centers);
}

# init
k = 8;
numCol = ncol(D);
C <- randCenters(k, numCol);
L <- labelAll(D, C);

# step to get the accepted result
steps <- 20;

# calculate new centers and labels
for (i in 1:steps) {
  C <- averaging(D, L, k);
  L <- labelAll(D, C);
}

# We then reconstruct the vectors from labels and centers
reconstruct <- function(labels, centers) 
{
  X <- matrix(nrow=length(labels), ncol=ncol(centers));
  for(k in 1:length(labels) ) 
  {
    X[k, ] <- centers[labels[k], ];
  }
  
  return(data.frame(X) ); 
}

# We can measure errors (differences)
diff <- function(A, B) 
{
  rows <- nrow(A);
  e <- 1:rows;
  for(k in 1:rows)
  {
    e[k] <- sum(abs(A[k, ] - B[k, ]));
  }
  
  return(e);
}

D1 <- reconstruct(L, C);

# output use package R2HTML
n <- length(L);

HTMLStart(outdir = "D:/2015Fall/AIL_Machine_Learning/project_1", filename = "kmeans_output", extension = "html",
          echo = FALSE, autobrowse = FALSE, HTMLframe = TRUE, withprompt = "HTML> ",
          CSSFile = "R2HTML.css", BackGroundColor = "FFFFFF", BackGroundImg = "",
          Title = "K-Means")

HTML(paste("K = ", k));
E <- diff(D, D1);

HTML(paste("Error: ", sum(E)));

for (i in 1:k) {
  HTML.title(paste("Group ", i, ":"), HR = 3)
  
  inGroup <- which(L %in% c(i));
  
  if (length(inGroup) > 0) {
    for (j in 1:length(inGroup)) {
      HTMLInsertGraph(fs[inGroup[j]],
                      Caption = "IMG",
                      HeightHTML = NULL,
                      WidthHTML = NULL);
    }
  }
  HTML(paste("--------------------------------------------------------------"));
}

HTMLStop()
