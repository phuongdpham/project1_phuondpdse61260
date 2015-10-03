### Q1. Find the feature selection function to convert an image into a vector

 I use color average to find the feature selection to convert an image into a vector.
 Which image I calculate in layer 3 spatial color average table and convert them to a vector has dim = 63
 '''{r}
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
'''
	
 I create data set from matrix X
 
### Q2. Divide the images into k clusters using kmeans.
 I divide them to k = 8 clusters, and loop the kmeans method steps times to find the best solution.
 set.seed(1234)
'''{r}
	# calculate new centers and labels
	for (i in 1:steps) {
	  C <- averaging(D, L, k);
	  L <- labelAll(D, C);
	}
'''
 
### Q3. Rendering the result in HTML page
 I use R2HTML library to render output from R to HTML
'''{r}
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
'''