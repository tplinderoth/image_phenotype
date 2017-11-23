# blackProportion.R

# calculates black proportion of png

blackProportion <- function(image, buffer=c(0,0,0)) {

	# image: png image to perform calculation on
	# buffer: offset for black RGB values (0+buffer[1], 0+buffer[2], 0+buffer[3])

	# load png library
	if (! "png" %in% (.packages())) library(png)

	# check black buffer values
	for (i in 1:3) {
		if (buffer[i] < 0 || buffer[i] > 255) stop(paste(buffer[i], "is an invalid offset"))
		buffer[i] <- buffer[i]/255
	}

	img <- readPNG(image)

	# number of nontransparent pixels
	colpix <- nrow(which(img[,,4] == 1, arr.ind=TRUE))
	if (length(colpix) < 1) {
		print(paste(image, "has zero size"))
		return(0)
	}

	# number of black pixels
	blackpix <- nrow(which(img[,,4] == 1 & img[,,1] <= buffer[1] & img[,,2] <= buffer[2] & img[,,3] <= buffer[3], arr.ind=TRUE))

	# calculate proportion of image that is black
	return(blackpix/colpix)
}	

blackList <- function(imageDir, imageList, suffix="png", buffer=c(0,0,0)) {

	# imageDir: directory containing images
	# imageList: a text file with the names of images to be analyzed
	# suffix: suffix of the image files
	# buffer: offset for black RGB values (0+buffer[1], 0+buffer[2], 0+buffer[3])

	# prepare image directory
	imageDir <- gsub("/$", x=imageDir, replacement='')

	# read in images
	imagevec <- as.vector(read.table(imageList)$V1)
	nsamples <- length(imagevec)

	# process images
	blackdf <- data.frame(id=rep(NA,nsamples), black=as.numeric(rep(NA,nsamples)))
        
	for (i in 1:length(imagevec)) {
		image <- paste(imageDir, "/", imagevec[i], ".", suffix, sep='')
		print(imagevec[i])
		blackdf$id[i] <- imagevec[i]
		blackdf$black[i] <- blackProportion(image=image, buffer=buffer)
	}

	return(blackdf)
}
