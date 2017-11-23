# gapscore.R

# the gap score quantifies pattern rotation

# gap score = (vertical_number_gaps_between_pattern - horizontal_number_gaps_between_pattern)/total_number_gaps
# total_number_gaps = vertical_number_gaps_between_pattern + horizontal_number_gaps_between_pattern

gapScore <- function(image, pattern_color, mingap=2) {

	# image: png image to perform calculation on
	# pattern_color: RGB values for pattern color of interest
	# mingap: a gap must have mingap number of pixels between pattern

	# load png library
	if (! "png" %in% (.packages())) library(png)

	# check arguments
	if (mingap < 2) stop("mingap must be at least 2")

	# define pattern color
	patcol <- rep(NA, 3)
	for (i in 1:3) {
		if (pattern_color[i] < 0 || pattern_color[i] > 255) stop("pattern RGB value out of [0,255] range")
		patcol[i] <- pattern_color[i]/255
	}

	# proces image
	img <- readPNG(image)

	# raster indices of nontransparent image
	if (length(which(img[,,4] == 1, arr.ind=TRUE)) < 1) stop(paste(image, "has zero size"))

	# raster indices that are the pattern color
	patternidx <- which(img[,,4] == 1 & img[,,1] == patcol[1] & img[,,2] == patcol[2] & img[,,3] == patcol[3], arr.ind=TRUE)
	if (length(patternidx) < 1) return(0)
	
	# count number of times crossing pattern in vertical direction
	xcounts <- 0
	for (i in unique(patternidx[,2])) {
		patrows <- patternidx[,1][which(patternidx[,2] == i)]
		xcounts <- xcounts + length(which(diff(patrows) >= mingap))
	}

	# count number of times crossing pattern in horizontal direction
	ycounts <- 0
	for (i in unique(patternidx[,1])) {
		patcols <- patternidx[,2][which(patternidx[,1] == i)]
		ycounts <- ycounts + length(which(diff(patcols) >= mingap))
	}
	
	# calculate gap score
	ngap <- xcounts + ycounts
	gapscore <- ifelse(ngap > 0, (xcounts-ycounts)/ngap, 0)

	return(gapscore)
}


patRotList <- function(imageDir, imageList, suffix="png", pattern_color, gap=2) {

	# imageDir: directory containing images
	# imageList: a text file with the names of images to be analyzed
	# suffix: suffix of the image files
	# pattern_color: RGB value of pattern color
	# gap: minimum number of pixels between pattern to consider gaps

	# prepare image directory
	imageDir <- gsub("/$", x=imageDir, replacement='')

        # read in images
        imagevec <- as.vector(read.table(imageList)$V1)
	nsamples <- length(imagevec)

        # process images
        rotvals <- data.frame(id=rep(NA,nsamples), rotation=as.numeric(rep(NA,nsamples)))
	
	for (i in 1:length(imagevec)) {
		image <- paste(imageDir, "/", imagevec[i], ".", suffix, sep='')
		print(imagevec[i])
		rotvals$id[i] <- imagevec[i]
		rotvals$rotation[i] <- gapScore(image=image, pattern_color=pattern_color, mingap=gap)
	}

	return(rotvals)
}
