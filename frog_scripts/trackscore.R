# trackscore.R

# the track score quantifies pattern rotation

# track score = avg(longest_horizontal_pattern_track/image_row_width) - avg(longest_vertical_pattern_track/image_column_height)

trackScore <- function(image, pattern_color) {

	# image: png image to perform calculation on
	# pattern_color: RGB values for pattern color of interest

	# these should be made as options:
	# xfrac: minimum proportion of max image width for row to be analyzed
	# yfrac: minimum proportion of max image height for column to be analyzed
	# hmin: minimum horizontal track length for analyzing pattern
	# vmin: minimum vertical track length for analyzing pattern

	# load png library
	if (! "png" %in% (.packages())) library(png)

	# define pattern color
	patcol <- rep(NA, 3)
	for (i in 1:3) {
		if (pattern_color[i] < 0 || pattern_color[i] > 255) stop("pattern RGB value out of [0,255] range")
		patcol[i] <- pattern_color[i]/255
	}

	# proces image
	img <- readPNG(image)

	# array indices of nontransparent image
	imgidx <- which(img[,,4] == 1, arr.ind=TRUE)
	if (length(imgidx) < 1) stop(paste(image, "has zero size"))

	# array indices that are the pattern color
	patternidx <- which(img[,,4] == 1 & img[,,1] == patcol[1] & img[,,2] == patcol[2] & img[,,3] == patcol[3], arr.ind=TRUE)
	if (length(patternidx) < 1) return(0)
	
	# define portion of image to analyze

	xfrac <- 0.5 # proportion of max horizontal color length for row to be analyzed
	xmax <- length(unique(imgidx[,2]))
	xcutoff <- floor(xfrac * xmax)

	yfrac <- 0.5 # proportion of max vertical color length for column to be analyzed
	ymax <- length(unique(imgidx[,1]))
	ycutoff <- floor(yfrac * ymax)

	# find average longest horizontal track length

	hmin <- 20 # minimum horizontal track length for pattern to be analyzed 
	patidx <- numeric()
	pattern_lengths <- numeric()
	n <- 1
	havg <- 0
	
	for (i in unique(patternidx[,1])) {
		rowlen <- length(unique(imgidx[,2][which(imgidx[,1] == i)]))
		if (rowlen >= xcutoff) {
			# calculate max horizontal pattern proportion
			patidx  <- patternidx[,2][which(patternidx[,1] == i)]
			diffruns <- rle(diff(patidx))
			pattern_lengths <- diffruns$lengths[which(diffruns$values == 1)]
			hmax <- ifelse(length(pattern_lengths) > 0, max(pattern_lengths)+1, 0) # max pattern width
			if (hmax > hmin) {
				hmax_ratio <- hmax/rowlen
				# calculate average horizontal track length ratio
				havg <- havg + (hmax_ratio - havg)/n
				n <- n + 1
			}
		}
	}
	#print(havg)

	# find average longest vertical track length

	vmin <- 20 # minimum vertical track length for pattern to be analyzed
	n <- 1
	vavg <- 0

	for (i in unique(patternidx[,2])) {
		collen <- length(unique(imgidx[,1][which(imgidx[,2] == i)]))
		if (collen >= ycutoff) {
			# calculate max vertical pattern proportion
			patidx <- patternidx[,1][which(patternidx[,2] == i)]
			diffruns <- diffruns <- rle(diff(patidx))
			pattern_lengths <- diffruns$lengths[which(diffruns$values == 1)]
			vmax <- ifelse(length(pattern_lengths) > 0, max(pattern_lengths)+1, 0) # max pattern height
			if (vmax > vmin) {
				vmax_ratio <- vmax/collen
				# calculate average vertical track legnth ratio
				vavg <- vavg + (vmax_ratio - vavg)/n
				n <- n + 1
			}
		}
	}
	#print(vavg)
	
	# calculate track score
	trackscore <- havg - vavg;

	return(trackscore)
}


patRotList <- function(imageDir, imageList, suffix="png", pattern_color) {

	# imageDir: directory containing images
	# imageList: a text file with the names of images to be analyzed
	# suffix: suffix of the image files
	# pattern_color: RGB value of pattern color

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
		rotvals$rotation[i] <- trackScore(image=image, pattern_color=pattern_color)
	}

	return(rotvals)
}
