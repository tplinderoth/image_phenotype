# rotationScore.R

# the rotation score quantifies pattern rotation

# gap score = (vertical_number_gaps_between_pattern - horizontal_number_gaps_between_pattern)/total_number_gaps
# total_number_gaps = vertical_number_gaps_between_pattern + horizontal_number_gaps_between_pattern
# track score = avg(longest_horizontal_pattern_track/image_row_width) - avg(longest_vertical_pattern_track/image_column_height)
# rotation score = gap_score + track_score

gapScore <- function(patternidx, mingap) {

	# patternidx: matrix of raster coordinates that are pattern color
	# mingap: minimum number of pixels between pattern to be considered a gap

	# check arguments
	if (mingap < 2) stop("mingap must be at least 2 in call to gapScore")

	# check if pattern is present
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

trackScore <- function(imgidx, patternidx, xfrac=0.5, yfrac=0.5, hmin=20, vmin=20) {

	# imgidx: matrix of nontransparent raster coordinates
	# patternidx: matrix of raster coordinates that are pattern color
	# xfrac: minimum proportion of max image width for row to be analyzed
	# yfrac: minimum proportion of max image height for column to be analyzed
	# hmin: minimum horizontal track length for analyzing pattern
	# vmin: minimum vertical track length for analyzing pattern

	# check arguments
	if (xfrac <= 0 || xfrac > 1) stop(paste(xfrac, "for xfrac is out of (0,1] range in call to trackScore"))
	if (yfrac <= 0 || yfrac > 1) stop(paste(yfrac, "for yfrac is out of (0,1] range in call to trackScore"))
	if (hmin <= 0) stop("hmin must be > 0 in call to trackScore")
	if (vmin <= 0) stop("vmin must be > 0 in call to trackScore")
	
	# define portion of image to analyze

	xmax <- length(unique(imgidx[,2]))
	xcutoff <- floor(xfrac * xmax)

	ymax <- length(unique(imgidx[,1]))
	ycutoff <- floor(yfrac * ymax)

	# check if pattern is present
	if (length(patternidx) < 1) return(0)

	# find average longest horizontal track length

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

calcRotation <- function(image, pattern_color, mingap=2, xfrac=0.5, yfrac=0.5, hmin=20, vmin=20) {

	# image: png image to perform calculation on
	# pattern_color: RGB values for pattern color of interest
	# mingap: minimum number of pixels between pattern (gapScore)
	# xfrac: minimum proportion of max image width for row to be analyzed (trackScore)
	# yfrac: minimum proportion of max image height for column to be analyzed (trackScore)
	# hmin: minimum horizontal track length for analyzing pattern (trackScore)
	# vmin: minimum vertical track length for analyzing pattern (trackScore)

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
	
	# calculate gap score
	gapval <- gapScore(patternidx = patternidx, mingap = mingap)

	# calculate track score
	trackval <- trackScore(imgidx = imgidx, patternidx = patternidx, xfrac=xfrac, yfrac=yfrac, hmin=hmin, vmin=vmin)

	# calculate rotation score
	rotscore <- gapval + trackval

	return(rotscore) 
}


patRotList <- function(imageDir, imageList, suffix="png", patcolor, gap=2, xp=0.5, yp=0.5, hmin=20, vmin=20) {

	# imageDir: directory containing images
	# imageList: a text file with the names of images to be analyzed
	# suffix: suffix of the image files
	# patcolor: RGB value of pattern color
	# gap: minimum number of pixels between pattern (gapScore)
	# xp: minimum proportion of max image width for row to be analyzed (trackScore)
	# yp: minimum proportion of max image height for column to be analyzed (trackScore)
	# hmin: minimum horizontal track length for analyzing pattern (trackScore)
	# vmin: minimum vertical track length for analyzing pattern (trackScore)


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
		rotvals$rotation[i] <- calcRotation(image=image, pattern_color=patcolor, mingap=gap, xfrac=xp, yfrac=yp, hmin=hmin, vmin=vmin)
	}

	return(rotvals)
}
