# avgColor.R

# quantifies the average color of an image in various color spaces

avgColorList <- function(imageDir, imageList, suffix='png', rgb='sRGB', colorspace='Lab', white='D65') {
	# imageDir: directory containing images
	# imageList: a text file with the names of images to be analyzed
	# suffix: suffix of image files
	# rgb: input image RGB color space ('sRGB' or 'Apple RGB')
	# colorspace: color space of returned color. See convertColor() 'to' option.
	# white: reference white point

	# prepare image directory
	imageDir <- gsub("/$", x=imageDir, replacement='')

	# read in images
	imagevec <- as.vector(read.table(imageList)$V1)
	nsamples <- length(imagevec)

	# process images
	colors <- matrix(nrow = nsamples, ncol=3)

	for (i in 1:nsamples) {
		image <- paste(imageDir, "/", imagevec[i], ".", suffix, sep='')
		print(imagevec[i])
		if (i < nsamples) {
			colors[i,] <- avgColor(image=image, colspace=colorspace, rgbtype=rgb, refwhite=white)
		}
		else {
			# format color matrix
			avgcol <- avgColor(image=image, colspace=colorspace, rgbtype=rgb, refwhite=white)
			colors[i,] <- avgcol
			dimnames(colors) <- list(imagevec, names(avgcol))
		}
	}

	return(colors)
}

avgColor <- function(image, colspace='sRGB', rgbtype='sRGB', refwhite='D65') {
	# image: png image to analyze
	# colspace: color space of returned color. See convertColor() 'to' option.
	# rgbtype: input image color space ('sRGB' or 'Apple RGB')
	# refwhite: reference white point

	# load png library
	if (! "png" %in% (.packages())) library(png)

	# check parameters
	if (rgbtype != "sRGB" && rgbtype != "Apple RGB") stop(paste(rgbtype, "is not a valid rgb type ('sRGB' or 'Apple RGB')"))

	# read in image
	img <- readPNG(source=image)
	
	# find nontransparent portion of image
	imgidx <- which(img[,,4] == 1)
	rgbmat <- matrix(data=c(img[,,1][imgidx], img[,,2][imgidx], img[,,3][imgidx]), nrow=length(imgidx), ncol=3, dimnames=list(NULL,c('r','g','b')))
	
	# vector for storing average colors
	avgcolor <- rep(NA,3)

	# calculate average RGB value
	rgb <- avgRGB(rgbmat)
	
	# convert RGB to other color space
	if (length(grep('rgb', colspace, ignore.case=TRUE)) < 1)  {
		avgcolor <- convertColor(color=rgb, from=rgbtype, to=colspace, from.ref.white=refwhite)[1,]
		if (colspace == "Lab") names(avgcolor) <- c('L','a','b')
	}
	else {
		avgcolor <- rgb[1,]
	}

	return(avgcolor)
}

avgRGB <- function(rgbvals) {
	# rgbvals: matrix of RGB values

	# calculate average rgb
	nvals <- nrow(rgbvals)
	avgrgb <- matrix(data=c(sqrt(sum(rgbvals[,1]^2/nvals)), sqrt(sum(rgbvals[,2]^2/nvals)), sqrt(sum(rgbvals[,3]^2/nvals))), nrow=1,ncol=3,dimnames=list(NULL,c('r','g','b')))

	return(avgrgb)
}
