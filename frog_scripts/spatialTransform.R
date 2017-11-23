# spatialTransform.R

# Rescales and Rotates images

standardizeImage <- function(image, scaleLength, measurefile, cropfile, outname) {

	# image: JPEG image file to rescale and rotate
	# scaleLength: rostrum to pelvis length of frog to scale to
	# measurefile: ImageJ length and angle measurement file
	# cropfile: ImageJ XY cropping coordinate file
	# outname: name of output JPEG file

	if (scaleLength <= 0) stop(paste(scaleLength, "is an invalid scaling length"))

	# read in image
	img <- readImage(image)

	# read in imageJ files
	measure <- read.table(measurefile)
	cropdim <- read.table(cropfile, head=FALSE)

	# crop image
	img <- img[(cropdim[1,1]:cropdim[2,1]), (cropdim[1,2]:cropdim[3,2]),]

	# calculate new image width

	c1 <- measure$Length
	b1 <- c1 * cos(measure$Angle)
	w1 <- dim(img)[1]

	c2 <- 2114 # target length
	b2 <- c2 * cos(measure$Angle)
	w2 <- b2/(b1/w1)

	# resize image
	img <- resize(img, w=w2)

	# rotate image
	img <- rotate(img, measure$Angle, bg.col = "white")

	# output image
	writeImage(img, files=outname, type='jpeg')
}

standardImgList <- function(imageDir, imageList, suffix="JPG", scalingLength, measureList, cropList, outDir) {
	
	# imageDir: Directory containining images
	# imageList: List of image names
	# suffix: image suffix
	# scalingLength: rostrum to pelvis length of frog to scale to
	# measureList: imageJ straight line measurements (length and angle)
	# cropList: XY coordinates from imageJ rectangle tool for cropping
	# outDir: output directory

	# the order of samples in imageList, measureList, and cropList must be the same

	# load EBImage library
	if (! "EBImage" %in% (.packages())) library(EBImage)

	# prepare image directory
	imageDir <- gsub("/$", x=imageDir, replacement='')

	# prepare output directory
	outDir <- gsub("/$", x=outDir, replacement='')

	# read in images
	imagevec <- as.vector(read.table(imageList)$V1)
	nsamples <- length(imagevec)

	# read in image measurements
	measurevec <- as.vector(read.table(measureList)$V1)
	if (nsamples != length(measurevec)) stop("Number of images does not match number of image measurement files")

	# read in cropList
	cropvec <- as.vector(read.table(cropList)$V1)
	if (nsamples != length(cropvec)) stop("Number of images does not match number of crop coordinate files")

	# process images
	for (i in 1:nsamples) {
		print(imagevec[i])
		inimg <- paste(imageDir, "/", imagevec[i], ".", suffix, sep='')
		outimg <- paste(outDir, "/", imagevec[i], "_std.", suffix, sep='')
		standardizeImage(image=inimg, scaleLength=scalingLength, measurefile=measurevec[i], cropfile=cropvec[i], outname=outimg)
	}
}
