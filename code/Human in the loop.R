# Credits:
#  Sara Vanaki, Dvora Hart, Jui-Han Chang 2022 - 2025
#  Please cite our paper:
#      How many dollars are in the sea?
#      Ecological Informatics 2025

# Browser to look through viame detections (or other annotations in the same format)
# Written by Dvora Hart and Sara Vanaki, example used for correcting annotations
# that will be used for iteration 2 of the yolov7 model. 

# How to (3 steps):
# click on an image to advance
# user gives the reason for the wrong ml image
# write the output and reason to a file
library(imager)
library(data.table)
library(utils)
library(shiny)
library(tcltk)

PATH_TO_APP <<- "C:/YourName/Project/PathToApp/AnnotatingImgs"
WRKNG_DIR <<- "C:/YourName/Project"
# lastdollars or your folder with images
PATH_TO_IMGS <<- "C:/YourName/Project/lastdollars"  
# TESTINGon07Jan2025fromI1yolov7.csv or your ouput file name
PATH_TO_OUTPUT <<-"C:/YourName/Project/TESTINGon07Jan2025fromI1yolov7.csv"
# yolov7_2016images2.csv or your input file
autodollarsLeft_FILE <<- 'yolov7_2016images2.csv'

# Path to where you want to copy the images
PATH_TO_COPY <- "C:/YourName/Project/copied_images"  

file_path <- file.path(WRKNG_DIR, autodollarsLeft_FILE)
csv_data <- read.csv(file_path)
image_names <- unique(csv_data[, 2])
number_of_unique_values <- length(image_names)

image_files_to_copy <- file.path(PATH_TO_IMGS, image_names)
existing_images <- image_files_to_copy[file.exists(image_files_to_copy)]

if (!dir.exists(PATH_TO_COPY)) {
  dir.create(PATH_TO_COPY)
}

file.copy(existing_images, PATH_TO_COPY)

waiting <- function() {
  tt2 <- tktoplevel()
  tkwm.title(tt2, "Testing Phase: Hollings 22 - SV")
  
  FP <- function() {
    falseP <<- "TRUE"
    tkdestroy(tt2)
  }
  FP.but <- tkbutton(tt2, text = "False positive", command = FP)
  
  MC <- function() {
    runApp(PATH_TO_APP)
    MisC <<- theUser
    tkdestroy(tt2)
  }
  MC.but <- tkbutton(tt2, text = "Misclassified", command = MC)
  
  tkgrid(tklabel(tt2, text = "Why is this(blue) annotation incorrect?"), columnspan = 3, pady = 10)
  tkgrid(FP.but, MC.but, pady = 10, padx = 10)
  
  tkwait.window(tt2)
}

#Function to save progress
save_progress <- function(){
  write.csv(correctnessData,PATH_TO_OUTPUT,quote = FALSE, row.names = FALSE)
  print("progress saved.")
}

plotautodetect <- function(autoimage, autocolor, linewidth) {
  ndetections <- dim(autoimage)[1]
  if (ndetections > 0) {
    for (icount in 1:ndetections) {
      thedetection <- autoimage[icount, ]
      polygon(x = c(thedetection$TLx, thedetection$BRx, thedetection$BRx, thedetection$TLx), y = c(thedetection$TLy, thedetection$TLy, thedetection$BRy, thedetection$BRy),
              border = autocolor, lwd = linewidth)
      TLY <- as.numeric(thedetection$TLy); TLX <- as.numeric(thedetection$TLx); BRX <- as.numeric(thedetection$BRx); BRY <- as.numeric(thedetection$BRy)
      conf <- as.numeric(thedetection$Conf)
      xoffset <- abs(TLX - BRX) / 2 + 3
      if (TLY > 1000) text(x = TLX + xoffset, y = TLY - 15, labels = paste0(thedetection$Spname, ' ', round(conf, 2)), col = autocolor, cex = 0.9)
      if (TLY <= 1000) text(x = TLX + xoffset, y = BRY + 17, labels = paste0(thedetection$Spname, ' ', round(conf, 2)), col = autocolor, cex = 0.9)
    }
  }
}

# Function to add a new annotation if it doesn't already exist
add_annotation <- function(uniqueID, imageid, spname, conf, tlx, tly, brx, bry, correct, fp, mc, fn, imagename) {
  new_row <- data.frame(UniqueID = uniqueID, Imageid = imageid, Spname = spname, Conf = conf, TLx = tlx, TLy = tly, BRx = brx, BRy = bry, Correct = correct, FP = fp, MC = mc, FN = fn, Imagename = imagename)
  
  # Check if the new row already exists in correctnessData
  if (!any(apply(correctnessData, 1, function(row) all(row == new_row)))) {
    correctnessData <<- rbind(correctnessData, new_row)
    print(paste("Adding annotation:", uniqueID, imageid, spname, conf, tlx, tly, brx, bry, correct, fp, mc, fn, imagename))
  } else {
    print(paste("Duplicate annotation found, not adding:", uniqueID, imageid, spname, conf, tlx, tly, brx, bry, correct, fp, mc, fn, imagename))
  }
}

options(stringsAsFactors = FALSE)
setwd(WRKNG_DIR)

NumImages <- number_of_unique_values
autodollars <- data.table(read.csv(autodollarsLeft_FILE))
autodollarsLeft <- subset(autodollars, (TLx + BRx) / 2 < 1361)

# Initialize correctnessData
correctnessData <- data.frame(matrix(ncol = 13, nrow = 0))
colnames(correctnessData) <- c('UniqueID', 'Imageid', 'Spname', 'Conf', 'TLx', 'TLy', 'BRx', 'BRy', 'Correct', 'FP', 'MC', 'FN', 'Imagename')

# # Filter out detections with confidence less than 0.2 and mark them as false positives
# low_conf_detections <- subset(autodollarsLeft, Conf < 0.2)
# for (i in 1:nrow(low_conf_detections)) {
#  theUniqueID <- i
#  add_annotation(theUniqueID, low_conf_detections$Imageid[i], low_conf_detections$Spname[i], low_conf_detections$Conf[i], low_conf_detections$TLx[i], low_conf_detections$TLy[i], low_conf_detections$BRx[i], low_conf_detections$BRy[i], "FALSE", "TRUE", "NA", "FALSE", low_conf_detections$Imagename[i])
# }
# # Filter out low confidence detections from autodollarsLeft
# autodollarsLeft <- subset(autodollarsLeft, Conf >= 0.2)

imagelist <- subset(autodollarsLeft, !duplicated(Imagename))
autocolor <- 'yellow2'
mancolor <- 'cyan1'
multicolor <- 'orange2'
windows(height = 12, width = 16.8)
lastimagename <- 'null'
i <- 1
theUniqueID <- nrow(correctnessData)  # Start unique ID from the last added ID


setwd(PATH_TO_IMGS)
repeat {
  iid <- imagelist$Imageid[i]
  print(iid)
  iname <- imagelist$Imagename[i]
  thisimage <- load.image(iname)
  plot(thisimage)
  text(x = 100, y = 1100, labels = paste0('Image#', i))
  text(x = 1500, y = 1100, labels = iname)
  
  thisimageautodollarsLeft1 <- subset(autodollarsLeft, Imagename == iname)
  boxsum <- (thisimageautodollarsLeft1$TLx + thisimageautodollarsLeft1$TLy + thisimageautodollarsLeft1$BRx + thisimageautodollarsLeft1$BRy)
  thisimageautodollarsLeft <- thisimageautodollarsLeft1[order(boxsum), ]
  plotautodetect(thisimageautodollarsLeft, autocolor, 2)
  machine_images <- dim(thisimageautodollarsLeft)
  numofMachineImgs = machine_images[1]
  
  
  
  if (numofMachineImgs != 0) {
    for (x in 1:numofMachineImgs) {
      theUniqueID = theUniqueID + 1
      falseP <<- "FALSE"
      MisC <<- "NA"
      falseN <<- "FALSE"
      
      polygon(x = c(thisimageautodollarsLeft[x]$TLx, thisimageautodollarsLeft[x]$BRx, thisimageautodollarsLeft[x]$BRx, thisimageautodollarsLeft[x]$TLx),
              y = c(thisimageautodollarsLeft[x]$TLy, thisimageautodollarsLeft[x]$TLy, thisimageautodollarsLeft[x]$BRy, thisimageautodollarsLeft[x]$BRy),
              border = "blue", lwd = 5)
      
      correctness = "FALSE"
      answer <- winDialog("yesno", "Is this(blue) annotation correctly identified?")
      if (answer == 'YES') {
        correctness = "TRUE"
      } else {
        waiting()
      }
      
      add_annotation(theUniqueID, thisimageautodollarsLeft$Imageid[x], thisimageautodollarsLeft$Spname[x], thisimageautodollarsLeft$Conf[x], thisimageautodollarsLeft$TLx[x], thisimageautodollarsLeft$TLy[x], thisimageautodollarsLeft$BRx[x], thisimageautodollarsLeft$BRy[x], correctness, falseP, MisC, falseN, thisimageautodollarsLeft$Imagename[x])
      
      polygon(x = c(thisimageautodollarsLeft[x]$TLx, thisimageautodollarsLeft[x]$BRx, thisimageautodollarsLeft[x]$BRx, thisimageautodollarsLeft[x]$TLx),
              y = c(thisimageautodollarsLeft[x]$TLy, thisimageautodollarsLeft[x]$TLy, thisimageautodollarsLeft[x]$BRy, thisimageautodollarsLeft[x]$BRy),
              border = multicolor, lwd = 3)
    }
  }
  
  
  secondPop <- winDialog("yesno", "Is there any we missed?")
  if (secondPop == "YES") {
    theUniqueID = theUniqueID + 1
    correctness = "FALSE"
    falseP <<- "FALSE"
    MisC <<- "NA"
    falseN <<- "TRUE"
    User_chooses <<- "NA"
    flag = TRUE
    while (flag) {
      answer <- winDialog("ok", "Click on the Top Left coordinate and the Bottom Right coordinate of the sea creature.")
      TopLeft <<- locator(1)
      BottomRight <<- locator(1)
      polygon(x = c(TopLeft$x, BottomRight$x, BottomRight$x, TopLeft$x),
              y = c(TopLeft$y, TopLeft$y, BottomRight$y, BottomRight$y),
              border = "yellow", lwd = 5)
      winDialog("ok", "Your sea creature is named Sand Dollar for this data set.")
      User_chooses <<- "sanddollars"
      
      add_annotation(theUniqueID, iid, User_chooses, "NA", round(TopLeft$x, digits = 3), round(TopLeft$y, digits = 3), round(BottomRight$x, digits = 3), round(BottomRight$y, digits = 3), "FALSE", "FALSE", "NA", "TRUE", iname)
      
      checkLoop <- winDialog("yesno", "Did the machine miss another sea creature?")
      if (checkLoop == "NO") {
        flag = FALSE
      }
    }
    print(c(theUniqueID, iid, User_chooses))
  }
  winDialog("ok", "Proceed to the next image with a click anywhere on the current image.")
  pos <- locator(1)
  if (i == NumImages) break
  i <- i + 1
  lastimagename <- iname
  save_progress()
}
#write.csv(correctnessData, PATH_TO_OUTPUT, quote = FALSE, row.names = FALSE)
save_progress()
text(x = 1500, y = 1200, 'Browser exited', cex = 3)