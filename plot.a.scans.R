 ###plot.a.scans <- function(...) {
# Giulio Curioni
# December 2015

#imports GPR sections taken during the UoB monitoring and plot A-scans corresponding to the (approximate) apex of the buried pipes 
#plot the A-scans in the same graph for comparison

## note: the GPR image has a range of 512 (pixels, representing the depth)
## the range in time is 128 ns, therefore each pixel represents 128/512 = 0.25 ns
## ## IMPORTANT: 128 ns corresponds to the range of the raw images. The standard processing by GRED moves the start time,
## reducing the total vertical range in ns
## however, it is thought that the "length" of each pixel remains the same, and simply a portion of the original image is shown after processing
## therefore each pixel should be 0.25 ns even in the processed images


# check ### before launching the script


#close all existing plots
graphics.off()



#runs myRGB in case I want to use this for colors
source("C:\\R.wd\\scripts\\operations\\myrgb.R"); myrgb()



message("copy GPR images (B-scans) in a folder called: C:/R.wd/gpr transects")
message("")
setwd("C:/R.wd/gpr transects")
n <- length(list.files())   # number of GPR images (B-scans, i.e. transects) to plot



### if files have not been analysed previously use this, otherwise comment:
#      new.filenames <- substring(list.files(), 1, 12)
#      file.rename(from=list.files(), to=paste(new.filenames, ".txt", sep=""))       ## ## ## rename filenames without importing the files!!!

          gpr.filename <- list.files()
          
          
          
           # create objects for each pipe
           pipe1 <- matrix(nrow=512, ncol=n)   #note: 512 is the range (=number of pixels in the y-axis; each pixel is 0.25 ns)
           pipe2 <- pipe1
           pipe3 <- pipe1
           pipe4 <- pipe1
           pipe5 <- pipe1

for(i in 1:n) {
          #gpr <- read.table(gpr.filename[i], sep='\t', skip=20)
          
          # use readr package for faster import:
          gpr <- read_delim(gpr.filename[i], delim = "\t", col_names = F,   
                 locale = locale(date_names = "en", date_format = "%Y-%m-%d", time_format = "%H:%M", decimal_mark = ".", tz = "UTC"),
                 na = "NA", skip = 20, n_max = -1)
          gpr <- data.frame(gpr)
          
          # import header to find range (depth) and number of A-scans, i.e. SWEEP along the transect:
          header <- read_delim(gpr.filename[i], delim = "\t", col_names = F, col_types = "c", 
                 locale = locale(date_names = "en", date_format = "%Y-%m-%d", time_format = "%H:%M", decimal_mark = ".", tz = "UTC"),
                 na = "NA", skip = 0, n_max = 20)
          header <- data.frame(header)
                 
          range.gpr <- as.numeric(header[17,1])				#512 is the range (depth), i.e. number of pixels        
          transect.len <- as.numeric(header[19,1])    #number of SWEEPS acquired (= A-scans), i.e. length of the transect       

          ### define title for images (corresponding to the imported file. LID1 is 700MHz, LID2 is 250MHz):
          ## ## mytitle <- c("2013-04-24 700MHz")
          mytitle <- substring(gpr.filename[i], 1, 12)  # or use nchar() instead of 12 !


          ### define Ea to have depth (m)   [only if depth is necessary!]
          #Ea <-  7.05

          #transect.len <- length(gpr[,1])/512	#length of transect  (= NUMBER OF A-SCANS)
          message(paste("the number of A-scans for", mytitle, "is:", transect.len, sep=" "))
          

          ##note that in image() the columns are the x-axis (i.e. transect),
          ##the rows are the y-axis (i.e. depth or time)
          ##defines matrix dimensions (transect.len x range.gpr)

          xx <<- c(1:transect.len)  #columns
          yy <<- c(1:range.gpr)     #rows


          gpr <- as.matrix(gpr)
          aa <- array(gpr, dim=c(range.gpr, transect.len))


          zz <<- t(aa)          ## ## this contains the data (reflections)
             
              
          sampling.res <- 0.025           ### sampling resolution (m) of the GPR (IDS samples every approx 2.5 cm), modify if necessary
          L.transect <- transect.len * sampling.res            # approx length of the transect (m)
          xlabel <- seq(0, L.transect-1, by=1)               #labels on x-axis (distance) for UoB test site:  ## modify seq if necessary!
          xlabel.index <- round(xlabel/0.0232)      #x coord (distance along the transect): gpr takes measurments every 2.32 cm (the labels are not in their exact position, but I need to round the numbers)




              








 
           ## save the A scans corresponding to the buried pipes (approximately to their apeces):
           
           index.apex.pipes <- c(180, 201, 224, 246, 275)
                      
           pipe1[,i] <- zz[index.apex.pipes[1],]
           pipe2[,i] <- zz[index.apex.pipes[2],]
           pipe3[,i] <- zz[index.apex.pipes[3],]
           pipe4[,i] <- zz[index.apex.pipes[4],]
           pipe5[,i] <- zz[index.apex.pipes[5],]
} # end of for(i in 1:n)




                   ### ###
                  stop("maybe better to select a section of the A-scans near the pipe apex and compare only this section. Use zoom to find y-indeces")
                   

                 ## plot A-scans from different radar sections in the same graph
                 par(new=F)
                 windows()
                 for(pp in 1:n) {
                       plot(pipe1[,pp], yy, type='l', ylim = rev(range(yy)), xlim = range(zz), ylab = "index for time/depth", xlab = "Single trace crossing the apex of pipe 1")
                       par(new=T)
                 }
                 
                par(new=F) 
                windows()                 
                for(pp in 1:n) {
                       plot(pipe2[,pp], yy, type='l', ylim = rev(range(yy)), xlim = range(zz), ylab = "index for time/depth", xlab = "Single trace crossing the apex of pipe 2")
                       par(new=T)
                 }
                par(new=F) 
                windows()                 
                for(pp in 1:n) {
                       plot(pipe3[,pp], yy, type='l', ylim = rev(range(yy)), xlim = range(zz), ylab = "index for time/depth", xlab = "Single trace crossing the apex of pipe 3")
                       par(new=T)
                 }
                par(new=F) 
                windows() 
                for(pp in 1:n) {
                       plot(pipe4[,pp], yy, type='l', ylim = rev(range(yy)), xlim = range(zz), ylab = "index for time/depth", xlab = "Single trace crossing the apex of pipe 4")
                       par(new=T)
                 }                
                par(new=F) 
                windows()                 
                for(pp in 1:n) {
                       plot(pipe5[,pp], yy, type='l', ylim = rev(range(yy)), xlim = range(zz), ylab = "index for time/depth", xlab = "Single trace crossing the apex of pipe 5")
                       par(new=T)
                 } 
                 
                 
          write.table(pipe1, "C:\\R.wd\\pipe1.txt", sep='\t', row.names=F)
          write.table(pipe2, "C:\\R.wd\\pipe2.txt", sep='\t', row.names=F)
          write.table(pipe3, "C:\\R.wd\\pipe3.txt", sep='\t', row.names=F)
          write.table(pipe4, "C:\\R.wd\\pipe4.txt", sep='\t', row.names=F)
          write.table(pipe5, "C:\\R.wd\\pipe5.txt", sep='\t', row.names=F)  
          
          
          
          
###}                 