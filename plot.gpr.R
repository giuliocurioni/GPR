###plot.gpr <- function(...) {
# Giulio Curioni
# December 2015

#imports ascii files exported by GRED (generally already processed in GRED with Processing_standard_200_600.gpr) 
#and plots the resulting GPR image(s)!
#it can also plot single A-scans by selecting the right index (search ### ### in the script)

## note: the GPR image has a range of 512 (pixels, representing the depth)
## the range in time is 128 ns, therefore each pixel represents 128/512 = 0.25 ns
## ## IMPORTANT: 128 ns corresponds to the range of the raw images. The standard processing by GRED moves the start time,
## reducing the total vertical range in ns
## however, it is thought that the "length" of each pixel remains the same, and simply a portion of the original image is shown after processing
## therefore each pixel should be 0.25 ns even in the processed images

## NOTE: depth labels are likely to be wrong. Check y.depth1 and modify rounding in aa1, aa2 etc. (see below)


# check ### before launching the script


#close all existing plots
graphics.off()

## use x11() or windows() to make the picture to full screen
    ## IMP NOTE: IF I USE DIFFERENT COMPUTERS THE IMAGES MIGHT BE OF DIFFERENT DIMENSIONS!!!
    ## IT IS PROBABLY BETTER TO SPECIFY THE DIMENSIONS!



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
          mytitle <- substring(gpr.filename[i], 1, 11)   # or use nchar() instead of 11 !


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



                                 
              





           ### ### to plot a single A scan:
#           windows()
#           transect.index <- 101      ## define which index (along the transect) to plot 
#           plot(zz[transect.index,], yy, type='l', ylim = rev(range(yy)), xlim = range(zz), ylab = "index for time/depth", xlab = paste("A-scan at transect.index:", transect.index, sep=' '))
#           write.table(zz[transect.index,], paste("C:\\R.wd\\a.scan_", transect.index, "_", mytitle, ".txt", sep=''), row.names=F)
           
                    
           
           ### to plot a single reflection trace along the transect at a certain depth/time:
#           windows()
#           nanosec <- 10
#           depth.index <- nanosec/0.25     ## define which index (depth/time) to plot 
#           plot(xx, zz[,depth.index], type='l', ylim = c(range(zz)), ylab = paste("reflection trace at depth.index:", depth.index, sep=' '), xlab = "index for transect")



           # ZERO TIME CORRECTION: [IMPORTANT: THIS MIGHT BE RELIABLE ONLY ON PROCESSED DATA!]
           # the first depth.index with values different from zeros is the new time zero
           for (nanosec in seq(0.25, 10, by=0.25)) {        ### ### ??? start from 7 or another value instead of 0.25 if the zero time is not correct (sometimes there are reflections in the air that shouldn't appear in this region...)
                 depth.index <- nanosec/0.25     ## define which index (depth/time) to plot 
                 #plot(xx, zz[,depth.index], type='l', ylim = c(range(zz)), ylab = paste("reflection trace at depth.index:", depth.index, sep=' '), xlab = "index for transect")
                 ro.depth.sum <- sum(abs(zz[,depth.index]))      # sum the absolute reflections at 'depth index' along the GPR transect
                 if(ro.depth.sum > 50) {                         # value empirically chosen IN PROCESSED DATA (even in processed data, the first depth.indeces do not have exact zero values so the sum along a certain depth.index (i.e. along the GPR transect) can be 10, 20 or more. When the actual reflection starts (see GPR image) the values are significantly different from zero. Use this as the zero time (i.e. the first depth.index with values different from zeros is the new time zero)
                                 zero.time <- nanosec
                                 zero.time.index <- zero.time/0.25
                                 print(paste("sum of reflections along the GPR transect at depth.index: ", depth.index, " = ", ro.depth.sum, sep=''))
                                 print(paste("zero time (ns): ", zero.time, sep=''))
                                 print(paste("zero time index: ", zero.time.index, sep=''))
                                 
                                 if(depth.index == 1) { 
                                     depth.index <- 2          # this is necessary for plotting
                                     windows(h=8, w=12)
                                     plot(xx, zz[,depth.index-1], type='l', ylim = c(range(zz)), ylab = paste("reflection trace at depth.index:", depth.index, sep=' '), xlab = "index for transect")
                                     depth.index <- 1          # set it back again to 1
                                     }  else  {
                                  windows(h=8, w=12)
                                  plot(xx, zz[,depth.index-1], type='l', ylim = c(range(zz)), ylab = paste("reflection trace at depth.index:", depth.index, sep=' '), xlab = "index for transect")  }
                                  
                                 par(new=T)
                                 plot(xx, zz[,depth.index], type='l', ylim = c(range(zz)), ylab = paste("reflection trace at depth.index:", depth.index, sep=' '), xlab = "index for transect", col='red')
                                 title("red is zero.time.index, black is zero.time.index-1")
                                 savePlot(filename = paste("C:/R.wd/", mytitle, "zero.time.bmp", sep=""), type = c("bmp"))
                                 break
                                 }
           }







                       










          ### comment if(FALSE{} to plot the entire original image 
                    ### if(FALSE) {

                    ## plots entire image

                    windows(h=8, w=12)
                    par(mar=c(6, 5, 4, 6) + 0.1)
                    image(xx,yy,zz, ylim = rev(range(yy)), col=sapply((0:32)/32, gray), xlab=NA, ylab=NA, xaxt="n", yaxt="n")                ## gray scale
                    #image(xx,yy,zz, ylim = rev(range(yy)), col=gray(0:(2*length(pretty(zz)))/(2*length(pretty(zz)))), xlab=NA, ylab=NA, xaxt="n", yaxt="n")                ## gray scale
                    #image(xx,yy,zz, ylim = rev(range(yy)), col=gray.colors(64, start = 0, end = 1, gamma = 2.2, alpha = NULL), xlab=NA, ylab=NA, xaxt="n", yaxt="n")                ## gray scale
                         
                    #image(xx,yy,zz, ylim = rev(range(yy)), col=myRGB[seq(60,100, by=3)], xlab=NA, ylab=NA, xaxt="n", yaxt="n")               ## my colors
                    #image(xx,yy,zz, ylim = rev(range(yy)), col=heat.colors(12), xlab=NA, ylab=NA, xaxt="n", yaxt="n")                         ## default

                    y.time <- yy * 0.25                       #this corresponds to the time in ns (each vertical pixel is 0.25 ns)
                    ylabel <- seq(10, 120, by=10)             #these are the labels on y-axis (I don't use y.time to have better values, e.g. 10, 20 etc. instead of 11.25, 15 etc.
                    ylabel.index <- ylabel/0.25               #these are the y coord (pixel number) where to put the labels

                    title(mytitle, cex.main=1.5, font.main=1)
                    axis(1, at = xlabel.index, labels = F, tcl=-0.5)
                    text(x = xlabel.index, labels = xlabel, par("usr")[3], srt = 0, pos = 1, offset = 1, xpd = TRUE, font=1, cex=1.8)
                    mtext("transect length (m)", col="black", 1, line=3.5, font=1, cex=2.0)
                    ## arrows(x0 = 50, y0 = 570, x1 = transect.len-50, y1 = 570, length = 0.2, angle = 20, col="black", lwd=3, xpd=T)
                    axis(2, at = ylabel.index, labels = F, tcl=-0.5)
                    text(y = ylabel.index, labels = ylabel, par("usr")[1], srt = 0, pos = 2, offset = 1, xpd = TRUE, font=1, cex=1.8)
                    mtext("time (ns)", col="black", 2, line=3.5, font=1, cex=2.0)
                    box(which = "plot", lty = "solid")

                    savePlot(filename = paste("C:/R.wd/", mytitle, ".bmp", sep=""), type = c("bmp"))

                    #depth calculation: ## maybe better not to do it for entire image
                    #y.depth <- velocita * (y.time/1e9)      #depth in metres
                    #ylabel.depth <- round(y.depth[ylabel.index], digits=2)
                    #axis(4, at = ylabel.index, labels = F, tcl=-0.5)
                    #text(y = ylabel.index, labels = ylabel.depth, par("usr")[2], srt = 0, pos = 4, offset = 1, xpd = TRUE, font=1, cex=1.8)
                    #mtext("depth (m).. wrong!", col="black", 4, line=5, font=1, cex=1.8)



                    ###} ## end of if(FALSE) {}









           
           
         ### comment if(FALSE{} to plot the upper part of the original image WITH ZERO TIME CORRECTION
         ### if(FALSE) {
          ## plots only upper part of the image up to a certain index, such as 400 (400 * 0.25 ns = 100 ns; this is the interesting bit, with targets):


          n.rows.to.del <- zero.time.index-1         # add -1 because zero.time.index is the new zero, I need to delete what came before (note: each row is 0.25 ns)
          max.depth.index <- 220                     #empirically define max depth.index to be plotted
          max.ns <- (max.depth.index*0.25)-10                    #define time in ns corresponding to max.depth.index (subtract 10 ns for better y-axis labesl)

          z1 <- zz[,n.rows.to.del:max.depth.index]               ##this corrects for zero time and selects the upper part of the image (up to index max.depth.index, with all the interesting elements)
          y1 <- c(1:length(n.rows.to.del:max.depth.index))       ##I only need up to y-index defined by length(n.rows.to.del:max.depth.index) after deleting 'n.rows.to.del' rows (zero time correction)

          windows(w=12, h=8)
          par(mar=c(6, 5, 4, 6) + 0.1)
          image(xx,y1,z1, ylim = rev(range(y1)), col=sapply((0:32)/32, gray), xlab=NA, ylab=NA, xaxt="n", yaxt="n")
          #image(xx,yy,zz, ylim = rev(range(yy)), col=gray(0:26/26), xlab=NA, ylab=NA, xaxt="n", yaxt="n")                ## gray scale
          #image(xx,yy,zz, ylim = rev(range(yy)), col=gray(0:(2*length(pretty(zz)))/(2*length(pretty(zz)))), xlab=NA, ylab=NA, xaxt="n", yaxt="n")                ## gray scale
                    
          title(mytitle, cex.main=1.5, font.main=1)
          axis(1, at = xlabel.index, labels = F, tcl=-0.5)
          text(x = xlabel.index, labels = xlabel, par("usr")[3], srt = 0, pos = 1, offset = 1, xpd = TRUE, font=1, cex=1.8)
          mtext("transect length (m)", col="black", 1, line=4, font=1, cex=2.0)
          ## ## arrows(x0 = 50, y0 = 305, x1 = transect.len-50, y1 = 305, length = 0.2, angle = 20, col="black", lwd=3, xpd=T)

          y.time1 <- y1 * 0.25                   #time in ns
#          correction <- 7                        ### ### ???add some nanoseconds to the processed data with GRED. It seems that GRED gets rid of the first bit in air (zero time correction) but also of the noisy reflection at the ground surface. In any case, the nanoseconds calculate don the processed data are not necessarily accurate, this is an empirical method to fix this problem and obtain more accurate labels on the plot.
#          y.time1 <- y.time+correction ### ###
          ylabel1 <- seq(10, max.ns, by=10)      #actual labels
#          ylabel1 <- ylabel1+correction ### ###
          ylabel.index1 <- ylabel1/0.25          #coord for labels

          axis(2, at = ylabel.index1, labels = F, tcl=-0.5)
          text(y = ylabel.index1, labels = ylabel1, par("usr")[1], srt = 0, pos = 2, offset = 1, xpd = TRUE, font=1, cex=1.8)
          mtext("time (ns)", col="black", 2, line=3.5, font=1, cex=2.0)
          ###} # end of if(FALSE){}
          
          
          #saves plot anyway, in case I don't have Ea
          #savePlot(filename = paste(substring(mytitle, 1,17), ".pdf", sep=""), type = c("pdf"))
          ### savePlot(filename = paste(gpr.filename, ".jpg", sep=""), type = c("jpg"))  ### use this for single file analisis
          ###savePlot(filename = paste(substring(gpr.filename[i], 1, 12), ".jpg", sep=""), type = c("jpg"))

          savePlot(filename = paste("C:/R.wd/", mytitle, ".upper.bmp", sep=""), type = c("bmp"))
          
          
        # graphics.off() 
          

} #end of for () {}




          






          message(" ")
          print("matrix with plotted data are stored in object zz")
          print("IMP! zz is a matrix with rows = x-axis (horizontal transect) and columns = y-axis (depth or time)")
          print("e.g. locator(1): $x 181.5223, $y 66.27795 corresponds to the element at approx row: 181, col: 66")
          print("check within the script if vertical A-scans or single traces along the transect and at specific depths are wanted") 
          message("ignore error 'object Ea not found' if I don't want depth")
          
          
          
          
          
          
          
          
          
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #          







### remove if(FALSE{} to also plot the depth
          if(FALSE) {

          #depth calculation:
          c0 <- 3e8
          velocita <- c0/sqrt(Ea)                 ## if it's not defined within the script,
                                                  ## define Ea (permittivity value), before launching the script
                                                  ## USE CORRESPONDING VALUE OBTAINED AFTER FITTING HYPERBOLAE

          pixel.depth <- (velocita * 0.25e-9)/2      #depth in metres of a single pixel (I must divide by 2 to account for two-way travel)
          y.depth1 <- y1 * pixel.depth               #depth in metres

          ##use this if I want labels corresponding to the delay in nanoseconds
          ##ylabel.depth1.coord <- round(y.depth1[ylabel.index1], digits=2)     #coord and labels

          ##use this to have 0.5, 1.0, 1.5 as labels
          ## ## sometimes there isn't a corresponding value equal to 0.50, 1.00 etc.
          ## ## e.g. I have in y.depth1  1.006 (round -> 1.01). If labels are wrong check y.depth1
          ## ## and modify here (e.g. which(round(y.depth1, digits=2) == 1.01) etc.
          aa1 <- which(round(y.depth1, digits=2) == 0.50)
                if(length(aa1) == 0) {aa1 <- which(round(y.depth1, digits=2) == 0.51)}
                    if(length(aa1) == 0) {aa1 <- which(round(y.depth1, digits=2) == 0.52)}
                          if(length(aa1) > 1) {aa1 <- aa1[1]}
          aa2 <- which(round(y.depth1, digits=2) == 1.00)
                if(length(aa2) == 0) {aa2 <- which(round(y.depth1, digits=2) == 1.01)}
                    if(length(aa2) == 0) {aa2 <- which(round(y.depth1, digits=2) == 1.02)}
                          if(length(aa2) > 1) {aa2 <- aa2[1]}
          aa3 <- which(round(y.depth1, digits=2) == 1.50)
                if(length(aa3) == 0) {aa3 <- which(round(y.depth1, digits=2) == 1.51)}
                    if(length(aa3) == 0) {aa3 <- which(round(y.depth1, digits=2) == 1.52)}
                          if(length(aa3) > 1) {aa3 <- aa3[1]}
          aa4 <- which(round(y.depth1, digits=2) == 2.00)
                if(length(aa4) == 0) {aa4 <- which(round(y.depth1, digits=2) == 2.01)}
                    if(length(aa4) == 0) {aa4 <- which(round(y.depth1, digits=2) == 2.02)}
                          if(length(aa4) > 1) {aa4 <- aa4[1]}
          aa5 <- which(round(y.depth1, digits=2) == 2.50)
                if(length(aa5) == 0) {aa5 <- which(round(y.depth1, digits=2) == 2.51)}
                    if(length(aa5) == 0) {aa5 <- which(round(y.depth1, digits=2) == 2.52)}
                          if(length(aa5) > 1) {aa5 <- aa5[1]}
          aa6 <- which(round(y.depth1, digits=2) == 3.00)
                if(length(aa6) == 0) {aa6 <- which(round(y.depth1, digits=2) == 3.01)}
                    if(length(aa6) == 0) {aa6 <- which(round(y.depth1, digits=2) == 3.02)}
                          if(length(aa6) > 1) {aa6 <- aa6[1]}
          aa7 <- which(round(y.depth1, digits=2) == 3.50)
                if(length(aa7) == 0) {aa7 <- which(round(y.depth1, digits=2) == 3.51)}
                    if(length(aa7) == 0) {aa7 <- which(round(y.depth1, digits=2) == 3.52)}
                          if(length(aa7) > 1) {aa7 <- aa7[1]}

          ylabel.depth1.coord <- c(aa1, aa2, aa3, aa4, aa5, aa6, aa7)     #coord for labels
          ylabel.depth1 <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5)           #labels

          #zero.depth <- n.rows.to.del * pixel.depth       ## zero time depth (m)
          #y.depth1 <- y.depth1 - zero.depth
          #y.depth1 <- y.depth1[-1:-n.rows.to.del]
          #ylabel.depth1 <- round(y.depth1[ylabel.index1], digits=2)

          axis(4, at = ylabel.depth1.coord, labels = F, tcl=-0.5)
          text(y = ylabel.depth1.coord, labels = ylabel.depth1, par("usr")[2], srt = 0, pos = 4, offset = 1, xpd = TRUE, font=1, cex=1.8)
          mtext("depth (m)", col="black", 4, line=4, font=1, cex=1.8)
          ## ## savePlot(filename = paste(substring(mytitle, 1,17), ".pdf", sep=""), type = c("pdf"))
          savePlot(filename = paste(substring(gpr.filename[i], 1, 12), ".pdf", sep=""), type = c("pdf"))

          print(y.depth1)
          message("if labels are wrong, first remove (with #) additional unwanted labels in ylabel.depth1.coord and ylabel.depth1: e.g. ylabel.depth1.coord <- c(aa1, aa2, aa3, aa4, aa5) #, aa6, aa7) ")
          message("if this doesn't solve check y.depth1 and manually modify rounding (aa1, aa2 etc.) to correspond to existing values")

          } ## end of if(FALSE) {}
          

#          # check that calculations are correct
#           d <- 0.50*2
#           tt <- 15e-9
#           c <- 3e8
#           k <- 13.5
#           v <- c/sqrt(k)
       
       ##v1 <- d/tt

          
          
###}