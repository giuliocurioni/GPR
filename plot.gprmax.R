###plot.gprmax <- function(...) {
# import ASCII files from gprmax simulations and plot the results

# check ### before launching the script

  #in case I want to use my palette:
  source("C:\\R.wd\\scripts\\operations\\myrgb.R"); myrgb()
  
  
  
 #-------------------- define parameters: --------------------------------

        ### choose the right file name:
        my.gpr <- "60weeks_700MHz_ECC"   #"3months_700MHz_OXC"  #"UoB_wet_250MHz_clutter"

        ## define 'by' in sequence for labelling x-axis (note, this is not in metres, it relates to the number of A-scans, N.step:
        #x.by <- 10

        ### define time window (ns), see time window in gprmax input file:
        time.window <- 80

        ### define N.iterations in sequence for labelling y-axis (see number of iteration calculated in Excel file 'gprmax_parameters'):
        N.iterations <- 33918

        ### define N.step (number of steps)
        #this is the number next to '#analysis:' in the gprmax input file (or just open the ASCII result to see it)
        #N.step <<- as.numeric(readline("Input the number of steps (open gprmax results, in ascii format, to find out):"))
        N.step <- 146



  
  gpr <- read.table(paste("C:\\gprmax\\", my.gpr, ".out", sep=""), sep=' ', skip=20) #note: I can't import the data in a better way than this.
  
  gpr <- gpr[,c(1,4,7,10)]   #the selected columns are TIME(NS), EZ(V/m) (= electric field), HX(A/m), HY(A/m) (= magnetic fields)  
  
  elec.field <- gpr[,2]      #this represents the electric field (EZ (V/m)), see ASCII file
  
  range.gpr <- length(gpr[,1])/N.step       #N.step is the range (depth), i.e. number of pixels
  transect.length <- N.step  			          #length of transect
  

  xx <<- c(1:transect.length)
  yy <<- c(1:range.gpr)
    
  #ee <- as.matrix(elec.field)                                             
  ee <- array(elec.field, dim=c(range.gpr, transect.length))
  
  zz <<- t(ee)
  

  
  #plot an A-scan corresponding to 1/3 of the transect (the antennas are not right above the target).
  #if there are no reflections at all, try other A-scans, if all of them don't have a reflection, the attenuation is very high.
  
  plot(zz[round(N.step/2),], type='l', xlab="index (time)", ylab=paste("A-scan N.:", round(N.step/2), sep=" "))
  
                
  #windows()
  par(mar=c(5, 5, 4, 4) + 0.1)
  #image(xx,yy,zz, ylim = rev(range(yy)), col=heat.colors(192), xlab=NA, ylab=NA, xaxt="n", yaxt="n")
  #image(xx,yy,zz, ylim = rev(range(yy)), col=myRGB[seq(60,100, by=1)], xlab=NA, ylab=NA, xaxt="n", yaxt="n")               ## my colors
  image(xx,yy,zz, ylim = rev(range(yy)), col=sapply((0:90)/90, gray), xlab=NA, ylab=NA, xaxt="n", yaxt="n")





        ## define transect length (m), see domain in x-direction in gprmax input file:
        # note!! this might not be exactly the domain. If I use pml boundaries I normally leave some space before starting
        # the scans. Generally it's 0.5 m both to the left and to the right boundaries. In this case, the transect.length is domainx-1
#        transect.length <- 5

        ## define 'by' in sequence for labelling x-axis (note, this is not in metres, it relates to the number of A-scans, N.step:
        x.by <- 20
                x.labels.index <- seq(0, N.step, by=x.by)
                #x.cell <- transect.length/N.step
                #x.labels <- x.labels.index*x.cell
                #x.labels <- round(x.labels, digits=2)

                y.by <- (N.iterations/(time.window/10))-1   #this adds (N.iterations/y.by) numbers in the y-axis, the -1 gives a more complete axis (it arrives down to approx the time.window)
                y.labels.index <<- seq(0, N.iterations, by=y.by)        #change sequence if necessary to get better labels
                y.cell <- (time.window*10^-9)/N.iterations
                y.labels <- y.labels.index*y.cell/10^-9
                y.labels <- round(y.labels, digits=0)


  title(my.gpr, cex.main=1.5, font.main=1)
  axis(1, at = x.labels.index, labels = F, tcl=-0.5)
  text(x = x.labels.index, labels = x.labels.index, par("usr")[3], srt = 0, pos = 1, offset = 1, xpd = TRUE, font=1, cex=1.5)
  mtext("trace number", col="black", 1, line=3, font=1, cex=1.5)
  #arrows(x0 = 50, y0 = 305, x1 = transect.len-50, y1 = 305, length = 0.2, angle = 20, col="black", lwd=3, xpd=T)

  axis(2, at = y.labels.index, labels = F, tcl=-0.5)
  text(y = y.labels.index, labels = y.labels, par("usr")[1], srt = 0, pos = 2, offset = 1, xpd = TRUE, font=1, cex=1.5)
  mtext("time (ns)", col="black", 2, line=3.5, font=1, cex=1.5)

  #saves plot anyway, in case I don't have Ea
  savePlot(filename = paste(my.gpr, ".sim.png", sep=""), type = c("png"))

  
###}

