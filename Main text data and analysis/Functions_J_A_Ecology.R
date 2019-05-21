

# this again is another function borrowed from the internet. I am not the creator of this function. Someone else is. But its really great!
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# this is a modified function taken from somewhere in the internet ( I am sure if one googles grid_shared_legend_(), it will direct you to the original source).
# I by no means am the creator of this awesome function and do not take credit at all. Just a nice small function that arranges plot. I dont use it i think in the manuscript!

grid_arrange_shared_legend <- function(..., ncol, nrow, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol =ncol , nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}





# the below generic EWS function is modified from the original generic_EWS() function from the 'earlywarnings' package.
# the modification was to make sure that this function does not plot . 
# other modifications were to include original phenotypic time-series data into it, - 
# that returns back a phenotypic timeseries of the same length used to analyse the abundance data.
# only half of the abundance time series data is used for early warning signal analysis. THis is made sure by the window size =50.
#window-size 50 means 50% of the time series is used for analysis. We used the same window-size on the phenotypic timeseries so that -
# it returns back 50% of the time series- given by genericEWS$ddata
genericEWS<-function (timeseries, winsize = 50, detrending = c("no", "gaussian", 
                                                               "loess", "linear", "first-diff"), bandwidth = NULL, span = NULL, 
                      degree = NULL, logtransform = FALSE, interpolate = FALSE, 
                      AR_n = FALSE, powerspectrum = FALSE) 
{
  timeseries <- data.matrix(timeseries)
  if (dim(timeseries)[2] == 1) {
    Y = timeseries
    timeindex = 1:dim(timeseries)[1]
  }
  else if (dim(timeseries)[2] == 2) {
    Y <- timeseries[, 2]
    timeindex <- timeseries[, 1]
  }
  else {
    warning("not right format of timeseries input")
  }
  if (interpolate) {
    YY <- approx(timeindex, Y, n = length(Y), method = "linear")
    Y <- YY$y
  }
  else {
    Y <- Y
  }
  if (logtransform) {
    Y <- log(Y + 1)
  }
  detrending <- match.arg(detrending)
  if (detrending == "gaussian") {
    if (is.null(bandwidth)) {
      bw <- round(bw.nrd0(timeindex))
    }
    else {
      bw <- round(length(Y) * bandwidth/100)
    }
    smYY <- ksmooth(timeindex, Y, kernel = "normal", bandwidth = bw, 
                    range.x = range(timeindex), x.points = timeindex)
    nsmY <- Y - smYY$y
    smY <- smYY$y
  }
  else if (detrending == "linear") {
    nsmY <- resid(lm(Y ~ timeindex))
    smY <- fitted(lm(Y ~ timeindex))
  }
  else if (detrending == "loess") {
    if (is.null(span)) {
      span <- 25/100
    }
    else {
      span <- span/100
    }
    if (is.null(degree)) {
      degree <- 2
    }
    else {
      degree <- degree
    }
    smYY <- loess(Y ~ timeindex, span = span, degree = degree, 
                  normalize = FALSE, family = "gaussian")
    smY <- predict(smYY, data.frame(x = timeindex), se = FALSE)
    nsmY <- Y - smY
  }
  else if (detrending == "first-diff") {
    nsmY <- diff(Y)
    timeindexdiff <- timeindex[1:(length(timeindex) - 1)]
  }
  else if (detrending == "no") {
    smY <- Y
    nsmY <- Y
  }
  mw <- round(length(Y) * winsize/100)
  omw <- length(nsmY) - mw + 1
  low <- 6
  high <- omw
  nMR <- matrix(data = NA, nrow = mw, ncol = omw)
  x1 <- 1:mw
  for (i in 1:omw) {
    Ytw <- nsmY[i:(i + mw - 1)]
    nMR[, i] <- Ytw
  }
  nARR <- numeric()
  nSD <- numeric()
  nSK <- numeric()
  rD<-numeric() #rD
  RoD<-numeric() # ROD
  nKURT <- numeric()
  nACF <- numeric()
  nDENSITYRATIO <- numeric()
  nSPECT <- matrix(0, nrow = omw, ncol = ncol(nMR))
  nCV <- numeric()
  smARall <- numeric()
  smARmaxeig <- numeric()
  detB <- numeric()
  timesss<-numeric()
  biomass<-numeric()
  ARn <- numeric()
  nSD <- apply(nMR, 2, sd, na.rm = TRUE)
  for (i in 1:ncol(nMR)) {
    nYR <- ar.ols(nMR[, i], aic = FALSE, order.max = 1, dmean = FALSE, 
                  intercept = FALSE)
    timesss[i]<-mean(nMR[,i], na.rm=TRUE)
    biomass[i]<- nMR[mw,i] 
    RoD[i]<-(sqrt(sum((nMR[2:nrow(nMR),i]-nMR[1:nrow(nMR),i])^2))/(length(nMR[,i])-1))/nSD[i]
    nARR[i] <- nYR$ar
    nSK[i] <- abs(moments::skewness(nMR[, i], na.rm = TRUE))
    nKURT[i] <- moments::kurtosis(nMR[, i], na.rm = TRUE)
    nCV[i] <- nSD[i]/mean(nMR[, i])
    ACF <- acf(nMR[, i], lag.max = 1, type = c("correlation"), 
               plot = FALSE)
    nACF[i] <- ACF$acf[2]
    spectfft <- spec.ar(nMR[, i], n.freq = omw, plot = FALSE, 
                        order = 1)
    nSPECT[, i] <- spectfft$spec
    nDENSITYRATIO[i] <- spectfft$spec[low]/spectfft$spec[high]
    if (AR_n) {
      ARall <- ar.ols(nMR[, i], aic = TRUE, order.max = 6, 
                      demean = F, intercept = F)
      smARall[i] <- ARall$ar[1]
      ARn[i] <- ARall$order
      roots <- Mod(polyroot(c(rev(-ARall$ar), 1)))
      smARmaxeig[i] <- max(roots)
      detB[i] <- (prod(roots))^(2/ARn[i])
    }
  }
  nRETURNRATE = 1/nARR
  timevec <- seq(1, length(nARR))
  KtAR <- cor.test(timevec, nARR, alternative = c("two.sided"), 
                   method = c("kendall"), conf.level = 0.95)
  KtACF <- cor.test(timevec, nACF, alternative = c("two.sided"), 
                    method = c("kendall"), conf.level = 0.95)
  KtSD <- cor.test(timevec, nSD, alternative = c("two.sided"), 
                   method = c("kendall"), conf.level = 0.95)
  KtSK <- cor.test(timevec, nSK, alternative = c("two.sided"), 
                   method = c("kendall"), conf.level = 0.95)
  KtKU <- cor.test(timevec, nKURT, alternative = c("two.sided"), 
                   method = c("kendall"), conf.level = 0.95)
  KtDENSITYRATIO <- cor.test(timevec, nDENSITYRATIO, alternative = c("two.sided"), 
                             method = c("kendall"), conf.level = 0.95)
  KtRETURNRATE <- cor.test(timevec, nRETURNRATE, alternative = c("two.sided"), 
                           method = c("kendall"), conf.level = 0.95)
  KtCV <- cor.test(timevec, nCV, alternative = c("two.sided"), 
                   method = c("kendall"), conf.level = 0.95)
  out <- data.frame(timeindex[mw:length(nsmY)], nARR, nSD, 
                    nSK, nKURT, nCV, nRETURNRATE, nDENSITYRATIO, nACF,timesss,biomass, RoD, round(KtSD$estimate, digits = 3),  round(KtAR$estimate,digits = 3) )
  
  
  colnames(out) <- c("timeindex", "ar1", "sd", "sk", "kurt", 
                     "cv", "returnrate", "densratio", "acf1", "ddata","adata","RoD", "KTauSD", "KTauAR")
  return(out)
}


# this function takes in values of AR1 , SD and phenotypic value and returns Kendall tau estimates of abundance and trait-based EWS
# this function also makes sure that phenotypic value is multiplied with -1 only if the slope of the trend of phenotypic shift is greater than 0.1
#-this ensures that only strong shifts are taken into account.
Composite.trait.ews<-function(AR,SD,trait){
  
  end.length<-1:length(AR)
  
  #z-standardization
  phenotype<- ((trait-mean(trait, na.rm=T))/(sd(trait,na.rm=T)))
  acc<- (AR-mean(AR))/sd(AR);
  
  sd.d <- (SD-mean(SD))/sd(SD);
  
  if ( (lm(phenotype~end.length)$coefficients[2]) < -0.1)
  {  phenotype<- -phenotype }
  else if ( (lm(phenotype~end.length)$coefficients[2]) > -0.1)
    ( phenotype<- phenotype)
  

 

  #Correlation test statistic and confidence intervals
  time<-seq(1,length(acc));
  tau.arr<-cor.test(acc, time, method="kendall", conf.level =0.95, alternative="two.sided")$estimate
  tau.SD<-cor.test(sd.d, time, method="kendall", conf.level =0.95, alternative="two.sided")$estimate
  tau.SD.AR<-cor.test((acc+sd.d), time, method="kendall", conf.level =0.95, alternative="two.sided")$estimate
  tau.Trait<-cor.test((phenotype),time, method ="kendall", conf.level =0.95, alternative = "two.sided")$estimate
  tau.Trait.ar<-cor.test((phenotype+acc),time, method ="kendall", conf.level =0.95, alternative = "two.sided")$estimate
  tau.Trait.SD<-cor.test((phenotype+sd.d),time, method ="kendall", conf.level =0.95, alternative = "two.sided")$estimate
  tau.Trait.SD.ar<-cor.test((phenotype+sd.d+acc),time, method ="kendall", conf.level =0.95, alternative = "two.sided")$estimate
  
  return(data.frame(phenotype=phenotype,AR1=acc, SD=sd.d, 
                    Tau.AR1=tau.arr,Tau.SD=tau.SD,
                    Tau.composite=tau.SD.AR, 
                    Tau.Trait.AR=tau.Trait.ar, 
                    Tau.Trait.SD= tau.Trait.SD, 
                    Tau.Trait.SD.AR= tau.Trait.SD.ar,
                    Tau.Trait=tau.Trait))

  
  
}


# this function takes into values of AR1 and SD and standardizes them and gives you the Kendall's tau estimate
Composite.ews<-function(AR,SD){
  
  acc<- (AR-mean(AR))/sd(AR);
  sd.d <- (SD-mean(SD))/sd(SD);
  
  w.ar<-numeric(); w.ar<-acc; w.sd<-numeric(); w.sd<-sd.d;
  W<-numeric(); W<-sd.d + acc;
  

  #Correlation test statistic and confidence intervals
  time<-seq(1,length(acc));
  tau.arr<-cor.test(acc, time, method="kendall", conf.level =0.95, alternative="two.sided")$estimate
  tau.SD<-cor.test(sd.d, time, method="kendall", conf.level =0.95, alternative="two.sided")$estimate
  tau.SD.AR<-cor.test((acc+sd.d), time, method="kendall", conf.level =0.95, alternative="two.sided")$estimate
  
  
  return(data.frame(Tau.AR1=tau.arr,Tau.SD=tau.SD,Tau.composite=tau.SD.AR))
  
  
  
}