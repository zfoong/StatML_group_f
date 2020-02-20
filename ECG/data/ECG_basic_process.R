setwd("C:/Users/Main/Documents/Work/SML/Term 2")

load("ECG.Rdata")

# while (dev.cur()>1) dev.off() # Delete all plots quickly

#install.packages("schoolmath")
#library ("schoolmath")


### Training set initialisation


data_sep <- rep(1,nrow(X.train))  # The index of the data matrix where the new ECG starts.
data <- matrix()                  # Flattened matrix of ECG intervals.

###################################################
remove_peaks = function(v) {
  n_inds <- v[1]
  for (i in 2:length(v)) {
    if ((v[i])<(v[i-1]+10)) {
      next
    }
    n_inds <- c(n_inds,v[i])
  }
  return(n_inds)
}
###################################################

for (j in 1:nrow(X.train)) {
  ### Rough way to remove drifting and standardize.
  x <- X.train[j,]
  ### Plots of the processing if you want to look, still needs work.
  # par(mfrow=c(2,1))
  # plot(x,type = 'l')
  x <- x+rep(10,30000)
  meanb <- mean(x[1:1000])
  for (i in 2:29) {
    mean <- mean(x[(i*1000):(i*1000+1000)])
    if (mean >= meanb) {
      x[(i*1000 + 1):(i*1000+1000)] = x[(i*1000+1):(i*1000+1000)] - rep(mean - meanb,1000)
    } else {
      x[(i*1000 + 1):(i*1000+1000)] = x[(i*1000+1):(i*1000+1000)] + rep(meanb - mean,1000)
    }
  }
  x <- x-rep(10,30000)
  x <- scale(x)
  # plot(x,type='l')                                           
  
  
  thresh_per <- 0.6  # Threshold where we consider the peaks, needs optimisation.
  thresh <- thresh_per*(max(x,na.rm=TRUE)) 
  q <- length(x)
  inds <- which(x >= c(x[1],x[1:q-1]) & x > c(x[2:q],x[q]) & x > thresh)   #Values of all the peaks in the ECG
  inds <- remove_peaks(inds)
  # points(inds,x[inds], col=2)
  
  
  
  ### Process each ECG into intervals and squeeze them into 200 unit vectors.
  m <- length(inds) -1  #Number of peaks
  p <- 200   #Index
  X1.new <- matrix(NA,m,p)
  
  for (m_ in 1:m){
    t.old <- inds[m_]:inds[m_+1]
    x.old <- x[inds[m_]:inds[m_+1]]
    t.new <- seq(inds[m_],inds[m_+1], length=p)
    x.new <- approx(t.old,x.old,t.new)$y
    X1.new[m_,] <- x.new
  }
  if (j==1) {
    data <- X1.new
    data_sep[j] <- nrow(data)
  } else {
    data <- rbind(data,X1.new)
    data_sep[j] <- nrow(data)
  }
  
}

### Plots of the intervals if you want to look.
# par(mfrow=c(1,1))
# plot(X1.new[2,], type='l')

# for (i in 2:10){
  #lines(X1.new[i,])
#}

### Getting new labels, might have to double-check the indices.
y.new <- matrix(NA,nrow(data),2)
y.new[1:data_sep[1],1] = y.train[1]
y.new[1:data_sep[1],2] = 1
for (i in 1:(length(data_sep)-1)) {
  y.new[(data_sep[i]+1):data_sep[i+1],1] = y.train[i+1]
  y.new[(data_sep[i]+1):data_sep[i+1],2] = i+1
}
