library(R.matlab)
library(data.table)
library(tidyverse)
library(ez)

readData <- function (filename) {
  fullname <- file.path('.', 'data',filename)
  d = readMat(fullname)
  raw <- as.data.table(d$trials)
  
  names(raw) <- c('tar_pos','tar_shape','tar_ori','dist_pos','color','freq_pos','response','rt')
  
  # calculate inverse reaction times: response speed (RS) and add block numbers and subject identifiers
  raw$blkNo <- rep(1:(nrow(raw)/30),each=30)
  raw$sub <- d$expInfo[[1]]
  raw$group <- d$expInfo[[4]]
  raw$correct <- raw$tar_ori == raw$response

  # exclude RTs outside 3*sigma, and first trial of each block
  raw$outlier <- (raw$rt > 3 | raw$rt<0.15) 
  
  # add trial numbers to be able to check for inter-trial effects later
  raw$tno=seq(1,nrow(raw))
  
  raw
}


