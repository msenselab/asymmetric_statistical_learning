
readData <- function (filename) {
  fullname <- file.path('.', 'data',filename)
  d = readMat(fullname)
  raw <- as.data.table(d$trials)
  
  names(raw) <- c('tar_pos','tar_shape','tar_ori','color','tar_pos_cond','response','rt')
  
  seqno <- seq(1,24)
  dyndirections <- as.numeric((seqno+1)%%4 < 2)*2 - 1
  
  # add block numbers and subject identifiers
  raw$blkNo <- rep(1:(nrow(raw)/60),each=60)
  raw$sub <- d$expInfo[[1]]
  raw$group <- d$expInfo[[4]]
  direction <- dyndirections[d$expInfo[[4]]]
  raw$direction <- direction
  raw$correct <- raw$response==raw$tar_ori
  raw$outlier <- raw$rt < 0.2 | raw$rt > 2.5
  
  # add trial numbers 
  raw$tno=seq(1,nrow(raw))
  
  # Questionnaire responses
  raw$q1 <- d$u[[1]]
  raw$q2 <- d$u[[2]]
  
  raw
}