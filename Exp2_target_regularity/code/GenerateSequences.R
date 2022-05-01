N <- 16
ntrials <- 840
seqno <- seq(1,N)
dyndirections <- as.numeric((seqno+1)%%4 < 2)*2 - 1

for(n in seqno) {
  
  dyndirection <- dyndirections[n]
  
  posconds <- sample(rep(seq(1,60), ntrials/60))
  
  letters <- sample(rep(c(0,1), ntrials/2))
  first_pos <- ceiling(runif(1)*8)
  pos <- rep(0,ntrials)
  postype <- rep(0,ntrials)
  pos[1] <- first_pos
  postype[1] <- 0
  
  for (i in 2:ntrials) {
    
    nextdynpos <- (pos[i-1]+dyndirection-1) %% 8 + 1  
    oppositedynpos <- (pos[i-1]-dyndirection-1) %% 8 + 1  
    if(posconds[i]>54) { # Random position
      posopt = 1:8
      posopt <- posopt[-c(nextdynpos, oppositedynpos)]
      pos[i] <- posopt[posconds[i] - 54]
      postype[i] <- 0
    } else if(posconds[i]>48) { # Move one step in rare direction
      pos[i] <- oppositedynpos
      postype[i] <- 1
    } else { # Move one step in frequent direction
      pos[i] <-  nextdynpos
      postype[i] <- 2
    }
  }
  seq <- data.frame(tar_ori=letters, tar_pos=pos-1, tar_pos_type=postype)
  write.csv(seq, paste0('./sequences/seq_', n, '.csv'), row.names = FALSE)
}
