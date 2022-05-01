N <- 24
ntrials <- 960
nblocks <- ntrials/60
seqno <- seq(1,N)
dyndirections <- as.numeric((seqno+1)%%4 < 2)*2 - 1

for(n in seqno) {
  
  # Select frequent direction of dynamic distractor contingencies
  dyndirection <- dyndirections[n]
  
  # Generate distractor position conditions
  posconds <- c(sample(rep(seq(1,60), ntrials*3/(4*60))), rep(0,ntrials/4))
  # First trial should be considered the "random" condition so we swap with the first one 
  # that is
  while(posconds[1]<=54) {
    j <- 2
    while(posconds[j] <= 54) {
      j <- j + 1
    }
    temp <- posconds[1]
    posconds[1] <- posconds[j]
    posconds[j] <- temp
  } 
  
  # Generate other item conditions: target position, target orientation, shapes, colors
  itemconds <- sample(rep(1:64,ntrials/64))
  tpos <- itemconds %% 8 + 1
  tarori <- bitwShiftR(itemconds,3) %% 2 + 1
  shapes <- bitwShiftR(itemconds,4) %% 2 + 1
  colors <- bitwShiftR(itemconds,5) %% 2 + 1
  
  first_dpos <- ceiling(runif(1)*8)
  while(first_dpos == tpos[1]) {
    first_dpos <- ceiling(runif(1)*8)
  }

  dpos <- rep(0,ntrials)
  dpostype <- rep(0,ntrials)
  tpostype <- rep(0,ntrials)
  tpostype[1] <- 1
  dpos[1] <- first_dpos
  dpostype[1] <- 1

  # Generate sequence of distractor positions
  for (i in 2:ntrials) {
    nextdynpos <- (dpos[i-1]+dyndirection-1) %% 8 + 1  
    oppositedynpos <- (dpos[i-1]-dyndirection-1) %% 8 + 1  
    if(posconds[i]>54) { # Random position
      posopt = 1:8
      posopt <- posopt[-c(nextdynpos, oppositedynpos)]
      dpos[i] <- posopt[posconds[i] - 54]
      dpostype[i] <- 1
    } else if(posconds[i]>48) { # Move one step in rare direction
      dpos[i] <- oppositedynpos
      dpostype[i] <- 2
    } else if(posconds[i]>0) { # Move one step in frequent direction
      dpos[i] <-  nextdynpos
      dpostype[i] <- 3
    } else { # Distractor absent
      dpos[i] <- 0
      dpostype[i] <- 0
    }
    
    if(tpos[i]==dpos[i]) {
      j <- 1
      while(tpos[j]==dpos[i] | posconds[j]!=0) {
        j <- j + 1
      }
      temp <- tpos[i]
      tpos[i] <- tpos[j]
      tpos[j] <- temp
    }
    
    if (dpos[i]==0) {
      tpostype[i] <- 0 # Distractor absent block
    } else if(tpos[i]==nextdynpos) {
      tpostype[i] <- 3 # Target in frequent dynamic dist. location
    } else if(tpos[i]==oppositedynpos) {
      tpostype[i] <- 2  # Target in rare dynamic dist. location
    } else  {
      tpostype[i] <- 1 # Target in "neutral" position 
    } 
  }
  
  seq <- data.frame(tar_pos=tpos, shape=shapes, tar_ori=tarori, color = colors,
                    dist_pos=dpos, dist_pos_type=dpostype, tar_pos_type=tpostype)
  
  
  # Randomly rearrange blocks, except for keeping first block first
  blocks <- 1:nblocks
  blocks <- c(1,sample(blocks[2:nblocks], nblocks-1))
  tno <- (rep(blocks, each=60)-1)*60+rep(1:60, nblocks)
  seq <- seq[tno,]
  
  write.csv(seq, paste0('./sequences/seq_', n, '.csv'), row.names = FALSE)
}
