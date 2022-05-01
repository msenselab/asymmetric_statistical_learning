readData <- function (filename) {
  fullname <- file.path('.', 'data', filename)
  raw <-  read.csv(fullname)
  raw$participant <- as.character(raw$participant)

  raw$tar_pos_type <- factor(raw$tar_pos_type, levels=c(0,1,2), labels=c('Random','Infreq. neighbour','Freq. neighbour'))
  raw$key_pq1.keys <- factor(raw$key_pq1.keys, levels=c("y", "n", "1", "2"), labels=c("y", "n", "Pattern", "Random"))

  return(raw)
}

readData_e2 <- function (filename) {
  fullname <- file.path('.', 'Experiment 2', 'data', filename)
  raw <-  read.csv(fullname)
  raw$participant <- as.character(raw$participant)

  raw$dist_pos_type <- factor(raw$dist_pos_type, levels=c(0,1,2,3), labels=c('Absent','Random','Infreq. neighbour','Freq. neighbour'))
  raw$key_pq1.keys <- factor(raw$key_pq1.keys, levels=c("y", "n", "1", "2"), labels=c("y", "n", "Pattern", "Random"))

  return(raw)
}

