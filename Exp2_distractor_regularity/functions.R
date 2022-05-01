readData <- function (filename) {
  fullname <- file.path('.', 'data', filename)
  raw <-  read.csv(fullname)
  raw$participant <- as.character(raw$participant)

  raw$tar_pos_type <- factor(raw$tar_pos_type, levels=c(0,1,2), labels=c('Random','Infreq. neighbour','Freq. neighbour'))
  raw$key_pq1.keys <- factor(raw$key_pq1.keys, levels=c("y", "n", "1", "2"), labels=c("y", "n", "Pattern", "Random"))
  raw$outlier <- raw$key_resp.rt < 0.2 | raw$key_resp.rt > 2.5
  return(raw)
}

readData_e2 <- function (filename) {
  fullname <- file.path('.', 'data', filename)
  raw <-  read.csv(fullname)
  raw$participant <- as.character(raw$participant)

  raw$dist_pos_type <- factor(raw$dist_pos_type, levels=c(0,1,2,3), labels=c('Absent','Random','Infreq. neighbour','Freq. neighbour'))
  raw$key_pq1.keys <- factor(raw$key_pq1.keys, levels=c("1", "2"), labels=c("Pattern", "Random"))
  raw$outlier <- raw$key_resp.rt < 0.2 | raw$key_resp.rt > 2.5
  
  # remove unimportant columns that were missing in one data-file (probably because of ending by pressing "escape" rather than some other key)
  if("finish_text.started" %in% names(raw)) {
    raw <- raw %>% select(-c("finish_text.started", "finish_text.stopped", "finish_key.keys", "finish_key.rt", "finish_key.started", "finish_key.stopped"))
  }
  
  return(raw)
}

