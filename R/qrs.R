qrs <- function(ecg, fs, return.fecg = FALSE, width = round(fs/10)+1, fl = 5, fh = 20,
                search = 10, threshold = 7, safe = 300){


  #ecg <- scale(ecg)

  fecg <- as.vector(filter(ecg, bpcoef(width, fs, fl, fh), circular = FALSE))

  decg <- c(NA, diff(fecg))

  decg[decg > 0 & !is.na(decg)] <- fecg[decg > 0 & !is.na(decg)]^2
  decg[decg <= 0 & !is.na(decg)] <- 0

  mean.decg <- mean(decg, na.rm = TRUE)

  safe <- round(safe/(1000/fs))
  search <- round(search/(1000/fs))
  ref.col <- search + 1

  r.fun <- function(x){ all(fecg[x[ref.col]] >= fecg[x]) & max(decg[x]) > threshold * mean.decg }

  r <- c(rep(NA, search), apply(embed(seq(fecg), 2*search + 1), 1, r.fun), rep(NA, search))
  r[r & !is.na(r)] <- c(safe,diff(which(r))) >= safe

  if(return.fecg){
    data.frame(fecg, r)
  }else{
    data.frame(r)
  }

}
