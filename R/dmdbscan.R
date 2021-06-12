#' Dynamic Method DBSCAN (DMDBSCAN) Function
#'
#' @param data data matrix, data.frame, dissimilarity matrix or 'dist'-object. Specify 'method="dist"' if the data should be interpreted as dissimilarity matrix or object. Otherwise Euclidean distances will be used.
#' @param MinPts Reachability minimum no. of points, see Ester et al. (1996).
#' @param scale scale the data if 'TRUE'.
#' @param method "dist" treats data as distance matrix (relatively fast but memory expensive), "raw" treats data as raw data and avoids calculating a distance matrix (saves memory but may be slow), "hybrid" expects also raw data, but calculates partial distance matrices (very fast with moderate memory requirements).
#' @param seeds FALSE to not include the isseed-vector in the dbscan-object.
#' @param showplot 0 = no plot, 1 = plot per iteration, 2 = plot per subiteration.
#' @param countmode NULL or vector of point numbers at which to report progress.
#'
#' @return
#' @export
#'
#' @examples
dmdbscan = function (data, MinPts, scale = FALSE,
                     method = c("hybrid", "raw", "dist"),
                     seeds = TRUE, showplot = FALSE,
                     countmode = NULL)
{
  #Modifikasi nilai optimum eps
  dataa <- data
  datadist <- dist(dataa)
  data_ <- as.matrix(datadist)
  n1 <- nrow(dataa)

  ## sorting k nearest neighbor
  dataa = data.frame()
  for (i in 1:n1) {
    sort3 = (sort(data_[i, 1:n1], partial = MinPts+1)[MinPts+1])
    dataa <- (rbind(dataa,data.frame(sort3)))
  }

  #distEps adalah sumbu y-nya dalam k-dist
  distEps <- sort(dataa$sort3, decreasing=FALSE) #sorting ascending
  datas <- data.frame(object=c(1:n1), distEps)

  #finding value Eps
  datas$dy <- 0
  for (i in 0:n1){
    datas$dy[i] = ((distEps[i+1]) - (distEps[i]))
  }
  datas2 <- datas[-c(n1-1,n1), ]
  datas2$highnumb <- 0
  highnumb = datas2$dy[1]
  r <- nrow(datas2)-1
  for (i in 1:r){
    in1 <- datas2$dy[i]
    in2 <- datas2$dy[i+1]

    if (in2 > highnumb)
      highnumb <- in2
    if (in2 < highnumb)
      highnumb <- highnumb

    datas2$highnumb[i+1] <- highnumb
  }
  datas2$highnumbdif <- 0
  highnumbdif = datas2$highnumb[1]
  for (i in 1:r){
    in1 <- datas2$highnumb[i]
    in2 <- datas2$highnumb[i+1]
    in3 <- in2-in1
    datas2$highnumbdif[i+1] <- in3
  }
  a <- which(datas2$highnumbdif==max(datas2$highnumbdif, na.rm = TRUE))
  y1 <- distEps[a]
  y2 <- distEps[a+1]
  eps <- (y2+y1)/2

  #k-dist plot
  plot(datas$object, datas$distEps, xlab="Object",
       ylab="K Distance", type="p")
  abline(h=eps, col = "red", lty=2)

  #Start
  distcomb <- function(x, data) {
    data <- t(data)
    temp <- apply(x, 1, function(x) {
      sqrt(colSums((data - x)^2))
    })
    if (is.null(dim(temp)))
      matrix(temp, nrow(x), ncol(data))
    else t(temp)
  }
  method <- match.arg(method)
  data <- as.matrix(data)
  n <- nrow(data)
  if (scale)
    data <- scale(data)

  ##inisialisasi cluster
  classn <- cv <- integer(n)
  isseed <- logical(n)
  cn <- integer(1)

  #jika countmode = n maka akan keluar titik mana yang sedang diproses
  for (i in 1:n) {
    if (i %in% countmode)
      cat("Processing point ", i, " of ", n, ".\n")
    unclass <- (1:n)[cv < 1]

    #mencari tetangga secara spasial dari titik ke-i
    if (cv[i] == 0) {
      if (method == "dist") {
        reachables <- unclass[data[i, unclass] <= eps]
      }
      else {
        reachables <- unclass[as.vector(distcomb(data[i,
                                                      , drop = FALSE], data[unclass, , drop = FALSE])) <=
                                eps]
      }
      if (length(reachables) + classn[i] < MinPts)
        cv[i] <- (-1)
      else {
        cn <- cn + 1
        cv[i] <- cn
        isseed[i] <- TRUE
        reachables <- setdiff(reachables, i)
        unclass <- setdiff(unclass, i)
        classn[reachables] <- classn[reachables] + 1
        while (length(reachables)) {
          if (showplot)
            plot(data, col = 1 + cv, pch = 1 + isseed)
          cv[reachables] <- cn
          ap <- reachables
          reachables <- integer()
          if (method == "hybrid") {
            tempdist <- distcomb(data[ap, , drop = FALSE],
                                 data[unclass, , drop = FALSE])
            frozen.unclass <- unclass
          }
          for (i2 in seq(along = ap)) {
            j <- ap[i2]
            if (showplot > 1)
              plot(data, col = 1 + cv, pch = 1 + isseed)
            if (method == "dist") {
              jreachables <- unclass[data[j, unclass] <=
                                       eps]
            }
            else if (method == "hybrid") {
              jreachables <- unclass[tempdist[i2, match(unclass,
                                                        frozen.unclass)] <= eps]
            }
            else {
              jreachables <- unclass[as.vector(distcomb(data[j,
                                                             , drop = FALSE], data[unclass, , drop = FALSE])) <=
                                       eps]
            }
            if (length(jreachables) + classn[j] >= MinPts) {
              isseed[j] <- TRUE
              cv[jreachables[cv[jreachables] < 0]] <- cn
              reachables <- union(reachables, jreachables[cv[jreachables] ==
                                                            0])
            }
            classn[jreachables] <- classn[jreachables] +
              1
            unclass <- setdiff(unclass, j)
          }
        }
      }
    }
    if (!length(unclass))
      break
  }
  rm(classn)
  if (any(cv == (-1))) {
    cv[cv == (-1)] <- 0
  }
  if (showplot)
    plot(data, col = 1 + cv, pch = 1 + isseed)
  out <- list(cluster = cv, eps = eps, MinPts = MinPts)
  if (seeds && cn > 0) {
    out$isseed <- isseed
  }
  class(out) <- "dmdbscan"
  out
}
