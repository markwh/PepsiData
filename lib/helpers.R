
# Format Pepsi data
getPepsiData <- function(pepsiPiece) {
  pp <- pepsiPiece[[1]] %>% 
    setNames(attr(., "dimnames")[[1]]) %>% 
    `[`(, 1, 1)
  
  obslist <- pp[c("W", "Q", "H", "S", "dA")]
  
  # Make each observation array into a nice data.frame
  makeNice <- function(obselem) {
    obselem %>% 
      as.data.frame() %>% 
      setNames(1:ncol(.)) %>% 
      mutate(xs = 1:nrow(.)) %>% 
      gather(key = time, value = value, -xs)
    # melt(id.vars = "xs", variable.name = "time")
  }
  
  # data.frame containing all observations
  # browser()
  obsdf <- obslist %>% 
    lapply(makeNice) %>% 
    bind_rows(.id = "variable") %>% 
    spread(key = variable, value = value)
  # dcast(xs + time ~ variable)
  
  # data.frame containing non-observation info
  auxdf <- as.data.frame(pp$Ao) %>% 
    setNames("Ao") %>% 
    mutate(name = pp$name[1, 1],
           QWBM = pp$QWBM[1, 1],
           wc = pp$wc[1, 1],
           xs = 1:nrow(.))
  
  out <- obsdf %>% 
    left_join(auxdf, by = "xs") %>% 
    mutate(time = as.numeric(time))
}

# replace a piece of the list
readPiece <- function(startchars, endchars, trulines) {
  snum <- grep(paste0("^", startchars), trulines) + 1
  if (is.na(endchars)) {
    numlines <- grep("^[[:digit:]]", trulines)
    enum <- max(numlines)
  } else {
    enum <- grep(paste0("^", endchars), trulines) - 1
  }
  
  stopifnot(length(enum) == 1 && length(snum) == 1)
  
  piece <- read.table(text = trulines[snum:enum])
  out <- unname(as.matrix(piece))
  out
}
putPiece <- function(Pepsi1_, riverind, what = NULL, newpiece) {
  if (!is(newpiece, "list")) {
    newpiece <- list(newpiece)
    if (is.null(what))
      what <- names(newpiece)
  }
  if (is.null(what))
    stop("if what is not supplied, newpiece must be a named list.")
  Pepsi1_[[riverind]][[1]][,,1][what] <- newpiece
  out <- Pepsi1_
}

# Scope flow for first cross-section of each river

plotQ <- function(Pepsi1_) {
  
  gdat <- lapply(Pepsi1_[badQinds], getPepsiData) %>% 
    setNames(badQrivs) %>% 
    bind_rows(.id = "river") %>% 
    arrange(time, xs, river) %>% 
    filter(xs == 1)
  
  out <- ggplot(gdat, aes(x = time, y = Q)) + 
    geom_line() + 
    facet_wrap(~river, scales = "free")
  out
}


# Read truth.txt data

calcdA <- function(width, height) {
  rnks <- rank(height, na.last = TRUE)
  ords <- order(height, na.last = TRUE)
  width <- width[ords]
  height <- height[ords]
  dH <- c(0, diff(height))
  delA <- width * dH
  
  out <- cumsum(delA)[rnks]
  out
}

readTruth <- function(truthfile, 
                      Aostart = "A0",
                      Aoend = "qt",
                      qstart = "Qt",
                      qend = "dA",
                      dAstart = "dA",
                      dAend = "h,",
                      hstart = "h,",
                      hend = "W,",
                      wstart = "W,",
                      wend = NA) {
  tru <- readLines(con <- file(truthfile))
  close(con)
  
  qpiece <- readPiece(startchars = qstart, endchars = qend, trulines = tru)
  Aopiece <- readPiece(Aostart, Aoend, tru)
  hpiece <- readPiece(hstart, hend, tru)
  wpiece <- readPiece(wstart, wend, tru)
  
  wt <- as.data.frame(t(wpiece))
  ht <- as.data.frame(t(hpiece))
  dApiece <- mapply(calcdA, width = wt, height = ht) %>% 
    t() %>% 
    as.matrix() %>% 
    unname()
  out <- list(W = wpiece, Q = qpiece, Ao.calc = t(Aopiece), dA = dApiece)
  out
}


# plot the data
# 
# plotPepsi <- function(Pepsi, x = "time", y = "Q", xs = "rand", river = "all") {
#   if()
# }
