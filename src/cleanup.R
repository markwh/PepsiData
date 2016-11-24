# cleanup.R
# Fixing Pepsi data to be in singular format.

badQrivs <- c("MississippiDownstream", "MississippiUpstream", "Kanawha",
              "StLawrenceDownstream", "StLawrenceUpstream", "Wabash")
PepsiNames <- sapply(Pepsi1, function(pc) pc[[1]][[1]])
badQinds <- match(badQrivs, PepsiNames)

qfiles <- c("LowerMiss.mat", "UpperMiss.mat", "Kanawha.mat", 
            "downstreamData.mat", "upstreamData.mat", "Wabash.mat")
qpaths <- paste("data", badQrivs, qfiles, sep = "/")

qmats <- grepl("\\.mat$", qpaths)
origs <- lapply(qpaths[qmats], readMat)

getQ <- function(path, qname = "Qall") {
  if (!grepl("\\.mat$", path))
    return(NA)
  out0 <- readMat(path)
  out1 <- out0[[qname]]
  out1
}

qnames <- c(rep("Qall", 3), "Q", "Q", "Qall")
origQ <- mapply(getQ, path = qpaths[qmats], qname = qnames)

# lapply(origQ, dim)
# lapply(Pepsi9, function(x) dim(x$W))

toRotate <- c(1:3, 6)
origQ[toRotate] <- lapply(origQ[toRotate], t)

Pepsi2 <- Pepsi1

for (i in 1:length(badQinds)) {
  ind <- badQinds[i]
  Pepsi2 <- putPiece(Pepsi1_ = Pepsi2, riverind = ind, what = "Q", 
                     newpiece = origQ[[i]])
  print(badQrivs[i])
  print(i)
  print(badQinds[i])
  print("")
}

## Sacramento flow

sacq <- read.csv("data/SacramentoUpstream/upsacQ.csv", header = FALSE) %>% 
  as.matrix() %>% 
  unname()

Pepsi2 <- putPiece(Pepsi1_ = Pepsi2, riverind = grep("mentoUps", PepsiNames), 
                   what = "Q", newpiece = sacq)
Pepsi3 <- setNames(Pepsi2, PepsiNames)

## 5. Kanawha widths

kanawha <- readMat("data/Kanawha/Kanawha.mat")

Pepsi4 <- putPiece(Pepsi3, grep("Kanawha", PepsiNames), 
                   "W", t(kanawha$Wall))

## 6. Platte

plattru <- readTruth("data/Platte/truth.txt")
Pepsi5 <- putPiece(Pepsi4, grep("Platte", PepsiNames), what = "dA", 
                   newpiece = plattru$dA)

# finish munging Pepsi data frame

Pepsi_new <- lapply(Pepsi5, 
       getPepsiData) %>% 
  bind_rows() %>% 
  arrange(name, xs, time)

glimpse(Pepsi_new)


# Make into better tree structure
Pepsi6 <- setNames(Pepsi5, PepsiNames)

Pepsi6 %>% str(1)

Pepsi7 <- lapply(Pepsi6, `[[`, 1)
str(Pepsi7[[1]], 1)

Pepsi8 <- lapply(Pepsi7, `[` , , , 1)
str(Pepsi8[[1]], 1)

fixPiece <- function(piece) {
  piece$name <- as.character(piece$name)
  piece
}
Pepsi9 <- lapply(Pepsi8, fixPiece)

# Write to json
Pepsi.json <- jsonlite::toJSON(Pepsi9, auto_unbox = TRUE)
write(Pepsi.json, file = "Pepsi.json")

lapply(Pepsi9$Platte, dim)

cache("Pepsi_new")
