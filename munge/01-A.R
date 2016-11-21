
# Pepsi0 <- R.matlab::readMat("data/Formatted_Pepsi_data.mat")
Pepsi0 <- R.matlab::readMat("https://umass.box.com/shared/static/ranpdd5heauq50z6krh18mwg7hriw1g9.mat")

# Get a nice data.frame for each river
Pepsi1 <- Pepsi0[[1]]
nulls <- sapply(Pepsi1, is.null)

Pepsi <- lapply(Pepsi1[!nulls], 
                getPepsiData) %>% 
  bind_rows() %>% 
  arrange(name, xs, time)

filter(Pepsi, name == "MississippiUpstream") %>% 
  summary

library(R.matlab)
data1 <- readMat("data/MississippiUpstream/UpperMiss.mat")
str(data1)
