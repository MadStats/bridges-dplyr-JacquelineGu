library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(ggplot)

dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
tmp1 = read_csv(dest)
tmp2 = read_csv(dest, col_types = "c")  # could make them all characters...
classes = sapply(tmp, class)


states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat=list()

dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 =ldply(dest, fread, colClasses = classes)  

save(x16, file = "bridgesdata.RData")
keep = c("STATE_CODE_001",   "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017",
         "YEAR_BUILT_027", "DECK_COND_058")
M=x16
M=as.tbl(M)
data16=select(M, one_of(keep))
#stole the function from Karl
min2dec = function(x){
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}

data16=mutate(data16,latitude=min2dec(LAT_016),longitude= min2dec(LONG_017))
x=filter(data16,latitude<49,latitude>25,longitude<140,longitude>70,YEAR_BUILT_027>=2000)

ggplot(data = x) +geom_point(mapping = aes(y = latitude, x =longitude))


  