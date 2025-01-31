library(dplyr)
library(readxl)

df<-read.csv(file = "/Users/bopeng/Desktop/Lab/Clemson_Sorghum/Clemson Sorghum samples ID.csv")


#Add Technical Reps
df1 <- df %>%
  bind_rows(df) %>%
  bind_rows(df) %>%
  dplyr::mutate(TecReps = rep(1:3, each = nrow(df)))

#Add Well Numbers Acrossing All Plates
df1$AllNum<-rownames(df1)

#Add Well Numbers Within Each Plate
df1$WitinNum<-c(rep(c(1:96), times=12),1:75)


#Ramdomize Well Numbers
#set.seed(123)
#Plate <- df3[df3$AllNum[97:192], ]
#Num<-Plate[,c("AllNum","WitinNum")]
#Num<-Num[sample(nrow(Num)),]
#Plate<-Plate[,-c(5,6)]
#Plate<-cbind(Plate,Num)#


#Ramdomize Dispensening Order
df2<-df1
df2$WitinNum <- as.character(df2$WitinNum)
Plates <- list()
set.seed(1234)
for (i in 1:12) {
  Plate <- df2[df2$AllNum[(96 * (i - 1) + 1):(96 * i)], ]
  Num<-Plate[,c("AllNum","WitinNum")]
  Num<-Num[sample(nrow(Num)),]
  Plate<-Plate[,-c(6,7)]
  Plate<-cbind(Plate,Num)
  Plates[[i]] <- Plate
}

df3 <- do.call(rbind, Plates)
#The rest part
Plate <- df2[df2$AllNum[(96 * (13 - 1) + 1):1227], ]
Num<-Plate[,c("AllNum","WitinNum")]
Num<-Num[sample(nrow(Num)),]
Plate<-Plate[,-c(6,7)]
Plate<-cbind(Plate,Num)
df3 <- rbind(df3,Plate)

rownames(df3) <- NULL

#Generate Metadata file for Plate Map
df5<-df3
df5<-df5 %>%
  mutate(meta = paste(ID, FeildRep, TecReps, sep = "_"))
df5$AllNum<-as.numeric(df5$AllNum)
df5<-df5[order(df5$AllNum),]
rownames(df5)<-NULL
write.csv(df5,"/Users/bopeng/Desktop/Lab/Clemson_Sorghum/Clemson_Map.csv")
  

#Change the Data Frame to Dispensing Machine Readable Form

df6<-read.csv(file = "/Users/bopeng/Desktop/Lab/Clemson_Sorghum/Clemson_Map_2.csv")
df6<-df6[,c("ID", "AllNum" )]

df6 <- na.omit(df6)
df6$ID<-as.numeric(df6$ID)
# Check if the ID column is a factor and convert it to numeric if needed

Mahcsv <- function(WeightX, j) {
  AllNumbs <- list()
  start <- 1 + 72 * (j - 1)
  end <- 72 * j
  
  for (i in start:end) {
    AllNumb <- WeightX[WeightX$ID == i, ]
    if (nrow(AllNumb) > 0) {  # Check if AllNumb is not empty
      AllNumb <- t(AllNumb[, 2])
      rownames(AllNumb) <- i
      AllNumbs[[as.character(i)]] <- AllNumb
    }
  }
  
  Wide <- do.call(rbind, AllNumbs)
  ID <- start:end
  Wide <- data.frame(ID, Wide, check.names = FALSE)
  
  return(list(Wide = Wide, AllNumbs = AllNumbs))
}


r1<-df6[df6$ID %in% 1:72, ]
r2<-df6[df6$ID %in% 73:144, ]
r3<-df6[df6$ID %in% 145:216, ]
r4<-df6[df6$ID %in% 217:288, ]
r5<-df6[df6$ID %in% 289:360, ]
r6<-df6[df6$ID %in% 361:401, ]


Run1 <- Mahcsv(WeightX = r1,j=1 )
write.csv(Run1$Wide,"/Users/bopeng/Desktop/Lab/Clemson_Sorghum/Run1.csv")
Run2 <- Mahcsv(WeightX = r2,j=2 )
write.csv(Run2$Wide,"/Users/bopeng/Desktop/Lab/Clemson_Sorghum/Run2.csv")
Run3 <- Mahcsv(WeightX = r3,j=3 )
write.csv(Run3$Wide,"/Users/bopeng/Desktop/Lab/Clemson_Sorghum/Run3.csv")
Run4 <- Mahcsv(WeightX = r4,j=4 )
write.csv(Run4$Wide,"/Users/bopeng/Desktop/Lab/Clemson_Sorghum/Run4.csv")
Run5 <- Mahcsv(WeightX = r5,j=5 )
write.csv(Run5$Wide,"/Users/bopeng/Desktop/Lab/Clemson_Sorghum/Run5.csv")

#Run6
AllNumbs <- list()
start <- 1 + 72 * (6 - 1)
end <- 401

for (i in start:end) {
  AllNumb <- r6[r6$ID == i, ]
  if (nrow(AllNumb) > 0) {  # Check if AllNumb is not empty
    AllNumb <- t(AllNumb[, 2])
    rownames(AllNumb) <- i
    AllNumbs[[as.character(i)]] <- AllNumb
  }
}

Wide <- do.call(rbind, AllNumbs)
ID <- start:end
Wide <- data.frame(ID, Wide, check.names = FALSE)

write.csv(Wide,"/Users/bopeng/Desktop/Lab/Clemson_Sorghum/Run6.csv")
