
library(qpcR)
library(readr)

############ IMPORT QS FILE #######################
mergefile <- "n37_Results_20211214 154916.csv"
df <- read_csv("n37_Raw Data_20211214 154916.csv", skip = 22)
colnames(df) <- make.names(colnames(df))
df <- transform(df, X1_M1 = as.numeric(X1_M1))

#1. select Wells(samples)
#2. sort fluorescence channel by cycle

unique.wells <- unlist(unique(df["Well.Position"]))
df.result <- data.frame(well = character(), 
                        efficiency=double(),
                 rsq=double(),
                 stringsAsFactors=FALSE)


for(i in unique.wells){
  tryCatch(
    expr = {
      df.transitory <- subset.data.frame(df, df$Well.Position == i)
      #plot(df.transitory$Cycle.Number, df.transitory$X1_M1, main="Scatterplot Example",
      #     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
      pcr.obj <- pcrfit(df.transitory , cyc = 3, fluo = 4)
      
      result <- sliwin(pcr.obj, wsize = 5:7, basecyc = 1:6, base = 0, border = NULL,
                       type = c("rsq", "slope"))
      
      new.row <- list(well = i, efficiency=result$eff, rsq=result$rsq)
      df.result = rbind(df.result,new.row, stringsAsFactors=FALSE)
    },
    error = function(e){ 
      # (Optional)
      # Do this if an error is caught...
    },
    warning = function(w){
      # (Optional)
      # Do this if an warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
  )
}



# Change columns names to probes. 
# 1. import result csv toe xtract targets. 
results_to_merge <- read_csv(mergefile, skip = 22)
results_to_merge <- results_to_merge[,c("Well Position","Target")]

output <- merge.data.frame(x = df.result , y = results_to_merge , by.x = "well" , by.y = "Well Position" )

write.csv(output, "n37_efficiency.csv" , sep = ",")


