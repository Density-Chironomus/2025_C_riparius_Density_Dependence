# Calculation of EmT50 values from emergence data for all food-density-combinations using a two-parameter log-logistic model
#setwd("<SET YOUR FILE PATH HERE>")
      
allemergdata <- read.table("food_dep_emerg_data.csv",sep=",",dec=".",header=T)

library(drc)

# Initialize output vectors
e_values <- 0
b_values <- 0
R2_values <- 0

# Loop through all treatments 
for (i in 1:(ncol(allemergdata)-1)){
  
  # Create data structure in correct format for drm function
  notNA <- which(!is.na(allemergdata[,i+1]))
  emergdata <- matrix(c(allemergdata[notNA,1],allemergdata[notNA,1+i]/max(allemergdata[notNA,1+i])), ncol = 2, byrow = FALSE)
  colnames(emergdata) <- c('time','emergence')
  emergdata <- as.data.frame(emergdata)
  
  # Fit the drm model (normally used for dose-response curves)
  emergdata.m1<-drm(emergence~time, data = emergdata, fct=LL.2(), type="binomial")

  # Extract the fitted parameters
  e_values[i] <- emergdata.m1$coefficients[2]
  b_values[i] <- emergdata.m1$coefficients[1]
  
  # Compute R squared
  realdata <- emergdata$emergence
  simdata <- (1/(1+(emergdata$time/e_values[i])^b_values[i]))
  R2_values[i] <- cor(realdata, simdata)^2

  # Create plot
  nam <- colnames(allemergdata)[i+1]
  plot(emergdata.m1,ylab="Emerged fraction",xlab="Time (d)",ylim=c(0,1), log = "", main = nam) # Default plot
  # curve(1/(1+(x/e_values[i])^b_values[i]), add = TRUE,  col = "red", lty = 2) # Curve function for validation that this is the correct equation
  
  # Save plot to file
  PlotRecord<-recordPlot()
  # Create png with width, height and resolution as input
  png(paste(i,"_",nam,".png", sep = ""), width=4, height=4, units="in", res = 300)
  replayPlot(PlotRecord)
  dev.off()
}

# Output results in csv file
output <- matrix(c(e_values,b_values,R2_values), ncol = ncol(allemergdata)-1 , byrow=TRUE)
colnames(output) <- colnames(allemergdata)[2:ncol(allemergdata)]
rownames(output) <- c("e","b","Rsqrd")
output <- as.data.frame(output)
write.csv(output, "drc_par_summary.csv", row.names=TRUE)
   

