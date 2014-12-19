require(Ranadu)
Directory <- DataDirectory ()
Flight <- "rf16"     		
Project = "DEEPWAVE"			
fname = sprintf("%s%s/%s%s.nc", Directory,Project,Project,Flight)
Data <- getNetCDF (fname, standardVariables(c("THETAP")))		
r <- setRange (Data$Time, 123100, 125500)
DS <- Data[r, c("PSXC", "ATX", "DPXC")]
colnames(DS) <- c("Pressure", "Temperature", "Dewpoint")
print (SkewTSounding(DS, AverageInterval=5))

