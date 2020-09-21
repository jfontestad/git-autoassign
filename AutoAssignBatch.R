#auto assign batch.R
load(paste("C:\\Users\\",Sys.getenv("USERNAME"), "\\OneDrive - EFSA\\Automation\\myEnvironmentAutoAssign.RData",sep=""))

logFile = file(paste("C:\\Users\\",Sys.getenv("USERNAME"), "\\OneDrive - EFSA\\Automation\\logs\\",gsub(":", "-", Sys.time())," myEnvironmentAutoAssign.log",sep=""))
sink(logFile, type = c("output"), append=TRUE)
sink(logFile, type = c("message"), append=TRUE)

print(paste("*** AutoAssign starting at: ", Sys.time()))

#constants
#mode: prod performs the assignments; simulation does not
mode_assign = "prod"
#number of working days that are considered for the computation of availabliity
avail_window = 21
#minimum number of available days in the window to be considered as candidate for assignment
absence_max = 5
#minimum skill to be considered as candidate for assignment
skill_min = 3

source(paste("C:\\Users\\",Sys.getenv("USERNAME"), "\\OneDrive - EFSA\\Automation\\AutoAssign.R",sep=""))

save.image(file=paste("C:\\Users\\",Sys.getenv("USERNAME"), "\\OneDrive - EFSA\\Automation\\myEnvironmentAutoAssign.RData",sep=""))

print("image saved, job completed")

print(paste("*** AutoAssign closing at: ", Sys.time()))

#restore output to console
sink(type="output") 
sink(type="message")
