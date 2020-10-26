#auto assign batch.R
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

print("start batch")
#for (count in seq(32)) {
#    print(paste("start iteration ", count, sep=""))
    load(paste("/home/docker/myEnvironmentAutoAssign.RData",sep=""))

    print(paste("*** Autoassign starting at: ", Sys.time()))

    source(paste("/home/docker/AutoAssign.R",sep=""))

    save.image(file=paste("/home/docker/myEnvironmentAutoAssign.RData",sep=""))

    print("image saved, job completed")

#    Sys.sleep(900) #15 minutes intervals
#}
print(paste("*** AutoAssign closing at: ", Sys.time()))


print("image saved, job completed")

print(paste("*** AutoAssign closing at: ", Sys.time()))
