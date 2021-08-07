source("item_sim.R")


cell1.1.boot<-simdata(3000,4000,15,1,mod15.cluster,mod15.complex,LD='cluster',1000); save.image();


item_sim(3000, 15,15,1)

