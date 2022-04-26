##author: TOSHAN MAJUMDAR

# TIPS:
# (1) Read about st_nn():help(nngeo), spatial manipulation with sf-cheat sheet
# (2) See variables after executing every line of code:View(var),str(var),head(var),summary(var)

## CALCULATE NEAREST NEIGHBOR COORDINATES


setwd("E:/JPAL/DS WORK FILES/MAP TASK/R code") ##setting current directory of code
rm(list=ls()) ##remove previous variables

library("easypackages")
libraries("nngeo","sp","dplyr","openxlsx","xlsx","rlist") ##include packages

## STEP 1: read from table containing co-ordinates(latitude,longitude)

var <-as_tibble( read.xlsx("panipat.xlsx") )

##separate sample and non sample schools
treat<-var %>%
filter(sample=="Sample school" & treatment=="1")
control<-var %>%
filter(sample=="Sample school" & treatment=="0")

## STEP 2: obtain co-ordinates(latitude,longitude) and store them as spatial data

coordinates(treat) <- c("lon", "lat")
treat <- st_as_sf(treat)
treat<-st_set_crs(treat,4326)

coordinates(control) <- c("lon", "lat")
control <- st_as_sf(control)
control<-st_set_crs(control,4326)

## STEP 3:Implement nearest neighbor algorithm (separately for sample and non sample schools)

## k:desired number of nearest neighbors, maxdist: maximum distance of neighbors,returnDist:store distance matrix
nn_treat <-st_nn(treat,control, sparse = TRUE, k = 3, maxdist = 5000,returnDist = TRUE, progress = TRUE) ##nearest neighbor for treatment schools
nn_control <-st_nn(control,treat, sparse = TRUE, k = 3, maxdist = 5000,returnDist = TRUE, progress = TRUE) ##nearest neighbor for control schools

##STEP 4:COMPUTATION FOR TREATMENT SCHOOLS:

nn_treat_name<-lapply(nn_treat$nn,function(x) control[x,]$schoolname)
##shaping matrix of nearest control schools names:
for (i in 1:length(nn_treat_name) )
{
    j<-length(nn_treat_name[[i]])
	if ( j<3 )
	{
	   k<-rep(NA,3-j)
       nn_treat_name[[i]]<-list.append(nn_treat_name[[i]],k)
	}
}

nn_treat_name <- matrix(unlist(nn_treat_name), ncol = 3, byrow = TRUE)
temp <- matrix(unlist(treat$schoolname), ncol = 1, byrow = TRUE)

combine<-cbind(temp,nn_treat_name)
combine<-cbind(combine,nn_treat$dist/1000)

##column name for nearby schools matrix
colnames(combine)<-c("TREATMENT SCHOOL","NEARBY CONTROL SCHOOL 1","NEARBY CONTROL SCHOOL 2","NEARBY CONTROL SCHOOL 3","CONTROL SCHOOL 1 DISTANCE","CONTROL SCHOOL 2 DISTANCE","CONTROL SCHOOL 3 DISTANCE")
treat_final<-combine

##writing treatment matrix to file:
write.xlsx(treat_final,file="panipat_t1.xlsx", sheetName="treat", append=TRUE)

##STEP 5:COMPUTATION FOR CONTROL SCHOOLS:

nn_control_name<-lapply(nn_control$nn,function(x) treat[x,]$schoolname)
##shaping matrix of nearest treatment schools names:
for (i in 1:length(nn_control_name) )
{
    j<-length(nn_control_name[[i]])
	if ( j<3 )
	{
	   k<-rep(NA,3-j)
       nn_control_name[[i]]<-list.append(nn_control_name[[i]],k)
	}
}

nn_control_name <- matrix(unlist(nn_control_name), ncol = 3, byrow = TRUE)
temp <- matrix(unlist(control$schoolname), ncol = 1, byrow = TRUE)

combine<-NULL
combine<-cbind(temp,nn_control_name)
combine<-cbind(combine,nn_control$dist/1000)

##column name for nearby schools matrix
colnames(combine)<-c("CONTROL SCHOOL","NEARBY TREATMENT SCHOOL 1","NEARBY TREATMENT SCHOOL 2","NEARBY TREATMENT SCHOOL 3","TREATMENT SCHOOL 1 DISTANCE","CONTROL SCHOOL 2 DISTANCE","CONTROL SCHOOL 3 DISTANCE")
control_final<-combine

#combining nearby treatment schools matrix and nearby control schools matrix:
final<-rbind(treat_final,control_final)
##writitng control matrix to excel file:
write.xlsx(control_final,file="panipat_t1.xlsx", sheetName="control", append=TRUE)

###################################################END OF CODE#################################################################
