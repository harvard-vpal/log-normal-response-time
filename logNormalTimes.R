##This script grabs HarvardX data tables. Create a file "settings.R" from "settings_example.R", providing the path 
# "coursedir" that contains course data in folders (one for a course), the folder names will serve as course_ids.
##It ultimately transforms the data to a data-frame ("Pcheck"), which looks like a submit-event log (problem_id, user_id, correctness, etc.) with variables "logwait1" and "logwait2" that contain logarithms of 1st and 2nd response times
##Then it calls the function fit.gaussian() from "fitGaussian.R" to fit our model to these logtime variables.
# It stores the results creating folders with the same names as it saw in "coursedir". For each course, it stores fit1 and fit2 (outputs of fit.gaussian() for 1st and 2nd response) 
# and a person_course_survey table (PCS) with the usual HarvardX user variables, and it does so three times: the fit on responses of any correctness, correct only, and incorrect only.

source('settings.R')
source('fitGaussian.R')
library(plyr)


calculate=TRUE
max.iter=2500
max.attempt.limit=5
min.problems=10
min.users=10


##This script assumes that coursedir contains course data in folders (each course has a folder, the folder name will serve as course_id)

course_ids=list.dirs(coursedir,recursive=F, full.names=F)


#Function for recoding different entries into Boolean
toBoolean=function(x,true=c("true","TRUE","True","1","1.0","T"), na.is=F){
  y=rep(F,length(x))    
  y[is.na(x)]=na.is
  y[which(x %in% true)]=T
  return(y) 
}



for(course_j in 1:length(course_ids)){
course_id=course_ids[course_j]
cat(course_id,"\b,",course_j,"out of",length(course_ids),"\n")
  try({
    datadir=file.path(coursedir,course_id)
    Pcheck=read.csv(file.path(datadir,"problem_check.csv.gz"),header=T, stringsAsFactors = F)
    Pcheck=subset(Pcheck, Pcheck$attempts>0) ##Because sometimes there is a weird 0 in 1 or 2 rows.
    PCS=read.csv(file.path(datadir,"person_course_survey.csv.gz"),header=T, stringsAsFactors = F)
    PCS=subset(PCS,(PCS$roles=="Student")|(PCS$roles=="student"))
    course.axis=read.csv(file.path(datadir,"course_axis.csv.gz"),header=T, stringsAsFactors = F)
    course.problem=read.csv(file.path(datadir,"course_problem.csv.gz"),header=T, stringsAsFactors = F)
    course.problem=merge(course.problem,course.axis, by.x="problem_id",by.y="url_name")
    person.problem=read.csv(file.path(datadir,"person_problem.csv.gz"),header=T, stringsAsFactors = F)

    person.problem=merge(person.problem,course.problem, by="problem_nid")
    person.problem=merge(person.problem,PCS[,c("user_id","username")], by="user_id")
    person.problem$user_id=NULL

    var.boolean=c("completed","certified","explored")
    for(v in var.boolean){
      PCS[,v]=toBoolean(PCS[,v])
    }


    ##Anonymize user_ids in person_course_survey and problem_check and convert timestamps to format
    user.map=data.frame(username=unique(PCS$username[PCS$explored]))
    user.map$user_id=paste0("fake",1:nrow(user.map),"ID")
    PCS$user_id=NULL
    PCS=merge(PCS,user.map,by="username")
    
    options(digits.secs=2)
    timestamp.vars=c("first_event", "last_event", "start_time")
    for (v in timestamp.vars){
      PCS=subset(PCS,PCS[,v]!="")
      PCS[,v]=as.numeric(as.POSIXct(PCS[,v],tz="UTC"))
    }
    Pcheck$time=as.POSIXct(Pcheck$time,tz="UTC")
    Pcheck$time=as.numeric(Pcheck$time)
    person.problem$date=as.POSIXct(person.problem$date,tz="UTC")
    person.problem$date=as.numeric(person.problem$date)


    st=regexpr("problem/",Pcheck$module_id[1])+nchar("problem/")
    Pcheck$problem_id=substr(Pcheck$module_id,st,nchar(Pcheck$module_id))
    Pcheck=merge(Pcheck,user.map, by="username")
    person.problem=merge(person.problem,user.map, by="username")
    Pcheck$username=NULL
    person.problem$username=NULL


    ##Create id for user-problem pairing
    Pcheck$userproblem_id=paste(Pcheck$user_id,Pcheck$problem_id)
    person.problem$userproblem_id=paste(person.problem$user_id,person.problem$problem_id)
    person.problem=plyr::rename(person.problem,c("date"="serve_time"))

    ##Remove duplicate attempts (keep the one with the earlier time). They should not exist but I think I saw it once. Weird.
    Pcheck=Pcheck[order(Pcheck$time),]
    userproblematt_id=paste(Pcheck$userproblem_id,Pcheck$attempts)
    Pcheck=Pcheck[!duplicated(userproblematt_id),]

    ##Record the maximum observed number of attempts for each problem
    temp=aggregate(Pcheck$attempts, by=list("problem_id"=Pcheck$problem_id), FUN=max)
    temp=plyr::rename(temp,c("x"="max_attempts"))
    Pcheck=merge(Pcheck, temp, by="problem_id")

    ##Record the maximum observed number of attempts for each problem for each user
    temp=aggregate(Pcheck$attempts, by=list("userproblem_id"=Pcheck$userproblem_id), FUN=max)
    temp=plyr::rename(temp,c("x"="max_user_attempts"))
    Pcheck=merge(Pcheck, temp, by="userproblem_id")
  
    ##Is the attempt final? Create a boolean variable that answers that
    Pcheck$final_attempt=(Pcheck$max_user_attempts==Pcheck$attempts)
    Pcheck=merge(Pcheck,person.problem[,c("userproblem_id","serve_time", "problem_short_id","category","gformat","name","graded","data")],by="userproblem_id")
    
    #######################serve_time REDEFINITION###############
    #"serve_time" is the timestamp given when the page with the problem was loaded. 
    #But if there are multiple problems on a page, they will share this timestamp, which will be incorrect serve time for all problems on the page except the one submitted first.
    #So, after the first one, we will call serve_time the time of the submit of the preceding problem from the same page. I.e. for the first submit of a problem, look for the time of the last submit on a different problem from the same page.
    Pcheck_f=Pcheck
    #Form an identifier for user and serve_time, so to speak a user-pageload id:
    Pcheck_f$userserve_time=paste(Pcheck_f$user_id,Pcheck_f$serve_time);
    
    #Order by user-pageload, and within by submit time:
    Pcheck_f=Pcheck_f[order(Pcheck_f$userserve_time,Pcheck_f$time),]
    
    #For each first-attempt user-problem interaction, replace the serve_time with the submit time from the previous row
    ind=which(Pcheck_f$attempts==1)
    ind=ind[ind!=1] #Drop the first row
    #Store aside the pageload times:
    Pcheck_f$load_time=Pcheck_f$serve_time
    #Perform the serve_time replacement:
    Pcheck_f$serve_time[ind]=Pcheck_f$time[ind-1]
    #Undo the time replacement for first-submitted problem in each page:
    ind=1+which(Pcheck_f$userserve_time[-nrow(Pcheck_f)]!=Pcheck_f$userserve_time[-1])
    Pcheck_f$serve_time[ind]=Pcheck_f$load_time[ind]
    
    ##Now we have a situation when different attempts by the same user at the same problem display different serve_time: the 1st attempt will display the clever one, and the others still display the page-load time.
    ##It doesn't really matter for us, but might as well fix it:
    Pcheck_f=aggregate(Pcheck_f$serve_time, by=list(userproblem_id=Pcheck_f$userproblem_id), FUN=max)
    names(Pcheck_f)[names(Pcheck_f)=="x"]="serve_time"
    ##Now merge the new serve_times back into Pcheck (since Pcheck_f is only one attempt per problem), replacing the existing serve_time:
    Pcheck$serve_time=NULL
    Pcheck=merge(Pcheck,Pcheck_f[,c("userproblem_id","serve_time")], by="userproblem_id")

    
    #######################END OF serve_time REDEFINITION###############
    
    #Ta-da! We have our Pcheck ready! Let's just back it up before calculations
    Pcheck.backup=Pcheck

    if(calculate){
    
      cases=c("correct","incorrect","any")
      for (case in cases){
        Pcheck=Pcheck.backup
        ##This is to make sure that we look at only those second attempts that followed an incorrect 1st attempt, because sometimes people are crazy.
        temp1=subset(Pcheck,(Pcheck$attempts==1)&(Pcheck$success=="incorrect"))
        
        if(case=="correct"){
          temp=subset(Pcheck, (Pcheck$attempts==1)&(Pcheck$success=="correct"));
          temp2=subset(Pcheck,(Pcheck$attempts==2)&(Pcheck$success=="correct"))
        }
        if(case=="incorrect"){
          temp=subset(Pcheck, (Pcheck$attempts==1)&(Pcheck$success=="incorrect"));
          temp2=subset(Pcheck,(Pcheck$attempts==2)&(Pcheck$success=="incorrect"))
        }
        if(case=="any"){
          temp=subset(Pcheck, (Pcheck$attempts==1));
          temp2=subset(Pcheck,(Pcheck$attempts==2))
        }
  
        temp$logwait1=log(temp$time-temp$serve_time)

        ind=intersect(temp1$userproblem_id,temp2$userproblem_id)
        temp1=subset(temp1,temp1$userproblem_id %in% ind)
        temp2=subset(temp2,temp2$userproblem_id %in% ind)
        temp1=temp1[order(temp1$userproblem_id),]
        temp2=temp2[order(temp2$userproblem_id),]
        temp1$logwait2=log(temp2$time-temp1$time)
        Pcheck=merge(Pcheck,temp[,c("userproblem_id","logwait1")],by="userproblem_id", all=T)
        Pcheck=merge(Pcheck,temp1[,c("userproblem_id","logwait2")],by="userproblem_id", all=T)

        ##Now keep only one row for each user-problem pairing. But this row contains the logtimes for 1st and 2nd attempts.
        Pcheck=subset(Pcheck,(Pcheck$attempts==1)&(Pcheck$max_attempts<=max.attempt.limit))

        #Restrict to users and problems that saw a lot of action
        temp=aggregate(Pcheck$problem_id, by=list("user_id"=Pcheck$user_id), FUN=length)
        if(max(temp$x)<min.problems){
          cat("No users who attempted",min.problems,"problems. Proceeding with users who attempted",max(temp$x),"problems\n")
          min.problems=max(temp$x)
        }
        users.much=temp$user_id[temp$x>=min.problems]

        temp=aggregate(Pcheck$user_id, by=list("problem_id"=Pcheck$problem_id), FUN=length)
        if(max(temp$x)<min.users){
          cat("No problems attempted by",min.users,"users. Proceeding with problems attempted by",max(temp$x),"users.\n")
          min.users=max(temp$x)
        }
        problems.much=temp$problem_id[temp$x>=min.users]

        Pcheck=subset(Pcheck, (Pcheck$user_id %in% users.much)&(Pcheck$problem_id %in% problems.much))


        ##Fit the model
        fit1=fit.gaussian(Pcheck,var.fit = "logwait1",max.iter=max.iter)
        fit2=fit.gaussian(Pcheck,var.fit = "logwait2",max.iter=max.iter)

        ##Create folder for output data:
        # if(!file.exists(course_id)){
        #   dir.create(course_id)
        # }
        # save(fit1,fit2,PCS,file=file.path(course_id,paste0("fits_explored_",case,".RData")))
      }##End of cycling through cases

    }##End of the if-clause for "calculate"

  }) #End of "try"
}#End of cycling through courses

