#This function takes in a log of problem-submit events and fits the our model to it. IT takes LOGARITHMS of response-times as inputs, and so fits 
# a normal, not a log-normal, distribution.

# Inputs:
#   
#   Pcheck: the log data-frame, should contain a column "problem_id", a column "user_id", 
#   and a column containing a variable that we would like to fit GAUSSIAN model to, i.e. 
#   for our purposes the logarithm of response time.
# 
#   var.fit: the name of the column in Pcck to which the Gaussian model will be fitted. Default: "logwait1"
#   
#   epsilon: the minimum value of the parameter alpha (since alpha=0 causes singularity). Default 1e-15
#   
#   tolerance, max.iter: parameters passed on to Rcgmin.
# 
# Output: a list containing the following elements:
#   users: a data-frame of user slownesses
#   probs: a data-frame of question parameters (discrimination alpha and time-intensity beta)
#   L: log-likelihood achieved in maximization
#   p.ave: likelihood achieved in maximization divided by the number of data rows, as a measure of goodness of fit
#   rcgmax: the output object of the Rcgmin technique (the function being minimized is the negative log-likelihood)
#   m.var: the fitted variable (i.e. logarithm of response time) in the matrix form, with problem_ids on columns and user_ids on rows
#   m.var.norm: similar to m.var, but each value in the matrix is normalized (shifted by the problem time-intensity and user slowness, 
#                                                                             multiplied by problem discrimination). In an ideal fit the values of m.var.norm would form standard normal distribution.


    

fit.gaussian=function(Pcheck,var.fit="logwait1", epsilon=1e-15, tolerance=NULL, max.iter=2500){
library(Rcgmin)
  
  
df0=Pcheck[!is.na(Pcheck[,var.fit]),c("problem_id","user_id",var.fit)]





probs=data.frame(problem_id=sort(unique(df0$problem_id)))
users=data.frame(user_id=sort(unique(df0$user_id)))

m.var<<-matrix(NA,ncol=nrow(probs),nrow=nrow(users))
rownames(m.var)=users$user_id
colnames(m.var)=probs$problem_id
for (i in 1:nrow(df0)){
  temp=df0[i,]
  m.var[temp$user_id,temp$problem_id]=temp[,var.fit]
}

m.pattern<<-matrix(1,ncol=nrow(probs),nrow=nrow(users))
m.pattern[which(is.na(m.var))]=NA;



lp<<-nrow(probs)
lu<<-nrow(users)
probs$alpha=1/sd(df0[,var.fit])
probs$alpha[is.na(probs$alpha)]=mean(probs$alpha,na.rm=T)



probs$beta=mean(df0[,var.fit])


users$zeta=0

par=c(probs$alpha,probs$beta,users$zeta);
lpar<<-length(par)


m.alpha<<-matrix(0,ncol=lp,nrow=lu)
rownames(m.alpha)=users$user_id
colnames(m.alpha)=probs$problem_id

m.beta<<-matrix(0,ncol=lp,nrow=lu)
rownames(m.beta)=users$user_id
colnames(m.beta)=probs$problem_id

m.zeta<<-matrix(0,ncol=lp,nrow=lu)
rownames(m.zeta)=users$user_id
colnames(m.zeta)=probs$problem_id

fn=function(par){
  
  m.alpha=matrix(rep(par[1:lp],lu),ncol=lp,nrow=lu, byrow = T)*m.pattern
  m.beta=matrix(rep(par[(1+lp):(2*lp)],lu),ncol=lp,nrow=lu, byrow = T)*m.pattern
  m.zeta=matrix(rep(par[(1+2*lp):lpar],lp),ncol=lp,nrow=lu, byrow = F)*m.pattern
  
  L=sum(-log(m.alpha)+0.5*((m.alpha*(m.beta+m.zeta-m.var))^2),na.rm=T)
  return(L)
}

gr=function(par){
  m.alpha=matrix(rep(par[1:lp],lu),ncol=lp,nrow=lu, byrow = T)*m.pattern
  m.beta=matrix(rep(par[(1+lp):(2*lp)],lu),ncol=lp,nrow=lu, byrow = T)*m.pattern
  m.zeta=matrix(rep(par[(1+2*lp):lpar],lp),ncol=lp,nrow=lu, byrow = F)*m.pattern
  
  m.bzv=m.beta+m.zeta-m.var
  
  grad.alpha=colSums(-1/m.alpha + m.alpha*(m.bzv)^2, na.rm=T)
  temp=m.bzv*(m.alpha)^2
  grad.beta=colSums(temp,na.rm=T)
  grad.zeta=rowSums(temp,na.rm=T)

  grad=c(grad.alpha,grad.beta,grad.zeta)
  
  return(grad)
}

lower=c(rep(epsilon,lp),rep(-Inf,lp),rep(-Inf,lu))
upper=c(rep(Inf,lp),rep(Inf,lp),rep(Inf,lu))

if(is.null(tolerance)){
  rcgmax=Rcgmin(par=par, fn=fn, gr=gr, lower=lower, upper=upper, control=list(maxit=max.iter))
}else{
rcgmax=Rcgmin(par=par, fn=fn, gr=gr, lower=lower, upper=upper, control=list(maxit=max.iter, tol=tolerance))
}

probs$alpha=rcgmax$par[1:lp]
probs$beta=rcgmax$par[(1+lp):(2*lp)]
users$zeta=rcgmax$par[(1+2*lp):lpar]

L=-rcgmax$value;

##Since beta and zeta enter only as a sum, there is gauge freedom of shifting them by opposite amounts. Let's fix this gauge by setting the mean of zeta to 0.
shift=mean(users$zeta);
users$zeta=users$zeta-shift
probs$beta=probs$beta+shift

m.var.norm=(m.var-matrix(rep(users$zeta,lp),nrow=lu,byrow=F)-matrix(rep(probs$beta,lu),nrow=lu,byrow=T))*matrix(rep(probs$alpha,lu),nrow=lu,byrow=T)







ls=list(users=users[,c("user_id","zeta")],probs=probs[,c("problem_id","alpha","beta")],L=L, p.ave=exp(L/nrow(df0)), rcgmax=rcgmax, m.var.norm=m.var.norm, m.var=m.var)

return(ls)
}
