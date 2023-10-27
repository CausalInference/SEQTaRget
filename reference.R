#################################################################################
###    Target Trial Emulation                                                 ### 
###    Hands-on Session 6 - Adjusting for Time-Varying Confounding and        ### 
###                         Selection Bias                                    ### 
###    Summer 2023                                                            ### 
###    CAUSALab                                                               ### 
#################################################################################

###########################   SOLUTIONS   #######################################

# Set working directory
#setwd("/path_to_your_data")
rm(list = ls())
#setwd("/Users/emma/Documents/Harvard/Teaching/Teaching Fellow Positions/CAUSALab Target Trial Course_Summer 2023/Simulated Data")

# Import data
load(file = "vac_obs.Rdata")

# Load required packages
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("Hmisc")) install.packages("Hmisc")

### Data preparation ###

# Collapse categories of baseline_risk to decrease sparse strata and model variance
vac_obs$baseline_risk <- ifelse(vac_obs$baseline_risk==5, 4, vac_obs$baseline_risk)

# Create time since first treat variable (to be used for restricting model set 2)
vac_obs$time_since_first_treat <- vac_obs$cal_time - vac_obs$first_treat

# Censor administratively 24 weeks after the latest possible baseline date 
# i.e., 24 weeks after June 28, 2021
vac_obs <- vac_obs[(vac_obs$cal_time <= 26), ]

### Create function that performs confounding and selection bias adjustment in time-varying setting ###

### Create general function for censoring for treatment non-adherence ###
# This function will be called in the seq_em_sustain function
# The exact censoring protocol for our target trial will be implemented in later steps

any.censor <- function(censor){
  # find first time my.censor == 1 in a trial
  # n = number of time points in trial, fu (follow-up time in trial) ranges from 0 to n-1
  # censtime is observation of first censor
  # time of first censor will be censtime - 1 
  # want observations from fu = 0 through censtime - 1, or fu < censtime
  # when my.censor = 1 (censored when deviate from treatment protocol),  fu = censtime - 1 , outcome = hosp = NA 
  
  n <- length(censor)
  censtime <- n 
  for(time in 1:n){
    if (censor[time] == 1  & censtime == n) {
      censtime <- time }
  }
  return(censtime)
}

### Create seq_em_sustain function ###

seq_em_sustain <- function(
			datain = data, # define dataset
			timepoints = 24,
			use.weights = 1 , # define if treatment weights will be used
			pool_wt_models = 0,
			stratify_wt_cond1 = treat_cum_lag1 ==0   ,
			stratify_wt_cond2 = (treat_cum_lag1==1 & 2 <= time_since_first_treat & time_since_first_treat<=8) ,
			nonstratify_wt_cond = (TRUE ) ,
			eligible_cense = (TRUE ),
			use.censor = 0 , # define if censoring weights for loss to follow-up will be used
			use.stabilized.weights = 1 , # define whether weights will be stabilized (1) or not (0)
			nboot = 0 ,        # defne # of bootstrap samples
			start.seed = 1234, # define a random seed
			model_switchd = "", # define the model for the denominator of the treatment weights
			model_switchn = "", # define the model for the numerator of the treatment weights
			model_censed = "",  # define the model for the denominator of the censoring weights for loss to follow-up
			model_censen = "",
			followup_time_as_class = 0 , # for short follow-up length in trials,  consider follow-up time as a categorical variable 
      followup_time_as_spline = 0 , #  for weighted model include extra spline variables for follow-up time 
			weight_pct = 0.99 
		  
		  
		  
		  
		  ){ # define the model for the numerator of the censoring weights for loss to follow-up


set.seed(start.seed)
sample.seeds <- sample.int(2^30, size = (nboot + 1))
timepoints.m1 <- timepoints - 1 
local.env <- environment() 

if(nboot>0){
  # Read in dataset
  mydata0.tmp <- datain
  
  # Restrict to variables used in our analyses to improve computation time
  mydata0 <- copy(mydata0.tmp[,.(id,cal_time,cal_timesqr,censor,death,hosp,elig_1,elig_2,treat,treat_cum,treat_cum_lag1,first_treat,time_since_first_treat,sex,race,urban,age,baseline_risk,symp,symp_lag1)])
  rm(mydata0.tmp)
  
} else {

  # Equivalent code as above, for case where bootstrap = 0
  mydata.tmp <- datain
  
  # Restrict to variables used in our analyses (to improve computation time)
  mydata <- copy(mydata.tmp[,.(id,cal_time,cal_timesqr,censor,death,hosp,elig_1,elig_2,treat,treat_cum,treat_cum_lag1,first_treat,time_since_first_treat,sex,race,urban,age,baseline_risk,symp,symp_lag1)])
  rm(mydata.tmp)
}

if (nboot > 0 ){data_len <- length(unique(mydata0$id))}

for(bootstrap.sample in 0:nboot){
  
  print(paste("running bootstrap sample " ,bootstrap.sample))
  if(nboot > 0){  
    if(bootstrap.sample > 0){
      # Draw bootstrap samples and generate bootstrap-specific id
      set.seed(sample.seeds[bootstrap.sample+1])
      ids <- as.data.table(sample(1:data_len, data_len, replace = TRUE))
      ids[, 'bid' := 1:data_len]
      colnames(ids) <- c("id", "bid")
      setkey(mydata0, "id")
      mydata<- mydata0[J(ids), allow.cartesian = TRUE] [,id:=NULL]  # Create the new data set names "sample"
      setnames(mydata,"bid","id")
    } else { mydata<- copy(mydata0)}
  }
  
if(use.weights == 1) {  
	if( pool_wt_models == 0 ){
		# Fit model for denominator of treatment weight
		# Model set 1
		t_d1 <- speedglm::speedglm(data=mydata[ eval(substitute(stratify_wt_cond1, local.env)) ,], 
                          family=binomial(), 
                          model_switchd)
	#	print(summary(t_d1))
		
		# Model set 2
		t_d2 <- speedglm::speedglm(data=mydata[eval(substitute(stratify_wt_cond2, local.env)),], 
                             family=binomial(), 
                             model_switchd)
							 
	#	print(summary(t_d2))
		
		mydata[,pA_d := NA_real_ ]
	  mydata[eval(substitute(stratify_wt_cond1, local.env)), pA_d := predict(t_d1, newdata= mydata[ eval(substitute(stratify_wt_cond1 , local.env)), ] , type="response")]
		mydata[eval(substitute(stratify_wt_cond1, local.env)), pA_d := fcase(treat==1 , pA_d,
                                        treat==0 , 1-pA_d)]
		mydata[eval(substitute(stratify_wt_cond2, local.env)), pA_d := predict(t_d2, newdata=mydata[eval(substitute(stratify_wt_cond2 , local.env)),], type="response")]
		mydata[eval(substitute(stratify_wt_cond2, local.env)), pA_d := fcase(treat==1 , pA_d,
                                                treat==0 , 1-pA_d)]
		
	
		mydata[ !( eval(substitute(stratify_wt_cond1 , local.env)) | eval(substitute(stratify_wt_cond2, local.env))) , pA_d := 1]
		

		rm(t_d1, t_d2)
	} else {
		# Fit model for denominator of treatment weight
		# Pooled model set 
		t_d <- speedglm::speedglm(data=mydata[ eval(substitute(nonstratify_wt_cond, local.env)) ,] , 
                          family=binomial(), 
                          model_switchd)
		mydata[,pA_d := NA_real_ ]
		mydata[eval(substitute(nonstratify_wt_cond, local.env)) , pA_d := predict(t_d, newdata=mydata[eval(substitute(nonstratify_wt_cond , local.env)) , ], type="response")]
		mydata[eval(substitute(nonstratify_wt_cond, local.env)) , pA_d := fcase(treat==1 , pA_d,
                                                  treat==0 , 1-pA_d)]
		mydata[!(eval(substitute(nonstratify_wt_cond, local.env))) , pA_d := 1 ]
										
		rm(t_d)
	}
	
### Weights for censoring due to treatment non-adherence (treatment weights)
if(use.stabilized.weights == 1){
	if(pool_wt_models == 0){
		# Fit model for numerator of treatment weights, if using stabilized weights
		# Model set 1
		t_n1 <- speedglm::speedglm(data=mydata[ eval(substitute(stratify_wt_cond1, local.env)),], 
                            family=binomial(),
                            model_switchn
			) 
		# Model set 2
		t_n2 <- speedglm::speedglm(data=mydata[eval(substitute(stratify_wt_cond2, local.env)) ,], 
                             family=binomial(),
                             model_switchn 
			) 
  
		# Generate time-specific numerators and denominators, accounting for times when no treatment can be recieved and probability = 1
		mydata[,pA_n := NA_real_ ]
		mydata[eval(substitute(stratify_wt_cond1, local.env)) ,pA_n := predict(t_n1, newdata=mydata[eval(substitute(stratify_wt_cond1, local.env)) ,], type="response")]
		mydata[eval(substitute(stratify_wt_cond1, local.env)) ,pA_n := fcase(treat==1 , pA_n,
											                                                   treat==0 , 1-pA_n)]
		mydata[eval(substitute(stratify_wt_cond2, local.env)), pA_n := predict(t_n2, newdata=mydata[eval(substitute( stratify_wt_cond2 , local.env)),], type="response")]
		mydata[eval(substitute(stratify_wt_cond2 ,local.env)), pA_n := fcase(treat==1 , pA_n,
											                                                   treat==0 , 1-pA_n)]
		mydata[ !(eval(substitute(stratify_wt_cond1, local.env)) | eval(substitute(stratify_wt_cond2,local.env ))), pA_n := 1]
		rm(t_n1, t_n2)
		
		} else {
		# Fit model for numerator of treatment weights, if using stabilized weights
		# Pooled model set 1
		t_n <- speedglm::speedglm(data=mydata[ eval(substitute(nonstratify_wt_cond , local.env)) , ], 
                            family=binomial(),
                            model_switchn
			) 
		mydata[,pA_n := NA_real_ ]
		mydata[eval(substitute(nonstratify_wt_cond, local.env)) ,pA_n := predict(t_n, newdata=mydata[eval(substitute(nonstratify_wt_cond, local.env)),], type="response")]
		mydata[ eval(substitute(nonstratify_wt_cond, local.env)) ,pA_n := fcase(treat==1 , pA_n,
												                                                    treat==0 , 1-pA_n)]
    mydata[ !(eval(substitute(nonstratify_wt_cond,local.env))) , pA_n:= 1 ]												   
    rm(t_n)																					  

	}	
} else {
  # Setting numerator to 1 if using unstabilized weights
  mydata[,pA_n := 1]
}

### Weights for censoring due to loss to follow-up
if ( use.censor == 1) {
 # Fit model for the denominator of the censoring weights
  c_d <- speedglm::speedglm(data=mydata[ eval(substitute(eligible_cense, local.env)) , ] , 
                            family=binomial(), 
                            model_censed)
  mydata[,pC_d := NA_real_ ]
  mydata[eval(substitute(eligible_cense,local.env)),pC_d := predict(c_d, newdata=mydata[ eval(substitute(eligible_cense, local.env)) ,] , type="response")]
  mydata[eval(substitute(eligible_cense, local.env)) ,pC_d := 1-pC_d ]
  mydata[ !(eval(substitute(eligible_cense, local.env))) , pC_d := 1 ]
  rm(c_d)
  
  if(use.stabilized.weights == 1){
    # Fit model for the numerator of censoring weights, if using stabilized weights
    c_n <- speedglm::speedglm(data=mydata[ eval(substitute(eligible_cense,local.env)) ,] , 
                              family=binomial(), 
                              model_censen
    )
    
    
    mydata[,pC_n := NA_real_ ]
    mydata[ eval(substitute(eligible_cense, local.env)) ,pC_n := predict(c_n, newdata=mydata[eval(substitute(eligible_cense, local.env)) ,], type="response")]
    mydata[ eval(substitute(eligible_cense, local.env)),pC_n := 1-pC_n ]
	  mydata[ !(eval(substitute(eligible_cense, local.env))) , pC_d := 1 ]
    rm(c_n)
  } else {
    # Setting numerator equal to 1 for unstabilized weights
    mydata[,pC_n := 1]
  }
  # Generate time-specific contributions to the censoring weights
mydata[,wtC:= pC_n/pC_d] [is.na(censor) , wtC:= 1 ] 
   
} else  { mydata[, wtC:= 1 ] }

# Generate the time-specific contributions to the final weights
mydata[ , wtA := pA_n/pA_d ][, wt:=wtA * wtC ] 
} else {
  mydata[, wt:= 1 ]
}


if(followup_time_as_class == 1){
   max.fu = max(mydata[,cal_time])
   }
mydata[,maxt:=max(cal_time),by=id]


elig<-mydata[elig_1 == 1][, trial := cal_time ][, arm := treat]

# Implement the censoring protocol for treatment non-adherence
# I.e., censor individuals when they deviate from their 'assigned' treatment strategy
mytrial<-  elig[, .(cal_time = trial:maxt , fu = trial :maxt - trial  ),by=.(id,trial)
][elig[,.(id,trial,arm)],':='(arm=i.arm ), on=.(id,trial)
][ mydata[,.(id,cal_time,wt,treat,hosp ,death)],':='(wt=i.wt,treat=i.treat,hosp=i.hosp , death = i.death),on=.(id,cal_time)
][,my.censor:= fcase((arm == 0 &           treat == 1), 1 ,
                  (arm == 1 & fu == 1 & treat == 1), 1 ,
                  (arm == 1 & fu == 2 & treat == 1), 1 ,
                  (arm == 1 & fu == 3 & treat == 0), 1,
                  default = 0 )
][,mycenstime:=any.censor(my.censor),by=.(id,trial)][fu<mycenstime][my.censor == 1, hosp:=NA]
# mycenstime = observation of first time my.censor == 1. 
# Restrict followup time to be when fu < mycenstime 




rm(mydata)

# Take the cumulative product of the weights at each time point, starting at the start of the mth sequential trial
mytrial[,wtprod:=cumprod(wt),by=c('id','trial')][,c('trial2','fu2'):=.(trial^2 , fu^2)]

# Truncate weights at the 99th percentile
max.weight<- quantile(mytrial[my.censor == 0 & death == 0 ,wtprod],probs= weight_pct , na.rm = TRUE)

# Print summary statistics for weights
print(summary(pmin(mytrial[my.censor == 0 & death == 0,wtprod],max.weight)))
print(sd(pmin(mytrial[my.censor == 0 & death == 0,wtprod],max.weight)))

 
# Fit a pooled logistic regression outcome model in the weighted pseudopopulation
#options(warn=-1) 
if(followup_time_as_class == 0 & followup_time_as_spline == 0){
  outc.model <- as.formula("hosp ~ trial + trial2 +  fu + fu2 + arm + arm:fu + arm:fu2 ")
} else if (followup_time_as_class == 0 & followup_time_as_spline == 1) {
 outc.model <- as.formula("hosp ~ trial + trial2  +  arm*fu + arm*Hmisc::rcspline.eval(fu)")


} else if (followup_time_as_class == 1 & followup_time_as_spline == 0){
     mytrial[,fu.cat := factor(fu,levels=0:max.fu)]
	 outc.model <- as.formula("hosp ~ trial + trial2 + arm * as.factor(fu) ")
}



m_n <- speedglm::speedglm(data=mytrial[my.censor==0 & death == 0], 
                          family=binomial(link="logit"), 
                          outc.model ,
                          weights = pmin(mytrial[my.censor==0 & death == 0 ,wtprod],max.weight))
#options(warn=-0) 
# NOTE [my.censor==0 & death == 0] is not necessary here - the model is identical without explicitly specifying this

#print(summary(m_n))
m<-mytrial[fu == 0,.(id,trial)][,( fu=0:(timepoints-1)),by=.(id,trial)]
setnames(m,"V1","fu")

# Estimate survival probabilities under each arm
m[,c('arm','trial2','fu2'):= .( 0 , trial*trial, fu*fu )]
m[,p0:=as.data.table(predict(m_n,newdata=m,type='response'))][,s0:=1.0-p0][,surv0id:=cumprod(s0),by=.(id,trial)][, c("p0","s0"):=NULL ]


m[,arm:=1]
m[,p1:=as.data.table(predict(m_n,newdata=m,type='response'))][,s1:=1.0-p1][,surv1id:=cumprod(s1),by=.(id,trial)]

# Estimate risks from survival probabilities
risk <- m[, .(risk1= 1-mean(surv1id) ,  risk0=1-mean(surv0id) ) ,by=fu][,':='(risk_diff = risk1 - risk0 , risk_ratio = risk1/risk0 , bsample = bootstrap.sample )]

if (bootstrap.sample == 0) {
     all.risk <- risk 
 }
 else {
   all.risk <- rbind(all.risk,risk )
 } 
rm(m,mytrial,risk,m_n,elig,max.weight)
}

test2 <-copy(all.risk)

base.sample <-test2[bsample == 0][,bsample:=NULL]
base.sample$risk_diff_lb <-test2[,quantile(risk_diff,probs=0.025), by = fu ][,V1]
base.sample$risk_diff_ub <-test2[,quantile(risk_diff,probs=0.975), by = fu ][,V1]
base.sample$risk_ratio_lb <-test2[,quantile(risk_ratio,probs=0.025), by = fu ][,V1]
base.sample$risk_ratio_ub <-test2[,quantile(risk_ratio,probs=0.975), by = fu ][,V1]


output <- list("all.results"=all.risk,"base.sample.results"=base.sample)

return(output)
}
# End of the seq_em_sustain function

gr <- 0 

if(1){

#################################################################################
###     DATA ANALYSIS EXERCISE 2                                              ###
#################################################################################

### Define models for nonstabilized weights for treatment ###

model_switchd <- "treat ~ age + sex +  as.factor(race) + urban + as.factor(baseline_risk) +
                          symp + symp_lag1 + cal_time + cal_timesqr "

#################################################################################
###     DATA ANALYSIS EXERCISE 3-4                                            ###
#################################################################################

### Fit an outcome regression using nonstabilized weights truncated at the 99th percentile ###

nonst_t <- seq_em_sustain(
  datain = vac_obs,
  use.weights = 1 ,
  use.censor = 0   ,
  use.stabilized.weights = 0 ,
  nboot = 0,
  start.seed = 1234,
  model_switchd = model_switchd)

# Print results

print(nonst_t$base.sample.results[fu==23])
}
if(1){

#################################################################################
###     DATA ANALYSIS EXERCISE 5                                              ###
#################################################################################

### Define models for stabilized weights for treatment ###

model_switchd <- "treat ~ age + sex +  as.factor(race) + urban + as.factor(baseline_risk) +
                          symp + symp_lag1 + cal_time + cal_timesqr "

model_switchn <- "treat ~ cal_time + cal_timesqr "

#################################################################################
###     DATA ANALYSIS EXERCISE 6-7                                            ###
#################################################################################

### Fit an outcome regression using stabilized weights truncated at the 99th percentile ###

st_t<-seq_em_sustain(
  datain = vac_obs,
  use.weights = 1 ,
  use.censor = 0   ,
  use.stabilized.weights = 1 ,
  nboot = 0,
  start.seed = 1234,
  model_switchd = model_switchd,
  model_switchn = model_switchn)

# Print results

print(st_t$base.sample.results[fu==23])

#################################################################################
###     DATA ANALYSIS EXERCISE 8                                              ###
#################################################################################

### Construct marginal parametric cumulative incidence (risk) curves ###
if(gr ==1 ){
# Create plot (without CIs)
graph.pred <- st_t$base.sample.results[,.(fu,risk0,risk1)]
graph.pred$time_0 <- graph.pred$fu + 1
zero <- data.frame(cbind(0,0,0,0))
zero <- setNames(zero,names(graph.pred))
graph <- rbind(zero, graph.pred)

plot.plr <- ggplot(graph, 
                   aes(x=time_0, y=risk)) + # set x and y axes
  geom_line(aes(y = risk1, # create line for vaccine group
                color = "CROWN Vaccine"),
            size = 1.5) + 
  geom_line(aes(y = risk0, # create line for no vaccine group
                color = "No Vaccine"),
            size = 1.5) +
  xlab("Weeks") + # label x axis
  scale_x_continuous(limits = c(0, 24), # format x axis
                     breaks=c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)) + 
  ylab("Cumulative Incidence (%)") + # label y axis
  scale_y_continuous(limits=c(0, 0.2), # format y axis
                     breaks=c(0, 0.05, 0.1, 0.15, 0.2),
                     labels=c("0.0%", "5.0%", "10.0%",
                              "15.0%", "20.0%")) + 
  theme_minimal()+ # set plot theme elements
  theme(axis.text = element_text(size=14), legend.position = c(0.2, 0.8),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_text(size= 14),
        legend.text = element_text(size = 10)) +
  scale_color_manual(values=c("#E7B800","#2E9FDF"), # set colors
                     breaks=c('No Vaccine', 'CROWN Vaccine')) 

# Plot
plot.plr
}
}
if(1){
#################################################################################
###     DATA ANALYSIS EXERCISE 9                                              ###
#################################################################################

### Define denominator model for stabilized weights for censoring due to loss to follow-up ###

model_censed <- "censor ~ age + sex +  as.factor(race) + urban + as.factor(baseline_risk) +
                         symp + symp_lag1 + as.factor(treat_cum) + cal_time + cal_timesqr "


#################################################################################
###     DATA ANALYSIS EXERCISE 10                                             ###
#################################################################################

### Define numerator model for stabilized weights for censoring due to loss to follow-up ###

model_censen <- "censor ~ as.factor(treat_cum) + cal_time + cal_timesqr "

#################################################################################
###     DATA ANALYSIS EXERCISE 11-12                                          ###
#################################################################################

### Fit an outcome regression using stabilized weights for treatment and censoring, truncated at the 99th percentile ###

st_c<-seq_em_sustain(
  datain = vac_obs,
  use.weights = 1 ,
  use.censor = 1   ,
  use.stabilized.weights = 1 ,
  nboot = 2,
  start.seed = 1234,
  model_switchd = model_switchd,
  model_switchn = model_switchn,
  model_censed = model_censed,
  model_censen = model_censen)

# Print results for the summary statistics for the weights, and also point estimates

print(st_c$base.sample.results[fu==23])

### Construct marginal parametric cumulative incidence (risk) curves ###
if(gr == 1){
# Create plot (without CIs)
graph.pred <- st_c$base.sample.results[,.(fu,risk0,risk1)]
graph.pred$time_0 <- graph.pred$fu + 1
zero <- data.frame(cbind(0,0,0,0))
zero <- setNames(zero,names(graph.pred))
graph <- rbind(zero, graph.pred)

plot.plr <- ggplot(graph, 
                   aes(x=time_0, y=risk)) + # set x and y axes
  geom_line(aes(y = risk1, # create line for vaccine group
                color = "CROWN Vaccine"),
            size = 1.5) + 
  geom_line(aes(y = risk0, # create line for no vaccine group
                color = "No Vaccine"),
            size = 1.5) +
  xlab("Weeks") + # label x axis
  scale_x_continuous(limits = c(0, 24), # format x axis
                     breaks=c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)) + 
  ylab("Cumulative Incidence (%)") + # label y axis
  scale_y_continuous(limits=c(0, 0.2), # format y axis
                     breaks=c(0, 0.05, 0.1, 0.15, 0.2),
                     labels=c("0.0%", "5.0%", "10.0%",
                              "15.0%", "20.0%")) +  
  theme_minimal()+ # set plot theme elements
  theme(axis.text = element_text(size=14), legend.position = c(0.2, 0.8),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_text(size= 14),
        legend.text = element_text(size = 10)) +
  scale_color_manual(values=c("#E7B800","#2E9FDF"), # set colors
                     breaks=c('No Vaccine', 'CROWN Vaccine')) 

# Plot
plot.plr
}
}


