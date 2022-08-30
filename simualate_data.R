## Function to create data simulated data for foret trends power analysis.
library(tidyverse)


## Start with count data


## type = type of distribution ("p"= Poisson, "nb" = negative binomial)
## plots = number of observations per time step (e.g. number of plots)
## init_mean = mean of data at time 1
## trend = average proporiton change per unit time (e.g 1= no change, 1.01= +1%)
## length = number of time periods in the study
## plot_eff = random effect size of plots on the log scale. (e.g. the SD of the random effect of plot on log scale)
# resid = residual variance not otherwise accounred for, on the log scale
#size = size parameter for rnbinom() function

simulate_count_data<-function(type, plots,length, intercept, plot_eff_sd, trend=1, resid_sd, size){
  
# Create Time 0 data:  
  
   Data<-data.frame(Plot=rep(paste0("Plot-",1:plots), times=length), 
                  Time=rep(0:(length-1), each=plots),
                  Intercept=intercept,
                  Plot_eff=rnorm(plots, 0, plot_eff_sd),
                  Resid=rnorm(plots*length, 0, resid_sd),
                  Trend=trend,
                  Type=type,
                  Size=size)

   
    Data<- Data %>% mutate(Log_Intercept=log(Intercept),
                         Log_Trend=ifelse(Time==0, 0, log(Trend))) %>% 
      dplyr::rowwise() %>% 
      mutate(
        Expected_Lin_Change=ifelse(Time==0, sum(Log_Intercept,Log_Trend,Plot_eff,Resid),
                                         sum(Log_Trend,Resid) )) %>% 
      ungroup() %>% 
      group_by(Plot) %>% arrange (Time) %>% 
      mutate(Lag_Mean=ifelse(Time==0, 0, cumsum(Expected_Lin_Change))) %>% 
    
      rowwise() %>% 
      mutate(Expected_Mean=sum(Expected_Lin_Change, Lag_Mean),
             Count=switch(Type,
               p = rpois(n=1, lambda = exp(Expected_Mean)),
              nb = rnbinom(n=1, mu=exp(Expected_Mean), size=Size))
      )
    
    
  return(Data)

 
}


#output needs to be data.frame with time stamp, plot code and value