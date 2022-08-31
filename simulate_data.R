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

#### Count Data ####
simulate_count_data<-function(type, plots,length, intercept, plot_eff_sd, trend=1, size=1){

  Count_Accumlator<-function(old, new, type, size){
    switch(type,
    p=rpois(n=1, lambda = exp(new + log(old) ) ),
    nb=rnbinom(n=1, mu=exp(new + log(old)), size=size)
    )
  }
    
   Data<-data.frame(Plot=rep(paste0("Plot-",1:plots), times=length), 
                  Time=rep(0:(length-1), each=plots),
                  Intercept=intercept,
                  Plot_eff=rnorm(plots, 0, plot_eff_sd),
                  Trend=trend,
                  Type=type,
                  Size=size)
   
  Data<- Data %>% mutate(Log_Intercept=log(Intercept),
                         Log_Trend=ifelse(Time==0, 0, log(Trend))) %>% 
      dplyr::rowwise() %>% 
      mutate(Expected_Lin_Change=ifelse(Time==0, 
                        sum(Log_Intercept,Log_Trend,Plot_eff),
                        sum(Log_Trend) )) %>% 
      ungroup() %>% 
      group_by(Plot) %>% arrange (Time) %>% 
      mutate(Count=(Expected_Lin_Change %>% accumulate(.f=Count_Accumlator, type=type,size=size,
                      .init=1))[-1]
                  )
  return(Data)
}





#### Continuous Data ####

simulate_continuous_data<-function(type, plots,length, intercept, plot_eff_sd, trend=1, sd){
  
  Count_Accumlator<-function(old, new, type, sd){
    switch(type,
           n=rnorm(n=1, mean = new +old, sd=sd ) #,
           #nb=rnbinom(n=1, mu=exp(new + log(old)), size=size)
    )
  }
  
  Data<-data.frame(Plot=rep(paste0("Plot-",1:plots), times=length), 
                   Time=rep(0:(length-1), each=plots),
                   Intercept=intercept,
                   Plot_eff=rnorm(plots, 0, plot_eff_sd),
                   Trend=trend,
                   Type=type,
                   Size=size)
  
  Data<- Data %>% mutate(Log_Intercept=log(Intercept),
                         Log_Trend=ifelse(Time==0, 0, log(Trend))) %>% 
    dplyr::rowwise() %>% 
    mutate(Expected_Lin_Change=ifelse(Time==0, 
                                      sum(Log_Intercept,Log_Trend,Plot_eff),
                                      sum(Log_Trend) )) %>% 
    ungroup() %>% 
    group_by(Plot) %>% arrange (Time) %>% 
    mutate(Count=(Expected_Lin_Change %>% accumulate(.f=Count_Accumlator, type=type,size=size,
                                                     .init=1))[-1]
    )
  return(Data)
}
