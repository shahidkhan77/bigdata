## Author: Rajesh Jakhotia
## Company Name: K2 Analytics Finishing School Pvt. Ltd
## Email : ar.jakhotia@k2analytics.co.in
## Website : k2analytics.co.in

## List of Libraries
library(data.table)
library(scales)

## deciling code
decile <- function(x){
  x
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
    ifelse(x<deciles[2], 2,
    ifelse(x<deciles[3], 3,
    ifelse(x<deciles[4], 4,
    ifelse(x<deciles[5], 5,
    ifelse(x<deciles[6], 6,
    ifelse(x<deciles[7], 7,
    ifelse(x<deciles[8], 8,
    ifelse(x<deciles[9], 9, 10
    ))))))))))
}

## set the working directory of folder to dump the output
## compile the function


fn_biz_viz <- function(df, target, var)
{

  tmp <- df[, c(var , target)]
  colnames(tmp)[1] = "Xvar"
  colnames(tmp)[2] = "Target"
  
  
  tmp$deciles <- decile(tmp$Xvar)
  
  
  tmp_DT = data.table(tmp)
  
  RRate <- tmp_DT[, list(
                  min_ = min(Xvar), max_ = max(Xvar), avg_ = mean(Xvar),
                  cnt = length(Target), cnt_resp = sum(Target), 
                  cnt_non_resp = sum(Target == 0)
                  ) , 
                by=deciles][order(deciles)]
  
  RRate$range = paste(RRate$min_ , RRate$max_ , sep = " to ");
  RRate$prob <- round(RRate$cnt_resp / RRate$cnt,3);
  
  setcolorder(RRate, c(1, 8, 2:7, 9))

  
  RRate$cum_tot <- cumsum(RRate$cnt)
  RRate$cum_resp <- cumsum(RRate$cnt_resp)
  RRate$cum_non_resp <- cumsum(RRate$cnt_non_resp)
  RRate$cum_tot_pct <- round(RRate$cum_tot / sum(RRate$cnt),3);
  RRate$cum_resp_pct <- round(RRate$cum_resp / sum(RRate$cnt_resp),3);
  RRate$cum_non_resp_pct <- round(RRate$cum_non_resp / sum(RRate$cnt_non_resp),3);
  RRate$ks <- percent(abs(RRate$cum_resp_pct - RRate$cum_non_resp_pct));
  
  RRate$prob = percent(RRate$prob)
  RRate$cum_tot_pct = percent(RRate$cum_tot_pct)
  RRate$cum_resp_pct = percent(RRate$cum_resp_pct)
  RRate$cum_non_resp_pct = percent(RRate$cum_non_resp_pct)
  
  ## Output the RRate table to csv file
  ## you should ensure the setwd -  working directory 
  write.csv(RRate, file = paste0(output_folder, var, ".csv"),
            row.names = FALSE)
  View(RRate)
}


