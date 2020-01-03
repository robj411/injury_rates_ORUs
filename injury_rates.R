setwd('~/overflow_dropbox/injury_rates_ORUs/')
## load libraries
library(tidyr)
library(dplyr)
library(splines)
library(sfsmisc)
library(tensorA)
library(xlsx)
library(mvtnorm)
library(MASS)

## read data
roads <- c("rural_A", "urban_A", "rural_B", "urban_B")
modes <- c('car/taxi','van','lorry','motorcycle','bus','cyclist')
severity_levels <- c('Fatal','Serious','Slight')

raw_distance <- read.xlsx('~/overflow_dropbox/ITHIM/InjuryModel/five_road_data.xlsx',sheetIndex=1)
distance_sum <- #lapply(1:2,function(y)
  sapply(unique(raw_distance$Road.Type),function(x) colSums(subset(raw_distance,Road.Type==x)[,3:9]))/1e6
rownames(distance_sum) <- c('car/taxi','van','lorry','motorcycle','bus','total','cyclist')
colnames(distance_sum) <- c('M',"rural_A", "urban_A", "rural_B", "urban_B")
distance_sum <- distance_sum[-6,-1]

distance_sum_gen <- readRDS('~/overflow_dropbox/ITHIM/InjuryModel/total_distance_RA.Rds')
dimnames(distance_sum_gen)[[2]][c(5,7)] <- c('van','lorry')
distance_sum_gen <- distance_sum_gen[[road=roads,mode=modes,passenger=1]]*1e-9

ssg.file <- '~/overflow_dropbox/ITHIM/InjuryModel/stats19_05-15_with_NA.Rds'
test_data_str_raw <- readRDS(ssg.file)
test_data_str_raw$strike_mode[test_data_str_raw$strike_mode=='heavy goods'] <- 'lorry'
test_data_str_raw$strike_mode[test_data_str_raw$strike_mode=='light goods'] <- 'van'

## view some summaries
bike_ids <- unique(droplevels(subset(test_data_str_raw,strike_mode=='cyclist'&cas_severity=='Fatal',drop=T)$accident_index))
bike_fatalities <- subset(test_data_str_raw,accident_index%in%bike_ids)

multi_vehicle_subset <- function(x){
  a <- nrow(x)
  b <- nrow(subset(x,number_of_vehicles>2|(number_of_vehicles==2&cas_mode=='pedestrian')))
  data.frame(total=a,multivehicle=b,percent=b/a*100)
}
multi_vehicle_subset(test_data_str_raw)
multi_vehicle_subset(subset(test_data_str_raw,cas_severity=='Fatal'))
multi_vehicle_subset(subset(test_data_str_raw,cas_severity=='Fatal'&
                              cas_mode%in%c('pedestrian','car/taxi','light goods','heavy goods','motorcycle','bus','cyclist')&
                              strike_mode%in%c('car/taxi','light goods','heavy goods','motorcycle','bus','cyclist')&
                              roadtype%in%c('A','B, C, Unclassified')&
                              urban_or_rural_area%in%1:2))
multi_vehicle_subset(subset(test_data_str_raw,cas_severity=='Fatal'&
                              cas_mode%in%c('pedestrian')&
                              strike_mode%in%c('car/taxi','light goods','heavy goods','motorcycle','bus','cyclist')&
                              roadtype%in%c('A','B, C, Unclassified')&
                              urban_or_rural_area%in%1:2))

## rm other or unknown cas
test_data_str_raw <- subset(test_data_str_raw,cas_mode!='other or unknown')


test_data_str <- group_by(test_data_str_raw, cas_severity, roadtype, strike_mode, cas_mode, cas_male, strike_male, urban_or_rural_area) %>% 
  summarise(count=dplyr::n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_severity, roadtype, strike_mode, cas_mode, cas_male, strike_male, urban_or_rural_area, fill=list(count=0)) %>%
  arrange(cas_severity, roadtype, strike_mode)  


test_data_str$roadtype[test_data_str$roadtype=='Motorway/A(M)'] <- 'M'
test_data_str$roadtype[test_data_str$roadtype=='A'&test_data_str$urban_or_rural_area==2] <- 'rural_A'
test_data_str$roadtype[test_data_str$roadtype=='A'&test_data_str$urban_or_rural_area==1] <- 'urban_A'
test_data_str$roadtype[test_data_str$roadtype=='B, C, Unclassified'&test_data_str$urban_or_rural_area==2] <- 'rural_B'
test_data_str$roadtype[test_data_str$roadtype=='B, C, Unclassified'&test_data_str$urban_or_rural_area==1] <- 'urban_B'

get_numbers <- function(cas_sev='Fatal',str_gen=0:1,c_mode='pedestrian'){
  sapply(roads,function(x)
    sapply(rownames(distance_sum),function(y)
      sum(subset(test_data_str,cas_mode%in%c_mode&strike_mode==y&roadtype==x&cas_severity==cas_sev&strike_male%in%str_gen)$count)))
}

for(sev in severity_levels){
  ## raw numbers
  numbers <- get_numbers(c_mode=unique(test_data_str$cas_mode),cas_sev = sev,str_gen = unique(test_data_str$strike_male))
  
  print(numbers/distance_sum)
  
  ## pedestrian casualties
  ped_numbers <- get_numbers(cas_sev = sev,str_gen = unique(test_data_str$strike_male))
  
  print(ped_numbers/distance_sum)
  
  ## write rates and numbers
  #year_group_names <- c('pre2010','post2009')
  to_save <- rbind(distance_sum,
                   numbers,
                   numbers/distance_sum,
                   ped_numbers,
                   ped_numbers/distance_sum)
  to_save <- cbind(to_save,
                   c(rowSums(distance_sum),
                     rowSums(numbers),
                     rowSums(numbers)/rowSums(distance_sum),
                     rowSums(ped_numbers),
                     rowSums(ped_numbers)/rowSums(distance_sum)))
  colnames(to_save)[5] <- 'total'
  write.csv(to_save,paste0('outputs/',sev,'_summaries.csv'))
  
  
  ## by gender
  numbers_gen <- lapply(c(0,1),function(z) get_numbers(c_mode=unique(test_data_str$cas_mode),cas_sev = sev,str_gen = z))
  
  ped_numbers_gen <- lapply(c(0,1),function(z) get_numbers(cas_sev = sev,str_gen = z))
  
  
  
  rates <- #lapply(1:2,function(w) 
    lapply(1:2,function(x) add.tensor(to.tensor(
      as.vector(numbers_gen[[x]]),
      dims=c(mode=nrow(numbers_gen[[1]]),road=ncol(numbers_gen[[1]])),
      ndimnames=list(rownames(numbers_gen[[1]]),colnames(numbers_gen[[1]]))),
      1/distance_sum_gen[[gen=x]],op='*'))
  
  rates_ped <- #lapply(1:2,function(w) 
    lapply(1:2,function(x) add.tensor(to.tensor(
      as.vector(ped_numbers_gen[[x]]),
      dims=c(mode=nrow(ped_numbers_gen[[1]]),road=ncol(ped_numbers_gen[[1]])),
      ndimnames=list(rownames(ped_numbers_gen[[1]]),colnames(ped_numbers_gen[[1]]))),
      1/distance_sum_gen[[gen=x]],op='*'))
  
  
  ## save
  for(x in 1:2){
    to_save <- rbind(distance_sum_gen[[gen=x]],
                     numbers_gen[[x]],
                     rates[[x]],
                     ped_numbers_gen[[x]],
                     rates_ped[[x]])
    to_save <- cbind(to_save,c(rowSums(distance_sum_gen[[gen=x]]),
                               rowSums(numbers_gen[[x]]),
                               rowSums(numbers_gen[[x]])/rowSums(distance_sum_gen[[gen=x]]),
                               rowSums(ped_numbers_gen[[x]]),
                               rowSums(ped_numbers_gen[[x]])/rowSums(distance_sum_gen[[gen=x]])))
    colnames(to_save)[5] <- 'total'
    #print(to_save)
    write.csv(to_save,paste0('outputs/',sev,'_summaries_',c('F','M')[x],'.csv'))
  }
  
  na_numbers <- rbind(
    sapply(roads,function(x)sapply(modes,function(y)sum(subset(test_data_str,strike_mode==y&roadtype==x&cas_severity==sev&is.na(strike_male))$count))),
    sapply(roads,function(x)sapply(modes,function(y)sum(subset(test_data_str,cas_mode=='pedestrian'&strike_mode==y&roadtype==x&cas_severity==sev&is.na(strike_male))$count))),
    sapply(roads,function(x)sapply(c("NOV","other or unknown","pedestrian"),function(y)sum(subset(test_data_str,strike_mode==y&roadtype==x&cas_severity==sev)$count))),
    sapply(roads,function(x)sapply(c("NOV","other or unknown","pedestrian"),function(y)sum(subset(test_data_str,cas_mode=='pedestrian'&strike_mode==y&roadtype==x&cas_severity==sev)$count)))
  )
  na_numbers <- cbind(na_numbers,rowSums(na_numbers))
  colnames(na_numbers)[5] <- 'total'
  write.csv(na_numbers,paste0('outputs/',sev,'_summaries_NA.csv'))
}

##### model
test_data_str <- group_by(test_data_str_raw, cas_severity, roadtype, strike_mode, cas_mode, urban_or_rural_area) %>% 
  summarise(count=dplyr::n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_severity, roadtype, strike_mode, cas_mode, urban_or_rural_area, fill=list(count=0)) %>%
  arrange(cas_severity, roadtype, strike_mode)  

test_data_str$roadtype[test_data_str$roadtype=='Motorway/A(M)'] <- 'M'
test_data_str$roadtype[test_data_str$roadtype=='A'&test_data_str$urban_or_rural_area==2] <- 'rural_A'
test_data_str$roadtype[test_data_str$roadtype=='A'&test_data_str$urban_or_rural_area==1] <- 'urban_A'
test_data_str$roadtype[test_data_str$roadtype=='B, C, Unclassified'&test_data_str$urban_or_rural_area==2] <- 'rural_B'
test_data_str$roadtype[test_data_str$roadtype=='B, C, Unclassified'&test_data_str$urban_or_rural_area==1] <- 'urban_B'

test_data_str <- subset(test_data_str,urban_or_rural_area%in%1:2&!strike_mode%in%c('NOV','other or unknown','pedestrian')&roadtype!='M')
test_data_str <- test_data_str[complete.cases(test_data_str),]

distance_sum_sum <- distance_sum#[[1]]+distance_sum[[2]]
test_data_str$strike_distance <- apply(test_data_str,1,function(x){
  distance_sum_sum[which(dimnames(distance_sum_sum)[[1]]==as.character(x[3])),which(dimnames(distance_sum_sum)[[2]]==as.character(x[2]))]})


mod1 <- glm.nb(count~(cas_severity+roadtype+cas_mode+strike_mode)^3-cas_mode:strike_mode:cas_severity-cas_mode:strike_mode:roadtype+offset(log(strike_distance)),
            data=test_data_str)
AIC(mod1)
mod1 <- glm(count~(cas_severity+roadtype+cas_mode+strike_mode)^3-cas_mode:strike_mode:cas_severity,
            family=poisson(),
            data=test_data_str,offset=log(strike_distance))
AIC(mod1)
test_data_str$pred <- fitted.values(mod1)
test_data_str$se <- predict(mod1,se.fit=T)[[2]]
fatal <- subset(test_data_str,cas_severity=='Fatal')
sapply(modes,function(x){ # residual
  sub_data <- subset(fatal,strike_mode==x)
  Y <- sub_data$count
  mu <- sub_data$pred
  ylogy <- Y*log(Y/mu)
  ylogy[is.na(ylogy)] <- 0
  D <- 2*sum(ylogy-(Y-mu))
  D
})
sapply(modes,function(x){ # deviance
  sub_data <- subset(fatal,strike_mode==x)
  Y <- sub_data$count
  mu <- sub_data$pred
  D <- sum((Y-mu)^2/mu)
  D
})
sapply(modes,function(x){
  sub_data <- subset(test_data_str,strike_mode==x)
  Y <- sub_data$count
  mu <- sub_data$pred
  ylogy <- Y*log(Y/mu)
  ylogy[is.na(ylogy)] <- 0
  D <- 2*sum(ylogy-(Y-mu))
  D
})
sapply(modes,function(x){
  sub_data <- subset(test_data_str,strike_mode==x)
  Y <- sub_data$count
  mu <- sub_data$pred
  D <- sum((Y-mu)^2/mu)
  D
})
plot(fatal$count,fatal$pred,xlim=c(0,10),ylim=c(0,10))
points(subset(fatal,strike_mode=='cyclist')$count,subset(fatal,strike_mode=='cyclist')$pred,col='red')
subset(fatal,count>500)

pred <- predict(mod1,se.fit = T)
test_data_str$pred_mean <- pred[[1]]
test_data_str$pred_se <- pred[[2]]
test_data_str$count_pred <- rpois(nrow(test_data_str),exp(test_data_str$pred_mean))
#mod1 <- glm(count_pred~(cas_severity+roadtype+cas_mode+strike_mode)^3-cas_mode:strike_mode:cas_severity,
#            family=poisson(),
#            data=test_data_str,offset=log(strike_distance))

plot(test_data_str$pred_mean,model.matrix(mod1)%*%mod1$coefficients+mod1$model$`(offset)`)
sum(exp(subset(test_data_str,cas_severity=='Fatal'&roadtype=='rural_A'&strike_mode=='car/taxi')$pred_mean))/sum(unique(subset(test_data_str,roadtype=='rural_A'&strike_mode=='car/taxi')$strike_distance))
sub_data <- subset(test_data_str,cas_severity=='Fatal')
for(j in modes) print(sapply(roads,function(x)sum(subset(sub_data,roadtype==x&strike_mode==j)$count)/distance_sum_sum[j,x]))
for(j in modes) print(sapply(roads,function(x)sum(exp(subset(sub_data,roadtype==x&strike_mode==j)$pred_mean))/distance_sum_sum[j,x]))

mod_mat <- model.matrix(mod1)
coef_param <- as.data.frame(coef(summary(mod1)))
coef_param[coef_param[,2]>1,]
distances <- mod1$model$`(offset)`
## use std error as std dv
samples <- 1000
quant_range <- c(0.05,0.95)
for(sev in severity_levels){
  table3 <- list()
  for(j in modes){
    table3[[j]] <- matrix(0,ncol=5,nrow=samples)
    colnames(table3[[j]]) <- c(roads,'all')
  }
  for(i in 1:samples){
    parameters <- t(rmvnorm(1,coef_param[,1],vcov(mod1)))
    test_data_str$pred <- exp(mod_mat %*% parameters + distances)
    sub_data <- test_data_str[test_data_str$cas_severity==sev,]
    for(j in modes) table3[[j]][i,1:4] <- sapply(roads,function(x)sum(sub_data[sub_data$roadtype==x&sub_data$strike_mode==j,]$pred)/
                                                   distance_sum_sum[j,x])
    for(j in modes) table3[[j]][i,5] <- sum(sub_data[sub_data$strike_mode==j,]$pred)/sum(distance_sum_sum[j,])
  }
  
  saved_file <- read.csv(paste0('outputs/',sev,'_summaries.csv'))
  dist_raw <- saved_file[1:6,2:6]
  table4 <- lapply(1:length(table3),function(x)(table3[[x]][,5]*dist_raw[x,5]))
  mn_raw <- sapply(table4,mean)
  std_raw <- sapply(table4,sd)
  cov_raw <- std_raw/mn_raw^2
  print(std_raw^2/mn_raw)
  
  mn <- t(sapply(table3,function(x)apply(x,2,mean)))
  mdn <- t(sapply(table3,function(x)apply(x,2,median)))
  std <- t(sapply(table3,function(x)apply(x,2,sd)))
  lower <- t(sapply(table3,function(x)apply(x,2,quantile,quant_range[1])))
  upper <- t(sapply(table3,function(x)apply(x,2,quantile,quant_range[2])))
  print(std[,5]^2/mn[,5])
  
  mdnraw <- mdn
  mdnraw <- (saved_file[7:12,2:6])/(saved_file[1:6,2:6])
  roadnames <- c('Rural major','Urban major','Rural minor','Urban minor')
  modenames <- modes
  modenames[6] <- 'cycle'
  hlines <- c(5,10,15,20,25)
  if(sev=='Serious') hlines <- hlines*8
  if(sev=='Slight') hlines <- hlines*50
  ypos <- 21
  if(sev=='Serious') ypos <- ypos*6
  if(sev=='Slight') ypos <- ypos*42.5
  {pdf(paste0('outputs/',sev,'_rate_ranges.pdf'))
    #x11(width=8); 
    par(mfrow=c(2,2),mar=c(7,5,4,1))
    for(i in 1:length(roads)){
      #plot(1:length(modes),(mdn[,i]),pch=15,cex=1,col='navyblue',ylim=c(min(lower[,i]),max(upper[,i])),main=roads[i],frame=F,
      #     cex.axis=1.5,cex.lab=1.5,xlab='',ylab='Deaths per bn km',xaxt='n',log='y')
      plot(1:length(modes),1:length(modes),col='white',ylim=c(min(lower[,i]),max(upper[,i])),main=roadnames[i],frame=F,
           cex.axis=1.5,cex.lab=1.5,xlab='',ylab=ifelse(i%in%c(2,4),'',paste0(sev,' injuries per bn km')),xaxt='n',las=2)
      for(j in hlines) abline(h=j,col='grey',lwd=2,lty=2) # 
      points(1:length(modes),(mdnraw[,i]),pch=15,cex=0.5,col='darkorange')
      axis(1,labels=modenames,at=1:length(modes),las=2)
      for(j in 1:length(modes)) lines(c(j-0.1,j+0.1),(c(mdnraw[j,i],mdnraw[j,i])),col='darkorange',lwd=3)
      for(j in 1:length(modes)) lines(c(j,j),(c(lower[j,i],upper[j,i])),col='navyblue',lwd=3)
    }
    legend(col=c('darkorange','navyblue'),legend=c('Data','Model'),lwd=2,bty='n',x=0.65,y=ypos)
    dev.off()
  }
  
  xlsx::write.xlsx(mn,file=paste0('outputs/',sev,'_rate_ranges.xlsx'),sheetName = 'mean') 
  xlsx::write.xlsx(mdn,file=paste0('outputs/',sev,'_rate_ranges.xlsx'),sheetName = 'median',append = T) 
  xlsx::write.xlsx(lower,file=paste0('outputs/',sev,'_rate_ranges.xlsx'),sheetName = paste0('lower ',quant_range[1]*100),append = T) 
  xlsx::write.xlsx(upper,file=paste0('outputs/',sev,'_rate_ranges.xlsx'),sheetName = paste0('upper ',quant_range[2]*100),append = T) 
}

cas_modes <- unique(test_data_str$cas_mode)
table3 <- list()
for(j in cas_modes){
  table3[[j]] <- matrix(0,ncol=5,nrow=samples)
  colnames(table3[[j]]) <- c(roads,'all')
}
for(test_mode in c(1,4,6)){
  for(i in 1:samples){
    parameters <- t(rmvnorm(1,coef_param[,1],vcov(mod1)))
    test_data_str$pred <- exp(mod_mat %*% parameters + distances)
    sub_data <- subset(test_data_str,cas_severity=='Fatal')
    for(j in cas_modes) table3[[j]][i,1:4] <- sapply(roads,function(x)sum(subset(sub_data,roadtype==x&strike_mode==modes[test_mode]&cas_mode==j)$pred)/
                                                       distance_sum[test_mode,x])
    for(j in cas_modes) table3[[j]][i,5] <- sum(subset(sub_data,strike_mode==modes[test_mode])$pred)/sum(distance_sum[test_mode,])
  }
  
  mn <- t(sapply(table3,function(x)apply(x,2,mean)))
  mdn <- t(sapply(table3,function(x)apply(x,2,median)))
  lower <- t(sapply(table3,function(x)apply(x,2,quantile,quant_range[1])))
  upper <- t(sapply(table3,function(x)apply(x,2,quantile,quant_range[2])))
  
  mdnraw <- mdn
  mdnraw <- sapply(roads,
                   function(x)sapply(cas_modes,
                                     function(y)sum(subset(test_data_str,cas_severity=='Fatal'&roadtype==x&cas_mode==y&strike_mode==modes[test_mode])$count))/
                     (dist_raw[test_mode,which(colnames(dist_raw)==x)]))
  x11()#{pdf('rate_ranges_cyc.pdf'); 
  par(mfrow=c(2,2),mar=c(7,5,4,1))
  for(i in 1:length(roads)){
    mdn_vals <- mdnraw[mdnraw[,i]>0,i]
    plot(1:length(cas_modes),(mdnraw[,i]),pch=15,cex=0.5,col='darkorange',ylim=c(min(c(lower[,i],mdn_vals)),max(c(upper[,i],mdn_vals))),main=roads[i],frame=F,
         cex.axis=1.5,cex.lab=1.5,xlab='',ylab='Deaths per bn km',xaxt='n',log='y')
    axis(1,labels=cas_modes,at=1:length(cas_modes),las=2)
    for(j in 1:length(cas_modes)) lines(c(j-0.1,j+0.1),(c(mdnraw[j,i],mdnraw[j,i])),col='darkorange',lwd=3)
    for(j in 1:length(cas_modes)) lines(c(j,j),(c(lower[j,i],upper[j,i])),col='navyblue',lwd=3)
  }
  legend(col=c('darkorange','navyblue'),legend=c('Data','Model'),lwd=2,bty='n',x=0.65,y=30)
  # dev.off()}
  
}


##### model gender
test_data_str <- group_by(test_data_str_raw, cas_severity, roadtype, strike_mode, cas_mode, urban_or_rural_area,strike_male) %>% 
  summarise(count=dplyr::n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_severity, roadtype, strike_mode, cas_mode, urban_or_rural_area,strike_male, fill=list(count=0)) %>%
  arrange(cas_severity, roadtype, strike_mode)  

test_data_str$roadtype[test_data_str$roadtype=='Motorway/A(M)'] <- 'M'
test_data_str$roadtype[test_data_str$roadtype=='A'&test_data_str$urban_or_rural_area==2] <- 'rural_A'
test_data_str$roadtype[test_data_str$roadtype=='A'&test_data_str$urban_or_rural_area==1] <- 'urban_A'
test_data_str$roadtype[test_data_str$roadtype=='B, C, Unclassified'&test_data_str$urban_or_rural_area==2] <- 'rural_B'
test_data_str$roadtype[test_data_str$roadtype=='B, C, Unclassified'&test_data_str$urban_or_rural_area==1] <- 'urban_B'

test_data_str <- subset(test_data_str,urban_or_rural_area%in%1:2&!strike_mode%in%c('NOV','other or unknown','pedestrian')&roadtype!='M')
test_data_str <- test_data_str[complete.cases(test_data_str),]

test_data_str$strike_distance <- apply(test_data_str,1,function(x){
  distance_sum_gen[which(dimnames(distance_sum_gen)[[1]]==c('f','m')[as.numeric(x[6])+1]),which(dimnames(distance_sum_gen)[[2]]==as.character(x[3])),which(dimnames(distance_sum_gen)[[3]]==as.character(x[2]))]})

#test_data_str <- subset(test_data_str,!cas_mode%in%c('bus','heavy goods','light goods'))
mod1 <- glm(count~(cas_severity+roadtype+cas_mode+strike_mode+strike_male)^3-cas_mode:strike_mode:cas_severity,
            family=poisson(),
            data=test_data_str,offset=log(strike_distance))
AIC(mod1)
test_data_str$pred <- fitted.values(mod1)
test_data_str$se <- predict(mod1,se.fit=T)[[2]]
fatal <- subset(test_data_str,cas_severity=='Fatal')
sapply(modes,function(x){
  sub_data <- subset(fatal,strike_mode==x)
  Y <- sub_data$count
  mu <- sub_data$pred
  ylogy <- Y*log(Y/mu)
  ylogy[is.na(ylogy)] <- 0
  D <- 2*sum(ylogy-(Y-mu))
  D
})
sapply(modes,function(x){
  sub_data <- subset(fatal,strike_mode==x)
  Y <- sub_data$count
  mu <- sub_data$pred
  D <- sum((Y-mu)^2/mu)
  D
})
sapply(modes,function(x){
  sub_data <- subset(test_data_str,strike_mode==x)
  Y <- sub_data$count
  mu <- sub_data$pred
  ylogy <- Y*log(Y/mu)
  ylogy[is.na(ylogy)] <- 0
  D <- 2*sum(ylogy-(Y-mu))
  D
})
sapply(modes,function(x){
  sub_data <- subset(test_data_str,strike_mode==x)
  Y <- sub_data$count
  mu <- sub_data$pred
  D <- sum((Y-mu)^2/mu)
  D
})
plot(fatal$count,fatal$pred,xlim=c(0,10),ylim=c(0,10))
points(subset(fatal,strike_mode=='cyclist')$count,subset(fatal,strike_mode=='cyclist')$pred,col='red')
subset(fatal,count>500)

pred <- predict(mod1,se.fit = T)
test_data_str$pred_mean <- pred[[1]]
test_data_str$pred_se <- pred[[2]]
test_data_str$count_pred <- rpois(nrow(test_data_str),exp(test_data_str$pred_mean))
#mod1 <- glm(count_pred~(cas_severity+roadtype+cas_mode+strike_mode)^3-cas_mode:strike_mode:cas_severity,
#            family=poisson(),
#            data=test_data_str,offset=log(strike_distance))

plot(test_data_str$pred_mean,model.matrix(mod1)%*%mod1$coefficients+mod1$model$`(offset)`)
sum(exp(subset(test_data_str,cas_severity=='Fatal'&roadtype=='rural_A'&strike_mode=='car/taxi')$pred_mean))/sum(unique(subset(test_data_str,roadtype=='rural_A'&strike_mode=='car/taxi')$strike_distance))
sub_data <- subset(test_data_str,cas_severity=='Fatal')
for(j in modes) print(sapply(roads,function(x)sum(subset(sub_data,roadtype==x&strike_mode==j)$count)/distance_sum_sum[j,x]))
for(j in modes) print(sapply(roads,function(x)sum(exp(subset(sub_data,roadtype==x&strike_mode==j)$pred_mean))/distance_sum_sum[j,x]))

mod_mat <- model.matrix(mod1)
coef_param <- as.data.frame(coef(summary(mod1)))
coef_param[coef_param[,2]>1,]
distances <- mod1$model$`(offset)`
## use std error as std dv
samples <- 1000
for(sev in severity_levels){
  val_list <- list()
  for(gen_ind in 0:1){
    gen <- c('F','M')[gen_ind+1]
    table3 <- list()
    for(j in modes){
      table3[[j]] <- matrix(0,ncol=5,nrow=samples)
      colnames(table3[[j]]) <- c(roads,'all')
    }
    for(i in 1:samples){
      parameters <- t(rmvnorm(1,coef_param[,1],vcov(mod1)))
      test_data_str$pred <- exp(mod_mat %*% parameters + distances)
      sub_data <- test_data_str[test_data_str$strike_male==gen_ind&test_data_str$cas_severity==sev,]
      for(j in modes) table3[[j]][i,1:4] <- sapply(roads,function(x)sum(sub_data[sub_data$roadtype==x&sub_data$strike_mode==j,]$pred)/
                                                     distance_sum_gen[gen_ind+1,j,x])
      for(j in modes) table3[[j]][i,5] <- sum(sub_data[sub_data$strike_mode==j,]$pred)/sum(distance_sum_gen[gen_ind+1,j,])
    }
    
    saved_file <- read.csv(paste0('outputs/',sev,'_summaries_',gen,'.csv'))
    dist_raw <- saved_file[1:6,2:6]
    table4 <- lapply(1:length(table3),function(x)(table3[[x]][,5]*dist_raw[x,5]))
    mn_raw <- sapply(table4,mean)
    std_raw <- sapply(table4,sd)
    cov_raw <- std_raw/mn_raw^2
    print(std_raw^2/mn_raw)
    
    mn <- t(sapply(table3,function(x)apply(x,2,mean)))
    mdn <- t(sapply(table3,function(x)apply(x,2,median)))
    std <- t(sapply(table3,function(x)apply(x,2,sd)))
    lower <- t(sapply(table3,function(x)apply(x,2,quantile,quant_range[1])))
    upper <- t(sapply(table3,function(x)apply(x,2,quantile,quant_range[2])))
    print(std[,5]^2/mn[,5])
    
    mdnraw <- mdn
    mdnraw <- (saved_file[7:12,2:6])/(saved_file[1:6,2:6])
    
    val_list[[gen_ind+1]] <- list(lower,upper,mdnraw)
    xlsx::write.xlsx(mn,file=paste0('outputs/',sev,'_rate_ranges_',gen,'.xlsx'),sheetName = 'mean') 
    xlsx::write.xlsx(mdn,file=paste0('outputs/',sev,'_rate_ranges_',gen,'.xlsx'),sheetName = 'median',append = T) 
    xlsx::write.xlsx(lower,file=paste0('outputs/',sev,'_rate_ranges_',gen,'.xlsx'),sheetName = paste0('lower ',quant_range[1]*100),append = T) 
    xlsx::write.xlsx(upper,file=paste0('outputs/',sev,'_rate_ranges_',gen,'.xlsx'),sheetName = paste0('upper ',quant_range[2]*100),append = T) 
    
  }
  
  roadnames <- c('Rural major','Urban major','Rural minor','Urban minor')
  col1 <- c('turquoise','navyblue')
  col2 <- c('darkorange','hotpink')
  {pdf(paste0('outputs/',sev,'_rate_ranges_gen.pdf'))
    #x11(width=8); 
    par(mfrow=c(2,2),mar=c(7,5,4,1))
    for(i in 1:length(roads)){
      ylim <- c(min(c(val_list[[1]][[1]][,i],val_list[[2]][[1]][,i])),
                max(c(val_list[[1]][[2]][,i],val_list[[2]][[2]][,i])))
      for(gen_ind in 1:0){
        lower <- val_list[[gen_ind+1]][[1]]
        upper <- val_list[[gen_ind+1]][[2]]
        mdnraw <- val_list[[gen_ind+1]][[3]]
        gen <- c('F','M')[gen_ind+1]
        if(gen_ind==1){
          plot(1:length(modes),1:length(modes),col='white',xlim=c(0.75,length(modes)),ylim=ylim,main=roadnames[i],frame=F,
               cex.axis=1.5,cex.lab=1.5,xlab='',ylab=ifelse(i%in%c(2,4),'',paste0(sev,' injuries per bn km')),xaxt='n',las=2)
        }
        hlines <- c(5,10,15,20,25)
        if(sev=='Serious') hlines <- hlines*5
        if(sev=='Slight') hlines <- hlines*50
        for(j in hlines) abline(h=j,col='grey',lwd=1,lty=2) # 0.1,0.2,0.5,1,2,5,10,20
        points(1:length(modes)-gen_ind/5,(mdnraw[,i]),pch=15,cex=0.5,col=col2[gen_ind+1])
        axis(1,labels=modenames,at=1:length(modes)-0.1,las=2)
        for(j in 1:length(modes)) lines(c(j-0.1,j+0.1)-gen_ind/5,(c(mdnraw[j,i],mdnraw[j,i])),col=col2[gen_ind+1],lwd=3)
        for(j in 1:length(modes)) lines(c(j,j)-gen_ind/5,(c(lower[j,i],upper[j,i])),col=col1[gen_ind+1],lwd=3)
      }
    }
    ypos <- 16.25
    if(sev=='Serious') ypos <- ypos*8
    if(sev=='Slight') ypos <- ypos*52.5
    legend(col=col1,legend=c('',''),lwd=2,bty='n',x=0.5,y=ypos)
    legend(col=col2,legend=c('F','M'),lwd=2,bty='n',x=1.2,y=ypos,x.intersp=0.5)
    text('Model Data',x=0.55,y=ypos-0.55,pos=4,cex=0.8)
    dev.off()
  }
}
