# R code for mortality estimate in rural Bangladesh.
# Code author: Yuling Yao and others.
# Dated: Apr 2021. 
# This file is licensed under the MIT License.
# For detailed model description, please refer to the preprint.

library(foreign)
library(readstata13)
library(rstan)
library(ggplot2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd("~/Desktop/mortality")

# read data
dta <- read.dta13("~/Desktop/listing+round1+round2_indivual_level_data_24042021.dta")
# some rows are duplicated. Alternatively, we could remove them by hand (not run).
# duplicate_id=which( dta$dd19_verified_r1==0 |  dta$dd20_verified_r1==0 )
# cd=dta[-duplicate_id, ]
# cd=cd[-which(cd$dd20_verified_r2_==0 & is.na(cd$dd20_verified_r1)  ), ]  
cd=dta[-which(dta$duplicate_rows==1),]

#cd: the Complete Dataset, only run on survey respondents who gave consent 
cd=cd[which(cd$survey_done_r1=="Complete"), ] 


# remove in-migrations. 
cd=cd[- which (cd$enlisted_period=="R1 May2020: In-migrants" |cd$enlisted_period=="R2 Oct2020: In-migrants"  ) ,]

# death cases
case= (cd$dd19_verified_r1 ==1 | cd$dd20_verified_r1 ==1 |cd$dd20_verified_r2_==1)
case[is.na(case)]=0
sum(case)  # case size
nrow(cd) # population size


# check the number:
length( which (cd$survey_done_r2=="Complete" & cd$dd20_verified_r1==1 ))  
length( which (cd$survey_done_r2=="Complete" & cd$dd20_verified_r2_==1 )) 
length( which (cd$survey_done_r2=="Complete" & cd$dd19_verified_r1==1 ))  
length( which (cd$survey_done_r2=="Complete" & cd$dd19_verified_r2==1 )) 




## aggregrate age into multilevel cells:  
for(i in  which(is.na(cd$age_))){
	for(char in c("dd19_age_r1", "dd20_age_r1", "dd20_age_r2")){
		if(!is.na(cd[i,char] )){
			cd[i,"age_"]=cd[i,char]
			break
		}
	}
}
length(which(is.na(cd$age_))) 


for(i in  which(is.na(cd$sex_))){
	for(char in c("dd19_sex_r1", "dd20_sex_r1", "dd20_sex_r2")){
		if(!is.na(cd[i,char] )){
			cd[i,"sex_"]=cd[i,char]
			break
		}
	}
}
length(which(is.na(cd$sex_)))


for(i in  which(is.na(cd$sex_))){
	for(char in c("dd19_sex_r1", "dd20_sex_r1", "dd20_sex_r2")){
		if(!is.na(cd[i,char] )){
			cd[i,"sex_"]=cd[i,char]
			break
		}
	}
}
length(which(is.na(cd$sex_)))

# edu=1 if the respondent had any education.
edu=cd$edu_!="None"

 
# case=1 if the respondent died 
case= (cd$dd19_verified_r1 ==1 | cd$dd20_verified_r1 ==1 |cd$dd20_verified_r2_==1)
case[is.na(case)]=0

sum(case)
#get all death cases (=796)
t1=as.Date(cd$dd19_month_r1)
for(i in  which(cd$dd20_verified_r1 ==1 |cd$dd20_verified_r2_==1)){
	for(j in 1:2){
		char = c("dd20_verified_r1", "dd20_verified_r2_")[j]
		char2 = c("dd20_month_r1", "dd20_month_r2")[j]
		if(!is.na(cd[i,char]) &cd[i,char]==1 ){
			t1[i]=as.Date(cd[i, char2])
			break
		}
	}
}
 




#get all death cases reasons
cause=as.character( cd$dd19_cause_r1)
for(i in  which(cd$dd20_verified_r1 ==1 |cd$dd20_verified_r2_==1)){
	for(j in 1:2){
		char = c("dd20_verified_r1", "dd20_verified_r2_")[j]
		char2 = c("dd20_cause_r1", "dd20_cause_r2")[j]
		if(!is.na(cd[i,char]) &cd[i,char]==1 ){
			cause[i]=as.character( cd[i, char2])
			break
		}
	}
}
for(i in  which(cause=="Other"))
{
	if(grepl("Natural" , paste(cd$dd19_cause_specify_r1[i], cd$dd20_cause_specify_r1[i], cd$dd20_cause_specify_r2[i])))
		cause[i]="Natural"
}



#the month and day of death counts 
t1_m= as.numeric(format(t1, "%m"))
t1_d= as.numeric( format(t1, "%d"))
 
# if the person dropped out the survey
dropout=is.na(cd$survey_done_r2)
# age and gender
age=cd$age_
male=as.numeric( cd$sex_=="Male")

# explore data by a sequence of regressions
summary(glm(case~age+male+edu, family = binomial(link = "logit")))
summary(glm(case~age+male+edu_family, family = binomial(link = "logit")))
summary(glm(dropout~age+male+edu_family, family = binomial(link = "logit")))


# the family-wise education 
edu_family=rep(NA, nrow(cd))
family_size=rep(NA, nrow(cd))
for(i in 1:nrow(cd)){
	if(is.na(edu_family[i])){
		fid=which(cd$caseid==cd$caseid[i])
		family_size[fid]=length(fid)
		id2=which(age[fid]>=10)
		if(mean(edu[id2], na.rm = TRUE)>0.5)
			edu_family[fid]=1
		else
			edu_family[fid]=0
	}
}



# divide data into groups by age, gender, education, and month 
#age: 0:8
#gender: 0:1
#edu_family: 0:1	
#month
# case and exposure

date_vec=seq(as.Date("2019/1/1"), as.Date("2020/12/1"), by = "month")
age_vec=seq( 0,90,by = 10)
age_capped=ifelse(age<80,age, 80)
exposure=array(NA, c(9,2,2,23))
case_count=array(NA, c(9,2,2,23))  # total =771
for( id_age in 1:9)
	for(id_gender in 1:2)
		for(id_edu in 1:2)
			for(id_month in 1:23)
			{
				case_count[id_age, id_gender, id_edu,id_month]=length(which( 
					!is.na(t1) & t1>= date_vec[id_month] & t1< date_vec[id_month+1] & 
						age_capped>=age_vec[id_age] & age_capped<age_vec[id_age+1]  &
						edu_family==id_edu-1 & male==id_gender-1)  )
				exposure[id_age, id_gender, id_edu,id_month]=length(which( 
					(age_capped>=age_vec[id_age] & age_capped<age_vec[id_age+1]  &
					 	edu_family==id_edu-1 & male==id_gender-1) &
						(is.na(t1)	|  !is.na(t1)&  t1>= date_vec[id_month]) &
						(
							(dropout==0 & cd$fielddate_r2 >=date_vec[id_month+1]) |
								(dropout==1 & cd$fielddate_r1 >=date_vec[id_month+1])
						)
				))
			}
sum(case_count)

 mean((apply(case_count, 4, sum) /apply(exposure , 4, sum))[1:22] )*12*1000



 



#graph
pdf("~/desktop/month_count.pdf", width=2.3, height=2.3)
par(mfrow=c(1,1),oma=c(1,1,1,0),mar=c(1,1,1,0) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.7, cex.lab=0.6, cex.main=0.9)
hist(t1, breaks =date_vec-1, ylim=c(0,70),freq=T, yaxs='i', xaxs='i', axes = FALSE, main="", xlab="", ylab="", col="#B97C7C", border = "#A25050")
abline(h=c(20, 40, 60), col='gray60', lty=2)
axis(1, lwd=0.5,  at=date_vec[seq(1,23,by=3)], labels =data_lab , padj=-0.8 )
axis(2, lwd=0.5,at=c(0,20,40,60),  las=2)
mtext(1, line=0.5, text="month",cex=0.7)
mtext(3, line=1, text="surveyed  death cases by month" ,cex=0.7)
mtext(2, line=1.2, text="case count" ,cex=0.7)
box(lwd=0.5, bty='l')
dev.off()





### basic visualization without model
pdf("~/desktop/day_count.pdf", width=2.5, height=2)
par(mfrow=c(1,1),oma=c(1,1,1,0),mar=c(1,1,1,1) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.7, cex.lab=0.6, cex.main=0.9)
hist(t1_d, breaks = c(0:31)+0.5, ylim=c(0,65),xlim=c(0.5,31.5), yaxs='i', xaxs='i', axes = FALSE, main="", xlab="", ylab="", col="#B97C7C", border = "#A25050")
abline(h=c(20, 40, 60), col='gray60', lty=2)
abline(v=c(15, 10, 5), col=1, lty=2)
axis(1, lwd=0.5, at=c(1,5, 10,15,20,30), padj=-0.8)
axis(2, lwd=0.5,at=c(0,20,40,60),  las=2)
mtext(1, line=0.5, text="day of month",cex=0.7)
mtext(3, line=1, text="'rounding error' of reported counts" ,cex=0.7)
mtext(2, line=1.2, text="case count" ,cex=0.7)
box(lwd=0.5, bty='l')
dev.off()

pdf("~/desktop/day_count_2R.pdf", width=5, height=2)
par(mfrow=c(1,2),oma=c(1,1,1,0),mar=c(1,1,1,0.5) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.7, cex.lab=0.6, cex.main=0.9)
hist(t1_d[-which(cd$dd20_verified_r2_==1)], breaks = c(0:31)+0.5, ylim=c(0,60),xlim=c(0.5,31.5), yaxs='i', xaxs='i', axes = FALSE, main="", xlab="", ylab="", col="#B97C7C", border = "#A25050")
abline(h=c(20, 40, 60), col='gray60', lty=2)
abline(v=c(15, 10, 5), col=1, lty=2)
axis(1, lwd=0.5, at=c(1,5, 10,15,20,30), padj=-0.8)
axis(2, lwd=0.5,at=c(0,20,40,60),  las=2)
mtext(1, line=0.5, text="day of month",cex=0.7)
mtext(3, line=0.5, text="Round 1 survey" ,cex=0.7)
mtext(2, line=1.2, text="case count" ,cex=0.7)
box(lwd=0.5, bty='l')
hist(t1_d[cd$dd20_verified_r2_==1], breaks = c(0:31)+0.5, ylim=c(0,15),xlim=c(0.5,31.5), yaxs='i', xaxs='i', axes = FALSE, main="", xlab="", ylab="", col="#B97C7C", border = "#A25050")
abline(h=c(5, 10, 15), col='gray60', lty=2)
abline(v=c(15, 10, 5), col=1, lty=2)
axis(1, lwd=0.5, at=c(1,5, 10,15,20,30), padj=-0.8)
axis(2, lwd=0.5,at=c(0,5,10,15),  las=2)
mtext(1, line=0.5, text="day of month",cex=0.7)
mtext(3, line=0.5, text="Round 2 survey" ,cex=0.7)
box(lwd=0.5, bty='l')
dev.off()

 
library(ggplot2)
data_lab=seq(1,23,by=3) %%12
data_lab[1]="19/1"
data_lab[5]="20/1"

pdf("~/desktop/case_count_adj3.pdf", width=3.3, height=3)
par(mfrow=c(2,1),oma=c(0,1.5,0.5,0),mar=c(2,1.5,1,0.5) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.7, cex.lab=0.6, cex.main=0.9)


rate_est=(apply(case_count, 4, sum) /apply(exposure , 4, sum))[1:22]
size_est=(2*apply(exposure , 4, sum)/ mean(apply(exposure , 4, sum))) [1:22]

plot(date_vec[1:22], pch=19, col=alpha("darkred", alpha=0.6), xpd=T, y=rate_est,  xaxs='i', axes = FALSE, main="", xlab="", ylab="",  cex=size_est,  yaxs='i', xaxs='i', ylim=c(0,1.05e-3) )
lines(date_vec[1:22],  y=rate_est, col="darkred", lwd=0.9)
axis(1, lwd=0.5,  at=date_vec[seq(1,23,by=3)], labels =data_lab , padj=-0.8 )
axis(2, lwd=0.5, at=c(0, 5e-4,1e-3), labels = c("0","0.5", "1"),  las=2)
abline(h=c(2.5e-4,5e-4,7.5e-4), col='gray60', lty=2)
abline(v=date_vec[seq(1,23,by=3)], col='gray60', lty=2)
mtext(3, line=0.5, text="surveyed monthly mortality rate, all" ,cex=0.7)
mtext(2, line=0.2, text=expression(10^{-3}) ,cex=0.7, las=2, padj = -3.5)
box(lwd=0.5, bty='l')
legend("topright", pch=19, col=alpha("grey", alpha=0.6),xpd=T,pt.cex=(c(150000,100000) / mean(apply(exposure , 4, sum))) ,cex=0.5,legend=c("size= 75k", "size= 50k"),  box.lwd =0,  bty = 'n')


rate_est=(apply(case_count[6:9,1,,], 3, sum) /apply(exposure[6:9,1,,], 3, sum))[1:22]
size_est=(10*apply(exposure[6:9,1,,], 3, sum)/ mean(apply(exposure, 4, sum))) [1:22]
plot(date_vec[1:22], pch=19, col=alpha("darkorange", alpha=0.6), xpd=T, y=rate_est,  xaxs='i', axes = FALSE, main="", xlab="", ylab="",  cex=size_est,  yaxs='i', xaxs='i', ylim=c(0,5e-3) )
lines(date_vec[1:22],  y=rate_est, col="darkorange", lwd=0.9)


rate_est=(apply(case_count[6:9,2,,], 3, sum) /apply(exposure[6:9,2,,], 3, sum))[1:22]
size_est=(5*apply(exposure[6:9,2,,], 3, sum)/ mean(apply(exposure, 4, sum))) [1:22]
points(date_vec[1:22], pch=19, col=alpha("darkblue", alpha=0.6), xpd=T, y=rate_est,   cex=size_est,  yaxs='i' )
lines(date_vec[1:22],  y=rate_est, col="darkblue", lwd=0.9, xpd=T)


axis(1, lwd=0.5,  at=date_vec[seq(1,23,by=3)], labels =data_lab , padj=-0.8 )
axis(2, lwd=0.5, at=c(0, 2e-3,4e-3), labels = c("0","2", "4"),  las=2)
abline(h=c(2e-3,1e-3,3e-3), col='gray60', lty=2)
abline(v=date_vec[seq(1,23,by=3)], col='gray60', lty=2)

mtext(1, line=0.5, text="month",cex=0.7)
mtext(3, line=0.5, text="surveyed mortality, older people (50+)" ,cex=0.7)
mtext(2, line=0.5, text=expression(10^{-3}) ,cex=0.7, las=2, padj = -3.5)
mtext(2, line=0, text="sample monthly mortality rate" ,cex=0.7, outer=T)
box(lwd=0.5, bty='l')
legend("topright", pch=19, col=alpha("grey", alpha=0.6),xpd=T,pt.cex=(c(50000,25000) / mean(apply(exposure , 4, sum))) ,cex=0.5,legend=c("size= 5k", "size= 2.5k"),  box.lwd =0,  bty = 'n')
text(x=date_vec[c(7,9)],y=c(4e-3,0.5e-3), labels = c("Male", "Female"),col=c("darkblue","darkorange" ), cex=0.7)

dev.off()







pdf("~/desktop/case_count_adj_old.pdf", width=2.4, height=2)
par(mfrow=c(1,1),oma=c(0.5,1,0.5,0),mar=c(1,1,1,0.5) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.7, cex.lab=0.6, cex.main=0.9)
rate_est=(apply(case_count[9,1,,], 2, sum) /apply(exposure[9,1,,], 2, sum))[1:22]
size_est=(200*apply(exposure[9,1,,], 2, sum)/ mean(apply(exposure, 4, sum))) [1:22]
plot(date_vec[1:22], pch=19, col=alpha("darkorange", alpha=0.6), xpd=T, y=rate_est,  xaxs='i', axes = FALSE, main="", xlab="", ylab="",  cex=size_est,  yaxs='i', xaxs='i', ylim=c(0,0.04) )
lines(date_vec[1:22],  y=rate_est, col="darkorange", lwd=0.9)


rate_est=(apply(case_count[9,2,,], 2, sum) /apply(exposure[9,2,,], 2, sum))[1:22]
size_est=(200*apply(exposure[9,2,,], 2, sum)/ mean(apply(exposure, 4, sum))) [1:22]
points(date_vec[1:22], pch=19, col=alpha("darkblue", alpha=0.6), xpd=T, y=rate_est,   cex=size_est,  yaxs='i' )
lines(date_vec[1:22],  y=rate_est, col="darkblue", lwd=0.9, xpd=T)


axis(1, lwd=0.5,  at=date_vec[seq(1,23,by=3)], labels =data_lab , padj=-0.8 )
axis(2, lwd=0.5, at=c(0, 2e-2,4e-2), labels = c("0","20", "40"),  las=2)
abline(h=c(20e-3,10e-3,30e-3), col='gray60', lty=2)
abline(v=date_vec[seq(1,23,by=3)], col='gray60', lty=2)

mtext(1, line=0.5, text="month",cex=0.7)
mtext(3, line=0.5, text="surveyed mortality, high age (80+)" ,cex=0.7)
mtext(2, line=0.7, text=expression(10^{-3}) ,cex=0.7, las=2, padj = -7)
mtext(2, line=0, text="sample monthly mortality rate" ,cex=0.7, outer=T)
box(lwd=0.5, bty='l')
legend("topright", pch=19, col=alpha("grey", alpha=0.6),xpd=T,pt.cex=(c(50000,25000) / mean(apply(exposure , 4, sum))) ,cex=0.5,legend=c("size= 250", "size= 125"),  box.lwd =0,  bty = 'n')
text(x=date_vec[c(7,18)],y=c(3.5e-2,2e-2), labels = c("Male", "Female"),col=c("darkblue","darkorange" ), cex=0.7)

dev.off()



case_count_trim=case_count[,,,-23] # data are incomplete in Nov 2020 
exposure_trim=exposure[,,,-23] 
 
# running stan model 
m1=stan(file="mortality.stan", data=list(N_age=dim(exposure_trim)[1], N_month=dim(exposure_trim)[4],  cases=case_count_trim, exposures=exposure_trim, covid_start=2), chains = 3, iter = 4000)
sss=extract(m1)  
### may see warnings. Don't panic, NA of Rhat comes from a constant parameter, it is legit!

age_vec_cap=age_vec[-10]

print(m1, pars="month_eff")
### visualize 
draw_base_rate=function(v, yl=NULL, print_label=TRUE){
plot(age_vec_cap,  colMeans(v),  axes=F,ylab="",xlab="",type='l',col="#8F2727",lwd=1.5,  xlim= c(0,82), xaxs='i', ylim=yl)
axis(2, las=2,lwd=0.5)
zz=apply(v, 2, quantile, c(0.975, 0.75, 0.25, 0.025))
points(age_vec_cap,  colMeans(v),  col="#8F2727",pch=19,cex=0.6)
polygon(x=c(age_vec_cap,rev(age_vec_cap)), y=c(zz[1,],rev(zz[4,] )  ), border=NA,col=  adjustcolor( "#B97C7C", alpha.f = 0.4), xpd=T)
polygon(x=c(age_vec_cap,rev(age_vec_cap)), y=c(zz[2,],rev(zz[3,] )  ), border=NA,col=  adjustcolor( "#B97C7C", alpha.f = 0.6), xpd=T)
axis(1, at=c(70,seq(0,80, by=20)), padj=-0.8,lwd=0.5)
box(lwd=0.5,bty='l')
if(print_label)
mtext(1, text='age', cex=0.7, line=0.6)
lines(age_vec_cap,  colMeans(v), col="#8F2727")
}


pdf("~/desktop/base_rate08.pdf", width =6.5, height=1.8)
layout(matrix(c(1:4),nrow=1), width = c(1,1,1,0.7),height = c(1))
par(oma=c(1,0.5,1,0), pty='m',mar=c(1,1,1,1) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.7, cex.lab=0.6, cex.main=0.7) 
v=sss$age_eff 
plot(age_vec_cap,  colMeans(v),  axes=F,ylab="",xlab="",type='l',col="#8F2727",lwd=1.5,  xlim= c(0,82), xaxs='i', ylim=c(-11.9,-4))	
axis(2, las=2,lwd=0.5, at=seq(-4,-12,by=-2))
zz=apply(v, 2, quantile, c(0.975, 0.75, 0.25, 0.025))
abline(h=c(-10,-8, -6),col= 1, lty=2, lwd=0.5 )
abline(v=c(20,40, 60),col= 'grey60', lty=2, lwd=0.5 )
points(age_vec_cap,  colMeans(v),  col="#8F2727",pch=19,cex=0.6)
polygon(x=c(age_vec_cap,rev(age_vec_cap)), y=c(zz[1,],rev(zz[4,] )  ), border=NA,col=  adjustcolor( "#B97C7C", alpha.f = 0.4), xpd=T)
polygon(x=c(age_vec_cap,rev(age_vec_cap)), y=c(zz[2,],rev(zz[3,] )  ), border=NA,col=  adjustcolor( "#B97C7C", alpha.f = 0.6), xpd=T)
axis(1, at=seq(0,80, by=20), padj=-0.8,lwd=0.5)
box(lwd=0.5,bty='l')
mtext(1, text='age', cex=0.7, line=0.6)
lines(age_vec_cap,  colMeans(v), col="#8F2727")


mtext(3,text= "baseline monthly mortality by age, \n log odds", cex=0.7,line=-0.5)
plot(1:12,  colMeans(sss$time_eff[,1:12]),  axes=F,ylab="",xlab="",type='l',col="#8F2727",lwd=1.5,  xlim= c(1,12), xaxs='i', ylim=c(-1.15,1.15))
axis(2, las=2,lwd=0.5 )
abline(v=c(4,7,10),col= 'grey60', lty=2, lwd=0.5 )
points(1:12,  colMeans(sss$time_eff[,1:12]),  col="#8F2727",pch=19,cex=0.6, xpd=T)
zz=apply(sss$time_eff[,1:12], 2, quantile, c(0.975, 0.75, 0.25, 0.025))
polygon(x=c(1:12,rev(1:12)), y=c(zz[1,],rev(zz[4,] )  ), border=NA,col=  adjustcolor( "#B97C7C", alpha.f = 0.4), xpd=T)
polygon(x=c(1:12,rev(1:12)), y=c(zz[2,],rev(zz[3,] )  ), border=NA,col=  adjustcolor( "#B97C7C", alpha.f = 0.6), xpd=T)
axis(1, at=c(1,4,7,10), padj=-0.8,lwd=0.5)
box(lwd=0.5,bty='l')
mtext(1, text='month', cex=0.7, line=0.6)
abline(h=c(0.5,0,-0.5),col= 1, lty=2, lwd=0.5 )
lines(1:12,  colMeans(sss$time_eff[,1:12]),  col="#8F2727")
mtext(3,text= "baseline monthly trend, \n log odds", cex=0.7,line=-0.5)


draw_base_rate(sss$covid_eff, yl=c(-1.2,1.2))	
abline(v=c(20,40,60),col= 'grey60', lty=2, lwd=0.5 )

abline(h=c(0.5,0,-0.5),col= 1, lty=2, lwd=0.5 )
mtext(3,text= "excess monthly mortality by age, \n log odds", cex=0.7,line=-0.5)




plot(c(0,0),type='n', axes=F,ylab="",xlab="",xaxs='i', xpd=T, 
		 xlim=c(0, 5.7), ylim=c(-1.15,1.15))
abline(h=c(-.5,.5,0, -1, 1), col='gray40', lty=2)
draw_coef=function(x,v){
points(x=x, y=  mean(v), pch=18, col="#8F2727", cex=1)
lines(x=c(x,x), y= as.vector(   quantile(v, c(0.975, 0.025) )), lwd=1, col="#B97C7C" )
lines(x=c(x,x), y= as.vector( quantile(v, c(0.75, 0.25) )), lwd=2.3, col="#8F2727" )
}
draw_coef(x=1, v=sss$gender_eff)
draw_coef(x=2, v=sss$edu_eff)
draw_coef(x=4.5, v=sss$covid_male)
draw_coef(x=5.5, v=sss$covid_edu)
axis(2, las=2, at=c(-1,1,-.5, .5,0))
text(x= c(1, 2.5, 4, 5), y=c(0.65, 1, 0.5, -0.5), col= 1, labels = c("male\n baseline", "edu\n baseline", "male\n excess", "edu\n excess") , cex=0.75, xpd=T)
mtext(1, text="coefficients of\n male and education", cex=0.7,line=0.5)
text( x=1.5, y=1.45, labels ="baseline", cex=0.8,line=0.5,xpd=T, col="grey30")
text( x=5, y=1.45, labels ="excess", cex=0.8,line=0.5,xpd=T, col="grey30")

dev.off()

 
 

post_prob= c(mean(male==1 & edu_family==1),  mean(male==0 & edu_family==1),  mean(male==1 & edu_family==0),  mean(male==0 & edu_family==0))

invlogit=function (x) 
{
	temp=exp(x)
	temp/(1+ temp)
}

sim_exce=sss$age_eff


multi=mean(sss$month_eff_excess)
sss$covid_eff=multi*sss$covid_eff
sss$covid_edu=multi*sss$covid_edu
sss$covid_male=multi*sss$covid_male


for(i in 1:ncol(sim_exce))
	sim_exce[,i]=
	cbind(invlogit(sss$age_eff[,i]+  sss$covid_eff[,i]+  sss$gender_eff +sss$edu_eff +sss$covid_male+ sss$covid_edu)-
					invlogit(sss$age_eff[,i]+  sss$gender_eff +sss$edu_eff),
				
				invlogit(sss$age_eff[,i]+   sss$covid_eff[,i] + sss$edu_eff+ sss$covid_edu)- 
					invlogit(sss$age_eff[,i]+  sss$edu_eff),
				invlogit(sss$age_eff[,i]+  sss$covid_eff[,i]+  sss$gender_eff + sss$covid_male)- 
					invlogit(sss$age_eff[,i]+ sss$gender_eff),
				invlogit(sss$age_eff[,i]+  sss$covid_eff[,i])- 
					invlogit(sss$age_eff[,i])) %*% post_prob

sim_exce_agg=rep(0,nrow(sim_exce))
for(i in 1:ncol(sim_exce)){
	post_prob= c(mean(male==1 & edu_family==1	&	age_capped>=age_vec[i] & age_capped<age_vec[i+1] ),  mean(male==0 & edu_family==1&	age_capped>=age_vec[i] & age_capped<age_vec[i+1]),  mean(male==1 & edu_family==0&	age_capped>=age_vec[i] & age_capped<age_vec[i+1]),  mean(male==0 & edu_family==0&	age_capped>=age_vec[i] & age_capped<age_vec[i+1]))
	sim_exce_agg=sim_exce_agg+ 	cbind(invlogit(sss$age_eff[,i]+  sss$covid_eff[,i]+  sss$gender_eff +sss$edu_eff +sss$covid_male+ sss$covid_edu)-
																			invlogit(sss$age_eff[,i]+  sss$gender_eff +sss$edu_eff),
																		invlogit(sss$age_eff[,i]+   sss$covid_eff[,i] + sss$edu_eff+ sss$covid_edu)- 
																			invlogit(sss$age_eff[,i]+  sss$edu_eff),
																		invlogit(sss$age_eff[,i]+  sss$covid_eff[,i]+  sss$gender_eff + sss$covid_male)- 
																			invlogit(sss$age_eff[,i]+ sss$gender_eff),
																		invlogit(sss$age_eff[,i]+  sss$covid_eff[,i])- 
																			invlogit(sss$age_eff[,i])) %*% post_prob
}



# pdf("~/desktop/excess_prob.pdf", width = 5.6, height=2)
# layout(matrix(c(1:3),nrow=1), width = c(1.2,0.25,0.4),height = c(1))
# par(oma=c(1,1.5,1,0), pty='m',mar=c(1,1.5,1,1) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.7, cex.lab=0.6, cex.main=0.7) 
# draw_base_rate(sim_exce*1000, yl=c(-0.02,0.02))	
# abline(h=c(-.5,.5), col='gray40', lty=2)
# abline(h=c(0), col=1, lty=2)
# mtext(3,text= "excess mortality rate, \n poststratified", cex=0.7,line=-0.5)
# mtext(2, text=expression(10^{-3}), cex=0.7,las=2, line=1.2)
# 
# plot(c(0,0),type='n', axes=F,ylab="",xlab="",xaxs='i', xpd=T, 
# 		 xlim=c(0.8, 1.2), ylim=c(-10,10))
# abline(h=c(-5,0,5), col='gray40', lty=2)
# draw_coef=function(x,v){
# 	points(x=x, y=  mean(v), pch=18, col="#8F2727", cex=1.5)
# 	lines(x=c(x,x), y= as.vector(   quantile(v, c(0.975, 0.025) )), lwd=1, col="#B97C7C", xpd=T)
# 	lines(x=c(x,x), y= as.vector( quantile(v, c(0.75, 0.25) )), lwd=3, col="#8F2727" )
# }
# draw_coef(x=1, v=sim_exce[,9]*1000)
# axis(2, las=2, at=c(-5,-10,0,5,10),lwd=0.5)
# mtext(2, text=expression(10^{-3}), cex=0.7,las=2, line=1)
# abline(h=c(-2.5,-7.5), col='gray60', lty=2, lwd=0.5)
# axis(1, at=c(1), labels = "80+", padj=-0.8,lwd=0.5)
# box(lwd=0.5,bty='l')
# 
# plot(c(0,0),type='n', axes=F,ylab="",xlab="",xaxs='i', xpd=T, 
# 		 xlim=c(0.8, 1.2), ylim=c(-0.1,0.1))
# abline(h=c(-0.1,0,0.1), col='gray40', lty=2)
# abline(h=c(-0.05,-0.15), col='gray60', lty=2, lwd=0.5)
# draw_coef=function(x,v){
# 	points(x=x, y=  mean(v), pch=18, col="#8F2727", cex=1.5)
# 	lines(x=c(x,x), y= as.vector(   quantile(v, c(0.975, 0.025) )), lwd=1, col="#B97C7C", xpd=T)
# 	lines(x=c(x,x), y= as.vector( quantile(v, c(0.75, 0.25) )), lwd=3, col="#8F2727" )
# }
# draw_coef(x=1, v=sim_exce_agg*1000)
# axis(2, las=2, at=c(-.2,-.1,0,.1,.2),lwd=0.5)
# mtext(2, text=expression(10^{-3}), cex=0.7,las=2, line=1.1)
# axis(1, at=c(1), labels = "all ages", padj=-0.8,lwd=0.5, cex=0.7)
# box(lwd=0.5,bty='l')
# dev.off()
data_lab=seq(1,23,by=3) %%12
data_lab[1]="19/1"
data_lab[5]="20/1"
plot_rate=function(rate_est, size_est, ylim=c(0,30e-3)){
plot(date_vec[1:22], pch=19, col=alpha("darkred", alpha=0.6), xpd=T, y=rate_est,  xaxs='i', axes = FALSE, main="", xlab="", ylab="",  cex=size_est,  yaxs='i', xaxs='i', ylim=ylim)
lines(date_vec[1:22],  y=rate_est, col="darkred", lwd=0.9)
axis(1, lwd=0.5,  at=date_vec[seq(1,23,by=3)], labels =data_lab , padj=-0.8 )
axis(2, lwd=0.5, at=c(0, 10e-3,20e-3, 30e-3), labels = c("0","10", "20", "30"),  las=2)
abline(h=c( 10e-3,20e-3, 30e-3), col='gray60', lty=2)
mtext(1, line=0.5, text="month",cex=0.7)
box(lwd=0.5, bty='l')
}



#graph
pdf("~/desktop/month_count.pdf", width=2.5, height=2)
par(mfrow=c(1,1),oma=c(1,1,1,0),mar=c(1,1,1,0) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.7, cex.lab=0.6, cex.main=0.9)
hist(t1, breaks =date_vec-1, ylim=c(0,70),freq=T, yaxs='i', xaxs='i', axes = FALSE, main="", xlab="", ylab="", col="#B97C7C", border = "#A25050")
abline(h=c(20, 40, 60), col='gray60', lty=2)
axis(1, lwd=0.5,  at=date_vec[seq(1,23,by=3)], labels =data_lab , padj=-0.8 )
axis(2, lwd=0.5,at=c(0,20,40,60),  las=2)
mtext(1, line=0.5, text="month",cex=0.7)
mtext(3, line=1, text="surveyed  death cases by month" ,cex=0.7)
mtext(2, line=1.2, text="case count" ,cex=0.7)
box(lwd=0.5, bty='l')
dev.off()






pdf("~/desktop/day_count.pdf", width=2.5, height=2)
par(mfrow=c(1,1),oma=c(1,1,1,0),mar=c(1,1,1,1) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.7, cex.lab=0.6, cex.main=0.9)
hist(t1_d, breaks = c(0:31)+0.5, ylim=c(0,65),xlim=c(0.5,31.5), yaxs='i', xaxs='i', axes = FALSE, main="", xlab="", ylab="", col="#B97C7C", border = "#A25050")
abline(h=c(20, 40, 60), col='gray60', lty=2)
abline(v=c(15, 10, 5), col=1, lty=2)
axis(1, lwd=0.5, at=c(1,5, 10,15,20,30), padj=-0.8)
axis(2, lwd=0.5,at=c(0,20,40,60),  las=2)
mtext(1, line=0.5, text="day of month",cex=0.7)
mtext(3, line=1, text="'rounding error' of reported counts" ,cex=0.7)
mtext(2, line=1.2, text="case count" ,cex=0.7)
box(lwd=0.5, bty='l')
dev.off()

pdf("~/desktop/day_count_2R.pdf", width=5, height=2)
par(mfrow=c(1,2),oma=c(1,1,1,0),mar=c(1,1,1,0.5) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.7, cex.lab=0.6, cex.main=0.9)

hist(t1_d[-which(cd$dd20_verified_r2_==1)], breaks = c(0:31)+0.5, ylim=c(0,60),xlim=c(0.5,31.5), yaxs='i', xaxs='i', axes = FALSE, main="", xlab="", ylab="", col="#B97C7C", border = "#A25050")
abline(h=c(20, 40, 60), col='gray60', lty=2)
abline(v=c(15, 10, 5), col=1, lty=2)
axis(1, lwd=0.5, at=c(1,5, 10,15,20,30), padj=-0.8)
axis(2, lwd=0.5,at=c(0,20,40,60),  las=2)
mtext(1, line=0.5, text="day of month",cex=0.7)
mtext(3, line=0.5, text="Round 1 survey" ,cex=0.7)
mtext(2, line=1.2, text="case count" ,cex=0.7)
box(lwd=0.5, bty='l')

hist(t1_d[cd$dd20_verified_r2_==1], breaks = c(0:31)+0.5, ylim=c(0,15),xlim=c(0.5,31.5), yaxs='i', xaxs='i', axes = FALSE, main="", xlab="", ylab="", col="#B97C7C", border = "#A25050")
abline(h=c(5, 10, 15), col='gray60', lty=2)
abline(v=c(15, 10, 5), col=1, lty=2)
axis(1, lwd=0.5, at=c(1,5, 10,15,20,30), padj=-0.8)
axis(2, lwd=0.5,at=c(0,5,10,15),  las=2)
mtext(1, line=0.5, text="day of month",cex=0.7)
mtext(3, line=0.5, text="Round 2 survey" ,cex=0.7)
box(lwd=0.5, bty='l')
dev.off()





pdf("~/desktop/age_count.pdf", width=2, height=4)
par(mfrow=c(3,1),oma=c(1,1,1,0),mar=c(1,1.3,0.5,0.5) ,mgp=c(1.5,0.25,0), lwd=0.5,tck=-0.01, cex.axis=0.7, cex.lab=0.6, cex.main=0.9)



hist(age_capped[t1<as.Date("2020-01-01")], ylim=c(0,0.058), breaks =seq(0,85, by=5)-0.0001,  yaxs='i', xaxs='i', axes = FALSE, main="", xlab="", ylab="", col="#B97C7C", border = "#A25050", probability = T)
abline(h=c(1:5)/100, col='gray60', lty=2)
axis(1, lwd=0.5, at=c(seq(0,80,by=20)[-5], 80),  labels = c(seq(0,80,by=20)),  padj=-0.8)
axis(2, lwd=0.5, at=c(0,2,4)/100, labels = c("0","2%", "4%"),  las=2)
mtext(3, line=-1, text="deceased population 2019" ,cex=0.7)
box(lwd=0.5, bty='l')
mtext(2, line=1.5, text="proportion" ,cex=0.7)


hist(age_capped[t1>=as.Date("2020-01-01")] ,ylim=c(0,0.058), breaks = seq(0,85, by=5)-0.0001,  yaxs='i', xaxs='i', axes = FALSE, main="", xlab="", ylab="", col="#B97C7C", border = "#A25050", probability = T)
abline(h=c(1:5)/100, col='gray60', lty=2)
axis(1, lwd=0.5, at=c(seq(0,80,by=20)[-5], 80),  labels = c(seq(0,80,by=20)),  padj=-0.8)
axis(2, lwd=0.5, at=c(0,2,4)/100, labels = c("0","2%", "4%"),  las=2)
mtext(3, line=-1, text="deceased population 2020" ,cex=0.7)
box(lwd=0.5, bty='l')
mtext(2, line=1.5, text="proportion" ,cex=0.7)

hist(ifelse(age<80,age,80), seq(0,85, by=5)-0.0001,  yaxs='i', xaxs='i', axes = FALSE, main="", xlab="", ylab="", col="#B97C7C", border = "#A25050", ylim=c(0,1e4))
abline(h=c(2500,5000,7500), col='gray60', lty=2)
axis(1, lwd=0.5, at=c(seq(0,80,by=20)[-5], 80),  labels = c(seq(0,80,by=20)),  padj=-0.8)
axis(2, lwd=0.5, at=c(0,5000,1e4),	c("0","5000",expression(10^4) ),  las=2)
mtext(1, line=0.5, text="age",cex=0.7)
mtext(3, line=-1, text="survey population" ,cex=0.7)
mtext(2, line=1.5, text="count" ,cex=0.7)
box(lwd=0.5, bty='l')

dev.off()
