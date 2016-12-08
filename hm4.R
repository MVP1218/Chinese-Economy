setwd("~/Desktop")
pro<-read.csv('pro.csv')
attach(pro)
library(stargazer)
stargazer(pro,title='Summary')
lma<-lm(gdp_pc_real~em_dpe)
stargazer(lma,title='real GDP per capita on private employment')
em_dpe_sh<-em_dpe/(em_soe+em_dpe)
pro1<-cbind(pro,em_dpe_sh)
png('pic3.png')
plot(gdp_pc_real,em_dpe_sh,main='Real per capita GDP and share of private employment',
     ylab='share of private employment(percent)',xlab='Real per capita GDP(yuan)',pch=20)
dev.off()
lma1<-lm(gdp_pc_real~em_dpe_sh)
stargazer(lma1)
loggdp<-log(gdp_pc_real)
pro2<-cbind(pro1,loggdp)
lma2<-lm(loggdp~em_dpe_sh)
lma3<-lm(loggdp~em_dpe)
stargazer(lma3,lma2)
gdp_pc_growth<-vector(length=310)
for ( i in 1:31){
  for (j in 1:10){
    gdp_pc_growth[(10*(i-1)+j)]<-(pro2[(j+11*(i-1)+1),3]-
                                    pro2[(j+11*(i-1)),3])/pro2[(j+11*(i-1)),3]
  }
}
em_s<-vector(length=310)
for ( i in 1:31)
  {for (j in 1:10){
    em_s[j+10*(i-1)]<-em_sh[j+11*(i-1)]}
}
png('pic5.png')
plot(gdp_pc_growth,em_s,main='Growth rate of real per capita GDP and share of private employment',ylab='share of private employment(percent)',xlab='Growth rate of real per capita GDP(yuan)',pch=20,xlim=c(0.0,0.2))
dev.off()

gdp_pc_gro<-vector(length=0)
for( i in 1:31){
  gdp_pc_gro<-c(gdp_pc_gro,gdp_pc_growth[(1+10*(i-1)):(10*i)],NA)
}
pro3<-cbind(pro2,gdp_pc_gro)
detach(pro)
pro4<-plm.data(pro3,index=c("province","year"))
lma4<-plm(gdp_pc_growth~em_s,data=pro4,model='random')
stargazer(lma4)
lma5<-plm(gdp_pc_gro~em_dpe_sh,data=pro4,effect='individual',model='within')
stargazer(lma5)
east<-vector(length=341)
for(i in 1:341){
  if (pro$province[i]=='Liaoning'||pro$province[i]=='Hebei'||pro$province[i]=='Beijing'||pro$province[i]=='Tianjin'||pro$province[i]=='Shandong'||pro$province[i]=='Shanghai'||pro$province[i]=='Jiangsu'||pro$province[i]=='Zhejiang'||pro$province[i]=='Fujian'||pro$province[i]=='Hainan'||pro$province[i]=='Guangdong'){
    east[i]<-1
    }
  else east[i]<-0
}
pro5<-cbind(pro3,east)
attach(pro5)
eastern<-gdp_pc_gro[pro5$east==1]
noneastern<-gdp_pc_gro[pro5$east==0]
t.test(eastern,noneastern)
ea<-em_dpe_sh[pro5$east==1]
nea<-em_dpe_sh[pro5$east==0]
t<-t.test(ea,nea)
detach(pro5)
lma7<-plm(data=pro8,gdp_pc_gro~em_dpe_sh+east+east*em_dpe_sh,effect='individual',model='within')
stargazer(lma7)
pro7<-pro5[((year!=1997)&(year!=1998)&(year!=1999)&(year!=2000)),]
pro8<-plm.data(pro7,index=c('province','year'))
lma10<-plm(data=pro8,gdp_pc_gro~em_dpe_sh,model='within',effect='time',lag=0.89)
