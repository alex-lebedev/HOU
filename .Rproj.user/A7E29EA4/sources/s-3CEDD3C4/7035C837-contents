# BLOODWORK

# CLean the workspace:
rm(list=ls())

# Load libraries:
library(xlsx)
library(R.matlab)
library(ggplot2)
library(nlme)  

# Load data:
load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/cogdat_cleaned.rda')
hplc <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/RBTII_HPLC_blood-2018-09-28.xlsx',1)[,c('ID', 'ldopa', 'hva', 'Visit')]


# Calculate averaged N-back performance:
scogdat <- cogdat_cleaned
scogdat$v1.NBACKtrained <- apply(scogdat[,c('trained2back.OA.v1', 'trained3back.OA.v1')],1, mean)
scogdat$v2.NBACKtrained <- apply(scogdat[,c('trained2back.OA.v2', 'trained3back.OA.v2')],1, mean)
scogdat$v1.NBACKnear <- apply(scogdat[,c('near2back.OA.v1', 'near3back.OA.v1')],1, mean)
scogdat$v2.NBACKnear <- apply(scogdat[,c('near2back.OA.v2', 'near3back.OA.v2')],1, mean)
scogdat <- cogdat_cleaned
scogdat$G[scogdat$group=='con'] <- 0
scogdat$G[scogdat$group=='act'] <- 1
scogdat$G <- as.factor(scogdat$G)

# Calculate averaged updating performance:
scogdat$v1.NearUPD <- apply(scogdat[,c('v1.nearUpd.count.lvl2', 'v1.nearUpd.count.lvl4')],1, mean)
scogdat$v2.NearUPD <- apply(scogdat[,c('v2.nearUpd.count.lvl2', 'v2.nearUpd.count.lvl4')],1, mean)
scogdat$v1.TrainedUPD <- apply(scogdat[,c('v1.trainedUpd.count.lvl2', 'v1.trainedUpd.count.lvl4')],1, mean)
scogdat$v2.TrainedUPD <- apply(scogdat[,c('v2.trainedUpd.count.lvl2', 'v2.trainedUpd.count.lvl4')],1, mean)



# Visuospatial Reasoning
dat <- scogdat[,c('G','v1.rav', 'v2.rav', 'v1.beta', 'v2.beta', 'v1.wasi', 'v2.wasi', 'ID')] 
x1 <- as.numeric(as.vector(dat[,2]))
x2 <- as.numeric(as.vector(dat[,3]))
y1 <- as.numeric(as.vector(dat[,4]))
y2 <- as.numeric(as.vector(dat[,5]))
z1 <- as.numeric(as.vector(dat[,6]))
z2 <- as.numeric(as.vector(dat[,7]))
x1s <- (x1-mean(x1, na.rm=T))/sd(x1, na.rm=T)
x2s <- (x2-mean(x1, na.rm=T))/sd(x1, na.rm=T)
y1s <- (y1-mean(y1, na.rm=T))/sd(y1, na.rm=T)
y2s <- (y2-mean(y1, na.rm=T))/sd(y1, na.rm=T)
z1s <- (z1-mean(z1, na.rm=T))/sd(z1, na.rm=T)
z2s <- (z2-mean(z1, na.rm=T))/sd(z1, na.rm=T)
tmp1 <- cbind(x1s,y1s,z1s)
tmp2 <- cbind(x2s,y2s,z2s)
dat$SR1 <- apply(tmp1, 1,mean)
dat$SR2 <- apply(tmp2, 1,mean)
datSR12 <- dat




hplcv1 <- hplc[hplc$Visit==1,c('ID', 'ldopa', 'hva')]
hplcv2 <- hplc[hplc$Visit==2,c('ID', 'ldopa', 'hva')]
hplcv12 <- merge(hplcv1,hplcv2, by='ID', all=T)


d <- merge(hplcv12, datSR12, by='ID') 
d$hva.mean <- apply(d[,c('hva.x', 'hva.y')],1,mean, na.rm=T)
d$ldopa.mean <- apply(d[,c('ldopa.x', 'ldopa.y')],1,mean, na.rm=T)
d$dopamine.comp <- apply(cbind((d$hva.mean-mean(d$hva.mean))/sd(d$hva.mean),(d$ldopa.mean-mean(d$ldopa.mean))/sd(d$ldopa.mean)),1,mean)


beh <- d$SR2-d$SR1
bio <- d$ldopa.x
#cor(beh,bio, use='complete.obs')
dd <- merge(d, scogdat, by='ID')

d



#GMV:
gm1 <- subset(gmvdat[,c('ID', 'gmv')], gmvdat$visit=='V1')
gm2 <- subset(gmvdat[,c('ID', 'gmv')], gmvdat$visit=='V2')
corsource <- merge(dd, gm1, by ='ID', all=T)
corsource <- merge(corsource, gm2, by ='ID', all=T)

#Progress:
corsource <- merge(corsource, tab_upd[,c('ID','maxlvl', 'nsess')], by ='ID', all=T)
corsource <- merge(corsource, tab_tsw[,c('ID','maxlvl', 'nsess')], by ='ID', all=T)
corsource <- merge(corsource, tab_nb[,c('ID','maxlvl', 'nsess')], by ='ID', all=T)
colnames(corsource[,c(which(colnames(corsource)=='maxlvl.x'),which(colnames(corsource)=='nsess.x'))]) <- c('upd.maxlvl', 'upd.nsess')
colnames(corsource[,c(which(colnames(corsource)=='maxlvl.y'),which(colnames(corsource)=='nsess.y'))]) <- c('tsw.maxlvl', 'tsw.nsess')
colnames(corsource[,c(which(colnames(corsource)=='maxlvl'),which(colnames(corsource)=='nsess'))]) <- c('nb.maxlvl', 'nb.nsess')

save(corsource, file='/Users/alebedev/Documents/R/REBOOT2/corsource.rda')

# Start here:
library(scales)

load('/Users/alebedev/Documents/R/REBOOT2/corsource.rda')

ddd <- corsource
#beh <- ddd$SR2-ddd$SR1
beh <- ddd$gmv.y-ddd$gmv.x
bio <- log10(ddd$ldopa.x+10)


plot(bio, beh, typ='n', cex.axis=2.5, xlim = c(0,7), ylab='', xlab='')

points(bio[ddd$group=='con'], beh[ddd$group=='con'], pch=17, cex=5, col=hue_pal()(4)[3])
points(bio[ddd$group=='act'], beh[ddd$group=='act'], pch=16, cex=5, col=hue_pal()(4)[1])
abline(summary(glm(beh~bio)), col='black', lwd=7)
cor(bio, beh, use='complete.obs')




### MLE:
library(nlme)
ddd <- corsource



df <- data.frame(ID=as.factor(c(rep(ddd$ID,2))), visit=as.factor(c(rep(1,dim(ddd)[1]),rep(2,dim(ddd)[1]))),
                 group=as.factor(c(rep(ddd$group,2))),
                 sr = c(ddd$SR1,ddd$SR2), gmv = c(ddd$gmv.x,ddd$gmv.y),
                 ldopa = log10(c(ddd$ldopa.x, ddd$ldopa.y)+10), hva = log10(c(ddd$hva.x, ddd$hva.y)+10))
df$sr_ch <- rep(c(df$sr[df$visit==2]-df$sr[df$visit==1]),2)
df$gmv_ch <- rep(c(df$gmv[df$visit==2]-df$gmv[df$visit==1]),2)

modME <- lme(gmv_ch~group*ldopa,data=df, random=~1|ID,na.action=na.exclude)
summary(modME)




df1 <- subset(df, df$group==1)
df2 <- subset(df, df$group==2)


# SR

# L-dopa
ggplot(df, aes(x=ldopa, y=sr_ch, color=visit, shape=group)) +
  geom_line(aes(x=ldopa, y=sr_ch, group=ID,color=visit, shape=group),size=1, data=df1)+
  geom_line(aes(x=ldopa, y=sr_ch, group=ID,color=visit, shape=group), size=1, data=df2)+
  geom_point(size=7) + 
  geom_smooth(method=lm, aes(group=visit, color=visit, fill=visit), size=3, data=df1) + geom_smooth(method=lm, aes(group=visit,color=visit,fill=visit), size=3, data=df2) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# HVA:
ggplot(df, aes(x=hva, y=sr_ch, color=visit, shape=group)) +
  geom_line(aes(x=hva, y=sr_ch, group=ID,color=visit, shape=group),size=1, data=df1)+
  geom_line(aes(x=hva, y=sr_ch, group=ID,color=visit, shape=group), size=1, data=df2)+
  geom_point(size=7) + 
  geom_smooth(method=lm, aes(group=visit, color=visit, fill=visit), size=3, data=df1) + geom_smooth(method=lm, aes(group=visit,color=visit,fill=visit), size=3, data=df2) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# GMV
# L-dopa
ggplot(df, aes(x=ldopa, y=gmv_ch, color=visit, shape=group)) +
  geom_line(aes(x=ldopa, y=gmv_ch, group=ID,color=visit, shape=group),size=1, data=df1)+
  geom_line(aes(x=ldopa, y=gmv_ch, group=ID,color=visit, shape=group), size=1, data=df2)+
  geom_point(size=7) + 
  geom_smooth(method=lm, aes(group=visit, color=visit, fill=visit), size=3, data=df1) + geom_smooth(method=lm, aes(group=visit,color=visit,fill=visit), size=3, data=df2) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# HVA:
ggplot(df, aes(x=hva, y=gmv_ch, color=visit, shape=group)) +
  geom_line(aes(x=hva, y=gmv_ch, group=ID,color=visit, shape=group),size=1, data=df1)+
  geom_line(aes(x=hva, y=gmv_ch, group=ID,color=visit, shape=group), size=1, data=df2)+
  geom_point(size=7) + 
  geom_smooth(method=lm, aes(group=visit, color=visit, fill=visit), size=3, data=df1) + geom_smooth(method=lm, aes(group=visit,color=visit,fill=visit), size=3, data=df2) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# SR, L-dopa
cor(df1$ldopa[df1$group==1 & df1$visit==1], df1$sr_ch[df1$group==1 & df1$visit==1], use='complete.obs')
cor(df1$ldopa[df1$group==1 & df1$visit==2], df1$sr_ch[df1$group==1 & df1$visit==2], use='complete.obs')
#
cor(df2$ldopa[df2$group==2 & df2$visit==1], df2$sr_ch[df2$group==2 & df2$visit==1], use='complete.obs')
cor(df2$ldopa[df2$group==2 & df2$visit==2], df2$sr_ch[df2$group==2 & df2$visit==2], use='complete.obs')


cor(df1$ldopa[df1$group==1], df1$sr_ch[df1$group==1], use='complete.obs')


cor(df2$ldopa[df2$group==2], df2$sr_ch[df2$group==2], use='complete.obs')


### ARCH:
#######
# SR: #
#######

# L-dopa
ggplot(df, aes(x=ldopa, y=sr_ch, color=visit, shape=group)) +
  geom_line(aes(x=ldopa, y=sr_ch, group=ID,color=visit, shape=group),size=1, linetype='dotted', col='grey', data=df1)+
  geom_line(aes(x=ldopa, y=sr_ch, group=ID,color=visit, shape=group), size=1, linetype='dotted', col='grey', data=df2)+
  geom_point(size=7) + 
  geom_smooth(method=lm, aes(group=group, linetype=group), color='black', size=3) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# HVA:
ggplot(df, aes(x=hva, y=sr_ch, color=visit, shape=group)) +
  geom_line(aes(x=hva, y=sr_ch, group=ID,color=visit, shape=group),size=1, linetype='dotted', col='grey', data=df1)+
  geom_line(aes(x=hva, y=sr_ch, group=ID,color=visit, shape=group), size=1, linetype='dotted', col='grey', data=df2)+
  geom_point(size=7) + 
  geom_smooth(method=lm, aes(group=group, linetype=group), color='black', size=3) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

########
# GMV: #
########

# L-dopa
ggplot(df, aes(x=ldopa, y=gmv_ch, color=visit, shape=group)) +
  geom_line(aes(x=ldopa, y=gmv_ch, group=ID,color=visit, shape=group),size=1, linetype='dotted', col='grey', data=df1)+
  geom_line(aes(x=ldopa, y=gmv_ch, group=ID,color=visit, shape=group), size=1, linetype='dotted', col='grey', data=df2)+
  geom_point(size=7) + 
  geom_smooth(method=lm, aes(group=group, linetype=group), color='black', size=3) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# HVA:
ggplot(df, aes(x=hva, y=gmv_ch, color=visit, shape=group)) +
  geom_line(aes(x=hva, y=gmv_ch, group=ID,color=visit, shape=group),size=1, linetype='dotted', col='grey', data=df1)+
  geom_line(aes(x=hva, y=gmv_ch, group=ID,color=visit, shape=group), size=1, linetype='dotted', col='grey', data=df2)+
  geom_point(size=7) + 
  geom_smooth(method=lm, aes(group=group, linetype=group), color='black', size=3) +
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

