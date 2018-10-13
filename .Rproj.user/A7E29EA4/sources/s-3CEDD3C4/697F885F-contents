# House of Ourororos: Article 2
# Filename: HOU_2_drugs_and_sch.R
# Title: Drugs and pyschosis risk 
# Date: 2018-10-13

library(xlsx)
library(ggplot2)
df1 <- as.data.frame(read.xlsx2('/Users/alebedev/GitHub/HOU/materials/HOU_2_drugs_and_sch.xlsx',1))
df2 <- as.data.frame(read.xlsx2('/Users/alebedev/GitHub/HOU/materials/HOU_2_drugs_and_sch.xlsx',2))



# 
df2[,2:4]<-apply(df2[,2:4],2,as.numeric)
df2 <- df2[order(df2$HR),]
rownames(df2)<-1:5
df2$Группа = factor(df2$Группа, levels = as.vector(df2$Группа))
df2$Группа <- factor(df2$Группа, order=as.numeric(rownames(df2)))

# Bar plot
p<- ggplot(df2, aes(x=Группа, y=HR, fill=Группа)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) 
print(p)

p+theme_bw()+theme(axis.text=element_text(size=32),
  axis.title=element_text(size=14,face="bold"),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  coord_flip()

# Meta-analyses
df1[,3:5]<-apply(df1[,3:5],2,as.numeric)
df1 <- df1[order(df1$OR),]
df1$Drug = factor(df1$Drug, levels = as.vector(df1$Drug))
df1$Drug <- factor(df1$Drug, order=as.numeric(rownames(df1)))

# Bar plot
p<- ggplot(df1, aes(x=Drug, y=OR, fill=Author)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) 

p+theme_bw()+theme(axis.text=element_text(size=32),
                   axis.title=element_text(size=14,face="bold"),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  coord_flip()
