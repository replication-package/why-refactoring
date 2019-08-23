##################################################################################
# SET YOUR PATH HERE
##################################################################################
setwd(".")

##################################################################################
# READS THE TABLE
t<-read.csv("rq1-data-analysis-table.csv")
##################################################################################

##################################################################################
# METRICS
##################################################################################
maxSlopes=c("MaxCBO","MaxWMC","MaxDIT","MaxNOC","MaxRFC","MaxLCOM","MaxNOM","MaxNOPM","MaxNOSM","MaxNOF","MaxNOPF","MaxNOSF","MaxNOSI","MaxLOC","MinC3","MaxHsLCOM","MinStructRead","MinSemantRead")

process=c("MaxCountFileRelatedToIssueFixFromCommitMsg","MaxavgLinesImpactedInCommit","MaxageInCommits","MinavgGeneralExp","MinavgFileExp")

pmd=c("sumAvoidDeeplyNestedIfStmts","sumCouplingBtwObjects","sumExcessiveImport","sumNPathComplexity","sumExcessiveMethodLength","sumExcessiveParameterList","sumExcessiveClassLength","sumCyclomaticComplexity","sumTooManyField","sumNcssMethodCount","sumNcssTypeCount","sumNcssConstructorCount","sumTooManyMethods","sumIsGodClass")

decor=c("SumisGodDecor","SumisCDSBPDecor","SumisComplexDecor","SumisFuncDecDecor","sumisSpaghCodeDecor")

releases=c("MajorPrevious","MinorPrevious","MajorNext","MinorNext")


##################################################################################
# CORRELATION ANALYSIS
##################################################################################

library(Hmisc)
#select here the combineation where you want to compute correlations
allMetrics=c(maxSlopes,releases,process,decor,pmd)
allMetrics=c(maxSlopes)
allMetrics=c(releases,process)
allMetrics=c(decor,pmd)

t2<-subset(t,select=allMetrics)
mx<-na.omit((t2))
v<-varclus(as.matrix(mx,similarity="spearman",type="data.matrix"))

plot(v)

Threshold=0.5**2
a<-cutree(v$hclust,h=1-Threshold)
write.csv(a,file="prunemetrics.csv",row.names=TRUE,quote=FALSE)


met<-read.csv("SelectedMetrics.txt",header=FALSE)
mpruned=c(as.character(met$V1))

metrics=c(maxSlopes,process,releases,decor,pmd)



##################################################################################
# NORMALIZATION
##################################################################################

range01 <- function(x){(x-min(x,na.rm=TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm=TRUE))}

fields=metrics
res=list()
l=levels(t$ProjectName)
for(i in l)
{
  st<-subset(t,t$ProjectName==as.character(i))
  res$ProjectName=c(res$ProjectName,st$ProjectName)
  res$Commit_ID=c(res$Commit,st$Commit_ID)
  res$TS=c(res$TS,st$TS)
  res$Ref=c(res$Ref,st$Ref)
  res$RefNoRen=c(res$RefNoRen,st$RefNoRen)
  for(field in fields)
  {
    if(sum(st[[field]],na.rm = TRUE)>0  && ! is.na(sum(st[[field]],na.rm = TRUE)))
    {
      f<-range01(st[[field]])
    } else
    {
      f<-st[[field]]
    }
    res[[field]]=c(res[[field]],f)
  }
}

res=data.frame(res)



##################################################################################
# MIXED MODEL
##################################################################################
library(lme4)
attach(res)

#Boolean variable for commits containing refactorings
r=res$Ref>0
#Boolean variable for commits containing refactorings (excluding renaming)
rn=res$RefNoRen>0

#Boolean variable for commits containing refactorings (only renaming)
rren=res$RefNoRen==0 & res$Ref>0


# INVERT METRICS

LackC3=1-MinC3
LackStructRead=1-MinStructRead
LackSemantRead=1-MinSemantRead
LackGeneralExp=1-MinavgGeneralExp
LackFileExp=1-MinavgFileExp

# BUILD THE MODEL, REPLACE r WITH rn OR rren TO ANALYZE SUBSETS OF REFACTORINGS
m<-glmer(r~MaxRFC+MaxDIT+MaxHsLCOM+MaxNOC+MaxNOSM+MaxNOF+MaxNOSI+LackC3+LackStructRead+LackSemantRead+
           sumAvoidDeeplyNestedIfStmts+sumNPathComplexity+sumCouplingBtwObjects+sumExcessiveParameterList+
           SumisGodDecor+SumisCDSBPDecor+SumisComplexDecor+SumisFuncDecDecor+sumisSpaghCodeDecor+
           MaxCountFileRelatedToIssueFixFromCommitMsg+MaxavgLinesImpactedInCommit+LackGeneralExp+LackFileExp+
           MinorPrevious+MinorNext+
           MajorPrevious+MajorNext+
           (1|ProjectName),family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


##################################################################################
# CREATE TABLE WITH MODEL SUMMARY AND ODDS RATIO
##################################################################################

library(xtable)
xtable(summary(m)$coefficients)
mod=summary(m)$coefficients

m2=data.frame(OR=or,Estimate=mod[,1],Std.Error=mod[,2],z.value=mod[,3],p.value=mod[,4])

print(xtable(m2,digits=4),include.rownames=TRUE)

