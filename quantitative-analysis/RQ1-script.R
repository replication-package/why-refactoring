##################################################################################
# SET YOUR PATH HERE
##################################################################################
setwd(".")

##################################################################################
# READS THE TABLE
t<-read.csv("BigTable-Consolidated-Final.csv")
##################################################################################

##################################################################################
# METRICS
##################################################################################

maxSlopes=c("MaxCBO","MaxWMC","MaxDIT","MaxNOC","MaxRFC","MaxNOM","MaxNOPM","MaxNOSM","MaxNOF","MaxNOPF","MaxNOSF","MaxNOSI","MaxLOC","MinC3","MaxHsLCOM","MinStructRead","MinSemantRead")

pmd=c("sumAvoidDeeplyNestedIfStmts","sumCouplingBtwObjects","sumExcessiveImport","sumNPathComplexity","sumExcessiveMethodLength","sumExcessiveParameterList","sumExcessiveClassLength","sumCyclomaticComplexity","sumTooManyField","sumNcssMethodCount","sumNcssTypeCount","sumNcssConstructorCount","sumTooManyMethods","sumIsGodClass")

decor=c("SumisGodDecor","SumisCDSBPDecor","SumisComplexDecor","SumisFuncDecDecor","sumisSpaghCodeDecor")

process=c("MaxCountFileRelatedToIssueFixFromCommitMsg","MaxavgLinesImpactedInCommit","MinavgGeneralExp","MinavgFileExp")

releases=c("MinorPrevious","MajorNext")


##################################################################################
# CORRELATION ANALYSIS
##################################################################################

library(Hmisc)
#select here the combineation where you want to compute correlations
allMetrics=c(maxSlopes,releases,process,decor,pmd)
#allMetrics=c(maxSlopes)
#allMetrics=c(releases,process)
#allMetrics=c(decor,pmd)

t2<-subset(t,select=allMetrics)


# REDUNDANCY ANALYSIS

f=paste(allMetrics,sep="+",collapse="+")
f2=paste("~",f,sep="")
redun(eval(parse(text=f2)),data=t2,r2=0.8,nk=0)

#Remove constant features
#sumNcssMethodCount, sumNcssTypeCount, sumNcssConstructorCount

pmdReduced=c("sumAvoidDeeplyNestedIfStmts","sumCouplingBtwObjects","sumExcessiveImport","sumNPathComplexity","sumExcessiveMethodLength","sumExcessiveParameterList","sumExcessiveClassLength","sumCyclomaticComplexity","sumTooManyField","sumTooManyMethods","sumIsGodClass")
allMetricsReduced=c(maxSlopes,releases,process,decor,pmdReduced)
t2<-subset(t,select=allMetricsReduced)
f=paste(allMetricsReduced,sep="+",collapse="+")
f2=paste("~",f,sep="")
variables=redun(eval(parse(text=f2)),data=t2,r2=0.8,nk=10)

#excluded metrics
excluded=variables$Out
excluded

#retained metrics
metrics=variables$In
metrics


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

# GENERATE NEW FORMULA

f=paste(metrics,sep="+",collapse="+")
f2=paste("r~",f,sep="")
f2=sub("MinC3","LackC3",f2)
f2=sub("MinStructRead","LackStructRead",f2)
f2=sub("MinSemantRead","LackSemantRead",f2)
f2=sub("MinavgGeneralExp","LackGeneralExp",f2)
f2=sub("MinavgFileExp","LackFileExp",f2)

f2=paste(f2,"+(1|ProjectName)",sep="")

m=glmer(eval(parse(text=f2)),family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
save(m,file="model-revised")

summary(m)



##################################################################################
# CREATE TABLE WITH MODEL SUMMARY AND ODDS RATIO
##################################################################################

library(xtable)
xtable(summary(m)$coefficients)
mod=summary(m)$coefficients
or=exp(mod[,1])

m2=data.frame(OR=or,Estimate=mod[,1],Std.Error=mod[,2],z.value=mod[,3],p.value=mod[,4])
m2

print(xtable(m2,digits=4),include.rownames=TRUE)
print(xtable(m2,digits=2),include.rownames=TRUE)

##################################################################################
# COMPARE AIC OF ALTERNATIVE MODELS
##################################################################################

#inspect the model
inspect(m)

#logistic
AIClmer=AIC(m)

#linear
mlin=lm(eval(parse(text=f2)))
AIClm=AIC(mlin)

#Poisson
mpoisson=glmer(eval(parse(text=f2)),family = poisson,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
AICpoisson=AIC(mpoisson)


# Partial models
fstruct="r~MaxCBO+MaxWMC+MaxDIT+MaxNOC+MaxRFC+MaxNOM+MaxNOPM+MaxNOSM+MaxNOF+MaxNOSF+MaxNOSI+MaxLOC+(1|ProjectName)"
fconcept="r~LackC3+MaxHsLCOM+LackStructRead+LackSemantRead+(1|ProjectName)"
fprocess="r~MinorPrevious+MajorNext+MaxCountFileRelatedToIssueFixFromCommitMsg+MaxavgLinesImpactedInCommit+LackGeneralExp+LackFileExp+(1|ProjectName)"
fsmell="r~SumisGodDecor+SumisCDSBPDecor+SumisComplexDecor+SumisFuncDecDecor+sumisSpaghCodeDecor+sumAvoidDeeplyNestedIfStmts+sumCouplingBtwObjects+sumExcessiveImport+sumExcessiveMethodLength+sumExcessiveParameterList+sumTooManyField+sumTooManyMethods+(1|ProjectName)"

mstruct=glmer(eval(parse(text=fstruct)),family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mconcept=glmer(eval(parse(text=fconcept)),family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mprocess=glmer(eval(parse(text=fprocess)),family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
msmell=glmer(eval(parse(text=fsmell)),family = binomial,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

AICstruct=AIC(mstruct)
AICconcept=AIC(mconcept)
AICprocess=AIC(mprocess)
AICsmell=AIC(msmell)

lmstruct=lm(eval(parse(text="r~MaxCBO+MaxWMC+MaxDIT+MaxNOC+MaxRFC+MaxNOM+MaxNOPM+MaxNOSM+MaxNOF+MaxNOSF+MaxNOSI+MaxLOC")))
lmconcept=lm(eval(parse(text="r~LackC3+MaxHsLCOM+LackStructRead+LackSemantRead")))
lmprocess=lm(eval(parse(text="r~MinorPrevious+MajorNext+MaxCountFileRelatedToIssueFixFromCommitMsg+MaxavgLinesImpactedInCommit+LackGeneralExp+LackFileExp")))
lmsmell=lm(eval(parse(text="r~SumisGodDecor+SumisCDSBPDecor+SumisComplexDecor+SumisFuncDecDecor+sumisSpaghCodeDecor+sumAvoidDeeplyNestedIfStmts+sumCouplingBtwObjects+sumExcessiveImport+sumExcessiveMethodLength+sumExcessiveParameterList+sumTooManyField+sumTooManyMethods")))


AIClmstruct=AIC(lmstruct)
AIClmconcept=AIC(lmconcept)
AIClmprocess=AIC(lmprocess)
AIClmsmell=AIC(lmsmell)


#Summary table
res=list(name=c(),AIC=c())
res$name=c(res$name,"Logistic")
res$AIC=c(res$AIC,AIClmer)
res$name=c(res$name,"Linear")
res$AIC=c(res$AIC,AIClm)
res$name=c(res$name,"Poisson")
res$AIC=c(res$AIC,AICpoisson)
res$name=c(res$name,"Logistic, structural metrics")
res$AIC=c(res$AIC,AICstruct)
res$name=c(res$name,"Logistic, conceptual metrics")
res$AIC=c(res$AIC,AICconcept)
res$name=c(res$name,"Logistic, process metrics")
res$AIC=c(res$AIC,AICprocess)
res$name=c(res$name,"Logistic, smells")
res$AIC=c(res$AIC,AICsmell)
res=data.frame(res)

print(xtable(res),include.rownames=FALSE)


