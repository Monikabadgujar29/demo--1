patient=1:10
drug=c("A","P")
p=0.5

prob=rep(0,10)
drug1=rep(0,10)
drug1[1]=sample(drug,1,prob=c(p,1-p))
prob[1]=p
for (i in 2:10)
{
  p=p+ifelse(drug1[i-1]=="A",-0.02,0.02)
  drug1[i]=sample(drug,1,prob=c(p,1-p))
  prob[i]=p
}
df=data.frame("patients"=patient,"Drug"=drug1,"prob"=prob)
print(table(df$Drug))
sprintf("sample fraction:%1.3f",min(table(df$Drug))/10)
df


patient=1:20
p=0.5
resPAct=0.7
resPPla=0.3
drug=c("A","P")
drug1=rep(0,20)
res=rep(0,20)
prob=rep(0,20)
drug1[1]=sample(drug,1,prob=c(p,1-p))
respr=ifelse(drug1[1]=="A",resPAct,resPPla)
res[1]=sample(c("pos","neg"),1,prob=c(respr,1-respr))
prob[1]=p
for (i in 2:20)
{
  if(drug1[i-1]=="A")
  {
    p=p+ifelse(res[i-1]=="pos",0.02,-0.02)
  }else{p=p+ifelse(res[i-1]=="pos",-0.02,0.02)}
  prob[i]=p
  drug1[i]=sample(drug,1,prob=c(p,1-p))
  respr=ifelse(drug1[i]=="A",resPAct,resPPla)
  res[i]=sample(c("pos","neg"),1,prob=c(respr,1-respr))
}
df=data.frame(patient,prob,res,drug1)
df

patient=30
drug=c("A","P")
df=data.frame(patient,drug=sample(drug,30,replace=T))
print(table(df$drug))
sprintf("sample fraction:%1.3f",min(table(df$drug))/30)

N=30
nb=3
n=N/nb
D1=sample(rep(drug,n/2),n,replace=F)
D2=sample(rep(drug,n/2),n,replace=F)
D3=sample(rep(drug,n/2),n.replace=F)
df=data.frame(patient,c(D1,D2,D3))
print(table(df$drug))
sprintf("sample fraction: %1.3f",min(table(df$drug))/30)


t.test(c(2,3,4),c(4,5,3,9),conf.level = 0.95)
