#MCPR calculation 
mdrn_mtd=c(7214,5240,8130,274,5821,9837,4459,19760,29004,14637
           ,1385,6926,2743,1005,1516,2790,2028,11881,10394,
           9613,10245,11823,23901,13871,1101,17657,6494,
           16081,891,259,4590,13184,1792,1065,14831,659) 
length(mdrn_mtd) 
married=c(13987,7681,15341,490,9297,15752,7434,30740,63064,31824,
          2157,13442,5797,5264,4252,5787,8182,25715,16522,19344,
          20037,18876,35188,24321,1797,25207,8308,21855,
          1267,832,8130,18537,2486,1678,20441,1372) 
length(married) 
mcpr=(mdrn_mtd/married)*100 
mcpr 
max(mcpr) 
#Independent variables 
#1Proportion of individuals with no education 
x1=c(0.233016452,0.088541667,0.156997841,0.120643432,0.143524096, 
     0.160755854,0.125817726,0.325819958,0.292609854,0.392933644,
     0.099052278,0.229496585,0.100268207,0.096120368,0.065805743,
     0.122231337,0.140881656,0.18625461,0.198617339,0.328326099,
     0.239641057,0.276169734,0.277380706,0.227574004,0.159970512,
     0.132424826,0.289020501,0.209161057,0.050738916,0.015397083,
     0.010484092,0.097582846,0.060779504,0.081768878,0.353623083, 
     0.222505308) 
#2Proportion of individuals whose partners are not educated 
x2=c(0.158507029,0.046215139,0.161044177,0.112359551,0.070086705,
     0.098790323,0.087959009,0.155826018,0.193505155,0.303597422,
     0.078488372,0.221140473,0.094017094,0.077448747,0.058278146,
     0.116033755,0.296886314,0.200386287,0.219984802,0.248284874,
     0.185240964,0.226241368,0.19442876,0.1400818,0.068852459,
     0.105668821,0.3412527,0.229687939,0.044334975,0.032258065,
     0.023882897,0.087978509,0.106888361,0.09929078,0.314734089,
     0.139175258) 
#3 Proportion of individuals from Middle class or lower 
x3=c(0.535833659,0.45476466,0.181801479,0.111260054,0.579668675,
     0.247158702,0.10117394,0.582042335,0.684044929,0.857660711,
     0.693060226,0.775360486,0.831338973,0.818453121,0.540733617,
     0.899507793,0.891053556,0.877011921,0.796431241,0.847480657,
     0.814843946,0.768230996,0.724912208,0.540773176,0.411352746,
     0.536246482,0.553257403,0.601149237,0.13546798,0.160453809,
     0.250250707,0.493216374,0.18288362,0.595327493,0.566392906,
     0.704883227) 
#4 Proportion of individuals who met community health workers in the last 3 months 
x4=c(0.155011503,0.34211034,0.116852694,0.061662198,0.185466867,
     0.293669268,0.185769334,0.214282391,0.320690692,0.243156086,
     0.275756649,0.129471288,0.092840933,0.073862223,0.071713147,
     0.238993711,0.228505923,0.312530375,0.357903587,0.206680506,
     0.504129277,0.385485457,0.277256765,0.39007858,0.418356063,
     0.201819098,0.174685621,0.537711377,0.148275862,0.273905997,
     0.508979852,0.246325393,0.391114745,0.10387985,0.268488571,
     0.184713376) 
state=c("Jammu & Kashmir","Himachal Pradesh","Punjab","Chandigarh",
"Uttarakhand", "Haryana","Nct Of Delhi","Rajasthan","Uttar Pradesh",
"Bihar","Sikkim","Arunachal Pradesh","Nagaland","Manipur","Mizoram",
"Tripura","Meghalaya","Assam","West Bengal","Jharkhand","Odisha",
"Chhattisgarh","Madhya Pradesh","Gujarat","Dadra & Nagar Haveli And Daman & Diu"
,"Maharashtra","Andhra Pradesh","Karnataka","Goa","Lakshadweep","Kerala",
"Tamil Nadu","Puducherry","Andaman & Nicobar Islands","Telangana","Ladakh") 

#mCPR calculation 
df=data.frame(State=state,Modern_Method=mdrn_mtd,Married=married,mCPR=mcpr) 
df

#Check for normality and Box Cox transformation 
qqnorm(mcpr) 
qqline(mcpr) 
hist(mcpr,prob=TRUE) 
shapiro.test(mcpr) 
library(MASS)
m1=lm(mcpr~x1+x2+x3+x4) 
bc=boxcox(m1) 
lambda <- bc$x[which.max(bc$y)] 
lambda 
y=((mcpr^lambda)-1)/lambda 
shapiro.test(y) 
hist(y,prob=TRUE) 
qqnorm(y)
qqline(y) 

#original and transformed dependent variable 
df2=data.frame(State=state,mCPR=mcpr,Transformed_Variable=y) 
df2

#Independent variables 
regressor=data.frame(State=state,X1=x1,X2=x2,X3=x3,X4=x4) 
regressor 

#Checking Correlation among explanatory variables 
X=cbind(x1,x2,x3,x4) 
cor(X)

#New model after Box-Cox Transformation 

model=lm(y~x1+x3+x4) 
summary(model)
