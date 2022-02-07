data ken.dd;*current data that include cropland and Rental Averages**;
set ken.importd; 
** mean for before 2005**; 
data ken.b2004;
set ken.dd;
if time>=10 then delete; 
proc corr pearson data=ken.b2004;
var CornAc SoyAc BarlyAc OatAc wheatAc TotAc  GECorn GESoy cornpr soypr WheatPr years  ;
run;

** summary of variables by states-before**;
Proc means data=ken.b2004;
class Id;
var CornAc SoyAc BarlyAc OatAc wheatAc TotAc  GECorn GESoy cornpr soypr WheatPr years  ;
run;


** mean for after 2005**;
data ken.A2004;
set ken.dd;
if time<10 then delete;
proc corr pearson data=ken.A2004;
var CornAc SoyAc BarlyAc OatAc wheatAc TotAc  GECorn GESoy cornpr soypr WheatPr years  ;
run;

** summary of variables by states-After**;

Proc means data=ken.A2004;
class Id;
var CornAc SoyAc BarlyAc OatAc wheatAc TotAc  GECorn GESoy cornpr soypr WheatPr years  ;
run;

Proc means data=ken.dd;
class Id;
var CornAc SoyAc BarlyAc OatAc wheatAc TotAc  GECorn GESoy cornpr soypr WheatPr years  ;
run;

proc corr pearson data=ken.dd;
var CornAc SoyAc BarlyAc OatAc wheatAc TotAc  GECorn GESoy cornpr soypr WheatPr  ;
run;
data ken.a7;** main data stored here 200-`17 data or data ken.a1**;
*data ken.a3;       ** main data stored here 2000-`19 data**;
*set IMPORT1*;
set ken.dd; 
cornbean= cornac + soyac;
ll=log(cbprop);
cbprop= cornbean/totac;
CBACratio= cornac/soyac;
cornprop= (cornac/totac);
soyprop= soyac/totac;
lgsoy=lag(soyac);
lgsoyp= lag(soyprop);
lgcorn= lag(cornac);
CBratio= (cornpr/soypr);
lagCBratio= lag(cornpr/soypr);
cde=(CornAc**2/totac**2)+(SoyAc**2/totac**2)+(BarlyAc**2/totac**2)+(OatAc**2/totac**2)+(wheatAc**2/totac**2);
***creating first difference variables***;
scaleecons=AvgFsize-medFsize;
scale=Fsize-medFsize;
ges=gesoy/100;
GEC=gecorn/100;
ethanol1=ethanol/1000; 
Fsize1=Fsize/1000;
CRP11=CRP/1000; 


** create bew variables base on the reviewers comments**;
GMacres=GEC*cornac;
NonGMacres=cornac-GMacres;
cae1=GMacres/totac;
cae2=NonGMacres/totac;
cae3=NonGMacres/cornac;
*** end code for the reviewers comments***;

kethanol=log(ethanol+1);
lagethanol=lag(ethanol1);
kcrp=log(crp);
kcrp1=log(crp1);
kFsize=log(Fsize);
*****DR fausti code*****;
CRPAC= CRP/1000;***let CRP be in 1000 of total acres as totac***;
CRPINT=CRPAC/(CRPAC+totac);
CRPINT1=CRPAC/(totac);
lcrp=lag(crpint);
lcrp1=lag(crpint1);
lcrp2=lag(crpac);

irrigat= irrigate/1000;***let irrigation be in 1000 of total acres as totac***;
irrigat1=irrigat/(CRPAC+totac);
lagdefcroplandAvg=lag(defcroplandAvg);

lagCRP=lag(CRP);
lagCRP1=lag(CRP1);
lagCRPRentals=lagCRP1/lagCRP;

CRPa=log(CRP);
CRPb=log(CRP1);
CRPaa=lag(CRPa);
CRPbb=lag(CRPb);
CRPc=CRP2/100;***for each year**;
CRPd=CRP3/100;** at the beginning of each year***;
CRPe=log(CRP2);
CRPf=log(CRP3);
lagCRP=lag(CRP2);** take lag to account for possible endogenrity)**;
    pp=(gecorn+gesoy)/2;
	CAI1 = cornAc/(CRPAC+ totAc);
		CAI = cornAc/ totAc;
	CAE=log(CAI);
	CSprRatio= soypr/cornpr;
	lCSprRatio=lag(CSprRatio);
	kk=lag(CSprRatio);
	crpacRatio= crpac/totac;
	m=lag(CAI);
	mm=lag(CSprRatio);
	mmm=lag(gecorn);
*****ens here****;	
lagcorn=lag(cornprop);
difcorn= cornprop-lagcorn;
lgeth1= lag(ethprod);
difETH= ethprod- lgeth1;
lagGEcorn=lag(gecorn);
difGMCorn= gecorn - laggecorn;
lagCBR=lag(cbratio);
difcbr= cbratio-lagcbr;
lagbt=lag(cornbt);
difbt= cornbt - lagbt;
CWR=(cornpr/Wheatpr)*100;
lgCWR=lag(CWR);
difCWR=CWR-lgCWR;
L1dfC=lag(difcorn);
L2difC=lag(L1dfc);
L3Dfc=lag( L2difC);
crp1000=crp/1000;
lgcrp1000=lag(crp1000);
difcrp=crp1000-lgcrp1000;
crp1=(crp1000/totac)*100;
lgcrp1=lag(crp1);
difcrp1=crp1-lgcrp1;
*b=log(gecorn);
b1=(gecorn)**2;
gec1=(gec)**2;
gec2=(gec)**3;
**creating RFS dummy variable for pre 2007 period***;
if 11<time then RFS=1; 
else RFS=0;
**creating RFS dummy variable for pre 2005 period***;
if 9<time then RFS1=1; 
else RFS1=0;

**creating RFS dummy variable for pre 2005 period***;
if time>=10 then RFS2=1; 
else RFS2=0;
if years>=2005 then RFS3=1; 
else RFS3=0;
**removing pre 2000 data***;

**8creating dummy for GEcorn **;
if GECorn>75 then gedummy=1; 
else gedummy=0;

if GECorn>80 then gedummy1=1; 
else gedummy1=0;

if GECorn>85 then gedummy2=1; 
else gedummy2=0;

if time>=23 then delete;
if time <5 then dum=0 ; 
else dum=1;
if dum=0 then delete;
if id=1 then IA1=1;
else IA1=0;
if id=2 then IL1=1;
else IL1=0;
if id=3 then NE1=1;
else NE1=0;
If id=4 then MN1=1;
else MN1=0;
if id=5 then IN1=1;
else IN1=0;
if id=6 then SD1=1;
else SD1=0;
if id=7 then WI1=1;
else WI1=0;
if id=8 then OH1=1;
else OH1=0;
if id=9 then KS1=1;
else KS1=0;
if id=10 then MO1=1;
else MO1=0;
*irrigate1=log(irrigate);
YearSq= Years*Years;
timesqr= time*time;

** CAI to remove GEC**;
caiN= cai-gec;
caisq=cai*cai;

run; 


proc export data=ken.a1 /*export data as csv file. */
   OUTFILE='/home/u48732282/paper2/a1.csv'
   DBMS=csv replace;  
RUN; 

proc export data=ken.a222 /*export data as csv file. */
   OUTFILE='/home/u48732282/paper2/a222.csv'
   DBMS=csv replace; 
RUN; 
**NEW results following Dr.Fausti but used corn realtive to soy PR,***;
**be corncern when correlation is above 0.5, do not inclide RFS1 in the correlation table because it is not continous(dummy);***;
title "Table 1.2 pearson correlaton table, use pearson because spearman is good for ordinal data, pearson trully establishes a ,inear relationship btn two continous predictors";
proc corr pearson data=ken.a222;
var cae1 cae2 cae3 lagCBratio gec lagethanol crpint  irrigat1 scale defcroplandAvg  ;
run;
TITLE 'correlation table '; 
proc corr pearson data=ken.a3;
var cai lagCBratio gec ges  ;
run; 

     
irr
**model respomsivenes to crpint**;

**Base model**;
TITLE 'MODEL 9a: Simple model,Base model  , gec =GEcorn/100 for all to be in ratio '; ** DIFCRP *;
proc mixed data=ken.a22 covtest empirical;
*class id time  RFS1(ref="0");
class id time;
model cae= lagCBratio RFS1 / S chisq outpred=c;*****MI IS THE BASE STATE DUMMY**;
random INTERCEPT  /type=vc subject=id S;
repeated / type=ar(1)   subject=id ;
run;

proc reg data = sp500;
  model open = /dw;
run;
quit;
*******************************************************************************************
**test of relevance for ges**; ****;
TITLE 'MODEL 3:  , simple regression, cai as response'; 
proc reg data=ken.a3 ;
model gec=lagCBratio RFS1 ges ID /white dw; ** white to correct for heteroscedasticity**;
run;

**Endogeneity test on ges**; **1st satge from Gujarati page 704 edit 45* ,,
 econometrics models and forecast;
TITLE 'MODEL 11:  , simple regression, ges as response'; 
proc reg data=ken.a3 ;
model ges=lagCBratio gec RFS1 IA1 IL1 NE1 MN1 IN1 SD1 WI1  OH1 KS1 MO1   / r white dw p ; ** white to correct for heteroscedasticity**;
output out=endogeneity_test_data r= v1Resid predicted=gespred ;** // store the residuals as v to run the second reg**;
run;
proc means data=endogeneity_test_data;** look at the saved data**;
 
 TITLE 'MODEL 33:  , second reg to check for endogeneity'; 
proc reg data=endogeneity_test_data ;
model cai=lagCBratio  gespred RFS1 v1Resid IA1 IL1 NE1 MN1 IN1 SD1 WI1  OH1 KS1 MO1  /white dw ; ** white to correct for heteroscedasticity**;
run;    
 *************************************************************************************************************
**Endogeneity test on ges**; **1st satge from Worldridge page page 516 edit 12**;
TITLE 'MODEL 1:  , simple regressio n, ges as response'; 
proc reg data=ken.a3 ;
model ges=lagCBratio gec RFS1 IA1 IL1 NE1 MN1 IN1 SD1 WI1  OH1 KS1 MO1   / r white dw p ; ** white to correct for heteroscedasticity**;
output out=endogeneity_test_data r= v11 ;** // store the residuals as v to run the second reg**;
run;
proc means data=endogeneity_test_data;** look at the saved data**;
 
 TITLE 'MODEL 3:  , second reg to check for endogeneity'; 
proc reg data=endogeneity_test_data ;
model cai=lagCBratio  ges RFS1 v11 IA1 IL1 NE1 MN1 IN1 SD1 WI1  OH1 KS1 MO1  /white dw ; ** white to correct for heteroscedasticity**;
run; 



******new specificataion**;
**Endogeneity test on ges**; **1st satge**;
TITLE 'MODEL 1:  , simple regressio n, ges as response'; 
proc reg data=ken.a3 ;
model gec=lagCBratio RFS1 IA1 IL1 NE1 MN1 IN1 SD1 WI1  OH1 KS1 MO1   / r white dw; ** white to correct for heteroscedasticity**;
output out=endogeneity_test_data r= vRsid ;** // store the residuals as v to run the second reg**;
run;
*proc means data=endogeneity_test_data;** look at the saved data**;
 
 TITLE 'MODEL 3:  , second reg to check for endogeneity'; 
proc reg data=endogeneity_test_data ;
model cai=lagCBratio RFS1 vRsid IA1 IL1 NE1 MN1 IN1 SD1 WI1  OH1 KS1 MO1  /white dw ; ** white to correct for heteroscedasticity**;
run;  



* Paper 2 code*;
TITLE 'Base model-Model 1 '; ** DIFCRP *;
proc mixed data=ken.a1 covtest empirical;
*class id time  RFS1(ref="0");
class id time;
model cai1= lagCBratio RFS1 ges  / S chisq outpred=c;*****MI IS THE BASE STATE DUMMY**;
random INTERCEPT  /type=vc subject=id S;
repeated / type=ar(1)   subject=id ;
run; 



TITLE 'Model 2- adding CRP '; ** DIFCRP *;
proc mixed data=ken.a7 covtest empirical;
*class id time  RFS1(ref="0");
class id time;
model cai1= lagCBratio ges RFS1 crpint / S chisq outpred=c;*****MI IS THE BASE STATE DUMMY**;
random INTERCEPT  /type=vc subject=id S;
repeated / type=ar(1)   subject=id ;
run; 

**model respomsivenes to scaleecons**;
TITLE 'MODEL 3: adding scale '; ** DIFCRP *;
proc mixed data=ken.a1 covtest empirical;
class id time;
model cai1= lagCBratio RFS1 ges scale / S chisq outpred=c;*****MI IS THE BASE STATE DUMMY**;
random INTERCEPT  /type=vc subject=id S;
repeated / type=ar(1)   subject=id ;
run;

TITLE 'MODEL 4: Adding irrigation '; ** DIFCRP *;
proc mixed data=ken.a1 covtest empirical;
class id time;
model cai1= lagCBratio RFS1 ges irrigat1  / S chisq outpred=c;*****MI IS THE BASE STATE DUMMY**;
random INTERCEPT  /type=vc subject=id S;
repeated / type=ar(1)   subject=id ;
run;

TITLE 'MODEL 5a: Adding ethanol  '; ** DIFCRP *;
proc mixed data=ken.a1 covtest empirical;
class id time;
model cai1= lagCBratio RFS1 ges lagethanol / S chisq outpred=c;*****MI IS THE BASE STATE DUMMY**;
random INTERCEPT  /type=vc subject=id S;
repeated / type=ar(1)   subject=id ;
run;

TITLE 'MODEL 5b: Adding ethanol  '; ** DIFCRP *;
proc mixed data=ken.a1 covtest empirical;
class id time;
model cai1= lagCBratio RFS1 ges lagethanol scale  / S chisq outpred=c;*****MI IS THE BASE STATE DUMMY**;
random INTERCEPT  /type=vc subject=id S;
repeated / type=ar(1)   subject=id ;
run;

TITLE 'MODEL 6: Adding cropland prices '; ** DIFCRP *;
proc mixed data=ken.a1 covtest empirical;
class id time;
model cai1= lagCBratio RFS1 ges defcroplandAvg / S chisq outpred=c;*****MI IS THE BASE STATE DUMMY**;
random INTERCEPT  /type=vc subject=id S;
repeated / type=ar(1)   subject=id ;
run;  



**log likelihood test**;
** Model 1 and 2**;
TITLE 'Models 1 and 2 ,log likelihood test'; ** DIFCRP *;
data LRT;
 dev1 = -931.1;** for reduced model/base model**;
 dev2 = -952.4; **for full model**;
 chi = abs(dev1-dev2);
 p = 1-probchi(chi,1);
run;
proc print data=LRT; run;

** Model 1 and 3**;
TITLE 'Models 1 and 3 '; ** DIFCRP *;
data LRT;
 dev1 = -931.1;** for reduced model/base model**;
 dev2 = -920.9; **for full model**;
 chi = abs(dev1-dev2);
 p = 1-probchi(chi,1);
run;
proc print data=LRT; run;

** Model 1 and 4**;
TITLE 'Models 1 and 4 '; ** DIFCRP *;
data LRT;
 dev1 = -931.1;** for reduced model**;
 dev2 = -931.3; **for full model**;
 chi = abs(dev1-dev2);
 p = 1-probchi(chi,1);
run;
proc print data=LRT; run;

** Model 1 and 5**;
TITLE 'Models 1 and 5 '; ** DIFCRP *;
data LRT;
 dev1 = -931.1;** for reduced model**;
 dev2 = -923.5; **for full model**;
 chi = abs(dev1-dev2);
 p = 1-probchi(chi,1);
run;
proc print data=LRT; run;


** Model 1 and 6**;
TITLE 'Models 1 and 6 '; ** DIFCRP *;
data LRT;
 dev1 = -931.1;** for reduced model**;
 dev2 = -915.0; **for full model**;
 chi = abs(dev1-dev2);
 p = 1-probchi(chi,1);
run;
proc print data=LRT; run; 




**Endogeneity test on ges**; **1st satge from Gujarati page 704 edit 45* ,,
 econometrics models and forecast;
TITLE 'MODEL 11:  , simple regression, ges as response'; 
proc reg data=ken.a1 ;
model ges=lagCBratio gec RFS1 IA1 IL1 NE1 MN1 IN1 SD1 WI1  OH1 KS1 MO1   / r white dw p ; ** white to correct for heteroscedasticity**;
output out=endogeneity_test_data1 r= v1Resid1 predicted=gespred1 ;** // store the residuals as v to run the second reg**;
run;
*proc means data=endogeneity_test_data;** look at the saved data**;
 
 TITLE 'MODEL 33:  , second reg to check for endogeneity'; 
proc reg data=endogeneity_test_data1 ;
model cai=lagCBratio  gespred1 RFS1 v1Resid1 IA1 IL1 NE1 MN1 IN1 SD1 WI1  OH1 KS1 MO1  /white dw ; ** white to correct for heteroscedasticity**;
run;    



