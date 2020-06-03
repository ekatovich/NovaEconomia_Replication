*******************************************************************************************
*How Have Changes in Labor Productivity Affected Real Wages in Brazil? A Sectoral Analysis*
*******************************************************************************************

*Authors:*
*Erik Katovich, Fulbright Research Fellow, Instituto de Economia, Unicamp*
*Alexandre Gori Maia, Professor, Instituto de Economia, Unicamp*

*Date: January 3rd, 2017*

***************************************************
*Stata Do-file  for all analyses described in text*
***************************************************


*Contents*
*******************************************************************************************
*1. Pooling PNAD Data, Years 1996-2014, Data cleaning,///                                 *
*    *Organizing Activities into Economic Sectors, Tabulating Hours Worked per Sector     *
*2. Processing of Pooled dataset, Merging PNAD data with state-level Value-Added data     *
*3. Regression Code (baseline and sectoral), formating regression output                   *
*4. Creating Table of Descriptive Statistics                                                *
*******************************************************************************************
		
	
	
******************************************************************************************************************
*Part 1: Pooling 1996-2014 PNAD datasets into one pooled dataset, Creating Economic Sectors, Tabulating Hours and Earnings
******************************************************************************************************************

*1996 Import variables from PNAD 
infix  V0101 1-2 /*ano da pesquisa*/ ///
	   UF    3-4 /*estado do Brasil*/  ///
	   V0302 16-16 /*sexo*/  ///
	   V8005 24-26  /*idade*/  ///
	   V0404 30-30  /*cor*/ ///
	   V4703 1411-1412 /*anos de estudo*/  ///
	   V9907 220-222 /*codigo da atividade*/ ///
	   V9058 493-494 /*horas trabalhadas por semana*/ ///
	   V4729 1511-1515 /*peso da pessoa*/ ///
	   V4718 1431-1442 /*rendimento mensal em dinheiro*/ ///
	   V4728 1510-1510 /*urbano ou rural*/ ///
	   V9087 653-653	/*sindicato*/ ///
	   V9042 449-449	/*trabalho formal*/ ///
	     using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\1996\Dados\PES1996.txt", clear

*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4703==17
generate anosestudo=.
replace anosestudo=(V4703-1) if V4703>=1 & V4703<=16
drop V4703

***Deflate renda according to annual inflation data
replace V4718=V4718/0.3304 

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728

rename V9907 occupation

*Now create variable for economic sector
*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (011	012	013	014	015	016	017	018	019	020	021	022	023	024	025	026	027	028	029	030	031	032	033	034	035	037	038	039	040	041	042	545 901 = 1), gen(sector1)

recode occupation (050 051 052 053	054	055	056	057	058	059 036 100	110	120	130	140	150	151	160	170	180	190	200	201	202	210	220	230	240 241 250	251	260 261 270	280	290	300 522 = 3), gen(sector3)

recode occupation (351 352	353 354 340 524 = 5), gen(sector5)

recode occupation (410	411	412	413	414	415	416	417	418	419	420	421	422	423	424	521 525 532 582 = 6), gen(sector6)

recode occupation (471	472	473	474	475	476	477	481	583 586 587 588 = 7), gen(sector7)

recode occupation (451	452	453	462	463	464 585 482	552	571 572 573	574	575	576	578 581 584 589 614 801 = 9), gen(sector9)

recode occupation (461 511	512	523	531 533 541 542 543	544 545	551	577	613 615 616	 617 618 619	902	903 = 11), gen(sector11)

recode occupation (610 611	612	621	622	623	624	631	632	711	712	713	714	715	716	717	721	722	723	724	725	726	727 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector

*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)

	   
	*Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\1996.dta", replace
	   
*1997 Import variables from PNAD 
infix  V0101 1-2 /*ano da pesquisa*/ ///
	   UF    3-4 /*estado do Brasil*/  ///
	   V0302 16-16 /*sexo*/  ///
	   V8005 24-26  /*idade*/  ///
	   V0404 30-30  /*cor*/ ///
	   V4703 1146-1147 /*anos de estudo*/  ///
	   V9907 220-222 /*codigo da atividade*/ ///
	   V9058 493-494 /*horas trabalhadas por semana*/ ///
	   V4729 1246-1250 /*peso da pessoa*/ ///
	   V4718 1166-1177 /*rendimento mensal em dinheiro*/ ///
	   V4728 1245-1245 /*urbano ou rural*/ ///
	   V9087 653-653	/*sindicato*/ ///
	   V9042 449-449	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\1997\Dados\PES1997.txt", clear
	   
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4703==17
generate anosestudo=.
replace anosestudo=(V4703-1) if V4703>=1 & V4703<=16
drop V4703

***Deflate renda according to annual inflation data
replace V4718=V4718/0.3448

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)


*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728	   
	 
rename V9907 occupation

*Now create variable for economic sector
*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (011	012	013	014	015	016	017	018	019	020	021	022	023	024	025	026	027	028	029	030	031	032	033	034	035	037	038	039	040	041	042	545 901 = 1), gen(sector1)

recode occupation (050 051 052 053	054	055	056	057	058	059 036 100	110	120	130	140	150	151	160	170	180	190	200	201	202	210	220	230	240 241 250	251	260 261 270	280	290	300 522 = 3), gen(sector3)

recode occupation (351 352	353 354 340 524 = 5), gen(sector5)

recode occupation (410	411	412	413	414	415	416	417	418	419	420	421	422	423	424	521 525 532 582 = 6), gen(sector6)

recode occupation (471	472	473	474	475	476	477	481	583 586 587 588 = 7), gen(sector7)

recode occupation (451	452	453	462	463	464 585 482	552	571 572 573	574	575	576	578 581 584 589 614 801 = 9), gen(sector9)

recode occupation (461 511	512	523	531 533 541 542 543	544 545	551	577	613 615 616	 617 618 619	902	903 = 11), gen(sector11)

recode occupation (610 611	612	621	622	623	624	631	632	711	712	713	714	715	716	717	721	722	723	724	725	726	727 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector	 

 *Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)
	 
	 
	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\1997.dta", replace
	   
*1998 Import variables from PNAD 
infix  V0101 1-2 /*ano da pesquisa*/ ///
	   UF    3-4 /*estado do Brasil*/  ///
	   V0302 16-16 /*sexo*/  ///
	   V8005 24-26  /*idade*/  ///
	   V0404 30-30  /*cor*/ ///
	   V4703 641-642 /*anos de estudo*/  ///
	   V9907 128-130 /*codigo da atividade*/ ///
	   V9058 342-343 /*horas trabalhadas por semana*/ ///
	   V4729 741-745 /*peso da pessoa*/ ///
	   V4718 661-672 /*rendimento mensal em dinheiro*/ ///
	   V4728 740-740 /*urbano ou rural*/ ///
	   V9087 384-384	/*sindicato*/ ///
	   V9042 298-298	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\1998\Dados\PES1998.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4703==17
generate anosestudo=.
replace anosestudo=(V4703-1) if V4703>=1 & V4703<=16
drop V4703

***Deflate renda according to annual inflation data
replace V4718=V4718/0.3554

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)


*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728
	
rename V9907 occupation

*Now create variable for economic sector
*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (011	012	013	014	015	016	017	018	019	020	021	022	023	024	025	026	027	028	029	030	031	032	033	034	035	037	038	039	040	041	042	545 901 = 1), gen(sector1)

recode occupation (050 051 052 053	054	055	056	057	058	059 036 100	110	120	130	140	150	151	160	170	180	190	200	201	202	210	220	230	240 241 250	251	260 261 270	280	290	300 522 = 3), gen(sector3)

recode occupation (351 352	353 354 340 524 = 5), gen(sector5)

recode occupation (410	411	412	413	414	415	416	417	418	419	420	421	422	423	424	521 525 532 582 = 6), gen(sector6)

recode occupation (471	472	473	474	475	476	477	481	583 586 587 588 = 7), gen(sector7)

recode occupation (451	452	453	462	463	464 585 482	552	571 572 573	574	575	576	578 581 584 589 614 801 = 9), gen(sector9)

recode occupation (461 511	512	523	531 533 541 542 543	544 545	551	577	613 615 616	 617 618 619	902	903 = 11), gen(sector11)

recode occupation (610 611	612	621	622	623	624	631	632	711	712	713	714	715	716	717	721	722	723	724	725	726	727 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector	
	
*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)
	
	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\1998.dta", replace
	   
*1999 Import variables from PNAD 
infix  V0101 1-2 /*ano da pesquisa*/ ///
	   UF    3-4 /*estado do Brasil*/  ///
	   V0302 16-16 /*sexo*/  ///
	   V8005 24-26  /*idade*/  ///
	   V0404 30-30  /*cor*/ ///
	   V4703 641-642 /*anos de estudo*/  ///
	   V9907 128-130 /*codigo da atividade*/ ///
	   V9058 342-343 /*horas trabalhadas por semana*/ ///
	   V4729 741-745 /*peso da pessoa*/ ///
	   V4718 661-672 /*rendimento mensal em dinheiro*/ ///
	   V4728 740-740 /*urbano ou rural*/ ///
	   V9087 384-384	/*sindicato*/ ///
	   V9042 298-298	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\1999\Dados\PES1999.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4703==17
generate anosestudo=.
replace anosestudo=(V4703-1) if V4703>=1 & V4703<=16
drop V4703

***Deflate renda according to annual inflation data
replace V4718=V4718/0.3782

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042


*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728	   

rename V9907 occupation

*Now create variable for economic sector
*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (011	012	013	014	015	016	017	018	019	020	021	022	023	024	025	026	027	028	029	030	031	032	033	034	035	037	038	039	040	041	042	545 901 = 1), gen(sector1)

recode occupation (050 051 052 053	054	055	056	057	058	059 036 100	110	120	130	140	150	151	160	170	180	190	200	201	202	210	220	230	240 241 250	251	260 261 270	280	290	300 522 = 3), gen(sector3)

recode occupation (351 352	353 354 340 524 = 5), gen(sector5)

recode occupation (410	411	412	413	414	415	416	417	418	419	420	421	422	423	424	521 525 532 582 = 6), gen(sector6)

recode occupation (471	472	473	474	475	476	477	481	583 586 587 588 = 7), gen(sector7)

recode occupation (451	452	453	462	463	464 585 482	552	571 572 573	574	575	576	578 581 584 589 614 801 = 9), gen(sector9)

recode occupation (461 511	512	523	531 533 541 542 543	544 545	551	577	613 615 616	 617 618 619	902	903 = 11), gen(sector11)

recode occupation (610 611	612	621	622	623	624	631	632	711	712	713	714	715	716	717	721	722	723	724	725	726	727 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector

 *Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)

 
	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\1999.dta", replace
	   
*2001 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4703 620-621 /*anos de estudo*/  ///
	   V9907 86-88 /*codigo da atividade*/ ///
	   V9058 298-299 /*horas trabalhadas por semana*/ ///
	   V4729 720-724 /*peso da pessoa*/ ///
	   V4718 640-651 /*rendimento mensal em dinheiro*/ ///
	   V4728 719-719 /*urbano ou rural*/ ///
	   V9087 340-340	/*sindicato*/ ///
	   V9042 254-254	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2001\Dados\PES2001.txt", clear

*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4703==17
generate anosestudo=.
replace anosestudo=(V4703-1) if V4703>=1 & V4703<=16
drop V4703

***Deflate renda according to annual inflation data
replace V4718=V4718/0.4343 

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728   
	
rename V9907 occupation

*Now create variable for economic sector
*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (011	012	013	014	015	016	017	018	019	020	021	022	023	024	025	026	027	028	029	030	031	032	033	034	035	037	038	039	040	041	042	545 901 = 1), gen(sector1)

recode occupation (050 051 052 053	054	055	056	057	058	059 036 100	110	120	130	140	150	151	160	170	180	190	200	201	202	210	220	230	240 241 250	251	260 261 270	280	290	300 522 = 3), gen(sector3)

recode occupation (351 352	353 354 340 524 = 5), gen(sector5)

recode occupation (410	411	412	413	414	415	416	417	418	419	420	421	422	423	424	521 525 532 582 = 6), gen(sector6)

recode occupation (471	472	473	474	475	476	477	481	583 586 587 588 = 7), gen(sector7)

recode occupation (451	452	453	462	463	464 585 482	552	571 572 573	574	575	576	578 581 584 589 614 801 = 9), gen(sector9)

recode occupation (461 511	512	523	531 533 541 542 543	544 545	551	577	613 615 616	 617 618 619	902	903 = 11), gen(sector11)

recode occupation (610 611	612	621	622	623	624	631	632	711	712	713	714	715	716	717	721	722	723	724	725	726	727 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector

 *Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)
	
	  *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2001.dta", replace
	   
*2002 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4703 666-667 /*anos de estudo*/  ///
	   V9907 141-145 /*codigo da atividade*/ ///
	   V9058 355-356 /*horas trabalhadas por semana*/ ///
	   V4729 768-772 /*peso da pessoa*/ ///
	   V4718 688-699 /*rendimento mensal em dinheiro*/ ///
	   V4728 767-767 /*urbano ou rural*/ ///
	   V9087 400-400	/*sindicato*/ ///
	   V9042 311-311	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2002\Dados\PES2002.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4703==17
generate anosestudo=.
replace anosestudo=(V4703-1) if V4703>=1 & V4703<=16
drop V4703

***Deflate renda according to annual inflation data
replace V4718=V4718/0.4774 

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728
	
*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050	74090 74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector	

*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)

	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2002.dta", replace
	   
*2003 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4703 785-786 /*anos de estudo*/  ///
	   V9907 146-150 /*codigo da atividade*/ ///
	   V9058 360-361 /*horas trabalhadas por semana*/ ///
	   V4729 887-891 /*peso da pessoa*/ ///
	   V4718 807-818 /*rendimento mensal em dinheiro*/ ///
	   V4728 886-886 /*urbano ou rural*/ ///
	   V9087 405-405	/*sindicato*/ ///
	   V9042 316-316	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2003\Dados\PES2003.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4703==17
generate anosestudo=.
replace anosestudo=(V4703-1) if V4703>=1 & V4703<=16
drop V4703

***Deflate renda according to annual inflation data
replace V4718=V4718/0.5577

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728
	
*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050 74090	74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector		

*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)
	
	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2003.dta", replace
	   
*2004 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4703 681-682 /*anos de estudo*/  ///
	   V9907 156-160 /*codigo da atividade*/ ///
	   V9058 370-371 /*horas trabalhadas por semana*/ ///
	   V4729 783-787 /*peso da pessoa*/ ///
	   V4718 703-714 /*rendimento mensal em dinheiro*/ ///
	   V4728 782-782 /*urbano ou rural*/ ///
	   V9087 415-415	/*sindicato*/ ///
	   V9042 326-326	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2004\Dados\PES2004.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4703==17
generate anosestudo=.
replace anosestudo=(V4703-1) if V4703>=1 & V4703<=16
drop V4703

***Deflate renda according to annual inflation data
replace V4718=V4718/0.5903

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)


*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728
	
*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050 74090	74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector		

*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)

	
	*Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2004.dta", replace
	   
*2005 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4703 696-697 /*anos de estudo*/  ///
	   V9907 145-149 /*codigo da atividade*/ ///
	   V9058 359-360 /*horas trabalhadas por semana*/ ///
	   V4729 798-802 /*peso da pessoa*/ ///
	   V4718 718-729 /*rendimento mensal em dinheiro*/ ///
	   V4728 797-797 /*urbano ou rural*/ ///
	   V9087 404-404	/*sindicato*/ ///
	   V9042 315-315	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2005\Dados\PES2005.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4703==17
generate anosestudo=.
replace anosestudo=(V4703-1) if V4703>=1 & V4703<=16
drop V4703

***Deflate renda according to annual inflation data
replace V4718=V4718/0.621 

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728

*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050 74090	74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector	
	
*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)
	
	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2005.dta", replace
	   
*2006 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4703 705-706 /*anos de estudo*/  ///
	   V9907 162-166 /*codigo da atividade*/ ///
	   V9058 376-377 /*horas trabalhadas por semana*/ ///
	   V4729 807-811 /*peso da pessoa*/ ///
	   V4718 727-738 /*rendimento mensal em dinheiro*/ ///
	   V4728 806-806 /*urbano ou rural*/ ///
	   V9087 421-421	/*sindicato*/ ///
	   V9042 332-332	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2006\Dados\PES2006.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4703==17
generate anosestudo=.
replace anosestudo=(V4703-1) if V4703>=1 & V4703<=16
drop V4703

***Deflate renda according to annual inflation data
replace V4718=V4718/0.6383 

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53


*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728

*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050 74090	74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector	

 *Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)

	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2006.dta", replace
	   
*2007 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4803 650-651 /*anos de estudo*/  ///
	   V9907 149-153 /*codigo da atividade*/ ///
	   V9058 343-344 /*horas trabalhadas por semana*/ ///
	   V4729 738-742 /*peso da pessoa*/ ///
	   V4718 672-683 /*rendimento mensal em dinheiro*/ ///
	   V4728 737-737 /*urbano ou rural*/ ///
	   V9087 388-388	/*sindicato*/ ///
	   V9042 301-301	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2007\Dados\PES2007.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4803==17
generate anosestudo=.
replace anosestudo=(V4803-1) if V4803>=1 & V4803<=16
drop V4803

***Deflate renda according to annual inflation data
replace V4718=V4718/0.6693 

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728

*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050 74090	74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector	

*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)
	   
	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2007.dta", replace
	   
*2008 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4803 654-655 /*anos de estudo*/  ///
	   V9907 151-155 /*codigo da atividade*/ ///
	   V9058 345-346 /*horas trabalhadas por semana*/ ///
	   V4729 742-746 /*peso da pessoa*/ ///
	   V4718 676-687 /*rendimento mensal em dinheiro*/ ///
	   V4728 741-741 /*urbano ou rural*/ ///
	   V9087 390-390	/*sindicato*/ ///
	   V9042 303-303	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2008\Dados\PES2008.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4803==17
generate anosestudo=.
replace anosestudo=(V4803-1) if V4803>=1 & V4803<=16
drop V4803

***Deflate renda according to annual inflation data
replace V4718=V4718/ 0.7171

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728
	
*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050	74090 74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector		

*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)
	
	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2008.dta", replace
	   
*2009 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4803 659-660 /*anos de estudo*/  ///
	   V9907 155-159 /*codigo da atividade*/ ///
	   V9058 350-351 /*horas trabalhadas por semana*/ ///
	   V4729 747-751 /*peso da pessoa*/ ///
	   V4718 681-692 /*rendimento mensal em dinheiro*/ ///
	   V4728 746-746 /*urbano ou rural*/ ///
	   V9087 395-395	/*sindicato*/ ///
	   V9042 307-307	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2009\Dados\PES2009.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4803==17
generate anosestudo=.
replace anosestudo=(V4803-1) if V4803>=1 & V4803<=16
drop V4803

***Deflate renda according to annual inflation data
replace V4718=V4718/0.7481

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728
	
*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050 74090	74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector		
	
*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)
	
	*Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2009.dta", replace
	   
*2011 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33	  /*cor*/ ///
	   V4803 663-664 /*anos de estudo*/  ///
	   V9907 157-161 /*codigo da atividade*/ ///
	   V9058 354-355 /*horas trabalhadas por semana*/ ///
	   V4729 751-755 /*peso da pessoa*/ ///
	   V4718 685-696 /*rendimento mensal em dinheiro*/ ///
	   V4728 750-750 /*urbano ou rural*/ ///
	   V9087 399-399	/*sindicato*/ ///
	   V9042 309-309	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2011\Dados\PES2011.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4803==17
generate anosestudo=.
replace anosestudo=(V4803-1) if V4803>=1 & V4803<=16
drop V4803

***Deflate renda according to annual inflation data
replace V4718=V4718/0.8406 

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728

*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050	74090 74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector	
	
*Create binary variable = 1 if observation belongs to advanced sector of economy
generate advanced=.
replace advanced=1 if economicsector==3 | economicsector==9
replace advanced=0 if economicsector==1 | economicsector==5 | economicsector==6 | economicsector==7 | ///
economicsector==11 | economicsector==12 	
	
*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)
	
	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2011.dta", replace
	   
*2012 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4803 664-665 /*anos de estudo*/  ///
	   V9907 158-162 /*codigo da atividade*/ ///
	   V9058 355-356 /*horas trabalhadas por semana*/ ///
	   V4729 752-756 /*peso da pessoa*/ ///
	   V4718 686-697 /*rendimento mensal em dinheiro*/ ///
	   V4728 751-751 /*urbano ou rural*/ ///
	   V9087 400-400	/*sindicato*/ ///
	   V9042 310-310	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2012\Dados\PES2012.txt", clear	   
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4803==17
generate anosestudo=.
replace anosestudo=(V4803-1) if V4803>=1 & V4803<=16
drop V4803

***Deflate renda according to annual inflation data
replace V4718=V4718/0.8892 

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728	   
	
*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050 74090	74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector		

*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)

	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2012.dta", replace
	   
*2013 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4803 669-670 /*anos de estudo*/  ///
	   V9907 163-167 /*codigo da atividade*/ ///
	   V9058 360-361 /*horas trabalhadas por semana*/ ///
	   V4729 757-761 /*peso da pessoa*/ ///
	   V4718 691-702 /*rendimento mensal em dinheiro*/ ///
	   V4728 756-756 /*urbano ou rural*/ ///
	   V9087 405-405	/*sindicato*/ ///
	   V9042 315-315	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2013\Dados\PES2013.txt", clear
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4803==17
generate anosestudo=.
replace anosestudo=(V4803-1) if V4803>=1 & V4803<=16
drop V4803

***Deflate renda according to annual inflation data
replace V4718=V4718/0.9393

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728

*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050	74090 74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector	
	   
 *Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)
	
	   *Save data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2013.dta", replace
	   
*2014 Import variables from PNAD 
infix  V0101 1-4 /*ano da pesquisa*/ ///
	   UF    5-6 /*estado do Brasil*/  ///
	   V0302 18-18 /*sexo*/  ///
	   V8005 27-29  /*idade*/  ///
	   V0404 33-33  /*cor*/ ///
	   V4803 669-670 /*anos de estudo*/  ///
	   V9907 163-167 /*codigo da atividade*/ ///
	   V9058 360-361 /*horas trabalhadas por semana*/ ///
	   V4729 757-761 /*peso da pessoa*/ ///
	   V4718 691-702 /*rendimento mensal em dinheiro*/ ///
	   V4728 756-756 /*urbano ou rural*/ ///
	   V9087 405-405	/*sindicato*/ ///
	   V9042 315-315	/*trabalho formal*/ ///
	   using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\2014\Dados\PES2014.txt", clear	      
*****************
*Data cleaning

*Drop missing income values
drop if V4718>999000000000
drop if V4718==-1
drop if V4718==.
drop if V4718==0

*Drop if job type is missing
drop if V9907==.
*Drop if horas de trabalho is -1
drop if V9058==-1

*Drop observations if age is lt 15 and greater than 65
drop if V8005<15 | V8005>65
drop if V8005==999
summarize V8005

*Create anos de estudo variable that gives correct number of years per observation
drop if V4803==17
generate anosestudo=.
replace anosestudo=(V4803-1) if V4803>=1 & V4803<=16
drop V4803

***Deflate renda according to annual inflation data
replace V4718=V4718/1

*Create variable ln(renda) to measure log of renda*
*For incomes of zero, add one real to maintain observations in sample
generate lnrenda = ln(V4718+1)

*Create binary variable for sindicate, where 1=belongs to labor sindicate, 0=otherwise
generate sindicate=.
replace sindicate=1 if V9087==1
replace sindicate=0 if V9087==2|V9087==9
drop V9087

*Create binary variable for formal work, where 1=holds carteira assinada, 0=otherwise
generate formal=.
replace formal=1 if V9042==2
replace formal=0 if V9042==4|V9042==9
drop V9042

*Drop northern states for consistency
drop if UF==11| UF==12|UF==13|UF==14|UF==15|UF==16|UF==17

*Organize UFs into regions
generate region=.
replace region=1 if UF==21 |UF==22 |UF==23 |UF==24 |UF==25 |UF==26 |UF==27 |UF==28 |UF==29
replace region=2 if UF==31 |UF==32 |UF==33 |UF==35
replace region=3 if UF==41 |UF==42 |UF==43
replace region=4 if UF==50 |UF==51 |UF==52 |UF==53

*Create binary variable for sex, where mulher = 1
generate mulher=.
replace mulher=1 if V0302==4
replace mulher=0 if V0302==2
drop V0302

*Create binary variable for race, where black, pardo, indigenous = 1
generate raca=.
replace raca=1 if V0404== 4|V0404==8|V0404==0
replace raca=0 if V0404==2|V0404==6
drop V0404

*Create binary variable for rural/urban, where rural = 1
generate rural=.
replace rural=1 if V4728==4|V4728==5|V4728==6|V4728==7|V4728==8
replace rural=0 if V4728==1|V4728==2|V4728==3	   
drop V4728

*Now create new variable for economic sector	
rename V9907 occupation

*Generate new variables for each economic sector (1-12) that equal 1-12 when occupation falls into that sector*
recode occupation (01101 01102 01103 01104 01105 01106 01107 01108 01109 01110 01111 01112	01113	01114	01115	01116	01117 01118	01201	01202 01203	01204 01205	01206	01207	01208	01209	01300	01401	01402	01500	02001	02002	05001 05002 = 1), gen(sector1)

recode occupation (10000 11000 12000 13001	13002 14001	14002 14003	14004 15010 15021	15022	15030	15041	15042	15043	15055 16000	17001 17002	18001 18002	19011 19012	19020 20000	21001 21002	22000	23010	23020	23030	23400	24010	24020	24030	24090	25010	25020	26010	26091	26092	27001	27002	27003	28001	28002	29001	29002	30000	31001	31002	32000	33001	33002	33003	33004	33005	34001	34002	34003	35010	35020	35030	35090	36010	36090	37000 = 3), gen(sector3)

recode occupation (40010 40020	41000	90000 45999 45005 = 5), gen(sector5)

recode occupation (50010 50020	50030	50040	50050	53010	53020	53030	53041	53042	53050	53061	53062	53063	53064	53065	53066	53067	53068	53070	53080	53090	53101	53102	53111	53112	53113 = 6), gen(sector6)

recode occupation (60010	60020	60031	60032	60040	60091	60092	61000	62000	63010	63021	63022	63030	64010 71010 = 7), gen(sector7)

recode occupation (65000	66000	67010	67020 92011 92012	92013	92014	92020 71020	72010	72020	73000	74011	74012	74021	74022	74030	74040	99000 64020 = 9), gen(sector9)

recode occupation (55010 55020 55030 70001 70002 91010	91020	91091	91092	92015	92030	92040	93010	93020	93030	93091	93092 95000	99888 71030 74050	74090	74060 = 11), gen(sector11)

recode occupation (75011	75012	75013	75014	75015	75016	75017	75020	80011	80012	80090	85011	85012	85013	85020	85030 = 12), gen(sector12)

*Generate a new variable, economicsector, that currently equals zero in all cases,but will later be reassigned to equal 1-12 based on sector1-12 variables*
gen economicsector = 0

*Reclassify economicsector to equal 1-12 in correspondence with sector variables*
*Result is a variable, economicsector, that classifies the sector into which each individual's job places them*
replace economicsector = 1 if sector1==1

replace economicsector = 3 if sector3==3

replace economicsector = 5 if sector5==5

replace economicsector = 6 if sector6==6

replace economicsector = 7 if sector7==7

replace economicsector = 9 if sector9==9

replace economicsector = 11 if sector11==11

replace economicsector = 12 if sector12==12

*Verify that all individuals have been successfuly classified into economic sectors*
*There should be no zero values*
tab economicsector	

*Individuals have been classified into sectors by economicsector variable*
*Now calculate annual hours worked by multiplying weekly hours be 50*
rename V9058 weeklyhrs

generate annualhrs = weeklyhrs*51

*Next, calculate the total hours worked by all individuals represented by this observation*
rename V4729 obsweight
generate totalannualhrs = annualhrs*obsweight

*Format data output to improve precision*
format totalannualhrs %12.0g

*Sum the annual hours worked in each economic sector to find total hrs per sector for the entire economy*
tabstat totalannualhrs, by(economicsector) stat(sum)  format(%20.10f)

****************************************************************************************************************

*Now Calculate Income*

*Rename V4718 as renda*
rename V4718 renda

*Create table describing average monthly wage per sector*
tabstat renda, by(economicsector) stat(mean)

*Now calculating income per hour*
generate monthlyhrs = annualhrs/12

generate hrwage = renda/monthlyhrs

*Create table describing hourly wage by economic sector*
tabstat hrwage, by(economicsector) stat(mean)

*****************************************************************************************************************

*Tabulate average number of years of schooling by economic sector*
tabstat anosestudo, by(economicsector) stat(mean)

*****************************************************************************************************
*Now Pool all saved datasets to create pooled dataset	   
	   
	*Pool PNADs 1996 through 2014*
  append using "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\1996.dta" ///
			   "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\1997.dta" ///
               "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\1998.dta" ///
			   "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\1999.dta" ///
			   "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2001.dta" ///
			   "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2002.dta" ///
               "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2003.dta" ///
			   "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2004.dta" ///
	           "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2005.dta" ///
			   "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2006.dta" ///
               "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2007.dta" ///
			   "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2008.dta" ///
	           "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2009.dta" ///
			   "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2011.dta" ///
               "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2012.dta" ///
			   "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\2013.dta" ///
	
*************************************************************************************************************









******************************************************************************************************
*2. Processing of Pooled dataset, Merging PNAD data with state-level value-added data
******************************************************************************************************

*Rename varibles for clarity
rename V0101 year
rename V8005 age

	  *Save pooled data to folder*
	save "C:\Users\ekato\Documents\Fulbright Research 2016\PNAD\Data Pooling\Pooled1996_2014.dta", replace		   
			   
***************************************************		  
*All data now pooled
***************************************************

*Check that data have been pooled correctly
summarize year 
summarize UF    
summarize age  
summarize occupation 
summarize weeklyhrs  
summarize obsweight  
summarize lnrenda 
summarize anosestudo
summarize mulher
summarize raca
summarize rural 
summarize advanced

*********************************************************
*Begin data processing for regressions models
*********************************************************

*Next, divide renda/month by hours/month
generate rendahora=renda/monthlyhrs

*Finally, return renda to its logged form
generate lnrendahr=ln(rendahora)

*Check results of calculation
summarize renda
summarize rendahora
summarize lnrendahr

*Then, add data on minimum wage
generate salminimo=.
replace salminimo=339 if year==96
replace salminimo=348 if year==97
replace salminimo=366 if year==98
replace salminimo=360 if year==99
replace salminimo=414 if year==2001
replace salminimo=419 if year==2002
replace salminimo=430 if year==2003
replace salminimo=440 if year==2004
replace salminimo=483 if year==2005
replace salminimo=548 if year==2006
replace salminimo=568 if year==2007
replace salminimo=579 if year==2008
replace salminimo=622 if year==2009
replace salminimo=648 if year==2011
replace salminimo=700 if year==2012
replace salminimo=722 if year==2013
replace salminimo=724 if year==2014

*Create time variable (non-dummy)
generate time=.
replace time=0 if year==96
replace time=1 if year==97
replace time=2 if year==98
replace time=3 if year==99
replace time=4 if year==2001
replace time=5 if year==2002
replace time=6 if year==2003
replace time=7 if year==2004
replace time=8 if year==2005
replace time=9 if year==2006
replace time=10 if year==2007
replace time=11 if year==2008
replace time=12 if year==2009
replace time=13 if year==2011
replace time=14 if year==2012
replace time=15 if year==2013
replace time=16 if year==2014

*Now create binary age variable agegroup
generate agegroup=.
replace agegroup=1 if age<20
replace agegroup=2 if age>=20 & age<30
replace agegroup=3 if age>=30 & age<40
replace agegroup=4 if age>=40 & age<50
replace agegroup=5 if age>=50 & age<60
replace agegroup=6 if age>=60

************************************************************************************
*Merging State-level Value Added data with hours-worked data from PNAD to calculate 
*state-level productivity, formalization, organization*
************************************************************************************

*Upload PNAD data file
use "C:\Users\ekato\Documents\Fulbright Research 2016\Data and Code\Regression Data with sindicate formal and state productivity 11-21.dta", clear

*Use Collapse to calculate means and sums of sindicate, formal, and state-level total hours
collapse (sum) annualhrs_sum=annualhrs (mean) formal_mean=formal (mean) sindicate_mean=sindicate, by(year UF economicsector)
save "C:\Users\ekato\Documents\Fulbright Research 2016\Data and Code\collapsedata.dta", replace

*Upload data file for regressions
use "C:\Users\ekato\Documents\Fulbright Research 2016\Data and Code\Regression Data with sindicate formal and state productivity 11-21.dta", clear
drop _merge

*Merge collapsed data with individual PNAD dataset
merge m:1 year  UF economicsector using "C:\Users\ekato\Documents\Fulbright Research 2016\Data and Code\collapsedata.dta"

*process data to correct for errors
drop stateprod
drop lnstateprod
generate realvalor=valor*1000000
generate totalannualhrs = annualhrs_sum*obsweight
generate lnsalminimo=ln(salminimo)

*Now calculate state-level productivity
generate stateprod = realvalor/totalannualhrs
generate lnstateprod = ln(stateprod)

***************************************************************************************************************
save "C:\Users\ekato\Documents\Fulbright Research 2016\Data and Code\final regression data 11-30.dta", replace
***************************************************************************************************************

*^^^^^^This is the final pooled, cleaned, and processed dataset, ready for regression analysis^^^^^^*

****************************************************************************************************************




******************************************************************************************
*3. Regression analysis of pooled dataset
*******************************************************************************************

*Use saved pooled dataset with sindicate, formal, and state-level productivity values
use "C:\Users\ekato\Documents\Fulbright Research 2016\Data and Code\final regression data 11-30.dta", replace

*Basic log-linear regression model 
regress lnrendahr anosestudo lnstateprod mulher raca i.agegroup i.UF i.year, robust
estimates store reg1

*Extended regression controlling for UF
regress lnrendahr anosestudo lnstateprod mulher raca  i.agegroup i.UF lnsalminimo, robust
estimates store reg2

*Extended regression controlling for year, UF, and economic sector
regress lnrendahr anosestudo lnstateprod mulher raca  i.agegroup sindicate_mean formal_mean i.UF lnsalminimo, robust
estimates store reg3


*Produce individual regression results table
estimates table reg1 reg2 reg3, b se


***********************************************************************************************
*Now calculate regressions for each sector, individually
***********************************************************************************************

*Sector 1: Agriculture
regress lnrendahr anosestudo lnstateprod  mulher raca i.agegroup sindicate_mean formal_mean i.UF i.year if economicsector==1, robust
estimates store reg7

*Sector 3: Industry
regress lnrendahr anosestudo lnstateprod  mulher raca i.agegroup sindicate_mean formal_mean i.UF i.year if economicsector==3 , robust
estimates store reg8

*Sector 5: Construction
regress lnrendahr anosestudo lnstateprod  mulher raca i.agegroup sindicate_mean formal_mean i.UF i.year if economicsector==5 , robust
estimates store reg9

*Sector 6: Commerce
regress lnrendahr anosestudo lnstateprod  mulher raca i.agegroup sindicate_mean formal_mean i.UF i.year if economicsector==6 , robust
estimates store reg10

*Sector 9: Financial and Information Services
regress lnrendahr anosestudo lnstateprod  mulher raca i.agegroup sindicate_mean formal_mean i.UF i.year if economicsector==9 , robust
estimates store reg12

*Sector 12: Public Administration
regress lnrendahr anosestudo lnstateprod  mulher raca i.agegroup sindicate_mean i.UF i.year if economicsector==12, robust
estimates store reg14

*Sector 11: Real Estate and Other Services
regress lnrendahr anosestudo lnstateprod  mulher raca i.agegroup sindicate_mean formal_mean i.UF i.year if economicsector==11, robust
estimates store reg13


*Produce individual regression results table
estimates table reg7 reg8 reg9 reg10 reg12 reg13 reg14, b se


******************************************************************************************************************************************










*********************************************************
*4. Create Table of Descriptive Statistics
*********************************************************

*Variables for table*
*Labor income, years of study, gender, race, age, state-level productivity, min. wage, % sindicate, % formal*

*Years: 1996, 2014*

*Labor Income*
summarize rendahora if year==96 
summarize rendahora if year==2014

*Years of study*
summarize anosestudo if year==96 
summarize anosestudo if year==2014

*Gender*
summarize mulher if year==96 
summarize mulher if year==2014

*Race*
summarize raca if year==96 
summarize raca if year==2014

*Age*
summarize age if year==96 
summarize age if year==2014

*State-level productivity*
summarize productivity if year==96 
summarize productivity if year==2014

*Real minimum wage*
summarize salminimo if year==96 
summarize salminimo if year==2014

*% sindicate*
summarize sindpercent if year==96 
summarize sindpercent if year==2014

*% formal*
summarize formpercent if year==96 
summarize formpercent if year==2014


*******************************************************************************************************************
                                                       *End*
*******************************************************************************************************************




