libname OF "H:\OptimalFertility\Reference";



PROC IMPORT OUT=of.lfp_rate 
            DATAFILE='H:\OptimalFertility\Reference\lfp_rate.csv'
            DBMS=csv REPLACE;
        GETNAMES=YES;
        guessingrows=100000;
RUN;

ods noresults;

%macro Proj(styr,endyr);
ods listing close;
/*Immigration*/
proc sort data=of.pop&styr; by year sex agest edu region; run;
proc sort data=of.immig; by year sex agest edu region; run;
data work.immig_event;
merge of.pop&styr of.immig;
by year sex agest edu region;
if immig=. then immig=0;
if year ne &styr then delete;
run;

/*Emigration*/
proc sort data=work.immig_event; by year sex agest region; run;
proc sort data=of.emig; by year sex agest region; run;
data work.emig_event;
merge work.immig_event(in=in1) of.emig;
by year sex agest region;
if in1;
emig=emig_rate*pop;
run;

/*Mortality*/
proc sort data=work.emig_event; by  region year agest sex edu; run;
proc sort data=of.sx; by  region year agest sex edu; run;
data work.mort_event;
merge work.emig_event(in=in1) of.sx;
by region year agest sex edu; 
if in1;
death=pop*(1-sx);
pop_end=pop+immig-emig-death;
agest=agest+5;
run;

/*Fertility*/
data work.fert_beg;
set work.mort_event;
pop_beg=pop;
agest=agest-5;
keep year agest sex region edu pop_beg;
run;

proc sort data=work.mort_event; by year agest sex region edu; run;
proc sort data=work.fert_beg; by year agest sex region edu; run;
data work.fert_event;
merge work.mort_event(in=in1) work.fert_beg;
by year agest sex region edu;
if in1;
drop birth;
run;

proc sort data=work.fert_event; by year agest sex region; run;
proc sort data=of.fert_sched; by year agest sex region; run;
data work.fert_event;
merge work.fert_event(in=in1) of.fert_sched;
by year agest sex region ;
if in1;
run;


proc sort data=work.fert_event; by year region; run;
proc sort data=of.srb; by year region; run;
data work.fert_event;
merge work.fert_event(in=in1) of.srb;
by year  region ;
if in1;
birth_f=(&TFR/10)*(1/(SRB+1))*fert_sched*((pop_beg+pop_end)/2);
birth_m=(&TFR/10)*(SRB/(SRB+1))*fert_sched*((pop_beg+pop_end)/2);
run;


proc tabulate data=work.fert_event out=work.birthyear;
var birth_f birth_m;
class region;
TABLE region,SUM*(birth_f birth_m);
run;

proc transpose data=work.birthyear out=work.birthyear;by region; run;

data work.birthyear;
set work.birthyear;
agest=-5;
year=&styr;
birth=col1;
if _NAME_ in ("_PAGE_" "_TABLE_") then delete;
if _NAME_ = "birth_f_Sum" then sex='f';
if _NAME_ = "birth_m_Sum" then sex='m';
edu='e1';

drop  _NAME_ _LABEL_ col1;
run;

	/*Baby survival*/
	proc sort data=work.birthyear; by  year agest sex edu; run;
	proc sort data=of.sx; by  year agest sex edu; run;
	data work.birthyear;
	merge work.birthyear(in=in1) of.sx;
	if in1;
	by year agest sex edu; 
	pop_end=birth*sx;
	death=birth-birth*sx;
	agest=agest+5;
	run;


/*Year progress*/
proc sort data=work.birthyear; by  year region agest sex edu; run;
proc sort data=work.fert_event; by  year region agest sex edu; run;
data work.pop_progress;
merge work.fert_event work.birthyear;
by year region agest sex edu;
year=year+5;

drop sx fert_sched birth_f birth_m srb;
run; 

/*Education progress*/
proc tabulate data=work.pop_progress out=work.popcount;
var pop_end;
class year region agest sex;
table year*agest*sex*region,pop_end*(sum);
where 15<=agest<=30;
run;


proc sort data=work.popcount; by year region agest sex;run;
proc sort data=of.edu; by year region agest sex;run;
data work.pop_edu15_30;
merge work.popcount(in=in1) of.edu;
by year region agest sex;
if in1;
if 15<=agest<=30 then do;
e1=pop_end_sum*pe1;
e2=pop_end_sum*pe2;
e3=pop_end_sum*pe3;
e4=pop_end_sum*pe4;
e5=pop_end_sum*pe5;
e6=pop_end_sum*pe6;
end;
keep e1-e6 region year sex agest;
run;

proc sort data=work.pop_edu15_30; by year region agest sex;run;
proc transpose data=work.pop_edu15_30 out=work.pop_edu15_30; by year region agest sex;run;

data work.pop_edu15_30;
set work.pop_edu15_30;
edu=_NAME_;
pop_adj=COL1;
if col1=. then delete;
drop _NAME_ COL1;
run;


proc sort data=work.pop_edu15_30; by year agest sex edu; run;
proc sort data=work.pop_progress; by year agest sex edu; run;
data of.pop&endyr;
merge work.pop_progress work.pop_edu15_30;
by year agest sex edu;
pop=pop_end;
if 15<=agest<=30 then pop=pop_adj;
if agest>120 then delete;
drop immig pop_adj emig_rate;
run;



/*Clean temporary dataset*/
proc datasets lib=work kill nolist memtype=data;
quit;

DM log "OUT;CLEAR;LOG;CLEAR;";

%mend Proj;


%macro loop(first,last);
   %local year;
   %do year = &first %to &last %by 5;
      %proj (&year, %eval(&year+5))
   %end;

%mend loop;



%macro run_proj_for_tfr(tfr_values);
   %local tfr;
   /* Loop through the list of TFR values */
   %do i = 1 %to %sysfunc(countw(&tfr_values));
      %let TFR = %scan(&tfr_values, &i); /* Get each TFR value */
      %put Running projection for TFR=&TFR;

      /* Run the original projection for each TFR value */
      %loop(2020, 2095);

	  /*Computation of dependency ratios*/
		data of.completepop;
		set of.pop:;
		AgeDep=0;
		if agest<15 then AgeDep=1;
		if agest>=65 then AgeDep=1;
		run;

%let nbedup=0.2371; 
%let nbedu_meanp=-0.00873; 
%let world_edu=9.3;
%let world_saf=0.664294573;

		proc sort data=of.completepop; by year region; run;
		proc sort data=of.saf; by year region; run;
		data of.completepop;
		merge of.completepop(in=in1) of.saf;
		if in1;
		by year region;
		if edu in ('e1' 'e2') then nbedu=1;
		if edu in ('e3') then nbedu=6;
		if edu in ('e4') then nbedu=9;
		if edu in ('e5') then nbedu=12;
		if edu in ('e6') then nbedu=16;
		run;

		proc tabulate data=of.completepop out=work.nbeduaverage (keep=region year nbedu_mean);
		var nbedu;
		class region year;
		table region*year,nbedu*(mean);
		where 25<=agest<=54;
		freq pop;
		run;

		proc sort data=of.completepop; by agest sex edu region; run;
		proc sort data=of.lfp_rate; by agest sex edu region; run;
		data of.completepop;
		merge of.completepop(in=in1) of.lfp_rate;
		by agest sex edu region;
		active=pop*lf_rate;
		inactive=pop-active;
		if lf_rate=. then inactive=pop;
		run;

		proc sort data=work.nbeduaverage; by region year;run;
		proc sort data=of.completepop; by region year;run;
		data of.completepop;
		merge of.completepop (in=in1) work.nbeduaverage;
		by region year;
		if in1;
		normalizedsaf=saf/&world_saf;
		w=exp(&nbedup*nbedu+&nbedu_meanp*nbedu*nbedu_mean) /exp(&nbedup*&world_edu+&nbedu_meanp*&world_edu*nbedu_mean)*normalizedsaf;
		act_weight=active*w;
		run;


     /* Save the output for the current TFR */

data of.completepop_&TFR;
set of.completepop;
EU=1; if region in ("CN" "IN" "IR" "TH" "US") then EU=0;
run;

proc tabulate data=of.completepop_&TFR out=work.sumlf;
class year region eu;
var inactive active act_weight;
table year*region,inactive*(sum) active*(sum) act_weight*(sum);
table year*eu,inactive*(sum) active*(sum) act_weight*(sum);
run;

data work.sumlf; set work.sumlf;
if region="" and EU=1 then region="EU";
if EU=0 then delete;
drop EU;
run;


proc sort data=work.sumlf; by region;run;
proc transpose data=work.sumlf out=work.sumlf2; by region; id year; var active_sum inactive_sum act_weight_sum;
run;


proc tabulate data=of.completepop_&TFR out=work.sumage;
class year EU AgeDep region;
var pop;
table year*region,ageDep*pop*(sum);
table year*EU,ageDep*pop*(sum);
run;

data work.sumage; set work.sumage;
if region="" and EU=1 then region="EU";
if EU=0 then delete;
drop EU;
run;

proc sort data=work.sumage; by region agedep;run;
proc transpose data=work.sumage out=work.sumage2; by region agedep; id year; var pop_sum;
run;

data work.sumpop;
set work.sumlf2 work.sumage2;
if agedep=0 then _NAME_="agesup";
if agedep=1 then _NAME_="agedep";

    array orig_vars[*] _2020 _2025 _2030 _2035 _2040 _2045 _2050 _2055 _2060 _2065 
                    _2070 _2075 _2080 _2085 _2090 _2095 _2100/* _2105 _2110 _2115 
                    _2120 _2125 _2130 _2135 _2140 _2145 _2150*/;

    array cum_vars[*] cum2025 cum2030 cum2035 cum2040 cum2045 cum2050 cum2055 cum2060 
                  cum2065 cum2070 cum2075 cum2080 cum2085 cum2090 cum2095 cum2100 
                  /*cum2105 cum2110 cum2115 cum2120 cum2125 cum2130 cum2135 cum2140 
                  cum2145 cum2150*/;


    /* Initialize the first cumulative value */
    cum_vars[1] = (orig_vars[1] + orig_vars[2]) / 2;

    /* Loop to calculate cumulative values incrementally */
    do i = 2 to dim(cum_vars);
        cum_vars[i] = cum_vars[i-1] + (orig_vars[i] + orig_vars[i+1]) / 2;
    end;

    drop i agedep;

run;

proc sort data=work.sumpop; by region;run;
proc transpose data=work.sumpop out=work.poptranspose; by region; run;

data work.poptranspose2; set work.poptranspose;
	if substr(_name_, 1, 3) = 'cum' then type = 'cumul';
    else type = 'trad';

    /* Step 2: Extract numerical part and store in "year" */
    year = input(substr(_name_, length(_name_) - 3, 4), 8.);

adr=agedep/agesup;
lfdr=inactive_sum/active_sum;
pwlfdr=inactive_sum/act_weight_sum;

keep region year adr lfdr pwlfdr type ;
run;



data trad_data; set work.poptranspose2;
    where type = 'trad';
	drop type;
run;

data cumul_data; set work.poptranspose2;
    where type = 'cumul';
    cumadr=adr; cumlfdr=lfdr; cumpwlfdr=pwlfdr;
	drop adr lfdr pwlfdr type;
run;

proc sort data=trad_data; by region year; run;
proc sort data=cumul_data; by region year; run;
data of.depratio_&TFR;
    merge trad_data cumul_data;
    by region year;
	TFR=&TFR/10;
	duration=year-2020;
	drop year;
run;

   %end;
%mend run_proj_for_tfr;

/* Example call with a list of TFR values */
%run_proj_for_tfr(/*1 2 3 4*/ 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40);



data of.depratiocompiled;
set of.depratio_:;
run;

      proc export data=of.depratiocompiled
         outfile="H:\OptimalFertility\depratiocompiled.csv"
         dbms=csv
         replace;
      run;

proc sort data=of.depratiocompiled; by duration region TFR; run;
proc transpose data=of.depratiocompiled out=work.drtranpose;
by duration region;
id TFR;
run;

data of.optimal_fert;
    set work.drtranpose;
    
    /* Create an array for the columns */
    array num_vars {*} _0D5--_4;
    
    /* Initialize variables to track the minimum value and the corresponding column name */
    min_value = num_vars[1]; /* Start with the first value */
    min_var_name = vname(num_vars[1]); /* Start with the first variable's name */
    
    /* Loop through the array to find the minimum value and corresponding variable name */
    do i = 2 to dim(num_vars);
        if num_vars[i] < min_value then do;
            min_value = num_vars[i];
            min_var_name = vname(num_vars[i]); /* Store the name of the variable */
        end;
    end;
    
    /* Drop unnecessary temporary variables */
    drop i min_value;
run;


      proc export data=of.optimal_fert
         outfile="H:\OptimalFertility\Reference\optimal_fert.csv"
         dbms=csv
         replace;
      run;

/*Population count*/

%macro pop(TFR);
proc tabulate data=of.completepop_&TFR out=work.pop&TFR;
var pop;
class region year EU;
table year,region*pop*(sum);
table year,EU*pop*(sum);
run;

data work.pop&TFR;
set work.pop&TFR;
if EU=1 then region='EU';
TFR=&TFR/10;
if region not in ('EU' 'US' 'CN') then delete;
drop _TABLE_ _PAGE_ _TYPE_ EU;
run;

%mend;

%pop(1);	%pop(2);	%pop(3);	%pop(4);	%pop(5);	%pop(6);	%pop(7);	%pop(8);	%pop(9);	%pop(10);	%pop(11);	%pop(12);	%pop(13);	%pop(14);	%pop(15);	%pop(16);	%pop(17);	%pop(18);	%pop(19);	%pop(20);	%pop(21);	%pop(22);	%pop(23);	%pop(24);	%pop(25);	%pop(26);	%pop(27);	%pop(28);	%pop(29);	%pop(30);	%pop(31);	%pop(32);	%pop(33);	%pop(34);	%pop(35);	%pop(36);	%pop(37);	%pop(38);	%pop(39);	%pop(40);

data pop;
set pop:;
run;

proc tabulate data=pop out=of.tot_pop;
var pop_sum;
class region year TFR;
table region*year,TFR*pop_sum*(sum);
run;

      proc export data=of.tot_pop
         outfile="H:\OptimalFertility\Reference\pop_TFR.csv"
         dbms=csv
         replace;
      run;


	  proc export data=of.Completepop_12
         outfile="H:\OptimalFertility\Reference\completepop_12.csv"
         dbms=csv
         replace;
      run;
	  proc export data=of.Completepop_22
         outfile="H:\OptimalFertility\Reference\completepop_22.csv"
         dbms=csv
         replace;
      run;
