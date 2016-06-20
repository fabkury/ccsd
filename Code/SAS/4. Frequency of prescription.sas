%LET _CLIENTTASKLABEL='Frequency of prescription';
%LET _CLIENTPROJECTPATH='\\ccwdata.org\Profiles\fku838\Documents\Projects\CCSD\CCSD.egp';
%LET _CLIENTPROJECTNAME='CCSD.egp';
%LET _SASPROGRAMFILE=;

%let debug_mode = 0;
%let debug_threshold = 1000000;
%let drop_tables = 1;

%let percent_beneficiaries_precision = 0.0001; /* Used for writing the numbers
 to the finalized tables. */

%let do_add_info = 0;
%let do_aggregate_by_class = 0;
%let do_assemble_xlsx = 1;

%let year_list = 9;
%let min_discontinuous_days_list = /*456*/ 0;

%let pdelib = IN026250;
%let pdereq = R3632;
%let userlib = FKU838SL;
%let proj_cn = CCSD;
%let total_benef_suffix = TB;
%let class_list = SATC1 SATC2 SATC3 SATC4 SATC5 SEPC SVAL1 SVAL2 BEER /*RXNI*/; /* CHEM PE MOA;*/

%let freq_of_p_table_pf = &userlib..&proj_cn._PR;
%let tot_benef_table_pf = &userlib..&proj_cn._PR;

%let min_beneficiaries = 11; /* CMS imposes that no cell can refer
 to less than 11 beneficiaries. */
%let minimum_age = 65;
%let all_age_groups_label = %str('All 65+');
%let age_groups_query = %str(when BENE_AGE_AT_END_REF_YR < 65 then '0 - 64'
		when BENE_AGE_AT_END_REF_YR < 75 then '65 - 74'
		when BENE_AGE_AT_END_REF_YR < 85 then '75 - 84'
		when BENE_AGE_AT_END_REF_YR < 95 then '85 - 94'
		else '95+');

%let wide_fp_precision = 0.001; /* Used to avoid comparison errors due to
 numerical imprecision in small numbers. */

%macro add_class_and_age_groups;
proc sql;
create table ACAG_RET&sfx1 as
select PDE_ID as MEID, a.BENE_ID, case &age_groups_query end as AGE_GROUP, ClassID,
	MAX(SRVC_DT, &date_truncation_begin) as SRVC_DT,
	MIN(SRVC_DT+(DAYS_SUPLY_NUM-1), &date_truncation_end) as SRVC_DT_END,
	CATX('-', a.BENE_ID, ClassID) as merge_id, BENE_AGE_AT_END_REF_YR
from &pdefile a, BENE_CC.MBSF_AB_%eval(2000+&y) b, SH026250.NDC_TO_&class c
/* Question: if I change this line below to a HAVING clause using the SRVC_DT_END,
 therefore avoiding writing its formula twice (which is good), will the code lose
 efficiency? */
where SRVC_DT+(DAYS_SUPLY_NUM-1) >= &date_truncation_begin
%if &debug_mode %then and a.BENE_ID < &debug_threshold;
	and a.BENE_ID = b.BENE_ID
	and BENE_AGE_AT_END_REF_YR >= &minimum_age
	and PROD_SRVC_ID = NDC
	and DAYS_SUPLY_NUM > 0
order by merge_id, SRVC_DT, SRVC_DT_END;
quit;
%mend;


%macro aggregate_by_class;
/* Aggregate at patient-class level */

/* First, merge the intervals of the same beneficiary and class that
 cover the same days of the year. */
data AC_SUB_A&sfx2;
	set ACAG_RET&sfx1;
	by merge_id SRVC_DT SRVC_DT_END;
	retain curstart curend curmeid;
	if first.merge_id then
		do;
			curend=.;
			curstart=.;
			curmeid=.;
		end;
	if SRVC_DT > curend then
		do;
			if not (first.merge_id) then output;
			curstart = SRVC_DT;
			curend = SRVC_DT_END;
			curmeid = MEID;
		end;
	else if SRVC_DT_END >= curend then /* The use of ">=" rather than
	 only ">" is to force curmeid to include all MEIDs. */
		do;
			curend = SRVC_DT_END;
			curmeid = mean(curmeid, MEID);
		end;
	if last.merge_id then output;
run;

data AC_A&sfx2;
	set AC_SUB_A&sfx2;
	DURATION = (curend-curstart)+1;
run;

%if &drop_tables %then
	%do;
	proc sql;
	drop table AC_SUB_A&sfx2;
	quit;
	%end;

proc sql;
alter table AC_A&sfx2
drop column merge_id, SRVC_DT, SRVC_DT_END, MEID;

create index GroupByIndex
on AC_A&sfx2 (AGE_GROUP, ClassID);
quit;

proc datasets lib=WORK nodetails nolist;
modify AC_A&sfx2;
rename curmeid=MEID curstart=SRVC_DT curend=SRVC_DT_END;
run;

/* Aggregate by age group and class ID */
ods select none;
proc tabulate
	data = AC_A&sfx2
	out = AC_C&sfx2;
class AGE_GROUP ClassID;
var DURATION;
table DURATION*(SUM MEAN STD MEDIAN MODE MIN MAX)*ClassID*AGE_GROUP;
run;

/* Aggregate by class ID only */
proc tabulate
	data = AC_A&sfx2
	out = AC_D&sfx2;
class ClassID;
var DURATION;
table DURATION*(SUM MEAN STD MEDIAN MODE MIN MAX)*ClassID;
run;
ods select all;

/* Calculate the total number of beneficiaries for the purpose of calculating the
 concomitant medication index. */
%let e = %str(count(unique(BENE_ID)) as Beneficiaries,
	sum(DURATION) as 'Sum duration'n, max(DURATION) as 'Max duration'n, min(DURATION) as 'Min duration'n,
	round(mean(DURATION), &wide_fp_precision) as 'Avg. duration'n,
	round(std(DURATION), &wide_fp_precision) as 'Std. duration'n,
	round(mean(BENE_AGE_AT_END_REF_YR), &wide_fp_precision) as 'Avg. age'n,
	round(std(BENE_AGE_AT_END_REF_YR), &wide_fp_precision) as 'Std. age'n);

proc sql;
create table &tot_benef_table as
select a.'Age group'n, a.Beneficiaries,
	round(a.Beneficiaries*100/b.Beneficiaries, &percent_beneficiaries_precision) as '% age group'n,
	'Sum duration'n, 'Max duration'n, 'Min duration'n, 'Avg. duration'n, 'Std. duration'n, 'Avg. age'n, 'Std. age'n
from (select AGE_GROUP as 'Age group'n, &e
	from AC_A&sfx2
	/*where ClassID in (select ClassID from AC_B&sfx2)*/
	group by 'Age group'n
	having Beneficiaries >= &min_beneficiaries
	union all
	select &all_age_groups_label as 'Age group'n, &e
	from AC_A&sfx2
	/*where ClassID in (select ClassID from AC_B&sfx2)*/
	having Beneficiaries >= &min_beneficiaries) a, &age_groups_table b
where a.'Age group'n = b.'Age group'n
order by 'Age group'n desc;
quit;

/* Add the SAS results to the table created by SQL */
%let e1 = %str(count(unique(BENE_ID)) as Beneficiaries,
	round(mean(BENE_AGE_AT_END_REF_YR), &wide_fp_precision) as AVG_AGE,
	round(std(BENE_AGE_AT_END_REF_YR), &wide_fp_precision) as STD_AGE);

%let e2 = %str(DURATION_Max as MAX_DURATION, DURATION_Min as MIN_DURATION,
	DURATION_Median as MEDIAN_DURATION, DURATION_Mode as MODE_DURATION,
	DURATION_Sum as SUM_DURATION,
	round(DURATION_Mean, &wide_fp_precision) as AVG_DURATION,
	round(DURATION_Std, &wide_fp_precision) as STD_DURATION);

proc sql;
create table AC_B&sfx2 as
select a.*, &e2
from (select AGE_GROUP, ClassID, &e1
	from AC_A&sfx2 group by AGE_GROUP, ClassID
	having Beneficiaries >= &min_beneficiaries) a, AC_C&sfx2 b
where a.AGE_GROUP = b.AGE_GROUP and a.ClassID = b.ClassID
union all
select a.*, &e2
from (select &all_age_groups_label as AGE_GROUP, ClassID, &e1
	from AC_A&sfx2 group by ClassID
	having Beneficiaries >= &min_beneficiaries) a, AC_D&sfx2 b
where a.ClassID = b.ClassID;

%if &drop_tables %then drop table AC_A&sfx2, AC_C&sfx2, AC_D&sfx2; ;
quit;

/* Add class names */
proc sql;
create table &freq_of_p_table as
select a.AGE_GROUP as 'Age group'n, a.ClassID as "&verbose_class code"n, ClassName as "&verbose_class name"n,
	a.Beneficiaries, round(a.Beneficiaries*100/c.Beneficiaries, &percent_beneficiaries_precision) as '% age group'n,
	SUM_DURATION as 'Sum duration'n, MAX_DURATION as 'Max duration'n,
	MIN_DURATION as 'Min duration'n, AVG_DURATION as 'Avg. duration'n, STD_DURATION as 'Std. duration'n,
	MEDIAN_DURATION as 'Median duration'n, MODE_DURATION as 'Mode duration'n,
	AVG_AGE as 'Avg. age'n, STD_AGE as 'Std. age'n
from AC_B&sfx2 a, (select distinct ClassID, ClassName from SH026250.NDC_TO_&class) b, &age_groups_table c
where a.ClassID = b.ClassID and c.'Age group'n = a.AGE_GROUP
order by 'Age group'n desc, Beneficiaries desc, 'Sum duration'n desc;

%if &drop_tables %then drop table AC_B&sfx2; ;
quit;
%mend;


%macro assemble_xlsx;
proc export
	data=&freq_of_p_table
	dbms=xlsx replace
	outfile=&outfile;
	sheet="&sheet_name";
run;

proc export
	data=&tot_benef_table
	dbms=xlsx replace
	outfile=&outfile_summary;
	sheet="&sheet_name";
run;
%mend;


%macro make_tables;
/* di = debug identifier */
%if &debug_mode %then %let di = di;
%else %let di =;

%do YL=1 %to %sysfunc(countw(&year_list));
	%let y = %scan(&year_list, &YL);
	%let date_truncation_begin = MDY(2, 1, &y);
	%let date_truncation_end = MDY(12, 31, &y);

	%let age_groups_table = &userlib..&proj_cn._AGE_GROUPS_&y;
	%if &y < 12 %then %let pdefile = &pdelib..PDESAF%sysfunc(putn(&y, z2))_&pdereq;
		%else %let pdefile = &pdelib..PDE&y._&pdereq;

	%if &do_assemble_xlsx %then
		%do;
		%let outfile = "&myfiles_root./&proj_cn. Frequency of prescription %eval(2000+&y)";
		%put %sysfunc(fdelete(&outfile));
		%let outfile_summary = "&myfiles_root./&proj_cn. Frequency of prescription %eval(2000+&y) (summary)";
		%put %sysfunc(fdelete(&outfile_summary));
		%end;

	%do I=1 %to %sysfunc(countw(&class_list));
		%let class = %scan(&class_list, &I);
		%do n=1 %to 5;
			%if &class = SATC&n %then %let verbose_class = %str(ATC &n);
			%if &class = SATC&n %then %let sheet_name = %str(ATC &n);
		%end;
		%if &class = SEPC %then %let verbose_class = %str(EPC);
		%if &class = SEPC %then %let sheet_name = %str(EPC);
		%if &class = SVAL1 %then %let verbose_class = %str(VA L1);
		%if &class = SVAL1 %then %let sheet_name = %str(VA L1);
		%if &class = SVAL2 %then %let verbose_class = %str(VA L2);
		%if &class = SVAL2 %then %let sheet_name = %str(VA L2);
		%if &class = RXNI %then %let verbose_class = %str(RxN Ingredient);
		%if &class = RXNI %then %let sheet_name = %str(ONC DDI);
		%if &class = BEER %then %let verbose_class = %str(RxN Ingredient);
		%if &class = BEER %then %let sheet_name = %str(Beers list);
	
		%let sfx1 = _&y._&class.&di; /* Suffix */
		%if &do_add_info %then %add_class_and_age_groups;
		%do MDDL=1 %to %sysfunc(countw(&min_discontinuous_days_list));
			%let min_discontinuous_days = %scan(&min_discontinuous_days_list, &MDDL);
			%let sfx2 = _&y._&class._&min_discontinuous_days.&di; /* Suffix */
			
			%let freq_of_p_table = &freq_of_p_table_pf.&sfx2;
			%let tot_benef_table = &freq_of_p_table._&total_benef_suffix;
			%if &do_aggregate_by_class %then %aggregate_by_class;
			%if &do_assemble_xlsx %then %assemble_xlsx;
		%end;

		/* Clean mess left behind. */
		%if &drop_tables %then
			%do;
			proc sql;
			drop table ACAG_RET&sfx1;
			quit;
			%end;
	%end;
%end;
%mend;

%make_tables;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
