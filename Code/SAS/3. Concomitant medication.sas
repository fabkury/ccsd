%LET _CLIENTTASKLABEL='Concomitant medication';
%LET _CLIENTPROJECTPATH='\\ccwdata.org\Profiles\fku838\Documents\Projects\CCSD\CCSD.egp';
%LET _CLIENTPROJECTNAME='CCSD.egp';
%LET _SASPROGRAMFILE=;

%let debug_mode = 0;
%let debug_threshold = 100000;
%let drop_tables = 1;
%let min_discontinuous_days_list = /*456*/ 21 /*14 0*/;
%let do_onc_ddi_list = 0 1;
%let year_list = 9 /*12 6*/;
%let dd_drug_class_list = SATC1 SATC2 SATC3 SATC4 SATC5 SVAL1 SVAL2 SEPC;
%let hd_drug_class_list = RXNI;

%let wide_fp_precision = 0.001; /* Used to avoid comparison errors due to
 numerical imprecision in small numbers. */
%let short_fp_precision = 0.01; /* Used to avoid comparison errors due to
 numerical imprecision in large numbers. */
%let percent_beneficiaries_precision = 0.0001; /* Used for writing the numbers
 to the finalized tables. */

%let do_make_overlaps_table = 0;
%let do_merge_overlaps = 0;
%let do_make_dc_overlaps_table = 0;
%let do_aggregate_class_pairs = 0;
%let do_make_finalized_tables = 0;
%let do_assemble_xlsx = 1;

%let userlib = FKU838SL;
%let pdelib = IN026250;
%let pde_suffix = _R3632;
%let proj_cn = CCSD; /* Project codename */
%let data_driven_infix = ALL_;
%let hypothesis_driven_infix = ONC_;

%let overlaps_table_pf = &userlib..&proj_cn._C_OL_;
%let merged_overlaps_table_pf = &userlib..&proj_cn._MG_OL_;
%let dc_overlaps_table_pf = &userlib..&proj_cn._DC_OL_; /* "Discontinuous Overlaps" table */
%let summary_table_pf = &userlib..&proj_cn._SUMMARY_;
%let aggregated_pairs_table_pf = &userlib..&proj_cn._AG_;
%let finalized_table_pf = &userlib..&proj_cn._F_;
%let freq_of_prescription_table_pf = &userlib..&proj_cn._PR_;
%let assembled_xlsx_pf = &userlib..&proj_cn._A_;
/*%let sex_groups_table = &userlib..&proj_cn._SEX_GROUPS;*/

%let min_beneficiaries = 11; /* CMS imposes that no cell can refer
 to less than 11 beneficiaries. */
%let minimum_age = 65;
%let all_age_groups_label = %str('All 65+');
%let age_groups_query = %str(when BENE_AGE_AT_END_REF_YR < 65 then '0 - 64'
	when BENE_AGE_AT_END_REF_YR < 75 then '65 - 74'
	when BENE_AGE_AT_END_REF_YR < 85 then '75 - 84'
	when BENE_AGE_AT_END_REF_YR < 95 then '85 - 94'
	else '95+');

%macro make_overlaps_table;
/* Do the initial identification of overlaps. */
proc sql;
create table MOT_SUB_A as
select distinct NDC, RxNIngredientCUI %if &do_onc_ddi %then as ClassID; %else, ClassID;, ClassName
from SH026250.NDC_TO_&class;

create index NDC
on MOT_SUB_A (NDC);

/* Table MOT_A is temporary table based on the PDE file. MOT_A will receive an important
 index, which we cannot do with the PDE file. */
create table MOT_A as
select PDE_ID as MEID, BENE_ID, PROD_SRVC_ID, ClassID, ClassName, /*GNN,*/
	%if ^&do_onc_ddi %then RxNIngredientCUI,;
	MAX(SRVC_DT, &date_truncation_begin) as SRVC_DT,
	MIN(SRVC_DT+(DAYS_SUPLY_NUM-1), &date_truncation_end) as SRVC_DT_END /* The -1 imposes
 that the day the prescription is filled is already counting as a day of medication use, i.e.
 patients are considered	to begin taking their medications on the day the prescription is
 filled. The MIN(...,MDY()) truncates the durations to the last day of the year. */
from &pde_table a, MOT_SUB_A b %if &do_onc_ddi %then , SH026250.ONC_DDI_NDC_LIST c;
where SRVC_DT+(DAYS_SUPLY_NUM-1) >= &date_truncation_begin /* This line truncates the durations
 to after the start of the truncation period */
	and PROD_SRVC_ID = b.NDC
	%if &do_onc_ddi %then and PROD_SRVC_ID = c.NDC;
	%if &debug_mode %then and BENE_ID < &debug_threshold;;

drop table MOT_SUB_A;

create index OverlapIndex
on MOT_A (BENE_ID, SRVC_DT, SRVC_DT_END %if ^&do_onc_ddi %then, RxNIngredientCUI;);
quit;

/* The query belows does the actual identification of overlaps. */
/* We need to reorder the columns so that the DISTINCT clause works to prevent
 each overlap from appearing in two rows (A-B and B-A) instead of just one.
 In this case, we will use the column ordering that will be needed for the
 merging of the intervals, which has the added benefit of removing right in
 this query a few million rows that would be erased later anyway. */
%let e = %str(a.ClassID < b.ClassID);
proc sql;
create table %if &do_onc_ddi %then MOT_B; %else &overlaps_table; as
select distinct
	a.BENE_ID as Beneficiary,
	round(mean(a.MEID, b.MEID), &short_fp_precision) as MEID,
	case when a.SRVC_DT > b.SRVC_DT then a.SRVC_DT else b.SRVC_DT end as OVERLAP_BEGIN,
	case when a.SRVC_DT_END < b.SRVC_DT_END then a.SRVC_DT_END else b.SRVC_DT_END end as OVERLAP_END,

	%if &do_onc_ddi %then
		%do;
		case when &e then a.PROD_SRVC_ID else b.PROD_SRVC_ID end as aPROD_SRVC_ID,
		case when &e then b.PROD_SRVC_ID else a.PROD_SRVC_ID end as bPROD_SRVC_ID,
		%end;	

	case when &e then a.ClassID else b.ClassID end as aClass_ID,
	case when &e then b.ClassID else a.ClassID end as bClass_ID,			
	case when &e then a.ClassName else b.ClassName end as aClass_Name,
	case when &e then b.ClassName else a.ClassName end as bClass_Name,	
	case when &e then
		CATX('-', a.BENE_ID, a.ClassID, b.ClassID)
		else
		CATX('-', a.BENE_ID, b.ClassID, a.ClassID)
		end as merge_id
from MOT_A a, MOT_A b
where a.BENE_ID = b.BENE_ID %if &debug_mode %then and a.BENE_ID < &debug_threshold;
	and min(a.SRVC_DT_END, b.SRVC_DT_END) >= max(a.SRVC_DT, b.SRVC_DT) /* This line eliminates
	 impossible overlaps with negative durations. */
	and a.SRVC_DT >= b.SRVC_DT and a.SRVC_DT <= b.SRVC_DT_END /* This line identifies the overlaps. */
	and (%if ^&do_onc_ddi %then a.RxNIngredientCUI <> b.RxNIngredientCUI or; a.ClassID <> b.ClassID)
%if ^&do_onc_ddi %then order by merge_id, OVERLAP_BEGIN, OVERLAP_END;;

%if &drop_tables %then drop table MOT_A; ;
quit;

%if &do_onc_ddi %then
	%do;
		/* Reorder columns by NDC code to as to match the NDC_TO_ONC_DDI table. */
		proc sql;
		create index NDCIngredientPair
		on MOT_B (aClass_ID, bClass_ID, aPROD_SRVC_ID, bPROD_SRVC_ID);
		quit;

		proc sql;
		create table &overlaps_table as
		select Beneficiary, OVERLAP_BEGIN, OVERLAP_END, MEID, merge_id,
			aClass_ID, bClass_ID, aClass_Name, bClass_Name
		from MOT_B a, SH026250.NDC_TO_ONC_DDI b
		where aPROD_SRVC_ID = ndc1_id and bPROD_SRVC_ID = ndc2_id
			and aClass_ID = rxn_ingredient1_id and bClass_ID = rxn_ingredient2_id
		union all
		select Beneficiary, OVERLAP_BEGIN, OVERLAP_END, MEID, merge_id,
			aClass_ID, bClass_ID, aClass_Name, bClass_Name
		from MOT_B a, SH026250.NDC_TO_ONC_DDI b
		where bPROD_SRVC_ID = ndc1_id and aPROD_SRVC_ID = ndc2_id
			and bClass_ID = rxn_ingredient1_id and aClass_ID = rxn_ingredient2_id
 		order by merge_id, OVERLAP_BEGIN, OVERLAP_END;

		%if &drop_tables %then drop table MOT_B; ;
		quit;
	%end;
%mend;

%macro merge_overlaps;
/* Merge overlaps of the same merge_id (i.e. same beneficiary and same
 normalized pair or class ID) that cover the same days. */
data &merged_overlaps_table;
	set &overlaps_table;
	by merge_id OVERLAP_BEGIN OVERLAP_END;
	retain curstart curend curmeid;
	if first.merge_id then
		do;
			curend=.;
			curstart=.;
			curmeid=.;
		end;
	if OVERLAP_BEGIN > curend then
		do;
			if not (first.merge_id) then output;
			curstart=OVERLAP_BEGIN;
			curend=OVERLAP_END;
			curmeid=MEID;
		end;
	else if OVERLAP_END >= curend then /* The use of ">=" rather than only ">" is
		to force curmeid to include all MEIDs. */
		do;
			curend=OVERLAP_END;
			curmeid=mean(curmeid,MEID);
		end;
	if last.merge_id then output;
run;

/* Use this for debugging/inspecting. */
%if &debug_mode %then
	%do;
	proc sort
		data=&merged_overlaps_table;
		by Beneficiary curstart curend;
	run;

	proc sort
		data=&overlaps_table;
		by Beneficiary OVERLAP_BEGIN OVERLAP_END;
	run;	
	%end;

proc sql;
%if &drop_tables %then drop table &overlaps_table; ;

alter table &merged_overlaps_table
drop column merge_id, OVERLAP_BEGIN, OVERLAP_END, MEID;

create index GroupByIndex
on &merged_overlaps_table (Beneficiary, aClass_ID, bClass_ID);
quit;

proc datasets lib=&userlib nodetails nolist;
modify %sysfunc(substr(&merged_overlaps_table, %length(&userlib)+2));
	rename curmeid=MEID curstart=OVERLAP_BEGIN curend=OVERLAP_END;
run;
%mend;

%macro make_dc_overlaps_table;
/* Sum the continuous overlaps (of each pair, for each beneficiary) to get discontinuous overlaps,
and enforce the minimum discontinuous overlap. */
proc sql;
create table MDO_A as
select Beneficiary, aClass_ID, aClass_Name, bClass_ID, bClass_Name,
	count(*) as Occurrences, sum((OVERLAP_END-OVERLAP_BEGIN)+1) as DC_OVERLAP,
	mean(MEID) as MEID
from &merged_overlaps_table
group by Beneficiary, aClass_ID, aClass_Name, bClass_ID, bClass_Name
having DC_OVERLAP >= &min_discontinuous_days;

%if &drop_tables %then drop table &merged_overlaps_table; ;

create index Beneficiary
on MDO_A (Beneficiary);

/* Add age groups and enforce the restriction of age. */
create table &dc_overlaps_table as
select a.*,	BENE_AGE_AT_END_REF_YR,
	case &age_groups_query end as AGE_GROUP
from MDO_A a, BENE_CC.MBSF_AB_%eval(2000+&y) as b
where a.Beneficiary=b.BENE_ID
	and BENE_AGE_AT_END_REF_YR >= &minimum_age;

/* TODO: Verify if tables MDO_A have the same number of rows. Check if the
 two SQL queries can become one and still hold that same number of rows. */
drop table MDO_A;
quit;

/* Produce the table of any concomitant medication  */
%let e = %str(count(unique(Beneficiary)) as Beneficiaries,
	sum(DC_OVERLAP) as 'Sum overlap'n, max(DC_OVERLAP) as 'Max overlap'n,
	min(DC_OVERLAP) as 'Min overlap'n,
	round(mean(DC_OVERLAP), &wide_fp_precision) as 'Avg. overlap'n,
	round(std(DC_OVERLAP), &wide_fp_precision) as 'Std. overlap'n,
	round(mean(BENE_AGE_AT_END_REF_YR), &wide_fp_precision) as 'Avg. age'n,
	round(std(BENE_AGE_AT_END_REF_YR), &wide_fp_precision) as 'Std. age'n);

proc sql;
create table &summary_table as
select a.'Age group'n, a.Beneficiaries,
	round(a.Beneficiaries*100/b.Beneficiaries, &percent_beneficiaries_precision) as '% age group'n,
	'Sum overlap'n, 'Max overlap'n, 'Min overlap'n, 'Avg. overlap'n, 'Std. overlap'n, 'Avg. age'n, 'Std. age'n
from (select AGE_GROUP as 'Age group'n, &e
	from &dc_overlaps_table a
	group by 'Age group'n
	union all
	select &all_age_groups_label as 'Age group'n, &e
	from &dc_overlaps_table) a, &age_groups_table b
where a.'Age group'n = b.'Age group'n
order by 'Age group'n desc;
quit;
%mend;

%macro aggregate_class_pairs;
/* Use TABULATE to aggregate by pair and age group, then by drug pair only. */
proc sql;
create table ACP_A as
select CATX('-', aClass_ID, bClass_ID) as PAIR, AGE_GROUP, DC_OVERLAP
from &dc_overlaps_table;
quit;

ods select none;
proc tabulate
	data=ACP_A
	out=ACP_B;
class PAIR AGE_GROUP;
var DC_OVERLAP;
table DC_OVERLAP*(MEAN STD MEDIAN MODE)*PAIR*AGE_GROUP;
run;

proc tabulate
	data=ACP_A
	out=ACP_BA;
class PAIR;
var DC_OVERLAP;
table DC_OVERLAP*(MEAN STD MEDIAN MODE)*PAIR;
run;
ods select all;

proc sql;
drop table ACP_A;
quit;

/* Use SQL to aggregate by drug class and age group, then by class pairs only. */
%let e = %str(aClass_ID, aClass_Name, bClass_ID, bClass_Name,
	sum(Occurrences) as Sum_Occurrences,
	round(mean(Occurrences), &wide_fp_precision) as Mean_Occurrences,
	count(unique(Beneficiary)) as Beneficiaries,
	sum(DC_OVERLAP) as SUM_DC_OVERLAP,
	max(DC_OVERLAP) as MAX_DC_OVERLAP,
	min(DC_OVERLAP) as MIN_DC_OVERLAP,
	round(mean(DC_OVERLAP), &wide_fp_precision) as MEAN_DC_OVERLAP,
	round(std(DC_OVERLAP), &wide_fp_precision) as STD_DC_OVERLAP,
	round(mean(BENE_AGE_AT_END_REF_YR), &wide_fp_precision) as AVG_AGE,
	round(std(BENE_AGE_AT_END_REF_YR), &wide_fp_precision) as STD_AGE,
	round(mean(MEID), &short_fp_precision) as MEID);

proc sql;
create table ACP_C as
select AGE_GROUP, &e
from &dc_overlaps_table
group by AGE_GROUP, aClass_ID, aClass_Name, bClass_ID, bClass_Name;

create table ACP_CA as
select &e	
from &dc_overlaps_table
group by aClass_ID, aClass_Name, bClass_ID, bClass_Name;

%if &drop_tables %then drop table &dc_overlaps_table; ;

/* Add variables from PROC TABULATE to get the final table with age groups, 
 and unite tables with and without age groups. */
%let e = %str(aClass_ID, aClass_Name, bClass_ID, bClass_Name,
	Sum_Occurrences, Mean_Occurrences, Beneficiaries, SUM_DC_OVERLAP, MAX_DC_OVERLAP, MIN_DC_OVERLAP,
	MEAN_DC_OVERLAP, STD_DC_OVERLAP, DC_OVERLAP_Median as MEDIAN_DC_OVERLAP, DC_OVERLAP_Mode as MODE_DC_OVERLAP, 
	AVG_AGE, STD_AGE, MEID,
	CATX('-', Mean_Occurrences, Beneficiaries, STD_DC_OVERLAP, MEID, STD_AGE) as DI_ID);
create table ACP_D as
select a.AGE_GROUP, &e
from ACP_C a, ACP_B d
where CATX('-', aClass_ID, bClass_ID)=d.PAIR and a.AGE_GROUP=d.AGE_GROUP
	and Beneficiaries >= &min_beneficiaries /* Enforce CMS's cell size restriction. */
union all
select &all_age_groups_label as AGE_GROUP, &e
from ACP_CA a, ACP_BA d
where CATX('-', aClass_ID, bClass_ID)=d.PAIR
	and Beneficiaries >= &min_beneficiaries /* Enforce CMS's cell size restriction. */
order by DI_ID; 

drop table ACP_C, ACP_CA, ACP_B, ACP_BA;
quit;

data &aggregated_pairs_table;
	set ACP_D;
	by DI_ID;
	if _N_=1 then Duplicate_Identifier=1;
	else if first.DI_ID then Duplicate_Identifier+1;
run;

proc sql;
drop table ACP_D;

alter table &aggregated_pairs_table
drop column DI_ID;
quit;

proc sort
	data=&aggregated_pairs_table;
	by descending AGE_GROUP descending Beneficiaries descending SUM_DC_OVERLAP descending Mean_Occurrences;
run;
%mend;


%macro make_finalized_tables;
proc sql;
/* Compute beneficiary percentages and the Concomitant Medication index. */
create table MFT_A as
select distinct /* TO DO: Do we need this DISTINCT clause? */
	a.AGE_GROUP as 'Age group'n,
	aClass_ID as "&verbose_class A"n, aClass_Name as "&verbose_class A Name"n,
	bClass_ID as "&verbose_class B"n, bClass_Name as "&verbose_class B Name"n,
	a.Beneficiaries,
	round(a.Beneficiaries*100/d.Beneficiaries, &percent_beneficiaries_precision)
		as '% age group'n,
	SUM_DC_OVERLAP as 'Sum overlap'n, MAX_DC_OVERLAP as 'Max overlap'n,
	MIN_DC_OVERLAP as 'Min overlap'n, MEAN_DC_OVERLAP as 'Avg. overlap'n,
	STD_DC_OVERLAP as 'Std. overlap'n, MEDIAN_DC_OVERLAP as 'Median overlap'n,
	MODE_DC_OVERLAP as 'Mode overlap'n,
	a.AVG_AGE as 'Avg. age'n, a.STD_AGE as 'Std. age'n,
	d.Beneficiaries*a.Beneficiaries/(b.Beneficiaries*c.Beneficiaries) as 'CM Index'n,
	Duplicate_Identifier as 'Dup. ident.'n
from &aggregated_pairs_table a
left join &freq_of_prescription_table b
	on b.'Age group'n = a.AGE_GROUP and b."&verbose_class code"n = a.aClass_ID
left join &freq_of_prescription_table c
	on c.'Age group'n = a.AGE_GROUP and c."&verbose_class code"n = a.bClass_ID
left join &age_groups_table d
	on d.'Age group'n = a.AGE_GROUP;

/* Duplicate table with inverted A-B/B-A to facilitate browsing. */
create table &finalized_table as
select 'Age group'n,
	"&verbose_class B"n as "&verbose_class A"n, "&verbose_class B Name"n as "&verbose_class A Name"n,
	"&verbose_class A"n as "&verbose_class B"n, "&verbose_class A Name"n as "&verbose_class B Name"n,
	Beneficiaries, '% age group'n, 'Sum overlap'n, 'Max overlap'n, 'Min overlap'n,
	'Avg. overlap'n, 'Std. overlap'n, 'Median overlap'n, 'Mode overlap'n, 'Avg. age'n, 'Std. age'n,
	'CM Index'n, 'Dup. ident.'n
from MFT_A
union
select * from MFT_A
order by 'Age group'n desc, Beneficiaries desc, "&verbose_class A"n, "&verbose_class B"n;

drop table MFT_A;
quit;
%mend;

%macro assemble_xlsx;
proc export
	data=&finalized_table
	dbms=xlsx replace
	outfile=&outfile;
	sheet="&sheet_name";
run;

proc export
	data=&summary_table
	dbms=xlsx replace
	outfile=&outfile_summary;
	sheet="&sheet_name";
run;
%mend;


%macro make_tables;
%if &debug_mode %then %let di = u; /* di = debug identifier */
%else %let di =;

%do YL=1 %to %sysfunc(countw(&year_list));
	%let y = %scan(&year_list, &YL);
	%let date_truncation_begin = MDY(2, 1, &y);
	%let date_truncation_end = MDY(12, 31, &y);

	%let age_groups_table = &userlib..&proj_cn._AGE_GROUPS_&y;
	%if &y > 11 %then %let pde_table = &pdelib..PDE&y.&pde_suffix;
		%else %let pde_table = &pdelib..PDESAF%sysfunc(putn(&y, z2))&pde_suffix;

	%if &do_assemble_xlsx %then
		%do;
		%let outfile = "&myfiles_root./&proj_cn. Concomitant medication %eval(2000+&y)";
		%put %sysfunc(fdelete(&outfile));
		%let outfile_summary = "&myfiles_root./&proj_cn. Concomitant medication %eval(2000+&y) (summary)";
		%put %sysfunc(fdelete(&outfile_summary));	
		%end;

	%do OD=1 %to %sysfunc(countw(&do_onc_ddi_list));
		%let do_onc_ddi = %scan(&do_onc_ddi_list, &OD);

		%if &do_onc_ddi %then %let drug_class_list = &hd_drug_class_list;
			%else %let drug_class_list = &dd_drug_class_list;

		%do I=1 %to %sysfunc(countw(&drug_class_list));
			%let class = %scan(&drug_class_list, &I);
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

			%if &do_onc_ddi %then %let suffix = &hypothesis_driven_infix.&di.&y._&class;
			%else %let suffix = &data_driven_infix.&di.&y._&class;

			%let overlaps_table = &overlaps_table_pf.&suffix;
			%if &do_make_overlaps_table %then %make_overlaps_table;

			%let merged_overlaps_table = &merged_overlaps_table_pf.&suffix;
			%if &do_merge_overlaps %then %merge_overlaps;
			
			%do O=1 %to %sysfunc(countw(&min_discontinuous_days_list));
				%let min_discontinuous_days = %scan(&min_discontinuous_days_list, &O);

				%let dc_overlaps_table = &dc_overlaps_table_pf.&suffix._&min_discontinuous_days;
				%let summary_table = &summary_table_pf.&suffix._&min_discontinuous_days;
				%if &do_make_dc_overlaps_table %then %make_dc_overlaps_table;

				%let aggregated_pairs_table = &aggregated_pairs_table_pf.&suffix._&min_discontinuous_days;
				%if &do_aggregate_class_pairs %then %aggregate_class_pairs;

				%let finalized_table = &finalized_table_pf.&suffix._&min_discontinuous_days;
				%let freq_of_prescription_table = &freq_of_prescription_table_pf.&y._&class._0; /* Notice: no debug identifier here */
				%if &do_make_finalized_tables %then %make_finalized_tables;

				%let assembled_xlsx = &assembled_xlsx_pf.&suffix._&min_discontinuous_days;
				%if &do_assemble_xlsx %then %assemble_xlsx;
			%end;
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
