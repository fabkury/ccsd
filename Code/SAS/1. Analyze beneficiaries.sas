%LET _CLIENTTASKLABEL='Analyze beneficiaries';
%LET _CLIENTPROJECTPATH='\\ccwdata.org\Profiles\fku838\Documents\Projects\CCSD\CCSD.egp';
%LET _CLIENTPROJECTNAME='CCSD.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;
%let proj_cn = CCSD;
%let userlib = &userlib..;
%let bene_w_info_table = BENE_W_INFO;
%let age_groups_table = &userlib..&proj_cn._AGE_GROUPS;

%let age_groups_query = %str(when BENE_AGE_AT_END_REF_YR < 65 then '0 - 64'
		when BENE_AGE_AT_END_REF_YR < 75 then '65 - 74'
		when BENE_AGE_AT_END_REF_YR < 85 then '75 - 84'
		when BENE_AGE_AT_END_REF_YR < 95 then '85 - 94'
		else '95+');

%let y = 9;

%macro make_tables;
proc sql;
create table &bene_w_info_table as
select a.BENE_ID, BENE_AGE_AT_END_REF_YR,
	case
		when BENE_SEX_IDENT_CD = '1' then 'Male'
		when BENE_SEX_IDENT_CD = '2' then 'Female'
		else 'Unknown' end as GenderIdentity,
	case
		when BENE_AGE_AT_END_REF_YR < 65 then '0 - 64'
		when BENE_AGE_AT_END_REF_YR < 75 then '65 - 74'
		when BENE_AGE_AT_END_REF_YR < 85 then '75 - 84'
		when BENE_AGE_AT_END_REF_YR < 95 then '85 - 94'
		else '95+' end as AGE_GROUP
from IN026250.MFF_REQ3632 a, BENE_CC.MBSF_AB_%eval(2000+&y) b
where a.BENE_ID=b.BENE_ID and BENE_AGE_AT_END_REF_YR >= 65;

create table &age_groups_table as
select AGE_GROUP, count(*) as TotalBeneficiaries
	from &bene_w_info_table group by AGE_GROUP
union
select 'All 65+' as AGE_GROUP, count(*) as TotalBeneficiaries
	from &bene_w_info_table
order by AGE_GROUP;

%if &drop_tables %then
	%do;
	proc sql
	drop table &bene_w_info_table;
	quit;
	%end;
%mend;

%make_tables;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
