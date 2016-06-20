%LET _CLIENTTASKLABEL='Prepare NDC maps';
%LET _CLIENTPROJECTPATH='\\ccwdata.org\Profiles\fku838\Documents\Projects\CCSD\CCSD.egp';
%LET _CLIENTPROJECTNAME='CCSD.egp';
%LET _SASPROGRAMFILE=;

%let class_list = ATC1 ATC2 ATC3 ATC4 ATC5 EPC VAL1 VAL2;

%macro subset_table(class);
proc sql;
create table SH026250.NDC_TO_S&class as
select * from RES_NDC_TO_&class
where Route = 'systemic';

create index NDC
on SH026250.NDC_TO_S&class (NDC);

create index ClassID
on SH026250.NDC_TO_S&class (ClassID);

create index RxNIngredientCUI
on SH026250.NDC_TO_S&class (RxNIngredientCUI);
quit;
%mend;

%macro make_ndc_to_rxni;
proc sql;
create index NDC
on SH026250.NDC_TO_IN (NDC);

create index RxNormIngredientID
on SH026250.NDC_TO_IN (RxNormIngredientID);
quit;
%mend;

%macro make_tables;
%do I=1 %to %sysfunc(countw(&class_list));
	%subset_table(%scan(&class_list, &I));
%end;

%make_ndc_to_rxni;

proc sql;
create index NDCPair
on SH026250.NDC_TO_ONC_DDI (norm_ndc1_id, norm_ndc2_id);
quit;

proc sql;
create unique index NDC
on SH026250.ONC_DDI_NDC_LIST (NDC);
quit;
%mend;

%make_tables;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;
