-- Get IDs of people involved in allegations and removals in order to narrow down the list of people
-- from the database of all WA parents and births on file (CW Involvement and Risk for Injury Study.rpt.txt).
use CA_ODS;

if object_id('tempdb..#allegation_select') is not null
    drop table #allegation_select;

select
	ric.id_access_report event_id   
	,ric.vics_id_children child_id 
	,alf.id_prsn_subject parent_id
	,ric.dt_access_rcvd event_date
	,'allegation subject' event_type
into #allegation_select
from base.rptIntake_children ric
	join intake_victim_fact ivf
		on ric.id_access_report = ivf.id_access_report
			and ric.vics_id_children = ivf.id_prsn_vctm 
	join allegation_fact alf 
		on ivf.id_intake_fact = alf.id_intake_fact 
			and ivf.id_prsn_vctm = alf.id_prsn_victim
where ric.dt_access_rcvd >= '2010-01-01';

if object_id('tempdb..#removal_select_mom') is not null
    drop table #removal_select_mom;

select
	rp.id_removal_episode_fact event_id   
	,rp.child child_id 
	,rp.mom_id parent_id
	,rp.removal_dt report_date
	,'removal mom' event_type
into #removal_select_mom
from base.rptPlacement rp
where rp.removal_dt >= '2010-01-01';


if object_id('tempdb..#removal_select_dad') is not null
    drop table #removal_select_dad;

select
	rp.id_removal_episode_fact event_id   
	,rp.child child_id 
	,rp.dad_id parent_id
	,rp.removal_dt report_date
	,'removal dad' event_type
into #removal_select_dad
from base.rptPlacement rp
where rp.removal_dt >= '2010-01-01';

select * 
from #removal_select_dad
union  
select * 
from #removal_select_mom
union 
select * 
from #allegation_select