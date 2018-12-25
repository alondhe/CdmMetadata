
select distinct A.*,
  B.activity_start_date, 
  B.activity_end_date,
  C.value_as_string,
  C.value_as_number
from @resultsDatabaseSchema.achilles_results A
left join @resultsDatabaseSchema.meta_entity_activity B on cast(A.stratum_1 as integer) = B.entity_concept_id
  and convert(DATE, concat(A.stratum_2, '01')) = B.activity_start_date
  and B.activity_as_string = 'Temporal Event'
left join @resultsDatabaseSchema.meta_value C on B.meta_entity_activity_id = C.meta_entity_activity_id
where A.stratum_1 = '@conceptId'
and A.analysis_id = @analysisId
;