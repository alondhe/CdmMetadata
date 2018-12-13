select A.*, C.value_concept_id, C.value_as_string 
from @resultsDatabaseSchema.achilles_heel_results A
left join @resultsDatabaseSchema.meta_entity_activity B on A.achilles_heel_warning = B.activity_as_string
left join @resultsDatabaseSchema.meta_value C on B.meta_entity_activity_id = C.meta_entity_activity_id 
order by A.analysis_id;