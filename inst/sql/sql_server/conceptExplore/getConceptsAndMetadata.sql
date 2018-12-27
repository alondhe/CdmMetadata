with cte
as
(
  SELECT
    num.stratum_2,
    round(1000 * (1.0 * num.count_value / denom.count_value), 5) AS count_value
  FROM (
         SELECT
           stratum_1,
           stratum_2,
           count_value
         FROM @resultsDatabaseSchema.achilles_results
         WHERE analysis_id = @analysisId AND stratum_1 = '@conceptId'
  ) num
  INNER JOIN (
  	SELECT
  		 stratum_1,
  		 count_value
  	 FROM @resultsDatabaseSchema.achilles_results
  	 WHERE analysis_id = 117
  ) denom ON num.stratum_2 = denom.stratum_1
)
select distinct A.stratum_2, A.count_value,
  B.activity_start_date, 
  B.activity_end_date,
  C.value_as_string,
  C.value_as_number
from cte A
left join @resultsDatabaseSchema.meta_entity_activity B on convert(DATE, concat(A.stratum_2, '01')) = B.activity_start_date
  and B.activity_as_string = 'Temporal Event'
left join @resultsDatabaseSchema.meta_value C on B.meta_entity_activity_id = C.meta_entity_activity_id
where B.entity_concept_id = @conceptId
;