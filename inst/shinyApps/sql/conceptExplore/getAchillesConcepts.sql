select distinct A.analysis_id, A.stratum_1, B.concept_name, B.concept_id from @resultsDatabaseSchema.achilles_results A
                                  join @vocabDatabaseSchema.concept B on cast(stratum_1 as integer) = B.concept_id
                                  and B.concept_id <> 0
                                  where A.analysis_id in (402,602,2101,702,1801,802);