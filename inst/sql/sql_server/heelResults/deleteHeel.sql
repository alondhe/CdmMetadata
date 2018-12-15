delete from @resultsDatabaseSchema.meta_entity_activity
where meta_entity_activity_id = @metaEntityActivityId
;


delete from @resultsDatabaseSchema.meta_annotation
where meta_annotation_id = @metaAnnotationId
;


delete from @resultsDatabaseSchema.meta_value
where meta_value_id = @metaValueId
;