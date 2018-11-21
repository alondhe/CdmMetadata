--HINT DISTRIBUTE_ON_KEY(annotation_concept_id)
create table @resultsDatabaseSchema.meta_annotation
(
  meta_annotation_id               integer not null,
  meta_agent_id                    varchar(250) not null,
  meta_entity_activity_id          integer not null,
  annotation_concept_id            integer not null,
  annotation_type_concept_id       integer not null,
  security_concept_id              integer not null
);