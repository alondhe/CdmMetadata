--HINT DISTRIBUTE_ON_KEY(activity_concept_id)
create table @resultsDatabaseSchema.meta_entity_activity
(
  meta_entity_activity_id       integer not null,
  meta_agent_id                 varchar(250) not null,
  activity_concept_id           integer not null,
  activity_type_concept_id      integer not null,
  activity_as_string            varchar(1000) null,
  entity_concept_id             integer not null,
  entity_as_string              varchar(1000) null,
  entity_identifier             integer null,
  activity_start_datetime       timestamp null,
  activity_end_datetime         timestamp null,
  security_concept_id           integer not null
);