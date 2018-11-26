--HINT DISTRIBUTE_ON_KEY(meta_agent_concept_id)
create table @resultsDatabaseSchema.meta_agent
(
  meta_agent_id               varchar(250) not null,
  meta_agent_concept_id       integer not null,
  agent_first_name            varchar(250) null,
  agent_last_name             varchar(250) null,
  agent_suffix                varchar(100) null,
  agent_algorithm             varchar(250) null,
  agent_description           varchar(1000) null
);