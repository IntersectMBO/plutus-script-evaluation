-- Database generated with pgModeler (PostgreSQL Database Modeler).
-- pgModeler version: 1.1.4
-- PostgreSQL version: 16.0
-- Project Site: pgmodeler.io
-- Model Author: ---
-- object: "plutus-admin" | type: ROLE --
-- DROP ROLE IF EXISTS "plutus-admin";
CREATE ROLE "plutus-admin" WITH 
	SUPERUSER
	CREATEDB
	CREATEROLE
	LOGIN;
-- ddl-end --

-- object: "plutus-indexer" | type: ROLE --
-- DROP ROLE IF EXISTS "plutus-indexer";
CREATE ROLE "plutus-indexer" WITH 
	LOGIN;
-- ddl-end --

-- object: "plutus-reader" | type: ROLE --
-- DROP ROLE IF EXISTS "plutus-reader";
CREATE ROLE "plutus-reader" WITH 
	LOGIN;
-- ddl-end --


-- Database creation must be performed outside a multi lined SQL file. 
-- These commands were put in this file only as a convenience.
-- 
-- object: mainnet_plutus_events | type: DATABASE --
-- DROP DATABASE IF EXISTS mainnet_plutus_events;
CREATE DATABASE mainnet_plutus_events;
-- ddl-end --


SET check_function_bodies = false;
-- ddl-end --

-- object: public.script_evaluation_events | type: TABLE --
-- DROP TABLE IF EXISTS public.script_evaluation_events CASCADE;
CREATE TABLE public.script_evaluation_events (
	slot bigint NOT NULL,
	block bigint NOT NULL,
	evaluated_successfully bool NOT NULL,
	exec_budget_cpu bigint NOT NULL,
	exec_budget_mem bigint NOT NULL,
	script_hash bytea NOT NULL,
	datum bytea,
	redeemer bytea,
	script_context bytea NOT NULL,
	cost_model_params bigint NOT NULL

);
-- ddl-end --
COMMENT ON COLUMN public.script_evaluation_events.slot IS E'Absolute Slot Number';
-- ddl-end --
COMMENT ON COLUMN public.script_evaluation_events.block IS E'Block Height';
-- ddl-end --
COMMENT ON COLUMN public.script_evaluation_events.datum IS E'CBOR-encoded Datum';
-- ddl-end --
COMMENT ON COLUMN public.script_evaluation_events.redeemer IS E'CBOR-encoded redeemer';
-- ddl-end --
COMMENT ON COLUMN public.script_evaluation_events.script_context IS E'CBOR-encoded ScriptContext';
-- ddl-end --
ALTER TABLE public.script_evaluation_events OWNER TO "plutus-admin";
-- ddl-end --

-- object: public.cost_model_params | type: TABLE --
-- DROP TABLE IF EXISTS public.cost_model_params CASCADE;
CREATE TABLE public.cost_model_params (
	pk bigint NOT NULL,
	param_values bigint[] NOT NULL,
	CONSTRAINT cost_model_params_pk PRIMARY KEY (pk)
);
-- ddl-end --
COMMENT ON COLUMN public.cost_model_params.pk IS E'A synthetic PK obtained by hashing param_values using a fast non-cryptographically strong hashing algorithm like XXH3';
-- ddl-end --
ALTER TABLE public.cost_model_params OWNER TO "plutus-admin";
-- ddl-end --

-- object: public.serialised_scripts | type: TABLE --
-- DROP TABLE IF EXISTS public.serialised_scripts CASCADE;
CREATE TABLE public.serialised_scripts (
	hash bytea NOT NULL,
	ledger_language smallint NOT NULL,
	major_protocol_ver smallint NOT NULL,
	serialised bytea NOT NULL,
	CONSTRAINT serialised_scripts_pk PRIMARY KEY (hash)
);
-- ddl-end --
ALTER TABLE public.serialised_scripts OWNER TO "plutus-admin";
-- ddl-end --

-- object: public.deserialised_scripts | type: TABLE --
-- DROP TABLE IF EXISTS public.deserialised_scripts CASCADE;
CREATE TABLE public.deserialised_scripts (
	hash bytea NOT NULL,
	deserialised jsonb NOT NULL,
	CONSTRAINT deserialised_scripts_pk PRIMARY KEY (hash)
);
-- ddl-end --
ALTER TABLE public.deserialised_scripts OWNER TO "plutus-admin";
-- ddl-end --

-- object: public.scripts | type: VIEW --
-- DROP VIEW IF EXISTS public.scripts CASCADE;
CREATE VIEW public.scripts
AS 
select ds.hash, ss.ledger_language, ss.major_protocol_ver, ds.deserialised
from deserialised_scripts as ds 
join serialised_scripts as ss on ds.hash = ss.hash;
-- ddl-end --
ALTER VIEW public.scripts OWNER TO "plutus-admin";
-- ddl-end --

-- object: public.jsonb_array_to_text_array | type: FUNCTION --
-- DROP FUNCTION IF EXISTS public.jsonb_array_to_text_array(jsonb) CASCADE;
CREATE FUNCTION public.jsonb_array_to_text_array (IN _js jsonb)
	RETURNS text[]
	LANGUAGE sql
	IMMUTABLE 
	STRICT
	SECURITY INVOKER
	PARALLEL SAFE
	COST 100
	AS $$
RETURN ARRAY(SELECT jsonb_array_elements_text(_js) AS jsonb_array_elements_text);
$$;
-- ddl-end --
ALTER FUNCTION public.jsonb_array_to_text_array(jsonb) OWNER TO "plutus-admin";
-- ddl-end --

-- object: public.builtin_version_num_usages | type: MATERIALIZED VIEW --
-- DROP MATERIALIZED VIEW IF EXISTS public.builtin_version_num_usages CASCADE;
CREATE MATERIALIZED VIEW public.builtin_version_num_usages
AS 
SELECT
  UNNEST(
    JSONB_ARRAY_TO_TEXT_ARRAY (
      JSONB_PATH_QUERY_ARRAY(
        DESERIALISED,
        '$.**?(@.ctor == "Builtin").fun'::JSONPATH
      )
    )
  ) AS BUILTIN,
  LEDGER_LANGUAGE,
  MAJOR_PROTOCOL_VER,
  COUNT(*) AS NUM_USAGES
FROM
  SCRIPTS
GROUP BY
  BUILTIN,
  LEDGER_LANGUAGE,
  MAJOR_PROTOCOL_VER
ORDER BY
  NUM_USAGES DESC;
-- ddl-end --
COMMENT ON MATERIALIZED VIEW public.builtin_version_num_usages IS E'For each combo of builtin function, Plutus ledger language and major protocol version selects how many times it was used by scripts.';
-- ddl-end --
ALTER MATERIALIZED VIEW public.builtin_version_num_usages OWNER TO "plutus-admin";
-- ddl-end --

-- object: public.builtin_version_num_scripts | type: MATERIALIZED VIEW --
-- DROP MATERIALIZED VIEW IF EXISTS public.builtin_version_num_scripts CASCADE;
CREATE MATERIALIZED VIEW public.builtin_version_num_scripts
AS 
SELECT
  UNNEST(
    JSONB_ARRAY_TO_TEXT_ARRAY (
      JSONB_PATH_QUERY_ARRAY(
        DESERIALISED,
        '$.**?(@."ctor" == "Builtin")."fun"'::JSONPATH
      )
    )
  ) AS BUILTIN,
  LEDGER_LANGUAGE,
  MAJOR_PROTOCOL_VER,
  COUNT(DISTINCT HASH) AS NUM_SCRIPTS
FROM
  SCRIPTS
GROUP BY
  BUILTIN,
  LEDGER_LANGUAGE,
  MAJOR_PROTOCOL_VER
ORDER BY
  NUM_SCRIPTS DESC;
-- ddl-end --
COMMENT ON MATERIALIZED VIEW public.builtin_version_num_scripts IS E'For each combo of builtin function, Plutus ledger language and major protocol version selects how many scripts used it.';
-- ddl-end --
ALTER MATERIALIZED VIEW public.builtin_version_num_scripts OWNER TO "plutus-admin";
-- ddl-end --

-- object: cost_model_params_fk | type: CONSTRAINT --
-- ALTER TABLE public.script_evaluation_events DROP CONSTRAINT IF EXISTS cost_model_params_fk CASCADE;
ALTER TABLE public.script_evaluation_events ADD CONSTRAINT cost_model_params_fk FOREIGN KEY (cost_model_params)
REFERENCES public.cost_model_params (pk) MATCH SIMPLE
ON DELETE RESTRICT ON UPDATE RESTRICT;
-- ddl-end --

-- object: serialised_scripts_fk | type: CONSTRAINT --
-- ALTER TABLE public.script_evaluation_events DROP CONSTRAINT IF EXISTS serialised_scripts_fk CASCADE;
ALTER TABLE public.script_evaluation_events ADD CONSTRAINT serialised_scripts_fk FOREIGN KEY (script_hash)
REFERENCES public.serialised_scripts (hash) MATCH FULL
ON DELETE RESTRICT ON UPDATE RESTRICT;
-- ddl-end --

-- object: hash_fk | type: CONSTRAINT --
-- ALTER TABLE public.deserialised_scripts DROP CONSTRAINT IF EXISTS hash_fk CASCADE;
ALTER TABLE public.deserialised_scripts ADD CONSTRAINT hash_fk FOREIGN KEY (hash)
REFERENCES public.serialised_scripts (hash) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: grant_a_a2e0e10332 | type: PERMISSION --
GRANT INSERT
   ON TABLE public.cost_model_params
   TO "plutus-indexer";
-- ddl-end --

-- object: grant_a_c9a5a89e7c | type: PERMISSION --
GRANT INSERT
   ON TABLE public.deserialised_scripts
   TO "plutus-indexer";
-- ddl-end --

-- object: grant_rad_f053bd12c3 | type: PERMISSION --
GRANT SELECT,INSERT,DELETE
   ON TABLE public.script_evaluation_events
   TO "plutus-indexer";
-- ddl-end --

-- object: grant_a_44c1addc25 | type: PERMISSION --
GRANT INSERT
   ON TABLE public.serialised_scripts
   TO "plutus-indexer";
-- ddl-end --

-- object: grant_r_d000ca5349 | type: PERMISSION --
GRANT SELECT
   ON TABLE public.serialised_scripts
   TO "plutus-reader";
-- ddl-end --

-- object: grant_r_f708f638a3 | type: PERMISSION --
GRANT SELECT
   ON TABLE public.script_evaluation_events
   TO "plutus-reader";
-- ddl-end --

-- object: grant_r_6748eb044d | type: PERMISSION --
GRANT SELECT
   ON TABLE public.deserialised_scripts
   TO "plutus-reader";
-- ddl-end --

-- object: grant_r_d8c5deae97 | type: PERMISSION --
GRANT SELECT
   ON TABLE public.cost_model_params
   TO "plutus-reader";
-- ddl-end --

-- object: grant_r_8bda607156 | type: PERMISSION --
GRANT SELECT
   ON TABLE public.scripts
   TO "plutus-reader";
-- ddl-end --

-- object: "grant_X_c339d7188b" | type: PERMISSION --
GRANT EXECUTE
   ON FUNCTION public.jsonb_array_to_text_array(jsonb)
   TO "plutus-reader";
-- ddl-end --

-- object: grant_r_df877403b6 | type: PERMISSION --
GRANT SELECT
   ON TABLE public.builtin_version_num_scripts
   TO "plutus-reader";
-- ddl-end --

-- object: grant_r_578bfe8442 | type: PERMISSION --
GRANT SELECT
   ON TABLE public.builtin_version_num_usages
   TO "plutus-reader";
-- ddl-end --


