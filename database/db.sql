-- Database generated with pgModeler (PostgreSQL Database Modeler).
-- pgModeler version: 1.1.4
-- PostgreSQL version: 16.0
-- Project Site: pgmodeler.io
-- Model Author: ---
-- object: admin | type: ROLE --
-- DROP ROLE IF EXISTS admin;
CREATE ROLE admin WITH 
	CREATEROLE
	LOGIN;
-- ddl-end --


-- Database creation must be performed outside a multi lined SQL file. 
-- These commands were put in this file only as a convenience.
-- 
-- object: mainnet_plutus_events | type: DATABASE --
-- DROP DATABASE IF EXISTS mainnet_plutus_events;
CREATE DATABASE mainnet_plutus_events;
-- ddl-end --


-- object: public.script_evaluation_events | type: TABLE --
-- DROP TABLE IF EXISTS public.script_evaluation_events CASCADE;
CREATE TABLE public.script_evaluation_events (
	slot bigint NOT NULL,
	block bigint NOT NULL,
	evaluated_successfully bool NOT NULL,
	ledger_language smallint NOT NULL,
	major_protocol_ver smallint NOT NULL,
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
ALTER TABLE public.script_evaluation_events OWNER TO admin;
-- ddl-end --

-- object: public.cost_model_params | type: TABLE --
-- DROP TABLE IF EXISTS public.cost_model_params CASCADE;
CREATE TABLE public.cost_model_params (
	pk bigint NOT NULL,
	param_values bigint[] NOT NULL

);
-- ddl-end --
COMMENT ON COLUMN public.cost_model_params.pk IS E'A synthetic PK obtained by hashing (ledger_language, major_protocol_version, param_values) using a fast non-cryptographically strong hashing algorithm like XXH3';
-- ddl-end --
ALTER TABLE public.cost_model_params OWNER TO admin;
-- ddl-end --

-- object: cost_model_params_pk | type: INDEX --
-- DROP INDEX IF EXISTS public.cost_model_params_pk CASCADE;
CREATE UNIQUE INDEX cost_model_params_pk ON public.cost_model_params
USING btree
(
	pk
);
-- ddl-end --

-- object: public.serialised_scripts | type: TABLE --
-- DROP TABLE IF EXISTS public.serialised_scripts CASCADE;
CREATE TABLE public.serialised_scripts (
	hash bytea NOT NULL,
	serialised bytea NOT NULL,
	CONSTRAINT serialised_scripts_pk PRIMARY KEY (hash)
);
-- ddl-end --
ALTER TABLE public.serialised_scripts OWNER TO postgres;
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


