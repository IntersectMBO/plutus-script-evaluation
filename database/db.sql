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
	exec_budget_cpu bigint NOT NULL,
	exec_budget_mem bigint NOT NULL,
	serialised_script bytea NOT NULL,
	datum bytea,
	redeemer bytea,
	script_context bytea NOT NULL,
	ledger_language smallint NOT NULL,
	major_protocol_version smallint NOT NULL

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
	ledger_language smallint NOT NULL,
	major_protocol_version smallint NOT NULL,
	param_values bigint[] NOT NULL,
	CONSTRAINT cost_model_params_pk PRIMARY KEY (ledger_language,major_protocol_version)
);
-- ddl-end --
ALTER TABLE public.cost_model_params OWNER TO admin;
-- ddl-end --

-- object: cost_model_params_fk | type: CONSTRAINT --
-- ALTER TABLE public.script_evaluation_events DROP CONSTRAINT IF EXISTS cost_model_params_fk CASCADE;
ALTER TABLE public.script_evaluation_events ADD CONSTRAINT cost_model_params_fk FOREIGN KEY (ledger_language,major_protocol_version)
REFERENCES public.cost_model_params (ledger_language,major_protocol_version) MATCH FULL
ON DELETE RESTRICT ON UPDATE CASCADE;
-- ddl-end --


