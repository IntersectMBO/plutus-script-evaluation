-- DROP VIEW public.script_builtin_counts;
CREATE OR REPLACE VIEW PUBLIC.SCRIPT_BUILTIN_COUNTS AS
SELECT
  HASH,
  ARRAY_AGG(JSONB_BUILD_OBJECT(BI, NUM_BIS)) AS BI
FROM
  (
    SELECT
      ENCODE(DESERIALISED_SCRIPTS.HASH, 'hex'::TEXT) AS HASH,
      UNNEST(
        JSONB_ARRAY_TO_TEXT_ARRAY (
          JSONB_PATH_QUERY_ARRAY(
            DESERIALISED_SCRIPTS.DESERIALISED,
            '$.**?(@."ctor" == "Builtin")."fun"'::JSONPATH
          )
        )
      ) AS BI,
      COUNT(*) AS NUM_BIS
    FROM
      DESERIALISED_SCRIPTS
    GROUP BY
      DESERIALISED_SCRIPTS.HASH,
      (
        UNNEST(
          JSONB_ARRAY_TO_TEXT_ARRAY (
            JSONB_PATH_QUERY_ARRAY(
              DESERIALISED_SCRIPTS.DESERIALISED,
              '$.**?(@."ctor" == "Builtin")."fun"'::JSONPATH
            )
          )
        )
      )
    ORDER BY
      (COUNT(*)) DESC
  ) UNNAMED_SUBQUERY
GROUP BY
  HASH;
