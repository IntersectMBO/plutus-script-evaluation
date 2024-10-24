-- DROP VIEW public.builtin_counts;
CREATE OR REPLACE VIEW PUBLIC.BUILTIN_COUNTS AS
SELECT
  UNNEST(
    JSONB_ARRAY_TO_TEXT_ARRAY (
      JSONB_PATH_QUERY_ARRAY(
        DESERIALISED,
        '$.**?(@."ctor" == "Builtin")."fun"'::JSONPATH
      )
    )
  ) AS BI,
  COUNT(*) AS NUM_BIS
FROM
  DESERIALISED_SCRIPTS
GROUP BY
  (
    UNNEST(
      JSONB_ARRAY_TO_TEXT_ARRAY (
        JSONB_PATH_QUERY_ARRAY(
          DESERIALISED,
          '$.**?(@."ctor" == "Builtin")."fun"'::JSONPATH
        )
      )
    )
  )
ORDER BY
  (COUNT(*)) DESC;
