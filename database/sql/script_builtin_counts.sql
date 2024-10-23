-- For each scripts aggregate the builtin function counts
SELECT
  hash,
  array_agg(jsonb_build_object(bi, num_bis)) AS bi
FROM (
  SELECT
    encode(deserialised_scripts.hash, 'hex'::text) AS hash,
    unnest(jsonb_array_to_text_array(jsonb_path_query_array(deserialised_scripts.deserialised, '$.**?(@."ctor" == "Builtin")."fun"'::jsonpath))) AS bi,
    count(*) AS num_bis
  FROM
    deserialised_scripts
  GROUP BY
    deserialised_scripts.hash,
(unnest(jsonb_array_to_text_array(jsonb_path_query_array(deserialised_scripts.deserialised, '$.**?(@."ctor" == "Builtin")."fun"'::jsonpath))))
  ORDER BY
    (count(*)) DESC) unnamed_subquery
GROUP BY
  HASH;

