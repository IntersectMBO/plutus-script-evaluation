-- For each builtin count how many times it is used across all scripts
SELECT
  unnest(jsonb_array_to_text_array(jsonb_path_query_array(deserialised, '$.**?(@."ctor" == "Builtin")."fun"'::jsonpath))) AS bi,
  count(*) AS num_bis
FROM
  deserialised_scripts
GROUP BY
  (unnest(jsonb_array_to_text_array(jsonb_path_query_array(deserialised, '$.**?(@."ctor" == "Builtin")."fun"'::jsonpath))))
ORDER BY
  (count(*)) DESC;

