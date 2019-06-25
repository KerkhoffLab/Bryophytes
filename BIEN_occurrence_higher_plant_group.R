require(BIEN)
BIEN_occurrence_higher_plant_group <- function (higher_plant_group, cultivated = FALSE, only.new.world = TRUE, 
                            observation.type = FALSE, all.taxonomy = FALSE, native.status = FALSE, 
                            natives.only = TRUE, political.boundaries = FALSE, collection.info = F, 
                            ...) 
{
  BIEN:::.is_char(higher_plant_group)
  BIEN:::.is_log(cultivated)
  BIEN:::.is_log(only.new.world)
  BIEN:::.is_log(observation.type)
  BIEN:::.is_log(all.taxonomy)
  BIEN:::.is_log(native.status)
  BIEN:::.is_log(natives.only)
  BIEN:::.is_log(political.boundaries)
  BIEN:::.is_log(collection.info)
  cultivated_ <- BIEN:::.cultivated_check(cultivated)
  newworld_ <- BIEN:::.newworld_check(only.new.world)
  taxonomy_ <- BIEN:::.taxonomy_check(all.taxonomy)
  native_ <- BIEN:::.native_check(native.status)
  observation_ <- BIEN:::.observation_check(observation.type)
  political_ <- BIEN:::.political_check(political.boundaries)
  natives_ <- BIEN:::.natives_check(natives.only)
  collection_ <- BIEN:::.collection_check(collection.info)
  query <- paste("SELECT higher_plant_group ", taxonomy_$select, 
                 native_$select, political_$select, ", scrubbed_species_binomial, latitude, longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id", 
                 collection_$select, cultivated_$select, newworld_$select, 
                 observation_$select, "\n                 FROM view_full_occurrence_individual \n                 WHERE higher_plant_group in (", 
                 paste(shQuote(higher_plant_group, type = "sh"), collapse = ", "), 
                 ")", cultivated_$query, newworld_$query, natives_$query, 
                 " AND higher_plant_group IS NOT NULL AND (is_geovalid = 1 OR is_geovalid IS NULL) \n                 ORDER BY scrubbed_species_binomial ;")
  return(BIEN:::.BIEN_sql(query, ...))
}


#to run command and store as dataset
#bryophyte_occurrence <- BIEN_occurrence_higher_plant_group("bryophytes")
