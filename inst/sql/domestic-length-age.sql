SELECT        TRIP.TRIP_ID, VESSEL.VESSEL_ID, VESSEL.VESSEL_NAME, TRIP.TRIP_END_DATE, TRIP_SUB_TYPE.TRIP_SUB_TYPE_DESC, 
                         FISHING_EVENT.MAJOR_STAT_AREA_CODE, FISHING_EVENT.MINOR_STAT_AREA_CODE, FISHING_EVENT.FE_MAJOR_LEVEL_ID AS setnumber, 
                         SPECIMEN.SAMPLE_ID, SPECIMEN.SPECIMEN_AGE, SPECIMEN.SPECIMEN_SERIAL_PREFIX, SPECIMEN.SPECIMEN_SERIAL_NUMBER, 
                         MAJOR_STAT_AREA.MAJOR_STAT_AREA_NAME, SAMPLE.SAMPLE_WEIGHT, CATCH.CATCH_WEIGHT, SAMPLE.SAMPLE_DATE, 
                         SPECIMEN_MORPHOMETRICS.MORPHOMETRICS_ATTRIBUTE_CODE, SPECIMEN_MORPHOMETRICS.SPECIMEN_MORPHOMETRICS_VALUE, 
                         SPECIMEN_MORPHOMETRICS.MORPHOMETRICS_UNIT_CODE, TRIP.TRIP_SUB_TYPE_CODE, CATCH.SPECIES_CODE
FROM            SAMPLE_COLLECTED INNER JOIN
                         VESSEL INNER JOIN
                         TRIP INNER JOIN
                         FISHING_EVENT_CATCH ON TRIP.TRIP_ID = FISHING_EVENT_CATCH.TRIP_ID INNER JOIN
                         TRIP_SUB_TYPE ON TRIP.TRIP_SUB_TYPE_CODE = TRIP_SUB_TYPE.TRIP_SUB_TYPE_CODE ON VESSEL.VESSEL_ID = TRIP.VESSEL_ID AND 
                         VESSEL.SUFFIX = TRIP.SUFFIX INNER JOIN
                         FISHING_EVENT INNER JOIN
                         MAJOR_STAT_AREA ON FISHING_EVENT.MAJOR_STAT_AREA_CODE = MAJOR_STAT_AREA.MAJOR_STAT_AREA_CODE ON 
                         TRIP.TRIP_ID = FISHING_EVENT.TRIP_ID AND FISHING_EVENT_CATCH.FISHING_EVENT_ID = FISHING_EVENT.FISHING_EVENT_ID INNER JOIN
                         CATCH ON FISHING_EVENT_CATCH.CATCH_ID = CATCH.CATCH_ID INNER JOIN
                         CATCH_SAMPLE INNER JOIN
                         SAMPLE ON CATCH_SAMPLE.SAMPLE_ID = SAMPLE.SAMPLE_ID ON CATCH.CATCH_ID = CATCH_SAMPLE.CATCH_ID ON 
                         SAMPLE_COLLECTED.SAMPLE_ID = SAMPLE.SAMPLE_ID INNER JOIN
                         SPECIMEN INNER JOIN
                         SPECIMEN_MORPHOMETRICS ON SPECIMEN.SAMPLE_ID = SPECIMEN_MORPHOMETRICS.SAMPLE_ID AND 
                         SPECIMEN.SPECIMEN_ID = SPECIMEN_MORPHOMETRICS.SPECIMEN_ID ON SAMPLE.SAMPLE_ID = SPECIMEN.SAMPLE_ID
WHERE        (SPECIMEN_MORPHOMETRICS.MORPHOMETRICS_ATTRIBUTE_CODE = 1 OR
              SPECIMEN_MORPHOMETRICS.MORPHOMETRICS_ATTRIBUTE_CODE = 2 OR
              SPECIMEN_MORPHOMETRICS.MORPHOMETRICS_ATTRIBUTE_CODE = 4 OR
              SPECIMEN_MORPHOMETRICS.MORPHOMETRICS_ATTRIBUTE_CODE = 10) AND
             (CATCH.SPECIES_CODE = '225') AND
			 (TRIP.TRIP_SUB_TYPE_CODE = 1 OR
			  TRIP.TRIP_SUB_TYPE_CODE = 4)
ORDER BY TRIP.TRIP_END_DATE, SPECIMEN.SAMPLE_ID