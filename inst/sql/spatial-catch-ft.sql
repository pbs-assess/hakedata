select
c.FISHING_EVENT_ID as FID,
c.BEST_DATE as catchdate,
c.VESSEL_REGISTRATION_NUMBER as vessel,
c.BEST_RETAINED_ROUND_KG as catch,
c.LON as X,
c.LAT as Y,
c.MAJOR_STAT_AREA_CODE as PFMC,
c.BEST_DEPTH_FM as fdep
from GF_D_OFFICIAL_FE_CATCH c
where
c.FISHERY_SECTOR = 'GROUNDFISH TRAWL' and
(c.TRIP_CATEGORY = 'OPT A - HAKE QUOTA (SHORESIDE)' or
 c.TRIP_CATEGORY = 'OPT A - QUOTA') and
c.SPECIES_CODE = '225'
group by
c.FISHING_EVENT_ID,
c.BEST_DATE,
c.VESSEL_REGISTRATION_NUMBER,
c.BEST_RETAINED_ROUND_KG,
c.LON,
c.LAT,
c.MAJOR_STAT_AREA_CODE,
c.BEST_DEPTH_FM
having
(c.VESSEL_REGISTRATION_NUMBER = 310913 or
 c.VESSEL_REGISTRATION_NUMBER = 312275 or
 c.VESSEL_REGISTRATION_NUMBER = 310988 or
 c.VESSEL_REGISTRATION_NUMBER = 312405) and
c.BEST_RETAINED_ROUND_KG is not null
order by
c.BEST_DATE desc
