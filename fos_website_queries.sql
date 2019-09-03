-- (1)
-- Catch by Species by Date (LOGS)

SELECT ACT_TRIP.ACT_ID TRIP_ID,
               '' LICENCE,
               '' TRIP_TYPE,
               '' VRN,
               '' VESSEL,
               '' SKIPPER,
               '' LANDING_PORT,
               '' HAIL_OUT_DATE,
               '' LANDING_DATE,
               '' SET_COUNT,
               '' SETS_REVIEWED,
               CRPT_LOG_HEADER.CRPT_SOURCE,
               FISHRY_AREA.FA_NME AREA,
               CATCH.SPECIES_SPECIES_CDE,
               SPECIES_VW.SPECIES_COMMON_NME SPECIES_NAME,
               DECODE(CATCH.SUBLEGAL_UNMARKETABLE_IND,0,'YES','NO') LEGAL_MARKETABLE,
               SUM(DECODE(CATCH.CATCH_RELEASED,0,NVL(CATCH.CATCH_WT,0),0)) RET_WT,
               SUM(DECODE(CATCH.CATCH_RELEASED,0,NVL(CATCH.CATCH_QTY,0),0)) RET_QTY,
               SUM(DECODE(CATCH.CATCH_RELEASED,0,NVL(CATCH.BAIT_QTY,0),0)) BAIT_QTY,
               SUM(DECODE(CATCH.CATCH_RELEASED,1,NVL(CATCH.CATCH_WT,0),0)) REL_WT,
               SUM(DECODE(CATCH.CATCH_RELEASED,1,NVL(CATCH.CATCH_QTY,0),0)) REL_QTY,
               SUM(NVL(CATCH.LICED_QTY,0)) LICED_QTY,
               UTILIZATION.CATCHUTIL_DESC CATCH_UTILIZATION,
               COMP_MTHD.SPECIES_COMP_MTHD_DESC COMP_METHOD,
               DECODE(TRIP_HEADER.FSC_COMM_IND,1,'Yes','No') FSC_TRIP
          FROM FOS_V1_1.CATCH,
               FOS_V1_1.FISHING_EVENT,
               FOS_V1_1.CATCH_REPORT CRPT_LOG_HEADER,
               FOS_V1_1.ACTIVITY ACT_TRIP,
               FOS_V1_1.TRIP_HEADER,
               FOS_V1_1.SPECIES_VW,
               FOS_V1_1.CDE_CATCHUTIL UTILIZATION,
               FOS_V1_1.CDE_SPECIES_COMP_MTHD COMP_MTHD,
               FOS_V1_1.FISHRY_AREA
         WHERE CATCH.FE_FE_ID = FISHING_EVENT.FE_ID
           AND FISHING_EVENT.CRPT_CRPT_ID = CRPT_LOG_HEADER.CRPT_ID
           AND CRPT_LOG_HEADER.ACT_ACT_ID = ACT_TRIP.ACT_ID
           AND TRIP_HEADER.ACT_ACT_ID = ACT_TRIP.ACT_ID
           AND SPECIES_VW.SPECIES_CDE (+) = CATCH.SPECIES_SPECIES_CDE
           AND UTILIZATION.CATCHUTIL_ID (+) = CATCH.CATCHUTIL_ID
           AND COMP_MTHD.SPECIES_COMP_MTHD_ID (+) = CATCH.SPECIES_COMP_MTHD_ID
           AND FISHRY_AREA.FA_ID = CATCH.FA_FA_ID
           AND CRPT_LOG_HEADER.CRPTSTAT_ID = 29 -- committed
           AND CRPT_LOG_HEADER.CRPT_SOURCE IN ('FLOG','ASOP') -- Fishing Logs, At Sea Observer Logs
           AND CATCH.SPECIES_SPECIES_CDE = '225' -- Pacific Hake
           AND CATCH.FA_FA_ID IN (3709,3710,3721,3711,3712,3713,3718,3719,3845,3852,3850,4362,3846,4365,3848,3853,3844,4366,4367,4363,4364,4360,4361,4371,3847,4389,3849) -- Areas
           AND ACT_TRIP.ACT_ID IN (SELECT ACT_HAILS.ACT_ACT_ID
                                     FROM FOS_V1_1.ACTIVITY ACT_HAILS,
                                          FOS_V1_1.CATCH_REPORT CRPT_DMP
                                    WHERE CRPT_DMP.ACT_ACT_ID = ACT_HAILS.ACT_ID
                                      AND ACT_HAILS.ACTYP_ACTYP_CDE = 'STOP'
                                      AND CRPT_DMP.CRPTSTAT_ID = 29 -- committed
                                      AND TRUNC(CRPT_DMP.VALIDATE_START_DTT) BETWEEN TO_DATE('10/12/2018','DD/MM/YYYY') AND TO_DATE('17/12/2018','DD/MM/YYYY') -- Start Date within the range                                      
                                  )
GROUP BY ACT_TRIP.ACT_ID,
         CRPT_LOG_HEADER.CRPT_SOURCE,
         CATCH.SPECIES_SPECIES_CDE,
         SPECIES_VW.SPECIES_COMMON_NME,
         CATCH.SUBLEGAL_UNMARKETABLE_IND,
         UTILIZATION.CATCHUTIL_DESC,
         COMP_MTHD.SPECIES_COMP_MTHD_DESC,
         FISHRY_AREA.FA_NME,
         TRIP_HEADER.FSC_COMM_IND

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- (2)
--  Landings by Species by Date (DMP) 

SELECT DISTINCT
    	   act.act_act_id  TRIPID
    	   , lic.LIC_NO_PREFIX ||' ' || lic.LIC_NO_ROOT ||' ' || lic.LIC_NO_SUFFIX|| ' / ' || tt.TRIPTYPE_NME  LICENCE_TRIP_TYPE
    	   , (SELECT DISTINCT
                 prm.LIC_NO_PREFIX||' '||prm.LIC_NO_ROOT
            FROM FOS_V1_1.TRIP_LICENCE tl
                 , FOS_V1_1.ACTIVITY act3
                 , FOS_V1_1.LICENCE lic
                 , (SELECT
                        lic2.LIC_NO_PREFIX
                        , lic2.LIC_NO_ROOT
                        , lic2.LIEL_SEQ
                   FROM FOS_V1_1.LICENCE lic2
                  WHERE lic2.SECONDARY_LIC_YN = 'N'
                    AND lic2.LIEL_SEQ IS NOT NULL
                    ) prm
                    WHERE act3.ACT_ID = act.ACT_ACT_ID
                      AND act3.ACT_ID = tl.ACT_ACT_ID
                      AND lic.LIC_ID = tl.LIC_LIC_ID
                      AND lic.LIEL_SEQ = prm.LIEL_SEQ
                      AND rownum = 1) PRIMARY_LIC
    	   , ves.VRN_ID VRN
    	   , ves.LEGAL_NME VESSEL
    	   , sv.SKIPPER_ID
    	   , sv.skipper_alpha_cde SKIPPER_CDE
    	   , upper(sv.skipper_desc) SKIPPER
    	   ,( SELECT MIN(to_char(ho.DEPART_DTT,'MON-DD-YYYY'))
    				  FROM FOS_V1_1.HAIL_OUT ho
    			   WHERE ho.ACT_ACT_ID IN (SELECT a2.ACT_ID
                                       FROM FOS_V1_1.ACTIVITY a2
                                      WHERE a2.ACT_ACT_ID = act.ACT_ACT_ID ) )  HAIL_OUT_DATE
    	   , upper(TO_CHAR(hi.LANDING_DTT,'Mon dd yyyy hh24:mi')) LANDING_DTT
    	   , pv.PORT_NME LANDING_PORT
    	   , REPLACE(bv.BUYER_NME, ',', '') Buyer
    	   , upper(oloc.OFFLOADLOC_NME) OFFLOADLOC_NME
    	   , upper(olod.OFFLOADER_NME) OFFLOADER_NME
    	   , fa.FA_NME
    	   , sp.SPECIES_CDE SPECIES_CDE
    	   , nvl(sp.SPECIES_LABEL,sp.species_scientific_nme) SPECIES_NME
    	   , upper(stv.STATE_NME) STATE_NME
    	   , upper(fov.FORM_NME) FORM_NME
    	   , ca.catch_wt
    	   , ca.wtconv_factor confct
    	   , round(ca.catch_wt * ca.wtconv_factor) as convwt
    	   , ccat.catchcat_nme as catchCat
         , ccat.catchcat_id
    	   , cr.fsc_permit
         , decode(ccat.catchcat_alpha_cde, 'F', decode(fn.first_nation_nme, NULL, '-unspecified-', fn.first_nation_nme), '') as first_nation
    	FROM
    	   FOS_V1_1.CATCH_REPORT cr
    	   , FOS_V1_1.LICENCE lic
    	   , FOS_V1_1.FISHING_EVENT fe
    	   , FOS_V1_1.CATCH ca
    	   , FOS_V1_1.ACTIVITY act
    	   , FOS_V1_1.HAIL_IN hi
    	   , FOS_V1_1.VESS_VW ves
    	   , FOS_V1_1.BUYER_VW bv
    	   , FOS_V1_1.PORT_VW pv
    	   , FOS_V1_1.FORM_VW fov
    	   , FOS_V1_1.STATE_VW stv
    	   , FOS_V1_1.CATCHCAT_VW ccat
    	   , FOS_V1_1.TRIP_LICENCE tl
    	   , FOS_V1_1.TRIPTYPE_VW tt
    	   , FOS_V1_1.SPECIES_VW sp
    	   , FOS_V1_1.OFFLOADLOC_VW oloc
    	   , FOS_V1_1.OFFLOADER_VW olod
    	   , FOS_V1_1.SKIPPER_VW sv
    	   , FOS_V1_1.FISHRY_AREA fa
         , FOS_V1_1.CDE_FIRST_NATION fn
    WHERE
         cr.ACT_ACT_ID = act.ACT_ID
     AND act.CANCELLED_IND = 0
     AND cr.OFFLOAD_PORT = pv.PORT_ID
     AND act.VESS_SEQ_TEMP = ves.SEQ
     AND hi.ACT_ACT_ID = act.ACT_ID
     AND tl.ACT_ACT_ID     = act.ACT_ACT_ID
     AND tt.triptype_id    = TL.triptype_id
     AND tl.lic_lic_id = lic.lic_id
     AND sp.species_cde = ca.species_species_cde
     AND oloc.offloadloc_id (+)= cr.offloadloc
     AND olod.offloader_id (+)= cr.offloader
     AND fn.first_nation_id (+)= cr.first_nation_id
     AND hi.SKIPPER_ID = sv.SKIPPER_ID
     AND trunc(hi.LANDING_DTT) BETWEEN to_date('10/12/2018','DD/MM/YYYY') AND to_date('17/12/2018','DD/MM/YYYY') -- Landing Date within the range
     AND cr.CRPTSTAT_ID = 29    -- committed
     AND cr.CDSRC_CDSRC_ID = 11 -- validation
     AND lic.LIC_ID = cr.LIC_LIC_ID
     AND cr.CRPT_ID = fe.CRPT_CRPT_ID
     AND fe.FE_ID = ca.FE_FE_ID
     AND ca.CATCH_RELEASED = 0
     AND ca.BUYER_ID = bv.BUYER_ID (+)
     AND ca.FORM_ID = fov.FORM_ID (+)
     AND ca.STATE_ID = stv.STATE_ID (+)
     AND ca.catchcat_id = ccat.catchcat_id (+)
     AND fe.fa_fa_id = fa.FA_ID (+)
     AND ca.SPECIES_SPECIES_CDE = '225' -- Pacific Hake
ORDER BY TRIPID,VRN,LANDING_DTT,SPECIES_CDE