-- This is a copy of the code found in the stored procedure on the SQL Server
select * from openquery(ORAINTA, '

SELECT DISTINCT
        act.act_act_id  TRIPID
        , lic.LIC_NO_PREFIX ||'' '' || lic.LIC_NO_ROOT ||'' '' || lic.LIC_NO_SUFFIX|| '' / '' || tt.TRIPTYPE_NME  LICENCE_TRIP_TYPE
        , (SELECT DISTINCT
                 prm.LIC_NO_PREFIX||'' ''||prm.LIC_NO_ROOT
            FROM FOS_V1_1.TRIP_LICENCE tl
                 , FOS_V1_1.ACTIVITY act3
                 , FOS_V1_1.LICENCE lic
                 , (SELECT
                        lic2.LIC_NO_PREFIX
                        , lic2.LIC_NO_ROOT
                        , lic2.LIEL_SEQ
                   FROM FOS_V1_1.LICENCE lic2
                  WHERE lic2.SECONDARY_LIC_YN = ''N''
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
        ,( SELECT MIN(to_char(ho.DEPART_DTT,''MON-DD-YYYY''))
          FROM FOS_V1_1.HAIL_OUT ho
          WHERE ho.ACT_ACT_ID IN (SELECT a2.ACT_ID
                                       FROM FOS_V1_1.ACTIVITY a2
                                      WHERE a2.ACT_ACT_ID = act.ACT_ACT_ID ) )  HAIL_OUT_DATE
        , upper(TO_CHAR(hi.LANDING_DTT,''Mon dd yyyy hh24:mi'')) LANDING_DTT
        , pv.PORT_NME LANDING_PORT
        , REPLACE(bv.BUYER_NME, '','', '''') Buyer
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
        , decode(ccat.catchcat_alpha_cde, ''F'', decode(fn.first_nation_nme, NULL, ''-unspecified-'', fn.first_nation_nme), '''') as first_nation
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
     AND ca.SPECIES_SPECIES_CDE = ''225'' -- Pacific Hake
ORDER BY TRIPID,VRN,LANDING_DTT,SPECIES_CDE

')
