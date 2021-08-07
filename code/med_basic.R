  med_basic <- 
    sqlQuery(
      db_hospital,
      " select distinct MED_CODE, ATC_CODE,
                  's' as hospital 
          	from DB_Hospital.dbo.v_med_basic_s
          		union all
          select distinct MED_CODE, ATC_CODE,
                  't' as hospital 
          	from DB_Hospital.dbo.v_med_basic_t
          		union all
          select distinct MED_CODE, ATC_CODE,
                  'w' as hospital
          	from DB_Hospital.dbo.v_med_basic_w;"
    ) %>% 
    setDT()
  
# 門急診用藥 --------------------------------------------------------------------
  
  opd_med <- 
    map_dfr(
      c('s', 't', 'w'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0(
            "select OPD_DATE,
                    FEE_NO,
                    MED_CODE,
                    CHR_NO,
                    try_convert(float, UNIT_QTY)      as UNIT_QTY,
                    try_convert(float, PER_QTY)       as PER_QTY,
                    UNIT_TYPE,
                    UNIT_PACKS,
                    CIR_CODE,
                    MED_DAYS,
                    DEL_FLAG,
                    try_convert(float, INS_PAY_UAMT)  as INS_PAY_UAMT,
                    try_convert(float, SELF_PAY_UAMT) as SELF_PAY_UAMT,
                    try_convert(float, PART_UAMT)     as PART_UAMT,
                    try_convert(float, TTL_QTY)       as TTL_QTY,
                    try_convert(float, TTL_RTN_QTY)   as TTL_RTN_QTY,
                    DEPT_CODE ",
            " from DB_Hospital.dbo.v_opd_med_", x,
            " where CHR_NO ",
            " in ( ",
            str_c("'", 
                  chr_no_cohort[hospital == x, ]$chr_no,
                  "'",
                  collapse = ', '),
            ") ;"
          )
        ) %>% 
          mutate(hospital = x)
      }
    )
  
  opd_med_atc <- 
    left_join(
      opd_med,
      med_basic,
      by = c('MED_CODE' = 'MED_CODE',
             'hospital' = 'hospital')
    ) %>% 
    setDT()
  
# 住院用藥 --------------------------------------------------------------------
  
  ipd_med <- 
    map_dfr(
      c('s', 't', 'w'),
      function(x){
        sqlQuery(
          db_hospital,
          paste0(
            "select FEE_NO,
                        CHR_NO,
                        MED_CODE,
                        MED_DESC,
                        try_convert(float, UD_DOSE) as UD_DOSE,
                        UD_UNIT,
                        UD_CIR,
                        PAY_FLAG,
                        BEGIN_DATE,
                        DC_DATE,
                        SEND_AMT,
                        BACK_AMT,
                        FEE_DATE",
            " from DB_Hospital.dbo.v_ud_order_", x,
            " where CHR_NO ",
            " in ( ",
            str_c("'", 
                  chr_no_cohort[hospital == x, ]$chr_no,
                  "'",
                  collapse = ', '),
            ") ;"
          )
        ) %>% 
          mutate(
            hospital = x)
      }
    )
  
  ipd_med_atc <- 
    left_join(
      ipd_med,
      med_basic,
      by = c('MED_CODE' = 'MED_CODE',
             'hospital' = 'hospital')
    ) %>% 
    setDT()
  
