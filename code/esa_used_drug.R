  esa_drug_code <- 
    sqlQuery(
      ckd_fracture_Grace,
      'select distinct QTY_DESC, hospital, MED_CODE,
                qty_in_unit, MED_DESC
          from ckd_fracture_Grace.dbo.med_basic_epo;'
    )
  
  opd_med_atc <- 
    sqlQuery(
      ckd_epo_jerry,
      'select * from ckd_epo_jerry.dbo.opd_med_atc;'
    )
  
  ipd_med_atc <- 
    sqlQuery(
      ckd_epo_jerry,
      'select * from ckd_epo_jerry.dbo.ipd_med_atc;'
    )
  

# 門急診用藥 -------------------------------------------------------------------
  esa_opd <- 
    inner_join(
      opd_med_atc %>% 
        filter(DEL_FLAG == 'N' & TTL_QTY > TTL_RTN_QTY),
      esa_drug_code,
      by = c('hospital' = 'hospital',
             'MED_CODE' = 'MED_CODE')
    ) %>% 
    mutate(
      tmp_opd_date = ymd(as.integer(OPD_DATE) + 19110000)
    ) %>% 
    mutate(
      begin_date = tmp_opd_date,
      end_date = tmp_opd_date + MED_DAYS,
      units = TTL_QTY*qty_in_unit
    ) %>% 
    select(
      hospital, CHR_NO, FEE_NO,
      begin_date, end_date, MED_DESC,
      MED_CODE, TTL_QTY, ATC_CODE,
      QTY_DESC, qty_in_unit, units
    )
  

# 住院用藥 --------------------------------------------------------------------

  esa_ipd <- 
    inner_join(
      ipd_med_atc %>% 
        select(-MED_DESC) %>% 
        filter(SEND_AMT > BACK_AMT & 
               !is.na(BEGIN_DATE)),
      esa_drug_code,
      by = c('hospital' = 'hospital',
             'MED_CODE' = 'MED_CODE')
    ) %>% 
    mutate(
      begin_date = 
        ymd(as.integer(BEGIN_DATE) + 19110000),
      end_date = 
        if_else(is.na(DC_DATE),
                ymd(as.integer(BEGIN_DATE) + 19110000),
                ymd(as.integer(DC_DATE) + 19110000),
                ymd(as.integer(BEGIN_DATE) + 19110000)),
      send_amt = as.double(SEND_AMT)
    ) %>%
    mutate(
      units = send_amt*qty_in_unit
    ) %>% 
    select(
      hospital, CHR_NO, FEE_NO, 
      begin_date, end_date, MED_DESC,
      MED_CODE, send_amt,
      QTY_DESC, qty_in_unit, units, ATC_CODE
    )
  

# esa 用藥結果 ----------------------------------------------------------------

  esa_used_result <- 
    bind_rows(
      esa_opd %>% 
        select(hospital, CHR_NO, FEE_NO,
               begin_date, end_date, MED_DESC, 
               MED_CODE, TTL_QTY, ATC_CODE, QTY_DESC,
               qty_in_unit, units),
      esa_ipd %>% 
        select(hospital, CHR_NO, FEE_NO,
               begin_date, end_date, MED_DESC, 
               MED_CODE, send_amt, ATC_CODE, QTY_DESC,
               qty_in_unit, units) %>% 
        rename(TTL_QTY = send_amt)
    ) %>% 
    mutate(
      esa_type = 
        case_when(
          grepl(tolower(MED_DESC), pattern = 'recormon') ~
            'recormon',
          grepl(tolower(MED_DESC), pattern = 'nesp') ~
            'nesp',
          grepl(tolower(MED_DESC), pattern = 'mircera') ~
            'mircera',
          T ~ 'undefined'
        ),
      begin_date = format(begin_date, format = '%Y-%m-%d'),
      end_date = format(end_date, format = '%Y-%m-%d')
    )
  
  names(esa_used_result) <- tolower(names(esa_used_result))
  write_to_db(
    esa_used_result %>% 
      mutate(),
    'esa_used_result'
  )