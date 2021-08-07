-- =======================================================================
-- 用chr_no撈取門急診用藥紀錄
select *
into ckd_fracture_Grace.dbo.opd_med
from (
         select 's' as hospital,
                OPD_DATE,
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
                DEPT_CODE
         from DB_Hospital.dbo.v_opd_med_s oms
         where exists(
                       select *
                       from ckd_fracture_Grace.dbo.chr_basic_ckd as cbc
                       where cbc.hospital = 's'
                         and cbc.CHR_NO = oms.CHR_NO
                   )
         union all
         select 'w' as hospital,
                OPD_DATE,
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
                DEPT_CODE
         from DB_Hospital.dbo.v_opd_med_w omw
         where exists(
                       select *
                       from ckd_fracture_Grace.dbo.chr_basic_ckd as cbc
                       where cbc.hospital = 'w'
                         and cbc.CHR_NO = omw.CHR_NO
                   )
         union all
         select 't' as hospital,
                OPD_DATE,
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
                DEPT_CODE
         from DB_Hospital.dbo.v_opd_med_t omt
         where exists(
                       select *
                       from ckd_fracture_Grace.dbo.chr_basic_ckd as cbc
                       where cbc.hospital = 't'
                         and cbc.CHR_NO = omt.CHR_NO
                   )
     ) as tmp
-- =======================================================================



-- =======================================================================
-- 用chr_no撈取住院用藥紀錄
select *
into ckd_fracture_Grace.dbo.ud_order
from (
                  select 's' as hospital,
                         FEE_NO,
                         CHR_NO,
                         MED_CODE,
                         MED_DESC,
                         UD_DOSE,
                         UD_UNIT,
                         UD_CIR,
                         PAY_FLAG,
                         BEGIN_DATE,
                         DC_DATE,
                         SEND_AMT,
                         BACK_AMT,
                         FEE_DATE
                  from DB_Hospital.dbo.v_ud_order_s vuos
                  where exists(
                                select *
                                from ckd_fracture_Grace.dbo.chr_basic_ckd as cbc
                                where cbc.hospital = 's'
                                  and cbc.CHR_NO = vuos.CHR_NO
                            )
                  union all
                  select 't' as hospital,
                         FEE_NO,
                         CHR_NO,
                         MED_CODE,
                         MED_DESC,
                         UD_DOSE,
                         UD_UNIT,
                         UD_CIR,
                         PAY_FLAG,
                         BEGIN_DATE,
                         DC_DATE,
                         SEND_AMT,
                         BACK_AMT,
                         FEE_DATE
                  from DB_Hospital.dbo.v_ud_order_t vuot
                  where exists(
                                select *
                                from ckd_fracture_Grace.dbo.chr_basic_ckd as cbc
                                where cbc.hospital = 't'
                                  and cbc.CHR_NO = vuot.CHR_NO
                            )
                  union all
                  select 'w' as hospital,
                         FEE_NO,
                         CHR_NO,
                         MED_CODE,
                         MED_DESC,
                         UD_DOSE,
                         UD_UNIT,
                         UD_CIR,
                         PAY_FLAG,
                         BEGIN_DATE,
                         DC_DATE,
                         SEND_AMT,
                         BACK_AMT,
                         FEE_DATE
                  from DB_Hospital.dbo.v_ud_order_w vuow
                  where exists(
                                select *
                                from ckd_fracture_Grace.dbo.chr_basic_ckd as cbc
                                where cbc.hospital = 'w'
                                  and cbc.CHR_NO = vuow.CHR_NO
                            )
              ) as tmp
-- =======================================================================



-- =======================================================================
-- 整理med_basic

select * into ckd_fracture_Grace.dbo.med_basic_w_desc
from(
select distinct(MED_CODE), hospital, INS_MED_CODE, ATC_CODE, MED_DESC, IMPL_DATE, ADD_DATE, UPD_DATE, MAX_STOP_FLAG
from (
                  select 's' as hospital, *
                  from DB_Hospital.dbo.v_med_basic_s
                  union all
                  select 'w' as hospital, *
                  from DB_Hospital.dbo.v_med_basic_w
                  union all
                  select 't' as hospital, *
                  from DB_Hospital.dbo.v_med_basic_t
              ) as tmp

    ) as tmp
;


select * into ckd_fracture_Grace.dbo.med_basic
from(
        select distinct(MED_CODE), ATC_CODE
        from (
                 select 's' as hospital, *
                 from DB_Hospital.dbo.v_med_basic_s
                 union all
                 select 'w' as hospital, *
                 from DB_Hospital.dbo.v_med_basic_w
                 union all
                 select 't' as hospital, *
                 from DB_Hospital.dbo.v_med_basic_t
             ) as tmp
    ) as tmp
;


drop table med_basic_temp;
select *
into med_basic_temp
from ckd_fracture_Grace.dbo.med_basic mb
where exists
(
    select * from (
                      select MED_CODE, count(ATC_CODE) as count
                      from ckd_fracture_Grace.dbo.med_basic
                      where ATC_CODE <> ''
                      group by MED_CODE
                  ) as tmp
    where  count = 1 and tmp.MED_CODE = mb.MED_CODE and mb.ATC_CODE <> ''
)






select MED_CODE, ATC_CODE
into med_basic_temp1
from (
select MED_CODE, ATC_CODE, count(hospital) as count from ckd_fracture_Grace.dbo.med_basic_w_desc mb
where  exists (

select * from (
select MED_CODE, count(ATC_CODE) as count from ckd_fracture_Grace.dbo.med_basic
where ATC_CODE <> ''
group by MED_CODE) as tmp
where count >1 and mb.MED_CODE = tmp.MED_CODE)
group by MED_CODE, ATC_CODE)
as tmp
where count > 1;

select *
into med_basic_temp2
from (
         select *
         from med_basic_temp
         union all
         select *
         from med_basic_temp1
     ) as tmp


select *
into med_basic_temp3
from (
                  select 'all' as hospital, *
                  from med_basic_temp2
                  union all
                  select hospital, MED_CODE, ATC_CODE
                  from ckd_fracture_Grace.dbo.med_basic_w_desc mb
                  where not exists(
                          select *
                          from med_basic_temp2 tmp
                          where tmp.MED_CODE = mb.MED_CODE
                      )
                    and ATC_CODE <> ''
              ) as tmp


select hospital, MED_CODE, ATC_CODE
from ckd_fracture_Grace.dbo.med_basic_w_desc mb
where not exists(
        select *
        from med_basic_temp3 tmp
        where tmp.MED_CODE = mb.MED_CODE
    )
  and ATC_CODE <> ''



select *
into ckd_fracture_Grace.dbo.med_basic
from med_basic_temp3
drop table med_basic_temp1;
drop table med_basic_temp2;
drop table med_basic_temp3;
-- =======================================================================


-- =======================================================================
-- 統計門急診領藥次數 依照人抓取第一次、最後一次領藥時間

select hospital, CHR_NO, MED_CODE,
       min(OPD_DATE) as first_opd_date,
       max(OPD_DATE) as last_opd_date,
       count(FEE_NO) as count
into ckd_fracture_Grace.dbo.opd_med_count_tmp
from (
                  select hospital,
                         try_convert(date,
                                     substring(OPD_DATE, 1, 4) + '-' + substring(OPD_DATE, 5, 2) + '-' +
                                     substring(OPD_DATE, 7, 2)) as OPD_DATE,
                         FEE_NO,
                         MED_CODE,
                         CHR_NO
                  from (
                           select hospital, try_convert(char, OPD_DATE + 19110000) as OPD_DATE, FEE_NO, MED_CODE, CHR_NO
                           from ckd_fracture_Grace.dbo.opd_med
                       ) as tmp
              ) as tmp
group by hospital, CHR_NO, MED_CODE
;


-- =======================================================================
-- convert med_code -> atc_code, 在統計一遍


select hospital, CHR_NO, ATC_CODE,
       min(first_opd_date) as first_opd_date,
       max(last_opd_date) as last_opd_date,
       sum(count) as count
    into ckd_fracture_Grace.dbo.opd_med_atc_count
       from (
                  select omct.*, mb.ATC_CODE
                  from ckd_fracture_Grace.dbo.opd_med_count_tmp omct
                           inner join ckd_fracture_Grace.dbo.med_basic mb
                                      on omct.MED_CODE = mb.MED_CODE and
                                         (mb.hospital = 'all' or omct.hospital = mb.hospital)
              ) as tmp
group by hospital, CHR_NO, ATC_CODE
-- =======================================================================



-- =======================================================================
-- 統計住院領藥次數 依照人抓取第一次、最後一次領藥時間



select hospital, CHR_NO, MED_CODE,
       min(BEGIN_DATE) as first_begin_date,
       max(BEGIN_DATE) as last_begin_date,
       count(FEE_NO) as count
into ud_order_count_tmp
from (

    select hospital,
           FEE_NO,
           CHR_NO,
           MED_CODE,
           try_convert(date,
                       substring(BEGIN_DATE, 1, 4) + '-' + substring(BEGIN_DATE, 5, 2) + '-' +
                       substring(BEGIN_DATE, 7, 2)) as BEGIN_DATE
    from (
      select hospital,
             FEE_NO,
             CHR_NO,
             MED_CODE,
             try_convert(char, BEGIN_DATE + 19110000) as BEGIN_DATE
      from ckd_fracture_Grace.dbo.ud_order
      where try_convert(float, SEND_AMT) > try_convert(float, BACK_AMT)
         ) tmp
 ) tmp
group by hospital, CHR_NO, MED_CODE
-- =======================================================================





-- =======================================================================
-- convert med_code -> atc_code, 在統計一遍

select hospital, CHR_NO, ATC_CODE,
       min(first_begin_date) as first_begin_date,
       max(last_begin_date) as last_begin_date,
       sum(count) as count
    into ckd_fracture_Grace.dbo.ud_order_atc_count
    from (
        select uoct.*, ATC_CODE from ud_order_count_tmp uoct
            inner join ckd_fracture_Grace.dbo.med_basic mb
            on uoct.MED_CODE = mb.MED_CODE and (mb.hospital = 'all' or uoct.hospital = mb.hospital)
        ) as tmp
    group by hospital, CHR_NO, ATC_CODE
;

-- =======================================================================




-- =======================================================================
-- 所有領藥次數加總



select hospital, CHR_NO, ATC_CODE,
       min(first_opd_date) as first_opd_date,
       max(last_opd_date) as last_opd_date,
       sum(count) as count
into ckd_fracture_Grace.dbo.med_atc_count
from (
    select hospital,CHR_NO, substring(ATC_CODE, 1, 5) as ATC_CODE, first_opd_date, last_opd_date, count
    from ckd_fracture_Grace.dbo.opd_med_atc_count
    union all
    select hospital,CHR_NO, substring(ATC_CODE, 1, 5) as ATC_CODE, first_begin_date as first_opd_date, last_begin_date as last_opd_date, count
    from ckd_fracture_Grace.dbo.ud_order_atc_count
) as tmp
group by hospital, CHR_NO, ATC_CODE
-- =======================================================================





-- =======================================================================
-- 領藥年資計算
select hospital, CHR_NO, ATC_CODE, first_opd_date, last_opd_date, count,
       (datediff(day , first_opd_date, last_opd_date) / 365.0 ) as period_in_year
into ckd_epo_grace.dbo.med_atc_count
from ckd_fracture_Grace.dbo.med_atc_count mac
where exists(
    select * from ckd_epo_grace.dbo.patients p
    where mac.hospital = p.hospital and mac.CHR_NO = p.chr_no
          )
-- =======================================================================

