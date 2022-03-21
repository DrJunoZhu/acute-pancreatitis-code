with basic_select as (select subject_id, hadm_id
                      from mimiciii.diagnoses_icd
                      where icd9_code = '5770'
                        and (subject_id, hadm_id) not in (
                          select subject_id, hadm_id
                          from mimiciii.diagnoses_icd
                          where icd9_code between 'V42' and 'V429' -- organ transplantation
                             or icd9_code = '042'                  --  aids
                             or icd9_code between '283' and '2839' -- hemolytic anemia
                             or icd9_code between '570' and '5722' -- acute liver failure
                             or icd9_code between '585' and '5856' -- end-stage kidney disease
                             or icd9_code between '140' and '240'  -- malignant tumor
                          group by subject_id, hadm_id
                      )),
     icu_info as (
         select subject_id,
                hadm_id,
                icustay_id,
                admission_age,
                gender,
                ethnicity,
--                 first_icu_stay,
                los_icu,
                los_hospital,
                case when dod between intime and outtime then true else false end      as icu_death,
                case when dod between admittime and dischtime then true else false end as hospital_death,
                case
                    when dod between admittime and admittime + INTERVAL '30 day' then true
                    else false end                                                     as hospital_30_days_death
         from icustay_detail
         where first_icu_stay = true
           and admission_age >= 18
           and los_icu >= 1
     ),
     vital as (select * from vital_first_time),
     ventilation as (select * from ventilation_first_day),
     venti_dura as (select icustay_id, sum(duration_hours) as vent_duration
                    from ventilation_durations
                    group by icustay_id),
     vasopressin as (select * from vasopressin_firstday),
     lab as (select * from lab_first_time),
     blood as (select * from blood_gas_first_time),
     sofa as (select * from sofa),
     elix as (select *
              from elixhauser_ahrq_v37_no_drg),

     elix_score as (
         select *
         from elixhauser_score_ahrq),
     first_weight as (
         select icustay_id, weight_first, height_first
         from weight_height
     ),
     sepsis as (
         select subject_id, hadm_id, 1 as sepsis from mimiciii.diagnoses_icd where icd9_code like '9959%'
     )

select *,
       case when icu_death then 0 else 1 end              as non_survival_icu,
       case when hospital_death then 0 else 1 end         as non_survival_hospital,
       case when hospital_30_days_death then 0 else 1 end as non_survival_hospital_30_days,
       case when sepsis is null then 0 else 1 end         as sepsis
from basic_select
         join icu_info using (subject_id, hadm_id)
         left join vital using (subject_id, hadm_id, icustay_id)
         left join ventilation using (subject_id, hadm_id, icustay_id)
         left join vasopressin using (subject_id, hadm_id, icustay_id)
         left join lab using (subject_id, hadm_id, icustay_id)
         left join blood using (subject_id, hadm_id, icustay_id)
         left join sofa using (subject_id, hadm_id, icustay_id)
         left join elix using (subject_id, hadm_id)
         left join elix_score using (subject_id, hadm_id)
         left join venti_dura using (icustay_id)
         left join first_weight using (icustay_id)
         left join gcs_first_time using (subject_id, hadm_id, icustay_id)
         left join sepsis using (subject_id, hadm_id);


