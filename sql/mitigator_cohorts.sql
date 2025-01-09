-- NHSE Community strategy 
-- SUS dataset for central mitigators relating to frailty, falls, end of life care and emergency elderly admissions

-- Spell level variables required

USE NHSE_SUSPlus_Live;
GO

--(114,714,138 rows affected)  
DROP TABLE IF EXISTS #temp_base
SELECT 
	-- identifiers
	a.[APCS_Ident],
	a.[Der_Pseudo_NHS_Number],

	-- patient info
	a.[Der_Age_at_CDS_Activity_Date],
	a.[Ethnic_Group],
	a.[Sex],
	a.[Der_Postcode_LSOA_2011_Code],
	a.[Der_Postcode_LSOA_2021_Code],

	-- admission info
	a.[Admission_Date],
	a.[Discharge_Date],
	a.[Discharge_Ready_Date],
	a.[Der_Activity_Month],
	a.[Der_Financial_Year],
	a.[Admission_Method],
	a.[Administrative_Category],
	a.[Discharge_Method],
	a.[Spell_Core_HRG_SUS],
	a.[Der_Spell_LoS],
	a.[Der_Diagnosis_All],
	a.[Der_Procedure_All],

	SUBSTRING(
        a.[Der_Procedure_All],
        CHARINDEX('||', a.[Der_Procedure_All]) + 2,
        CASE 
            WHEN CHARINDEX(',', a.[Der_Procedure_All], CHARINDEX('||', a.[Der_Procedure_All]) + 2) > 0 
            THEN CHARINDEX(',', a.[Der_Procedure_All], CHARINDEX('||', a.[Der_Procedure_All]) + 2) - (CHARINDEX('||', a.[Der_Procedure_All]) + 2)
            WHEN CHARINDEX('NULL', a.[Der_Procedure_All], CHARINDEX('||', a.[Der_Procedure_All]) + 2) > 0 
            THEN CHARINDEX('NULL', a.[Der_Procedure_All], CHARINDEX('||', a.[Der_Procedure_All]) + 2) - (CHARINDEX('||', a.[Der_Procedure_All]) + 2)
            ELSE 4
        END
    ) AS First_Procedure_Code,

	b.[Spell_Primary_Diagnosis],
	b.[Spell_Secondary_Diagnosis],
	b.[Spell_Core_HRG],
	b.[Spell_Treatment_Function_Code],
	b.[Spell_Main_Specialty_Code],

	-- organisation info
	a.[Der_Provider_Code],
	a.[Der_Provider_Site_Code],
	a.[Der_Commissioner_Code],
	a.[GP_Practice_Code]

INTO #temp_base

FROM [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCS] a
	LEFT JOIN [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCS_2425_Der] b 
	ON a.[APCS_Ident] = b.[APCS_Ident]

WHERE 
	a.Der_Activity_Month > 201803
	AND 
	LEFT(a.Der_Commissioner_Code,2) NOT LIKE '7A%' -- Excludes Welsh commissioners
	AND
	a.[Sex] IN ('1','2')
	AND 
    -- remove well babies babies
    NOT (
            -- healthy baby HRG
            b.[Spell_Core_HRG] = 'PB03Z'
        OR
        (
            -- well baby specialty on a birth episode
                b.[Spell_Treatment_Function_Code] = '424'
            --AND
            --   i.EPITYPE = '3'
        )
    )
;
GO

-- Frailty spells
-- Distinguish between medium and high frailty score later
-- (12,933,241 rows affected) 7791194 
DROP TABLE IF EXISTS #temp_frail 
SELECT APCS_Ident, 'frail' as 'flag' 
INTO #temp_frail
FROM #temp_base
WHERE
Admission_Method LIKE '2%'
AND 
LEFT([Spell_Primary_Diagnosis],3) IN (
'F00','G81','G30','I69','R29','R39','F05','W19',
'S00','R31','B96','R41','R26','I67','R56','R40',
'T83','S06','S42','E87','M25','E86','R54','Z50',
'F03','W18','Z75','F01','S80','L03','H54','E53',
'Z60','G20','R55','S22','K59','N17','L89','Z22',	
'B95','L97','R44','K26','I95','N19','A41','Z87',	
'J96','X59','M19','G40','M81','S72','S32','E16',	
'R94','N18','R33','R69','N28','R32','G31','Y95',	
'S09','R45','G45','Z74','M79','W06','S01','A04',	
'A09','J18','J69','R47','E55','Z93','R02','R63',	
'H91','W10','W01','E05','M41','R13','Z99','U80',	
'M80','K92','I63','N20','F10','Y84','R00','J22',	
'Z73','R79','Z91','S51','F32','M48','E83','M15',	
'D64','L08','R11','K52','R50'
) 
AND
[Der_Age_at_CDS_Activity_Date] >= 65
;
GO

-- Elderly emergency care
-- (11,715,813 rows affected) 12,034,362 
DROP TABLE IF EXISTS #temp_elderly_emergency
SELECT APCS_Ident, 'elderly emergency' as 'flag' 
INTO #temp_elderly_emergency  
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND
	Der_Age_at_CDS_Activity_Date >= 75
;
GO

-- End of life spells
-- Identify if death within 0-2 or 3-14 days later
-- (354,893 rows affected) 362,068 
DROP TABLE IF EXISTS #temp_eol
SELECT APCS_Ident, 'eol' as 'flag' 
INTO #temp_eol
FROM #temp_base 
WHERE 
	Discharge_Method = '4' 
	AND
	(Der_Spell_LoS < 14)
	AND
	(Spell_Primary_Diagnosis NOT LIKE '[V-Y]%') 
	AND
	(Der_Procedure_All IS NULL)
;
GO

-- Falls 
-- Explicit
-- (149 rows affected) 149
DROP TABLE IF EXISTS #temp_falls_expl
SELECT APCS_Ident, 'falls - explicit' as 'flag' 
INTO #temp_falls_expl
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND
	Der_Age_at_CDS_Activity_Date >= 65
	AND 
	Spell_Primary_Diagnosis LIKE 'W[01][0-9]%'
;
GO

-- Implicit - fracture
-- (655,035 rows affected) 672859 
DROP TABLE IF EXISTS #temp_falls_impl_fract
SELECT APCS_Ident, 'falls - implicit fracture' as 'flag' 
INTO #temp_falls_impl_fract
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND
	Der_Age_at_CDS_Activity_Date >= 65
	AND (
          Spell_Primary_Diagnosis LIKE 'M48[45]%'
        OR
          Spell_Primary_Diagnosis LIKE 'M80[0-589]%'
        OR
          Spell_Primary_Diagnosis LIKE 'S22[01]%'
        OR
          Spell_Primary_Diagnosis LIKE 'S32[0-47]%'
        OR
          Spell_Primary_Diagnosis LIKE 'S42[234]%'
        OR
          Spell_Primary_Diagnosis LIKE 'S52%'
        OR
          Spell_Primary_Diagnosis LIKE 'S620%'
        OR
          Spell_Primary_Diagnosis LIKE 'S72[0-48]%'
        OR
          Spell_Primary_Diagnosis LIKE 'T08X%'
      ) 
	  AND
	  Spell_Primary_Diagnosis NOT LIKE '[VWXY]%'
;
GO

-- Implicit: Tendency to fall
-- (375,560 rows affected)
DROP TABLE IF EXISTS #temp_falls_impl_tend
SELECT APCS_Ident, 'falls - implicit tendency to fall' as 'flag' 
INTO #temp_falls_impl_tend 
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND
	Der_Age_at_CDS_Activity_Date >= 65
	AND 
	(Spell_Primary_Diagnosis LIKE 'R296%')
;
GO

-- Combine all falls temporary tables
-- (1,048,568 rows affected) 
DROP TABLE IF EXISTS #temp_falls_comb
SELECT * 
INTO #temp_falls_comb
FROM (
	SELECT * FROM #temp_falls_expl
	UNION ALL
	SELECT * FROM #temp_falls_impl_fract
	UNION ALL
	SELECT * FROM #temp_falls_impl_tend
	) a
;
GO

---------------------------------- Additions to cohort (virtual wards not included)

-- Virtual wards - ari
DROP TABLE IF EXISTS #temp_virtual_ward_ari
SELECT APCS_Ident, 'virtual ward - ari' as 'flag' 
INTO #temp_virtual_ward_ari  
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND
	Discharge_Method IN ('1', '2', '3')
	AND
	Der_Age_at_CDS_Activity_Date >= 18
	AND 
	Der_Procedure_All IS NULL
	AND
	(
          Spell_Primary_Diagnosis LIKE 'B3[34]%'
        OR
          Spell_Primary_Diagnosis LIKE 'B97%'
        OR
          -- ^J(?!0[^69])
          Spell_Primary_Diagnosis BETWEEN 'J060' AND 'J99X'
        OR
          Spell_Primary_Diagnosis LIKE 'U0[467]%'
      )
;
GO

-- Ambulatory care - chronic

-- ambulatory_care_conditions_chronic(angina).fsql
-- (100,449 rows affected)  
DROP TABLE IF EXISTS #temp_amb_chronic_angina
SELECT APCS_Ident, 'amb_chronic_angina' as 'flag' 
INTO #temp_amb_chronic_angina 
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND	
	(
	Spell_Primary_Diagnosis LIKE 'I20%'
	OR
	Spell_Primary_Diagnosis LIKE 'I24[089]%'
	)
	AND
	(
	First_Procedure_Code NOT LIKE '[ABCDEFGHJKLMNOPQRSTVW]%'
	OR
    First_Procedure_Code NOT LIKE 'X[01245]%'
	)
;
GO




-- ambulatory_care_conditions_chronic(asthma).fsql
-- (352,162 rows affected)
DROP TABLE IF EXISTS #temp_amb_chronic_asthma
SELECT APCS_Ident, 'amb_chronic_asthma' as 'flag' 
INTO #temp_amb_chronic_asthma 
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND	
	Spell_Primary_Diagnosis LIKE 'J4[56]%'	
;
GO

-- ambulatory_care_conditions_chronic(congestive_heart_failure).fsql
-- (328,284 rows affected)
DROP TABLE IF EXISTS #temp_amb_chronic_chf
SELECT APCS_Ident, 'amb_chronic_chf' as 'flag' 
INTO #temp_amb_chronic_chf
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND 
	(Spell_Primary_Diagnosis LIKE 'I110%'
     OR
     Spell_Primary_Diagnosis LIKE 'I50%'
     OR
     Spell_Primary_Diagnosis LIKE 'J81X%'
     )
	 AND 
	 (
	 First_Procedure_Code NOT LIKE 'K[0-4]%'
     OR
     First_Procedure_Code NOT LIKE 'K5[02567]%'
     OR
     First_Procedure_Code NOT LIKE 'K6[016789]%'
     OR
     First_Procedure_Code NOT LIKE 'K71%'
     )	
;
GO

-- ambulatory_care_conditions_chronic(copd).fsql
-- (784,125 rows affected)
DROP TABLE IF EXISTS #temp_amb_chronic_copd
SELECT APCS_Ident, 'amb_chronic_copd' as 'flag' 
INTO #temp_amb_chronic_copd
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND 
	(Spell_Primary_Diagnosis LIKE 'J4[12347]%'
     OR
     Spell_Primary_Diagnosis LIKE 'J20%'
     OR
     Spell_Primary_Diagnosis LIKE 'J4[12347]%'
     )	 
;
GO

-- ambulatory_care_conditions_chronic(diabetes_complications).fsql
-- 222,720 rows affected
DROP TABLE IF EXISTS #temp_amb_chronic_diab_compl
SELECT APCS_Ident, 'amb_chronic_diab_compl' as 'flag' 
INTO #temp_amb_chronic_diab_compl
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND 
	Spell_Primary_Diagnosis LIKE 'E1[0-4][0-8]%'
;
GO

-- ambulatory_care_conditions_chronic(hypertension).fsql
-- (44,842 rows affected)
DROP TABLE IF EXISTS #temp_amb_chronic_hypt
SELECT APCS_Ident, 'amb_chronic_hypt' as 'flag' 
INTO #temp_amb_chronic_hypt
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND 
	(Spell_Primary_Diagnosis LIKE 'I10X%'
     OR
     Spell_Primary_Diagnosis LIKE 'I119%'
     )
	 AND 
	 (
	 First_Procedure_Code NOT LIKE 'K[0-4]%'
     OR
     First_Procedure_Code NOT LIKE 'K5[02567]%'
     OR
     First_Procedure_Code NOT LIKE 'K6[016789]%'
     OR
     First_Procedure_Code NOT LIKE 'K71%'
     )	
;
GO

-- ambulatory_care_conditions_chronic(iron-deficiency_anaemia).fsql
-- 199,570 rows affected
DROP TABLE IF EXISTS #temp_amb_chronic_iron_anaemia
SELECT APCS_Ident, 'amb_chronic_iron_anaemia' as 'flag' 
INTO #temp_amb_chronic_iron_anaemia
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND 
	Spell_Primary_Diagnosis LIKE 'D50[189]%'
;
GO

-- ambulatory_care_conditions_chronic(nutritional_deficiencies).fsql
-- (943 rows affected)
DROP TABLE IF EXISTS #temp_amb_chronic_nutri_defic
SELECT APCS_Ident, 'amb_chronic_nutri_defic' as 'flag' 
INTO #temp_amb_chronic_nutri_defic
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND 
	(Spell_Primary_Diagnosis LIKE 'E4[0123]X%'
     OR
     Spell_Primary_Diagnosis LIKE 'E550%'
     OR
     Spell_Primary_Diagnosis LIKE 'E643%'
     )	 
;
GO

-- Combine all chronic ambulatory temporary tables
-- (2,033,095 rows affected) 
DROP TABLE IF EXISTS #temp_amb_chronic_comb
SELECT * 
INTO #temp_amb_chronic_comb
FROM (
	SELECT * FROM #temp_amb_chronic_angina
	UNION ALL
	SELECT * FROM #temp_amb_chronic_asthma
	UNION ALL
	SELECT * FROM #temp_amb_chronic_chf
	UNION ALL
	SELECT * FROM #temp_amb_chronic_copd
	UNION ALL
	SELECT * FROM #temp_amb_chronic_diab_compl
	UNION ALL
	SELECT * FROM #temp_amb_chronic_hypt
	UNION ALL
	SELECT * FROM #temp_amb_chronic_iron_anaemia
	UNION ALL
	SELECT * FROM #temp_amb_chronic_nutri_defic
	) a
;
GO

-- Ambulatory care - acute

-- ambulatory_care_conditions_acute(cellulitis).fsql
-- (146,989 rows affected)
DROP TABLE IF EXISTS #temp_amb_acute_cellulitis
SELECT APCS_Ident, 'amb_acute_cellulitis' as 'flag' 
INTO #temp_amb_acute_cellulitis 
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND	
	(
	Spell_Primary_Diagnosis LIKE 'L0[34]%'
	OR
	Spell_Primary_Diagnosis LIKE 'L08[089]%'
	OR
	Spell_Primary_Diagnosis LIKE 'L88X%'
	OR
	Spell_Primary_Diagnosis LIKE 'L980%'
	)
	AND
	(
	First_Procedure_Code NOT LIKE '[ABCDEFGHJKLMNOPQRTVW]%'
	OR
    First_Procedure_Code NOT LIKE 'S[123]%'
	OR
    First_Procedure_Code NOT LIKE 'S4[1234589]%'
	OR
    First_Procedure_Code NOT LIKE 'X0[1245]%'
	)
;
GO

-- ambulatory_care_conditions_acute(convulsions_and_epilepsy).fsql
-- (447,750 rows affected)
DROP TABLE IF EXISTS #temp_amb_acute_conv_epilep
SELECT APCS_Ident, 'amb_acute_conv_epilep' as 'flag' 
INTO #temp_amb_acute_conv_epilep 
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND	
	(
	Spell_Primary_Diagnosis LIKE 'G4[01]%'
	OR
	Spell_Primary_Diagnosis LIKE 'O15%'
	OR
	Spell_Primary_Diagnosis LIKE 'R56%'
	)	
;
GO

-- ambulatory_care_conditions_acute(dehydration_and_gastroenteritis).fsql
-- (83,716 rows affected)
DROP TABLE IF EXISTS #temp_amb_acute_dehydr_gastroent
SELECT APCS_Ident, 'amb_acute_dehydr_gastroent' as 'flag' 
INTO #temp_amb_acute_dehydr_gastroent 
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND	
	(
	Spell_Primary_Diagnosis LIKE 'E86X%'
	OR
	Spell_Primary_Diagnosis LIKE 'K52[289]%'
	)
;
GO

-- ambulatory_care_conditions_acute(dental_conditions).fsql
-- (99,842 rows affected)
DROP TABLE IF EXISTS #temp_amb_acute_dental
SELECT APCS_Ident, 'amb_acute_dental' as 'flag' 
INTO #temp_amb_acute_dental
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND	
	(
	Spell_Primary_Diagnosis LIKE 'A690%'
	OR
	Spell_Primary_Diagnosis LIKE 'K0[2-68]%'
	OR
	Spell_Primary_Diagnosis LIKE 'K09[89]%'
	OR
	Spell_Primary_Diagnosis LIKE 'K1[23]%'
	)
;
GO

-- ambulatory_care_conditions_acute(ent_infections).fsql
-- (639,311 rows affected)
DROP TABLE IF EXISTS #temp_amb_acute_ent_inf
SELECT APCS_Ident, 'amb_acute_ent_inf' as 'flag' 
INTO #temp_amb_acute_ent_inf
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND	
	(
	Spell_Primary_Diagnosis LIKE 'H6[67]%'
	OR
	Spell_Primary_Diagnosis LIKE 'J0[236]%'
	OR
	Spell_Primary_Diagnosis LIKE 'J312%'
	)
;
GO

-- ambulatory_care_conditions_acute(gangrene).fsql
-- (4,832 rows affected)
DROP TABLE IF EXISTS #temp_amb_acute_gangrene
SELECT APCS_Ident, 'amb_acute_gangrene' as 'flag' 
INTO #temp_amb_acute_gangrene
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND
	Spell_Primary_Diagnosis LIKE 'R02X%'
;
GO

-- ambulatory_care_conditions_acute(pelvic_inflammatory_disease).fsql
-- (48,106 rows affected)
DROP TABLE IF EXISTS #temp_amb_acute_pelvic_inflammatory_disease
SELECT APCS_Ident, 'amb_acute_pelvic_inflammatory_disease' as 'flag' 
INTO #temp_amb_acute_pelvic_inflammatory_disease
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND
	Spell_Primary_Diagnosis LIKE 'N7[034]%%'
;
GO

-- ambulatory_care_conditions_acute(perforated_bleeding_ulcer).fsql
-- (42,389 rows affected)
DROP TABLE IF EXISTS #temp_amb_acute_perforated_bleeding_ulcer
SELECT APCS_Ident, 'amb_acute_perforated_bleeding_ulcer' as 'flag' 
INTO #temp_amb_acute_perforated_bleeding_ulcer
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND
	Spell_Primary_Diagnosis LIKE 'K2[5-8][012456]%'
;
GO

-- ambulatory_care_conditions_acute(pyelonephritis).fsql
-- (188,169 rows affected)
DROP TABLE IF EXISTS #temp_amb_acute_pyelonephritis
SELECT APCS_Ident, 'amb_acute_pyelonephritis' as 'flag' 
INTO #temp_amb_acute_pyelonephritis
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND
	(
	Spell_Primary_Diagnosis LIKE 'N1[012]%'
	OR
	Spell_Primary_Diagnosis LIKE 'N136%'
	)
;
GO

-- Combine all acute ambulatory temporary tables
-- (1,701,104 rows affected)
DROP TABLE IF EXISTS #temp_amb_acute_comb
SELECT * 
INTO #temp_amb_acute_comb
FROM (
	SELECT * FROM #temp_amb_acute_cellulitis
	UNION ALL
	SELECT * FROM #temp_amb_acute_conv_epilep
	UNION ALL
	SELECT * FROM #temp_amb_acute_dehydr_gastroent
	UNION ALL
	SELECT * FROM #temp_amb_acute_dental
	UNION ALL
	SELECT * FROM #temp_amb_acute_ent_inf
	UNION ALL
	SELECT * FROM #temp_amb_acute_gangrene
	UNION ALL
	SELECT * FROM #temp_amb_acute_pelvic_inflammatory_disease
	UNION ALL
	SELECT * FROM #temp_amb_acute_perforated_bleeding_ulcer
	UNION ALL
	SELECT * FROM #temp_amb_acute_pyelonephritis
	) a
;
GO

-- Ambulatory care - vaccine preventable

-- ambulatory_care_conditions_vaccine_preventable(influenza_and_pneumonia).fsql
-- (1,032,352 rows affected)
DROP TABLE IF EXISTS #temp_amb_vacc_prev_flu_pne
SELECT APCS_Ident, 'amb_vacc_prev_flu_pne' as 'flag' 
INTO #temp_amb_vacc_prev_flu_pne 
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND	
	(
	Spell_Primary_Diagnosis LIKE 'J1[0134]%'
	OR
	Spell_Primary_Diagnosis LIKE 'J15[3479]%'
	OR
	Spell_Primary_Diagnosis LIKE 'J168%'
	OR
	Spell_Primary_Diagnosis LIKE 'J18[18]%'
	)
	AND
	Spell_Primary_Diagnosis NOT LIKE 'D57%'
;
GO

-- ambulatory_care_conditions_vaccine_preventable(other).fsql
-- (6,808 rows affected)
DROP TABLE IF EXISTS #temp_amb_vacc_prev_other
SELECT APCS_Ident, 'amb_vacc_prev_other' as 'flag' 
INTO #temp_amb_vacc_prev_other 
FROM #temp_base 
WHERE 
	Admission_Method LIKE '2%'
	AND	
	(
	Spell_Primary_Diagnosis LIKE 'A3[567]%'
	OR
	Spell_Primary_Diagnosis LIKE 'A80%'
	OR
	Spell_Primary_Diagnosis LIKE 'B0[56]%'
	OR
	Spell_Primary_Diagnosis LIKE 'B16[19]%'
	OR
	Spell_Primary_Diagnosis LIKE 'B18[01]%'
	OR
	Spell_Primary_Diagnosis LIKE 'B26%'
	OR
	Spell_Primary_Diagnosis LIKE 'G000%'
	OR
	Spell_Primary_Diagnosis LIKE 'M014%'
	)
;
GO

-- Combine all acute ambulatory temporary tables
-- (1,039,160 rows affected)
DROP TABLE IF EXISTS #temp_amb_vac_prev_comb
SELECT * 
INTO #temp_amb_vac_prev_comb
FROM (
	SELECT * FROM #temp_amb_vacc_prev_flu_pne
	UNION ALL
	SELECT * FROM #temp_amb_vacc_prev_other	
	) a
;
GO

----------------------------------------------------------------
-- Mortality data
USE NHSE_Mortality
GO
;

-- Identify deaths during period
-- (4,128,695 rows affected) 4139005
DROP TABLE IF EXISTS #mortality_2017_plus
SELECT
	[MORTALITY_IDENT],
	[DER_PSEUDO_NHS_NUMBER],
	CONVERT(DATE, REG_DATE_OF_DEATH) AS REG_DATE_OF_DEATH,
	[POD_CODE],
	[POD_NHS_ESTABLISHMENT],
	[POD_ESTABLISHMENT_TYPE]
INTO #mortality_2017_plus
FROM [NHSE_Mortality].[mort].[Mortality]
WHERE 
	[DER_DQ_VALID_NHS_NUMBER] = 'VAL'
	AND 
	[DER_DATEDEATH_YEAR] >= '2017'
	AND
	(
		(CCG_OF_RESIDENCE_CODE not like '7%' or CCG_OF_RESIDENCE_CODE is null) --england resi whether can find CCG or not
			OR 
			(
			CCG_OF_RESIDENCE_CODE like '7%'and (DER_NHAIS_CCG_OF_REGISTRATION is not null or CCG_OF_REGISTRATION_CODE is not null)
			)
		) --wales resi but eng ccg resp
	AND CANCELLED_FLAG<>'Y' --only 64 records
	AND DER_AGE_AT_DEATH>=18
	AND DER_PSEUDO_NHS_NUMBER is not null
;
GO

-- Add field to identify data 1 year before death
-- (4,128,695 rows affected) 4139005
DROP TABLE IF EXISTS #mortality_2017_plus_year_before
SELECT *,
	DATEADD(YEAR, -1, REG_DATE_OF_DEATH) AS one_year_before_death,
	case 
		when POD_CODE='H' then 'Home'
		when POD_CODE='E' then 'Elsewhere/Other'
		when POD_NHS_ESTABLISHMENT=1 and 
				POD_ESTABLISHMENT_TYPE in ('02','04','07','10','21','2','4','7') then 'Care Home'
		when POD_NHS_ESTABLISHMENT=1 and 
				POD_ESTABLISHMENT_TYPE in ('01','03','18','99','1','3') then 'Hospital' --CHECK POD ESTABLISHMENT TYPE =1/01 DO A GROUP AND CHECK OUT COMMUNAL_ESTABLISHMENT TOO. ARE THESE ALL BLANK? BY
		when POD_NHS_ESTABLISHMENT=2 and 
				POD_ESTABLISHMENT_TYPE in ('03','04','07','10','14','20','22','32','33','99','3','4','7') then 'Care Home'
		when POD_NHS_ESTABLISHMENT=2 and 
				POD_ESTABLISHMENT_TYPE in ('01','18','19','1') then 'Hospital'
		when POD_ESTABLISHMENT_TYPE in ('83') then 'Hospice'
		when POD_NHS_ESTABLISHMENT=1 and 
				POD_ESTABLISHMENT_TYPE in ('5','6','8','9','11','05','06','08','09') then 'Elsewhere/Other'
		when POD_NHS_ESTABLISHMENT=2 and 
		(POD_ESTABLISHMENT_TYPE in ('5','8','9','11','12','13','15','16','17','05','08','09') or 
			POD_ESTABLISHMENT_TYPE between '23' and '31' or POD_ESTABLISHMENT_TYPE between '34' and '82') then 'Elsewhere/Other'
	else 'Unknown' end as LocationType
INTO #mortality_2017_plus_year_before
FROM #mortality_2017_plus
;
GO

-- Link base to mortality data
-- (10,700,938 rows affected) 10731521
DROP TABLE IF EXISTS #temp_spells_in_year_before_death
SELECT
	a.APCS_IDent,
	a.Der_Pseudo_NHS_Number,
	a.Discharge_Date,
	b.MORTALITY_IDENT as 'mortality_ident',
	b.REG_DATE_OF_DEATH as 'reg_date_death',
	b.one_year_before_death,
	b.LocationType as 'death_location_type'
INTO #temp_spells_in_year_before_death
FROM #temp_base a
LEFT JOIN #mortality_2017_plus_year_before b
ON a.Der_Pseudo_NHS_Number = b.DER_PSEUDO_NHS_NUMBER
WHERE b.one_year_before_death < a.Discharge_Date  -- Spells that occured within 1 year of death
;
GO

----------------------------------------------------------------------------
-- Filter base table on flags
-- (29,668,646 rows affected)  23354061
DROP TABLE IF EXISTS #temp_base_reduced
SELECT 
	a.*,
	b.flag as 'flag_frail',
	c.flag as 'flag_eol',
	d.flag as 'flag_falls_exp',
	e.flag as 'flag_fall_imp_frac',
	f.flag as 'flag_fall_imp_tend',
	g.flag as 'flag_elderly_emergency',
	h.flag as 'amb_chronic',
	i.flag as 'amb_acute',
	j.flag as 'amb_vacc_prev',
	k.reg_date_death,
	k.death_location_type

INTO #temp_base_reduced
FROM #temp_base a
	LEFT JOIN #temp_frail				b ON a.APCS_Ident = b.APCS_Ident
	LEFT JOIN #temp_eol					c ON a.APCS_Ident = c.APCS_Ident
	LEFT JOIN #temp_falls_expl			d ON a.APCS_Ident = d.APCS_Ident
	LEFT JOIN #temp_falls_impl_fract	e ON a.APCS_Ident = e.APCS_Ident
	LEFT JOIN #temp_falls_impl_tend		f ON a.APCS_Ident = f.APCS_Ident
	LEFT JOIN #temp_elderly_emergency	g ON a.APCS_Ident = g.APCS_Ident
	LEFT JOIN #temp_amb_chronic_comb    h ON a.APCS_Ident = h.APCS_Ident
	LEFT JOIN #temp_amb_acute_comb		i ON a.APCS_Ident = i.APCS_ident
	LEFT JOIN #temp_amb_vac_prev_comb   j ON a.APCS_Ident = j.APCS_Ident
	LEFT JOIN #temp_spells_in_year_before_death k ON a.APCS_Ident = k.APCS_Ident
WHERE
	b.flag IS NOT NULL OR
	c.flag IS NOT NULL OR
	d.flag IS NOT NULL OR
	e.flag IS NOT NULL OR
	f.flag IS NOT NULL OR
	g.flag IS NOT NULL OR
	h.flag IS NOT NULL OR
	i.flag IS NOT NULL OR
	j.flag IS NOT NULL OR
	k.mortality_ident IS NOT NULL
; 
GO

-- Write into sandbox
-- (29,668,646 rows affected)
DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_frail_eld_eol]
SELECT * 
INTO [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_frail_eld_eol]
FROM #temp_base_reduced

-- Version 2  23354061
DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2]
SELECT * 
INTO [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2]
FROM #temp_base_reduced

---------------------------------------------------------------
-- inestigate frail activity

SELECT top 10 * FROM #temp_base

SELECT top 10 * FROM #temp_frail

-- 7791194
SELECT 
	a.*,
	b.flag as 'flag_frail'
INTO #test
FROM #temp_base a
LEFT JOIN #temp_frail b ON a.APCS_Ident = b.APCS_IDENT
WHERE
	b.flag IS NOT NULL

SELECT 
	Der_Activity_Month,
	count(distinct(APCS_Ident)) as 'admissions'
FROM #test
GROUP BY
	Der_Activity_Month
ORDER BY Der_Activity_Month DESC



