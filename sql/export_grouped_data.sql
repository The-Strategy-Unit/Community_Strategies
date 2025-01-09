
 USE [NHSE_Sandbox_StrategyUnit];
 GO

-- Create temporary table to identify spells with errors in LoS measure to exclude below
DROP TABLE IF EXISTS #temp_los_check
SELECT  
	APCS_Ident,
	Admission_Date,
	Discharge_Date,
	Der_Spell_LoS,
	DATEDIFF(day, Admission_Date, Discharge_Date) AS 'LoS_check' 
INTO #temp_los_check
FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2] 
WHERE Der_Spell_LoS != DATEDIFF(day, Admission_Date, Discharge_Date)

-- Group and count from base extract (11,219,496 rows affected)
DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2_grouped]
SELECT
	a.Der_Activity_Month,
	a.Sex,
	--FLOOR((a.Der_Age_at_CDS_Activity_Date - 1) / 5) * 5 AS age_range,
	CASE 
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 0 AND 4 THEN '0-4'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 5 AND 9 THEN '5-9'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 10 AND 14 THEN '10-14'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 15 AND 19 THEN '15-19'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 20 AND 24 THEN '20-24'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 25 AND 29 THEN '25-29'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 30 AND 34 THEN '30-34'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 35 AND 39 THEN '35-39'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 40 AND 44 THEN '40-44'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 45 AND 49 THEN '45-49'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 50 AND 54 THEN '50-54'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 55 AND 59 THEN '55-59'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 60 AND 64 THEN '60-64'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 65 AND 69 THEN '65-69'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 70 AND 74 THEN '70-74'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 75 AND 79 THEN '75-79'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 80 AND 84 THEN '80-84'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 85 AND 89 THEN '85-89'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 90 AND 94 THEN '90-94'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 95 AND 99 THEN '95-99'
		WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 100 AND 104 THEN '100-104'
		WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 105 AND 109 THEN '105-109'
        ELSE 'NA'
    END AS age_range,
	a.Ethnic_Group,
	c.[IMD_Decile],
	CASE 
	WHEN a.Admission_Method IN ('11', '12', '13') THEN 'elective'
	WHEN a.Admission_Method IN ('21', '22', '23', '24', '25', '2A','2B', '2C','2D','28') THEN 'emergency'
	WHEN a.Admission_Method IN ('31', '32','82','83','81') THEN 'maternity/other'
	END 
	AS 'Admission_Method_group',

	a.Discharge_Method,
	--FLOOR((a.Der_Spell_LoS - 1) / 5) * 5 AS LoS_range,
	b.ICB_Name_Short,
	a.flag_frail,
	a.flag_eol,
	a.flag_falls_exp,
	a.flag_fall_imp_frac,
	a.flag_fall_imp_tend,
	a.flag_elderly_emergency,
	a.amb_chronic,
	a.amb_acute,
	a.amb_vacc_prev,
	a.death_location_type,
	
	count(distinct(Der_Pseudo_NHS_Number)) as 'person_n',
	count(distinct(APCS_ident)) as 'spells',
	sum(Der_Spell_LoS) as 'los_sum'

INTO [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2_grouped] 
FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2] a
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_Other_CCGToICB_2425] b 
	ON a.Der_Commissioner_Code = b.Org_Code
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_Other_Deprivation_By_LSOA] c
	ON a.Der_Postcode_LSOA_2011_Code = c.[LSOA_Code]

WHERE 
	c.Effective_Snapshot_Date = '2019-12-31'
	AND
	NOT EXISTS (
    SELECT 1
    FROM #temp_los_check AS temp
    WHERE temp.APCS_Ident = a.APCS_Ident
	)

GROUP BY 
	a.Der_Activity_Month,
	a.Sex,
	CASE 
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 0 AND 4 THEN '0-4'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 5 AND 9 THEN '5-9'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 10 AND 14 THEN '10-14'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 15 AND 19 THEN '15-19'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 20 AND 24 THEN '20-24'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 25 AND 29 THEN '25-29'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 30 AND 34 THEN '30-34'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 35 AND 39 THEN '35-39'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 40 AND 44 THEN '40-44'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 45 AND 49 THEN '45-49'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 50 AND 54 THEN '50-54'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 55 AND 59 THEN '55-59'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 60 AND 64 THEN '60-64'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 65 AND 69 THEN '65-69'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 70 AND 74 THEN '70-74'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 75 AND 79 THEN '75-79'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 80 AND 84 THEN '80-84'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 85 AND 89 THEN '85-89'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 90 AND 94 THEN '90-94'
        WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 95 AND 99 THEN '95-99'
		WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 100 AND 104 THEN '100-104'
		WHEN a.Der_Age_at_CDS_Activity_Date BETWEEN 105 AND 109 THEN '105-109'
        ELSE 'NA'
    END,	
	--FLOOR((a.Der_Age_at_CDS_Activity_Date - 1) / 5) * 5,
	a.Ethnic_Group,
	c.IMD_Decile,
	CASE 
	WHEN a.Admission_Method IN ('11', '12', '13') THEN 'elective'
	WHEN a.Admission_Method IN ('21', '22', '23', '24', '25', '2A','2B', '2C','2D','28') THEN 'emergency'
	WHEN a.Admission_Method IN ('31', '32','82','83','81') THEN 'maternity/other'
	END,

	a.Discharge_Method,
	--FLOOR((a.Der_Spell_LoS - 1) / 5) * 5,
	b.ICB_Name_Short,
	a.flag_frail,
	a.flag_eol,
	a.flag_falls_exp,
	a.flag_fall_imp_frac,
	a.flag_fall_imp_tend,
	a.flag_elderly_emergency,
	a.amb_chronic,
	a.amb_acute,
	a.amb_vacc_prev,
	a.death_location_type
;
GO

