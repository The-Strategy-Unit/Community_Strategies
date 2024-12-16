-- Top procedures in cohort

USE [NHSE_Sandbox_StrategyUnit]
GO
;

-- Frail
SELECT 
	*
FROM (
	SELECT TOP 20
		First_Procedure_Code,
		count(distinct(APCS_Ident)) as 'frail_count'
	FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2]
	WHERE 
		flag_frail IS NOT NULL
	GROUP BY First_Procedure_Code
	ORDER BY count(distinct(APCS_Ident)) DESC
	) a
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_ClinCode_OPCS] b
ON a.First_Procedure_Code = b.[OPCS_L4_Code]
;

-- EoL
SELECT 
	*
FROM (
	SELECT TOP 20
		First_Procedure_Code,
		count(distinct(APCS_Ident)) as 'eol_count'
	FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2]
	WHERE 
		flag_eol IS NOT NULL
	GROUP BY First_Procedure_Code
	ORDER BY count(distinct(APCS_Ident)) DESC
	) a
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_ClinCode_OPCS] b
ON a.First_Procedure_Code = b.[OPCS_L4_Code]
;

-- Emergency elderly
SELECT 
	*
FROM (
	SELECT TOP 20
		First_Procedure_Code,
		count(distinct(APCS_Ident)) as 'emergency_elderly_count'
	FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2]
	WHERE 
		flag_elderly_emergency IS NOT NULL
	GROUP BY First_Procedure_Code
	ORDER BY count(distinct(APCS_Ident)) DESC
	) a
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_ClinCode_OPCS] b
ON a.First_Procedure_Code = b.[OPCS_L4_Code]
;

-- Falls
SELECT 
	*
FROM (
	SELECT TOP 20
		First_Procedure_Code,
		count(distinct(APCS_Ident)) as 'falls_count'
	FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2]
	WHERE 
		flag_falls_exp IS NOT NULL OR
		flag_fall_imp_frac IS NOT NULL OR
		flag_fall_imp_tend IS NOT NULL
	GROUP BY First_Procedure_Code
	ORDER BY count(distinct(APCS_Ident)) DESC
	) a
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_ClinCode_OPCS] b
ON a.First_Procedure_Code = b.[OPCS_L4_Code]
;

-- Ambulatory - chronic
SELECT 
	*
FROM (
	SELECT TOP 20
		First_Procedure_Code,
		count(distinct(APCS_Ident)) as 'amb_chronic_count'
	FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2]
	WHERE 
		amb_chronic IS NOT NULL 
	GROUP BY First_Procedure_Code
	ORDER BY count(distinct(APCS_Ident)) DESC
	) a
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_ClinCode_OPCS] b
ON a.First_Procedure_Code = b.[OPCS_L4_Code]
;

-- Ambulatory - acute
SELECT 
	*
FROM (
	SELECT TOP 20
		First_Procedure_Code,
		count(distinct(APCS_Ident)) as 'amb_acute_count'
	FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2]
	WHERE 
		amb_acute IS NOT NULL 
	GROUP BY First_Procedure_Code
	ORDER BY count(distinct(APCS_Ident)) DESC
	) a
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_ClinCode_OPCS] b
ON a.First_Procedure_Code = b.[OPCS_L4_Code]
;

-- Ambulatory - vaccine preventable
SELECT 
	*
FROM (
	SELECT TOP 20
		First_Procedure_Code,
		count(distinct(APCS_Ident)) as 'amb_vac_prev_count'
	FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2]
	WHERE 
		amb_vacc_prev IS NOT NULL 
	GROUP BY First_Procedure_Code
	ORDER BY count(distinct(APCS_Ident)) DESC
	) a
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_ClinCode_OPCS] b
ON a.First_Procedure_Code = b.[OPCS_L4_Code]
;

-- EoL - 1-year
SELECT 
	*
FROM (
	SELECT TOP 20
		First_Procedure_Code,
		count(distinct(APCS_Ident)) as 'eol_broad_count'
	FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2]
	WHERE 
		death_location_type IS NOT NULL 
	GROUP BY First_Procedure_Code
	ORDER BY count(distinct(APCS_Ident)) DESC
	) a
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_ClinCode_OPCS] b
ON a.First_Procedure_Code = b.[OPCS_L4_Code]
;
