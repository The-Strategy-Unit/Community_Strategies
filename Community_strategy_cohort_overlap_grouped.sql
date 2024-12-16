 -- Format data for cohort overlap analysis

 USE [NHSE_Sandbox_StrategyUnit];
 GO

------
DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_overlap]
SELECT
    a.Der_Pseudo_NHS_Number,
	a.APCS_Ident,
	a.Der_Financial_Year,
	a.Sex,
	FLOOR((a.Der_Age_at_CDS_Activity_Date - 1) / 5) * 5 AS age_range,
	a.Ethnic_Group,
	c.IMD_Decile,
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
	a.Der_Spell_LoS

INTO [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_overlap]
FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_cohort_v2] a
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_Other_CCGToICB_2425] b 
ON a.Der_Commissioner_Code = b.Org_Code
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_Other_Deprivation_By_LSOA]c
ON a.Der_Postcode_LSOA_2011_Code=c.LSOA_Code

;
GO
