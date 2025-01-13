 -- Format data for time to readmission/ time to death

 USE [NHSE_Sandbox_StrategyUnit];
 GO

DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_time_to_readmission]

SELECT *,
LEAD(Admission_Date) OVER(PARTITION by Der_Pseudo_NHS_Number
							ORDER BY Admission_Date
                          ) as next_admission_date

INTO [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_time_to_readmission]
FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_frail_eld_eol] 

------
DROP TABLE IF EXISTS [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_time_to_readmission_final]
SELECT
    a.Der_Pseudo_NHS_Number,
	a.Der_Financial_Year,
	a.Sex,
	a.Der_Age_at_CDS_Activity_Date,
	a.Ethnic_Group,
	c.IMD_Decile,
	DATEDIFF(day, a.Discharge_Date, a.next_admission_date) as time_to_readmission,
	DATEDIFF(day, a.Admission_Date, d.REG_DATE_OF_DEATH) as time_to_death,
	DATEDIFF(day, a.Discharge_Date, d.REG_DATE_OF_DEATH) as time_to_death_from_discharge,
	a.Admission_Date,
	b.ICB_Name_Short,
	a.flag_frail,
	a.flag_eol,
	a.flag_falls_exp,
	a.flag_fall_imp_frac,
	a.flag_fall_imp_tend,
	a.flag_elderly_emergency,
	d.REG_DATE_OF_DEATH
	
	---count(distinct(APCS_ident)) as 'spells'

INTO [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_time_to_readmission_final]
FROM [NHSE_Sandbox_StrategyUnit].[dbo].[nhse_comm_strat_time_to_readmission] a
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_Other_CCGToICB_2425] b 
ON a.Der_Commissioner_Code = b.Org_Code
LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_Other_Deprivation_By_LSOA]c
ON a.Der_Postcode_LSOA_2011_Code=c.LSOA_Code
LEFT JOIN [NHSE_Mortality].[mort].[Mortality] d
ON a.Der_Pseudo_NHS_Number= d.DER_PSEUDO_NHS_NUMBER

;
GO
