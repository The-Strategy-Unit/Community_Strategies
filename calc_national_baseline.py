# Databricks notebook source
import pyspark.sql.functions as F
from databricks.connect import DatabricksSession

spark = DatabricksSession.builder.getOrCreate()


# COMMAND ----------

nhp_apc = (
    spark.read.table("su_data.nhp.apc")
    .filter(F.col("fyear") == 201920)
    .filter(F.col("person_id").isNotNull())
)

# COMMAND ----------

# Load mitigators
mitigators = spark.read.table("su_data.nhp.apc_mitigators")
natioanal_summary = (
    spark.read.table("su_data.nhp.apc_mitigators")
    .join(nhp_apc, "epikey")
    .groupBy("strategy")
    .agg(F.sum("sample_rate").alias("sample_rate"),
         F.count("sample_rate").alias("admissions"),
         F.sum("SPELDUR").alias("bed_days"))
)