# AutomatedExceedanceDescriptiveEpi
An automated system to produce descriptive epidemiology for Public Health England SGSS exceedance reports

Production of the report is a two-stage process, described below. 

Stage 1: Obtain a linelist from SGSS
--------

•	This is an SQL query (produced by our local information team) where the user can specify a number of parameters to retrieve the appropriate data (important filters include the organism of interest, the centre and the start/end specimen dates – further data filtering can be performed in Stage 2)

•	Deduplication is embedded within the query 

•	The SQL query is titled SGSS_Extract_Data_Descriptive_Epidemiological_Report_SEaL.sql

• The user guide is titled User Guide.docx

•	For more information contact Jordan Wilson (jordan.wilson@phe.gov.uk)

Stage 2: Generate the automated exceedance descriptive epi report using R
--------

•	A Microsoft Word report is generating using a series of R scripts and a single Rmd script

•	The user may specify a number of parameters depending on the desired output (these include filters to specify appropriate reporting dates, sex, age, laboratory, requesting organisation, specimen type and travel, as well as figure aesthetics) 
