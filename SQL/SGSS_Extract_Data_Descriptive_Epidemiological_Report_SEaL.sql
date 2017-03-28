/*
See User Guide in same folder in file system.
*/





use sgssdw

set dateformat dmy

declare @Organism varchar(50)
	,@StartDate date
	,@EndDate date
	,@MicrobiologyLaboratory_SGSSName varchar(50)
	,@SuperOutputArea varchar(25)
	,@DataSet varchar(10)
	,@Serotype varchar(50)
	,@Phagetype varchar(50)
	,@SpecimenType varchar(50)
	,@Antimicrobial varchar(50)





------------------------------------------------------------------------------------------------------------
--ENTER PASS-IN ARGUMENTS
--Mandatory -
set @Organism='listeria'
set	@Antimicrobial=''
set	@StartDate='1 jan 2013'
set	@EndDate='31 jan 2017'

--One or the other -
set	@MicrobiologyLaboratory_SGSSName=''
set	@SuperOutputArea='s east' -- either 's east' or 'london'

--All optional -
set @DataSet=''
set	@Phagetype=''
set	@Serotype=''
set	@SpecimenType=''
------------------------------------------------------------------------------------------------------------





if @SuperOutputArea='south west'
set @SuperOutputArea='s west'


select DataSet='Organism'
	,pid.SPECIMEN_NUMBER SpecimenCode
	,core.CDR_OPIE_ID RecordCode
	,organism.Organism_Species_Name Organism
	,organism.Serotype
	,organism.Phagetype
	,organism.Toxin_Type ToxinType
	,case specimentype.Specimen_Group_Code
		when 'T0X0'
			then 'Blood'
		when 'T6Y1'
			then 'Faeces or gut'
		when 'T201'
			then 'Respiratory tract'
		when 'T202'
			then 'Respiratory tract'
		when 'T701'
			then 'Urine'
		else 'Other'
		end SpecimenTypeClass
	,specimentype.Specimen_Type_Description SpecimenType
	,testmethod.Test_Method_Description TestMethod
	,antimicrobial.Antimicrobial_Description Antimicrobial
	,antimicrobial.Sensitivity_Result_Code SensitivityTestResult
	,reportinglaboratory.Lab_Geography_Name_Current ReportingLaboratory
	,cast(specimendate.[Date] as date) SpecimenDate
	,specimendate.Calendar_Quarter [Quarter]
	,requestororganisation.Site_Name RequestorOrganisation
	,requestororganisation.PostCode RequestorOrganisationPostcode
	,requestororganisationtype.Requesting_Organisation_Type_Description RequestorOrganisationType
	,pid.WARD RequestorOrganisationWard
	,sourcelaboratory.[Lab_Geography_Name_Current] SourceLaboratory
	,pid.Reference_Lab_Name ReferenceLaboratory
	,demographic.Age_in_Years AgeInYears
	,demographic.Age_in_Months AgeInMonths
	,case
		when demographic.Age_in_Years<5
			then '0-4 years'
		when demographic.Age_in_Years<20
			then '5-19 years'
		when demographic.Age_in_Years<45
			then '20-44 years'
		when demographic.Age_in_Years<64
			then '45-64 years'
		when demographic.Age_in_Years>=65
			then '65+ years'
		else 'Unknown'
		end AgeGroup
	,demographic.Patient_Sex Sex
	,superoutputarea.PHE_Region_Name GeographicalRegion
	,superoutputarea.Local_Authority_Name LocalAuthority
	,superoutputarea.PHE_Centre_Name PHECentre
	,indicators.Travel_Abroad_Indicator TravelledAbroadFlag
	,pid.TRAVEL_INFORMATION TravelledAbroadDetails
	,pid.PATIENT_NHS_NUMBER PID_NHSNumber
	,pid.HOSPITAL_PATIENT_NUMBER PID_LocalPatientCode
	,pid.Patient_Forename PID_Forename
	,pid.Patient_Surname PID_Surname
	,pid.Patient_Date_Of_Birth PID_BirthDate
	,pid.Patient_PostCode PID_Postcode
	,pid.LAB_COMMENTS PID_LaboratoryComments

into #Preprocessing_1

from FACT_OPIE_AND_SPECIMEN_REQUEST core

	left join DIMENSION_ORGANISM organism
		on core.Organism_SK=organism.Organism_SK
	left join DIMENSION_DATE specimendate
		on core.Specimen_Date_SK=specimendate.Date_SK
	left join DIMENSION_TEST_METHOD testmethod
		on core.test_method_sk=testmethod.Test_Method_SK
	left join DIMENSION_SPECIMEN_BACTERAEMIA specimentype
		on core.[Specimen/Bacteraemia_SK]=specimentype.Specimen_Bacteraemia_SK
	left join DIMENSION_SUPER_OUTPUT_AREA superoutputarea
		on core.Super_Output_Area_SK=superoutputarea.Lower_Super_Output_Area_SK
	left join DIMENSION_LAB_GEOGRAPHY reportinglaboratory
		on core.Reporting_Lab_Geography_SK=reportinglaboratory.Lab_Geography_SK
	left join DIMENSION_INDICATORS indicators
		on core.Indicators_SK=indicators.Indicators_SK
	left join DIMENSION_REQUESTING_ORGANISATION requestororganisationtype
		on core.Requesting_Organisation_SK=requestororganisationtype.Medical_Requestor_SK
	left join DIMENSION_SITE_GEOGRAPHY requestororganisation
		on core.Site_Geography_SK=requestororganisation.Site_Geography_SK
	left join DIMENSION_DEMOGRAPHIC demographic
		on core.Demographic_SK=demographic.Demographic_SK
	left join DIMENSION_LAB_GEOGRAPHY sourcelaboratory
		on core.OPIE_Source_Lab_Geography_SK=sourcelaboratory.[Lab_Geography_SK]

	--left join DIMENSION_VACCINATION_STATUS vaccination
		--on core.VACCINATION_STATUS_SKEY=vaccination.Vaccination_Status_SK
	--left join [dbo].[GP_GEOGRAPHY] requestorgp
		--on core.GP_Geography_SK=requestorgp.GP_Geography_SK
	--left join DIMENSION_HOSPITAL_CONSULTANT medicalconsultant
		--on core.Hospital_Consultant_Geography_SK=medicalconsultant.Hospital_Consultant_SK
	--left join GP_Practice_Geography requestormedicalpractice
		--on core.Patient_GP_Practice_Geography_SK=requestormedicalpractice.GP_Practice_Geography_SK
	--left join FACT_AMR_SUSCEPTIBILITY core3
		--on antimicrobial.Antimicrobial_Result_SK=core3.Antimicrobial_Result_SK

	left join OUTRIGGER_SPECIMEN_REQUEST pid
		on core.CDR_Specimen_Request_SK=pid.Specimen_Request_Outrigger_SK
	left join REF_CDR_SUSCEPTIBILITY bridge
		on pid.Specimen_Request_Outrigger_SK=bridge.Outrigger_SK
	left join DIMENSION_ANTIMICROBIAL_RESULT antimicrobial
		on bridge.Antimicrobial_Result_SK=antimicrobial.Antimicrobial_Result_SK

where organism.Organism_Species_Name like '%'+@Organism+'%'
	and (specimendate.[Date] between @StartDate and @EndDate)

	/*
	and (
	(@MicrobiologyLaboratory_SGSSName='all'
	and reportinglaboratory.Lab_Geography_Name_Current in (
	'NORTH DEVON DISTRICT HOSPITAL (BARNSTAPLE)'
	,'ROYAL BOURNEMOUTH HOSPITAL'
	,'DORCHESTER MICROBIOLOGY LABORATORY'
	,'EXETER MICROBIOLOGY LABORATORY'
	,'GLOUCESTERSHIRE ROYAL HOSPITAL'
	,'HPA SOUTH WEST BRISTOL LAB'
	,'DERRIFORD HOSP. (PLYMOUTH)'
	,'POOLE MICROBIOLOGY LABORATORY'
	,'SALISBURY MICROBIOLOGY LABORATORY'
	,'SOUTHMEAD HOSPITAL (BRISTOL)'
	,'GREAT WESTERN HOSPITAL'
	,'TAUNTON MICROBIOLOGY LABORATORY'
	,'TORBAY HOSPITAL (TORQUAY)'
	,'TRURO MICROBIOLOGY LABORATORY'
	,'WESTON GENERAL HOSPITAL (WESTON-SUPER-MARE)'
	,'ROYAL UNITED HOSPITAL (BATH)'
	,'SEVERN PATHOLOGY'
	)
	)
	*/

	and (reportinglaboratory.Lab_Geography_Name_Current=@MicrobiologyLaboratory_SGSSName or @MicrobiologyLaboratory_SGSSName='')
	and (superoutputarea.PHE_region_name=@SuperOutputArea or @SuperOutputArea='')

	and(organism.Serotype=@Serotype or @Serotype='')
	and(organism.Phagetype=@Phagetype or @Phagetype='')
	and(antimicrobial.Antimicrobial_Description=@Antimicrobial or @Antimicrobial='')

union all

select DataSet='Antimicrobial'
	,pid3.SPECIMEN_NUMBER SpecimenCode
	,RecordCode=0
	,organism3.Organism_Species_Name Organism
	,organism3.Serotype
	,organism3.Phagetype
	,organism3.Toxin_Type ToxinType
	,case specimentype3.Specimen_Group_Code
		when 'T0X0'
			then 'Blood'
		when 'T6Y1'
			then 'Faeces or gut'
		when 'T201'
			then 'Respiratory tract'
		when 'T202'
			then 'Respiratory tract'
		when 'T701'
			then 'Urine'
		else 'Other'
		end SpecimenTypeClass
	,specimentype3.Specimen_Type_Description SpecimenType
	,TestMethod='N/a'
	,antimicrobial3.Antimicrobial_Description Antimicrobial
	,antimicrobial3.Sensitivity_Result_Code SensitivityTestResult
	,reportinglaboratory3.Lab_Geography_Name_Current ReportingLaboratory
	,cast(specimendate3.[Date] as date) SpecimenDate
	,specimendate3.Calendar_Quarter [Quarter]
	,requestororganisation3.Site_Name RequestorOrganisation
	,requestororganisation3.PostCode RequestorOrganisationPostcode
	,requestororganisationtype3.Requesting_Organisation_Type_Description RequestorOrganisationType
	,RequestorOrganisationWard='N/a'
	,SourceLaboratory='N/a'
	,ReferenceLaboratory='N/a'
	,demographic3.Age_in_Years AgeInYears
	,demographic3.Age_in_Months AgeInMonths
	,case
		when demographic3.Age_in_Years<5
			then '0-4 years'
		when demographic3.Age_in_Years<20
			then '5-19 years'
		when demographic3.Age_in_Years<45
			then '20-44 years'
		when demographic3.Age_in_Years<64
			then '45-64 years'
		when demographic3.Age_in_Years>=65
			then '65+ years'
		else 'Unknown'
		end AgeGroup
	,demographic3.Patient_Sex Sex
	,superoutputarea3.PHE_region_name GeographicalRegion
	,superoutputarea3.Local_Authority_Name LocalAuthority
	,superoutputarea3.PHE_Centre_Name PHECentre
	,TravelledAbroadFlag=''
	,TravelledAbroadDetails='N/a'
	,pid3.PATIENT_NHS_NUMBER PID_NHSNumber
	,pid3.HOSPITAL_PATIENT_NUMBER PID_LocalPatientCode
	,pid3.Patient_Forename PID_Forename
	,pid3.Patient_Surname PID_Surname
	,pid3.Patient_Date_Of_Birth PID_BirthDate
	,pid3.Patient_PostCode PID_Postcode
	,pid3.LAB_COMMENTS PID_LaboratoryComments
from FACT_AMR_SUSCEPTIBILITY core3

	left join DIMENSION_REQUESTING_ORGANISATION requestororganisationtype3
		on core3.[Requesting Organisation_SK]=requestororganisationtype3.Medical_Requestor_SK
	left join DIMENSION_SITE_GEOGRAPHY requestororganisation3
		on core3.Site_Geography_SK=requestororganisation3.Site_Geography_SK
	left join DIMENSION_DATE specimendate3
		on core3.Specimen_Date_SK=specimendate3.Date_SK
	left join DIMENSION_LAB_GEOGRAPHY reportinglaboratory3
		on core3.Reporting_Lab_Geography_SK=reportinglaboratory3.Lab_Geography_SK
	left join DIMENSION_SUPER_OUTPUT_AREA superoutputarea3
		on core3.Super_Output_Area_Geography_SK=superoutputarea3.Lower_Super_Output_Area_SK
	left join DIMENSION_ORGANISM organism3
		on core3.Organism_SK=organism3.Organism_SK
	left join DIMENSION_SPECIMEN_BACTERAEMIA specimentype3
		on core3.Specimen_Bacteraemia_SK=specimentype3.Specimen_Bacteraemia_SK
	left join DIMENSION_DEMOGRAPHIC demographic3
		on core3.Demographic_SK=demographic3.Demographic_SK
	
	--left join [dbo].[GP_GEOGRAPHY] requestorgp2
		--on core2.Submitting_GP_Practice_Geography_SK=requestorgp2.GP_Geography_SK
	--left join DIMENSION_INDICATORS indicators3
		--on core4.Indicators_SK=indicators3.Indicators_SK
	--left join REF_CDR_SUSCEPTIBILITY bridge3
		--on core3.AMR_Specimen_Request_Susceptibility_SK=bridge3.Outrigger_SK
	--left join OUTRIGGER_SPECIMEN_REQUEST pid4
		--on bridge3.Outrigger_SK=pid4.Specimen_Request_Outrigger_SK
	--left join FACT_OPIE_AND_SPECIMEN_REQUEST core4
		--on pid4.Specimen_Request_Outrigger_SK=core4.CDR_Specimen_Request_SK
	
	left join DIMENSION_ANTIMICROBIAL_RESULT antimicrobial3
		on core3.Antimicrobial_Result_SK=antimicrobial3.Antimicrobial_Result_SK
	left join OUTRIGGER_AMR_SUSCEPTIBILITY pid3
		on core3.AMR_Specimen_Request_Susceptibility_SK=pid3.Outrigger_AMR_Susceptibility_SK

where organism3.Organism_Species_Name like '%'+@Organism+'%'
	and (specimendate3.[Date] between @StartDate and @EndDate)

	/*
	and (
	(@MicrobiologyLaboratory_SGSSName='all'
	and reportinglaboratory.Lab_Geography_Name_Current in (
	'NORTH DEVON DISTRICT HOSPITAL (BARNSTAPLE)'
	,'ROYAL BOURNEMOUTH HOSPITAL'
	,'DORCHESTER MICROBIOLOGY LABORATORY'
	,'EXETER MICROBIOLOGY LABORATORY'
	,'GLOUCESTERSHIRE ROYAL HOSPITAL'
	,'HPA SOUTH WEST BRISTOL LAB'
	,'DERRIFORD HOSP. (PLYMOUTH)'
	,'POOLE MICROBIOLOGY LABORATORY'
	,'SALISBURY MICROBIOLOGY LABORATORY'
	,'SOUTHMEAD HOSPITAL (BRISTOL)'
	,'GREAT WESTERN HOSPITAL'
	,'TAUNTON MICROBIOLOGY LABORATORY'
	,'TORBAY HOSPITAL (TORQUAY)'
	,'TRURO MICROBIOLOGY LABORATORY'
	,'WESTON GENERAL HOSPITAL (WESTON-SUPER-MARE)'
	,'ROYAL UNITED HOSPITAL (BATH)'
	,'SEVERN PATHOLOGY'
	)
	)
	*/

	and (reportinglaboratory3.Lab_Geography_Name_Current=@MicrobiologyLaboratory_SGSSName or @MicrobiologyLaboratory_SGSSName='')
	and (superoutputarea3.PHE_region_name=@SuperOutputArea or @SuperOutputArea='')

	and(organism3.Serotype=@Serotype or @Serotype='')
	and(organism3.Phagetype=@Phagetype or @Phagetype='')
	and(antimicrobial3.Antimicrobial_Description=@Antimicrobial or @Antimicrobial='')

order by DataSet,SpecimenDate


if @DataSet='org'
delete from #Preprocessing_1
where DataSet='Antimicrobial'


if @DataSet='ant'
delete from #Preprocessing_1
where DataSet='Organism'


select identity(int,1,1) DeduplicationCode
	,SpecimenCode
	,RecordCode
	,upper(Organism) Organism
	,upper(Serotype) Serotype
	,upper(Phagetype) Phagetype
	,upper(ToxinType) ToxinType
	,upper(SpecimenTypeClass) SpecimenTypeClass
	,upper(SpecimenType) SpecimenType
	,upper(TestMethod) TestMethod
	,upper(Antimicrobial) Antimicrobial
	,upper(SensitivityTestResult) SensitivityTestResult
	,upper(ReportingLaboratory) ReportingLaboratory
	,SpecimenDate
	,[Quarter]
	,upper(RequestorOrganisation) RequestorOrganisation
	,upper(RequestorOrganisationPostcode) RequestorOrganisationPostcode
	,upper(RequestorOrganisationType) RequestorOrganisationType
	,upper(RequestorOrganisationWard) RequestorOrganisationWard
	,upper(SourceLaboratory) SourceLaboratory
	,upper(ReferenceLaboratory) ReferenceLaboratory
	,AgeInYears
	,AgeInMonths
	,upper(AgeGroup) AgeGroup
	,upper(Sex) Sex
	,upper(GeographicalRegion) GeographicalRegion
	,upper(LocalAuthority) LocalAuthority
	,upper(PHECentre) PHECentre
	,upper(TravelledAbroadFlag) TravelledAbroadFlag
	,upper(TravelledAbroadDetails) TravelledAbroadDetails
	,PID_NHSNumber
	,upper(PID_LocalPatientCode) PID_LocalPatientCode
	,upper(PID_Forename) PID_Forename
	,upper(PID_Surname) PID_Surname
	,PID_BirthDate
	,upper(PID_Postcode) PID_Postcode
	,upper(PID_LaboratoryComments) PID_LaboratoryComments
into #Preprocessing_2
from #Preprocessing_1
order by SpecimenCode,SpecimenDate


/*
delete from #Preprocessing_2

where PID_NHSNumber in (
		'*ACCURUN*'
		,'0000NEQAS'
		,'1111111'
		,'1402171'
		,'AAA'
		,'ACCURUN'
		,'BACT'
		,'BACTERIOLOGY'
		,'BACTI'
		,'EDITESTPATIENT'
		,'EQA'
		,'IQA'
		,'IQA2013'
		,'IQA2014'
		,'IQA2015'
		,'IQA2016'
		,'K00538F'
		,'Keistra'
		,'Kiestra'
		,'LABQUALITY'
		,'MICIQA2012'
		,'MICIQA2012'
		,'MICROBIOLOGY'
		,'NEQAS'
		,'PHARMACY'
		,'PID'
		,'QAFA'
		,'QAMA'
		,'QAMC'
		,'QC1'
		,'QCMD'
		,'QCMI BC1'
		,'QCMI BC2'
		,'QMI'
		,'QUALITY ASS'
		,'SEROLOGY'
		,'STERILITY'
		,'TESTPATIENT'
		,'TESTPHARMACY'
		,'UKMEQUAS'
		,'UKNEQASMI'
		,'XXTESTPATIENTAFJL'
		)
	or PID_LocalPatientCode in (
		'*ACCURUN*'
		,'0000NEQAS'
		,'1111111'
		,'1402171'
		,'AAA'
		,'ACCURUN'
		,'BACT'
		,'BACTERIOLOGY'
		,'BACTI'
		,'EDITESTPATIENT'
		,'EQA'
		,'IQA'
		,'IQA2013'
		,'IQA2014'
		,'IQA2015'
		,'IQA2016'
		,'K00538F'
		,'Keistra'
		,'Kiestra'
		,'LABQUALITY'
		,'MICIQA2012'
		,'MICIQA2012'
		,'MICROBIOLOGY'
		,'NEQAS'
		,'PHARMACY'
		,'PID'
		,'QAFA'
		,'QAMA'
		,'QAMC'
		,'QC1'
		,'QCMD'
		,'QCMI BC1'
		,'QCMI BC2'
		,'QMI'
		,'QUALITY ASS'
		,'SEROLOGY'
		,'STERILITY'
		,'TESTPATIENT'
		,'TESTPHARMACY'
		,'UKMEQUAS'
		,'UKNEQASMI'
		,'XXTESTPATIENTAFJL'
		)
	or PID_Forename in (
		'*ACCURUN*'
		,'0000NEQAS'
		,'1111111'
		,'1402171'
		,'AAA'
		,'ACCURUN'
		,'BACT'
		,'BACTERIOLOGY'
		,'BACTI'
		,'EDITESTPATIENT'
		,'EQA'
		,'IQA'
		,'IQA2013'
		,'IQA2014'
		,'IQA2015'
		,'IQA2016'
		,'K00538F'
		,'Keistra'
		,'Kiestra'
		,'LABQUALITY'
		,'MICIQA2012'
		,'MICIQA2012'
		,'MICROBIOLOGY'
		,'NEQAS'
		,'PHARMACY'
		,'PID'
		,'QAFA'
		,'QAMA'
		,'QAMC'
		,'QC1'
		,'QCMD'
		,'QCMI BC1'
		,'QCMI BC2'
		,'QMI'
		,'QUALITY ASS'
		,'SEROLOGY'
		,'STERILITY'
		,'TESTPATIENT'
		,'TESTPHARMACY'
		,'UKMEQUAS'
		,'UKNEQASMI'
		,'XXTESTPATIENTAFJL'
		)
	or PID_Surname in (
		'*ACCURUN*'
		,'0000NEQAS'
		,'1111111'
		,'1402171'
		,'AAA'
		,'ACCURUN'
		,'BACT'
		,'BACTERIOLOGY'
		,'BACTI'
		,'EDITESTPATIENT'
		,'EQA'
		,'IQA'
		,'IQA2013'
		,'IQA2014'
		,'IQA2015'
		,'IQA2016'
		,'K00538F'
		,'Keistra'
		,'Kiestra'
		,'LABQUALITY'
		,'MICIQA2012'
		,'MICIQA2012'
		,'MICROBIOLOGY'
		,'NEQAS'
		,'PHARMACY'
		,'PID'
		,'QAFA'
		,'QAMA'
		,'QAMC'
		,'QC1'
		,'QCMD'
		,'QCMI BC1'
		,'QCMI BC2'
		,'QMI'
		,'QUALITY ASS'
		,'SEROLOGY'
		,'STERILITY'
		,'TESTPATIENT'
		,'TESTPHARMACY'
		,'UKMEQUAS'
		,'UKNEQASMI'
		,'XXTESTPATIENTAFJL'
		)
	or PID_NHSNumber like 'mqc%'
	or PID_NHSNumber like 'accurun%'
	or PID_LocalPatientCode like 'mqc%'
	or PID_LocalPatientCode like 'accurun%'
	or PID_Forename like 'mqc%'
	or PID_Forename like 'accurun%'
	or PID_Surname like '%mqc%'
	or PID_Surname like 'accurun%'
*/


select DeduplicationCode
	,SpecimenCode
	,RecordCode
	,Organism
	,Serotype
	,Phagetype
	,ToxinType
	,SpecimenTypeClass
	,SpecimenType
	,TestMethod
	,Antimicrobial
	,SensitivityTestResult
	,ReportingLaboratory
	,SpecimenDate
	,[Quarter]
	,RequestorOrganisation
	,RequestorOrganisationPostcode
	,RequestorOrganisationType
	,RequestorOrganisationWard
	,SourceLaboratory
	,ReferenceLaboratory
	,AgeInYears
	,AgeInMonths
	,AgeGroup
	,Sex
	,GeographicalRegion
	,LocalAuthority
	,PHECentre
	,TravelledAbroadFlag
	,TravelledAbroadDetails
	,PID_NHSNumber
	,PID_LocalPatientCode
	,PID_Forename
	,PID_Surname
	,PID_BirthDate
	,PID_Postcode
	,PID_LaboratoryComments
into #Preprocessing_3
from #Preprocessing_2
order by SpecimenCode,SpecimenDate


delete from #Preprocessing_2
where PID_NHSNumber is not null
	and DeduplicationCode not in (
		select max(DeduplicationCode)
		from #Preprocessing_2
		group by PID_NHSNumber
		)


delete from #Preprocessing_2
where PID_Surname is not null
	and PID_Forename is not null
	and DeduplicationCode not in (
		select max(DeduplicationCode)
		from #Preprocessing_2
		group by PID_Surname+'~'+PID_Forename+'~'+convert(varchar,PID_BirthDate)
		)


delete from #Preprocessing_2
where PID_LocalPatientCode is not null
	and PID_BirthDate is not null
	and DeduplicationCode not in (
		select max(DeduplicationCode)
		from #Preprocessing_2
		group by PID_LocalPatientCode+'~'+convert(varchar,PID_BirthDate)
		)


delete from #Preprocessing_2
where SpecimenCode is not null
	and DeduplicationCode not in (
		select max(DeduplicationCode)
		from #Preprocessing_2
		group by SpecimenCode
		)


select PID_NHSNumber
	,lag(PID_NHSNumber) over (order by PID_NHSNumber) PID_NHSNumberAbove
	,SpecimenDate
	,datediff(day,SpecimenDate,(lag(SpecimenDate) over (order by PID_NHSNumber))) MaximumDaysDifferenceBetweenSpecimenDates
	,DeduplicationCode
	,Organism
into #EpisodeLengthCheck_1
from #Preprocessing_3


select PID_NHSNumber
	,PID_NHSNumberAbove
	,SpecimenDate
	,MaximumDaysDifferenceBetweenSpecimenDates
	,case
		when #EpisodeLengthCheck_1.MaximumDaysDifferenceBetweenSpecimenDates>=14
			and PID_NHSNumber=PID_NHSNumberAbove
			and Organism not in ('jakob-creutzfeld agent','hepatitis b','hepatitis b acute','hepatitis b chronic','hepatitis c','hiv 1','hiv 2','hiv other types','htlv I','htlv I/II','htlv II','htlv/hiv not further specified')
			and Organism not like 'salmonella%'
			and Organism not like 'mycobacterium%'
			then 'Episode length has been exceeded (14 days) for multiple NHS Numbers'
		when #EpisodeLengthCheck_1.MaximumDaysDifferenceBetweenSpecimenDates>=14
			and PID_NHSNumber=PID_NHSNumberAbove
			and Organism like 'influenza a%'
			then 'Episode length has been exceeded (42 days) for multiple NHS Numbers'
		when #EpisodeLengthCheck_1.MaximumDaysDifferenceBetweenSpecimenDates>=91
			and PID_NHSNumber=PID_NHSNumberAbove
			and Organism like 'salmonella%'
			then 'Episode length has been exceeded (91 days) for multiple NHS Numbers'
		when #EpisodeLengthCheck_1.MaximumDaysDifferenceBetweenSpecimenDates>=182
			and PID_NHSNumber=PID_NHSNumberAbove
			and Organism like 'mycobacterium%'
			then 'Episode length has been exceeded (182 days) for multiple NHS Numbers'
		when #EpisodeLengthCheck_1.PID_NHSNumber is null and #EpisodeLengthCheck_1.PID_NHSNumber in (
			select PID_NHSNumber
			from #Preprocessing_2
			where PID_NHSNumber is null)
			then 'NHS Number is null value'
			else ''
		end Flag
	,DeduplicationCode
into #EpisodeLengthCheck_2
from #EpisodeLengthCheck_1


select PID_NHSNumber
	,SpecimenDate
	,MaximumDaysDifferenceBetweenSpecimenDates
	,Flag
	,DeduplicationCode
into #EpisodeLengthCheck_3
from #EpisodeLengthCheck_2
where Flag='Episode length has been exceeded (14 days) for multiple NHS Numbers'
	or Flag='Episode length has been exceeded (42 days) for multiple NHS Numbers'
	or Flag='Episode length has been exceeded (91 days) for multiple NHS Numbers'
	or Flag='Episode length has been exceeded (182 days) for multiple NHS Numbers'


select DeduplicationCode
	,SpecimenCode
	,RecordCode
	,Organism
	,Serotype
	,Phagetype
	,ToxinType
	,SpecimenTypeClass
	,SpecimenType
	,TestMethod
	,Antimicrobial
	,SensitivityTestResult
	,ReportingLaboratory
	,SpecimenDate
	,[Quarter]
	,RequestorOrganisation
	,RequestorOrganisationPostcode
	,RequestorOrganisationType
	,RequestorOrganisationWard
	,SourceLaboratory
	,ReferenceLaboratory
	,AgeInYears
	,AgeInMonths
	,AgeGroup
	,Sex
	,GeographicalRegion
	,LocalAuthority
	,PHECentre
	,TravelledAbroadFlag
	,TravelledAbroadDetails
	,PID_NHSNumber
	,PID_LocalPatientCode
	,PID_Forename
	,PID_Surname
	,PID_BirthDate
	,PID_Postcode
	,PID_LaboratoryComments
into #NewEpisodes
from #Preprocessing_3
where DeduplicationCode in (
	select DeduplicationCode
	from #EpisodeLengthCheck_3)
		and PID_NHSNumber in (
	select PID_NHSNumber
	from #EpisodeLengthCheck_3)
		and SpecimenDate in (
	select SpecimenDate
	from #EpisodeLengthCheck_3)


update #NewEpisodes
set SpecimenCode=SpecimenCode+' '+'(new epi.)'


select DeduplicationCode
	,SpecimenCode
	,RecordCode
	,Organism
	,Serotype
	,Phagetype
	,ToxinType
	,SpecimenTypeClass
	,SpecimenType
	,TestMethod
	,Antimicrobial
	,SensitivityTestResult
	,ReportingLaboratory
	,SpecimenDate
	,[Quarter]
	,RequestorOrganisation
	,RequestorOrganisationPostcode
	,RequestorOrganisationType
	,RequestorOrganisationWard
	,SourceLaboratory
	,ReferenceLaboratory
	,AgeInYears
	,AgeInMonths
	,AgeGroup
	,Sex
	,GeographicalRegion
	,LocalAuthority
	,PHECentre
	,TravelledAbroadFlag
	,TravelledAbroadDetails
	,PID_NHSNumber
	,PID_LocalPatientCode
	,PID_Forename
	,PID_Surname
	,PID_BirthDate
	,PID_Postcode
	,PID_LaboratoryComments
into #Preprocessing_4
from #Preprocessing_2
union
select DeduplicationCode
	,SpecimenCode
	,RecordCode
	,Organism
	,Serotype
	,Phagetype
	,ToxinType
	,SpecimenTypeClass
	,SpecimenType
	,TestMethod
	,Antimicrobial
	,SensitivityTestResult
	,ReportingLaboratory
	,SpecimenDate
	,[Quarter]
	,RequestorOrganisation
	,RequestorOrganisationPostcode
	,RequestorOrganisationType
	,RequestorOrganisationWard
	,SourceLaboratory
	,ReferenceLaboratory
	,AgeInYears
	,AgeInMonths
	,AgeGroup
	,Sex
	,GeographicalRegion
	,LocalAuthority
	,PHECentre
	,TravelledAbroadFlag
	,TravelledAbroadDetails
	,PID_NHSNumber
	,PID_LocalPatientCode
	,PID_Forename
	,PID_Surname
	,PID_BirthDate
	,PID_Postcode
	,PID_LaboratoryComments
from #NewEpisodes


print 'Table #1: Raw data.
Table #2: Pre-processed data.'


select SpecimenCode
	,RecordCode
	,Organism
	,Serotype
	,Phagetype
	,ToxinType
	,SpecimenTypeClass
	,SpecimenType
	,TestMethod
	,Antimicrobial
	,SensitivityTestResult
	,ReportingLaboratory
	,SpecimenDate
	,[Quarter]
	,RequestorOrganisation
	,RequestorOrganisationPostcode
	,RequestorOrganisationType
	,RequestorOrganisationWard
	,SourceLaboratory
	,ReferenceLaboratory
	,AgeInYears
	,AgeInMonths
	,AgeGroup
	,Sex
	,GeographicalRegion
	,LocalAuthority
	,PHECentre
	,TravelledAbroadFlag
	,TravelledAbroadDetails
	,PID_NHSNumber
	,PID_LocalPatientCode
	,PID_Forename
	,PID_Surname
	,PID_BirthDate
	,PID_Postcode
	,PID_LaboratoryComments
from #Preprocessing_2


/*
select SpecimenCode
	,RecordCode
	,Organism
	,Serotype
	,Phagetype
	,ToxinType
	,SpecimenTypeClass
	,SpecimenType
	,TestMethod
	,Antimicrobial
	,SensitivityTestResult
	,ReportingLaboratory
	,SpecimenDate
	,[Quarter]
	,RequestorOrganisation
	,RequestorOrganisationPostcode
	,RequestorOrganisationType
	,RequestorOrganisationWard
	,SourceLaboratory
	,ReferenceLaboratory
	,AgeInYears
	,AgeInMonths
	,AgeGroup
	,Sex
	,GeographicalRegion
	,LocalAuthority
	,PHECentre
	,TravelledAbroadFlag
	,TravelledAbroadDetails
	,PID_NHSNumber
	,PID_LocalPatientCode
	,PID_Forename
	,PID_Surname
	,PID_BirthDate
	,PID_Postcode
	,PID_LaboratoryComments
from #Preprocessing_4
*/


select RecordCode cdr_opie_id
	,Organism organism_species_name
	,Phagetype phagetype
	,Serotype serotype
	,ToxinType
	,PID_Forename patient_forename
	,PID_Surname patient_surname
	,PID_NHSNumber patient_nhs_number
	,PID_LocalPatientCode hospital_patient_number
	,SpecimenCode specimen_number
	,PID_BirthDate patient_date_of_birth
	,Sex patient_sex
	,PID_Postcode patient_postcode
	,LocalAuthority local_authority_name
	,SpecimenDate specimen_date
	,SpecimenDate specimen_dateamend
	,ReportingLaboratory lab_geography_name_current
	,RequestorOrganisationType requesting_organisation_type_des
	,RequestorOrganisationType medical_facility_type_name
	,RequestorOrganisation site_name
	,RequestorOrganisationPostcode site_postcode
	,RequestorOrganisationWard ward
	,SpecimenType specimen_type_description
	,TravelledAbroadFlag travel_abroad_indicator
	,TravelledAbroadDetails travel_information
from #Preprocessing_4


drop table #Preprocessing_1
drop table #Preprocessing_2
drop table #Preprocessing_3
drop table #Preprocessing_4
drop table #EpisodeLengthCheck_1
drop table #EpisodeLengthCheck_2
drop table #EpisodeLengthCheck_3
drop table #NewEpisodes