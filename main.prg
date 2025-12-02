' IMPORTANT WARNING!! You may need to RUN E-views as administrator.
'run(1,c,q) main ' Run a program. c : run program file without display the program file window. v / q : verbose / quiet; ver4 / ver5 : Execute program in previous version script.

logmode l

%path = "C:\GitHub\ThreeME"
cd %path

include .\configuration

' Utility procedures
include .\src\utils\subroutines
include .\src\utils\tes
include .\src\utils\results_outputs
include .\src\utils\matter 

'Addin
include .\src\addin\export.prg

' Load data
include .\src\data\load_calibration
include .\src\data\load_data_shocks
include .\src\data\load_data_hybrid
include .\src\data\load_data_realist
include .\src\data\standard_shocks

include .\src\data\load_data_baseline_realist
include .\src\data\load_data_realist_dgt
include .\src\data\load_data_calibrage_ame_dgt

'Run model
include .\src\model\tracker.prg
include .\src\model\run
include .\src\model\solve


'Modify depending on which SCEN_AMS2 file you want to run and how many
%scen_list ="ADEME"
'ADEME AUTO ENER ETS2 PL MPR CEE TER SOBRERESID SOBRETER SOBREAUTO RM AUTRE TIC IND
%exceptions_DGT = "yes"  ' "no"
%exceptions_PAC = "no"  ' yes/no. no si exceptions_DGT =no. no si exceptions_PAC=yes.
%exceptions_VAC = "no" '' yes/no. no si exceptions_DGT =no. no si exceptions_VAC=yes.

' CORRECTION SI ERREUR: on ne peut pas faire tourner VAC + PAC
If %exceptions_DGT = "no" then
%exceptions_PAC = "no"  
%exceptions_VAC = "no"
else
	if %exceptions_PAC = "yes"  then
		%exceptions_VAC = "no"
	endif
	if  %exceptions_VAC = "yes" then
		%exceptions_PAC = "no"  
	endif
endif


' "E1 F1 I1 Res1 R2 R3 TE1 TRM1 TRM2 TRM3 TRV1 TRV2 TRV3 TRV4 TRV5"

'7_MesuresReelles 7_MesuresReelles_TaxCarbGel " 
'1_AME 1_AME_TaxCarbGel 2_AMS_ALL 2_AMS_ALL_TaxCarbGel 3_AMS_SignauxPrix 3_AMS_SignauxPrix_TaxCarbGel 3_AMS_CoopInter 4_AMS_BonusEcolo 4_AMS_CITE 4_AMS_ConsoGazElec 4_AMS_ExportElec 4_AMS_MixEner 4_AMS_PrixAutoElec 4_AMS_ReglemAuto 4_AMS_ReglemTranspt 4_AMS_RenovTertiaire 4_AMS_TaxeCarb 4_AMS_TICPED 4_AMS_TICPEM 4_AMS_TICPE 4_AMS_TravEnerTiersFin 5_AMS_EffetComport 6_AMS_EC_Covoit 6_AMS_EC_OccupAuto 6_AMS_EC_Pieton_MobilitesDouces 6_AMS_EC_Teletravail 6_AMS_EC_Train 7_AMS_MesReelles 7_MesuresReelles 7_MesuresReelles_TaxCarbGel

'8_AME_TaxCarb 8_AME_TaxCarbGel 8_AME_BonusEcolo 8_AME_CITE 8_AME_ConsoGazElec 8_AME_CoopInter 8_AME_EC_Covoit 8_AME_EC_OccupAuto 8_AME_EC_Pieton_MobilitesDouces 8_AME_EC_Teletravail 8_AME_EC_Train 8_AME_EffetComport 8_AME_ExportElec 8_AME_MixEner 8_AME_PrixAutoElec 8_AME_ReglemAuto 8_AME_ReglemTranspt 8_AME_RenovTertiaire 8_AME_SignauxPrix 8_AME_TICPE 8_AME_TravEnerTiersFin

exec .\src\addin\model_addin.prg 'CD

For %scen_number {%scen_list}

	' ***************
	' Configuration
	call configuration	

	' Addin: External compiler
'	exec .\src\addin\model_addin.prg 
	
	' ***********
	' Model run
	
	For %DC {%calibrations}
	
    		' Relative paths
		if %scen_list ="VAC" then
		%data_calibration = ".\..\..\data\calibrations\SAM_"+%DC+"_VAC.xls"
		else
		%data_calibration = ".\..\..\data\calibrations\SAM_"+%DC+".xls"
		endif
		
    		call run(%data_calibration,%data_shocks)
		
    		'call export_TES
    		
		'***********
		' Saving results
		
		'Creating name of excel file for results
		%rpt1 = "reporting_1_"+%scen_number+".xlsx"
		%rpt2 = "reporting_2_"+%scen_number+".xlsx"
		%rpt3 = "reporting_3_"+%scen_number+".xlsx"
		'%rpt5 = "reporting_5_"+%scen_number+".xlsx"
		'%rpt6 = "reporting_6_"+%scen_number+".xlsx"
		'%rpt7 = "reporting_finPO_"+%scen_number+".xlsx"
		'%rptMPR = "reporting_MPR_"+%scen_number+".xlsx"
		'%rptLogan = "reporting_Logan_"+%scen_number+".xlsx"
		'%rptmatmat=""+"reporting_Matmat_"+%scen_number+".csv"
		%rptmatmat=@strnow("yyyy-mm-dd-HH-mi-ss")+"-Matmat.csv"

	    if %exceptions_PAC = "yes" then
		%rptPAC = "reporting_PAC_"+%scen_number+".xlsx"
		endif

		'Folder path and file path
		%scenfolder_path = %path + "\results\"+%scen_number+"_results"  'CD
		%xlname1 = %scenfolder_path+"\"+%rpt1
		%xlname2 = %scenfolder_path+"\"+%rpt2
		%xlname3 = %scenfolder_path+"\"+%rpt3
		'%xlname5 = %scenfolder_path+"\"+%rpt5
		'%xlname6 = %scenfolder_path+"\"+%rpt6
		'%xlname7 = %scenfolder_path+"\"+%rpt7
		'%xlnameMPR = %scenfolder_path+"\"+%rptMPR
		'%xlnameLogan = %scenfolder_path+"\"+%rptLogan
		%xlnamematmat = %scenfolder_path+"\"+%rptmatmat

	    if %exceptions_PAC = "yes" then
		%xlnamePAC = %scenfolder_path+"\"+%rptPAC
		endif

		If not @folderexist(%scenfolder_path) then		
			logmode logmsg
			logmsg %scenfolder_path
			shell mkdir {%scenfolder_path}			
	
			wfsave(type=excelxml, mode=overwrite) {%xlname1} range="data!A1" byrow @keep reporting @smpl "2004 2050"
			wfsave(type=excelxml, mode=overwrite) {%xlname2} range="data!A1" byrow @keep reporting_2 @smpl "2004 2050"
			wfsave(type=excelxml, mode=overwrite) {%xlname3} range="data!A1"  byrow @keep reporting_3 @smpl "2004 2050"
		'	wfsave(type=excelxml, mode=overwrite) {%xlname5} range="data!A1"  byrow @keep reporting_5 @smpl "2004 2050"
		'	wfsave(type=excelxml, mode=overwrite) {%xlname6} range="data!A1"  byrow @keep reporting_6 @smpl "2004 2050"
		'	wfsave(type=excelxml, mode=overwrite) {%xlname7} range="data!A1"  byrow @keep reporting_finPO @smpl "2004 2050"
		'	wfsave(type=excelxml, mode=overwrite) {%xlnameMPR} range="data!A1"  byrow @keep reporting_MPR @smpl "2004 2050"
		'	wfsave(type=excelxml, mode=overwrite) {%xlnameLogan} range="data!A1"  byrow @keep reporting_Logan @smpl "2004 2050"
			wfsave(type=text, mode=overwrite) {%xlnamematmat}  @keep export_group_matmat @smpl "2004 2050"


	    if %exceptions_PAC = "yes" then
			wfsave(type=excelxml, mode=overwrite) {%xlnamePAC} range="data!A1"  byrow @keep reporting_PAC @smpl "2004 2050"
			endif

		Else

			wfsave(type=excelxml, mode=update) {%xlname1} range="data!A1" byrow @keep reporting @smpl "2004 2050"
			wfsave(type=excelxml, mode=update) {%xlname2} range="data!A1" byrow @keep reporting_2 @smpl "2004 2050"
			wfsave(type=excelxml, mode=update) {%xlname3} range="data!A1"  byrow @keep reporting_3 @smpl "2004 2050"
		'	wfsave(type=excelxml, mode=update) {%xlname5} range="data!A1"  byrow @keep reporting_5 @smpl "2004 2050"
		'	wfsave(type=excelxml, mode=update) {%xlname6} range="data!A1"  byrow @keep reporting_6 @smpl "2004 2050"
	    if %exceptions_DGT= "yes" then
		'	wfsave(type=excelxml, mode=update) {%xlname7} range="data!A1"  byrow @keep reporting_finPO @smpl "2004 2050"
		'	wfsave(type=excelxml, mode=update) {%xlnameMPR} range="data!A1"  byrow @keep reporting_MPR @smpl "2004 2050"
		'	wfsave(type=excelxml, mode=update) {%xlnameLogan} range="data!A1"  byrow @keep reporting_Logan @smpl "2004 2050"
			wfsave(type=text, mode=update) {%xlnamematmat} @keep export_group_matmat @smpl "2004 2050"

		Endif
	    if %exceptions_PAC = "yes" then
			wfsave(type=excelxml, mode=update) {%xlnamePAC} range="data!A1"  byrow @keep reporting_PAC @smpl "2004 2050"
			endif
'wfsave(type=excelxml, mode=update) {%xlname3} range="data!A695" byrow @keep debt_auto_val_h01_ca_0 debt_auto_val_h01_ca_2 debt_auto_val_h01_cb_0 debt_auto_val_h01_cb_2 debt_auto_val_h01_cc_0 debt_auto_val_h01_cc_2 debt_auto_val_h01_cd_0 debt_auto_val_h01_cd_2 debt_auto_val_h01_ce_0 debt_auto_val_h01_ce_2 debt_auto_val_h01_cf_0 debt_auto_val_h01_cf_2 debt_auto_val_h01_cg_0 debt_auto_val_h01_cg_2 debt_newb_val_h01_ca_0 debt_newb_val_h01_ca_2 debt_newb_val_h01_cb_0 debt_newb_val_h01_cb_2 debt_newb_val_h01_cc_0 debt_newb_val_h01_cc_2 debt_newb_val_h01_cd_0 debt_newb_val_h01_cd_2 debt_newb_val_h01_ce_0 debt_newb_val_h01_ce_2 debt_newb_val_h01_cf_0 debt_newb_val_h01_cf_2 debt_newb_val_h01_cg_0 debt_newb_val_h01_cg_2 debt_rehab_val_h01_ca_0 debt_rehab_val_h01_ca_2 debt_rehab_val_h01_cb_0 debt_rehab_val_h01_cb_2 debt_rehab_val_h01_cc_0 debt_rehab_val_h01_cc_2 debt_rehab_val_h01_cd_0 debt_rehab_val_h01_cd_2 debt_rehab_val_h01_ce_0 debt_rehab_val_h01_ce_2 debt_rehab_val_h01_cf_0 debt_rehab_val_h01_cf_2 debt_rehab_val_h01_cg_0 debt_rehab_val_h01_cg_2 @smpl "2006 2050"
'wfsave(type=excelxml, mode=update) {%xlname3} range="data!A740" byrow @keep q_mtep_h_buil_21_0 q_mtep_h_auto_22_0+q_mtep_h_buil_22_0 q_mtep_h_auto_23_0+q_mtep_h_buil_23_0 q_mtep_h_auto_24_0+q_mtep_h_buil_24_0 q_mtep_h_buil_21_2 q_mtep_h_auto_22_2+q_mtep_h_buil_22_2 q_mtep_h_auto_23_2+q_mtep_h_buil_23_2 q_mtep_h_auto_24_2+q_mtep_h_buil_24_2 q_mtep_sec_21_05_0+q_mtep_sec_21_06_0+q_mtep_sec_21_07_0+q_mtep_sec_21_08_0+q_mtep_sec_21_10_0+q_mtep_sec_21_12_0 q_mtep_sec_22_01_0+q_mtep_sec_22_02_0+q_mtep_sec_22_03_0+q_mtep_sec_22_04_0+q_mtep_sec_22_05_0+q_mtep_sec_22_06_0+q_mtep_sec_22_07_0+q_mtep_sec_22_08_0+q_mtep_sec_22_09_0+q_mtep_sec_22_12_0+q_mtep_sec_22_13_0+q_mtep_sec_22_14_0+q_mtep_sec_22_15_0+q_mtep_sec_22_16_0+q_mtep_sec_22_17_0+q_mtep_sec_22_18_0+q_mtep_sec_22_19_0+q_mtep_sec_22_20_0 q_mtep_sec_23_01_0+q_mtep_sec_23_02_0+q_mtep_sec_23_03_0+q_mtep_sec_23_04_0+q_mtep_sec_23_05_0+q_mtep_sec_23_06_0+q_mtep_sec_23_07_0+q_mtep_sec_23_08_0+q_mtep_sec_23_09_0+q_mtep_sec_23_10_0+q_mtep_sec_23_11_0+q_mtep_sec_23_12_0+q_mtep_sec_23_14_0+q_mtep_sec_23_15_0+q_mtep_sec_23_16_0+q_mtep_sec_23_19_0 q_mtep_sec_24_01_0+q_mtep_sec_24_02_0+q_mtep_sec_24_03_0+q_mtep_sec_24_04_0+q_mtep_sec_24_05_0+q_mtep_sec_24_06_0+q_mtep_sec_24_07_0+q_mtep_sec_24_08_0+q_mtep_sec_24_09_0+q_mtep_sec_24_10_0+q_mtep_sec_24_11_0+q_mtep_sec_24_12_0+q_mtep_sec_24_13_0+q_mtep_sec_24_14_0+q_mtep_sec_24_15_0+q_mtep_sec_24_16_0+q_mtep_sec_24_17_0+q_mtep_sec_24_18_0+q_mtep_sec_24_19_0+q_mtep_sec_24_20_0 q_mtep_sec_21_05_2+q_mtep_sec_21_06_2+q_mtep_sec_21_07_2+q_mtep_sec_21_08_2+q_mtep_sec_21_10_2+q_mtep_sec_21_12_2 q_mtep_sec_22_01_2+q_mtep_sec_22_02_2+q_mtep_sec_22_03_2+q_mtep_sec_22_04_2+q_mtep_sec_22_05_2+q_mtep_sec_22_06_2+q_mtep_sec_22_07_2+q_mtep_sec_22_08_2+q_mtep_sec_22_09_2+q_mtep_sec_22_12_2+q_mtep_sec_22_13_2+q_mtep_sec_22_14_2+q_mtep_sec_22_15_2+q_mtep_sec_22_16_2+q_mtep_sec_22_17_2+q_mtep_sec_22_18_2+q_mtep_sec_22_19_2+q_mtep_sec_22_20_2 q_mtep_sec_23_01_2+q_mtep_sec_23_02_2+q_mtep_sec_23_03_2+q_mtep_sec_23_04_2+q_mtep_sec_23_05_2+q_mtep_sec_23_06_2+q_mtep_sec_23_07_2+q_mtep_sec_23_08_2+q_mtep_sec_23_09_2+q_mtep_sec_23_10_2+q_mtep_sec_23_11_2+q_mtep_sec_23_12_2+q_mtep_sec_23_14_2+q_mtep_sec_23_15_2+q_mtep_sec_23_16_2+q_mtep_sec_23_19_2 q_mtep_sec_24_01_2+q_mtep_sec_24_02_2+q_mtep_sec_24_03_2+q_mtep_sec_24_04_2+q_mtep_sec_24_05_2+q_mtep_sec_24_06_2+q_mtep_sec_24_07_2+q_mtep_sec_24_08_2+q_mtep_sec_24_09_2+q_mtep_sec_24_10_2+q_mtep_sec_24_11_2+q_mtep_sec_24_12_2+q_mtep_sec_24_13_2+q_mtep_sec_24_14_2+q_mtep_sec_24_15_2+q_mtep_sec_24_16_2+q_mtep_sec_24_17_2+q_mtep_sec_24_18_2+q_mtep_sec_24_19_2+q_mtep_sec_24_20_2  @smpl "2006 2050"
		
		Endif
	
	Next

	'wfclose

Next


