' *******************************
' Main entry point to the model
' Performs a complete model run:
' - loads the data calibration specified
' - loads the specification of the model
' - makes a first run to calculate the baseline (optionnally with a realistic baseline), with outputs if requested
' - makes a second run for the %data_shock specified, with outputs if requested
'NB: all configuration is to be made in the general configuration.prg file, not here
subroutine run(string %data_calibration, string %data_shock)

  ' ***********************
  ' Create the Workfile
  %wfname = "./../../"+%modelname+"_"+%DC
  wfcreate(wf=%wfname, page=%modelname) {%freq} {%firstdate} {%lastdate}

  call create_lists


  ' ******************
  ' Load the model

  if %load="new"  then
    'Give a name to the Model
    model {%modelname}
    ' Load calibration data from the Excel file
    call load_calibration
	
	logmsg ******* DATA LOADED *********

    ' Export all variables to a csv file (used by the external compiler)
    call export_all_to_csv
	logmsg ******* EXPORT DONE *********

    if %exceptions_DGT = "yes" then
	  call load_baseline_realist
	  {%modelname}.series round0_dgt round1_dgt round2_dgt demography government_dgt household_dgt ghg carbon_tax prices_dgt exceptions_tracker
	else
      {%modelname}.series round0 round1 round2 demography government household ghg carbon_tax prices exceptions_tracker
	endif
    call export_all_to_csv

' Stop here to create/save a workfile where data are already initialized (with option %load = "new"). Then choose the option %load = ""
'    stop
    if %ModelVersion = "master"  then

       ' Load the model specification from the model/ folder
       {%modelname}.load blocks

    endif


    if %ModelVersion = "hybrid"  then
       call load_calibration_hybrid
      if %exceptions_DGT = "yes" then
	  {%modelname}.series    hybrid_round1_dgt 
	 else 
	  {%modelname}.series    hybrid_round1
      endif
       call export_all_to_csv

	if %exceptions_DGT = "yes" then
	    if %exceptions_PAC = "no" then
       {%modelname}.series exceptions_hybrid_data exceptions_data_AME-AMS  Exceptions_DGT_data Fonds_chaleur_data
	    else
       {%modelname}.series exceptions_hybrid_data exceptions_data_AME-AMS  Exceptions_DGT_data  Exceptions_PAC_data
		endif
	else
       {%modelname}.series exceptions_hybrid_data exceptions_data_AME-AMS Exceptions_Variantes_data
     endif     
 
       call export_all_to_csv

       ' Load the model specification from the model/ folder

	if %exceptions_DGT = "yes" then
	  	if %exceptions_PAC = "no" then 
      		if %exceptions_VAC = "yes" then
				{%modelname}.load blocks hybrid_new exceptions_AME-AMS Exceptions_dgt  exceptions_dgt_VAC
	  		else
				{%modelname}.load blocks hybrid_new exceptions_AME-AMS Exceptions_dgt  Fonds_chaleur 
			endif
		else
{%modelname}.load blocks hybrid_new exceptions_AME-AMS Exceptions_dgt  Exceptions_PAC
	  endif
	else
      {%modelname}.load blocks hybrid_new exceptions_AME-AMS Exceptions_Variantes
	 endif   

    endif

    if %ModelVersion = "IO"  then

       {%modelname}.load blocks exceptions_IO

    endif
	
	'string varendo = "CH_* CHD_* CHM_* CI_* CK_* CL_* CU_* CUR* DS_* DSD_* DSM_* E_* ED_* EM_* ENER_0* ENER_1* ENER_2* EXP_* EXPG_* G_* GD_* GM_* GDP_* I_* IA_* IAD_* IAM_* K_* L_* M_* MARKUP_* MAT_* MATD_* MATM_* MC_* MCD_* MCM_* MS_* MT_* MTD_* MTM_* NCU_* OTHT_* OTHTD_* OTHTM_* PARTR_* PCH_* PCHD_* PCHM_* PCID_* PCIM_* PE_* PED_* PEM_* PENER_* PEXP_* PEXPG_* PGD_* PGDP_* PI_* PIA_* PIAD_* PIAM_* PID_* PIM_* PIS_* PIY_* PK_* PM_* PMAT_* PMATD_* PMATM_* PMCD_* PMCM_* PMS_* PMT_* PMTD_* PMTM_* PNEXP_* POTHT_* POTHTD_* POTHTM_* PQ_* PQD_* PQM_* PRF_* PSUB_* PSY_* PTAX_* PVA_* PVAT_* PVATD_* PVATM_* PX_* PXD_* PXM_* PY_* PYQ_* PYQS_* Q_* QD_* QM_* R_* RF_* RK_* SD_* SM_* SUBST_* SY_* TMD_* VA_* VAT_* VATD_* VATM_* W_* X_* XD_* XM_* Y_* YCAP_* YOPT_* YQ_* YQBIS_* YQS_* "
	'string varendo = "CK_* MAT_* PMAT_* SUBST_* R* Ibis_bis_* K_* IA_* K_NE_n_19_* K_E_n_19_* IA_n_20_* Ibis_20_* K_NE_n_20_* PM_*  YQS_SM* PYQ_SM* MATM_* MATD_* CHM_* CHD_* GM_* GD_* IAM_* IAD_* EM_* ED_* TMD_n* Y_e_* E_* MAT_* L_* SD_e_* SM_e_* PARTR_e_* R_e PY_* PY_e_* EXP_e_* SUBST_L_* SUBST_E_* SUBST_K_* SUBST_MAT_* SUBST_CHD_* SUBST_CHM_* SUBST_GD_* SUBST_GM_* SUBST_X_* SUBST_XD_* SUBST_XM_* P_e PY_n_* Ibis_* IA_n_* K_n_* W_* P_* Y_* VA_* "
	'string varendo = "CK_* MAT_* PMAT_* SUBST_*_n* R_DIR Ibis_bis_* K_n_* IA_n_* K_NE_n_19_* K_E_n_19_* IA_n_20_* Ibis_20_* K_NE_n_20_* PM_*  YQS_SM* PYQ_SM* MATM_* MATD_* CHM_* CHD_* GM_* GD_* IAM_* IAD_* EM_* ED_* TMD_n* Y_e_* E_* MAT_* L_* SD_e_* SM_e_* PARTR_e_* R_e PY_* PY_e_* EXP_e_* SUBST_L_* SUBST_E_* SUBST_K_* SUBST_MAT_* SUBST_CHD_* SUBST_CHM_* SUBST_GD_* SUBST_GM_* SUBST_X_* SUBST_XD_* SUBST_XM_* P_e PY_n_* Ibis_* IA_n_* K_n_* W_*"
	'string varendo = "CK_* MATM_* CHM_* GM_* IAM_* EM_* PM_n* SUBST_*_n* R_DIR Ibis_bis_* K_n_* IA_n_* K_NE_n_19_* K_E_n_19_* IA_n_20_* Ibis_20_* K_NE_n_20_* YQS_SM* PYQ_SM* TMD_n* Y_e_* PY_* PY_e_* EXP_e_* SUBST_L_* SUBST_E_* SUBST_K_* SUBST_MAT_* SUBST_CHD_* SUBST_CHM_* SUBST_GD_* SUBST_GM_* SUBST_X_* SUBST_XD_* SUBST_XM_* P_e PY_n_* Ibis_* IA_n_* K_n_* W_*"

'"PM_* YQS_SM PYQS_SM MATM_* GRMATM_* MATD_* CHM_* CHD_* GRCHM_* GM_* GD_* GRGM_* GD_* IAM_* GRIAM_* IAD_* IAM_* EM_* GREM_* ED_* PXD_n_* PXD_* R_DIR* MAT_* PMAT_* W_S_0* W_S_1* W_S_2* TMD_n*  W_S_n_0* W_S_n_1* W_S_n_2* GRPM_* IA_n_* IAM_n*"
''j'ai mis W_S_0*, W_S_1* et W_S_2* et non W_S_* pour �viter d'inclure les variables W_S_n_* qui ne sont pas concern�es par nos modifications	
''X_* XD_*
''BetaExp_20_h01 PEXP_CES_H01
'
	'if %exceptions_DGT = "yes" then
	'string list = a_3me.@stochastic
	'list = @wkeep(list,varendo)
	'{%modelname}.addassign(i) {list}
	'{%modelname}.addinit(v=n) {list}
    'endif
	
    wfsave {%wfname}

  else
    'wfopen {%wfname}    ' Load workfile if already created


    if %ModelVersion = "IO"  then

    endif

  endif

  smpl @all

  ' Clean up results folder
  '%cmd = @left(@linepath, 2) + " & cd " + @addquotes(@linepath) + " & del /Q ..\..\results\*.txt"
 'shell(h) {%cmd}

  !scenario = 0
 '************************************************
  'Using de solver to calibrate a scenario'
  
    ' ***************************************
    ' Run the shock scenario if requested
    if %run_shock="yes" then
      For %DS {%shocks}

        wfopen {%wfname}

        ' Run the baseline scenario to initialize add factors
        if %exceptions_DGT = "yes" then
		  call run_scenario("baseline_dgt")
		  call run_scenario("AME_SCEN_AMS2")
		else 
		  ' Run the baseline scenario (AME)
		  call run_scenario("baseline")
		endif
		

        ' Run scenario
        call run_scenario(%DS)
  
         next
      ' *******************************************
      ' If no shocks, just run the baseline
    else
      wfopen {%wfname}
        ' Run the baseline scenario
      call run_scenario("baseline")
    endif
  
  

''   call additional_outputs

  ' *******************
  ' Error reporting

  string a_errors="Number of errors: "+@str(@errorcount)

  !error_count = @errorcount
  if !error_count > 0 then
    logmode all
    logsave errors
  endif


  ' **********************
  ' Saving and cleanup

  if %savewf = "yes" then
    Wfsave(c) output_{%DC}\{%DS}.wf1
  endif

  if %close = "yes" then
    close @all
  endif

endsub


' ************************************************
' Runs an individual scenario, baseline or shock
' Pass in "baseline" as the %scenario_name for the baseline scenario
subroutine run_scenario(string %scenario_name)

'	string varendo = "CK_* MATM_DES_N* CHM_DES_N* GM_DES_N* IAM_DES_N* IAM_DES_N_19_19 EM_DES_N* PM_N* IA_N_DES* I_MDE_19 I_MDE_20 TMD_N* W_S_DES_N* W_S_DGT*  PY_DES* L_DES* EXP_DES* PRESOC_DOM_U_TETE_DES PRESOC_DOM_OTH_VAL_DES X_DES*  MAT_n_12_13 PPACI " 'IA_NOTIONNEL_NOSHOCK*

	string varendo = "MATM_DES_N* CHM_DES_N* GM_DES_N* IAM_DES_N* IAM_DES_N_19_19 EM_DES_N* PM_N* IA_N_DES* I_MDE_19 I_MDE_20 TMD_N* W_S_DES_N* W_S_DGT*  PY_DES* L_DES* EXP_DES* PRESOC_DOM_U_TETE_DES PRESOC_DOM_OTH_VAL_DES X_DES* MAT_n_12_13 PPACI CU_N_BIS* EXP_13_des*" 'IA_NOTIONNEL_NOSHOCK*
	'string varendo = "CH_* CHD_* CHM_* CI_* CK_* CL_* CU_* CUR* DS_* DSD_* DSM_* E_* ED_* EM_* ENER_0* ENER_1* ENER_2* EXP_* EXPG_* G_* GD_* GM_* GDP_* I_* IA_* IAD_* IAM_* K_* L_* M_* MARKUP_* MAT_* MATD_* MATM_* MC_* MCD_* MCM_* MS_* MT_* MTD_* MTM_* NCU_* OTHT_* OTHTD_* OTHTM_* PARTR_* PCH_* PCHD_* PCHM_* PCID_* PCIM_* PE_* PED_* PEM_* PENER_* PEXP_* PEXPG_* PGD_* PGDP_* PI_* PIA_* PIAD_* PIAM_* PID_* PIM_* PIS_* PIY_* PK_* PM_* PMAT_* PMATD_* PMATM_* PMCD_* PMCM_* PMS_* PMT_* PMTD_* PMTM_* PNEXP_* POTHT_* POTHTD_* POTHTM_* PQ_* PQD_* PQM_* PRF_* PSUB_* PSY_* PTAX_* PVA_* PVAT_* PVATD_* PVATM_* PX_* PXD_* PXM_* PY_* PYQ_* PYQS_* Q_* QD_* QM_* RF_* RK_* SD_* SM_* SUBST_* SY_* TCO_* TMD_* VA_* VAT_* VATD_* VATM_* W_* X_* XD_* XM_* Y_* YCAP_* YOPT_* YQ_* YQBIS_* YQS_* " 
	if %exceptions_DGT = "yes" then
		string list = a_3me.@stochastic
		list = @wkeep(list,varendo)
	endif	

  if %scenario_name = "baseline_dgt" then
    '{%modelname}.scenario(n, a=0) {%scenario_name}
	
	' Load a realistic reference scenario if requested
    if %ref="realist" then
      call load_realist_dgt
    endif
	
	if %exceptions_DGT = "yes" then
		{%modelname}.addassign(v=i) {list}
		{%modelname}.exclude {list}
    endif
	
	call solvemodel(%solveopt)
	logmsg ********* CALAGE DU BASELINE ********
	
	if %exceptions_DGT = "yes" then
		{%modelname}.addassign(v=i) {list}
		{%modelname}.addinit(v=a,s=b) {list}

		{%modelname}.reinclude {list}
	endif

    call solvemodel(%solveopt)
	logmsg ********* BASELINE CALE ********

  else 
  
  if %scenario_name = "AME_SCEN_AMS2" then
    
	{%modelname}.scenario(n, a=1) {%scenario_name}
	
	' Load a realistic reference scenario if requested
    if %ref="realist" then
      call load_realist	  
    endif

    call solvemodel(%solveopt)

 group reporting_verif VERIF_GDP_GDPBIS_1 VERIF_GDP_GDPTER_1  VERIF_PGDP_PGDPBIS_1 VERIF_GDPBIS_GDPTER_1 VERIF_GDPTER_GDPBIS_1  VERIF_PGDP_PGDPTER_1 VERIF_ValGDP_ValGDPTER_1 _
    verif_CH_1 Verif_G_1 Verif_I_1 verif_X_1 verif_DS_1 verif_M_1
    reporting_verif.sheet(t)
    show reporting_verif
            
 group Reporting ER_oil_1 ER_oil_2201_1 ER_Oil_2202_1 ER_elec_2301_1 ER_elec_2302_1 ER_elec_2303_1 ER_elec_2304_1 ER_elec_2305_1 ER_elec_2306_1 ER_elec_2307_1 ER_elec_2308_1 _
    ER_elec_1 ER_gas_1 ER_gas_2401_1 ER_gas_2402_1 ER_gas_2403_1 ER_gas_2404_1 ER_gas_2405_1 ER_gas_2406_1 ER_coal_1 ER_Total_1 _
    ER_Agriculture_1 ER_Indus_1 ER_Residential_1 ER_Tertiary_1 ER_Trans_Private_1 ER_Trans_Public_1  _ 
    ER_Auto_1 ER_AUTO_th_A_1 ER_AUTO_th_B_1 ER_AUTO_th_C_1 ER_AUTO_th_D_1 ER_AUTO_th_E_1 ER_AUTO_th_F_1 ER_AUTO_th_G_1 _
    ER_AUTO_elec_A_1 ER_AUTO_elec_B_1 ER_AUTO_elec_C_1 ER_AUTO_elec_D_1 ER_AUTO_elec_E_1 ER_AUTO_elec_F_1 ER_AUTO_elec_G_1 ER_Auto_Coal_1 ER_auto_th_1 ER_Auto_Elec_1 ER_Auto_Gas_1 _
    ER_NEWAUTO_1 ER_NEWAUTO_th_1 ER_newauto_th_A_1 ER_newauto_th_B_1 ER_newauto_th_C_1 ER_newauto_th_D_1 ER_newauto_th_E_1 ER_newauto_th_F_1 ER_newauto_th_G_1 _ 
    ER_NEWAUTO_elec_1 ER_NEWAUTO_elec_A_1 ER_NEWAUTO_elec_B_1 ER_NEWAUTO_elec_C_1 ER_NEWAUTO_elec_D_1 ER_NEWAUTO_elec_E_1 ER_NEWAUTO_elec_F_1 ER_NEWAUTO_elec_G_1 _ 
    ER_Agriculture_coal_1 ER_Indus_coal_1 ER_Residential_coal_1 ER_Tertiary_coal_1 ER_Trans_Private_coal_1 ER_Trans_Public_coal_1 ER_Agriculture_oil_1 ER_Indus_oil_1 ER_Residential_oil_1 ER_Tertiary_oil_1 ER_Trans_Private_oil_1 ER_Trans_Public_oil_1 _
    ER_Agriculture_elec_1 ER_Indus_elec_1 ER_Residential_elec_1 ER_Tertiary_elec_1 ER_Trans_Private_elec_1 ER_Trans_Public_elec_1 _  
    ER_Agriculture_gas_1 ER_Indus_gas_1 ER_Residential_gas_1 ER_Tertiary_gas_1 ER_Trans_Private_gas_1 ER_Trans_Public_gas_1 _
    ER_buil_1 ER_buil_A_1 ER_buil_B_1 ER_buil_C_1 ER_buil_D_1 ER_buil_E_1 ER_buil_F_1 ER_buil_G_1 _
    EMS_DC_04_1	EMS_DC_05_1	EMS_DC_1	EMS_HH_1	EMS_HH_21_1	EMS_HH_21_H01_1	EMS_HH_22_1	EMS_HH_22_H01_1	EMS_HH_24_1	EMS_HH_24_H01_1	EMS_SEC_TOT_01_1 _
    EMS_SEC_TOT_02_1	EMS_SEC_TOT_03_1	EMS_SEC_TOT_04_1	EMS_SEC_TOT_05_1	EMS_SEC_TOT_06_1	EMS_SEC_TOT_07_1	EMS_SEC_TOT_08_1	EMS_SEC_TOT_09_1	EMS_SEC_TOT_10_1	EMS_SEC_TOT_11_1	EMS_SEC_TOT_12_1	EMS_SEC_TOT_13_1	EMS_SEC_TOT_14_1 _
    EMS_SEC_TOT_15_1	EMS_SEC_TOT_16_1	EMS_SEC_TOT_17_1	EMS_SEC_TOT_18_1	EMS_SEC_TOT_19_1	EMS_SEC_TOT_1	EMS_SEC_TOT_20_1	EMS_SEC_TOT_21_05_1	EMS_SEC_TOT_21_06_1	EMS_SEC_TOT_21_07_1	EMS_SEC_TOT_21_08_1 _
    EMS_SEC_TOT_21_10_1	EMS_SEC_TOT_21_12_1	EMS_SEC_TOT_21_19_1	EMS_SEC_TOT_21_20_1	EMS_SEC_TOT_21_2304_1	EMS_SEC_TOT_2201_1	EMS_SEC_TOT_22_01_1	EMS_SEC_TOT_22_02_1	EMS_SEC_TOT_22_03_1 _
    EMS_SEC_TOT_22_04_1	EMS_SEC_TOT_22_05_1	EMS_SEC_TOT_22_06_1	EMS_SEC_TOT_22_07_1	EMS_SEC_TOT_22_08_1	EMS_SEC_TOT_22_09_1	EMS_SEC_TOT_22_12_1	EMS_SEC_TOT_22_13_1	EMS_SEC_TOT_22_14_1 _
    EMS_SEC_TOT_22_15_1	EMS_SEC_TOT_22_16_1	EMS_SEC_TOT_22_17_1	EMS_SEC_TOT_22_18_1	EMS_SEC_TOT_22_19_1	EMS_SEC_TOT_22_20_1	EMS_SEC_TOT_22_2201_1	EMS_SEC_TOT_22_2302_1	EMS_SEC_TOT_2302_1 _
    EMS_SEC_TOT_2303_1	EMS_SEC_TOT_2304_1	EMS_SEC_TOT_2401_1	EMS_SEC_TOT_24_01_1	EMS_SEC_TOT_24_02_1	EMS_SEC_TOT_24_03_1	EMS_SEC_TOT_24_04_1	EMS_SEC_TOT_24_05_1	EMS_SEC_TOT_24_06_1 _
    EMS_SEC_TOT_24_07_1	EMS_SEC_TOT_24_08_1	EMS_SEC_TOT_24_09_1	EMS_SEC_TOT_24_10_1	EMS_SEC_TOT_24_11_1	EMS_SEC_TOT_24_12_1	EMS_SEC_TOT_24_13_1 _
    EMS_SEC_TOT_24_14_1	EMS_SEC_TOT_24_15_1	EMS_SEC_TOT_24_16_1	EMS_SEC_TOT_24_17_1	EMS_SEC_TOT_24_18_1	EMS_SEC_TOT_24_19_1	EMS_SEC_TOT_24_20_1 _
    EMS_SEC_TOT_24_2201_1	 EMS_SEC_TOT_24_2303_1	 EMS_SEC_TOT_24_2401_1	 EMS_SECSOU_1	EMS_SECSOU_21_1	EMS_SECSOU_22_1	EMS_SECSOU_24_1	EMS_SOU_1	EMS_SOU_21_1	EMS_SOU_22_1	EMS_SOU_24_1	EMS_TOT_1 _
    Q_MTEP_EP_1 Q_MTEP_EP_21_1 Q_MTEP_EP_21_21_1 Q_MTEP_EP_2201_1 Q_MTEP_EP_2202_1 Q_MTEP_EP_22_2201_1 Q_MTEP_EP_22_2202_1 Q_MTEP_EP_2301_1 _
    Q_MTEP_EP_2302_1 Q_MTEP_EP_2303_1 Q_MTEP_EP_2304_1 Q_MTEP_EP_2305_1 Q_MTEP_EP_2306_1 Q_MTEP_EP_2307_1 Q_MTEP_EP_2308_1 Q_MTEP_EP_23_2301_1 _
    Q_MTEP_EP_23_2302_1 Q_MTEP_EP_23_2303_1 Q_MTEP_EP_23_2304_1 Q_MTEP_EP_23_2305_1 Q_MTEP_EP_23_2306_1 Q_MTEP_EP_23_2307_1 Q_MTEP_EP_23_2308_1 _
    Q_MTEP_EP_2401_1 Q_MTEP_EP_2402_1 Q_MTEP_EP_2403_1 Q_MTEP_EP_2404_1 Q_MTEP_EP_2405_1 Q_MTEP_EP_2406_1 Q_MTEP_EP_24_2401_1 Q_MTEP_EP_24_2402_1 _
    Q_MTEP_EP_24_2403_1 Q_MTEP_EP_24_2404_1 Q_MTEP_EP_24_2405_1 Q_MTEP_EP_24_2406_1 Q_MTEP_INDUS_21_1 Q_MTEP_INDUS_22_1 Q_MTEP_INDUS_23_1 Q_MTEP_INDUS_24_1 _
    PHIY_EF_TOT_22_2201_1 PHIY_EF_TOT_22_2202_1 PHIY_EF_TOT_23_2301_1 PHIY_EF_TOT_23_2302_1 PHIY_EF_TOT_23_2303_1 PHIY_EF_TOT_23_2304_1 PHIY_EF_TOT_23_2305_1 _
    PHIY_EF_TOT_23_2306_1 PHIY_EF_TOT_23_2307_1 PHIY_EF_TOT_23_2308_1 PHIY_EF_TOT_24_2401_1 PHIY_EF_TOT_24_2402_1 PHIY_EF_TOT_24_2403_1 PHIY_EF_TOT_24_2404_1 _
    PHIY_EF_TOT_24_2405_1 PHIY_EF_TOT_24_2406_1 IC_HH_22_h01_1 IC_HH_24_h01_0
  Reporting.sheet(t)
  show Reporting


  else
  
   if %scenario_name = "baseline" then
    ' Load a realistic reference scenario if requested
    if %ref="realist" then
      call load_realist
    endif

    call solvemodel(%solveopt)
  
 group reporting_verif VERIF_GDP_GDPBIS_0 VERIF_GDP_GDPTER_0  VERIF_PGDP_PGDPBIS_0 VERIF_GDPBIS_GDPTER_0 VERIF_GDPTER_GDPBIS_0  VERIF_PGDP_PGDPTER_0 VERIF_ValGDP_ValGDPTER_0 _
    verif_CH_0 Verif_G_0 Verif_I_0 verif_X_0 verif_DS_0 verif_M_0
    reporting_verif.sheet(t)
    show reporting_verif
            
 group Reporting ER_oil_0 ER_oil_2201_0 ER_Oil_2202_0 ER_elec_2301_0 ER_elec_2302_0 ER_elec_2303_0 ER_elec_2304_0 ER_elec_2305_0 ER_elec_2306_0 ER_elec_2307_0 ER_elec_2308_0 _
    ER_elec_0 ER_gas_0 ER_gas_2401_0 ER_gas_2402_0 ER_gas_2403_0 ER_gas_2404_0 ER_gas_2405_0 ER_gas_2406_0 ER_coal_0 ER_Total_0 _
    ER_Agriculture_0 ER_Indus_0 ER_Residential_0 ER_Tertiary_0 ER_Trans_Private_0 ER_Trans_Public_0  _ 
    ER_Auto_0 ER_AUTO_th_A_0 ER_AUTO_th_B_0 ER_AUTO_th_C_0 ER_AUTO_th_D_0 ER_AUTO_th_E_0 ER_AUTO_th_F_0 ER_AUTO_th_G_0 _
    ER_AUTO_elec_A_0 ER_AUTO_elec_B_0 ER_AUTO_elec_C_0 ER_AUTO_elec_D_0 ER_AUTO_elec_E_0 ER_AUTO_elec_F_0 ER_AUTO_elec_G_0 ER_Auto_Coal_0 ER_auto_th_0 ER_Auto_Elec_0 ER_Auto_Gas_0 _
    ER_NEWAUTO_0 ER_NEWAUTO_th_0 ER_newauto_th_A_0 ER_newauto_th_B_0 ER_newauto_th_C_0 ER_newauto_th_D_0 ER_newauto_th_E_0 ER_newauto_th_F_0 ER_newauto_th_G_0 _ 
    ER_NEWAUTO_elec_0 ER_NEWAUTO_elec_A_0 ER_NEWAUTO_elec_B_0 ER_NEWAUTO_elec_C_0 ER_NEWAUTO_elec_D_0 ER_NEWAUTO_elec_E_0 ER_NEWAUTO_elec_F_0 ER_NEWAUTO_elec_G_0 _ 
    ER_Agriculture_coal_0 ER_Indus_coal_0 ER_Residential_coal_0 ER_Tertiary_coal_0 ER_Trans_Private_coal_0 ER_Trans_Public_coal_0 ER_Agriculture_oil_0 ER_Indus_oil_0 ER_Residential_oil_0 ER_Tertiary_oil_0 ER_Trans_Private_oil_0 ER_Trans_Public_oil_0 _
    ER_Agriculture_elec_0 ER_Indus_elec_0 ER_Residential_elec_0 ER_Tertiary_elec_0 ER_Trans_Private_elec_0 ER_Trans_Public_elec_0 _  
    ER_Agriculture_gas_0 ER_Indus_gas_0 ER_Residential_gas_0 ER_Tertiary_gas_0 ER_Trans_Private_gas_0 ER_Trans_Public_gas_0 _
    ER_buil_0 ER_buil_A_0 ER_buil_B_0 ER_buil_C_0 ER_buil_D_0 ER_buil_E_0 ER_buil_F_0 ER_buil_G_0 _
    EMS_DC_04_0	EMS_DC_05_0	EMS_DC_0	EMS_HH_0	EMS_HH_21_0	EMS_HH_21_H01_0	EMS_HH_22_0	EMS_HH_22_H01_0	EMS_HH_24_0	EMS_HH_24_H01_0	EMS_SEC_TOT_01_0 _
    EMS_SEC_TOT_02_0	EMS_SEC_TOT_03_0	EMS_SEC_TOT_04_0	EMS_SEC_TOT_05_0	EMS_SEC_TOT_06_0	EMS_SEC_TOT_07_0	EMS_SEC_TOT_08_0	EMS_SEC_TOT_09_0	EMS_SEC_TOT_10_0	EMS_SEC_TOT_11_0	EMS_SEC_TOT_12_0	EMS_SEC_TOT_13_0	EMS_SEC_TOT_14_0 _
    EMS_SEC_TOT_15_0	EMS_SEC_TOT_16_0	EMS_SEC_TOT_17_0	EMS_SEC_TOT_18_0	EMS_SEC_TOT_19_0	EMS_SEC_TOT_0	EMS_SEC_TOT_20_0	EMS_SEC_TOT_21_05_0	EMS_SEC_TOT_21_06_0	EMS_SEC_TOT_21_07_0	EMS_SEC_TOT_21_08_0 _
    EMS_SEC_TOT_21_10_0	EMS_SEC_TOT_21_12_0	EMS_SEC_TOT_21_19_0	EMS_SEC_TOT_21_20_0	EMS_SEC_TOT_21_2304_0	EMS_SEC_TOT_2201_0	EMS_SEC_TOT_22_01_0	EMS_SEC_TOT_22_02_0	EMS_SEC_TOT_22_03_0 _
    EMS_SEC_TOT_22_04_0	EMS_SEC_TOT_22_05_0	EMS_SEC_TOT_22_06_0	EMS_SEC_TOT_22_07_0	EMS_SEC_TOT_22_08_0	EMS_SEC_TOT_22_09_0	EMS_SEC_TOT_22_12_0	EMS_SEC_TOT_22_13_0	EMS_SEC_TOT_22_14_0 _
    EMS_SEC_TOT_22_15_0	EMS_SEC_TOT_22_16_0	EMS_SEC_TOT_22_17_0	EMS_SEC_TOT_22_18_0	EMS_SEC_TOT_22_19_0	EMS_SEC_TOT_22_20_0	EMS_SEC_TOT_22_2201_0	EMS_SEC_TOT_22_2302_0	EMS_SEC_TOT_2302_0 _
    EMS_SEC_TOT_2303_0	EMS_SEC_TOT_2304_0	EMS_SEC_TOT_2401_0	EMS_SEC_TOT_24_01_0	EMS_SEC_TOT_24_02_0	EMS_SEC_TOT_24_03_0	EMS_SEC_TOT_24_04_0	EMS_SEC_TOT_24_05_0	EMS_SEC_TOT_24_06_0 _
    EMS_SEC_TOT_24_07_0	EMS_SEC_TOT_24_08_0	EMS_SEC_TOT_24_09_0	EMS_SEC_TOT_24_10_0	EMS_SEC_TOT_24_11_0	EMS_SEC_TOT_24_12_0	EMS_SEC_TOT_24_13_0 _
    EMS_SEC_TOT_24_14_0	EMS_SEC_TOT_24_15_0	EMS_SEC_TOT_24_16_0	EMS_SEC_TOT_24_17_0	EMS_SEC_TOT_24_18_0	EMS_SEC_TOT_24_19_0	EMS_SEC_TOT_24_20_0 _
    EMS_SEC_TOT_24_2201_0	 EMS_SEC_TOT_24_2303_0	 EMS_SEC_TOT_24_2401_0	 EMS_SECSOU_0	EMS_SECSOU_21_0	EMS_SECSOU_22_0	EMS_SECSOU_24_0	EMS_SOU_0	EMS_SOU_21_0	EMS_SOU_22_0	EMS_SOU_24_0	EMS_TOT_0 _
    Q_MTEP_EP_0 Q_MTEP_EP_21_0 Q_MTEP_EP_21_21_0 Q_MTEP_EP_2201_0 Q_MTEP_EP_2202_0 Q_MTEP_EP_22_2201_0 Q_MTEP_EP_22_2202_0 Q_MTEP_EP_2301_0 _
    Q_MTEP_EP_2302_0 Q_MTEP_EP_2303_0 Q_MTEP_EP_2304_0 Q_MTEP_EP_2305_0 Q_MTEP_EP_2306_0 Q_MTEP_EP_2307_0 Q_MTEP_EP_2308_0 Q_MTEP_EP_23_2301_0 _
    Q_MTEP_EP_23_2302_0 Q_MTEP_EP_23_2303_0 Q_MTEP_EP_23_2304_0 Q_MTEP_EP_23_2305_0 Q_MTEP_EP_23_2306_0 Q_MTEP_EP_23_2307_0 Q_MTEP_EP_23_2308_0 _
    Q_MTEP_EP_2401_0 Q_MTEP_EP_2402_0 Q_MTEP_EP_2403_0 Q_MTEP_EP_2404_0 Q_MTEP_EP_2405_0 Q_MTEP_EP_2406_0 Q_MTEP_EP_24_2401_0 Q_MTEP_EP_24_2402_0 _
    Q_MTEP_EP_24_2403_0 Q_MTEP_EP_24_2404_0 Q_MTEP_EP_24_2405_0 Q_MTEP_EP_24_2406_0 Q_MTEP_INDUS_21_0 Q_MTEP_INDUS_22_0 Q_MTEP_INDUS_23_0 Q_MTEP_INDUS_24_0 _
    PHIY_EF_TOT_22_2201_0 PHIY_EF_TOT_22_2202_0 PHIY_EF_TOT_23_2301_0 PHIY_EF_TOT_23_2302_0 PHIY_EF_TOT_23_2303_0 PHIY_EF_TOT_23_2304_0 PHIY_EF_TOT_23_2305_0 _
    PHIY_EF_TOT_23_2306_0 PHIY_EF_TOT_23_2307_0 PHIY_EF_TOT_23_2308_0 PHIY_EF_TOT_24_2401_0 PHIY_EF_TOT_24_2402_0 PHIY_EF_TOT_24_2403_0 PHIY_EF_TOT_24_2404_0 _
    PHIY_EF_TOT_24_2405_0 PHIY_EF_TOT_24_2406_0 IC_HH_22_h01_0 IC_HH_24_h01_0
  Reporting.sheet(t)
  show Reporting
  
  else

    ' Create a new scenario that can be compared with the baseline
    {%modelname}.scenario(n, a=2) {%scenario_name}

    ' Load data for the shock to be simulated
    call load_shocks(%scenario_name)

    call solvemodel(%solveopt)

    'call output_template(%scenario_name)

   if %run_shock="yes" then
    
	if %exceptions_DGT="yes" then 
	
    group Reporting_2 ER_oil_2 ER_oil_2201_2 ER_Oil_2202_2 ER_elec_2301_2 ER_elec_2302_2 ER_elec_2303_2 ER_elec_2304_2 ER_elec_2305_2 ER_elec_2306_2 _
    ER_elec_2307_2 ER_elec_2308_2 ER_elec_2 ER_gas_2 ER_gas_2401_2 ER_gas_2402_2 ER_gas_2403_2 ER_gas_2404_2 ER_gas_2405_2 ER_gas_2406_2 ER_coal_2 _ 
    ER_Total_2 ER_Agriculture_2 ER_Indus_2 ER_Residential_2 ER_Tertiary_2 ER_Trans_Private_2 ER_Trans_Public_2  _
    ER_Auto_2 ER_AUTO_th_A_2 ER_AUTO_th_B_2 ER_AUTO_th_C_2 ER_AUTO_th_D_2 ER_AUTO_th_E_2 ER_AUTO_th_F_2 ER_AUTO_th_G_2 ER_AUTO_elec_A_2 _
    ER_AUTO_elec_B_2 ER_AUTO_elec_C_2 ER_AUTO_elec_D_2 ER_AUTO_elec_E_2 ER_AUTO_elec_F_2 ER_AUTO_elec_G_2 ER_Auto_Coal_2 ER_Auto_th_2 _ 
    ER_Auto_Elec_2 ER_Auto_Gas_2 ER_NEWAUTO_2 ER_NEWAUTO_th_2 ER_newauto_th_A_2 ER_newauto_th_B_2 ER_newauto_th_C_2 ER_newauto_th_D_2 _
    ER_newauto_th_E_2 ER_newauto_th_F_2 ER_newauto_th_G_2 ER_NEWAUTO_elec_2 ER_NEWAUTO_elec_A_2 ER_NEWAUTO_elec_B_2 ER_NEWAUTO_elec_C_2 _
    ER_NEWAUTO_elec_D_2 ER_NEWAUTO_elec_E_2 ER_NEWAUTO_elec_F_2 ER_NEWAUTO_elec_G_2 ER_Agriculture_coal_2 ER_Indus_coal_2 ER_Residential_coal_2 _
    ER_Tertiary_coal_2 ER_Trans_Private_coal_2 ER_Trans_Public_coal_2 ER_Agriculture_oil_2 ER_Indus_oil_2 _
    ER_Residential_oil_2 ER_Tertiary_oil_2 ER_Trans_Private_oil_2 ER_Trans_Public_oil_2 ER_Agriculture_elec_2 ER_Indus_elec_2 _
    ER_Residential_elec_2 ER_Tertiary_elec_2 ER_Trans_Private_elec_2 ER_Trans_Public_elec_2  ER_Agriculture_gas_2 ER_Indus_gas_2 _
    ER_Residential_gas_2 ER_Tertiary_gas_2 ER_Trans_Private_gas_2 ER_Trans_Public_gas_2 ER_buil_2 ER_buil_A_2 ER_buil_B_2 ER_buil_C_2 _
    ER_buil_D_2 ER_buil_E_2 ER_buil_F_2 ER_buil_G_2 TTCO_VOL_signal_2 TTCO_VOL_signal_1 PGDP_2 PGDP_1 (GDP_2/GDP_1-1)*100 _
    (CH_2/CH_1-1)*100 (I_2/I_1-1)*100 (X_2/X_1-1)*100 (M_2/M_1-1)*100 (DC_val_2/(PGDP_2*GDP_2)-DC_val_1/(PGDP_1*GDP_1))*100 (UnR_TOT_2-UnR_TOT_1)*100 _
    (L_2/L_1-1)*100 ((W_S_2/PCh_2)/(W_S_1/PCH_1)-1)*100 infl_FR_2-infl_FR_1 R_2-R_1 (DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL_1/(PGDP_1*GDP_1))*100 _
    (DP_G_VAL_2-DP_G_VAL_1)*100  EMS_TOT_2/@elem(EMS_TOT,%baseyear)*100 100*(GDP_2/GDP_1-1) 100*(CH_2/CH_1-1) 100*(I_2/I_1-1) 100*(X_2/X_1-1) 100*(M_2/M_1-1) _
    100*(UNR_TOT_2-UNR_TOT_1)  100*(L_2/L_1-1) 100*((W_2/PCH_2)/(W_1/PCH_1)-1) 100*(PCH_2/PCH_1-1) 100*(R_2-R_1) 100*(Debt_g_val_2/(PGDP_2*GDP_2)-Debt_g_val_1/(PGDP_1*GDP_1)) _
    100*(DP_G_val_2-DP_G_val_1) 100*(GDP_2/@ELEM(GDP_2,"2006")) 100*(EMS_TOT_2/EMS_TOT_1-1) 100*(EMS_TOT_2/@ELEM(EMS_TOT_2,"2006")) _
    EMS_DC_04_2	EMS_DC_05_2	EMS_DC_2	EMS_HH_2	EMS_HH_21_2	EMS_HH_21_H01_2	EMS_HH_22_2	EMS_HH_22_H01_2	EMS_HH_24_2	EMS_HH_24_H01_2	EMS_SEC_TOT_01_2 _
    EMS_SEC_TOT_02_2	EMS_SEC_TOT_03_2	EMS_SEC_TOT_04_2	EMS_SEC_TOT_05_2	EMS_SEC_TOT_06_2	EMS_SEC_TOT_07_2	EMS_SEC_TOT_08_2	EMS_SEC_TOT_09_2	EMS_SEC_TOT_10_2	EMS_SEC_TOT_11_2	EMS_SEC_TOT_12_2	EMS_SEC_TOT_13_2	EMS_SEC_TOT_14_2 _
    EMS_SEC_TOT_15_2	EMS_SEC_TOT_16_2	EMS_SEC_TOT_17_2	EMS_SEC_TOT_18_2	EMS_SEC_TOT_19_2	EMS_SEC_TOT_2	EMS_SEC_TOT_20_2	EMS_SEC_TOT_21_05_2	EMS_SEC_TOT_21_06_2	EMS_SEC_TOT_21_07_2	EMS_SEC_TOT_21_08_2 _
    EMS_SEC_TOT_21_10_2	EMS_SEC_TOT_21_12_2	EMS_SEC_TOT_21_19_2	EMS_SEC_TOT_21_20_2	EMS_SEC_TOT_21_2304_2	EMS_SEC_TOT_2201_2	EMS_SEC_TOT_22_01_2	EMS_SEC_TOT_22_02_2	EMS_SEC_TOT_22_03_2 _
    EMS_SEC_TOT_22_04_2	EMS_SEC_TOT_22_05_2	EMS_SEC_TOT_22_06_2	EMS_SEC_TOT_22_07_2	EMS_SEC_TOT_22_08_2	EMS_SEC_TOT_22_09_2	EMS_SEC_TOT_22_12_2	EMS_SEC_TOT_22_13_2	EMS_SEC_TOT_22_14_2 _
    EMS_SEC_TOT_22_15_2	EMS_SEC_TOT_22_16_2	EMS_SEC_TOT_22_17_2	EMS_SEC_TOT_22_18_2	EMS_SEC_TOT_22_19_2	EMS_SEC_TOT_22_20_2	EMS_SEC_TOT_22_2201_2	EMS_SEC_TOT_22_2302_2	EMS_SEC_TOT_2302_2 _
    EMS_SEC_TOT_2303_2	EMS_SEC_TOT_2304_2	EMS_SEC_TOT_2401_2	EMS_SEC_TOT_24_01_2	EMS_SEC_TOT_24_02_2	EMS_SEC_TOT_24_03_2	EMS_SEC_TOT_24_04_2	EMS_SEC_TOT_24_05_2	EMS_SEC_TOT_24_06_2 _
    EMS_SEC_TOT_24_07_2	EMS_SEC_TOT_24_08_2	EMS_SEC_TOT_24_09_2	EMS_SEC_TOT_24_10_2	EMS_SEC_TOT_24_11_2	EMS_SEC_TOT_24_12_2	EMS_SEC_TOT_24_13_2 _
    EMS_SEC_TOT_24_14_2	EMS_SEC_TOT_24_15_2	EMS_SEC_TOT_24_16_2	EMS_SEC_TOT_24_17_2	EMS_SEC_TOT_24_18_2	EMS_SEC_TOT_24_19_2	EMS_SEC_TOT_24_20_2 _
    EMS_SEC_TOT_24_2201_2	EMS_SEC_TOT_24_2303_2	EMS_SEC_TOT_24_2401_2	EMS_SECSOU_2	EMS_SECSOU_21_2	EMS_SECSOU_22_2	EMS_SECSOU_24_2	EMS_SOU_2	EMS_SOU_21_2	EMS_SOU_22_2	EMS_SOU_24_2	EMS_TOT_2 _ 
    Q_MTEP_EP_2 Q_MTEP_EP_21_2 Q_MTEP_EP_21_21_2 Q_MTEP_EP_2201_2 Q_MTEP_EP_2202_2 Q_MTEP_EP_22_2201_2 Q_MTEP_EP_22_2202_2 Q_MTEP_EP_2301_2 _
    Q_MTEP_EP_2302_2 Q_MTEP_EP_2303_2 Q_MTEP_EP_2304_2 Q_MTEP_EP_2305_2 Q_MTEP_EP_2306_2 Q_MTEP_EP_2307_2 Q_MTEP_EP_2308_2 Q_MTEP_EP_23_2301_2 _
    Q_MTEP_EP_23_2302_2 Q_MTEP_EP_23_2303_2 Q_MTEP_EP_23_2304_2 Q_MTEP_EP_23_2305_2 Q_MTEP_EP_23_2306_2 Q_MTEP_EP_23_2307_2 Q_MTEP_EP_23_2308_2 _
    Q_MTEP_EP_2401_2 Q_MTEP_EP_2402_2 Q_MTEP_EP_2403_2 Q_MTEP_EP_2404_2 Q_MTEP_EP_2405_2 Q_MTEP_EP_2406_2 Q_MTEP_EP_24_2401_2 Q_MTEP_EP_24_2402_2 _
    Q_MTEP_EP_24_2403_2 Q_MTEP_EP_24_2404_2 Q_MTEP_EP_24_2405_2 Q_MTEP_EP_24_2406_2 Q_MTEP_INDUS_21_2 Q_MTEP_INDUS_22_2 Q_MTEP_INDUS_23_2 Q_MTEP_INDUS_24_2 _
    PHIY_EF_TOT_22_2201_2 PHIY_EF_TOT_22_2202_2 PHIY_EF_TOT_23_2301_2 PHIY_EF_TOT_23_2302_2 PHIY_EF_TOT_23_2303_2 PHIY_EF_TOT_23_2304_2 PHIY_EF_TOT_23_2305_2 _
    PHIY_EF_TOT_23_2306_2 PHIY_EF_TOT_23_2307_2 PHIY_EF_TOT_23_2308_2 PHIY_EF_TOT_24_2401_2 PHIY_EF_TOT_24_2402_2 PHIY_EF_TOT_24_2403_2 PHIY_EF_TOT_24_2404_2 _
    PHIY_EF_TOT_24_2405_2 PHIY_EF_TOT_24_2406_2 IC_HH_22_h01_2 IC_HH_24_h01_2 _
    KM_auto_H01_2 KM_TRAVELER_18_H01_2 KM_TRAV_AUTO_LD_H01_2 KM_TRAV_AUTO_CD_H01_2 KM_TRAVELER_14_H01_2 KM_TRAVELER_15_H01_2 KM_TRAVELER_CD_H01_2 KM_TRAVELER_LD_H01_2 _
    KM_AUTO_H01_1 KM_TRAVELER_18_H01_1 KM_TRAV_AUTO_LD_H01_1 KM_TRAV_AUTO_CD_H01_1 KM_TRAVELER_14_H01_1 _
    KM_TRAVELER_15_H01_1 KM_TRAVELER_CD_H01_1 KM_TRAVELER_LD_H01_1

    

  Reporting_2.sheet(t)
  show Reporting_2
  

   group Reporting_3 100*(GDP_2/GDP_1-1) 100*((VA_2-VA_20_2)/(VA_1-VA_20_1)-1) 100*((CH_2-CH_1)/GDP_1) 100*((CH_03_2-CH_03_1)/GDP_1) _
               100*((G_2-G_1)/GDP_1) 100*((I_2-I_1)/GDP_1) 100*((DS_2-DS_1)/GDP_1) 100*((X_2-X_1)/GDP_1)-100*((M_2-M_1)/GDP_1) _
               100*((CH_2-CH_13_2-(CH_1-CH_13_1))/GDP_1) 100*((I_2+CH_13_2-(I_1+CH_13_1))/GDP_1) 100*((I_2-IA_20_2-(I_1-IA_20_1))/GDP_1) 100*((CH_13_2-CH_13_1)/GDP_1) 100*((IA_20_2-IA_20_1)/GDP_1) _
               100*(CH_2/CH_1-1) 100*(CH_03_2/CH_03_1-1) 100*(G_2/G_1-1) 100*(I_2/I_1-1) 100*((I_2-IA_20_2)/(I_1-IA_20_1)-1) 100*(X_2/X_1-1) 100*(M_2/M_1-1) _
              100*((CH_2-CH_13_2)/(CH_1-CH_13_1)-1) 100*((I_2+CH_13_2)/(I_1+CH_13_1)-1) 100*((CH_13_2)/(CH_13_1)-1) 100*((IA_20_2)/(IA_20_1)-1) _
      100*(DISPINC_VAL_2/DISPINC_VAL_1-1) 100*(DISPINC_VAL_2/DISPINC_VAL_1*PCH_1/PCH_2-1) 100*(DISPINC_VAL_2/DISPINC_VAL_1*L_1/L_2-1) _
      100*(DISPINC_VAL_2/DISPINC_VAL_1*L_1/L_2*PCH_1/PCH_2-1) 100*((dispinc_val_H01_2-(PCH_2*CH_2-pch_13_2*ch_13_2))/dispinc_val_H01_2-(dispinc_val_H01_1-(PCH_1*CH_1-pch_13_1*ch_13_1))/dispinc_val_H01_1) 100*(PCH_2/PCH_1-1) 100*(PY_2/PY_1-1) 100*(PX_2/PX_1-1) _
      100*(PM_2/PM_1-1) 100*(W_2/W_1-1) 100*((W_2/PCH_2)/(W_1/PCH_1)-1) 100*(CL_2/CL_1-1) 100*((CL_2/PVA_2)/(CL_1/PVA_1)-1) ((L_2/L_1)-1)*100 _
      L_2-L_1 100*(UNR_TOT_2-UNR_TOT_1) 100*(DC_VAL_2/(GDP_2*PGDP_2)-DC_VAL_1/(GDP_1*PGDP_1)) _
      100*((M_21_2*PM_21_2-X_21_2*PX_21_2+M_22_2*PM_22_2-X_22_2*PX_22_2+M_23_2*PM_23_2-X_23_2*PX_23_2+M_24_2*PM_24_2-X_24_2*PX_24_2)/(GDP_2*PGDP_2))-100*((M_21_1*PM_21_1-X_21_1*PX_21_1+M_22_1*PM_22_1-X_22_1*PX_22_1+M_23_1*PM_23_1-X_23_1*PX_23_1+M_24_1*PM_24_1-X_24_1*PX_24_1)/(GDP_1*PGDP_1)) 100*((-SP_G_VAL_2)/(GDP_2*PGDP_2)-(-SP_G_VAL_1)/(GDP_1*PGDP_1)) _
      100*((-BF_G_VAL_2)/(GDP_2*PGDP_2)-(-BF_G_VAL_1)/(GDP_1*PGDP_1)) (DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL_1/(PGDP_1*GDP_1))*100 _ 
DEP_val_2 REC_VAL_2 (0-BF_G_VAL_2) DEBT_G_VAL_2 REC_TCO_VAL_2 ENERT_22_2*PENERT_22_2 ENERT_24_2*PENERT_24_2 ENERT_23_2*PENERT_23_2 ENERT_21_2*PENERT_21_2 TCO_VAL_HH_2 _
TCO_VAL_HH_21_H01_2+TCO_VAL_HH_24_H01_2+TCO_VAL_HH_22_H01_2*(Q_MTEP_H_BUIL_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) _
TCO_VAL_HH_22_H01_2*(Q_MTEP_H_BUIL_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) TCO_VAL_HH_24_H01_2 TCO_VAL_HH_21_H01_2 _
TCO_VAL_HH_22_H01_2*(Q_MTEP_H_AUTO_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) TCO_VAL_SEC_2 REC_TCO_VAL_ETS_2  _
TCO_VAL_22_04_2+TCO_VAL_22_05_2+TCO_VAL_22_06_2+TCO_VAL_22_07_2+TCO_VAL_22_08_2+TCO_VAL_22_09_2+TCO_VAL_22_10_2+TCO_VAL_22_18_2+TCO_VAL_22_21_2+tco_val_22_2201_2+tco_val_22_2202_2+TCO_VAL_22_2301_2+TCO_VAL_22_2302_2+TCO_VAL_22_2303_2+TCO_VAL_22_2304_2+TCO_VAL_22_2305_2+TCO_VAL_22_2306_2+TCO_VAL_22_2307_2+TCO_VAL_22_2308_2+TCO_VAL_22_2401_2+TCO_VAL_22_2402_2+TCO_VAL_22_2403_2+TCO_VAL_22_2404_2+TCO_VAL_22_2405_2+TCO_VAL_22_2406_2 _ 	
TCO_VAL_24_04_2+TCO_VAL_24_05_2+TCO_VAL_24_06_2+TCO_VAL_24_07_2+TCO_VAL_24_08_2+TCO_VAL_24_09_2+TCO_VAL_24_10_2+TCO_VAL_24_18_2+TCO_VAL_24_21_2+tco_val_24_2201_2+tco_val_24_2202_2+TCO_VAL_24_2301_2+TCO_VAL_24_2302_2+TCO_VAL_24_2303_2+TCO_VAL_24_2304_2+TCO_VAL_24_2305_2+TCO_VAL_24_2306_2+TCO_VAL_24_2307_2+TCO_VAL_24_2308_2+TCO_VAL_24_2401_2+TCO_VAL_24_2402_2+TCO_VAL_24_2403_2+TCO_VAL_24_2404_2+TCO_VAL_24_2405_2+TCO_VAL_24_2406_2 _	
TCO_VAL_21_04_2+TCO_VAL_21_05_2+TCO_VAL_21_06_2+TCO_VAL_21_07_2+TCO_VAL_21_08_2+TCO_VAL_21_09_2+TCO_VAL_21_10_2+TCO_VAL_21_18_2+TCO_VAL_21_21_2+tco_val_21_2201_2+tco_val_21_2202_2+TCO_VAL_21_2301_2+TCO_VAL_21_2302_2+TCO_VAL_21_2303_2+TCO_VAL_21_2304_2+TCO_VAL_21_2305_2+TCO_VAL_21_2306_2+TCO_VAL_21_2307_2+TCO_VAL_21_2308_2+TCO_VAL_21_2401_2+TCO_VAL_21_2402_2+TCO_VAL_21_2403_2+TCO_VAL_21_2404_2+TCO_VAL_21_2405_2+TCO_VAL_21_2406_2 _
REC_TCO_VAL_NETS_2 _
TCO_VAL_22_01_2+TCO_VAL_22_02_2+TCO_val_22_03_2+TCO_VAL_22_11_2+TCO_VAL_22_12_2+TCO_VAL_22_13_2 _
TCO_VAL_24_01_2+TCO_VAL_24_02_2+TCO_val_24_03_2+TCO_VAL_24_11_2+TCO_VAL_24_12_2+TCO_VAL_24_13_2 _
TCO_VAL_21_01_2+TCO_VAL_21_02_2+TCO_val_21_03_2+TCO_VAL_21_11_2+TCO_VAL_21_12_2+TCO_VAL_21_13_2 _
TCO_VAL_SEC_14_2+TCO_VAL_SEC_15_2+TCO_VAL_SEC_16_2+TCO_VAL_SEC_17_2+TCO_VAL_SEC_19_2+TCO_VAL_SEC_20_2	TCO_VAL_22_14_2+TCO_VAL_22_15_2+TCO_VAL_22_16_2+TCO_VAL_22_17_2+TCO_VAL_22_19_2+TCO_VAL_22_20_2 _ 
TCO_VAL_24_14_2+TCO_VAL_24_15_2+TCO_VAL_24_16_2+TCO_VAL_24_17_2+TCO_VAL_24_19_2+TCO_VAL_24_20_2 _
TCO_VAL_21_14_2+TCO_VAL_21_15_2+TCO_VAL_21_16_2+TCO_VAL_21_17_2+TCO_VAL_21_19_2+TCO_VAL_21_20_2 _
DEP_val_1 REC_VAL_1 (0-BF_G_VAL_1) DEBT_G_VAL_1 REC_TCO_VAL_1	ENERT_22_1*PENERT_22_1	ENERT_24_1*PENERT_24_1	ENERT_23_1*PENERT_23_1 ENERT_21_1*PENERT_21_1 _
TCO_VAL_HH_1 _
TCO_VAL_HH_21_H01_1+TCO_VAL_HH_24_H01_1+TCO_VAL_HH_22_H01_1*(Q_MTEP_H_BUIL_22_1)/(Q_MTEP_H_BUIL_22_1+Q_MTEP_H_AUTO_22_1) _
TCO_VAL_HH_22_H01_1*(Q_MTEP_H_BUIL_22_1)/(Q_MTEP_H_BUIL_22_1+Q_MTEP_H_AUTO_22_1) _
TCO_VAL_HH_24_H01_1 _
TCO_VAL_HH_21_H01_1 _
TCO_VAL_HH_22_H01_1*(Q_MTEP_H_AUTO_22_1)/(Q_MTEP_H_BUIL_22_1+Q_MTEP_H_AUTO_22_1) _
TCO_VAL_SEC_1 REC_TCO_VAL_ETS_1 _
TCO_VAL_22_04_1+TCO_VAL_22_05_1+TCO_VAL_22_06_1+TCO_VAL_22_07_1+TCO_VAL_22_08_1+TCO_VAL_22_09_1+TCO_VAL_22_10_1+TCO_VAL_22_18_1+TCO_VAL_22_21_1+tco_val_22_2201_1+tco_val_22_2202_1+TCO_VAL_22_2301_1+TCO_VAL_22_2302_1+TCO_VAL_22_2303_1+TCO_VAL_22_2304_1+TCO_VAL_22_2305_1+TCO_VAL_22_2306_1+TCO_VAL_22_2307_1+TCO_VAL_22_2308_1+TCO_VAL_22_2401_1+TCO_VAL_22_2402_1+TCO_VAL_22_2403_1+TCO_VAL_22_2404_1+TCO_VAL_22_2405_1+TCO_VAL_22_2406_1 _
TCO_VAL_24_04_1+TCO_VAL_24_05_1+TCO_VAL_24_06_1+TCO_VAL_24_07_1+TCO_VAL_24_08_1+TCO_VAL_24_09_1+TCO_VAL_24_10_1+TCO_VAL_24_18_1+TCO_VAL_24_21_1+tco_val_24_2201_1+TCO_VAL_24_2202_1+TCO_VAL_24_2301_1+TCO_VAL_24_2302_1+TCO_VAL_24_2303_1+TCO_VAL_24_2304_1+TCO_VAL_24_2305_1+TCO_VAL_24_2306_1+TCO_VAL_24_2307_1+TCO_VAL_24_2308_1+TCO_VAL_24_2401_1+TCO_VAL_24_2402_1+TCO_VAL_24_2403_1+TCO_VAL_24_2404_1+TCO_VAL_24_2405_1+TCO_VAL_24_2406_1 _
TCO_VAL_21_04_1+TCO_VAL_21_05_1+TCO_VAL_21_06_1+TCO_VAL_21_07_1+TCO_VAL_21_08_1+TCO_VAL_21_09_1+TCO_VAL_21_10_1+TCO_VAL_21_18_1+TCO_VAL_21_21_1+tco_val_21_2201_1+tco_val_21_2202_1+TCO_VAL_21_2301_1+TCO_VAL_21_2302_1+TCO_VAL_21_2303_1+TCO_VAL_21_2304_1+TCO_VAL_21_2305_1+TCO_VAL_21_2306_1+TCO_VAL_21_2307_1+TCO_VAL_21_2308_1+TCO_VAL_21_2401_1+TCO_VAL_21_2402_1+TCO_VAL_21_2403_1+TCO_VAL_21_2404_1+TCO_VAL_21_2405_1+TCO_VAL_21_2406_1 _
REC_TCO_VAL_NETS_1 _
TCO_VAL_22_01_1+TCO_VAL_22_02_1+TCO_val_22_03_1+TCO_VAL_22_11_1+TCO_VAL_22_12_1+TCO_VAL_22_13_1 _
TCO_VAL_24_01_1+TCO_VAL_24_02_1+TCO_val_24_03_1+TCO_VAL_24_11_1+TCO_VAL_24_12_1+TCO_VAL_24_13_1 _
TCO_VAL_21_01_1+TCO_VAL_21_02_1+TCO_val_21_03_1+TCO_VAL_21_11_1+TCO_VAL_21_12_1+TCO_VAL_21_13_1 _ 
TCO_VAL_SEC_14_1+TCO_VAL_SEC_15_1+TCO_VAL_SEC_16_1+TCO_VAL_SEC_17_1+TCO_VAL_SEC_19_1+TCO_VAL_SEC_20_1	TCO_VAL_22_14_1+TCO_VAL_22_15_1+TCO_VAL_22_16_1+TCO_VAL_22_17_1+TCO_VAL_22_19_1+TCO_VAL_22_20_1 TCO_VAL_24_14_1+TCO_VAL_24_15_1+TCO_VAL_24_16_1+TCO_VAL_24_17_1+TCO_VAL_24_19_1+TCO_VAL_24_20_1 _ 
TCO_VAL_21_14_1+TCO_VAL_21_15_1+TCO_VAL_21_16_1+TCO_VAL_21_17_1+TCO_VAL_21_19_1+TCO_VAL_21_20_1 _
EMS_TOT_2 _
EMS_HH_22_2+EMS_SEC_22_01_2+EMS_SEC_22_02_2+EMS_SEC_22_03_2+EMS_SEC_22_04_2+EMS_SEC_22_05_2+EMS_SEC_22_06_2+EMS_SEC_22_07_2+EMS_SEC_22_08_2+EMS_SEC_22_09_2+EMS_SEC_22_12_2+EMS_SEC_22_13_2+EMS_SEC_22_14_2+EMS_SEC_22_15_2+EMS_SEC_22_16_2+EMS_SEC_22_17_2+EMS_SEC_22_18_2+EMS_SEC_22_19_2+EMS_SEC_22_20_2 EMS_HH_24_2+EMS_SEC_2401_2+EMS_SEC_24_01_2+EMS_SEC_24_02_2+EMS_SEC_24_03_2+EMS_SEC_24_04_2+EMS_SEC_24_05_2+EMS_SEC_24_06_2+EMS_SEC_24_07_2+EMS_SEC_24_08_2+EMS_SEC_24_09_2+EMS_SEC_24_10_2+EMS_SEC_24_11_2+EMS_SEC_24_12_2+EMS_SEC_24_13_2+EMS_SEC_24_14_2+EMS_SEC_24_15_2+EMS_SEC_24_16_2+EMS_SEC_24_17_2+EMS_SEC_24_18_2+EMS_SEC_24_19_2+EMS_SEC_24_20_2 _
       EMS_SEC_2302_2+EMS_SEC_2303_2+EMS_SEC_2304_2 EMS_HH_21_2+EMS_SEC_21_05_2+EMS_SEC_21_06_2+EMS_SEC_21_07_2+EMS_SEC_21_08_2+EMS_SEC_21_10_2+EMS_SEC_21_12_2+EMS_SEC_21_19_2+EMS_SEC_21_20_2 EMS_DC_05_2+EMS_DC_04_2 EMS_HH_21_2+EMS_HH_24_2+EMS_HH_22_2*(@ELEM(PENER_BUIL_H01_22,2006)*ENER_BUIL_H01_22_2)/EXP_22_H01_2 _
       EMS_HH_22_2*(EXP_AUTO_H01_22_2/EXP_22_H01_2) _
       EMS_SEC_04_2+EMS_SEC_05_2+EMS_SEC_06_2+EMS_SEC_07_2+EMS_SEC_08_2+EMS_SEC_09_2+EMS_SEC_10_2+EMS_SEC_18_2+EMS_SEC_21_19_2+EMS_SEC_21_20_2+EMS_SEC_2201_2+EMS_SEC_2302_2+EMS_SEC_2303_2+EMS_SEC_2304_2+EMS_SEC_2401_2+EMS_DC_04_2+EMS_DC_05_2 _
        EMS_SEC_01_2+EMS_SEC_02_2+EMS_SEC_03_2+EMS_SEC_11_2+EMS_SEC_12_2+EMS_SEC_13_2 _             
        EMS_SEC_14_2+EMS_SEC_15_2+EMS_SEC_16_2+EMS_SEC_17_2+EMS_SEC_19_2+EMS_SEC_20_2 _               
        EMS_TOT_1 EMS_HH_22_1+EMS_SEC_22_01_1+EMS_SEC_22_02_1+EMS_SEC_22_03_1+EMS_SEC_22_04_1+EMS_SEC_22_05_1+EMS_SEC_22_06_1+EMS_SEC_22_07_1+EMS_SEC_22_08_1+EMS_SEC_22_09_1+EMS_SEC_22_12_1+EMS_SEC_22_13_1+EMS_SEC_22_14_1+EMS_SEC_22_15_1+EMS_SEC_22_16_1+EMS_SEC_22_17_1+EMS_SEC_22_18_1+EMS_SEC_22_19_1+EMS_SEC_22_20_1 EMS_HH_24_1+EMS_SEC_2401_1+EMS_SEC_24_01_1+EMS_SEC_24_02_1+EMS_SEC_24_03_1+EMS_SEC_24_04_1+EMS_SEC_24_05_1+EMS_SEC_24_06_1+EMS_SEC_24_07_1+EMS_SEC_24_08_1+EMS_SEC_24_09_1+EMS_SEC_24_10_1+EMS_SEC_24_11_1+EMS_SEC_24_12_1+EMS_SEC_24_13_1+EMS_SEC_24_14_1+EMS_SEC_24_15_1+EMS_SEC_24_16_1+EMS_SEC_24_17_1+EMS_SEC_24_18_1+EMS_SEC_24_19_1+EMS_SEC_24_20_1 _
        EMS_SEC_2302_1+EMS_SEC_2303_1+EMS_SEC_2304_1 EMS_HH_21_1+EMS_SEC_21_05_1+EMS_SEC_21_06_1+EMS_SEC_21_07_1+EMS_SEC_21_08_1+EMS_SEC_21_10_1+EMS_SEC_21_12_1+EMS_SEC_21_19_1+EMS_SEC_21_20_1 _
       EMS_DC_05_1+EMS_DC_04_1 _
       EMS_HH_21_1+EMS_HH_24_1+EMS_HH_22_1*(@ELEM(PENER_BUIL_H01_22,2006)*ENER_BUIL_H01_22_1)/EXP_22_H01_1 _
       EMS_HH_22_1*(EXP_AUTO_H01_22_1/EXP_22_H01_1) _
       EMS_SEC_04_1+EMS_SEC_05_1+EMS_SEC_06_1+EMS_SEC_07_1+EMS_SEC_08_1+EMS_SEC_09_1+EMS_SEC_10_1+EMS_SEC_18_1+EMS_SEC_21_19_1+EMS_SEC_21_20_1+EMS_SEC_2201_1+EMS_SEC_2302_1+EMS_SEC_2303_1+EMS_SEC_2304_1+EMS_SEC_2401_1+EMS_DC_04_1+EMS_DC_05_1 _
       EMS_SEC_01_1+EMS_SEC_02_1+EMS_SEC_03_1+EMS_SEC_11_1+EMS_SEC_12_1+EMS_SEC_13_1 _
       EMS_SEC_14_1+EMS_SEC_15_1+EMS_SEC_16_1+EMS_SEC_17_1+EMS_SEC_19_1+EMS_SEC_20_1 ER_TOTAL_2 _
       ER_TRANS_PRIVATE_2+ER_TRANS_PUBLIC_2 ER_TRANS_PRIVATE_2 ER_TRANS_PRIVATE_OIL_2 ER_TRANS_PRIVATE_ELEC_2 ER_TRANS_PUBLIC_2 ER_TRANS_PUBLIC_OIL_2 _
       ER_TRANS_PUBLIC_ELEC_2 ER_RESIDENTIAL_2 ER_RESIDENTIAL_OIL_2 ER_RESIDENTIAL_ELEC_2 ER_RESIDENTIAL_GAS_2 ER_AGRICULTURE_2 ER_AGRICULTURE_OIL_2 _
       ER_AGRICULTURE_ELEC_2 ER_INDUS_2 ER_INDUS_OIL_2 ER_INDUS_ELEC_2 ER_INDUS_GAS_2 ER_INDUS_COAL_2 ER_TERTIARY_2 ER_TERTIARY_OIL_2 ER_TERTIARY_ELEC_2 ER_TERTIARY_GAS_2 ER_OIL_2 ER_GAS_2 ER_COAL_2 ER_ELEC_2 ER_ELEC_2301_2 ER_ELEC_2302_2 ER_ELEC_2303_2 _
        ER_ELEC_2304_2 ER_ELEC_2305_2 ER_ELEC_2306_2 ER_ELEC_2307_2 ER_ELEC_2308_2 ER_TOTAL_1 ER_TRANS_PRIVATE_1+ER_TRANS_PUBLIC_1 ER_TRANS_PRIVATE_1 ER_TRANS_PRIVATE_OIL_1 ER_TRANS_PRIVATE_ELEC_1 ER_TRANS_PUBLIC_1 ER_TRANS_PUBLIC_OIL_1 ER_TRANS_PUBLIC_ELEC_1 ER_RESIDENTIAL_1 ER_RESIDENTIAL_OIL_1 ER_RESIDENTIAL_ELEC_1 ER_RESIDENTIAL_GAS_1 ER_AGRICULTURE_1 ER_AGRICULTURE_OIL_1 ER_AGRICULTURE_ELEC_1 ER_INDUS_1 ER_INDUS_OIL_1 ER_INDUS_ELEC_1 ER_INDUS_GAS_1 ER_INDUS_COAL_1 ER_TERTIARY_1 ER_TERTIARY_OIL_1 ER_TERTIARY_ELEC_1 ER_TERTIARY_GAS_1 ER_OIL_1 _
        ER_GAS_1 ER_COAL_1 ER_ELEC_1 ER_ELEC_2301_1 ER_ELEC_2302_1 ER_ELEC_2303_1 ER_ELEC_2304_1 ER_ELEC_2305_1 ER_ELEC_2306_1 ER_ELEC_2307_1 ER_ELEC_2308_1 ENER_H01_2 _
        @ELEM(PENER_BUIL_H01,2006)*ENER_BUIL_H01_2 EXP_AUTO_H01_2 ENER_H01_1 @ELEM(PENER_BUIL_H01,2006)*ENER_BUIL_H01_1 EXP_AUTO_H01_1 KM_auto_H01_2 _
        KM_TRAVELER_18_H01_2 KM_TRAV_AUTO_LD_H01_2 KM_TRAV_AUTO_CD_H01_2 KM_TRAVELER_14_H01_2 KM_TRAVELER_15_H01_2 KM_TRAVELER_CD_H01_2 KM_TRAVELER_LD_H01_2 _
        KM_AUTO_H01_1 KM_TRAVELER_18_H01_1 KM_TRAV_AUTO_LD_H01_1 KM_TRAV_AUTO_CD_H01_1 KM_TRAVELER_14_H01_1 _
        KM_TRAVELER_15_H01_1 KM_TRAVELER_CD_H01_1 KM_TRAVELER_LD_H01_1 _
        ER_Auto_2 ER_AUTO_th_A_2 ER_AUTO_th_B_2 ER_AUTO_th_C_2 ER_AUTO_th_D_2 ER_AUTO_th_E_2 ER_AUTO_th_F_2 ER_AUTO_th_G_2 _
        ER_AUTO_elec_A_2 ER_AUTO_elec_B_2 ER_AUTO_elec_C_2 ER_AUTO_elec_D_2 ER_AUTO_elec_E_2 ER_AUTO_elec_F_2 ER_AUTO_elec_G_2 ER_Auto_Coal_2 ER_auto_th_2 ER_Auto_Elec_2 ER_Auto_Gas_2 _
        ER_NEWAUTO_2 ER_NEWAUTO_th_2 ER_newauto_th_A_2 ER_newauto_th_B_2 ER_newauto_th_C_2 ER_newauto_th_D_2 ER_newauto_th_E_2 ER_newauto_th_F_2  ER_newauto_th_G_2  _
        ER_NEWAUTO_elec_2 ER_NEWAUTO_elec_A_2 ER_NEWAUTO_elec_B_2 ER_NEWAUTO_elec_C_2 ER_NEWAUTO_elec_D_2 ER_NEWAUTO_elec_E_2 ER_NEWAUTO_elec_F_2 ER_NEWAUTO_elec_G_2 _
        ER_Auto_1 ER_AUTO_th_A_1 ER_AUTO_th_B_1 ER_AUTO_th_C_1 ER_AUTO_th_D_1 ER_AUTO_th_E_1 ER_AUTO_th_F_1 ER_AUTO_th_G_1 ER_AUTO_elec_A_1 ER_AUTO_elec_B_1 ER_AUTO_elec_C_1 ER_AUTO_elec_D_1 ER_AUTO_elec_E_1 ER_AUTO_elec_F_1 ER_AUTO_elec_G_1 ER_Auto_Coal_1 ER_auto_th_1 ER_Auto_Elec_1 ER_Auto_Gas_1 _
        ER_NEWAUTO_1 ER_NEWAUTO_th_1 ER_newauto_th_A_1 ER_newauto_th_B_1 ER_newauto_th_C_1 ER_newauto_th_D_1 ER_newauto_th_E_1 ER_newauto_th_F_1 _
        ER_NEWAUTO_elec_1 ER_NEWAUTO_elec_A_1 ER_NEWAUTO_elec_B_1 ER_NEWAUTO_elec_C_1 ER_NEWAUTO_elec_D_1 ER_NEWAUTO_elec_E_1 ER_NEWAUTO_elec_F_1 _
        ER_BUIL_2 ER_BUIL_A_2 ER_BUIL_B_2 ER_BUIL_C_2 ER_BUIL_D_2 ER_BUIL_E_2 ER_BUIL_F_2 ER_BUIL_G_2  SUB_REHAB_VAL_2 tax_cr_19_2 ER_BUIL_1 _
        ER_BUIL_A_1 ER_BUIL_B_1 ER_BUIL_C_1 ER_BUIL_D_1 ER_BUIL_E_1 ER_BUIL_F_1 ER_BUIL_G_1 SUB_REHAB_VAL_1 tax_cr_19_1 _
	  pch_2 pch_1 pgdp_2 pgdp_1 gdp_2 gdp_1 _
       L_01_1	L_02_1	L_03_1	L_04_1	L_05_1	L_06_1	L_07_1	L_08_1	L_09_1	L_10_1	L_11_1 L_12_1 L_13_1 L_14_1 L_15_1	L_16_1	L_17_1	L_18_1	L_19_1	L_20_1	L_21_1 _
      L_2201_1	L_2202_1	L_2301_1	L_2302_1	L_2303_1	L_2304_1	L_2305_1	L_2306_1	L_2307_1 L_2308_1 L_2401_1 L_2402_1 L_2403_1 L_2404_1 L_2405_1 _
	 L_2406_1	L_01_2	L_02_2	L_03_2	L_04_2	L_05_2	L_06_2	L_07_2	L_08_2	L_09_2	L_10_2	L_11_2	L_12_2	L_13_2	L_14_2	L_15_2	L_16_2	L_17_2	L_18_2	L_19_2	L_20_2	L_21_2 _ 
      L_2201_2	L_2202_2	L_2301_2	L_2302_2	L_2303_2	L_2304_2	L_2305_2	L_2306_2	L_2307_2 L_2308_2 _
      L_2401_2	L_2402_2	L_2403_2	L_2404_2	L_2405_2	L_2406_2 _ 	
      VA_01_1	VA_02_1	VA_03_1	VA_04_1	VA_05_1	VA_06_1	VA_07_1	VA_08_1	VA_09_1 VA_10_1 VA_11_1 VA_12_1 VA_13_1 VA_14_1 VA_15_1 VA_16_1 VA_17_1 VA_18_1 VA_19_1 VA_20_1 VA_21_1 _
      VA_2201_1 VA_2202_1 VA_2301_1 VA_2302_1 VA_2303_1 VA_2304_1	VA_2305_1 VA_2306_1 VA_2307_1 VA_2308_1 _
      VA_2401_1 VA_2402_1 VA_2403_1	 VA_2404_1 VA_2405_1 VA_2406_1 _	
      VA_01_2	VA_02_2	VA_03_2 VA_04_2 VA_05_2 VA_06_2 VA_07_2 VA_08_2 VA_09_2 VA_10_2 VA_11_2 VA_12_2 VA_13_2 VA_14_2 VA_15_2 VA_16_2 VA_17_2 VA_18_2 VA_19_2 VA_20_2 VA_21_2 _
      VA_2201_2 VA_2202_2 VA_2301_2 VA_2302_2 VA_2303_2 VA_2304_2	VA_2305_2	 VA_2306_2 VA_2307_2 VA_2308_2 _
      VA_2401_2 VA_2402_2 VA_2403_2 VA_2404_2 VA_2405_2 VA_2406_2 _
TTCO_VOL_21_2 TTCO_VOL_22_2 TTCO_VOL_24_2 TTCO_VOL_21_1 TTCO_VOL_22_1 TTCO_VOL_24_1 DISPINC_VAL_2 DISPINC_VAL_1 _
(gdpter_2/gdpbis_2-1)*100 (gdpter_1/gdpbis_1-1)*100 _
IA_1 IA_01_1 IA_02_1 IA_03_1 IA_04_1 IA_05_1 IA_06_1 IA_07_1 IA_08_1 IA_09_1 IA_10_1 IA_11_1 IA_12_1 IA_13_1 IA_13_20_1 IA_14_1 IA_15_1  IA_16_1 IA_17_1  IA_18_1 IA_19_1 IA_20_1 IA_21_1 _
IA_2201_1 IA_2202_1 IA_2301_1 IA_2302_1 IA_2303_1 IA_2304_1 IA_2305_1 IA_2306_1 IA_2307_1 IA_2308_1 IA_2401_1 IA_2402_1 IA_2403_1 IA_2404_1 IA_2405_1 IA_2406_1   _
REHAB_VAL_1 NEWBUIL_H01_1 PNEWBUIL_H01_1 PREHAB_H01_1 REHAB_H01_1 PEXP_13_H01_1 CH_03_1 PGDP_1 _
IA_2 IA_01_2 IA_02_2 IA_03_2 IA_04_2 IA_05_2 IA_06_2 IA_07_2 IA_08_2 IA_09_2 IA_10_2 IA_11_2 IA_12_2 IA_13_2 IA_13_20_2 IA_14_2 IA_15_2 IA_16_2 IA_17_2 IA_18_2 IA_19_2 IA_20_2 IA_21_2 _
IA_2201_2 IA_2202_2 IA_2301_2 IA_2302_2 IA_2303_2 IA_2304_2 IA_2305_2 IA_2306_2 IA_2307_2 IA_2308_2 IA_2401_2 IA_2402_2 IA_2403_2 IA_2404_2 IA_2405_2 IA_2406_2  _
REHAB_VAL_2 NEWBUIL_H01_2 PNEWBUIL_H01_2 PREHAB_H01_2 REHAB_H01_2 PEXP_13_H01_2 CH_03_2 PGDP_2 _
CH_01_2 CH_02_2 CH_03_2 CH_04_2 CH_05_2 CH_06_2 CH_07_2 CH_08_2 CH_09_2 CH_10 CH_11_2 CH_12_2 CH_13_2 CH_14_2 _
CH_15_2 CH_16_2 CH_17_2 CH_18_2 CH_19_2 CH_20_2 CH_21_2 CH_22_2 CH_23_2 CH_24_2 _ 
CH_01_1 CH_02_1 CH_03_1 CH_04_1 CH_05_1 CH_06_1 CH_07_1 CH_08_1 CH_09_1 CH_10 CH_11_1 CH_12_1 CH_13_1 CH_14_1 _
CH_15_1 CH_16_1 CH_17_1 CH_18_1 CH_19_1 CH_20_1 CH_21_1 CH_22_1 CH_23_1 CH_24_1 _
PCH_01_2 PCH_02_2 PCH_03_2 PCH_04_2 PCH_05_2 PCH_06_2 PCH_07_2 PCH_08_2 PCH_09_2 PCH_10 _
PCH_11_2 PCH_12_2 PCH_13_2 PCH_14_2 PCH_15_2 PCH_16_2 PCH_17_2 PCH_18_2 PCH_19_2 PCH_20_2 _
PCH_21_2 PCH_22_2 PCH_23_2 PCH_24_2 PCH_01_1 PCH_02_1 PCH_03_1 PCH_04_1 PCH_05_1 PCH_06_1 _ 
PCH_07_1 PCH_08_1 PCH_09_1 PCH_10 PCH_11_1 PCH_12_1 PCH_13_1 PCH_14_1 PCH_15_1 PCH_16_1 _ 
PCH_17_1 PCH_18_1 PCH_19_1 PCH_20_1 PCH_21_1 PCH_22_1 PCH_23_1 PCH_24_1 PENER_BUIL_H01_2 PENER_BUIL_H01_1 ENER_BUIL_H01_2 ENER_BUIL_H01_1 pexp_auto_h01_2 pexp_auto_h01_1 _
exp_mobauto_val_h01_2 exp_mobauto_val_h01_1 exp_housing_val_h01_2 exp_housing_val_h01_1 pop_tot _
I_MDE_01_1 I_MDE_02_1 I_MDE_03_1 I_MDE_04_1	I_MDE_05_1 I_MDE_06_1 I_MDE_07_1 I_MDE_08_1	I_MDE_09_1 I_MDE_10_1 I_MDE_11_1 _
 I_MDE_12_1 I_MDE_13_1 I_MDE_19_1 I_MDE_20_1 I_MDE_01_2 I_MDE_02_2 I_MDE_03_2 I_MDE_04_2 I_MDE_05_2 I_MDE_06_2 I_MDE_07_2 I_MDE_08_2 I_MDE_09_2 I_MDE_10_2 I_MDE_11_2 I_MDE_12_2 I_MDE_13_2  _
I_MDE_19_2 I_MDE_20_2 NDI_ADEB_VOL_1 NDI_ADEB_VOL_2 INV_Road PIA_13_19_2 PIA_13_19_1 PIA_13_19_1 PIA_13_20_2 PIA_20_2 PIA_20_1 PIA_15_2 CSPE_1 CSPE_2 _
Carbon_foot_1 Carbon_foot_2 PSUBD_15_2*SUBD_15_2 CU_MWH_PGDP_21_1 CU_MWH_PGDP_22_1 CU_MWH_PGDP_23_1  CU_MWH_PGDP_24_1 CU_MWH_PGDP_2201_1 CU_MWH_PGDP_2202_1 CU_MWH_PGDP_2301_1 _
CU_MWH_PGDP_2302_1 CU_MWH_PGDP_2303_1 CU_MWH_PGDP_2304_1 CU_MWH_PGDP_2305_1 CU_MWH_PGDP_2306_1 CU_MWH_PGDP_2307_1 CU_MWH_PGDP_2308_1 CU_MWH_PGDP_2401_1 CU_MWH_PGDP_2402_1 _
CU_MWH_PGDP_2403_1 CU_MWH_PGDP_2404_1 CU_MWH_PGDP_2405_1 CU_MWH_PGDP_2406_1 CU_MWH_PGDP_21_2 CU_MWH_PGDP_22_2 CU_MWH_PGDP_23_2  CU_MWH_PGDP_24_2 CU_MWH_PGDP_2201_2 _
CU_MWH_PGDP_2202_2 CU_MWH_PGDP_2301_2 CU_MWH_PGDP_2302_2 CU_MWH_PGDP_2303_2 CU_MWH_PGDP_2304_2 CU_MWH_PGDP_2305_2 CU_MWH_PGDP_2306_2 CU_MWH_PGDP_2307_2 CU_MWH_PGDP_2308_2 _
CU_MWH_PGDP_2401_2 CU_MWH_PGDP_2402_2 CU_MWH_PGDP_2403_2 CU_MWH_PGDP_2404_2 CU_MWH_PGDP_2405_2 CU_MWH_PGDP_2406_2 UNR_TOT_1 UNR_TOT_2 _
GDP_1 CH_1 G_1 I_1+CH_13_1 I_1-IA_20_1 CH_13_1 IA_20_1 X_1 M_1 DISPINC_VAL_1/(POP_TOT*PCH_1) L_1 SP_G_VAL_1/PGDP_1 DEBT_G_VAL_1/PGDP_1 GDP_2 CH_2 G_2 I_2+CH_13_2 I_2-IA_20_2 CH_13_2 IA_20_2 X_2 M_2 DISPINC_VAL_2/(POP_TOT*PCH_2) L_2 SP_G_VAL_2/PGDP_2 DEBT_G_VAL_2/PGDP_2 _
Exp_13_1 Exp_13_2 exp_newauto_val_h01_ca_1 exp_newauto_val_h01_cb_1 exp_newauto_val_h01_cc_1 exp_newauto_val_h01_cd_1 exp_newauto_val_h01_ce_1 exp_newauto_val_h01_cf_1 exp_newauto_val_h01_cg_1 exp_newauto_val_h01_ca_2 exp_newauto_val_h01_cb_2 exp_newauto_val_h01_cc_2 _
exp_newauto_val_h01_cd_2 exp_newauto_val_h01_ce_2 exp_newauto_val_h01_cf_2 exp_newauto_val_h01_cg_2 100*(ts_2-ts_1) _
100*((PY_2*Y_2/(Y_2-Y_20_2)-PY_20_2*Y_20_2/(Y_2-Y_20_2))/(PY_1*Y_1/(Y_1-Y_20_1)-PY_20_1*Y_20_1/(Y_1-Y_20_1))-1) _
100*((PVA_2*VA_2/(VA_2-VA_20_2)-PVA_20_2*VA_20_2/(VA_2-VA_20_2))/(PVA_1*VA_1/(VA_1-VA_20_1)-PVA_20_1*VA_20_1/(VA_1-VA_20_1))-1) _

100*(DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL_1/(PGDP_1*GDP_1)) _
100*((DEBT_NewB_Val_H01_CA_2+DEBT_NewB_Val_H01_CB_2+DEBT_NewB_Val_H01_CC_2+DEBT_NewB_Val_H01_CD_2+DEBT_NewB_Val_H01_CE_2+DEBT_NewB_Val_H01_CF_2+DEBT_NewB_Val_H01_CG_2+DEBT_REHAB_Val_H01_CA_2+DEBT_REHAB_Val_H01_CB_2+DEBT_REHAB_Val_H01_CC_2+DEBT_REHAB_Val_H01_CD_2+DEBT_REHAB_Val_H01_CE_2+DEBT_REHAB_Val_H01_CF_2+DEBT_REHAB_Val_H01_CG_2+DEBT_auto_Val_H01_CA_2+DEBT_auto_Val_H01_CB_2+DEBT_auto_Val_H01_CC_2+DEBT_auto_Val_H01_CD_2+DEBT_auto_Val_H01_CE_2+DEBT_auto_Val_H01_CF_2+DEBT_auto_Val_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_NewB_Val_H01_CA_1+DEBT_NewB_Val_H01_CB_1+DEBT_NewB_Val_H01_CC_1+DEBT_NewB_Val_H01_CD_1+DEBT_NewB_Val_H01_CE_1+DEBT_NewB_Val_H01_CF_1+DEBT_NewB_Val_H01_CG_1+DEBT_REHAB_Val_H01_CA_1+DEBT_REHAB_Val_H01_CB_1+DEBT_REHAB_Val_H01_CC_1+DEBT_REHAB_Val_H01_CD_1+DEBT_REHAB_Val_H01_CE_1+DEBT_REHAB_Val_H01_CF_1+DEBT_REHAB_Val_H01_CG_1+DEBT_auto_Val_H01_CA_1+DEBT_auto_Val_H01_CB_1+DEBT_auto_Val_H01_CC_1+DEBT_auto_Val_H01_CD_1+DEBT_auto_Val_H01_CE_1+DEBT_auto_Val_H01_CF_1+DEBT_auto_Val_H01_CG_1)/(PGDP_1*GDP_1)) _

100*((DEBT_NewB_Val_H01_CA_2+DEBT_NewB_Val_H01_CB_2+DEBT_NewB_Val_H01_CC_2+DEBT_NewB_Val_H01_CD_2+DEBT_NewB_Val_H01_CE_2+DEBT_NewB_Val_H01_CF_2+DEBT_NewB_Val_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_NewB_Val_H01_CA_1+DEBT_NewB_Val_H01_CB_1+DEBT_NewB_Val_H01_CC_1+DEBT_NewB_Val_H01_CD_1+DEBT_NewB_Val_H01_CE_1+DEBT_NewB_Val_H01_CF_1+DEBT_NewB_Val_H01_CG_1)/(PGDP_1*GDP_1)) _

100*((DEBT_REHAB_Val_H01_CA_2+DEBT_REHAB_Val_H01_CB_2+DEBT_REHAB_Val_H01_CC_2+DEBT_REHAB_Val_H01_CD_2+DEBT_REHAB_Val_H01_CE_2+DEBT_REHAB_Val_H01_CF_2+DEBT_REHAB_Val_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_REHAB_Val_H01_CA_1+DEBT_REHAB_Val_H01_CB_1+DEBT_REHAB_Val_H01_CC_1+DEBT_REHAB_Val_H01_CD_1+DEBT_REHAB_Val_H01_CE_1+DEBT_REHAB_Val_H01_CF_1+DEBT_REHAB_Val_H01_CG_1)/(PGDP_1*GDP_1)) _

100*((DEBT_auto_Val_H01_CA_2+DEBT_auto_Val_H01_CB_2+DEBT_auto_Val_H01_CC_2+DEBT_auto_Val_H01_CD_2+DEBT_auto_Val_H01_CE_2+DEBT_auto_Val_H01_CF_2+DEBT_auto_Val_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_auto_Val_H01_CA_1+DEBT_auto_Val_H01_CB_1+DEBT_auto_Val_H01_CC_1+DEBT_auto_Val_H01_CD_1+DEBT_auto_Val_H01_CE_1+DEBT_auto_Val_H01_CF_1+DEBT_auto_Val_H01_CG_1)/(PGDP_1*GDP_1)) _

L_20_2-L_20_1 _
100*((L_20_2/L_20_1)-1) _

(L_2-L_20_2)-(L_1-L_20_1) _
100*((L_s_2)/(L_s_1)-1) _

L_s_2-L_s_1 _
100*((L_s_20_2/L_s_20_1)-1) _

L_s_20_2-L_s_20_1 _
100*((L_s_20_2/L_s_20_1)-1) _

(L_s_2-L_s_20_2)-(L_s_1-L_s_20_1) _
100*(((L_s_2-L_s_20_2)/(L_s_1-L_s_20_1))-1) _ 

100*(((PE_sec_2*E_sec_2/(E_sec_2-Ener_20_2)-PEner_20_2*Ener_20_2/(E_sec_2-Ener_20_2))*(E_sec_2-Ener_20_2)/(MAT_2-MAT_20_2+E_sec_2-Ener_20_2)+(PMAT_2*MAT_2/(MAT_2-MAT_20_2)-PMAT_20_2*MAT_20_2/(MAT_2-MAT_20_2))*(MAT_2-MAT_20_2)/(MAT_2-MAT_20_2+E_sec_2-Ener_20_2))/((PE_sec_1*E_sec_1/(E_sec_1-Ener_20_1)-PEner_20_1*Ener_20_1/(E_sec_1-Ener_20_1))*(E_sec_2-Ener_20_2)/(MAT_1-MAT_20_1+E_sec_1-Ener_20_1)+(PMAT_1*MAT_1/(MAT_1-MAT_20_1)-PMAT_20_1*MAT_20_1/(MAT_1-MAT_20_1))*(MAT_1-MAT_20_1)/(MAT_1-MAT_20_1+E_sec_1-Ener_20_1))-1) _

100*((W_SPB_2/Y_2)/(W_SPB_1/Y_1)-1) _
100*((cl_bis_2/pva_2)/(cl_bis_1/pva_1)-1) _

100*((DEBT_NewB_Val_tot_H01_CA_2+DEBT_NewB_Val_tot_H01_CB_2+DEBT_NewB_Val_tot_H01_CC_2+DEBT_NewB_Val_tot_H01_CD_2+DEBT_NewB_Val_tot_H01_CE_2+DEBT_NewB_Val_tot_H01_CF_2+DEBT_NewB_Val_tot_H01_CG_2+DEBT_REHAB_Val_tot_H01_CA_2+DEBT_REHAB_Val_tot_H01_CB_2+DEBT_REHAB_Val_tot_H01_CC_2+DEBT_REHAB_Val_tot_H01_CD_2+DEBT_REHAB_Val_tot_H01_CE_2+DEBT_REHAB_Val_tot_H01_CF_2+DEBT_REHAB_Val_tot_H01_CG_2+DEBT_auto_Val_tot_H01_CA_2+DEBT_auto_Val_tot_H01_CB_2+DEBT_auto_Val_tot_H01_CC_2+DEBT_auto_Val_tot_H01_CD_2+DEBT_auto_Val_tot_H01_CE_2+DEBT_auto_Val_tot_H01_CF_2+DEBT_auto_Val_tot_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_NewB_Val_tot_H01_CA_1+DEBT_NewB_Val_tot_H01_CB_1+DEBT_NewB_Val_tot_H01_CC_1+DEBT_NewB_Val_tot_H01_CD_1+DEBT_NewB_Val_tot_H01_CE_1+DEBT_NewB_Val_tot_H01_CF_1+DEBT_NewB_Val_tot_H01_CG_1+DEBT_REHAB_Val_tot_H01_CA_1+DEBT_REHAB_Val_tot_H01_CB_1+DEBT_REHAB_Val_tot_H01_CC_1+DEBT_REHAB_Val_tot_H01_CD_1+DEBT_REHAB_Val_tot_H01_CE_1+DEBT_REHAB_Val_tot_H01_CF_1+DEBT_REHAB_Val_tot_H01_CG_1+DEBT_auto_Val_tot_H01_CA_1+DEBT_auto_Val_tot_H01_CB_1+DEBT_auto_Val_tot_H01_CC_1+DEBT_auto_Val_tot_H01_CD_1+DEBT_auto_Val_tot_H01_CE_1+DEBT_auto_Val_tot_H01_CF_1+DEBT_auto_Val_tot_H01_CG_1)/(PGDP_1*GDP_1)) _

100*((DEBT_NewB_Val_tot_H01_CA_2+DEBT_NewB_Val_tot_H01_CB_2+DEBT_NewB_Val_tot_H01_CC_2+DEBT_NewB_Val_tot_H01_CD_2+DEBT_NewB_Val_tot_H01_CE_2+DEBT_NewB_Val_tot_H01_CF_2+DEBT_NewB_Val_tot_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_NewB_Val_tot_H01_CA_1+DEBT_NewB_Val_tot_H01_CB_1+DEBT_NewB_Val_tot_H01_CC_1+DEBT_NewB_Val_tot_H01_CD_1+DEBT_NewB_Val_tot_H01_CE_1+DEBT_NewB_Val_tot_H01_CF_1+DEBT_NewB_Val_tot_H01_CG_1)/(PGDP_1*GDP_1)) _

100*((DEBT_REHAB_Val_tot_H01_CA_2+DEBT_REHAB_Val_tot_H01_CB_2+DEBT_REHAB_Val_tot_H01_CC_2+DEBT_REHAB_Val_tot_H01_CD_2+DEBT_REHAB_Val_tot_H01_CE_2+DEBT_REHAB_Val_tot_H01_CF_2+DEBT_REHAB_Val_tot_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_REHAB_Val_tot_H01_CA_1+DEBT_REHAB_Val_tot_H01_CB_1+DEBT_REHAB_Val_tot_H01_CC_1+DEBT_REHAB_Val_tot_H01_CD_1+DEBT_REHAB_Val_tot_H01_CE_1+DEBT_REHAB_Val_tot_H01_CF_1+DEBT_REHAB_Val_tot_H01_CG_1)/(PGDP_1*GDP_1)) _

100*((DEBT_auto_Val_tot_H01_CA_2+DEBT_auto_Val_tot_H01_CB_2+DEBT_auto_Val_tot_H01_CC_2+DEBT_auto_Val_tot_H01_CD_2+DEBT_auto_Val_tot_H01_CE_2+DEBT_auto_Val_tot_H01_CF_2+DEBT_auto_Val_tot_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_auto_Val_tot_H01_CA_1+DEBT_auto_Val_tot_H01_CB_1+DEBT_auto_Val_tot_H01_CC_1+DEBT_auto_Val_tot_H01_CD_1+DEBT_auto_Val_tot_H01_CE_1+DEBT_auto_Val_tot_H01_CF_1+DEBT_auto_Val_tot_H01_CG_1)/(PGDP_1*GDP_1)) _

REC_VAL_1 DIV_GOV_VAL_1 IR_VAL_1 AIC_VAL_1 -CL_S_20_1*L_S_20_1*PROG_L_20_1 PY_20_1*Y_20_1 PTAX_1*TAX_1 PIY_1*IY_1 PIS_1*IS_1 PCSE_TOT_1*CSE_TOT_1 PCSS_TOT_1*CSS_TOT_1 -(PE_20_1*E_20_1) -(PMAT_20_1*MAT_20_1) -(PIY_20_1*IY_20_1) REC_VAL_2 DIV_GOV_VAL_2 IR_VAL_2 AIC_VAL_2 -CL_S_20_2*L_S_20_2*PROG_L_20_2 PY_20_2*Y_20_2 PTAX_2*TAX_2 PIY_2*IY_2 PIS_2*IS_2 PCSE_TOT_2*CSE_TOT_2 PCSS_TOT_2*CSS_TOT_2 -(PE_20_2*E_20_2) -(PMAT_20_2*MAT_20_2) -(PIY_20_2*IY_20_2) DEP_VAL_1 CL_S_20_1*L_S_20_1*PROG_L_20_1 R_G_1(-1)*DEBT_G_VAL_1(-1) PRESOC_VAL_1 SUB_RENOV_VAL_1 SUB_AUTO_VAL_1 PE_20_1*E_20_1 PMAT_20_1*MAT_20_1 PIY_20_1*IY_20_1 PIA_20_1*IA_20_1 PG_1*G_1-PG_20_1*G_20_1 -(PSUB_1*SUB_1-PSUB_01_1*SUB_01_1) -(PSY_1*SY_1-PSY_01_1*SY_01_1) DEP_VAL_2 CL_S_20_2*L_S_20_2*PROG_L_20_2 R_G_2(-1)*DEBT_G_VAL_2(-1) PRESOC_VAL_2 SUB_RENOV_VAL_2 SUB_AUTO_VAL_2 PE_20_2*E_20_2 PMAT_20_2*MAT_20_2 PIY_20_2*IY_20_2 PIA_20_2*IA_20_2 PG_2*G_2-PG_20_2*G_20_2 -(PSUB_2*SUB_2-PSUB_01_2*SUB_01_2) -(PSY_2*SY_2-PSY_01_2*SY_01_2) PGDP_1 PGDP_2 GDP_1 GDP_2 INC_GOV_OTH_NET REDIS_VAL_H _

100*(DEBT_SNF_VAL_2/(PGDP_2*GDP_2)-DEBT_SNF_VAL_1/(PGDP_1*GDP_1)) _
100*(DEBT_SNF_ENER_2/(PGDP_2*GDP_2)-DEBT_SNF_ENER_1/(PGDP_1*GDP_1)) _
100*(DEBT_SNF_VAL_2/DEBT_SNF_VAL_1-1) _
100*(DEBT_SNF_ENER_2/DEBT_SNF_ENER_1-1) _

PRESOC_DOM_Oth_VAL_1 PRESOC_DOM_Oth_VAL_2 PRESOC_DOM_U_VAL_1 PRESOC_DOM_U_VAL_2 PRESOC_ROW_VAL (-PG_20_1*G_20_1) (-PG_20_2*G_20_2) (PG_1*G_1) (PG_2*G_2) REC_VAL_SEC_ETS_2   REC_VAL_SEC_ETS2_2   REC_VAL_ETS2_HH_2   REC_VAL_TCO_HH_2  REC_VAL_SEC_TCO_2 REC_VAL_SEC_ETS_1   REC_VAL_SEC_ETS2_1   REC_VAL_ETS2_HH_1   REC_VAL_TCO_HH_1  REC_VAL_SEC_TCO_1 REDIS_VAL_TCO_H_2  REDIS_VAL_SEC_TCO_2   REDIS_VAL_SEC_ETS_2  REDIS_VAL_ETS2_H_2   REDIS_VAL_SEC_ETS2_2  REDIS_VAL_TCO_H_1  REDIS_VAL_SEC_TCO_1   REDIS_VAL_SEC_ETS_1  REDIS_VAL_ETS2_H_1   REDIS_VAL_SEC_ETS2_1 100*(EMS_tot_2/EMS_tot_1-1) 100*(EMS_hh_2/EMS_hh_1-1)  100*((EMS_SEC_tot_2  + EMS_DC_2)/(EMS_SEC_tot_1  + EMS_DC_1)-1) px_2 x_2 pm_2 m_2 pgdp_2 gdp_2 px_1 x_1 pm_1 m_1 pgdp_1 gdp_1 ch_2 g_2 i_2 ch_13_2 x_2 ds_2 m_2 ch_1 g_1 i_1 ch_13_1 x_1 ds_1 m_1 pqd_2 pqd_1


'Endif
'	if %exceptions_DGT = "yes" then
'ajout de variables de sortie calculees dans Exceptions_DGT pour le cahier de variantes Reporting_3.add' 
                                                                                                                                                                                                                                        
Reporting_3.sheet(t)
show Reporting_3

   group Reporting_4 Y_01_1 Y_02_1 Y_03_1 Y_04_1 Y_05_1 Y_06_1 Y_07_1 Y_08_1 Y_09_1 Y_10_1 Y_11_1 Y_12_1 Y_13_1 Y_14_1 Y_15_1 Y_16_1 Y_17_1 Y_18_1 Y_19_1 Y_20_1 Y_21_1 _
Y_01_2 Y_02_2 Y_03_2 Y_04_2 Y_05_2 Y_06_2 Y_07_2 Y_08_2 Y_09_2 Y_10_2 Y_11_2 Y_12_2 Y_13_2 Y_14_2 Y_15_2 Y_16_2 Y_17_2 Y_18_2 Y_19_2 Y_20_2 Y_21_2  _
E_01_1 E_02_1 E_03_1 E_04_1 E_05_1 E_06_1 E_07_1 E_08_1 E_09_1 E_10_1 E_11_1 E_12_1 E_13_1 E_14_1 E_15_1 E_16_1 E_17_1 E_18_1 E_19_1 E_20_1 _ 
E_01_2 E_02_2 E_03_2 E_04_2 E_05_2 E_06_2 E_07_2 E_08_2 E_09_2 E_10_2 E_11_2 E_12_2 E_13_2 E_14_2 E_15_2 E_16_2 E_17_2 E_18_2 E_19_2 E_20_2   _
VA_01_1 VA_02_1 VA_03_1 VA_04_1 VA_05_1 VA_06_1 VA_07_1 VA_08_1 VA_09_1 VA_10_1 VA_11_1 VA_12_1 VA_13_1 VA_14_1 VA_15_1 VA_16_1 VA_17_1 VA_18_1 VA_19_1 VA_20_1  _
VA_01_2 VA_02_2 VA_03_2 VA_04_2 VA_05_2 VA_06_2 VA_07_2 VA_08_2 VA_09_2 VA_10_2 VA_11_2 VA_12_2 VA_13_2 VA_14_2 VA_15_2 VA_16_2 VA_17_2 VA_18_2 VA_19_2 VA_20_2 _ 
AUTO_1 AUTO_2 AUTO_ELEC_1 AUTO_ELEC_2 Q_MTEP_SEC_22_15_1 Q_MTEP_SEC_22_15_2 Q_MTEP_SEC_22_16_1 Q_MTEP_SEC_22_16_2 CID_14_1 CID_14_2  BUIL_1 BUIL_2 M2PERCAPITA  _
POP_TOT BUIL_H01_DES_1/BUIL_1(-1) BUIL_H01_DES_2/BUIL_2(-1) NEWBUIL_1/100 NEWBUIL_2/100 EXP_18_1 EXP_18_2 _
Q_Mtep_sec_01_1 Q_Mtep_sec_02_1 Q_Mtep_sec_03_1 Q_Mtep_sec_04_1 Q_Mtep_sec_05_1 Q_Mtep_sec_06_1 Q_Mtep_sec_07_1 Q_Mtep_sec_08_1 Q_Mtep_sec_09_1 Q_Mtep_sec_10_1 _
Q_Mtep_sec_11_1 Q_Mtep_sec_12_1 Q_Mtep_sec_13_1 Q_Mtep_sec_14_1 Q_Mtep_sec_15_1 Q_Mtep_sec_16_1 Q_Mtep_sec_17_1 Q_Mtep_sec_18_1 Q_Mtep_sec_19_1 Q_Mtep_sec_20_1 _
Q_Mtep_sec_01_2 Q_Mtep_sec_02_2 Q_Mtep_sec_03_2 Q_Mtep_sec_04_2 Q_Mtep_sec_05_2 Q_Mtep_sec_06_2 Q_Mtep_sec_07_2 Q_Mtep_sec_08_2 Q_Mtep_sec_09_2 Q_Mtep_sec_10_2 _
 Q_Mtep_sec_11_2 Q_Mtep_sec_12_2 Q_Mtep_sec_13_2 Q_Mtep_sec_14_2 Q_Mtep_sec_15_2 Q_Mtep_sec_16_2 Q_Mtep_sec_17_2 Q_Mtep_sec_18_2 Q_Mtep_sec_19_2 Q_Mtep_sec_20_2



                                                                                                                                                                                                                                         
'Reporting_4.sheet(t)
'show Reporting_4


group Reporting_5 100*(GDP_2/GDP_0-1) 100*((VA_2-VA_20_2)/(VA_0-VA_20_0)-1) 100*((CH_2-CH_0)/GDP_0) 100*((CH_03_2-CH_03_0)/GDP_0) _
               100*((G_2-G_0)/GDP_0) 100*((I_2-I_0)/GDP_0) 100*((DS_2-DS_0)/GDP_0) 100*((X_2-X_0)/GDP_0)-100*((M_2-M_0)/GDP_0) _
               100*((CH_2-CH_13_2-(CH_0-CH_13_0))/GDP_0) 100*((I_2+CH_13_2-(I_0+CH_13_0))/GDP_0) 100*((I_2-IA_20_2-(I_0-IA_20_0))/GDP_0) 100*((CH_13_2-CH_13_0)/GDP_0) 100*((IA_20_2-IA_20_0)/GDP_0) _
               100*(CH_2/CH_0-1) 100*(CH_03_2/CH_03_0-1) 100*(G_2/G_0-1) 100*(I_2/I_0-1) 100*((I_2-IA_20_2)/(I_0-IA_20_0)-1) 100*(X_2/X_0-1) 100*(M_2/M_0-1) _
              100*((CH_2-CH_13_2)/(CH_0-CH_13_0)-1) 100*((I_2+CH_13_2)/(I_0+CH_13_0)-1) 100*((CH_13_2)/(CH_13_0)-1) 100*((IA_20_2)/(IA_20_0)-1) _
      100*(DISPINC_VAL_2/DISPINC_VAL_0-1) 100*(DISPINC_VAL_2/DISPINC_VAL_0*PCH_0/PCH_2-1) 100*(DISPINC_VAL_2/DISPINC_VAL_0*L_0/L_2-1) _
      100*(DISPINC_VAL_2/DISPINC_VAL_0*L_0/L_2*PCH_0/PCH_2-1) 100*((dispinc_val_H01_2-(PCH_2*CH_2-pch_13_2*ch_13_2))/dispinc_val_H01_2-(dispinc_val_H01_0-(PCH_0*CH_0-pch_13_0*ch_13_0))/dispinc_val_H01_0) 100*(PCH_2/PCH_0-1) 100*(PY_2/PY_0-1) 100*(PX_2/PX_0-1) _
      100*(PM_2/PM_0-1) 100*(W_2/W_0-1) 100*((W_2/PCH_2)/(W_0/PCH_0)-1) 100*(CL_2/CL_0-1) 100*((CL_2/PVA_2)/(CL_0/PVA_0)-1) ((L_2/L_0)-1)*100 _
      L_2-L_0 100*(UNR_TOT_2-UNR_TOT_0) 100*(DC_VAL_2/(GDP_2*PGDP_2)-DC_VAL_0/(GDP_0*PGDP_0)) _
      100*((M_21_2*PM_21_2-X_21_2*PX_21_2+M_22_2*PM_22_2-X_22_2*PX_22_2+M_23_2*PM_23_2-X_23_2*PX_23_2+M_24_2*PM_24_2-X_24_2*PX_24_2)/(GDP_2*PGDP_2))-100*((M_21_0*PM_21_0-X_21_0*PX_21_0+M_22_0*PM_22_0-X_22_0*PX_22_0+M_23_0*PM_23_0-X_23_0*PX_23_0+M_24_0*PM_24_0-X_24_0*PX_24_0)/(GDP_0*PGDP_0)) 100*((-SP_G_VAL_2)/(GDP_2*PGDP_2)-(-SP_G_VAL_0)/(GDP_0*PGDP_0)) _
      100*((-BF_G_VAL_2)/(GDP_2*PGDP_2)-(-BF_G_VAL_0)/(GDP_0*PGDP_0)) (DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL_0/(PGDP_0*GDP_0))*100 _ 
DEP_val_2 REC_VAL_2 (0-BF_G_VAL_2) DEBT_G_VAL_2 REC_TCO_VAL_2 ENERT_22_2*PENERT_22_2 ENERT_24_2*PENERT_24_2 ENERT_23_2*PENERT_23_2 ENERT_21_2*PENERT_21_2 TCO_VAL_HH_2 _
TCO_VAL_HH_21_H01_2+TCO_VAL_HH_24_H01_2+TCO_VAL_HH_22_H01_2*(Q_MTEP_H_BUIL_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) _
TCO_VAL_HH_22_H01_2*(Q_MTEP_H_BUIL_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) TCO_VAL_HH_24_H01_2 TCO_VAL_HH_21_H01_2 _
TCO_VAL_HH_22_H01_2*(Q_MTEP_H_AUTO_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) TCO_VAL_SEC_2 REC_TCO_VAL_ETS_2  _
TCO_VAL_22_04_2+TCO_VAL_22_05_2+TCO_VAL_22_06_2+TCO_VAL_22_07_2+TCO_VAL_22_08_2+TCO_VAL_22_09_2+TCO_VAL_22_10_2+TCO_VAL_22_18_2+TCO_VAL_22_21_2+tco_val_22_2201_2+tco_val_22_2202_2+TCO_VAL_22_2301_2+TCO_VAL_22_2302_2+TCO_VAL_22_2303_2+TCO_VAL_22_2304_2+TCO_VAL_22_2305_2+TCO_VAL_22_2306_2+TCO_VAL_22_2307_2+TCO_VAL_22_2308_2+TCO_VAL_22_2401_2+TCO_VAL_22_2402_2+TCO_VAL_22_2403_2+TCO_VAL_22_2404_2+TCO_VAL_22_2405_2+TCO_VAL_22_2406_2 _ 	
TCO_VAL_24_04_2+TCO_VAL_24_05_2+TCO_VAL_24_06_2+TCO_VAL_24_07_2+TCO_VAL_24_08_2+TCO_VAL_24_09_2+TCO_VAL_24_10_2+TCO_VAL_24_18_2+TCO_VAL_24_21_2+tco_val_24_2201_2+tco_val_24_2202_2+TCO_VAL_24_2301_2+TCO_VAL_24_2302_2+TCO_VAL_24_2303_2+TCO_VAL_24_2304_2+TCO_VAL_24_2305_2+TCO_VAL_24_2306_2+TCO_VAL_24_2307_2+TCO_VAL_24_2308_2+TCO_VAL_24_2401_2+TCO_VAL_24_2402_2+TCO_VAL_24_2403_2+TCO_VAL_24_2404_2+TCO_VAL_24_2405_2+TCO_VAL_24_2406_2 _	
TCO_VAL_21_04_2+TCO_VAL_21_05_2+TCO_VAL_21_06_2+TCO_VAL_21_07_2+TCO_VAL_21_08_2+TCO_VAL_21_09_2+TCO_VAL_21_10_2+TCO_VAL_21_18_2+TCO_VAL_21_21_2+tco_val_21_2201_2+tco_val_21_2202_2+TCO_VAL_21_2301_2+TCO_VAL_21_2302_2+TCO_VAL_21_2303_2+TCO_VAL_21_2304_2+TCO_VAL_21_2305_2+TCO_VAL_21_2306_2+TCO_VAL_21_2307_2+TCO_VAL_21_2308_2+TCO_VAL_21_2401_2+TCO_VAL_21_2402_2+TCO_VAL_21_2403_2+TCO_VAL_21_2404_2+TCO_VAL_21_2405_2+TCO_VAL_21_2406_2 _
REC_TCO_VAL_NETS_2 _
TCO_VAL_22_01_2+TCO_VAL_22_02_2+TCO_val_22_03_2+TCO_VAL_22_11_2+TCO_VAL_22_12_2+TCO_VAL_22_13_2 _
TCO_VAL_24_01_2+TCO_VAL_24_02_2+TCO_val_24_03_2+TCO_VAL_24_11_2+TCO_VAL_24_12_2+TCO_VAL_24_13_2 _
TCO_VAL_21_01_2+TCO_VAL_21_02_2+TCO_val_21_03_2+TCO_VAL_21_11_2+TCO_VAL_21_12_2+TCO_VAL_21_13_2 _
TCO_VAL_SEC_14_2+TCO_VAL_SEC_15_2+TCO_VAL_SEC_16_2+TCO_VAL_SEC_17_2+TCO_VAL_SEC_19_2+TCO_VAL_SEC_20_2	TCO_VAL_22_14_2+TCO_VAL_22_15_2+TCO_VAL_22_16_2+TCO_VAL_22_17_2+TCO_VAL_22_19_2+TCO_VAL_22_20_2 _ 
TCO_VAL_24_14_2+TCO_VAL_24_15_2+TCO_VAL_24_16_2+TCO_VAL_24_17_2+TCO_VAL_24_19_2+TCO_VAL_24_20_2 _
TCO_VAL_21_14_2+TCO_VAL_21_15_2+TCO_VAL_21_16_2+TCO_VAL_21_17_2+TCO_VAL_21_19_2+TCO_VAL_21_20_2 _
DEP_val_0 REC_VAL_0 (0-BF_G_VAL_0) DEBT_G_VAL_0 REC_TCO_VAL_0	ENERT_22_0*PENERT_22_0	ENERT_24_0*PENERT_24_0	ENERT_23_0*PENERT_23_0 ENERT_21_0*PENERT_21_0 _
TCO_VAL_HH_0 _
TCO_VAL_HH_21_H01_0+TCO_VAL_HH_24_H01_0+TCO_VAL_HH_22_H01_0*(Q_MTEP_H_BUIL_22_0)/(Q_MTEP_H_BUIL_22_0+Q_MTEP_H_AUTO_22_0) _
TCO_VAL_HH_22_H01_0*(Q_MTEP_H_BUIL_22_0)/(Q_MTEP_H_BUIL_22_0+Q_MTEP_H_AUTO_22_0) _
TCO_VAL_HH_24_H01_0 _
TCO_VAL_HH_21_H01_0 _
TCO_VAL_HH_22_H01_0*(Q_MTEP_H_AUTO_22_0)/(Q_MTEP_H_BUIL_22_0+Q_MTEP_H_AUTO_22_0) _
TCO_VAL_SEC_0 REC_TCO_VAL_ETS_0 _
TCO_VAL_22_04_0+TCO_VAL_22_05_0+TCO_VAL_22_06_0+TCO_VAL_22_07_0+TCO_VAL_22_08_0+TCO_VAL_22_09_0+TCO_VAL_22_10_0+TCO_VAL_22_18_0+TCO_VAL_22_21_0+tco_val_22_2201_0+tco_val_22_2202_0+TCO_VAL_22_2301_0+TCO_VAL_22_2302_0+TCO_VAL_22_2303_0+TCO_VAL_22_2304_0+TCO_VAL_22_2305_0+TCO_VAL_22_2306_0+TCO_VAL_22_2307_0+TCO_VAL_22_2308_0+TCO_VAL_22_2401_0+TCO_VAL_22_2402_0+TCO_VAL_22_2403_0+TCO_VAL_22_2404_0+TCO_VAL_22_2405_0+TCO_VAL_22_2406_0 _
TCO_VAL_24_04_0+TCO_VAL_24_05_0+TCO_VAL_24_06_0+TCO_VAL_24_07_0+TCO_VAL_24_08_0+TCO_VAL_24_09_0+TCO_VAL_24_10_0+TCO_VAL_24_18_0+TCO_VAL_24_21_0+tco_val_24_2201_0+TCO_VAL_24_2202_0+TCO_VAL_24_2301_0+TCO_VAL_24_2302_0+TCO_VAL_24_2303_0+TCO_VAL_24_2304_0+TCO_VAL_24_2305_0+TCO_VAL_24_2306_0+TCO_VAL_24_2307_0+TCO_VAL_24_2308_0+TCO_VAL_24_2401_0+TCO_VAL_24_2402_0+TCO_VAL_24_2403_0+TCO_VAL_24_2404_0+TCO_VAL_24_2405_0+TCO_VAL_24_2406_0 _
TCO_VAL_21_04_0+TCO_VAL_21_05_0+TCO_VAL_21_06_0+TCO_VAL_21_07_0+TCO_VAL_21_08_0+TCO_VAL_21_09_0+TCO_VAL_21_10_0+TCO_VAL_21_18_0+TCO_VAL_21_21_0+tco_val_21_2201_0+tco_val_21_2202_0+TCO_VAL_21_2301_0+TCO_VAL_21_2302_0+TCO_VAL_21_2303_0+TCO_VAL_21_2304_0+TCO_VAL_21_2305_0+TCO_VAL_21_2306_0+TCO_VAL_21_2307_0+TCO_VAL_21_2308_0+TCO_VAL_21_2401_0+TCO_VAL_21_2402_0+TCO_VAL_21_2403_0+TCO_VAL_21_2404_0+TCO_VAL_21_2405_0+TCO_VAL_21_2406_0 _
REC_TCO_VAL_NETS_0 _
TCO_VAL_22_01_0+TCO_VAL_22_02_0+TCO_val_22_03_0+TCO_VAL_22_11_0+TCO_VAL_22_12_0+TCO_VAL_22_13_0 _
TCO_VAL_24_01_0+TCO_VAL_24_02_0+TCO_val_24_03_0+TCO_VAL_24_11_0+TCO_VAL_24_12_0+TCO_VAL_24_13_0 _
TCO_VAL_21_01_0+TCO_VAL_21_02_0+TCO_val_21_03_0+TCO_VAL_21_11_0+TCO_VAL_21_12_0+TCO_VAL_21_13_0 _ 
TCO_VAL_SEC_14_0+TCO_VAL_SEC_15_0+TCO_VAL_SEC_16_0+TCO_VAL_SEC_17_0+TCO_VAL_SEC_19_0+TCO_VAL_SEC_20_0	TCO_VAL_22_14_0+TCO_VAL_22_15_0+TCO_VAL_22_16_0+TCO_VAL_22_17_0+TCO_VAL_22_19_0+TCO_VAL_22_20_0 TCO_VAL_24_14_0+TCO_VAL_24_15_0+TCO_VAL_24_16_0+TCO_VAL_24_17_0+TCO_VAL_24_19_0+TCO_VAL_24_20_0 _ 
TCO_VAL_21_14_0+TCO_VAL_21_15_0+TCO_VAL_21_16_0+TCO_VAL_21_17_0+TCO_VAL_21_19_0+TCO_VAL_21_20_0 _
EMS_TOT_2 _
EMS_HH_22_2+EMS_SEC_22_01_2+EMS_SEC_22_02_2+EMS_SEC_22_03_2+EMS_SEC_22_04_2+EMS_SEC_22_05_2+EMS_SEC_22_06_2+EMS_SEC_22_07_2+EMS_SEC_22_08_2+EMS_SEC_22_09_2+EMS_SEC_22_12_2+EMS_SEC_22_13_2+EMS_SEC_22_14_2+EMS_SEC_22_15_2+EMS_SEC_22_16_2+EMS_SEC_22_17_2+EMS_SEC_22_18_2+EMS_SEC_22_19_2+EMS_SEC_22_20_2 EMS_HH_24_2+EMS_SEC_2401_2+EMS_SEC_24_01_2+EMS_SEC_24_02_2+EMS_SEC_24_03_2+EMS_SEC_24_04_2+EMS_SEC_24_05_2+EMS_SEC_24_06_2+EMS_SEC_24_07_2+EMS_SEC_24_08_2+EMS_SEC_24_09_2+EMS_SEC_24_10_2+EMS_SEC_24_11_2+EMS_SEC_24_12_2+EMS_SEC_24_13_2+EMS_SEC_24_14_2+EMS_SEC_24_15_2+EMS_SEC_24_16_2+EMS_SEC_24_17_2+EMS_SEC_24_18_2+EMS_SEC_24_19_2+EMS_SEC_24_20_2 _
       EMS_SEC_2302_2+EMS_SEC_2303_2+EMS_SEC_2304_2 EMS_HH_21_2+EMS_SEC_21_05_2+EMS_SEC_21_06_2+EMS_SEC_21_07_2+EMS_SEC_21_08_2+EMS_SEC_21_10_2+EMS_SEC_21_12_2+EMS_SEC_21_19_2+EMS_SEC_21_20_2 EMS_DC_05_2+EMS_DC_04_2 EMS_HH_21_2+EMS_HH_24_2+EMS_HH_22_2*(@ELEM(PENER_BUIL_H01_22,2006)*ENER_BUIL_H01_22_2)/EXP_22_H01_2 _
       EMS_HH_22_2*(EXP_AUTO_H01_22_2/EXP_22_H01_2) _
       EMS_SEC_04_2+EMS_SEC_05_2+EMS_SEC_06_2+EMS_SEC_07_2+EMS_SEC_08_2+EMS_SEC_09_2+EMS_SEC_10_2+EMS_SEC_18_2+EMS_SEC_21_19_2+EMS_SEC_21_20_2+EMS_SEC_2201_2+EMS_SEC_2302_2+EMS_SEC_2303_2+EMS_SEC_2304_2+EMS_SEC_2401_2+EMS_DC_04_2+EMS_DC_05_2 _
        EMS_SEC_01_2+EMS_SEC_02_2+EMS_SEC_03_2+EMS_SEC_11_2+EMS_SEC_12_2+EMS_SEC_13_2 _             
        EMS_SEC_14_2+EMS_SEC_15_2+EMS_SEC_16_2+EMS_SEC_17_2+EMS_SEC_19_2+EMS_SEC_20_2 _               
        EMS_TOT_0 EMS_HH_22_0+EMS_SEC_22_01_0+EMS_SEC_22_02_0+EMS_SEC_22_03_0+EMS_SEC_22_04_0+EMS_SEC_22_05_0+EMS_SEC_22_06_0+EMS_SEC_22_07_0+EMS_SEC_22_08_0+EMS_SEC_22_09_0+EMS_SEC_22_12_0+EMS_SEC_22_13_0+EMS_SEC_22_14_0+EMS_SEC_22_15_0+EMS_SEC_22_16_0+EMS_SEC_22_17_0+EMS_SEC_22_18_0+EMS_SEC_22_19_0+EMS_SEC_22_20_0 EMS_HH_24_0+EMS_SEC_2401_0+EMS_SEC_24_01_0+EMS_SEC_24_02_0+EMS_SEC_24_03_0+EMS_SEC_24_04_0+EMS_SEC_24_05_0+EMS_SEC_24_06_0+EMS_SEC_24_07_0+EMS_SEC_24_08_0+EMS_SEC_24_09_0+EMS_SEC_24_10_0+EMS_SEC_24_11_0+EMS_SEC_24_12_0+EMS_SEC_24_13_0+EMS_SEC_24_14_0+EMS_SEC_24_15_0+EMS_SEC_24_16_0+EMS_SEC_24_17_0+EMS_SEC_24_18_0+EMS_SEC_24_19_0+EMS_SEC_24_20_0 _
        EMS_SEC_2302_0+EMS_SEC_2303_0+EMS_SEC_2304_0 EMS_HH_21_0+EMS_SEC_21_05_0+EMS_SEC_21_06_0+EMS_SEC_21_07_0+EMS_SEC_21_08_0+EMS_SEC_21_10_0+EMS_SEC_21_12_0+EMS_SEC_21_19_0+EMS_SEC_21_20_0 _
       EMS_DC_05_0+EMS_DC_04_0 _
       EMS_HH_21_0+EMS_HH_24_0+EMS_HH_22_0*(@ELEM(PENER_BUIL_H01_22_0,2006)*ENER_BUIL_H01_22_0)/EXP_22_H01_0 _
       EMS_HH_22_0*(EXP_AUTO_H01_22_0/EXP_22_H01_0) _
       EMS_SEC_04_0+EMS_SEC_05_0+EMS_SEC_06_0+EMS_SEC_07_0+EMS_SEC_08_0+EMS_SEC_09_0+EMS_SEC_10_0+EMS_SEC_18_0+EMS_SEC_21_19_0+EMS_SEC_21_20_0+EMS_SEC_2201_0+EMS_SEC_2302_0+EMS_SEC_2303_0+EMS_SEC_2304_0+EMS_SEC_2401_0+EMS_DC_04_0+EMS_DC_05_0 _
       EMS_SEC_01_0+EMS_SEC_02_0+EMS_SEC_03_0+EMS_SEC_11_0+EMS_SEC_12_0+EMS_SEC_13_0 _
       EMS_SEC_14_0+EMS_SEC_15_0+EMS_SEC_16_0+EMS_SEC_17_0+EMS_SEC_19_0+EMS_SEC_20_0 ER_TOTAL_2 _
       ER_TRANS_PRIVATE_2+ER_TRANS_PUBLIC_2 ER_TRANS_PRIVATE_2 ER_TRANS_PRIVATE_OIL_2 ER_TRANS_PRIVATE_ELEC_2 ER_TRANS_PUBLIC_2 ER_TRANS_PUBLIC_OIL_2 _
       ER_TRANS_PUBLIC_ELEC_2 ER_RESIDENTIAL_2 ER_RESIDENTIAL_OIL_2 ER_RESIDENTIAL_ELEC_2 ER_RESIDENTIAL_GAS_2 ER_AGRICULTURE_2 ER_AGRICULTURE_OIL_2 _
       ER_AGRICULTURE_ELEC_2 ER_INDUS_2 ER_INDUS_OIL_2 ER_INDUS_ELEC_2 ER_INDUS_GAS_2 ER_INDUS_COAL_2 ER_TERTIARY_2 ER_TERTIARY_OIL_2 ER_TERTIARY_ELEC_2 ER_TERTIARY_GAS_2 ER_OIL_2 ER_GAS_2 ER_COAL_2 ER_ELEC_2 ER_ELEC_2301_2 ER_ELEC_2302_2 ER_ELEC_2303_2 _
        ER_ELEC_2304_2 ER_ELEC_2305_2 ER_ELEC_2306_2 ER_ELEC_2307_2 ER_ELEC_2308_2 ER_TOTAL_0 ER_TRANS_PRIVATE_0+ER_TRANS_PUBLIC_0 ER_TRANS_PRIVATE_0 ER_TRANS_PRIVATE_OIL_0 ER_TRANS_PRIVATE_ELEC_0 ER_TRANS_PUBLIC_0 ER_TRANS_PUBLIC_OIL_0 ER_TRANS_PUBLIC_ELEC_0 ER_RESIDENTIAL_0 ER_RESIDENTIAL_OIL_0 ER_RESIDENTIAL_ELEC_0 ER_RESIDENTIAL_GAS_0 ER_AGRICULTURE_0 ER_AGRICULTURE_OIL_0 ER_AGRICULTURE_ELEC_0 ER_INDUS_0 ER_INDUS_OIL_0 ER_INDUS_ELEC_0 ER_INDUS_GAS_0 ER_INDUS_COAL_0 ER_TERTIARY_0 ER_TERTIARY_OIL_0 ER_TERTIARY_ELEC_0 ER_TERTIARY_GAS_0 ER_OIL_0 _
        ER_GAS_0 ER_COAL_0 ER_ELEC_0 ER_ELEC_2301_0 ER_ELEC_2302_0 ER_ELEC_2303_0 ER_ELEC_2304_0 ER_ELEC_2305_0 ER_ELEC_2306_0 ER_ELEC_2307_0 ER_ELEC_2308_0 ENER_H01_2 _
        @ELEM(PENER_BUIL_H01_2,2006)*ENER_BUIL_H01_2 EXP_AUTO_H01_2 ENER_H01 @ELEM(PENER_BUIL_H01,2006)*ENER_BUIL_H01 EXP_AUTO_H01 KM_auto_H01_2 _
        KM_TRAVELER_18_H01_2 KM_TRAV_AUTO_LD_H01_2 KM_TRAV_AUTO_CD_H01_2 KM_TRAVELER_14_H01_2 KM_TRAVELER_15_H01_2 KM_TRAVELER_CD_H01_2 KM_TRAVELER_LD_H01_2 _
        KM_AUTO_H01_0 KM_TRAVELER_18_H01_0 KM_TRAV_AUTO_LD_H01_0 KM_TRAV_AUTO_CD_H01_0 KM_TRAVELER_14_H01_0 _
        KM_TRAVELER_15_H01_0 KM_TRAVELER_CD_H01_0 KM_TRAVELER_LD_H01_0 _
        ER_Auto_2 ER_AUTO_th_A_2 ER_AUTO_th_B_2 ER_AUTO_th_C_2 ER_AUTO_th_D_2 ER_AUTO_th_E_2 ER_AUTO_th_F_2 ER_AUTO_th_G_2 _
        ER_AUTO_elec_A_2 ER_AUTO_elec_B_2 ER_AUTO_elec_C_2 ER_AUTO_elec_D_2 ER_AUTO_elec_E_2 ER_AUTO_elec_F_2 ER_AUTO_elec_G_2 ER_Auto_Coal_2 ER_auto_th_2 ER_Auto_Elec_2 ER_Auto_Gas_2 _
        ER_NEWAUTO_2 ER_NEWAUTO_th_2 ER_newauto_th_A_2 ER_newauto_th_B_2 ER_newauto_th_C_2 ER_newauto_th_D_2 ER_newauto_th_E_2 ER_newauto_th_F_2  ER_newauto_th_G_2  _
        ER_NEWAUTO_elec_2 ER_NEWAUTO_elec_A_2 ER_NEWAUTO_elec_B_2 ER_NEWAUTO_elec_C_2 ER_NEWAUTO_elec_D_2 ER_NEWAUTO_elec_E_2 ER_NEWAUTO_elec_F_2 ER_NEWAUTO_elec_G_2 _
        ER_Auto_0 ER_AUTO_th_A_0 ER_AUTO_th_B_0 ER_AUTO_th_C_0 ER_AUTO_th_D_0 ER_AUTO_th_E_0 ER_AUTO_th_F_0 ER_AUTO_th_G_0 ER_AUTO_elec_A_0 ER_AUTO_elec_B_0 ER_AUTO_elec_C_0 ER_AUTO_elec_D_0 ER_AUTO_elec_E_0 ER_AUTO_elec_F_0 ER_AUTO_elec_G_0 ER_Auto_Coal_0 ER_auto_th_0 ER_Auto_Elec_0 ER_Auto_Gas_0 _
        ER_NEWAUTO_0 ER_NEWAUTO_th_0 ER_newauto_th_A_0 ER_newauto_th_B_0 ER_newauto_th_C_0 ER_newauto_th_D_0 ER_newauto_th_E_0 ER_newauto_th_F_0 _
        ER_NEWAUTO_elec_0 ER_NEWAUTO_elec_A_0 ER_NEWAUTO_elec_B_0 ER_NEWAUTO_elec_C_0 ER_NEWAUTO_elec_D_0 ER_NEWAUTO_elec_E_0 ER_NEWAUTO_elec_F_0 _
        ER_BUIL_2 ER_BUIL_A_2 ER_BUIL_B_2 ER_BUIL_C_2 ER_BUIL_D_2 ER_BUIL_E_2 ER_BUIL_F_2 ER_BUIL_G_2  SUB_REHAB_VAL_2 tax_cr_19_2 ER_BUIL_0 _
        ER_BUIL_A_0 ER_BUIL_B_0 ER_BUIL_C_0 ER_BUIL_D_0 ER_BUIL_E_0 ER_BUIL_F_0 ER_BUIL_G_0 SUB_REHAB_VAL_0 tax_cr_19_0 _
	  pch_2 pch_0 pgdp_2 pgdp_0 gdp_2 gdp_0 _
       L_01_0	L_02_0	L_03_0	L_04_0	L_05_0	L_06_0	L_07_0	L_08_0	L_09_0	L_10_0	L_11_0 L_12_0 L_13_0 L_14_0 L_15_0	L_16_0	L_17_0	L_18_0	L_19_0	L_20_0	L_21_0 _
      L_2201_0	L_2202_0	L_2301_0	L_2302_0	L_2303_0	L_2304_0	L_2305_0	L_2306_0	L_2307_0 L_2308_0 L_2401_0 L_2402_0 L_2403_0 L_2404_0 L_2405_0 _
	 L_2406_0	L_01_2	L_02_2	L_03_2	L_04_2	L_05_2	L_06_2	L_07_2	L_08_2	L_09_2	L_10_2	L_11_2	L_12_2	L_13_2	L_14_2	L_15_2	L_16_2	L_17_2	L_18_2	L_19_2	L_20_2	L_21_2 _ 
      L_2201_2	L_2202_2	L_2301_2	L_2302_2	L_2303_2	L_2304_2	L_2305_2	L_2306_2	L_2307_2 L_2308_2 _
      L_2401_2	L_2402_2	L_2403_2	L_2404_2	L_2405_2	L_2406_2 _ 	
      VA_01_0	VA_02_0	VA_03_0	VA_04_0	VA_05_0	VA_06_0	VA_07_0	VA_08_0	VA_09_0 VA_10_0 VA_11_0 VA_12_0 VA_13_0 VA_14_0 VA_15_0 VA_16_0 VA_17_0 VA_18_0 VA_19_0 VA_20_0 VA_21_0 _
      VA_2201_0 VA_2202_0 VA_2301_0 VA_2302_0 VA_2303_0 VA_2304_0	VA_2305_0 VA_2306_0 VA_2307_0 VA_2308_0 _
      VA_2401_0 VA_2402_0 VA_2403_0	 VA_2404_0 VA_2405_0 VA_2406_0 _	
      VA_01_2	VA_02_2	VA_03_2 VA_04_2 VA_05_2 VA_06_2 VA_07_2 VA_08_2 VA_09_2 VA_10_2 VA_11_2 VA_12_2 VA_13_2 VA_14_2 VA_15_2 VA_16_2 VA_17_2 VA_18_2 VA_19_2 VA_20_2 VA_21_2 _
      VA_2201_2 VA_2202_2 VA_2301_2 VA_2302_2 VA_2303_2 VA_2304_2	VA_2305_2	 VA_2306_2 VA_2307_2 VA_2308_2 _
      VA_2401_2 VA_2402_2 VA_2403_2 VA_2404_2 VA_2405_2 VA_2406_2 _
TTCO_VOL_21_2 TTCO_VOL_22_2 TTCO_VOL_24_2 TTCO_VOL_21_0 TTCO_VOL_22_0 TTCO_VOL_24_0 DISPINC_VAL_2 DISPINC_VAL_0 _
(gdpter_2/gdpbis_2-1)*100 (gdpter_0/gdpbis_0-1)*100 _
IA_0 IA_01_0 IA_02_0 IA_03_0 IA_04_0 IA_05_0 IA_06_0 IA_07_0 IA_08_0 IA_09_0 IA_10_0 IA_11_0 IA_12_0 IA_13_0 IA_13_20_0 IA_14_0 IA_15_0  IA_16_0 IA_17_0  IA_18_0 IA_19_0 IA_20_0 IA_21_0 _
IA_2201_0 IA_2202_0 IA_2301_0 IA_2302_0 IA_2303_0 IA_2304_0 IA_2305_0 IA_2306_0 IA_2307_0 IA_2308_0 IA_2401_0 IA_2402_0 IA_2403_0 IA_2404_0 IA_2405_0 IA_2406_0   _
REHAB_VAL_0 NEWBUIL_H01_0 PNEWBUIL_H01_0 PREHAB_H01_0 REHAB_H01_0 PEXP_13_H01_0 CH_03_0 PGDP_0 _
IA_2 IA_01_2 IA_02_2 IA_03_2 IA_04_2 IA_05_2 IA_06_2 IA_07_2 IA_08_2 IA_09_2 IA_10_2 IA_11_2 IA_12_2 IA_13_2 IA_13_20_2 IA_14_2 IA_15_2 IA_16_2 IA_17_2 IA_18_2 IA_19_2 IA_20_2 IA_21_2 _
IA_2201_2 IA_2202_2 IA_2301_2 IA_2302_2 IA_2303_2 IA_2304_2 IA_2305_2 IA_2306_2 IA_2307_2 IA_2308_2 IA_2401_2 IA_2402_2 IA_2403_2 IA_2404_2 IA_2405_2 IA_2406_2  _
REHAB_VAL_2 NEWBUIL_H01_2 PNEWBUIL_H01_2 PREHAB_H01_2 REHAB_H01_2 PEXP_13_H01_2 CH_03_2 PGDP_2 _
CH_01_2 CH_02_2 CH_03_2 CH_04_2 CH_05_2 CH_06_2 CH_07_2 CH_08_2 CH_09_2 CH_10 CH_11_2 CH_12_2 CH_13_2 CH_14_2 _
CH_15_2 CH_16_2 CH_17_2 CH_18_2 CH_19_2 CH_20_2 CH_21_2 CH_22_2 CH_23_2 CH_24_2 _ 
CH_01_0 CH_02_0 CH_03_0 CH_04_0 CH_05_0 CH_06_0 CH_07_0 CH_08_0 CH_09_0 CH_10 CH_11_0 CH_12_0 CH_13_0 CH_14_0 _
CH_15_0 CH_16_0 CH_17_0 CH_18_0 CH_19_0 CH_20_0 CH_21_0 CH_22_0 CH_23_0 CH_24_0 _
PCH_01_2 PCH_02_2 PCH_03_2 PCH_04_2 PCH_05_2 PCH_06_2 PCH_07_2 PCH_08_2 PCH_09_2 PCH_10 _
PCH_11_2 PCH_12_2 PCH_13_2 PCH_14_2 PCH_15_2 PCH_16_2 PCH_17_2 PCH_18_2 PCH_19_2 PCH_20_2 _
PCH_21_2 PCH_22_2 PCH_23_2 PCH_24_2 PCH_01_0 PCH_02_0 PCH_03_0 PCH_04_0 PCH_05_0 PCH_06_0 _ 
PCH_07_0 PCH_08_0 PCH_09_0 PCH_10 PCH_11_0 PCH_12_0 PCH_13_0 PCH_14_0 PCH_15_0 PCH_16_0 _ 
PCH_17_0 PCH_18_0 PCH_19_0 PCH_20_0 PCH_21_0 PCH_22_0 PCH_23_0 PCH_24_0 PENER_BUIL_H01_2 PENER_BUIL_H01_0 ENER_BUIL_H01_2 ENER_BUIL_H01_0 pexp_auto_h01_2 pexp_auto_h01_0 _
exp_mobauto_val_h01_2 exp_mobauto_val_h01_0 exp_housing_val_h01_2 exp_housing_val_h01_0 pop_tot _
I_MDE_01_0 I_MDE_02_0 I_MDE_03_0 I_MDE_04_0	I_MDE_05_0 I_MDE_06_0 I_MDE_07_0 I_MDE_08_0	I_MDE_09_0 I_MDE_10_0 I_MDE_11_0 _
 I_MDE_12_0 I_MDE_13_0 I_MDE_19_0 I_MDE_20_0 I_MDE_01_2 I_MDE_02_2 I_MDE_03_2 I_MDE_04_2 I_MDE_05_2 I_MDE_06_2 I_MDE_07_2 I_MDE_08_2 I_MDE_09_2 I_MDE_10_2 I_MDE_11_2 I_MDE_12_2 I_MDE_13_2  _
I_MDE_19_2 I_MDE_20_2 NDI_ADEB_VOL_0 NDI_ADEB_VOL_2 INV_Road PIA_13_19_2 PIA_13_19_0 PIA_13_20_0 PIA_13_20_2 PIA_20_2 PIA_20_0 PIA_15_2 PIA_15_0 CSPE_0 CSPE_2 _
Carbon_foot_0 Carbon_foot_2 PSUBD_15_2*SUBD_15_2 CU_MWH_PGDP_21_0 CU_MWH_PGDP_22_0 CU_MWH_PGDP_23_0  CU_MWH_PGDP_24_0 CU_MWH_PGDP_2201_0 CU_MWH_PGDP_2202_0 CU_MWH_PGDP_2301_0 _
CU_MWH_PGDP_2302_0 CU_MWH_PGDP_2303_0 CU_MWH_PGDP_2304_0 CU_MWH_PGDP_2305_0 CU_MWH_PGDP_2306_0 CU_MWH_PGDP_2307_0 CU_MWH_PGDP_2308_0 CU_MWH_PGDP_2401_0 CU_MWH_PGDP_2402_0 _
CU_MWH_PGDP_2403_0 CU_MWH_PGDP_2404_0 CU_MWH_PGDP_2405_0 CU_MWH_PGDP_2406_0 CU_MWH_PGDP_21_2 CU_MWH_PGDP_22_2 CU_MWH_PGDP_23_2  CU_MWH_PGDP_24_2 CU_MWH_PGDP_2201_2 _
CU_MWH_PGDP_2202_2 CU_MWH_PGDP_2301_2 CU_MWH_PGDP_2302_2 CU_MWH_PGDP_2303_2 CU_MWH_PGDP_2304_2 CU_MWH_PGDP_2305_2 CU_MWH_PGDP_2306_2 CU_MWH_PGDP_2307_2 CU_MWH_PGDP_2308_2 _
CU_MWH_PGDP_2401_2 CU_MWH_PGDP_2402_2 CU_MWH_PGDP_2403_2 CU_MWH_PGDP_2404_2 CU_MWH_PGDP_2405_2 CU_MWH_PGDP_2406_2 UNR_TOT UNR_TOT_2 _
GDP_0 CH_0 G_0 I_0+CH_13_0 I_0-IA_20_0 CH_13_0 IA_20_0 X_0 M_0 DISPINC_VAL_0/(POP_TOT*PCH_0) L_0 SP_G_VAL_0/PGDP_0 DEBT_G_VAL_0/PGDP_0 GDP_2 CH_2 G_2 I_2+CH_13_2 I_2-IA_20_2 CH_13_2 IA_20_2 X_2 M_2 DISPINC_VAL_2/(POP_TOT*PCH_2) L_2 SP_G_VAL_2/PGDP_2 DEBT_G_VAL_2/PGDP_2 _
Exp_13_0 Exp_13_2 exp_newauto_val_h01_ca_0 exp_newauto_val_h01_cb_0 exp_newauto_val_h01_cc_0 exp_newauto_val_h01_cd_0 exp_newauto_val_h01_ce_0 exp_newauto_val_h01_cf_0 exp_newauto_val_h01_cg_0 exp_newauto_val_h01_ca_2 exp_newauto_val_h01_cb_2 exp_newauto_val_h01_cc_2 _
exp_newauto_val_h01_cd_2 exp_newauto_val_h01_ce_2 exp_newauto_val_h01_cf_2 exp_newauto_val_h01_cg_2

                                                                                                                                                                                                                                          
'Reporting_5.sheet(t)
'show Reporting_5



 
group Reporting_6 REC_VAL_1 DIV_GOV_VAL_1 IR_VAL_1 AIC_VAL_1 -CL_S_20_1*L_S_20_1*PROG_L_20_1 PY_20_1*Y_20_1 PTAX_1*TAX_1 PIY_1*IY_1 PIS_1*IS_1 PCSE_TOT_1*CSE_TOT_1 PCSS_TOT_1*CSS_TOT_1 -(PE_20_1*E_20_1) -(PMAT_20_1*MAT_20_1) -(PIY_20_1*IY_20_1) REC_VAL_2 DIV_GOV_VAL_2 IR_VAL_2 AIC_VAL_2 -CL_S_20_2*L_S_20_2*PROG_L_20_2 PY_20_2*Y_20_2 PTAX_2*TAX_2 PIY_2*IY_2 PIS_2*IS_2 PCSE_TOT_2*CSE_TOT_2 PCSS_TOT_2*CSS_TOT_2 -(PE_20_2*E_20_2) -(PMAT_20_2*MAT_20_2) -(PIY_20_2*IY_20_2) DEP_VAL_1 CL_S_20_1*L_S_20_1*PROG_L_20_1 R_G_1(-1)*DEBT_G_VAL_1(-1) PRESOC_VAL_1 SUB_RENOV_VAL_1 SUB_AUTO_VAL_1 PE_20_1*E_20_1 PMAT_20_1*MAT_20_1 PIY_20_1*IY_20_1 PIA_20_1*IA_20_1 PG_1*G_1-PG_20_1*G_20_1 -(PSUB_1*SUB_1-PSUB_01_1*SUB_01_1) -(PSY_1*SY_1-PSY_01_1*SY_01_1) DEP_VAL_2 CL_S_20_2*L_S_20_2*PROG_L_20_2 R_G_2(-1)*DEBT_G_VAL_2(-1) PRESOC_VAL_2 SUB_RENOV_VAL_2 SUB_AUTO_VAL_2 PE_20_2*E_20_2 PMAT_20_2*MAT_20_2 PIY_20_2*IY_20_2 PIA_20_2*IA_20_2 PG_2*G_2-PG_20_2*G_20_2 -(PSUB_2*SUB_2-PSUB_01_2*SUB_01_2) -(PSY_2*SY_2-PSY_01_2*SY_01_2) PGDP_0 PGDP_2 GDP_0 GDP_2 INC_GOV_OTH_NET REDIS_VAL_H

'Reporting_6.sheet(t)
'show Reporting_6

 
group Reporting_MPR SUB_RENOV_VAL_1	SUB_RENOV_VAL_2 GDP_1	GDP_2	PGDP_1	PGDP_2 GDP_1	GDP_2	PGDP_1 PGDP_2 R_sub_CEE_h01_cb_ca_1 R_sub_CEE_h01_cb_ca_2	CEE REHAB_0	REHAB_2			RENOV_VAL_0	RENOV_VAL_2			PREHAB_1	PREHAB_2 PCH_13_1	PCH_13_2 RENOV_VAL_CA_1	RENOV_VAL_CB_1	RENOV_VAL_CC_1	RENOV_VAL_CD_1	RENOV_VAL_CE_0	RENOV_VAL_CF_1	RENOV_VAL_CG_1  RENOV_VAL_CA_2	RENOV_VAL_CB_2	RENOV_VAL_CC_2	RENOV_VAL_CD_2	RENOV_VAL_CE_2	RENOV_VAL_CF_2	RENOV_VAL_CG_2 REHAB_H01_CA_CA	REHAB_H01_CA_CB	REHAB_H01_CA_CC	REHAB_H01_CA_CD	REHAB_H01_CA_CE	REHAB_H01_CA_CF	REHAB_H01_CA_CG	REHAB_H01_CB_CA_1	REHAB_H01_CC_CA_1	REHAB_H01_CC_CB_1	REHAB_H01_CD_CA_1	REHAB_H01_CD_CB_1	REHAB_H01_CD_CC_1	REHAB_H01_CE_CA_1	REHAB_H01_CE_CB_1	REHAB_H01_CE_CC_1	REHAB_H01_CE_CD_1	REHAB_H01_CF_CA_1	REHAB_H01_CF_CB_1	REHAB_H01_CF_CC_1	REHAB_H01_CF_CD_1	REHAB_H01_CF_CE_1	REHAB_H01_CG_CA_1	REHAB_H01_CG_CB_1	REHAB_H01_CG_CC_1	REHAB_H01_CG_CD_1	REHAB_H01_CG_CE_1	REHAB_H01_CG_CF_1 REHAB_H01_CA_CA	REHAB_H01_CA_CB	REHAB_H01_CA_CC	REHAB_H01_CA_CD	REHAB_H01_CA_CE	REHAB_H01_CA_CF	REHAB_H01_CA_CG	REHAB_H01_CB_CA_2	REHAB_H01_CC_CA_2	REHAB_H01_CC_CB_2	REHAB_H01_CD_CA_2	REHAB_H01_CD_CB_2	REHAB_H01_CD_CC_2	REHAB_H01_CE_CA_2	REHAB_H01_CE_CB_2	REHAB_H01_CE_CC_2	REHAB_H01_CE_CD_2	REHAB_H01_CF_CA_2	REHAB_H01_CF_CB_2	REHAB_H01_CF_CC_2	REHAB_H01_CF_CD_2	REHAB_H01_CF_CE_2	REHAB_H01_CG_CA_2	REHAB_H01_CG_CB_2	REHAB_H01_CG_CC_2	REHAB_H01_CG_CD_2	REHAB_H01_CG_CE_2	REHAB_H01_CG_CF_2 REHAB_H01_CA	REHAB_H01_CB_1	REHAB_H01_CC_1	REHAB_H01_CD_1	REHAB_H01_CE_1	REHAB_H01_CF_1	REHAB_H01_CG_1			REHAB_H01_CA	REHAB_H01_CB_2	REHAB_H01_CC_2	REHAB_H01_CD_2	REHAB_H01_CE_2	REHAB_H01_CF_2	REHAB_H01_CG_2 PREHAB_H01_CA_CA_1	PREHAB_H01_CB_CA_1	PREHAB_H01_CB_CB_1	PREHAB_H01_CC_CA_1	PREHAB_H01_CC_CB_1	PREHAB_H01_CC_CC_1	PREHAB_H01_CD_CA_1	PREHAB_H01_CD_CB_1	PREHAB_H01_CD_CC_1	PREHAB_H01_CD_CD_1	PREHAB_H01_CE_CA_1	PREHAB_H01_CE_CB_1	PREHAB_H01_CE_CC_1	PREHAB_H01_CE_CD_1	PREHAB_H01_CE_CE_1	PREHAB_H01_CF_CA_1	PREHAB_H01_CF_CB_1	PREHAB_H01_CF_CC_1	PREHAB_H01_CF_CD_1	PREHAB_H01_CF_CE_1	PREHAB_H01_CF_CF_1	PREHAB_H01_CG_CA_1	PREHAB_H01_CG_CB_1	PREHAB_H01_CG_CC_1	PREHAB_H01_CG_CD_1	PREHAB_H01_CG_CE_1	PREHAB_H01_CG_CF_1	PREHAB_H01_CG_CG_1  PREHAB_H01_CA_CA_2	PREHAB_H01_CB_CA_2	PREHAB_H01_CB_CB_2	PREHAB_H01_CC_CA_2	PREHAB_H01_CC_CB_2	PREHAB_H01_CC_CC_2	PREHAB_H01_CD_CA_2	PREHAB_H01_CD_CB_2	PREHAB_H01_CD_CC_2	PREHAB_H01_CD_CD_2	PREHAB_H01_CE_CA_2	PREHAB_H01_CE_CB_2	PREHAB_H01_CE_CC_2	PREHAB_H01_CE_CD_2	PREHAB_H01_CE_CE_2	PREHAB_H01_CF_CA_2	PREHAB_H01_CF_CB_2	PREHAB_H01_CF_CC_2	PREHAB_H01_CF_CD_2	PREHAB_H01_CF_CE_2	PREHAB_H01_CF_CF_2	PREHAB_H01_CG_CA_2	PREHAB_H01_CG_CB_2	PREHAB_H01_CG_CC_2	PREHAB_H01_CG_CD_2	PREHAB_H01_CG_CE_2	PREHAB_H01_CG_CF_2	PREHAB_H01_CG_CG_2   BUIL_CA_1	BUIL_CB_1	BUIL_CC_1	BUIL_CD_1	BUIL_CE_1	BUIL_CF_1	BUIL_CG_1 BUIL_CA_2	BUIL_CB_2	BUIL_CC_2	BUIL_CD_2	BUIL_CE_2	BUIL_CF_2	BUIL_CG_2 BUIL_1	BUIL_2 CH_13_2	CH_13_1 EXP_REHAB_VAL_1	EXP_REHAB_VAL_2  ENER_BUIL_1	ENER_BUIL_2 ENER_BUIL_21_1	ENER_BUIL_22_1 ENER_BUIL_23_1	ENER_BUIL_24_1	 ENER_BUIL_21_2	ENER_BUIL_22_2	ENER_BUIL_23_2	ENER_BUIL_24_2 PENER_BUIL_H01_22_1	PENER_BUIL_H01_CA_22_1	PENER_BUIL_H01_CB_22_1	PENER_BUIL_H01_CC_22_1	PENER_BUIL_H01_CD_22_1	PENER_BUIL_H01_CE_22_1	PENER_BUIL_H01_CF_22_1	PENER_BUIL_H01_CG_22_1 PENER_BUIL_H01_22_2	PENER_BUIL_H01_CA_22_2	PENER_BUIL_H01_CB_22_2	PENER_BUIL_H01_CC_22_2	PENER_BUIL_H01_CD_22_2	PENER_BUIL_H01_CE_22_2	PENER_BUIL_H01_CF_22_2	PENER_BUIL_H01_CG_22_2 EMS_HH_1	EMS_HH_21_H01_1	EMS_HH_22_H01_1	EMS_HH_24_H01_1 EMS_HH_2	EMS_HH_21_H01_2	EMS_HH_22_H01_2	EMS_HH_24_H01_2 L_S_13_1	L_S_13_2  L_SE_13_1	L_SE_13_2 L_13_1 L_13_2 MAT_13_1  MAT_13_2  E_13_1   E_13_2         R_SUB_RENOV_CB_1	R_SUB_RENOV_CB_2 PREHAB_H01_ca    PREHAB_H01_cb_1  PREHAB_H01_cc_1  PREHAB_H01_cd_1  PREHAB_H01_ce_1  PREHAB_H01_cf_1    PREHAB_H01_cg_1   PREHAB_H01_ca   PREHAB_H01_cb_2  PREHAB_H01_cc_2  PREHAB_H01_cd_2  PREHAB_H01_ce_2  PREHAB_H01_cf_2    PREHAB_H01_cg_2 PENER_BUIL_h01_1 PENER_BUIL_h01_2    PENER_BUIL_21_1	PENER_BUIL_22_1	PENER_BUIL_23_1	PENER_BUIL_24_1			PENER_BUIL_21_2	PENER_BUIL_22_2	PENER_BUIL_23_2	PENER_BUIL_24_2    	ENER_BUIL_H01_CA_22_1	ENER_BUIL_H01_CB_22_1 ENER_BUIL_H01_CC_22_1	ENER_BUIL_H01_CD_22_1	ENER_BUIL_H01_CE_22_1	ENER_BUIL_H01_CF_22_1	ENER_BUIL_H01_CG_22_1 ENER_BUIL_H01_22_2	ENER_BUIL_H01_CA_22_2	ENER_BUIL_H01_CB_22_2	ENER_BUIL_H01_CC_22_2	ENER_BUIL_H01_CD_22_2	ENER_BUIL_H01_CE_22_2	ENER_BUIL_H01_CF_22_2	ENER_BUIL_H01_CG_22_2 tau_REHAB_H01_CA tau_REHAB_H01_CA  tau_REHAB_H01_CB_1 tau_REHAB_H01_CB_2  tau_REHAB_H01_CC_1 tau_REHAB_H01_CC_2  tau_REHAB_H01_CD_1 tau_REHAB_H01_CD_2   tau_REHAB_H01_CE_1 tau_REHAB_H01_CE_2  tau_REHAB_H01_CF_1 tau_REHAB_H01_CF_2  tau_REHAB_H01_CG_1 tau_REHAB_H01_CG_2  DEBT_REHAB_VAL_H01_CA_1	DEBT_REHAB_VAL_H01_CB_1	DEBT_REHAB_VAL_H01_CC_1	DEBT_REHAB_VAL_H01_CD_1	DEBT_REHAB_VAL_H01_CE_1	DEBT_REHAB_VAL_H01_CF_1	DEBT_REHAB_VAL_H01_CG_1	DEBT_REHAB_VAL_H01_CA_2	DEBT_REHAB_VAL_H01_CB_2	DEBT_REHAB_VAL_H01_CC_2	DEBT_REHAB_VAL_H01_CD_2	DEBT_REHAB_VAL_H01_CE_2	DEBT_REHAB_VAL_H01_CF_2	DEBT_REHAB_VAL_H01_CG_2         PHI_REHAB_H01_CB_CA_1 PHI_REHAB_H01_CC_CA_1 PHI_REHAB_H01_CC_CB_1 PHI_REHAB_H01_CD_CA_1 PHI_REHAB_H01_CD_CB_1 PHI_REHAB_H01_CD_CC_1 PHI_REHAB_H01_CE_CA_1 PHI_REHAB_H01_CE_CB_1 PHI_REHAB_H01_CE_CC_1 PHI_REHAB_H01_CE_CD_1 PHI_REHAB_H01_CF_CA_1 PHI_REHAB_H01_CF_CB_1 PHI_REHAB_H01_CF_CC_1 PHI_REHAB_H01_CF_CD_1 PHI_REHAB_H01_CF_CE_1 PHI_REHAB_H01_CG_CA_1 PHI_REHAB_H01_CG_CB_1 PHI_REHAB_H01_CG_CC_1 PHI_REHAB_H01_CG_CD_1 PHI_REHAB_H01_CG_CE_1 PHI_REHAB_H01_CG_CF_1   PHI_REHAB_H01_CB_CA_2 PHI_REHAB_H01_CC_CA_2 PHI_REHAB_H01_CC_CB_2 PHI_REHAB_H01_CD_CA_2 PHI_REHAB_H01_CD_CB_2 PHI_REHAB_H01_CD_CC_2 PHI_REHAB_H01_CE_CA_2 PHI_REHAB_H01_CE_CB_2 PHI_REHAB_H01_CE_CC_2 PHI_REHAB_H01_CE_CD_2 PHI_REHAB_H01_CF_CA_2 PHI_REHAB_H01_CF_CB_2 PHI_REHAB_H01_CF_CC_2 PHI_REHAB_H01_CF_CD_2 PHI_REHAB_H01_CF_CE_2 PHI_REHAB_H01_CG_CA_2 PHI_REHAB_H01_CG_CB_2 PHI_REHAB_H01_CG_CC_2 PHI_REHAB_H01_CG_CD_2 PHI_REHAB_H01_CG_CE_2 PHI_REHAB_H01_CG_CF_2     PAYBACK_REHAB_H01_CB_1 PAYBACK_REHAB_H01_CC_1 PAYBACK_REHAB_H01_CD_1 PAYBACK_REHAB_H01_CE_1 PAYBACK_REHAB_H01_CF_1 PAYBACK_REHAB_H01_CG_1     PAYBACK_REHAB_H01_CB_2 PAYBACK_REHAB_H01_CC_2 PAYBACK_REHAB_H01_CD_2 PAYBACK_REHAB_H01_CE_2 PAYBACK_REHAB_H01_CF_2 PAYBACK_REHAB_H01_CG_2  UC_K_REHAB_H01_CB_1 UC_K_REHAB_H01_CC_1 UC_K_REHAB_H01_CD_1 UC_K_REHAB_H01_CE_1 UC_K_REHAB_H01_CF_1 UC_K_REHAB_H01_CG_1    UC_K_REHAB_H01_CB_2 UC_K_REHAB_H01_CC_2 UC_K_REHAB_H01_CD_2 UC_K_REHAB_H01_CE_2 UC_K_REHAB_H01_CF_2 UC_K_REHAB_H01_CG_2   w_s_13_0 w_s_13_2    PNewBUIL_H01_1  NewBUIL_H01_1   PNewBUIL_H01_2  NewBUIL_H01_2       EXP_13_OTH_VAL_H01_1 EXP_13_OTH_VAL_H01_1  ENER_BUIL_H01_CA_1	ENER_BUIL_H01_CB_1	ENER_BUIL_H01_CC_1	ENER_BUIL_H01_CD_1	ENER_BUIL_H01_CE_1	ENER_BUIL_H01_CF_1	ENER_BUIL_H01_CG_1	ENER_BUIL_H01_CA_2	ENER_BUIL_H01_CB_2	ENER_BUIL_H01_CC_2	ENER_BUIL_H01_CD_2	ENER_BUIL_H01_CE_2	ENER_BUIL_H01_CF_2	ENER_BUIL_H01_CG_2 ENER_BUIL_H01_CA_1	ENER_BUIL_H01_CB_1	ENER_BUIL_H01_CC_1	ENER_BUIL_H01_CD_1	ENER_BUIL_H01_CE_1	ENER_BUIL_H01_CF_1	ENER_BUIL_H01_CG_1	ENER_BUIL_H01_CA_2	ENER_BUIL_H01_CB_2	ENER_BUIL_H01_CC_2	ENER_BUIL_H01_CD_2	ENER_BUIL_H01_CE_2	ENER_BUIL_H01_CF_2	ENER_BUIL_H01_CG_2  ENER_BUIL_H01_CA_24 	ENER_BUIL_H01_CB_24_1 ENER_BUIL_H01_CC_24_1	ENER_BUIL_H01_CD_24_1	ENER_BUIL_H01_CE_24_1	ENER_BUIL_H01_CF_24_1	ENER_BUIL_H01_CG_24_1 ENER_BUIL_H01_24_2	ENER_BUIL_H01_CA_24	ENER_BUIL_H01_CB_24_2	ENER_BUIL_H01_CC_24_2	ENER_BUIL_H01_CD_24_2	ENER_BUIL_H01_CE_24_2	ENER_BUIL_H01_CF_24_2	ENER_BUIL_H01_CG_24_2                                                                                                                             
                                                                                                                                                                                                                                          
'Reporting_MPR.sheet(t)
'show Reporting_MPR

 
group Reporting_finPO PIA_20_1	IR_VAL_1	PIS_1*IS_1	PCSE_TOT_1*CSE_TOT_1	PCSS_1*CSS_1	PCSS_SE_1*CSS_SE_1	I_MDE_exo_20	PRF_01_1(-1)*RF_01_1(-1)	PRF_02_1(-1)*RF_02_1(-1)	PRF_03_1(-1)*RF_03_1(-1)	PRF_04_1(-1)*RF_04_1(-1)	PRF_05_1(-1)*RF_05_1(-1)	PRF_06_1(-1)*RF_06_1(-1)	PRF_07_1(-1)*RF_07_1(-1)	PRF_08_1(-1)*RF_08_1(-1)	PRF_09_1(-1)*RF_09_1(-1)	PRF_10_1(-1)*RF_10_1(-1)	PRF_11_1(-1)*RF_11_1(-1)	PRF_12_1(-1)*RF_12_1(-1)	PRF_13_1(-1)*RF_13_1(-1)	PRF_14_1(-1)*RF_14_1(-1)	PRF_15_1(-1)*RF_15_1(-1)	PRF_16_1(-1)*RF_16_1(-1)	PRF_17_1(-1)*RF_17_1(-1)	PRF_18_1(-1)*RF_18_1(-1)	PRF_19_1(-1)*RF_19_1(-1)	PRF_20_1(-1)*RF_20_1(-1)	PRF_21_1(-1)*RF_21_1(-1)	PRF_2201_1(-1)*RF_2201_1(-1)	PRF_2202_1(-1)*RF_2202_1(-1)	PRF_2301_1(-1)*RF_2301_1(-1)	PRF_2302_1(-1)*RF_2302_1(-1)	PRF_2303_1(-1)*RF_2303_1(-1)	PRF_2304_1(-1)*RF_2304_1(-1)	PRF_2305_1(-1)*RF_2305_1(-1)	PRF_2306_1(-1)*RF_2306_1(-1)	PRF_2307_1(-1)*RF_2307_1(-1)	PRF_2308_1(-1)*RF_2308_1(-1)	PRF_2401_1(-1)*RF_2401_1(-1)	PRF_2402_1(-1)*RF_2402_1(-1)	PRF_2403_1(-1)*RF_2403_1(-1)	PRF_2404_1(-1)*RF_2404_1(-1)	PRF_2405_1(-1)*RF_2405_1(-1)	PRF_2406_1(-1)*RF_2406_1(-1) SUB_AUTO_VAL_H01_2	SUB_AUTO_ELEC_VAL_H01_2	SUB_AUTO_TH_VAL_H01_2	SUB_IRVE_VAL_2	SUB_AUTO_VAL_H01_1	SUB_AUTO_ELEC_VAL_H01_1	SUB_AUTO_TH_VAL_H01_1	SUB_IRVE_VAL_1 SUB_RENOV_VAL_2	SUB_RENOV_VAL_1 TAX_CR_VAL_15_2	TAX_CR_VAL_16_2	TAX_CR_VAL_15_1	TAX_CR_VAL_16_1 TAX_CR_VAL_02_2	TAX_CR_VAL_03_2	TAX_CR_VAL_04_2	TAX_CR_VAL_05_2	TAX_CR_VAL_06_2	TAX_CR_VAL_07_2	TAX_CR_VAL_08_2	TAX_CR_VAL_09_2	TAX_CR_VAL_10_2	TAX_CR_VAL_11_2	TAX_CR_VAL_12_2	TAX_CR_VAL_02_1	TAX_CR_VAL_03_1	TAX_CR_VAL_04_1	TAX_CR_VAL_05_1	TAX_CR_VAL_06_1	TAX_CR_VAL_07_1	TAX_CR_VAL_08_1	TAX_CR_VAL_09_1	TAX_CR_VAL_10_1	TAX_CR_VAL_11_1	TAX_CR_VAL_12_1 	TAX_CR_VAL_2 TAX_CR_VAL_1 PRESOC_DOM_OTH_VAL_DES_1	PRESOC_DOM_U_VAL_1	EXPG_03_1	EXPG_12_1	EXPG_14_1	EXPG_15_1	EXPG_16_1	EXPG_19_1	EXPG_24_1	PEXPG_03_1	PEXPG_12_1	PEXPG_14_1	PEXPG_15_1	PEXPG_16_1	PEXPG_19_1	PEXPG_24_1	PGDP_1	GDP_1	P_1

'Reporting_finPO.sheet(t)
'show Reporting_finPO

 
group Reporting_Logan NEWAUTO_ELEC_H01_1	NEWAUTO_TH_H01_1	NEWAUTO_H01_1	PAUTO_ELEC_H01_1	PNEWAUTO_TH_H01_1	PNEWAUTO_H01_1	PCH_1	AUTO_ELEC_H01_1	AUTO_TH_H01_1	AUTO_1	PENER_AUTOELEC_H01_CA_1	PEXP_AUTO_H01_1	EXP_AUTO_H01_1	EXP_AUTO_H01_22_1	EXP_AUTO_H01_23_1	EXP_AUTO_H01_24_1	Q_MTEP_H_AUTO_22_1	Q_MTEP_H_AUTO_23_1	Q_MTEP_H_AUTO_24_1	KM_AUTO_H01_1	KM_AUTO_ELEC_H01_1	KM_AUTO_TH_H01_1	SUB_AUTO_ELEC_VAL_H01_1	SUB_AUTO_TH_VAL_H01_1	SUB_AUTO_VAL_H01_1	IC_HH_22_H01_1	EXP_NEWAUTO_VAL_H01_1	ENER_BUIL_H01_1	PENER_BUIL_H01_1	EMS_HH_1	Q_MTEP_H_BUIL_1	Q_MTEP_H_BUIL_21_1	Q_MTEP_H_BUIL_22_1	Q_MTEP_H_BUIL_23_1	Q_MTEP_H_BUIL_24_1	BUIL_H01_1	BUIL_H01_CA_1	BUIL_H01_CB_1	BUIL_H01_CC_1	BUIL_H01_CD_1	BUIL_H01_CE_1	BUIL_H01_CF_1	BUIL_H01_CG_1	BUIL_H01_DES_1	REHAB_H01_1	PREHAB_H01_1	RENOV_VAL_1	SUB_RENOV_VAL_1	R_SUB_CEE_H01_CF_CA_1	ER_TERTIARY_1	ER_TERTIARY_COAL_1	ER_TERTIARY_OIL_1	ER_TERTIARY_ELEC_1	ER_TERTIARY_GAS_1	EMS_SEC_TOT_19_1	EMS_SEC_TOT_20_1	ENER_19_1	PENER_19_1	ENER_20_1	PENER_20_1	I_MDE_19_1	I_MDE_20_1	PGDP_1	IA_01_1	IA_02_1	IA_03_1	IA_04_1	IA_05_1	IA_06_1	IA_07_1	IA_08_1	IA_09_1	IA_10_1	IA_11_1	IA_12_1	IA_13_1	IA_14_1	IA_15_1	IA_16_1	IA_17_1	IA_18_1	IA_19_1	IA_20_1	IA_21_1	IA_2201_1	IA_2202_1	IA_2301_1	IA_2302_1	IA_2303_1	IA_2304_1	IA_2305_1	IA_2306_1	IA_2307_1	IA_2308_1	IA_2401_1	IA_2402_1	IA_2403_1	IA_2404_1	IA_2405_1	IA_2406_1	PIA_01_1	PIA_02_1	PIA_03_1	PIA_04_1	PIA_05_1	PIA_06_1	PIA_07_1	PIA_08_1	PIA_09_1	PIA_10_1	PIA_11_1	PIA_12_1	PIA_13_1	PIA_14_1	PIA_15_1	PIA_16_1	PIA_17_1	PIA_18_1	PIA_19_1	PIA_20_1	PIA_21_1	PIA_2201_1	PIA_2202_1	PIA_2301_1	PIA_2302_1	PIA_2303_1	PIA_2304_1	PIA_2305_1	PIA_2306_1	PIA_2307_1	PIA_2308_1	PIA_2401_1	PIA_2402_1	PIA_2403_1	PIA_2404_1	PIA_2405_1	PIA_2406_1	CU_OPEX_01_1	CU_OPEX_02_1	CU_OPEX_03_1	CU_OPEX_04_1	CU_OPEX_05_1	CU_OPEX_06_1	CU_OPEX_07_1	CU_OPEX_08_1	CU_OPEX_09_1	CU_OPEX_10_1	CU_OPEX_11_1	CU_OPEX_12_1	CU_OPEX_13_1	CU_OPEX_14_1	CU_OPEX_15_1	CU_OPEX_16_1	CU_OPEX_17_1	CU_OPEX_18_1	CU_OPEX_19_1	CU_OPEX_20_1	CU_OPEX_21_1	CU_OPEX_2201_1	CU_OPEX_2202_1	CU_OPEX_2301_1	CU_OPEX_2302_1	CU_OPEX_2303_1	CU_OPEX_2304_1	CU_OPEX_2305_1	CU_OPEX_2306_1	CU_OPEX_2307_1	CU_OPEX_2308_1	CU_OPEX_2401_1	CU_OPEX_2402_1	CU_OPEX_2403_1	CU_OPEX_2404_1	CU_OPEX_2405_1	CU_OPEX_2406_1	Y_01_1	Y_02_1	Y_03_1	Y_04_1	Y_05_1	Y_06_1	Y_07_1	Y_08_1	Y_09_1	Y_10_1	Y_11_1	Y_12_1	Y_13_1	Y_14_1	Y_15_1	Y_16_1	Y_17_1	Y_18_1	Y_19_1	Y_20_1	Y_21_1	Y_2201_1	Y_2202_1	Y_2301_1	Y_2302_1	Y_2303_1	Y_2304_1	Y_2305_1	Y_2306_1	Y_2307_1	Y_2308_1	Y_2401_1	Y_2402_1	Y_2403_1	Y_2404_1	Y_2405_1	Y_2406_1	PY_01_1	PY_02_1	PY_03_1	PY_04_1	PY_05_1	PY_06_1	PY_07_1	PY_08_1	PY_09_1	PY_10_1	PY_11_1	PY_12_1	PY_13_1	PY_14_1	PY_15_1	PY_16_1	PY_17_1	PY_18_1	PY_19_1	PY_20_1	PY_21_1	PY_2201_1	PY_2202_1	PY_2301_1	PY_2302_1	PY_2303_1	PY_2304_1	PY_2305_1	PY_2306_1	PY_2307_1	PY_2308_1	PY_2401_1	PY_2402_1	PY_2403_1	PY_2404_1	PY_2405_1	PY_2406_1	L_01_1	L_02_1	L_03_1	L_04_1	L_05_1	L_06_1	L_07_1	L_08_1	L_09_1	L_10_1	L_11_1	L_12_1	L_13_1	L_14_1	L_15_1	L_16_1	L_17_1	L_18_1	L_19_1	L_20_1	L_21_1	L_2201_1	L_2202_1	L_2301_1	L_2302_1	L_2303_1	L_2304_1	L_2305_1	L_2306_1	L_2307_1	L_2308_1	L_2401_1	L_2402_1	L_2403_1	L_2404_1	L_2405_1	L_2406_1	CL_01_1	CL_02_1	CL_03_1	CL_04_1	CL_05_1	CL_06_1	CL_07_1	CL_08_1	CL_09_1	CL_10_1	CL_11_1	CL_12_1	CL_13_1	CL_14_1	CL_15_1	CL_16_1	CL_17_1	CL_18_1	CL_19_1	CL_20_1	CL_21_1	CL_2201_1	CL_2202_1	CL_2301_1	CL_2302_1	CL_2303_1	CL_2304_1	CL_2305_1	CL_2306_1	CL_2307_1	CL_2308_1	CL_2401_1	CL_2402_1	CL_2403_1	CL_2404_1	CL_2405_1	CL_2406_1	PENER_01_1	PENER_02_1	PENER_03_1	PENER_04_1	PENER_05_1	PENER_06_1	PENER_07_1	PENER_08_1	PENER_09_1	PENER_10_1	PENER_11_1	PENER_12_1	PENER_13_1	PENER_14_1	PENER_15_1	PENER_16_1	PENER_17_1	PENER_18_1	PENER_19_1	PENER_20_1	PENER_21_1	PENER_2201_1	PENER_2202	PENER_2301_1	PENER_2302_1	PENER_2303_1	PENER_2304_1	PENER_2305_1	PENER_2306_1	PENER_2307_1	PENER_2308_1	PENER_2401_1	PENER_2402	PENER_2403	PENER_2404	PENER_2405	PENER_2406	ENER_01_1	ENER_02_1	ENER_03_1	ENER_04_1	ENER_05_1	ENER_06_1	ENER_07_1	ENER_08_1	ENER_09_1	ENER_10_1	ENER_11_1	ENER_12_1	ENER_13_1	ENER_14_1	ENER_15_1	ENER_16_1	ENER_17_1	ENER_18_1	ENER_19_1	ENER_20_1	ENER_21_1	ENER_2201_1	ENER_2202_1	ENER_2301_1	ENER_2302_1	ENER_2303_1	ENER_2304_1	ENER_2305_1	ENER_2306_1	ENER_2307_1	ENER_2308_1	ENER_2401_1	ENER_2402_1	ENER_2403_1	ENER_2404_1	ENER_2405_1	ENER_2406_1	PMAT_01_1	PMAT_02_1	PMAT_03_1	PMAT_04_1	PMAT_05_1	PMAT_06_1	PMAT_07_1	PMAT_08_1	PMAT_09_1	PMAT_10_1	PMAT_11_1	PMAT_12_1	PMAT_13_1	PMAT_14_1	PMAT_15_1	PMAT_16_1	PMAT_17_1	PMAT_18_1	PMAT_19_1	PMAT_20_1	PMAT_21_1	PMAT_2201_1	PMAT_2202_1	PMAT_2301_1	PMAT_2302_1	PMAT_2303_1	PMAT_2304_1	PMAT_2305_1	PMAT_2306_1	PMAT_2307_1	PMAT_2308_1	PMAT_2401_1	PMAT_2402_1	PMAT_2403_1	PMAT_2404_1	PMAT_2405_1	PMAT_2406_1	MAT_01_1	MAT_02_1	MAT_03_1	MAT_04_1	MAT_05_1	MAT_06_1	MAT_07_1	MAT_08_1	MAT_09_1	MAT_10_1	MAT_11_1	MAT_12_1	MAT_13_1	MAT_14_1	MAT_15_1	MAT_16_1	MAT_17_1	MAT_18_1	MAT_19_1	MAT_20_1	MAT_21_1	MAT_2201_1	MAT_2202_1	MAT_2301_1	MAT_2302_1	MAT_2303_1	MAT_2304_1	MAT_2305_1	MAT_2306_1	MAT_2307_1	MAT_2308_1	MAT_2401_1	MAT_2402_1	MAT_2403_1	MAT_2404_1	MAT_2405_1	MAT_2406_1	Q_MTEP_SEC_01_1	Q_MTEP_SEC_02_1	Q_MTEP_SEC_03_1	Q_MTEP_SEC_04_1	Q_MTEP_SEC_05_1	Q_MTEP_SEC_06_1	Q_MTEP_SEC_07_1	Q_MTEP_SEC_08_1	Q_MTEP_SEC_09_1	Q_MTEP_SEC_10_1	Q_MTEP_SEC_11_1	Q_MTEP_SEC_12_1	Q_MTEP_SEC_13_1	Q_MTEP_SEC_14_1	Q_MTEP_SEC_15_1	Q_MTEP_SEC_16_1	Q_MTEP_SEC_17_1	Q_MTEP_SEC_18_1	Q_MTEP_SEC_19_1	Q_MTEP_SEC_20_1	EMS_SEC_TOT_01_1	EMS_SEC_TOT_02_1	EMS_SEC_TOT_03_1	EMS_SEC_TOT_04_1	EMS_SEC_TOT_05_1	EMS_SEC_TOT_06_1	EMS_SEC_TOT_07_1	EMS_SEC_TOT_08_1	EMS_SEC_TOT_09_1	EMS_SEC_TOT_10_1	EMS_SEC_TOT_11_1	EMS_SEC_TOT_12_1	EMS_SEC_TOT_13_1	EMS_SEC_TOT_14_1	EMS_SEC_TOT_15_1	EMS_SEC_TOT_16_1	EMS_SEC_TOT_17_1	EMS_SEC_TOT_18_1	EMS_SEC_TOT_19_1	EMS_SEC_TOT_20_1	EMS_SEC_TOT_21_1	EMS_SEC_TOT_2201_1	EMS_SEC_TOT_2202_1	EMS_SEC_TOT_2301_1	EMS_SEC_TOT_2302_1	EMS_SEC_TOT_2303_1	EMS_SEC_TOT_2304_1	EMS_SEC_TOT_2305_1	EMS_SEC_TOT_2306_1	EMS_SEC_TOT_2307_1	EMS_SEC_TOT_2308_1	EMS_SEC_TOT_2401_1	EMS_SEC_TOT_2402_1	EMS_SEC_TOT_2403_1	EMS_SEC_TOT_2404_1	EMS_SEC_TOT_2405_1	EMS_SEC_TOT_2406_1	TAX_CR_VAL_01	TAX_CR_VAL_02_1	TAX_CR_VAL_03_1	TAX_CR_VAL_04_1	TAX_CR_VAL_05_1	TAX_CR_VAL_06_1	TAX_CR_VAL_07_1	TAX_CR_VAL_08_1	TAX_CR_VAL_09_1	TAX_CR_VAL_10_1	TAX_CR_VAL_11_1	TAX_CR_VAL_12_1	TAX_CR_VAL_13	TAX_CR_VAL_14	TAX_CR_VAL_15_1	TAX_CR_VAL_16_1	TAX_CR_VAL_17	TAX_CR_VAL_18	TAX_CR_VAL_19_1	TAX_CR_VAL_20	I_MDE_01_1	I_MDE_02_1	I_MDE_03_1	I_MDE_04_1	I_MDE_05_1	I_MDE_06_1	I_MDE_07_1	I_MDE_08_1	I_MDE_09_1	I_MDE_10_1	I_MDE_11_1	I_MDE_12_1	I_MDE_13_1	I_MDE_14_1	I_MDE_15_1	I_MDE_16_1	I_MDE_17_1	I_MDE_18_1	ER_COAL_1	ER_OIL_2201_1	ER_OIL_2202_1	ER_ELEC_2301_1	ER_ELEC_2302_1	ER_ELEC_2303_1	ER_ELEC_2304_1	ER_ELEC_2305_1	ER_ELEC_2306_1	ER_ELEC_2307_1	ER_ELEC_2308_1	ER_GAS_2401_1	ER_GAS_2402_1	ER_GAS_2403_1	ER_GAS_2404_1	ER_GAS_2405_1	ER_GAS_2406_1	CU_MWH_PGDP_21_1	CU_MWH_PGDP_22_1	CU_MWH_PGDP_23_1	CU_MWH_PGDP_24_1	CU_MWH_PGDP_2201_1	CU_MWH_PGDP_2202_1	CU_MWH_PGDP_2301_1	CU_MWH_PGDP_2302_1	CU_MWH_PGDP_2303_1	CU_MWH_PGDP_2304_1	CU_MWH_PGDP_2305_1	CU_MWH_PGDP_2306_1	CU_MWH_PGDP_2307_1	CU_MWH_PGDP_2308_1	CU_MWH_PGDP_2401_1	CU_MWH_PGDP_2402_1	CU_MWH_PGDP_2403_1	CU_MWH_PGDP_2404_1	CU_MWH_PGDP_2405_1	CU_MWH_PGDP_2406_1	Q_MTEP_SEC_22_16_1	Q_MTEP_SEC_23_16_1	Q_MTEP_SEC_24_16_1	MAT_N_06_01_1/Y_01_1	MAT_N_06_02_1/Y_02_1	MAT_N_06_03_1/Y_03_1	MAT_N_06_04_1/Y_04_1	MAT_N_06_05_1/Y_05_1	MAT_N_06_06_1/Y_06_1	MAT_N_06_07_1/Y_07_1	MAT_N_06_08_1/Y_08_1	MAT_N_06_09_1/Y_09_1	MAT_N_06_11_1/Y_11_1	MAT_N_06_12_1/Y_12_1	MAT_N_06_13_1/Y_13_1	MAT_N_06_14_1/Y_14_1	MAT_N_06_15_1/Y_15_1	MAT_N_06_16_1/Y_16_1	MAT_N_06_17_1/Y_17_1	MAT_N_06_18_1/Y_18_1	MAT_N_06_19_1/Y_19_1	MAT_N_06_20_1/Y_20_1	MAT_N_07_01_1/Y_01_1	MAT_N_07_02_1/Y_02_1	MAT_N_07_03_1/Y_03_1	MAT_N_07_04_1/Y_04_1	MAT_N_07_05_1/Y_05_1	MAT_N_07_06_1/Y_06_1	MAT_N_07_07_1/Y_07_1	MAT_N_07_08_1/Y_08_1	MAT_N_07_09_1/Y_09_1	MAT_N_07_10_1/Y_10_1	MAT_N_07_11_1/Y_11_1	MAT_N_07_12_1/Y_12_1	MAT_N_07_13_1/Y_13_1	MAT_N_07_14_1/Y_14_1	MAT_N_07_15_1/Y_15_1	MAT_N_07_16_1/Y_16_1	MAT_N_07_17_1/Y_17_1	MAT_N_07_18_1/Y_18_1	MAT_N_07_19_1/Y_19_1	MAT_N_07_20_1/Y_20_1	MAT_N_08_02_1/Y_02_1	MAT_N_08_03_1/Y_03_1	MAT_N_08_04_1/Y_04_1	MAT_N_08_05_1/Y_05_1	MAT_N_08_06_1/Y_06_1	MAT_N_08_07_1/Y_07_1	MAT_N_08_08_1/Y_08_1	MAT_N_08_09_1/Y_09_1	MAT_N_08_10_1/Y_10_1	MAT_N_08_11_1/Y_11_1	MAT_N_08_12_1/Y_12_1	MAT_N_08_13_1/Y_13_1	MAT_N_08_14_1/Y_14_1	MAT_N_08_15_1/Y_15_1	MAT_N_08_16_1/Y_16_1	MAT_N_08_17_1/Y_17_1	MAT_N_08_18_1/Y_18_1	MAT_N_08_19_1/Y_19_1	MAT_N_08_20_1/Y_20_1	MAT_N_09_01_1/Y_01_1	MAT_N_09_02_1/Y_02_1	MAT_N_09_03_1/Y_03_1	MAT_N_09_04_1/Y_04_1	MAT_N_09_05_1/Y_05_1	MAT_N_09_06_1/Y_06_1	MAT_N_09_07_1/Y_07_1	MAT_N_09_08_1/Y_08_1	MAT_N_09_09_1/Y_09_1	MAT_N_09_10_1/Y_10_1	MAT_N_09_11_1/Y_11_1	MAT_N_09_12_1/Y_12_1	MAT_N_09_13_1/Y_13_1	MAT_N_09_14_1/Y_14_1	MAT_N_09_15_1/Y_15_1	MAT_N_09_16_1/Y_16_1	MAT_N_09_17_1/Y_17_1	MAT_N_09_18_1/Y_18_1	MAT_N_09_19_1/Y_19_1	MAT_N_09_20_1/Y_20_1	MAT_N_14_01_1/Y_01_1	MAT_N_14_02_1/Y_02_1	MAT_N_14_03_1/Y_03_1	MAT_N_14_04_1/Y_04_1	MAT_N_14_05_1/Y_05_1	MAT_N_14_06_1/Y_06_1	MAT_N_14_07_1/Y_07_1	MAT_N_14_08_1/Y_08_1	MAT_N_14_09_1/Y_09_1	MAT_N_14_10_1/Y_10_1	MAT_N_14_11_1/Y_11_1	MAT_N_14_12_1/Y_12_1	MAT_N_14_13_1/Y_13_1	MAT_N_14_14_1/Y_14_1	MAT_N_14_15_1/Y_15_1	MAT_N_14_16_1/Y_16_1	MAT_N_14_17_1/Y_17_1	MAT_N_14_18_1/Y_18_1	MAT_N_14_19_1/Y_19_1	MAT_N_14_20_1/Y_20_1	MAT_N_16_01_1/Y_01_1	MAT_N_16_02_1/Y_02_1	MAT_N_16_03_1/Y_03_1	MAT_N_16_04_1/Y_04_1	MAT_N_16_05_1/Y_05_1	MAT_N_16_06_1/Y_06_1	MAT_N_16_07_1/Y_07_1	MAT_N_16_08_1/Y_08_1	MAT_N_16_09_1/Y_09_1	MAT_N_16_10_1/Y_10_1	MAT_N_16_11_1/Y_11_1	MAT_N_16_12_1/Y_12_1	MAT_N_16_13_1/Y_13_1	MAT_N_16_14_1/Y_14_1	MAT_N_16_15_1/Y_15_1	MAT_N_16_16_1/Y_16_1	MAT_N_16_17_1/Y_17_1	MAT_N_16_18_1/Y_18_1	MAT_N_16_19_1/Y_19_1	MAT_N_16_20_1/Y_20_1	MAT_N_17_02_1/Y_02_1	MAT_N_17_03_1/Y_03_1	MAT_N_17_04_1/Y_04_1	MAT_N_17_05_1/Y_05_1	MAT_N_17_06_1/Y_06_1	MAT_N_17_07_1/Y_07_1	MAT_N_17_08_0/Y_08_0	MAT_N_17_09_1/Y_09_1	MAT_N_17_10_1/Y_10_1	MAT_N_17_11_1/Y_11_1	MAT_N_17_12_1/Y_12_1	MAT_N_17_13_1/Y_13_1	MAT_N_17_14_1/Y_14_1	MAT_N_17_15_1/Y_15_1	MAT_N_17_16_1/Y_16_1	MAT_N_17_17_1/Y_17_1	MAT_N_17_18_1/Y_18_1	MAT_N_17_19_1/Y_19_1	MAT_N_17_20_1/Y_20_1	MAT_N_01_13_1/Y_13_1	MAT_N_02_13_1/Y_13_1	MAT_N_03_13_1/Y_13_1	MAT_N_04_13_1/Y_13_1	MAT_N_05_13_1/Y_13_1	MAT_N_06_13_1/Y_13_1	MAT_N_07_13_1/Y_13_1	MAT_N_08_13_1/Y_13_1	MAT_N_09_13_1/Y_13_1	MAT_N_10_13_1/Y_13_1	MAT_N_11_13_1/Y_13_1	MAT_N_12_13_1/Y_13_1	MAT_N_13_13_1/Y_13_1	MAT_N_14_13_1/Y_13_1	MAT_N_15_13_1/Y_13_1	MAT_N_16_13_1/Y_13_1	MAT_N_17_13_1/Y_13_1	MAT_N_18_13_1/Y_13_1	MAT_N_19_13_1/Y_13_1	MAT_N_20_13_1/Y_13_1	EXP_13_1	PEXP_13_H01_1	NEWBUIL_H01_1	PNEWBUIL_H01_1	NEW_TRUCKS_15_1	NEW_TRUCKS_16_1	NEW_LUV_16_1	TRUCKS_15_1	TRUCKS_22_15_1	TRUCKS_23_15_1	TRUCKS_24_15_1	NEW_TRUCKS_22_15_1	NEW_TRUCKS_23_15_1	NEW_TRUCKS_24_15_1	TRUCKS_16_1	TRUCKS_22_16_1	TRUCKS_23_16_1	TRUCKS_24_16_1	NEW_TRUCKS_22_16_1	NEW_TRUCKS_23_16_1	NEW_TRUCKS_24_16_1	PNEW_TRUCKS_22_15_1	PNEW_TRUCKS_23_15_1	PNEW_TRUCKS_24_15_1	PNEW_TRUCKS_22_16_1	PNEW_TRUCKS_23_16_1	PNEW_TRUCKS_24_16_1	LUV_16_1	LUV_22_16_1	LUV_23_16_1	LUV_24_16_1	PNEW_LUV_22_16_1	PNEW_LUV_23_16_1	PNEW_LUV_24_16_1	NEW_LUV_22_16_1	NEW_LUV_23_16_1	NEW_LUV_24_16_1	YQ_01_1	YQ_02_1	YQ_03_1	YQ_04_1	YQ_05_1	YQ_06_1	YQ_07_1	YQ_08_1	YQ_09_1	YQ_10_1	YQ_11_1	YQ_12_1	YQ_13_1	YQ_14_1	YQ_15_1	YQ_16_1	YQ_17_1	YQ_18_1	YQ_19_1	YQ_20_1	PYQ_01_1	PYQ_02_1	PYQ_03_1	PYQ_04_1	PYQ_05_1	PYQ_06_1	PYQ_07_1	PYQ_08_1	PYQ_09_1	PYQ_10_1	PYQ_11_1	PYQ_12_1	PYQ_13_1	PYQ_14_1	PYQ_15_1	PYQ_16_1	PYQ_17_1	PYQ_18_1	PYQ_19_1	PYQ_20_1	PMAT_03_15_1	PMAT_03_16_1	PINV_IRVE_1	INV_IRVE_1 NEWAUTO_ELEC_H01_2	 NEWAUTO_TH_H01_2	NEWAUTO_H01_2	PAUTO_ELEC_H01_2	PNEWAUTO_TH_H01_2	PNEWAUTO_H01_2	PCH_2	AUTO_ELEC_H01_2	AUTO_TH_H01_2	AUTO_2	PENER_AUTOELEC_H01_CA_2	PEXP_AUTO_H01_2	EXP_AUTO_H01_2	EXP_AUTO_H01_22_2	EXP_AUTO_H01_23_2	EXP_AUTO_H01_24_2	Q_MTEP_H_AUTO_22_2	Q_MTEP_H_AUTO_23_2	Q_MTEP_H_AUTO_24_2	KM_AUTO_H01_2	KM_AUTO_ELEC_H01_2	KM_AUTO_TH_H01_2	SUB_AUTO_ELEC_VAL_H01_2	SUB_AUTO_TH_VAL_H01_2	SUB_AUTO_VAL_H01_2	IC_HH_22_H01_2	EXP_NEWAUTO_VAL_H01_2	ENER_BUIL_H01_2	PENER_BUIL_H01_2	EMS_HH_2	Q_MTEP_H_BUIL_2	Q_MTEP_H_BUIL_21_2	Q_MTEP_H_BUIL_22_2	Q_MTEP_H_BUIL_23_2	Q_MTEP_H_BUIL_24_2	BUIL_H01_2	BUIL_H01_CA_2	BUIL_H01_CB_2	BUIL_H01_CC_2	BUIL_H01_CD_2	BUIL_H01_CE_2	BUIL_H01_CF_2	BUIL_H01_CG_2	BUIL_H01_DES_2	REHAB_H01_2	PREHAB_H01_2	RENOV_VAL_2	SUB_RENOV_VAL_2	R_SUB_CEE_H01_CF_CA_2	ER_TERTIARY_2	ER_TERTIARY_COAL_2	ER_TERTIARY_OIL_2	ER_TERTIARY_ELEC_2	ER_TERTIARY_GAS_2	EMS_SEC_TOT_19_2	EMS_SEC_TOT_20_2	ENER_19_2	PENER_19_2	ENER_20_2	PENER_20_2	I_MDE_19_2	I_MDE_20_2	PGDP_2	IA_01_2	IA_02_2	IA_03_2	IA_04_2	IA_05_2	IA_06_2	IA_07_2	IA_08_2	IA_09_2	IA_10_2	IA_11_2	IA_12_2	IA_13_2	IA_14_2	IA_15_2	IA_16_2	IA_17_2	IA_18_2	IA_19_2	IA_20_2	IA_21_2	IA_2201_2	IA_2202_2	IA_2301_2	IA_2302_2	IA_2303_2	IA_2304_2	IA_2305_2	IA_2306_2	IA_2307_2	IA_2308_2	IA_2401_2	IA_2402_2	IA_2403_2	IA_2404_2	IA_2405_2	IA_2406_2	PIA_01_2	PIA_02_2	PIA_03_2	PIA_04_2	PIA_05_2	PIA_06_2	PIA_07_2	PIA_08_2	PIA_09_2	PIA_10_2	PIA_11_2	PIA_12_2	PIA_13_2	PIA_14_2	PIA_15_2	PIA_16_2	PIA_17_2	PIA_18_2	PIA_19_2	PIA_20_2	PIA_21_2	PIA_2201_2	PIA_2202_2	PIA_2301_2	PIA_2302_2	PIA_2303_2	PIA_2304_2	PIA_2305_2	PIA_2306_2	PIA_2307_2	PIA_2308_2	PIA_2401_2	PIA_2402_2	PIA_2403_2	PIA_2404_2	PIA_2405_2	PIA_2406_2	CU_OPEX_01_2	CU_OPEX_02_2	CU_OPEX_03_2	CU_OPEX_04_2	CU_OPEX_05_2	CU_OPEX_06_2	CU_OPEX_07_2	CU_OPEX_08_2	CU_OPEX_09_2	CU_OPEX_10_2	CU_OPEX_11_2	CU_OPEX_12_2	CU_OPEX_13_2	CU_OPEX_14_2	CU_OPEX_15_2	CU_OPEX_16_2	CU_OPEX_17_2	CU_OPEX_18_2	CU_OPEX_19_2	CU_OPEX_20_2	CU_OPEX_21_2	CU_OPEX_2201_2	CU_OPEX_2202_2	CU_OPEX_2301_2	CU_OPEX_2302_2	CU_OPEX_2303_2	CU_OPEX_2304_2	CU_OPEX_2305_2	CU_OPEX_2306_2	CU_OPEX_2307_2	CU_OPEX_2308_2	CU_OPEX_2401_2	CU_OPEX_2402_2	CU_OPEX_2403_2	CU_OPEX_2404_2	CU_OPEX_2405_2	CU_OPEX_2406_2	Y_01_2	Y_02_2	Y_03_2	Y_04_2	Y_05_2	Y_06_2	Y_07_2	Y_08_2	Y_09_2	Y_10_2	Y_11_2	Y_12_2	Y_13_2	Y_14_2	Y_15_2	Y_16_2	Y_17_2	Y_18_2	Y_19_2	Y_20_2	Y_21_2	Y_2201_2	Y_2202_2	Y_2301_2	Y_2302_2	Y_2303_2	Y_2304_2	Y_2305_2	Y_2306_2	Y_2307_2	Y_2308_2	Y_2401_2	Y_2402_2	Y_2403_2	Y_2404_2	Y_2405_2	Y_2406_2	PY_01_2	PY_02_2	PY_03_2	PY_04_2	PY_05_2	PY_06_2	PY_07_2	PY_08_2	PY_09_2	PY_10_2	PY_11_2	PY_12_2	PY_13_2	PY_14_2	PY_15_2	PY_16_2	PY_17_2	PY_18_2	PY_19_2	PY_20_2	PY_21_2	PY_2201_2	PY_2202_2	PY_2301_2	PY_2302_2	PY_2303_2	PY_2304_2	PY_2305_2	PY_2306_2	PY_2307_2	PY_2308_2	PY_2401_2	PY_2402_2	PY_2403_2	PY_2404_2	PY_2405_2	PY_2406_2	L_01_2	L_02_2	L_03_2	L_04_2	L_05_2	L_06_2	L_07_2	L_08_2	L_09_2	L_10_2	L_11_2	L_12_2	L_13_2	L_14_2	L_15_2	L_16_2	L_17_2	L_18_2	L_19_2	L_20_2	L_21_2	L_2201_2	L_2202_2	L_2301_2	L_2302_2	L_2303_2	L_2304_2	L_2305_2	L_2306_2	L_2307_2	L_2308_2	L_2401_2	L_2402_2	L_2403_2	L_2404_2	L_2405_2	L_2406_2	CL_01_2	CL_02_2	CL_03_2	CL_04_2	CL_05_2	CL_06_2	CL_07_2	CL_08_2	CL_09_2	CL_10_2	CL_11_2	CL_12_2	CL_13_2	CL_14_2	CL_15_2	CL_16_2	CL_17_2	CL_18_2	CL_19_2	CL_20_2	CL_21_2	CL_2201_2	CL_2202_2	CL_2301_2	CL_2302_2	CL_2303_2	CL_2304_2	CL_2305_2	CL_2306_2	CL_2307_2	CL_2308_2	CL_2401_2	CL_2402_2	CL_2403_2	CL_2404_2	CL_2405_2	CL_2406_2	PENER_01_2	PENER_02_2	PENER_03_2	PENER_04_2	PENER_05_2	PENER_06_2	PENER_07_2	PENER_08_2	PENER_09_2	PENER_10_2	PENER_11_2	PENER_12_2	PENER_13_2	PENER_14_2	PENER_15_2	PENER_16_2	PENER_17_2	PENER_18_2	PENER_19_2	PENER_20_2	PENER_21_2	PENER_2201_2	PENER_2202	PENER_2301_2	PENER_2302_2	PENER_2303_2	PENER_2304_2	PENER_2305_2	PENER_2306_2	PENER_2307_2	PENER_2308_2	PENER_2401_2	PENER_2402	PENER_2403	PENER_2404	PENER_2405	PENER_2406	ENER_01_2	ENER_02_2	ENER_03_2	ENER_04_2	ENER_05_2	ENER_06_2	ENER_07_2	ENER_08_2	ENER_09_2	ENER_10_2	ENER_11_2	ENER_12_2	ENER_13_2	ENER_14_2	ENER_15_2	ENER_16_2	ENER_17_2	ENER_18_2	ENER_19_2	ENER_20_2	ENER_21_2	ENER_2201_2	ENER_2202_2	ENER_2301_2	ENER_2302_2	ENER_2303_2	ENER_2304_2	ENER_2305_2	ENER_2306_2	ENER_2307_2	ENER_2308_2	ENER_2401_2	ENER_2402_2	ENER_2403_2	ENER_2404_2	ENER_2405_2	ENER_2406_2	PMAT_01_2	PMAT_02_2	PMAT_03_2	PMAT_04_2	PMAT_05_2	PMAT_06_2	PMAT_07_2	PMAT_08_2	PMAT_09_2	PMAT_10_2	PMAT_11_2	PMAT_12_2	PMAT_13_2	PMAT_14_2	PMAT_15_2	PMAT_16_2	PMAT_17_2	PMAT_18_2	PMAT_19_2	PMAT_20_2	PMAT_21_2	PMAT_2201_2	PMAT_2202_2	PMAT_2301_2	PMAT_2302_2	PMAT_2303_2	PMAT_2304_2	PMAT_2305_2	PMAT_2306_2	PMAT_2307_2	PMAT_2308_2	PMAT_2401_2	PMAT_2402_2	PMAT_2403_2	PMAT_2404_2	PMAT_2405_2	PMAT_2406_2	MAT_01_2	MAT_02_2	MAT_03_2	MAT_04_2	MAT_05_2	MAT_06_2	MAT_07_2	MAT_08_2	MAT_09_2	MAT_10_2	MAT_11_2	MAT_12_2	MAT_13_2	MAT_14_2	MAT_15_2	MAT_16_2	MAT_17_2	MAT_18_2	MAT_19_2	MAT_20_2	MAT_21_2	MAT_2201_2	MAT_2202_2	MAT_2301_2	MAT_2302_2	MAT_2303_2	MAT_2304_2	MAT_2305_2	MAT_2306_2	MAT_2307_2	MAT_2308_2	MAT_2401_2	MAT_2402_2	MAT_2403_2	MAT_2404_2	MAT_2405_2	MAT_2406_2	Q_MTEP_SEC_01_2	Q_MTEP_SEC_02_2	Q_MTEP_SEC_03_2	Q_MTEP_SEC_04_2	Q_MTEP_SEC_05_2	Q_MTEP_SEC_06_2	Q_MTEP_SEC_07_2	Q_MTEP_SEC_08_2	Q_MTEP_SEC_09_2	Q_MTEP_SEC_10_2	Q_MTEP_SEC_11_2	Q_MTEP_SEC_12_2	Q_MTEP_SEC_13_2	Q_MTEP_SEC_14_2	Q_MTEP_SEC_15_2	Q_MTEP_SEC_16_2	Q_MTEP_SEC_17_2	Q_MTEP_SEC_18_2	Q_MTEP_SEC_19_2	Q_MTEP_SEC_20_2	EMS_SEC_TOT_01_2	EMS_SEC_TOT_02_2	EMS_SEC_TOT_03_2	EMS_SEC_TOT_04_2	EMS_SEC_TOT_05_2	EMS_SEC_TOT_06_2	EMS_SEC_TOT_07_2	EMS_SEC_TOT_08_2	EMS_SEC_TOT_09_2	EMS_SEC_TOT_10_2	EMS_SEC_TOT_11_2	EMS_SEC_TOT_12_2	EMS_SEC_TOT_13_2	EMS_SEC_TOT_14_2	EMS_SEC_TOT_15_2	EMS_SEC_TOT_16_2	EMS_SEC_TOT_17_2	EMS_SEC_TOT_18_2	EMS_SEC_TOT_19_2	EMS_SEC_TOT_20_2	EMS_SEC_TOT_21_2	EMS_SEC_TOT_2201_2	EMS_SEC_TOT_2202_2	EMS_SEC_TOT_2301_2	EMS_SEC_TOT_2302_2	EMS_SEC_TOT_2303_2	EMS_SEC_TOT_2304_2	EMS_SEC_TOT_2305_2	EMS_SEC_TOT_2306_2	EMS_SEC_TOT_2307_2	EMS_SEC_TOT_2308_2	EMS_SEC_TOT_2401_2	EMS_SEC_TOT_2402_2	EMS_SEC_TOT_2403_2	EMS_SEC_TOT_2404_2	EMS_SEC_TOT_2405_2	EMS_SEC_TOT_2406_2	TAX_CR_VAL_01	TAX_CR_VAL_02_2	TAX_CR_VAL_03_2	TAX_CR_VAL_04_2	TAX_CR_VAL_05_2	TAX_CR_VAL_06_2	TAX_CR_VAL_07_2	TAX_CR_VAL_08_2	TAX_CR_VAL_09_2	TAX_CR_VAL_10_2	TAX_CR_VAL_11_2	TAX_CR_VAL_12_2	TAX_CR_VAL_13	TAX_CR_VAL_14	TAX_CR_VAL_15_2	TAX_CR_VAL_16_2	TAX_CR_VAL_17	TAX_CR_VAL_18	TAX_CR_VAL_19_2	TAX_CR_VAL_20	I_MDE_01_2	I_MDE_02_2	I_MDE_03_2	I_MDE_04_2	I_MDE_05_2	I_MDE_06_2	I_MDE_07_2	I_MDE_08_2	I_MDE_09_2	I_MDE_10_2	I_MDE_11_2	I_MDE_12_2	I_MDE_13_2	I_MDE_14_2	I_MDE_15_2	I_MDE_16_2	I_MDE_17_2	I_MDE_18_2	ER_COAL_2	ER_OIL_2201_2	ER_OIL_2202_2	ER_ELEC_2301_2	ER_ELEC_2302_2	ER_ELEC_2303_2	ER_ELEC_2304_2	ER_ELEC_2305_2	ER_ELEC_2306_2	ER_ELEC_2307_2	ER_ELEC_2308_2	ER_GAS_2401_2	ER_GAS_2402_2	ER_GAS_2403_2	ER_GAS_2404_2	ER_GAS_2405_2	ER_GAS_2406_2	CU_MWH_PGDP_21_2	CU_MWH_PGDP_22_2	CU_MWH_PGDP_23_2	CU_MWH_PGDP_24_2	CU_MWH_PGDP_2201_2	CU_MWH_PGDP_2202_2	CU_MWH_PGDP_2301_2	CU_MWH_PGDP_2302_2	CU_MWH_PGDP_2303_2	CU_MWH_PGDP_2304_2	CU_MWH_PGDP_2305_2	CU_MWH_PGDP_2306_2	CU_MWH_PGDP_2307_2	CU_MWH_PGDP_2308_2	CU_MWH_PGDP_2401_2	CU_MWH_PGDP_2402_2	CU_MWH_PGDP_2403_2	CU_MWH_PGDP_2404_2	CU_MWH_PGDP_2405_2	CU_MWH_PGDP_2406_2	Q_MTEP_SEC_22_16_2	Q_MTEP_SEC_23_16_2	Q_MTEP_SEC_24_16_2	MAT_N_06_01_2/Y_01_2	MAT_N_06_02_2/Y_02_2	MAT_N_06_03_2/Y_03_2	MAT_N_06_04_2/Y_04_2	MAT_N_06_05_2/Y_05_2	MAT_N_06_06_2/Y_06_2	MAT_N_06_07_2/Y_07_2	MAT_N_06_08_2/Y_08_2	MAT_N_06_09_2/Y_09_2	MAT_N_06_11_2/Y_11_2	MAT_N_06_12_2/Y_12_2	MAT_N_06_13_2/Y_13_2	MAT_N_06_14_2/Y_14_2	MAT_N_06_15_2/Y_15_2	MAT_N_06_16_2/Y_16_2	MAT_N_06_17_2/Y_17_2	MAT_N_06_18_2/Y_18_2	MAT_N_06_19_2/Y_19_2	MAT_N_06_20_2/Y_20_2	MAT_N_07_01_2/Y_01_2	MAT_N_07_02_2/Y_02_2	MAT_N_07_03_2/Y_03_2	MAT_N_07_04_2/Y_04_2	MAT_N_07_05_2/Y_05_2	MAT_N_07_06_2/Y_06_2	MAT_N_07_07_2/Y_07_2	MAT_N_07_08_2/Y_08_2	MAT_N_07_09_2/Y_09_2	MAT_N_07_10_2/Y_10_2	MAT_N_07_11_2/Y_11_2	MAT_N_07_12_2/Y_12_2	MAT_N_07_13_2/Y_13_2	MAT_N_07_14_2/Y_14_2	MAT_N_07_15_2/Y_15_2	MAT_N_07_16_2/Y_16_2	MAT_N_07_17_2/Y_17_2	MAT_N_07_18_2/Y_18_2	MAT_N_07_19_2/Y_19_2	MAT_N_07_20_2/Y_20_2	MAT_N_08_02_2/Y_02_2	MAT_N_08_03_2/Y_03_2	MAT_N_08_04_2/Y_04_2	MAT_N_08_05_2/Y_05_2	MAT_N_08_06_2/Y_06_2	MAT_N_08_07_2/Y_07_2	MAT_N_08_08_2/Y_08_2	MAT_N_08_09_2/Y_09_2	MAT_N_08_10_2/Y_10_2	MAT_N_08_11_2/Y_11_2	MAT_N_08_12_2/Y_12_2	MAT_N_08_13_2/Y_13_2	MAT_N_08_14_2/Y_14_2	MAT_N_08_15_2/Y_15_2	MAT_N_08_16_2/Y_16_2	MAT_N_08_17_2/Y_17_2	MAT_N_08_18_2/Y_18_2	MAT_N_08_19_2/Y_19_2	MAT_N_08_20_2/Y_20_2	MAT_N_09_01_2/Y_01_2	MAT_N_09_02_2/Y_02_2	MAT_N_09_03_2/Y_03_2	MAT_N_09_04_2/Y_04_2	MAT_N_09_05_2/Y_05_2	MAT_N_09_06_2/Y_06_2	MAT_N_09_07_2/Y_07_2	MAT_N_09_08_2/Y_08_2	MAT_N_09_09_2/Y_09_2	MAT_N_09_10_2/Y_10_2	MAT_N_09_11_2/Y_11_2	MAT_N_09_12_2/Y_12_2	MAT_N_09_13_2/Y_13_2	MAT_N_09_14_2/Y_14_2	MAT_N_09_15_2/Y_15_2	MAT_N_09_16_2/Y_16_2	MAT_N_09_17_2/Y_17_2	MAT_N_09_18_2/Y_18_2	MAT_N_09_19_2/Y_19_2	MAT_N_09_20_2/Y_20_2	MAT_N_14_01_2/Y_01_2	MAT_N_14_02_2/Y_02_2	MAT_N_14_03_2/Y_03_2	MAT_N_14_04_2/Y_04_2	MAT_N_14_05_2/Y_05_2	MAT_N_14_06_2/Y_06_2	MAT_N_14_07_2/Y_07_2	MAT_N_14_08_2/Y_08_2	MAT_N_14_09_2/Y_09_2	MAT_N_14_10_2/Y_10_2	MAT_N_14_11_2/Y_11_2	MAT_N_14_12_2/Y_12_2	MAT_N_14_13_2/Y_13_2	MAT_N_14_14_2/Y_14_2	MAT_N_14_15_2/Y_15_2	MAT_N_14_16_2/Y_16_2	MAT_N_14_17_2/Y_17_2	MAT_N_14_18_2/Y_18_2	MAT_N_14_19_2/Y_19_2	MAT_N_14_20_2/Y_20_2	MAT_N_16_01_2/Y_01_2	MAT_N_16_02_2/Y_02_2	MAT_N_16_03_2/Y_03_2	MAT_N_16_04_2/Y_04_2	MAT_N_16_05_2/Y_05_2	MAT_N_16_06_2/Y_06_2	MAT_N_16_07_2/Y_07_2	MAT_N_16_08_2/Y_08_2	MAT_N_16_09_2/Y_09_2	MAT_N_16_10_2/Y_10_2	MAT_N_16_11_2/Y_11_2	MAT_N_16_12_2/Y_12_2	MAT_N_16_13_2/Y_13_2	MAT_N_16_14_2/Y_14_2	MAT_N_16_15_2/Y_15_2	MAT_N_16_16_2/Y_16_2	MAT_N_16_17_2/Y_17_2	MAT_N_16_18_2/Y_18_2	MAT_N_16_19_2/Y_19_2	MAT_N_16_20_2/Y_20_2	MAT_N_17_02_2/Y_02_2	MAT_N_17_03_2/Y_03_2	MAT_N_17_04_2/Y_04_2	MAT_N_17_05_2/Y_05_2	MAT_N_17_06_2/Y_06_2	MAT_N_17_07_2/Y_07_2	MAT_N_17_08_2/Y_08_2	MAT_N_17_09_2/Y_09_2	MAT_N_17_10_2/Y_10_2	MAT_N_17_11_2/Y_11_2	MAT_N_17_12_2/Y_12_2	MAT_N_17_13_2/Y_13_2	MAT_N_17_14_2/Y_14_2	MAT_N_17_15_2/Y_15_2	MAT_N_17_16_2/Y_16_2	MAT_N_17_17_2/Y_17_2	MAT_N_17_18_2/Y_18_2	MAT_N_17_19_2/Y_19_2	MAT_N_17_20_2/Y_20_2	MAT_N_01_13_2/Y_13_2	MAT_N_02_13_2/Y_13_2	MAT_N_03_13_2/Y_13_2	MAT_N_04_13_2/Y_13_2	MAT_N_05_13_2/Y_13_2	MAT_N_06_13_2/Y_13_2	MAT_N_07_13_2/Y_13_2	MAT_N_08_13_2/Y_13_2	MAT_N_09_13_2/Y_13_2	MAT_N_10_13_2/Y_13_2	MAT_N_11_13_2/Y_13_2	MAT_N_12_13_2/Y_13_2	MAT_N_13_13_2/Y_13_2	MAT_N_14_13_2/Y_13_2	MAT_N_15_13_2/Y_13_2	MAT_N_16_13_2/Y_13_2	MAT_N_17_13_2/Y_13_2	MAT_N_18_13_2/Y_13_2	MAT_N_19_13_2/Y_13_2	MAT_N_20_13_2/Y_13_2	EXP_13_2	PEXP_13_H01_2	NEWBUIL_H01_2	PNEWBUIL_H01_2	NEW_TRUCKS_15_2	NEW_TRUCKS_16_2	NEW_LUV_16_2	TRUCKS_15_2	TRUCKS_22_15_2	TRUCKS_23_15_2	TRUCKS_24_15_2	NEW_TRUCKS_22_15_2	NEW_TRUCKS_23_15_2	NEW_TRUCKS_24_15_2	TRUCKS_16_2	TRUCKS_22_16_2	TRUCKS_23_16_2	TRUCKS_24_16_2	NEW_TRUCKS_22_16_2	NEW_TRUCKS_23_16_2	NEW_TRUCKS_24_16_2	PNEW_TRUCKS_22_15_2	PNEW_TRUCKS_23_15_2	PNEW_TRUCKS_24_15_2	PNEW_TRUCKS_22_16_2	PNEW_TRUCKS_23_16_2	PNEW_TRUCKS_24_16_2	LUV_16_2	LUV_22_16_2	LUV_23_16_2	LUV_24_16_2	PNEW_LUV_22_16_2	PNEW_LUV_23_16_2	PNEW_LUV_24_16_2	NEW_LUV_22_16_2	NEW_LUV_23_16_2	NEW_LUV_24_16_2	YQ_01_2	YQ_02_2	YQ_03_2	YQ_04_2	YQ_05_2	YQ_06_2	YQ_07_2	YQ_08_2	YQ_09_2	YQ_10_2	YQ_11_2	YQ_12_2	YQ_13_2	YQ_14_2	YQ_15_2	YQ_16_2	YQ_17_2	YQ_18_2	YQ_19_2	YQ_20_2	PYQ_01_2	PYQ_02_2	PYQ_03_2	PYQ_04_2	PYQ_05_2	PYQ_06_2	PYQ_07_2	PYQ_08_2	PYQ_09_2	PYQ_10_2	PYQ_11_2	PYQ_12_2	PYQ_13_2	PYQ_14_2	PYQ_15_2	PYQ_16_2	PYQ_17_2	PYQ_18_2	PYQ_19_2	PYQ_20_2	PMAT_03_15_2	PMAT_03_16_2	PINV_IRVE_2	INV_IRVE_2

'Reporting_Logan.sheet(t)
'show Reporting_Logan

'Nouveau reporting créé par la DGT (Pierre-Louis Girard) pour exporter les données utiles à MatMat
'fait pour contourner le pb que la subroutine export_matmat dans matter.prg ne parvenait pas à sortir les variables avec des _1 au lieu des _0 pour AME
'on crée ici un reporting puis dans main un fichier csv reprenant ces séries est créé
'(attention dans le fichier que il avait envoyé il avait repris les séries de export_matter au lieu de export_matmat)
group export_group_matmat CH_01 CH_02 CH_03 CH_04 CH_05 CH_06 CH_07 CH_08 CH_09 CH_10 CH_11 CH_12 CH_13 CH_14 CH_15 CH_16 CH_17 CH_18 CH_19 CH_20 CH_21 CH_22 CH_23 CH_24 G_01 G_02 G_03 G_04 G_05 G_06 G_07 G_08 G_09 G_10 G_11 G_12 G_13 G_14 G_15 G_16 G_17 G_18 G_19 G_20 G_21 G_22 G_23 G_24 I_01 I_02 I_03 I_04 I_05 I_06 I_07 I_08 I_09 I_10 I_11 I_12 I_13 I_14 I_15 I_16 I_17 I_18 I_19 I_20 I_21 I_22 I_23 I_24 CID_01 CID_02 CID_03 CID_04 CID_05 CID_06 CID_07 CID_08 CID_09 CID_10 CID_11 CID_12 CID_14 CID_16 CID_17 CID_18 CID_19 CID_21 CID_22 CID_23 CID_24 CIM_01 CIM_02 CIM_03 CIM_04 CIM_05 CIM_06 CIM_07 CIM_08 CIM_09 CIM_10 CIM_11 CIM_12 CIM_14 CIM_16 CIM_17 CIM_18 CIM_19 CIM_21 CIM_22 CIM_23 CIM_24 CHD_01 CHD_02 CHD_03 CHD_04 CHD_05 CHD_06 CHD_07 CHD_08 CHD_09 CHD_10 CHD_11 CHD_12 CHD_13 CHD_14 CHD_15 CHD_16 CHD_17 CHD_18 CHD_19 CHD_20 CHD_21 CHD_22 CHD_23 CHD_24 CHM_01 CHM_02 CHM_03 CHM_04 CHM_05 CHM_06 CHM_07 CHM_08 CHM_09 CHM_10 CHM_11 CHM_12 CHM_14 CHM_16 CHM_17 CHM_18 CHM_19  CHM_21 CHM_22 CHM_23 CHM_24 GD_01 GD_02 GD_03 GD_04 GD_05 GD_06 GD_07 GD_08 GD_09 GD_10 GD_11 GD_12 GD_13 GD_14 GD_15 GD_16 GD_17 GD_18 GD_19 GD_20 GD_21 GD_22 GD_23 GD_24 GM_01 GM_02 GM_03 GM_04 GM_05 GM_06 GM_07 GM_08 GM_09 GM_10 GM_11 GM_12 GM_14 GM_16 GM_17 GM_18 GM_19 GM_21 GM_22 GM_23 GM_24 ID_01 ID_02 ID_03 ID_04 ID_05 ID_06 ID_07 ID_08 ID_09 ID_10 ID_11 ID_12 ID_13 ID_14 ID_15 ID_16 ID_17 ID_18 ID_19 ID_20 ID_21 ID_22 ID_23 ID_24 IM_01 IM_02 IM_03 IM_04 IM_05 IM_06 IM_07 IM_08 IM_09 IM_10 IM_11 IM_12 IM_14 IM_16 IM_17 IM_18 IM_19 IM_21 IM_22 IM_23 IM_24  XD_01 XD_02 XD_03 XD_04 XD_05 XD_06 XD_07 XD_08 XD_09 XD_10 XD_11 XD_12 XD_14 XD_16 XD_17 XD_18 XD_19 XD_21 XD_22 XD_23 XD_24 XM_03 XM_04 XM_05 XM_09 XM_10 XM_12 XM_22 XM_23 XM_24 PhiY_22_2201 PhiY_22_2202  PhiY_23_2301 PhiY_23_2302 PhiY_23_2303 PhiY_23_2304 PhiY_23_2305 PhiY_23_2306 PhiY_23_2307 PhiY_23_2308 PhiY_24_2401 PhiY_24_2402 PhiY_24_2403 PhiY_24_2404 PhiY_24_2405 PhiY_24_2406 M_03 M_04 M_05 M_09 M_10 M_12 M_22 M_23 M_24 X_01 X_02 X_03 X_04 X_05 X_06 X_07 X_08 X_09 X_10 X_11 X_12 X_14 X_16 X_17 X_18 X_19 X_21 X_22 X_23 X_24 YQ_01 YQ_02 YQ_03 YQ_04 YQ_05 YQ_06 YQ_07 YQ_08 YQ_09 YQ_10 YQ_11 YQ_12 YQ_13 YQ_14 YQ_15 YQ_16 YQ_17 YQ_18 YQ_19 YQ_20 YQ_21 YQ_22 YQ_23 YQ_24 PCH_01 PCH_02 PCH_03 PCH_04 PCH_05 PCH_06 PCH_07 PCH_08 PCH_09 PCH_10 PCH_11 PCH_12 PCH_13 PCH_14 PCH_15 PCH_16 PCH_17 PCH_18 PCH_19 PCH_20 PCH_21 PCH_22 PCH_23 PCH_24 PG_01 PG_02 PG_03 PG_04 PG_05 PG_06 PG_07 PG_08 PG_09 PG_10 PG_11 PG_12 PG_13 PG_14 PG_15 PG_16 PG_17 PG_18 PG_19 PG_20 PG_21 PG_22 PG_23 PG_24 PI_01 PI_02 PI_03 PI_04 PI_05 PI_06 PI_07 PI_08 PI_09 PI_10 PI_11 PI_12 PI_13 PI_14 PI_15 PI_16 PI_17 PI_18 PI_19 PI_20 PI_21 PI_22 PI_23 PI_24 PX_01 PX_02 PX_03 PX_04 PX_05 PX_06 PX_07 PX_08 PX_09 PX_10 PX_11 PX_12 PX_14 PX_16 PX_17 PX_18 PX_19 PX_21 PX_22 PX_23 PX_24 PM_03 PM_04 PM_05 PM_09 PM_10 PM_12 PM_22 PM_23 PM_24 PYQ_01 PYQ_02 PYQ_03 PYQ_04 PYQ_05 PYQ_06 PYQ_07 PYQ_08 PYQ_09 PYQ_10 PYQ_11 PYQ_12 PYQ_13 PYQ_14 PYQ_15 PYQ_16 PYQ_17 PYQ_18 PYQ_19 PYQ_20 PYQ_21 PYQ_22 PYQ_23 PYQ_24  IA IA_01 IA_02 IA_03 IA_04 IA_05 IA_06 IA_07 IA_08 IA_09 IA_10 IA_11 IA_12 IA_13 IA_14 IA_15 IA_16 IA_17 IA_18 IA_19 IA_20 IA_21 IA_2201 IA_2202 IA_2301 IA_2302 IA_2303 IA_2304 IA_2305 IA_2306 IA_2307 IA_2308 IA_2401 IA_2402 IA_2403 IA_2404 IA_2405 IA_2406 IA_01_01 IA_01_19 IA_01_20 IA_03_01 IA_03_02 IA_03_03 IA_03_04 IA_03_05 IA_03_06 IA_03_07 IA_03_08 IA_03_09 IA_03_10 IA_03_11 IA_03_12 IA_03_13 IA_03_14 IA_03_15 IA_03_16 IA_03_17 IA_03_18 IA_03_19 IA_03_20 IA_03_2301 IA_03_2302 IA_03_2303 IA_03_2304 IA_03_2305 IA_03_2306 IA_03_2307 IA_03_2308 IA_03_2401 IA_03_2402 IA_03_2403 IA_03_2404 IA_03_2405 IA_03_2406 IA_05_01 IA_05_02 IA_05_03 IA_05_04 IA_05_05 IA_05_06 IA_05_07 IA_05_08 IA_05_09 IA_05_10 IA_05_11 IA_05_12 IA_05_13 IA_05_14 IA_05_15 IA_05_16 IA_05_17 IA_05_18 IA_05_19 IA_05_21 IA_05_2201 IA_05_2202 IA_05_2301 IA_05_2302 IA_05_2303 IA_05_2304 IA_05_2305 IA_05_2306 IA_05_2307 IA_05_2308 IA_05_2401 IA_05_2402 IA_05_2403 IA_05_2404 IA_05_2405 IA_05_2406 IA_12_01 IA_12_02 IA_12_03 IA_12_04 IA_12_05 IA_12_06 IA_12_07 IA_12_08 IA_12_09 IA_12_10 IA_12_11 IA_12_12 IA_12_13 IA_12_14 IA_12_15 IA_12_16 IA_12_17 IA_12_18 IA_12_19 IA_12_20 IA_12_21 IA_12_2201 IA_12_2202 IA_12_2301 IA_12_2302 IA_12_2303 IA_12_2304 IA_12_2305 IA_12_2306 IA_12_2307 IA_12_2308 IA_12_2401 IA_12_2402 IA_12_2403 IA_12_2404 IA_12_2405 IA_12_2406 IA_13_01 IA_13_02 IA_13_03 IA_13_04 IA_13_05 IA_13_06 IA_13_07 IA_13_08 IA_13_09 IA_13_10 IA_13_11 IA_13_12 IA_13_13 IA_13_14 IA_13_15 IA_13_16 IA_13_17 IA_13_18 IA_13_19 IA_13_20 IA_13_21 IA_13_2201 IA_13_2202 IA_13_2301 IA_13_2302 IA_13_2303 IA_13_2304 IA_13_2305 IA_13_2306 IA_13_2307 IA_13_2308 IA_13_2401 IA_13_2402 IA_13_2403 IA_13_2404 IA_13_2405 IA_13_2406 IA_19_01 IA_19_02 IA_19_03 IA_19_04 IA_19_05 IA_19_06 IA_19_07 IA_19_08 IA_19_09 IA_19_10 IA_19_11 IA_19_12 IA_19_13 IA_19_14 IA_19_15 IA_19_16 IA_19_17 IA_19_18 IA_19_19 IA_19_20 IA_19_21 IA_19_2201 IA_19_2202 IA_19_2301 IA_19_2302 IA_19_2303 IA_19_2304 IA_19_2305 IA_19_2306 IA_19_2307 IA_19_2308 IA_19_2401 IA_19_2402 IA_19_2403 IA_19_2404 IA_19_2405 IA_19_2406 PIA PIA_01 PIA_02 PIA_03 PIA_04 PIA_05 PIA_06 PIA_07 PIA_08 PIA_09 PIA_10 PIA_11 PIA_12 PIA_13 PIA_14 PIA_15 PIA_16 PIA_17 PIA_18 PIA_19 PIA_20 PIA_21 PIA_2201 PIA_2202 PIA_2301 PIA_2302 PIA_2303 PIA_2304 PIA_2305 PIA_2306 PIA_2307 PIA_2308 PIA_2401 PIA_2402 PIA_2403 PIA_2404 PIA_2405 PIA_2406 PIA_01_01 PIA_01_19 PIA_01_20 PIA_03_01 PIA_03_02 PIA_03_03 PIA_03_04 PIA_03_05 PIA_03_06 PIA_03_07 PIA_03_08 PIA_03_09 PIA_03_10 PIA_03_11 PIA_03_12 PIA_03_13 PIA_03_14 PIA_03_15 PIA_03_16 PIA_03_17 PIA_03_18 PIA_03_19 PIA_03_20 PIA_03_2301 PIA_03_2302 PIA_03_2303 PIA_03_2304 PIA_03_2305 PIA_03_2306 PIA_03_2307 PIA_03_2308 PIA_03_2401 PIA_03_2402 PIA_03_2403 PIA_03_2404 PIA_03_2405 PIA_03_2406 PIA_05_01 PIA_05_02 PIA_05_03 PIA_05_04 PIA_05_05 PIA_05_06 PIA_05_07 PIA_05_08 PIA_05_09 PIA_05_10 PIA_05_11 PIA_05_12 PIA_05_13 PIA_05_14 PIA_05_15 PIA_05_16 PIA_05_17 PIA_05_18 PIA_05_19 PIA_05_21 PIA_05_2201 PIA_05_2202 PIA_05_2301 PIA_05_2302 PIA_05_2303 PIA_05_2304 PIA_05_2305 PIA_05_2306 PIA_05_2307 PIA_05_2308 PIA_05_2401 PIA_05_2402 PIA_05_2403 PIA_05_2404 PIA_05_2405 PIA_05_2406 PIA_12_01 PIA_12_02 PIA_12_03 PIA_12_04 PIA_12_05 PIA_12_06 PIA_12_07 PIA_12_08 PIA_12_09 PIA_12_10 PIA_12_11 PIA_12_12 PIA_12_13 PIA_12_14 PIA_12_15 PIA_12_16 PIA_12_17 PIA_12_18 PIA_12_19 PIA_12_20 PIA_12_21 PIA_12_2201 PIA_12_2202 PIA_12_2301 PIA_12_2302 PIA_12_2303 PIA_12_2304 PIA_12_2305 PIA_12_2306 PIA_12_2307 PIA_12_2308 PIA_12_2401 PIA_12_2402 PIA_12_2403 PIA_12_2404 PIA_12_2405 PIA_12_2406 PIA_13_01 PIA_13_02 PIA_13_03 PIA_13_04 PIA_13_05 PIA_13_06 PIA_13_07 PIA_13_08 PIA_13_09 PIA_13_10 PIA_13_11 PIA_13_12 PIA_13_13 PIA_13_14 PIA_13_15 PIA_13_16 PIA_13_17 PIA_13_18 PIA_13_19 PIA_13_20 PIA_13_21 PIA_13_2201 PIA_13_2202 PIA_13_2301 PIA_13_2302 PIA_13_2303 PIA_13_2304 PIA_13_2305 PIA_13_2306 PIA_13_2307 PIA_13_2308 PIA_13_2401 PIA_13_2402 PIA_13_2403 PIA_13_2404 PIA_13_2405 PIA_13_2406 PIA_19_01 PIA_19_02 PIA_19_03 PIA_19_04 PIA_19_05 PIA_19_06 PIA_19_07 PIA_19_08 PIA_19_09 PIA_19_10 PIA_19_11 PIA_19_12 PIA_19_13 PIA_19_14 PIA_19_15 PIA_19_16 PIA_19_17 PIA_19_18 PIA_19_19 PIA_19_20 PIA_19_21 PIA_19_2201 PIA_19_2202 PIA_19_2301 PIA_19_2302 PIA_19_2303 PIA_19_2304 PIA_19_2305 PIA_19_2306 PIA_19_2307 PIA_19_2308 PIA_19_2401 PIA_19_2402 PIA_19_2403 PIA_19_2404 PIA_19_2405 PIA_19_2406

'export_group_matmat.sheet(t)
'show export_group_matmat

 if %exceptions_PAC="yes" then

group Reporting_PAC  dispinc_ai_val_h01_1	dispinc_ai_val_h01_2	tcss_h01+choc_tcss	tcss_se_h01+choc_tcss_se	w_s_h01_1	l_s_h01_1	w_s_h01_2	l_s_h01_2	w_se_h01_1	l_se_h01_1	w_se_h01_2	l_se_h01_2	presoc_dom_val_h01_1	presoc_dom_val_h01_2	presoc_dom_oth_val_1	presoc_dom_oth_val_2	presoc_dom_u_val_1	presoc_dom_u_val_2	fw_val_h01_1	fw_val_h01_2	ir_val_h01_1	aic_val_h01_1	redis_val_h	sub_auto_val_1	sub_renov_val_1	ir_val_h01_2	aic_val_h01_2	sub_auto_val_2	sub_renov_val_2	dispinc_val_h01_1	dispinc_val_h01_2	arbinc_val_h01_1	pnexp_h01_1*nexp_h01_1	exp_housing_val_h01_1	exp_13_oth_val_h01_1	exp_mob_val_h01_1	arbinc_val_h01_2	pnexp_h01_2*nexp_h01_2	exp_housing_val_h01_2	exp_13_oth_val_h01_2	exp_mob_val_h01_2	exp_housing_val_h01_ca_1	exp_housing_val_h01_cb_1	exp_housing_val_h01_cc_1	exp_housing_val_h01_cd_1	exp_housing_val_h01_ce_1	exp_housing_val_h01_cf_1	exp_housing_val_h01_cg_1	exp_housing_val_h01_ca_2	exp_housing_val_h01_cb_2	exp_housing_val_h01_cc_2	exp_housing_val_h01_cd_2	exp_housing_val_h01_ce_2	exp_housing_val_h01_cf_2	exp_housing_val_h01_cg_2	debt_rehab_val_h01_cb_1(-1)*(r_i_rehab_h01_cb_1(-1)+r_rmbs_rehab_h01_cb_1(-1))	r_cash_rehab_h01_cb*renov_val_apCEE_cb_1	debt_newb_val_h01_cb_1(-1)*(r_i_newbuil_h01_cb_1(-1)+r_rmbs_newbuil_h01_cb(-1))	r_cash_newbuil_h01_cb*pnewbuil_h01_cb_1*newbuil_h01_cb_1	pener_buil_h01_cb_1*ener_buil_h01_cb_1	debt_rehab_val_h01_cb_2(-1)*(r_i_rehab_h01_cb_2(-1)+r_rmbs_rehab_h01_cb_2(-1))	r_cash_rehab_h01_cb*renov_val_apCEE_cb_2	debt_newb_val_h01_cb_2(-1)*(r_i_newbuil_h01_cb_2(-1)+r_rmbs_newbuil_h01_cb(-1))	r_cash_newbuil_h01_cb*pnewbuil_h01_cb_2*newbuil_h01_cb_2	pener_buil_h01_cb_2*ener_buil_h01_cb_2	prehab_h01_cb_1	rehab_h01_cb_1	prehab_h01_cb_2	rehab_h01_cb_2	rehab_h01_ca	rehab_h01_cc_1	rehab_h01_cd_1	rehab_h01_ce_1	rehab_h01_cf_1	rehab_h01_cg_1	rehab_h01_cc_2	rehab_h01_cd_2	rehab_h01_ce_2	rehab_h01_cf_2	rehab_h01_cg_2	choc_pac_val_1	choc_pac_val_2	ener_buil_h01_ca_21	ener_buil_h01_ca_22_1	ener_buil_h01_ca_23_1	ener_buil_h01_ca_24	ener_buil_h01_cb_21	ener_buil_h01_cb_22_1	ener_buil_h01_cb_23_1	ener_buil_h01_cb_24_1	ener_buil_h01_cc_21_1	ener_buil_h01_cc_22_1	ener_buil_h01_cc_23_1	ener_buil_h01_cc_24_1	ener_buil_h01_cd_21_1	ener_buil_h01_cd_22_1	ener_buil_h01_cd_23_1	ener_buil_h01_cd_24_1	ener_buil_h01_ce_21_1	ener_buil_h01_ce_22_1	ener_buil_h01_ce_23_1	ener_buil_h01_ce_24_1	ener_buil_h01_cf_21_1	ener_buil_h01_cf_22_1	ener_buil_h01_cf_23_1	ener_buil_h01_cf_24_1	ener_buil_h01_cg_21_1	ener_buil_h01_cg_22_1	ener_buil_h01_cg_23_1	ener_buil_h01_cg_24_1	ener_buil_h01_ca_22_2	ener_buil_h01_ca_23_2	ener_buil_h01_cb_22_2	ener_buil_h01_cb_23_2	ener_buil_h01_cb_24_2	ener_buil_h01_cc_21_2	ener_buil_h01_cc_22_2	ener_buil_h01_cc_23_2	ener_buil_h01_cc_24_2	ener_buil_h01_cd_21_2	ener_buil_h01_cd_22_2	ener_buil_h01_cd_23_2	ener_buil_h01_cd_24_2	ener_buil_h01_ce_21_2	ener_buil_h01_ce_22_2	ener_buil_h01_ce_23_2	ener_buil_h01_ce_24_2	ener_buil_h01_cf_21_2	ener_buil_h01_cf_22_2	ener_buil_h01_cf_23_2	ener_buil_h01_cf_24_2	ener_buil_h01_cg_21_2	ener_buil_h01_cg_22_2	ener_buil_h01_cg_23_2	ener_buil_h01_cg_24_2	buil_21_ca_1	buil_21_cb_1	buil_21_cc_1	buil_21_cd_1	buil_21_ce_1	buil_21_cf_1	buil_21_cg_1	buil_22_ca_1	buil_22_cb_1	buil_22_cc_1	buil_22_cd_1	buil_22_ce_1	buil_22_cf_1	buil_22_cg_1	buil_23_ca_1	buil_23_cb_1	buil_23_cc_1	buil_23_cd_1	buil_23_ce_1	buil_23_cf_1	buil_23_cg_1	buil_24_ca_1	buil_24_cb_1	buil_24_cc_1	buil_24_cd_1	buil_24_ce_1	buil_24_cf_1	buil_24_cg_1	buil_h01_ca_1	buil_h01_cb_1	buil_h01_cc_1	buil_h01_cd_1	buil_h01_ce_1	buil_h01_cf_1	buil_h01_cg_1	buil_h01_ca_2	buil_h01_cb_2	buil_h01_cc_2	buil_h01_cd_2	buil_h01_ce_2	buil_h01_cf_2	buil_h01_cg_2	buil_21_ca_2	buil_21_cb_2	buil_21_cc_2	buil_21_cd_2	buil_21_ce_2	buil_21_cf_2	buil_21_cg_2	buil_22_ca_2	buil_22_cb_2	buil_22_cc_2	buil_22_cd_2	buil_22_ce_2	buil_22_cf_2	buil_22_cg_2	buil_23_ca_2	buil_23_cb_2	buil_23_cc_2	buil_23_cd_2	buil_23_ce_2	buil_23_cf_2	buil_23_cg_2	buil_24_ca_2	buil_24_cb_2	buil_24_cc_2	buil_24_cd_2	buil_24_ce_2	buil_24_cf_2	buil_24_cg_2	nb_pac_fioul_ca_1	nb_pac_fioul_cb_1	nb_pac_fioul_cc_1	nb_pac_fioul_cd_1	nb_pac_fioul_ce_1	nb_pac_fioul_cf_1	nb_pac_fioul_cg_1	nb_pac_gaz_ca_1	nb_pac_gaz_cb_1	nb_pac_gaz_cc_1	nb_pac_gaz_cd_1	nb_pac_gaz_ce_1	nb_pac_gaz_cf_1	nb_pac_gaz_cg_1	nb_pac_fioul_ca_2	nb_pac_fioul_cb_2	nb_pac_fioul_cc_2	nb_pac_fioul_cd_2	nb_pac_fioul_ce_2	nb_pac_fioul_cf_2	nb_pac_fioul_cg_2	nb_pac_gaz_ca_2	nb_pac_gaz_cb_2	nb_pac_gaz_cc_2	nb_pac_gaz_cd_2	nb_pac_gaz_ce_2	nb_pac_gaz_cf_2	nb_pac_gaz_cg_2	choc_pac_ca_2	choc_pac_cb_2	choc_pac_cc_2	choc_pac_cd_2	choc_pac_ce_2	choc_pac_cf_2	choc_pac_cg_2	choc_pac_ci_2	choc_pac_fioul_2	choc_pac_fioul_ca_2	choc_pac_fioul_cb_2	choc_pac_fioul_cc_2	choc_pac_fioul_cd_2	choc_pac_fioul_ce_2	choc_pac_fioul_cf_2	choc_pac_fioul_cg_2	choc_pac_fioul_val_2	choc_pac_fioul_val_ca_2	choc_pac_fioul_val_cb_2	choc_pac_fioul_val_cc_2	choc_pac_fioul_val_cd_2	choc_pac_fioul_val_ce_2	choc_pac_fioul_val_cf_2	choc_pac_fioul_val_cg_2	choc_pac_gaz_2	choc_pac_gaz_ca_2	choc_pac_gaz_cb_2	choc_pac_gaz_cc_2	choc_pac_gaz_cd_2	choc_pac_gaz_ce_2	choc_pac_gaz_cf_2	choc_pac_gaz_cg_2	choc_pac_gaz_val_2	choc_pac_gaz_val_ca_2	choc_pac_gaz_val_cb_2	choc_pac_gaz_val_cc_2	choc_pac_gaz_val_cd_2	choc_pac_gaz_val_ce_2	choc_pac_gaz_val_cf_2	choc_pac_gaz_val_cg_2	choc_pac_val_apsub_2	choc_pac_val_ca_2	choc_pac_val_cb_2	choc_pac_val_cc_2	choc_pac_val_cd_2	choc_pac_val_ce_2	choc_pac_val_cf_2	choc_pac_val_cg_2	choc_pac_ca_1	choc_pac_cb_1	choc_pac_cc_1	choc_pac_cd_1	choc_pac_ce_1	choc_pac_cf_1	choc_pac_cg_1	choc_pac_ci_1	choc_pac_fioul_1	choc_pac_fioul_ca_1	choc_pac_fioul_cb_1	choc_pac_fioul_cc_1	choc_pac_fioul_cd_1	choc_pac_fioul_ce_1	choc_pac_fioul_cf_1	choc_pac_fioul_cg_1	choc_pac_fioul_val_1	choc_pac_fioul_val_ca_1	choc_pac_fioul_val_cb_1	choc_pac_fioul_val_cc_1	choc_pac_fioul_val_cd_1	choc_pac_fioul_val_ce_1	choc_pac_fioul_val_cf_1	choc_pac_fioul_val_cg_1	choc_pac_gaz_1	choc_pac_gaz_ca_1	choc_pac_gaz_cb_1	choc_pac_gaz_cc_1	choc_pac_gaz_cd_1	choc_pac_gaz_ce_1	choc_pac_gaz_cf_1	choc_pac_gaz_cg_1	choc_pac_gaz_val_1	choc_pac_gaz_val_ca_1	choc_pac_gaz_val_cb_1	choc_pac_gaz_val_cc_1	choc_pac_gaz_val_cd_1	choc_pac_gaz_val_ce_1	choc_pac_gaz_val_cf_1	choc_pac_gaz_val_cg_1	choc_pac_val_apsub_1	choc_pac_val_ca_1	choc_pac_val_cb_1	choc_pac_val_cc_1	choc_pac_val_cd_1	choc_pac_val_ce_1	choc_pac_val_cf_1	choc_pac_val_cg_1	dc_val_01_1	dc_val_02_1	dc_val_03_1	dc_val_04_1	dc_val_05_1	dc_val_06_1	dc_val_07_1	dc_val_08_1	dc_val_09_1	dc_val_10_1	dc_val_11_1	dc_val_12_1	dc_val_13_1	dc_val_14_1	dc_val_15_1	dc_val_16_1	dc_val_17_1	dc_val_18_1	dc_val_19_1	dc_val_20_1	dc_val_21_1	dc_val_22_1	dc_val_23_1	dc_val_24_1	dc_val_01_2	dc_val_02_2	dc_val_03_2	dc_val_04_2	dc_val_05_2	dc_val_06_2	dc_val_07_2	dc_val_08_2	dc_val_09_2	dc_val_10_2	dc_val_11_2	dc_val_12_2	dc_val_13_2	dc_val_14_2	dc_val_15_2	dc_val_16_2	dc_val_17_2	dc_val_18_2	dc_val_19_2	dc_val_20_2	dc_val_21_2	dc_val_22_2	dc_val_23_2	dc_val_24_2	x_01_1	x_02_1	x_03_1	x_04_1	x_05_1	x_06_1	x_07_1	x_08_1	x_09_1	x_10_1	x_11_1	x_12_1	x_14_1	x_16_1	x_17_1	x_18_1	x_19_1	x_21_1	x_22_1	x_23_1	x_24_1	x_01_2	x_02_2	x_03_2	x_04_2	x_05_2	x_06_2	x_07_2	x_08_2	x_09_2	x_10_2	x_11_2	x_12_2	x_14_2	x_16_2	x_17_2	x_18_2	x_19_2	x_21_2	x_22_2	x_23_2	x_24_2	m_1	m_01_1	m_02_1	m_03_1	m_04_1	m_05_1	m_06_1	m_07_1	m_08_1	m_09_1	m_10_1	m_11_1	m_12_1	m_14_1	m_16_1	m_17_1	m_18_1	m_19_1	m_21_1	m_22_1	m_23_1	m_24_1	m_01_2	m_02_2	m_03_2	m_04_2	m_05_2	m_06_2	m_07_2	m_08_2	m_09_2	m_10_2	m_11_2	m_12_2	m_14_2	m_16_2	m_17_2	m_18_2	m_19_2	m_2	m_21_2	m_22_2	m_23_2	m_24_2	pch_1	pch_2	div_hh_val_h01_1	int_val_h01_1	div_hh_val_h01_2	int_val_h01_2	debt_rehab_val_h01_ca_1(-1)*(r_i_rehab_h01_ca_1(-1)+r_rmbs_rehab_h01_ca_1(-1))	r_cash_rehab_h01_ca*renov_val_apCEE_CA_1	debt_newb_val_h01_ca_1(-1)*(r_i_newbuil_h01_ca_1(-1)+r_rmbs_newbuil_h01_ca(-1))	r_cash_newbuil_h01_ca*pnewbuil_h01_ca_1*newbuil_h01_ca_1	pener_buil_h01_ca_1*ener_buil_h01_ca_1	debt_rehab_val_h01_ca_2(-1)*(r_i_rehab_h01_ca_2(-1)+r_rmbs_rehab_h01_ca_2(-1))	debt_newb_val_h01_ca_2(-1)*(r_i_newbuil_h01_ca_2(-1)+r_rmbs_newbuil_h01_ca(-1))	r_cash_newbuil_h01_ca*pnewbuil_h01_ca_2*newbuil_h01_ca_2	pener_buil_h01_ca_2*ener_buil_h01_ca_2	debt_rehab_val_h01_cc_1(-1)*(r_i_rehab_h01_cc_1(-1)+r_rmbs_rehab_h01_cc_1(-1))	r_cash_rehab_h01_cc*renov_val_apCEE_CC_1	debt_newb_val_h01_cc_1(-1)*(r_i_newbuil_h01_cc_1(-1)+r_rmbs_newbuil_h01_cc(-1))	r_cash_newbuil_h01_cc*pnewbuil_h01_cc_1*newbuil_h01_cc_1	pener_buil_h01_cc_1*ener_buil_h01_cc_1	debt_rehab_val_h01_cc_2(-1)*(r_i_rehab_h01_cc_2(-1)+r_rmbs_rehab_h01_cc_2(-1))	r_cash_rehab_h01_cc*renov_val_apCEE_CC_2	debt_newb_val_h01_cc_2(-1)*(r_i_newbuil_h01_cc_2(-1)+r_rmbs_newbuil_h01_cc(-1))	r_cash_newbuil_h01_cc*pnewbuil_h01_cc_2*newbuil_h01_cc_2	pener_buil_h01_cc_2*ener_buil_h01_cc_2	debt_rehab_val_h01_cd_1(-1)*(r_i_rehab_h01_cd_1(-1)+r_rmbs_rehab_h01_cd_1(-1))	r_cash_rehab_h01_cd*renov_val_apCEE_CD_1	debt_newb_val_h01_cd_1(-1)*(r_i_newbuil_h01_cd_1(-1)+r_rmbs_newbuil_h01_cd(-1))	r_cash_newbuil_h01_cd*pnewbuil_h01_cd_1*newbuil_h01_cd_1	pener_buil_h01_cd_1*ener_buil_h01_cd_1	debt_rehab_val_h01_cd_2(-1)*(r_i_rehab_h01_cd_2(-1)+r_rmbs_rehab_h01_cd_2(-1))	r_cash_rehab_h01_cd*renov_val_apCEE_CD_2	debt_newb_val_h01_cd_2(-1)*(r_i_newbuil_h01_cd_2(-1)+r_rmbs_newbuil_h01_cd(-1))	r_cash_newbuil_h01_cd*pnewbuil_h01_cd_2*newbuil_h01_cd_2	pener_buil_h01_cd_2*ener_buil_h01_cd_2	debt_rehab_val_h01_ce_1(-1)*(r_i_rehab_h01_ce_1(-1)+r_rmbs_rehab_h01_ce_1(-1))	r_cash_rehab_h01_ce*renov_val_apCEE_CE_1	debt_newb_val_h01_ce_1(-1)*(r_i_newbuil_h01_ce_1(-1)+r_rmbs_newbuil_h01_ce(-1))	r_cash_newbuil_h01_ce*pnewbuil_h01_ce_1*newbuil_h01_ce_1	pener_buil_h01_ce_1*ener_buil_h01_ce_1	debt_rehab_val_h01_ce_2(-1)*(r_i_rehab_h01_ce_2(-1)+r_rmbs_rehab_h01_ce_2(-1))	r_cash_rehab_h01_ce*renov_val_apCEE_CE_2	debt_newb_val_h01_ce_2(-1)*(r_i_newbuil_h01_ce_2(-1)+r_rmbs_newbuil_h01_ce(-1))	r_cash_newbuil_h01_ce*pnewbuil_h01_ce_2*newbuil_h01_ce_2	pener_buil_h01_ce_2*ener_buil_h01_ce_2	debt_rehab_val_h01_cf_1(-1)*(r_i_rehab_h01_cf_1(-1)+r_rmbs_rehab_h01_cf_1(-1))	r_cash_rehab_h01_cf*renov_val_apCEE_CF_1	debt_newb_val_h01_cf_1(-1)*(r_i_newbuil_h01_cf_1(-1)+r_rmbs_newbuil_h01_cf(-1))	r_cash_newbuil_h01_cf*pnewbuil_h01_cf_1*newbuil_h01_cf_1	pener_buil_h01_cf_1*ener_buil_h01_cf_1	debt_rehab_val_h01_cf_2(-1)*(r_i_rehab_h01_cf_2(-1)+r_rmbs_rehab_h01_cf_2(-1))	r_cash_rehab_h01_cf*renov_val_apCEE_CF_2	debt_newb_val_h01_cf_2(-1)*(r_i_newbuil_h01_cf_2(-1)+r_rmbs_newbuil_h01_cf(-1))	r_cash_newbuil_h01_cf*pnewbuil_h01_cf_2*newbuil_h01_cf_2	pener_buil_h01_cf_2*ener_buil_h01_cf_2	debt_rehab_val_h01_cg_1(-1)*(r_i_rehab_h01_cg_1(-1)+r_rmbs_rehab_h01_cg_1(-1))	r_cash_rehab_h01_cg*renov_val_apCEE_CG_1	debt_newb_val_h01_cg_1(-1)*(r_i_newbuil_h01_cg_1(-1)+r_rmbs_newbuil_h01_cg(-1))	r_cash_newbuil_h01_cg*pnewbuil_h01_cg_1*newbuil_h01_cg_1	pener_buil_h01_cg_1*ener_buil_h01_cg_1	debt_rehab_val_h01_cg_2(-1)*(r_i_rehab_h01_cg_2(-1)+r_rmbs_rehab_h01_cg_2(-1))	r_cash_rehab_h01_cg*renov_val_apCEE_CG_2	debt_newb_val_h01_cg_2(-1)*(r_i_newbuil_h01_cg_2(-1)+r_rmbs_newbuil_h01_cg(-1))	r_cash_newbuil_h01_cg*pnewbuil_h01_cg_2*newbuil_h01_cg_2	pener_buil_h01_cg_2*ener_buil_h01_cg_2	r_cash_rehab_h01_ca*choc_pac_val_ca_2	r_cash_rehab_h01_cb*choc_pac_val_cb_2	r_cash_rehab_h01_cc*choc_pac_val_cc_2	r_cash_rehab_h01_cd*choc_pac_val_cd_2	r_cash_rehab_h01_ce*choc_pac_val_ce_2	r_cash_rehab_h01_cf*choc_pac_val_cf_2	r_cash_rehab_h01_cg*choc_pac_val_cg_2	100*((x_2*px_2-m_2*pm_2)/(gdp_2*pgdp_2)-(x_1*px_1-m_1*pm_1)/(gdp_1*pgdp_1))	100*((x_2*px_1-m_1*pm_1)/(gdp_1*pgdp_1)-(x_1*px_1-m_1*pm_1)/(gdp_1*pgdp_1))	100*((x_2*px_2-m_1*pm_1)/(gdp_1*pgdp_1)-(x_2*px_1-m_1*pm_1)/(gdp_1*pgdp_1))	100*((x_2*px_2-m_2*pm_1)/(gdp_1*pgdp_1)-(x_2*px_2-m_1*pm_1)/(gdp_1*pgdp_1))	100*((x_2*px_2-m_2*pm_2)/(gdp_1*pgdp_1)-(x_2*px_2-m_2*pm_1)/(gdp_1*pgdp_1))	100*((x_2*px_2-m_2*pm_2)/(gdp_2*pgdp_2)-(x_2*px_2-m_2*pm_2)/(gdp_1*pgdp_1))	sub_pac_val_2	sub_pac_val_1	ppaci_2	ppaci_1	p_pac_2	p_pac_1	REDIS_VAL_TCO_H_1	REDIS_VAL_TCO_H_2	REDIS_VAL_ETS2_H_1	REDIS_VAL_ETS2_H_2	r_cash_rehab_h01_ca*renov_val_apCEE_CA_2	r_cash_rehab_h01_ca*renov_val_apCEE_CC_2	mat_01_13_2	mat_02_13_2	mat_03_13_2	mat_04_13_2	mat_05_13_2	mat_06_13_2	mat_07_13_2	mat_08_13_2	mat_09_13_2	mat_10_13_2	mat_11_13_2	mat_12_13_2	mat_13_13_2	mat_14_13_2	mat_15_13_2	mat_16_13_2	mat_17_13_2	mat_18_13_2	mat_19_13_2	mat_20_13_2	mat_01_13_1	mat_02_13_1	mat_03_13_1	mat_04_13_1	mat_05_13_1	mat_06_13_1	mat_07_13_1	mat_08_13_1	mat_09_13_1	mat_10_13_1	mat_11_13_1	mat_12_13_1	mat_13_13_1	mat_14_13_1	mat_15_13_1	mat_16_13_1	mat_17_13_1	mat_18_13_1	mat_19_13_1	mat_20_13_1	mat_13_2	mat_13_1	matm_01_13_1	matm_02_13_1	matm_03_13_1	matm_04_13_1	matm_05_13_1	matm_06_13_1	matm_07_13_1	matm_08_13_1	matm_09_13_1	matm_10_13_1	matm_11_13_1	matm_12_13_1	matm_14_13_1	matm_16_13_1	matm_17_13_1	matm_18_13_1	matm_19_13_1	matm_01_13_2	matm_02_13_2	matm_03_13_2	matm_04_13_2	matm_05_13_2	matm_06_13_2	matm_07_13_2	matm_08_13_2	matm_09_13_2	matm_10_13_2	matm_11_13_2	matm_12_13_2	matm_14_13_2	matm_16_13_2	matm_17_13_2	matm_18_13_2	matm_19_13_2	e_21_13_2	e_22_13_2	e_23_13_2	e_24_13_2	e_21_13_1	e_22_13_1	e_23_13_1	e_24_13_1	em_22_13_2	em_23_13_2	em_22_13_1	em_23_13_1
                                                                                                                                                                                                                      
Reporting_PAC.sheet(t)
show Reporting_PAC
 
endif

else 

group Reporting_2 ER_oil_2 ER_oil_2201_2 ER_Oil_2202_2 ER_elec_2301_2 ER_elec_2302_2 ER_elec_2303_2 ER_elec_2304_2 ER_elec_2305_2 ER_elec_2306_2 _
    ER_elec_2307_2 ER_elec_2308_2 ER_elec_2 ER_gas_2 ER_gas_2401_2 ER_gas_2402_2 ER_gas_2403_2 ER_gas_2404_2 ER_gas_2405_2 ER_gas_2406_2 ER_coal_2 _ 
    ER_Total_2 ER_Agriculture_2 ER_Indus_2 ER_Residential_2 ER_Tertiary_2 ER_Trans_Private_2 ER_Trans_Public_2  _
    ER_Auto_2 ER_AUTO_th_A_2 ER_AUTO_th_B_2 ER_AUTO_th_C_2 ER_AUTO_th_D_2 ER_AUTO_th_E_2 ER_AUTO_th_F_2 ER_AUTO_th_G_2 ER_AUTO_elec_A_2 _
    ER_AUTO_elec_B_2 ER_AUTO_elec_C_2 ER_AUTO_elec_D_2 ER_AUTO_elec_E_2 ER_AUTO_elec_F_2 ER_AUTO_elec_G_2 ER_Auto_Coal_2 ER_Auto_th_2 _ 
    ER_Auto_Elec_2 ER_Auto_Gas_2 ER_NEWAUTO_2 ER_NEWAUTO_th_2 ER_newauto_th_A_2 ER_newauto_th_B_2 ER_newauto_th_C_2 ER_newauto_th_D_2 _
    ER_newauto_th_E_2 ER_newauto_th_F_2 ER_newauto_th_G_2 ER_NEWAUTO_elec_2 ER_NEWAUTO_elec_A_2 ER_NEWAUTO_elec_B_2 ER_NEWAUTO_elec_C_2 _
    ER_NEWAUTO_elec_D_2 ER_NEWAUTO_elec_E_2 ER_NEWAUTO_elec_F_2 ER_NEWAUTO_elec_G_2 ER_Agriculture_coal_2 ER_Indus_coal_2 ER_Residential_coal_2 _
    ER_Tertiary_coal_2 ER_Trans_Private_coal_2 ER_Trans_Public_coal_2 ER_Agriculture_oil_2 ER_Indus_oil_2 _
    ER_Residential_oil_2 ER_Tertiary_oil_2 ER_Trans_Private_oil_2 ER_Trans_Public_oil_2 ER_Agriculture_elec_2 ER_Indus_elec_2 _
    ER_Residential_elec_2 ER_Tertiary_elec_2 ER_Trans_Private_elec_2 ER_Trans_Public_elec_2  ER_Agriculture_gas_2 ER_Indus_gas_2 _
    ER_Residential_gas_2 ER_Tertiary_gas_2 ER_Trans_Private_gas_2 ER_Trans_Public_gas_2 ER_buil_2 ER_buil_A_2 ER_buil_B_2 ER_buil_C_2 _
    ER_buil_D_2 ER_buil_E_2 ER_buil_F_2 ER_buil_G_2 TTCO_VOL_signal_2 TTCO_VOL_signal_0 PGDP_2 PGDP_0 (GDP_2/GDP_0-1)*100 _
    (CH_2/CH_0-1)*100 (I_2/I_0-1)*100 (X_2/X_0-1)*100 (M_2/M_0-1)*100 (DC_val_2/(PGDP_2*GDP_2)-DC_val_0/(PGDP_0*GDP_0))*100 (UnR_TOT_2-UnR_TOT_0)*100 _
    (L_2/L_0-1)*100 ((W_S_2/PCh_2)/(W_S_0/PCH_0)-1)*100 infl_FR_2-infl_FR_0 R_2-R_0 (DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL_0/(PGDP_0*GDP_0))*100 _
    (DP_G_VAL_2-DP_G_VAL_0)*100  EMS_TOT_2/@elem(EMS_TOT,%baseyear)*100 100*(GDP_2/GDP_0-1) 100*(CH_2/CH_0-1) 100*(I_2/I_0-1) 100*(X_2/X_0-1) 100*(M_2/M_0-1) _
    100*(UNR_TOT_2-UNR_TOT_0)  100*(L_2/L_0-1) 100*((W_2/PCH_2)/(W_0/PCH_0)-1) 100*(PCH_2/PCH_0-1) 100*(R_2-R_0) 100*(Debt_g_val_2/(PGDP_2*GDP_2)-Debt_g_val_0/(PGDP_0*GDP_0)) _
    100*(DP_G_val_2-DP_G_val_0) 100*(GDP_2/@ELEM(GDP_2,"2006")) 100*(EMS_TOT_2/EMS_TOT_0-1) 100*(EMS_TOT_2/@ELEM(EMS_TOT_2,"2006")) _
    EMS_DC_04_2	EMS_DC_05_2	EMS_DC_2	EMS_HH_2	EMS_HH_21_2	EMS_HH_21_H01_2	EMS_HH_22_2	EMS_HH_22_H01_2	EMS_HH_24_2	EMS_HH_24_H01_2	EMS_SEC_TOT_01_2 _
    EMS_SEC_TOT_02_2	EMS_SEC_TOT_03_2	EMS_SEC_TOT_04_2	EMS_SEC_TOT_05_2	EMS_SEC_TOT_06_2	EMS_SEC_TOT_07_2	EMS_SEC_TOT_08_2	EMS_SEC_TOT_09_2	EMS_SEC_TOT_10_2	EMS_SEC_TOT_11_2	EMS_SEC_TOT_12_2	EMS_SEC_TOT_13_2	EMS_SEC_TOT_14_2 _
    EMS_SEC_TOT_15_2	EMS_SEC_TOT_16_2	EMS_SEC_TOT_17_2	EMS_SEC_TOT_18_2	EMS_SEC_TOT_19_2	EMS_SEC_TOT_2	EMS_SEC_TOT_20_2	EMS_SEC_TOT_21_05_2	EMS_SEC_TOT_21_06_2	EMS_SEC_TOT_21_07_2	EMS_SEC_TOT_21_08_2 _
    EMS_SEC_TOT_21_10_2	EMS_SEC_TOT_21_12_2	EMS_SEC_TOT_21_19_2	EMS_SEC_TOT_21_20_2	EMS_SEC_TOT_21_2304_2	EMS_SEC_TOT_2201_2	EMS_SEC_TOT_22_01_2	EMS_SEC_TOT_22_02_2	EMS_SEC_TOT_22_03_2 _
    EMS_SEC_TOT_22_04_2	EMS_SEC_TOT_22_05_2	EMS_SEC_TOT_22_06_2	EMS_SEC_TOT_22_07_2	EMS_SEC_TOT_22_08_2	EMS_SEC_TOT_22_09_2	EMS_SEC_TOT_22_12_2	EMS_SEC_TOT_22_13_2	EMS_SEC_TOT_22_14_2 _
    EMS_SEC_TOT_22_15_2	EMS_SEC_TOT_22_16_2	EMS_SEC_TOT_22_17_2	EMS_SEC_TOT_22_18_2	EMS_SEC_TOT_22_19_2	EMS_SEC_TOT_22_20_2	EMS_SEC_TOT_22_2201_2	EMS_SEC_TOT_22_2302_2	EMS_SEC_TOT_2302_2 _
    EMS_SEC_TOT_2303_2	EMS_SEC_TOT_2304_2	EMS_SEC_TOT_2401_2	EMS_SEC_TOT_24_01_2	EMS_SEC_TOT_24_02_2	EMS_SEC_TOT_24_03_2	EMS_SEC_TOT_24_04_2	EMS_SEC_TOT_24_05_2	EMS_SEC_TOT_24_06_2 _
    EMS_SEC_TOT_24_07_2	EMS_SEC_TOT_24_08_2	EMS_SEC_TOT_24_09_2	EMS_SEC_TOT_24_10_2	EMS_SEC_TOT_24_11_2	EMS_SEC_TOT_24_12_2	EMS_SEC_TOT_24_13_2 _
    EMS_SEC_TOT_24_14_2	EMS_SEC_TOT_24_15_2	EMS_SEC_TOT_24_16_2	EMS_SEC_TOT_24_17_2	EMS_SEC_TOT_24_18_2	EMS_SEC_TOT_24_19_2	EMS_SEC_TOT_24_20_2 _
    EMS_SEC_TOT_24_2201_2	EMS_SEC_TOT_24_2303_2	EMS_SEC_TOT_24_2401_2	EMS_SECSOU_2	EMS_SECSOU_21_2	EMS_SECSOU_22_2	EMS_SECSOU_24_2	EMS_SOU_2	EMS_SOU_21_2	EMS_SOU_22_2	EMS_SOU_24_2	EMS_TOT_2 _ 
    Q_MTEP_EP_2 Q_MTEP_EP_21_2 Q_MTEP_EP_21_21_2 Q_MTEP_EP_2201_2 Q_MTEP_EP_2202_2 Q_MTEP_EP_22_2201_2 Q_MTEP_EP_22_2202_2 Q_MTEP_EP_2301_2 _
    Q_MTEP_EP_2302_2 Q_MTEP_EP_2303_2 Q_MTEP_EP_2304_2 Q_MTEP_EP_2305_2 Q_MTEP_EP_2306_2 Q_MTEP_EP_2307_2 Q_MTEP_EP_2308_2 Q_MTEP_EP_23_2301_2 _
    Q_MTEP_EP_23_2302_2 Q_MTEP_EP_23_2303_2 Q_MTEP_EP_23_2304_2 Q_MTEP_EP_23_2305_2 Q_MTEP_EP_23_2306_2 Q_MTEP_EP_23_2307_2 Q_MTEP_EP_23_2308_2 _
    Q_MTEP_EP_2401_2 Q_MTEP_EP_2402_2 Q_MTEP_EP_2403_2 Q_MTEP_EP_2404_2 Q_MTEP_EP_2405_2 Q_MTEP_EP_2406_2 Q_MTEP_EP_24_2401_2 Q_MTEP_EP_24_2402_2 _
    Q_MTEP_EP_24_2403_2 Q_MTEP_EP_24_2404_2 Q_MTEP_EP_24_2405_2 Q_MTEP_EP_24_2406_2 Q_MTEP_INDUS_21_2 Q_MTEP_INDUS_22_2 Q_MTEP_INDUS_23_2 Q_MTEP_INDUS_24_2 _
    PHIY_EF_TOT_22_2201_2 PHIY_EF_TOT_22_2202_2 PHIY_EF_TOT_23_2301_2 PHIY_EF_TOT_23_2302_2 PHIY_EF_TOT_23_2303_2 PHIY_EF_TOT_23_2304_2 PHIY_EF_TOT_23_2305_2 _
    PHIY_EF_TOT_23_2306_2 PHIY_EF_TOT_23_2307_2 PHIY_EF_TOT_23_2308_2 PHIY_EF_TOT_24_2401_2 PHIY_EF_TOT_24_2402_2 PHIY_EF_TOT_24_2403_2 PHIY_EF_TOT_24_2404_2 _
    PHIY_EF_TOT_24_2405_2 PHIY_EF_TOT_24_2406_2 IC_HH_22_h01_2 IC_HH_24_h01_2 _
    KM_auto_H01_2 KM_TRAVELER_18_H01_2 KM_TRAV_AUTO_LD_H01_2 KM_TRAV_AUTO_CD_H01_2 KM_TRAVELER_14_H01_2 KM_TRAVELER_15_H01_2 KM_TRAVELER_CD_H01_2 KM_TRAVELER_LD_H01_2 _
    KM_AUTO_H01_0 KM_TRAVELER_18_H01_0 KM_TRAV_AUTO_LD_H01_0 KM_TRAV_AUTO_CD_H01_0 KM_TRAVELER_14_H01_0 _
    KM_TRAVELER_15_H01_0 KM_TRAVELER_CD_H01_0 KM_TRAVELER_LD_H01_0

    

  Reporting_2.sheet(t)
  show Reporting_2
  

   group Reporting_3 100*(GDP_2/GDP_0-1) 100*((VA_2-VA_20_2)/(VA_0-VA_20_0)-1) 100*((CH_2-CH_0)/GDP_0) 100*((CH_03_2-CH_03_0)/GDP_0) _
               100*((G_2-G_0)/GDP_0) 100*((I_2-I_0)/GDP_0) 100*((DS_2-DS_0)/GDP_0) 100*((X_2-X_0)/GDP_0)-100*((M_2-M_0)/GDP_0) _
               100*((CH_2-CH_13_2-(CH_0-CH_13_0))/GDP_0) 100*((I_2+CH_13_2-(I_0+CH_13_0))/GDP_0) 100*((I_2-IA_20_2-(I_0-IA_20_0))/GDP_0) 100*((CH_13_2-CH_13_0)/GDP_0) 100*((IA_20_2-IA_20_0)/GDP_0) _
               100*(CH_2/CH_0-1) 100*(CH_03_2/CH_03_0-1) 100*(G_2/G_0-1) 100*(I_2/I_0-1) 100*((I_2-IA_20_2)/(I_0-IA_20_0)-1) 100*(X_2/X_0-1) 100*(M_2/M_0-1) _
              100*((CH_2-CH_13_2)/(CH_0-CH_13_0)-1) 100*((I_2+CH_13_2)/(I_0+CH_13_0)-1) 100*((CH_13_2)/(CH_13_0)-1) 100*((IA_20_2)/(IA_20_0)-1) _
      100*(DISPINC_VAL_2/DISPINC_VAL_0-1) 100*(DISPINC_VAL_2/DISPINC_VAL_0*PCH_0/PCH_2-1) 100*(DISPINC_VAL_2/DISPINC_VAL_0*L_0/L_2-1) _
      100*(DISPINC_VAL_2/DISPINC_VAL_0*L_0/L_2*PCH_0/PCH_2-1) 100*((dispinc_val_H01_2-(PCH_2*CH_2-pch_13_2*ch_13_2))/dispinc_val_H01_2-(dispinc_val_H01_0-(PCH_0*CH_0-pch_13_0*ch_13_0))/dispinc_val_H01_0) 100*(PCH_2/PCH_0-1) 100*(PY_2/PY_0-1) 100*(PX_2/PX_0-1) _
      100*(PM_2/PM_0-1) 100*(W_2/W_0-1) 100*((W_2/PCH_2)/(W_0/PCH_0)-1) 100*(CL_2/CL_0-1) 100*((CL_2/PVA_2)/(CL_0/PVA_0)-1) ((L_2/L_0)-1)*100 _
      L_2-L_0 100*(UNR_TOT_2-UNR_TOT_0) 100*(DC_VAL_2/(GDP_2*PGDP_2)-DC_VAL_0/(GDP_0*PGDP_0)) _
      100*((M_21_2*PM_21_2-X_21_2*PX_21_2+M_22_2*PM_22_2-X_22_2*PX_22_2+M_23_2*PM_23_2-X_23_2*PX_23_2+M_24_2*PM_24_2-X_24_2*PX_24_2)/(GDP_2*PGDP_2))-100*((M_21_0*PM_21_0-X_21_0*PX_21_0+M_22_0*PM_22_0-X_22_0*PX_22_0+M_23_0*PM_23_0-X_23_0*PX_23_0+M_24_0*PM_24_0-X_24_0*PX_24_0)/(GDP_0*PGDP_0)) 100*((-SP_G_VAL_2)/(GDP_2*PGDP_2)-(-SP_G_VAL_0)/(GDP_0*PGDP_0)) _
      100*((-BF_G_VAL_2)/(GDP_2*PGDP_2)-(-BF_G_VAL_0)/(GDP_0*PGDP_0)) (DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL_0/(PGDP_0*GDP_0))*100 _ 
DEP_val_2 REC_VAL_2 (0-BF_G_VAL_2) DEBT_G_VAL_2 REC_TCO_VAL_2 ENERT_22_2*PENERT_22_2 ENERT_24_2*PENERT_24_2 ENERT_23_2*PENERT_23_2 ENERT_21_2*PENERT_21_2 TCO_VAL_HH_2 _
TCO_VAL_HH_21_H01_2+TCO_VAL_HH_24_H01_2+TCO_VAL_HH_22_H01_2*(Q_MTEP_H_BUIL_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) _
TCO_VAL_HH_22_H01_2*(Q_MTEP_H_BUIL_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) TCO_VAL_HH_24_H01_2 TCO_VAL_HH_21_H01_2 _
TCO_VAL_HH_22_H01_2*(Q_MTEP_H_AUTO_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) TCO_VAL_SEC_2 REC_TCO_VAL_ETS_2  _
TCO_VAL_22_04_2+TCO_VAL_22_05_2+TCO_VAL_22_06_2+TCO_VAL_22_07_2+TCO_VAL_22_08_2+TCO_VAL_22_09_2+TCO_VAL_22_10_2+TCO_VAL_22_18_2+TCO_VAL_22_21_2+tco_val_22_2201_2+tco_val_22_2202_2+TCO_VAL_22_2301_2+TCO_VAL_22_2302_2+TCO_VAL_22_2303_2+TCO_VAL_22_2304_2+TCO_VAL_22_2305_2+TCO_VAL_22_2306_2+TCO_VAL_22_2307_2+TCO_VAL_22_2308_2+TCO_VAL_22_2401_2+TCO_VAL_22_2402_2+TCO_VAL_22_2403_2+TCO_VAL_22_2404_2+TCO_VAL_22_2405_2+TCO_VAL_22_2406_2 _ 	
TCO_VAL_24_04_2+TCO_VAL_24_05_2+TCO_VAL_24_06_2+TCO_VAL_24_07_2+TCO_VAL_24_08_2+TCO_VAL_24_09_2+TCO_VAL_24_10_2+TCO_VAL_24_18_2+TCO_VAL_24_21_2+tco_val_24_2201_2+tco_val_24_2202_2+TCO_VAL_24_2301_2+TCO_VAL_24_2302_2+TCO_VAL_24_2303_2+TCO_VAL_24_2304_2+TCO_VAL_24_2305_2+TCO_VAL_24_2306_2+TCO_VAL_24_2307_2+TCO_VAL_24_2308_2+TCO_VAL_24_2401_2+TCO_VAL_24_2402_2+TCO_VAL_24_2403_2+TCO_VAL_24_2404_2+TCO_VAL_24_2405_2+TCO_VAL_24_2406_2 _	
TCO_VAL_21_04_2+TCO_VAL_21_05_2+TCO_VAL_21_06_2+TCO_VAL_21_07_2+TCO_VAL_21_08_2+TCO_VAL_21_09_2+TCO_VAL_21_10_2+TCO_VAL_21_18_2+TCO_VAL_21_21_2+tco_val_21_2201_2+tco_val_21_2202_2+TCO_VAL_21_2301_2+TCO_VAL_21_2302_2+TCO_VAL_21_2303_2+TCO_VAL_21_2304_2+TCO_VAL_21_2305_2+TCO_VAL_21_2306_2+TCO_VAL_21_2307_2+TCO_VAL_21_2308_2+TCO_VAL_21_2401_2+TCO_VAL_21_2402_2+TCO_VAL_21_2403_2+TCO_VAL_21_2404_2+TCO_VAL_21_2405_2+TCO_VAL_21_2406_2 _
REC_TCO_VAL_NETS_2 _
TCO_VAL_22_01_2+TCO_VAL_22_02_2+TCO_val_22_03_2+TCO_VAL_22_11_2+TCO_VAL_22_12_2+TCO_VAL_22_13_2 _
TCO_VAL_24_01_2+TCO_VAL_24_02_2+TCO_val_24_03_2+TCO_VAL_24_11_2+TCO_VAL_24_12_2+TCO_VAL_24_13_2 _
TCO_VAL_21_01_2+TCO_VAL_21_02_2+TCO_val_21_03_2+TCO_VAL_21_11_2+TCO_VAL_21_12_2+TCO_VAL_21_13_2 _
TCO_VAL_SEC_14_2+TCO_VAL_SEC_15_2+TCO_VAL_SEC_16_2+TCO_VAL_SEC_17_2+TCO_VAL_SEC_19_2+TCO_VAL_SEC_20_2	TCO_VAL_22_14_2+TCO_VAL_22_15_2+TCO_VAL_22_16_2+TCO_VAL_22_17_2+TCO_VAL_22_19_2+TCO_VAL_22_20_2 _ 
TCO_VAL_24_14_2+TCO_VAL_24_15_2+TCO_VAL_24_16_2+TCO_VAL_24_17_2+TCO_VAL_24_19_2+TCO_VAL_24_20_2 _
TCO_VAL_21_14_2+TCO_VAL_21_15_2+TCO_VAL_21_16_2+TCO_VAL_21_17_2+TCO_VAL_21_19_2+TCO_VAL_21_20_2 _
DEP_val_0 REC_VAL_0 (0-BF_G_VAL_0) DEBT_G_VAL_0 REC_TCO_VAL_0	ENERT_22_0*PENERT_22_0	ENERT_24_0*PENERT_24_0	ENERT_23_0*PENERT_23_0 ENERT_21_0*PENERT_21_0 _
TCO_VAL_HH_0 _
TCO_VAL_HH_21_H01_0+TCO_VAL_HH_24_H01_0+TCO_VAL_HH_22_H01_0*(Q_MTEP_H_BUIL_22_0)/(Q_MTEP_H_BUIL_22_0+Q_MTEP_H_AUTO_22_0) _
TCO_VAL_HH_22_H01_0*(Q_MTEP_H_BUIL_22_0)/(Q_MTEP_H_BUIL_22_0+Q_MTEP_H_AUTO_22_0) _
TCO_VAL_HH_24_H01_0 _
TCO_VAL_HH_21_H01_0 _
TCO_VAL_HH_22_H01_0*(Q_MTEP_H_AUTO_22_0)/(Q_MTEP_H_BUIL_22_0+Q_MTEP_H_AUTO_22_0) _
TCO_VAL_SEC_0 REC_TCO_VAL_ETS_0 _
TCO_VAL_22_04_0+TCO_VAL_22_05_0+TCO_VAL_22_06_0+TCO_VAL_22_07_0+TCO_VAL_22_08_0+TCO_VAL_22_09_0+TCO_VAL_22_10_0+TCO_VAL_22_18_0+TCO_VAL_22_21_0+tco_val_22_2201_0+tco_val_22_2202_0+TCO_VAL_22_2301_0+TCO_VAL_22_2302_0+TCO_VAL_22_2303_0+TCO_VAL_22_2304_0+TCO_VAL_22_2305_0+TCO_VAL_22_2306_0+TCO_VAL_22_2307_0+TCO_VAL_22_2308_0+TCO_VAL_22_2401_0+TCO_VAL_22_2402_0+TCO_VAL_22_2403_0+TCO_VAL_22_2404_0+TCO_VAL_22_2405_0+TCO_VAL_22_2406_0 _
TCO_VAL_24_04_0+TCO_VAL_24_05_0+TCO_VAL_24_06_0+TCO_VAL_24_07_0+TCO_VAL_24_08_0+TCO_VAL_24_09_0+TCO_VAL_24_10_0+TCO_VAL_24_18_0+TCO_VAL_24_21_0+tco_val_24_2201_0+TCO_VAL_24_2202_0+TCO_VAL_24_2301_0+TCO_VAL_24_2302_0+TCO_VAL_24_2303_0+TCO_VAL_24_2304_0+TCO_VAL_24_2305_0+TCO_VAL_24_2306_0+TCO_VAL_24_2307_0+TCO_VAL_24_2308_0+TCO_VAL_24_2401_0+TCO_VAL_24_2402_0+TCO_VAL_24_2403_0+TCO_VAL_24_2404_0+TCO_VAL_24_2405_0+TCO_VAL_24_2406_0 _
TCO_VAL_21_04_0+TCO_VAL_21_05_0+TCO_VAL_21_06_0+TCO_VAL_21_07_0+TCO_VAL_21_08_0+TCO_VAL_21_09_0+TCO_VAL_21_10_0+TCO_VAL_21_18_0+TCO_VAL_21_21_0+tco_val_21_2201_0+tco_val_21_2202_0+TCO_VAL_21_2301_0+TCO_VAL_21_2302_0+TCO_VAL_21_2303_0+TCO_VAL_21_2304_0+TCO_VAL_21_2305_0+TCO_VAL_21_2306_0+TCO_VAL_21_2307_0+TCO_VAL_21_2308_0+TCO_VAL_21_2401_0+TCO_VAL_21_2402_0+TCO_VAL_21_2403_0+TCO_VAL_21_2404_0+TCO_VAL_21_2405_0+TCO_VAL_21_2406_0 _
REC_TCO_VAL_NETS_0 _
TCO_VAL_22_01_0+TCO_VAL_22_02_0+TCO_val_22_03_0+TCO_VAL_22_11_0+TCO_VAL_22_12_0+TCO_VAL_22_13_0 _
TCO_VAL_24_01_0+TCO_VAL_24_02_0+TCO_val_24_03_0+TCO_VAL_24_11_0+TCO_VAL_24_12_0+TCO_VAL_24_13_0 _
TCO_VAL_21_01_0+TCO_VAL_21_02_0+TCO_val_21_03_0+TCO_VAL_21_11_0+TCO_VAL_21_12_0+TCO_VAL_21_13_0 _ 
TCO_VAL_SEC_14_0+TCO_VAL_SEC_15_0+TCO_VAL_SEC_16_0+TCO_VAL_SEC_17_0+TCO_VAL_SEC_19_0+TCO_VAL_SEC_20_0	TCO_VAL_22_14_0+TCO_VAL_22_15_0+TCO_VAL_22_16_0+TCO_VAL_22_17_0+TCO_VAL_22_19_0+TCO_VAL_22_20_0 TCO_VAL_24_14_0+TCO_VAL_24_15_0+TCO_VAL_24_16_0+TCO_VAL_24_17_0+TCO_VAL_24_19_0+TCO_VAL_24_20_0 _ 
TCO_VAL_21_14_0+TCO_VAL_21_15_0+TCO_VAL_21_16_0+TCO_VAL_21_17_0+TCO_VAL_21_19_0+TCO_VAL_21_20_0 _
EMS_TOT_2 _
EMS_HH_22_2+EMS_SEC_22_01_2+EMS_SEC_22_02_2+EMS_SEC_22_03_2+EMS_SEC_22_04_2+EMS_SEC_22_05_2+EMS_SEC_22_06_2+EMS_SEC_22_07_2+EMS_SEC_22_08_2+EMS_SEC_22_09_2+EMS_SEC_22_12_2+EMS_SEC_22_13_2+EMS_SEC_22_14_2+EMS_SEC_22_15_2+EMS_SEC_22_16_2+EMS_SEC_22_17_2+EMS_SEC_22_18_2+EMS_SEC_22_19_2+EMS_SEC_22_20_2 EMS_HH_24_2+EMS_SEC_2401_2+EMS_SEC_24_01_2+EMS_SEC_24_02_2+EMS_SEC_24_03_2+EMS_SEC_24_04_2+EMS_SEC_24_05_2+EMS_SEC_24_06_2+EMS_SEC_24_07_2+EMS_SEC_24_08_2+EMS_SEC_24_09_2+EMS_SEC_24_10_2+EMS_SEC_24_11_2+EMS_SEC_24_12_2+EMS_SEC_24_13_2+EMS_SEC_24_14_2+EMS_SEC_24_15_2+EMS_SEC_24_16_2+EMS_SEC_24_17_2+EMS_SEC_24_18_2+EMS_SEC_24_19_2+EMS_SEC_24_20_2 _
       EMS_SEC_2302_2+EMS_SEC_2303_2+EMS_SEC_2304_2 EMS_HH_21_2+EMS_SEC_21_05_2+EMS_SEC_21_06_2+EMS_SEC_21_07_2+EMS_SEC_21_08_2+EMS_SEC_21_10_2+EMS_SEC_21_12_2+EMS_SEC_21_19_2+EMS_SEC_21_20_2 EMS_DC_05_2+EMS_DC_04_2 EMS_HH_21_2+EMS_HH_24_2+EMS_HH_22_2*(@ELEM(PENER_BUIL_H01_22,2006)*ENER_BUIL_H01_22_2)/EXP_22_H01_2 _
       EMS_HH_22_2*(EXP_AUTO_H01_22_2/EXP_22_H01_2) _
       EMS_SEC_04_2+EMS_SEC_05_2+EMS_SEC_06_2+EMS_SEC_07_2+EMS_SEC_08_2+EMS_SEC_09_2+EMS_SEC_10_2+EMS_SEC_18_2+EMS_SEC_21_19_2+EMS_SEC_21_20_2+EMS_SEC_2201_2+EMS_SEC_2302_2+EMS_SEC_2303_2+EMS_SEC_2304_2+EMS_SEC_2401_2+EMS_DC_04_2+EMS_DC_05_2 _
        EMS_SEC_01_2+EMS_SEC_02_2+EMS_SEC_03_2+EMS_SEC_11_2+EMS_SEC_12_2+EMS_SEC_13_2 _             
        EMS_SEC_14_2+EMS_SEC_15_2+EMS_SEC_16_2+EMS_SEC_17_2+EMS_SEC_19_2+EMS_SEC_20_2 _               
        EMS_TOT_0 EMS_HH_22_0+EMS_SEC_22_01_0+EMS_SEC_22_02_0+EMS_SEC_22_03_0+EMS_SEC_22_04_0+EMS_SEC_22_05_0+EMS_SEC_22_06_0+EMS_SEC_22_07_0+EMS_SEC_22_08_0+EMS_SEC_22_09_0+EMS_SEC_22_12_0+EMS_SEC_22_13_0+EMS_SEC_22_14_0+EMS_SEC_22_15_0+EMS_SEC_22_16_0+EMS_SEC_22_17_0+EMS_SEC_22_18_0+EMS_SEC_22_19_0+EMS_SEC_22_20_0 EMS_HH_24_0+EMS_SEC_2401_0+EMS_SEC_24_01_0+EMS_SEC_24_02_0+EMS_SEC_24_03_0+EMS_SEC_24_04_0+EMS_SEC_24_05_0+EMS_SEC_24_06_0+EMS_SEC_24_07_0+EMS_SEC_24_08_0+EMS_SEC_24_09_0+EMS_SEC_24_10_0+EMS_SEC_24_11_0+EMS_SEC_24_12_0+EMS_SEC_24_13_0+EMS_SEC_24_14_0+EMS_SEC_24_15_0+EMS_SEC_24_16_0+EMS_SEC_24_17_0+EMS_SEC_24_18_0+EMS_SEC_24_19_0+EMS_SEC_24_20_0 _
        EMS_SEC_2302_0+EMS_SEC_2303_0+EMS_SEC_2304_0 EMS_HH_21_0+EMS_SEC_21_05_0+EMS_SEC_21_06_0+EMS_SEC_21_07_0+EMS_SEC_21_08_0+EMS_SEC_21_10_0+EMS_SEC_21_12_0+EMS_SEC_21_19_0+EMS_SEC_21_20_0 _
       EMS_DC_05_0+EMS_DC_04_0 _
       EMS_HH_21_0+EMS_HH_24_0+EMS_HH_22_0*(@ELEM(PENER_BUIL_H01_22,2006)*ENER_BUIL_H01_22_0)/EXP_22_H01_0 _
       EMS_HH_22_0*(EXP_AUTO_H01_22_0/EXP_22_H01_0) _
       EMS_SEC_04_0+EMS_SEC_05_0+EMS_SEC_06_0+EMS_SEC_07_0+EMS_SEC_08_0+EMS_SEC_09_0+EMS_SEC_10_0+EMS_SEC_18_0+EMS_SEC_21_19_0+EMS_SEC_21_20_0+EMS_SEC_2201_0+EMS_SEC_2302_0+EMS_SEC_2303_0+EMS_SEC_2304_0+EMS_SEC_2401_0+EMS_DC_04_0+EMS_DC_05_0 _
       EMS_SEC_01_0+EMS_SEC_02_0+EMS_SEC_03_0+EMS_SEC_11_0+EMS_SEC_12_0+EMS_SEC_13_0 _
       EMS_SEC_14_0+EMS_SEC_15_0+EMS_SEC_16_0+EMS_SEC_17_0+EMS_SEC_19_0+EMS_SEC_20_0 ER_TOTAL_2 _
       ER_TRANS_PRIVATE_2+ER_TRANS_PUBLIC_2 ER_TRANS_PRIVATE_2 ER_TRANS_PRIVATE_OIL_2 ER_TRANS_PRIVATE_ELEC_2 ER_TRANS_PUBLIC_2 ER_TRANS_PUBLIC_OIL_2 _
       ER_TRANS_PUBLIC_ELEC_2 ER_RESIDENTIAL_2 ER_RESIDENTIAL_OIL_2 ER_RESIDENTIAL_ELEC_2 ER_RESIDENTIAL_GAS_2 ER_AGRICULTURE_2 ER_AGRICULTURE_OIL_2 _
       ER_AGRICULTURE_ELEC_2 ER_INDUS_2 ER_INDUS_OIL_2 ER_INDUS_ELEC_2 ER_INDUS_GAS_2 ER_INDUS_COAL_2 ER_TERTIARY_2 ER_TERTIARY_OIL_2 ER_TERTIARY_ELEC_2 ER_TERTIARY_GAS_2 ER_OIL_2 ER_GAS_2 ER_COAL_2 ER_ELEC_2 ER_ELEC_2301_2 ER_ELEC_2302_2 ER_ELEC_2303_2 _
        ER_ELEC_2304_2 ER_ELEC_2305_2 ER_ELEC_2306_2 ER_ELEC_2307_2 ER_ELEC_2308_2 ER_TOTAL_0 ER_TRANS_PRIVATE_0+ER_TRANS_PUBLIC_0 ER_TRANS_PRIVATE_0 ER_TRANS_PRIVATE_OIL_0 ER_TRANS_PRIVATE_ELEC_0 ER_TRANS_PUBLIC_0 ER_TRANS_PUBLIC_OIL_0 ER_TRANS_PUBLIC_ELEC_0 ER_RESIDENTIAL_0 ER_RESIDENTIAL_OIL_0 ER_RESIDENTIAL_ELEC_0 ER_RESIDENTIAL_GAS_0 ER_AGRICULTURE_0 ER_AGRICULTURE_OIL_0 ER_AGRICULTURE_ELEC_0 ER_INDUS_0 ER_INDUS_OIL_0 ER_INDUS_ELEC_0 ER_INDUS_GAS_0 ER_INDUS_COAL_0 ER_TERTIARY_0 ER_TERTIARY_OIL_0 ER_TERTIARY_ELEC_0 ER_TERTIARY_GAS_0 ER_OIL_0 _
        ER_GAS_0 ER_COAL_0 ER_ELEC_0 ER_ELEC_2301_0 ER_ELEC_2302_0 ER_ELEC_2303_0 ER_ELEC_2304_0 ER_ELEC_2305_0 ER_ELEC_2306_0 ER_ELEC_2307_0 ER_ELEC_2308_0 ENER_H01_2 _
        @ELEM(PENER_BUIL_H01,2006)*ENER_BUIL_H01_2 EXP_AUTO_H01_2 ENER_H01_0 @ELEM(PENER_BUIL_H01,2006)*ENER_BUIL_H01_0 EXP_AUTO_H01_0 KM_auto_H01_2 _
        KM_TRAVELER_18_H01_2 KM_TRAV_AUTO_LD_H01_2 KM_TRAV_AUTO_CD_H01_2 KM_TRAVELER_14_H01_2 KM_TRAVELER_15_H01_2 KM_TRAVELER_CD_H01_2 KM_TRAVELER_LD_H01_2 _
        KM_AUTO_H01_0 KM_TRAVELER_18_H01_0 KM_TRAV_AUTO_LD_H01_0 KM_TRAV_AUTO_CD_H01_0 KM_TRAVELER_14_H01_0 _
        KM_TRAVELER_15_H01_0 KM_TRAVELER_CD_H01_0 KM_TRAVELER_LD_H01_0 _
        ER_Auto_2 ER_AUTO_th_A_2 ER_AUTO_th_B_2 ER_AUTO_th_C_2 ER_AUTO_th_D_2 ER_AUTO_th_E_2 ER_AUTO_th_F_2 ER_AUTO_th_G_2 _
        ER_AUTO_elec_A_2 ER_AUTO_elec_B_2 ER_AUTO_elec_C_2 ER_AUTO_elec_D_2 ER_AUTO_elec_E_2 ER_AUTO_elec_F_2 ER_AUTO_elec_G_2 ER_Auto_Coal_2 ER_auto_th_2 ER_Auto_Elec_2 ER_Auto_Gas_2 _
        ER_NEWAUTO_2 ER_NEWAUTO_th_2 ER_newauto_th_A_2 ER_newauto_th_B_2 ER_newauto_th_C_2 ER_newauto_th_D_2 ER_newauto_th_E_2 ER_newauto_th_F_2  ER_newauto_th_G_2  _
        ER_NEWAUTO_elec_2 ER_NEWAUTO_elec_A_2 ER_NEWAUTO_elec_B_2 ER_NEWAUTO_elec_C_2 ER_NEWAUTO_elec_D_2 ER_NEWAUTO_elec_E_2 ER_NEWAUTO_elec_F_2 ER_NEWAUTO_elec_G_2 _
        ER_Auto_0 ER_AUTO_th_A_0 ER_AUTO_th_B_0 ER_AUTO_th_C_0 ER_AUTO_th_D_0 ER_AUTO_th_E_0 ER_AUTO_th_F_0 ER_AUTO_th_G_0 ER_AUTO_elec_A_0 ER_AUTO_elec_B_0 ER_AUTO_elec_C_0 ER_AUTO_elec_D_0 ER_AUTO_elec_E_0 ER_AUTO_elec_F_0 ER_AUTO_elec_G_0 ER_Auto_Coal_0 ER_auto_th_0 ER_Auto_Elec_0 ER_Auto_Gas_0 _
        ER_NEWAUTO_0 ER_NEWAUTO_th_0 ER_newauto_th_A_0 ER_newauto_th_B_0 ER_newauto_th_C_0 ER_newauto_th_D_0 ER_newauto_th_E_0 ER_newauto_th_F_0 _
        ER_NEWAUTO_elec_0 ER_NEWAUTO_elec_A_0 ER_NEWAUTO_elec_B_0 ER_NEWAUTO_elec_C_0 ER_NEWAUTO_elec_D_0 ER_NEWAUTO_elec_E_0 ER_NEWAUTO_elec_F_0 _
        ER_BUIL_2 ER_BUIL_A_2 ER_BUIL_B_2 ER_BUIL_C_2 ER_BUIL_D_2 ER_BUIL_E_2 ER_BUIL_F_2 ER_BUIL_G_2  SUB_REHAB_VAL_2 tax_cr_19_2 ER_BUIL_0 _
        ER_BUIL_A_0 ER_BUIL_B_0 ER_BUIL_C_0 ER_BUIL_D_0 ER_BUIL_E_0 ER_BUIL_F_0 ER_BUIL_G_0 SUB_REHAB_VAL_0 tax_cr_19_0 _
	  pch_2 pch_0 pgdp_2 pgdp_0 gdp_2 gdp_0 _
       L_01_0	L_02_0	L_03_0	L_04_0	L_05_0	L_06_0	L_07_0	L_08_0	L_09_0	L_10_0	L_11_0 L_12_0 L_13_0 L_14_0 L_15_0	L_16_0	L_17_0	L_18_0	L_19_0	L_20_0	L_21_0 _
      L_2201_0	L_2202_0	L_2301_0	L_2302_0	L_2303_0	L_2304_0	L_2305_0	L_2306_0	L_2307_0 L_2308_0 L_2401_0 L_2402_0 L_2403_0 L_2404_0 L_2405_0 _
	 L_2406_0	L_01_2	L_02_2	L_03_2	L_04_2	L_05_2	L_06_2	L_07_2	L_08_2	L_09_2	L_10_2	L_11_2	L_12_2	L_13_2	L_14_2	L_15_2	L_16_2	L_17_2	L_18_2	L_19_2	L_20_2	L_21_2 _ 
      L_2201_2	L_2202_2	L_2301_2	L_2302_2	L_2303_2	L_2304_2	L_2305_2	L_2306_2	L_2307_2 L_2308_2 _
      L_2401_2	L_2402_2	L_2403_2	L_2404_2	L_2405_2	L_2406_2 _ 	
      VA_01_0	VA_02_0	VA_03_0	VA_04_0	VA_05_0	VA_06_0	VA_07_0	VA_08_0	VA_09_0 VA_10_0 VA_11_0 VA_12_0 VA_13_0 VA_14_0 VA_15_0 VA_16_0 VA_17_0 VA_18_0 VA_19_0 VA_20_0 VA_21_0 _
      VA_2201_0 VA_2202_0 VA_2301_0 VA_2302_0 VA_2303_0 VA_2304_0	VA_2305_0 VA_2306_0 VA_2307_0 VA_2308_0 _
      VA_2401_0 VA_2402_0 VA_2403_0	 VA_2404_0 VA_2405_0 VA_2406_0 _	
      VA_01_2	VA_02_2	VA_03_2 VA_04_2 VA_05_2 VA_06_2 VA_07_2 VA_08_2 VA_09_2 VA_10_2 VA_11_2 VA_12_2 VA_13_2 VA_14_2 VA_15_2 VA_16_2 VA_17_2 VA_18_2 VA_19_2 VA_20_2 VA_21_2 _
      VA_2201_2 VA_2202_2 VA_2301_2 VA_2302_2 VA_2303_2 VA_2304_2	VA_2305_2	 VA_2306_2 VA_2307_2 VA_2308_2 _
      VA_2401_2 VA_2402_2 VA_2403_2 VA_2404_2 VA_2405_2 VA_2406_2 _
TTCO_VOL_21_2 TTCO_VOL_22_2 TTCO_VOL_24_2 TTCO_VOL_21_0 TTCO_VOL_22_0 TTCO_VOL_24_0 DISPINC_VAL_2 DISPINC_VAL_0 _
(gdpter_2/gdpbis_2-1)*100 (gdpter_0/gdpbis_0-1)*100 _
IA_0 IA_01_0 IA_02_0 IA_03_0 IA_04_0 IA_05_0 IA_06_0 IA_07_0 IA_08_0 IA_09_0 IA_10_0 IA_11_0 IA_12_0 IA_13_0 IA_13_20_0 IA_14_0 IA_15_0  IA_16_0 IA_17_0  IA_18_0 IA_19_0 IA_20_0 IA_21_0 _
IA_2201_0 IA_2202_0 IA_2301_0 IA_2302_0 IA_2303_0 IA_2304_0 IA_2305_0 IA_2306_0 IA_2307_0 IA_2308_0 IA_2401_0 IA_2402_0 IA_2403_0 IA_2404_0 IA_2405_0 IA_2406_0   _
REHAB_VAL_0 NEWBUIL_H01_0 PNEWBUIL_H01_0 PREHAB_H01_0 REHAB_H01_0 PEXP_13_H01_0 CH_03_0 PGDP_0 _
IA_2 IA_01_2 IA_02_2 IA_03_2 IA_04_2 IA_05_2 IA_06_2 IA_07_2 IA_08_2 IA_09_2 IA_10_2 IA_11_2 IA_12_2 IA_13_2 IA_13_20_2 IA_14_2 IA_15_2 IA_16_2 IA_17_2 IA_18_2 IA_19_2 IA_20_2 IA_21_2 _
IA_2201_2 IA_2202_2 IA_2301_2 IA_2302_2 IA_2303_2 IA_2304_2 IA_2305_2 IA_2306_2 IA_2307_2 IA_2308_2 IA_2401_2 IA_2402_2 IA_2403_2 IA_2404_2 IA_2405_2 IA_2406_2  _
REHAB_VAL_2 NEWBUIL_H01_2 PNEWBUIL_H01_2 PREHAB_H01_2 REHAB_H01_2 PEXP_13_H01_2 CH_03_2 PGDP_2 _
CH_01_2 CH_02_2 CH_03_2 CH_04_2 CH_05_2 CH_06_2 CH_07_2 CH_08_2 CH_09_2 CH_10 CH_11_2 CH_12_2 CH_13_2 CH_14_2 _
CH_15_2 CH_16_2 CH_17_2 CH_18_2 CH_19_2 CH_20_2 CH_21_2 CH_22_2 CH_23_2 CH_24_2 _ 
CH_01_0 CH_02_0 CH_03_0 CH_04_0 CH_05_0 CH_06_0 CH_07_0 CH_08_0 CH_09_0 CH_10 CH_11_0 CH_12_0 CH_13_0 CH_14_0 _
CH_15_0 CH_16_0 CH_17_0 CH_18_0 CH_19_0 CH_20_0 CH_21_0 CH_22_0 CH_23_0 CH_24_0 _
PCH_01_2 PCH_02_2 PCH_03_2 PCH_04_2 PCH_05_2 PCH_06_2 PCH_07_2 PCH_08_2 PCH_09_2 PCH_10 _
PCH_11_2 PCH_12_2 PCH_13_2 PCH_14_2 PCH_15_2 PCH_16_2 PCH_17_2 PCH_18_2 PCH_19_2 PCH_20_2 _
PCH_21_2 PCH_22_2 PCH_23_2 PCH_24_2 PCH_01_0 PCH_02_0 PCH_03_0 PCH_04_0 PCH_05_0 PCH_06_0 _ 
PCH_07_0 PCH_08_0 PCH_09_0 PCH_10 PCH_11_0 PCH_12_0 PCH_13_0 PCH_14_0 PCH_15_0 PCH_16_0 _ 
PCH_17_0 PCH_18_0 PCH_19_0 PCH_20_0 PCH_21_0 PCH_22_0 PCH_23_0 PCH_24_0 PENER_BUIL_H01_2 PENER_BUIL_H01_0 ENER_BUIL_H01_2 ENER_BUIL_H01_0 pexp_auto_h01_2 pexp_auto_h01_0 _
exp_mobauto_val_h01_2 exp_mobauto_val_h01_0 exp_housing_val_h01_2 exp_housing_val_h01_0 pop_tot _
I_MDE_01_0 I_MDE_02_0 I_MDE_03_0 I_MDE_04_0	I_MDE_05_0 I_MDE_06_0 I_MDE_07_0 I_MDE_08_0	I_MDE_09_0 I_MDE_10_0 I_MDE_11_0 _
 I_MDE_12_0 I_MDE_13_0 I_MDE_19_0 I_MDE_20_0 I_MDE_01_2 I_MDE_02_2 I_MDE_03_2 I_MDE_04_2 I_MDE_05_2 I_MDE_06_2 I_MDE_07_2 I_MDE_08_2 I_MDE_09_2 I_MDE_10_2 I_MDE_11_2 I_MDE_12_2 I_MDE_13_2  _
I_MDE_19_2 I_MDE_20_2 NDI_ADEB_VOL_0 NDI_ADEB_VOL_2 INV_Road PIA_13_19_2 PIA_13_19_0 PIA_13_19_0 PIA_13_20_2 PIA_20_2 PIA_20_0 PIA_15_2 CSPE_0 CSPE_2 _
Carbon_foot_0 Carbon_foot_2 PSUBD_15_2*SUBD_15_2 CU_MWH_PGDP_21_0 CU_MWH_PGDP_22_0 CU_MWH_PGDP_23_0  CU_MWH_PGDP_24_0 CU_MWH_PGDP_2201_0 CU_MWH_PGDP_2202_0 CU_MWH_PGDP_2301_0 _
CU_MWH_PGDP_2302_0 CU_MWH_PGDP_2303_0 CU_MWH_PGDP_2304_0 CU_MWH_PGDP_2305_0 CU_MWH_PGDP_2306_0 CU_MWH_PGDP_2307_0 CU_MWH_PGDP_2308_0 CU_MWH_PGDP_2401_0 CU_MWH_PGDP_2402_0 _
CU_MWH_PGDP_2403_0 CU_MWH_PGDP_2404_0 CU_MWH_PGDP_2405_0 CU_MWH_PGDP_2406_0 CU_MWH_PGDP_21_2 CU_MWH_PGDP_22_2 CU_MWH_PGDP_23_2  CU_MWH_PGDP_24_2 CU_MWH_PGDP_2201_2 _
CU_MWH_PGDP_2202_2 CU_MWH_PGDP_2301_2 CU_MWH_PGDP_2302_2 CU_MWH_PGDP_2303_2 CU_MWH_PGDP_2304_2 CU_MWH_PGDP_2305_2 CU_MWH_PGDP_2306_2 CU_MWH_PGDP_2307_2 CU_MWH_PGDP_2308_2 _
CU_MWH_PGDP_2401_2 CU_MWH_PGDP_2402_2 CU_MWH_PGDP_2403_2 CU_MWH_PGDP_2404_2 CU_MWH_PGDP_2405_2 CU_MWH_PGDP_2406_2 UNR_TOT_0 UNR_TOT_2 _
GDP_0 CH_0 G_0 I_0+CH_13_0 I_0-IA_20_0 CH_13_0 IA_20_0 X_0 M_0 DISPINC_VAL_0/(POP_TOT*PCH_0) L_0 SP_G_VAL_0/PGDP_0 DEBT_G_VAL_0/PGDP_0 GDP_2 CH_2 G_2 I_2+CH_13_2 I_2-IA_20_2 CH_13_2 IA_20_2 X_2 M_2 DISPINC_VAL_2/(POP_TOT*PCH_2) L_2 SP_G_VAL_2/PGDP_2 DEBT_G_VAL_2/PGDP_2 _
Exp_13_0 Exp_13_2 exp_newauto_val_h01_ca_0 exp_newauto_val_h01_cb_0 exp_newauto_val_h01_cc_0 exp_newauto_val_h01_cd_0 exp_newauto_val_h01_ce_0 exp_newauto_val_h01_cf_0 exp_newauto_val_h01_cg_0 exp_newauto_val_h01_ca_2 exp_newauto_val_h01_cb_2 exp_newauto_val_h01_cc_2 _
exp_newauto_val_h01_cd_2 exp_newauto_val_h01_ce_2 exp_newauto_val_h01_cf_2 exp_newauto_val_h01_cg_2 100*(ts_2-ts_0) _
100*((PY_2*Y_2/(Y_2-Y_20_2)-PY_20_2*Y_20_2/(Y_2-Y_20_2))/(PY_0*Y_0/(Y_0-Y_20_0)-PY_20_0*Y_20_0/(Y_0-Y_20_0))-1) _
100*((PVA_2*VA_2/(VA_2-VA_20_2)-PVA_20_2*VA_20_2/(VA_2-VA_20_2))/(PVA_0*VA_0/(VA_0-VA_20_0)-PVA_20_0*VA_20_0/(VA_0-VA_20_0))-1) _

100*(DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL_0/(PGDP_0*GDP_0)) _
100*((DEBT_NewB_Val_H01_CA_2+DEBT_NewB_Val_H01_CB_2+DEBT_NewB_Val_H01_CC_2+DEBT_NewB_Val_H01_CD_2+DEBT_NewB_Val_H01_CE_2+DEBT_NewB_Val_H01_CF_2+DEBT_NewB_Val_H01_CG_2+DEBT_REHAB_Val_H01_CA_2+DEBT_REHAB_Val_H01_CB_2+DEBT_REHAB_Val_H01_CC_2+DEBT_REHAB_Val_H01_CD_2+DEBT_REHAB_Val_H01_CE_2+DEBT_REHAB_Val_H01_CF_2+DEBT_REHAB_Val_H01_CG_2+DEBT_auto_Val_H01_CA_2+DEBT_auto_Val_H01_CB_2+DEBT_auto_Val_H01_CC_2+DEBT_auto_Val_H01_CD_2+DEBT_auto_Val_H01_CE_2+DEBT_auto_Val_H01_CF_2+DEBT_auto_Val_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_NewB_Val_H01_CA_0+DEBT_NewB_Val_H01_CB_0+DEBT_NewB_Val_H01_CC_0+DEBT_NewB_Val_H01_CD_0+DEBT_NewB_Val_H01_CE_0+DEBT_NewB_Val_H01_CF_0+DEBT_NewB_Val_H01_CG_0+DEBT_REHAB_Val_H01_CA_0+DEBT_REHAB_Val_H01_CB_0+DEBT_REHAB_Val_H01_CC_0+DEBT_REHAB_Val_H01_CD_0+DEBT_REHAB_Val_H01_CE_0+DEBT_REHAB_Val_H01_CF_0+DEBT_REHAB_Val_H01_CG_0+DEBT_auto_Val_H01_CA_0+DEBT_auto_Val_H01_CB_0+DEBT_auto_Val_H01_CC_0+DEBT_auto_Val_H01_CD_0+DEBT_auto_Val_H01_CE_0+DEBT_auto_Val_H01_CF_0+DEBT_auto_Val_H01_CG_0)/(PGDP_0*GDP_0)) _

100*((DEBT_NewB_Val_H01_CA_2+DEBT_NewB_Val_H01_CB_2+DEBT_NewB_Val_H01_CC_2+DEBT_NewB_Val_H01_CD_2+DEBT_NewB_Val_H01_CE_2+DEBT_NewB_Val_H01_CF_2+DEBT_NewB_Val_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_NewB_Val_H01_CA_0+DEBT_NewB_Val_H01_CB_0+DEBT_NewB_Val_H01_CC_0+DEBT_NewB_Val_H01_CD_0+DEBT_NewB_Val_H01_CE_0+DEBT_NewB_Val_H01_CF_0+DEBT_NewB_Val_H01_CG_0)/(PGDP_0*GDP_0)) _

100*((DEBT_REHAB_Val_H01_CA_2+DEBT_REHAB_Val_H01_CB_2+DEBT_REHAB_Val_H01_CC_2+DEBT_REHAB_Val_H01_CD_2+DEBT_REHAB_Val_H01_CE_2+DEBT_REHAB_Val_H01_CF_2+DEBT_REHAB_Val_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_REHAB_Val_H01_CA_0+DEBT_REHAB_Val_H01_CB_0+DEBT_REHAB_Val_H01_CC_0+DEBT_REHAB_Val_H01_CD_0+DEBT_REHAB_Val_H01_CE_0+DEBT_REHAB_Val_H01_CF_0+DEBT_REHAB_Val_H01_CG_0)/(PGDP_0*GDP_0)) _

100*((DEBT_auto_Val_H01_CA_2+DEBT_auto_Val_H01_CB_2+DEBT_auto_Val_H01_CC_2+DEBT_auto_Val_H01_CD_2+DEBT_auto_Val_H01_CE_2+DEBT_auto_Val_H01_CF_2+DEBT_auto_Val_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_auto_Val_H01_CA_0+DEBT_auto_Val_H01_CB_0+DEBT_auto_Val_H01_CC_0+DEBT_auto_Val_H01_CD_0+DEBT_auto_Val_H01_CE_0+DEBT_auto_Val_H01_CF_0+DEBT_auto_Val_H01_CG_0)/(PGDP_0*GDP_0)) _

L_20_2-L_20_0 _
100*((L_20_2/L_20_0)-1) _

(L_2-L_20_2)-(L_0-L_20_0) _
100*((L_s_2)/(L_s_0)-1) _

L_s_2-L_s_0 _
100*((L_s_20_2/L_s_20_0)-1) _

L_s_20_2-L_s_20_0 _
100*((L_s_20_2/L_s_20_0)-1) _

(L_s_2-L_s_20_2)-(L_s_0-L_s_20_0) _
100*(((L_s_2-L_s_20_2)/(L_s_0-L_s_20_0))-1) _ 

100*(((PE_sec_2*E_sec_2/(E_sec_2-Ener_20_2)-PEner_20_2*Ener_20_2/(E_sec_2-Ener_20_2))*(E_sec_2-Ener_20_2)/(MAT_2-MAT_20_2+E_sec_2-Ener_20_2)+(PMAT_2*MAT_2/(MAT_2-MAT_20_2)-PMAT_20_2*MAT_20_2/(MAT_2-MAT_20_2))*(MAT_2-MAT_20_2)/(MAT_2-MAT_20_2+E_sec_2-Ener_20_2))/((PE_sec_0*E_sec_0/(E_sec_0-Ener_20_0)-PEner_20_0*Ener_20_0/(E_sec_0-Ener_20_0))*(E_sec_2-Ener_20_2)/(MAT_0-MAT_20_0+E_sec_0-Ener_20_0)+(PMAT_0*MAT_0/(MAT_0-MAT_20_0)-PMAT_20_0*MAT_20_0/(MAT_0-MAT_20_0))*(MAT_0-MAT_20_0)/(MAT_0-MAT_20_0+E_sec_0-Ener_20_0))-1) _

100*((W_SPB_2/Y_2)/(W_SPB_0/Y_0)-1) _
100*((cl_bis_2/pva_2)/(cl_bis_0/pva_0)-1) _

100*((DEBT_NewB_Val_tot_H01_CA_2+DEBT_NewB_Val_tot_H01_CB_2+DEBT_NewB_Val_tot_H01_CC_2+DEBT_NewB_Val_tot_H01_CD_2+DEBT_NewB_Val_tot_H01_CE_2+DEBT_NewB_Val_tot_H01_CF_2+DEBT_NewB_Val_tot_H01_CG_2+DEBT_REHAB_Val_tot_H01_CA_2+DEBT_REHAB_Val_tot_H01_CB_2+DEBT_REHAB_Val_tot_H01_CC_2+DEBT_REHAB_Val_tot_H01_CD_2+DEBT_REHAB_Val_tot_H01_CE_2+DEBT_REHAB_Val_tot_H01_CF_2+DEBT_REHAB_Val_tot_H01_CG_2+DEBT_auto_Val_tot_H01_CA_2+DEBT_auto_Val_tot_H01_CB_2+DEBT_auto_Val_tot_H01_CC_2+DEBT_auto_Val_tot_H01_CD_2+DEBT_auto_Val_tot_H01_CE_2+DEBT_auto_Val_tot_H01_CF_2+DEBT_auto_Val_tot_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_NewB_Val_tot_H01_CA_0+DEBT_NewB_Val_tot_H01_CB_0+DEBT_NewB_Val_tot_H01_CC_0+DEBT_NewB_Val_tot_H01_CD_0+DEBT_NewB_Val_tot_H01_CE_0+DEBT_NewB_Val_tot_H01_CF_0+DEBT_NewB_Val_tot_H01_CG_0+DEBT_REHAB_Val_tot_H01_CA_0+DEBT_REHAB_Val_tot_H01_CB_0+DEBT_REHAB_Val_tot_H01_CC_0+DEBT_REHAB_Val_tot_H01_CD_0+DEBT_REHAB_Val_tot_H01_CE_0+DEBT_REHAB_Val_tot_H01_CF_0+DEBT_REHAB_Val_tot_H01_CG_0+DEBT_auto_Val_tot_H01_CA_0+DEBT_auto_Val_tot_H01_CB_0+DEBT_auto_Val_tot_H01_CC_0+DEBT_auto_Val_tot_H01_CD_0+DEBT_auto_Val_tot_H01_CE_0+DEBT_auto_Val_tot_H01_CF_0+DEBT_auto_Val_tot_H01_CG_0)/(PGDP_0*GDP_0)) _

100*((DEBT_NewB_Val_tot_H01_CA_2+DEBT_NewB_Val_tot_H01_CB_2+DEBT_NewB_Val_tot_H01_CC_2+DEBT_NewB_Val_tot_H01_CD_2+DEBT_NewB_Val_tot_H01_CE_2+DEBT_NewB_Val_tot_H01_CF_2+DEBT_NewB_Val_tot_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_NewB_Val_tot_H01_CA_0+DEBT_NewB_Val_tot_H01_CB_0+DEBT_NewB_Val_tot_H01_CC_0+DEBT_NewB_Val_tot_H01_CD_0+DEBT_NewB_Val_tot_H01_CE_0+DEBT_NewB_Val_tot_H01_CF_0+DEBT_NewB_Val_tot_H01_CG_0)/(PGDP_0*GDP_0)) _

100*((DEBT_REHAB_Val_tot_H01_CA_2+DEBT_REHAB_Val_tot_H01_CB_2+DEBT_REHAB_Val_tot_H01_CC_2+DEBT_REHAB_Val_tot_H01_CD_2+DEBT_REHAB_Val_tot_H01_CE_2+DEBT_REHAB_Val_tot_H01_CF_2+DEBT_REHAB_Val_tot_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_REHAB_Val_tot_H01_CA_0+DEBT_REHAB_Val_tot_H01_CB_0+DEBT_REHAB_Val_tot_H01_CC_0+DEBT_REHAB_Val_tot_H01_CD_0+DEBT_REHAB_Val_tot_H01_CE_0+DEBT_REHAB_Val_tot_H01_CF_0+DEBT_REHAB_Val_tot_H01_CG_0)/(PGDP_0*GDP_0)) _

100*((DEBT_auto_Val_tot_H01_CA_2+DEBT_auto_Val_tot_H01_CB_2+DEBT_auto_Val_tot_H01_CC_2+DEBT_auto_Val_tot_H01_CD_2+DEBT_auto_Val_tot_H01_CE_2+DEBT_auto_Val_tot_H01_CF_2+DEBT_auto_Val_tot_H01_CG_2)/(PGDP_2*GDP_2)-(DEBT_auto_Val_tot_H01_CA_0+DEBT_auto_Val_tot_H01_CB_0+DEBT_auto_Val_tot_H01_CC_0+DEBT_auto_Val_tot_H01_CD_0+DEBT_auto_Val_tot_H01_CE_0+DEBT_auto_Val_tot_H01_CF_0+DEBT_auto_Val_tot_H01_CG_0)/(PGDP_0*GDP_0)) _

REC_VAL_0 DIV_GOV_VAL_0 IR_VAL_0 AIC_VAL_0 -CL_S_20_0*L_S_20_0*PROG_L_20_0 PY_20_0*Y_20_0 PTAX_0*TAX_0 PIY_0*IY_0 PIS_0*IS_0 PCSE_TOT_0*CSE_TOT_0 PCSS_TOT_0*CSS_TOT_0 -(PE_20_0*E_20_0) -(PMAT_20_0*MAT_20_0) -(PIY_20_0*IY_20_0) REC_VAL_2 DIV_GOV_VAL_2 IR_VAL_2 AIC_VAL_2 -CL_S_20_2*L_S_20_2*PROG_L_20_2 PY_20_2*Y_20_2 PTAX_2*TAX_2 PIY_2*IY_2 PIS_2*IS_2 PCSE_TOT_2*CSE_TOT_2 PCSS_TOT_2*CSS_TOT_2 -(PE_20_2*E_20_2) -(PMAT_20_2*MAT_20_2) -(PIY_20_2*IY_20_2) DEP_VAL_0 CL_S_20_0*L_S_20_0*PROG_L_20_0 R_G_0(-1)*DEBT_G_VAL_0(-1) PRESOC_VAL_0 SUB_RENOV_VAL_0 SUB_AUTO_VAL_0 PE_20_0*E_20_0 PMAT_20_0*MAT_20_0 PIY_20_0*IY_20_0 PIA_20_0*IA_20_0 PG_0*G_0-PG_20_0*G_20_0 -(PSUB_0*SUB_0-PSUB_01_0*SUB_01_0) -(PSY_0*SY_0-PSY_01_0*SY_01_0) DEP_VAL_2 CL_S_20_2*L_S_20_2*PROG_L_20_2 R_G_2(-1)*DEBT_G_VAL_2(-1) PRESOC_VAL_2 SUB_RENOV_VAL_2 SUB_AUTO_VAL_2 PE_20_2*E_20_2 PMAT_20_2*MAT_20_2 PIY_20_2*IY_20_2 PIA_20_2*IA_20_2 PG_2*G_2-PG_20_2*G_20_2 -(PSUB_2*SUB_2-PSUB_01_2*SUB_01_2) -(PSY_2*SY_2-PSY_01_2*SY_01_2) PGDP_0 PGDP_2 GDP_0 GDP_2 INC_GOV_OTH_NET REDIS_VAL_H _

100*(DEBT_SNF_VAL_2/(PGDP_2*GDP_2)-DEBT_SNF_VAL_0/(PGDP_0*GDP_0)) _
100*(DEBT_SNF_ENER_2/(PGDP_2*GDP_2)-DEBT_SNF_ENER_0/(PGDP_0*GDP_0)) _
100*(DEBT_SNF_VAL_2/DEBT_SNF_VAL_0-1) _
100*(DEBT_SNF_ENER_2/DEBT_SNF_ENER_0-1) _

PRESOC_DOM_Oth_VAL_0 PRESOC_DOM_Oth_VAL_2 PRESOC_DOM_U_VAL_0 PRESOC_DOM_U_VAL_2 PRESOC_ROW_VAL (-PG_20_0*G_20_0) (-PG_20_2*G_20_2) (PG_0*G_0) (PG_2*G_2)


'Endif
'	if %exceptions_DGT = "yes" then
'ajout de variables de sortie calculees dans Exceptions_DGT pour le cahier de variantes Reporting_3.add' 
                                                                                                                                                                                                                                        
Reporting_3.sheet(t)
show Reporting_3

   group Reporting_4 Y_01_0 Y_02_0 Y_03_0 Y_04_0 Y_05_0 Y_06_0 Y_07_0 Y_08_0 Y_09_0 Y_10_0 Y_11_0 Y_12_0 Y_13_0 Y_14_0 Y_15_0 Y_16_0 Y_17_0 Y_18_0 Y_19_0 Y_20_0 Y_21_0 _
Y_01_2 Y_02_2 Y_03_2 Y_04_2 Y_05_2 Y_06_2 Y_07_2 Y_08_2 Y_09_2 Y_10_2 Y_11_2 Y_12_2 Y_13_2 Y_14_2 Y_15_2 Y_16_2 Y_17_2 Y_18_2 Y_19_2 Y_20_2 Y_21_2  _
E_01_0 E_02_0 E_03_0 E_04_0 E_05_0 E_06_0 E_07_0 E_08_0 E_09_0 E_10_0 E_11_0 E_12_0 E_13_0 E_14_0 E_15_0 E_16_0 E_17_0 E_18_0 E_19_0 E_20_0 _ 
E_01_2 E_02_2 E_03_2 E_04_2 E_05_2 E_06_2 E_07_2 E_08_2 E_09_2 E_10_2 E_11_2 E_12_2 E_13_2 E_14_2 E_15_2 E_16_2 E_17_2 E_18_2 E_19_2 E_20_2   _
VA_01_0 VA_02_0 VA_03_0 VA_04_0 VA_05_0 VA_06_0 VA_07_0 VA_08_0 VA_09_0 VA_10_0 VA_11_0 VA_12_0 VA_13_0 VA_14_0 VA_15_0 VA_16_0 VA_17_0 VA_18_0 VA_19_0 VA_20_0  _
VA_01_2 VA_02_2 VA_03_2 VA_04_2 VA_05_2 VA_06_2 VA_07_2 VA_08_2 VA_09_2 VA_10_2 VA_11_2 VA_12_2 VA_13_2 VA_14_2 VA_15_2 VA_16_2 VA_17_2 VA_18_2 VA_19_2 VA_20_2 _ 
AUTO_0 AUTO_2 AUTO_ELEC_0 AUTO_ELEC_2 Q_MTEP_SEC_22_15_0 Q_MTEP_SEC_22_15_2 Q_MTEP_SEC_22_16_0 Q_MTEP_SEC_22_16_2 CID_14_0 CID_14_2  BUIL_0 BUIL_2 M2PERCAPITA  _
POP_TOT BUIL_H01_DES_0/BUIL_0(-1) BUIL_H01_DES_2/BUIL_2(-1) NEWBUIL_0/100 NEWBUIL_2/100 EXP_18_0 EXP_18_2 _
Q_Mtep_sec_01_0 Q_Mtep_sec_02_0 Q_Mtep_sec_03_0 Q_Mtep_sec_04_0 Q_Mtep_sec_05_0 Q_Mtep_sec_06_0 Q_Mtep_sec_07_0 Q_Mtep_sec_08_0 Q_Mtep_sec_09_0 Q_Mtep_sec_10_0 _
Q_Mtep_sec_11_0 Q_Mtep_sec_12_0 Q_Mtep_sec_13_0 Q_Mtep_sec_14_0 Q_Mtep_sec_15_0 Q_Mtep_sec_16_0 Q_Mtep_sec_17_0 Q_Mtep_sec_18_0 Q_Mtep_sec_19_0 Q_Mtep_sec_20_0 _
Q_Mtep_sec_01_2 Q_Mtep_sec_02_2 Q_Mtep_sec_03_2 Q_Mtep_sec_04_2 Q_Mtep_sec_05_2 Q_Mtep_sec_06_2 Q_Mtep_sec_07_2 Q_Mtep_sec_08_2 Q_Mtep_sec_09_2 Q_Mtep_sec_10_2 _
 Q_Mtep_sec_11_2 Q_Mtep_sec_12_2 Q_Mtep_sec_13_2 Q_Mtep_sec_14_2 Q_Mtep_sec_15_2 Q_Mtep_sec_16_2 Q_Mtep_sec_17_2 Q_Mtep_sec_18_2 Q_Mtep_sec_19_2 Q_Mtep_sec_20_2



                                                                                                                                                                                                                                         
'Reporting_4.sheet(t)
'show Reporting_4


group Reporting_5 100*(GDP_2/GDP-1) 100*((VA_2-VA_20_2)/(VA-VA_20)-1) 100*((CH_2-CH)/GDP) 100*((CH_03_2-CH_03)/GDP) _
               100*((G_2-G)/GDP) 100*((I_2-I)/GDP) 100*((DS_2-DS)/GDP) 100*((X_2-X)/GDP)-100*((M_2-M)/GDP) _
               100*((CH_2-CH_13_2-(CH-CH_13))/GDP) 100*((I_2+CH_13_2-(I+CH_13))/GDP) 100*((I_2-IA_20_2-(I-IA_20))/GDP) 100*((CH_13_2-CH_13)/GDP) 100*((IA_20_2-IA_20)/GDP) _
               100*(CH_2/CH-1) 100*(CH_03_2/CH_03-1) 100*(G_2/G-1) 100*(I_2/I-1) 100*((I_2-IA_20_2)/(I-IA_20)-1) 100*(X_2/X-1) 100*(M_2/M-1) _
              100*((CH_2-CH_13_2)/(CH-CH_13)-1) 100*((I_2+CH_13_2)/(I+CH_13)-1) 100*((CH_13_2)/(CH_13)-1) 100*((IA_20_2)/(IA_20)-1) _
      100*(DISPINC_VAL_2/DISPINC_VAL-1) 100*(DISPINC_VAL_2/DISPINC_VAL*PCH/PCH_2-1) 100*(DISPINC_VAL_2/DISPINC_VAL*L/L_2-1) _
      100*(DISPINC_VAL_2/DISPINC_VAL*L/L_2*PCH/PCH_2-1) 100*((dispinc_val_H01_2-(PCH_2*CH_2-pch_13_2*ch_13_2))/dispinc_val_H01_2-(dispinc_val_H01-(PCH*CH-pch_13*ch_13))/dispinc_val_H01) 100*(PCH_2/PCH-1) 100*(PY_2/PY-1) 100*(PX_2/PX-1) _
      100*(PM_2/PM-1) 100*(W_2/W-1) 100*((W_2/PCH_2)/(W/PCH)-1) 100*(CL_2/CL-1) 100*((CL_2/PVA_2)/(CL/PVA)-1) ((L_2/L)-1)*100 _
      L_2-L 100*(UNR_TOT_2-UNR_TOT) 100*(DC_VAL_2/(GDP_2*PGDP_2)-DC_VAL/(GDP*PGDP)) _
      100*((M_21_2*PM_21_2-X_21_2*PX_21_2+M_22_2*PM_22_2-X_22_2*PX_22_2+M_23_2*PM_23_2-X_23_2*PX_23_2+M_24_2*PM_24_2-X_24_2*PX_24_2)/(GDP_2*PGDP_2))-100*((M_21*PM_21-X_21*PX_21+M_22*PM_22-X_22*PX_22+M_23*PM_23-X_23*PX_23+M_24*PM_24-X_24*PX_24)/(GDP*PGDP)) 100*((-SP_G_VAL_2)/(GDP_2*PGDP_2)-(-SP_G_VAL)/(GDP*PGDP)) _
      100*((-BF_G_VAL_2)/(GDP_2*PGDP_2)-(-BF_G_VAL)/(GDP*PGDP)) (DEBT_G_VAL_2/(PGDP_2*GDP_2)-DEBT_G_VAL/(PGDP*GDP))*100 _ 
DEP_val_2 REC_VAL_2 (0-BF_G_VAL_2) DEBT_G_VAL_2 REC_TCO_VAL_2 ENERT_22_2*PENERT_22_2 ENERT_24_2*PENERT_24_2 ENERT_23_2*PENERT_23_2 ENERT_21_2*PENERT_21_2 TCO_VAL_HH_2 _
TCO_VAL_HH_21_H01_2+TCO_VAL_HH_24_H01_2+TCO_VAL_HH_22_H01_2*(Q_MTEP_H_BUIL_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) _
TCO_VAL_HH_22_H01_2*(Q_MTEP_H_BUIL_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) TCO_VAL_HH_24_H01_2 TCO_VAL_HH_21_H01_2 _
TCO_VAL_HH_22_H01_2*(Q_MTEP_H_AUTO_22_2)/(Q_MTEP_H_BUIL_22_2+Q_MTEP_H_AUTO_22_2) TCO_VAL_SEC_2 REC_TCO_VAL_ETS_2  _
TCO_VAL_22_04_2+TCO_VAL_22_05_2+TCO_VAL_22_06_2+TCO_VAL_22_07_2+TCO_VAL_22_08_2+TCO_VAL_22_09_2+TCO_VAL_22_10_2+TCO_VAL_22_18_2+TCO_VAL_22_21_2+tco_val_22_2201_2+tco_val_22_2202_2+TCO_VAL_22_2301_2+TCO_VAL_22_2302_2+TCO_VAL_22_2303_2+TCO_VAL_22_2304_2+TCO_VAL_22_2305_2+TCO_VAL_22_2306_2+TCO_VAL_22_2307_2+TCO_VAL_22_2308_2+TCO_VAL_22_2401_2+TCO_VAL_22_2402_2+TCO_VAL_22_2403_2+TCO_VAL_22_2404_2+TCO_VAL_22_2405_2+TCO_VAL_22_2406_2 _ 	
TCO_VAL_24_04_2+TCO_VAL_24_05_2+TCO_VAL_24_06_2+TCO_VAL_24_07_2+TCO_VAL_24_08_2+TCO_VAL_24_09_2+TCO_VAL_24_10_2+TCO_VAL_24_18_2+TCO_VAL_24_21_2+tco_val_24_2201_2+tco_val_24_2202_2+TCO_VAL_24_2301_2+TCO_VAL_24_2302_2+TCO_VAL_24_2303_2+TCO_VAL_24_2304_2+TCO_VAL_24_2305_2+TCO_VAL_24_2306_2+TCO_VAL_24_2307_2+TCO_VAL_24_2308_2+TCO_VAL_24_2401_2+TCO_VAL_24_2402_2+TCO_VAL_24_2403_2+TCO_VAL_24_2404_2+TCO_VAL_24_2405_2+TCO_VAL_24_2406_2 _	
TCO_VAL_21_04_2+TCO_VAL_21_05_2+TCO_VAL_21_06_2+TCO_VAL_21_07_2+TCO_VAL_21_08_2+TCO_VAL_21_09_2+TCO_VAL_21_10_2+TCO_VAL_21_18_2+TCO_VAL_21_21_2+tco_val_21_2201_2+tco_val_21_2202_2+TCO_VAL_21_2301_2+TCO_VAL_21_2302_2+TCO_VAL_21_2303_2+TCO_VAL_21_2304_2+TCO_VAL_21_2305_2+TCO_VAL_21_2306_2+TCO_VAL_21_2307_2+TCO_VAL_21_2308_2+TCO_VAL_21_2401_2+TCO_VAL_21_2402_2+TCO_VAL_21_2403_2+TCO_VAL_21_2404_2+TCO_VAL_21_2405_2+TCO_VAL_21_2406_2 _
REC_TCO_VAL_NETS_2 _
TCO_VAL_22_01_2+TCO_VAL_22_02_2+TCO_val_22_03_2+TCO_VAL_22_11_2+TCO_VAL_22_12_2+TCO_VAL_22_13_2 _
TCO_VAL_24_01_2+TCO_VAL_24_02_2+TCO_val_24_03_2+TCO_VAL_24_11_2+TCO_VAL_24_12_2+TCO_VAL_24_13_2 _
TCO_VAL_21_01_2+TCO_VAL_21_02_2+TCO_val_21_03_2+TCO_VAL_21_11_2+TCO_VAL_21_12_2+TCO_VAL_21_13_2 _
TCO_VAL_SEC_14_2+TCO_VAL_SEC_15_2+TCO_VAL_SEC_16_2+TCO_VAL_SEC_17_2+TCO_VAL_SEC_19_2+TCO_VAL_SEC_20_2	TCO_VAL_22_14_2+TCO_VAL_22_15_2+TCO_VAL_22_16_2+TCO_VAL_22_17_2+TCO_VAL_22_19_2+TCO_VAL_22_20_2 _ 
TCO_VAL_24_14_2+TCO_VAL_24_15_2+TCO_VAL_24_16_2+TCO_VAL_24_17_2+TCO_VAL_24_19_2+TCO_VAL_24_20_2 _
TCO_VAL_21_14_2+TCO_VAL_21_15_2+TCO_VAL_21_16_2+TCO_VAL_21_17_2+TCO_VAL_21_19_2+TCO_VAL_21_20_2 _
DEP_val REC_VAL (0-BF_G_VAL) DEBT_G_VAL REC_TCO_VAL	ENERT_22*PENERT_22	ENERT_24*PENERT_24	ENERT_23*PENERT_23 ENERT_21*PENERT_21 _
TCO_VAL_HH _
TCO_VAL_HH_21_H01+TCO_VAL_HH_24_H01+TCO_VAL_HH_22_H01*(Q_MTEP_H_BUIL_22)/(Q_MTEP_H_BUIL_22+Q_MTEP_H_AUTO_22) _
TCO_VAL_HH_22_H01*(Q_MTEP_H_BUIL_22)/(Q_MTEP_H_BUIL_22+Q_MTEP_H_AUTO_22) _
TCO_VAL_HH_24_H01 _
TCO_VAL_HH_21_H01 _
TCO_VAL_HH_22_H01*(Q_MTEP_H_AUTO_22)/(Q_MTEP_H_BUIL_22+Q_MTEP_H_AUTO_22) _
TCO_VAL_SEC REC_TCO_VAL_ETS _
TCO_VAL_22_04+TCO_VAL_22_05+TCO_VAL_22_06+TCO_VAL_22_07+TCO_VAL_22_08+TCO_VAL_22_09+TCO_VAL_22_10+TCO_VAL_22_18+TCO_VAL_22_21+tco_val_22_2201+tco_val_22_2202+TCO_VAL_22_2301+TCO_VAL_22_2302+TCO_VAL_22_2303+TCO_VAL_22_2304+TCO_VAL_22_2305+TCO_VAL_22_2306+TCO_VAL_22_2307+TCO_VAL_22_2308+TCO_VAL_22_2401+TCO_VAL_22_2402+TCO_VAL_22_2403+TCO_VAL_22_2404+TCO_VAL_22_2405+TCO_VAL_22_2406 _
TCO_VAL_24_04+TCO_VAL_24_05+TCO_VAL_24_06+TCO_VAL_24_07+TCO_VAL_24_08+TCO_VAL_24_09+TCO_VAL_24_10+TCO_VAL_24_18+TCO_VAL_24_21+tco_val_24_2201+TCO_VAL_24_2202+TCO_VAL_24_2301+TCO_VAL_24_2302+TCO_VAL_24_2303+TCO_VAL_24_2304+TCO_VAL_24_2305+TCO_VAL_24_2306+TCO_VAL_24_2307+TCO_VAL_24_2308+TCO_VAL_24_2401+TCO_VAL_24_2402+TCO_VAL_24_2403+TCO_VAL_24_2404+TCO_VAL_24_2405+TCO_VAL_24_2406 _
TCO_VAL_21_04+TCO_VAL_21_05+TCO_VAL_21_06+TCO_VAL_21_07+TCO_VAL_21_08+TCO_VAL_21_09+TCO_VAL_21_10+TCO_VAL_21_18+TCO_VAL_21_21+tco_val_21_2201+tco_val_21_2202+TCO_VAL_21_2301+TCO_VAL_21_2302+TCO_VAL_21_2303+TCO_VAL_21_2304+TCO_VAL_21_2305+TCO_VAL_21_2306+TCO_VAL_21_2307+TCO_VAL_21_2308+TCO_VAL_21_2401+TCO_VAL_21_2402+TCO_VAL_21_2403+TCO_VAL_21_2404+TCO_VAL_21_2405+TCO_VAL_21_2406 _
REC_TCO_VAL_NETS _
TCO_VAL_22_01+TCO_VAL_22_02+TCO_val_22_03+TCO_VAL_22_11+TCO_VAL_22_12+TCO_VAL_22_13 _
TCO_VAL_24_01+TCO_VAL_24_02+TCO_val_24_03+TCO_VAL_24_11+TCO_VAL_24_12+TCO_VAL_24_13 _
TCO_VAL_21_01+TCO_VAL_21_02+TCO_val_21_03+TCO_VAL_21_11+TCO_VAL_21_12+TCO_VAL_21_13 _ 
TCO_VAL_SEC_14+TCO_VAL_SEC_15+TCO_VAL_SEC_16+TCO_VAL_SEC_17+TCO_VAL_SEC_19+TCO_VAL_SEC_20	TCO_VAL_22_14+TCO_VAL_22_15+TCO_VAL_22_16+TCO_VAL_22_17+TCO_VAL_22_19+TCO_VAL_22_20 TCO_VAL_24_14+TCO_VAL_24_15+TCO_VAL_24_16+TCO_VAL_24_17+TCO_VAL_24_19+TCO_VAL_24_20 _ 
TCO_VAL_21_14+TCO_VAL_21_15+TCO_VAL_21_16+TCO_VAL_21_17+TCO_VAL_21_19+TCO_VAL_21_20 _
EMS_TOT_2 _
EMS_HH_22_2+EMS_SEC_22_01_2+EMS_SEC_22_02_2+EMS_SEC_22_03_2+EMS_SEC_22_04_2+EMS_SEC_22_05_2+EMS_SEC_22_06_2+EMS_SEC_22_07_2+EMS_SEC_22_08_2+EMS_SEC_22_09_2+EMS_SEC_22_12_2+EMS_SEC_22_13_2+EMS_SEC_22_14_2+EMS_SEC_22_15_2+EMS_SEC_22_16_2+EMS_SEC_22_17_2+EMS_SEC_22_18_2+EMS_SEC_22_19_2+EMS_SEC_22_20_2 EMS_HH_24_2+EMS_SEC_2401_2+EMS_SEC_24_01_2+EMS_SEC_24_02_2+EMS_SEC_24_03_2+EMS_SEC_24_04_2+EMS_SEC_24_05_2+EMS_SEC_24_06_2+EMS_SEC_24_07_2+EMS_SEC_24_08_2+EMS_SEC_24_09_2+EMS_SEC_24_10_2+EMS_SEC_24_11_2+EMS_SEC_24_12_2+EMS_SEC_24_13_2+EMS_SEC_24_14_2+EMS_SEC_24_15_2+EMS_SEC_24_16_2+EMS_SEC_24_17_2+EMS_SEC_24_18_2+EMS_SEC_24_19_2+EMS_SEC_24_20_2 _
       EMS_SEC_2302_2+EMS_SEC_2303_2+EMS_SEC_2304_2 EMS_HH_21_2+EMS_SEC_21_05_2+EMS_SEC_21_06_2+EMS_SEC_21_07_2+EMS_SEC_21_08_2+EMS_SEC_21_10_2+EMS_SEC_21_12_2+EMS_SEC_21_19_2+EMS_SEC_21_20_2 EMS_DC_05_2+EMS_DC_04_2 EMS_HH_21_2+EMS_HH_24_2+EMS_HH_22_2*(@ELEM(PENER_BUIL_H01_22,2006)*ENER_BUIL_H01_22_2)/EXP_22_H01_2 _
       EMS_HH_22_2*(EXP_AUTO_H01_22_2/EXP_22_H01_2) _
       EMS_SEC_04_2+EMS_SEC_05_2+EMS_SEC_06_2+EMS_SEC_07_2+EMS_SEC_08_2+EMS_SEC_09_2+EMS_SEC_10_2+EMS_SEC_18_2+EMS_SEC_21_19_2+EMS_SEC_21_20_2+EMS_SEC_2201_2+EMS_SEC_2302_2+EMS_SEC_2303_2+EMS_SEC_2304_2+EMS_SEC_2401_2+EMS_DC_04_2+EMS_DC_05_2 _
        EMS_SEC_01_2+EMS_SEC_02_2+EMS_SEC_03_2+EMS_SEC_11_2+EMS_SEC_12_2+EMS_SEC_13_2 _             
        EMS_SEC_14_2+EMS_SEC_15_2+EMS_SEC_16_2+EMS_SEC_17_2+EMS_SEC_19_2+EMS_SEC_20_2 _               
        EMS_TOT EMS_HH_22+EMS_SEC_22_01+EMS_SEC_22_02+EMS_SEC_22_03+EMS_SEC_22_04+EMS_SEC_22_05+EMS_SEC_22_06+EMS_SEC_22_07+EMS_SEC_22_08+EMS_SEC_22_09+EMS_SEC_22_12+EMS_SEC_22_13+EMS_SEC_22_14+EMS_SEC_22_15+EMS_SEC_22_16+EMS_SEC_22_17+EMS_SEC_22_18+EMS_SEC_22_19+EMS_SEC_22_20 EMS_HH_24+EMS_SEC_2401+EMS_SEC_24_01+EMS_SEC_24_02+EMS_SEC_24_03+EMS_SEC_24_04+EMS_SEC_24_05+EMS_SEC_24_06+EMS_SEC_24_07+EMS_SEC_24_08+EMS_SEC_24_09+EMS_SEC_24_10+EMS_SEC_24_11+EMS_SEC_24_12+EMS_SEC_24_13+EMS_SEC_24_14+EMS_SEC_24_15+EMS_SEC_24_16+EMS_SEC_24_17+EMS_SEC_24_18+EMS_SEC_24_19+EMS_SEC_24_20 _
        EMS_SEC_2302+EMS_SEC_2303+EMS_SEC_2304 EMS_HH_21+EMS_SEC_21_05+EMS_SEC_21_06+EMS_SEC_21_07+EMS_SEC_21_08+EMS_SEC_21_10+EMS_SEC_21_12+EMS_SEC_21_19+EMS_SEC_21_20 _
       EMS_DC_05+EMS_DC_04 _
       EMS_HH_21+EMS_HH_24+EMS_HH_22*(@ELEM(PENER_BUIL_H01_22,2006)*ENER_BUIL_H01_22)/EXP_22_H01 _
       EMS_HH_22*(EXP_AUTO_H01_22/EXP_22_H01) _
       EMS_SEC_04+EMS_SEC_05+EMS_SEC_06+EMS_SEC_07+EMS_SEC_08+EMS_SEC_09+EMS_SEC_10+EMS_SEC_18+EMS_SEC_21_19+EMS_SEC_21_20+EMS_SEC_2201+EMS_SEC_2302+EMS_SEC_2303+EMS_SEC_2304+EMS_SEC_2401+EMS_DC_04+EMS_DC_05 _
       EMS_SEC_01+EMS_SEC_02+EMS_SEC_03+EMS_SEC_11+EMS_SEC_12+EMS_SEC_13 _
       EMS_SEC_14+EMS_SEC_15+EMS_SEC_16+EMS_SEC_17+EMS_SEC_19+EMS_SEC_20 ER_TOTAL_2 _
       ER_TRANS_PRIVATE_2+ER_TRANS_PUBLIC_2 ER_TRANS_PRIVATE_2 ER_TRANS_PRIVATE_OIL_2 ER_TRANS_PRIVATE_ELEC_2 ER_TRANS_PUBLIC_2 ER_TRANS_PUBLIC_OIL_2 _
       ER_TRANS_PUBLIC_ELEC_2 ER_RESIDENTIAL_2 ER_RESIDENTIAL_OIL_2 ER_RESIDENTIAL_ELEC_2 ER_RESIDENTIAL_GAS_2 ER_AGRICULTURE_2 ER_AGRICULTURE_OIL_2 _
       ER_AGRICULTURE_ELEC_2 ER_INDUS_2 ER_INDUS_OIL_2 ER_INDUS_ELEC_2 ER_INDUS_GAS_2 ER_INDUS_COAL_2 ER_TERTIARY_2 ER_TERTIARY_OIL_2 ER_TERTIARY_ELEC_2 ER_TERTIARY_GAS_2 ER_OIL_2 ER_GAS_2 ER_COAL_2 ER_ELEC_2 ER_ELEC_2301_2 ER_ELEC_2302_2 ER_ELEC_2303_2 _
        ER_ELEC_2304_2 ER_ELEC_2305_2 ER_ELEC_2306_2 ER_ELEC_2307_2 ER_ELEC_2308_2 ER_TOTAL ER_TRANS_PRIVATE+ER_TRANS_PUBLIC ER_TRANS_PRIVATE ER_TRANS_PRIVATE_OIL ER_TRANS_PRIVATE_ELEC ER_TRANS_PUBLIC ER_TRANS_PUBLIC_OIL ER_TRANS_PUBLIC_ELEC ER_RESIDENTIAL ER_RESIDENTIAL_OIL ER_RESIDENTIAL_ELEC ER_RESIDENTIAL_GAS ER_AGRICULTURE ER_AGRICULTURE_OIL ER_AGRICULTURE_ELEC ER_INDUS ER_INDUS_OIL ER_INDUS_ELEC ER_INDUS_GAS ER_INDUS_COAL ER_TERTIARY ER_TERTIARY_OIL ER_TERTIARY_ELEC ER_TERTIARY_GAS ER_OIL _
        ER_GAS ER_COAL ER_ELEC ER_ELEC_2301 ER_ELEC_2302 ER_ELEC_2303 ER_ELEC_2304 ER_ELEC_2305 ER_ELEC_2306 ER_ELEC_2307 ER_ELEC_2308 ENER_H01_2 _
        @ELEM(PENER_BUIL_H01,2006)*ENER_BUIL_H01_2 EXP_AUTO_H01_2 ENER_H01 @ELEM(PENER_BUIL_H01,2006)*ENER_BUIL_H01 EXP_AUTO_H01 KM_auto_H01_2 _
        KM_TRAVELER_18_H01_2 KM_TRAV_AUTO_LD_H01_2 KM_TRAV_AUTO_CD_H01_2 KM_TRAVELER_14_H01_2 KM_TRAVELER_15_H01_2 KM_TRAVELER_CD_H01_2 KM_TRAVELER_LD_H01_2 _
        KM_AUTO_H01 KM_TRAVELER_18_H01 KM_TRAV_AUTO_LD_H01 KM_TRAV_AUTO_CD_H01 KM_TRAVELER_14_H01 _
        KM_TRAVELER_15_H01 KM_TRAVELER_CD_H01 KM_TRAVELER_LD_H01 _
        ER_Auto_2 ER_AUTO_th_A_2 ER_AUTO_th_B_2 ER_AUTO_th_C_2 ER_AUTO_th_D_2 ER_AUTO_th_E_2 ER_AUTO_th_F_2 ER_AUTO_th_G_2 _
        ER_AUTO_elec_A_2 ER_AUTO_elec_B_2 ER_AUTO_elec_C_2 ER_AUTO_elec_D_2 ER_AUTO_elec_E_2 ER_AUTO_elec_F_2 ER_AUTO_elec_G_2 ER_Auto_Coal_2 ER_auto_th_2 ER_Auto_Elec_2 ER_Auto_Gas_2 _
        ER_NEWAUTO_2 ER_NEWAUTO_th_2 ER_newauto_th_A_2 ER_newauto_th_B_2 ER_newauto_th_C_2 ER_newauto_th_D_2 ER_newauto_th_E_2 ER_newauto_th_F_2  ER_newauto_th_G_2  _
        ER_NEWAUTO_elec_2 ER_NEWAUTO_elec_A_2 ER_NEWAUTO_elec_B_2 ER_NEWAUTO_elec_C_2 ER_NEWAUTO_elec_D_2 ER_NEWAUTO_elec_E_2 ER_NEWAUTO_elec_F_2 ER_NEWAUTO_elec_G_2 _
        ER_Auto ER_AUTO_th_A ER_AUTO_th_B ER_AUTO_th_C ER_AUTO_th_D ER_AUTO_th_E ER_AUTO_th_F ER_AUTO_th_G ER_AUTO_elec_A ER_AUTO_elec_B ER_AUTO_elec_C ER_AUTO_elec_D ER_AUTO_elec_E ER_AUTO_elec_F ER_AUTO_elec_G ER_Auto_Coal ER_auto_th ER_Auto_Elec ER_Auto_Gas _
        ER_NEWAUTO ER_NEWAUTO_th ER_newauto_th_A ER_newauto_th_B ER_newauto_th_C ER_newauto_th_D ER_newauto_th_E ER_newauto_th_F _
        ER_NEWAUTO_elec ER_NEWAUTO_elec_A ER_NEWAUTO_elec_B ER_NEWAUTO_elec_C ER_NEWAUTO_elec_D ER_NEWAUTO_elec_E ER_NEWAUTO_elec_F _
        ER_BUIL_2 ER_BUIL_A_2 ER_BUIL_B_2 ER_BUIL_C_2 ER_BUIL_D_2 ER_BUIL_E_2 ER_BUIL_F_2 ER_BUIL_G_2  SUB_REHAB_VAL_2 tax_cr_19_2 ER_BUIL _
        ER_BUIL_A ER_BUIL_B ER_BUIL_C ER_BUIL_D ER_BUIL_E ER_BUIL_F ER_BUIL_G SUB_REHAB_VAL tax_cr_19 _
	  pch_2 pch pgdp_2 pgdp gdp_2 gdp _
       L_01	L_02	L_03	L_04	L_05	L_06	L_07	L_08	L_09	L_10	L_11 L_12 L_13 L_14 L_15	L_16	L_17	L_18	L_19	L_20	L_21 _
      L_2201	L_2202	L_2301	L_2302	L_2303	L_2304	L_2305	L_2306	L_2307 L_2308 L_2401 L_2402 L_2403 L_2404 L_2405 _
	 L_2406	L_01_2	L_02_2	L_03_2	L_04_2	L_05_2	L_06_2	L_07_2	L_08_2	L_09_2	L_10_2	L_11_2	L_12_2	L_13_2	L_14_2	L_15_2	L_16_2	L_17_2	L_18_2	L_19_2	L_20_2	L_21_2 _ 
      L_2201_2	L_2202_2	L_2301_2	L_2302_2	L_2303_2	L_2304_2	L_2305_2	L_2306_2	L_2307_2 L_2308_2 _
      L_2401_2	L_2402_2	L_2403_2	L_2404_2	L_2405_2	L_2406_2 _ 	
      VA_01	VA_02	VA_03	VA_04	VA_05	VA_06	VA_07	VA_08	VA_09 VA_10 VA_11 VA_12 VA_13 VA_14 VA_15 VA_16 VA_17 VA_18 VA_19 VA_20 VA_21 _
      VA_2201 VA_2202 VA_2301 VA_2302 VA_2303 VA_2304	VA_2305 VA_2306 VA_2307 VA_2308 _
      VA_2401 VA_2402 VA_2403	 VA_2404 VA_2405 VA_2406 _	
      VA_01_2	VA_02_2	VA_03_2 VA_04_2 VA_05_2 VA_06_2 VA_07_2 VA_08_2 VA_09_2 VA_10_2 VA_11_2 VA_12_2 VA_13_2 VA_14_2 VA_15_2 VA_16_2 VA_17_2 VA_18_2 VA_19_2 VA_20_2 VA_21_2 _
      VA_2201_2 VA_2202_2 VA_2301_2 VA_2302_2 VA_2303_2 VA_2304_2	VA_2305_2	 VA_2306_2 VA_2307_2 VA_2308_2 _
      VA_2401_2 VA_2402_2 VA_2403_2 VA_2404_2 VA_2405_2 VA_2406_2 _
TTCO_VOL_21_2 TTCO_VOL_22_2 TTCO_VOL_24_2 TTCO_VOL_21 TTCO_VOL_22 TTCO_VOL_24 DISPINC_VAL_2 DISPINC_VAL _
(gdpter_2/gdpbis_2-1)*100 (gdpter/gdpbis-1)*100 _
IA IA_01 IA_02 IA_03 IA_04 IA_05 IA_06 IA_07 IA_08 IA_09 IA_10 IA_11 IA_12 IA_13 IA_13_20 IA_14 IA_15  IA_16 IA_17  IA_18 IA_19 IA_20 IA_21 _
IA_2201 IA_2202 IA_2301 IA_2302 IA_2303 IA_2304 IA_2305 IA_2306 IA_2307 IA_2308 IA_2401 IA_2402 IA_2403 IA_2404 IA_2405 IA_2406   _
REHAB_VAL NEWBUIL_H01 PNEWBUIL_H01 PREHAB_H01 REHAB_H01 PEXP_13_H01 CH_03 PGDP _
IA_2 IA_01_2 IA_02_2 IA_03_2 IA_04_2 IA_05_2 IA_06_2 IA_07_2 IA_08_2 IA_09_2 IA_10_2 IA_11_2 IA_12_2 IA_13_2 IA_13_20_2 IA_14_2 IA_15_2 IA_16_2 IA_17_2 IA_18_2 IA_19_2 IA_20_2 IA_21_2 _
IA_2201_2 IA_2202_2 IA_2301_2 IA_2302_2 IA_2303_2 IA_2304_2 IA_2305_2 IA_2306_2 IA_2307_2 IA_2308_2 IA_2401_2 IA_2402_2 IA_2403_2 IA_2404_2 IA_2405_2 IA_2406_2  _
REHAB_VAL_2 NEWBUIL_H01_2 PNEWBUIL_H01_2 PREHAB_H01_2 REHAB_H01_2 PEXP_13_H01_2 CH_03_2 PGDP_2 _
CH_01_2 CH_02_2 CH_03_2 CH_04_2 CH_05_2 CH_06_2 CH_07_2 CH_08_2 CH_09_2 CH_10 CH_11_2 CH_12_2 CH_13_2 CH_14_2 _
CH_15_2 CH_16_2 CH_17_2 CH_18_2 CH_19_2 CH_20_2 CH_21_2 CH_22_2 CH_23_2 CH_24_2 _ 
CH_01 CH_02 CH_03 CH_04 CH_05 CH_06 CH_07 CH_08 CH_09 CH_10 CH_11 CH_12 CH_13 CH_14 _
CH_15 CH_16 CH_17 CH_18 CH_19 CH_20 CH_21 CH_22 CH_23 CH_24 _
PCH_01_2 PCH_02_2 PCH_03_2 PCH_04_2 PCH_05_2 PCH_06_2 PCH_07_2 PCH_08_2 PCH_09_2 PCH_10 _
PCH_11_2 PCH_12_2 PCH_13_2 PCH_14_2 PCH_15_2 PCH_16_2 PCH_17_2 PCH_18_2 PCH_19_2 PCH_20_2 _
PCH_21_2 PCH_22_2 PCH_23_2 PCH_24_2 PCH_01 PCH_02 PCH_03 PCH_04 PCH_05 PCH_06 _ 
PCH_07 PCH_08 PCH_09 PCH_10 PCH_11 PCH_12 PCH_13 PCH_14 PCH_15 PCH_16 _ 
PCH_17 PCH_18 PCH_19 PCH_20 PCH_21 PCH_22 PCH_23 PCH_24 PENER_BUIL_H01_2 PENER_BUIL_H01 ENER_BUIL_H01_2 ENER_BUIL_H01 pexp_auto_h01_2 pexp_auto_h01 _
exp_mobauto_val_h01_2 exp_mobauto_val_h01 exp_housing_val_h01_2 exp_housing_val_h01 pop_tot _
I_MDE_01 I_MDE_02 I_MDE_03 I_MDE_04	I_MDE_05 I_MDE_06 I_MDE_07 I_MDE_08	I_MDE_09 I_MDE_10 I_MDE_11 _
 I_MDE_12 I_MDE_13 I_MDE_19 I_MDE_20 I_MDE_01_2 I_MDE_02_2 I_MDE_03_2 I_MDE_04_2 I_MDE_05_2 I_MDE_06_2 I_MDE_07_2 I_MDE_08_2 I_MDE_09_2 I_MDE_10_2 I_MDE_11_2 I_MDE_12_2 I_MDE_13_2  _
I_MDE_19_2 I_MDE_20_2 NDI_ADEB_VOL NDI_ADEB_VOL_2 INV_Road PIA_13_19_2 PIA_13_19 PIA_13_19 PIA_13_20_2 PIA_20_2 PIA_20 PIA_15_2 CSPE CSPE_2 _
Carbon_foot Carbon_foot_2 PSUBD_15_2*SUBD_15_2 CU_MWH_PGDP_21 CU_MWH_PGDP_22 CU_MWH_PGDP_23  CU_MWH_PGDP_24 CU_MWH_PGDP_2201 CU_MWH_PGDP_2202 CU_MWH_PGDP_2301 _
CU_MWH_PGDP_2302 CU_MWH_PGDP_2303 CU_MWH_PGDP_2304 CU_MWH_PGDP_2305 CU_MWH_PGDP_2306 CU_MWH_PGDP_2307 CU_MWH_PGDP_2308 CU_MWH_PGDP_2401 CU_MWH_PGDP_2402 _
CU_MWH_PGDP_2403 CU_MWH_PGDP_2404 CU_MWH_PGDP_2405 CU_MWH_PGDP_2406 CU_MWH_PGDP_21_2 CU_MWH_PGDP_22_2 CU_MWH_PGDP_23_2  CU_MWH_PGDP_24_2 CU_MWH_PGDP_2201_2 _
CU_MWH_PGDP_2202_2 CU_MWH_PGDP_2301_2 CU_MWH_PGDP_2302_2 CU_MWH_PGDP_2303_2 CU_MWH_PGDP_2304_2 CU_MWH_PGDP_2305_2 CU_MWH_PGDP_2306_2 CU_MWH_PGDP_2307_2 CU_MWH_PGDP_2308_2 _
CU_MWH_PGDP_2401_2 CU_MWH_PGDP_2402_2 CU_MWH_PGDP_2403_2 CU_MWH_PGDP_2404_2 CU_MWH_PGDP_2405_2 CU_MWH_PGDP_2406_2 UNR_TOT UNR_TOT_2 _
GDP CH G I+CH_13 I-IA_20 CH_13 IA_20 X M DISPINC_VAL/(POP_TOT*PCH) L SP_G_VAL/PGDP DEBT_G_VAL/PGDP GDP_2 CH_2 G_2 I_2+CH_13_2 I_2-IA_20_2 CH_13_2 IA_20_2 X_2 M_2 DISPINC_VAL_2/(POP_TOT*PCH_2) L_2 SP_G_VAL_2/PGDP_2 DEBT_G_VAL_2/PGDP_2 _
Exp_13 Exp_13_2 exp_newauto_val_h01_ca exp_newauto_val_h01_cb exp_newauto_val_h01_cc exp_newauto_val_h01_cd exp_newauto_val_h01_ce exp_newauto_val_h01_cf exp_newauto_val_h01_cg exp_newauto_val_h01_ca_2 exp_newauto_val_h01_cb_2 exp_newauto_val_h01_cc_2 _
exp_newauto_val_h01_cd_2 exp_newauto_val_h01_ce_2 exp_newauto_val_h01_cf_2 exp_newauto_val_h01_cg_2

                                                                                                                                                                                                                                          
'Reporting_5.sheet(t)
'show Reporting_5

 
group Reporting_6 REC_VAL_0 DIV_GOV_VAL_0 IR_VAL_0 AIC_VAL_0 -CL_S_20_0*L_S_20_0*PROG_L_20_0 PY_20_0*Y_20_0 PTAX_0*TAX_0 PIY_0*IY_0 PIS_0*IS_0 PCSE_TOT_0*CSE_TOT_0 PCSS_TOT_0*CSS_TOT_0 -(PE_20_0*E_20_0) -(PMAT_20_0*MAT_20_0) -(PIY_20_0*IY_20_0) REC_VAL_2 DIV_GOV_VAL_2 IR_VAL_2 AIC_VAL_2 -CL_S_20_2*L_S_20_2*PROG_L_20_2 PY_20_2*Y_20_2 PTAX_2*TAX_2 PIY_2*IY_2 PIS_2*IS_2 PCSE_TOT_2*CSE_TOT_2 PCSS_TOT_2*CSS_TOT_2 -(PE_20_2*E_20_2) -(PMAT_20_2*MAT_20_2) -(PIY_20_2*IY_20_2) DEP_VAL_0 CL_S_20_0*L_S_20_0*PROG_L_20_0 R_G_0(-1)*DEBT_G_VAL_0(-1) PRESOC_VAL_0 SUB_RENOV_VAL_0 SUB_AUTO_VAL_0 PE_20_0*E_20_0 PMAT_20_0*MAT_20_0 PIY_20_0*IY_20_0 PIA_20_0*IA_20_0 PG_0*G_0-PG_20_0*G_20_0 -(PSUB_0*SUB_0-PSUB_01_0*SUB_01_0) -(PSY_0*SY_0-PSY_01_0*SY_01_0) DEP_VAL_2 CL_S_20_2*L_S_20_2*PROG_L_20_2 R_G_2(-1)*DEBT_G_VAL_2(-1) PRESOC_VAL_2 SUB_RENOV_VAL_2 SUB_AUTO_VAL_2 PE_20_2*E_20_2 PMAT_20_2*MAT_20_2 PIY_20_2*IY_20_2 PIA_20_2*IA_20_2 PG_2*G_2-PG_20_2*G_20_2 -(PSUB_2*SUB_2-PSUB_01_2*SUB_01_2) -(PSY_2*SY_2-PSY_01_2*SY_01_2) PGDP_0 PGDP_2 GDP_0 GDP_2 INC_GOV_OTH_NET REDIS_VAL_H

'Reporting_6.sheet(t)
'show Reporting_6
                          
'Nouveau reporting créé par la DGT (Pierre-Louis Girard) pour exporter les données utiles à MatMat
'fait pour contourner le pb que la subroutine export_matmat dans matter.prg ne parvenait pas à sortir les variables avec des _1 au lieu des _0 pour AME
'on crée ici un reporting puis dans main un fichier csv reprenant ces séries est créé
'(attention dans le fichier que il avait envoyé il avait repris les séries de export_matter au lieu de export_matmat)
group export_group_matmat CH_01 CH_02 CH_03 CH_04 CH_05 CH_06 CH_07 CH_08 CH_09 CH_10 CH_11 CH_12 CH_13 CH_14 CH_15 CH_16 CH_17 CH_18 CH_19 CH_20 CH_21 CH_22 CH_23 CH_24 G_01 G_02 G_03 G_04 G_05 G_06 G_07 G_08 G_09 G_10 G_11 G_12 G_13 G_14 G_15 G_16 G_17 G_18 G_19 G_20 G_21 G_22 G_23 G_24 I_01 I_02 I_03 I_04 I_05 I_06 I_07 I_08 I_09 I_10 I_11 I_12 I_13 I_14 I_15 I_16 I_17 I_18 I_19 I_20 I_21 I_22 I_23 I_24 CID_01 CID_02 CID_03 CID_04 CID_05 CID_06 CID_07 CID_08 CID_09 CID_10 CID_11 CID_12 CID_14 CID_16 CID_17 CID_18 CID_19 CID_21 CID_22 CID_23 CID_24 CIM_01 CIM_02 CIM_03 CIM_04 CIM_05 CIM_06 CIM_07 CIM_08 CIM_09 CIM_10 CIM_11 CIM_12 CIM_14 CIM_16 CIM_17 CIM_18 CIM_19 CIM_21 CIM_22 CIM_23 CIM_24 CHD_01 CHD_02 CHD_03 CHD_04 CHD_05 CHD_06 CHD_07 CHD_08 CHD_09 CHD_10 CHD_11 CHD_12 CHD_13 CHD_14 CHD_15 CHD_16 CHD_17 CHD_18 CHD_19 CHD_20 CHD_21 CHD_22 CHD_23 CHD_24 CHM_01 CHM_02 CHM_03 CHM_04 CHM_05 CHM_06 CHM_07 CHM_08 CHM_09 CHM_10 CHM_11 CHM_12 CHM_14 CHM_16 CHM_17 CHM_18 CHM_19  CHM_21 CHM_22 CHM_23 CHM_24 GD_01 GD_02 GD_03 GD_04 GD_05 GD_06 GD_07 GD_08 GD_09 GD_10 GD_11 GD_12 GD_13 GD_14 GD_15 GD_16 GD_17 GD_18 GD_19 GD_20 GD_21 GD_22 GD_23 GD_24 GM_01 GM_02 GM_03 GM_04 GM_05 GM_06 GM_07 GM_08 GM_09 GM_10 GM_11 GM_12 GM_14 GM_16 GM_17 GM_18 GM_19 GM_21 GM_22 GM_23 GM_24 ID_01 ID_02 ID_03 ID_04 ID_05 ID_06 ID_07 ID_08 ID_09 ID_10 ID_11 ID_12 ID_13 ID_14 ID_15 ID_16 ID_17 ID_18 ID_19 ID_20 ID_21 ID_22 ID_23 ID_24 IM_01 IM_02 IM_03 IM_04 IM_05 IM_06 IM_07 IM_08 IM_09 IM_10 IM_11 IM_12 IM_14 IM_16 IM_17 IM_18 IM_19 IM_21 IM_22 IM_23 IM_24  XD_01 XD_02 XD_03 XD_04 XD_05 XD_06 XD_07 XD_08 XD_09 XD_10 XD_11 XD_12 XD_14 XD_16 XD_17 XD_18 XD_19 XD_21 XD_22 XD_23 XD_24 XM_03 XM_04 XM_05 XM_09 XM_10 XM_12 XM_22 XM_23 XM_24 PhiY_22_2201 PhiY_22_2202  PhiY_23_2301 PhiY_23_2302 PhiY_23_2303 PhiY_23_2304 PhiY_23_2305 PhiY_23_2306 PhiY_23_2307 PhiY_23_2308 PhiY_24_2401 PhiY_24_2402 PhiY_24_2403 PhiY_24_2404 PhiY_24_2405 PhiY_24_2406 M_03 M_04 M_05 M_09 M_10 M_12 M_22 M_23 M_24 X_01 X_02 X_03 X_04 X_05 X_06 X_07 X_08 X_09 X_10 X_11 X_12 X_14 X_16 X_17 X_18 X_19 X_21 X_22 X_23 X_24 YQ_01 YQ_02 YQ_03 YQ_04 YQ_05 YQ_06 YQ_07 YQ_08 YQ_09 YQ_10 YQ_11 YQ_12 YQ_13 YQ_14 YQ_15 YQ_16 YQ_17 YQ_18 YQ_19 YQ_20 YQ_21 YQ_22 YQ_23 YQ_24 PCH_01 PCH_02 PCH_03 PCH_04 PCH_05 PCH_06 PCH_07 PCH_08 PCH_09 PCH_10 PCH_11 PCH_12 PCH_13 PCH_14 PCH_15 PCH_16 PCH_17 PCH_18 PCH_19 PCH_20 PCH_21 PCH_22 PCH_23 PCH_24 PG_01 PG_02 PG_03 PG_04 PG_05 PG_06 PG_07 PG_08 PG_09 PG_10 PG_11 PG_12 PG_13 PG_14 PG_15 PG_16 PG_17 PG_18 PG_19 PG_20 PG_21 PG_22 PG_23 PG_24 PI_01 PI_02 PI_03 PI_04 PI_05 PI_06 PI_07 PI_08 PI_09 PI_10 PI_11 PI_12 PI_13 PI_14 PI_15 PI_16 PI_17 PI_18 PI_19 PI_20 PI_21 PI_22 PI_23 PI_24 PX_01 PX_02 PX_03 PX_04 PX_05 PX_06 PX_07 PX_08 PX_09 PX_10 PX_11 PX_12 PX_14 PX_16 PX_17 PX_18 PX_19 PX_21 PX_22 PX_23 PX_24 PM_03 PM_04 PM_05 PM_09 PM_10 PM_12 PM_22 PM_23 PM_24 PYQ_01 PYQ_02 PYQ_03 PYQ_04 PYQ_05 PYQ_06 PYQ_07 PYQ_08 PYQ_09 PYQ_10 PYQ_11 PYQ_12 PYQ_13 PYQ_14 PYQ_15 PYQ_16 PYQ_17 PYQ_18 PYQ_19 PYQ_20 PYQ_21 PYQ_22 PYQ_23 PYQ_24  IA IA_01 IA_02 IA_03 IA_04 IA_05 IA_06 IA_07 IA_08 IA_09 IA_10 IA_11 IA_12 IA_13 IA_14 IA_15 IA_16 IA_17 IA_18 IA_19 IA_20 IA_21 IA_2201 IA_2202 IA_2301 IA_2302 IA_2303 IA_2304 IA_2305 IA_2306 IA_2307 IA_2308 IA_2401 IA_2402 IA_2403 IA_2404 IA_2405 IA_2406 IA_01_01 IA_01_19 IA_01_20 IA_03_01 IA_03_02 IA_03_03 IA_03_04 IA_03_05 IA_03_06 IA_03_07 IA_03_08 IA_03_09 IA_03_10 IA_03_11 IA_03_12 IA_03_13 IA_03_14 IA_03_15 IA_03_16 IA_03_17 IA_03_18 IA_03_19 IA_03_20 IA_03_2301 IA_03_2302 IA_03_2303 IA_03_2304 IA_03_2305 IA_03_2306 IA_03_2307 IA_03_2308 IA_03_2401 IA_03_2402 IA_03_2403 IA_03_2404 IA_03_2405 IA_03_2406 IA_05_01 IA_05_02 IA_05_03 IA_05_04 IA_05_05 IA_05_06 IA_05_07 IA_05_08 IA_05_09 IA_05_10 IA_05_11 IA_05_12 IA_05_13 IA_05_14 IA_05_15 IA_05_16 IA_05_17 IA_05_18 IA_05_19 IA_05_21 IA_05_2201 IA_05_2202 IA_05_2301 IA_05_2302 IA_05_2303 IA_05_2304 IA_05_2305 IA_05_2306 IA_05_2307 IA_05_2308 IA_05_2401 IA_05_2402 IA_05_2403 IA_05_2404 IA_05_2405 IA_05_2406 IA_12_01 IA_12_02 IA_12_03 IA_12_04 IA_12_05 IA_12_06 IA_12_07 IA_12_08 IA_12_09 IA_12_10 IA_12_11 IA_12_12 IA_12_13 IA_12_14 IA_12_15 IA_12_16 IA_12_17 IA_12_18 IA_12_19 IA_12_20 IA_12_21 IA_12_2201 IA_12_2202 IA_12_2301 IA_12_2302 IA_12_2303 IA_12_2304 IA_12_2305 IA_12_2306 IA_12_2307 IA_12_2308 IA_12_2401 IA_12_2402 IA_12_2403 IA_12_2404 IA_12_2405 IA_12_2406 IA_13_01 IA_13_02 IA_13_03 IA_13_04 IA_13_05 IA_13_06 IA_13_07 IA_13_08 IA_13_09 IA_13_10 IA_13_11 IA_13_12 IA_13_13 IA_13_14 IA_13_15 IA_13_16 IA_13_17 IA_13_18 IA_13_19 IA_13_20 IA_13_21 IA_13_2201 IA_13_2202 IA_13_2301 IA_13_2302 IA_13_2303 IA_13_2304 IA_13_2305 IA_13_2306 IA_13_2307 IA_13_2308 IA_13_2401 IA_13_2402 IA_13_2403 IA_13_2404 IA_13_2405 IA_13_2406 IA_19_01 IA_19_02 IA_19_03 IA_19_04 IA_19_05 IA_19_06 IA_19_07 IA_19_08 IA_19_09 IA_19_10 IA_19_11 IA_19_12 IA_19_13 IA_19_14 IA_19_15 IA_19_16 IA_19_17 IA_19_18 IA_19_19 IA_19_20 IA_19_21 IA_19_2201 IA_19_2202 IA_19_2301 IA_19_2302 IA_19_2303 IA_19_2304 IA_19_2305 IA_19_2306 IA_19_2307 IA_19_2308 IA_19_2401 IA_19_2402 IA_19_2403 IA_19_2404 IA_19_2405 IA_19_2406 PIA PIA_01 PIA_02 PIA_03 PIA_04 PIA_05 PIA_06 PIA_07 PIA_08 PIA_09 PIA_10 PIA_11 PIA_12 PIA_13 PIA_14 PIA_15 PIA_16 PIA_17 PIA_18 PIA_19 PIA_20 PIA_21 PIA_2201 PIA_2202 PIA_2301 PIA_2302 PIA_2303 PIA_2304 PIA_2305 PIA_2306 PIA_2307 PIA_2308 PIA_2401 PIA_2402 PIA_2403 PIA_2404 PIA_2405 PIA_2406 PIA_01_01 PIA_01_19 PIA_01_20 PIA_03_01 PIA_03_02 PIA_03_03 PIA_03_04 PIA_03_05 PIA_03_06 PIA_03_07 PIA_03_08 PIA_03_09 PIA_03_10 PIA_03_11 PIA_03_12 PIA_03_13 PIA_03_14 PIA_03_15 PIA_03_16 PIA_03_17 PIA_03_18 PIA_03_19 PIA_03_20 PIA_03_2301 PIA_03_2302 PIA_03_2303 PIA_03_2304 PIA_03_2305 PIA_03_2306 PIA_03_2307 PIA_03_2308 PIA_03_2401 PIA_03_2402 PIA_03_2403 PIA_03_2404 PIA_03_2405 PIA_03_2406 PIA_05_01 PIA_05_02 PIA_05_03 PIA_05_04 PIA_05_05 PIA_05_06 PIA_05_07 PIA_05_08 PIA_05_09 PIA_05_10 PIA_05_11 PIA_05_12 PIA_05_13 PIA_05_14 PIA_05_15 PIA_05_16 PIA_05_17 PIA_05_18 PIA_05_19 PIA_05_21 PIA_05_2201 PIA_05_2202 PIA_05_2301 PIA_05_2302 PIA_05_2303 PIA_05_2304 PIA_05_2305 PIA_05_2306 PIA_05_2307 PIA_05_2308 PIA_05_2401 PIA_05_2402 PIA_05_2403 PIA_05_2404 PIA_05_2405 PIA_05_2406 PIA_12_01 PIA_12_02 PIA_12_03 PIA_12_04 PIA_12_05 PIA_12_06 PIA_12_07 PIA_12_08 PIA_12_09 PIA_12_10 PIA_12_11 PIA_12_12 PIA_12_13 PIA_12_14 PIA_12_15 PIA_12_16 PIA_12_17 PIA_12_18 PIA_12_19 PIA_12_20 PIA_12_21 PIA_12_2201 PIA_12_2202 PIA_12_2301 PIA_12_2302 PIA_12_2303 PIA_12_2304 PIA_12_2305 PIA_12_2306 PIA_12_2307 PIA_12_2308 PIA_12_2401 PIA_12_2402 PIA_12_2403 PIA_12_2404 PIA_12_2405 PIA_12_2406 PIA_13_01 PIA_13_02 PIA_13_03 PIA_13_04 PIA_13_05 PIA_13_06 PIA_13_07 PIA_13_08 PIA_13_09 PIA_13_10 PIA_13_11 PIA_13_12 PIA_13_13 PIA_13_14 PIA_13_15 PIA_13_16 PIA_13_17 PIA_13_18 PIA_13_19 PIA_13_20 PIA_13_21 PIA_13_2201 PIA_13_2202 PIA_13_2301 PIA_13_2302 PIA_13_2303 PIA_13_2304 PIA_13_2305 PIA_13_2306 PIA_13_2307 PIA_13_2308 PIA_13_2401 PIA_13_2402 PIA_13_2403 PIA_13_2404 PIA_13_2405 PIA_13_2406 PIA_19_01 PIA_19_02 PIA_19_03 PIA_19_04 PIA_19_05 PIA_19_06 PIA_19_07 PIA_19_08 PIA_19_09 PIA_19_10 PIA_19_11 PIA_19_12 PIA_19_13 PIA_19_14 PIA_19_15 PIA_19_16 PIA_19_17 PIA_19_18 PIA_19_19 PIA_19_20 PIA_19_21 PIA_19_2201 PIA_19_2202 PIA_19_2301 PIA_19_2302 PIA_19_2303 PIA_19_2304 PIA_19_2305 PIA_19_2306 PIA_19_2307 PIA_19_2308 PIA_19_2401 PIA_19_2402 PIA_19_2403 PIA_19_2404 PIA_19_2405 PIA_19_2406 

'export_group_matmat.sheet(t)
'show export_group_matmat
 
endif

'Pour export des donn�es vers MatMat
'   call export_matter
  'call export_matmat
    'MAT_13_0 MATM_sec_13_0 MATD_sec_13_0 MAT_13_2 MATM_sec_13_2 MATD_sec_13_2 E_13_0 EM_sec_13_0 ED_sec_13_0     E_13_2 EM_sec_13_2 ED_sec_13_2  
    endif
  
  endif
  
  endif
  
  endif

smpl @all

endsub

subroutine load_shocks(string %scenario_name)
  ' Load data for the shock to be simulated
  call load_data_shocks(".\..\..\data\shocks\" + %scenario_name + ".xls")
endsub


