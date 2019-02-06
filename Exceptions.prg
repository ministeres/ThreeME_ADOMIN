' CHOSES QUI POSENT PROBLEMES

      series TvatD_{%com}=0.196


	
      series TenertM_{%com}=0


For %sec 2201 2202 
     series PM_{%sec}=PM_22
next 

For %sec 2301 2302 2303 2304 2305 2306 2307 2308 
     series PM_{%sec}=PM_23
next 

For %sec 2401 2402 2403 2404 2405 2406                      
    series PM_{%sec}=PM_24
next 


For %mar {%list_trsp}
    For %varname MTD MTM       
      For %com  01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24  
          series SUBST_{%varname}_n_{%mar}_{%com}=0 
          series SUBST_{%varname}_{%mar}_{%com}=0          
      next
    next 
next

call create_series_aggr_nrj("Y")
call create_series_aggr_nrj("E")


series IMP_BUD_20_bis=0


Sortir secteurs hybride

For %sec {%list_sec}      
        series EMS_21_{%sec}=EMS_CH_{%sec}
        series EMS_22_{%sec}=EMS_PT_{%sec}
        series EMS_24_{%sec}=EMS_GZ_{%sec}
next

series IC_21=EMS_CH/(QD_21+M_21-X_21)
series IC_22=(EMS_PT/(QD_22+M_22-X_22))*PhiY_22_2201
series IC_24=(EMS_GZ/(QD_24+M_24-X_24))*PhiY_24_2401


!step_2=0
For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
      call create_series("Q_Mtep_"+%ene,STEADYSTATE(2,1),MTEP(1+!step_2,5))
!step_2=!step_2+1  
next 


!step_2=0
For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406  
      call create_series("Phi_Mtep_"+%ene,0,MTEP(1+!step_2,6))
!step_2=!step_2+1  
next

!step_2=0
For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406  
      call create_series("fac_conv_phi_"+%ene,0,MTEP(1+!step_2,7))
!step_2=!step_2+1  
next  

!step_2=0
For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406  
      call create_series("fac_conv_EP_"+%ene,0,MTEP(1+!step_2,9))
!step_2=!step_2+1  
next  


%equation = "Q_Mtep=0"  
  For %ene 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
    if @elem(Q_Mtep_{%ene},%baseyear) <> 0 then         
      %equation = %equation +"+ Q_Mtep_"+%ene
    endif
  next  
series {%equation}


%equation = "Q_Mtep_22=0"  
  For %ene  2201 2202 
    if @elem(Q_Mtep_{%ene},%baseyear) <> 0 then         
      %equation = %equation +"+ Q_Mtep_"+%ene
    endif
  next  
series {%equation}


%equation = "Q_Mtep_23=0"  
  For %ene 2301 2302 2303 2304 2305 2306 2307 2308 
    if @elem(Q_Mtep_{%ene},%baseyear) <> 0 then         
      %equation = %equation +"+ Q_Mtep_"+%ene
    endif
  next  
series {%equation}



%equation = "Q_Mtep_24=0"  
  For %ene 2401 2402 2403 2404 2405 2406
    if @elem(Q_Mtep_{%ene},%baseyear) <> 0 then         
      %equation = %equation +"+ Q_Mtep_"+%ene
    endif
  next  
series {%equation}


series {%equation}
%equation = "Q_Mtep_EP=0"  
  For %ene {%list_com_E}
    if @elem(Q_Mtep_EP_{%ene},%baseyear) <> 0 then         
      %equation = %equation +"+ Q_Mtep_EP_"+%ene
    endif
  next  
series {%equation}


series Rec_TCO_VAL=Ttco*(EMS_HH+EMS_S-EMS_21_10-EMS_22_09-EMS_24_08-EMS_24_07)
series Rec_TCO=REC_TCO_VAL/PGDP
series CO_VAL=1000000*(REC_TCO/(EMS_TOT*PGDP))
'series CO_VAL=1000000*(REC_TCO+CIDD+Bonus+Bonus_elec+CSPE)/(EMS_TOT*PGDP)) 'Equation telle qu'elle est dans la version V1'


series CO2_price_signal= (ENERT_21+OTHT_21+ENERT_22+ENERT_24+Rec_TCO)*1000000/EMS_TOT

series EN_price_signal = (ENERT_21 +OTHT_21+ENERT_22+OTHTD_23+ENERT_24+Rec_TCO)/Q_Mtep_EP
 
 '' ***********ENDOGENOUS EMPLOYER SOCIAL CONTRIBUTION RATE WHEN CARBON TAX
%equation = "Rec_TCO_ETS ="
  For %sec 04 05 06 07 08 09 10 18 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
    %equation = %equation + "+ Ttco*EMS_"+%sec
  next
series {%equation}

%equation = "WAGES_ETS ="
  For %sec 04 05 06 07 08 09 10 18 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
    %equation = %equation + "+ W_S_"+%sec+"*L_S_"+%sec
  next
series {%equation}


%equation = "Rec_TCO_NETS ="
  For %sec 01 02 03 11 12 13 14 15 16 17 19 20
    %equation = %equation + "+ tTCO*EMS_"+%sec
  next
series {%equation}

For %com 01 02 03 04 05 06 07 08 09 10 11 20 21 22 23 24 
  series YQS_{%com}=YQ_{%com}*@elem(1+TenertD_{%com}+TothtD_{%com}+Tsub_{%com}+(MTD_{%com}+MCD_{%com})/YQ_{%com},%baseyear)
  series MS_{%com}=M_{%com}*@elem(1+TenertM_{%com}+TothtM_{%com}+(MTM_{%com}+MCM_{%com})/M_{%com},%baseyear)
next

For %com 12 13 14 15 16 17 18 19
  series YQS_{%com}=YQ_{%com}*@elem(1+TenertD_{%com}+TothtD_{%com}+Tsub_{%com},%baseyear)
  series MS_{%com}=M_{%com}*@elem(1+TenertM_{%com}+TothtM_{%com},%baseyear)
next


  series PRESOC_DOM_U_VAL=0.3*W_S*Un_TOT



series REC_VAL=PY_20*Y_20-(CL_S_20*L_S_20*PROG_L_20+PE_20*E_20+PMAT_20*MAT_20+PIY_20*IY_20)+DIV_GOV_VAL+PTAX*TAX+PIY*IY+PIS*IS+IR_VAL+AIC_VAL+PCSE_TOT*CSE_TOT+PCSS_TOT*CSS_TOT+INC_GOV_OTH_net
series DEP_VAL= BF_G_VAL + REC_VAL  
series R_G =@elem((DEP_VAL-(CL_S_20*L_S_20*PROG_L_20+PE_20*E_20+PMAT_20*MAT_20+PIY_20*IY_20+PRESOC_VAL+(PG*G-PG_20*G_20)-((PSUB*SUB-PSUB_01*SUB_01)+(PSY*SY-PSY_01*SY_01))+PIA_20*IA_20+TRANS_GOV_OTH))/DEBT_G_VAL(-1),%baseyear) 
 
    series elem = @elem(PEXP_13*EXP_13 - (PNEWBUIL*NEWBUIL + PREHAB*REHAB),%baseyear)
    scalar calib = @elem(elem,%baseyear)
    call create_series("EXP_13_OTH_VAL",STEADYSTATE(1,1),calib)


' *******************************************************************************************************************************
' ***************************************************** MODEL SPECIFICATION SUBROUTINE***************************** 
'******************************************************************************************************************************   

For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
      if @elem(YQ_{%com},%baseyear) <> 0 then
'equation 1.32
          {%modelname}.append YQ_{%com}*PYQ_{%com} = PQD_{%com}*QD_{%com} - PVATD_{%com}*VATD_{%com} - POTHTD_{%com}*OTHTD_{%com} - PSUB_{%com}*SUB_{%com} -(PMCD_{%com}*MCD_{%com}+PMTD_{%com}*MTD_{%com})- PENERTD_{%com}*ENERTD_{%com} - TCO_VALD_sec_{%com} - TCO_HH_VAL_com_{%com}*CHD_{%com}/((CH_{%com}>0)*CH_{%com}+(CH_{%com}<=0)*1)
      
'equation 1.33
          {%modelname}.append YQbis_{%com} = QD_{%com} - VATD_{%com} - OTHTD_{%com} - SUB_{%com} - (MCD_{%com}+MTD_{%com}) - ENERTD_{%com}
      endif
    


'equation 1.36 1.37  PROBLEME! Double indice sur les prix superflux. Corriger après l'ambiguité sur les indices. Puis endogeneiser PMTD_%mar (sans _%com)
For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
  if @elem(MTD_{%com},%baseyear) <> 0 then
  	%equation ="PMTD_"+%com+"*MTD_"+%com+"=0"
			For %mar {%list_trsp}  
				  if @elem(MTD_{%mar}_{%com},%baseyear) <> 0 then
				      %equation = %equation+"+PMTD_"+%mar+"_"+%com+"*MTD_"+%mar+"_"+%com	
				  endif
     	next  


'equation 1.38 1.39  
For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 19 20 21 22 23 24
  if @elem(MTM_{%com},%baseyear) <> 0 then
  	%equation ="PMTM_"+%com+"*MTM_"+%com+"=0"
			  For %mar {%list_trsp}  
				  if @elem(MTM_{%mar}_{%com},%baseyear) <> 0 then
					     %equation = %equation+"+PMTM_"+%mar+"_"+%com+"*MTM_"+%mar+"_"+%com	
				  endif
     	  next  
    {%modelname}.append {%equation}	


  

call aggregate_energy ("Y")
call aggregate_energy ("E")

'equation 2.13
For %mar 12 13 14 15 16 17 18      
    For %com 01 02 03 04 05 06 07 08 09 10 11 19 20 21 22 23 24 
          if @elem(MTM_{%mar}_{%com},%baseyear) <> 0 then
              {%modelname}.append d(log(MTM_{%mar}_{%com}))=d(log(M_{%com}))+d(SUBST_MTM_{%mar}_{%com})
          endif
    next
next  
                          
!step_L=1
For %com  01 02 03 04 05 06 07 08 09 10 11 19 20 21 22 23 24

    if @elem(MTM_12_{%com},%baseyear) <> 0 then
           {%modelname}.append d(SUBST_MTM_n_12_{%com})=  - ES_TRANSP_MARG(!step_L,1)*d(log(PE_12)-log(PE_13))*(PMTM_13_{%com}(-1)*MTM_13_{%com}(-1)/(PMTM_12_{%com}(-1)*MTM_12_{%com}(-1)+PMTM_13_{%com}(-1)*MTM_13_{%com}(-1))) + _
					                                                - ES_TRANSP_MARG(!step_L,2)*d(log(PE_12)-log(PE_14))*(PMTM_14_{%com}(-1)*MTM_14_{%com}(-1)/(PMTM_12_{%com}(-1)*MTM_12_{%com}(-1)+PMTM_14_{%com}(-1)*MTM_14_{%com}(-1))) + _
		

'equation 2.14
For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24 
     if @elem(MCD_{%com},%baseyear) <> 0 then
        {%modelname}.append d(log(MCD_{%com}))=d(log(YQ_{%com}))
     endif
next

 'equation 2.15
For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24 
   if @elem(MCM_{%com},%baseyear) <> 0 then
       {%modelname}.append d(log(MCM_{%com}))=d(log(M_{%com}))
   endif
next



'equation 2.33 
For %mar {%list_trsp} 
    if @elem(MTM_{%mar},%baseyear) <> 0 then
  	       %equation ="PMTM_"+%mar+"*MTM_"+%mar+"=0 - M_"+%mar+"/(YQ_"+%mar+"+M_"+%mar+")*("
			        For %com 01 02 03 04 05 06 07 08 09 10 11 19 20 21 22 23 24
				          if @elem(MTM_{%mar}_{%com},%baseyear) <> 0 then
			                 %equation = %equation+"+ PMTD_"+%mar+"_"+%com+"*MTD_"+%mar+"_"+%com+"+ PMTM_"+%mar+"_"+%com+"*MTM_"+%mar+"_"+%com
                  endif
     		      next  
            {%modelname}.append {%equation}	)
    endif
next   
  
'equation 2.34
For %mar {%list_trsp} 
    if @elem(MTM_{%mar},%baseyear) <> 0 then
  	       %equation ="MTM_"+%mar+"=0 - M_"+%mar+"/(YQ_"+%mar+"+M_"+%mar+")*("
			       For %com 01 02 03 04 05 06 07 08 09 10 11 19 20 21 22 23 24
				          if @elem(MTM_{%mar}_{%com},%baseyear) <> 0 then
			                %equation = %equation+"+ MTD_"+%mar+"_"+%com+"+ MTM_"+%mar+"_"+%com	
				          endif
     		     next  
          {%modelname}.append {%equation} )	
    endif
next 


if @elem(MCD_19,%baseyear) <> 0 then

  'equation 2.35 
	%equation ="PMCD_19*MCD_19=0 - YQ_19/(YQ_19+M_19)*("
		For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24
			%equation = %equation+"+ PMCM_"+%com+"*MCM_"+%com+"+PMCD_"+%com+"*MCD_"+%com	
    next  
  {%modelname}.append {%equation} )	

    
  'equation 2.36
	%equation ="MCD_19=0 - YQ_19/(YQ_19+M_19)*("
		  For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24
			     %equation = %equation+"+ MCM_"+%com+"+MCD_"+%com	
     	next  
  {%modelname}.append {%equation} )	

endif 

if @elem(MCM_19,%baseyear) <> 0 then

  'equation 2.37
	%equation ="PMCM_19*MCM_19=0 - M_19/(YQ_19+M_19)*("
		For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24
			%equation = %equation+"+ PMCM_"+%com+"*MCM_"+%com+"+PMCD_"+%com+"*MCD_"+%com	   
    next  
	{%modelname}.append {%equation} )	

  'equation 2.38
	%equation ="MCM_19=0 - M_19/(YQ_19+M_19)*("
		For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 20 21 22 23 24
			%equation = %equation+"+ MCM_"+%com+"+MCD_"+%com	
   	next  
    {%modelname}.append {%equation} )	 
endif 



'-------------------***Social Security Accounting***-----------------
''***************** For ETS sectors
For %sec 04 05 06 07 08 09 10 18 21 2201 2202 2301 2302 2303 2304 2305 2306 2307 2308 2401 2402 2403 2404 2405 2406
  {%modelname}.append TCSE_{%sec} = @elem(TCSE_{%sec},%baseyear) - Rec_TCO_ETS/WAGES_ETS
next

''***************** For non ETS sectors
For %sec 01 02 03 11 12 13 14 15 16 17 19 20
  {%modelname}.append TCSE_{%sec} = @elem(TCSE_{%sec},%baseyear) - Rec_TCO_NETS/WAGES_NETS
next



'equation 3.54
{%modelname}.append PCSE_ROW = PCH_19



    'equation 3.58
    {%modelname}.append PCSS_{%sec} = PCH_19  


    'equation 3.60
    {%modelname}.append PCSS_SE_{%sec} = PCH_19



'equation 3.68
{%modelname}.append REC_VAL=PY_20*Y_20-(CL_S_20*L_S_20*PROG_L_20+PE_20*E_20+PMAT_20*MAT_20+PIY_20*IY_20)+DIV_GOV_VAL+PTAX*TAX+PIY*IY+PIS*IS+IR_VAL+AIC_VAL+PCSE_TOT*CSE_TOT+PCSS_TOT*CSS_TOT+INC_GOV_OTH_net+REC_TCO_VAL
       


'equation 3.69
{%modelname}.append PRESOC_DOM_U_VAL=0.3*W_S*Un_TOT

'-----------------------Total Spendings--------------------------------                     
'equation 3.79
{%modelname}.append DEP_VAL=CL_S_20*L_S_20*PROG_L_20+PE_20*E_20+PMAT_20*MAT_20+PIY_20*IY_20+R_G(-1)*DEBT_G_VAL(-1)+PRESOC_VAL+(PG*G-PG_20*G_20)-((PSUB*SUB-PSUB_01*SUB_01)+(PSY*SY-PSY_01*SY_01))+PIA_20*IA_20+TRANS_GOV_OTH+TCO_HH_VAL
 
        {%modelname}.append d(log(PREHAB_{%hous}_{%class}_{%class2})) = d(log(PCH_13))

For %hous {%list_household}
 'gelvol '{%modelname}.append d(log(EXP_13_OTH_VAL_{%hous})) = (@year>{%baseyear})*(d(log(DISPINC_VAL_{%hous})) + d(log((1 - MPS_HH_{%hous}))))
 {%modelname}.append d(log(EXP_13_OTH_VAL_{%hous})) = (d(log(DISPINC_VAL_{%hous})) + d(log((1 - MPS_HH_{%hous}))))
     
 'gelvol '{%modelname}.append EXP_13_{%hous} = @elem(PNewBUIL_{%hous},%baseyear)*NewBUIL_{%hous} + @elem(PREHAB_{%hous},%baseyear)*REHAB_{%hous} +  EXP_13_OTH_val_{%hous}/PEXP_13_{%hous}
 {%modelname}.append EXP_13_{%hous} = (@year>{%baseyear})*(@elem(PNewBUIL_{%hous},%baseyear)*NewBUIL_{%hous} + @elem(PREHAB_{%hous},%baseyear)*REHAB_{%hous} +  EXP_13_OTH_val_{%hous}/PEXP_13_{%hous}) _
                                        + (@year=<{%baseyear})*EXP_13_{%hous}(-1)*(1+STEADYSTATE(2,1))
next


' Equation H.4.69 
    %equation = "EXP_13= 0" 
  For %hous {%list_household}     
      if @elem(EXP_13_{%hous},%baseyear) <> 0 then
        %equation = %equation + "+ EXP_13_"+%hous
      endif 
  next             
{%modelname}.append {%equation}  


      {%modelname}.append d(log(PNewAUTO_{%hous}_{%class})) = d(log(PCH_03))


    {%modelname}.append EXP_03_{%hous} = (@year>{%baseyear})*(@elem(PNewAUTO_{%hous},%baseyear)*NewAUTO_{%hous} +  EXP_03_OTH_val_{%hous}/PEXP_03_{%hous}) _
                                            + (@year=<{%baseyear})*EXP_03_{%hous}(-1)*(1+STEADYSTATE(2,1))




if %sec="21" then
          {%modelname}.append PY_n_{%sec} = PM_{%sec}


'equation 5.18
For %com 01 02 03 04 05 06 07 08 09 10 11 20 22
  if @elem(YQS_{%com},%baseyear) <> 0 then
    {%modelname}.append PYQS_{%com}*YQS_{%com}=PYQ_{%com}*YQ_{%com}*(1+TothtD_{%com}) + YQ_{%com}*(TenertD_{%com}+Tsub_{%com})+PMTD_{%com}*MTD_{%com}+PMCD_{%com}*MCD_{%com}      
  endif
next

For %com 12 13 14 15 16 17 18 19
  if @elem(YQS_{%com},%baseyear) <> 0 then
    {%modelname}.append PYQS_{%com}*YQS_{%com}=PYQ_{%com}*YQ_{%com}*(1+TothtD_{%com}) + YQ_{%com}*(TenertD_{%com}+Tsub_{%com})
  endif
next


For %com 01 02 03 04 05 06 07 08 09 10 11 20 22
  if @elem(MS_{%com},%baseyear) <> 0 then
    {%modelname}.append PMS_{%com}*MS_{%com}=PM_{%com}*M_{%com}*(1+TothtM_{%com})+M_{%com}*TenertM_{%com}+PMTM_{%com}*MTM_{%com}+PMCM_{%com}*MCM_{%com}      
  endif
next

For %com 12 13 14 15 16 17 18 19
  if @elem(MS_{%com},%baseyear) <> 0 then
    {%modelname}.append PMS_{%com}*MS_{%com}=PM_{%com}*M_{%com}*(1+TothtM_{%com})+M_{%com}*TenertM_{%com}
  endif 
next




'equation 5.26
For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 22
  if @elem(CHD_{%com},%baseyear) <> 0 then
      {%modelname}.append PCHD_{%com}=PYQS_{%com}*(1+TvatD_{%com})/(1+@elem(TvatD_{%com},%baseyear))
  endif
next


For %com 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 22
  	  if @elem(CHM_{%com},%baseyear) <> 0 then
          {%modelname}.append PCHM_{%com}=PMS_{%com}*(1+TvatM_{%com})/(1+@elem(TvatM_{%com},%baseyear))
      endif 
next



For %com 01 02 03 04 05 06 07 08 09 10 11 20 21 22 23 24
  For %mar {%list_trsp}
    if @elem(MTD_{%mar}_{%com},%baseyear) <> 0 then
      {%modelname}.append PMTD_{%mar}_{%com} = YQ_{%mar}/(YQ_{%mar}+M_{%mar})*PYQS_{%mar} + M_{%mar}/(YQ_{%mar}+M_{%mar})*PMS_{%mar}
    endif

    if @elem(MTM_{%mar}_{%com},%baseyear) <> 0 then
      {%modelname}.append PMTM_{%mar}_{%com} = PMTD_{%mar}_{%com}    
    endif
  
  next

    if @elem(MCD_{%com},%baseyear) <> 0 then
      {%modelname}.append PMCD_{%com} = YQ_19/(YQ_19+M_19)*PYQS_19 + M_19/(YQ_19+M_19)*PMS_19
    endif

    if @elem(MCM_{%com},%baseyear) <> 0 then
      {%modelname}.append PMCM_{%com} = PMCD_{%com}
    endif
next


For %sec 2201 2202 
    {%modelname}.append PM_{%sec}=PM_22
next 

For %sec 2301 2302 2303 2304 2305 2306 2307 2308 
    {%modelname}.append PM_{%sec}=PM_23
next 

For %sec 2401 2402 2403 2404 2405 2406                      
    {%modelname}.append PM_{%sec}=PM_24
next


'***************************************************************************************************************************************
'******************************************** BLOCK 6 : GREEN HOUSE GASES EMISSIONS **************************************************** 
'************************************************************************************************************************************** 
'************************************************************************************************************************************** 
if %block_GHGemission="yes" then 'Load the block if "yes" in the main
  
  
'equation 6.1    ' EQUATION DE PASSAGE A METTRE A LA FIN SANS NUMEROTE'    
For %sec {%list_sec}      
      {%modelname}.append EMS_CH_{%sec}=EMS_21_{%sec}
      {%modelname}.append EMS_PT_{%sec}=EMS_22_{%sec}
      {%modelname}.append EMS_GZ_{%sec}=EMS_24_{%sec}
next

         
For %sec {%list_sec} 
  if @elem(EMS_21_{%sec},%baseyear) <> 0 then

      {%modelname}.append d(log(EMS_21_{%sec}))=d(log(E_21_{%sec}))

  endif
next
          
For %sec {%list_sec} 
  if @elem(EMS_22_{%sec},%baseyear) <> 0 then

      {%modelname}.append d(log(EMS_22_{%sec}))=d(log(E_22_{%sec})+log(PhiY_22_2201)) 

  endif
next
          
For %sec {%list_sec} 
  if @elem(EMS_24_{%sec},%baseyear) <> 0 then

      {%modelname}.append d(log(EMS_24_{%sec}))=d(log(E_24_{%sec})+log(PhiY_24_2401))

  endif
next


For %ene  22 23 
    {%modelname}.append d(log(Q_Mtep_H_AUTO_{%ene}))=(@year>{%baseyear})*(d(log(EXP_AUTO_{%ene})))+(@year=<{%baseyear})*log(1+STEADYSTATE(2,1))
next

%equation = "Q_Mtep_H_AUTO=0"  
For %ene 22 23
    if @elem(Q_Mtep_H_AUTO_{%ene},%baseyear) <> 0 then         
          %equation = %equation +"+ Q_Mtep_H_AUTO_"+%ene
    endif
next  
{%modelname}.append {%equation}

if %hybrid_household="yes" then

  {%modelname}.append Q_Mtep_21=Q_Mtep_SEC_21+Q_Mtep_H_BUIL_21+Q_Mtep_ESEC_21

  For %subsec 2201 2202     
    {%modelname}.append Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_BUIL_22+Q_Mtep_H_AUTO_22+Q_Mtep_SEC_22+Q_Mtep_ESEC_22)
  next

  For %subsec 2301 2302 2303 2304 2305 2306 2307 2308     
    {%modelname}.append  Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_BUIL_23+Q_Mtep_H_AUTO_23+Q_Mtep_SEC_23+Q_Mtep_ESEC_23)
  next

  For %subsec 2401 2402 2403 2404 2405 2406     
    {%modelname}.append Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_BUIL_24+Q_Mtep_SEC_24+Q_Mtep_ESEC_24)
  next

else

    {%modelname}.append Q_Mtep_21=Q_Mtep_SEC_21+Q_Mtep_H_21+Q_Mtep_ESEC_21

  For %subsec 2201 2202     
    {%modelname}.append Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_22+Q_Mtep_SEC_22+Q_Mtep_ESEC_22)
  next

  For %subsec 2301 2302 2303 2304 2305 2306 2307 2308     
    {%modelname}.append  Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_23+Q_Mtep_SEC_23+Q_Mtep_ESEC_23)
  next

  For %subsec 2401 2402 2403 2404 2405 2406     
    {%modelname}.append Q_Mtep_{%subsec}=phi_Mtep_{%subsec}*(Q_Mtep_H_24+Q_Mtep_SEC_24+Q_Mtep_ESEC_24)
  next
  

endif



For %coh M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65
    if @elem(empl_{%coh},%baseyear) <> 0 then
          {%modelname}.append d(log(empl_{%coh}))=d(log(L)) 
    endif
next

'equation 7.3
For %coh M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65
    {%modelname}.append LF_{%coh}=PARTR_{%coh}*WAPop_{%coh}
next

'equation 7.4
For %coh M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65 
    {%modelname}.append d(PARTR_n_{%coh})=d(PARTR_trend_{%coh}) + betaEmp_{%coh}*d(UnR_{%coh})   
next
'equation 7.5
For %coh  M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65  
    {%modelname}.append Un_{%coh}=LF_{%coh}- Empl_{%coh}
next



'equation 7.6-7.8
For %coh M15 M20 M25 M55 M60 M65 W15 W20 W25 W55 W60 W65 M W 15 20 25 55 60 65
    {%modelname}.append UnR_{%coh}=Un_{%coh}/LF_{%coh}
next

'equation 7.9
{%modelname}.append UnR_TOT=Un_TOT/LF_TOT

'equation 7.10
For %age 15 20 25 55 60 65
{%modelname}.append UN_{%age}= UN_M{%age}+UN_W{%age}
next

'equation 7.11
For %sex W M
%equation = "UN_"+%sex+"=0"
        For %age 15 20 25 55 60 65         
              %equation = %equation +"+ UN_"+%sex+%age
        next
   {%modelname}.append {%equation}
next

'equation 7.12
%equation = "UN_TOT=0"
  For %sex W M         
    %equation = %equation +"+ UN_"+%sex
  next
{%modelname}.append {%equation}

'equation 7.13
For %age 15 20 25 55 60 65
{%modelname}.append LF_{%age}= LF_M{%age}+LF_W{%age}
next

'equation 7.14
For %sex W M
%equation = "LF_"+%sex+"=0"
        For %age 15 20 25 55 60 65         
              %equation = %equation +"+ LF_"+%sex+%age
        next
   {%modelname}.append {%equation}
next

'equation 7.15
%equation = "LF_TOT=0"
  For %sex W M         
    %equation = %equation +"+ LF_"+%sex
  next
{%modelname}.append {%equation}

   
   