' ***********************************************************************************************************************
' ******************************************* HOUSEHOLDS HYBRID****************************************************************************
' ***********************************************************************************************************************8


subroutine load_calibration_hybrid

  

  matrix(10,6) ES_BUILNRJ                         ' Matrix of elasticity of substitution ( between type of energy for building)
  ES_BUILNRJ.read(B4,s=ELAS_Hybrid_BUILNRJ) {%data_calibration}

  matrix(4,4) MTEP_SHARE                     ' Matrix of final energy consumption share between households and sectors, and within households, by energy type
  MTEP_SHARE.read(C39,s=Donnees_energie) {%data_calibration}

  matrix(20,4) MTEP_SECTOR_SHARE                     ' Matrix of final energy consumption by sector and energy carrier
  MTEP_SECTOR_SHARE.read(C68,s=Donnees_energie) {%data_calibration}

endsub