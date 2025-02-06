' Loads the data defining the shock applied in the currently run scenario
subroutine load_data_shocks(string %data_shocks)
  smpl @all
  vector(1) vectnb                    ' Create a vector With 1 roW
  vectnb.read(a1,s=series) {%data_shocks} 1       ' Load the nuMber of series in the vector
  !seriesnb=vectnb(1)                 ' Load the nuMber of series in a paraMeter

  ' Load the historical data froM Excel Inputs : Cell_nuMber; ForMat omitted(t=xls); s = Sheet_naMe; "t" : transpose (read in roW); File_naMe; nuMber of series
  read(c2,s=series,t) {%data_shocks} !seriesnb

  'matrix(37,37) TECH_COEF_VAR_AMS                         Matrix of the TECHNICAL COEFFICIENT VARIATION
  'TECH_COEF_VAR_AMS.read(B3,s=technical_coef_var) {%data_shocks}
  
  'matrix(37,37) TECH_COEF_VAR_AMS2                         Matrix of the TECHNICAL COEFFICIENT VARIATION  in choc scenario
  'TECH_COEF_VAR_AMS2.read(B3,s=technical_coef_var_2) {%data_shocks}

endsub


' Loads the data defining the shock applied in the currently run scenario
subroutine load_xl(string %file, string %sheet)
  smpl @all
  vector(1) vectnb                    ' Create a vector With 1 roW
  vectnb.read(a1,s={%sheet}) .\..\..\data\shocks\{%file}.xls 1       ' Load the nuMber of series in the vector
  !seriesnb=vectnb(1)                 ' Load the nuMber of series in a paraMeter

  ' Load the historical data froM Excel Inputs : Cell_nuMber; ForMat omitted(t=xls); s = Sheet_naMe; "t" : transpose (read in roW); File_naMe; nuMber of series
  read(c2,s={%sheet},t) .\..\..\data\shocks\{%file}.xls !seriesnb
endsub
