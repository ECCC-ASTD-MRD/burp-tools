pro idl_burp_obj, HELP_REPORT=help_report,HELP_BLOCK=help_block,$
                  HELP_FILE=help_file, EXAMPLE=example

IF (KEYWORD_SET(HELP_FILE)) THEN BEGIN

data=[ $
"                                                                             ",$
"                                                                             ",$
" *************************************************************************** ",$
"                                                                             ",$
" ;; instanciate file object from burp_obj_file Class                         ",$
"                                                                             ",$
"    MyFile  = OBJ_NEW('burp_obj_file'[,FILE=your_file] [,UNIT=unit])         ",$
"                                                                             ",$
"    IF file is not set when object is created, use the method                ",$
"    SetProperty to set the property FILE of the file object,                 ",$ 
"    to the file to be scanned                                                ",$
"                                                                             ",$
"    err       = MyFile->SetProperty(FILE = your_file_name[,UNIT=unit])       ",$
"                                                                             ",$
"   err = -1 if error                                                         ",$
"                                                                             ",$
"   you can access File object parameters this way:                           ",$
"   1)                                                                        ",$
"   params_structure = MyFile->GetProperty(/ALL)                              ",$
"   params_structure tags are:FILE,UNIT,NRPTS,                                ",$
"   FILE : file opened                                                        ",$
"   unit  : unit associated with file                                         ",$
"   nrpts : number of reports in the file                                     ",$
"   ..                                                                        ",$
"                                                                             ",$
"   2)  no need to get the whole structure, each parameter                    ",$
"      is returned by GetProperty method with  keywords                       ",$
"                                                                             ",$
"      unit    = MyFile->GetProperty(/UNIT)                                   ",$
"      nrpts   = MyFile->GetProperty(/NRPTS)                                  ",$
"      file    = MyFile->GetProperty(/FILE)                                   ",$
"                                                                             ",$
"   Searching for a report in the file :                                      ",$
"   ----------------------------------                                        ",$
"   Use File object Find_Report Method to find a report that matches the      ",$
"    STNID,IDTYP,LAT,LON,DATE,HEURE parameters                                ",$
"                                                                             ",$
" ;; scan your file_name to find the report pointed by ref_report             ",$
" ;; starting the search from  ref_start. ref_start eq 0 means from           ",$
" ;; the beginning of the file                                                ",$
"                                                                             ",$
"   ref_report = MyFile->Find_Report(REPORT=Report_object,HANDLE=ref_start,$  ",$
"                STNID=your_station,$                                         ",$
"                IDTYP=your_idtyp, LAT=your_latitude, LON=your_longitude,$    ",$
"                DATE=your_date, HEURE=your_hour)                             ",$
"                                                                             ",$
"   ref_report = -1 if nothing is found and ref_report GT 0 otherwise         ",$
"                                                                             ",$
"  REPORT argument required and it' s a report object                         ",$
"                                                                             ",$
"   If ref_report GT 0 then  the report_object argument will be filled with   ",$
"   report header and report data                                             ",$
"                                                                             ",$
"   to access report's parameters see IDL_BURP_OBJ,/HELP_REPORT for datails   ",$
"                                                                             ",$
"                                                                              "]

print,data

ENDIF

IF (KEYWORD_SET(HELP_REPORT)) THEN BEGIN

data1 = [ $
"                                                                             ",$
"                                                                             ",$
" *************************************************************************** ",$
"                                                                             ",$
" ;; instanciate report object from burp_obj_rpt Class                        ",$
"                                                                             ",$
"    MyReport  = OBJ_NEW('burp_obj_rpt')                                      ",$
"                                                                             ",$
"  the report object is used with File  object                                ",$
"  type BURP_OBJ_INTERFACE,/HELP_FILE  for more details                       ",$
"                                                                             ",$
"   You can access report's parameters this way:                              ",$
"                                                                             ",$
"   1)                                                                        ",$
"   params_structure = MyReport->GetProperty(/ALL)                            ",$
"   params_structure tags are:TEMPS,FLGS,STNID,IDTYPE,LAT,LON, DX,DY,ELEV,    ",$
"                             DRND,RUNN,DATE,OARS,NBLK, UNIT, FILE,ID_RPT     ",$
"   STNID : station                                                           ",$
"   DATE  : date                                                              ",$
"   TEMPS : Hour                                                              ",$
"   ..                                                                        ",$
"   ID_RPT: pointer to the report, same as ref_report                         ",$
"                                                                             ",$
"   2)  no need to get the whole structure, each parameter                    ",$
"      is returned by GetProperty method with  keywords                       ",$
"                                                                             ",$
"      temps   = MyReport->GetProperty(/TEMPS)                                ",$
"      flags   = MyReport->GetProperty(/FLGS)                                 ",$
"      station = MyReport->GetProperty(/STNID                                 ",$
"      codetype= MyReport->GetProperty(/IDTYPE)                               ",$
"      runn     MyReport->GetProperty(/RUNN)                                  ",$
"      --                                                                     ",$ 
"      nblk    = MyReport->GetProperty(/NBLK)                                 ",$
"      date    = MyReport->GetProperty(/DATE)                                 ",$
"      file    = MyReport->GetProperty(/FILE)                                 ",$
"                                                                             ",$
"                                                                             ",$
"    SEARCHING the report for a block that mathes  BFAM,BTYP,BDESC            ",$
"    ------------------------------------------------------------             ",$
"                                                                             ",$
" ;; scan your_report to find the block pointed by ref_block                  ",$
" ;; starting the search from  ref_start. ref_start eq 0 means from           ",$
" ;; the beginning of the report                                              ",$
"                                                                             ",$
"   ref_block = MyReport->Find_BLock(BLOCK=obj_Block,BLK=ref_start,$          ",$
"                BFAM=your_bfam, BDESC=your_bdesc, BTYP=your_btyp,$           ",$
"                                                                             ",$
"   BLOCK argument is required and it's a block obejct!                       ",$
"                                                                             ",$
"   ref_block = -1 if nothing is found and ref_block GT 0 otherwise           ",$
"                                                                             ",$
"   if ref_block GT 0 you can access blocks' parameters  and data             ",$
"    TYPE IDL_BURP_OBJ,/HELP_BLOCK for more details                           ",$
"                                                                             ",$
"                                                                              "]
     

data2 = [ $
"                                                                            ",$
"    To  translate report runn parameter from binary to text                 ",$
"    MyReport->Runn_Type(number) is useful:       i                          ",$
"                                                                            ",$
"       number must be one of these values 0, 1,2,3 or blank                 ",$
"                                                                            ",$
"   MyReport->Runn_Type(0) returns a STRING:                                 ",$
"                                                                            ",$
"   STRING       :      ?                                                    ",$
"   -----------------------------------------------------------------------  ",$
"                                                                            ",$
"   REGUL        : passe reguliere                                           ",$
"   PARAL        : passe parallele                                           ",$
"   RESERVED     : en reserve                                                ",$
"                                                                            ",$
"   MyReport->Runn_Type(1) returns a STRING:                                 ",$
"   STRING       :      ?                                                    ",$
"   -----------------------------------------------------------------------  ",$
"                                                                            ",$
"   GLO          : passe globale                                             ",$
"   REG          : passe regionale                                           ",$
"   RESERVED     :                                                           ",$
"                                                                            ",$
"   MyReport->Runn_Type(2) returns a STRING:                                 ",$
"   STRING       :      ?                                                    ",$
"   -----------------------------------------------------------------------  ",$
"                                                                            ",$
"   AO_ALT_PRELI : AO en altitude preliminaire                               ",$
"   AO_ALT_COMPL : AO en altitude complete                                   ",$
"   AO_ALT_FINAL : AO en altitude finale                                     ",$
"   AO_SFC_SPECI : AO en surface speciale                                    ",$
"   AO_SFC_PRELI : AO en surface preliminaire                                ",$
"   AO_SFC_COMPL : AO en surface complete                                    ",$
"   AO_SFC_FINAL : AO en surface finale                                      ",$
"   RESERVED     :                                                           ",$
"                                                                            ",$
"                                                                             "]
data3 = [ $
"                                                                            ",$
"   MyReport->Block_Type(3) returns a STRING:                                ",$
"   STRING       :      ?                                                    ",$
"   -----------------------------------------------------------------------  ",$
"                                                                            ",$
"                                                                            ",$
"   00Z          :                                                           ",$
"   03Z          :                                                           ",$
"   06Z          :                                                           ",$
"   09Z          :                                                           ",$
"   12Z          :                                                           ",$
"   15Z          :                                                           ",$
"   18Z          :                                                           ",$
"   21Z          :                                                           ",$
"   RESERVED     :                                                           ",$
"                                                                            ",$
"   MyReport->Report_Type() returns a STRING:                                ",$
"                                                                            ",$
"   MyReport->Report_Type(0) + MyReport->Report_Type(1) + $                  ",$
"   MyReport->Report_Type(2) + MyReport->Report_Type(3)                      ",$
"                                                                            ",$
"   example: to check if the runn is of type GOLBAL:                         ",$
"         IF MyReport->Report_Type(1) EQ 'GLO' THEN do something             ",$
"   example: to check if the runn is of type PARALLEL:                       ",$
"         IF MyReport->Report_Type(0) EQ 'PARAL' THEN do something           ",$
"                                                                            ",$
"   note:don't forget destroying objects, at the end of your progs           ",$
"                                                                            ",$
"                                                                            ",$
"                                               CMDA August 2001             ",$
"                                                                            ",$
" ************************************************************************** ",$
"                                                                                 "]

print,data1
print,'Press any key to continue.......'
 A = GET_KBRD(1)

print,data2
print,'Press any key to continue.......'
 A = GET_KBRD(1)
print,data3
ENDIF


IF (KEYWORD_SET(HELP_BLOCK)) THEN BEGIN

data1 = [ $
"                                                                            ",$
" ************************************************************************** ",$
"                                                                            ",$
"                                                                            ",$
" ;; instanciate block object from burp_obj_block Class                      ",$
"                                                                            ",$
"    MyBlock  = OBJ_NEW('burp_obj_block')                                    ",$
"                                                                            ",$
"  the block object is used with report object                               ",$
"  see BURP_OBJ_INTERFACE,/HELP_REPORT  for more detals                      ",$
"                                                                            ",$
"                                                                            ",$
"   You can access blocks' parameters this way:                              ",$
"                                                                            ",$
"   1)                                                                       ",$
"   params_structure = MyBlock->GetProperty(/ALL)                            ",$
"   params_structure tags are:NELEM, NVAL, NT, BFAM, BDESC, BTYP, NBIT,      ",$
"                             BIT0, ID_BLOK, DATYP                           ",$
"   NELEM  :  number of elements                     i                       ",$
"   NVAL  :  number of values                                                ",$
"   ..                                                                       ",$
"   ID_BLK: pointer to the block, eq ref_block                               ",$
"                                                                            ",$
"   2)  no need to get the whole structure, each parameter                   ",$
"      is returned by GetProperty method with  keywords                      ",$
"                                                                            ",$
"      nelem      = MyBlock->GetProperty(/NELEM)                             ",$
"      nval      = MyBlock->GetProperty(/NVAL)                               ",$
"      nt        = MyBlock->GetProperty(/NT)                                 ",$
"      bfam      = MyBlock->GetProperty(/BFAM)                               ",$
"      --                                                                    ",$
"      bdesc     = MyBlock->GetProperty(/BDESC)                              ",$
"      btyp      = MyBlock->GetProperty(/BTYP)                               ",$
"      datyp     = MyBlock->GetProperty(/DATYP)                              ",$
"                                                                            ",$
"                                                                            ",$
"      use the functons below, to get the elememts and values                ",$
"                                                                            ",$
"      dliste   = MyBlock->Get_Elements_List(/CONV)                          ",$
"            dliste   is an LONG array of dimension [nelem]                  ",$
"                                                                            ",$
"      tblval   = MyBlock->Get_Elements_Values(/CONV)                        ",$
"            tblval   is an LONG or FLOAT array of dimension [nelem,nval,nt] ",$
"                                                                            ",$
"      in dliste, CONV  to have elements converted to decimal buffer format   ",$
"      in tblval, CONV  to have long integers values converted to real values ",$
"      if the block is of the type marquor, tbval remains of type long        ",$
"                                                                             ",$
"                                                                              "]
data2 = [ $
"                                                                            ",$
"    To  translate block btyp from binary to text                            ",$
"    MyBlock->Block_Type(number) is useful:                                  ",$
"                                                                            ",$
"       number must be one of these values 0, 1,2,3 or blank                 ",$
"                                                                            ",$
"   MyBlock->Block_Type(0) returns a STRING:                                 ",$
"                                                                            ",$
"   STRING       :      ?                                                    ",$
"   ------------------------------------------------------------------------ ",$
"                                                                            ",$
"   UNI          : uniniveau                                                 ",$
"   MULTI        : mutltiniveaux                                             ",$
"   RESERVED     : en reserve                                                ",$
"                                                                            ",$
"   MyBlock->Block_Type(1) returns a STRING:                                 ",$
"   STRING       :      ?                                                    ",$
"   ------------------------------------------------------------------------ ",$
"                                                                            ",$
"   DATA         : block donnees                                             ",$
"   INFO         : block info                                                ",$
"   3-D          : block 3-D                                                 ",$
"   MRQR         : block marqueur                                            ",$
"   RESERVED     :                                                           ",$
"                                                                            ",$
"   MyBlock->Block_Type(2) returns a STRING:                                 ",$
"   STRING       :      ?                                                    ",$
"   ------------------------------------------------------------------------ ",$
"                                                                            ",$
"   SFC          : surface                                                   ",$
"   ALT          : altitude                                                  ",$
"   RESERVED     :                                                           ",$
"                                                                            ",$
"                                                                            "]
data3 = [ $
"                                                                               ",$
"   MyBlock->Block_Type(3) returns a STRING:                                    ",$
"   STRING       :      ?                                                       ",$
"   -------------------------------------------------------------------------   ",$
"                                                                               ",$
"   OBS_ADE      : observations ADE                                             ",$
"   OBS_BRUTES   : observations brutes, non decodees                            ",$
"                                                                               ",$
"   DERI_ALT_GLO : donnees derivees, entree a l'AO en altitude, modele global   ",$
"   DERI_ALT_REG : donnees derivees, entree a l'AO en altitude, modele regional ",$
"   DERI_SFC_GLO : donnees derivees, entree a l'AO en surface, modele global    ",$
"   DERI_SFC_REG : donnees derivees, entree a l'AO en surface, modele regional  ",$
"                                                                               ",$
"   POST_ALT_GLO : donnees vues par l'AO en altitude,modele global              ",$
"   POST_ALT_REG : donnees vues par l'AO en altitude,modele regional            ",$
"   POST_SFC_GLO : donnees vues par l'AO en surface, modele global              ",$
"   POST_SFC_REG : donnees vues par l'AO en surface, modele regional            ",$
"                                                                               ",$
"   PRFL_ALT_GLO : profils verticaux, AO en altitude,modele global              ",$
"   PRFL_ALT_REG : profils verticaux, AO en altitude,modele regional            ",$
"                                                                               ",$
"   ANAL_ALT_GLO : val.analysees(incluant residus) par AO en altitude,mod global",$
"   ANAL_ALT_REG : val.analysees(incluant residus) par AO en altitude,mod regional",$
"   ANAL_SFC_GLO : val.analysees(incluant residus) par AO en surface,mod global ",$
"   ANAL_SFC_REG : val.analysees(incluant residus) par AO en surface,mod regional",$
"                                                                                ",$
"   PREV_GLO     : previsions, modele global                                     ",$
"   PREV_REG     : previsions, modele regional                                   ",$
"                                                                                ",$
"   STAT_PENSE   : statistiques des elements du temps (projet PENSE)             ",$
"   STAT_KALMAN  : statistiques des elements du temps (filtres de Kalman, PENSE) ",$
"                                                                                ",$
"   SSMI         : donnees SSMI                                                  ",$
"                                                                                ",$
"   RESERVED     :                                                               ",$
"                                                                                ",$
"   MyBlock->Block_Type() returns a STRING:                                      ",$
"                                                                                ",$
"   MyBlock->Block_Type(0) + MyBlock->Block_Type(1) + $                          ",$
"   MyBlock->Block_Type(2) + MyBlock->Block_Type(3)                              ",$
"                                                                                ",$
"   example: to check if the block is of type marquor:                           ",$
"         IF MyBlock->Block_Type(1) EQ 'MRQR' THEN do something                  ",$
"   to check if the block is UNI :                                               ",$
"         IF MyBlock->Block_Type(0) EQ 'UNI' THEN do something                   ",$
"   to check if the block has contains surface data  :                           ",$
"         IF MyBlock->Block_Type(2) EQ 'SFC' THEN do something                   ",$
"                                                                                ",$
" ****************************************************************************   ",$
"                                                                                 "]        
    
print,data1
print,'Press any key to continue.......'
 A = GET_KBRD(1)

print,data2
print,'Press any key to continue.......'
 A = GET_KBRD(1)
print,data3
ENDIF

IF (KEYWORD_SET(EXAMPLE)) THEN BEGIN

data = [ $
"                                                                             ",$
";*************************************************************************** ",$
" ;; there is an exmaple to scan the whole file                               ",$
"                                                                             ",$
"    MyFile    = Obj_New('burp_obj_file')                 ; file object       ",$
"    err       = MyFile->SetProperty(FILE=your_file_here)                     ",$
"                                                                             ",$
"    MyReport  = Obj_New('burp_obj_rpt')                  ; report object     ",$
"    MyBlock   = Obj_New('burp_obj_block')                ; block object      ",$
"                                                                             ",$
"    handle    = 0                                     ;beginning of the file ",$
"    handle    = MyFile->Find_Report(REPORT=MyReport,HANDLE=handle )          ",$
"    WHILE(handle  GT 0) DO BEGIN                                             ",$
"                                                                             ",$
"         params    = MyReport->GetProperty(/ALL)      ;all parameters        ",$
"         help, params,/structure                                             ",$
"         station   = MyReport->GetProperty(/STNID)    ;one parameter         ",$
"         help, station                                                       ",$
"                                                                             ",$
"         blk       = 0                                ;beginning of the report",$
"         blk       = MyReport->Find_Block(BLOCK=MyBlock,BLK=blk)            ",$
"         WHILE(blk GT 0) DO BEGIN                                            ",$
"             params    = MyBlock->GetProperty(/ALL)   ;all parameters        ",$
"             help, params,/structure                                         ",$
"             nelem     = MyBlock->GetProperty(/NELEM)  ;one parameter        ",$
"                                                                             ",$
"             dliste    = MyBlock->Get_Elements_List(/CONV)                   ",$
"             rval      = MyBlock->Get_Elements_values(/CONV)                 ",$
"                                                                             ",$
"         blk        = MyReport->Find_Block(BLOCK=MyBlock,BLK=blk)            ",$
"         ENDWHILE                                                            ",$
"                                                                             ",$
"    handle    = MyFile->Find_Report(REPORT=MyReport,HANDLE=handle )          ",$
"    ENDWHILE                                                                 ",$
"                                                                             ",$
"    obj_destroy,[MyReport,MyBlock,MyFile]                                    ",$
"                                                                             ",$
"                                               CMDA August 2001              ",$
"                                                                             ",$
"                                                                             ",$
";*************************************************************************** ",$
"                                                                             ",$
"                                                                              "]

print,data
ENDIF

end
 
