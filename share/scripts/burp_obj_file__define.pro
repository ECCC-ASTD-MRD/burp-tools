
Function Burp_Obj_File::GetProperty, FILE=file,Unit=unit,NRPTS=nrpts,DIM=dim,ALL=all

;;
   IF (KEYWORD_SET(file))    THEN RETURN, self.file
   IF (KEYWORD_SET(unit))    THEN RETURN, self.unit
   IF (KEYWORD_SET(nrpts))    THEN RETURN, self.nrpts
   IF (KEYWORD_SET(DIM))    THEN RETURN, self.dim


struct = {$
           nrpts   : self.nrpts, $
           unit   : self.unit, $
           file   : self.file $

     }





RETURN, struct

END ;--------------------------------------------------------------------


FUNCTION Burp_Obj_File::SetProperty, $
        file              = file, $
        unit              = unit, $
        OPTNOM_REAL       = optnom_real,$
        VALUE_OPTNOM_REAL = value_optnom_real,$ 
        OPTNOM_CHAR       = optnom_Char,$
        VALUE_OPTNOM_CHAR = value_optnom_char,$ 
        _Extra   = extra

common path_to_library

       IF N_ELEMENTS(OPTNOM_REAL) GT 0 THEN BEGIN
           optnom_real = STRING(optnom_real)
          IF N_ELEMENTS(VALUE_OPTNOM_REAL) GT 0 THEN $
                   value_optnom_real = float(value_optnom_real) ELSE return,-1
       ENDIF

       IF N_ELEMENTS(OPTNOM_CHAR) GT 0 THEN  BEGIN 
            optnom_char = STRING(optnom_char)
          IF N_ELEMENTS(VALUE_OPTNOM_CHAR) GT 0 THEN $
                    value_optnom_char = STRING(value_optnom_char) ELSE return,-1
       ENDIF


       IF (KEYWORD_SET(file)) THEN begin
           IF self.unit GT 0 then begin
              print,'closing file ', self.file
              status=MRFCLS(unit=self.unit)
              FREE_LUN,self.unit
           ENDIF
         file=file
;         print,file
         command ='r.filetype '+ file
         spawn,command,type_fichier
         type_fichier= STRUPCASE(type_fichier)
;         help,type_fichier
;         print,type_fichier
         error = STRPOS(type_fichier, 'BURP')
;         help,error
;         print,error
         position = where(error GT -1,count)
;         print,position,count
;         help,count
         if count EQ 0 then begin
            print,file,' is not a burp file'
            return,-1
         endif

;         if file_type(file=file) NE 3L then begin
;            print,file,' is not a burp file"
;            return,-1
;         endif

       IF NOT(KEYWORD_SET(unit)) THEN begin
        GET_LUN,unit
       ENDIF 

        nombre=MRFOPN(UNIT=unit, FILE=file,/READ)
        self.unit = unit
        self.file = file 
        self.nrpts = nombre 
        dim = MRFMXL(UNIT=unit) +100
        self.dim = long(dim)
         self.handle = -1L

      ENDIF
 
 
       IF N_ELEMENTS(OPTNOM_REAL) GT 0 THEN BEGIN
           a  = mrfopr(OPTNOM=OPTNOM_REAL,opvalr=value_optnom_real)
       ENDIF 
 
       IF N_ELEMENTS(OPTNOM_CHAR) GT 0 THEN  BEGIN 
           b  = mrfopc(OPTNOM=OPTNOM_CHAR,OPVALC= value_optnom_char)
       ENDIF

      IF ((N_ELEMENTS(OPTNOM_REAL) GT 0 ) OR (N_ELEMENTS(OPTNOM_CHAR) GT 0) OR $
          (N_ELEMENTS(FILE) GT 0 )) THEN return,1 ELSE return,-1
           


RETURN, 1

END ;--------------------------------------------------------------------


Function Burp_Obj_File::Find_Report, $         ; The name of the method.

           REPORT   = report,$
           handle   = handle, $
           stnid    = stnid, $              ;
           idtyp    = idtyp, $             ;
           lat      = lat, $              ;
           lon      = lon, $              ;
           date     = date,$
           heure    = heure,$
           sup      = sup,$
           nsup     = nsup,$


          _Extra =  extra

common path_to_library

  IF NOT(KEYWORD_SET(report)) THEN begin
      print,'report object required as argument to Find_report...'
      return,-1
  ENDIF ELSE  BEGIN
      IF (SIZE(report,/TYPE) EQ 11) THEN BEGIN
          IF obj_class(report) EQ 'BURP_OBJ_RPT' THEN BEGIN 
             report =report
          ENDIF ELSE BEGIN
             print,'report object required as argument to Find_report...'
             return,-1
          ENDELSE
            
      ENDIF ELSE BEGIN
          print,'object reference required to find_report'
          return,-1
      ENDELSE
     
  ENDELSE


  IF (N_ELEMENTS(handle) eq 0) THEN begin
     handle=0L
  ENDIF ELSE begin
     handle=handle
  ENDELSE
  IF (N_ELEMENTS(stnid) eq 0) THEN begin
     stnid='*********'
  ENDIF ELSE begin
     stnid=stnid
  ENDELSE
  IF (N_ELEMENTS(idtyp) eq 0) THEN BEGIN
     idtyp=-1L
  ENDIF ELSE begin
     idtyp=idtyp
  ENDELSE
  IF (N_ELEMENTS(lat) eq 0) THEN begin
     lat=-1L
  ENDIF ELSE begin
     lat=lat
  ENDELSE
  IF (N_ELEMENTS(lon) eq 0) THEN begin
    lon=-1L
  ENDIF ELSE begin
    lon=lon
  ENDELSE
  IF (N_ELEMENTS(date) eq 0) THEN begin
    date=-1L
  ENDIF ELSE begin
    date=date
  ENDELSE
  IF (N_ELEMENTS(heure) eq 0) THEN begin
    heure=-1L
  ENDIF ELSE begin
    heure=heure
  ENDELSE
  IF (N_ELEMENTS(sup) eq 0) THEN begin
    sup=-1L
  ENDIF ELSE begin
    sup=sup
  ENDELSE
  IF (N_ELEMENTS(nsup) eq 0) THEN begin
    nsup=0L
  ENDIF ELSE begin
    nsup=nsup
  ENDELSE

  handle=long(handle)
  stnid =string(stnid)
  idtyp =long(idtyp)
  lat   =long(lat)
  lon   =long(lon)
  date  =long(date)
  heure =long(heure)
  sup   =long(sup)
  nsup  =long(nsup)

; pour les besoins de la methode next

           self.last_handle=handle
           self.last_stnid=stnid
           self.last_idtyp=idtyp
           self.last_lat=lat
           self.last_lon=lon
           self.last_date=date
           self.last_heure=heure
           self.last_sup=sup
           self.last_nsup=0L


;      ID_Rpt=MRFLOC(UNIT=self.unit,handle=handle,idtyp=idtyp)

       ID_Rpt=CALL_EXTERNAL(libso,'i_idl_mrfloc',$
             self.unit,handle,stnid,idtyp,lat,lon,date,heure,sup,nsup)

      IF ID_Rpt GT 0 THEN BEGIN
         self.handle=ID_Rpt
         self.last_handle=Id_Rpt
         err = report->Get_Report(FILE=self.file,UNIT=self.unit,DIM=self.dim,$
                                 HANDLE= self.handle) 
      ENDIF

RETURN, ID_Rpt
END ;--------------------------------------------------------------------

Function Burp_Obj_File::Next_Report, $         ; The name of the method.

           REPORT   = report,$

          _Extra =  extra

common path_to_library

  IF NOT(KEYWORD_SET(report)) THEN begin
      print,'report object required as argument to Find_report...'
      return,-1
  ENDIF ELSE  BEGIN
      IF (SIZE(report,/TYPE) EQ 11) THEN BEGIN
          IF obj_class(report) EQ 'BURP_OBJ_RPT' THEN BEGIN
             report =report
          ENDIF ELSE BEGIN
             print,'report object required as argument to Find_report...'
             return,-1
          ENDELSE

      ENDIF ELSE BEGIN
          print,'object reference required to find_report'
          return,-1
      ENDELSE

  ENDELSE

       ID_Rpt=CALL_EXTERNAL(libso,'i_idl_mrfloc',$
             self.unit,self.last_handle,self.last_stnid,$
             self.last_idtyp,self.last_lat,self.last_lon,self.last_date,$
             self.last_heure,self.last_sup,self.last_nsup)

      IF ID_Rpt GT 0 THEN BEGIN
         self.handle=ID_Rpt
         self.last_handle=Id_Rpt
         err = report->Get_Report(FILE=self.file,UNIT=self.unit,DIM=self.dim,$
                                 HANDLE= self.handle)
      ENDIF
RETURN,ID_Rpt
END ;--------------------------------------------------------------------

FUNCTION Burp_Obj_File::Init,File=file,unit=unit

       IF NOT(KEYWORD_SET(file)) THEN begin
         file=""
         self.unit = -1 
         Return, 1
       ENDIF ELSE begin
;         print,file
         command ='r.filetype '+ file
         spawn,command,type_fichier
         type_fichier= STRUPCASE(type_fichier)
;         help,type_fichier
;         print,type_fichier
         error = STRPOS(type_fichier, 'BURP')
;         help,error
;         print,error
         position = where(error GT -1,count)
;         print,position,count
;         help,count
         if count EQ 0 then begin
;            print,'your object is created, but the file is "
;            print,'not of type burp. use the object, SetProperty(FILE=yours)"
;            print,'to set the file"
            return,-1
         endif
;         if file_type(file=file) NE 3L then begin
;            print,'your object is created, but the file is "
;            print,'not of type burp. use the object, SetProperty(FILE=yours)"
;            print,'to set the file"
;            return,1
;         endif
         file=file
       ENDELSE

       IF NOT(KEYWORD_SET(unit)) THEN begin
          GET_LUN,unit
       ENDIF
        nombre=MRFOPN(UNIT=unit, FILE=file,/READ)
        self.unit = unit
        self.file = file 
        self.nrpts = nombre 
        dim = MRFMXL(UNIT=unit) +100
        self.dim = long(dim)
        self.handle = -1L

RETURN, 1

END ;--------------------------------------------------------------------

PRO Burp_Obj_File::Cleanup         ; The name of the method.



           IF self.unit GT 0 then begin
              status=MRFCLS(unit=self.unit)
              FREE_LUN,self.unit
           ENDIF

END ;--------------------------------------------------------------------



PRO Burp_Obj_File__Define

; The definition of the Obj_File_Class object class.

struct = { BURP_OBJ_FILE, $            ;
           file   :'', $
           unit   : -1L, $              ; 
           nrpts  : 0L, $              ; 
           handle : -1L, $              ; 
           dim    : 0L, $              ; 

           last_handle:0L,$
           last_stnid:'*********',$
           last_idtyp:-1L,$
           last_lat:-1L,$
           last_lon:-1L,$
           last_date:-1L,$
           last_heure:-1L,$
           last_sup:-1L,$
           last_nsup:0L,$

           extra: Ptr_New() $         ; 
          }

END ;--------------------------------------------------------------------

