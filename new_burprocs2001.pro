;; NAME:
;;
;;      LONG_TIME_TO_STR_TIME
;;
;; PURPOSE:
;;      This IDL function returns a time converted from int or long to string
;;      argument time is array or simple variable
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      IDL>time_converted=LONG_TIME_TO_STR_TIME(time [,/BY_VALUE])
;;
;; KEYWORDS:
;;      HELP - If this keyword is set, print the calling sequence
;;      BY_VALUE - If this keyword is set, argument time is used by value
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written oct, 2001   H. Benhocine
;;
FUNCTION long_time_to_str_time, time, BY_VALUE=by_value,HELP=help

  if (KEYWORD_SET(help)) then begin
    print,''
    print,'PURPOSE:'
    print,'       This IDL function returns a time converted from int or long to string.'
    print,'       Argument time is an  array or simple variable'
    print,''
    print,'        USAGE :time_converted=LONG_TIME_TO_STR_TIME(time [,/BY_VALUE])'
    print,'        HELP - If this keyword is set, print the calling sequence'
    print,'        BY_VALUE - If this keyword is set, argument time is used by value'
    return,0
  endif

     r='0000'
     IF (KEYWORD_SET(by_value)) THEN BEGIN
         tempo= STRMID(r,0,4-strlen(string(strtrim(time,2)))) + string(strtrim(time,2))
         return,tempo
     ENDIF ELSE BEGIN
         time = STRMID(r,0,4-strlen(string(strtrim(time,2)))) + string(strtrim(time,2))
         RETURN,time
     ENDELSE


end ;_______________________________________________________________________________________

;; NAME: select_time_in_interval
;;

function select_time_in_interval,hour_array,min_array,time=time,count,signes=signes
;    hh=fix(STRMID(time,0,2))
    hh_mm = (fix(STRMID(time,0,2)))*60+fix(STRMID(time,2,2))
;    print,hh_mm


            h_range  = (hour_array[[0,2]])*60 + min_array
            in_range = -1L
;            help,h_range
;            print,h_range


  CASE 1 OF
        h_range[0] GT h_range[1]: BEGIN
            in_range_a = where((hh_mm GE  h_range[0]), count_a)
                if count_a  GT 0 then in_range = in_range_a
            in_range_b = where((hh_mm LT  h_range[1]), count_b)
                if count_b  GT 0 then begin
                    if in_range[0] NE -1 then begin
                        in_range = [in_range,in_range_b]
                    endif else begin
                        in_range = in_range_b
                    endelse
                endif
;                help,in_range
;                print,in_range


        END

        h_range[0] EQ h_range[1]: BEGIN
          IF ((signes[0] EQ 0 ) AND (signes[1] EQ -1))  THEN BEGIN
            in_range_a = where((hh_mm GE  h_range[0]), count_a)
                if count_a  GT 0 then in_range = in_range_a
            in_range_b = where((hh_mm LT  h_range[1]), count_b)
                if count_b  GT 0 then in_range = [in_range,in_range_b]

          ENDIF ELSE BEGIN
            in_range = where((hh_mm GE  h_range[0]) AND (hh_mm LE h_range[1]), in_count)
          ENDELSE

        END
        ELSE: BEGIN
            in_range = where((hh_mm GE  h_range[0]) AND (hh_mm LT h_range[1]), in_count)
        END
  ENDCASE


   IF in_range[0] NE -1 THEN BEGIN
      COUNT = N_ELEMENTS(in_range)
      RETURN,in_range
   ENDIF ELSE BEGIN
      count = 0
      return,-1
   ENDELSE
END;________________________________________________________________________________________

;; NAME:
;;
;;      HEURE_SYNOP
;;
;; PURPOSE:
;;      This IDL function returns an synoptic hour, given a burp file.
;;      the burp file must contain a RESUME report
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      IDL>hour=HEURE_SYNOP()
;;
;; KEYWORDS:
;;      HELP - If this keyword is set, print the calling sequence
;;      BURP_FILE - t give a burp file to search in,mandatroy!
;;      INT       - to return the hour with type int
;;      LONG      - to return the hour with type long, as recorded in a burp file
;;      STRING    - to return the hour with type string
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Oct, 2001   H. Benhocine
;;

function heure_synop,BURP_FILE=fichier,INT=int,LONG=long,STRING=string,trouve,HELP=help

  if (KEYWORD_SET(help)) then begin
    print,''
    print,' PURPOSE:'
    print,'        This IDL function returns a synoptic hour, given a burp file.'
    print,'        the burp file must contain a RESUME report.'
    print,''
    print,'      USAGE : heure_synop = HEURE_SYNOP(BURP_FILE=burp_file  [,/INT]$'
    print,'                         [,/LONG] [,/STRING],TROUVE  )'
    print,'      BURP_FILE - t give a burp file to search in,mandatory!'
    print,'      INT       - to return the hour with type int'
    print,'      LONG      - to return the hour with type long'
    print,'      STRING    - to return the hour with type string'
    print,'      TROUVE    - named variable, receive 0 if there is no resume report in the f ile'
    return,0
  endif

     MyReport  = Obj_New('burp_obj_rpt')
     MyFile    =  Obj_New('burp_obj_file')
     err = MyFile->SetProperty(FILE=fichier)
     if err LT 0 then begin
        obj_destroy,[MyReport,Myfile]
        trouve = 0
        return,-1
     endif


     handle = 0L
     handle = MyFile->Find_Report(REPORT=MyReport,HANDLE=handle,STNID=">>*******")
     if handle GT 0 then begin
        trouve =1
        time = MyReport->getproperty(/temps)
        obj_destroy,[MyReport,Myfile]
        r = LONG_TIME_TO_STR_TIME(time)
;help,r
        heure  = fix(STRMID(r,0,2))
        minute = fix(STRMID(r,2,2))

        IF (Keyword_set(string)) then return,r
        IF (Keyword_set(int)) then return,heure
        IF (Keyword_set(long)) then return,long(heure)

     endif else begin

       obj_destroy,[MyReport,Myfile]
       trouve = 0
       return = -1
     endelse



end ; ____________________________________________________________________________________

;; NAME:
;;
;;       time_range_select
;;
;; PURPOSE:
;;      This IDL function returns a longword vector tha contains the
;;      one-dimensional subscripts of elements of time array expression,
;;      contained in time range  around the synoptic hour.
;;
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      IDL>indices= time_range_select(TIME=time_expression,BURP_FILE = burp_file,$
;;                     SYNOP_HOUR=synop_hour, RANGE=range,COUNT)
;;
;; KEYWORDS:
;;      HELP - If this keyword is set, print the calling sequence
;;      SYNOP_HOUR - the synoptic hour
;;      BURP_FILE  - if set to a burp_file, search for resume report and
;;                   get synoptic hour. overrides the given SYNOP_HOUR
;;      TIME      - Time Expression
;;      RANGE     - [-(+)hhmm,-(+)hhmm]
;;
;; POSTIONAL ARGUMENNTS
;;       COUNT    - A named variable that will receive the number of selected
;;                  subscripts.
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Oct, 2001   H. Benhocine
;;

function time_range_select,BURP_FILE = burp_file,TIME=time, RANGE=range,$
                    SYNOP_HOUR=synop_hour, count,HELP=help,BY_VALUE=by_value

  if (KEYWORD_SET(help)) then begin
    print,''
    print,' PURPOSE:'
    print,'      This IDL function returns a longword vector that contains the'
    print,'      one-dimensional subscripts of elements of time array expression,'
    print,'      contained in time range  around the synoptic hour.'
    print,''
    print,'  USAGE : indices= time_range_select(TIME=time_expression,BURP_FILE = burp_file,$'
    print,'                        SYNOP_HOUR=synop_hour, RANGE=range,COUNT[,/BY_VALUE])'
    print,'  SYNOP_HOUR - the synoptic hour'
    print,'  BURP_FILE  - if set to a burp_file, search for resume report and'
    print,'              get synoptic hour. overrides the given SYNOP_HOUR'
    print,'  RANGE     - [-(+)hhmm,-(+)hhmm] '
    print,'  TIME      - Time Expression '
    print,'  COUNT     - named variable,that will receive the number of selected subscripts'
    print,'  BY_VALUE - If this keyword is set, argument time is used by value'
    return,0
  endif

_NB_HOURS_PER_DAY = 24
_NB_MINS_PER_DAY  = 24*60
_NB_MINS_PER_HOUR = 60

COUNT=0
   IF (KEYWORD_SET(BY_VALUE)) THEN by_value = 1 ELSE by_value = 0

     HEURE_SYNOP = 0
;;     IF (KEYWORD_SET(synop_hour)) THEN BEGIN
     IF (N_ELEMENTS(synop_hour) ne 0) THEN BEGIN
         HEURE_SYNOP= fix(synop_hour)
         IF NOT((HEURE_SYNOP GE 0) AND (HEURE_SYNOP LE 23)) THEN BEGIN
              MESSAGE,' Heure Synoptique uncorrecte 0 <= h =< 23 ',/INFORMATION
              RETURN,-1
         ENDIF
     ENDIF
     IF (KEYWORD_SET(BURP_FILE)) THEN BEGIN
        HEURE_SYNOP = heure_synop(BURP_FILE=burp_file, trouve,/INT)
        IF trouve EQ 0 THEN BEGIN
           PRINT,'pas enregistrement RESUME'
           OBJ_DESTROY,[MyReport,MyBlock,Myfile]
             COUNT = 0
             RETURN,-1
        ENDIF
     ENDIF

     print,'Heure Synoptique =',HEURE_SYNOP


     IF NOT(KEYWORD_SET(range)) THEN BEGIN
         print,'interval par defaut est  [-0300,+0300]'
         range = [-300,+300]
         delta = STRTRIM(STRING(range),2)
         signes= STRPOS(delta,'-')
     ENDIF ELSE BEGIN
         IF SIZE(range,/N_DIMENSIONS) NE 1 THEN BEGIN
         print,'interval est un vecteur de type [(-+)hhmm,(-+)hhmm]'
             count = 0
             return,-1
         ENDIF
         delta = STRTRIM(STRING(range),2)
;         help,delta
;         print,delta
         signes= STRPOS(delta,'-')
;         help,signes
;         print,signes
     ENDELSE


         time_inf = ABS(FIX(range[0]))
         time_sup = ABS(FIX(range[1]))

         IF (time_inf GT 9999) OR  (time_sup GT 9999) THEN BEGIN
             print,'interval est un vecteur de type [(-+)hhmm,(-+)hhmm]'
             count = 0
             return,-1
         ENDIF


         time_inf = LONG_TIME_TO_STR_TIME(time_inf)
         n_hour_inf   = fix(STRMID(time_inf,0,2))
         n_min_inf    = fix(STRMID(time_inf,2,2))

         time_sup = LONG_TIME_TO_STR_TIME(time_sup)
         n_hour_sup   = fix(STRMID(time_sup,0,2))
         n_min_sup    = fix(STRMID(time_sup,2,2))

                    IF  n_min_inf GT 59 THEN BEGIN
                        print," minutes <=59"
                        count = 0
                        return,-1
                    ENDIF
                    IF  n_min_sup GT 59 THEN BEGIN
                        print," minutes <=59"
                        count = 0
                        return,-1
                    ENDIF
CASE 1 OF
    ( (signes[0] EQ 0 ) AND (signes[1] EQ 0)) :BEGIN
                    print,'Negatif -Negatif'
                    IF n_hour_inf LT n_hour_sup THEN BEGIN
                       print,"si range= [-h0m0,-hm]"
                       print," h0 >= h"
                       count = 0
                       return,-1
                    ENDIF
                    IF n_hour_inf EQ n_hour_sup THEN BEGIN
                       IF n_min_inf LT n_min_sup THEN BEGIN
                          print,"si range= [-hm0,-hm]"
                          print," m0 >= m"
                          count = 0
                          return,-1
                       ENDIF
                    ENDIF
    n_hour_inf = n_hour_inf MOD 24
    n_hour_sup = n_hour_sup MOD 24

    heure_debut = HEURE_SYNOP[0]*_NB_MINS_PER_HOUR -$
                      ( n_hour_inf*_NB_MINS_PER_HOUR + n_min_inf)
    heure_fin = HEURE_SYNOP[0]*_NB_MINS_PER_HOUR -$
                      ( n_hour_sup*_NB_MINS_PER_HOUR + n_min_sup)
    IF heure_debut LT 0 THEN heure_debut = _NB_MINS_PER_DAY + heure_debut
       HOUR_D  = heure_debut/_NB_MINS_PER_HOUR
       MIN_D   = heure_debut MOD _NB_MINS_PER_HOUR

;      Help,HOUR_D,MIN_D

    IF heure_fin   LT 0 THEN heure_fin   = _NB_MINS_PER_DAY + heure_fin
       HOUR_F  = heure_fin/_NB_MINS_PER_HOUR
       MIN_F   = heure_fin MOD _NB_MINS_PER_HOUR
;      Help,HOUR_F,MIN_F




     END
     ((signes[0] EQ 0 ) AND (signes[1] EQ -1))  : BEGIN
                    print,'Negatif -Positif'
    n_hour_inf = n_hour_inf MOD 24
    n_hour_sup = n_hour_sup MOD 24

    heure_debut = HEURE_SYNOP[0]*_NB_MINS_PER_HOUR -$
                      ( n_hour_inf*_NB_MINS_PER_HOUR + n_min_inf)
    heure_fin = HEURE_SYNOP[0]*_NB_MINS_PER_HOUR +$
                      ( n_hour_sup*_NB_MINS_PER_HOUR + n_min_sup)
    IF heure_debut LT 0 THEN heure_debut = _NB_MINS_PER_DAY + heure_debut
       HOUR_D  = heure_debut/_NB_MINS_PER_HOUR
       MIN_D   = heure_debut MOD _NB_MINS_PER_HOUR

;      Help,HOUR_D,MIN_D

       HOUR_F  = heure_fin/_NB_MINS_PER_HOUR
       MIN_F   = heure_fin MOD _NB_MINS_PER_HOUR
;      Help,HOUR_F,MIN_F

     END
     ((signes[0] EQ -1 ) AND (signes[1] EQ -1)) : BEGIN
                    print,'Positif -Positif'

                    IF n_hour_inf GT n_hour_sup THEN BEGIN
                       print,"si range= [+h0m0,+hm]"
                       print," h0 <= h"
                       count = 0
                       return,-1
                    ENDIF
                    IF n_hour_inf EQ n_hour_sup THEN BEGIN
                       IF n_min_inf GT n_min_sup THEN BEGIN
                          print,"si range= [+hm0,+hm]"
                          print," m0 <= m"
                          count = 0
                          return,-1
                       ENDIF
                    ENDIF

    n_hour_inf = n_hour_inf MOD 24
    n_hour_sup = n_hour_sup MOD 24

    heure_debut = HEURE_SYNOP[0]*_NB_MINS_PER_HOUR  + $
                      ( n_hour_inf*_NB_MINS_PER_HOUR + n_min_inf)
    heure_fin = HEURE_SYNOP[0]*_NB_MINS_PER_HOUR +$
                      ( n_hour_sup*_NB_MINS_PER_HOUR + n_min_sup)
       HOUR_D  = heure_debut/_NB_MINS_PER_HOUR
       MIN_D   = heure_debut MOD _NB_MINS_PER_HOUR

;      Help,HOUR_D,MIN_D

       HOUR_F  = heure_fin/_NB_MINS_PER_HOUR
       MIN_F   = heure_fin MOD _NB_MINS_PER_HOUR
;      Help,HOUR_F,MIN_F

     END
     ELSE :  BEGIN
                    print,"Usage [-x,-y]"
                    print,"Usage [-x,+y]"
                    print,"Usage [+x,+y]"
                    count = 0
                    return,-1
     END
ENDCASE



       hour_array = [HOUR_D,HEURE_SYNOP[0],HOUR_F]
       min_array = [MIN_D,MIN_F]


           rpt_time =LONG_TIME_TO_STR_TIME(time,BY_VALUE=by_value)
;           help,rpt_time
indices=select_time_in_interval(hour_array,min_array,time=rpt_time,count_range,signes=signes)

; print,'temps entre',hour_array[[0,2]]
; print,'           ',min_array

 print,'temps entre',hour_array[0],': ',min_array[0]
 print,'et         ',hour_array[2],': ',min_array[1]

           if count_range GT 0 then begin
               count = count_range
               return,indices
           endif else begin
               count = 0
               return,-1
           endelse




end ; ____________________________________________________________________________________




;;
;; NAME:
;;
;;      RPT_PRM_STRUC
;;
;; PURPOSE:
;;      This IDL function returns an empty report parameters structure 
;;      This template  structure can than be used to call mrbhdr to
;;      get reort header parameters.
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      IDL>parms=rpt_prm_struc()
;;
;; KEYWORDS:
;;      HELP - If this keyword is set, print the calling sequence
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Feb, 2001   H. Benhocine
;;
FUNCTION rpt_prm_struc,HELP=help
  if (KEYWORD_SET(help)) then begin
    print,"USAGE: struc_name=rpt_prm_struc()"
    return,0
  endif
   p = {RPT_PRM, $
        temps: 0L,                      $
        flgs: 0L,                       $
        stnid: '*********',              $
        idtype: 0L,                     $
        lat: 0L,                        $
        lon: 0L,                        $
        dx: 0L,                         $
        dy: 0L,                         $
        elev: 0L,                       $
        drnd: 0L,                       $
        date: 0L,                       $
        oars:  0L,                      $
        runn: 0L,                       $
        nblk: 0L,                        $
        lngr: 0L$
       }
return, p
end;____________________________________________________________________________

;;
;; NAME:
;;
;;      BLK_PRM_STRUC
;;
;; PURPOSE:
;;      This IDL function returns an empty block parameters structure
;;      This template  structure can than be used to call mrbprm to
;;      get block descriptor parameters.
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      IDL>parms=blk_prm_struc()
;;
;; KEYWORDS:
;;      HELP - If this keyword is set, print the calling sequence
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Feb, 2001   H. Benhocine
;;
FUNCTION blk_prm_struc,HELP=help
  if (KEYWORD_SET(help)) then begin
    print,"USAGE: struc_name=blk_prm_struc()"
    return,0
  endif
   p = {BLK_PRM, $
        nele: 0L,                       $
        nval: 0L,                       $
        nt: 0L,                         $
        bfam: 0L,                       $
        bdesc: 0L,                      $
        btyp: 0L,                       $
        nbit: 0L,                       $
        bit0: 0L,                       $
        datyp: 0L                       $
       }
return, p
end;____________________________________________________________________________


;;
;; NAME:
;;
;; 	MRFOPN
;;
;; PURPOSE:
;;	This IDL procedure calls a CALL_EXTERNAL routine 
;;      called mrfopn  to:
;;      1) open a BURP file 
;;
;; CATEGORY:
;;	Dynamic Link
;;
;; CALLING SEQUENCE:
;;	This routine is called from IDL using the following commands:
;;      IDL>ier=mrfopn(unit=unit,filename=filename[,/READ,/DEBUG])
;;
;; INPUTS:
;;      unit   - unit number associated to the burp file
;;      file   - burp file name
;;
;; KEYWORDS:
;;      HELP  - If this keyword is set, print the calling sequence
;;      READ  - If set, the file will be opened for reading 
;;              this 1st version support only reading files, so READ will
;;              be set anyways 
;;      DEBUG - If this keyword is set, this routine will print debug informations
;;
;; ON RETURN:
;;     number of the active reports in the burp file 
;;
;; SIDE EFFECTS:
;;	None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;	Written Feb, 2001   H. Benhocine
;;
FUNCTION mrfopn,unit=unit,filename=filename,DEBUG=debug,READ=read,HELP=help
  common path_to_library, libso

  ier=0
  if (KEYWORD_SET(help)) then begin
    print,"USAGE: number_of_active_reports=MRFOPN(UNIT=nnn,FILE='some_file_name'[,/READ])"
    return,ier
  endif

  mode='RND'

  if (KEYWORD_SET(debug)) then begin
    HELP,unit,filename,mode
  endif	

  unit =long(unit)
  filename=string(filename)
  mode=string(mode)
  help,libso

  ier = CALL_EXTERNAL(libso,'i_idl_mrfopn',unit,filename,mode)

  return,ier
end;____________________________________________________________________________

FUNCTION mrfopr,OPTNOM=optnom,DEBUG=debug,opvalr=opvalr,HELP=help
  common path_to_library, libso

  ier=0
  if (KEYWORD_SET(help)) then begin
    print,"USAGE: stat=MRFOPR(OPTNOM=string_option,OPVALR=real_value)"
    print, 'example'
    data=[$
    " stat = mrfopr(OPTNOM='MISSING',OPVALR=-999.0)"]
    print,data 
    return,ier
  endif


  if (KEYWORD_SET(debug)) then begin
    HELP,optnom,opvalr
  endif


optnom = STRING(optnom)
opvalr  = float(opvalr)
  ier = CALL_EXTERNAL(libso,'i_idl_mrfopr',optnom,opvalr)

  return,ier
end;____________________________________________________________________________


FUNCTION mrfopc,OPTNOM=optnom,DEBUG=debug,opvalc=opvalc,HELP=help
  common path_to_library, libso

  ier=0
  if (KEYWORD_SET(help)) then begin
    print,"USAGE: stat=MRFOPC(OPTNOM=string_option,OPVALC=string_value)"
    print, 'example'
    data=[$
    " stat = mrfopc(OPTNOM='MSGLVL',OPVALC='FATAL')"]
    print,data
    return,ier
  endif


  if (KEYWORD_SET(debug)) then begin
    HELP,optnom,opvalc
  endif


optnom = STRING(optnom)
opvalc = STRING(opvalc)
  ier = CALL_EXTERNAL(libso,'i_idl_mrfopc',optnom,opvalc)

  return,ier
end;____________________________________________________________________________



;; NAME:
;;
;;      MRFCLS
;;
;; PURPOSE:
;;      This IDL procedure calls a CALL_EXTERNAL routine
;;      called mrfcls  to:
;;      1) close a burp file
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      This routine is called from IDL using the following commands:
;;      IDL>ier=mrfcls(unit=unit,/DEBUG])
;;
;; INPUTS:
;;      unit   - unit number associated to the burp file
;;
;; KEYWORDS:
;;      DEBUG - If this keyword is set, this routine will print debug informations
;;      HELP  - If this keyword is set, print the calling sequence
;;
;; ON RETURN:
;;      status of execution
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Feb, 2001   H. Benhocine
;;
FUNCTION mrfcls,unit=unit,DEBUG=debug,HELP=help
  common path_to_library
  ier=0
  if (KEYWORD_SET(help)) then begin
    print,"USAGE: status=MRFCLS(UNIT=nnn)"
    return,ier
  endif

;to be sure unit is of type long

  unit=long(unit)

  ier=CALL_EXTERNAL(libso,'i_idl_mrfcls',unit)

  if (KEYWORD_SET(debug)) then begin
     print,"mrfcls ier=",ier
     print,'mrfcls termine'
  endif

  return,ier
end;____________________________________________________________________________

;; NAME:
;;
;;      MRFMXL
;;
;; PURPOSE:
;;      This IDL procedure calls a CALL_EXTERNAL routine
;;      called mrfmxl  to:
;;      1) get the length of the longest report in the burp file 
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      This routine is called from IDL using the following commands:
;;      IDL>ier=mrfmxl(unit=unit,/DEBUG])
;;
;; INPUTS:
;;      unit   - unit number associated to the burp file
;;
;; KEYWORDS:
;;      DEBUG - If this keyword is set, this routine will print debug informations
;;      HELP  - If this keyword is set, print the calling sequence
;;
;; ON RETURN:
;;      status of execution
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Feb, 2001   H. Benhocine
;;
FUNCTION mrfmxl,unit=unit,DEBUG=debug,HELP=help
  common path_to_library
  ier=0
  if (KEYWORD_SET(help)) then begin
    print,"USAGE: status=MRFMXL(UNIT=nnn)"
    return,ier
  endif

;to be sure unit is of type long

  unit=long(unit)

  ier=CALL_EXTERNAL(libso,'i_idl_mrfmxl',unit)

  if (KEYWORD_SET(debug)) then begin
     print,"mrfmxl ier=",ier
     print,'mrfmxl termine'
  endif

  return,ier
end;____________________________________________________________________________

;; NAME:
;;
;; 	MRFLOC
;;
;; PURPOSE:
;;	This IDL procedure calls a CALL_EXTERNAL routine 
;;      called mrfloc  to:
;;      1) To locate the pointer(HANDLE) of the report that matches the STNID
;;       IDTYP, LATI, LONG, DATE, TEMPS parameters and the contents of array
;;      SUP.
;;      The search will start at the beginning if "handle" is equal to 0
;;      otherwise, the search will start on the report that follows
;;      the report pointed to by "handle". If an element of STNID is
;;      equal to an asterik  ('*'), this element will be considered like
;;      a "wildcard" and will be ignored during the search. It is the
;;      same for IDTYP, LATI, LONG, DATE, TEMPS and SUP if their
;;      values are -1. Note that only the "hour" portion of the argument
;;      TEMPS is used during the search.
;;      handle=c_mrfloc(iun,handle,stnid,idtyp,lati,long,date,temps,sup,nsup)
;;
;; CATEGORY:
;;	Dynamic Link
;;
;; CALLING SEQUENCE:
;;	This routine is called from IDL using the following commands:
;;      IDL>handle=MRFLOC(REF=record_handle), parameters..)
;;          see below..
;;
;; INPUTS:
;;      unit    - unit number associated to the burp file 
;;      handle  - record handle to start the search, if "handle" is equal to 0
;;                the search will start at the beginning 
;;      stind   - station identification
;;      idtyp   - type of the report 
;;      lat     - latitude 
;;      lon     - longitude
;;      date    - valid synoptic date
;;      heure   - hour of the observation
;;      sup     - array to contain supplementary primary keys 
;;      nsup    - dimension of the array sup 
;;
;; OUTPUTS:
;;      a structure with all the standard file parameters of fstprm
;;
;; KEYWORDS:
;;      DEBUG - If this keyword is set, this routine will print debug informations
;;      HELP  - If this keyword is set, print the calling sequence
;;
;; ON RETURN:
;;       handle of a burp file record
;;
;; SIDE EFFECTS:
;;	None.
;;
;; RESTRICTIONS:
;;      None    
;;
;; MODIFICATION HISTORY:
;;	Written feb, 2001  Hamid.Benhocine 
;;
FUNCTION mrfloc,UNIT=file_unit,HANDLE=i_handle,STNID=i_stind,IDTYP=i_idtyp,  LAT=i_lat, LON=i_lon, DATE=i_date, HEURE=i_heure,SUP=i_sup, NSUP=i_nsup,  HELP=help,DEBUG=debug
  common path_to_library
  ier=0

file_unit=long(file_unit)

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: handle=MRFLOC(UNIT=nnn[,HANDLE=handle_start] [,STNID='stind'] [,IDTYP=] [, LAT=] [,LON=][,DATE=] [,HEURE=] [, SUP=LonArr(dim), NSUP=dim ])"
        print, "USAGE: (UNSPECIFIED keywords will be set to defaults)"
    return,ier
  endif
;;  IF NOT(KEYWORD_SET(i_handle)) THEN begin
  IF (N_ELEMENTS(i_handle) eq 0) THEN begin
     handle=0L
  ENDIF ELSE begin
     handle=i_handle
  ENDELSE
;;  IF NOT(KEYWORD_SET(i_stind)) THEN begin
  IF (N_ELEMENTS(i_stind) eq 0) THEN begin
     stind='*********' 
  ENDIF ELSE begin
     stind=i_stind 
  ENDELSE
  IF (N_ELEMENTS(i_idtyp) eq 0) THEN BEGIN
     idtyp=-1L 
  ENDIF ELSE begin
     idtyp=i_idtyp 
  ENDELSE
  IF (N_ELEMENTS(i_lat) eq 0) THEN begin
     lat=-1L 
  ENDIF ELSE begin
     lat=i_lat 
  ENDELSE
  IF (N_ELEMENTS(i_lon) eq 0) THEN begin
    lon=-1L
  ENDIF ELSE begin
    lon=i_lon 
  ENDELSE
  IF (N_ELEMENTS(i_date) eq 0) THEN begin
    date=-1L
  ENDIF ELSE begin
    date=i_date
  ENDELSE
  IF (N_ELEMENTS(i_heure) eq 0) THEN begin
    heure=-1L
  ENDIF ELSE begin
    heure=i_heure
  ENDELSE
  IF (N_ELEMENTS(i_sup) eq 0) THEN begin
    sup=-1L
  ENDIF ELSE begin
    sup=i_sup
  ENDELSE
  IF (N_ELEMENTS(i_nsup) eq 0) THEN begin
    nsup=0L
  ENDIF ELSE begin
    nsup=i_nsup
  ENDELSE

  handle=long(handle)
  stind =string(stind)
  idtyp =long(idtyp)
  lat   =long(lat)
  lon   =long(lon)
  date  =long(date)
  heure =long(heure)
  sup   =long(sup)
  nsup  =long(nsup)

  if (KEYWORD_SET(debug)) then begin
    HELP,file_unit,handle,stind,idtyp,lat,lon,date,heure,sup,nsup
  endif

  record_handle=CALL_EXTERNAL(libso,'i_idl_mrfloc',$
     file_unit,handle,stind,idtyp,lat,lon,date,heure,sup,nsup)

  if (KEYWORD_SET(debug)) then begin
    help,record_handle
  endif
return, record_handle

end;____________________________________________________________________________

;; NAME:
;;
;;      MRBLOC
;;
;; PURPOSE:
;;      This IDL procedure calls a CALL_EXTERNAL routine
;;      called mrbloc  to:
;;      1) find the position of the first block of data of type
;;     "bfam,bdesc,btyp" which follows the block "blk0". We set "blk0 = 0"
;;      if we want to start in the first block. If the value of one of the
;;      keys (bfam,bdesc,btyp) is -1, then that key will not be used during
;;     the search.
;;     bkno=c_mrbloc(buf,bfam,bdesc,btyp,blk0)
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      This routine is called from IDL using the following commands:
;;      IDL>bloc_number=MRBLOC(BUF=buf,BLK=blk [,BFAM=] [,BDESC=] [,BTYP=])
;;
;; INPUTS:
;;      buf    - LongArray containing the report 
;;      blk    - bloc number from to start teh search, 0= beginning of the report
;;
;; OUTPUTS:
;;      a bloc number whitch satisfy the requirements 
;;
;; KEYWORDS:
;;      DEBUG - If this keyword is set, this routine will print debug informations
;;      HELP  - If this keyword is set, print the calling sequence
;;
;; ON RETURN:
;;       bloc number  
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None
;;
;; MODIFICATION HISTORY:
;;      Written feb, 2001  Hamid.Benhocine
;;
FUNCTION mrbloc,BUF=buf, BLK=i_blk ,BFAM=i_bfam,BDESC=i_bdesc, BTYP=i_btyp,HELP=help, DEBUG=debug
  common path_to_library
  ier=0

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: blok_number=MRBLOC(BUF=buf[,BLK=blok_start] [,BFAM=] [,BDESC=] [,BTYP=])" 
        print, "USAGE: (UNSPECIFIED keywords will be set to defaults)"
    return,ier
  endif
  IF (N_ELEMENTS(i_blk) eq 0) THEN begin
     blk0=0
  ENDIF ELSE begin
     blk0=i_blk
  ENDELSE
  IF (N_ELEMENTS(i_bfam) eq 0) THEN begin
     bfam=-1L
  ENDIF ELSE begin
     bfam=i_bfam
  ENDELSE
  IF (N_ELEMENTS(i_btyp) eq 0) THEN BEGIN
     btyp=-1L
  ENDIF ELSE begin
     btyp=i_btyp
  ENDELSE
  IF (N_ELEMENTS(i_bdesc) eq 0) THEN begin
     bdesc=-1L
  ENDIF ELSE begin
     bdesc=i_bdesc
  ENDELSE

  blk0  =long(blk0)
  bdesc =long(bdesc)
  btyp  =long(btyp)
  bfam  =long(bfam)

  if (KEYWORD_SET(debug)) then begin
   HELP, blk0, bdesc, btyp, bfam
  endif

  block_number=CALL_EXTERNAL(libso,'i_idl_mrbloc',$
     buf,bfam, bdesc, btyp, blk0)
  
   if (KEYWORD_SET(debug)) then begin
     HELP, block_number 
   endif
  return, block_number
end;____________________________________________________________________________

;;
;; NAME:
;;
;;      MRFGET
;;
;; PURPOSE:
;;      This IDL procedure calls a CALL_EXTERNAL routine
;;      called mrfget  to:
;;      1) get the contents of the report pointed by "handle"
;;         and put in the LONG ARRAY buf 
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      This routine is called from IDL using the following commands:
;;      IDL>ier=mrfget(HANDLE=handle, BUF=buf,[,/HELP,/DEBUG])
;;
;; INPUTS:
;;      handle   - report handle number 
;;      buf      - LONG ARRAY of size >1000 
;;
;; KEYWORDS:
;;      HELP  - If this keyword is set, print the calling sequence
;;      DEBUG - If this keyword is set, this routine will print debug informations;;
;; ON RETURN:
;;      status of execution 
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Feb, 2001   H. Benhocine
;;

FUNCTION mrfget,HANDLE=handle , BUF=buf, HELP=help,DEBUG=debug
  common path_to_library
  ier=0

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: status=MRFGET(HANDLE=handle_number , BUF=LonArr(size))"
    return,ier
  endif

    buf = long(buf)
    dim = N_Elements(buf)

    buf(0)=dim

  if (KEYWORD_SET(debug)) then begin
    dim = N_Elements(buf)
    HELP, dim, buf, handle
    buf=long(buf)
    HELP, dim, buf, handle
  endif

  ier = CALL_EXTERNAL(libso,'i_idl_mrfget',handle,buf)
end;____________________________________________________________________________


;;
;; NAME:
;;
;;      MRBHDR
;;
;; PURPOSE:
;;      This IDL procedure calls a CALL_EXTERNAL routine
;;      called mrbhdr  to:
;;      1) get the parameters from the header of a report 
;;         and put them in structure variable of type RPT_PRM 
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      This routine is called from IDL using the following commands:
;;      IDL>params=mrbhdr( BUFR=buf [,NSUP=,SUP=LonArr(NSUP)]
;;                                 [,NXAUX=,XAUX=LonArr(NXAUX)][,/HELP] [,/DEBUG])
;;
;; INPUTS:
;;      buf     - LONG ARRAY of size >1000 containing the report
;;      nsup    - OPTIONNAL, it is set to 0 in the function, indicating
;;                that array sup is not used. (future expansion) 
;;      sup     - OPTIONNAL,nsup size longarray used with nsup. 
;;      nxaux   - OPTIONNAL, it is set to 0 in the function, indicating
;;                that array xaux is not used. (future expansion) 
;;      xaux   - OPTIONNAL,nxaux size longarray used with nxaux. 
;;
;; KEYWORDS:
;;      HELP  - If this keyword is set, print the calling sequence
;;      DEBUG - If this keyword is set, this routine will print debug informations;;
;; ON RETURN:
;;      a structure containing each header parameter of a burp file report
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Feb, 2001   H. Benhocine
;;

FUNCTION mrbhdr,BUFR=buf,NSUP=nsup,SUP=sup,NXAUX=nxaux,XAUX=xaux,HELP=help,DEBUG=debug
  common path_to_library
  ier=0

  rpt_station=bytarr(9)

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: params=MRBHDR(BUFR=LonArr(size) [,NSUP=,SUP=LonArr(NSUP)],[,NXAUX=,XAUX=LonArr(NXAUX)])"
    return,ier
  endif
;
;Appel de la fonction rpt_prm_struc()
params=rpt_prm_struc()
buf(0)=N_Elements(buf)
  if (KEYWORD_SET(debug)) then begin
dim = N_Elements(buf)
HELP, dim, buf
print, buf(0)
HELP, params, /structure
  endif

; call to external routine

  ier = CALL_EXTERNAL(libso,'i_idl_mrbhdr',buf,params, rpt_station)
  params.stnid=string(rpt_station)
return, params
end;____________________________________________________________________________

;;
;; NAME:
;;
;;      MRFPRM
;;
;; PURPOSE:
;;      This IDL procedure calls a CALL_EXTERNAL routine
;;      called mrfprm  to:
;;      1) get the parameters from the header of a report
;;         and put them in structure variable of type RPT_PRM
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      This routine is called from IDL using the following commands:
;;      IDL>params=mrfprm( HANDLE=handle [,NSUP=,SUP=LonArr(NSUP)]
;;                                 [,NXAUX=,XAUX=LonArr(NXAUX)][,/HELP] [,/DEBUG])
;;
;; INPUTS:
;;      handle     - LONG ARRAY of size >1000 containing the report
;;      nsup    - OPTIONNAL, it is set to 0 in the function, indicating
;;                that array sup is not used. (future expansion)
;;      sup     - OPTIONNAL,nsup size longarray used with nsup.
;;      nxaux   - OPTIONNAL, it is set to 0 in the function, indicating
;;                that array xaux is not used. (future expansion)
;;      xaux   - OPTIONNAL,nxaux size longarray used with nxaux.
;;
;; KEYWORDS:
;;      HELP  - If this keyword is set, print the calling sequence
;;      DEBUG - If this keyword is set, this routine will print debug informations;;
;; ON RETURN:
;;      a structure containing each header parameter of a burp file report
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written July, 2001   H. Benhocine
;;

FUNCTION mrfprm,HANDLE=handle,NSUP=nsup,SUP=sup,NXAUX=nxaux,XAUX=xaux,HELP=help,DEBUG=debug
  common path_to_library
  ier=0

  rpt_station=bytarr(10)

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: params=MRFPRM(HANDLE=handle [,NSUP=,SUP=LonArr(NSUP)],[,NXAUX=,XAUX=LonArr(NXAUX)])"
    return,ier
  endif
;
;Appel de la fonction rpt_prm_struc()
params=rpt_prm_struc()
  if (KEYWORD_SET(debug)) then begin
print, 'handle=', handle 
HELP, params, /structure
  endif

; call to external routine

  ier = CALL_EXTERNAL(libso,'i_idl_mrfprm',handle,params, rpt_station)
  params.stnid=string(rpt_station)
return, params
end;____________________________________________________________________________

;;
;;      MRBPRM
;;
;; PURPOSE:
;;      This IDL procedure calls a CALL_EXTERNAL routine
;;      called mrbprm  to:
;;      1) extract the descriptor parameters from a block of data 
;;         and put them in structure variable of type BLK_PRM
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      This routine is called from IDL using the following commands:
;;      IDL>params=mrbhdr( BUF=buf,BLKN=blkno[,/HELP] [,/DEBUG]);;
;; INPUTS:
;;      buf     - LONG ARRAY of size >1000 containing the report
;;      nlkno   - the block number to extract parameters from 
;;
;; KEYWORDS:
;;      HELP  - If this keyword is set, print the calling sequence
;;      DEBUG - If this keyword is set, this routine will print debug informations;;
;; ON RETURN:
;;      a structure containing each  parameter of a block data 
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Feb, 2001   H. Benhocine
;;

FUNCTION mrbprm,BUF=buf,BLKN=blkno,HELP=help,DEBUG=debug
  common path_to_library
  ier=0

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: params=MRBPRM(BUF=LonArr(size>1000) ,BLKN=block_number)"
    return,ier
  endif
;
;Appel de la fonction blk_prm_struc()

  params=blk_prm_struc()
  buf(0)=N_Elements(buf)
  if (KEYWORD_SET(debug)) then begin

    dim = N_Elements(buf)
    HELP, dim, buf
    print, buf(0)
    HELP, params, /structure
  endif

; call to external routine

  ier = CALL_EXTERNAL(libso,'i_idl_mrbprm',buf,params, blkno)
return, params
end;____________________________________________________________________________

;;
;;      MRBXTR
;;
;; PURPOSE:
;;      This IDL procedure calls a CALL_EXTERNAL routine
;;      called mrbxtr  to:
;;      1) extract a block of data from a report 
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      This routine is called from IDL using the following commands:
;;      IDL>params=mrbxtr( BUF=buf,BLKN=blkno,LSTELE=lstele,
;;          TBLVAL=tblval [,/HELP] [,/DEBUG])
;; INPUTS:
;;      buf     - LONG ARRAY of size >1000 containing the report
;;      nlkno   - the block number to data  from
;;
;; KEYWORDS:
;;      HELP  - If this keyword is set, print the calling sequence
;;      DEBUG - If this keyword is set, this routine will print debug informations;;
;; ON RETURN:
;;      status of the execution 
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Feb, 2001   H. Benhocine
;;

FUNCTION mrbxtr,BUF=buf,BLKN=blkno,LSTELE=lstele,TBLVAL=tblval,HELP=help,DEBUG=debug
  common path_to_library
  ier=0

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: status=MRBXTR(BUF=LonArr(size) ,BLKN=block_number, LSTELE=array(nelem), TBLVAL=array(nelem, nval, nt))"
    return,ier
  endif
;

  if (KEYWORD_SET(debug)) then begin
     dim = N_Elements(buf)
     buf(0)=N_Elements(buf)
     HELP, dim, buf
     print,'buf=', buf(0)
     dimlstele=N_Elements(lstele)
     dimtblval=N_Elements(tblval)
     HELP, dimlstele, dimtblval

  endif

; call to external routine

   p     = MRBPRM(BUF=buf,BLKN=blkno)
   nelem = p.nele
   nval  = p.nval
   nt    = p.nt
   tblval =long(tblval)
   lstele =long(lstele)

  ier = CALL_EXTERNAL(libso,'i_idl_mrbxtr',buf, blkno, lstele,tblval $
                     , nelem, nval, nt)

return, ier 
end;____________________________________________________________________________


;;
;;      MRBCVT
;;
;; PURPOSE:
;;      This IDL procedure calls a CALL_EXTERNAL routine
;;      called mrbxtr  to:
;;      1) extract a block of data from a report
;;      Conversion of values from RVAL (real) to TBLVAL (integer) or the
;;      inverse depending on the MODE. If MODE=0, convert from TBLVAL to RVAL
;;      and if MODE=1, convert from RVAL to TBLVAL.
;;      
;;      istat=c_mrbcvt(liste,tblval,rval,nele,nval,nt,mode)
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      This routine is called from IDL using the following commands:
;;      IDL>status=mrbcvt(LSTELE=lstele,TBLVAL=tblval, RVAL=rval,
;;         NELE=nele, NVAL=nval, NT=nt [,/HELP] [,/DEBUG])
;; INPUTS:
;;      LSTELE - List of element names in the report in numeric BUFR codes
;;      TBLVAL - [nele,nval,nt] Array for data (block)
;;      RVAL   - [nele,nval,nt] Array for data (block)
;;      MODE   - MODE=0 convert from TBLVAL to RVAL, if MODE=1
;;               convert from RVAL to TBLVAL.
;;      NELE,NVAL, NT - data block descriptor parameters  
;;      BTYPE, Type de blok
;;      DEBUG - If this keyword is set, this routine will print debug informations;;
;; ON RETURN:
;;      status of the execution
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Feb, 2001   H. Benhocine
;;

FUNCTION mrbcvt,LSTELE=lstele,TBLVAL=tblval, RVAL=rval, NELE=nele, NVAL=nval, NT=nt, MODE=mode, HELP=help,DEBUG=debug
  common path_to_library
  ier=0

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: status=mrbcvt(LSTELE=array_variable(nele),TBLVAL=array_variable(nele,nval,nt), RVAL=array_variable(nele,nval,nt), NELE=, NVAL=, NT=, MODE=)"
    return,ier
  endif
;
 if (KEYWORD_SET(debug)) then begin

  dim = N_Elements(lstele)
  HELP, dim,lstele, tblval, rval, nele,nval, nt, mode 
  dimlstele=N_Elements(lstele)
  dimtblval=N_Elements(tblval)
  dimrval=N_Elements(rval)
  HELP, dimlstele, dimtblval,dimrval

  endif



; call to external routine

  ier = CALL_EXTERNAL(libso,'i_idl_mrbcvt', lstele,tblval,rval, nele,nval,nt,mode)

return, ier
end;____________________________________________________________________________


;;
;;      MRBDCL
;;
;; PURPOSE:
;;      This IDL procedure calls a CALL_EXTERNAL routine
;;      called mrbdcl  to:
;;      1)To convert a list of of coded (CMC) element names in array LISTE into 6 
;;      digit decimal BUFR format. The "decoded" elements are returned  
;;      in the array DLISTE  
;;
;;      istat=c_mrbdcl(liste,dliste,nele) 
;;
;; CATEGORY:
;;      Dynamic Link
;;
;; CALLING SEQUENCE:
;;      This routine is called from IDL using the following commands:
;;      IDL>status=mrbdcl(LSTELE=lstele,DLISTE=dliste,
;;         NELE=nele, NT=nt [,/HELP] [,/DEBUG])
;; INPUTS:
;;      LSTELE - List of element names in the report in numeric BUFR codes
;;      DLISTE - LSTELE converted 
;;      NELE   - data block NELEM descriptor parameter
;;      DEBUG - If this keyword is set, this routine will print debug informations;;
;; ON RETURN:
;;      status of the execution
;;
;; SIDE EFFECTS:
;;      None.
;;
;; RESTRICTIONS:
;;      None.
;;
;; MODIFICATION HISTORY:
;;      Written Feb, 2001   H. Benhocine
;;

FUNCTION mrbdcl,LSTELE=lstele,DLISTE=dliste, NELE=nele, HELP=help,DEBUG=debug
  common path_to_library
  ier=0

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: status=mrbdcl(LSTELE=array_variable(nele),DLISTE=array_variable(nele),  NELE=)"
    return,ier
  endif
;

  if (KEYWORD_SET(debug)) then begin

  dim = N_Elements(lstele)
  HELP, dim,lstele, dliste,  nele
  dimlstele=N_Elements(lstele)
  dimtblval=N_Elements(dliste)
  HELP, dimlstele, dimtblval
  endif

; call to external routine

  ier = CALL_EXTERNAL(libso,'i_idl_mrbdcl', lstele,dliste, nele)


return, ier
end;____________________________________________________________________________


FUNCTION blk_type_marqueur,BTYPE=i_btype, HELP=help 
  common path_to_library,libso

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: status=blk_type_marqueur(BTYPE=btype)"
    return,0
  endif

  IF (N_Elements(i_btype) eq 0) THEN begin
     btype=0L
  ENDIF ELSE begin
     btype=long(i_btype)
  ENDELSE

  bknat = CALL_EXTERNAL(libso,'i_idl_blk_type_marqueur',btype)
return, bknat
end;____________________________________________________________________________

FUNCTION file_type,FILE=file, HELP=help
  common path_to_library,libso

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: status=file_type(FILE=your_file)"
    return,0
  endif

  IF (N_Elements(FILE) eq 0) THEN begin
     return,-1L
  ENDIF ELSE begin
     file= string(file)
  ENDELSE

  type = CALL_EXTERNAL(libso,'i_idl_file_type',file)
return, type
end;____________________________________________________________________________

FUNCTION block_type,BTYPE=btype, HELP=help
  common path_to_library,libso

  bknat1 = STRING(REPLICATE(32B,10))
  bknat2 = STRING(REPLICATE(32B,10))

  bktyp1 = STRING(REPLICATE(32B,10))
  bktyp2 = STRING(REPLICATE(32B,20))

  

  if (KEYWORD_SET(help)) then begin
    print,"USAGE: status=file_type(FILE=your_file)"
    return,0
  endif

IF N_elements(btype) NE 0 then btype=fix(btype) else return, -1L

  type = CALL_EXTERNAL(libso,'i_idl_block_type',btype,bknat1,bknat2,bktyp1,bktyp2)

return,STRTRIM([bknat1,bknat2,bktyp1,bktyp2],2) 
end;____________________________________________________________________________

FUNCTION runn_type,RUNN=runn, HELP=help
  common path_to_library,libso

  rnat1 = STRING(REPLICATE(32B,10))
  rnat2 = STRING(REPLICATE(32B,10))

  rtyp1 = STRING(REPLICATE(32B,20))
  rtyp2 = STRING(REPLICATE(32B,10))



  if (KEYWORD_SET(help)) then begin
    print,"USAGE: status=runn_type(RUNN=your_runn)"
    return,0
  endif

IF N_elements(runn) NE 0 then runn=fix(runn) else return, -1L


  type = CALL_EXTERNAL(libso,'i_idl_runn_type',runn,rnat1,rnat2,rtyp1,rtyp2)

return,STRTRIM([rnat1,rnat2,rtyp1,rtyp2],2)
end;____________________________________________________________________________


pro init_burp_idl,Vers
  common path_to_library,libso

  spawn,'uname -s',archi
  liblocation='/usr/local/env/armnlib/idl/'+archi+'/afsd/'
;  liblocation='/users/dor/afsd/hmd/src/projet/'
;  liblocation='/users/dor/afsd/hmd/tmp/idl_burp/'
  libso=STRING(liblocation+'i_idl_'+STRING(Vers,FORMAT='(I2.2)')+'_burp2001.so',/PRINT)
  print,"Debug init_burp_idl libso=",libso


  help,libso
end;____________________________________________________________________________

pro IDL_BURP
print,""
print,"+------------------------------------------------------------------------+"
print,"| istat = MRFOPN(....) to open a burp file                               |"
print,"| istat = MRFMXL(....) to get the length of the longest report           |"
print,"|                                                                        |"
print,"| istat = MRFOPR(....) initialize an option that is in real format       |"
print,"| istat = MRFOPC(....) initialize an option that is in Character  format |"
print,"|                                                                        |"
print,"| istat = MRFLOC(....) to locate the pointer of a report that matches    |"
print,"|                      stinid, idtyp etc.., parameters                   |"
print,"| istat = MRFGET(....) to get the report pointed by handle               |"
print,"|                      and put it in array                               |"
print,"| istat = MRBHDR(....) to get the parameters from the header of a report |"
print,"|                      witch in the array buffer..                       |"
print,"| istat = MRFPRM(....) to get the parameters from the header of a report |"
print,"|                      pointed by handle..                               |"
print,"| istat = MRBLOC(....) to  find the position of the  data block          |"
print,"|                      of type bfam, bdesk, btyp..                       |"
print,"| istat = BLK_TYPE_MARQUEUR (..) returns 1  flags block,  0 othewise     |"
print,"|                                                                        |"
print,"| istat = MRBPRM(....) to get the parameters from the header of a block  |"
print,"| istat = MRBXTR(....) to extract data from a  block                     |"
print,"| istat = MRBCVT(....) converts block's data values from int to real     |"
print,"| istat = MRBDCL(....) converts a list of code(CMC) element names into   |"
print,"|                      6 digit BUFR format                               |"
print,"| istat = MRFCLS(....) to close a burp file                              |"
print,"|                                                                        |"
print,"| istat = RPT_PRM_STRUC() definition of the structure returned by MRBHDR |"
print,"| istat = BLK_PRM_STRUC() definition of the structure returned by MRBPRM |"
print,"|                                                                        |"
print,"|                                                                        |"
print,"|                                               (CMDA) - April 2001      |"
print,"+------------------------------------------------------------------------+"
end

pro new_burprocs2001, Vers
init_burp_idl, Vers

; repertoire developpement
;Dir='/users/dor/afsd/hmd/src/projet/obj_interface'

Dir='/users/dor/afsd/cvs/idl/samples'

!path= Dir + ':' + !path


print,""
print,"+------------------------------------------------------------------------------+"
print,"|                                                                              |"
print,"| note: to return to this screen, type initburplib again ..                    |"
print,"|                                                                              |"
print,"| Two ways to read burp files from IDL:                                        |"
print,"|                                                                              |"
print,"| 1 ) Using the standard functions,                                            |"
print,"|     idl versions 5.1, 5.3, 5.4, 6.3, 7.1 (7.1 only on linux)                 |"
print,"|                                                                              |"
print,"|     To get the list of the IDL_BURP functions,                               |"
print,"|     type IDL_BURP at the command line of IDL                                 |"
print,"|                                                                              |"
print,"|     For calling sequence information                                         |"
print,"|     type  status=FUNCTION_NAME(/HELP)                                        |"
print,"|                                                                              |"
print,"|     Sample program using these functions, see                                |"
print,"|     /users/dor/afsd/cvs/idl/samples/idl_test.pro                             |"
print,"|                                                                              |"
print,"| 2 ) Using IDL_BURP OBJECT interface,                                         |"
print,"|     warning: idl version >= 5.3, 6.3, 7.1                                    |"
print,"|                                                                              |"
print,"|     Type IDL_BURP_OBJ,/HELP_FILE   to get help about burp object file        |"
print,"|     Type IDL_BURP_OBJ,/HELP_REPORT to get help about burp object report      |"
print,"|     Type IDL_BURP_OBJ,/HELP_BLOCK  to get help about burp object block       |"
print,"|     Type IDL_BURP_OBJ,/EXAMPLE     example code using the three objects.     |"
print,"|                                                                              |"
print,"|     Sample programs using these two objects, see                             |"
print,"|     /users/dor/afsd/cvs/idl/samples/idl_test_new.pro                         |"
print,"|                                                                              |"
;print,"|     /users/dor/afsd/cvs/idl/samples/arcad_obj_config_use.pro                 |"
print,"|                                                                              |"
print,"+------------------------------------------------------------------------------+"
end

