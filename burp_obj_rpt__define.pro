
Function Burp_Obj_Rpt::GetProperty,TEMPS=temps,FLGS=flgs,STNID =stnid, $
         IDTYPE=idtype,LAT=lat,LON=lon ,DX=dx,DY=dy,ELEV=elev,ID_RPT=ID_Rpt,$
         DRND=drnd,RUNN=runn,DATE=date,OARS=oars,NBLK=nblk,FILE =file,ALL=all

;;
   IF (KEYWORD_SET(TEMPS))    THEN RETURN, self.temps
   IF (KEYWORD_SET(flgs))    THEN RETURN, self.flgs
   IF (KEYWORD_SET(stnid))      THEN RETURN, self.stnid
   IF (KEYWORD_SET(idtype))    THEN RETURN, self.idtype
   IF (KEYWORD_SET(lat))   THEN RETURN, self.lat
   IF (KEYWORD_SET(lon))    THEN RETURN, self.lon
   IF (KEYWORD_SET(dx))    THEN RETURN, self.dx
   IF (KEYWORD_SET(dy))    THEN RETURN, self.dy
   IF (KEYWORD_SET(elev))   THEN RETURN, self.elev
   IF (KEYWORD_SET(ID_Rpt)) THEN RETURN, self.id_rpt
   IF (KEYWORD_SET(drnd))   THEN RETURN, self.drnd
   IF (KEYWORD_SET(runn))   THEN RETURN, self.runn
   IF (KEYWORD_SET(date))   THEN RETURN, self.date
   IF (KEYWORD_SET(oars))   THEN RETURN, self.oars
   IF (KEYWORD_SET(nblk))   THEN RETURN, self.nblk
   IF (KEYWORD_SET(file))   THEN RETURN, self.file


struct = {$
           temps  : self.temps, $              ;
           flgs   : self.flgs, $              ;
           stnid     : self.stnid, $              ;

           idtype   : self.idtype, $              ;

           lat  : self.lat, $              ;
           lon   : self.lon, $              ;

           dx   : self.dx, $              ;
           dy   : self.dy, $              ;
           elev  : self.elev, $              ;
           ID_Rpt: self.ID_Rpt, $             ;
           drnd  : self.drnd, $              ;

           runn : self.runn, $       ;
           date : self.date, $       ;
           oars   : self.oars,$        ;
           nblk   : self.nblk, $
           unit   : self.unit, $
           file   : self.file $

     }




RETURN, struct

END ;--------------------------------------------------------------------

FUNCTION Burp_Obj_Rpt::get_date,Hour=hour,min=min
    r='0000'
    strput,r,string(strtrim(self.temps,2)),4-strlen(string(strtrim(self.temps,2)))

    IF (KEYWORD_SET(hour))   THEN RETURN,string(self.date)+ strmid(r,0,2)
    IF (KEYWORD_SET(min))   THEN RETURN, string(self.date) + r
    return,string(self.date)


END ;--------------------------------------------------------------------

FUNCTION Burp_Obj_Rpt::runn_Type,Type

  IF N_ELEMENTS(type) EQ 0 THEN type = 10
  CASE type OF
       0: RETURN, (Runn_Type(RUNN=self.runn))[0]
       1: RETURN, (Runn_Type(RUNN=self.runn))[1]
       2: RETURN, (Runn_Type(RUNN=self.runn))[2]
       3: RETURN, (Runn_Type(RUNN=self.runn))[3]
  ELSE  : RETURN, Runn_Type(RUNN=self.runn)
  ENDCASE



END ;--------------------------------------------------------------------


Function Burp_Obj_Rpt::Get_Buffer         ; The name of the method.

RETURN, *self.buffer

END ;--------------------------------------------------------------------

Function Burp_Obj_Rpt::Get_Ptr_Buffer         ; The name of the method.


RETURN, self.buffer

END ;--------------------------------------------------------------------


Function Burp_Obj_Rpt::Find_Block, $         ; The name of the method.


           BLOCK   =  block, $              ;
           BLK   = blk , $              ;
           BFAM  = BFAM, $              ;
           BDESC = BDESC, $             ;
           BTYP  = BTYP, $              ;
          _Extra =  extra

  common path_to_library

  IF NOT(KEYWORD_SET(block)) THEN begin
      print,'block object required as argument to Find_report...'
      return,-1
  ENDIF ELSE  BEGIN
      IF (SIZE(block,/TYPE) EQ 11) THEN BEGIN
          IF obj_class(block) EQ 'BURP_OBJ_BLOCK' THEN BEGIN 
             report =report
          ENDIF ELSE BEGIN
             print,'block object required as argument to Find_report...'
             return,-1
          ENDELSE
            
      ENDIF ELSE BEGIN
          print,'object reference required to find_report'
          return,-1
      ENDELSE

  ENDELSE

  IF N_elements(blk) NE 0 then blk0=blk else blk0 =0L
  IF N_elements(bfam) NE 0 then bfam=bfam else bfam =-1L
  IF N_elements(btyp) NE 0 then btyp=btyp else btyp =-1L
  IF N_elements(bdesc) NE 0 then bdesc=bdesc else bdesc =-1L

  blk0 =long(blk0) & bdesc =long(bdesc) & btyp =long(btyp) & bfam =long(bfam)

;; pour les besoins de la fonction next_block

  self.last_blk0=blk0 & self.last_bdesc=bdesc & self.last_btyp=btyp & self.last_bfam=bfam

  ID_Blok = CALL_EXTERNAL(libso,'i_idl_mrbloc',$
     *self.buffer,bfam, bdesc, btyp, blk0)


;; ID_Blok=MRBLOC(BUF=*self.buffer,BLK=blk,BTYP=BTYP,BFAM=BFAM)

      IF ID_Blok GT 0 THEN BEGIN
         err = block->Get_block(BUFFER=self.buffer,blk=ID_blok)
         self.last_blk0 = ID_Blok
         self.blk=ID_Blok
      ENDIF




RETURN, ID_Blok


END ;--------------------------------------------------------------------

Function Burp_Obj_Rpt::Next_Block, $         ; The name of the method.


           BLOCK   =  block, $              ;
          _Extra =  extra

  common path_to_library

  IF NOT(KEYWORD_SET(block)) THEN begin
      print,'block object required as argument to Find_report...'
      return,-1
  ENDIF ELSE  BEGIN
      IF (SIZE(block,/TYPE) EQ 11) THEN BEGIN
          IF obj_class(block) EQ 'BURP_OBJ_BLOCK' THEN BEGIN
             report =report
          ENDIF ELSE BEGIN
             print,'block object required as argument to Find_report...'
             return,-1
          ENDELSE

      ENDIF ELSE BEGIN
          print,'object reference required to find_report'
          return,-1
      ENDELSE

  ENDELSE
  ID_Blok = CALL_EXTERNAL(libso,'i_idl_mrbloc',$
     *self.buffer,self.last_bfam, self.last_bdesc, self.last_btyp, self.last_blk0)


;; ID_Blok=MRBLOC(BUF=*self.buffer,BLK=blk,BTYP=BTYP,BFAM=BFAM)

      IF ID_Blok GT 0 THEN BEGIN
         err = block->Get_block(BUFFER=self.buffer,blk=ID_blok)
         self.last_blk0 = ID_Blok
         self.blk=ID_Blok
      ENDIF




RETURN, ID_Blok


END ;--------------------------------------------------------------------



Function Burp_Obj_Rpt::Get_Report, $         ; The name of the method.

           File     = file, $
           unit     = unit, $
           handle   = ID_Rpt, $
           dim      = dim, $              ;


          _Extra =  extra

common path_to_library



  ;       Ptr_Free, self.buffer
         buffer = Make_Array(dim,Value=0, /LONG)
         buffer[0]    = N_ELEMENTS(buffer)
         *self.buffer  = buffer

         self.ID_Rpt=ID_Rpt
;         err      = MRFGET(handle=ID_Rpt, BUF=*self.buffer)
         ier = CALL_EXTERNAL(libso,'i_idl_mrfget',ID_Rpt,*self.buffer)

;         p=MRBHDR(BUF=*self.buffer) 
          rpt_station=bytarr(9)
          p=rpt_prm_struc()
          ier = CALL_EXTERNAL(libso,'i_idl_mrbhdr',*self.buffer,p, rpt_station)
          p.stnid=string(rpt_station)

         self.temps   = p.temps
         self.flgs    = p.flgs
         self.stnid   = p.stnid
         self.idtype  = p.idtype
         self.lat     = p.lat
         self.lon     = p.lon
         self.dx      = p.dx
         self.dy      = p.dy
         self.elev    = p.elev
         self.drnd    = p.drnd
         self.runn    = p.runn
         self.date    = p.date
         self.oars    = p.oars
         self.nblk    = p.nblk


         self.file    = file
         self.unit    = unit




RETURN, ier

END ;--------------------------------------------------------------------

FUNCTION Burp_Obj_Rpt::SetProperty, $
        file     = file, $
        _Extra   = extra
      
       IF NOT(KEYWORD_SET(file)) THEN begin
         file=""
         Return, -1
       ENDIF ELSE begin
          unit = file->GetProperty(/UNIT)
       ENDELSE

        self.unit =   unit
        self.file =  file->GetProperty(/FILE)



RETURN, 1

END ;--------------------------------------------------------------------




FUNCTION Burp_Obj_Rpt::Init
         buffer =[-1L]
         self.buffer  =Ptr_New(buffer)
         self.blk  = -1L
     

RETURN, 1

END ;--------------------------------------------------------------------

PRO Burp_Obj_Rpt::Cleanup         ; The name of the method.

           Ptr_Free, self.buffer

END ;--------------------------------------------------------------------



PRO Burp_Obj_Rpt__Define

; The definition of the Obj_Rpt_Class object class.

struct = { BURP_OBJ_RPT, $            ;
           file   :'', $
           unit   : -1L, $              ; 
           temps  : 0L, $              ; 
           flgs   : 0L, $              ; 
           stnid     : "", $              ;

           idtype   : 0L, $              ;

           lat  : 0L, $              ;
           lon   : 0L, $              ;

           dx   : 0L, $              ;
           dy   : 0L, $              ;
           elev  : 0L, $              ; 

           drnd  : 0L, $              ;
           date  : 0L, $              ;
           oars  : 0L, $              ;
           runn  : 0L, $              ;
           nblk  : 0L, $              ;
           blk   : -1L, $              ;
           ID_Rpt  : -1L, $              ;

           buffer : Ptr_New(),$        ;

           last_blk0  : 0L,$ 
           last_bdesc : -1L,$ 
           last_btyp  : -1L,$ 
           last_bfam  : -1L,$




           extra: Ptr_New() $         ; 
          }

END ;--------------------------------------------------------------------

