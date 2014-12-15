Function Burp_Obj_Block::GetProperty, BFAM=bfam,BDESC=bdesc, BTYP=btyp,$
                         NBIT=nbit, BIT0=bit0,BKNAT=bknat,ID_BLOK=id_blok,$
                         DATYP=datyp,NVAL=nval,NELEM=nelem,Nt=nt,All=all


;; 
   IF (KEYWORD_SET(NVAL))    THEN RETURN, self.nval
   IF (KEYWORD_SET(NELEM))    THEN RETURN, self.nelem
   IF (KEYWORD_SET(NT))      THEN RETURN, self.nt
   IF (KEYWORD_SET(BFAM))    THEN RETURN, self.bfam
   IF (KEYWORD_SET(BDESC))   THEN RETURN, self.bdesc
   IF (KEYWORD_SET(BTYP))    THEN RETURN, self.btyp
   IF (KEYWORD_SET(NBIT))    THEN RETURN, self.nbit
   IF (KEYWORD_SET(BIT0))    THEN RETURN, self.bit0
   IF (KEYWORD_SET(BKNAT))   THEN RETURN, self.bknat
   IF (KEYWORD_SET(ID_BLOK)) THEN RETURN, self.id_blok
   IF (KEYWORD_SET(DATYP))   THEN RETURN, self.datyp


  

   struct = {$
           nelem  : self.nelem, $              ;
           nval   : self.nval, $              ;
           nt     : self.nt, $              ;

           bfam   : self.bfam, $              ;

           bdesc  : self.bdesc, $              ;
           btyp   : self.btyp, $              ;

           nbit   : self.nbit, $              ;
           bit0   : self.bit0, $              ;
           ID_blok: self.Id_Blok, $             ;
           datyp  : self.datyp $              ;
    }

     RETURN, struct


END ;--------------------------------------------------------------------


Function Burp_Obj_Block::Get_Block, $         ; The name of the method.

           BUFFER   =  buffer, $              ;
           BLK   = ID_Blok , $              ;
          _Extra =  extra

  common path_to_library
   

self.ID_blok = Id_blok

;; p=MRBPRM(BUF=*buffer,BLKN=ID_blok)

         params=blk_prm_struc()
         ier = CALL_EXTERNAL(libso,'i_idl_mrbprm',*buffer,params, ID_Blok)

         self.nelem = params.nele
         self.nval  = params.nval
         self.nt    = params.nt
         self.bfam = params.bfam
         self.bdesc= params.bdesc
         self.btyp = params.btyp
         self.nbit = params.nbit
         self.bit0 = params.bit0
         self.datyp= params.datyp

         lstele = LonArr(self.nelem)
         dliste = LonArr(self.nelem)

   self.bknat = CALL_EXTERNAL(libso,'i_idl_blk_type_marqueur',self.btyp)

;; self.bknat=blk_type_marqueur(BTYPE=self.btyp)

        ival   = LonArr(self.nelem,self.nval, self.nt)

;; istat = MRBXTR(BUF=*buffer, BLK=self.ID_blok,LSTELE=lstele, TBLVAL=ival)

   ier = CALL_EXTERNAL(libso,'i_idl_mrbxtr',*buffer, ID_Blok, lstele,ival $
                     , self.nelem, self.nval, self.nt)
   ier = CALL_EXTERNAL(libso,'i_idl_mrbdcl', lstele,dliste, self.nelem)

;; istat1= MRBDCL(LSTELE=lstele, DLISTE=dliste, NELE=self.nelem)

        *self.lstele=lstele
        *self.dliste=dliste
        *self.ival  =ival
        *self.rval  =ival


       IF  self.bknat EQ 0 THEN BEGIN
         rval   = Make_Array(self.nelem,self.nval, self.nt, value=0.0,/FLOAT)
;; istat2=  MRBCVT(LSTELE=lstele,TBLVAL=ival,RVAL=rval,NELE=self.nelem,$
;;              NVAL=self.nval,NT=self.nt,MODE=0)

          ier = CALL_EXTERNAL(libso,'i_idl_mrbcvt', lstele,ival,rval,$
                       self.nelem,self.nval,self.nt,0)

           *self.rval  =rval
       ENDIF


RETURN, ID_Blok

END ;--------------------------------------------------------------------


FUNCTION Burp_Obj_Block::Get_Elements_List, CONV=conv

  IF NOT(KEYWORD_SET(CONV)) THEN begin
     RETURN,*self.lstele 
  ENDIF ELSE begin
     RETURN,*self.dliste 
  ENDELSE


END ;--------------------------------------------------------------------

FUNCTION Burp_Obj_Block::Get_Elements_Values, CONV=conv

  IF NOT(KEYWORD_SET(CONV)) THEN begin
       RETURN,*self.ival 
  ENDIF ELSE begin
     RETURN,*self.rval
  ENDELSE


END ;--------------------------------------------------------------------

FUNCTION Burp_Obj_Block::Block_Type,Type

  IF N_ELEMENTS(type) EQ 0 THEN type = 10
  CASE type OF
       0: RETURN, (Block_Type(BTYPE=self.btyp))[0] 
       1: RETURN, (Block_Type(BTYPE=self.btyp))[1] 
       2: RETURN, (Block_Type(BTYPE=self.btyp))[2] 
       3: RETURN, (Block_Type(BTYPE=self.btyp))[3] 
  ELSE  : RETURN, Block_Type(BTYPE=self.btyp)
  ENDCASE



END ;--------------------------------------------------------------------





FUNCTION Burp_Obj_Block::Init
lstele = [-1L]
dliste = [-1L]
ival   = [-1L]
rval   = [-1.0]


        self.lstele=Ptr_New(lstele)
        self.dliste=Ptr_New(dliste)
        self.ival  =Ptr_New(ival)
        self.rval  =Ptr_New(rval)
     



RETURN, 1

END ;--------------------------------------------------------------------

PRO Burp_Obj_Block::Cleanup         ; The name of the method.



           Ptr_Free, self.lstele
           Ptr_Free, self.dliste
           Ptr_Free, self.ival
           Ptr_Free, self.rval
           Ptr_Free, self.extra

END ;--------------------------------------------------------------------



PRO Burp_Obj_Block__Define

; The definition of the BOXCONFIG object class.

struct = { BURP_OBJ_BLOCK, $            ;
           nelem  : 0L, $              ; 
           nval   : 0L, $              ; 
           nt     : 0L, $              ;

           bfam   : 0L, $              ;

           bdesc  : 0L, $              ;
           btyp   : 0L, $              ;

           nbit   : 0L, $              ;
           bit0   : 0L, $              ;
           datyp  : 0L, $              ; 

           bknat  : 0L, $              ;
           ID_blok: -1L, $             ;
           lstele : Ptr_New(), $       ; 
           dliste : Ptr_New(), $       ;
           ival   : Ptr_New(),$        ;
           rval   : Ptr_New(),$        ;




           extra: Ptr_New() $         ; 
          }

END ;--------------------------------------------------------------------

