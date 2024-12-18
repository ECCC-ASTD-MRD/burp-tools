Function arcad_obj_config::Get_Stations

      Catch, TheError
      IF TheError NE 0 THEN BEGIN
         Catch, /Cancel
         print, "Error, Calling, this function!"
         RETURN, -1
      ENDIF
   
     IF self.set_liste EQ 1 THEN BEGIN
        RETURN,Get_OMM_Stations(FILE=self.stations,Template=OMM_Template_structure())
     ENDIF ELSE BEGIN
         Message, "Liste not found!"
         RETURN, -1
     ENDELSE
        
END;-------------------------------------------------------------------

Function arcad_obj_config::Get_signature

;ecrire au debut de fichier d'une noouvelle structure arcad'
; 3 esapces suivi de ARCADBINSTRUC
; 16 caracteres en tout

 return,'   ARCADBINSTRUC'

END;________________________________________________________________________

Function arcad_obj_config::Get_Struct_Definition,OBS_ARCAD=obs_arcad,LONG=long,TIME=time

      Catch, TheError
      IF TheError NE 0 THEN BEGIN
         Catch, /Cancel
         print, "Error, Calling, this function!"
         RETURN, -1
      ENDIF


     str12 = STRING(REPLICATE(32B, 12))  
IF (KEYWORD_SET(OBS_ARCAD)) THEN BEGIN

 IF (KEYWORD_SET(LONG))    THEN  BEGIN

     IF *self.type_struct EQ 'UNI' THEN BEGIN

      IF (KEYWORD_SET(TIME)) THEN BEGIN 
            Enreg = { stnid: str12,               $
                  time: -1L,                       $
                  lon: 0.0,                       $
                  lat: 0.0,                       $
                  niv: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data), $
                  flags: LONARR(self.Max_Niveaux,self.Max_Data), $
                  count: LONARR(self.Max_Niveaux,self.Max_Data)  }
      ENDIF ELSE BEGIN
            Enreg = { stnid: str12,               $
                  lon: 0.0,                       $
                  lat: 0.0,                       $
                  niv: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data), $
                  flags: LONARR(self.Max_Niveaux,self.Max_Data), $
                  count: LONARR(self.Max_Niveaux,self.Max_Data)  }
    
      ENDELSE                                      ; endlese time


     ENDIF ELSE BEGIN
      IF (KEYWORD_SET(TIME)) THEN BEGIN 
            Enreg = { stnid: str12,               $
                  time: -1L,                       $
                  lon: 0.0,                       $
                  lat: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data), $
                  flags: LONARR(self.Max_Niveaux,self.Max_Data), $
                  count: LONARR(self.Max_Niveaux,self.Max_Data)  }
      ENDIF ELSE BEGIN
            Enreg = { stnid: str12,               $
                  lon: 0.0,                       $
                  lat: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data), $
                  flags: LONARR(self.Max_Niveaux,self.Max_Data), $
                  count: LONARR(self.Max_Niveaux,self.Max_Data)  }
      ENDELSE                                      ; endelse time


     ENDELSE                                      ;endelse type_struc UNI

 ENDIF ELSE BEGIN   ; else for keywor long

     IF *self.type_struct EQ 'UNI' THEN BEGIN

      IF (KEYWORD_SET(TIME)) THEN BEGIN 
            Enreg = { stnid: str12,               $
                  time: -1L,                       $
                  lon: 0.0,                       $
                  lat: 0.0,                       $
                  niv: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data), $
                  flags: INTARR(self.Max_Niveaux,self.Max_Data), $
                  count: INTARR(self.Max_Niveaux,self.Max_Data)  }
      ENDIF ELSE BEGIN
            Enreg = { stnid: str12,               $
                  lon: 0.0,                       $
                  lat: 0.0,                       $
                  niv: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data), $
                  flags: INTARR(self.Max_Niveaux,self.Max_Data), $
                  count: INTARR(self.Max_Niveaux,self.Max_Data)  }
      ENDELSE                                      ; endelse time


     ENDIF ELSE BEGIN
      IF (KEYWORD_SET(TIME)) THEN BEGIN 
            Enreg = { stnid: str12,               $
                  time: -1L,                       $
                  lon: 0.0,                       $
                  lat: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data), $
                  flags: INTARR(self.Max_Niveaux,self.Max_Data), $
                  count: INTARR(self.Max_Niveaux,self.Max_Data)  }
      ENDIF ELSE BEGIN
            Enreg = { stnid: str12,               $
                  lon: 0.0,                       $
                  lat: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data), $
                  flags: INTARR(self.Max_Niveaux,self.Max_Data), $
                  count: INTARR(self.Max_Niveaux,self.Max_Data)  }
      ENDELSE                                      ; endelse time


     ENDELSE

 ENDELSE           ; endelse for  keyword long

ENDIF ELSE BEGIN  ; else obs_arcad 

 IF (KEYWORD_SET(LONG))    THEN  BEGIN

     IF *self.type_struct EQ 'UNI' THEN BEGIN
      IF (KEYWORD_SET(TIME)) THEN BEGIN 

            Enreg = { stnid: str12,               $
                  time: -1L,                       $
                  lat: 0.0,                       $
                  lon: 0.0,                       $
                  niv: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data,self.Max_Stat), $
                  flags: LONARR(self.Max_Niveaux,self.Max_Data)  }
      ENDIF ELSE BEGIN
            Enreg = { stnid: str12,               $
                  lat: 0.0,                       $
                  lon: 0.0,                       $
                  niv: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data,self.Max_Stat), $
                  flags: LONARR(self.Max_Niveaux,self.Max_Data)  }
      ENDELSE                                      ; endelse time


     ENDIF ELSE BEGIN
      IF (KEYWORD_SET(TIME)) THEN BEGIN 
            Enreg = { stnid: str12,               $
                  time: -1L,                       $
                  lat: 0.0,                       $
                  lon: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data,self.Max_Stat), $
                  flags: LONARR(self.Max_Niveaux,self.Max_Data)  }
      ENDIF ELSE BEGIN
            Enreg = { stnid: str12,               $
                  lat: 0.0,                       $
                  lon: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data,self.Max_Stat), $
                  flags: LONARR(self.Max_Niveaux,self.Max_Data)  }
      ENDELSE                                      ; endelse time

     ENDELSE
 ENDIF ELSE BEGIN
     IF *self.type_struct EQ 'UNI' THEN BEGIN
      IF (KEYWORD_SET(TIME)) THEN BEGIN 
            Enreg = { stnid: str12,               $
                  time: -1L,                       $
                  lat: 0.0,                       $
                  lon: 0.0,                       $
                  niv: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data,self.Max_Stat), $
                  flags: INTARR(self.Max_Niveaux,self.Max_Data)  }
      ENDIF ELSE BEGIN
            Enreg = { stnid: str12,               $
                  lat: 0.0,                       $
                  lon: 0.0,                       $
                  niv: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data,self.Max_Stat), $
                  flags: INTARR(self.Max_Niveaux,self.Max_Data)  }
      ENDELSE                                      ; endelse time


     ENDIF ELSE BEGIN
      IF (KEYWORD_SET(TIME)) THEN BEGIN 
            Enreg = { stnid: str12,               $
                  time: -1L,                       $
                  lat: 0.0,                       $
                  lon: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data,self.Max_Stat), $
                  flags: INTARR(self.Max_Niveaux,self.Max_Data)  }
      ENDIF ELSE BEGIN
            Enreg = { stnid: str12,               $
                  lat: 0.0,                       $
                  lon: 0.0,                       $
                  data: FLTARR(self.Max_Niveaux,self.Max_Data,self.Max_Stat), $
                  flags: INTARR(self.Max_Niveaux,self.Max_Data)  }
      ENDELSE                                      ; endelse time

     ENDELSE

 ENDELSE




ENDELSE          ; endelse for keyword obs_aracd

RETURN,Enreg

END;-------------------------------------------------------------------


Function arcad_obj_config::get_Masq_Rejets

      Catch, TheError
      IF TheError NE 0 THEN BEGIN
         Catch, /Cancel
         print, "Error, Calling, this function!"
         RETURN, -1
      ENDIF

Masq_Rejets = MAKE_ARRAY(N_ELEMENTS(*self.bits_mrq_rej),/LONG,VALUE=2L)
Masq_Rejets = Masq_Rejets^*self.bits_mrq_rej
Masq_Rejets = LONG(TOTAL(Masq_Rejets))
RETURN,Masq_Rejets

END;--------------------------------------------------------------------

Function arcad_obj_config::Extract, field

      Catch, TheError
      IF TheError NE 0 THEN BEGIN
         Catch, /Cancel
         print, "Error, verifiez-vos parametres!!"
         RETURN, -1
      ENDIF



; Check if "field" is a valid field name. If it is,
; return the value of the field. If not, return -1.

; Make sure "field" is a string variable.

   s = Size(field)
   IF s[s[0]+1] NE 7 THEN Message, "Field variable must be a sting."

; Get the name of the object class.

   thisClass = Obj_Class(self)

; Create a local structure of this type.

   ok = Execute('thisStruct = {' + thisClass + '}')

; Find the field identifier (index) in the local structure.

   structFields = Tag_Names(thisStruct)
   index = WHERE(structFields EQ StrUpCase(field), count)

; Extract and return the field if it is found.

   IF count EQ 1 THEN BEGIN

       IF (EXECUTE ('retVal = self.' + structFields[index[0]])) THEN BEGIN

; Don't ever return a valid pointer into an object
    
         IF (Ptr_Valid(retVal)) THEN BEGIN
            RETURN, *retVal
         ENDIF ELSE BEGIN
            RETURN, retVal
         ENDELSE

       ENDIF

   ENDIF ELSE BEGIN
    Message, 'Can not find field "' + field + $
     '" in object ' + OBJ_CLASS (self), /Informational
     RETURN, -1
   ENDELSE


END ;--------------------------------------------------------------------




Function arcad_obj_config::GetProperty,FAMILLE=famille,TYPE_OBS=type_obs, $
         CODE_TYPE=code_type,MAX_ENREG=max_enreg,TYPE_STRUCT=type_struct,$
         LISTE_NIVEAUX=liste_niveaux,MAX_NIVEAUX=max_niveaux,$
         LISTE_VARS=liste_vars,CODE_ELEMENTS=code_elements,$
         VARS_ASS=vars_ass,$
         MAX_DATA=max_data,BTYPE_ELEMENT=btype_element,$
         btype_marqueur=btype_marqueur,$
         BITS_MRQ_REJ=bits_mrq_rej,o_bfam=o_bfam,o_p_bfam=o_p_bfam,$
         BITS_MRQ_ASS=bits_mrq_ass,$
         o_a_bfam=o_a_bfam,LISTE_STAT=liste_stat,MAX_STAT=max_stat,$
         TYPE_VERIF_FIX=type_verif_fix,NOMBRE_DE_CAS=nombre_de_cas,$
         LEVEL_ELEMENT=level_element,CANAL_ELEMENT=canal_element,$
         SRC_FILE=src_file,DST_FILE=dst_file,$
         STATIONS=stations,SET_LISTE=set_liste, $
         STN_MISS=stn_miss ,FLT_MISS=flt_miss,INT_DEF=int_def,$
         INT_REJ=int_rej,ALL=all,CONS_CONVERSION=cons_conversion,$
         FACT_CONVERSION=fact_conversion,BTYPE_O_A_O_P=btype_o_a_o_p,$
         BTYPE_O_F=btype_o_f,O_F_BFAM=o_f_bfam,HEURE_VERIF_FIX=heure_verif_fix 
                     
;;
   IF (KEYWORD_SET(FAMILLE))    THEN RETURN, *self.famille
   IF (KEYWORD_SET(type_obs))    THEN RETURN,*self.type_obs
   IF (KEYWORD_SET(TYPE_STRUCT))    THEN RETURN,*self.type_struct
   IF (KEYWORD_SET(code_type))      THEN RETURN,*self.code_type 
   IF (KEYWORD_SET(max_enreg))    THEN RETURN,long(self.max_enreg) 
   IF (KEYWORD_SET(liste_niveaux))   THEN RETURN,*self.liste_niveaux 
   IF (KEYWORD_SET(max_niveaux))    THEN RETURN,fix(self.max_niveaux) 
   IF (KEYWORD_SET(liste_vars))    THEN RETURN, *self.liste_vars 
   IF (KEYWORD_SET(vars_ass))    THEN RETURN, *self.vars_ass 
   IF (KEYWORD_SET(code_elements))    THEN RETURN, *self.code_elements
   IF (KEYWORD_SET(fact_conversion ))    THEN RETURN, *self.fact_conversion 
   IF (KEYWORD_SET(cons_conversion))    THEN RETURN, *self.cons_conversion
   IF (KEYWORD_SET(max_data))   THEN RETURN,fix(self.max_data) 
   IF (KEYWORD_SET(btype_element)) THEN RETURN,Long(*self.btype_element) 
   IF (KEYWORD_SET(btype_marqueur))   THEN RETURN,Long(*self.btype_marqueur) 
;;
   IF (KEYWORD_SET(bits_mrq_rej))    THEN RETURN,*self.bits_mrq_rej 
   IF (KEYWORD_SET(bits_mrq_ass))    THEN RETURN,*self.bits_mrq_ass 
   IF (KEYWORD_SET(o_bfam))    THEN RETURN,Long(*self.o_bfam) 
   IF (KEYWORD_SET(o_p_bfam))      THEN RETURN,Long(self.o_p_bfam) 
   IF (KEYWORD_SET(o_a_bfam))    THEN RETURN,Long(self.o_a_bfam) 
   IF (KEYWORD_SET(liste_stat))   THEN RETURN,*self.liste_stat 
   IF (KEYWORD_SET(max_stat))    THEN RETURN,fix(self.max_stat) 
   IF (KEYWORD_SET(type_verif_fix))    THEN RETURN,fix(self.type_verif_fix)
   IF (KEYWORD_SET(nombre_de_cas))    THEN RETURN,fix(self.nombre_de_cas) 
   IF (KEYWORD_SET(level_element))   THEN RETURN,self.level_element 
   IF (KEYWORD_SET(canal_element)) THEN RETURN, self.canal_element
   IF (KEYWORD_SET(src_file))   THEN RETURN, self.src_file
;;
   IF (KEYWORD_SET(dst_file))    THEN RETURN, self.dst_file
   IF (KEYWORD_SET(stations))    THEN RETURN, self.stations
   IF (KEYWORD_SET(set_liste))      THEN RETURN, self.set_liste
   IF (KEYWORD_SET(stn_miss))    THEN RETURN, self.stn_miss
   IF (KEYWORD_SET(flt_miss))   THEN RETURN, self.flt_miss
   IF (KEYWORD_SET(int_def))    THEN RETURN, self.int_def
   IF (KEYWORD_SET(int_rej))    THEN RETURN, self.int_rej

   IF (KEYWORD_SET(BTYPE_O_A_O_P))    THEN RETURN, long(*self.BTYPE_O_A_O_P)
   IF (KEYWORD_SET(BTYPE_O_F))    THEN RETURN, long(*self.BTYPE_O_F)
   IF (KEYWORD_SET(O_F_BFAM))    THEN RETURN, *self.O_F_BFAM
   IF (KEYWORD_SET(HEURE_VERIF_FIX))    THEN RETURN, *self.HEURE_VERIF_FIX






struct = {$

     famille       :     *self.famille,$ 
     type_obs      :     *self.type_obs, $
     type_struct   :     *self.type_struct, $
     code_type     :     *self.code_type,$
     max_enreg     :     long(self.max_enreg), $
     liste_niveaux :     *self.liste_niveaux, $
     max_niveaux   :     fix(self.max_niveaux), $
     liste_vars    :     *self.liste_vars, $
     vars_ass:     *self.vars_ass, $
     code_elements :     *self.code_elements, $
     cons_conversion :     *self.cons_conversion, $
     fact_conversion :     *self.fact_conversion, $
     max_data      :     fix(self.max_data), $
     btype_element :     Long(*self.btype_element),$
     btype_marqueur:     Long(*self.btype_marqueur),$
     bits_mrq_rej  :     *self.bits_mrq_rej,$ ;
     bits_mrq_ass  :     *self.bits_mrq_ass,$ ;
     o_bfam        :     Long(*self.o_bfam), $
     o_p_bfam      :     Long(self.o_p_bfam),$
     o_a_bfam      :     Long(self.o_a_bfam),$
     liste_stat    :     *self.liste_stat,$
     max_stat      :     fix(self.max_stat),$
     type_verif_fix:     fix(self.type_verif_fix),$
     nombre_de_cas :     fix(self.nombre_de_cas),$

     level_element :     self.level_element,$
     canal_element :     self.canal_element,$
     src_file      :     self.src_file,$
     dst_file      :     self.dst_file,$
     stations:     self.stations,$
     set_liste     :     self.set_liste,$



     stn_miss      :     self.stn_miss , $
     flt_miss      :     self.flt_miss , $ ;
     int_def       :     self.int_def , $ ;
     int_rej       :     self.int_rej , $ 
     btype_o_a_o_p :     long(*self.BTYPE_O_A_O_P),$
     btype_o_f     :     long(*self.BTYPE_O_F),$
     o_f_bfam      :     *self.O_F_BFAM,$
     heure_verif_fix:    *self.HEURE_VERIF_FIX $

     }




RETURN, struct

END ;--------------------------------------------------------------------

Function arcad_obj_config::SetProperty, $         ; The name of the method.
     famille       = famille,$
     Type_obs      =     type_obs, $
     type_struct   =     type_struct, $
     Code_type     =     code_type,$
     Max_enreg     =     max_enreg, $
     Liste_niveaux =     liste_niveaux, $
     Max_niveaux   =     max_niveaux, $
     Liste_vars    =     liste_vars, $
     vars_ass      =     vars_ass, $
     Code_elements =     code_elements, $
     cons_conversion =     cons_conversion, $
     fact_conversion =     fact_conversion, $
     Max_data      =     max_data, $
     Btype_element =     btype_element,$
     Btype_marqueur=     btype_marqueur,$
     Btype_o_a_o_p =     btype_o_a_o_p ,$
     Btype_o_f     =     btype_o_f,$ 
     bits_mrq_rej  =     bits_mrq_rej,$ ;
     bits_mrq_ass  =     bits_mrq_ass,$ ;
     o_bfam        =     o_bfam, $
     o_p_bfam      =     o_p_bfam,$
     o_a_bfam      =     o_a_bfam,$
     o_f_bfam      =     o_f_bfam,$ 
     Liste_stat    =     liste_stat,$
     Max_stat      =     max_stat,$
     Type_verif_fix=     type_verif_fix,$
     heure_verif_fix=      heure_verif_fix,$
     Nombre_de_cas =     nombre_de_cas,$
     Level_element =     level_element,$          ;
     Canal_element =     canal_element ,$         ;

     Src_file      =     src_file,$               ;
     Dst_file      =     dst_file,$               ;
     stations      =     stations,$               ;
     Set_liste     =     set_liste,$               ;


     _Extra        =     extra


      Catch, TheError
      IF TheError NE 0 THEN BEGIN
         Catch, /Cancel
         print,'Function SetProperty'
         print, "Error, verifiez-vos parametres!!"
         RETURN, -1
      ENDIF

     IF N_Elements(famille) NE 0 THEN BEGIN
        data =loadconfig(famille) 
     s = Size(DATA);
     IF s[1] EQ 2 THEN BEGIN

      Catch, TheError
      IF TheError NE 0 THEN BEGIN
         Catch, /Cancel
         print, "Famille not found in config file"
         RETURN, -1
      ENDIF

     ENDIF


     *self.famille       = data.famille
     *self.type_obs      = data.type_obs
     *self.type_struct   = data.type_struct
     *self.code_type     = data.code_type
     self.max_enreg      = data.max_enreg
     *self.liste_niveaux = data.liste_niveaux

     self.max_niveaux    = N_Elements(data.liste_niveaux)

     *self.liste_vars    = data.liste_vars
     *self.vars_ass= data.vars_ass
     *self.code_elements = data.code_elements

     *self.cons_conversion = data.cons_conversion
     *self.fact_conversion = data.fact_conversion

     self.max_data       = N_Elements(data.code_elements) 

     *self.btype_element  = data.btype_element
     *self.btype_marqueur = data.btype_marqueur
     *self.btype_o_a_o_p = data.btype_o_a_o_p
     *self.btype_o_f      = data.btype_o_f
     *self.bits_mrq_rej  = data.bits_mrq_rej ;
     *self.bits_mrq_ass  = data.bits_mrq_ass ;
     *self.o_bfam         = data.o_bfam
     self.o_p_bfam       = data.o_p_bfam
     self.o_a_bfam       = data.o_a_bfam
     *self.o_f_bfam       = data.o_f_bfam
     *self.liste_stat    = data.liste_stat
     self.max_stat       = data.max_stat
     self.type_verif_fix = data.type_verif_fix
     *self.heure_verif_fix = data.heure_verif_fix
     self.nombre_de_cas  = data.nombre_de_cas
     self.level_element  = data.level_element         ;
     self.canal_element  = data.canal_element         ;

     self.src_file       = data.src_file              ;
     self.dst_file       = data.dst_file              ;
     self.stations       = data.stations              ;
     self.set_liste      = data.set_liste


     ENDIF

     IF N_Elements(famille) NE 0 THEN *self.famille            = famille
     IF N_Elements(type_obs) NE 0 THEN *self.type_obs          = type_obs
     IF N_Elements(type_struct) NE 0 THEN *self.type_struct    = type_struct
     IF N_Elements(code_type) NE 0 THEN *self.code_type        = code_type
     IF N_Elements(max_enreg) NE 0 THEN self.max_enreg         = max_enreg

     IF N_Elements(liste_niveaux) NE 0 THEN BEGIN
          *self.liste_niveaux= liste_niveaux
           self.max_niveaux  = N_Elements(liste_niveaux)
     ENDIF
     IF N_Elements(max_niveaux) NE 0 THEN self.max_niveaux=max_niveaux 

     IF N_Elements(liste_vars)     NE 0 THEN *self.liste_vars      = liste_vars
     IF N_Elements(vars_ass) NE 0 THEN *self.vars_ass   = vars_ass

     IF N_Elements(code_elements) NE 0 THEN BEGIN
          *self.code_elements= code_elements
          self.max_data      = N_Elements(code_elements)
     ENDIF
     IF N_Elements(cons_conversion) NE 0 THEN BEGIN
          *self.cons_conversion= cons_conversion
     ENDIF
     IF N_Elements(fact_conversion) NE 0 THEN BEGIN
          *self.fact_conversion= fact_conversion
     ENDIF

     IF N_Elements(max_data) NE 0 THEN self.max_data= max_data 

     IF N_Elements(btype_element) NE 0 THEN BEGIN
          *self.btype_element = btype_element
          *self.btype_marqueur= *self.btype_element + 6144 
     ENDIF
     IF N_Elements(btype_marqueur) NE 0 THEN $
        *self.btype_marqueur = btype_marqueur
 
     IF N_Elements(btype_o_a_o_p) NE 0 THEN $
        *self.btype_o_a_o_p = btype_o_a_o_p
     IF N_Elements( btype_o_f) NE 0 THEN $
        *self. btype_o_f =  btype_o_f
   

     IF N_Elements(bits_mrq_rej) NE 0 THEN $
        *self.bits_mrq_rej = bits_mrq_rej
     IF N_Elements(bits_mrq_ass) NE 0 THEN $
        *self.bits_mrq_ass = bits_mrq_ass

     IF N_Elements(o_bfam) NE 0 THEN *self.o_bfam               = o_bfam
     IF N_Elements(o_p_bfam) NE 0 THEN self.o_p_bfam           = o_p_bfam
     IF N_Elements(o_a_bfam) NE 0 THEN self.o_a_bfam           = o_a_bfam
     IF N_Elements(o_f_bfam) NE 0 THEN *self.o_f_bfam           = o_f_bfam 

     IF N_Elements(liste_stat) NE 0 THEN *self.liste_stat      = liste_stat
     IF N_Elements(max_stat) NE 0 THEN self.max_stat           = max_stat
     IF N_Elements(type_verif_fix) NE 0 THEN $
        self.type_verif_fix = type_verif_fix

     IF N_Elements(heure_verif_fix) NE 0 THEN $
       *self.heure_verif_fix = heure_verif_fix 

     IF N_Elements(nombre_de_cas) NE 0 THEN $
        self.nombre_de_cas  = nombre_de_cas

     IF N_Elements(level_element) NE 0 THEN $
        self.level_element  = level_element
     IF N_Elements(canal_element) NE 0 THEN $
        self.canal_element  = canal_element
     IF N_Elements(src_file) NE 0 THEN $
        self.src_file  = src_file
     IF N_Elements(dst_file) NE 0 THEN $
        self.dst_file  = dst_file
     IF N_Elements(stations) NE 0 THEN $
        self.stations  = stations
     IF N_Elements(set_liste) NE 0 THEN $
        self.set_liste  = set_liste





RETURN, 1

END ;--------------------------------------------------------------------


Function arcad_obj_config::Init, $        ; The name of the method.
     famille,$
     Type_obs      =     type_obs, $
     type_struct   =     type_struct, $
     Code_type     =     code_type,$
     Max_enreg     =     max_enreg, $
     Liste_niveaux =     liste_niveaux, $
     Max_niveaux   =     max_niveaux, $
     Liste_vars    =     liste_vars, $
     vars_ass      =     vars_ass, $
     Code_elements =     code_elements, $
     cons_conversion =     cons_conversion, $
     fact_conversion =     fact_conversion, $

     Max_data      =     max_data, $
     Btype_element =     btype_element,$
     Btype_marqueur=     btype_marqueur,$
     Btype_o_a_o_p =     btype_o_a_o_p ,$
     Btype_o_f     =     btype_o_f,$
     bits_mrq_rej  =     bits_mrq_rej,$ ; 
     bits_mrq_ass  =     bits_mrq_ass,$ ; 

     o_bfam        =     o_bfam, $
     o_p_bfam      =     o_p_bfam,$
     o_a_bfam      =     o_a_bfam,$
     o_f_bfam      =     o_f_bfam,$

     Liste_stat    =     liste_stat,$
     Max_stat      =     max_stat,$
     Type_verif_fix=     type_verif_fix,$
     heure_verif_fix=      heure_verif_fix,$

     Nombre_de_cas =     nombre_de_cas,$

     Level_element =     level_element,$          ;
     Canal_element =     canal_element ,$         ;

     Src_file      =     src_file,$               ;
     Dst_file      =     dst_file,$               ;
     stations=     stations,$               ;
     Set_liste     =     set_liste,$               ;

     _Extra        =     extra
     
      
     IF N_Elements(famille) EQ 0 THEN data =loadconfig('ua')
     IF N_Elements(famille) NE 0 THEN data =loadconfig(famille)


     s = Size(DATA);
     IF s[1] EQ 2 THEN BEGIN

      Catch, TheError
      IF TheError NE 0 THEN BEGIN
         Catch, /Cancel
         print, "Famille not found in config file"
         print, " Object is not created    "
         return,0
      ENDIF

     ENDIF




           self.famille= Ptr_New(data.famille)            ;
           self.type_obs=Ptr_New(data.type_obs)           ;
           self.type_struct=Ptr_New(data.type_struct)           ;
           self.code_type=Ptr_New(data.code_type)         ;

           self.max_enreg=data.max_enreg                  ;

           self.liste_niveaux=Ptr_New(data.liste_niveaux) ;
           self.max_niveaux=N_Elements(data.liste_niveaux)

           self.liste_vars=Ptr_New(data.liste_vars)       ;
           self.vars_ass=Ptr_New(data.vars_ass)       ;
           self.code_elements=Ptr_New(data.code_elements) ;

           self.cons_conversion=Ptr_New(data.cons_conversion) ;
           self.fact_conversion=Ptr_New(data.fact_conversion) ;

           self.max_data=N_Elements(*self.code_elements)                    ;

           self.btype_element=Ptr_New(data.btype_element)          ;
           self.btype_marqueur=Ptr_New(data.btype_marqueur)        ;

           self.btype_o_a_o_p=Ptr_New(data.btype_o_a_o_p)        ;
           self.btype_o_f=Ptr_New(data.btype_o_f)        ;

           self.bits_mrq_rej  =Ptr_New(data.bits_mrq_rej) ;
           self.bits_mrq_ass  =Ptr_New(data.bits_mrq_ass) ;

           self.o_bfam= Ptr_New(data.o_bfam)                       ;
           self.o_p_bfam=data.o_p_bfam                    ;
           self.o_a_bfam=data.o_a_bfam                    ;

           self.o_f_bfam=Ptr_new(data.o_f_bfam)                    ;

           self.liste_stat=Ptr_New(data.liste_stat)       ;
           self.max_stat=data.max_stat                    ;

           self.type_verif_fix= data.type_verif_fix       ;

           self.heure_verif_fix= Ptr_New(data.heure_verif_fix)       ;

           self.nombre_de_cas= data.nombre_de_cas


           self.level_element= data.level_element         ;
           self.canal_element= data.canal_element         ;
           self.src_file= data.src_file                   ;
           self.dst_file= data.dst_file                   ;
           self.stations= data.stations                   ;
           self.set_liste= data.set_liste 

           self.stn_miss = data.stn_miss  ;
           self.flt_miss = data.flt_miss  ;
           self.int_def  = data.int_def   ;
           self.int_rej  = data.int_rej   ;


           self.extra= Ptr_New(extra)                     ;

     IF N_Elements(famille) NE 0 THEN *self.famille            = famille
     IF N_Elements(type_obs) NE 0 THEN *self.type_obs          = type_obs
     IF N_Elements(type_struct) NE 0 THEN *self.type_struct    = type_struct
     IF N_Elements(code_type) NE 0 THEN *self.code_type        = code_type
     IF N_Elements(max_enreg) NE 0 THEN self.max_enreg         = max_enreg
     IF N_Elements(liste_niveaux) NE 0 THEN BEGIN
        *self.liste_niveaux= liste_niveaux
         self.max_niveaux  =N_Elements(liste_niveaux)
     ENDIF
     IF N_Elements(max_niveaux) NE 0 THEN self.max_niveaux      =max_niveaux 

     IF N_Elements(liste_vars)     NE 0 THEN *self.liste_vars     = liste_vars
     IF N_Elements(vars_ass) NE 0 THEN *self.vars_ass = vars_ass

     IF N_Elements(code_elements) NE 0 THEN BEGIN
          *self.code_elements= code_elements
          self.max_data      = N_Elements(code_elements)
     ENDIF
     IF N_Elements(cons_conversion) NE 0 THEN BEGIN
          *self.cons_conversion= cons_conversion
     ENDIF
     IF N_Elements(fact_conversion ) NE 0 THEN BEGIN
          *self.fact_conversion = fact_conversion 
     ENDIF

     IF N_Elements(max_data) NE 0 THEN self.max_data      =max_data 

     IF N_Elements(btype_element) NE 0 THEN *self.btype_element = btype_element
     IF N_Elements(btype_marqueur) NE 0 THEN $
        *self.btype_marqueur = btype_marqueur

     IF N_Elements(btype_o_a_o_p) NE 0 THEN $
        *self.btype_o_a_o_p = btype_o_a_o_p  
     IF N_Elements( btype_o_f) NE 0 THEN $
        *self. btype_o_f =  btype_o_f  

     IF N_Elements(bits_mrq_rej) NE 0 THEN $
        self.bits_mrq_rej = bits_mrq_rej
     IF N_Elements(bits_mrq_ass) NE 0 THEN $
        self.bits_mrq_ass = bits_mrq_ass

     IF N_Elements(o_bfam) NE 0 THEN *self.o_bfam               = o_bfam
     IF N_Elements(o_p_bfam) NE 0 THEN self.o_p_bfam           = o_p_bfam
     IF N_Elements(o_a_bfam) NE 0 THEN self.o_a_bfam           = o_a_bfam
     IF N_Elements(o_f_bfam) NE 0 THEN *self.o_f_bfam           = o_f_bfam
     IF N_Elements(liste_stat) NE 0 THEN *self.liste_stat      = liste_stat
     IF N_Elements(max_stat) NE 0 THEN self.max_stat           = max_stat
     IF N_Elements(type_verif_fix) NE 0 THEN $
        self.type_verif_fix = type_verif_fix

     IF N_Elements(heure_verif_fix) NE 0 THEN $
       *self.heure_verif_fix = heure_verif_fix

     IF N_Elements(nombre_de_cas) NE 0 THEN $
        self.nombre_de_cas  = nombre_de_cas

     IF N_Elements(level_element) NE 0 THEN $
        self.level_element  = level_element
     IF N_Elements(canal_element) NE 0 THEN $
        self.canal_element  = canal_element
     IF N_Elements(src_file) NE 0 THEN $
        self.src_file  = src_file
     IF N_Elements(dst_file) NE 0 THEN $
        self.dst_file  = dst_file

     IF N_Elements(stations) NE 0 THEN $
        self.stations  = stations
     IF N_Elements(set_liste) NE 0 THEN $
        self.set_liste  = set_liste



RETURN, 1

END ;--------------------------------------------------------------------

PRO arcad_obj_config::Cleanup         ; The name of the method.



           Ptr_Free, self.famille
           Ptr_Free, self.type_obs
           Ptr_Free, self.type_struct
           Ptr_Free, self.code_type
           Ptr_Free, self.liste_niveaux
           Ptr_Free, self.liste_vars
           Ptr_Free, self.vars_ass
           Ptr_Free, self.code_elements
           Ptr_Free, self.cons_conversion
           Ptr_Free, self.fact_conversion
           Ptr_Free, self.liste_stat
           Ptr_Free, self.extra
           Ptr_Free, self.bits_mrq_rej 
           Ptr_Free, self.bits_mrq_ass 
           Ptr_Free, self.o_bfam 
           Ptr_Free, self.heure_verif_fix 
           Ptr_Free, self.o_f_bfam 

           Ptr_Free,self.btype_element
           Ptr_Free,self.btype_marqueur

           Ptr_Free,self.btype_o_a_o_p
           Ptr_Free,self.btype_o_f



END ;--------------------------------------------------------------------



PRO arcad_obj_config__Define

; The definition of the BOXCONFIG object class.

struct = { arcad_obj_config, $               ;
           famille: Ptr_New(), $      ; 
           type_obs:Ptr_New(), $      ; 
           type_struct:Ptr_New(), $      ; 
           code_type:Ptr_New(), $     ;

           max_enreg:0L, $             ;

           liste_niveaux:Ptr_New(), $ ;
           max_niveaux:0, $           ;

           liste_vars:Ptr_New(), $    ;
           vars_ass:Ptr_New(), $    ;
           code_elements:Ptr_New(), $ ;
           cons_conversion:Ptr_New(), $ ;
           fact_conversion:Ptr_New(), $ ;
           max_data:0, $              ; 

           btype_element:Ptr_New(), $        ; 
           btype_marqueur:Ptr_New(), $       ;

           btype_o_a_o_p:Ptr_New(), $       ; new
           btype_o_f:Ptr_New(), $           ; new

           bits_mrq_rej  :Ptr_New(),$ ;
           bits_mrq_ass  :Ptr_New(),$ ;

           o_bfam: Ptr_New(), $              ;
           o_p_bfam:0L, $             ;
           o_a_bfam:0L, $             ;

           o_f_bfam:Ptr_new(), $             ; new

           liste_stat:Ptr_New(),$     ; 
           max_stat:0, $              ;

           type_verif_fix: 0, $       ;

           heure_verif_fix   : Ptr_New(),$  ; new

           nombre_de_cas: 0, $

           level_element     : 0L,$;
           canal_element     : 0L,$   ;

           src_file          : "",$   ;
           dst_file          : "",$   ;
           stations    : "",$   ;
           set_liste         : 0,$    ;

           stn_miss          : "",$   ;
           flt_miss          : 0.0, $ ;
           int_def           : 0, $   ;
           int_rej           : 0, $ 



           extra: Ptr_New() $         ; 
          }
END ;--------------------------------------------------------------------

