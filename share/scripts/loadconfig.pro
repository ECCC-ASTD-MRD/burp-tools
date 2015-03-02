;#CVS: RCSId  : loadconfig.pro,v 1.2 2002/11/26 15:22:06 afsdjmo Exp
;#CVS: Version: cfg-1-0
FUNCTION LOADCONFIG, famille 

IF N_Elements(famille) EQ 0 THEN BEGIN
   Message,"Data set famille  is required in LOADCONFIG."
   RETURN, -1
ENDIF

data=-1

IF (famille eq 'ai') THEN BEGIN

   data = {$
        famille           : 'ai',$                                  ;
        type_obs          : 'airep     ',$                          ;
        type_struct       : 'UNI',$                          ;
        code_type         : [128],$                                 ;

        max_enreg         : 2000,$                                  ;

        liste_niveaux     : [-1],$                                   ;
        max_niveaux       : 1,$                                     ;

        vars_ass          : [1   ,  1 , 0  , 1  , 1  ],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT'],$            ;
;        code_elements     : [011001, 011002, 0010194, 012192, 12001],$
        code_elements     : [011003, 011004, 0010194, 012192, 12001],$
        cons_conversion   : [0.,0.,0.,0.,0.],$
        fact_conversion   : [1.,1.,1.,1.,1.],$
        max_data          : 5,$                                     ;

        btype_element     : [1120,1136],$                                  ;
        btype_marqueur    : [7264,7280],$                                  ;1120 +6144
        btype_o_a_o_p     : [1130,1146],$                               ;
        btype_o_f         : [1322,1338],$                               ;

        bits_mrq_rej      : [9,18,19],$                        ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                                     ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 7004L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$  ;
        dst_file          : "xx.dat",$                                    ;
        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_omm_jun97.dat",$   ;
        set_liste         : 0,$                        ;

        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ; 
        INT_REJ           : 0 }

ENDIF 

IF (famille eq 'ac') THEN BEGIN

   data = {$
        famille           : 'ac',$                                  ;
        type_obs          : 'acars     ',$                          ;
        type_struct       : 'UNI',$                          ;
        code_type         : [42,157],$                                 ;

        max_enreg         : 32000,$                                  ;

        liste_niveaux     : [-1],$                                   ;
        max_niveaux       : 1,$                                     ;

        vars_ass          : [1   ,  1 , 0  , 1  , 1  ],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT'],$            ;
;        code_elements     : [011001, 011002, 0010194, 012192, 12001],$
        code_elements     : [011003, 011004, 0010194, 012192, 12001],$
        cons_conversion   : [0.,0.,0.,0.,0.],$
        fact_conversion   : [1.,1.,1.,1.,1.],$
        max_data          : 5,$                                     ;

        btype_element     : [1120,1136],$                                  ;
        btype_marqueur    : [7264,7280],$                                  ;1120 +6144
        btype_o_a_o_p     : [1130,1146],$                               ;
        btype_o_f         : [1322,1338],$                               ;


        bits_mrq_rej      : [9,18,19],$                        ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                                     ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 7004L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$
        dst_file          : "xx.dat",$                                    ;
        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_omm_jun97.dat",$   ;
        set_liste         : 0,$                        ;

        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }

ENDIF

IF (famille eq 'ai3') THEN BEGIN

   data = {$
        famille           : 'ai3',$                                  ;
        type_obs          : 'acars     ',$                           ;
        type_struct       : 'UNI',$                                  ;
        code_type         : [42,128,157,177],$                       ;

        max_enreg         : 32000,$                                  ;

        liste_niveaux     : [-1],$                                   ;
        max_niveaux       : 1,$                                     ;

        vars_ass          : [1   ,  1 , 0  , 1  , 1  ],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT'],$            ;
;        code_elements     : [011001, 011002, 0010194, 012192, 12001],$
        code_elements     : [011003, 011004, 0010194, 012192, 12001],$
        cons_conversion   : [0.,0.,0.,0.,0.],$
        fact_conversion   : [1.,1.,1.,1.,1.],$
        max_data          : 5,$                                     ;

        btype_element     : [1120,1136],$                                  ;
        btype_marqueur    : [7264,7280],$                                  ;1120 +6144
        btype_o_a_o_p     : [1130,1146],$                               ;
        btype_o_f         : [1322,1338],$                               ;


        bits_mrq_rej      : [9,18,19],$                        ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                                     ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 7004L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$
        dst_file          : "xx.dat",$                                    ;
        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_omm_jun97.dat",$   ;
        set_liste         : 0,$                        ;

        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }

ENDIF

IF (famille eq 'sw') THEN BEGIN

   data = {$
        famille           : 'sw',$                                  ;
        type_obs          : 'satwnd    ',$                          ;
        type_struct       : 'UNI',$                          ;
        code_type         : [88,188],$                                 ;

        max_enreg         : 20000,$                                  ;

        liste_niveaux     : [-1],$                                   ;
        max_niveaux       : 1,$                                     ;

        vars_ass          : [1   ,  1 , 0  , 1  , 1  ],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT'],$            ;
;        code_elements     : [011001, 011002, 0010194, 012192, 12001],$
        code_elements     : [011003, 011004, 0010194, 012192, 12001],$
        cons_conversion   : [0.,0.,0.,0.,0.],$
        fact_conversion   : [1.,1.,1.,1.,1.],$
        max_data          : 5,$                                     ;

        btype_element     : [1120,1136],$                                  ;
        btype_marqueur    : [7264,7280],$                                  ;1120 +6144
        btype_o_a_o_p     : [1130,1146],$                               ;
        btype_o_f         : [1322,1338],$                               ;


        bits_mrq_rej      : [9,18,19],$                        ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                                     ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 7004L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$
        dst_file          : "xx.dat",$                                    ;
        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_omm_jun97.dat",$   ;
        set_liste         : 0,$                        ;


        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }

ENDIF

IF (famille eq 'hu') THEN BEGIN

   data = {$
        famille           : 'hu',$                                  ;
        type_obs          : 'humsat    ',$                          ;
        type_struct       : 'MULTI',$                          ;
        code_type         : [158],$                                 ;

        max_enreg         : 6000,$                                  ;

        liste_niveaux     : [1000.0,925.0,850.0,700.0,500.0,400.0,$
                            300.0,250.0,200.0,150.0,$
                            100.0,70.0,50.0,30.0,20.0,10.0],$        ;
        max_niveaux       : 16,$                                     ;

        vars_ass          : [0   ,  0 , 0  , 1  , 0  ],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT'],$            ;
;        code_elements     : [011001, 011002, 0010194, 012192, 12001],$
        code_elements     : [011003, 011004, 0010194, 012192, 12001],$
        cons_conversion   : [0.,0.,0.,0.,0.],$
        fact_conversion   : [1.,1.,1.,1.,1.],$
        max_data          : 5,$                                     ;

        btype_element     : [9312,9328],$                                  ;
        btype_marqueur    : [15456,15472],$                                  ;1120 +6144
        btype_o_a_o_p     : [9322,9338],$                               ;
        btype_o_f         : [9514,9530],$                               ;


        bits_mrq_rej      : [9,18,19],$                        ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                                     ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 7004L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$
        dst_file          : "xx.dat",$                                    ;
        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_omm_jun97.dat",$   ;
        set_liste         : 0,$                        ;


        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }

ENDIF


IF (famille eq 'st') THEN BEGIN

   data = {$
        famille           : 'st',$                                  ;
        type_obs          : 'satem     ' ,$                         ; 
        type_struct       : 'MULTI',$                          ;
        code_type         : [86],$                                  ;

        max_enreg         : 1500,$                                  ;

        liste_niveaux     : [ 700.0, 500.0, 300.0, 100.0, 50.0,  30.0, 10.0 ],$   ;                        ;
        max_niveaux       : 7,$                                     ;

        vars_ass          : [1],$            ;
        liste_vars        : ['DZ'],$                                ;
        code_elements     : [010192],$                              ;
        cons_conversion   : [0.],$
        fact_conversion   : [1.],$
        max_data          : 1,$                                     ;

        btype_element     : [9312,9313],$                                  ;
        btype_marqueur    : [15456,15457],$                                 ;9312 +6144
        btype_o_a_o_p     : [9322,9323],$                               ;
        btype_o_f         : [9322,9323],$                               ;

        bits_mrq_rej      : [9,18,19],$                        ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                                     ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 7004L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$                                    ;
        dst_file          : "xx.dat",$    ;

        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_omm_jun97.dat",$   ;
        set_liste         : 0,$                        ;

        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }



ENDIF 

IF (famille eq 'ua') THEN BEGIN

   data = {$
        famille           : 'ua',$                                  ;
        type_obs          : 'radiosonde',$                          ;
        type_struct       : 'MULTI',$                          ;
        code_type         : [32,33,34,35,36,37,38,$
                            135,136,137,138,139,$
                            140,141,142,159,160,161,162],$ ;

        max_enreg         : 800,$                                  ;

        liste_niveaux     : [1000.0,925.0,850.0,700.0,500.0,400.0,$
                            300.0,250.0,200.0,150.0,$
                            100.0,70.0,50.0,30.0,20.0,10.0],$        ;
        max_niveaux       : 16,$                                     ;

        vars_ass          : [1   ,  1 , 0  , 1  , 1  ],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT'],$            ;
;        code_elements     : [011001, 011002, 0010194, 012192, 12001],$
        code_elements     : [011003, 011004, 0010194, 012192, 12001],$
        cons_conversion   : [0.  , 0. , 0. , 0.,-273.15],$
        fact_conversion   : [1.  , 1. , 0.1, 1.,   1.  ],$
        max_data          : 5,$                                     ;

        btype_element     : [9312,9328],$                                  ;
        btype_marqueur    : [15456,15472],$                                 ;9312 +6144
        btype_o_a_o_p     : [9322,9338],$                               ;
        btype_o_f         : [9514,9530],$                               ;

        bits_mrq_rej      : [9,18,19],$                        ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                                     ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 7004L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$
        dst_file          : "xx.dat",$

        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_omm_jun97.dat",$   ;
        set_liste         : 0,$                        ;


        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }

ENDIF

IF (famille eq 'ua27') THEN BEGIN

   data = {$
        famille           : 'ua27',$                                  ;
        type_obs          : 'radiosonde',$                          ;
        type_struct       : 'MULTI',$                          ;
        code_type         : [32,33,34,35,36,37,38,$
                            135,136,137,138,139,$
                            140,141,142,159,160,161,162],$ ;

        max_enreg         : 800,$                                  ;

        liste_niveaux     : [1025.0,1000.0,975.0,950.0,925.0,900.0,$
                            850.0,800.0,750.0,700.0,$
                            650.0,600.0,550.0,500.0,450.0,400.0,$
                            350.0,300.0,250.0,200.0,150.0,100.0,$
                            70.0,50.0,30.0,20.0,10.0],$        ;
        max_niveaux       : 27,$                               ;

        vars_ass          : [1   ,  1 , 0  , 1  , 1  ],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT'],$            ;
;        code_elements     : [011001, 011002, 0010194, 012192, 12001],$
        code_elements     : [011003, 011004, 0010194, 012192, 12001],$
        cons_conversion   : [0.  , 0. , 0. , 0.,-273.15],$
        fact_conversion   : [1.  , 1. , 0.1, 1.,   1.  ],$
        max_data          : 5,$                                     ;

        btype_element     : [9312,9328],$                                  ;
        btype_marqueur    : [15456,15472],$                                 ;9312 +6144
        btype_o_a_o_p     : [9322,9338],$                               ;
        btype_o_f         : [9514,9530],$                               ;

        bits_mrq_rej      : [9,18,19],$                        ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                                     ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 7004L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$
        dst_file          : "xx.dat",$

        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_omm_jun97.dat",$   ;
        set_liste         : 0,$                        ;


        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }

ENDIF

IF (famille eq 'sf') THEN BEGIN

   data = {$
        famille           : 'sf',$                                  ;
        type_obs          : 'surface   ',$                          ;
        type_struct       : 'UNI',$                          ;
        code_type         : [12,13,14,18,145,146,147],$          ;

        max_enreg         : 10000,$                                  ;

        liste_niveaux     : [-1],$                                   ;
        max_niveaux       : 1,$                                     ;

        vars_ass          : [1   ,  1 , 0  , 1  , 1  , 1  , 1],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT','PN','P0'],$            ;
        code_elements     : [011215, 011216, 010194, 012203, 12004,010051,010004],$
        cons_conversion   : [0.  , 0. , 0. , 0.,-273.15, 0.,  0.  ],$
        fact_conversion   : [1.  , 1. , 0.1, 1.,   1.  , 0.01,0.01],$;
        max_data          : 7,$                                     ;

        btype_element     : [96  ,112 ,128 ,112],$      ;GLB/UA,REG/UA,GLB/SFC,REG/SFC
        btype_marqueur    : [6240,6256,6272,6273],$     ;btype_element+6144
        btype_o_a_o_p     : [106 ,122 ,138 ,139],$                               ;
        btype_o_f         : [1322,1338,298 ,299],$                               ;


        bits_mrq_rej      : [9,18,19],$                   ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 010004L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$
        dst_file          : "xx.dat",$

        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_sm1999.dat",$   ;
        set_liste         : 0,$                        ;

        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }

ENDIF

IF (famille eq 'sf-raobs') THEN BEGIN

   data = {$
        famille           : 'sf-raobs',$                            ;
        type_obs          : 'surface   ',$                          ;
        type_struct       : 'UNI',$                                 ;
        code_type         : [32,35,135,136,137,138,33,36,139,$
                            140,141,142,34,37,38,159,160,161,162],$ ;

        max_enreg         : 800,$                                  ;

        liste_niveaux     : [-1],$                                  ;
        max_niveaux       : 1,$                                     ;

        vars_ass          : [1   ,  1 , 0  , 1  , 1  , 1  , 1],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT','PN','P0'],$            ;
        code_elements     : [011215, 011216, 010194, 012203, 12004,010051,010004],$;        
        max_data          : 7,$                                     ;
        cons_conversion   : [0.,0.,0. ,0.,-273.15,0.  ,0. ],$
        fact_conversion   : [1.,1.,0.1,1.,   1.  ,0.01,0.01],$

        btype_element     : [98,114],$                                  ;
        btype_marqueur    : [6242,6258],$                              ; 98+6144
        btype_o_a_o_p     : [106,122],$                               ;
        btype_o_f         : [106,107],$                               ;


        bits_mrq_rej      : [9,18,19],$                        ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 010004L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$
        dst_file          : "xx.dat",$

        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_omm1997.dat",$   ;
        set_liste         : 0,$                        ;

        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }

ENDIF

IF (famille eq 'sf2') THEN BEGIN

   data = {$
        famille           : 'sf',$                                  ;
        type_obs          : 'surface   ',$                          ;
        type_struct       : 'UNI',$                          ;
        code_type         : [12,13,14,18,145,146,147],$          ;

        max_enreg         : 6000,$                                  ;

        liste_niveaux     : [-1],$                                   ;
        max_niveaux       : 1,$                                     ;

        vars_ass          : [1   ,  1 , 0  , 1  , 1  , 1  , 1],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT','PN','P0'],$            ;
        code_elements     : [011011, 011012, 010194, 012192, 12004,010051,010004],$;
        max_data          : 7,$                                     ;
        cons_conversion   : [0.,0.,0.,0.,0.,0.,0.],$
        fact_conversion   : [1.,1.,1.,1.,1.,1.,1.],$

        btype_element     : [128,129],$                                  ;
        btype_marqueur    : [6272,6273],$                              ; 128+6144
        btype_o_a_o_p     : [138,139],$                               ;
        btype_o_f         : [298,299],$                               ;


        bits_mrq_rej      : [9,18,19],$                        ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$
      ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 010004L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$
        dst_file          : "xx.dat",$

        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_omm_jun97.dat",$   ;
        set_liste         : 0,$                        ;

        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }

ENDIF

IF (famille eq 'go') THEN BEGIN

   data = {$
        famille           : 'go',$                                   ;
        type_obs          : 'goes      ',$                           ;
        type_struct       : 'MULTI',$                                ;
        code_type         : 180,$                                    ;

        max_enreg         : 20000,$                                  ;

        liste_niveaux     : [19,20,21,22],$                          ;
        max_niveaux       : 4,$                                      ;

        vars_ass          : 1,$                                      ;
        liste_vars        : 'BT',$                                   ;
        code_elements     : 012163,$                                 ;
        cons_conversion   : 0.,$                                     ;
        fact_conversion   : 100.,$                                   ;
        max_data          : 1,$                                      ;

        btype_element     : [9312,9328],$                            ;
        btype_marqueur    : [15456,15472],$                          ;1120 +6144
        btype_o_a_o_p     : [9322,9338],$                            ;
        btype_o_f         : [9514,9530],$                            ;


        bits_mrq_rej      : [9,18,19],$                              ;Default,3DVAR
        bits_mrq_ass      : [12],$                                   ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                 ;
        o_p_bfam          : 14,$                                     ;
        o_a_bfam          : 12,$                                     ;
        o_f_bfam          : 64,$                                     ;

        liste_stat        : ['moy','rms'],$                          ;
        max_stat          : 2,$                                      ;

        type_verif_fix    : 0,$                                      ;
        heure_verif_fix   : [0,12,24,36,48],$                        ;
        nombre_de_cas     : 1,$                                      ;
 
        level_element     : 5042L,$                                  ;
        canal_element     : 0L,$                                     ;

        src_file          : "",$                                     ;
        dst_file          : "xx.dat",$                               ;
        stations          : "",$                                     ;
        set_liste         : 0,$                                      ;


        STN_MISS          : 'NOSTN       ',$                         ;
        FLT_MISS          : -999.0        , $                        ;
        INT_DEF           : 0, $                                     ;
        INT_REJ           : 0 }

ENDIF

IF (famille eq 'pr') THEN BEGIN

   data = {$
        famille           : 'pr',$                                  ;
        type_obs          : 'profileur ',$                          ;
        type_struct       : 'MULTI',$                               ;
        code_type         : [130],$                                 ;
        max_enreg         : 800,$                                   ;
        liste_niveaux     : [500,750,1000,1250,1500,1750,2000,2250,$
                            2500,2750,3000,3250,3500,3750,4000,4250,$
                            4500,4750,5000,5250,5500,5750,6000,6250,$
                            6500,6750,7000,7250,7500,7750,8000,8250,$
                            8500,8750,9000,9250,9500,9750,10000,10250,$
                            10500,10750,11000,11250,11500,11750,$
                            12000,12250,12500,12750,13000,13250,$
                            13500,13750,14000,14250,14500,14750,15000,15250,$
                            15500,15750,16000],$                    ;
        max_niveaux       : 63,$                                    ;
        vars_ass          : [0   ,  0 , 0  , 0  , 0  ],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT'],$            ;
        code_elements     : [011003, 011004, 0010194, 012192, 12001],$
        cons_conversion   : [0.  , 0. , 0. , 0., 0.],$
        fact_conversion   : [1.  , 1. , 1. , 1., 1.],$
        max_data          : 5,$                                     ;

        btype_element     : [9312,9328],$                           ;
        btype_marqueur    : [15456,15472],$                         ;9312 +6144
        btype_o_a_o_p     : [9322,9338],$                           ;
        btype_o_f         : [9514,9530],$                           ;

        bits_mrq_rej      : [9,18,19],$                             ;Default,3DVAR
        bits_mrq_ass      : [12],$                                  ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                                     ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 7006L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$
        dst_file          : "xx.dat",$

        stations    : "/home/dormrb02/env_univ/arcad/env/fichiers_omm/liste_omm_jun97.dat",$   ;
        set_liste         : 0,$                        ;


        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }

ENDIF
IF (famille eq 'to') THEN BEGIN

   data = {$
        famille           : 'to',$                                  ;
        type_obs          : 'tovs      ',$                          ;
        type_struct       : 'MULTI',$                          ;
        code_type         : [164],$                                 ;

        max_enreg         : 32000,$                                  ;

        liste_niveaux     : [28.0, 29.0, 30.0, 31.0, 32.0, 33.0,$
                             34.0, 35.0, 36.0, 37.0, 38.0, 39.0,$
                             40.0, 41.0, 42.0],$        ;
        max_niveaux       : 15,$                                     ;

        vars_ass          : [0   ,  0 , 0  , 0  , 1  ],$            ;
        liste_vars        : ['UU','VV','GZ','ES','TT'],$            ;
;        code_elements     : [011001, 011002, 0010194, 012192, 12001],$
        code_elements     : [011003, 011004, 0010194, 012192, 12163],$
        cons_conversion   : [0.,0.,0.,0.,0.],$
        fact_conversion   : [1.,1.,1.,1.,1.],$
        max_data          : 5,$                                     ;

        btype_element     : [9312,9328],$                                  ;
        btype_marqueur    : [15456,15472],$                                  ;1120 +6144
        btype_o_a_o_p     : [9322,9338],$                               ;
        btype_o_f         : [9514,9530],$                               ;


        bits_mrq_rej      : [9,18,19],$                        ;Default,3DVAR
        bits_mrq_ass      : [12],$                        ;Default,3DVAR

        o_bfam            : [0,8,9,10,11,12,13,14],$                                     ;
        o_p_bfam          : 14,$                                    ;
        o_a_bfam          : 12,$                                    ;
        o_f_bfam          : [64],$                                    ;

        liste_stat        : ['moy','rms'],$                         ;
        max_stat          : 2,$                                     ;

        type_verif_fix    : 0,$                                     ;
        heure_verif_fix   : [0,12,24,36,48],$                                    ;
        nombre_de_cas     : 1,$                                     ;

        level_element     : 2150L,$                                 ;
        canal_element     : 0L,$                                    ;

        src_file          : "",$
        dst_file          : "xx.dat",$                                    ;

        stations          : "",$   ;
        set_liste         : 0,$                        ;


        STN_MISS          : 'NOSTN       ',$                        ;
        FLT_MISS          : -999.0        , $                       ;
        INT_DEF           : 0, $                                    ;
        INT_REJ           : 0 }

ENDIF


RETURN, data

END 

