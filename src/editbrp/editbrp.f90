!  PROGRAMME EDITBRP - EDITION DE FICHIERS BURP 
!
      PROGRAM EDITBRP
      use app
      IMPLICIT NONE 
#include "burp-tools_build_info.h"

!AUTEURS                         DATE    VERSION
!VERSION ORIGINALE  Y BOURASSA  FEV. 91  1.0 
!REVISION 001       "    "      OCT. 91  1.1 VERSION NEC
!         002       "    "      DEC. 91  1.2 QDFERR CORRECTION DES DATES
!                                            VS ET REMPLACE DANS SEQUENCE D'APPEL
!         003       "    "      DEC. 91  1.3 FUSUION DU FICHIER DE SORTIE.
!                                            JUSQU'A 10 SOURCES DANS SEQUENCE D'APPEL 
!                                            QLXINX POUR NEC
!         004       "    "      JAN. 92  1.4 CONTROLE DES MESSAGES VIA MRFOPC
!         005       "    "      FEV. 92  1.5 BUG DANS SBRT. BRPCOPI: APPEL A LOW2UP AVANT
!                                            LES APPELS A IOPDATM ET MSGLVL
!         006       "    "      FEV. 93  2.0 PERMET AUX LONGITUDES D'ALLER EN DECROISSANT
!                                            ACCEPTE ENSEMBLE D'EXCLURES SEULEMENT. 
!                                            MODE STATISTIQUE. MODE EXPRESS. cle echo
!         007       M. Lepine   Dec 94   2.1 Bug fix NARG(I) au lieu de NARG(J)
!                                            IAND au lieu de AND, ISHIFT ...
!         008       Y BOURASSA  AVR 95   2.2 Permet de continuer si BURPCOPI tente de
!                                            copier d'un fichier source inexistant
!         009       "    "      Mai 95   2.3 ENLEVE REFERENRES A 1950
!         010       M. Lepine   Aout 96  2.4 Bug fix traitement des dates dans opdate
!         010       M. Lepine   Avril 97 2.5 Reload avec librmnx32stack.a
!         011       M. Lepine   Avril 98 2.6 Conversion pour l'an 2000
!         012       M. Lepine   Mai 98   2.7 Ajout des fichiers sequentiels
!         013       M. Lepine   Sep 98   2.8 Rechargement (limite du nombre
!                                            d'enregistrements a 256k)
!         014       M. Lepine   Oct 98   2.9 Rechargement avec dernier release
!         015       M. Lepine   Mars 99  3.0 Mods dans fnom filename scratch
!         016       M. Lepine   Juil 99  3.1 Bug fix calcul des minutes (DATC)
!         017       M. Lepine   Mars 2000 3.2 Menage des if defined
!         018       M. Lepine   Oct  2005 3.3 Bug fix minute dans cmcdate
!         019       M. Lepine   Oct  2007 3.4 Reload avec librmn_009
!         020       M. Lepine   Mai  2010 3.5 Ajout des directives prefix(s,d) pour allonger les noms de fichiers
!         021       M. Lepine   Jan  2013 3.6 Changement de la longueur de def1,2 (4096, nom > 128), reload 013
!         022       M. Lepine   Mars 2014 3.7 Reload avec librmn_014
!         023       M. Valin   Avril 2014 3.8 Remplacement de memoirh dans barf
!         024       J.W.Blezius Juin 2014 3.9 Ajouter 'implicit none' a tous les routines
!         022       D. Bouhemhem   Dec. 2014 3.10 Reload avec librmn_015.1
!         023       M. Lepine   Fev. 2015 3.11 Reload avec librmn_015.2
!         024       M. Lepine   Juil 2016 3.12 Augmenter la limite des fichiers de 10 a 125
!         024       D. Bouhemhem   Avril 2022  Incrementer la version a 3.14.0 pour EXDB
!
!OBJET(EDITBRP)
!         - EDITION DE FICHIER BURP, ON SE SERT DE L'UTILITAIRE CCARD
!           POUR RAMASSER LES PARAMETRES SUR L'ENONCE D'EXECUTION,
!           QUI DOIT AVOIR LA FORME SUIVANTE: 
!
!           EDITBRP -D DESINATION -F FUSION -I DIRECTIVES -L SORTIES
!                   -N -V -M -R -VS -C -S SOURCE -S SOURCE -S SOURCE ...
!           FICHIERS 25@34=SOURCE 2=FUSION 3=DESTINATION 5=STDIN 6=STDOUT
!
#include "fiches.cdk"
!     - NP         - NOMBRE D'ARGUMENTS D'UNE SUBROUTINE
!     - COPIES     - NOMBRE D'AJOUTS AU FICHIER DESTINATION EN USAGE
!     - RENDUA     - NOMBRE DE COPIES CALCULE PAR EDITBRP
!     - LIMITE     - NOMBRE MAXIMUM DE COPIES PERMIS PAR L'USAGER
!     - NFS        -    "   DE FICHIERS SOURCES PASSES VIA APPEL
!     - NFO        -    "    "     "       "    OUVERTS
!     - SOURCES    - TABLEAU DES NUMERO DE FICHIER SOURCE
!     - VS         - =.TRUE. DESIRE UN VOIR DU FICHIER SOURCE    
!     - VD         -    "       "    "    "   "   "    "    DESTINATION
!     - OUVF       -    "       "    FUSION  "     "
!     - OUVD       -    "       "    DESTIN  "     "
!     - REMPLAC    -    "    " REMPLACE UN ENREG. EXIXTANT DANS DESTINATION
!     - ESAIS      -    "    " UNE TENTATIVE DE COPIE A ETE FAITE.
!     - BOX        -    "    " LA CLE NOBOX PAS DANS LA SEQUENSE D'APPEL
!     - STATS      -    "    " EN MODE STATISTIQUES
!     - INFORM     -    "    " EN MODE INFORMATIF
#include "maxprms.cdk"
!     - NMD        - MAXIMUM DE DESIRES/EXCLURES
!     - NCW        - NOMBRE DE CAR/MOTS.
!     - WL         - 1         ou 2
!     - LIN128     - '(16A8)'  ou '(32A4)'.
!     - CAR9       - '(A8,A1)' ou '(2A4,A1)'.
!     - CAR8       - '(A8)'    ou '(2A4)'.
#include "desrs.cdk"
!     - JOURS      - PERIODE A UTILISER PAR DESIRE/EXCLURE
!     - NREQ       - NOMBRE DE DIRECTIVES DESIRE/EXCLURE RENCONTREES
!     - SAUV       -    "    "     "      A CONSERVER APRES COPIE
!     - NEXC       -    "    "     "      EXCLURE 
!     - DESEXC(I)  - =0 (POUR EXCLURE),    =-1 (POUR DESIRE)
!     - SATISF(I)  - 0 = DIRECTIVE INSATISFAITE
!     - NARG(I)    - NOMBRE D'ARGUMENTS DE LA DIRECTIVE DESIRE/EXCLURE(I)
!     - FENETRE    - DUREE EN MINUTE DE LA FENETRE A ASSOCIER AUX DIRECTIVES
!                    DATE CMCDATE OPDATE QUI DETERMINENT UNE DATE COMMUNE
!     - REQ        - TABLEAU DES DESIRES/EXCLURES DE L'USAGER.
!     - STNS       - IDENTIFICATEUR DE STATIONS DES DESIRES/EXCLURES
!     - EXPRESS    - .true. si on veut une copie integrale du fichier
!     - ECHO       - ECRIT MESSAGES D'ECRITURE
#include "char.cdk"
!     - ID         - LISTE DES STNID LUS DES DIRECTIVES
!     - NS         - DN DU FICHIER SOURCE EN USAGE
!     - ND         -  "  "    "    DESTINATION
!     - NF         -  "  "    "    DES FUSIONS
!     - DEF7       - DEF1(7)

      EXTERNAL BRPCOPI, FERMBS, QLXINS, EXDB, OUVREBS, CCARD, NEWFUS, &
               BDESIRE, FERMBD, JULM, OUVREBD, EXFIN, NEWSRC,         &
               CMCDATE, READLX, OPDATE, FNOM, OUVREBF, SPOOL, NEWDST, &
               RESTDEZ, MRFOPC, FERMBF, DATC, BEXCLUR, BARF,  QLXINX, &
               PREFIXS, PREFIXD

      INTEGER  MOIN1, MOIN2, DATC, FNOM, BRPCOPI, I, J,               &
               MOIN3, MOIN4, OPDATE, JULM, EXDB, MRFOPC, EXFIN

      INTEGER DUMY, KERR
      INTEGER MAXCLE
      PARAMETER (MAXCLE=137)
      LOGICAL VRAI,FAUX
      DATA VRAI /.TRUE./
      DATA FAUX /.FALSE./
      CHARACTER (len=2)    KLE(MAXCLE)
      CHARACTER (len=4096) DEF1(MAXCLE), DEF2(MAXCLE)

!     KLE  = CLES DE LA SEQUENCE D'APPEL.
      DATA KLE /'V', 'd.', 'f.', 'N', 'i.', 'l.', 'M', 'VS', 'C', 'R',  'X', 'DS', 125*'s.'/
!     DEF1 = VALEURE DES CLES PAR DEFAUT
      DATA DEF1/'NON',' ',' ','OUI','$IN','$OUT','ERROR','NON','-1', 'NON', 'NON', 'NON', 125*' '/
!     DEF2 = VALEUR DES CLES SI PRESENTES
      DATA DEF2/'OUI',' ',' ','NON','$IN','$OUT','INFORMATIF', 'OUI', '-1', 'OUI', 'OUI', 'OUI', 125*' '/

      DATA    OUVD,    OUVF,   ESAIS, EXPRESS,  STATS &
         / .FALSE., .FALSE., .FALSE., .FALSE., .FALSE./
 
      DATA JOURS, DESEXC, SATISF, COPIES, NREQ, NEXC, NFS, NFO &
         /   4*0,  NMD*0,  NMD*0,      0,    0,    0,   0,   0/

      DATA MOIN1, MOIN2, MOIN3, MOIN4, FENETRE, SAUV &
         /    -1,    -2,    -3,    -4,       0,    0/

      DO I=1,125
        SOURCES(I) = I+20
      ENDDO
!     INTERPRETATION DES CLES DE LA SEQUENCE D'APPEL.
      I = -111
      CALL CCARD(KLE, DEF2, DEF1, MAXCLE, I)
      DEF7    = DEF1(7)
      I       = App_LogLevel(DEF7)
      VD      = DEF1(1)  .EQ. 'OUI'
      VS      = DEF1(8)  .EQ. 'OUI'
      BOX     = DEF1(4)  .EQ. 'OUI'
      ECHO    = DEF1(11) .EQ. 'OUI'
      INFORM  = DEF1(7)  .EQ. 'INFORMATIF'
      REMPLAC = DEF1(10) .EQ. 'OUI'
      READ(DEF1(9), '(I8)') LIMITE
      
!     IMPRIME L'INDICATIF DE DEBUT DU PROGRAMME.
      I = FNOM(6, DEF1(6), 'SEQ', 0)

      app_ptr=App_Init(APP_MAIN,'EDITBRP','3.15.1','',BUILD_TIMESTAMP)
      call app_logstream(DEF1(6))
     
      IF( BOX ) THEN
         call App_Start()
      ELSE
         call App_Log(APP_VERBATIM,'***   E D I T B R P   V3.15.1   ***')
      ENDIF
  
!     INITIALISATION
      CALL RESTDEZ
      DO 10 I=1,NMD
         STNS(I) = 0
         NARG(I) = 0
   10    CONTINUE
      s_prefix_path =''
      d_prefix_path = ''

      !     OUVRE LES FICHIERS SOURCES
      NFS = 0
      DO 20 I=13,137
         IF(DEF1(I) .ne. ' ') THEN
            NFS = NFS+1
            NS(NFS) = DEF1(I)
         ENDIF
   20    CONTINUE
      IF(NFS .gt. 0) THEN
         CALL OUVREBS( NS, .false. )
         IF(NFS .eq. 0) then
            call App_Log(APP_ERROR,'editbrp: Source file unknown')
            KERR = -2
         endif
      ENDIF

!     OUVRE LE FICHIER DESTINATION
      CALL OUVREBD( DEF1(2),(DEF1(12) .eq. 'OUI') )

!     OUVRE LE FICHIER FUSION
      IF(DEF1(3) .NE. ' ') CALL OUVREBF( DEF1(3), .false. )


!     PREPARE LE DICTIONAIRE DE READLX
      CALL QLXINS(INFORM , 'DEBUG'  , DUMY, 1, 1) 
      CALL QLXINS(INFORM , 'INFORM' , DUMY, 1, 1) 
      CALL QLXINS(LIMITE , 'LIMITE' , DUMY, 1, 1) 
      CALL QLXINS(LIMITE , 'COPIES' , DUMY, 1, 1) 
      CALL QLXINS(SAUV   , 'SAUVDES', DUMY, 1, 1) 
      CALL QLXINS(ECHO   , 'ECHO'   , DUMY, 1, 1) 
      CALL QLXINS(REMPLAC, 'REMPLAC', DUMY, 1, 1) 
      CALL QLXINS(VS     , 'VOIRS'  , DUMY, 1, 1) 
      CALL QLXINS(VD     , 'VOIRD'  , DUMY, 1, 1) 
      CALL QLXINS(FENETRE, 'FENETRE', DUMY, 1, 1) 

!     APELLE UN SOUS-PROGRAMME
      CALL QLXINX(SPOOL,   'OK'     , NP,   0, 2)
      CALL QLXINX(NEWSRC,  'S',       NP, 101, 2)
      CALL QLXINX(NEWDST,  'D',       NP, 101, 2)
      CALL QLXINX(NEWFUS,  'F',       NP, 101, 2)
      CALL QLXINX(BDESIRE, 'DESIRE',  NP, 109, 2)
      CALL QLXINX(BEXCLUR, 'EXCLURE', NP, 109, 2)
      CALL QLXINX(BRPCOPI, 'BRPCOPI', NP, 103, 2)
      CALL QLXINX(OPDATE,  'OPDATE',  NP, 104, 2)
      CALL QLXINX(CMCDATE, 'CMCDATE', NP, 104, 2)
      CALL QLXINX(DATC,    'DATE',    NP, 104, 2)
      CALL QLXINX(PREFIXS, 'PREFIXS', NP, 101, 2)
      CALL QLXINX(PREFIXD, 'PREFIXD', NP, 101, 2)

!     CHANGE UNE CONSTANTE
      CALL QLXINS(MOIN1  , 'TOUS'   , I, 1, 0) 
      CALL QLXINS(MOIN2  , '@'      , I, 1, 0) 
      CALL QLXINS(MOIN3  , 'DELTA'  , I, 1, 0) 
      CALL QLXINS(MOIN4  , 'COMMUNE', I, 1, 0) 
      CALL QLXINS(VRAI   , 'OUI'    , I, 1, 0) 
      CALL QLXINS(FAUX   , 'NON'    , I, 1, 0) 

!     LIT UN ENSEMBLE DE DIRECTIVES?
      IF( DEF1(5) .NE. 'NONE' ) THEN
         I = FNOM(5, DEF1(5), 'SEQ', 0)
         IF(DEF1(5) .EQ. '$IN') THEN
            PRINT*,'TAPEZ  VOS  DIRECTIVES S.V.P.' 
            PRINT*,'TAPEZ  END  A LA FIN'
         ENDIF
         CALL READLX(5, DUMY, KERR)

!         IF(KERR .NE. 0) then
         IF(DUMY .LT. 0) then
            call App_Log(APP_ERROR,'editbrp: Directive error')
         endif
      ENDIF


!     SI PAS DE TENTATIVE DE COPIE
      IF( .NOT.ESAIS ) CALL SPOOL

!     TOUT EST TERMINE , FERME LES FICHIERS
      CALL FERMBS
      CALL FERMBD
      CALL FERMBF
!     IMPRIME L'INDICATIF DE FIN DU PGM.
      IF( BOX ) THEN
         app_status=App_End(KERR)
      ELSE
         call App_Log(APP_VERBATIM,'***   E D I T I O N -  T E R M I N E E  ***')
      ENDIF
  
      STOP
      END 

