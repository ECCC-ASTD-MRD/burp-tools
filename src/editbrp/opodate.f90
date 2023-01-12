!!! S/R OPDATE ETABLIT UNE DATE OU UNE PERIODE A PARTIR D'UNE DATE
!     RETOURNEE DE 'IOPDATM'
      SUBROUTINE OPDATE(DN, ECART, DUREE, DELTA)
      use app
      IMPLICIT NONE 
  
      INTEGER  DN(2), ECART, DUREE, DELTA
!
!ARGUMENTS
! ENTREE    - DN    - 'NOM DU FICHIER' OU PRENDRE LA DATE.
!    "      - ECART - JJHHMM NOMBRE D'HEURES PAR LEQUEL IF FAUT MODIFIER LA
!                            DATE QUI VIENT DU FICHIER 'DN'. (SI PRESENT)
!    "      - DUREE - JJHHMM DUREE DE LA PERIODE.            (SI PRESENT)
!    "      - DELTA - JJHHMM INTERVALE ENTRE LES CAS.        (SI PRESENT)
!
!AUTEURS
! VERSION ORIGINALE   Y. BOURASSA NOV 90
! REVISION 001        "      "    DEC 91 CHANGE MESSAGES
!          002        "      "    JUN 92 NE CHANGE PLUS FENETRE
!          003        "      "    MAI 95 ENLEVE REFERENRES A 1950
!          004        M. Lepine   AVR 98 Conversion pour l'an 2000
!          005        M. Lepine   JUL 99 Bug fix calcul des minutes (DATC)
!
!LANGUAGE   - FTN77 
!
#include "maxprms.cdk"
#include "desrs.cdk"
#include "fiches.cdk"
  
      CHARACTER (len=8)   C
      INTEGER       ANNEE, JOUR, MOIS, HEURE, JS, I, J, CMC,        &
                    IOPDATM, ARGDIMS, JULM, JHMENM, DTG(14),        &
                    MINUTE, OFFSET
      EXTERNAL      IOPDATM, LOW2UP, JULM, JHMENM, INCDAT,          &
                    DATMGP2, ARGDIMS, JDATXX
      EQUIVALENCE   (DTG(5), HEURE), (DTG(4),ANNEE), (DTG(3),JOUR), &
                    (DTG(2), MOIS),  (DTG(1),JS),    (CMC, DTG(14))  


!     ETABLIR LE DATESTAMP DU CAS OU DU DEBUT DE LA PERIODE 
      WRITE(C, CAR8) (DN(I), I=1,ARGDIMS(1))
      CALL LOW2UP(C, C)
    1 MINUTE = 00
      CALL DATMGP2( DTG )

!     ecart entre la late passee et la date voulue en minutes
    2 IF(NP .GT. 1) THEN
         OFFSET = JHMENM( ECART )
      ELSE
         OFFSET = 0
      ENDIF

!     delta entre les dates desirees
      IF(NP .EQ. 4) THEN
         JOURS(3) = JHMENM( ABS(DELTA) )
      ELSE
         JOURS(3) = 1
      ENDIF

!     calcul la date julienne
      CALL JDATXX(J, ANNEE, MOIS, JOUR)
 
!     passe de date julienne @ date julienne-minute 
      JOURS(1) = J*24*60 + HEURE*60 + MINUTE + OFFSET

      IF(NP .GT. 2) THEN
         JOURS(4) = -1
         JOURS(2) = JOURS(1) + JHMENM( DUREE )
!        AJUSTEMENT DES DATES SI NECESSAIRE
         IF(JOURS(2) .LT. JOURS(1)) THEN
           J        = JOURS(1)
           JOURS(1) = JOURS(2) 
           JOURS(2) = J       
         ENDIF
         write(app_msg,600) JHMENM(DUREE), OFFSET, ANNEE, MOIS, JOUR, HEURE, MINUTE
         call App_Log(APP_INFO,app_msg)
  600    FORMAT('opdate: Common period of',I8,' minute(s) starts at', I8,' minute(s) of (',I4,2I2,',',2I2,')') 
      ELSE
         JOURS(4) = 1
         write(app_msg,601) OFFSET, ANNEE, MOIS, JOUR, HEURE,MINUTE
         call App_Log(APP_INFO,app_msg)
  601    FORMAT('opdate: Common date is at', I8,' minute(s) of (',I4,2I2,',',2I2,')') 
      ENDIF
      RETURN

!     ETABLIT UNE DATE OU UNE PERIODE A PARTIR D'UNE DATE BURP
!     ENTRY DATE([AAMMJJ,HHMM], JJHHMM, JJHHMM, JJHHMM)
      ENTRY DATC(DN,            ECART,  DUREE,  DELTA)
      IF(ARGDIMS(1) .EQ. 1) THEN
         HEURE = 0
      ELSE
         HEURE = DN(2)
      ENDIF
      ANNEE  = DN(1)/10000 
      MOIS   = (DN(1)-(ANNEE*10000))/100
      JOUR   = (DN(1)-ANNEE*10000)-(MOIS*100)
      MINUTE = MOD(HEURE,100)
      HEURE  = HEURE/100
      GO TO 2


!     ETABLIT UNE DATE OU UNE PERIODE A PARTIR DUN STAMP CMC
!     ENTRY CMCDATE(CMCSTAMP, JJHHMM, JJHHMM, JJHHMM)
      ENTRY CMCDATE(DN,       ECART,  DUREE,  DELTA)
      CMC = DN(1)
      GO TO 1

      END 
