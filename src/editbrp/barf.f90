!     S/P BARF COPIE DES FICHIERS BURP EN TOUT OU EN PARTIE. 
      SUBROUTINE BARF
      use app
      IMPLICIT NONE 
  
!AUTEUR
!             - Y. BOURASSA   - FEV 91
!REVISION 001 - "     "       - OCT 91 ALLEGEMENT DE LA LOGIQUE
!         002 - "     "       - UTILISATION DE QDFERR
!         003 - "     "       - BUG RANGE DE DATE
!         004 - "     "       - NFS FICHIERS D'ENTREE
!         005 - "     "       - SIMPLIFICATION DU CODE
!         006 - M. Valin      - Remplacement de memoirh
!LANGAGE  - FTN77
#include "maxprms.cdk"
#include "desrs.cdk"
#include "char.cdk"
#include "fiches.cdk"

!      start of fix for memoirh
!      COMMON /   / BUFL(1)
!      INTEGER      BUFL
      integer, pointer, dimension(:), save :: BUFL => NULL()
!      end of fix for memoirh
!     - BUFL       CHAMP DE LA LONGUEUR DE L'ENREGISTREMENT LE PLUS LONG (+10).
!                  LA ROUTINE MEMOIR RESERVE L'ESPASSE.

      EXTERNAL     MRFPRM, MRFPUT, JULM, MRFGET, MRFLOC,                 &
                   MRFMXL, MRFOPC
      INTEGER      LT, LN, I, MRFLOC, MRFMXL, JULM, EXCLUS, TEMP, REECR, &
                   DX, DY, J, MRFPRM, MRFGET, LONG, TROUVE, IREC, EXTRA, &
                   FG, LG, K, MRFPUT, BUFSIZ, AJOU, MRFOPC, DATE, IP(8), &
                   HD, DT, L, IST, MD   
      EQUIVALENCE  (IP(1), LT), (IP(2), LN), (IP(4), DT), (IP(6), DX),   &
                   (IP(7), DY), (IP(5), FG), (IP(8), LG)
      CHARACTER (len=3) TIP(3)
      CHARACTER (len=9) STN, WILD
      LOGICAL      BOITE, MODINF
      SAVE         BUFSIZ, IST, WILD, TIP
      DATA         BUFSIZ, IST /2*0/
      DATA         WILD    /'*********'/
      DATA         TIP     /' <<',' >>',' <>'/
      AJOU   = 0
      REECR  = 0
      EXCLUS = 0
      TROUVE = 0

!     arret des messages informatif.
      I = MRFOPC('MSGLVL','ERROR')

!     appel a MRFPRM?
      MODINF = ECHO .OR. REMPLAC .OR. .NOT.EXPRESS
    
!     TROUVE LA LONGUEUR DU CHAMP LE PLUS GRAND.
      IF( STATS ) THEN
         IF( ECHO ) THEN
            WRITE(6,603)
            WRITE(6,604)
         ENDIF
      ELSE
         IF( ECHO ) THEN
            WRITE(6,602)
            WRITE(6,601)
         ENDIF
         LONG = 0
         DO 10 L=1,NFS
            LONG = MAX(LONG, 10+MRFMXL(SOURCES(L)))
 10         CONTINUE
         IF(LONG .gt. BUFSIZ) THEN
!        start of fix for memoirh
!            IF(BUFSIZ .ne. 0) CALL MEMOIRH(BUFL, IST, 0)
            IF(BUFSIZ .ne. 0) deallocate(bufl)
!           RESERVE LA MEMOIRE TEMPON POUR LECTURE
!            CALL MEMOIRH(BUFL, IST, LONG)
            allocate(BUFL(LONG))
            IST = 1
!        end of fix for memoirh
            BUFL(IST) = LONG
            BUFSIZ    = LONG
         ENDIF
      ENDIF

!     BOUCLE DES FICHIERS SOURCES
      DO 130 L=1,NFS
         IREC = 0
!        Y A-T-IL UN AUTRE ENREGISTREMENT.
   20    IREC = MRFLOC(SOURCES(L), IREC, WILD, -1, -1, -1, -1, -1, I, 0)
         IF(IREC .LT. 0) THEN
            write(app_msg,*) 'barf: End of file',SOURCES(L)
            call App_Log(APP_INFO,app_msg)
            GO TO 130
         ENDIF
         IF( MODINF ) THEN
            I = MRFPRM(IREC, STNID, DT, LT, LN, DX, DY, DATE, TEMP, FG, EXTRA, 0, LG)
            IP(3) = JULM(DATE, TEMP)
            IF(LN .EQ. 3600) LN = 0  
         ENDIF
         IF( EXPRESS ) THEN
            K = 1
            GO TO 100
         ENDIF

!        VERIFIE SI L'ENREGISTREMENT SATISFAIT UN DESIRE OU EXCLURE
         DO 110 K=1,NREQ
!           VALIDATION DES STNID
            IF(STNS(K).EQ.3 .AND. ID(2,K).EQ.'@') THEN
!              SI ON A UN RANGE ['70123','@','72999']
              IF((STNID.LT.ID(1,K).OR.(STNID.GT.ID(3,K)))) GOTO 110
            ELSEIF( STNS(K) .GT. 0) THEN
!              ON A UNE LISTE DE STNID A TROUVER 
               DO 50 J=1,STNS(K)
                  STN = ID(J,K) 
                  DO 40 I=1,9
                    IF(STN(I:I).NE.'*' .AND. STN(I:I).NE.STNID(I:I)) GO TO 50
   40                CONTINUE
                  GO TO 60
   50             CONTINUE
               GO TO 110
            ENDIF

!           VALIDE  LAT, LON, DATE, TYPE, FLAG, DX, DY, LONG
   60       BOITE = (REQ(11,1,K).EQ.-1) .AND. (REQ(3,1,K).EQ.1) .AND. &
                    (REQ(11,2,K).EQ.-1) .AND. (REQ(3,2,K).EQ.1)  
            DO 90 J=1,NARG(K)
               IF(REQ(11,J,K) .ne. 0) THEN
!                 SI L'ARGUMENT N'EST PAS UNIVERSEL
                  IF(REQ(11,J,K) .gt. 0) THEN 
!                    L'ARGUMENT EST UNE LISTE DE PARAMETRES
                     IF(J .EQ. 5) THEN
!                       VALIDATION DES FLAGS
                        IF(REQ(11,5,K) .GT. 0) THEN
!                          DOIT-ON CONSIDERER LE FLAG
                           DO 70 I=1,REQ(11,5,K)
                              FG = iand(IP(5), REQ(I,5,K))
                              IF(FG .EQ. REQ(I,5,K)) GOTO 90 
   70                         CONTINUE
                           GO TO 110
                        ENDIF
                     ELSE   
                        DO 80 I=1,REQ(11,J,K)
   80                      IF(IP(J) .EQ. REQ(I,J,K)) GO TO 90  
                        GO TO 110
                     ENDIF
                  ELSE
!                    L'ARGUMENT EST UN INTERVALE AVEC SAUT
                     IF((IP(J) .LT. REQ(1,J,K)) .OR. (IP(J) .GT. REQ(2,J,K))) GO TO 110
!                    BOITES DE LAT, LON AVEC STATION AU POLE
                     IF((J.LT.3) .AND. BOITE) THEN
                        IF(IP(J).LT.REQ(2,J,K)) GO TO 90
                        IF( (IP(1).EQ.18000) .AND.(IP(2).EQ.0)) GOTO 90
                     ELSE
                        IF((MOD((IP(J)-REQ(1,J,K)),REQ(3,J,K)).EQ.0)) GOTO 90
                     ENDIF
                     GO TO 110
                  ENDIF
               ENDIF
   90          CONTINUE
  100       SATISF(K) = SATISF(K) + 1
            IF( STATS ) THEN
               TROUVE = TROUVE + 1
               IF(ECHO .and. (DESEXC(K).eq.-1)) WRITE(6,600) TIP(4), &
                  STNID, LT, LN, DX, DY, FG, DATE, TEMP, DT, LG, IREC
            ELSEIF(EXPRESS .or. (DESEXC(K).EQ.-1)) THEN 
!              REQUETE = DESIRE, COPIER SI PAS EN MODE STATISTIQUE
               HD = 0
               MD = 2
               IF( REMPLAC ) THEN
                  J = MRFLOC(3, HD, STNID, DT, LT, LN, DATE, TEMP, EXTRA, 0)
                  IF(J .GT. 0) THEN
                     HD = J
                     MD = 3
                     REECR = REECR + 1
                  ELSE
                     AJOU  = AJOU + 1
                  ENDIF   
               ELSE
                  AJOU  = AJOU + 1
               ENDIF
               I = MRFGET(IREC,  BUFL(IST))
               I = MRFPUT(3, HD, BUFL(IST))
               RENDUA = RENDUA - 1
               IF( ECHO ) WRITE(6,600) TIP(MD), STNID, LT, LN, DX, DY, FG, DATE, TEMP, DT, LG, IREC
               IF(RENDUA .EQ. 0) THEN
                  call App_Log(APP_INFO,'Maximum number of copies reached')
                  GO TO 140
               ENDIF
            ELSE
!              DIRECTIVE SATISFAITE  = EXCLURE
               EXCLUS = EXCLUS + 1
               IF( ECHO ) WRITE(6,600) TIP(1), STNID, LT, LN, DX, DY, FG, DATE, TEMP, DT, LG, IREC
            ENDIF
            GO TO 20
  110       CONTINUE
         GO TO 20
!        AUCUNE DES NREQ DESIRE/EXCLURE SATISFAITE
  130    CONTINUE
  140 I = MRFOPC('MSGLVL', DEF7)

      DO 150 I=1,NREQ
         write(app_msg,*) 'Request',I,' satisfied',SATISF(I),' times'
         call App_Log(APP_INFO,app_msg)
  150    CONTINUE
      IF( STATS ) THEN
         write(app_msg,*) 'Records found = ', TROUVE
         call App_Log(APP_VERBATIM,app_msg)
      ELSE
         IF( REECR  .GT.0) THEN
            write(app_msg,*) 'Records re-written = ', REECR
            call App_Log(APP_VERBATIM,app_msg)
         endif
         IF( AJOU   .GT.0) THEN
            write(app_msg,*) 'Records added = ', AJOU
            call App_Log(APP_VERBATIM,app_msg)
         endif
         IF( EXCLUS .GT.0) THEN
            write(app_msg,*) 'Records excluded = ', EXCLUS
            call App_Log(APP_VERBATIM,app_msg)
         endif
      ENDIF
      EXPRESS = .FALSE.
      RETURN

 600  FORMAT(A3, 3X, A9, 4I7, 3X, Z8, I8, 2I7, 2I10)
 601  FORMAT(' MODE STATION     LATI   LONG     DX     DY  FLGS(HEX) ', &
             '   DATE  TEMPS  IDTYP  LONGUEUR   ADRESSE')
 602  FORMAT(//,' DEFINITION DE MODE  >>=ECRIT  <<=EXCLU  <>=REECRIT')
 603  FORMAT(//,' *** ENREGISTREMENTS TROUVES DANS SOURCE ***')
 604  FORMAT('      STATION     LATI   LONG     DX     DY  FLGS(HEX) ', &
             '   DATE  TEMPS  IDTYP  LONGUEUR   ADRESSE')
      END 

