      SUBROUTINE JDATXX(I,A,M,J)
*
*auteur M. Lepine - aout 96
*revision 001  M. Lepine avr 98  annee a 4 chiffres
*
*Objet
*  retourner un jour julien par rapport a l'an 1900 pour eviter
*  un debordement lors de calculs subsequents (a 32 bits) dans opdate
*
      implicit none
      INTEGER I,A,M,J,I0
      CALL JDATEC(I0,1900,01,01)
      CALL JDATEC(I,A,M,J)
      I = I - I0
      RETURN
      END
