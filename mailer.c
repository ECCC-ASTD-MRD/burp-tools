/**
 *
 *nom         mailer
 *
 *auteur      jose garcia, fevrier 1992
 *
 *langage     c
 *
 *objet       fonction qui envoie un mail a un compte donne
 *            lors de la detection d'une erreur.
 *
 *parametres  usager  - nom de l'usager pour envoyer le mail
 *            message - message contenant la cause de l'erreur.
 *
 **/
#include <stdio.h>

int mailer_ ( usager, message, l1, l2 )
char *usager, *message ;
int   l1,      l2 ;
{
char comnd[256] ;
int  i ;
FILE *ferr ;
static char fichier_erreur[]  = "/data/ade/tmp/mailer_file" ;

ferr = fopen ( fichier_erreur, "w" ) ;
for ( i = 0 ; i < 80*10 ; i+=80 )
   fprintf ( ferr, "%.80s\n", &message[i] ) ;
fclose ( ferr ) ;

sprintf ( comnd, "mail %.7s < \0", usager ) ;
strcat  ( comnd, fichier_erreur ) ;
system  ( comnd ) ;
sprintf ( comnd, "rm %s", fichier_erreur ) ;
system  ( comnd ) ;
}
