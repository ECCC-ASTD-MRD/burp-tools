#include <rpnmacros.h>
#include <stdio.h>
void f77name(rah2char)(char *chaine, ftnword *f_entier, ftnword *f_nc, int lng)
{
int nc=*f_nc;
int entier=*f_entier;
int i; 
 
  printf("Debug nc=%d\n",nc);
  printf("Debug entier=%d\n",entier);
  printf("Debug *chaine avant ='%c'\n",*chaine);
  for (i=0; i<nc; i++) {
    *chaine = (entier >> (i*8)) & 0xFF;
    printf("Debug *chaine='%c'\n",*chaine);
    chaine++;
  }
}
