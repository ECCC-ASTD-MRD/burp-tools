#include <stdio.h>
#include <strings.h>
#include <malloc.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>

#include <App.h>
#include <rmn.h>

#include "idl_export.h"

/*
 * structure definition of a BURP Report
 */
typedef struct
{
    IDL_LONG   temps;
    IDL_LONG   flgs;
    IDL_STRING stnid;    /* char stnid[10] */
    IDL_LONG   idtype;
    IDL_LONG   lati;
    IDL_LONG   longi;
    IDL_LONG   dx;
    IDL_LONG   dy;
    IDL_LONG   elev;
    IDL_LONG   drnd;
    IDL_LONG   date;
    IDL_LONG   oars;
    IDL_LONG   runn;
    IDL_LONG   nblk;
    IDL_LONG   lngr;

} RPT_PRM ;

/*
 * structure definition of a block
 */
 
typedef  struct
{
    IDL_LONG    nele;
    IDL_LONG    nval;
    IDL_LONG    nt;
    IDL_LONG    bfam;
    IDL_LONG    bdesc;
    IDL_LONG    btyp;
    IDL_LONG    nbit;
    IDL_LONG    bit0;
    IDL_LONG    datyp;

} BLK_PRM ;

/*
 * To open a burb file. This function returns yhe number of the active
 * reports in the BURP file.
 */


IDL_LONG i_idl_mrfopn(int argc,void* argv[])
{
     char        *fname,*mode;
     IDL_LONG        *unit_adr;
     IDL_LONG        unit;
     IDL_LONG     ier, istat=0, err;
     IDL_STRING  *str_fname,*str_mode;
     float       a;


     /* Insure that the correct number of arguments were passed in */
     if(argc != 3) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (3) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     unit_adr   = (IDL_LONG *)      argv[0];
     unit       =  *(IDL_LONG *)    argv[0];
     str_fname  = (IDL_STRING *) argv[1];
     str_mode   = (IDL_STRING *) argv[2];

     fname = (*str_fname).s;
     mode  = (*str_mode).s;

/*
     printf("Debug i_idl_mrfopn unit=%d, filename=%s\n",*unit_adr,fname);
     printf("Debug i_idl_mrfopn unit=%d mode=%s\n",unit,mode);
*/
a=-99.99;
     err = c_mrfopc("MSGLVL","FATAL");
     err = c_mrfopr("MISSING",a);
     ier = c_fnom(&unit,fname,"RND",0);
     istat = c_mrfopn(unit,"READ");
     return(istat);
}

IDL_LONG i_idl_mrfopr(int argc,void* argv[])
{
     IDL_STRING  *optnom;
     IDL_LONG    err;
     float       value;


     /* Insure that the correct number of arguments were passed in */
     if(argc != 2) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (2) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     optnom  = (IDL_STRING *) argv[0];
     value       =  *(float *)    argv[1];

     Lib_Log(APP_LIBBRP,APP_DEBUG,"%s: optnom=%s, value=%f\n",__func__,(*optnom).s,value);
     err = c_mrfopr((*optnom).s,value);
     return(err);
}

IDL_LONG i_idl_mrfopc(int argc,void* argv[])
{
     IDL_STRING  *optnom;
     IDL_STRING  *opvalc;
     IDL_LONG    err;


     /* Insure that the correct number of arguments were passed in */
     if(argc != 2) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (2) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     optnom  = (IDL_STRING *) argv[0];
     opvalc  = (IDL_STRING *) argv[1];

     Lib_Log(APP_LIBBRP,APP_DEBUG,"%s: optnom=%s, opvalc=%s\n",__func__,(*optnom).s,(*opvalc).s);
     err = c_mrfopc((*optnom).s,(*opvalc).s);
     return(err);
}

/*
 * To close a BURP file pointed by unit
 */
 
IDL_LONG i_idl_mrfcls(int argc,void* argv[])
{
     IDL_LONG       unit, ier, ier2;


     /* Insure that the correct number of arguments were passed in */
     if(argc != 1) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (1) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     unit       =  *(IDL_LONG *)    argv[0];

     ier = c_mrfcls(unit);
     ier2= c_fclos(unit);

     return(0);
}

/*
 * Returning the length of the longest report in the BURP file
 */
 
IDL_LONG i_idl_mrfmxl(int argc,void* argv[])
{
     IDL_LONG       unit, ier=-1;


     /* Insure that the correct number of arguments were passed in */
     if(argc != 1) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (1) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     unit       =  *(IDL_LONG *)    argv[0];

     ier = c_mrfmxl(unit);

     return(ier);
}

/*
 * To locate a file handle that matches the parameters...
 */
 
IDL_LONG i_idl_mrfloc(int argc,void* argv[])
{
     char      stnid[10];
     IDL_LONG  *handle_adr, *idtyp_adr, *lat_adr, *lon_adr,*date_adr;
     IDL_LONG  *heure_adr, *sup_adr, *nsup_adr;
     IDL_LONG  handle, idtyp, lat, lon,date;
     IDL_LONG  heure, sup, nsup;
     IDL_LONG     unit; 
     IDL_STRING *statio_adr;

     /* Insure that the correct number of arguments were passed in */
     if(argc != 10) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (10) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     unit           =  *(IDL_LONG *)         argv[0];
     handle_adr     =  (IDL_LONG *)       argv[1];
     statio_adr     =  (IDL_STRING *)     argv[2];
     idtyp_adr      =  (IDL_LONG *)       argv[3];
     lat_adr        =  (IDL_LONG *)       argv[4];
     lon_adr        =  (IDL_LONG *)       argv[5]; 
     date_adr       =  (IDL_LONG *)       argv[6];
     heure_adr      =  (IDL_LONG *)       argv[7];
     sup_adr        =  (IDL_LONG *)       argv[8];
     nsup_adr       =  (IDL_LONG *)       argv[9];  

     strncpy(stnid,(*statio_adr).s,9);
     handle = *handle_adr;
     idtyp  = *idtyp_adr ;
     lat    = *lat_adr   ;
     lon    = *lon_adr   ;
     date   = *date_adr  ;
     heure  = *heure_adr ;
     sup    = *sup_adr   ;   
     nsup   = *nsup_adr      ;
/*
     printf("Debug i_idl_mrfloc unit=%d \n",unit);
     printf("Debug i_idl_mrfloc handle=%d \n",handle);
     printf("Debug i_idl_mrfloc idtyp =%d \n",idtyp);
     printf("Debug i_idl_mrfloc lat   =%d \n",lat);
     printf("Debug i_idl_mrfloc lon   =%d \n",lon);
     printf("Debug i_idl_mrfloc date  =%d \n",date);
     printf("Debug i_idl_mrfloc heure =%d \n",heure);
     printf("Debug i_idl_mrfloc nsup  =%d \n",nsup);
     printf("Debug i_idl_mrfloc stati =%s \n",stnid);
*/

/*   sup et nsup sont ignorés car pour la version 1990, les clés
 supplémentaires ne sont pas permises dans mrfloc */
     handle=c_mrfloc(unit,handle,stnid,idtyp,lat,lon,date,heure,NULL,0);
     return(handle);
}

/*
 * To find the position of a block of data of type bafam, bdesc, btyp
*/

IDL_LONG i_idl_mrbloc(int argc,void* argv[])
{
     IDL_LONG  blk0, bfam, bdesc, btyp, blkn;
     IDL_LONG  *buf;

     /* Insure that the correct number of arguments were passed in */
     if(argc != 5) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (5) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     buf            =   (IDL_LONG *)         argv[0];
     bfam           =  *(IDL_LONG *)       argv[1];
     bdesc          =  *(IDL_LONG *)       argv[2];
     btyp           =  *(IDL_LONG *)       argv[3];
     blk0           =  *(IDL_LONG *)       argv[4];

/*
     printf("Debug i_idl_mrbloc bfam   =%d \n",bfam);
     printf("Debug i_idl_mrbloc bdesc  =%d \n",bdesc);
     printf("Debug i_idl_mrbloc blk0   =%d \n",blk0);
     printf("Debug i_idl_mrbloc btyp   =%d \n",btyp);
*/

blkn=c_mrbloc(buf,bfam,bdesc, btyp, blk0);
     return(blkn);
}


IDL_LONG i_idl_mrb_get_blok_data(int argc,void* argv[])
{
     IDL_LONG        *buf, ier, err, bkno ;
     IDL_LONG        *lstele,*dliste,*dlisttmp, *tblval, *tblval2;
     IDL_LONG         nele,nval,nt;

     float           *source;
     float           *destination;

     IDL_LONG        *src;
     IDL_LONG        *dst;
     int i, k, j;

     short           mode;
     float           *rval, *rval2;

     /* Insure that the correct number of arguments were passed in */
     if(argc != 10) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (10) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     buf          =  (IDL_LONG *)      argv[0];
     bkno         =  *(IDL_LONG *)     argv[1];
     lstele       =  (IDL_LONG *)      argv[2];
     tblval       =  (IDL_LONG *)      argv[3];
     dliste       =  (IDL_LONG *)      argv[4];
     rval         =  (float *)         argv[5];
     nele         =  *(IDL_LONG *)     argv[6];
     nval         =  *(IDL_LONG *)     argv[7];
     nt           =  *(IDL_LONG *)     argv[8];
     mode         =  *(short *)        argv[9];
/*
printf("\nnele= %d\n", nele);
printf("nval = %d\n", nval);
printf("nt   = %d\n", nt);
*/

     tblval2      = (IDL_LONG *) malloc(nele*nval*nt* sizeof(IDL_LONG));
     rval2        = (float *   ) malloc(nele*nval*nt* sizeof(float));
     dlisttmp        = (IDL_LONG *   ) malloc(nele*sizeof(IDL_LONG));

     destination =rval;
     source      =rval2;

     src          =dlisttmp;
     dst          =dliste;

     ier = c_mrbxtr(buf,bkno,lstele,tblval2);
     err = c_mrbdcl(lstele, dlisttmp, nele);
     err = c_mrbcvt(lstele, tblval2,rval2, nele, nval, nt, mode);

    for (i=0;i<nele*nval*nt;i++) {
      *destination++=*source++;
    }
    for (i=0;i<nele;i++) {
      *dst++=*src++;
    }


/*
      for ( k = 0 ; k < nt ; k++ )
         {
         if ( nt != 1 ) printf ( "\n\nobservation %d/%d", k+1, nt ) ;
         printf ( "\nlstele =" ) ;
         for ( i = 0 ; i < nele ; i++ )
            {
            printf ( "    %6.6d", lstele[i] ) ;
            if ( ((i+1)%7) == 0 && i != 0 && i+1 < nele )
               printf ( "\n        " ) ;
            }
         for ( j = 0 ; j < nval ; j++ )
            {
            printf ( "\ntblval =" ) ;
            for ( i = 0 ; i < nele ; i++ )
               {
                  printf ( "%10d", rval[i+nele*(j+nval*k)] ) ;
               if ( ((i+1)%7) == 0 && i != 0 && i+1 < nele )
                  printf ( "\n        " ) ;
               }
            }
         }
*/

/* Do things clean */

   free(tblval2);
   free(rval2);
   free(dlisttmp);

   return(ier);

}


/*
 * To go to get the contents of the report pointed by
 * handle and put in the buf
*/


IDL_LONG i_idl_mrfget(int argc,void* argv[])
{
     IDL_LONG       handle, *buf, ier,err, *data;

     IDL_LONG       *buf2;
     IDL_LONG       *source;
     IDL_LONG       *destination;
     int i;


     /* Insure that the correct number of arguments were passed in */
     if(argc != 2) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (2) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     handle         =  *(IDL_LONG *)    argv[0];
     buf            =  (IDL_LONG *)     argv[1];
     
     buf2           =  (IDL_LONG *) malloc(buf[0]*sizeof(IDL_LONG) +1024000);
     buf2[0]        =  buf[0];


     destination    =  buf;
     source         =  buf2;

/*
     err = c_mrfopc("MSGLVL","FATAL");
     err = c_mrfopr("MISSING",-99.99);
*/

    ier = c_mrfget(handle, buf2);

    for (i=0;i<buf[0];i++) {
      *destination++=*source++;
    }
/* Do things clean */
     free(buf2);
     
     return (ier);
}

/*
 * To extract report parameters from array buf
*/

IDL_LONG i_idl_mrbhdr(int argc,void* argv[])
{
     IDL_LONG        *buf, ier;
     IDL_LONG        *sup=NULL, *xaux=NULL;
     RPT_PRM         *params;
     char            *stnid; 
     char             station[10];

     /* Insure that the correct number of arguments were passed in */
     if(argc != 3) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (2) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     buf          =  (IDL_LONG *)     argv[0];
     params       =  (RPT_PRM *)      argv[1];
     stnid        =  (char *)         argv[2];

     ier = c_mrbhdr(buf,&(params->temps),&(params->flgs),station,&(params->idtype),&(params->lati), &(params->longi), &(params->dx), &(params->dy),  &(params->elev), &(params->drnd), &(params->date), &(params->oars), &(params->runn), &(params->nblk), sup, 0, xaux, 0);

     strncpy(stnid, station,10);
     return(ier);
}

/*
 * To extract report parameters from report pointed by handle 
*/

IDL_LONG i_idl_mrfprm(int argc,void* argv[])
{
     IDL_LONG        handle, ier;
     IDL_LONG        *sup=NULL, *xaux=NULL;
     RPT_PRM         *params;
     char            *stnid;
     char             station[10];

     /* Insure that the correct number of arguments were passed in */
     if(argc != 3) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (2) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     handle          =  *(IDL_LONG *)     argv[0];
     params       =  (RPT_PRM *)      argv[1];
     stnid        =  (char *)         argv[2];

     ier = c_mrfprm(handle,station,&(params->idtype),&(params->lati), &(params->longi), &(params->dx), &(params->dy), &(params->date),&(params->temps),&(params->flgs), sup, 0, &(params->lngr));

     strncpy(stnid, station,10);
     return(ier);
}


/*
 * To extract the descriptor parameters from the block pointed by bkno
*/


IDL_LONG i_idl_mrbprm(int argc,void* argv[])
{
     IDL_LONG        *buf, ier, bkno;
     BLK_PRM         *params;

     /* Insure that the correct number of arguments were passed in */
     if(argc != 3) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (2) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     buf          =  (IDL_LONG *)     argv[0];
     params       =  (BLK_PRM *)      argv[1];
     bkno         =  *(IDL_LONG *)     argv[2];

     ier = c_mrbprm(buf,bkno,&(params->nele),&(params->nval),&(params->nt), &(params->bfam), &(params->bdesc), &(params->btyp),  &(params->nbit), &(params->bit0), &(params->datyp));

     return(ier);
}

/*
 * To extract bloc data and put the stuff in array tblval
*/

IDL_LONG i_idl_mrbxtr(int argc,void* argv[])
{
     IDL_LONG        *buf, ier, bkno ;
     IDL_LONG        *lstele, *tblval, *tblval2;
     IDL_LONG         nele,nval,nt;

     IDL_LONG        *source;
     IDL_LONG        *destination;
     int i, k, j;

     /* Insure that the correct number of arguments were passed in */
     if(argc != 7) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (7) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     buf          =  (IDL_LONG *)     argv[0];
     bkno         =  *(IDL_LONG *)     argv[1];
     lstele       =  (IDL_LONG *)     argv[2];

     tblval       =  (IDL_LONG *)     argv[3];

     nele         =  *(IDL_LONG *)     argv[4];
     nval         =  *(IDL_LONG *)     argv[5];
     nt           =  *(IDL_LONG *)     argv[6];
/*
printf("\nnele= %d\n", nele);
printf("nval = %d\n", nval);
printf("nt   = %d\n", nt);
*/


     tblval2      = (IDL_LONG *) malloc(nele*nval*nt* sizeof(IDL_LONG));
     destination =tblval;
     source      =tblval2;

     ier = c_mrbxtr(buf,bkno,lstele,tblval2);

    for (i=0;i<nele*nval*nt;i++) {
      *destination++=*source++;
    }

/*
      for ( k = 0 ; k < nt ; k++ )
         {
         if ( nt != 1 ) printf ( "\n\nobservation %d/%d", k+1, nt ) ;
         printf ( "\nlstele =" ) ;
         for ( i = 0 ; i < nele ; i++ )
            {
            printf ( "    %6.6d", lstele[i] ) ;
            if ( ((i+1)%7) == 0 && i != 0 && i+1 < nele )
               printf ( "\n        " ) ;
            }
         for ( j = 0 ; j < nval ; j++ )
            {
            printf ( "\ntblval =" ) ;
            for ( i = 0 ; i < nele ; i++ )
               {
                  printf ( "%10d", tblval[i+nele*(j+nval*k)] ) ;
               if ( ((i+1)%7) == 0 && i != 0 && i+1 < nele )
                  printf ( "\n        " ) ;
               }
            }
         }
*/

/* Do things clean */

   free(tblval2);

   return(ier);
}

/*
 * To convert a list of coded(CMC) element names in array lstele
 * into 6 digit decimal BUFR format.
*/

IDL_LONG i_idl_mrbdcl(int argc,void* argv[])
{
     IDL_LONG         err, nele;
     IDL_LONG        *lstele, *lstele2;

     /* Insure that the correct number of arguments were passed in */
     if(argc != 3) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (3) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     lstele       =  (IDL_LONG *)     argv[0];
     lstele2      =  (IDL_LONG *)     argv[1];
     nele         =  *(IDL_LONG *)     argv[2];

     err = c_mrbdcl(lstele, lstele2, nele);


     return(err);
}

/*
 * To convert values from tblval(integer) to rval(real) or the ionverse
 * depending on the mode
*/

IDL_LONG i_idl_mrbcvt(int argc,void* argv[])
{
     IDL_LONG         err, nele,nval, nt, btyp, bknat;
     short            mode;
     IDL_LONG        *lstele;
     IDL_LONG        *tblval;
     float           *rval;

     /* Insure that the correct number of arguments were passed in */
     if(argc != 7) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (7) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     lstele       =  (IDL_LONG *)     argv[0];
     tblval       =  (IDL_LONG *)     argv[1];
     rval         =  (float *)     argv[2];
     nele         =  *(IDL_LONG *)     argv[3];
     nval         =  *(IDL_LONG *)     argv[4];
     nt           =  *(IDL_LONG *)     argv[5];
     mode         =  *(short *)     argv[6];

        err = c_mrbcvt(lstele, tblval,rval, nele, nval, nt, mode);
 

     return(err);
}

IDL_LONG i_idl_blk_type_marqueur(int argc,void* argv[])
{
     IDL_LONG         err, btyp, bknat;

     /* Insure that the correct number of arguments were passed in */
     if(argc != 1) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (8) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     btyp         =  *(IDL_LONG *)     argv[0];
     bknat        =  (btyp >> 11) %16;


    if ( (bknat%4)!=3)
       err = 0;
    else
       err = 1;


     return(err);
}

IDL_LONG i_idl_file_type(int argc,void* argv[])
{
     char        *fname;
     int destination[1024];
     int nb = sizeof(destination);
     int *ptdest;
     int i;
     unsigned char *pt_i = &i;

     FILE * A_Lire;

     IDL_LONG         err;
     IDL_STRING  *str_fname;
     ptdest= &destination[0];
     i=1;

     /* Insure that the correct number of arguments were passed in */
     if(argc != 1) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (1) passed=%d\n",__func__,argc);
       return -1;
     }
     /* Cast the pointer in argv to the pointer variables */
     str_fname  = (IDL_STRING *) argv[0];
     fname      = (*str_fname).s;

     /*printf("\nfname= %s\n", fname);*/

    A_Lire = fopen(fname,"r");

    if (A_Lire == (FILE *) NULL) {
         printf("probleme d'ouverture de fichier\n");
         return(-1);
     }
    fseek(A_Lire,0,0);

    fread(ptdest,nb,1,A_Lire);  
/*
     printf("\n1int= %x\n",*(ptdest +0) );
     printf("\n2int= %x\n",*(ptdest +1) );
     printf("\n3int= %x\n",*(ptdest +2) );
     printf("\ndebut= %x\n",*(ptdest +3) );
     printf("\ni[0]= %d\n",pt_i[0] );
*/
     
if (pt_i[0] == 1) {
    Lib_Log(APP_LIBBRP,APP_INFO,"%s: Litte Endian platform\n",__func__);
   /* burp */
   if (*(ptdest +3) == 0X30505242)
      return(3);
   else
     return(-1);
    }
if (pt_i[0] == 0) {
    Lib_Log(APP_LIBBRP,APP_INFO,"%s: Big Endian platform\n",__func__);
   /* burp */
   if (*(ptdest +3) == 0X42525030)
      return(3);
   else
     return(-1);
    }

}

IDL_LONG i_idl_block_type(int argc,void* argv[])
{

/*
 decomposition du btyp

   btyp    =      bknat       |      bktyp      |  bkstp
   15 bits =      4 bits      |      7 bits     |  4 bits
           =  bknat1 | bknat2 | bktyp1 | bktyp2 |  bkstp
           =  2 bits | 2 bits | 1 bit  | 6 bits |  4 bitS
*/

      unsigned short int value,btyp,bknat1,bknat2,bktyp1, bktyp2,bkstp;
      IDL_STRING *bknat1_str;
      IDL_STRING *bknat2_str;
      IDL_STRING *bktyp1_str;
      IDL_STRING *bktyp2_str;


     /* Insure that the correct number of arguments were passed in */
     if(argc != 5) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (5) passed=%d\n",__func__,argc);
       return -1;
     }

     /* Cast the pointer in argv to the pointer variables */
     value       =  *( unsigned short int *) argv[0];
     bknat1_str  =           ( IDL_STRING *) argv[1];
     bknat2_str  =           ( IDL_STRING *) argv[2];
     bktyp1_str  =           ( IDL_STRING *) argv[3];
     bktyp2_str  =           ( IDL_STRING *) argv[4];

/* traitement de bknat1 
  00 = UNI
  01 = MULTI
*/ 
     bknat1 = value;
     bknat1 = (bknat1 << 1);
     bknat1 = (bknat1 >> 14);
     switch (bknat1)
      {
         case 0 : strncpy((*bknat1_str).s,"UNI",3);
                      break;
         case 1 : strncpy((*bknat1_str).s,"MULTI",5);
                      break;
         default : strncpy((*bknat1_str).s,"RESERVED",8);
                      break;
      } 

/*
 traitement de bknat2
 00 = donnees
 01 = info
 10 = descripteur 3-D
 11 = marqueurs
*/
 
    
     bknat2 = value;
     bknat2 = ( bknat2 << 3); 
     bknat2 = ( bknat2 >> 14); 
     switch (bknat2)
      {
         case 0 : strncpy((*bknat2_str).s,"DATA",4);
                      break;
         case 1 : strncpy((*bknat2_str).s,"INFO",4);
                      break;
         case 2 : strncpy((*bknat2_str).s,"3-D",3);
                      break;
         case 3 : strncpy((*bknat2_str).s,"MRQR",4);
                      break;
         default : strncpy((*bknat2_str).s,"RESERVED",8);
                      break;
      }

 
/*
 traitement de bktyp1
 00 = SFC
 01 = ALT
*/     

     bktyp1 = value;
     bktyp1 = (bktyp1 << 5);
     bktyp1 = (bktyp1 >> 15);
     switch (bktyp1)
      {
         case 0 : strncpy((*bktyp1_str).s,"SFC",3);
                      break;
         case 1 : strncpy((*bktyp1_str).s,"ALT",3);
                      break;
         default : strncpy((*bktyp1_str).s,"RESERVED",8);
                      break;
      }

/*
 traitement de bktyp2
 0 = observations (ADE)
 1 = observations brutes (NON DECODEES)
 ...............
 ...............
 22 = donnees SSMI
 23 @ 63 = EN RESERVE
*/

     bktyp2 = value;
     bktyp2 = (bktyp2 << 6);
     bktyp2 = (bktyp2 >> 10);
     switch (bktyp2)
      {
         case 0  : strncpy((*bktyp2_str).s,"OBS_ADE",7);
                      break;
         case 1  : strncpy((*bktyp2_str).s,"OBS_BRUTES",10);
                      break;
         case 2  : strncpy((*bktyp2_str).s,"DERI_ALT_GLO",12);
                      break;
         case 3  : strncpy((*bktyp2_str).s,"DERI_ALT_REG",12);
                      break;
         case 4  : strncpy((*bktyp2_str).s,"DERI_SFC_GLO",12);
                      break;
         case 5  : strncpy((*bktyp2_str).s,"DERI_SFC_REG",12);
                      break;
         case 6  : strncpy((*bktyp2_str).s,"POST_ALT_GLO",12);
                      break;
         case 7  : strncpy((*bktyp2_str).s,"POST_ALT_REG",12);
                      break;
         case 8  : strncpy((*bktyp2_str).s,"POST_SFC_GLO",12);
                      break;
         case 9  : strncpy((*bktyp2_str).s,"POST_SFC_REG",12);
                      break;
         case 10 : strncpy((*bktyp2_str).s,"PRFL_ALT_GLO",12);
                      break;
         case 11 : strncpy((*bktyp2_str).s,"PRFL_ALT_REG",12);
                      break;
         case 12 : strncpy((*bktyp2_str).s,"RESERVED",8);
                      break;
         case 13 : strncpy((*bktyp2_str).s,"RESERVED",8);
                      break;
         case 14 : strncpy((*bktyp2_str).s,"ANAL_ALT_GLO",12);
                      break;
         case 15 : strncpy((*bktyp2_str).s,"ANAL_ALT_REG",12);
                      break;
         case 16 : strncpy((*bktyp2_str).s,"ANAL_SFC_GLO",12);
                      break;
         case 17 : strncpy((*bktyp2_str).s,"ANAL_SFC_REG",12);
                      break;
         case 18 : strncpy((*bktyp2_str).s,"PREV_GLO",8);
                      break;
         case 19 : strncpy((*bktyp2_str).s,"PREV_REG",8);
                      break;
         case 20 : strncpy((*bktyp2_str).s,"STAT_PENSE",10);
                      break;
         case 21 : strncpy((*bktyp2_str).s,"STAT_KALMAN",11);
                      break;
         case 22 : strncpy((*bktyp2_str).s,"SSMI",4);
                      break;

         default : strncpy((*bktyp2_str).s,"RESERVED",8);
                      break;
      }



     return(bknat1);

}


IDL_LONG i_idl_runn_type(int argc,void* argv[])
{

/*
 decomposition de la runn 

   runn    =      rnat        |       rtyp     
   8  bits =      2 bits      |       6 bits  
           =  rnat1  | rnat2  | rtyp1  | rtyp2
           =  1 bits | 1 bits | 3 bit  | 3 bits 
*/

      unsigned short int value,runn,rnat1,rnat2,rtyp1, rtyp2;
      IDL_STRING *rnat1_str;
      IDL_STRING *rnat2_str;
      IDL_STRING *rtyp1_str;
      IDL_STRING *rtyp2_str;


     /* Insure that the correct number of arguments were passed in */
     if(argc != 5) {
       Lib_Log(APP_LIBBRP,APP_ERROR,"%s: Wrong number of arguments (5) passed=%d\n",__func__,argc);
       return -1;
     }

     /* Cast the pointer in argv to the pointer variables */
     value       =  *( unsigned short int *) argv[0];
     rnat1_str  =           ( IDL_STRING *) argv[1];
     rnat2_str  =           ( IDL_STRING *) argv[2];
     rtyp1_str  =           ( IDL_STRING *) argv[3];
     rtyp2_str  =           ( IDL_STRING *) argv[4];

/* traitement de rnat1 
  0 = REGUL
  1 = PARAL
*/ 
     rnat1 = value;
     rnat1 = (rnat1 << 8);
     rnat1 = (rnat1 >> 15);
     switch (rnat1)
      {
         case 0 : strncpy((*rnat1_str).s,"REGUL",5);
                      break;
         case 1 : strncpy((*rnat1_str).s,"PARAL",5);
                      break;
         default : strncpy((*rnat1_str).s,"RESERVED",8);
                      break;
      } 

/*
 traitement de rnat2
 0 = GLO
 1 = REG
*/
 
    
     rnat2 = value;
     rnat2 = ( rnat2 << 9); 
     rnat2 = ( rnat2 >> 15); 
     switch (rnat2)
      {
         case 0 : strncpy((*rnat2_str).s,"GLO",3);
                      break;
         case 1 : strncpy((*rnat2_str).s,"REG",3);
                      break;
         default : strncpy((*rnat2_str).s,"RESERVED",8);
                      break;
      }

 
/*
 traitement de rtyp1
 000 = AO_ALT_PRELI
 001 = AO_ALT_COMPL
 010 = AO_ALT_FINAL
 011 = AO_SFC_SPECI
 100 = AO_SFC_PRELI
 101 = AO_SFC_COMPL
 110 = AO_SFC_FINAL
 111 = RESERVED
*/     

     rtyp1 = value;
     rtyp1 = (rtyp1 << 10);
     rtyp1 = (rtyp1 >> 13);
     switch (rtyp1)
      {
         case 0 : strncpy((*rtyp1_str).s,"AO_ALT_PRELI",12);
                      break;
         case 1 : strncpy((*rtyp1_str).s,"AO_ALT_COMPL",12);
                      break;
         case 2 : strncpy((*rtyp1_str).s,"AO_ALT_FINAL",12);
                      break;
         case 3 : strncpy((*rtyp1_str).s,"AO_SFC_SPECI",12);
                      break;
         case 4 : strncpy((*rtyp1_str).s,"AO_SFC_PRELI",12);
                      break;
         case 5 : strncpy((*rtyp1_str).s,"AO_SFC_COMPL",12);
                      break;
         case 6 : strncpy((*rtyp1_str).s,"AO_SFC_FINAL",12);
                      break;
         case 7 : strncpy((*rtyp1_str).s,"RESERVED",8);
                      break;
         default : strncpy((*rtyp1_str).s,"RESERVED",8);
                      break;
      }

/*
 traitement de rtyp2
 000 = 00Z
 001 = 03Z
 010 = 06Z
 011 = 09Z
 100 = 12Z
 101 = 15Z
 110 = 18Z
 111 = 21Z
*/

     rtyp2 = value;
     rtyp2 = (rtyp2 << 13);
     rtyp2 = (rtyp2 >> 13);
     switch (rtyp2)
      {
         case 0 : strncpy((*rtyp2_str).s,"00Z",3);
                      break;
         case 1 : strncpy((*rtyp2_str).s,"03Z",3);
                      break;
         case 2 : strncpy((*rtyp2_str).s,"06Z",3);
                      break;
         case 3 : strncpy((*rtyp2_str).s,"09Z",3);
                      break;
         case 4 : strncpy((*rtyp2_str).s,"12Z",3);
                      break;
         case 5 : strncpy((*rtyp2_str).s,"15Z",3);
                      break;
         case 6 : strncpy((*rtyp2_str).s,"18Z",3);
                      break;
         case 7 : strncpy((*rtyp2_str).s,"21Z",3);
                      break;
         default : strncpy((*rtyp2_str).s,"RESERVED",8);
                      break;
      }





     return(1);

}




