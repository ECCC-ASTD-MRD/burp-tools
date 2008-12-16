#!/usr/bin/ksh
set -x
echo USING $0
PrOgRaM=${0##*/}
DEBUG=${1:-NO}
echo $DEBUG
rm burp_module.f90

if [ "$DEBUG" == "NO" ] 
then

   echo 'include "f2kcli.f90"' >> burp_module.f90
   echo 'include "burp_constants.f90"' >> burp_module.f90
   echo 'include "errormessages.f90"' >> burp_module.f90
   echo 'include "librmn_declaration.f90"' >> burp_module.f90
   echo 'include "object_initialization.f90"' >> burp_module.f90
   echo 'include "conversion_module.f90"' >> burp_module.f90
   echo 'include "burp_block_class.f90"' >> burp_module.f90
   echo 'include "burp_rpt_class.f90"' >> burp_module.f90
   echo 'include "burp_file_class.f90"' >> burp_module.f90
   echo 'include "burp_btyp_runn_flgs.f90"' >> burp_module.f90

   echo       "module burp_module" >> burp_module.f90
   echo       "  use f2kcli" >> burp_module.f90
   echo       "  use burp_constants" >> burp_module.f90
   echo       "  use errormessages" >> burp_module.f90
   echo       "  use librmn_declaration" >> burp_module.f90
   echo       "  use object_initialization" >> burp_module.f90
   echo       "  use conversion_module" >> burp_module.f90
   echo       "  use burp_block_class" >> burp_module.f90
   echo       "  use burp_rpt_class" >> burp_module.f90
   echo       "  use burp_file_class" >> burp_module.f90
   echo       "  use burp_btyp_runn_flgs" >> burp_module.f90
   echo       "end module burp_module" >> burp_module.f90
else
   cat f2kcli.f90 >> burp_module.f90
   cat burp_constants.f90 >> burp_module.f90
   cat errormessages.f90 >> burp_module.f90
   cat librmn_declaration.f90 >> burp_module.f90
   cat object_initialization.f90 >> burp_module.f90
   cat conversion_module.f90 >> burp_module.f90
   cat burp_block_class.f90 >> burp_module.f90
   cat burp_rpt_class.f90 >> burp_module.f90
   cat burp_file_class.f90 >> burp_module.f90
   cat burp_btyp_runn_flgs.f90 >> burp_module.f90

   echo       "module burp_module" >> burp_module.f90
   echo       "  use f2kcli" >> burp_module.f90
   echo       "  use burp_constants" >> burp_module.f90
   echo       "  use errormessages" >> burp_module.f90
   echo       "  use librmn_declaration" >> burp_module.f90
   echo       "  use object_initialization" >> burp_module.f90
   echo       "  use conversion_module" >> burp_module.f90
   echo       "  use burp_block_class" >> burp_module.f90
   echo       "  use burp_rpt_class" >> burp_module.f90
   echo       "  use burp_file_class" >> burp_module.f90
   echo       "  use burp_btyp_runn_flgs" >> burp_module.f90
   echo       "end module burp_module" >> burp_module.f90
fi
