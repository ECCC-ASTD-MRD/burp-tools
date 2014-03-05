MODULE burp_constants
  !
  ! external burp data types:
  !
  integer, parameter, public ::   &
    burp_byte     = 1,            &
    burp_int1     = burp_byte,    &
    burp_char     = 2,            &
    burp_short    = 3,            &
    burp_int2     = burp_short,   &
    burp_int      = 4,            &
    burp_int4     = burp_int,     &
    burp_float    = 5,            &
    burp_real     = burp_float,   &
    burp_real4    = burp_float,   &
    burp_double   = 6,            &
    burp_real8    = burp_double,  &
    int_def       = KIND(0),      &
    burp_integer  = int_def

  integer, parameter, public ::   &
    BUFR_to_MKSA     = 0,         &
    MKSA_to_BUFR     = 1
  !
  ! default file acces values:
  !
  character (len = 20), parameter, public :: &
    FILE_ACC_READ    = "READ",               &
    FILE_ACC_CREATE  = "CREATE",             &
    FILE_ACC_APPEND  = "APPEND",             &
    BURP_REAL_OPT_NAME  = "MISSING",         &
    BURP_CHAR_OPT_NAME  = "MSGLVL",          &
    BURP_CHAR_OPT_VALUE = "FATAL"

   real, parameter, public  ::               &
    burp_real_opt_value = -99.99
  !
  ! error codes:
  !
  integer, parameter, public :: &
    burp_noerr        = 0,      &
    rmn_TRIVIA        = 1,      &
    rmn_INFORM        = 2,      &
    rmn_WARNIN        = 3,      &
    rmn_ERROR         = 4,      &
    rmn_ERFATAL       = 5,      &
    rmn_SYSTEM        = 6,      &
    rmn_KAPUT         = 7,      &
    rmn_EREDAT        = 16,     &
    rmn_ERFRAP        = 30,     &
    rmn_ERFMOD        = 31,     &
    rmn_ERCLEF        = 32,     &
    rmn_ERBNUM        = 33,     &
    rmn_EROPTN        = 34,     &
    rmn_ERBTAB        = 35,     &
    rmn_ERBDOM        = 36,     &
    rmn_ERELEM        = 37,     &
    rmn_ERBTYP        = 38,     &
    rmn_ERRCELL       = 39,     &
    rmn_ERNPRM        = 40,     &
    rmn_ERCMPR        = 41,     &
    rmn_NONOFTB       = 42,     &
    rmn_ERBDSC        = 43,     &
    rmn_EREIVC        = 44

  !
  ! error string :
  !
  character (len = 80), parameter, public ::                         &
    rmn_TRIVIA_s        = "Trivial Error",                           &
    rmn_INFORM_s        = "Information messages for the user",       &
    rmn_WARNIN_s        = "Warning Error",                           &
    rmn_ERROR_s         = "Utmost Important Error",                  &
    rmn_ERFATAL_s       = "Errors the user know",                    &
    rmn_SYSTEM_s        = "Overflow Error",                          &
    rmn_KAPUT_s         = "Intolerable error, pgm crashed",          &
    rmn_EREDAT_s        = "Invalid Datyp",                           &
    rmn_ERFRAP_s        = "File is not a report file",               &
    rmn_ERFMOD_s        = "Only READ, CREATE and APPEND allowed",    &
    rmn_ERCLEF_s        = "Too many supplementary keys",             &
    rmn_ERBNUM_s        = "Block number invalid",                    &
    rmn_EROPTN_s        = "Option name unknown",                     &
    rmn_ERBTAB_s        = "Fatal error related to TABLEBURP",        &
    rmn_ERBDOM_s        = "Trivial error related  TABLEBURP",        &
    rmn_ERELEM_s        = "Invalid element name",                    &
    rmn_ERBTYP_s        = "Invalid BTP (smaller than zero",          &
    rmn_ERRCELL_s       = "Incorrect NCELL dimension",               &
    rmn_ERNPRM_s        = "Incorrect TBLPRM dimension",              &
    rmn_ERCMPR_s        = "Value too big for 32 bits and DATYP=2",   &
    rmn_NONOFTB_s       = "File created with non-official TABLEBURP",&
    rmn_ERBDSC_s        = "Bad initialization of BDESC",             &
    rmn_EREIVC_s        = "Element code Invalid for DATYP=(7 to 9)"

END MODULE burp_constants
