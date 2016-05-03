        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:23 2016
        MODULE INPUTWATBAL__genmod
          INTERFACE 
            SUBROUTINE INPUTWATBAL(NOSPEC,BPAR,PSIE,KSAT,ROOTRESIST,    &
     &ROOTRESFRAC,ROOTRADTABLE,ROOTSRLTABLE,ROOTMASSTOTTABLE,MINROOTWP, &
     &MINLEAFWP,PLANTKTABLE,KSCALING,THROUGHFALL,REASSIGNRAIN,RUTTERB,  &
     &RUTTERD,MAXSTORAGE,DRAINLIMIT,ROOTXSECAREA,EQUALUPTAKE,NLAYER,    &
     &NROOTLAYER,LAYTHICK,INITWATER,FRACROOTTABLE,POREFRAC,SOILTEMP,    &
     &KEEPWET,KEEPDRY,DRYTHICKMIN,TORTPAR,SIMTSOIL,RETFUNCTION,         &
     &FRACORGANIC,EXPINF,WSOILMETHOD,USEMEASET,USEMEASSW,SIMSOILEVAP,   &
     &USESTAND,ALPHARET,WS,WR,NRET,DATESKP,NOKPDATES,DATESROOT,         &
     &NOROOTDATES,NOROOTSPEC,RFAGEBEGIN,RFPAR1,RFPAR2,RFPAR3,           &
     &ROOTFRONTLIMIT,IWATTABLAYER,PLATDRAIN,ISIMWATTAB,DRYTHERM,SIMSTORE&
     &,STORECOEF,STOREEXP,STOPSIMONEMPTY)
              INTEGER(KIND=4) :: NOSPEC
              REAL(KIND=4) :: BPAR(51)
              REAL(KIND=4) :: PSIE(51)
              REAL(KIND=4) :: KSAT(51)
              REAL(KIND=4) :: ROOTRESIST
              REAL(KIND=4) :: ROOTRESFRAC
              REAL(KIND=4) :: ROOTRADTABLE(1000)
              REAL(KIND=4) :: ROOTSRLTABLE(1000)
              REAL(KIND=4) :: ROOTMASSTOTTABLE(1000)
              REAL(KIND=4) :: MINROOTWP
              REAL(KIND=4) :: MINLEAFWP(4)
              REAL(KIND=4) :: PLANTKTABLE(1000)
              REAL(KIND=4) :: KSCALING
              REAL(KIND=4) :: THROUGHFALL
              INTEGER(KIND=4) :: REASSIGNRAIN
              REAL(KIND=4) :: RUTTERB
              REAL(KIND=4) :: RUTTERD
              REAL(KIND=4) :: MAXSTORAGE
              REAL(KIND=4) :: DRAINLIMIT(51)
              REAL(KIND=4) :: ROOTXSECAREA
              INTEGER(KIND=4) :: EQUALUPTAKE
              INTEGER(KIND=4) :: NLAYER
              INTEGER(KIND=4) :: NROOTLAYER
              REAL(KIND=4) :: LAYTHICK(51)
              REAL(KIND=4) :: INITWATER(51)
              REAL(KIND=4) :: FRACROOTTABLE(51,1000,4)
              REAL(KIND=4) :: POREFRAC(51)
              REAL(KIND=4) :: SOILTEMP(51)
              INTEGER(KIND=4) :: KEEPWET
              INTEGER(KIND=4) :: KEEPDRY
              REAL(KIND=4) :: DRYTHICKMIN
              REAL(KIND=4) :: TORTPAR
              INTEGER(KIND=4) :: SIMTSOIL
              INTEGER(KIND=4) :: RETFUNCTION
              REAL(KIND=4) :: FRACORGANIC(51)
              REAL(KIND=4) :: EXPINF
              INTEGER(KIND=4) :: WSOILMETHOD
              INTEGER(KIND=4) :: USEMEASET
              INTEGER(KIND=4) :: USEMEASSW
              INTEGER(KIND=4) :: SIMSOILEVAP
              INTEGER(KIND=4) :: USESTAND
              REAL(KIND=4) :: ALPHARET(51)
              REAL(KIND=4) :: WS(51)
              REAL(KIND=4) :: WR(51)
              REAL(KIND=4) :: NRET(51)
              INTEGER(KIND=4) :: DATESKP(1000)
              INTEGER(KIND=4) :: NOKPDATES
              INTEGER(KIND=4) :: DATESROOT(1000)
              INTEGER(KIND=4) :: NOROOTDATES
              INTEGER(KIND=4) :: NOROOTSPEC
              INTEGER(KIND=4) :: RFAGEBEGIN(4)
              REAL(KIND=4) :: RFPAR1(4)
              REAL(KIND=4) :: RFPAR2(4)
              REAL(KIND=4) :: RFPAR3(4)
              INTEGER(KIND=4) :: ROOTFRONTLIMIT
              INTEGER(KIND=4) :: IWATTABLAYER
              REAL(KIND=4) :: PLATDRAIN
              INTEGER(KIND=4) :: ISIMWATTAB
              REAL(KIND=4) :: DRYTHERM
              INTEGER(KIND=4) :: SIMSTORE
              REAL(KIND=4) :: STORECOEF
              REAL(KIND=4) :: STOREEXP
              INTEGER(KIND=4) :: STOPSIMONEMPTY
            END SUBROUTINE INPUTWATBAL
          END INTERFACE 
        END MODULE INPUTWATBAL__genmod