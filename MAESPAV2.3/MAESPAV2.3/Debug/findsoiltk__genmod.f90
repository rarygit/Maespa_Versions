        !COMPILER-GENERATED INTERFACE MODULE: Wed Nov 30 18:40:55 2016
        MODULE FINDSOILTK__genmod
          INTERFACE 
            SUBROUTINE FINDSOILTK(IDAY,TAIRK,GAMSOIL,PRESSPA,SOILTK,    &
     &SOILTK2,VPDKPA,RGLOB,THERMCOND1,LAYTHICK1,LAYTHICK2,POREFRAC1,    &
     &SOILWP1,DRYTHICK,TORTPAR,VIEWFACTOR,RHOSOLSPEC,RGLOBUND1,RGLOBUND2&
     &,DOWNTHAV,DRYTHERM)
              INTEGER(KIND=4) :: IDAY
              REAL(KIND=4) :: TAIRK
              REAL(KIND=4) :: GAMSOIL
              REAL(KIND=4) :: PRESSPA
              REAL(KIND=4) :: SOILTK
              REAL(KIND=4) :: SOILTK2
              REAL(KIND=4) :: VPDKPA
              REAL(KIND=4) :: RGLOB
              REAL(KIND=4) :: THERMCOND1
              REAL(KIND=4) :: LAYTHICK1
              REAL(KIND=4) :: LAYTHICK2
              REAL(KIND=4) :: POREFRAC1
              REAL(KIND=4) :: SOILWP1
              REAL(KIND=4) :: DRYTHICK
              REAL(KIND=4) :: TORTPAR
              REAL(KIND=4) :: VIEWFACTOR
              REAL(KIND=4) :: RHOSOLSPEC(1:3,5)
              REAL(KIND=4) :: RGLOBUND1
              REAL(KIND=4) :: RGLOBUND2
              REAL(KIND=4) :: DOWNTHAV
              REAL(KIND=4) :: DRYTHERM
            END SUBROUTINE FINDSOILTK
          END INTERFACE 
        END MODULE FINDSOILTK__genmod
