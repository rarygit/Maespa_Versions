        !COMPILER-GENERATED INTERFACE MODULE: Fri Oct 07 14:37:29 2016
        MODULE SORTTREESP__genmod
          INTERFACE 
            SUBROUTINE SORTTREESP(X,Y,NOALLTREES,NOTREES,DX,DY,DZ,R1,R2,&
     &R3,TRUNK,FLT,DIAMA,DXT,DYT,DZT,RX,RY,RZ,FOLT,ZBC,DIAM,ISPECIES,   &
     &ISPECIEST,IT)
              REAL(KIND=4) :: X
              REAL(KIND=4) :: Y
              INTEGER(KIND=4) :: NOALLTREES
              INTEGER(KIND=4) :: NOTREES
              REAL(KIND=4) :: DX(5000)
              REAL(KIND=4) :: DY(5000)
              REAL(KIND=4) :: DZ(5000)
              REAL(KIND=4) :: R1(30,5000)
              REAL(KIND=4) :: R2(30,5000)
              REAL(KIND=4) :: R3(30,5000)
              REAL(KIND=4) :: TRUNK(30,5000)
              REAL(KIND=4) :: FLT(30,5000)
              REAL(KIND=4) :: DIAMA(30,5000)
              REAL(KIND=4) :: DXT(5000)
              REAL(KIND=4) :: DYT(5000)
              REAL(KIND=4) :: DZT(5000)
              REAL(KIND=4) :: RX(30,5000)
              REAL(KIND=4) :: RY(30,5000)
              REAL(KIND=4) :: RZ(30,5000)
              REAL(KIND=4) :: FOLT(30,5000)
              REAL(KIND=4) :: ZBC(30,5000)
              REAL(KIND=4) :: DIAM(30,5000)
              INTEGER(KIND=4) :: ISPECIES(5000)
              INTEGER(KIND=4) :: ISPECIEST(5000)
              INTEGER(KIND=4) :: IT(5000)
            END SUBROUTINE SORTTREESP
          END INTERFACE 
        END MODULE SORTTREESP__genmod
