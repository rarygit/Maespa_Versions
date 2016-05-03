        !COMPILER-GENERATED INTERFACE MODULE: Tue May 03 14:08:23 2016
        MODULE SORTTREES__genmod
          INTERFACE 
            SUBROUTINE SORTTREES(NOALLTREES,NOTREES,NOTARGET,DX,DY,DZ,R1&
     &,R2,R3,TRUNK,FLT,DIAMA,DXT,DYT,DZT,RX,RY,RZ,FOLT,ZBC,DIAM,ISPECIES&
     &,ISPECIEST,IT)
              INTEGER(KIND=4) :: NOALLTREES
              INTEGER(KIND=4) :: NOTREES
              INTEGER(KIND=4) :: NOTARGET
              REAL(KIND=4) :: DX(5000)
              REAL(KIND=4) :: DY(5000)
              REAL(KIND=4) :: DZ(5000)
              REAL(KIND=4) :: R1(1000,5000)
              REAL(KIND=4) :: R2(1000,5000)
              REAL(KIND=4) :: R3(1000,5000)
              REAL(KIND=4) :: TRUNK(1000,5000)
              REAL(KIND=4) :: FLT(1000,5000)
              REAL(KIND=4) :: DIAMA(1000,5000)
              REAL(KIND=4) :: DXT(5000)
              REAL(KIND=4) :: DYT(5000)
              REAL(KIND=4) :: DZT(5000)
              REAL(KIND=4) :: RX(1000,5000)
              REAL(KIND=4) :: RY(1000,5000)
              REAL(KIND=4) :: RZ(1000,5000)
              REAL(KIND=4) :: FOLT(1000,5000)
              REAL(KIND=4) :: ZBC(1000,5000)
              REAL(KIND=4) :: DIAM(1000,5000)
              INTEGER(KIND=4) :: ISPECIES(5000)
              INTEGER(KIND=4) :: ISPECIEST(5000)
              INTEGER(KIND=4) :: IT(5000)
            END SUBROUTINE SORTTREES
          END INTERFACE 
        END MODULE SORTTREES__genmod
