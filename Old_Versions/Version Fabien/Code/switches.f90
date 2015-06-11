MODULE switches
    IMPLICIT NONE
    
    INTEGER :: IOHRLY       ! Controls daily, hourly, and/or layer output
    INTEGER :: IOTUTD       ! Controls transmittance file output
    INTEGER :: IOHIST       ! Contro  histogram output
    INTEGER :: IORESP       ! Controls respiration output
    INTEGER :: IODAILY      ! Controls daily output: FIXED HERE 
    INTEGER :: IOWATBAL     ! Controls water balance output
    INTEGER :: IOFORMAT     ! default, write is ascii
    INTEGER :: ISUNLA  ! Modification M. Christina 27/11/12

    
END MODULE switches 
    
    
