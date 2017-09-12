      SUBROUTINE GETCCVR
C=====================================================================
C     GETCCVR Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Extract CCVR data for the current day based on 
C               available ONSITE and/or NWS data.  This is done
C               prior to full processing within MPPBL to allow
C               for substitutions for missing data by interpolating
C               across 1 or 2-hour gaps in CCVR.
c
C
C     Called By: MPPBL
C
C     ------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER   IH
      INTEGER   ITOTAL, IOPAQ, MISTOT, MISOPQ

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

C --- Save previous day's values for ICCVR23 and ICCVR24 to array element 2
      ICCVR23(2) = ICCVR23(1)
      ICCVR24(2) = ICCVR24(1)
      
      DO IH = 1, 24
      
         IF( ABS(OSSOBS(IH,19)-REAL(NO_SKY)) .GT. 0.01 )THEN    
C ---       Onsite cloud cover is available
            CCVR(IH)   = NINT(OSSOBS(IH,19))
            GOTSKY(IH) = .TRUE.

         ELSE
C ---       Note: SUBSTNWS option is not required for CCVR subs
C ---       No onsite CCVR; check for NWS substitution   ! Use NWS

c           First we split the concatenated variable TOTAL//OPAQUE
c           and the missing value indicator.

            ITOTAL = SFOBS(IH,34) / 100
            IOPAQ  = SFOBS(IH,34) - ITOTAL * 100
            MISTOT = SFQA(34,2) / 100
            MISOPQ = SFQA(34,2) - MISTOT * 100

c           Check opaque cloud cover first and use if it's not missing

            IF( IOPAQ .NE. MISOPQ )THEN                 ! Use opaque
               CCVR(IH)   = IOPAQ
               GOTSKY(IH) = .TRUE.

            ELSEIF( ITOTAL .NE. MISTOT )THEN            ! Use total
               CCVR(IH)   = ITOTAL
               GOTSKY(IH) = .TRUE.

            ELSEIF(SFOBS(IH,43) .NE. SFQA(43,2))THEN    ! Use ASOS
c              ! Check for ASOS data
               CCVR(IH)   = SFOBS(IH,43)
               GOTSKY(IH) = .TRUE.

            ELSE
C ----         CCVR is missing; apply missing code to CCVR 
               CCVR(IH)   = 99
               GOTSKY(IH) = .FALSE.

            ENDIF

         ENDIF

C ----   Save CCVR for hours 23 and 24 if available for use on next day
C        if substitutions are needed
         IF( GOTSKY(IH) .AND. IH .EQ. 23 )THEN
            ICCVR23(1) = CCVR(IH)
         ELSEIF( .NOT.GOTSKY(IH) .AND. IH .EQ. 23 )THEN
            ICCVR23(1) = 99
         ENDIF

         IF( GOTSKY(IH) .AND. IH .EQ. 24 )THEN
            ICCVR24(1) = CCVR(IH)
         ELSEIF( .NOT.GOTSKY(IH) .AND. IH .EQ. 24 )THEN
            ICCVR24(1) = 99
         ENDIF

         IF( .NOT. GOTSKY(IH) )THEN
C ---       Assign missing code of 99 for this hour; 
C           substitutions will be done in SUBST if possible
            CCVR(IH) = 99

         ENDIF

      ENDDO
      
      RETURN
      END
      


