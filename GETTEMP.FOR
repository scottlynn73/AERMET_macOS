      SUBROUTINE GETTEMP
C=====================================================================
C     GETTEMP Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Extract TEMP data for the current day based on 
C               available ONSITE and/or NWS data.  This is done
C               prior to full processing within MPPBL to allow
C               for substitutions for missing data by interpolating
C               across 1 or 2-hour gaps in TEMP data.
c
C
C     Called By: MPPBL
C
C---- Local variables --------------------------------------------------
C
C     ZMAX4T  - maximum height in on-site profile for which to search
C               for a reference level for temperature
C     LTZMAX4T- logical that indicates if the on-site profile level is
C               at or below ZMAX4T
C-----------------------------------------------------------------------

      IMPLICIT NONE

      REAL, PARAMETER :: ZMAX4T=100.0

      INTEGER   IH
      INTEGER   ILEVEL, ILVL, IZTMP
      LOGICAL   LTZMAX4T

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

      GOTTMP(:)    = .FALSE.
      GOT_OSTMP(:) = .FALSE.
      LTZMAX4T     = .TRUE.

C --- Save previous day's values for TEMP23 and TEMP24 to array element 2, 
C     along with ZTREF values
      TEMP23(2) = TEMP23(1)
      TEMP24(2) = TEMP24(1)
      ZTREF23(2)= ZTREF23(1)
      ZTREF24(2)= ZTREF24(1)
      
      DO IH = 1, 24
      
         IF( STATUS(4,16) .GE. 2 )THEN
C           The OSHEIGHTS keyword is in use; the data associated with the
C           keyword will be used to fill the on-site heights array,
C           OSVOBS(hour,level,1), and heights specified within the data
C           using the HTnn code will be ignored.
            DO ILEVEL = 1,OSNL
               OSVOBS(IH,ILEVEL,1) = OSHT(ILEVEL)
            ENDDO
         ENDIF

C                              TEMPERATURE
C     ------------------------------------------------------------------
C        Search for the lowest level of nonmissing temperature data
C        from the onsite profile
C     ------------------------------------------------------------------

C        OSVOBS(hour,level,7) = ambient temperature
C        OSVOBS(hour,level,1) = profile height
C        IZTMP = counter for the profile levels

C ---    Initialize counter for profile level and logical for height < ZMAX4T (100m)
         IZTMP = 1
         LTZMAX4T = .TRUE.

         DO WHILE( (IZTMP.LE.OSNL) .AND. (.NOT.GOTTMP(IH)) .AND. 
     &                                                      LTZMAX4T )

            IF( ABS(OSVOBS(IH,IZTMP,7)-OSQA(21,2)).GT. 0.01
     &    .AND. ABS(OSVOBS(IH,IZTMP,1)-OSQA(15,2)).GT. 0.01 )THEN

C              The temperature at this level is not missing;
C              is the profile level above the roughness length but
C              below ZMAX4T?

               IF( OSVOBS(IH,IZTMP,1) .GT. Z0(IH)  .AND.
     &             OSVOBS(IH,IZTMP,1) .LE. ZMAX4T )THEN

C                 The height is below ZMAX4T; convert to kelvins
C                 and save the information
                  GOTTMP(IH)    = .TRUE.
                  GOT_OSTMP(IH) = .TRUE.
                  T(IH)         = OSVOBS(IH,IZTMP,7) + 273.15
                  ZTREF(IH)     = OSVOBS(IH,IZTMP,1)

               ELSE
C                 Measurement height exceeds the maximum allowable
C                 for temperature
                  LTZMAX4T = .FALSE.

               ENDIF

             ELSE            !  height and/or temperature is missing
C               At least one piece of data was missing at this
C               level, so increment the height/level counter.

                IZTMP = IZTMP + 1

             ENDIF

C        End 'do while' for onsite temperature
         ENDDO

         IF( GOT_OSTMP(IH) )THEN
C ---       ONSITE temperature is available for this hour; set ITMPSUB flag to 0
            ITMPSUB(IH) = 0
            CONTINUE

         ELSEIF( SUBSTNWS )THEN
C ---       No ONSITE temperature available, but the user specified NWS substitution
        
            IF( SFOBS(IH,46) .NE. SFQA(46,2) )THEN
C ---          NWS temperature is not missing - substitute for missing OS
        
               GOTTMP(IH)  = .TRUE.
               T(IH)       = REAL(SFOBS(IH,46)) / 10.0 + 273.15
               ITMPSUB(IH) = 0
               ZTREF(IH)   = ZNWST
        
            ENDIF

         ENDIF

C ----   Save TEMP for hours 23 and 24 if available for use on next day
C        if substitutions are needed
         IF( GOTTMP(IH) .AND. IH .EQ. 23 )THEN
            TEMP23(1) = T(IH)
            ZTREF23(1)= ZTREF(IH)
         ELSEIF( .NOT.GOTTMP(IH) .AND. IH .EQ. 23 )THEN
            TEMP23(1) = 999.0
            ZTREF23(1)= -9.0
         ENDIF

         IF( GOTTMP(IH) .AND. IH .EQ. 24 )THEN
            TEMP24(1) = T(IH)
            ZTREF24(1)= ZTREF(IH)
         ELSEIF( .NOT.GOTTMP(IH) .AND. IH .EQ. 24 )THEN
            TEMP24(1) = 999.0
            ZTREF24(1)= -9.0
         ENDIF

         IF( .NOT. GOTTMP(IH) )THEN
C ---       Assign missing code of 999 for this hour; substitutions will be
C           done in SUBST if possible
            T(IH)     = 999.0
            ZTREF(IH) = -9.0
         ENDIF

      ENDDO
      
      RETURN
      END
      


