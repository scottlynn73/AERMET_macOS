      SUBROUTINE CHRCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C          CHRCRD Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Processes the surface characteristics
C
C     Initial Release:  December 1992
C
C     Revision History:
C          1/27/97  moved from ONSITE pathway
C
C          9/18/2013  Incorporated additional error handling for 
C                     invalid sector IDs
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE

      CHARACTER CARD*(*)
      INTEGER   I, J, ISTAT,FREQ,SECT,ITEST,MN1,MN2
      INTEGER   FREQ_PREV, SECT_PREV
      REAL      ALBED,BOWENR,ROUGH

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'                      
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT    Process status 1 = error in processing
C                              2 = processing ok
C      CARD     Record with the surface characteristics
C      FREQ     Frequency index,
C                      If MONTHLY, then FREQ is 1 through 12
C                      If SEASONAL, then FREQ is 1 through 4 
C                         1 = winter months: 12,1,2
C                         2 = spring months:  3,4,5
C                         3 = summer months:  6,7,8
C                         4 = autumn months:  9,10,11
C                      If ANNUAL, FREQ is 1
C      SECT     Wind direction sector, must be .le. OSNWDS
C      ICHCNT   Number of valid SITE_CHAR keywords encountered
C      GOTCHR   Tracks presence of valid surface characteristics
C               based on frequency and sector; array with values
C               of 0 and 1

C---- Data Initializations

      DATA FREQ_PREV/0/
      DATA SECT_PREV/0/
      PATH = 'METPREP'
      LOC  = 'CHRCRD' 
      ISTAT = 0


C---- Check the number of fields on the record: must be 6

      IF( NWORDS.LT.6 )THEN
         ECODE = 'E04'
         MESS =  BLNK80      
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''SITE_CHAR'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ELSEIF( NWORDS.GT.6 )THEN
         ECODE = 'E04'
         MESS =  BLNK80      
         WRITE( MESS,1100 )
1100     FORMAT(' Too many fields on ''SITE_CHAR'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF 

C---- Check that a valid FREQ_SECT keyword is defined

      IF( SFCFRQ.EQ.0 )THEN
C------- FREQ_SECT not defined
         ECODE = 'E15'
         MESS =  BLNK80
         WRITE( MESS,2000 ) 
2000     FORMAT(' Missing or misplaced ''FREQ_SECT'' keyword; ',
     &          '''FREQ_SECT'' must appear before ''SITE_CHAR''.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         OSNWDS = 0
         RETURN

      ELSEIF( SFCFRQ.EQ.1 )THEN
C------- FREQ_SECT in error
         ECODE = 'E15'
         MESS =  BLNK80
         WRITE( MESS,2100 ) 
2100     FORMAT(' Error on ''FREQ_SECT''; ',
     &          '''SITE_CHAR'' NOT processed!')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         OSNWDS = 0
         RETURN
      ENDIF

C---- Decipher surface characteristics

      FREQ = 0
      SECT = 0
      ALBED  = 0.0
      BOWENR = 0.0
      ROUGH  = 0.0
      CALL VALCRD( KOUNT,CARD,FREQ,SECT,ALBED,BOWENR,ROUGH,ITEST )
      IF( ITEST .EQ. 1 )THEN
         ISTAT = ITEST
         RETURN
      ENDIF

C---- First check for FREQ out-of-range
      IF( FREQ .LE. 0 )THEN
C------- Invalid frequency index
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,4000 ) FREQ
4000     FORMAT(' Invalid FREQ index (',I2,') for ''SITE_CHAR''')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ELSEIF( FREQ .GT. NKFREQ )THEN
C------- Invalid frequency index
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,4010 ) FREQ, NKFREQ
4010     FORMAT(' FREQ index (',I2,') > # allowed (',I2,
     &          ') for ''SITE_CHAR''')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

      IF( SECT .LE. 0 )THEN
C------- Invalid sector index
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,4001 ) SECT
4001     FORMAT(' Invalid SECTOR index (',I2,') for ''SITE_CHAR''')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ELSEIF( SECT .GT. OSNWDS )THEN
C------- Invalid sector index
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,4011 ) SECT, OSNWDS
4011     FORMAT(' SECTOR index (',I2,') > # allowed (',I2,
     &          ') for ''SITE_CHAR''')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

      GOTO 9876

C---- Check for possible gaps in FREQ inputs
      IF( FREQ .NE. FREQ_PREV .AND. 
     &    FREQ .NE. FREQ_PREV+1 .AND.
     &              FREQ_PREV .NE. NKFREQ )THEN
C------- Frequency index is out of order;
C        set ISTAT = 1 and return
         ISTAT = 1
         FREQ_PREV = FREQ
         RETURN
      ELSEIF( FREQ .EQ. FREQ_PREV .AND. 
     &        SECT .LT. SECT_PREV )THEN
C------- Sector index is out of order 
C        set ISTAT = 1, assign to FREQ_PREV
C        and return
         ISTAT = 1
         FREQ_PREV = FREQ
         RETURN
      ELSE
         FREQ_PREV = FREQ
      ENDIF

C---- Check for possible gaps in SECT inputs
      IF( SECT .NE. SECT_PREV .AND. 
     &    SECT .NE. SECT_PREV+1 .AND.
     &              SECT_PREV .NE. NUMSEC )THEN
C------- Sector index is out of order 
C        set ISTAT = 1, assign to SECT_PREV
C        and return
         ISTAT = 1
         SECT_PREV = SECT
         RETURN
      ELSE
         SECT_PREV = SECT
      ENDIF

9876  continue

C---- Check for this SECTOR and FREQUENCY combination has already
C     been "validated" (i.e., GOTCHR(FREQ,SECT) = 1). This means
C     that a FREQ/SECT combination has been duplicated.
      IF( GOTCHR(FREQ,SECT) .EQ. 1 )THEN
         MESS =  BLNK80
         WRITE( MESS,4300 ) FREQ, SECT
4300     FORMAT(' FREQ index (',I2,') and SECT index (',I2,') has',
     &          ' already been specified')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         SECT_PREV = SECT
         RETURN
      ENDIF

C---- Check the value of the surface characteristics in a general sense

      IF( ALBED.LE.0.0 .OR. ALBED.GT.1.0 )THEN
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,4500 ) ICHCNT,albed
4500     FORMAT(' ALBEDO invalid on primary SITE_CHAR # ',
     &          I3,': ',F8.3)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

      IF( BOWENR .LE. 0.0 )THEN
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,4600 ) ICHCNT,bowenr
4600     FORMAT(' BOWEN RATIO invalid on primary SITE_CHAR # ',
     &          I3,': ',F8.3)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

      IF( ROUGH .LE. 0.0 )THEN
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,4700 ) ICHCNT,rough
4700     FORMAT(' ROUGHNESS invalid on primary SITE_CHAR # ',
     &          I3,': ',F8.5)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Store values in OSSFC arrays

      IF( SFCFRQ.EQ.2 )THEN
C------- The FREQ-SECT keyword was successfully processed and is
C        defined as MONTHLY

C------- Check frequency on the SITE_CHAR keyword
         IF( FREQ.GE.1 .AND. FREQ.LE.12 )THEN
            OSSFC(FREQ,SECT,1) = ALBED
            OSSFC(FREQ,SECT,2) = BOWENR
            OSSFC(FREQ,SECT,3) = ROUGH
            IF( ISTAT .NE. 1 ) ISTAT = 2
         ELSE
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5000 ) FREQ
5000        FORMAT(' Primary frequency index =', I3,
     &             '; out of range for MONTHLY option')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSEIF( SFCFRQ.EQ.3 )THEN
C------- The FREQ-SECT keyword was successfully processed and is
C        defined as SEASONAL

C------- Check frequency on the SITE_CHAR keyword

         IF( FREQ.GE.1 .AND. FREQ.LE.4 )THEN
C---------- Loop on proper months for given 'SEASON'
            MN1 = 12 + (FREQ-1)*3                 
            MN2 = MN1 + 2
            DO I=MN1,MN2
               IF( I.GT.12 )THEN
                  J = I - 12
               ELSE
                  J = I
               ENDIF
               OSSFC(J,SECT,1) = ALBED
               OSSFC(J,SECT,2) = BOWENR
               OSSFC(J,SECT,3) = ROUGH

            ENDDO
            IF( ISTAT .NE. 1 ) ISTAT = 2

         ELSE
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5100 ) FREQ
5100        FORMAT(' Primary frequency index =', I3,
     &             '; out of range for SEASONAL option')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1           
         ENDIF
           
      ELSEIF( SFCFRQ.EQ.4 )THEN
C------- The FREQ-SECT keyword was successfully processed and is
C        defined as ANNUAL

C------- Check frequency on the SITE_CHAR keyword
         IF( FREQ.EQ.1 )THEN
            DO I=1,12
               OSSFC(I,SECT,1) = ALBED
               OSSFC(I,SECT,2) = BOWENR
               OSSFC(I,SECT,3) = ROUGH
            ENDDO
            IF( ISTAT.NE. 1 ) ISTAT = 2

         ELSE
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5200 ) FREQ
5200        FORMAT(' Primary frequency index =', I3,
     &             '; out of range for ANNUAL option')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1           

         ENDIF
      ENDIF

      IF( ISTAT .EQ. 2 )THEN
C------- Increment the counters:
C        ICHCNT = total number of valid SITE_CHAR keywords
C        GOTCHR = array of SITE_CHAR keywords based on frequency
C                 and sector: 1 ==> valid characteristics for the
C                                   period and sector 
         ICHCNT = ICHCNT + 1
         GOTCHR(FREQ,SECT) = 1
      ENDIF

      RETURN
      END

