       SUBROUTINE MPMET( ISTAT )
C======================================================================
C          MPMET Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Controls the calls to three subroutines
C               that in turn provide the following actions:
C                    1) fetch one day's merged meteorology (FETCH)
C                    2) process (as required by user's defined
C                       diffusion model) meteorological data (MODEL)
C                    3) write results to output met file
C                       according to requirements levied by
C                       user's defined diffusion model (MPOUT).
C
C     Calling arguments:
C             ISTAT    Out   Integer   Status of processing
C
C     Called by:  Main pgm: AERMET
C
C     Calls to:   LATLON, FETCH, CHROND, HTKEY, MODEL, MPOUT, ERRHDL
C
C     Initial Release:  December 1992
C
C
C     Revision history:
C        08/27/2012 (EPA)
C          - Account for ONSITE mixing heights (OSMIX = .T.) when
C            checking for blank UALOC or invalid UALAT/UALON.
C        02/04/2013 (EPA)
C          - Assign time-zone adjustment (ZONE) based on upper air 
C            time-zone (UALST) for cases where both upper-air data 
C            and ONSITE mixing heights are available.
C
C----------------------------------------------------------------------
      IMPLICIT NONE

C---- Variable declarations
      INTEGER :: NHDR,ISTAT,FSTAT,NDUM, I, J, K
      INTEGER :: iAsosMess

C---- Character strings for surface file header record, including the 
C     standard information in HDR_String and string for the CCVR_Sub and/or 
C     TEMP_Sub options.  Information on the THRESH_1MIN option, ADJ_U* 
C     and/or BULKRN options, and possible use of MMIF-generated inputs
C     will be inserted below
      CHARACTER :: HDR_String*98, CC_TT_Subs*17
      
      REAL :: RAD2DEG

C      NUMBER   Julian day being processed
C      NHDR     Number of headers processed
C      NDUM     Number of hours needed to convert solar time
C               to GMT.  NDUM = INT( GMT TIME - SOLAR TIME),
C               in hours.  Note in USA, local standard time is
C               fairly close to solar time (position of sun in
C               sky).
C      MPCRDY   Chronological day in the merged data file
C      ISTAT    Status of processing
C               0   condition OK
C               1   errors have occurred
C               2   reached eof of merge data in expected position
C                   (i.e., not prematurely)
C               3   end of user-specified processing window
C      FSTAT    Processing status after FETCHing a day of met. data

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'
      
      INTEGER   JMPDATE, JMPDAT8

C---- Data Initialization

      PATH = 'METPREP   '
      LOC  = ' MPMET'
      CC_TT_Subs = ''

      RAD2DEG =  57.29578
      FSTAT = 0
      NHDR = 0
      K_CALM = 0                                       ! dtb #131 02122
      iAsosMess = 0
      L_1minAsos_pre2000 = .FALSE.

C---- Read the headers (if any) within the merged input
C     meteorology.  We do not need to store these since
C     this was done during the setup processing.

      REWIND DEV40

  10  READ(DEV40,33,END=40) BUF03
  33  FORMAT(A3)

      IF( BUF03.EQ.'***' ) GO TO 20
      IF( BUF03(1:1) .EQ. '*' )THEN
         NHDR = NHDR + 1
         GO TO 10
      ENDIF

C---- Positioned to read data in merged-data file

  20  CONTINUE

C---- Assign latitude/longitude variables for later use in 
C     determining sunrise, sunset, and angle of sun to horizon.
C     MPLAT and MPLON are the latitude and longitude character
C     strings specified by the user on the LOCATION keyword in 
C     Stage 1 for the primary surface station, ONSITE pathway
C     if available, otherwise the SURFACE pathway. 

C --- Reintroduce MPLOC for cases with only onsite data
      IF( OSMIX .AND. .NOT.UADATA )THEN
         IF( LEN_TRIM(MPLOC) .EQ. 0 )THEN
C----       Write warning message that time zone for OSMIX only
C           could be wrong and METPREP LOCATION should be used
            MESS  =  BLNK80
            ECODE = 'W03'
            WRITE( MESS,100 ) MPLST
 100        FORMAT(' OSMIX only and no METPREP LOCATION keyword;',
     &             ' time zone adjustment (',I3,') may be erroneous.')
            CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         END IF
      END IF

      IF( LEN_TRIM(MPLOC) .GT. 0 )THEN
C----    MPLOC available from MP LOCATION keyword
         CONTINUE
      ELSEIF( LEN_TRIM(OSLOC) .GT. 0 )THEN
C----    Assign ONSITE lat/lon for sunrise in CBL height calc
         MPLAT = OSLAT
         MPLON = OSLON
      ELSEIF( LEN_TRIM(SFLOC) .GT. 0 )THEN
C----    Assign SURFACE lat/lon for sunrise in CBL height calc
         MPLAT = SFLAT
         MPLON = SFLON
      ELSE
C----    Fatal error condition occurs,
C        Issue messages and set ISTAT = 1 to abort processing.
         MESS  =  BLNK80
         ECODE = 'E89'
         ISTAT = 1
         WRITE( MESS,900 ) OSLOC, SFLOC, MPLOC
 900     FORMAT(' ERROR assigning STAGE3 LAT/LON!!! OSLON: ',A8,
     &          '  SFLON: ',A8,'  MPLON: ',A8)
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
C----    Also issue error code E00 for "programming error" 
C        and print error message to default output device
         MESS  =  BLNK80
         ECODE = 'E00'
         WRITE( MESS,909 ) 
 909     FORMAT(' ERROR assigning STAGE3 LAT/LON!!! ',
     &          ' Possible programming flaw in sub.MPMET!')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         print *, 'ERROR ASSIGNING STAGE3 LAT/LON!!!'
         print *, 'Possible programming flaw in sub.MPMET!'
         RETURN
      ENDIF
      
C --- CALL LATLON, first for LAT and then for LON
      CALL LATLON( 1,1,MPLAT,ST3LAT,ISTAT )
      IF( ISTAT.EQ.1 )THEN
         MESS =  BLNK80
         ECODE = 'E75'
         WRITE( MESS,1000 ) MPLAT
 1000    FORMAT(' ERROR converting latitude: ',A8)
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF

      CALL LATLON( 1,2,MPLON,ST3LON,ISTAT )
      IF( ISTAT.EQ.1 )THEN
         MESS =  BLNK80
         ECODE = 'E75'
         WRITE( MESS,2000 ) MPLON
 2000    FORMAT(' ERROR converting longitude: ',A8)
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF

C---- Check for upper air station information, including 
C     station location (UALOC) and time-zone adjustment (ZONE),
C     unless ONSITE mixing heights were provided
      IF( LEN_TRIM(UALOC) .EQ. 0 .AND. .NOT.OSMIX )THEN
C----    Upper air station location is blank and no OSMIX; 
C        could mean that no upper air data were included 
C        in the merged data. Since no OSMIX available,
C        issue an error message and return to MAIN program.
         MESS =  BLNK80
         ECODE = 'E78'
         WRITE( MESS,2010 ) 
 2010    FORMAT(' Upper air station location is blank and no OSMIX' )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF

C---- Check that the conversion from GMT to LST is within
C     reason (i.e. the correct magnitude)

C---- Assign time-zone adjustment (ZONE) based on UPPERAIR LOCATION keyword,
C     but check for presence of ONSITE mixing heights first.
      IF( .NOT. OSMIX )THEN
C----    No ONSITE mixing heights; assign ZONE based on UPPERAIR location
         ZONE = UALST
      ELSEIF( LEN_TRIM(UALOC) .GT. 0 )THEN
C----    ONSITE mixing heights and UPPERAIR data both available; 
C        assign ZONE based on UPPERAIR location
         ZONE = UALST
      ELSE
C----    ONSITE mixing heights available, but no UPPERAIR data; 
C        assign ZONE based on ONSITE 
         ZONE = MPLST
      ENDIF

      IF( ST3LON.GT.0 )THEN
         NDUM = INT( (ST3LON+7.5)/15.0 )
      ELSE
         NDUM = INT( (ST3LON-7.5)/15.0 )
      ENDIF

      IF( ABS( ZONE-NDUM ).GT.2 )THEN
         MESS =  BLNK80
         ECODE = 'W70'
         WRITE( MESS,3000 ) ZONE
 3000    FORMAT(' GMT-to-LST conversion looks suspect: ',I3)
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
      ENDIF

C---- Create character string for CCVR_Sub and/or TEMP_Sub options
      IF( CCVR_Sub .AND. .NOT.CCVR_NoPers )THEN
         IF( TEMP_Sub .AND. .NOT.TEMP_NoPers )THEN
            CC_TT_Subs = 'CCVR_Sub TEMP_Sub'
         ELSEIF( TEMP_Sub .AND. TEMP_NoPers )THEN
            CC_TT_Subs = 'CCVR_Sub TTNoPers'
         ELSE
            CC_TT_Subs = 'CCVR_Sub         '
         ENDIF
      ELSEIF( CCVR_Sub .AND. CCVR_NoPers )THEN
         IF( TEMP_Sub .AND. .NOT.TEMP_NoPers )THEN
            CC_TT_Subs = 'CCNoPers TEMP_Sub'
         ELSEIF( TEMP_Sub .AND. TEMP_NoPers )THEN
            CC_TT_Subs = 'CCNoPers TTNoPers'
         ELSE 
            CC_TT_Subs = 'CCNoPers         '
         ENDIF
      ELSEIF( TEMP_Sub )THEN
         IF( .NOT. TEMP_NoPers )THEN
            CC_TT_Subs = 'TEMP_Sub         '
         ELSE
            CC_TT_Subs = 'TTNoPers         '
         ENDIF
      ENDIF

C---- Write header record to SFC file, starting with basic HDR_String;
C     add WS threshold for 1-min ASOS data, and other options if used.
C     First build string for standard information
      WRITE (HDR_String, 800) MPLAT, MPLON, UALOC, SFLOC, OSLOC, 
     &                        VERSNO
  800 FORMAT ( 2(2X,A8), 8X, '  UA_ID: ',A8, '  SF_ID: ',A8,
     &                       '  OS_ID: ',A8, T85, 'VERSION:',A6 )

      IF( L_1MINASOS_THRESH )THEN
C ---    1-min ASOS WS threshold was used in AERMINUTE
         IF( ADJ_USTAR )THEN
C ---       ADJ_USTAR BETA option was used
            IF( BULKRN )THEN
C ---          BULKRN option for SBL calcs was used
               IF( .NOT. MMIF_Data )THEN
C ---             BULKRN option for SBL calcs was NOT based on MMIF data
                  WRITE (DEV80, 801) HDR_String, THRESH1SPD, CC_TT_Subs
  801             FORMAT (A98,'   THRESH_1MIN = ',F5.2,' m/s;',
     &                                      '  ADJ_U*  BULKRN',2X,A17 )
               ELSE
C ---             BULKRN option for SBL calcs WAS based on MMIF data
                  WRITE (DEV80,9801) HDR_String, THRESH1SPD, CC_TT_Subs,
     &                                                       MMIF_Vers
 9801             FORMAT (A98,'   THRESH_1MIN = ',F5.2,' m/s;',
     &                           '  ADJ_U*  BULKRN/MMIF',2X,A17,2X,
     &                                                    'MMIF ',A40 )
               ENDIF
            ELSE
C ---          BULKRN option for SBL calcs was NOT used
               WRITE (DEV80, 802) HDR_String, THRESH1SPD, CC_TT_Subs
  802          FORMAT (A98,'   THRESH_1MIN = ',F5.2,' m/s;',
     &                                      '  ADJ_U*',2X,A17 )
            ENDIF
         ELSE
C ---       ADJ_USTAR BETA option was NOT used
            IF( BULKRN )THEN
C ---          BULKRN option for SBL calcs was used
               IF( .NOT. MMIF_Data )THEN
C ---             BULKRN option for SBL calcs was NOT based on MMIF data
                  WRITE (DEV80, 803) HDR_String, THRESH1SPD, CC_TT_Subs
  803             FORMAT (A98,'   THRESH_1MIN = ',F5.2,' m/s;',
     &                                         '  BULKRN',2X,A17 )
               ELSE
C ---             BULKRN option for SBL calcs WAS based on MMIF data
                  WRITE (DEV80,9803) HDR_String, THRESH1SPD, CC_TT_Subs,
     &                                                       MMIF_Vers
 9803             FORMAT (A98,'   THRESH_1MIN = ',F5.2,' m/s;',
     &                                '  BULKRN/MMIF',2X,A17,2X,
     &                                                    'MMIF ',A40 )
               ENDIF
            ELSE
C ---          BULKRN option for SBL calcs was NOT used
               WRITE (DEV80, 804) HDR_String, THRESH1SPD, CC_TT_Subs
  804          FORMAT (A98,'   THRESH_1MIN = ',F5.2,' m/s;',2X,A17 )
            ENDIF
         ENDIF
      ELSE
C ---    1-min ASOS WS threshold was NOT used in AERMINUTE
         IF( ADJ_USTAR )THEN
C ---       ADJ_USTAR BETA option was used
            IF( BULKRN )THEN
C ---          BULKRN option for SBL calcs was used
               IF( .NOT. MMIF_Data )THEN
C ---             BULKRN option for SBL calcs was NOT based on MMIF data
                  WRITE (DEV80, 805) HDR_String, CC_TT_Subs
  805             FORMAT (A98,'  ADJ_U*  BULKRN',2X,A17 )
               ELSE
C ---             BULKRN option for SBL calcs WAS based on MMIF data
                  WRITE (DEV80,9805) HDR_String, CC_TT_Subs, MMIF_Vers
 9805             FORMAT (A98,'  ADJ_U*  BULKRN/MMIF',2X,A17,2X,
     &                                                   'MMIF ',A40 )
               ENDIF
            ELSE
C ---          BULKRN option for SBL calcs was NOT used
               WRITE (DEV80, 806) HDR_String, CC_TT_Subs
  806          FORMAT (A98,'  ADJ_U*',2X,A17 )
            ENDIF
         ELSE
C ---       ADJ_USTAR BETA option was NOT used
            IF( BULKRN )THEN
C ---          BULKRN option for SBL calcs was used
               IF( .NOT. MMIF_Data )THEN
C ---             BULKRN option for SBL calcs was NOT based on MMIF data
                  WRITE (DEV80, 807) HDR_String, CC_TT_Subs
  807             FORMAT (A98,'  BULKRN',2X,A17 )
               ELSE
C ---             BULKRN option for SBL calcs WAS based on MMIF data
                  WRITE (DEV80,9807) HDR_String, CC_TT_Subs, MMIF_Vers
 9807             FORMAT (A98,'  BULKRN/MMIF',2X,A17,2X,'MMIF ',A40 )
               ENDIF
            ELSE
C ---          BULKRN option for SBL calcs was NOT used
               WRITE (DEV80, 808) HDR_String, CC_TT_Subs
  808          FORMAT (A98,'  ',2X,A17 )
            ENDIF
         ENDIF
      ENDIF

C---- Convert the character-based latitude and longitude for the upper
C     air station to a real value, unless ONSITE mixing heights are
C     available.
      IF( .NOT. OSMIX )THEN 
         CALL LATLON( 1,1,UALAT,UAST3LAT,ISTAT )
         IF( ISTAT.EQ.1 )THEN
            MESS =  BLNK80
            ECODE = 'E75'
            WRITE( MESS,1010 ) UALAT
 1010       FORMAT(' ERROR converting UA latitude for sunrise: ',A8)
            CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
            RETURN
         ENDIF
      ELSE
C----    ONSITE mixing heights provided, assign OSLAT to UAST3LAT
         UAST3LAT = ST3LAT
      ENDIF

      IF( .NOT. OSMIX )THEN
         CALL LATLON( 1,2,UALON,UAST3LON,ISTAT )
         IF( ISTAT.EQ.1 )THEN
            MESS =  BLNK80
            ECODE = 'E75'
            WRITE( MESS,2020 ) UALON
 2020       FORMAT(' ERROR converting UA longitude for sunrise: ',A8)
            CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
            RETURN
         ENDIF
      ELSE
C----    ONSITE mixing heights provided, assign OSLAT to UAST3LAT
         UAST3LON = ST3LON
      ENDIF

C---- Set the upper air sounding selection window
C       If keyword UAWINDOW is present, window is defined by the user
C       If keyword UAWINDOW is NOT present, set the window
C         based on the default values for the type of sounding to
C         search for:  00Z/12Z (default) or relative to sunrise
C         (if METHOD UASELECT is present)

      IF( STATUS(6,25) .EQ. 2 )THEN
C----    UAWINDOWBEGIN and UAWINDOWEND defined by the UAWINDOW keyword 
C        on METPREP path - nothing to do
         
      ELSEIF( SUNRISE4UA )THEN
C        Keyword specified to use sunrise as basis for sounding search,
C        but no UAWINDOW keyword specified; assign default sounding window
         UAWINDOWBEGIN = UASRWINDOWBEGIN
         UAWINDOWEND   = UASRWINDOWEND
         
      ELSE
C        Using 00Z/12Z sounding as the basis for the search,
C        but no UAWINDOW keyword; assign default sounding window
         UAWINDOWBEGIN = UA0012WINDOWBEGIN
         UAWINDOWEND   = UA0012WINDOWEND
      ENDIF

C---- Extract anemometer height from ASOS data base, if available,
C     for comparison with user-specified height.
C     First extract surface station WBAN number for SFLOC, and
C     assign blanks for call IDs
      READ(SFLOC,'(I8)') iSfcObsWBAN
      cSfcObsCALL4 = '    '
      cSfcObsCALL3 = '   '

C---- Initialize ASOS anemometer height to 0
      ASOS_ANEM_HGT = 0.0

C---- Find ASOS commission date
      CALL FNDCOMDT(PATH)

C---- If anemometer height found (i.e., > 0), and NWS substitution is on, 
C     we'll compare anemometer height to user-specified value, and 
C     issue warning message if they differ by more than 5 percent  
      IF( ASOS_ANEM_HGT .GT. 0.0 .AND. SUBSTNWS )THEN
C        Anemometer height found, initialize flag for issuing message
C        if a discrepancy is found with user-specified value
         iAsosMess = 0
      ENDIF 

C---- Initialize the OSSOBS array of on-site scalar variables to missing

      DO I = 1, 24
         DO J = 1, 29
            OSSOBS(I,J) = OSQA(J,2)
         ENDDO

         DO J = 30, 34
            OSSOBS(I,J) = OSQA(J,2)
         ENDDO

         DO J = 35, 51
            OSSOBS(I,J) = REAL( SFQA(J,2) )
         ENDDO

C---- Initialize the OSVOBS array of on-site vector (multi-level) variables to missing

         DO J = 1, OSNL
            DO K = 1, 15
               OSVOBS(I,J,K) = OSQA(K+14,2)
            ENDDO
         ENDDO
      ENDDO

C---- Fetch one day of meteorology from merge data file;   ---- CALL FETCH
C     returning to statement 30 to fetch next day of data
   30 CONTINUE
   
      CALL FETCH( FSTAT )

      IF( FSTAT.EQ.1 )THEN
C------- An error was encountered fetching the next day's met data
         ISTAT = 1
         RETURN

      ELSEIF( FSTAT.EQ.3 )THEN
C------- An end of file was encountered fetching the next day's met data
         ISTAT = 2
         RETURN
      ENDIF


C---- Compute the chronological day since January 1, 1900  ---- CALL CHROND
      CALL CHROND( PATH,MPYR,MPJDY,MPCRDY )
      IF( MPCRDY.LT.0 )THEN
         ISTAT = 1
         RETURN
      ENDIF

C---- Compute date variable as YYYYMMDD, for comparison to ASOS comm date
      JMPDATE = MPYR*10000 + MPCMO*100 + MPCDY
      
C --- Calculate 8-digit year from 6-digit JMPDATE,
C     for comparison to ASOS commission date.
      IF( JMPDATE .GE. 500000 )THEN
         JMPDAT8 = 19000000 + JMPDATE
      ELSE
         JMPDAT8 = 20000000 + JMPDATE
      ENDIF

C---- Check dates and see if this day is within processing 'window'.
C     'Window' is defined only if an XDATES statement is present
C      (status(6,9)
      IF( STATUS(6,9).EQ.2 )THEN
         IF( MPCRDY .LT. JBCDY1 )THEN
            GO TO 30
         ELSEIF( MPCRDY .GT. JBCDY2 )THEN
            MESS =  BLNK80
            ECODE = 'I79'
            WRITE( MESS,5000 )
5000        FORMAT(' End of user-specified processing window ')
            CALL ERRHDL( MPJDY,PATH,ECODE,LOC,MESS )
            ISTAT = 3
            RETURN
         ENDIF

      ELSE
C------- The XDATES keyword was not present, process all data; 
C        was there an end of file?  (FSTAT =3)
         IF( FSTAT .EQ. 3 )THEN
            ISTAT = 2
            RETURN
         ENDIF

      ENDIF

C---- Data will be processed for this day, compare this date to 
C     ASOS commission date if NWS substitution is turned on.
C     If surface data are ASOS, issue message once regarding
C     anemometer height from ASOS list, and compare that value
C     to user-specified anemometer height.
      IF( ASOS_ANEM_HGT .NE. 0.0 .AND. SUBSTNWS .AND.
     &    JMPDAT8 .GE. iCommDate .AND. iAsosMess .EQ. 0 )THEN
C        Set iAsosMess flag to 1 to turn off additional messages
         iAsosMess = 1
         MESS =  BLNK80
         ECODE = 'I83'
         WRITE(MESS, 4001) SFLOC, ASOS_ANEM_HGT
4001     FORMAT(' Anem height found in ASOS list for station ',
     &          A8,': Height from ASOS list (m) is: ',F7.2)
         CALL ERRHDL ( 0,PATH,ECODE,LOC,MESS )
         
C----    Compare anemometer height from ASOS list to user-specified value
         IF( ABS(ASOS_ANEM_HGT - INSTHT(1))/INSTHT(1) .GT. 0.05 )THEN
C           Anemometer heights differ by more than 5 percent, issue warning
            MESS =  BLNK80
            ECODE = 'W83'
            WRITE(MESS, 4002) ASOS_ANEM_HGT, INSTHT(1)
4002        FORMAT(' Anem height from ASOS list (',F7.2,
     &            'm) differs from user-specified height of ',F7.2,'m!')
            CALL ERRHDL ( 0,PATH,ECODE,LOC,MESS )
         ENDIF
      ENDIF

C>>>> Display date being processed
      WRITE(*,6000) MPCMO,MPCDY,MPYR
 6000 FORMAT('+  Stage 3: Now processing month/day/year ',
     &           i2.2,'/',i2.2,'/',i2.2)


C---- Fill in any missing on-site profile heights        ---- CALL HTKEY
      CALL HTKEY( ISTAT )
      IF( ISTAT.EQ.1 )THEN
         RETURN
      ENDIF

C---- Initialize the counters for calms/variable and CCVR/TEMP subs
C     before processing data for the current day
      N_CALM  = 0                                        ! dtb #131 02122
      N_VARWD = 0
      N_CCVR_Sub = 0
      N_TEMP_Sub = 0

C---- Process day's meteorology                          ---- CALL MODEL
      CALL MODEL( ISTAT )
      IF( ISTAT.NE.2 )THEN
         RETURN
      ENDIF

C --- Add numbers of calms, varwds and CCVR_Subs for this day to 
C     overall totals
      K_CALM  = K_CALM + N_CALM                          ! dtb #131 02122
      K_VARWD = K_VARWD + N_VARWD
      K_CCVR_Sub = K_CCVR_Sub + N_CCVR_Sub
      K_TEMP_Sub = K_TEMP_Sub + N_TEMP_Sub


C---- Write to met output file as needed                 ---- CALL MPOUT
      CALL MPOUT( ISTAT )
      IF( ISTAT.NE.2 )RETURN


C---- Go back and fetch next day's meteorology

      GO TO 30

C---- This point is reached if an end of file is encountered
C     while reading headers of merge file.

  40  CONTINUE
      MESS =  BLNK80
      ECODE = 'E22'
      WRITE( MESS,5500 )
5500  FORMAT(' NO DATA IN INPUT FILE: HEADERS FOLLOWED BY EOF')
      CALL ERRHDL( NHDR,PATH,ECODE,LOC,MESS )
      ISTAT =  1

      RETURN
      END

