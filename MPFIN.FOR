      SUBROUTINE MPFIN( ISTAGE )
C=====================================================================**
C          MPFIN Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Print the general report for Stage 3
C
C     Initial Release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        00/00/00
C          - added additional run information to the report file
C
C-----------------------------------------------------------------------

C---- Data declaration

      IMPLICIT NONE
      
      INTEGER       I, ISECT, IMNTH, ISSP, NNN
      INTEGER       NERRS, NERRS2   ! number of missing values in OSSFC/OSSFC2 arrays of SurfChars

      INTEGER       ISTAGE, DEVICE
      CHARACTER*60  PROCESS
      CHARACTER*15  BLNK15
      CHARACTER (LEN=11), SAVE ::  DMY
      CHARACTER (LEN=8),  SAVE ::  HMS
      CHARACTER*9   ATEMP
      CHARACTER*8   DOTHIS
      CHARACTER*6   FREQ_CHR, FREQ_CHR2
      CHARACTER*11  DMY1,DMY2
      CHARACTER*3   MONTHS(12)

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

C---- Data initialization
      DATA PATH/'METPREP'/, LOC/' MPFIN'/, NERRS/0/, NERRS2/0/,
     &     BLNK15/'               '/
      DATA MONTHS/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1            'SEP','OCT','NOV','DEC'/


C---- Get the date from the system
      CALL DATER( DMY,HMS )


C---- Check to make sure that the report file is available.
C        If not, use DEVIO.

      IF( STATUS(1,1).EQ.2 )THEN
         DEVICE = DEV50
      ELSE
         DEVICE = DEVIO
      ENDIF

      PGNUM = PGNUM + 1
      CALL BANNER( PGNUM,ISTAGE,VERSNO,DEVICE )

C---> 0.  Check JBSTAT and syntax-card status

      IF( STATUS(1,3) .EQ. 2 )THEN
C------- User ran a checkout run on the setup data (no data processing)
         WRITE( DEVICE,1000 )
         IF( DEVICE .NE. DEVIO ) WRITE(DEVIO,1000)
1000     FORMAT(
     & /,14X, 56('*'),
     & /,14X,'***     THIS RUN ONLY CHECKS THE RUNSTREAM SETUP     ***',
     & /,14x,'***              NO DATA WERE PROCESSED              ***',
     & /,14X, 56('*'))


 5020 FORMAT( /14X,56('*'),/14X,'***   ',A44,'   ***',/14X,56('*')/)


      ENDIF


C---- Check SETERR
      IF( .NOT. SETERR )THEN
         WRITE( DEVICE,1030 )
      ELSE
         WRITE( DEVICE,1020 )
      ENDIF

 1020 FORMAT( /14X,56('*'),
     & /14X,'***       AERMET Setup Finished UN-successfully      ***',
     & /14X,'********************************************************')

 1030 FORMAT( /14X,56('*'),
     & /14X,'***        AERMET Setup Finished Successfully        ***',
     & /14X,'********************************************************')

C---- List the meaningful setup data that defines this run.

      BUF80(1)(1:20) =   '  ERROR OPENING FILE'
      BUF80(2)(1:20) =   ' OPENED SUCCESSFULLY'
      BUF80(3)(1:22) = '> STANDARD OUTPUT UNIT'

      IWORK1(100) = STATUS(1,1) + STATUS(1,2) + STATUS(6,4) +
     &              STATUS(6,22)


C---> 1.   I/O files

      IF( IWORK1(100).EQ.0 )THEN
         WRITE( DEVICE,1040)
1040     FORMAT( /,4X,'1.  NO INPUT OR OUTPUT FILES WERE DEFINED',
     &                ' DURING SETUP PROCESSING.',
     &           /,4X,'    THIS HAS RESULTED IN AN ABNORMAL JOB END.'/)

      ELSE
         WRITE( DEVICE,1050 )
1050     FORMAT( /,4X,'1.  Input/Output Files',/)

         BUF96 = BLNK96
         BUF96(1:19) = 'GENERAL REPORT FILE'
         IF( STATUS(1,1).GE.2 )THEN
C            File was defined and opened
             WRITE( DEVICE,1060 ) DISK50(1:52),BUF80(2)(1:20)
         ELSEIF( STATUS(1,1) .EQ. 0) THEN
C            File was not defined and is redirected to standard output
             WRITE( DEVICE,1061 ) BUF96(1:50),BUF80(3)(1:22)
         ELSE
C            File not opened due to an error
             WRITE( DEVICE,1060 ) BUF96(1:52),BUF80(1)(1:20)
1060         FORMAT( 8X,A52,A20 )
1061         FORMAT( 8X,A50,A22 )
         ENDIF

         BUF96 = BLNK96
         BUF96(1:13) = 'MESSAGE FILE '
         IF( STATUS(1,2).GE.2 )THEN
            WRITE( DEVICE,1060 ) DISK60(1:52),BUF80(2)(1:20)
         ELSE
            WRITE( DEVICE,1060 ) BUF96(1:52),BUF80(1)(1:20)
         ENDIF

         BUF96 = BLNK96
         BUF96(1:40) = 'INPUT FILE OF MERGED METEOROLOGICAL DATA'
         IF( STATUS(6,4).GE.2 )THEN
            WRITE( DEVICE,1060 ) DISK40(1:52),BUF80(2)(1:20)
         ELSE
            WRITE( DEVICE,1060 ) BUF96(1:52),BUF80(1)(1:20)
         ENDIF

         BUF96 = BLNK96
         BUF96(1:43) = 'OUTPUT FILE OF DISPERSION MODEL METEOROLOGY'
         IF( STATUS(6,22).GE.2 )THEN
            WRITE( DEVICE,1060 ) DISK80(1:52),BUF80(2)(1:20)
         ELSE
            WRITE( DEVICE,1060 ) BUF96(1:52),BUF80(1)(1:20)
         ENDIF

         BUF96 = BLNK96
         BUF96(1:43) = 'PROFILE OF METEOROLOGICAL DATA'
         IF( STATUS(6,27).GE.2 )THEN
            WRITE( DEVICE,1060 ) DISK85(1:52),BUF80(2)(1:20)
         ELSE
            WRITE( DEVICE,1060 ) BUF96(1:52),BUF80(1)(1:20)
         ENDIF
      ENDIF


C---> 2.  Dispersion model selected by the user.

      IF( MDSTAT .EQ. 0 )THEN
         WRITE( DEVICE,1070 )
1070     FORMAT( /,4X,'2.  THE DISPERSION MODEL WAS NOT SUCCESSFULLY',
     &                ' DETERMINED DURING SETUP.',
     &           /,4X,'    THIS HAS RESULTED IN AN ABNORMAL JOB END.'/)


      ELSEIF(MDSTAT .GT. 5) THEN                                        ! dtb #100 01249

         WRITE( DEVICE,1085 ) VERSNO
1085     FORMAT(/,5X,' THIS MODEL IS NOT SUPPORTED IN AERMET, ',
     &               'VERSION ',A6)

      ELSE

         WRITE( DEVICE,1080 ) DISPMD( MDSTAT )
1080     FORMAT(/,4X,'2.  Dispersion Model for which Data ',
     &                    'Are Processed',//, 8X, A8)

         IF( STATUS(6,9) .EQ. 2 )THEN
            IF( JBGMO1.GT.0 .AND. JBGMO2.GT.0 )THEN
               WRITE(DMY1,5080) JBGDY1,MONTHS(JBGMO1),JBYR1
               WRITE(DMY2,5080) JBGDY2,MONTHS(JBGMO2),JBYR2
            ELSE
               WRITE(DMY1,5080) JBGDY1,'ERR',JBYR1
               WRITE(DMY2,5080) JBGDY2,'ERR',JBYR2
            ENDIF
            WRITE(DEVICE,5060) DMY1,DMY2
         ELSEIF( STATUS(6,9) .EQ. 1 )THEN
            WRITE(DEVICE,5061)
         ELSE
            IF( MRGMO1.GT.0 .AND. MRGMO2.GT.0 )THEN
               WRITE(DMY1,5080) MRGDY1,MONTHS(MRGMO1),MRYR1
               WRITE(DMY2,5080) MRGDY2,MONTHS(MRGMO2),MRYR2
            ELSE
               WRITE(DMY1,5080) MRGDY1,'ERR',MRYR1
               WRITE(DMY2,5080) MRGDY2,'ERR',MRYR2
            ENDIF
            WRITE(DEVICE,5060) DMY1,DMY2
            WRITE(DEVICE,5062)
         ENDIF
      ENDIF


C---> 3.  Processing options

      WRITE( DEVICE,1090 )
1090  FORMAT( /,4X,'3.  Processing Options',//,
     &             5X,'      Process       Scheme         Description',/
     &             5X,'      -------       ------         -----------' )

C---- Initialize BUF80 array to blanks (note BUF80 is now 132 characters)
      DO I = 1,10
         BUF80(I) = BLN132
      ENDDO
      
C---- Assign Items/process to first 15 characters of BUF80 array
      BUF80(1)(1:15)   = 'WIND DIRECTION '
      BUF80(2)(1:15)   = 'CLOUD COVER    '
      BUF80(3)(1:15)   = 'TEMPERATURE    '
C     BUF80(3)(1:15)   = 'MIXING HEIGHTS '
C     BUF80(4)(1:15)   = 'CBL PROCESSING '
      BUF80(5)(1:15)   = 'SBL PROCESSING '
C     BUF80(6)(1:15)   = 'TURBULENCE     '
C     BUF80(7)(1:15)   = 'HEAT FLUX      '
      BUF80(8)(1:15)   = 'ASOS ADJUSTMENT'
      BUF80(9)(1:15)   = 'SDG SELECTION  '
      BUF80(10)(1:15)  = 'REFERENCE LEVEL'

C---- Print the process and associated action
      DO I = 1,10

         PROCESS = ' '

         IF( I .EQ. 1 )THEN
C---------- Action: wind direction
C           Items: (1) RANDOM  (2) NORAND
            IF( CONTRL(I) .EQ. 1 )THEN
C_ORIG               DOTHIS = ACTION(CONTRL(I))
C_ORIG               PROCESS(1:38)  = 'NWS wind directions are NOT RANDOMIZED'
C_ORIG               PROCESS(39:45) = '       '
C_ORIG            ELSE
C_ORIG               DOTHIS = ACTION(CONTRL(I))
C_ORIG               PROCESS(1:38)  = 'NWS wind directions are RANDOMIZED    '
C_ORIG               PROCESS(39:45) = '       '
C_ORIG            ENDIF
               DOTHIS = ACTION(CONTRL(I))
               PROCESS(1:38)  = 'NWS wind directions are RANDOMIZED'
               PROCESS(39:45) = '       '
            ELSEIF( CONTRL(I) .EQ. 2 )THEN
               DOTHIS = ACTION(CONTRL(I))
               PROCESS(1:38)  = 'NWS wind directions are NOT RANDOMIZED'
               PROCESS(39:45) = '       '
            ELSE
               CONTRL(I) = 1
               DOTHIS = ACTION(CONTRL(2))
               PROCESS(1:38)  = 'NWS wind directions are RANDOMIZED    '
               PROCESS(39:45) = '       '
            ENDIF
            WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS

         ELSEIF( I .EQ. 2 )THEN
C---------- Action: Missing CCVR Substitution
            IF( CCVR_Sub )THEN
               DOTHIS = 'CCVR_Sub'
               PROCESS(1:38)  = 'Missing CCVR substitutions ARE include'
               PROCESS(39:45) = 'd      '
               WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
               IF( CCVR_NoPers )THEN
                  DOTHIS = 'NoPers  '
                  PROCESS(1:35)  = ' No CCVR substitutions based on per'
                  PROCESS(36:45) = 'sistence  '
                  WRITE( DEVICE,2000 ) BLNK15, DOTHIS, PROCESS
               ENDIF
            ELSE
               DOTHIS = 'NO_SUB'
               PROCESS(1:38)  = 'Missing CCVR substitutions NOT include'
               PROCESS(39:45) = 'd      '
               WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
            ENDIF

         ELSEIF( I .EQ. 3 )THEN
C---------- Action: Missing TEMP Substitution
            IF( TEMP_Sub )THEN
               DOTHIS = 'TEMP_Sub'
               PROCESS(1:38)  = 'Missing TEMP substitutions ARE include'
               PROCESS(39:45) = 'd      '
               WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
               IF( TEMP_NoPers )THEN
                  DOTHIS = 'NoPers  '
                  PROCESS(1:35)  = ' No TEMP substitutions based on per'
                  PROCESS(36:45) = 'sistence  '
                  WRITE( DEVICE,2000 ) BLNK15, DOTHIS, PROCESS
               ENDIF
            ELSE
               DOTHIS = 'NOTSUB'
               PROCESS(1:38)  = 'Missing TEMP substitutions NOT include'
               PROCESS(39:45) = 'd      '
               WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
            ENDIF

         ELSEIF( I .EQ. 5 )THEN
C---------- Action: SBL Processing
            IF( BULKRN )THEN
               DOTHIS = 'BULKRN'
c                                ....+....1....+....2....+....3....+....
               PROCESS(1:38)  = 'The Bulk Richardson method is used    '
               PROCESS(39:45) = '       '
               WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
               WRITE( DEVICE,2004 ) OSLL(1), OSUL(1)
C ---          Check for range of Delta-T heights and lower height
C              
               IF( ABS( OSUL(1)-OSLL(1) ) .GT. 15.0 .OR.
     &                  OSLL(1) .GT. 5.0 .OR. OSUL(1) .GT. 20.0 )THEN 
                  MESS =  BLNK80
                  ECODE = 'W86'
                  WRITE( MESS,2012 ) OSLL(1), OSUL(1)
2012              FORMAT(' Delta-T height range may not be ',
     &                   'appropriate for BULKRN method. ',
     &                   'DT01 heights (m):',2F8.2 )
                  CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
               ENDIF

               IF( MMIF_Data )THEN
C ---             Indicate that BULKRN option is based on MMIF-generated data
                  WRITE( DEVICE,2005 ) MMIF_Vers
               ENDIF
            ELSE
               DOTHIS = 'UCALST'
               PROCESS(1:38)  = 'The default (Holtslag) method is used '
               PROCESS(39:60) = '       '
               WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
            ENDIF

            IF( ADJ_USTAR )THEN
               DOTHIS = 'ADJ_U*'
c                                ....+....1....+....2....+....3....+....
               PROCESS(1:38)  = 'Adjustments to u* for low WS are used '
               PROCESS(39:45) = '       '
               WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
            ENDIF

         ELSEIF( I .EQ. 8 )THEN
C---------- Action: ASOS truncation adjustment - only print this if ASOS
C                   data are actually used; old SCRAM format cannot
C                   be ASOS data (unless it is reformatted) and we do
C                   not want to confuse by indicating a adjustment was 
C                   made or not when the it does not apply to the format

            PROCESS = ' '
            IF( ASOS_ADJ )THEN
c                                ....+....1....+....2....+....3....+....
               DOTHIS = 'ASOS_ADJ'
               PROCESS(1:34)  = 'ASOS wind speeds, if present, ARE '
               PROCESS(35:60) = 'adjusted for truncation  '

            ELSEIF( .NOT. ASOS_ADJ )THEN
               DOTHIS = 'NO_ADJ  '
               PROCESS(1:34)  = 'ASOS wind speeds, if present, NOT '
               PROCESS(35:60) = 'adjusted for truncation  '

            ENDIF
            WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
             
         ELSEIF( I .EQ. 9 )THEN
C---------- Action: Upper air sounding selection
C           First check for whether UA data are available
            IF( LEN_TRIM(UALOC) .GT. 0 )THEN
C----------  UA data are available
             IF( .NOT. SUNRISE4UA )THEN
               PROCESS = ' '
               DOTHIS = '00Z/12Z'
               IF( MySounding .EQ. 0 )THEN
c                                   ....+....1....+....2....+....3....+....
                  PROCESS(1:35)  = 'Sounding selection based on 00Z sdg'
               ELSEIF( MySounding .EQ. 12 )THEN
c                                   ....+....1....+....2....+....3....+....
                  PROCESS(1:35)  = 'Sounding selection based on 12Z sdg'
               ELSEIF( MySounding .EQ. -12 )THEN
c                                   ....+....1....+....2....+....3....+....
                  PROCESS(1:35)  = 'Sounding selection based on 12Z sdg'
                  PROCESS(36:60) = ' of previous day'
               ELSE
c                                   ....+....1....+....2....+....3....+....
                  PROCESS(1:35)  = 'Sounding selection based on 00Z/12Z'
                  PROCESS(36:60) = ' sdgs'
               ENDIF
             ELSE
               DOTHIS = 'SUNRISE'
               PROCESS(1:35)  = 'Sounding selection based on sunrise'
               PROCESS(36:45) = ' '
             ENDIF
             WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
             ATEMP = ' '
             WRITE( ATEMP,2001 ) UAWINDOWBEGIN, UAWINDOWEND
             WRITE( DEVICE,2002 ) ATEMP            

            ELSEIF( OSMIX )THEN
C----------   No UA data available, but ONSITE mixing heights are 
C             available; 
              PROCESS = ' '
              DOTHIS = 'OSMIX  '
c                                  ....+....1....+....2....+....3....+....
                 PROCESS(1:35)  = 'No UPPERAIR data available - ONSITE'
                 PROCESS(36:60) = ' mixing heights used'
              WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
            ENDIF

         ELSEIF( I .EQ. 10 )THEN
C---------- Action: reference level
            IF( CONTRL(I) .EQ. 3 )THEN
               DOTHIS = ACTION(CONTRL(I))
c                                ....+....1....+....2....+....3....+....
               PROCESS(1:36)  = 'NWS WIND & TEMP data ARE SUBSTITUTED'
               PROCESS(37:60) = ' for missing ONSITE data'
               WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
               WRITE( DEVICE,2003 ) INSTHT(1)
            ELSE
               DOTHIS = '      '
c                                ....+....1....+....2....+....3....+....
               PROCESS(1:25)  = 'NWS WIND & TEMP data NOT '
               PROCESS(26:60) = 'SUBSTITUTED for missing ONSITE data'
               WRITE( DEVICE,2000 ) BUF80(I)(1:15), DOTHIS, PROCESS
            ENDIF

         ENDIF

2000     FORMAT( 8X,A15,2X,A8,4X,A60 )
2001     FORMAT( I3,'   ',I3)
2002     FORMAT(T38,' Selection window: ',A9)
2003     FORMAT(T38,' Anemometer height(m): ',F7.2)
2004     FORMAT(T38,' Delta-T heights (m): ',2F8.2)
2005     FORMAT(T38,' BULKRN inputs were generated by MMIF ',A40) 

      ENDDO


C.......................................................................
C---- NEW PAGE * NEED SPACE UNDER ITEM (3) ABOVE FOR
C              * LATER EXPANSION OF PROCESSING OPTIONS
C
C        CALL BANNER( DEVICE )
C        WRITE( DEVICE,5001)
C
C.......................................................................

C---> 4.  Site and data location

      WRITE( DEVICE,2030 )
2030  FORMAT( /,4X,'4.  Locations of Meteorological Data ',
     &     //,5X,'    Data           Site     Longitude   Latitude',
     &      /,5X,'   Pathway          ID      (degrees)   (degrees)',
     &      /,5x,'   -------         ----     ---------   ---------')

      IF( LEN_TRIM(UALOC) .GT. 0 )THEN
         WRITE( DEVICE,2040 ) PATHWD(2),UALOC,UALON,UALAT
2040     FORMAT(8X,A10,3X,A8,4X,A8,4X,A8)
      ENDIF

      IF( LEN_TRIM(SFLOC) .GT. 0 )THEN
        WRITE( DEVICE,2040 ) PATHWD(3),SFLOC,SFLON,SFLAT
      ENDIF

      IF( LEN_TRIM(OSLOC) .GT. 0 )THEN
        WRITE( DEVICE,2040 ) PATHWD(4),OSLOC,OSLON,OSLAT
      ENDIF


C---> 5a.  Site characteristics

C---  Assign label for primary surface characteristics frequency 
      IF( SFCFRQ .EQ. 2) THEN
         FREQ_CHR = 'Month '
      ELSEIF( SFCFRQ .EQ. 3) THEN
         FREQ_CHR = 'Season'
      ELSEIF( SFCFRQ .EQ. 4) THEN
         FREQ_CHR = 'Annual'
      ELSE
C---     There must have been an error processing the primary surface 
C        characteristics; assign 'ERROR!' to FREQ_CHR for output file
         FREQ_CHR = 'ERROR!'
      ENDIF
      IF( STATUS(6,34) .EQ. 2 )THEN
         WRITE(DEVICE,1201) DISK41, FREQ_CHR
      ELSE
         WRITE(DEVICE,1202) FREQ_CHR
      ENDIF
      
C---  Check for errors in the primary SITE_CHAR array based on OSSFC < 0.0
      IF( OSNWDS .GT. 0 )THEN
         DO ISECT = 1, OSNWDS
            IF( ISECT .GT. 1 ) WRITE( DEVICE,'(1X)' )
            DO IMNTH = 1, 12
C---           Check for any negative values, indicating input errors
               IF( OSSFC(IMNTH,ISECT,1) .LT. 0.0 .OR.
     &             OSSFC(IMNTH,ISECT,2) .LT. 0.0 .OR.
     &             OSSFC(IMNTH,ISECT,3) .LT. 0.0 )THEN
                  NERRS = NERRS + 1
                  MESS =  BLNK80
                  ECODE = 'E99'
C---              Issue error message identifying month and sector
                  WRITE( MESS,1300 ) IMNTH, ISECT
1300              FORMAT(' SITE_CHAR inputs missing for',
     &                   ' MONTH index ',I2,' and SECT index ',I2,'!')
                  CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
               END IF
               WRITE(DEVICE,1203) IMNTH,(OSWDS(ISECT,NNN),NNN=1,2),
     &                    (OSSFC(IMNTH,ISECT,ISSP),ISSP=1,OSMSITEC)
            ENDDO
         ENDDO
      ENDIF

C---  Check for errors in the primary SITE_CHAR array, NERRS > 0
      IF( NERRS .GT. 0 )THEN
         MESS =  BLNK80
         ECODE = 'E99'
         WRITE( MESS,1301 ) NERRS
1301     FORMAT(' Errors in SITE_CHAR Array; Number of missing',
     &          ' values (< 0.0) = ', I4 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
      END IF


C---> 5b.  Secondary site characteristics
C          OSNWDS2  = # wind directio sectors defined
C          OSMSITEC = # site characteristics
      IF( (STATUS(6,30) .EQ. 2  .and. STATUS(6,31) .EQ. 2 .and.
     &     STATUS(6,32) .EQ. 2) .OR.  STATUS(6,35) .GT. 0)THEN
C---     Assign label for secondary surface characteristics frequency 
         IF( SFCFRQ2 .EQ. 2) THEN
            FREQ_CHR2 = 'Month '
         ELSEIF( SFCFRQ2 .EQ. 3) THEN
            FREQ_CHR2 = 'Season'
         ELSEIF( SFCFRQ2 .EQ. 4) THEN
            FREQ_CHR2 = 'Annual'
         ELSE
C---        There must have been an error processing the primary surface 
C           characteristics; assign 'ERROR!' to FREQ_CHR for output file
            FREQ_CHR2 = 'ERROR!'
         ENDIF
         IF( STATUS(6,35) .EQ. 2 )THEN
            WRITE(DEVICE,1205) DISK42, FREQ_CHR2
         ELSE
            WRITE(DEVICE,1206) FREQ_CHR2
         ENDIF

C---     Check for errors in the secondary SITE_CHAR array based on OSSFC < 0.0
         IF( OSNWDS2 .GT. 0 )THEN
            DO ISECT = 1, OSNWDS2
               IF( ISECT .GT. 1 ) WRITE( DEVICE,'(1X)' )
               DO IMNTH = 1, 12
C---              Check for any negative values, indicating input errors
                  IF( OSSFC2(IMNTH,ISECT,1) .LT. 0.0 .OR.
     &                OSSFC2(IMNTH,ISECT,2) .LT. 0.0 .OR.
     &                OSSFC2(IMNTH,ISECT,3) .LT. 0.0 )THEN
                     NERRS2 = NERRS2 + 1
                  END IF
                  WRITE(DEVICE,1203) IMNTH,(OSWDS2(ISECT,NNN),NNN=1,2),
     &                       (OSSFC2(IMNTH,ISECT,ISSP),ISSP=1,OSMSITEC)
               ENDDO
            ENDDO
         ENDIF
      ENDIF

C---  Check for erros in the secondary SITE_CHAR2 array, NERRS2 > 0
      IF( NERRS2 .GT. 0 )THEN
         MESS =  BLNK80
         ECODE = 'E99'
         WRITE( MESS,1302 ) NERRS2
1302     FORMAT(' Errors in SITE_CHAR2 Array; Number of missing',
     &          ' values (< 0.0) = ', I4 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS ) 
      END IF


 1201 FORMAT(/4X, '5.  Primary Site Surface Characteristics from File: '
     &       //8X, A,
     &  //,10x,'User Inputs Varied by: ',A6,
     &  //,16x,          '  Wind Sector',15x, 'Bowen',3x,'Roughness',
     &  /,10x,'Month ', '  Start   End  ','  Albedo  ',
     &        '   Ratio  ',' Length (m)',
     &  /,10x,52('-') )

 1202 FORMAT(/4X, '5.  Primary Site Surface Characteristics ',
     &  //,10x,'User Inputs Varied by: ',A6,
     &  //,16x,          '  Wind Sector',15x, 'Bowen',3x,'Roughness',
     &  /,10x,'Month ', '  Start   End  ','  Albedo  ',
     &        '   Ratio  ',' Length (m)',
     &  /,10x,52('-') )

 1203 FORMAT(11x,I2,5x,F5.0,1x,F5.0,1x,F9.2,1x,F9.2,2x,F9.4)

 1205 FORMAT(/4X, '  Secondary Site Surface Characteristics from File: '
     &       //6X, A,
     &  //,10x,'User Inputs Varied by: ',A6,
     &  //,16x,          '  Wind Sector',15x, 'Bowen',3x,'Roughness',
     &  /,10x,'Month ', '  Start   End  ','  Albedo  ',
     &        '   Ratio  ',' Length (m)',
     &  /,10x,52('-') )
     
 1206 FORMAT(/4X, '  Secondary Site Surface Characteristics ',
     &  //,10x,'User Inputs Varied by: ',A6,
     &  //,16x,          '  Wind Sector',15x, 'Bowen',3x,'Roughness',
     &  /,10x,'Month ', '  Start   End  ','  Albedo  ',
     &        '   Ratio  ',' Length (m)',
     &  /,10x,52('-') )


C---> 6.  Names of the output files

      IF( ( STATUS(1,2)  .EQ. 2 ) .OR.
     &    ( STATUS(6,22) .EQ. 2 ) )THEN
         WRITE( DEVICE,2060 ) DISPMD(MDSTAT)
2060     FORMAT( /,4X,'6.  Input File(s) for ',A8,/)

         IF( STATUS(6,22) .EQ. 2 )THEN
            WRITE( DEVICE,2080 ) 
     &               DISK80(1:MIN(LEN_TRIM(DISK80),96))
2080        FORMAT(10X,'Surface Meteorology:  ', A:)
         ENDIF
         IF( STATUS(6,27) .EQ. 2 )THEN
            WRITE( DEVICE,2085 ) 
     &               DISK85(1:MIN(LEN_TRIM(DISK85),96))
2085        FORMAT(10X,'Profile Data       :  ',A:)
         ENDIF

      ENDIF


C---- Call subroutine to summarize messages
      CALL SUMRY2(3)


C---> 7.  Processing Statistics

      WRITE(DEVICE, 2200) K_CALM, K_VARWD
 2200 FORMAT(//' The number of CALM winds  encountered is: ', I6, 
     &        /' The number of VARIABLE WD encountered is: ', I6)
      IF( CCVR_SUB )THEN
         WRITE(DEVICE, 2201) K_CCVR_Sub
 2201 FORMAT(  ' The number of miss CCVR substitutions is: ', I6)
      ENDIF
      IF( TEMP_Sub )THEN
         WRITE(DEVICE, 2202) K_TEMP_Sub
 2202 FORMAT(  ' The number of miss TEMP substitutions is: ', I6)
      ENDIF
      IF( L_1MINASOS_THRESH )THEN
         WRITE(DEVICE, 2209) THRESH1SPD
 2209    FORMAT(/' A threshold of ',F5.2,' m/s has been applied to',
     &            ' 1-minute ASOS winds.')
         WRITE(DEVICE, 2210) iCALM1MIN        
 2210    FORMAT(' The number of CALM winds identified from applying',
     &          ' the',
     &         /'  threshold to the 1-minute ASOS winds is: ', I6,
     &         /' These cases may be inlcuded in the CALM winds',
     &          ' reported above.'//)

 5060 FORMAT( /10X,'The Processed Dates Are:    Starting: ',A11/
     &         10X,'                              Ending: ',A11)
 5080 FORMAT(  I2,'-',A3,'-',I4.4)

 5061 FORMAT( /10X,'The Extract Dates Are: *** IN ERROR ***')
 5062 FORMAT( /10X,'Based on the MERGE file header records.')

      ENDIF

      RETURN
      END

