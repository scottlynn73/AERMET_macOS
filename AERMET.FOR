      PROGRAM AERMET
C=======================================================================
C            MAIN Module of the AERMET Meteorological Processor
C                         Version Dated 16216
C
C                             August 3, 2016
C
C            *** SEE AERMET MODEL CHANGE BULLETIN MCB#7 ***
C
C       ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS (SCRAM) WEBSITE
C
C                      http://www.epa.gov/scram001/
C
C=======================================================================
C
C       This revised version of AERMET (dated 16216) includes several
C       modifications relative to the previous version (dated 15181);
C       see AERMET Model Change Bullentin (MCB) #7 and AERMET User's
C       Guide Addendum.
C
C       http://www.epa.gov/ttn/scram/metobsdata_procaccprogs.htm#aermet
C
C
C-----  MODIFIED BY:    Roger W. Brode
C                       U.S. EPA, OAQPS/AQAD
C                       Air Quality Modeling Group
C
C                       August 3, 2016
C
C-----  MODIFIED FROM:         AERMET
C                       (Version Dated 15181)
C
C
C-----  BUG FIXES
C
C       1.  Modified subroutines MPFIN and SUBST to set RANDOM as the
C           default selection for the WIND_DIR variable under the
C           METHOD option in Stage 3 for NWS wind directions. This
C           affects Stage 3.
C
C       2.  Modified subroutine UCALST to remove the code for
C           adjusting USTAR if CHEK .GT. 1 for the ADJ_U* option.
C           Also incorporated a lower limit of USTCR for USTAR based
C           on Equation 26 of Qian and Venkatram (BLM, 2011). This
C           affects Stage 3.
C
C       3.  Modified subroutine BULKRI to set a lower limit of 1.0m
C           for the Monin-Obukhov length (L) as one of the criteria
C           for exiting the DO WHILE loop to avoid NaN’s for USTAR in
C           the surface output file for the ADU_U* option. Subroutine
C           BULKRI was also modified to use BETAM = 5.0 for the
C           ADJ U* option instead of 4.7. This affects Stage 3.
C
C       4.  Modified subroutine MPMET to correct the FORMAT statement
C           for including the ‘BULKRN/MMIF’ string in the surface
C           file header record. This affects Stage 3.
C
C
C-----  Miscellaneous:
C
C       1.  Modified several subroutines with minor or cosmetic
C           changes to the documentation of user options and error
C           handling and reporting. This affects all stages.
C
C=======================================================================
C
C----- NOTE: Summaries of AERMET code changes prior to version 16216
C            have been moved to the end of the 'aermet.for' file,
C            just before the BLOCK DATA block there.      
C
C=======================================================================

      IMPLICIT NONE

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'


      CHARACTER*96 INPFILE
      INTEGER  :: ISTAT, ISTAGE, DEVICE

c     WRITE(6, *)'Enter the name of the input file:     '               ! ! dtb001 01085
c     WRITE(6, *)                                                       ! ! dtb001 01085
c     READ(5, '(A96)') INPFILE                                          ! ! dtb001 01085
c     WRITE(6, *)'The input file is:  ', INPFILE                        ! ! dtb001 01085

c     OPEN (DEVIN, FILE=INPFILE, STATUS='OLD', ERR=900)                 ! ! dtb001 01085

      INPFILE = 'AERMET.INP'

      OPEN (DEVIN, FILE=INPFILE, STATUS='OLD', ERR=900)

C---- Open the console/screen for model output displays
C      OPEN( UNIT=3, FILE='CON', CARRIAGECONTROL='FORTRAN')

C---- Display version of AERMET that is being run
      WRITE( *,1 ) VERSNO
    1 FORMAT('   AERMET Version ', A6 )


C---- Initialize OSLON & OSLAT in case no ONSITE data
      OSLON = BLNK08
      OSLAt = BLNK08
      ISTAGE = 0
      PGNUM  = 0
      STATUS = 0
      OSDNUM = 0
      OSHTPREV = 0
      L_HgtsVary = .FALSE.

C---- Call PRESET to pre-process runstream input file ('aermet.inp')
C     to determine which stage of processing is being performed
C     based on pathways included.
      CALL PRESET( ISTAGE )

C---- Process user supplied set up data.
      CALL SETUP( ISTAGE )

C---- Assign processing stage:
      IF( STABLK(5) .EQ. 2 )THEN
C        MERGE Pathway in Stage 2
         ISTAGE = 2
      ELSEIF( STABLK(6) .EQ. 2 )THEN
C        METPREP Pathway in Stage 3
         ISTAGE = 3
      ELSEIF( STABLK(2) .EQ. 2 .OR. STABLK(3) .EQ. 2 .OR.
     &        STABLK(4) .EQ. 2 )THEN
C        UPPERAIR, SURFACE, or ONSITE Pathway in Stage 1
         ISTAGE = 1
      ENDIF

C---- Call status summary, SUMRY1
C         This routine is called before any data processing because
C         all pathway statuses are set at this point.  Errors in data
C         processing may change the status and, hence, the original
C         intent for data processing is lost
      CALL SUMRY1( ISTAGE )

C---- Test status words (all must be ok, or we skip processing)

      IF( JBSTAT.LT.0 .OR. UASTAT.LT.0 .OR.
     &    SFSTAT.LT.0 .OR. OSSTAT.LT.0 .OR.
     &    MRSTAT.LT.0 .OR. MPSTAT.LT.0 )THEN
         GO TO 10
      ENDIF

      IF( .NOT. SETERR .AND. STABLK(6) .NE. 2 )THEN
C------- Perform STAGE1 or STAGE2 processing

C------- Rewind the temporary file (DEV70) for later use

         REWIND DEV70

C------  UPPERAIR data pathway - extract and/or QA.

         IF( STABLK(2) .EQ. 2  .AND.  JBSTAT .NE. -1 )THEN
            CALL UAPATH
            ISTAGE = 1
         ENDIF

C------- SURFACE data pathway - extract and/or QA.

         IF( STABLK(3) .EQ. 2  .AND.
     &      JBSTAT .NE. -1    .AND.  UASTAT .NE. -1 )THEN
            CALL SFPATH
            ISTAGE = 1
         ENDIF

C------- ONSITE data pathway - QA.

         IF( STABLK(4) .EQ. 2  .AND.  JBSTAT .NE. -1  .AND.
     &       UASTAT .NE. -1    .AND.  SFSTAT .NE. -1 )THEN
            CALL OSPATH
            ISTAGE = 1
         ENDIF

C------- MERGE data pathway.

         IF( STABLK(5) .EQ. 2 .AND.
     &       JBSTAT .NE. -1   .AND.  UASTAT .NE. -1 .AND.
     &       SFSTAT .NE. -1   .AND.  OSSTAT .NE. -1 )THEN
            CALL MRPATH
            ISTAGE = 2
         ENDIF

C------- Close out run, generating the summary of the run.

         WRITE( *,8 )
    8    FORMAT('   Processing completed; writing summary files'/ )

C----    Call the QA audit routine if there was a QA on any pathway

         ISTAT = 0
         IF( (UASTAT .EQ. 2) .OR. (UASTAT .EQ. 3) .OR.
     &       (SFSTAT .EQ. 2) .OR. (SFSTAT .EQ. 3) .OR.
     &       (OSSTAT .EQ. 2) .OR. (OSSTAT .EQ. 3) )THEN
            CALL AUDIT( ISTAGE,ISTAT )
            IF( ISTAT.EQ.1 )THEN
               RUNERR = .TRUE.
            ENDIF
         ENDIF

         IF( STATUS(1,1).EQ.2 )THEN
            DEVICE = DEV50
         ELSE
            DEVICE = DEVIO
         ENDIF

         IF( ISTAGE .EQ. 1 .AND. OSSTAT .NE. 0 )THEN
C----       Call routine to summarize ONSITE values:
C           OBS/HOUR, calm threshold and heights for tower data
            CALL OSSMRY (DEVICE)
         ENDIF

C----    Call the summary routine (the argument defines the stage)
         CALL SUMRY2( ISTAGE )

      ELSEIF( .NOT. SETERR .AND. STATUS(1,3) .EQ. 0 )THEN
C------- Perform STAGE3 processing
C

C------- METPREP data pathway.

         IF( STABLK(6) .EQ. 2 .AND.
     &       JBSTAT .NE. -1   .AND.  UASTAT .NE. -1 .AND.
     &       SFSTAT .NE. -1   .AND.  OSSTAT .NE. -1 .AND.
     &       MRSTAT .NE. -1 )THEN

            ISTAT = 0
            WRITE( *,* ) ' '
C---------- Call MPMET to proces merged data in Stage 3
            CALL MPMET( ISTAT )

            IF( ISTAT .EQ. 1 )THEN
C              An error condition occurred in Stage 3
               MPSTAT = -1
               RUNERR = .TRUE.
            ENDIF
         ENDIF

C        Close out the run, generating such reports as needed.

         WRITE( *, 18 )
   18    FORMAT('   Processing completed; writing summary files'/ )

         CALL MPFIN( ISTAGE )

      ENDIF

C --- Branch statement to skip processing when SETUP errors are found
   10 CONTINUE

      IF( STATUS(1,1) .EQ. 2 )THEN
C ---    Issue message regarding the Summary Report file
         WRITE(DEVIO,5060) DISK50(1:MIN(LEN_TRIM(DISK50),96))
      ENDIF

      IF( SETERR .AND. ISTAGE .EQ. 3 )THEN
C ---    Call MPFIN to get the summary of inputs if
C        setup errors occur for Stage 3
         WRITE( *, 18 )
         CALL MPFIN( ISTAGE )

      ELSEIF( SETERR .AND. (ISTAGE.EQ.1 .OR. ISTAGE.EQ.2) )THEN
C ---    Call SUMRY2 to get the summary of inputs if
C        setup errors occur for Stages 1 or 2
         CALL SUMRY2( ISTAGE )

      ENDIF

C --- Close and delete temporary files
      close(unit=dev70,status="delete")
      close(unit=dev75,status="delete")

      GO TO 910

  900 WRITE(6, '(A17,A)') ' Error opening:  ', INPFILE

  910 CONTINUE

      STOP

5060  FORMAT(/3X,'The Summary Report Generated by AERMET Is In: ',
     &         /5X,A:/)

      END

C=======================================================================
C
C----- NOTE: Summaries of AERMET code changes prior to version 16216
C            are summarized below:
C
C=======================================================================
C
C
C       This revised version of AERMET (dated 15181) includes several
C       modifications relative to the previous version (dated 14134);
C       see AERMET Model Change Bullentin (MCB) #6 and AERMET User's
C       Guide Addendum.
C
C       http://www.epa.gov/ttn/scram/metobsdata_procaccprogs.htm#aermet
C
C
C-----  MODIFIED BY:    Roger W. Brode
C                       U.S. EPA, OAQPS/AQAD
C                       Air Quality Modeling Group
C
C                       June 30, 2015
C
C-----  MODIFIED FROM:         AERMET
C                       (Version Dated 14134)
C
C
C-----  Bug Fixes:
C
C       1.  Modified subroutines UCALST and MPPBL to incorporate a
C           constant value of THSTAR of 0.08, full inclusion of the
C           displacement height, and a modified formulation for     
C           Monin-Obukhov length for the ADJ_U* option based on Qian
C           and Venkatram (BLM, v138, 2011). Subroutine UCALST was
C           also modified to adjust USTAR for wind speeds below the
C           “critical” wind speed. This affects Stage 3.
C
C       2.  Modified subroutine BULKRI to correct the calculation of
C           CDN to use ZREF(IHR)/Z0(IHR) instead of Z2/Z0(IHR).
C           Subroutine BULKRI was also modified to use BETAM = 0.5
C           for the ADJ_U* option instead of 0.47. This affects
C           Stage 3.
C
C
C-----  Enhancements:
C
C       1.  Modified subroutine BULKRI to incorporate additional
C           refinements to the ADJ_U* Beta option in conjunction with
C           the Bulk Richardson Number (BULKRN) option, based on
C           Luhar and Raynor (BLM, v132, 2009), including a more
C           refined method for calculating THSTAR, and extending its
C           applicability for very stable/low wind conditions. This
C           affects Stage 3.
C
C       2.  Modified subroutine MERGE to include the year in the
C           Stage 2 table of Daily Output Statistics. This affects   
C           Stage 2.
C
C       3.  Subroutine GET620 was modified to issue a message if the
C           UALOC in the upper data file does not match UALOC
C           specified on the LOCATION keyword. This allows users to
C           track when data from alternative representative upper air
C           stations are used in conjunction with the UPPERAER tool.
C           This affects Stage 1.
C
C-----  Miscellaneous:
C
C       1.  Modified several subroutines with minor or cosmetic
C           changes to the documentation of user options and error
C           handling and reporting. This affects all stages.
C
C=======================================================================
C
C       This revised version of AERMET (dated 14134) includes several
C       modifications relative to the previous version (dated 13350);
C       see AERMET Model Change Bullentin (MCB) #5 and AERMET User's
C       Guide Addendum.
C
C       http://www.epa.gov/ttn/scram/metobsdata_procaccprogs.htm#aermet
C
C
C-----  MODIFIED BY:    Roger W. Brode
C                       U.S. EPA, OAQPS/AQAD
C                       Air Quality Modeling Group
C
C                       May 14, 2014
C
C-----  MODIFIED FROM:         AERMET
C                       (Version Dated 13350)
C
C
C-----  Bug Fixes:
C
C       1.  Modified subroutines SUBST and MPPBL to correct potential
C           issues associated with the options that were introduced
C           in version 13350 to substitute for missing cloud cover 
C           (CCVR) and/or temperature (TEMP) by linear interpolation
C           across 1 or 2-hour gaps. This included an error that may
C           have resulted in substituted CCVR values larger than 10
C           for hour 01, which may have resulted in 'NaN' (not a
C           number) for some calculated variables.
C
C           Issues with CCVR substitutions may also have occurred for
C           applications using ONSITE meteorological data only, with
C           ONSITE net radiation (NRAD) or with solar radiation 
C           (INSO) and delta-T (DT01) data using the Bulk Richardson
C           Number (BULKRN) option. In these cases AERMET calculates
C           equivalent cloud cover values that are included in the
C           surface output file, but no substitutions for missing
C           values should be included. This affects Stage 3.
C
C       2.  Modified subroutine SUBST to remove the initialization of
C           the GOT_OSTMP logical variable to .FALSE. for the current
C           hour. The GOT_OSTMP variable is assigned a value in
C           subroutine GETTEMP prior to the call to subroutine SUBST.
C           This error may have affected calculations of station
C           pressure and relative humidity for applications with
C           ONSITE data, including relative humidity values greater
C           than 100 for applications with both SURFACE and ONSITE
C           data. This affects Stage 3.
C
C       3.  Modified subroutine GETTEMP to reinitialize the LTZMAX4T
C           variable to .TRUE. within the hour loop, indicating that
C           the maximum height for ONSITE temperature had not been
C           reached yet. The ONSITE temperature may have been
C           incorrectly interpreted as being missing in version 13350
C           for some cases. This affects Stage 3.
C
C       4.  Modified subroutines GETTEMP and SUBST to ensure that
C           substitutions for missing ONSITE temperature data are
C           only based on values from the same measurement level if
C           multiple levels of temperature are available. This
C           affects Stage 3.
C
C       5.  Modified subroutine MPPBL to address issues associated
C           with the selection of the appropriate upper air sounding
C           time. This includes use the INT function instead of NINT
C           when assigning ‘MyZone’ to determine the reference
C           sounding time based on the longitude of the location for
C           the default option. Subroutine MPPBL was also modified to
C           use an integer variable (ICHRND_UASRISE) instead of a
C           real variable (CHRND_UASRISE) for assigning the
C           ‘START_WINDOW’ when the UASELECT SUNRISE option is used.
C           This affects Stage 3.
C
C       6.  Modified subroutines FLOPEN and WRTCRD to use
C           STATUS='REPLACE' instead of STATUS='SCRATCH' when opening
C           the DEV70 and DEV75 "temporary" files to store the header
C           records in the ".IQA", ".OQA", and MERGE data files that
C           contain information for reprocessing of input options
C           during subsequent stages of AERMET processing. This
C           modification addresses issues that were encountered where
C           AERMET was not able to locate the 'SCRATCH' files when
C           applying a 64-bit version of AERMET. This affects all
C           stages.
C
C       7.  Modified subroutines SUMHF and SUNDAT to eliminate
C           integer/real mixed-mode calculations in summing the
C           hourly heat fluxes, and in calculating sunrise/sunset
C           times and solar elevation angles. This affects Stage 3.
C
C       8.  Modified subroutine BULKRI to include the THSTAR
C           adjustment for low solar elevation angles under the
C           original BULKRN method and for the
C           non-Default/BETA ADJ_U* option associated with the BULKRN
C           option. Subroutine BULKRI was also modified to avoid an
C           array subscript out-of-bounds error in cases where the
C           iterative approach does not converge within the specified
C           loop limits. This affects Stage 3.
C
C       9.  Modified subroutine MPFIN to include additional error
C           handling and reporting associated with processing of
C           surface characteristics This affects Stage 3.
C
C-----  Enhancements:
C
C       1.  Modified subroutines VRCARD, SUBST, MPPBL, and MPOUT to
C           allow users to disable substitutions for missing
C           CCVR and/or TEMP data that are based on persistence for
C           hours 23 and 24. Additional flags have been added to the
C           surface output file to identify when this option is
C           applicable. These changes also allow users to specify
C           either the NO_SUB or NOTSUB keyword to disable TEMP
C           substitutions. This affects Stage 3.
C
C       2.  Subroutine OSDTCD was modified to include logical
C           variables to identify the availability of ONSITE data for
C           ambient temperature, solar radiation/insolation, cloud
C           cover, net radiation, delta-T, and/or mixing heights.
C           Several other subroutines were modified, including MPPBL,
C           MPMET, MPFIN, and MPTEST to track whether substitutions
C           for missing temperature and/or cloud cover should be
C           implemented by default, based on whether only ONSITE data
C           or only SURFACE data are available for a given parameter.
C           Cloud cover and temperature substitutions were not
C           implemented by default in v13350 if both SURFACE and
C           ONSITE data were available, irrespective of whether the
C           substituted parameter was included in both SURFACE and
C           ONSITE data inputs. Version 14134 will apply CCVR and/or
C           TEMP substitutions by default if the parameter is only
C           included in the SURFACE or ONSITE data. However, the user
C           options to disable substitutions are still available.
C           This affects Stage 3.
C
C       3.  Modified subroutines MPPBL, SUBST, and EQ_CCVR to address
C           potential issues with the use of ONSITE solar radiation
C           and delta-T data in lieu of cloud cover (utilizing the
C           delta-T based Bulk Richardson Number (BULKRN) method for
C           stable conditions) or if delta-T/BULKRN is being applied
C           with ONSITE or NWS cloud cover. These modifications were
C           focused especially on the transition from nighttime
C           stable conditions (controlled by delta-T/BULKRN) and
C           daytime convective conditions (controlled by solar
C           radiation data). The BULKRN/delta-T method will be used
C           if valid delta-T and ONSITE wind data are available and
C           if the delta-T lapse rate is stable, or if the cloud
C           cover or insolation data is missing, or if the cloud
C           cover is an "equivalent" cloud cover derived from solar
C           radiation data when the solar elevation angle is less
C           than the critical elevation angle for convective
C           conditions.
C
C           The critical solar elevation angle was also modified to
C           use an average of the previous 24-hours of temperature
C           data (with at least 75% data capture), instead of a
C           default value of 288 K, for hours when the temperature is
C           missing. An assumption of 5/10 cloud cover is also used
C           in the calculation of critical solar elevation angle for
C           hours when cloud cover is missing. Since the model will
C           not compute concentrations for these hours due to missing
C           temperature and/or cloud cover the effect of these
C           changes on modeled concentrations is likely to be
C           minimal. Furthermore, this approach should provide a
C           reasonable estimate of early morning heat fluxes, and
C           treating these hours as missing in terms of heat flux
C           could result in the entire convective portion of the day
C           being missing. This affects Stage 3.
C
C       4.  Subroutine EQ_CCVR was modified to account for cases when
C           the clear sky insolation value, QRNOT, is less than or
C           equal to 0. The equivalent CCVR is set to 0 in these
C           cases. This affects Stage 3.
C
C       5.  Modified subroutines MPMET, MPFIN, MPOUT, and AERSURF to
C           track and report the use of MMIF-generated inputs, in the
C           form of pseudo-ONSITE data alone (including "ONSITE"
C           mixing heights) or as pseudo-ONSITE and UPPERAIR data.
C           The use of MMIF-generated inputs is gleaned from
C           information in the AERSURF input file of surface
C           characteristics if it is used in Stage 3. The use of MMIF
C           data is flagged in the header record of the surface file
C           and by the use of the 'MMIF-OS' flag instead of the
C           hourly NAD-OS, ADJ-SFC, etc., flags associated with
C           ONSITE and/or SURFACE data.
C
C           Subroutines MPMET, MPFIN, and MPOUT were also modified to
C           track and report the use of the Bulk Richardson Number 
C           (BULKRN) option based on delta-T measurements, in lieu of
C           cloud cover, for stable conditions. For applications
C           using the BULKRN option, the header record of the surface
C           file will include the 'BULKRN' flag. An additional 'MMIF'
C           flag will be included if the BULKRN option is associated
C           with use of MMIF data. This affects Stage 3.
C
C       6.  Modified subroutine GET620 to allow the user to specify a
C           standard 5-digit WBAN, instead of an 8-digit number with
C           3 leading 0's, for processing upper air data in the 6201
C           format. Checks have also been included to identify issues
C           that may occur when users specify the 6201VB (TD-6201
C           variable-length block) format for upper air data. This
C           affects Stage 1.
C
C       7.  Modified subroutine GETFSL to read the WMO number from
C           upper air data in the FSL format, in addition to reading
C           the WBAN, and assign the WMO number to the BUF08(1)
C           variable if WBAN = '99999' to support non-US
C           applications. This affects Stage 1.
C
C       8.  Modified subroutines SETUP, MPPROC, OSCARD, MRCARD,
C           WRTCRD, and HEADER to allow re-processing of the XDATES
C           information from the MERGE file header records. This
C           information is included in the Stage 3 report, unless the
C           user has specified XDATES in the Stage 3 input file. Also
C           modified subroutine MERGE to use a consistent form for
C           reporting the extract dates in the report files across
C           all three stages. Subroutine CHROND was modified to fully
C           support 4-digit years and subroutine XDTCRD was modified
C           to use 4-digit years for extract dates in the report
C           files instead of 2-digit years. This affects Stage 3.
C
C-----  Miscellaneous:
C
C       1.  Modified several subroutines with minor or cosmetic
C           changes to the documentation of user options and error
C           handling and reporting, including elimination of trailing
C           blanks associated with filenames, etc., to minimize line
C           wrap in the AERMET report files. This affects all stages.
C
C       2.  Modified several subroutines to replace the FLOAT Fortran
C           function with the REAL function to convert integer
C           variables to reals. This affects all stages.
C
C=======================================================================
C
C       This revised version of AERMET (dated 13350) includes the
C       following modifications relative to the previous version
C       (dated 12345); see MCB#4 and updated User's Guide Addendum:
C
C
C-- Bug Fixes:
C
C   1.  Modified subroutine UCALST to incorporate AECOM's recommended
C       corrections to theta-star under the ADJ_U* Beta option.
C
C   2.  Modified subroutine MPMET to correct assignment of time-zone
C       adjustment (ZONE) to be based on the upper air time-zone (UALST)
C       for cases where both upper-air data and ONSITE mixing heights
C       are available.
C
C   3.  Modified subroutine SBLHT to correct the coefficient for
C       the mechnical mixing height from 2300 to 2400 based on
C       the original Venkatram reference (BLM, 2009).
C
C   4.  Modified subroutine MPPBL to correct the initialization of
C       the NO_SKY variable for missing on-site cloud cover to use
C       the OSTSKY(2) value based on the missing data code input by
C       the user, instead of a value of 99. This error could have
C       resulted in on-site cloud cover being flagged as missing
C       inappropriately in previous versions.
C
C   5.  Modified subroutine O3NEXT to remove the conversion of
C       delta-T values to lapse rate values (delta-T/delta-z)
C       before calling subroutine REALQA, which compares the value
C       to upper and lower bounds based on delta-T values. This
C       could have resulted in a large number of spurious QA warnings
C       for delta-T values out-of-range.
C
C   6.  Modified subroutine RDISHD to correct the processing of ASOS
C       cloud cover data based on the GA1-GA6 codes to avoid
C       inadvertently assigning a missing code to the ASKY variable.
C       Also modified subroutine RDISHD to move the check for hour
C       0 (zero) later in the processing in order to avoid using
C       a "special" ISHD observation that occurs on the hour instead
C       of a "non-special" (i.e., preferred) observation that occurs
C       earlier.
C
C   7.  Modified subroutine MPFIN to include additional error
C       checking and reporting regarding the completeness of the
C       surface characteristics arrays. Errors in processing user-
C       specified surface characteristics (i.e., not generated by
C       AERSURFACE) may have occurred in previous versions. This
C       modification also includes initializing the storage arrays
C       for surface characteristics in BLOCK1.INC to -9.0 instead
C       of 0.0.
C
C   8.  Modified several subroutines to allow the use of on-site
C       mixing heights only (OSMIX), without requiring upper air
C       data.  In these cases, the LOCATION keyword on the METPREP
C       pathway can be used to specify the time zone adjustment from
C       local time to GMT, since the time zone specified on the
C       ONSITE pathway is likely to be for local time.  Otherwise,
C       AERMET would assume GMT as local time and results would
C       be incorrect (unless local time actually was GMT). This
C       includes a modification to subroutine MPCARD to call
C       subroutine MPPROC to obtain information on OSMIX and UADATA,
C       before the METPREP LOCATION keyword is processed, and to
C       to return the new MPLST variable from the location keyword
C       processed in subroutine LOCCRD to identify the local vs.
C       GMT zone adjustment for cases with only ONSITE data. The
C       call to subroutine MPPROC, previously made in subroutine
C       SETUP, has been removed.
C
C       The warning message generated in subroutine LOCCRD was
C       also modified to indicate that the METPREP LOCATION
C       keyword is no longer considered obsolete if OSMIX and
C       no UADATA are being used.
C
C       Additional modifications were made to the initialization
C       and processing of the location IDs for the surface (SF),
C       upper air (UA) and onsite (OS) locations in order to
C       select the appropriate location for determining time
C       zone adjustments in Stage 3 processing. Previous
C       versions used character variables of various lengths
C       that were initialized as blank fields in WORK1.INC.
C       However, the initializations were accomplished by an
C       EQUIVALENCE statement in WORK1.INC, which may cause
C       problems when checking whether a location ID was
C       blank by comparing to the equivalenced variables.
C
C   9.  Modified subroutine MPPBL to correct the error message
C       associated with missing station pressure (FORMAT 1022)
C       which was also used for missing sky cover data.  A new
C       FORMAT statement (1021) was added for use with missing
C       station pressure. Also removed several FORMAT statements
C       in MPPBL that were not used.
C
C-- Enhancements:
C
C   1.  Modified subroutine BULKRI to incorporate a modified Bulk
C       Richardson Number approach under the ADJ_U* Beta option,
C       based on Luhar and Raynor (2009).  As with the ADJ_U* Beta
C       option incorporated in UCALST for applications which do not
C       employ the BULKRN option, this method is considered a
C       non-DFAULT option requiring approval as an alternative
C       model for use in regulatory applications.
C
C       Luhar A.K. and K. N. Rayner, 2009. Methods to Estimate Surface
C            Fluxes of Momentum and Heat from Routine Weather Observations
C            for Dispersion Applications under Stable Stratification.
C            Boundary-Layer Meteorology. 132:437454.
C
C   2.  Modified subroutines MPPBL, SUBST, VRCARD, and MPTEST to
C       include substitutions for missing cloud cover data and/or
C       missing ambient temperature by interpolating across 1 or 2-hour
C       gaps in the input data. New subroutines (GETCCVR and GETTEMP)
C       were also created to facilitate this enhancement.  The default
C       option is for AERMET to apply the substition for both missing
C       cloud cover and temperature, unless the user has specified
C       both SURFACE and ONSITE data.  New options have been added
C       to the METHOD keyword on Stage 3 that allow the user to
C       disable the cloud cover and/or temperature substition(s)
C       in cases where only SURFACE or ONSITE data are available,
C       or to activate the options in cases where both SURFACE and
C       ONSITE data are available. Subroutines MPMET and MPOUT
C       were also modified to include appropiate flags in the
C       surface output file indicating when these options have
C       been utilized.
C
C-- Miscellaneous:
C
C   1.  Modified several subroutines to improve error handling and
C       reporting in general, in addition to the modifications in
C       subroutine MPFIN described above as item 5 under Bug Fixes
C       to correct a potential bug related to processing of surface
C       characteristics.
C
C   2.  Modified subroutine MPFIN to include the temporal frequency
C       of the original user input of surface characteristics, i.e.,
C       MONTHLY, SEASONAL, or ANNUAL, since the summary table included
C       in the Stage 3 report file automatically lists values by month.
C
C   3.  Modified subroutine MPFIN to eliminate a redundant message
C       if the default ((Holtslag) method is used for the stable
C       boundary layer, without the non-default ADJ_U* Beta option.
C
C   4.  Included a DATA statement in subroutine RDISHD to initialize
C       the default "window" for acceptable data records, INC = 30
C       (i.e., up to 30 minutes before the hour, including the hour)
C       instead of a standard assignment statement.
C
C
C-----  MODIFIED BY:    Roger W. Brode
C                       U.S. EPA, OAQPS/AQAD
C                       Air Quality Modeling Group
C
C                       December 16, 2013
C
C-----  MODIFIED FROM:         AERMET
C                       (Version Dated 12345)
C
C=======================================================================
C
C       This revised version of AERMET (dated 12345) includes the
C       following modifications relative to the previous version
C       (dated 11059); see MCB#3 and updated User's Guide Addendum:
C
C
C-- Bug Fixes:
C
C   1.  Modified subroutine DOCLDS to correct for errors that
C       result when blank spaces occur between the cloud cover
C       and ceiling height fields for HUSWO surface data. Also
C       modified DOCLDS to no longer accept invalid ASOS cloud
C       cover codes for HUSWO surface data. These bug fixes
C       should not affect results for HUSWO surface data that
C       were extracted from the HUSWO CD, but could affect
C       results significantly for applications using surface
C       data that were reformatted to the HUSWO format from
C       another data format.
C
C   2.  Modified subroutine CBLHT to correct an error in the
C       calculation of the convective mixing height. Previous
C       versions of AERMET may have underestimated convective
C       mixing heights, and in turn the convective velocity
C       scale (w*), due to this error. The magnitude of the
C       errors would tend to be larger for cases where the
C       vertical temperature profile based on the upper air
C       sounding was based on fewer sounding levels. However,
C       due to the widespread nature of these convective mixing
C       height errors, AERMOD modeling results are likely to
C       be affected to some degree by this AERMET bug fix in
C       most cases.
C
C       The magnitude and direction of changes in AERMOD model
C       results due to this convective mixing height bug fix
C       will vary depending on the resolution of the upper
C       air data and the characteristics of the sources and
C       terrain included in the AERMOD model application. Also,
C       as noted in Section 3.2 of the "AERMOD Implementation
C       Guide," upper air data input to AERMET should be based
C       on "all levels" or "mandatory and significant levels"
C       to ensure adequate resolution of the vertical profile;
C       use of upper air data based on "mandatory levels only"
C       is not acceptable for AERMOD modeling applications.
C
C   3.  Modified subroutines CBLHT, MPPBL, MPMET, and SETUP
C       to check for ONSITE mixing heights (OSMIX) in the
C       determination of data completeness, and adjusted
C       warning messages to account for possible presence
C       of both ONSITE mixing heights and UPPERAIR data.
C
C
C-- Enhancements:
C
C   1.  Added a new THRESH_1MIN keyword to Stage 3 processing.
C       This optional keyword allows the user to specify a threshold
C       wind speed below which winds reported in the 1-minute ASOS
C       data processed through AERMINUTE, if in use, are considered
C       calm. There is no default value; however, a warning message
C       will be generated if the user-specified threshold is greater
C       than 0.5 m/s, and a fatal error message will be generated if
C       the user-specified threshold is less than zero (0) or greater
C       than 1.0.
C
C       This new keyword also required that the length of the
C       KEYWRD variable be increased from 10 to 12 characters,
C       resulting in modifications to several subroutines.
C
C   2.  Incorporated new "BETA" option to adjust u* (ustar) for
C       low wind speed stable conditions, based on Qian, W.,
C       and A. Venkatram, 2011: Performance of Steady-State
C       Dispersion Models Under Low Wind-Speed Conditions.
C       Boundary-Layer Meteorology, 138, 475-491. The new option
C       is selected by including the METHOD STABLEBL ADJ_U*
C       keyword on the METPREP pathway in the Stage 3 input file.
C
C       The ADJ_U* "BETA" option is considered to be a non-Default
C       option and is therefore subject to the alternative model
C       provisions in Section 3.2 of Appendix W (40 CFR Part 51).
C       Users should coordinate with the appropriate reviewing
C       authority regarding the procedures and requirements for
C       approval of this BETA option for regulatory modeling
C       applications.  Use of this option also requires the user
C       to include the BETA option on the CO MODELOPT keyword in
C       the AERMOD input file.
C
C   3.  Arrays for storing upper air soundings were increased to
C       accommodate up to 48 soundings, including the current
C       day and the previous day.
C
C
C-- Miscellaneous:
C
C   1.  The value used to adjust ASOS wind speeds to account for
C       bias due to wind speeds being truncated to whole knots was
C       modified to include three significant digits (from 0.26 m/s
C       to 0.257 m/s, based on 0.5 kt). This modification avoids
C       potential for mis-characterization of calm vs. non-calm
C       due to precision issues associated with the truncation
C       adjustment under the new THRESH_1MIN option.
C
C   2.  Modified subroutine GETFSL to issue a non-fatal warning
C       instead of a fatal error message when the UA station ID
C       specified on the UA LOCATION keyword does not match the
C       UA station ID in the data file. This change will accommodate
C       substitutions for missing UA soundings using data from other
C       representative UA stations, and will document when such
C       substitutions were made in the Stage 1 report file.
C       Since all convective hours will be missing on days when the
C       UA sounding is missing, this will allow users to minimize
C       the number of missing hours in the AERMET-processed surface
C       file due to missing UA soundings, while maintaining a record
C       of when such substitutions were made.
C
C   3.  Modified subroutine RDHUSW for processing HUSWO surface
C       data to change the "no records extracted" warning message
C       to a fatal error.
C
C   4.  Modified subroutines EQ_CCVR and INCRAD to remove a trailing
C       period within the single quotes in the INCLUDE 'WORK1.INC'
C       statements.
C
C
C-----  MODIFIED BY:    Roger W. Brode
C                       U.S. EPA, OAQPS/AQAD
C                       Air Quality Modeling Group
C
C                       James O. Paumier
C                       AMEC, Inc.
C
C                       December 10, 2012
C
C-----  MODIFIED FROM:          AERMET
C                       (Version Dated 11059)
C
C=======================================================================
C
C       This revised version of AERMET (dated 11059) includes the
C       following modifications relative to the previous version
C       (dated 06341); see MCB#2 and updated User's Guide Addendum:
C
C
C-- Bug Fixes:
C
C   1.  Corrected several problems associated with the processing of
C       subhourly ONSITE meteorological data, including:
C
C       a) Modified the procedure for calculating hourly averages
C          to use the "hour-ending" convention in accordance with
C          Section 7.3 of "Meteorological Monitoring Guidance for
C          Regulatory Modeling Applications," EPA-454/R-99-005,
C          February 2000; previous versions used the "hour-beginning"
C          convention.
C
C       b) Corrected the procedure for calculating hourly averages
C          from subhourly values for sigma-theta.  Previous versions
C          used an equation in Section 5.2 of the AERMET User's Guide,
C          but this has been replaced with the equation referenced in
C          Table 8-3 of Appendix W. The equation from Section 5.2 of
C          the AERMET User's Guide was also coded incorrectly in
C          previous versions of AERMET, which resulted in runtime
C          errors in some cases due to taking a square root of a
C          negative number.
C
C       c) Corrected the processing of subhourly wind data to properly
C          code hours as calm when more than half of the samples for
C          the hour are non-missing, but below the wind threshold.
C          Previous versions coded these hours as missing rather than
C          calm.
C
C       d) Addressed several issues associated with the extraction of
C          ONSITE data that could result in erroneous values being
C          assigned to ONSITE variables, and also improved error
C          handling and reporting for the processing of ONSITE data.
C
C       e) Corrected problems associated with processing of ONSITE
C          precipitation data, including the fact that subhourly
C          precipitation values were averaged rather than summed
C          to determine the hourly value, and that negative values
C          were also included in the summed value (before averaging)
C          if the missing data code was not properly specified.
C
C   2.  The implementation of the SUBNWS option was corrected to
C       avoid using the BULKRN option using ONSITE delta-T data when
C       the ONSITE winds are missing and the reference winds are
C       based on SURFACE data. Vertical rofiles of ONSITE temperature,
C       sigma-theta, and sigma-w are also skipped if the reference
C       winds are based on SURFACE data.  These changes are intended
C       to avoid internal inconsistencies in the characterization of
C       the boundary layer that may occur with some combinations of
C       SURFACE and ONSITE data.
C
C   3.  Corrected problems with time zone adjustments for cases with
C       surface and onsite stations in different time zones.
C
C   4.  Incorporated changes to properly handle ISHD surface files
C       with the problem addressed on an interim basis by FIXISHD
C       utility, which is no longer required.
C
C-- Enhancements:
C
C   1.  Enhancements were made to the heirarchy for handling missing
C       station pressure to make full use of other available data and
C       of user-specified station elevations for SURFACE stations
C       and ONSITE stations, if available.
C
C   2.  Increased the maximum record length for the runstream input file
C       from 80 to 132 characters, removed any limit on the record length
C       for ONSITE data records, increased the maximum field length for
C       filenames from 48 to 96, and allow for use of double quotes ()
C       as field delimiters to support filenames with embedded spaces.
C
C   3.  Several enhancements to provide more flexibility in selecting
C       the most appropriate upper air sounding, including a new option
C       for a user-specified window on upper air observation time, and
C       an option for AERMET to automatically select the most appropriate
C       sounding based on local sunrise. These enhancements allow AERMET
C       to better utilize upper air data that occurs more frequently
C       than twice-daily, including "pseudo" upper air data derived from
C       gridded meteorological models or reanalysis data, if deemed to
C       be appropriate in the future (note that use of gridded model
C       outputs or reanalysis data in AERMET would be considered as
C       an alternative modeling technique at the present time).  These
C       enhancements also provide better support for applications of
C       AERMOD beyond the U.S.
C
C   4.  Enhancement to allow the use of hourly-averaged winds derived
C       from 1-minute ASOS wind data (TD-6405), generated by the new
C       AERMINUTE program, which can improve the representativeness
C       of airport wind data by providing hourly averages as well as
C       filling in for many hours that are missing in the standard
C       airport data archives due to calm or variable winds.
C
C   5.  Adjustment of ASOS-based wind speeds (including winds derived
C       from 1-minute ASOS data) by +0.5 knot to account for the bias
C       in reported ASOS wind speeds due to winds being truncated
C       (rather than rounded) to whole knots.  This includes changes
C       to several subroutines to allow tracking whether surface data
C       are ASOS or observer-based across all three stages of data
C       processing, and the inclusion of an ASOS 'flag' in the surface
C       extraction and surface QA files.  A database of ASOS commission
C       dates has been included to allow AERMET to determine whether
C       observations for a specific surface station are ASOS or not.
C       The surface output file format has also been modified to include
C       a character string for each hour identifying whether the wind
C       speeds have been adjusted or not to account for the truncation
C       of the ASOS winds ('ADJ-' or 'NAD-') and whether the reference
C       wind data is based on ONSITE, SURFACE or 1-MIN ASOS data (OS,
C       SFC, or A1).
C
C   6.  Several enhancements to the error handling and reporting related
C       to processing ONSITE data, including an option to use FREE
C       format to read the data and the option to specify missing data
C       codes and upper/lower bounds for ONSITE data as REAL variables.
C
C   7.  A new option/requirement incorporated in Stage 3 to specify a
C       secondary set of surface characteristics for use when NWS winds
C       are substituted for missing on-site winds using the SUBNWS option
C       on the METPREP pathway.  The primary set of surface characteristics
C       is used when the reference wind is based on ONSITE data, and the
C       secondary surface characteristics are used when SURFACE data are
C       substituted for missing ONSITE data.  This ensures that an
C       appropriate surface roughness will be used depending on the
C       location of the reference wind data.
C
C   8.  New keywords, AERSURF and AERSURF2, were added to the METPREP
C       pathway to allow users to specify an external file to read
C       for surface characteristics, such as an AERSURFACE output file,
C       rather than entering the data directly in the Stage 3 input
C       file.
C
C   9.  Enhancements to utilize on-site precipitation and relative
C       humidity data, if available, in the surface output file.
C
C-- Miscellaneous:
C
C   1.  The LOCATION keyword is no longer supported on the METPREP
C       pathway in Stage 3.  The METPREP LOCATION keyword had been used
C       as the location for determining sunrise for convective mixing
C       height calculations.  This could result in somewhat different
C       results using the same raw input data depending on the METPREP
C       LOCATION, which are not meaningful.  A non-fatal warning message
C       is issued if the METPREP LOCATION keyword is found, and the
C       location used to determine sunrise for convective mixing height
C       calculations is based on the primary surface station location,
C       i.e., the ONSITE station if available, or the SURFACE (NWS)
C       station location.
C
C   2.  Modified several subroutines to adjust the lower limit on the
C       value of surface roughness length to 0.0001 meters, rather than
C       0.001 meters.  This allows more flexibility for applications of
C       AERMET for overwater or other very low roughness settings.
C
C   3.  Several subroutines were modified to improve error handling and
C       reporting for all three processing stages, including adjustments
C       and improvements to the order and formatting of information in
C       the report file.
C
C   4.  Implemented some restrictions on the use of surface (NWS) data
C       outside the valid range of dates based on the surface data format,
C       i.e., data that have been reformatted from another file format.
C       These restrictions affect cloud cover for ASOS data in the SCRAM
C       or SAMSON format, since the SAMSON CDs and nearly all SCRAM surface
C       data predated the advent of ASOS in 1992.  The ASOS cloud covers
C       are set to missing for any SURFACE data input in the SCRAM or
C       SAMSON format, since the interpretation and formatting of ASOS
C       cloud cover codes cannot be verified in these cases.
C
C
C-----  MODIFIED BY:    Roger W. Brode
C                       U.S. EPA, OAQPS/AQAD
C                       Air Quality Modeling Group
C
C                       February 28, 2011
C
C-----  MODIFIED FROM:          AERMET
C                       (Version Dated 06341)
C
C=======================================================================
C                                 AERMET
C        Meteorological Preprocessor for the AERMOD Dispersion Model
C
C     PURPOSE:  Controls the overall flow of AERMET processing through
C               Stages 1, 2 and 3.
C
C               There are four data types that can be processed
C               through AERMET:
C
C                  UPPERAIR - Upper air soundings
C
C                  SURFACE  - Hourly surface weather observations
C
C                  ASOS1MIN - Hourly-averaged winds derived from
C                             1-minute ASOS data (TD-6405), processed
C                             by the AERMINUTE processor
C
C                  ONSITE   - User supplied on-site meteorological data
C
C                In Stage 1, the UPPERAIR and SURFACE data are extracted
C                from a standard archive-formatted file and the quality
C                of the data are assessed; also in this stage, the
C                quality of the ONSITE data are assessed, if provided
C                by the user. There is no standard archive format for
C                on-site data, hence, there is no data extraction.
C
C                In Stage 2, the data input in Stage 1 are combined,
C                or merged, into 24-hour blocks of data.
C
C                In Stage 3, the merged data from Stage 2 are processed
C                to determine boundary layer scaling parameters, compute
C                hourly mixing heights, and output hourly data formatted
C                for input to the AERMOD dispersion model.  Stage 3 output
c                consists of a "surface" file, with scaling paramters,
C                reference wind and temperature data, hourly mixing
C                heights, and other parameters that may be used with
C                the deposition algorithms in AERMOD; and a "profile"
C                file, with vertical profiles of wind, temperature, and
C                turbulence data collected from an onsite tower, if
C                available.
C
C     Initial Release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        10/18/96
C          - added the write statement to display processing setup and
C            processing completed messages
C
C        06/27/99
C          - added capability to process upper air data retrieved from
C            Radiosonde Data of North America compact disc (aka "FSL
C            data")
C
C        10/26/06
C          - A single executable (AERMET) was created, replacing
C            STAGE1N2 and STAGE3.
C
C        05/29/08: MACTEC Federal Programs
C          - Commented out opening unit 3 (the console or screen) all
C            references to unit 3 in the code have been changed to
C            * (default output)
C-----------------------------------------------------------------------


      BLOCK DATA

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
      INCLUDE 'BLOCK1.INC'
      INCLUDE 'BLOCK2.INC'

      END
