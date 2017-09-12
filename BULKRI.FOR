      SUBROUTINE BULKRI(IHR, ANGLE, ACRIT, DELT, T1, Z1, Z2)            ! dtb125 02107
C=====================================================================**
C
C     BULKRI Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Calculate USTAR and THSTAR using a Bulk Richardson Number
C               approach
C
C     Input:
C
C        IHR       Integer   Hour of day
C        ANGLE     Real      Solar elevation angle
C        ACRIT     Real      Critical solar elevation angle for conv (netrad=0)
C        WSPD      Real      Wind speed at reference height
C        ZO        Real      Surface roughness length
C        DELT      Real      Vertical temperature difference (T2-T1)
C        Z1        Real      Lower level for Delta-T
C        Z2        Real      Upper level for Delta-T
C        T1        Real      Temperature at Z1
C
C     Output:
C
C        USTAR     Real      Surface friction velocity
C
C        PTG       Real      Potential temperature gradient
c                            (output to error file for debug purposes only)
C
C     Initial release: January 2001
C
C     Revision history:
C        07/23/2004 (MACTEC/PES)
C          - modified to use profile method for estimating u* and L     ! rwb400 04205
C
C     Called by:   MPPBL
C
C     Note the equation numbers indicated in the right hand margins of
C     several records, refer to the equations documented in the memorandum
C     from Jim Clary and Associates, 'Code Changes to AERMET' dated
C     January 11 2001.
C
C-----------------------------------------------------------------------

C     VONK   = von Karman constant
C     GRAV   = acceleration due to gravity
C     BETAM  = constant used for profile relationships in the SBL
C     EPS    = convergence criterion (1% here) for Monin-Obukov length
C     DELT   = vertical temperature difference (T2-T1)
C     T1     = Temp at level 1
C     Z1     = Level 1 Height
C     Z2     = Level 2 Height

      IMPLICIT NONE

      INTEGER  IHR, JJJ, ITER, IMIN, J, K, counter, rpt

      REAL  THSTR1, CDN, VONK, GRAV, YYY
      REAL  BETAM, Z1, Z2, EPS, LASTU, DTDZ
      REAL  A, B, T1, USTAR1, CTHETA, DELT, PTG
      REAL  ACRIT, ANGLE, ALPHA, DTHETA, LASTOBU, OBU, LASTTH

      REAL alpha1, beta1, gamma1, z_on_l, z0_on_L, term, term0
      REAL UST(20),THST(20),LST(20),Z_over_L(20), Utemp, Ltemp,THtemp   ! CDS   082509

C --- Add values for adjusting if CHEK > 1
      REAL CHEK, UNOT, UCR, USTCR
      REAL USTARMax

      REAL tempOBU 
      REAL tempU 
      REAL tempTH

      LOGICAL test                                                      ! CDS   082509


      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

      DATA VONK/0.4/,  GRAV/9.80655/

      PATH  = 'METPREP'
      LOC   = 'BULKRI'

      rpt = 0
      z_on_L = 0.0

      ALPHA = 1.0                                                       ! rwb400 04205
      BETAM = 5.0

      test = .true.                                                     ! CDS   082509
!!!------------------ Luhar et al

      alpha1 = 4.0
      beta1  = 0.5
      gamma1 = 0.3

      EPS     = 0.01   
      LASTU   = 0.0
      LASTOBU = 0.0                                                     ! rwb400 04205
      LASTTH  = 0.0                                                     ! rwb400 04205 
      ITER    = 0                                                       ! rwb400 04205   

      USTARMax = -99.0

      JJJ = MPYR*10000 + MPCMO*100 + MPCDY

      DTDZ = DELT/(Z2-Z1)            ! temperature gradient
      PTG = DTDZ + 0.0098            ! potential temperature gradient
      DTHETA = DELT + (Z2-Z1)*0.0098 ! potential temperature difference ! rwb400 04205

      IF( WSPD(IHR) .EQ. 0.0 )THEN                    !  Calm hour
         THSTAR(IHR) = -9.
         USTAR(IHR)  = -9.
         MOL(IHR) = -99999.
         RETURN                                                         ! rwb400 04205

      ELSEIF( ABS(WSPD(IHR)-OSQA(23,2)) .LT. 0.01 )THEN   !  Missing wind speed
         THSTAR(IHR) =  -9.
         USTAR(IHR)  =  -9.
         MOL(IHR)    = -99999.
         MESS =  BLNK80
         ECODE='I87'
         WRITE(MESS,490) IHR
         CALL ERRHDL(JJJ,PATH,ECODE,LOC,MESS)
         RETURN                                                         ! rwb400 04205
 
      ELSE

         IF( DTHETA .LE. 0.0 )THEN               ! Negative DTHETA      ! rwb400 04205
            DTHETA = 0.0                                                ! rwb400 04205
            
C           Use neutral limit for USTAR and low value for THSTAR to drive MOL to limit

            USTAR(IHR)  = VONK*WSPD(IHR)/(ALOG(ZREF(IHR)/Z0(IHR)))      ! rwb400 04205
            THSTAR(IHR) = 0.00001
            MOL(IHR)    = 8888.                                    
            RETURN

         ENDIF

         CTheta = 1.0/(ALPHA*ALOG(Z2/Z1))                    !  Eq. (2)
         B      = BETAM * (Z2 - Z1) * VONK * GRAV / T1       !  Eq. (3)
         A      = BETAM * Z2 * GRAV / T1                     !  Eq. (4)

         IF( ADJ_USTAR )THEN
            CDN    = VONK /(ALOG( (ZREF(IHR)-BETAM*Z0(IHR))/
     &                                             Z0(IHR))) 
         ELSE
            CDN    = VONK /(ALOG( ZREF(IHR)/Z0(IHR)))        !  Eq. (5)
         ENDIF

C ---    Assign initial value of OBU = ZREF/2
         OBU = ZREF(IHR)/2.0
         THSTR1 = VONK*DTHETA/(ALOG(Z2/Z1)+BETAM*(Z2-Z1)/OBU)

C ---    Adjust THSTR1 for low solar angles 
         IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                   ! jsi033 01138

C           Correct TSTAR1 for low solar elevation; see Holtslag(1984), ! jsi031 01114
c           BLM(29):225-350  Equation A-11                              ! jsi031 01114
            THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                    ! jsi031 01114

         ENDIF                                                         ! jsi033 01138

C        Calculate U* for u < Uc
         USTAR1 = SQRT(CDN*A*THSTR1)                         !  Eq. (6)

C        Revise initial guess for L                                  ! rwb400 04205
         OBU = USTAR1*USTAR1*T1/(VONK*GRAV*THSTR1)                   ! rwb400 04205

! Save the initial guesses
         tempOBU = OBU                                               ! CDS   082509
         tempU = USTAR1                                              ! CDS   082509
         tempTH= THSTR1                                              ! CDS   082509


         IMIN = 3               ! Minimum of 3 iterations            ! rwb400 04205
         DO WHILE((ABS(LASTOBU-OBU)   .GT. ABS(EPS*OBU) .OR.         ! rwb400 04205
     &             ABS(LASTU-USTAR1)  .GT. ABS(EPS*USTAR1) .OR.      ! rwb400 04205
     &             ABS(LASTTH-THSTR1) .GT. ABS(EPS*THSTR1) .OR.      ! rwb400 04205
C ---              Set lower limit of 1.0m for OBU to avoid NaN's
     &             IMIN .GE. 1) .AND. OBU .GE. 1.0 .AND.             ! rwb400 16216
     &             ITER .LT. 20)    ! Maximum of 20 iterations       ! rwb400 04205

            IMIN = MAX( 0, IMIN - 1)                                 ! rwb400 04205
            ITER = ITER+1                                            ! rwb400 04205
            LASTOBU = OBU                                            ! rwb400 04205
            LASTU = USTAR1                                           ! rwb400 04205
            LASTTH= THSTR1                                           ! rwb400 04205

            IF( ADJ_USTAR )THEN
! LUHAR
               z_on_l = ZREF(IHR)/OBU
!
! MODIFIED: Override the current scheme with the LUHAR method
!
C ---          Use 0.7*OBU, cutoff based on p. 450 of Luhar and Raynor, BLM (2009)
               IF (ZREF(IHR) .GT. 0.7*OBU) THEN
C ---             Use Eq. 22 for "low wind conditions" for z/L > 0.7*OBU
                  z0_on_l=z0(IHR)/obu
                  term=alpha1*z_on_l**beta1*(1.0+gamma1*z_on_l**
     &                                                      (1.0-beta1))
                  term0=alpha1*z0_on_l**beta1*(1.0+gamma1*z0_on_l**
     &                                                      (1.0-beta1))
                  ustar1= vonk*wspd(IHR)/(term-term0) !! <<< FULL version of EQN 22 in Luhar et al, 2009
!                 ustar1= vonk*wspd(IHR)/(term)       !! <<< approximate or "simple" version of EQN 22
               ELSE
C ---             Use "original" method 
                  USTAR1=VONK*WSPD(IHR)/(ALOG((ZREF(IHR)-BETAM*z0(IHR))/
     &                                            z0(IHR))+BETAM*z_on_l)
               END IF

               THSTR1 = VONK*DTHETA/(ALOG(Z2/Z1)+BETAM*(Z2-Z1)/OBU)

C ---          Adjust THSTR1 for low solar angles 
               IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                ! jsi033 01138

C                 Correct TSTAR1 for low solar elevation; see Holtslag     ! jsi031 01114
c                 (1984), BLM(29):225-350  Equation A-11                   ! jsi031 01114
                  THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                 ! jsi031 01114

               ENDIF                                                      ! jsi033 01138      

C              Calculate new guess for L                                 ! rwb400 04205
               OBU = USTAR1*USTAR1*T1/(VONK*GRAV*THSTR1)                 ! rwb400 04205

! Save the variables to the appropriate arrays
               LST(ITER) = OBU                                           ! CDS   082509
               UST(ITER) = USTAR1                                        ! CDS   082509
               THST(ITER) = THSTR1                                       ! CDS   082509
               z_over_L(ITER) = z_on_l                                   ! CDS   082509

            ELSE   ! No ADJ_U*

               USTAR1 = VONK*WSPD(IHR)/(ALOG(ZREF(IHR)/Z0(IHR))+     
     &                  BETAM*ZREF(IHR)/OBU)                         

               IF (ZREF(IHR) .GT. 0.5*OBU) THEN                      
                  USTAR1 = VONK*WSPD(IHR)/(ALOG(ZREF(IHR)/Z0(IHR)) + 
     &              7.0*ALOG(ZREF(IHR)/OBU) + 4.25/(ZREF(IHR)/OBU) - 
     &              0.5/((ZREF(IHR)/OBU)*(ZREF(IHR)/OBU)) +          
     &              (BETAM/2.)-1.648)                                 
               ENDIF                                                 

               THSTR1 = VONK*DTHETA/(ALOG(Z2/Z1)+BETAM*(Z2-Z1)/OBU)

               IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                ! jsi033 01138

C                 Correct TSTAR1 for low solar elevation; see Holtslag     ! jsi031 01114
c                 (1984), BLM(29):225-350  Equation A-11                   ! jsi031 01114
                  THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                 ! jsi031 01114

               ENDIF                                                      ! jsi033 01138      

            ENDIF

C           Calculate new guess for L                                 ! rwb400 04205
            OBU = USTAR1*USTAR1*T1/(VONK*GRAV*THSTR1)                 ! rwb400 04205

         ENDDO

         THSTR1 = VONK*DTHETA/(ALOG(Z2/Z1)+BETAM*(Z2-Z1)/OBU)

C ---     Adjust THSTR1 for low solar angles 
          IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                ! jsi033 01138

C            Correct TSTAR1 for low solar elevation; see Holtslag     ! jsi031 01114
c            (1984), BLM(29):225-350  Equation A-11                   ! jsi031 01114
             THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                 ! jsi031 01114

          ENDIF                                                      ! jsi033 01138      

      END IF

      IF( ADJ_USTAR )THEN
!
! Below are the modifications to the Luhar Method to replace            ! CDS   082509
!      [1] Diverging values
!      [2] Oscillating Non-converging values
!      [3] Anomalously high Converged values

! Both [1] and [3] are replaced with the NON-ITERATING METHOD
!
! [2] is replaced with an average of values for the iterations (after
!      the 13th iteration) in which z/L < 0.4
!

         if(ITER.EQ.20)then  ! Check the NON-CONVERGED values
         	 ! The following condition assumes z/L > 99.0 implies diverging values
         	 if(z_over_L(20).GT.99.0)then

            ! For the diverging values, using the NON-ITERATIVE METHOD
               OBU = tempOBU
               USTAR1 = tempU
               THSTR1 = tempTH

               z_on_l = zref(IHR)/OBU

C ---          Add code for Eq. 22 08/11/2014
               z0_on_l=z0(IHR)/obu
               term=alpha1*z_on_l**beta1*(1.0+gamma1*z_on_l
     &                                                    **(1.0-beta1))
               term0=alpha1*z0_on_l**beta1*(1.0+gamma1*z0_on_l
     &                                                    **(1.0-beta1))
               ustar1= vonk*wspd(IHR)/(term-term0) !! <<< FULL version of EQN 22 in Luhar et al, 2009
!               ustar1= vonk*wspd(IHR)/(term) !! <<< approximate or "simple" version of EQN 22

               THSTR1 = VONK*DTHETA/(ALOG(Z2/Z1)+BETAM*(Z2-Z1)/OBU)

C ---          Apply upper limit on THSTAR baesd on Eq. 23 of Luhar and Raynor (2009);
C              first calculate MAX value of ustar based on Eq. 25 of Luhar and Raynor
               USTARMax = 0.6*(ZREF(IHR)/
     &                   (ALOG(ZREF(IHR)/z0(IHR))))**0.33333

               THSTR1 = MIN( THSTR1, 
     &           0.1*(1.0 - (MIN((2.*USTAR1)/USTARMax,1.0) - 1.0)**2) )

               IF( USTARMax .LE. USTAR1 )THEN
                  USTAR1 = USTARMax
               ENDIF

C ---          Adjust THSTR1 for low solar angles 
               IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                ! jsi033 01138

C                 Correct TSTAR1 for low solar elevation; see Holtslag     ! jsi031 01114
c                 (1984), BLM(29):225-350  Equation A-11                   ! jsi031 01114
                  THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                 ! jsi031 01114

               ENDIF                                                      ! jsi033 01138      

               OBU = USTAR1*USTAR1*T1/(VONK*GRAV*THSTR1)

               z_on_l = zref(IHR)/obu

            else
              K = 0
           ! The following loop detects the ping-pong pattern
             DO WHILE (test.EQV..true.)
             	K = K + 1

             if(K.LT.20)then
C ---          Check for convergence, but only if K < 20 to avoid 
C              array subscript out-of-bounds
               if(ABS(z_over_L(20)-z_over_L(20-K)).LT.0.0001)then
         	  	   rpt = K
         	  	   test = .false.
         	     endif
             else
C ---         Convergence not reached; apply the divergent solution
            ! For the diverging values, using the NON-ITERATIVE METHOD
         	   OBU = tempOBU
               USTAR1 = tempU
               THSTR1 = tempTH

               z_on_l = zref(IHR)/OBU

C ---             add code for Eq. 22 08/11/2014
                  z0_on_l=z0(IHR)/obu
                  term=alpha1*z_on_l**beta1*(1.0+gamma1*z_on_l**
     &             (1.0-beta1))
                  term0=alpha1*z0_on_l**beta1*(1.0+gamma1*z0_on_l
     &                 **(1.0-beta1))
                  ustar1= vonk*wspd(IHR)/(term-term0) !! <<< FULL version of EQN 22 in Luhar et al, 2009
!                  ustar1= vonk*wspd(IHR)/(term) !! <<< approximate or "simple" version of EQN 22

               THSTR1 = VONK*DTHETA/(ALOG(Z2/Z1)+BETAM*(Z2-Z1)/OBU)

C ---          Apply upper limit on THSTAR baesd on Eq. 23 of Luhar and Raynor (2009);
C              first calculate MAX value of ustar based on Eq. 25 of Luhar and Raynor
               USTARMax = 0.6*(ZREF(IHR)/
     &                   (ALOG(ZREF(IHR)/z0(IHR))))**0.33333

               THSTR1 = MIN( THSTR1, 
     &           0.1*(1.0 - (MIN((2.*USTAR1)/USTARMax,1.0) - 1.0)**2) )

               IF( USTARMax .LE. USTAR1 )THEN
                  USTAR1 = USTARMax
               ENDIF


C ---          Adjust THSTR1 for low solar angles 
               IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                ! jsi033 01138

C                 Correct TSTAR1 for low solar elevation; see Holtslag     ! jsi031 01114
c                 (1984), BLM(29):225-350  Equation A-11                   ! jsi031 01114
                  THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                 ! jsi031 01114

               ENDIF                                                      ! jsi033 01138      

               OBU = USTAR1*USTAR1*T1/(VONK*GRAV*THSTR1)

               z_on_l = zref(IHR)/obu
C ---          Exit from the DO WHILE loop
               exit

              endif
            ENDDO


           ! Initialize variables
         	    ITER = ITER+1
         	    Utemp = 0.0
         	    THtemp = 0.0
         	    Ltemp = 0.0
         	    counter = 0

           ! if there is a looping pattern
         	   if(rpt.GT.0)then

           ! The following loop finds all values within the ping-pong cycle that
           ! meets the criteria of z/L < 0.4
 
         	     DO J = 1,rpt
C ---           Modify z_over_L limit from 0.4 to 0.7, based on Luhar & Raynor (2009)
         	      if(z_over_L(ITER-J).LT.0.7)then
         	      	Utemp = Utemp + UST(ITER-J)
         	      	THtemp = THtemp + THST(ITER-J)
         	      	Ltemp = Ltemp + LST(ITER-J)
         	      	counter = counter + 1
         	      endif
         	     ENDDO

           ! The averages of all the variables with z/L < 0.7 are saved to the main variables
         	       USTAR1 = Utemp / REAL(counter)
         	       THSTR1 = THtemp / REAL(counter)
         	       OBU = Ltemp / REAL(counter)
         	       z_on_L = zref(IHR)/OBU
         	       !ITER = 99
         	   endif

           endif

         elseif(ITER.LT.20)then ! Check the CONVERGED values
          ! The following takes care of the bifurcating converged values at low wind speed
          ! The LUHAR METHOD is replaced by the NON-ITERATING METHOD
C --        Replace 0.4 with 0.7, based on p. 450 of Luhar and Raynor, BLM (2009)
         	  if(z_on_l .GT. 0.7)then ! For Converged z/L > 0.7

            	  OBU = tempOBU
            	  USTAR1 = tempU
            	  THSTR1 = tempTH

            	  z_on_l = zref(IHR)/OBU

C ---          add code for Eq. 22 08/11/2014
               z0_on_l=z0(IHR)/obu
               term=alpha1*z_on_l**beta1*(1.0+gamma1*z_on_l**
     &                                                   (1.0-beta1))
               term0=alpha1*z0_on_l**beta1*(1.0+gamma1*z0_on_l**
     &                                                   (1.0-beta1))
               ustar1= vonk*wspd(IHR)/(term-term0) !! <<< FULL version of EQN 22 in Luhar et al, 2009
!               ustar1= vonk*wspd(IHR)/(term) !! <<< approximate or "simple" version of EQN 22

               THSTR1 = VONK*DTHETA/(ALOG(Z2/Z1)+BETAM*(Z2-Z1)/OBU)

C ---          Apply upper limit on THSTAR baesd on Eq. 23 of Luhar and Raynor (2009);
C              first calculate MAX value of ustar based on Eq. 25 of Luhar and Raynor
               USTARMax = 0.6*(ZREF(IHR)/
     &                   (ALOG(ZREF(IHR)/z0(IHR))))**0.33333

               THSTR1 = MIN( THSTR1, 
     &           0.1*(1.0 - (MIN((2.*USTAR1)/USTARMax,1.0) - 1.0)**2) )

               IF( USTARMax .LE. USTAR1 )THEN
                  USTAR1 = USTARMax
               ENDIF

C ---          Adjust THSTR1 for low solar angles 
               IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                ! jsi033 01138

C                 Correct TSTAR1 for low solar elevation; see Holtslag     ! jsi031 01114
c                 (1984), BLM(29):225-350  Equation A-11                   ! jsi031 01114
                  THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                 ! jsi031 01114

               ENDIF                                                      ! jsi033 01138      

               OBU = USTAR1*USTAR1*T1/(VONK*GRAV*THSTR1)

               z_on_l = zref(IHR)/obu

               THSTR1 = VONK*DTHETA/(ALOG(Z2/Z1)+BETAM*(Z2-Z1)/OBU)

C ---          Adjust THSTR1 for low solar angles 
               IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                ! jsi033 01138

C                 Correct TSTAR1 for low solar elevation; see Holtslag     ! jsi031 01114
c                 (1984), BLM(29):225-350  Equation A-11                   ! jsi031 01114
                  THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                 ! jsi031 01114

               ENDIF                                                      ! jsi033 01138      

               OBU = USTAR1*USTAR1*T1/(VONK*GRAV*THSTR1)

               z_on_l = zref(IHR)/obu

            endif
         endif

      ENDIF

      THSTAR(IHR) = THSTR1
      USTAR(IHR)  = USTAR1
      MOL(IHR)    = OBU                                              ! rwb400 04205

  505 RETURN

  490 FORMAT(' Missing wind speed for hour: ', I2.2)
  491 FORMAT(' DTHETA amd PTG are: ', 2F8.3, ' for Hr: ',I2)            ! dtb134 02240
  492 FORMAT(' No solution for u* and L for SBL for hour: ',I2.2)

      END

