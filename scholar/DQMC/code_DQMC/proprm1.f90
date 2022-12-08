      SUBROUTINE PROPRM1(AIN,NT)

        Use Blockc
        Use Block_obs
        Implicit Real (KIND=8) (A-G,O-Z)
        Implicit Integer (H-N)
        
        Interface
           SUBROUTINE MMTHLM1(A)
             COMPLEX (Kind=8), Dimension(:,:) :: A 
           END SUBROUTINE MMTHLM1
           SUBROUTINE MMUULM1(A, NF, NTAU,  NFLAG)
             COMPLEX (KIND=8), Dimension(:,:) :: A
             Integer :: NF, NTAU, NFLAG
           END SUBROUTINE MMUULM1
           SUBROUTINE MMUULM1P(A, NF, NTAU,  NFLAG)
             COMPLEX (KIND=8), Dimension(:,:) :: A
             Integer :: NF, NTAU, NFLAG
           END SUBROUTINE MMUULM1P
        End Interface

        !Arguments.
        COMPLEX (Kind=8), Dimension(:,:) :: AIN
        Integer  :: NT


        CALL MMTHLM1(AIN)
        if (RJ > Zero)    then
           DO NF = 1,NFAM
              NFLAG = 2
              CALL MMUULM1(AIN,NF,NT,NFLAG)
              NFLAG = 1
              CALL MMUULM1(AIN,NF,NT,NFLAG)
           ENDDO
        endif
        NFLAG = 3   !Hubbard
        CALL MMUULM1(AIN,NF,NT,NFLAG)


      RETURN	
    END SUBROUTINE PROPRM1
