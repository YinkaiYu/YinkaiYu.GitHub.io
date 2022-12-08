      SUBROUTINE PROPR(AIN,NT)

        Use Blockc
        Use Block_obs
        Implicit Real (KIND=8) (A-G,O-Z)
        Implicit Integer (H-N)
        
        Interface
           SUBROUTINE  MMTHR(A) 
             COMPLEX (Kind=8), Dimension(:,:) :: A
           END SUBROUTINE MMTHR
           SUBROUTINE  MMUUR(A, NF, NT, NFLAG) 
             COMPLEX (Kind=8), Dimension(:,:) :: A 
             INTEGER :: NF, NT, NFLAG
           END SUBROUTINE MMUUR
           SUBROUTINE  MMUURP(A, NF, NT, NFLAG) 
             COMPLEX (Kind=8), Dimension(:,:) :: A 
             INTEGER :: NF, NT, NFLAG
           END SUBROUTINE MMUURP
        END Interface
        
        COMPLEX (Kind=8), Dimension(:,:) :: AIN
        Integer :: NT


        CALL MMTHR(AIN)
        if (RJ > Zero)    then 
           DO NF = 1,NFAM
              NFLAG = 2
              CALL MMUUR(AIN, NF, NT, NFLAG) 
              NFLAG = 1
              CALL MMUUR(AIN, NF, NT, NFLAG) 
           ENDDO
        endif
        NFLAG = 3                 ! Hubbard
        CALL MMUUR(AIN, NF, NT, NFLAG) 
        
        RETURN
      END SUBROUTINE PROPR


      
