      SUBROUTINE SPROJ(DEGEN,EN_FREE)
        !Sets projector.
        Use Blockc
        Use MyMats
        Implicit Real (KIND=8) (A-G,O-Z)
        Implicit Integer (H-N)
!#define DEC
        Interface
           Subroutine SetHproj(HLP2)
             Complex (Kind=8), Dimension(:,:) :: HLP2
           end Subroutine SetHproj
        end Interface
        INCLUDE 'mpif.h' 
        INTEGER STATUS(MPI_STATUS_SIZE)
        COMPLEX (Kind=8), Dimension(:,:), Allocatable :: TMP
        real(Kind=8), Dimension(:), Allocatable :: WC
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,ISIZE,IERR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,IRANK,IERR)
        Allocate(TMP(Ndim,Ndim), WC(Ndim))
        PROJ = CMPLX(0.d0,0.d0)
        TMP = CMPLX(0.d0,0.d0)
        Call SetHproj(TMP)
        CALL Diag(TMP,PROJ,WC)
        en_free = 0.d0
        do i = 1,Ne
           en_free = en_free + wc(i)
        enddo
        en_free = en_free*dble(N_SUN)
        DEGEN = WC(NE+1) - WC(NE)
        IF (IRANK == 0) WRITE(50,*) 'Degen: ', DEGEN

        Deallocate(TMP)
        Deallocate(WC)
 RETURN
      END SUBROUTINE SPROJ
