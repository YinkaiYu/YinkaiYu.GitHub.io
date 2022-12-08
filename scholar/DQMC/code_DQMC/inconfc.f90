       SUBROUTINE INCONFC(ISEED)
         Use blockc
         Implicit Real (KIND=8) (A-G,O-Z)
         Implicit Integer (H-N)
!#define DEC
         INCLUDE 'mpif.h'
         ! Local
         INTEGER STATUS(MPI_STATUS_SIZE)
         INTEGER, Dimension(:,:), Allocatable :: ITMPU
         INTEGER, Dimension(:,:,:), Allocatable :: ITMPK
         CALL MPI_COMM_SIZE(MPI_COMM_WORLD,ISIZE,IERR)
         CALL MPI_COMM_RANK(MPI_COMM_WORLD,IRANK,IERR)
         Allocate (ITMPU(NDIM,LTROT), ITMPK(LQ,Nfam,LTROT))
         IF (IRANK .EQ. 0 ) THEN
            OPEN (UNIT=30,FILE='confin', STATUS='UNKNOWN')
            OPEN (UNIT=10,FILE='seeds', STATUS='UNKNOWN')
         ENDIF
         IF ( IRANK.EQ.0 ) THEN
            WRITE(6,*) 'Number of nodes', ISIZE
            READ(30,*) ISEED
            IF (ISEED.EQ.0) THEN
               READ(10,*) ISEED0
               do N = 1,ISIZE - 1
                  !Setup node I and send data.
                  READ(10,*) ITMP
                  do NT = 1,LTROT
                     do I = 1,LQ
                        do nf = 1,Nfam
                           X = RANF(ITMP)
                           NSIGL_K(I,nf,NT) = 1
                           IF (X.GT.0.5) NSIGL_K(I,nf,NT) = -1
                        enddo
                     enddo
                     do I = 1,NDIM
                        X = RANF(ITMP)
                        NSIGL_U(I,NT) = 1
                        IF (X.GT.0.5) NSIGL_U(I,NT) = -1
                     enddo
                  enddo
                  call MPI_SEND(ITMP, 1,MPI_INTEGER, N, &
                       & N,MPI_COMM_WORLD,IERR)
                  call MPI_SEND(NSIGL_U, Ndim*LTROT, MPI_INTEGER, N, &
                       & N+512, MPI_COMM_WORLD,IERR)
                  call MPI_SEND(NSIGL_K, Nfam*LQ*LTROT, MPI_INTEGER, N, &
                       & N+1024, MPI_COMM_WORLD, IERR)
               enddo
               ! Set node zero.
               ISEED = ISEED0
               do NT = 1,LTROT
                  do I = 1,LQ
                     do nf = 1, Nfam
                        X = RANF(ISEED)
                        NSIGL_K(I,nf,NT) = 1
                        if (X.GT.0.5) NSIGL_K(I,nf,NT) = -1
                     enddo
                  enddo
                  do i = 1,NDIM
                     X = RANF(ISEED)
                     NSIGL_U(I,NT) = 1
                     if (X.GT.0.5) NSIGL_U(I,NT) = -1
                  enddo
               enddo
            else
               ! Read all confins from NODE 0.
               ! Setup Node 0
               do NT = 1,LTROT
                  do i = 1,NDIM
                     Read(30,*) NSIGL_U(I,NT)
                  enddo
                  do i = 1,LQ
                      do nf = 1, NFAM
                          Read(30,*) NSIGL_K(I,nf,NT)
                      enddo
                  enddo
               enddo
               do N = 1,ISIZE - 1
                  read(30,*) ITMP
                  call MPI_SEND(ITMP, 1,MPI_INTEGER, N, &
                       & N,MPI_COMM_WORLD,IERR)
                  do NT = 1,LTROT
                     do i = 1,NDIM
                        Read(30,*) ITMPU(I,NT)
                     enddo
                     do I = 1,LQ
                         do nf = 1,Nfam
                             READ(30,*) ITMPK(I,nf,NT)
                         enddo
                     enddo
                  enddo
                  call MPI_SEND(ITMPU,NDIM*LTROT,MPI_INTEGER, N, &
                       & N+512,MPI_COMM_WORLD,IERR)
                  call MPI_SEND(ITMPK, Nfam*LQ*LTROT,MPI_INTEGER, N, &
                      & N+1024,MPI_COMM_WORLD,IERR)
               enddo
            endif
         else
            call MPI_RECV(ISEED, 1,MPI_INTEGER,0, &
                 & IRANK , MPI_COMM_WORLD,STATUS,IERR)
            call MPI_RECV(NSIGL_U, NDIM*LTROT, MPI_INTEGER,0, &
                 & IRANK + 512, MPI_COMM_WORLD,STATUS,IERR)
            call MPI_RECV(NSIGL_K, Nfam*LQ*LTROT, MPI_INTEGER,0, &
                 & IRANK + 1024, MPI_COMM_WORLD,STATUS,IERR)
         endif
         if (IRANK .EQ. 0 ) then
            CLOSE(30); CLOSE(10)
         endif
         Deallocate (ITMPU, ITMPK)
         RETURN
       END SUBROUTINE INCONFC
