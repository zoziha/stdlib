submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_svd_drivers
  implicit none


  contains

     module subroutine stdlib_sgesvd( jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt,work, lwork, info )
     !! SGESVD computes the singular value decomposition (SVD) of a real
     !! M-by-N matrix A, optionally computing the left and/or right singular
     !! vectors. The SVD is written
     !! A = U * SIGMA * transpose(V)
     !! where SIGMA is an M-by-N matrix which is zero except for its
     !! min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
     !! V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
     !! are the singular values of A; they are real and non-negative, and
     !! are returned in descending order.  The first min(m,n) columns of
     !! U and V are the left and right singular vectors of A.
     !! Note that the routine returns V**T, not V.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu, jobvt
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: s(*), u(ldu,*), vt(ldvt,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wntua, wntuas, wntun, wntuo, wntus, wntva, wntvas, wntvn, wntvo,&
                      wntvs
           integer(ilp) :: bdspac, blk, chunk, i, ie, ierr, ir, iscl, itau, itaup, itauq, iu, &
           iwork, ldwrkr, ldwrku, maxwrk, minmn, minwrk, mnthr, ncu, ncvt, nru, nrvt, &
                     wrkbl
           integer(ilp) :: lwork_sgeqrf, lwork_sorgqr_n, lwork_sorgqr_m, lwork_sgebrd, &
                     lwork_sorgbr_p, lwork_sorgbr_q, lwork_sgelqf, lwork_sorglq_n, lwork_sorglq_m
           real(sp) :: anrm, bignum, eps, smlnum
           ! Local Arrays 
           real(sp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           minmn = min( m, n )
           wntua = stdlib_lsame( jobu, 'A' )
           wntus = stdlib_lsame( jobu, 'S' )
           wntuas = wntua .or. wntus
           wntuo = stdlib_lsame( jobu, 'O' )
           wntun = stdlib_lsame( jobu, 'N' )
           wntva = stdlib_lsame( jobvt, 'A' )
           wntvs = stdlib_lsame( jobvt, 'S' )
           wntvas = wntva .or. wntvs
           wntvo = stdlib_lsame( jobvt, 'O' )
           wntvn = stdlib_lsame( jobvt, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.( wntua .or. wntus .or. wntuo .or. wntun ) ) then
              info = -1_ilp
           else if( .not.( wntva .or. wntvs .or. wntvo .or. wntvn ) .or.( wntvo .and. wntuo ) ) &
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldu<1_ilp .or. ( wntuas .and. ldu<m ) ) then
              info = -9_ilp
           else if( ldvt<1_ilp .or. ( wntva .and. ldvt<n ) .or.( wntvs .and. ldvt<minmn ) ) &
                     then
              info = -11_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              minwrk = 1_ilp
              maxwrk = 1_ilp
              if( m>=n .and. minmn>0_ilp ) then
                 ! compute space needed for stdlib_sbdsqr
                 mnthr = stdlib_ilaenv( 6_ilp, 'SGESVD', jobu // jobvt, m, n, 0_ilp, 0_ilp )
                 bdspac = 5_ilp*n
                 ! compute space needed for stdlib_sgeqrf
                 call stdlib_sgeqrf( m, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_sgeqrf = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_sorgqr
                 call stdlib_sorgqr( m, n, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_sorgqr_n = int( dum(1_ilp),KIND=ilp)
                 call stdlib_sorgqr( m, m, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_sorgqr_m = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_sgebrd
                 call stdlib_sgebrd( n, n, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                           
                 lwork_sgebrd = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_sorgbr p
                 call stdlib_sorgbr( 'P', n, n, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                 lwork_sorgbr_p = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_sorgbr q
                 call stdlib_sorgbr( 'Q', n, n, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                 lwork_sorgbr_q = int( dum(1_ilp),KIND=ilp)
                 if( m>=mnthr ) then
                    if( wntun ) then
                       ! path 1 (m much larger than n, jobu='n')
                       maxwrk = n + lwork_sgeqrf
                       maxwrk = max( maxwrk, 3_ilp*n+lwork_sgebrd )
                       if( wntvo .or. wntvas )maxwrk = max( maxwrk, 3_ilp*n+lwork_sorgbr_p )
                       maxwrk = max( maxwrk, bdspac )
                       minwrk = max( 4_ilp*n, bdspac )
                    else if( wntuo .and. wntvn ) then
                       ! path 2 (m much larger than n, jobu='o', jobvt='n')
                       wrkbl = n + lwork_sgeqrf
                       wrkbl = max( wrkbl, n+lwork_sorgqr_n )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = max( n*n+wrkbl, n*n+m*n+n )
                       minwrk = max( 3_ilp*n+m, bdspac )
                    else if( wntuo .and. wntvas ) then
                       ! path 3 (m much larger than n, jobu='o', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_sgeqrf
                       wrkbl = max( wrkbl, n+lwork_sorgqr_n )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = max( n*n+wrkbl, n*n+m*n+n )
                       minwrk = max( 3_ilp*n+m, bdspac )
                    else if( wntus .and. wntvn ) then
                       ! path 4 (m much larger than n, jobu='s', jobvt='n')
                       wrkbl = n + lwork_sgeqrf
                       wrkbl = max( wrkbl, n+lwork_sorgqr_n )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = n*n + wrkbl
                       minwrk = max( 3_ilp*n+m, bdspac )
                    else if( wntus .and. wntvo ) then
                       ! path 5 (m much larger than n, jobu='s', jobvt='o')
                       wrkbl = n + lwork_sgeqrf
                       wrkbl = max( wrkbl, n+lwork_sorgqr_n )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = 2_ilp*n*n + wrkbl
                       minwrk = max( 3_ilp*n+m, bdspac )
                    else if( wntus .and. wntvas ) then
                       ! path 6 (m much larger than n, jobu='s', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_sgeqrf
                       wrkbl = max( wrkbl, n+lwork_sorgqr_n )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = n*n + wrkbl
                       minwrk = max( 3_ilp*n+m, bdspac )
                    else if( wntua .and. wntvn ) then
                       ! path 7 (m much larger than n, jobu='a', jobvt='n')
                       wrkbl = n + lwork_sgeqrf
                       wrkbl = max( wrkbl, n+lwork_sorgqr_m )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = n*n + wrkbl
                       minwrk = max( 3_ilp*n+m, bdspac )
                    else if( wntua .and. wntvo ) then
                       ! path 8 (m much larger than n, jobu='a', jobvt='o')
                       wrkbl = n + lwork_sgeqrf
                       wrkbl = max( wrkbl, n+lwork_sorgqr_m )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = 2_ilp*n*n + wrkbl
                       minwrk = max( 3_ilp*n+m, bdspac )
                    else if( wntua .and. wntvas ) then
                       ! path 9 (m much larger than n, jobu='a', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_sgeqrf
                       wrkbl = max( wrkbl, n+lwork_sorgqr_m )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, 3_ilp*n+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = n*n + wrkbl
                       minwrk = max( 3_ilp*n+m, bdspac )
                    end if
                 else
                    ! path 10 (m at least n, but not much larger)
                    call stdlib_sgebrd( m, n, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                              
                    lwork_sgebrd = int( dum(1_ilp),KIND=ilp)
                    maxwrk = 3_ilp*n + lwork_sgebrd
                    if( wntus .or. wntuo ) then
                       call stdlib_sorgbr( 'Q', m, n, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                       lwork_sorgbr_q = int( dum(1_ilp),KIND=ilp)
                       maxwrk = max( maxwrk, 3_ilp*n+lwork_sorgbr_q )
                    end if
                    if( wntua ) then
                       call stdlib_sorgbr( 'Q', m, m, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                       lwork_sorgbr_q = int( dum(1_ilp),KIND=ilp)
                       maxwrk = max( maxwrk, 3_ilp*n+lwork_sorgbr_q )
                    end if
                    if( .not.wntvn ) then
                      maxwrk = max( maxwrk, 3_ilp*n+lwork_sorgbr_p )
                    end if
                    maxwrk = max( maxwrk, bdspac )
                    minwrk = max( 3_ilp*n+m, bdspac )
                 end if
              else if( minmn>0_ilp ) then
                 ! compute space needed for stdlib_sbdsqr
                 mnthr = stdlib_ilaenv( 6_ilp, 'SGESVD', jobu // jobvt, m, n, 0_ilp, 0_ilp )
                 bdspac = 5_ilp*m
                 ! compute space needed for stdlib_sgelqf
                 call stdlib_sgelqf( m, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_sgelqf = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_sorglq
                 call stdlib_sorglq( n, n, m, dum(1_ilp), n, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_sorglq_n = int( dum(1_ilp),KIND=ilp)
                 call stdlib_sorglq( m, n, m, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_sorglq_m = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_sgebrd
                 call stdlib_sgebrd( m, m, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                           
                 lwork_sgebrd = int( dum(1_ilp),KIND=ilp)
                  ! compute space needed for stdlib_sorgbr p
                 call stdlib_sorgbr( 'P', m, m, m, a, n, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                 lwork_sorgbr_p = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_sorgbr q
                 call stdlib_sorgbr( 'Q', m, m, m, a, n, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                 lwork_sorgbr_q = int( dum(1_ilp),KIND=ilp)
                 if( n>=mnthr ) then
                    if( wntvn ) then
                       ! path 1t(n much larger than m, jobvt='n')
                       maxwrk = m + lwork_sgelqf
                       maxwrk = max( maxwrk, 3_ilp*m+lwork_sgebrd )
                       if( wntuo .or. wntuas )maxwrk = max( maxwrk, 3_ilp*m+lwork_sorgbr_q )
                       maxwrk = max( maxwrk, bdspac )
                       minwrk = max( 4_ilp*m, bdspac )
                    else if( wntvo .and. wntun ) then
                       ! path 2t(n much larger than m, jobu='n', jobvt='o')
                       wrkbl = m + lwork_sgelqf
                       wrkbl = max( wrkbl, m+lwork_sorglq_m )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = max( m*m+wrkbl, m*m+m*n+m )
                       minwrk = max( 3_ilp*m+n, bdspac )
                    else if( wntvo .and. wntuas ) then
                       ! path 3t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='o')
                       wrkbl = m + lwork_sgelqf
                       wrkbl = max( wrkbl, m+lwork_sorglq_m )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = max( m*m+wrkbl, m*m+m*n+m )
                       minwrk = max( 3_ilp*m+n, bdspac )
                    else if( wntvs .and. wntun ) then
                       ! path 4t(n much larger than m, jobu='n', jobvt='s')
                       wrkbl = m + lwork_sgelqf
                       wrkbl = max( wrkbl, m+lwork_sorglq_m )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = m*m + wrkbl
                       minwrk = max( 3_ilp*m+n, bdspac )
                    else if( wntvs .and. wntuo ) then
                       ! path 5t(n much larger than m, jobu='o', jobvt='s')
                       wrkbl = m + lwork_sgelqf
                       wrkbl = max( wrkbl, m+lwork_sorglq_m )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = 2_ilp*m*m + wrkbl
                       minwrk = max( 3_ilp*m+n, bdspac )
                       maxwrk = max( maxwrk, minwrk )
                    else if( wntvs .and. wntuas ) then
                       ! path 6t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='s')
                       wrkbl = m + lwork_sgelqf
                       wrkbl = max( wrkbl, m+lwork_sorglq_m )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = m*m + wrkbl
                       minwrk = max( 3_ilp*m+n, bdspac )
                    else if( wntva .and. wntun ) then
                       ! path 7t(n much larger than m, jobu='n', jobvt='a')
                       wrkbl = m + lwork_sgelqf
                       wrkbl = max( wrkbl, m+lwork_sorglq_n )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = m*m + wrkbl
                       minwrk = max( 3_ilp*m+n, bdspac )
                    else if( wntva .and. wntuo ) then
                       ! path 8t(n much larger than m, jobu='o', jobvt='a')
                       wrkbl = m + lwork_sgelqf
                       wrkbl = max( wrkbl, m+lwork_sorglq_n )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = 2_ilp*m*m + wrkbl
                       minwrk = max( 3_ilp*m+n, bdspac )
                    else if( wntva .and. wntuas ) then
                       ! path 9t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='a')
                       wrkbl = m + lwork_sgelqf
                       wrkbl = max( wrkbl, m+lwork_sorglq_n )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_p )
                       wrkbl = max( wrkbl, 3_ilp*m+lwork_sorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = m*m + wrkbl
                       minwrk = max( 3_ilp*m+n, bdspac )
                    end if
                 else
                    ! path 10t(n greater than m, but not much larger)
                    call stdlib_sgebrd( m, n, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                              
                    lwork_sgebrd = int( dum(1_ilp),KIND=ilp)
                    maxwrk = 3_ilp*m + lwork_sgebrd
                    if( wntvs .or. wntvo ) then
                      ! compute space needed for stdlib_sorgbr p
                      call stdlib_sorgbr( 'P', m, n, m, a, n, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                      lwork_sorgbr_p = int( dum(1_ilp),KIND=ilp)
                      maxwrk = max( maxwrk, 3_ilp*m+lwork_sorgbr_p )
                    end if
                    if( wntva ) then
                      call stdlib_sorgbr( 'P', n, n, m, a, n, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                      lwork_sorgbr_p = int( dum(1_ilp),KIND=ilp)
                      maxwrk = max( maxwrk, 3_ilp*m+lwork_sorgbr_p )
                    end if
                    if( .not.wntun ) then
                       maxwrk = max( maxwrk, 3_ilp*m+lwork_sorgbr_q )
                    end if
                    maxwrk = max( maxwrk, bdspac )
                    minwrk = max( 3_ilp*m+n, bdspac )
                 end if
              end if
              maxwrk = max( maxwrk, minwrk )
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGESVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = sqrt( stdlib_slamch( 'S' ) ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_slange( 'M', m, n, a, lda, dum )
           iscl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              iscl = 1_ilp
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, ierr )
           else if( anrm>bignum ) then
              iscl = 1_ilp
              call stdlib_slascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, ierr )
           end if
           if( m>=n ) then
              ! a has at least as many rows as columns. if a has sufficiently
              ! more rows than columns, first reduce using the qr
              ! decomposition (if sufficient workspace available)
              if( m>=mnthr ) then
                 if( wntun ) then
                    ! path 1 (m much larger than n, jobu='n')
                    ! no left singular vectors to be computed
                    itau = 1_ilp
                    iwork = itau + n
                    ! compute a=q*r
                    ! (workspace: need 2*n, prefer n+n*nb)
                    call stdlib_sgeqrf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                              ierr )
                    ! zero out below r
                    if( n > 1_ilp ) then
                       call stdlib_slaset( 'L', n-1, n-1, zero, zero, a( 2_ilp, 1_ilp ),lda )
                    end if
                    ie = 1_ilp
                    itauq = ie + n
                    itaup = itauq + n
                    iwork = itaup + n
                    ! bidiagonalize r in a
                    ! (workspace: need 4*n, prefer 3*n+2*n*nb)
                    call stdlib_sgebrd( n, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), &
                              work( iwork ), lwork-iwork+1,ierr )
                    ncvt = 0_ilp
                    if( wntvo .or. wntvas ) then
                       ! if right singular vectors desired, generate p'.
                       ! (workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
                       call stdlib_sorgbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       ncvt = n
                    end if
                    iwork = ie + n
                    ! perform bidiagonal qr iteration, computing right
                    ! singular vectors of a in a if desired
                    ! (workspace: need bdspac)
                    call stdlib_sbdsqr( 'U', n, ncvt, 0_ilp, 0_ilp, s, work( ie ), a, lda,dum, 1_ilp, dum, 1_ilp, &
                              work( iwork ), info )
                    ! if right singular vectors desired in vt, copy them there
                    if( wntvas )call stdlib_slacpy( 'F', n, n, a, lda, vt, ldvt )
                 else if( wntuo .and. wntvn ) then
                    ! path 2 (m much larger than n, jobu='o', jobvt='n')
                    ! n left singular vectors to be overwritten on a and
                    ! no right singular vectors to be computed
                    if( lwork>=n*n+max( 4_ilp*n, bdspac ) ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n+n )+lda*n ) then
                          ! work(iu) is lda by n, work(ir) is lda by n
                          ldwrku = lda
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n+n )+n*n ) then
                          ! work(iu) is lda by n, work(ir) is n by n
                          ldwrku = lda
                          ldwrkr = n
                       else
                          ! work(iu) is ldwrku by n, work(ir) is n by n
                          ldwrku = ( lwork-n*n-n ) / n
                          ldwrkr = n
                       end if
                       itau = ir + ldwrkr*n
                       iwork = itau + n
                       ! compute a=q*r
                       ! (workspace: need n*n+2*n, prefer n*n+n+n*nb)
                       call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to work(ir) and zero out below it
                       call stdlib_slacpy( 'U', n, n, a, lda, work( ir ), ldwrkr )
                       call stdlib_slaset( 'L', n-1, n-1, zero, zero, work( ir+1 ),ldwrkr )
                                 
                       ! generate q in a
                       ! (workspace: need n*n+2*n, prefer n*n+n+n*nb)
                       call stdlib_sorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + n
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in work(ir)
                       ! (workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
                       call stdlib_sgebrd( n, n, work( ir ), ldwrkr, s, work( ie ),work( itauq ), &
                                 work( itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing r
                       ! (workspace: need n*n+4*n, prefer n*n+3*n+n*nb)
                       call stdlib_sorgbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       iwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of r in work(ir)
                       ! (workspace: need n*n+bdspac)
                       call stdlib_sbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, work( ie ), dum, 1_ilp,work( ir ), &
                                 ldwrkr, dum, 1_ilp,work( iwork ), info )
                       iu = ie + n
                       ! multiply q in a by left singular vectors of r in
                       ! work(ir), storing result in work(iu) and copying to a
                       ! (workspace: need n*n+2*n, prefer n*n+m*n+n)
                       do i = 1, m, ldwrku
                          chunk = min( m-i+1, ldwrku )
                          call stdlib_sgemm( 'N', 'N', chunk, n, n, one, a( i, 1_ilp ),lda, work( ir )&
                                    , ldwrkr, zero,work( iu ), ldwrku )
                          call stdlib_slacpy( 'F', chunk, n, work( iu ), ldwrku,a( i, 1_ilp ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       ie = 1_ilp
                       itauq = ie + n
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize a
                       ! (workspace: need 3*n+m, prefer 3*n+(m+n)*nb)
                       call stdlib_sgebrd( m, n, a, lda, s, work( ie ),work( itauq ), work( itaup &
                                 ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing a
                       ! (workspace: need 4*n, prefer 3*n+n*nb)
                       call stdlib_sorgbr( 'Q', m, n, n, a, lda, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in a
                       ! (workspace: need bdspac)
                       call stdlib_sbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, work( ie ), dum, 1_ilp,a, lda, dum, 1_ilp, &
                                 work( iwork ), info )
                    end if
                 else if( wntuo .and. wntvas ) then
                    ! path 3 (m much larger than n, jobu='o', jobvt='s' or 'a')
                    ! n left singular vectors to be overwritten on a and
                    ! n right singular vectors to be computed in vt
                    if( lwork>=n*n+max( 4_ilp*n, bdspac ) ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n+n )+lda*n ) then
                          ! work(iu) is lda by n and work(ir) is lda by n
                          ldwrku = lda
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n+n )+n*n ) then
                          ! work(iu) is lda by n and work(ir) is n by n
                          ldwrku = lda
                          ldwrkr = n
                       else
                          ! work(iu) is ldwrku by n and work(ir) is n by n
                          ldwrku = ( lwork-n*n-n ) / n
                          ldwrkr = n
                       end if
                       itau = ir + ldwrkr*n
                       iwork = itau + n
                       ! compute a=q*r
                       ! (workspace: need n*n+2*n, prefer n*n+n+n*nb)
                       call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to vt, zeroing out below it
                       call stdlib_slacpy( 'U', n, n, a, lda, vt, ldvt )
                       if( n>1_ilp )call stdlib_slaset( 'L', n-1, n-1, zero, zero,vt( 2_ilp, 1_ilp ), ldvt )
                                 
                       ! generate q in a
                       ! (workspace: need n*n+2*n, prefer n*n+n+n*nb)
                       call stdlib_sorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + n
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in vt, copying result to work(ir)
                       ! (workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
                       call stdlib_sgebrd( n, n, vt, ldvt, s, work( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       call stdlib_slacpy( 'L', n, n, vt, ldvt, work( ir ), ldwrkr )
                       ! generate left vectors bidiagonalizing r in work(ir)
                       ! (workspace: need n*n+4*n, prefer n*n+3*n+n*nb)
                       call stdlib_sorgbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing r in vt
                       ! (workspace: need n*n+4*n-1, prefer n*n+3*n+(n-1)*nb)
                       call stdlib_sorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of r in work(ir) and computing right
                       ! singular vectors of r in vt
                       ! (workspace: need n*n+bdspac)
                       call stdlib_sbdsqr( 'U', n, n, n, 0_ilp, s, work( ie ), vt, ldvt,work( ir ), &
                                 ldwrkr, dum, 1_ilp,work( iwork ), info )
                       iu = ie + n
                       ! multiply q in a by left singular vectors of r in
                       ! work(ir), storing result in work(iu) and copying to a
                       ! (workspace: need n*n+2*n, prefer n*n+m*n+n)
                       do i = 1, m, ldwrku
                          chunk = min( m-i+1, ldwrku )
                          call stdlib_sgemm( 'N', 'N', chunk, n, n, one, a( i, 1_ilp ),lda, work( ir )&
                                    , ldwrkr, zero,work( iu ), ldwrku )
                          call stdlib_slacpy( 'F', chunk, n, work( iu ), ldwrku,a( i, 1_ilp ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       itau = 1_ilp
                       iwork = itau + n
                       ! compute a=q*r
                       ! (workspace: need 2*n, prefer n+n*nb)
                       call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to vt, zeroing out below it
                       call stdlib_slacpy( 'U', n, n, a, lda, vt, ldvt )
                       if( n>1_ilp )call stdlib_slaset( 'L', n-1, n-1, zero, zero,vt( 2_ilp, 1_ilp ), ldvt )
                                 
                       ! generate q in a
                       ! (workspace: need 2*n, prefer n+n*nb)
                       call stdlib_sorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + n
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in vt
                       ! (workspace: need 4*n, prefer 3*n+2*n*nb)
                       call stdlib_sgebrd( n, n, vt, ldvt, s, work( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! multiply q in a by left vectors bidiagonalizing r
                       ! (workspace: need 3*n+m, prefer 3*n+m*nb)
                       call stdlib_sormbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), a, lda,&
                                  work( iwork ),lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing r in vt
                       ! (workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
                       call stdlib_sorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in a and computing right
                       ! singular vectors of a in vt
                       ! (workspace: need bdspac)
                       call stdlib_sbdsqr( 'U', n, n, m, 0_ilp, s, work( ie ), vt, ldvt,a, lda, dum, &
                                 1_ilp, work( iwork ), info )
                    end if
                 else if( wntus ) then
                    if( wntvn ) then
                       ! path 4 (m much larger than n, jobu='s', jobvt='n')
                       ! n left singular vectors to be computed in u and
                       ! no right singular vectors to be computed
                       if( lwork>=n*n+max( 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(ir) is lda by n
                             ldwrkr = lda
                          else
                             ! work(ir) is n by n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (workspace: need n*n+2*n, prefer n*n+n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(ir), zeroing out below it
                          call stdlib_slacpy( 'U', n, n, a, lda, work( ir ),ldwrkr )
                          call stdlib_slaset( 'L', n-1, n-1, zero, zero,work( ir+1 ), ldwrkr )
                                    
                          ! generate q in a
                          ! (workspace: need n*n+2*n, prefer n*n+n+n*nb)
                          call stdlib_sorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(ir)
                          ! (workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, work( ir ), ldwrkr, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate left vectors bidiagonalizing r in work(ir)
                          ! (workspace: need n*n+4*n, prefer n*n+3*n+n*nb)
                          call stdlib_sorgbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(ir)
                          ! (workspace: need n*n+bdspac)
                          call stdlib_sbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, work( ie ), dum,1_ilp, work( ir ), &
                                    ldwrkr, dum, 1_ilp,work( iwork ), info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(ir), storing result in u
                          ! (workspace: need n*n)
                          call stdlib_sgemm( 'N', 'N', m, n, n, one, a, lda,work( ir ), ldwrkr, &
                                    zero, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need 2*n, prefer n+n*nb)
                          call stdlib_sorgqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_slaset( 'L', n-1, n-1, zero, zero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (workspace: need 4*n, prefer 3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left vectors bidiagonalizing r
                          ! (workspace: need 3*n+m, prefer 3*n+m*nb)
                          call stdlib_sormbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, work( ie ), dum,1_ilp, u, ldu, dum, &
                                    1_ilp, work( iwork ),info )
                       end if
                    else if( wntvo ) then
                       ! path 5 (m much larger than n, jobu='s', jobvt='o')
                       ! n left singular vectors to be computed in u and
                       ! n right singular vectors to be overwritten on a
                       if( lwork>=2_ilp*n*n+max( 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*n ) then
                             ! work(iu) is lda by n and work(ir) is lda by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+n )*n ) then
                             ! work(iu) is lda by n and work(ir) is n by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          else
                             ! work(iu) is n by n and work(ir) is n by n
                             ldwrku = n
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (workspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_slacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_slaset( 'L', n-1, n-1, zero, zero,work( iu+1 ), ldwrku )
                                    
                          ! generate q in a
                          ! (workspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
                          call stdlib_sorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to
                          ! work(ir)
                          ! (workspace: need 2*n*n+4*n,
                                      ! prefer 2*n*n+3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_slacpy( 'U', n, n, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (workspace: need 2*n*n+4*n, prefer 2*n*n+3*n+n*nb)
                          call stdlib_sorgbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (workspace: need 2*n*n+4*n-1,
                                      ! prefer 2*n*n+3*n+(n-1)*nb)
                          call stdlib_sorgbr( 'P', n, n, n, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in work(ir)
                          ! (workspace: need 2*n*n+bdspac)
                          call stdlib_sbdsqr( 'U', n, n, n, 0_ilp, s, work( ie ),work( ir ), ldwrkr, &
                                    work( iu ),ldwrku, dum, 1_ilp, work( iwork ), info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(iu), storing result in u
                          ! (workspace: need n*n)
                          call stdlib_sgemm( 'N', 'N', m, n, n, one, a, lda,work( iu ), ldwrku, &
                                    zero, u, ldu )
                          ! copy right singular vectors of r to a
                          ! (workspace: need n*n)
                          call stdlib_slacpy( 'F', n, n, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need 2*n, prefer n+n*nb)
                          call stdlib_sorgqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_slaset( 'L', n-1, n-1, zero, zero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (workspace: need 4*n, prefer 3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left vectors bidiagonalizing r
                          ! (workspace: need 3*n+m, prefer 3*n+m*nb)
                          call stdlib_sormbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right vectors bidiagonalizing r in a
                          ! (workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
                          call stdlib_sorgbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in a
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', n, n, m, 0_ilp, s, work( ie ), a,lda, u, ldu, dum, &
                                    1_ilp, work( iwork ),info )
                       end if
                    else if( wntvas ) then
                       ! path 6 (m much larger than n, jobu='s', jobvt='s'
                               ! or 'a')
                       ! n left singular vectors to be computed in u and
                       ! n right singular vectors to be computed in vt
                       if( lwork>=n*n+max( 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is n by n
                             ldwrku = n
                          end if
                          itau = iu + ldwrku*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (workspace: need n*n+2*n, prefer n*n+n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_slacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_slaset( 'L', n-1, n-1, zero, zero,work( iu+1 ), ldwrku )
                                    
                          ! generate q in a
                          ! (workspace: need n*n+2*n, prefer n*n+n+n*nb)
                          call stdlib_sorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to vt
                          ! (workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_slacpy( 'U', n, n, work( iu ), ldwrku, vt,ldvt )
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (workspace: need n*n+4*n, prefer n*n+3*n+n*nb)
                          call stdlib_sorgbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (workspace: need n*n+4*n-1,
                                      ! prefer n*n+3*n+(n-1)*nb)
                          call stdlib_sorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in vt
                          ! (workspace: need n*n+bdspac)
                          call stdlib_sbdsqr( 'U', n, n, n, 0_ilp, s, work( ie ), vt,ldvt, work( iu ),&
                                     ldwrku, dum, 1_ilp,work( iwork ), info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(iu), storing result in u
                          ! (workspace: need n*n)
                          call stdlib_sgemm( 'N', 'N', m, n, n, one, a, lda,work( iu ), ldwrku, &
                                    zero, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need 2*n, prefer n+n*nb)
                          call stdlib_sorgqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to vt, zeroing out below it
                          call stdlib_slacpy( 'U', n, n, a, lda, vt, ldvt )
                          if( n>1_ilp )call stdlib_slaset( 'L', n-1, n-1, zero, zero,vt( 2_ilp, 1_ilp ), ldvt &
                                    )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in vt
                          ! (workspace: need 4*n, prefer 3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, vt, ldvt, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in vt
                          ! (workspace: need 3*n+m, prefer 3*n+m*nb)
                          call stdlib_sormbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
                          call stdlib_sorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', n, n, m, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    end if
                 else if( wntua ) then
                    if( wntvn ) then
                       ! path 7 (m much larger than n, jobu='a', jobvt='n')
                       ! m left singular vectors to be computed in u and
                       ! no right singular vectors to be computed
                       if( lwork>=n*n+max( n+m, 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(ir) is lda by n
                             ldwrkr = lda
                          else
                             ! work(ir) is n by n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need n*n+2*n, prefer n*n+n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, n, a, lda, u, ldu )
                          ! copy r to work(ir), zeroing out below it
                          call stdlib_slacpy( 'U', n, n, a, lda, work( ir ),ldwrkr )
                          call stdlib_slaset( 'L', n-1, n-1, zero, zero,work( ir+1 ), ldwrkr )
                                    
                          ! generate q in u
                          ! (workspace: need n*n+n+m, prefer n*n+n+m*nb)
                          call stdlib_sorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(ir)
                          ! (workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, work( ir ), ldwrkr, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (workspace: need n*n+4*n, prefer n*n+3*n+n*nb)
                          call stdlib_sorgbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(ir)
                          ! (workspace: need n*n+bdspac)
                          call stdlib_sbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, work( ie ), dum,1_ilp, work( ir ), &
                                    ldwrkr, dum, 1_ilp,work( iwork ), info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(ir), storing result in a
                          ! (workspace: need n*n)
                          call stdlib_sgemm( 'N', 'N', m, n, n, one, u, ldu,work( ir ), ldwrkr, &
                                    zero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_slacpy( 'F', m, n, a, lda, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need n+m, prefer n+m*nb)
                          call stdlib_sorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_slaset( 'L', n-1, n-1, zero, zero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (workspace: need 4*n, prefer 3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in a
                          ! (workspace: need 3*n+m, prefer 3*n+m*nb)
                          call stdlib_sormbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, work( ie ), dum,1_ilp, u, ldu, dum, &
                                    1_ilp, work( iwork ),info )
                       end if
                    else if( wntvo ) then
                       ! path 8 (m much larger than n, jobu='a', jobvt='o')
                       ! m left singular vectors to be computed in u and
                       ! n right singular vectors to be overwritten on a
                       if( lwork>=2_ilp*n*n+max( n+m, 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*n ) then
                             ! work(iu) is lda by n and work(ir) is lda by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+n )*n ) then
                             ! work(iu) is lda by n and work(ir) is n by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          else
                             ! work(iu) is n by n and work(ir) is n by n
                             ldwrku = n
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need 2*n*n+n+m, prefer 2*n*n+n+m*nb)
                          call stdlib_sorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_slacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_slaset( 'L', n-1, n-1, zero, zero,work( iu+1 ), ldwrku )
                                    
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to
                          ! work(ir)
                          ! (workspace: need 2*n*n+4*n,
                                      ! prefer 2*n*n+3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_slacpy( 'U', n, n, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (workspace: need 2*n*n+4*n, prefer 2*n*n+3*n+n*nb)
                          call stdlib_sorgbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (workspace: need 2*n*n+4*n-1,
                                      ! prefer 2*n*n+3*n+(n-1)*nb)
                          call stdlib_sorgbr( 'P', n, n, n, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in work(ir)
                          ! (workspace: need 2*n*n+bdspac)
                          call stdlib_sbdsqr( 'U', n, n, n, 0_ilp, s, work( ie ),work( ir ), ldwrkr, &
                                    work( iu ),ldwrku, dum, 1_ilp, work( iwork ), info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(iu), storing result in a
                          ! (workspace: need n*n)
                          call stdlib_sgemm( 'N', 'N', m, n, n, one, u, ldu,work( iu ), ldwrku, &
                                    zero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_slacpy( 'F', m, n, a, lda, u, ldu )
                          ! copy right singular vectors of r from work(ir) to a
                          call stdlib_slacpy( 'F', n, n, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need n+m, prefer n+m*nb)
                          call stdlib_sorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_slaset( 'L', n-1, n-1, zero, zero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (workspace: need 4*n, prefer 3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in a
                          ! (workspace: need 3*n+m, prefer 3*n+m*nb)
                          call stdlib_sormbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in a
                          ! (workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
                          call stdlib_sorgbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in a
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', n, n, m, 0_ilp, s, work( ie ), a,lda, u, ldu, dum, &
                                    1_ilp, work( iwork ),info )
                       end if
                    else if( wntvas ) then
                       ! path 9 (m much larger than n, jobu='a', jobvt='s'
                               ! or 'a')
                       ! m left singular vectors to be computed in u and
                       ! n right singular vectors to be computed in vt
                       if( lwork>=n*n+max( n+m, 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is n by n
                             ldwrku = n
                          end if
                          itau = iu + ldwrku*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need n*n+2*n, prefer n*n+n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need n*n+n+m, prefer n*n+n+m*nb)
                          call stdlib_sorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_slacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_slaset( 'L', n-1, n-1, zero, zero,work( iu+1 ), ldwrku )
                                    
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to vt
                          ! (workspace: need n*n+4*n, prefer n*n+3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_slacpy( 'U', n, n, work( iu ), ldwrku, vt,ldvt )
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (workspace: need n*n+4*n, prefer n*n+3*n+n*nb)
                          call stdlib_sorgbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (workspace: need n*n+4*n-1,
                                      ! prefer n*n+3*n+(n-1)*nb)
                          call stdlib_sorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in vt
                          ! (workspace: need n*n+bdspac)
                          call stdlib_sbdsqr( 'U', n, n, n, 0_ilp, s, work( ie ), vt,ldvt, work( iu ),&
                                     ldwrku, dum, 1_ilp,work( iwork ), info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(iu), storing result in a
                          ! (workspace: need n*n)
                          call stdlib_sgemm( 'N', 'N', m, n, n, one, u, ldu,work( iu ), ldwrku, &
                                    zero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_slacpy( 'F', m, n, a, lda, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n+n*nb)
                          call stdlib_sgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need n+m, prefer n+m*nb)
                          call stdlib_sorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r from a to vt, zeroing out below it
                          call stdlib_slacpy( 'U', n, n, a, lda, vt, ldvt )
                          if( n>1_ilp )call stdlib_slaset( 'L', n-1, n-1, zero, zero,vt( 2_ilp, 1_ilp ), ldvt &
                                    )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in vt
                          ! (workspace: need 4*n, prefer 3*n+2*n*nb)
                          call stdlib_sgebrd( n, n, vt, ldvt, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in vt
                          ! (workspace: need 3*n+m, prefer 3*n+m*nb)
                          call stdlib_sormbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
                          call stdlib_sorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', n, n, m, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    end if
                 end if
              else
                 ! m < mnthr
                 ! path 10 (m at least n, but not much larger)
                 ! reduce to bidiagonal form without qr decomposition
                 ie = 1_ilp
                 itauq = ie + n
                 itaup = itauq + n
                 iwork = itaup + n
                 ! bidiagonalize a
                 ! (workspace: need 3*n+m, prefer 3*n+(m+n)*nb)
                 call stdlib_sgebrd( m, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), &
                           work( iwork ), lwork-iwork+1,ierr )
                 if( wntuas ) then
                    ! if left singular vectors desired in u, copy result to u
                    ! and generate left bidiagonalizing vectors in u
                    ! (workspace: need 3*n+ncu, prefer 3*n+ncu*nb)
                    call stdlib_slacpy( 'L', m, n, a, lda, u, ldu )
                    if( wntus )ncu = n
                    if( wntua )ncu = m
                    call stdlib_sorgbr( 'Q', m, ncu, n, u, ldu, work( itauq ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntvas ) then
                    ! if right singular vectors desired in vt, copy result to
                    ! vt and generate right bidiagonalizing vectors in vt
                    ! (workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
                    call stdlib_slacpy( 'U', n, n, a, lda, vt, ldvt )
                    call stdlib_sorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntuo ) then
                    ! if left singular vectors desired in a, generate left
                    ! bidiagonalizing vectors in a
                    ! (workspace: need 4*n, prefer 3*n+n*nb)
                    call stdlib_sorgbr( 'Q', m, n, n, a, lda, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvo ) then
                    ! if right singular vectors desired in a, generate right
                    ! bidiagonalizing vectors in a
                    ! (workspace: need 4*n-1, prefer 3*n+(n-1)*nb)
                    call stdlib_sorgbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 iwork = ie + n
                 if( wntuas .or. wntuo )nru = m
                 if( wntun )nru = 0_ilp
                 if( wntvas .or. wntvo )ncvt = n
                 if( wntvn )ncvt = 0_ilp
                 if( ( .not.wntuo ) .and. ( .not.wntvo ) ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in vt
                    ! (workspace: need bdspac)
                    call stdlib_sbdsqr( 'U', n, ncvt, nru, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, dum,&
                               1_ilp, work( iwork ), info )
                 else if( ( .not.wntuo ) .and. wntvo ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in a
                    ! (workspace: need bdspac)
                    call stdlib_sbdsqr( 'U', n, ncvt, nru, 0_ilp, s, work( ie ), a, lda,u, ldu, dum, &
                              1_ilp, work( iwork ), info )
                 else
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in a and computing right singular
                    ! vectors in vt
                    ! (workspace: need bdspac)
                    call stdlib_sbdsqr( 'U', n, ncvt, nru, 0_ilp, s, work( ie ), vt,ldvt, a, lda, dum,&
                               1_ilp, work( iwork ), info )
                 end if
              end if
           else
              ! a has more columns than rows. if a has sufficiently more
              ! columns than rows, first reduce using the lq decomposition (if
              ! sufficient workspace available)
              if( n>=mnthr ) then
                 if( wntvn ) then
                    ! path 1t(n much larger than m, jobvt='n')
                    ! no right singular vectors to be computed
                    itau = 1_ilp
                    iwork = itau + m
                    ! compute a=l*q
                    ! (workspace: need 2*m, prefer m+m*nb)
                    call stdlib_sgelqf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                              ierr )
                    ! zero out above l
                    if (m>1_ilp) call stdlib_slaset( 'U', m-1, m-1, zero, zero, a( 1_ilp, 2_ilp ), lda )
                    ie = 1_ilp
                    itauq = ie + m
                    itaup = itauq + m
                    iwork = itaup + m
                    ! bidiagonalize l in a
                    ! (workspace: need 4*m, prefer 3*m+2*m*nb)
                    call stdlib_sgebrd( m, m, a, lda, s, work( ie ), work( itauq ),work( itaup ), &
                              work( iwork ), lwork-iwork+1,ierr )
                    if( wntuo .or. wntuas ) then
                       ! if left singular vectors desired, generate q
                       ! (workspace: need 4*m, prefer 3*m+m*nb)
                       call stdlib_sorgbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                    end if
                    iwork = ie + m
                    nru = 0_ilp
                    if( wntuo .or. wntuas )nru = m
                    ! perform bidiagonal qr iteration, computing left singular
                    ! vectors of a in a if desired
                    ! (workspace: need bdspac)
                    call stdlib_sbdsqr( 'U', m, 0_ilp, nru, 0_ilp, s, work( ie ), dum, 1_ilp, a,lda, dum, 1_ilp, &
                              work( iwork ), info )
                    ! if left singular vectors desired in u, copy them there
                    if( wntuas )call stdlib_slacpy( 'F', m, m, a, lda, u, ldu )
                 else if( wntvo .and. wntun ) then
                    ! path 2t(n much larger than m, jobu='n', jobvt='o')
                    ! m right singular vectors to be overwritten on a and
                    ! no left singular vectors to be computed
                    if( lwork>=m*m+max( 4_ilp*m, bdspac ) ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n+m )+lda*m ) then
                          ! work(iu) is lda by n and work(ir) is lda by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n+m )+m*m ) then
                          ! work(iu) is lda by n and work(ir) is m by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = m
                       else
                          ! work(iu) is m by chunk and work(ir) is m by m
                          ldwrku = m
                          chunk = ( lwork-m*m-m ) / m
                          ldwrkr = m
                       end if
                       itau = ir + ldwrkr*m
                       iwork = itau + m
                       ! compute a=l*q
                       ! (workspace: need m*m+2*m, prefer m*m+m+m*nb)
                       call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to work(ir) and zero out above it
                       call stdlib_slacpy( 'L', m, m, a, lda, work( ir ), ldwrkr )
                       call stdlib_slaset( 'U', m-1, m-1, zero, zero,work( ir+ldwrkr ), ldwrkr )
                                 
                       ! generate q in a
                       ! (workspace: need m*m+2*m, prefer m*m+m+m*nb)
                       call stdlib_sorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + m
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in work(ir)
                       ! (workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                       call stdlib_sgebrd( m, m, work( ir ), ldwrkr, s, work( ie ),work( itauq ), &
                                 work( itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing l
                       ! (workspace: need m*m+4*m-1, prefer m*m+3*m+(m-1)*nb)
                       call stdlib_sorgbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       iwork = ie + m
                       ! perform bidiagonal qr iteration, computing right
                       ! singular vectors of l in work(ir)
                       ! (workspace: need m*m+bdspac)
                       call stdlib_sbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, work( ie ),work( ir ), ldwrkr, dum,&
                                  1_ilp, dum, 1_ilp,work( iwork ), info )
                       iu = ie + m
                       ! multiply right singular vectors of l in work(ir) by q
                       ! in a, storing result in work(iu) and copying to a
                       ! (workspace: need m*m+2*m, prefer m*m+m*n+m)
                       do i = 1, n, chunk
                          blk = min( n-i+1, chunk )
                          call stdlib_sgemm( 'N', 'N', m, blk, m, one, work( ir ),ldwrkr, a( 1_ilp, i &
                                    ), lda, zero,work( iu ), ldwrku )
                          call stdlib_slacpy( 'F', m, blk, work( iu ), ldwrku,a( 1_ilp, i ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       ie = 1_ilp
                       itauq = ie + m
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize a
                       ! (workspace: need 3*m+n, prefer 3*m+(m+n)*nb)
                       call stdlib_sgebrd( m, n, a, lda, s, work( ie ),work( itauq ), work( itaup &
                                 ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing a
                       ! (workspace: need 4*m, prefer 3*m+m*nb)
                       call stdlib_sorgbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + m
                       ! perform bidiagonal qr iteration, computing right
                       ! singular vectors of a in a
                       ! (workspace: need bdspac)
                       call stdlib_sbdsqr( 'L', m, n, 0_ilp, 0_ilp, s, work( ie ), a, lda,dum, 1_ilp, dum, 1_ilp, &
                                 work( iwork ), info )
                    end if
                 else if( wntvo .and. wntuas ) then
                    ! path 3t(n much larger than m, jobu='s' or 'a', jobvt='o')
                    ! m right singular vectors to be overwritten on a and
                    ! m left singular vectors to be computed in u
                    if( lwork>=m*m+max( 4_ilp*m, bdspac ) ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n+m )+lda*m ) then
                          ! work(iu) is lda by n and work(ir) is lda by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n+m )+m*m ) then
                          ! work(iu) is lda by n and work(ir) is m by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = m
                       else
                          ! work(iu) is m by chunk and work(ir) is m by m
                          ldwrku = m
                          chunk = ( lwork-m*m-m ) / m
                          ldwrkr = m
                       end if
                       itau = ir + ldwrkr*m
                       iwork = itau + m
                       ! compute a=l*q
                       ! (workspace: need m*m+2*m, prefer m*m+m+m*nb)
                       call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to u, zeroing about above it
                       call stdlib_slacpy( 'L', m, m, a, lda, u, ldu )
                       if (m>1_ilp) call stdlib_slaset( 'U', m-1, m-1, zero, zero, u( 1_ilp, 2_ilp ),ldu )
                       ! generate q in a
                       ! (workspace: need m*m+2*m, prefer m*m+m+m*nb)
                       call stdlib_sorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + m
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in u, copying result to work(ir)
                       ! (workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                       call stdlib_sgebrd( m, m, u, ldu, s, work( ie ),work( itauq ), work( itaup &
                                 ),work( iwork ), lwork-iwork+1, ierr )
                       call stdlib_slacpy( 'U', m, m, u, ldu, work( ir ), ldwrkr )
                       ! generate right vectors bidiagonalizing l in work(ir)
                       ! (workspace: need m*m+4*m-1, prefer m*m+3*m+(m-1)*nb)
                       call stdlib_sorgbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing l in u
                       ! (workspace: need m*m+4*m, prefer m*m+3*m+m*nb)
                       call stdlib_sorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + m
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of l in u, and computing right
                       ! singular vectors of l in work(ir)
                       ! (workspace: need m*m+bdspac)
                       call stdlib_sbdsqr( 'U', m, m, m, 0_ilp, s, work( ie ),work( ir ), ldwrkr, u, &
                                 ldu, dum, 1_ilp,work( iwork ), info )
                       iu = ie + m
                       ! multiply right singular vectors of l in work(ir) by q
                       ! in a, storing result in work(iu) and copying to a
                       ! (workspace: need m*m+2*m, prefer m*m+m*n+m))
                       do i = 1, n, chunk
                          blk = min( n-i+1, chunk )
                          call stdlib_sgemm( 'N', 'N', m, blk, m, one, work( ir ),ldwrkr, a( 1_ilp, i &
                                    ), lda, zero,work( iu ), ldwrku )
                          call stdlib_slacpy( 'F', m, blk, work( iu ), ldwrku,a( 1_ilp, i ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       itau = 1_ilp
                       iwork = itau + m
                       ! compute a=l*q
                       ! (workspace: need 2*m, prefer m+m*nb)
                       call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to u, zeroing out above it
                       call stdlib_slacpy( 'L', m, m, a, lda, u, ldu )
                       if (m>1_ilp) call stdlib_slaset( 'U', m-1, m-1, zero, zero, u( 1_ilp, 2_ilp ),ldu )
                       ! generate q in a
                       ! (workspace: need 2*m, prefer m+m*nb)
                       call stdlib_sorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + m
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in u
                       ! (workspace: need 4*m, prefer 3*m+2*m*nb)
                       call stdlib_sgebrd( m, m, u, ldu, s, work( ie ),work( itauq ), work( itaup &
                                 ),work( iwork ), lwork-iwork+1, ierr )
                       ! multiply right vectors bidiagonalizing l by q in a
                       ! (workspace: need 3*m+n, prefer 3*m+n*nb)
                       call stdlib_sormbr( 'P', 'L', 'T', m, n, m, u, ldu,work( itaup ), a, lda, &
                                 work( iwork ),lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing l in u
                       ! (workspace: need 4*m, prefer 3*m+m*nb)
                       call stdlib_sorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + m
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in u and computing right
                       ! singular vectors of a in a
                       ! (workspace: need bdspac)
                       call stdlib_sbdsqr( 'U', m, n, m, 0_ilp, s, work( ie ), a, lda,u, ldu, dum, 1_ilp, &
                                 work( iwork ), info )
                    end if
                 else if( wntvs ) then
                    if( wntun ) then
                       ! path 4t(n much larger than m, jobu='n', jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! no left singular vectors to be computed
                       if( lwork>=m*m+max( 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(ir) is lda by m
                             ldwrkr = lda
                          else
                             ! work(ir) is m by m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (workspace: need m*m+2*m, prefer m*m+m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(ir), zeroing out above it
                          call stdlib_slacpy( 'L', m, m, a, lda, work( ir ),ldwrkr )
                          call stdlib_slaset( 'U', m-1, m-1, zero, zero,work( ir+ldwrkr ), ldwrkr &
                                    )
                          ! generate q in a
                          ! (workspace: need m*m+2*m, prefer m*m+m+m*nb)
                          call stdlib_sorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(ir)
                          ! (workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, work( ir ), ldwrkr, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate right vectors bidiagonalizing l in
                          ! work(ir)
                          ! (workspace: need m*m+4*m, prefer m*m+3*m+(m-1)*nb)
                          call stdlib_sorgbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of l in work(ir)
                          ! (workspace: need m*m+bdspac)
                          call stdlib_sbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, work( ie ),work( ir ), ldwrkr, &
                                    dum, 1_ilp, dum, 1_ilp,work( iwork ), info )
                          ! multiply right singular vectors of l in work(ir) by
                          ! q in a, storing result in vt
                          ! (workspace: need m*m)
                          call stdlib_sgemm( 'N', 'N', m, n, m, one, work( ir ),ldwrkr, a, lda, &
                                    zero, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q
                          ! (workspace: need 2*m, prefer m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy result to vt
                          call stdlib_slacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need 2*m, prefer m+m*nb)
                          call stdlib_sorglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_slaset( 'U', m-1, m-1, zero, zero, a( 1_ilp, 2_ilp ),lda )
                          ! bidiagonalize l in a
                          ! (workspace: need 4*m, prefer 3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right vectors bidiagonalizing l by q in vt
                          ! (workspace: need 3*m+n, prefer 3*m+n*nb)
                          call stdlib_sormbr( 'P', 'L', 'T', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', m, n, 0_ilp, 0_ilp, s, work( ie ), vt,ldvt, dum, 1_ilp, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    else if( wntuo ) then
                       ! path 5t(n much larger than m, jobu='o', jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! m left singular vectors to be overwritten on a
                       if( lwork>=2_ilp*m*m+max( 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*m ) then
                             ! work(iu) is lda by m and work(ir) is lda by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+m )*m ) then
                             ! work(iu) is lda by m and work(ir) is m by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          else
                             ! work(iu) is m by m and work(ir) is m by m
                             ldwrku = m
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (workspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(iu), zeroing out below it
                          call stdlib_slacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_slaset( 'U', m-1, m-1, zero, zero,work( iu+ldwrku ), ldwrku &
                                    )
                          ! generate q in a
                          ! (workspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
                          call stdlib_sorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to
                          ! work(ir)
                          ! (workspace: need 2*m*m+4*m,
                                      ! prefer 2*m*m+3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, m, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (workspace: need 2*m*m+4*m-1,
                                      ! prefer 2*m*m+3*m+(m-1)*nb)
                          call stdlib_sorgbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (workspace: need 2*m*m+4*m, prefer 2*m*m+3*m+m*nb)
                          call stdlib_sorgbr( 'Q', m, m, m, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in work(ir) and computing
                          ! right singular vectors of l in work(iu)
                          ! (workspace: need 2*m*m+bdspac)
                          call stdlib_sbdsqr( 'U', m, m, m, 0_ilp, s, work( ie ),work( iu ), ldwrku, &
                                    work( ir ),ldwrkr, dum, 1_ilp, work( iwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in a, storing result in vt
                          ! (workspace: need m*m)
                          call stdlib_sgemm( 'N', 'N', m, n, m, one, work( iu ),ldwrku, a, lda, &
                                    zero, vt, ldvt )
                          ! copy left singular vectors of l to a
                          ! (workspace: need m*m)
                          call stdlib_slacpy( 'F', m, m, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m, prefer m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need 2*m, prefer m+m*nb)
                          call stdlib_sorglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_slaset( 'U', m-1, m-1, zero, zero, a( 1_ilp, 2_ilp ),lda )
                          ! bidiagonalize l in a
                          ! (workspace: need 4*m, prefer 3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right vectors bidiagonalizing l by q in vt
                          ! (workspace: need 3*m+n, prefer 3*m+n*nb)
                          call stdlib_sormbr( 'P', 'L', 'T', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors of l in a
                          ! (workspace: need 4*m, prefer 3*m+m*nb)
                          call stdlib_sorgbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, compute left
                          ! singular vectors of a in a and compute right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', m, n, m, 0_ilp, s, work( ie ), vt,ldvt, a, lda, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    else if( wntuas ) then
                       ! path 6t(n much larger than m, jobu='s' or 'a',
                               ! jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! m left singular vectors to be computed in u
                       if( lwork>=m*m+max( 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is lda by m
                             ldwrku = m
                          end if
                          itau = iu + ldwrku*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (workspace: need m*m+2*m, prefer m*m+m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_slacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_slaset( 'U', m-1, m-1, zero, zero,work( iu+ldwrku ), ldwrku &
                                    )
                          ! generate q in a
                          ! (workspace: need m*m+2*m, prefer m*m+m+m*nb)
                          call stdlib_sorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to u
                          ! (workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, m, work( iu ), ldwrku, u,ldu )
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (workspace: need m*m+4*m-1,
                                      ! prefer m*m+3*m+(m-1)*nb)
                          call stdlib_sorgbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (workspace: need m*m+4*m, prefer m*m+3*m+m*nb)
                          call stdlib_sorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in u and computing right
                          ! singular vectors of l in work(iu)
                          ! (workspace: need m*m+bdspac)
                          call stdlib_sbdsqr( 'U', m, m, m, 0_ilp, s, work( ie ),work( iu ), ldwrku, &
                                    u, ldu, dum, 1_ilp,work( iwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in a, storing result in vt
                          ! (workspace: need m*m)
                          call stdlib_sgemm( 'N', 'N', m, n, m, one, work( iu ),ldwrku, a, lda, &
                                    zero, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m, prefer m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need 2*m, prefer m+m*nb)
                          call stdlib_sorglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to u, zeroing out above it
                          call stdlib_slacpy( 'L', m, m, a, lda, u, ldu )
                          if (m>1_ilp) call stdlib_slaset( 'U', m-1, m-1, zero, zero, u( 1_ilp, 2_ilp ),ldu )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in u
                          ! (workspace: need 4*m, prefer 3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, u, ldu, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in u by q
                          ! in vt
                          ! (workspace: need 3*m+n, prefer 3*m+n*nb)
                          call stdlib_sormbr( 'P', 'L', 'T', m, n, m, u, ldu,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (workspace: need 4*m, prefer 3*m+m*nb)
                          call stdlib_sorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', m, n, m, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    end if
                 else if( wntva ) then
                    if( wntun ) then
                       ! path 7t(n much larger than m, jobu='n', jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! no left singular vectors to be computed
                       if( lwork>=m*m+max( n+m, 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(ir) is lda by m
                             ldwrkr = lda
                          else
                             ! work(ir) is m by m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need m*m+2*m, prefer m*m+m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! copy l to work(ir), zeroing out above it
                          call stdlib_slacpy( 'L', m, m, a, lda, work( ir ),ldwrkr )
                          call stdlib_slaset( 'U', m-1, m-1, zero, zero,work( ir+ldwrkr ), ldwrkr &
                                    )
                          ! generate q in vt
                          ! (workspace: need m*m+m+n, prefer m*m+m+n*nb)
                          call stdlib_sorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(ir)
                          ! (workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, work( ir ), ldwrkr, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (workspace: need m*m+4*m-1,
                                      ! prefer m*m+3*m+(m-1)*nb)
                          call stdlib_sorgbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of l in work(ir)
                          ! (workspace: need m*m+bdspac)
                          call stdlib_sbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, work( ie ),work( ir ), ldwrkr, &
                                    dum, 1_ilp, dum, 1_ilp,work( iwork ), info )
                          ! multiply right singular vectors of l in work(ir) by
                          ! q in vt, storing result in a
                          ! (workspace: need m*m)
                          call stdlib_sgemm( 'N', 'N', m, n, m, one, work( ir ),ldwrkr, vt, ldvt, &
                                    zero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_slacpy( 'F', m, n, a, lda, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m, prefer m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need m+n, prefer m+n*nb)
                          call stdlib_sorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_slaset( 'U', m-1, m-1, zero, zero, a( 1_ilp, 2_ilp ),lda )
                          ! bidiagonalize l in a
                          ! (workspace: need 4*m, prefer 3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in a by q
                          ! in vt
                          ! (workspace: need 3*m+n, prefer 3*m+n*nb)
                          call stdlib_sormbr( 'P', 'L', 'T', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', m, n, 0_ilp, 0_ilp, s, work( ie ), vt,ldvt, dum, 1_ilp, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    else if( wntuo ) then
                       ! path 8t(n much larger than m, jobu='o', jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! m left singular vectors to be overwritten on a
                       if( lwork>=2_ilp*m*m+max( n+m, 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*m ) then
                             ! work(iu) is lda by m and work(ir) is lda by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+m )*m ) then
                             ! work(iu) is lda by m and work(ir) is m by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          else
                             ! work(iu) is m by m and work(ir) is m by m
                             ldwrku = m
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need 2*m*m+m+n, prefer 2*m*m+m+n*nb)
                          call stdlib_sorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_slacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_slaset( 'U', m-1, m-1, zero, zero,work( iu+ldwrku ), ldwrku &
                                    )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to
                          ! work(ir)
                          ! (workspace: need 2*m*m+4*m,
                                      ! prefer 2*m*m+3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, m, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (workspace: need 2*m*m+4*m-1,
                                      ! prefer 2*m*m+3*m+(m-1)*nb)
                          call stdlib_sorgbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (workspace: need 2*m*m+4*m, prefer 2*m*m+3*m+m*nb)
                          call stdlib_sorgbr( 'Q', m, m, m, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in work(ir) and computing
                          ! right singular vectors of l in work(iu)
                          ! (workspace: need 2*m*m+bdspac)
                          call stdlib_sbdsqr( 'U', m, m, m, 0_ilp, s, work( ie ),work( iu ), ldwrku, &
                                    work( ir ),ldwrkr, dum, 1_ilp, work( iwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in vt, storing result in a
                          ! (workspace: need m*m)
                          call stdlib_sgemm( 'N', 'N', m, n, m, one, work( iu ),ldwrku, vt, ldvt, &
                                    zero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_slacpy( 'F', m, n, a, lda, vt, ldvt )
                          ! copy left singular vectors of a from work(ir) to a
                          call stdlib_slacpy( 'F', m, m, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m, prefer m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need m+n, prefer m+n*nb)
                          call stdlib_sorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_slaset( 'U', m-1, m-1, zero, zero, a( 1_ilp, 2_ilp ),lda )
                          ! bidiagonalize l in a
                          ! (workspace: need 4*m, prefer 3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in a by q
                          ! in vt
                          ! (workspace: need 3*m+n, prefer 3*m+n*nb)
                          call stdlib_sormbr( 'P', 'L', 'T', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in a
                          ! (workspace: need 4*m, prefer 3*m+m*nb)
                          call stdlib_sorgbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in a and computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', m, n, m, 0_ilp, s, work( ie ), vt,ldvt, a, lda, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    else if( wntuas ) then
                       ! path 9t(n much larger than m, jobu='s' or 'a',
                               ! jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! m left singular vectors to be computed in u
                       if( lwork>=m*m+max( n+m, 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(iu) is lda by m
                             ldwrku = lda
                          else
                             ! work(iu) is m by m
                             ldwrku = m
                          end if
                          itau = iu + ldwrku*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need m*m+2*m, prefer m*m+m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need m*m+m+n, prefer m*m+m+n*nb)
                          call stdlib_sorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_slacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_slaset( 'U', m-1, m-1, zero, zero,work( iu+ldwrku ), ldwrku &
                                    )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to u
                          ! (workspace: need m*m+4*m, prefer m*m+3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_slacpy( 'L', m, m, work( iu ), ldwrku, u,ldu )
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (workspace: need m*m+4*m, prefer m*m+3*m+(m-1)*nb)
                          call stdlib_sorgbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (workspace: need m*m+4*m, prefer m*m+3*m+m*nb)
                          call stdlib_sorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in u and computing right
                          ! singular vectors of l in work(iu)
                          ! (workspace: need m*m+bdspac)
                          call stdlib_sbdsqr( 'U', m, m, m, 0_ilp, s, work( ie ),work( iu ), ldwrku, &
                                    u, ldu, dum, 1_ilp,work( iwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in vt, storing result in a
                          ! (workspace: need m*m)
                          call stdlib_sgemm( 'N', 'N', m, n, m, one, work( iu ),ldwrku, vt, ldvt, &
                                    zero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_slacpy( 'F', m, n, a, lda, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m, prefer m+m*nb)
                          call stdlib_sgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_slacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need m+n, prefer m+n*nb)
                          call stdlib_sorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to u, zeroing out above it
                          call stdlib_slacpy( 'L', m, m, a, lda, u, ldu )
                          if (m>1_ilp) call stdlib_slaset( 'U', m-1, m-1, zero, zero, u( 1_ilp, 2_ilp ),ldu )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in u
                          ! (workspace: need 4*m, prefer 3*m+2*m*nb)
                          call stdlib_sgebrd( m, m, u, ldu, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in u by q
                          ! in vt
                          ! (workspace: need 3*m+n, prefer 3*m+n*nb)
                          call stdlib_sormbr( 'P', 'L', 'T', m, n, m, u, ldu,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (workspace: need 4*m, prefer 3*m+m*nb)
                          call stdlib_sorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_sbdsqr( 'U', m, n, m, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    end if
                 end if
              else
                 ! n < mnthr
                 ! path 10t(n greater than m, but not much larger)
                 ! reduce to bidiagonal form without lq decomposition
                 ie = 1_ilp
                 itauq = ie + m
                 itaup = itauq + m
                 iwork = itaup + m
                 ! bidiagonalize a
                 ! (workspace: need 3*m+n, prefer 3*m+(m+n)*nb)
                 call stdlib_sgebrd( m, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), &
                           work( iwork ), lwork-iwork+1,ierr )
                 if( wntuas ) then
                    ! if left singular vectors desired in u, copy result to u
                    ! and generate left bidiagonalizing vectors in u
                    ! (workspace: need 4*m-1, prefer 3*m+(m-1)*nb)
                    call stdlib_slacpy( 'L', m, m, a, lda, u, ldu )
                    call stdlib_sorgbr( 'Q', m, m, n, u, ldu, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvas ) then
                    ! if right singular vectors desired in vt, copy result to
                    ! vt and generate right bidiagonalizing vectors in vt
                    ! (workspace: need 3*m+nrvt, prefer 3*m+nrvt*nb)
                    call stdlib_slacpy( 'U', m, n, a, lda, vt, ldvt )
                    if( wntva )nrvt = n
                    if( wntvs )nrvt = m
                    call stdlib_sorgbr( 'P', nrvt, n, m, vt, ldvt, work( itaup ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntuo ) then
                    ! if left singular vectors desired in a, generate left
                    ! bidiagonalizing vectors in a
                    ! (workspace: need 4*m-1, prefer 3*m+(m-1)*nb)
                    call stdlib_sorgbr( 'Q', m, m, n, a, lda, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvo ) then
                    ! if right singular vectors desired in a, generate right
                    ! bidiagonalizing vectors in a
                    ! (workspace: need 4*m, prefer 3*m+m*nb)
                    call stdlib_sorgbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 iwork = ie + m
                 if( wntuas .or. wntuo )nru = m
                 if( wntun )nru = 0_ilp
                 if( wntvas .or. wntvo )ncvt = n
                 if( wntvn )ncvt = 0_ilp
                 if( ( .not.wntuo ) .and. ( .not.wntvo ) ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in vt
                    ! (workspace: need bdspac)
                    call stdlib_sbdsqr( 'L', m, ncvt, nru, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, dum,&
                               1_ilp, work( iwork ), info )
                 else if( ( .not.wntuo ) .and. wntvo ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in a
                    ! (workspace: need bdspac)
                    call stdlib_sbdsqr( 'L', m, ncvt, nru, 0_ilp, s, work( ie ), a, lda,u, ldu, dum, &
                              1_ilp, work( iwork ), info )
                 else
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in a and computing right singular
                    ! vectors in vt
                    ! (workspace: need bdspac)
                    call stdlib_sbdsqr( 'L', m, ncvt, nru, 0_ilp, s, work( ie ), vt,ldvt, a, lda, dum,&
                               1_ilp, work( iwork ), info )
                 end if
              end if
           end if
           ! if stdlib_sbdsqr failed to converge, copy unconverged superdiagonals
           ! to work( 2:minmn )
           if( info/=0_ilp ) then
              if( ie>2_ilp ) then
                 do i = 1, minmn - 1
                    work( i+1 ) = work( i+ie-1 )
                 end do
              end if
              if( ie<2_ilp ) then
                 do i = minmn - 1, 1, -1
                    work( i+1 ) = work( i+ie-1 )
                 end do
              end if
           end if
           ! undo scaling if necessary
           if( iscl==1_ilp ) then
              if( anrm>bignum )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,&
                        ierr )
              if( info/=0_ilp .and. anrm>bignum )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn-1,&
                         1_ilp, work( 2_ilp ),minmn, ierr )
              if( anrm<smlnum )call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,&
                        ierr )
              if( info/=0_ilp .and. anrm<smlnum )call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn-1,&
                         1_ilp, work( 2_ilp ),minmn, ierr )
           end if
           ! return optimal workspace in work(1)
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_sgesvd

     module subroutine stdlib_dgesvd( jobu, jobvt, m, n, a, lda, s, u, ldu,vt, ldvt, work, lwork, info )
     !! DGESVD computes the singular value decomposition (SVD) of a real
     !! M-by-N matrix A, optionally computing the left and/or right singular
     !! vectors. The SVD is written
     !! A = U * SIGMA * transpose(V)
     !! where SIGMA is an M-by-N matrix which is zero except for its
     !! min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
     !! V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
     !! are the singular values of A; they are real and non-negative, and
     !! are returned in descending order.  The first min(m,n) columns of
     !! U and V are the left and right singular vectors of A.
     !! Note that the routine returns V**T, not V.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu, jobvt
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: s(*), u(ldu,*), vt(ldvt,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wntua, wntuas, wntun, wntuo, wntus, wntva, wntvas, wntvn, wntvo,&
                      wntvs
           integer(ilp) :: bdspac, blk, chunk, i, ie, ierr, ir, iscl, itau, itaup, itauq, iu, &
           iwork, ldwrkr, ldwrku, maxwrk, minmn, minwrk, mnthr, ncu, ncvt, nru, nrvt, &
                     wrkbl
           integer(ilp) :: lwork_dgeqrf, lwork_dorgqr_n, lwork_dorgqr_m, lwork_dgebrd, &
                     lwork_dorgbr_p, lwork_dorgbr_q, lwork_dgelqf, lwork_dorglq_n, lwork_dorglq_m
           real(dp) :: anrm, bignum, eps, smlnum
           ! Local Arrays 
           real(dp) :: dum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           minmn = min( m, n )
           wntua = stdlib_lsame( jobu, 'A' )
           wntus = stdlib_lsame( jobu, 'S' )
           wntuas = wntua .or. wntus
           wntuo = stdlib_lsame( jobu, 'O' )
           wntun = stdlib_lsame( jobu, 'N' )
           wntva = stdlib_lsame( jobvt, 'A' )
           wntvs = stdlib_lsame( jobvt, 'S' )
           wntvas = wntva .or. wntvs
           wntvo = stdlib_lsame( jobvt, 'O' )
           wntvn = stdlib_lsame( jobvt, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.( wntua .or. wntus .or. wntuo .or. wntun ) ) then
              info = -1_ilp
           else if( .not.( wntva .or. wntvs .or. wntvo .or. wntvn ) .or.( wntvo .and. wntuo ) ) &
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldu<1_ilp .or. ( wntuas .and. ldu<m ) ) then
              info = -9_ilp
           else if( ldvt<1_ilp .or. ( wntva .and. ldvt<n ) .or.( wntvs .and. ldvt<minmn ) ) &
                     then
              info = -11_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! nb refers to the optimal block size for the immediately
             ! following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              minwrk = 1_ilp
              maxwrk = 1_ilp
              if( m>=n .and. minmn>0_ilp ) then
                 ! compute space needed for stdlib_dbdsqr
                 mnthr = stdlib_ilaenv( 6_ilp, 'DGESVD', jobu // jobvt, m, n, 0_ilp, 0_ilp )
                 bdspac = 5_ilp*n
                 ! compute space needed for stdlib_dgeqrf
                 call stdlib_dgeqrf( m, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_dgeqrf = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_dorgqr
                 call stdlib_dorgqr( m, n, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_dorgqr_n = int( dum(1_ilp),KIND=ilp)
                 call stdlib_dorgqr( m, m, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_dorgqr_m = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_dgebrd
                 call stdlib_dgebrd( n, n, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                           
                 lwork_dgebrd = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_dorgbr p
                 call stdlib_dorgbr( 'P', n, n, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                 lwork_dorgbr_p = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_dorgbr q
                 call stdlib_dorgbr( 'Q', n, n, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                 lwork_dorgbr_q = int( dum(1_ilp),KIND=ilp)
                 if( m>=mnthr ) then
                    if( wntun ) then
                       ! path 1 (m much larger than n, jobu='n')
                       maxwrk = n + lwork_dgeqrf
                       maxwrk = max( maxwrk, 3_ilp*n + lwork_dgebrd )
                       if( wntvo .or. wntvas )maxwrk = max( maxwrk, 3_ilp*n + lwork_dorgbr_p )
                       maxwrk = max( maxwrk, bdspac )
                       minwrk = max( 4_ilp*n, bdspac )
                    else if( wntuo .and. wntvn ) then
                       ! path 2 (m much larger than n, jobu='o', jobvt='n')
                       wrkbl = n + lwork_dgeqrf
                       wrkbl = max( wrkbl, n + lwork_dorgqr_n )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = max( n*n + wrkbl, n*n + m*n + n )
                       minwrk = max( 3_ilp*n + m, bdspac )
                    else if( wntuo .and. wntvas ) then
                       ! path 3 (m much larger than n, jobu='o', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_dgeqrf
                       wrkbl = max( wrkbl, n + lwork_dorgqr_n )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = max( n*n + wrkbl, n*n + m*n + n )
                       minwrk = max( 3_ilp*n + m, bdspac )
                    else if( wntus .and. wntvn ) then
                       ! path 4 (m much larger than n, jobu='s', jobvt='n')
                       wrkbl = n + lwork_dgeqrf
                       wrkbl = max( wrkbl, n + lwork_dorgqr_n )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = n*n + wrkbl
                       minwrk = max( 3_ilp*n + m, bdspac )
                    else if( wntus .and. wntvo ) then
                       ! path 5 (m much larger than n, jobu='s', jobvt='o')
                       wrkbl = n + lwork_dgeqrf
                       wrkbl = max( wrkbl, n + lwork_dorgqr_n )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = 2_ilp*n*n + wrkbl
                       minwrk = max( 3_ilp*n + m, bdspac )
                    else if( wntus .and. wntvas ) then
                       ! path 6 (m much larger than n, jobu='s', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_dgeqrf
                       wrkbl = max( wrkbl, n + lwork_dorgqr_n )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = n*n + wrkbl
                       minwrk = max( 3_ilp*n + m, bdspac )
                    else if( wntua .and. wntvn ) then
                       ! path 7 (m much larger than n, jobu='a', jobvt='n')
                       wrkbl = n + lwork_dgeqrf
                       wrkbl = max( wrkbl, n + lwork_dorgqr_m )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = n*n + wrkbl
                       minwrk = max( 3_ilp*n + m, bdspac )
                    else if( wntua .and. wntvo ) then
                       ! path 8 (m much larger than n, jobu='a', jobvt='o')
                       wrkbl = n + lwork_dgeqrf
                       wrkbl = max( wrkbl, n + lwork_dorgqr_m )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = 2_ilp*n*n + wrkbl
                       minwrk = max( 3_ilp*n + m, bdspac )
                    else if( wntua .and. wntvas ) then
                       ! path 9 (m much larger than n, jobu='a', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_dgeqrf
                       wrkbl = max( wrkbl, n + lwork_dorgqr_m )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, 3_ilp*n + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = n*n + wrkbl
                       minwrk = max( 3_ilp*n + m, bdspac )
                    end if
                 else
                    ! path 10 (m at least n, but not much larger)
                    call stdlib_dgebrd( m, n, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                              
                    lwork_dgebrd = int( dum(1_ilp),KIND=ilp)
                    maxwrk = 3_ilp*n + lwork_dgebrd
                    if( wntus .or. wntuo ) then
                       call stdlib_dorgbr( 'Q', m, n, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                       lwork_dorgbr_q = int( dum(1_ilp),KIND=ilp)
                       maxwrk = max( maxwrk, 3_ilp*n + lwork_dorgbr_q )
                    end if
                    if( wntua ) then
                       call stdlib_dorgbr( 'Q', m, m, n, a, lda, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                       lwork_dorgbr_q = int( dum(1_ilp),KIND=ilp)
                       maxwrk = max( maxwrk, 3_ilp*n + lwork_dorgbr_q )
                    end if
                    if( .not.wntvn ) then
                      maxwrk = max( maxwrk, 3_ilp*n + lwork_dorgbr_p )
                    end if
                    maxwrk = max( maxwrk, bdspac )
                    minwrk = max( 3_ilp*n + m, bdspac )
                 end if
              else if( minmn>0_ilp ) then
                 ! compute space needed for stdlib_dbdsqr
                 mnthr = stdlib_ilaenv( 6_ilp, 'DGESVD', jobu // jobvt, m, n, 0_ilp, 0_ilp )
                 bdspac = 5_ilp*m
                 ! compute space needed for stdlib_dgelqf
                 call stdlib_dgelqf( m, n, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_dgelqf = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_dorglq
                 call stdlib_dorglq( n, n, m, dum(1_ilp), n, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_dorglq_n = int( dum(1_ilp),KIND=ilp)
                 call stdlib_dorglq( m, n, m, a, lda, dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                 lwork_dorglq_m = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_dgebrd
                 call stdlib_dgebrd( m, m, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                           
                 lwork_dgebrd = int( dum(1_ilp),KIND=ilp)
                  ! compute space needed for stdlib_dorgbr p
                 call stdlib_dorgbr( 'P', m, m, m, a, n, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                 lwork_dorgbr_p = int( dum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_dorgbr q
                 call stdlib_dorgbr( 'Q', m, m, m, a, n, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                 lwork_dorgbr_q = int( dum(1_ilp),KIND=ilp)
                 if( n>=mnthr ) then
                    if( wntvn ) then
                       ! path 1t(n much larger than m, jobvt='n')
                       maxwrk = m + lwork_dgelqf
                       maxwrk = max( maxwrk, 3_ilp*m + lwork_dgebrd )
                       if( wntuo .or. wntuas )maxwrk = max( maxwrk, 3_ilp*m + lwork_dorgbr_q )
                       maxwrk = max( maxwrk, bdspac )
                       minwrk = max( 4_ilp*m, bdspac )
                    else if( wntvo .and. wntun ) then
                       ! path 2t(n much larger than m, jobu='n', jobvt='o')
                       wrkbl = m + lwork_dgelqf
                       wrkbl = max( wrkbl, m + lwork_dorglq_m )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = max( m*m + wrkbl, m*m + m*n + m )
                       minwrk = max( 3_ilp*m + n, bdspac )
                    else if( wntvo .and. wntuas ) then
                       ! path 3t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='o')
                       wrkbl = m + lwork_dgelqf
                       wrkbl = max( wrkbl, m + lwork_dorglq_m )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = max( m*m + wrkbl, m*m + m*n + m )
                       minwrk = max( 3_ilp*m + n, bdspac )
                    else if( wntvs .and. wntun ) then
                       ! path 4t(n much larger than m, jobu='n', jobvt='s')
                       wrkbl = m + lwork_dgelqf
                       wrkbl = max( wrkbl, m + lwork_dorglq_m )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = m*m + wrkbl
                       minwrk = max( 3_ilp*m + n, bdspac )
                    else if( wntvs .and. wntuo ) then
                       ! path 5t(n much larger than m, jobu='o', jobvt='s')
                       wrkbl = m + lwork_dgelqf
                       wrkbl = max( wrkbl, m + lwork_dorglq_m )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = 2_ilp*m*m + wrkbl
                       minwrk = max( 3_ilp*m + n, bdspac )
                    else if( wntvs .and. wntuas ) then
                       ! path 6t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='s')
                       wrkbl = m + lwork_dgelqf
                       wrkbl = max( wrkbl, m + lwork_dorglq_m )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = m*m + wrkbl
                       minwrk = max( 3_ilp*m + n, bdspac )
                    else if( wntva .and. wntun ) then
                       ! path 7t(n much larger than m, jobu='n', jobvt='a')
                       wrkbl = m + lwork_dgelqf
                       wrkbl = max( wrkbl, m + lwork_dorglq_n )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = m*m + wrkbl
                       minwrk = max( 3_ilp*m + n, bdspac )
                    else if( wntva .and. wntuo ) then
                       ! path 8t(n much larger than m, jobu='o', jobvt='a')
                       wrkbl = m + lwork_dgelqf
                       wrkbl = max( wrkbl, m + lwork_dorglq_n )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = 2_ilp*m*m + wrkbl
                       minwrk = max( 3_ilp*m + n, bdspac )
                    else if( wntva .and. wntuas ) then
                       ! path 9t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='a')
                       wrkbl = m + lwork_dgelqf
                       wrkbl = max( wrkbl, m + lwork_dorglq_n )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dgebrd )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_p )
                       wrkbl = max( wrkbl, 3_ilp*m + lwork_dorgbr_q )
                       wrkbl = max( wrkbl, bdspac )
                       maxwrk = m*m + wrkbl
                       minwrk = max( 3_ilp*m + n, bdspac )
                    end if
                 else
                    ! path 10t(n greater than m, but not much larger)
                    call stdlib_dgebrd( m, n, a, lda, s, dum(1_ilp), dum(1_ilp),dum(1_ilp), dum(1_ilp), -1_ilp, ierr )
                              
                    lwork_dgebrd = int( dum(1_ilp),KIND=ilp)
                    maxwrk = 3_ilp*m + lwork_dgebrd
                    if( wntvs .or. wntvo ) then
                      ! compute space needed for stdlib_dorgbr p
                      call stdlib_dorgbr( 'P', m, n, m, a, n, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                      lwork_dorgbr_p = int( dum(1_ilp),KIND=ilp)
                      maxwrk = max( maxwrk, 3_ilp*m + lwork_dorgbr_p )
                    end if
                    if( wntva ) then
                      call stdlib_dorgbr( 'P', n, n, m, a, n, dum(1_ilp),dum(1_ilp), -1_ilp, ierr )
                      lwork_dorgbr_p = int( dum(1_ilp),KIND=ilp)
                      maxwrk = max( maxwrk, 3_ilp*m + lwork_dorgbr_p )
                    end if
                    if( .not.wntun ) then
                       maxwrk = max( maxwrk, 3_ilp*m + lwork_dorgbr_q )
                    end if
                    maxwrk = max( maxwrk, bdspac )
                    minwrk = max( 3_ilp*m + n, bdspac )
                 end if
              end if
              maxwrk = max( maxwrk, minwrk )
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGESVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = sqrt( stdlib_dlamch( 'S' ) ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_dlange( 'M', m, n, a, lda, dum )
           iscl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              iscl = 1_ilp
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, ierr )
           else if( anrm>bignum ) then
              iscl = 1_ilp
              call stdlib_dlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, ierr )
           end if
           if( m>=n ) then
              ! a has at least as many rows as columns. if a has sufficiently
              ! more rows than columns, first reduce using the qr
              ! decomposition (if sufficient workspace available)
              if( m>=mnthr ) then
                 if( wntun ) then
                    ! path 1 (m much larger than n, jobu='n')
                    ! no left singular vectors to be computed
                    itau = 1_ilp
                    iwork = itau + n
                    ! compute a=q*r
                    ! (workspace: need 2*n, prefer n + n*nb)
                    call stdlib_dgeqrf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                              ierr )
                    ! zero out below r
                    if( n > 1_ilp ) then
                       call stdlib_dlaset( 'L', n-1, n-1, zero, zero, a( 2_ilp, 1_ilp ),lda )
                    end if
                    ie = 1_ilp
                    itauq = ie + n
                    itaup = itauq + n
                    iwork = itaup + n
                    ! bidiagonalize r in a
                    ! (workspace: need 4*n, prefer 3*n + 2*n*nb)
                    call stdlib_dgebrd( n, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), &
                              work( iwork ), lwork-iwork+1,ierr )
                    ncvt = 0_ilp
                    if( wntvo .or. wntvas ) then
                       ! if right singular vectors desired, generate p'.
                       ! (workspace: need 4*n-1, prefer 3*n + (n-1)*nb)
                       call stdlib_dorgbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       ncvt = n
                    end if
                    iwork = ie + n
                    ! perform bidiagonal qr iteration, computing right
                    ! singular vectors of a in a if desired
                    ! (workspace: need bdspac)
                    call stdlib_dbdsqr( 'U', n, ncvt, 0_ilp, 0_ilp, s, work( ie ), a, lda,dum, 1_ilp, dum, 1_ilp, &
                              work( iwork ), info )
                    ! if right singular vectors desired in vt, copy them there
                    if( wntvas )call stdlib_dlacpy( 'F', n, n, a, lda, vt, ldvt )
                 else if( wntuo .and. wntvn ) then
                    ! path 2 (m much larger than n, jobu='o', jobvt='n')
                    ! n left singular vectors to be overwritten on a and
                    ! no right singular vectors to be computed
                    if( lwork>=n*n+max( 4_ilp*n, bdspac ) ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n + n ) + lda*n ) then
                          ! work(iu) is lda by n, work(ir) is lda by n
                          ldwrku = lda
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n + n ) + n*n ) then
                          ! work(iu) is lda by n, work(ir) is n by n
                          ldwrku = lda
                          ldwrkr = n
                       else
                          ! work(iu) is ldwrku by n, work(ir) is n by n
                          ldwrku = ( lwork-n*n-n ) / n
                          ldwrkr = n
                       end if
                       itau = ir + ldwrkr*n
                       iwork = itau + n
                       ! compute a=q*r
                       ! (workspace: need n*n + 2*n, prefer n*n + n + n*nb)
                       call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to work(ir) and zero out below it
                       call stdlib_dlacpy( 'U', n, n, a, lda, work( ir ), ldwrkr )
                       call stdlib_dlaset( 'L', n-1, n-1, zero, zero, work( ir+1 ),ldwrkr )
                                 
                       ! generate q in a
                       ! (workspace: need n*n + 2*n, prefer n*n + n + n*nb)
                       call stdlib_dorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + n
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in work(ir)
                       ! (workspace: need n*n + 4*n, prefer n*n + 3*n + 2*n*nb)
                       call stdlib_dgebrd( n, n, work( ir ), ldwrkr, s, work( ie ),work( itauq ), &
                                 work( itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing r
                       ! (workspace: need n*n + 4*n, prefer n*n + 3*n + n*nb)
                       call stdlib_dorgbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       iwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of r in work(ir)
                       ! (workspace: need n*n + bdspac)
                       call stdlib_dbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, work( ie ), dum, 1_ilp,work( ir ), &
                                 ldwrkr, dum, 1_ilp,work( iwork ), info )
                       iu = ie + n
                       ! multiply q in a by left singular vectors of r in
                       ! work(ir), storing result in work(iu) and copying to a
                       ! (workspace: need n*n + 2*n, prefer n*n + m*n + n)
                       do i = 1, m, ldwrku
                          chunk = min( m-i+1, ldwrku )
                          call stdlib_dgemm( 'N', 'N', chunk, n, n, one, a( i, 1_ilp ),lda, work( ir )&
                                    , ldwrkr, zero,work( iu ), ldwrku )
                          call stdlib_dlacpy( 'F', chunk, n, work( iu ), ldwrku,a( i, 1_ilp ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       ie = 1_ilp
                       itauq = ie + n
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize a
                       ! (workspace: need 3*n + m, prefer 3*n + (m + n)*nb)
                       call stdlib_dgebrd( m, n, a, lda, s, work( ie ),work( itauq ), work( itaup &
                                 ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing a
                       ! (workspace: need 4*n, prefer 3*n + n*nb)
                       call stdlib_dorgbr( 'Q', m, n, n, a, lda, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in a
                       ! (workspace: need bdspac)
                       call stdlib_dbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, work( ie ), dum, 1_ilp,a, lda, dum, 1_ilp, &
                                 work( iwork ), info )
                    end if
                 else if( wntuo .and. wntvas ) then
                    ! path 3 (m much larger than n, jobu='o', jobvt='s' or 'a')
                    ! n left singular vectors to be overwritten on a and
                    ! n right singular vectors to be computed in vt
                    if( lwork>=n*n+max( 4_ilp*n, bdspac ) ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n + n ) + lda*n ) then
                          ! work(iu) is lda by n and work(ir) is lda by n
                          ldwrku = lda
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n + n ) + n*n ) then
                          ! work(iu) is lda by n and work(ir) is n by n
                          ldwrku = lda
                          ldwrkr = n
                       else
                          ! work(iu) is ldwrku by n and work(ir) is n by n
                          ldwrku = ( lwork-n*n-n ) / n
                          ldwrkr = n
                       end if
                       itau = ir + ldwrkr*n
                       iwork = itau + n
                       ! compute a=q*r
                       ! (workspace: need n*n + 2*n, prefer n*n + n + n*nb)
                       call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to vt, zeroing out below it
                       call stdlib_dlacpy( 'U', n, n, a, lda, vt, ldvt )
                       if( n>1_ilp )call stdlib_dlaset( 'L', n-1, n-1, zero, zero,vt( 2_ilp, 1_ilp ), ldvt )
                                 
                       ! generate q in a
                       ! (workspace: need n*n + 2*n, prefer n*n + n + n*nb)
                       call stdlib_dorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + n
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in vt, copying result to work(ir)
                       ! (workspace: need n*n + 4*n, prefer n*n + 3*n + 2*n*nb)
                       call stdlib_dgebrd( n, n, vt, ldvt, s, work( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       call stdlib_dlacpy( 'L', n, n, vt, ldvt, work( ir ), ldwrkr )
                       ! generate left vectors bidiagonalizing r in work(ir)
                       ! (workspace: need n*n + 4*n, prefer n*n + 3*n + n*nb)
                       call stdlib_dorgbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing r in vt
                       ! (workspace: need n*n + 4*n-1, prefer n*n + 3*n + (n-1)*nb)
                       call stdlib_dorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of r in work(ir) and computing right
                       ! singular vectors of r in vt
                       ! (workspace: need n*n + bdspac)
                       call stdlib_dbdsqr( 'U', n, n, n, 0_ilp, s, work( ie ), vt, ldvt,work( ir ), &
                                 ldwrkr, dum, 1_ilp,work( iwork ), info )
                       iu = ie + n
                       ! multiply q in a by left singular vectors of r in
                       ! work(ir), storing result in work(iu) and copying to a
                       ! (workspace: need n*n + 2*n, prefer n*n + m*n + n)
                       do i = 1, m, ldwrku
                          chunk = min( m-i+1, ldwrku )
                          call stdlib_dgemm( 'N', 'N', chunk, n, n, one, a( i, 1_ilp ),lda, work( ir )&
                                    , ldwrkr, zero,work( iu ), ldwrku )
                          call stdlib_dlacpy( 'F', chunk, n, work( iu ), ldwrku,a( i, 1_ilp ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       itau = 1_ilp
                       iwork = itau + n
                       ! compute a=q*r
                       ! (workspace: need 2*n, prefer n + n*nb)
                       call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to vt, zeroing out below it
                       call stdlib_dlacpy( 'U', n, n, a, lda, vt, ldvt )
                       if( n>1_ilp )call stdlib_dlaset( 'L', n-1, n-1, zero, zero,vt( 2_ilp, 1_ilp ), ldvt )
                                 
                       ! generate q in a
                       ! (workspace: need 2*n, prefer n + n*nb)
                       call stdlib_dorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + n
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in vt
                       ! (workspace: need 4*n, prefer 3*n + 2*n*nb)
                       call stdlib_dgebrd( n, n, vt, ldvt, s, work( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! multiply q in a by left vectors bidiagonalizing r
                       ! (workspace: need 3*n + m, prefer 3*n + m*nb)
                       call stdlib_dormbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), a, lda,&
                                  work( iwork ),lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing r in vt
                       ! (workspace: need 4*n-1, prefer 3*n + (n-1)*nb)
                       call stdlib_dorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in a and computing right
                       ! singular vectors of a in vt
                       ! (workspace: need bdspac)
                       call stdlib_dbdsqr( 'U', n, n, m, 0_ilp, s, work( ie ), vt, ldvt,a, lda, dum, &
                                 1_ilp, work( iwork ), info )
                    end if
                 else if( wntus ) then
                    if( wntvn ) then
                       ! path 4 (m much larger than n, jobu='s', jobvt='n')
                       ! n left singular vectors to be computed in u and
                       ! no right singular vectors to be computed
                       if( lwork>=n*n+max( 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(ir) is lda by n
                             ldwrkr = lda
                          else
                             ! work(ir) is n by n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (workspace: need n*n + 2*n, prefer n*n + n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(ir), zeroing out below it
                          call stdlib_dlacpy( 'U', n, n, a, lda, work( ir ),ldwrkr )
                          call stdlib_dlaset( 'L', n-1, n-1, zero, zero,work( ir+1 ), ldwrkr )
                                    
                          ! generate q in a
                          ! (workspace: need n*n + 2*n, prefer n*n + n + n*nb)
                          call stdlib_dorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(ir)
                          ! (workspace: need n*n + 4*n, prefer n*n + 3*n + 2*n*nb)
                          call stdlib_dgebrd( n, n, work( ir ), ldwrkr, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate left vectors bidiagonalizing r in work(ir)
                          ! (workspace: need n*n + 4*n, prefer n*n + 3*n + n*nb)
                          call stdlib_dorgbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(ir)
                          ! (workspace: need n*n + bdspac)
                          call stdlib_dbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, work( ie ), dum,1_ilp, work( ir ), &
                                    ldwrkr, dum, 1_ilp,work( iwork ), info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(ir), storing result in u
                          ! (workspace: need n*n)
                          call stdlib_dgemm( 'N', 'N', m, n, n, one, a, lda,work( ir ), ldwrkr, &
                                    zero, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need 2*n, prefer n + n*nb)
                          call stdlib_dorgqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_dlaset( 'L', n-1, n-1, zero, zero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (workspace: need 4*n, prefer 3*n + 2*n*nb)
                          call stdlib_dgebrd( n, n, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left vectors bidiagonalizing r
                          ! (workspace: need 3*n + m, prefer 3*n + m*nb)
                          call stdlib_dormbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, work( ie ), dum,1_ilp, u, ldu, dum, &
                                    1_ilp, work( iwork ),info )
                       end if
                    else if( wntvo ) then
                       ! path 5 (m much larger than n, jobu='s', jobvt='o')
                       ! n left singular vectors to be computed in u and
                       ! n right singular vectors to be overwritten on a
                       if( lwork>=2_ilp*n*n+max( 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*n ) then
                             ! work(iu) is lda by n and work(ir) is lda by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda + n )*n ) then
                             ! work(iu) is lda by n and work(ir) is n by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          else
                             ! work(iu) is n by n and work(ir) is n by n
                             ldwrku = n
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (workspace: need 2*n*n + 2*n, prefer 2*n*n + n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_dlacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_dlaset( 'L', n-1, n-1, zero, zero,work( iu+1 ), ldwrku )
                                    
                          ! generate q in a
                          ! (workspace: need 2*n*n + 2*n, prefer 2*n*n + n + n*nb)
                          call stdlib_dorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to
                          ! work(ir)
                          ! (workspace: need 2*n*n + 4*n,
                                      ! prefer 2*n*n+3*n+2*n*nb)
                          call stdlib_dgebrd( n, n, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_dlacpy( 'U', n, n, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (workspace: need 2*n*n + 4*n, prefer 2*n*n + 3*n + n*nb)
                          call stdlib_dorgbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (workspace: need 2*n*n + 4*n-1,
                                      ! prefer 2*n*n+3*n+(n-1)*nb)
                          call stdlib_dorgbr( 'P', n, n, n, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in work(ir)
                          ! (workspace: need 2*n*n + bdspac)
                          call stdlib_dbdsqr( 'U', n, n, n, 0_ilp, s, work( ie ),work( ir ), ldwrkr, &
                                    work( iu ),ldwrku, dum, 1_ilp, work( iwork ), info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(iu), storing result in u
                          ! (workspace: need n*n)
                          call stdlib_dgemm( 'N', 'N', m, n, n, one, a, lda,work( iu ), ldwrku, &
                                    zero, u, ldu )
                          ! copy right singular vectors of r to a
                          ! (workspace: need n*n)
                          call stdlib_dlacpy( 'F', n, n, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need 2*n, prefer n + n*nb)
                          call stdlib_dorgqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_dlaset( 'L', n-1, n-1, zero, zero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (workspace: need 4*n, prefer 3*n + 2*n*nb)
                          call stdlib_dgebrd( n, n, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left vectors bidiagonalizing r
                          ! (workspace: need 3*n + m, prefer 3*n + m*nb)
                          call stdlib_dormbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right vectors bidiagonalizing r in a
                          ! (workspace: need 4*n-1, prefer 3*n + (n-1)*nb)
                          call stdlib_dorgbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in a
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', n, n, m, 0_ilp, s, work( ie ), a,lda, u, ldu, dum, &
                                    1_ilp, work( iwork ),info )
                       end if
                    else if( wntvas ) then
                       ! path 6 (m much larger than n, jobu='s', jobvt='s'
                               ! or 'a')
                       ! n left singular vectors to be computed in u and
                       ! n right singular vectors to be computed in vt
                       if( lwork>=n*n+max( 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is n by n
                             ldwrku = n
                          end if
                          itau = iu + ldwrku*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (workspace: need n*n + 2*n, prefer n*n + n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_dlacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_dlaset( 'L', n-1, n-1, zero, zero,work( iu+1 ), ldwrku )
                                    
                          ! generate q in a
                          ! (workspace: need n*n + 2*n, prefer n*n + n + n*nb)
                          call stdlib_dorgqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to vt
                          ! (workspace: need n*n + 4*n, prefer n*n + 3*n + 2*n*nb)
                          call stdlib_dgebrd( n, n, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_dlacpy( 'U', n, n, work( iu ), ldwrku, vt,ldvt )
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (workspace: need n*n + 4*n, prefer n*n + 3*n + n*nb)
                          call stdlib_dorgbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (workspace: need n*n + 4*n-1,
                                      ! prefer n*n+3*n+(n-1)*nb)
                          call stdlib_dorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in vt
                          ! (workspace: need n*n + bdspac)
                          call stdlib_dbdsqr( 'U', n, n, n, 0_ilp, s, work( ie ), vt,ldvt, work( iu ),&
                                     ldwrku, dum, 1_ilp,work( iwork ), info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(iu), storing result in u
                          ! (workspace: need n*n)
                          call stdlib_dgemm( 'N', 'N', m, n, n, one, a, lda,work( iu ), ldwrku, &
                                    zero, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need 2*n, prefer n + n*nb)
                          call stdlib_dorgqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to vt, zeroing out below it
                          call stdlib_dlacpy( 'U', n, n, a, lda, vt, ldvt )
                          if( n>1_ilp )call stdlib_dlaset( 'L', n-1, n-1, zero, zero,vt( 2_ilp, 1_ilp ), ldvt &
                                    )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in vt
                          ! (workspace: need 4*n, prefer 3*n + 2*n*nb)
                          call stdlib_dgebrd( n, n, vt, ldvt, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in vt
                          ! (workspace: need 3*n + m, prefer 3*n + m*nb)
                          call stdlib_dormbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (workspace: need 4*n-1, prefer 3*n + (n-1)*nb)
                          call stdlib_dorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', n, n, m, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    end if
                 else if( wntua ) then
                    if( wntvn ) then
                       ! path 7 (m much larger than n, jobu='a', jobvt='n')
                       ! m left singular vectors to be computed in u and
                       ! no right singular vectors to be computed
                       if( lwork>=n*n+max( n+m, 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(ir) is lda by n
                             ldwrkr = lda
                          else
                             ! work(ir) is n by n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need n*n + 2*n, prefer n*n + n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, n, a, lda, u, ldu )
                          ! copy r to work(ir), zeroing out below it
                          call stdlib_dlacpy( 'U', n, n, a, lda, work( ir ),ldwrkr )
                          call stdlib_dlaset( 'L', n-1, n-1, zero, zero,work( ir+1 ), ldwrkr )
                                    
                          ! generate q in u
                          ! (workspace: need n*n + n + m, prefer n*n + n + m*nb)
                          call stdlib_dorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(ir)
                          ! (workspace: need n*n + 4*n, prefer n*n + 3*n + 2*n*nb)
                          call stdlib_dgebrd( n, n, work( ir ), ldwrkr, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (workspace: need n*n + 4*n, prefer n*n + 3*n + n*nb)
                          call stdlib_dorgbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(ir)
                          ! (workspace: need n*n + bdspac)
                          call stdlib_dbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, work( ie ), dum,1_ilp, work( ir ), &
                                    ldwrkr, dum, 1_ilp,work( iwork ), info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(ir), storing result in a
                          ! (workspace: need n*n)
                          call stdlib_dgemm( 'N', 'N', m, n, n, one, u, ldu,work( ir ), ldwrkr, &
                                    zero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_dlacpy( 'F', m, n, a, lda, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need n + m, prefer n + m*nb)
                          call stdlib_dorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_dlaset( 'L', n-1, n-1, zero, zero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (workspace: need 4*n, prefer 3*n + 2*n*nb)
                          call stdlib_dgebrd( n, n, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in a
                          ! (workspace: need 3*n + m, prefer 3*n + m*nb)
                          call stdlib_dormbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, work( ie ), dum,1_ilp, u, ldu, dum, &
                                    1_ilp, work( iwork ),info )
                       end if
                    else if( wntvo ) then
                       ! path 8 (m much larger than n, jobu='a', jobvt='o')
                       ! m left singular vectors to be computed in u and
                       ! n right singular vectors to be overwritten on a
                       if( lwork>=2_ilp*n*n+max( n+m, 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*n ) then
                             ! work(iu) is lda by n and work(ir) is lda by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda + n )*n ) then
                             ! work(iu) is lda by n and work(ir) is n by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          else
                             ! work(iu) is n by n and work(ir) is n by n
                             ldwrku = n
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n*n + 2*n, prefer 2*n*n + n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need 2*n*n + n + m, prefer 2*n*n + n + m*nb)
                          call stdlib_dorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_dlacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_dlaset( 'L', n-1, n-1, zero, zero,work( iu+1 ), ldwrku )
                                    
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to
                          ! work(ir)
                          ! (workspace: need 2*n*n + 4*n,
                                      ! prefer 2*n*n+3*n+2*n*nb)
                          call stdlib_dgebrd( n, n, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_dlacpy( 'U', n, n, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (workspace: need 2*n*n + 4*n, prefer 2*n*n + 3*n + n*nb)
                          call stdlib_dorgbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (workspace: need 2*n*n + 4*n-1,
                                      ! prefer 2*n*n+3*n+(n-1)*nb)
                          call stdlib_dorgbr( 'P', n, n, n, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in work(ir)
                          ! (workspace: need 2*n*n + bdspac)
                          call stdlib_dbdsqr( 'U', n, n, n, 0_ilp, s, work( ie ),work( ir ), ldwrkr, &
                                    work( iu ),ldwrku, dum, 1_ilp, work( iwork ), info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(iu), storing result in a
                          ! (workspace: need n*n)
                          call stdlib_dgemm( 'N', 'N', m, n, n, one, u, ldu,work( iu ), ldwrku, &
                                    zero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_dlacpy( 'F', m, n, a, lda, u, ldu )
                          ! copy right singular vectors of r from work(ir) to a
                          call stdlib_dlacpy( 'F', n, n, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need n + m, prefer n + m*nb)
                          call stdlib_dorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_dlaset( 'L', n-1, n-1, zero, zero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (workspace: need 4*n, prefer 3*n + 2*n*nb)
                          call stdlib_dgebrd( n, n, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in a
                          ! (workspace: need 3*n + m, prefer 3*n + m*nb)
                          call stdlib_dormbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in a
                          ! (workspace: need 4*n-1, prefer 3*n + (n-1)*nb)
                          call stdlib_dorgbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in a
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', n, n, m, 0_ilp, s, work( ie ), a,lda, u, ldu, dum, &
                                    1_ilp, work( iwork ),info )
                       end if
                    else if( wntvas ) then
                       ! path 9 (m much larger than n, jobu='a', jobvt='s'
                               ! or 'a')
                       ! m left singular vectors to be computed in u and
                       ! n right singular vectors to be computed in vt
                       if( lwork>=n*n+max( n+m, 4_ilp*n, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is n by n
                             ldwrku = n
                          end if
                          itau = iu + ldwrku*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need n*n + 2*n, prefer n*n + n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need n*n + n + m, prefer n*n + n + m*nb)
                          call stdlib_dorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_dlacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_dlaset( 'L', n-1, n-1, zero, zero,work( iu+1 ), ldwrku )
                                    
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to vt
                          ! (workspace: need n*n + 4*n, prefer n*n + 3*n + 2*n*nb)
                          call stdlib_dgebrd( n, n, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_dlacpy( 'U', n, n, work( iu ), ldwrku, vt,ldvt )
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (workspace: need n*n + 4*n, prefer n*n + 3*n + n*nb)
                          call stdlib_dorgbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (workspace: need n*n + 4*n-1,
                                      ! prefer n*n+3*n+(n-1)*nb)
                          call stdlib_dorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in vt
                          ! (workspace: need n*n + bdspac)
                          call stdlib_dbdsqr( 'U', n, n, n, 0_ilp, s, work( ie ), vt,ldvt, work( iu ),&
                                     ldwrku, dum, 1_ilp,work( iwork ), info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(iu), storing result in a
                          ! (workspace: need n*n)
                          call stdlib_dgemm( 'N', 'N', m, n, n, one, u, ldu,work( iu ), ldwrku, &
                                    zero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_dlacpy( 'F', m, n, a, lda, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (workspace: need 2*n, prefer n + n*nb)
                          call stdlib_dgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (workspace: need n + m, prefer n + m*nb)
                          call stdlib_dorgqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r from a to vt, zeroing out below it
                          call stdlib_dlacpy( 'U', n, n, a, lda, vt, ldvt )
                          if( n>1_ilp )call stdlib_dlaset( 'L', n-1, n-1, zero, zero,vt( 2_ilp, 1_ilp ), ldvt &
                                    )
                          ie = itau
                          itauq = ie + n
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in vt
                          ! (workspace: need 4*n, prefer 3*n + 2*n*nb)
                          call stdlib_dgebrd( n, n, vt, ldvt, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in vt
                          ! (workspace: need 3*n + m, prefer 3*n + m*nb)
                          call stdlib_dormbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (workspace: need 4*n-1, prefer 3*n + (n-1)*nb)
                          call stdlib_dorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          iwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', n, n, m, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    end if
                 end if
              else
                 ! m < mnthr
                 ! path 10 (m at least n, but not much larger)
                 ! reduce to bidiagonal form without qr decomposition
                 ie = 1_ilp
                 itauq = ie + n
                 itaup = itauq + n
                 iwork = itaup + n
                 ! bidiagonalize a
                 ! (workspace: need 3*n + m, prefer 3*n + (m + n)*nb)
                 call stdlib_dgebrd( m, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), &
                           work( iwork ), lwork-iwork+1,ierr )
                 if( wntuas ) then
                    ! if left singular vectors desired in u, copy result to u
                    ! and generate left bidiagonalizing vectors in u
                    ! (workspace: need 3*n + ncu, prefer 3*n + ncu*nb)
                    call stdlib_dlacpy( 'L', m, n, a, lda, u, ldu )
                    if( wntus )ncu = n
                    if( wntua )ncu = m
                    call stdlib_dorgbr( 'Q', m, ncu, n, u, ldu, work( itauq ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntvas ) then
                    ! if right singular vectors desired in vt, copy result to
                    ! vt and generate right bidiagonalizing vectors in vt
                    ! (workspace: need 4*n-1, prefer 3*n + (n-1)*nb)
                    call stdlib_dlacpy( 'U', n, n, a, lda, vt, ldvt )
                    call stdlib_dorgbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntuo ) then
                    ! if left singular vectors desired in a, generate left
                    ! bidiagonalizing vectors in a
                    ! (workspace: need 4*n, prefer 3*n + n*nb)
                    call stdlib_dorgbr( 'Q', m, n, n, a, lda, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvo ) then
                    ! if right singular vectors desired in a, generate right
                    ! bidiagonalizing vectors in a
                    ! (workspace: need 4*n-1, prefer 3*n + (n-1)*nb)
                    call stdlib_dorgbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 iwork = ie + n
                 if( wntuas .or. wntuo )nru = m
                 if( wntun )nru = 0_ilp
                 if( wntvas .or. wntvo )ncvt = n
                 if( wntvn )ncvt = 0_ilp
                 if( ( .not.wntuo ) .and. ( .not.wntvo ) ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in vt
                    ! (workspace: need bdspac)
                    call stdlib_dbdsqr( 'U', n, ncvt, nru, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, dum,&
                               1_ilp, work( iwork ), info )
                 else if( ( .not.wntuo ) .and. wntvo ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in a
                    ! (workspace: need bdspac)
                    call stdlib_dbdsqr( 'U', n, ncvt, nru, 0_ilp, s, work( ie ), a, lda,u, ldu, dum, &
                              1_ilp, work( iwork ), info )
                 else
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in a and computing right singular
                    ! vectors in vt
                    ! (workspace: need bdspac)
                    call stdlib_dbdsqr( 'U', n, ncvt, nru, 0_ilp, s, work( ie ), vt,ldvt, a, lda, dum,&
                               1_ilp, work( iwork ), info )
                 end if
              end if
           else
              ! a has more columns than rows. if a has sufficiently more
              ! columns than rows, first reduce using the lq decomposition (if
              ! sufficient workspace available)
              if( n>=mnthr ) then
                 if( wntvn ) then
                    ! path 1t(n much larger than m, jobvt='n')
                    ! no right singular vectors to be computed
                    itau = 1_ilp
                    iwork = itau + m
                    ! compute a=l*q
                    ! (workspace: need 2*m, prefer m + m*nb)
                    call stdlib_dgelqf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                              ierr )
                    ! zero out above l
                    if (m>1_ilp) call stdlib_dlaset( 'U', m-1, m-1, zero, zero, a( 1_ilp, 2_ilp ), lda )
                    ie = 1_ilp
                    itauq = ie + m
                    itaup = itauq + m
                    iwork = itaup + m
                    ! bidiagonalize l in a
                    ! (workspace: need 4*m, prefer 3*m + 2*m*nb)
                    call stdlib_dgebrd( m, m, a, lda, s, work( ie ), work( itauq ),work( itaup ), &
                              work( iwork ), lwork-iwork+1,ierr )
                    if( wntuo .or. wntuas ) then
                       ! if left singular vectors desired, generate q
                       ! (workspace: need 4*m, prefer 3*m + m*nb)
                       call stdlib_dorgbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                    end if
                    iwork = ie + m
                    nru = 0_ilp
                    if( wntuo .or. wntuas )nru = m
                    ! perform bidiagonal qr iteration, computing left singular
                    ! vectors of a in a if desired
                    ! (workspace: need bdspac)
                    call stdlib_dbdsqr( 'U', m, 0_ilp, nru, 0_ilp, s, work( ie ), dum, 1_ilp, a,lda, dum, 1_ilp, &
                              work( iwork ), info )
                    ! if left singular vectors desired in u, copy them there
                    if( wntuas )call stdlib_dlacpy( 'F', m, m, a, lda, u, ldu )
                 else if( wntvo .and. wntun ) then
                    ! path 2t(n much larger than m, jobu='n', jobvt='o')
                    ! m right singular vectors to be overwritten on a and
                    ! no left singular vectors to be computed
                    if( lwork>=m*m+max( 4_ilp*m, bdspac ) ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n + m ) + lda*m ) then
                          ! work(iu) is lda by n and work(ir) is lda by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n + m ) + m*m ) then
                          ! work(iu) is lda by n and work(ir) is m by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = m
                       else
                          ! work(iu) is m by chunk and work(ir) is m by m
                          ldwrku = m
                          chunk = ( lwork-m*m-m ) / m
                          ldwrkr = m
                       end if
                       itau = ir + ldwrkr*m
                       iwork = itau + m
                       ! compute a=l*q
                       ! (workspace: need m*m + 2*m, prefer m*m + m + m*nb)
                       call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to work(ir) and zero out above it
                       call stdlib_dlacpy( 'L', m, m, a, lda, work( ir ), ldwrkr )
                       call stdlib_dlaset( 'U', m-1, m-1, zero, zero,work( ir+ldwrkr ), ldwrkr )
                                 
                       ! generate q in a
                       ! (workspace: need m*m + 2*m, prefer m*m + m + m*nb)
                       call stdlib_dorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + m
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in work(ir)
                       ! (workspace: need m*m + 4*m, prefer m*m + 3*m + 2*m*nb)
                       call stdlib_dgebrd( m, m, work( ir ), ldwrkr, s, work( ie ),work( itauq ), &
                                 work( itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing l
                       ! (workspace: need m*m + 4*m-1, prefer m*m + 3*m + (m-1)*nb)
                       call stdlib_dorgbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       iwork = ie + m
                       ! perform bidiagonal qr iteration, computing right
                       ! singular vectors of l in work(ir)
                       ! (workspace: need m*m + bdspac)
                       call stdlib_dbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, work( ie ),work( ir ), ldwrkr, dum,&
                                  1_ilp, dum, 1_ilp,work( iwork ), info )
                       iu = ie + m
                       ! multiply right singular vectors of l in work(ir) by q
                       ! in a, storing result in work(iu) and copying to a
                       ! (workspace: need m*m + 2*m, prefer m*m + m*n + m)
                       do i = 1, n, chunk
                          blk = min( n-i+1, chunk )
                          call stdlib_dgemm( 'N', 'N', m, blk, m, one, work( ir ),ldwrkr, a( 1_ilp, i &
                                    ), lda, zero,work( iu ), ldwrku )
                          call stdlib_dlacpy( 'F', m, blk, work( iu ), ldwrku,a( 1_ilp, i ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       ie = 1_ilp
                       itauq = ie + m
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize a
                       ! (workspace: need 3*m + n, prefer 3*m + (m + n)*nb)
                       call stdlib_dgebrd( m, n, a, lda, s, work( ie ),work( itauq ), work( itaup &
                                 ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing a
                       ! (workspace: need 4*m, prefer 3*m + m*nb)
                       call stdlib_dorgbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + m
                       ! perform bidiagonal qr iteration, computing right
                       ! singular vectors of a in a
                       ! (workspace: need bdspac)
                       call stdlib_dbdsqr( 'L', m, n, 0_ilp, 0_ilp, s, work( ie ), a, lda,dum, 1_ilp, dum, 1_ilp, &
                                 work( iwork ), info )
                    end if
                 else if( wntvo .and. wntuas ) then
                    ! path 3t(n much larger than m, jobu='s' or 'a', jobvt='o')
                    ! m right singular vectors to be overwritten on a and
                    ! m left singular vectors to be computed in u
                    if( lwork>=m*m+max( 4_ilp*m, bdspac ) ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n + m ) + lda*m ) then
                          ! work(iu) is lda by n and work(ir) is lda by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n + m ) + m*m ) then
                          ! work(iu) is lda by n and work(ir) is m by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = m
                       else
                          ! work(iu) is m by chunk and work(ir) is m by m
                          ldwrku = m
                          chunk = ( lwork-m*m-m ) / m
                          ldwrkr = m
                       end if
                       itau = ir + ldwrkr*m
                       iwork = itau + m
                       ! compute a=l*q
                       ! (workspace: need m*m + 2*m, prefer m*m + m + m*nb)
                       call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to u, zeroing about above it
                       call stdlib_dlacpy( 'L', m, m, a, lda, u, ldu )
                       if (m>1_ilp) call stdlib_dlaset( 'U', m-1, m-1, zero, zero, u( 1_ilp, 2_ilp ),ldu )
                       ! generate q in a
                       ! (workspace: need m*m + 2*m, prefer m*m + m + m*nb)
                       call stdlib_dorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + m
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in u, copying result to work(ir)
                       ! (workspace: need m*m + 4*m, prefer m*m + 3*m + 2*m*nb)
                       call stdlib_dgebrd( m, m, u, ldu, s, work( ie ),work( itauq ), work( itaup &
                                 ),work( iwork ), lwork-iwork+1, ierr )
                       call stdlib_dlacpy( 'U', m, m, u, ldu, work( ir ), ldwrkr )
                       ! generate right vectors bidiagonalizing l in work(ir)
                       ! (workspace: need m*m + 4*m-1, prefer m*m + 3*m + (m-1)*nb)
                       call stdlib_dorgbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing l in u
                       ! (workspace: need m*m + 4*m, prefer m*m + 3*m + m*nb)
                       call stdlib_dorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + m
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of l in u, and computing right
                       ! singular vectors of l in work(ir)
                       ! (workspace: need m*m + bdspac)
                       call stdlib_dbdsqr( 'U', m, m, m, 0_ilp, s, work( ie ),work( ir ), ldwrkr, u, &
                                 ldu, dum, 1_ilp,work( iwork ), info )
                       iu = ie + m
                       ! multiply right singular vectors of l in work(ir) by q
                       ! in a, storing result in work(iu) and copying to a
                       ! (workspace: need m*m + 2*m, prefer m*m + m*n + m))
                       do i = 1, n, chunk
                          blk = min( n-i+1, chunk )
                          call stdlib_dgemm( 'N', 'N', m, blk, m, one, work( ir ),ldwrkr, a( 1_ilp, i &
                                    ), lda, zero,work( iu ), ldwrku )
                          call stdlib_dlacpy( 'F', m, blk, work( iu ), ldwrku,a( 1_ilp, i ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       itau = 1_ilp
                       iwork = itau + m
                       ! compute a=l*q
                       ! (workspace: need 2*m, prefer m + m*nb)
                       call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to u, zeroing out above it
                       call stdlib_dlacpy( 'L', m, m, a, lda, u, ldu )
                       if (m>1_ilp) call stdlib_dlaset( 'U', m-1, m-1, zero, zero, u( 1_ilp, 2_ilp ),ldu )
                       ! generate q in a
                       ! (workspace: need 2*m, prefer m + m*nb)
                       call stdlib_dorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = itau
                       itauq = ie + m
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in u
                       ! (workspace: need 4*m, prefer 3*m + 2*m*nb)
                       call stdlib_dgebrd( m, m, u, ldu, s, work( ie ),work( itauq ), work( itaup &
                                 ),work( iwork ), lwork-iwork+1, ierr )
                       ! multiply right vectors bidiagonalizing l by q in a
                       ! (workspace: need 3*m + n, prefer 3*m + n*nb)
                       call stdlib_dormbr( 'P', 'L', 'T', m, n, m, u, ldu,work( itaup ), a, lda, &
                                 work( iwork ),lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing l in u
                       ! (workspace: need 4*m, prefer 3*m + m*nb)
                       call stdlib_dorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       iwork = ie + m
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in u and computing right
                       ! singular vectors of a in a
                       ! (workspace: need bdspac)
                       call stdlib_dbdsqr( 'U', m, n, m, 0_ilp, s, work( ie ), a, lda,u, ldu, dum, 1_ilp, &
                                 work( iwork ), info )
                    end if
                 else if( wntvs ) then
                    if( wntun ) then
                       ! path 4t(n much larger than m, jobu='n', jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! no left singular vectors to be computed
                       if( lwork>=m*m+max( 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(ir) is lda by m
                             ldwrkr = lda
                          else
                             ! work(ir) is m by m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (workspace: need m*m + 2*m, prefer m*m + m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(ir), zeroing out above it
                          call stdlib_dlacpy( 'L', m, m, a, lda, work( ir ),ldwrkr )
                          call stdlib_dlaset( 'U', m-1, m-1, zero, zero,work( ir+ldwrkr ), ldwrkr &
                                    )
                          ! generate q in a
                          ! (workspace: need m*m + 2*m, prefer m*m + m + m*nb)
                          call stdlib_dorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(ir)
                          ! (workspace: need m*m + 4*m, prefer m*m + 3*m + 2*m*nb)
                          call stdlib_dgebrd( m, m, work( ir ), ldwrkr, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate right vectors bidiagonalizing l in
                          ! work(ir)
                          ! (workspace: need m*m + 4*m, prefer m*m + 3*m + (m-1)*nb)
                          call stdlib_dorgbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of l in work(ir)
                          ! (workspace: need m*m + bdspac)
                          call stdlib_dbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, work( ie ),work( ir ), ldwrkr, &
                                    dum, 1_ilp, dum, 1_ilp,work( iwork ), info )
                          ! multiply right singular vectors of l in work(ir) by
                          ! q in a, storing result in vt
                          ! (workspace: need m*m)
                          call stdlib_dgemm( 'N', 'N', m, n, m, one, work( ir ),ldwrkr, a, lda, &
                                    zero, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q
                          ! (workspace: need 2*m, prefer m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy result to vt
                          call stdlib_dlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need 2*m, prefer m + m*nb)
                          call stdlib_dorglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_dlaset( 'U', m-1, m-1, zero, zero, a( 1_ilp, 2_ilp ),lda )
                          ! bidiagonalize l in a
                          ! (workspace: need 4*m, prefer 3*m + 2*m*nb)
                          call stdlib_dgebrd( m, m, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right vectors bidiagonalizing l by q in vt
                          ! (workspace: need 3*m + n, prefer 3*m + n*nb)
                          call stdlib_dormbr( 'P', 'L', 'T', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', m, n, 0_ilp, 0_ilp, s, work( ie ), vt,ldvt, dum, 1_ilp, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    else if( wntuo ) then
                       ! path 5t(n much larger than m, jobu='o', jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! m left singular vectors to be overwritten on a
                       if( lwork>=2_ilp*m*m+max( 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*m ) then
                             ! work(iu) is lda by m and work(ir) is lda by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda + m )*m ) then
                             ! work(iu) is lda by m and work(ir) is m by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          else
                             ! work(iu) is m by m and work(ir) is m by m
                             ldwrku = m
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (workspace: need 2*m*m + 2*m, prefer 2*m*m + m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(iu), zeroing out below it
                          call stdlib_dlacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_dlaset( 'U', m-1, m-1, zero, zero,work( iu+ldwrku ), ldwrku &
                                    )
                          ! generate q in a
                          ! (workspace: need 2*m*m + 2*m, prefer 2*m*m + m + m*nb)
                          call stdlib_dorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to
                          ! work(ir)
                          ! (workspace: need 2*m*m + 4*m,
                                      ! prefer 2*m*m+3*m+2*m*nb)
                          call stdlib_dgebrd( m, m, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, m, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (workspace: need 2*m*m + 4*m-1,
                                      ! prefer 2*m*m+3*m+(m-1)*nb)
                          call stdlib_dorgbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (workspace: need 2*m*m + 4*m, prefer 2*m*m + 3*m + m*nb)
                          call stdlib_dorgbr( 'Q', m, m, m, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in work(ir) and computing
                          ! right singular vectors of l in work(iu)
                          ! (workspace: need 2*m*m + bdspac)
                          call stdlib_dbdsqr( 'U', m, m, m, 0_ilp, s, work( ie ),work( iu ), ldwrku, &
                                    work( ir ),ldwrkr, dum, 1_ilp, work( iwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in a, storing result in vt
                          ! (workspace: need m*m)
                          call stdlib_dgemm( 'N', 'N', m, n, m, one, work( iu ),ldwrku, a, lda, &
                                    zero, vt, ldvt )
                          ! copy left singular vectors of l to a
                          ! (workspace: need m*m)
                          call stdlib_dlacpy( 'F', m, m, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m, prefer m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need 2*m, prefer m + m*nb)
                          call stdlib_dorglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_dlaset( 'U', m-1, m-1, zero, zero, a( 1_ilp, 2_ilp ),lda )
                          ! bidiagonalize l in a
                          ! (workspace: need 4*m, prefer 3*m + 2*m*nb)
                          call stdlib_dgebrd( m, m, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right vectors bidiagonalizing l by q in vt
                          ! (workspace: need 3*m + n, prefer 3*m + n*nb)
                          call stdlib_dormbr( 'P', 'L', 'T', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors of l in a
                          ! (workspace: need 4*m, prefer 3*m + m*nb)
                          call stdlib_dorgbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, compute left
                          ! singular vectors of a in a and compute right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', m, n, m, 0_ilp, s, work( ie ), vt,ldvt, a, lda, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    else if( wntuas ) then
                       ! path 6t(n much larger than m, jobu='s' or 'a',
                               ! jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! m left singular vectors to be computed in u
                       if( lwork>=m*m+max( 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is lda by m
                             ldwrku = m
                          end if
                          itau = iu + ldwrku*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (workspace: need m*m + 2*m, prefer m*m + m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_dlacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_dlaset( 'U', m-1, m-1, zero, zero,work( iu+ldwrku ), ldwrku &
                                    )
                          ! generate q in a
                          ! (workspace: need m*m + 2*m, prefer m*m + m + m*nb)
                          call stdlib_dorglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to u
                          ! (workspace: need m*m + 4*m, prefer m*m + 3*m + 2*m*nb)
                          call stdlib_dgebrd( m, m, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, m, work( iu ), ldwrku, u,ldu )
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (workspace: need m*m + 4*m-1,
                                      ! prefer m*m+3*m+(m-1)*nb)
                          call stdlib_dorgbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (workspace: need m*m + 4*m, prefer m*m + 3*m + m*nb)
                          call stdlib_dorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in u and computing right
                          ! singular vectors of l in work(iu)
                          ! (workspace: need m*m + bdspac)
                          call stdlib_dbdsqr( 'U', m, m, m, 0_ilp, s, work( ie ),work( iu ), ldwrku, &
                                    u, ldu, dum, 1_ilp,work( iwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in a, storing result in vt
                          ! (workspace: need m*m)
                          call stdlib_dgemm( 'N', 'N', m, n, m, one, work( iu ),ldwrku, a, lda, &
                                    zero, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m, prefer m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need 2*m, prefer m + m*nb)
                          call stdlib_dorglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to u, zeroing out above it
                          call stdlib_dlacpy( 'L', m, m, a, lda, u, ldu )
                          if (m>1_ilp) call stdlib_dlaset( 'U', m-1, m-1, zero, zero, u( 1_ilp, 2_ilp ),ldu )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in u
                          ! (workspace: need 4*m, prefer 3*m + 2*m*nb)
                          call stdlib_dgebrd( m, m, u, ldu, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in u by q
                          ! in vt
                          ! (workspace: need 3*m + n, prefer 3*m + n*nb)
                          call stdlib_dormbr( 'P', 'L', 'T', m, n, m, u, ldu,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (workspace: need 4*m, prefer 3*m + m*nb)
                          call stdlib_dorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', m, n, m, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    end if
                 else if( wntva ) then
                    if( wntun ) then
                       ! path 7t(n much larger than m, jobu='n', jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! no left singular vectors to be computed
                       if( lwork>=m*m+max( n + m, 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(ir) is lda by m
                             ldwrkr = lda
                          else
                             ! work(ir) is m by m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need m*m + 2*m, prefer m*m + m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! copy l to work(ir), zeroing out above it
                          call stdlib_dlacpy( 'L', m, m, a, lda, work( ir ),ldwrkr )
                          call stdlib_dlaset( 'U', m-1, m-1, zero, zero,work( ir+ldwrkr ), ldwrkr &
                                    )
                          ! generate q in vt
                          ! (workspace: need m*m + m + n, prefer m*m + m + n*nb)
                          call stdlib_dorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(ir)
                          ! (workspace: need m*m + 4*m, prefer m*m + 3*m + 2*m*nb)
                          call stdlib_dgebrd( m, m, work( ir ), ldwrkr, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (workspace: need m*m + 4*m-1,
                                      ! prefer m*m+3*m+(m-1)*nb)
                          call stdlib_dorgbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of l in work(ir)
                          ! (workspace: need m*m + bdspac)
                          call stdlib_dbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, work( ie ),work( ir ), ldwrkr, &
                                    dum, 1_ilp, dum, 1_ilp,work( iwork ), info )
                          ! multiply right singular vectors of l in work(ir) by
                          ! q in vt, storing result in a
                          ! (workspace: need m*m)
                          call stdlib_dgemm( 'N', 'N', m, n, m, one, work( ir ),ldwrkr, vt, ldvt, &
                                    zero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_dlacpy( 'F', m, n, a, lda, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m, prefer m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need m + n, prefer m + n*nb)
                          call stdlib_dorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_dlaset( 'U', m-1, m-1, zero, zero, a( 1_ilp, 2_ilp ),lda )
                          ! bidiagonalize l in a
                          ! (workspace: need 4*m, prefer 3*m + 2*m*nb)
                          call stdlib_dgebrd( m, m, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in a by q
                          ! in vt
                          ! (workspace: need 3*m + n, prefer 3*m + n*nb)
                          call stdlib_dormbr( 'P', 'L', 'T', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', m, n, 0_ilp, 0_ilp, s, work( ie ), vt,ldvt, dum, 1_ilp, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    else if( wntuo ) then
                       ! path 8t(n much larger than m, jobu='o', jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! m left singular vectors to be overwritten on a
                       if( lwork>=2_ilp*m*m+max( n + m, 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*m ) then
                             ! work(iu) is lda by m and work(ir) is lda by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda + m )*m ) then
                             ! work(iu) is lda by m and work(ir) is m by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          else
                             ! work(iu) is m by m and work(ir) is m by m
                             ldwrku = m
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m*m + 2*m, prefer 2*m*m + m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need 2*m*m + m + n, prefer 2*m*m + m + n*nb)
                          call stdlib_dorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_dlacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_dlaset( 'U', m-1, m-1, zero, zero,work( iu+ldwrku ), ldwrku &
                                    )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to
                          ! work(ir)
                          ! (workspace: need 2*m*m + 4*m,
                                      ! prefer 2*m*m+3*m+2*m*nb)
                          call stdlib_dgebrd( m, m, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, m, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (workspace: need 2*m*m + 4*m-1,
                                      ! prefer 2*m*m+3*m+(m-1)*nb)
                          call stdlib_dorgbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (workspace: need 2*m*m + 4*m, prefer 2*m*m + 3*m + m*nb)
                          call stdlib_dorgbr( 'Q', m, m, m, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in work(ir) and computing
                          ! right singular vectors of l in work(iu)
                          ! (workspace: need 2*m*m + bdspac)
                          call stdlib_dbdsqr( 'U', m, m, m, 0_ilp, s, work( ie ),work( iu ), ldwrku, &
                                    work( ir ),ldwrkr, dum, 1_ilp, work( iwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in vt, storing result in a
                          ! (workspace: need m*m)
                          call stdlib_dgemm( 'N', 'N', m, n, m, one, work( iu ),ldwrku, vt, ldvt, &
                                    zero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_dlacpy( 'F', m, n, a, lda, vt, ldvt )
                          ! copy left singular vectors of a from work(ir) to a
                          call stdlib_dlacpy( 'F', m, m, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m, prefer m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need m + n, prefer m + n*nb)
                          call stdlib_dorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_dlaset( 'U', m-1, m-1, zero, zero, a( 1_ilp, 2_ilp ),lda )
                          ! bidiagonalize l in a
                          ! (workspace: need 4*m, prefer 3*m + 2*m*nb)
                          call stdlib_dgebrd( m, m, a, lda, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in a by q
                          ! in vt
                          ! (workspace: need 3*m + n, prefer 3*m + n*nb)
                          call stdlib_dormbr( 'P', 'L', 'T', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in a
                          ! (workspace: need 4*m, prefer 3*m + m*nb)
                          call stdlib_dorgbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in a and computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', m, n, m, 0_ilp, s, work( ie ), vt,ldvt, a, lda, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    else if( wntuas ) then
                       ! path 9t(n much larger than m, jobu='s' or 'a',
                               ! jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! m left singular vectors to be computed in u
                       if( lwork>=m*m+max( n + m, 4_ilp*m, bdspac ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(iu) is lda by m
                             ldwrku = lda
                          else
                             ! work(iu) is m by m
                             ldwrku = m
                          end if
                          itau = iu + ldwrku*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need m*m + 2*m, prefer m*m + m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need m*m + m + n, prefer m*m + m + n*nb)
                          call stdlib_dorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_dlacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_dlaset( 'U', m-1, m-1, zero, zero,work( iu+ldwrku ), ldwrku &
                                    )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to u
                          ! (workspace: need m*m + 4*m, prefer m*m + 3*m + 2*m*nb)
                          call stdlib_dgebrd( m, m, work( iu ), ldwrku, s,work( ie ), work( itauq &
                                    ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_dlacpy( 'L', m, m, work( iu ), ldwrku, u,ldu )
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (workspace: need m*m + 4*m, prefer m*m + 3*m + (m-1)*nb)
                          call stdlib_dorgbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (workspace: need m*m + 4*m, prefer m*m + 3*m + m*nb)
                          call stdlib_dorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in u and computing right
                          ! singular vectors of l in work(iu)
                          ! (workspace: need m*m + bdspac)
                          call stdlib_dbdsqr( 'U', m, m, m, 0_ilp, s, work( ie ),work( iu ), ldwrku, &
                                    u, ldu, dum, 1_ilp,work( iwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in vt, storing result in a
                          ! (workspace: need m*m)
                          call stdlib_dgemm( 'N', 'N', m, n, m, one, work( iu ),ldwrku, vt, ldvt, &
                                    zero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_dlacpy( 'F', m, n, a, lda, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (workspace: need 2*m, prefer m + m*nb)
                          call stdlib_dgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_dlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (workspace: need m + n, prefer m + n*nb)
                          call stdlib_dorglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to u, zeroing out above it
                          call stdlib_dlacpy( 'L', m, m, a, lda, u, ldu )
                          if (m>1_ilp) call stdlib_dlaset( 'U', m-1, m-1, zero, zero, u( 1_ilp, 2_ilp ),ldu )
                          ie = itau
                          itauq = ie + m
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in u
                          ! (workspace: need 4*m, prefer 3*m + 2*m*nb)
                          call stdlib_dgebrd( m, m, u, ldu, s, work( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in u by q
                          ! in vt
                          ! (workspace: need 3*m + n, prefer 3*m + n*nb)
                          call stdlib_dormbr( 'P', 'L', 'T', m, n, m, u, ldu,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (workspace: need 4*m, prefer 3*m + m*nb)
                          call stdlib_dorgbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          iwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (workspace: need bdspac)
                          call stdlib_dbdsqr( 'U', m, n, m, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, &
                                    dum, 1_ilp, work( iwork ),info )
                       end if
                    end if
                 end if
              else
                 ! n < mnthr
                 ! path 10t(n greater than m, but not much larger)
                 ! reduce to bidiagonal form without lq decomposition
                 ie = 1_ilp
                 itauq = ie + m
                 itaup = itauq + m
                 iwork = itaup + m
                 ! bidiagonalize a
                 ! (workspace: need 3*m + n, prefer 3*m + (m + n)*nb)
                 call stdlib_dgebrd( m, n, a, lda, s, work( ie ), work( itauq ),work( itaup ), &
                           work( iwork ), lwork-iwork+1,ierr )
                 if( wntuas ) then
                    ! if left singular vectors desired in u, copy result to u
                    ! and generate left bidiagonalizing vectors in u
                    ! (workspace: need 4*m-1, prefer 3*m + (m-1)*nb)
                    call stdlib_dlacpy( 'L', m, m, a, lda, u, ldu )
                    call stdlib_dorgbr( 'Q', m, m, n, u, ldu, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvas ) then
                    ! if right singular vectors desired in vt, copy result to
                    ! vt and generate right bidiagonalizing vectors in vt
                    ! (workspace: need 3*m + nrvt, prefer 3*m + nrvt*nb)
                    call stdlib_dlacpy( 'U', m, n, a, lda, vt, ldvt )
                    if( wntva )nrvt = n
                    if( wntvs )nrvt = m
                    call stdlib_dorgbr( 'P', nrvt, n, m, vt, ldvt, work( itaup ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntuo ) then
                    ! if left singular vectors desired in a, generate left
                    ! bidiagonalizing vectors in a
                    ! (workspace: need 4*m-1, prefer 3*m + (m-1)*nb)
                    call stdlib_dorgbr( 'Q', m, m, n, a, lda, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvo ) then
                    ! if right singular vectors desired in a, generate right
                    ! bidiagonalizing vectors in a
                    ! (workspace: need 4*m, prefer 3*m + m*nb)
                    call stdlib_dorgbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 iwork = ie + m
                 if( wntuas .or. wntuo )nru = m
                 if( wntun )nru = 0_ilp
                 if( wntvas .or. wntvo )ncvt = n
                 if( wntvn )ncvt = 0_ilp
                 if( ( .not.wntuo ) .and. ( .not.wntvo ) ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in vt
                    ! (workspace: need bdspac)
                    call stdlib_dbdsqr( 'L', m, ncvt, nru, 0_ilp, s, work( ie ), vt,ldvt, u, ldu, dum,&
                               1_ilp, work( iwork ), info )
                 else if( ( .not.wntuo ) .and. wntvo ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in a
                    ! (workspace: need bdspac)
                    call stdlib_dbdsqr( 'L', m, ncvt, nru, 0_ilp, s, work( ie ), a, lda,u, ldu, dum, &
                              1_ilp, work( iwork ), info )
                 else
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in a and computing right singular
                    ! vectors in vt
                    ! (workspace: need bdspac)
                    call stdlib_dbdsqr( 'L', m, ncvt, nru, 0_ilp, s, work( ie ), vt,ldvt, a, lda, dum,&
                               1_ilp, work( iwork ), info )
                 end if
              end if
           end if
           ! if stdlib_dbdsqr failed to converge, copy unconverged superdiagonals
           ! to work( 2:minmn )
           if( info/=0_ilp ) then
              if( ie>2_ilp ) then
                 do i = 1, minmn - 1
                    work( i+1 ) = work( i+ie-1 )
                 end do
              end if
              if( ie<2_ilp ) then
                 do i = minmn - 1, 1, -1
                    work( i+1 ) = work( i+ie-1 )
                 end do
              end if
           end if
           ! undo scaling if necessary
           if( iscl==1_ilp ) then
              if( anrm>bignum )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,&
                        ierr )
              if( info/=0_ilp .and. anrm>bignum )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn-1,&
                         1_ilp, work( 2_ilp ),minmn, ierr )
              if( anrm<smlnum )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,&
                        ierr )
              if( info/=0_ilp .and. anrm<smlnum )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn-1,&
                         1_ilp, work( 2_ilp ),minmn, ierr )
           end if
           ! return optimal workspace in work(1)
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_dgesvd


     module subroutine stdlib_cgesvd( jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt,work, lwork, rwork, &
     !! CGESVD computes the singular value decomposition (SVD) of a complex
     !! M-by-N matrix A, optionally computing the left and/or right singular
     !! vectors. The SVD is written
     !! A = U * SIGMA * conjugate-transpose(V)
     !! where SIGMA is an M-by-N matrix which is zero except for its
     !! min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
     !! V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
     !! are the singular values of A; they are real and non-negative, and
     !! are returned in descending order.  The first min(m,n) columns of
     !! U and V are the left and right singular vectors of A.
     !! Note that the routine returns V**H, not V.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu, jobvt
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*), s(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: u(ldu,*), vt(ldvt,*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, wntua, wntuas, wntun, wntuo, wntus, wntva, wntvas, wntvn, wntvo,&
                      wntvs
           integer(ilp) :: blk, chunk, i, ie, ierr, ir, irwork, iscl, itau, itaup, itauq, iu, &
           iwork, ldwrkr, ldwrku, maxwrk, minmn, minwrk, mnthr, ncu, ncvt, nru, nrvt, &
                     wrkbl
           integer(ilp) :: lwork_cgeqrf, lwork_cungqr_n, lwork_cungqr_m, lwork_cgebrd, &
                     lwork_cungbr_p, lwork_cungbr_q, lwork_cgelqf, lwork_cunglq_n, lwork_cunglq_m
           real(sp) :: anrm, bignum, eps, smlnum
           ! Local Arrays 
           real(sp) :: dum(1_ilp)
           complex(sp) :: cdum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           minmn = min( m, n )
           wntua = stdlib_lsame( jobu, 'A' )
           wntus = stdlib_lsame( jobu, 'S' )
           wntuas = wntua .or. wntus
           wntuo = stdlib_lsame( jobu, 'O' )
           wntun = stdlib_lsame( jobu, 'N' )
           wntva = stdlib_lsame( jobvt, 'A' )
           wntvs = stdlib_lsame( jobvt, 'S' )
           wntvas = wntva .or. wntvs
           wntvo = stdlib_lsame( jobvt, 'O' )
           wntvn = stdlib_lsame( jobvt, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.( wntua .or. wntus .or. wntuo .or. wntun ) ) then
              info = -1_ilp
           else if( .not.( wntva .or. wntvs .or. wntvo .or. wntvn ) .or.( wntvo .and. wntuo ) ) &
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldu<1_ilp .or. ( wntuas .and. ldu<m ) ) then
              info = -9_ilp
           else if( ldvt<1_ilp .or. ( wntva .and. ldvt<n ) .or.( wntvs .and. ldvt<minmn ) ) &
                     then
              info = -11_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace to
             ! real workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              minwrk = 1_ilp
              maxwrk = 1_ilp
              if( m>=n .and. minmn>0_ilp ) then
                 ! space needed for stdlib_zbdsqr is bdspac = 5*n
                 mnthr = stdlib_ilaenv( 6_ilp, 'CGESVD', jobu // jobvt, m, n, 0_ilp, 0_ilp )
                 ! compute space needed for stdlib_cgeqrf
                 call stdlib_cgeqrf( m, n, a, lda, cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                 lwork_cgeqrf = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_cungqr
                 call stdlib_cungqr( m, n, n, a, lda, cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                 lwork_cungqr_n = int( cdum(1_ilp),KIND=ilp)
                 call stdlib_cungqr( m, m, n, a, lda, cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                 lwork_cungqr_m = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_cgebrd
                 call stdlib_cgebrd( n, n, a, lda, s, dum(1_ilp), cdum(1_ilp),cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                           
                 lwork_cgebrd = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_cungbr
                 call stdlib_cungbr( 'P', n, n, n, a, lda, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                 lwork_cungbr_p = int( cdum(1_ilp),KIND=ilp)
                 call stdlib_cungbr( 'Q', n, n, n, a, lda, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                 lwork_cungbr_q = int( cdum(1_ilp),KIND=ilp)
                 mnthr = stdlib_ilaenv( 6_ilp, 'CGESVD', jobu // jobvt, m, n, 0_ilp, 0_ilp )
                 if( m>=mnthr ) then
                    if( wntun ) then
                       ! path 1 (m much larger than n, jobu='n')
                       maxwrk = n + lwork_cgeqrf
                       maxwrk = max( maxwrk, 2_ilp*n+lwork_cgebrd )
                       if( wntvo .or. wntvas )maxwrk = max( maxwrk, 2_ilp*n+lwork_cungbr_p )
                       minwrk = 3_ilp*n
                    else if( wntuo .and. wntvn ) then
                       ! path 2 (m much larger than n, jobu='o', jobvt='n')
                       wrkbl = n + lwork_cgeqrf
                       wrkbl = max( wrkbl, n+lwork_cungqr_n )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_q )
                       maxwrk = max( n*n+wrkbl, n*n+m*n )
                       minwrk = 2_ilp*n + m
                    else if( wntuo .and. wntvas ) then
                       ! path 3 (m much larger than n, jobu='o', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_cgeqrf
                       wrkbl = max( wrkbl, n+lwork_cungqr_n )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_q )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_p )
                       maxwrk = max( n*n+wrkbl, n*n+m*n )
                       minwrk = 2_ilp*n + m
                    else if( wntus .and. wntvn ) then
                       ! path 4 (m much larger than n, jobu='s', jobvt='n')
                       wrkbl = n + lwork_cgeqrf
                       wrkbl = max( wrkbl, n+lwork_cungqr_n )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_q )
                       maxwrk = n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    else if( wntus .and. wntvo ) then
                       ! path 5 (m much larger than n, jobu='s', jobvt='o')
                       wrkbl = n + lwork_cgeqrf
                       wrkbl = max( wrkbl, n+lwork_cungqr_n )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_q )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_p )
                       maxwrk = 2_ilp*n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    else if( wntus .and. wntvas ) then
                       ! path 6 (m much larger than n, jobu='s', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_cgeqrf
                       wrkbl = max( wrkbl, n+lwork_cungqr_n )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_q )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_p )
                       maxwrk = n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    else if( wntua .and. wntvn ) then
                       ! path 7 (m much larger than n, jobu='a', jobvt='n')
                       wrkbl = n + lwork_cgeqrf
                       wrkbl = max( wrkbl, n+lwork_cungqr_m )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_q )
                       maxwrk = n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    else if( wntua .and. wntvo ) then
                       ! path 8 (m much larger than n, jobu='a', jobvt='o')
                       wrkbl = n + lwork_cgeqrf
                       wrkbl = max( wrkbl, n+lwork_cungqr_m )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_q )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_p )
                       maxwrk = 2_ilp*n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    else if( wntua .and. wntvas ) then
                       ! path 9 (m much larger than n, jobu='a', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_cgeqrf
                       wrkbl = max( wrkbl, n+lwork_cungqr_m )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_q )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_cungbr_p )
                       maxwrk = n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    end if
                 else
                    ! path 10 (m at least n, but not much larger)
                    call stdlib_cgebrd( m, n, a, lda, s, dum(1_ilp), cdum(1_ilp),cdum(1_ilp), cdum(1_ilp), -1_ilp, &
                              ierr )
                    lwork_cgebrd = int( cdum(1_ilp),KIND=ilp)
                    maxwrk = 2_ilp*n + lwork_cgebrd
                    if( wntus .or. wntuo ) then
                       call stdlib_cungbr( 'Q', m, n, n, a, lda, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                                 
                       lwork_cungbr_q = int( cdum(1_ilp),KIND=ilp)
                       maxwrk = max( maxwrk, 2_ilp*n+lwork_cungbr_q )
                    end if
                    if( wntua ) then
                       call stdlib_cungbr( 'Q', m, m, n, a, lda, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                                 
                       lwork_cungbr_q = int( cdum(1_ilp),KIND=ilp)
                       maxwrk = max( maxwrk, 2_ilp*n+lwork_cungbr_q )
                    end if
                    if( .not.wntvn ) then
                       maxwrk = max( maxwrk, 2_ilp*n+lwork_cungbr_p )
                    end if
                    minwrk = 2_ilp*n + m
                 end if
              else if( minmn>0_ilp ) then
                 ! space needed for stdlib_cbdsqr is bdspac = 5*m
                 mnthr = stdlib_ilaenv( 6_ilp, 'CGESVD', jobu // jobvt, m, n, 0_ilp, 0_ilp )
                 ! compute space needed for stdlib_cgelqf
                 call stdlib_cgelqf( m, n, a, lda, cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                 lwork_cgelqf = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_cunglq
                 call stdlib_cunglq( n, n, m, cdum(1_ilp), n, cdum(1_ilp), cdum(1_ilp), -1_ilp,ierr )
                 lwork_cunglq_n = int( cdum(1_ilp),KIND=ilp)
                 call stdlib_cunglq( m, n, m, a, lda, cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                 lwork_cunglq_m = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_cgebrd
                 call stdlib_cgebrd( m, m, a, lda, s, dum(1_ilp), cdum(1_ilp),cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                           
                 lwork_cgebrd = int( cdum(1_ilp),KIND=ilp)
                  ! compute space needed for stdlib_cungbr p
                 call stdlib_cungbr( 'P', m, m, m, a, n, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                 lwork_cungbr_p = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_cungbr q
                 call stdlib_cungbr( 'Q', m, m, m, a, n, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                 lwork_cungbr_q = int( cdum(1_ilp),KIND=ilp)
                 if( n>=mnthr ) then
                    if( wntvn ) then
                       ! path 1t(n much larger than m, jobvt='n')
                       maxwrk = m + lwork_cgelqf
                       maxwrk = max( maxwrk, 2_ilp*m+lwork_cgebrd )
                       if( wntuo .or. wntuas )maxwrk = max( maxwrk, 2_ilp*m+lwork_cungbr_q )
                       minwrk = 3_ilp*m
                    else if( wntvo .and. wntun ) then
                       ! path 2t(n much larger than m, jobu='n', jobvt='o')
                       wrkbl = m + lwork_cgelqf
                       wrkbl = max( wrkbl, m+lwork_cunglq_m )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_p )
                       maxwrk = max( m*m+wrkbl, m*m+m*n )
                       minwrk = 2_ilp*m + n
                    else if( wntvo .and. wntuas ) then
                       ! path 3t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='o')
                       wrkbl = m + lwork_cgelqf
                       wrkbl = max( wrkbl, m+lwork_cunglq_m )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_p )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_q )
                       maxwrk = max( m*m+wrkbl, m*m+m*n )
                       minwrk = 2_ilp*m + n
                    else if( wntvs .and. wntun ) then
                       ! path 4t(n much larger than m, jobu='n', jobvt='s')
                       wrkbl = m + lwork_cgelqf
                       wrkbl = max( wrkbl, m+lwork_cunglq_m )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_p )
                       maxwrk = m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    else if( wntvs .and. wntuo ) then
                       ! path 5t(n much larger than m, jobu='o', jobvt='s')
                       wrkbl = m + lwork_cgelqf
                       wrkbl = max( wrkbl, m+lwork_cunglq_m )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_p )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_q )
                       maxwrk = 2_ilp*m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    else if( wntvs .and. wntuas ) then
                       ! path 6t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='s')
                       wrkbl = m + lwork_cgelqf
                       wrkbl = max( wrkbl, m+lwork_cunglq_m )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_p )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_q )
                       maxwrk = m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    else if( wntva .and. wntun ) then
                       ! path 7t(n much larger than m, jobu='n', jobvt='a')
                       wrkbl = m + lwork_cgelqf
                       wrkbl = max( wrkbl, m+lwork_cunglq_n )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_p )
                       maxwrk = m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    else if( wntva .and. wntuo ) then
                       ! path 8t(n much larger than m, jobu='o', jobvt='a')
                       wrkbl = m + lwork_cgelqf
                       wrkbl = max( wrkbl, m+lwork_cunglq_n )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_p )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_q )
                       maxwrk = 2_ilp*m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    else if( wntva .and. wntuas ) then
                       ! path 9t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='a')
                       wrkbl = m + lwork_cgelqf
                       wrkbl = max( wrkbl, m+lwork_cunglq_n )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_p )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_cungbr_q )
                       maxwrk = m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    end if
                 else
                    ! path 10t(n greater than m, but not much larger)
                    call stdlib_cgebrd( m, n, a, lda, s, dum(1_ilp), cdum(1_ilp),cdum(1_ilp), cdum(1_ilp), -1_ilp, &
                              ierr )
                    lwork_cgebrd = int( cdum(1_ilp),KIND=ilp)
                    maxwrk = 2_ilp*m + lwork_cgebrd
                    if( wntvs .or. wntvo ) then
                      ! compute space needed for stdlib_cungbr p
                      call stdlib_cungbr( 'P', m, n, m, a, n, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                      lwork_cungbr_p = int( cdum(1_ilp),KIND=ilp)
                      maxwrk = max( maxwrk, 2_ilp*m+lwork_cungbr_p )
                    end if
                    if( wntva ) then
                      call stdlib_cungbr( 'P', n,  n, m, a, n, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                      lwork_cungbr_p = int( cdum(1_ilp),KIND=ilp)
                      maxwrk = max( maxwrk, 2_ilp*m+lwork_cungbr_p )
                    end if
                    if( .not.wntun ) then
                       maxwrk = max( maxwrk, 2_ilp*m+lwork_cungbr_q )
                    end if
                    minwrk = 2_ilp*m + n
                 end if
              end if
              maxwrk = max( minwrk, maxwrk )
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGESVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              return
           end if
           ! get machine constants
           eps = stdlib_slamch( 'P' )
           smlnum = sqrt( stdlib_slamch( 'S' ) ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_clange( 'M', m, n, a, lda, dum )
           iscl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              iscl = 1_ilp
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, ierr )
           else if( anrm>bignum ) then
              iscl = 1_ilp
              call stdlib_clascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, ierr )
           end if
           if( m>=n ) then
              ! a has at least as many rows as columns. if a has sufficiently
              ! more rows than columns, first reduce using the qr
              ! decomposition (if sufficient workspace available)
              if( m>=mnthr ) then
                 if( wntun ) then
                    ! path 1 (m much larger than n, jobu='n')
                    ! no left singular vectors to be computed
                    itau = 1_ilp
                    iwork = itau + n
                    ! compute a=q*r
                    ! (cworkspace: need 2*n, prefer n+n*nb)
                    ! (rworkspace: need 0)
                    call stdlib_cgeqrf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                              ierr )
                    ! zero out below r
                    if( n > 1_ilp ) then
                       call stdlib_claset( 'L', n-1, n-1, czero, czero, a( 2_ilp, 1_ilp ),lda )
                    end if
                    ie = 1_ilp
                    itauq = 1_ilp
                    itaup = itauq + n
                    iwork = itaup + n
                    ! bidiagonalize r in a
                    ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                    ! (rworkspace: need n)
                    call stdlib_cgebrd( n, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ),&
                               work( iwork ), lwork-iwork+1,ierr )
                    ncvt = 0_ilp
                    if( wntvo .or. wntvas ) then
                       ! if right singular vectors desired, generate p'.
                       ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       ncvt = n
                    end if
                    irwork = ie + n
                    ! perform bidiagonal qr iteration, computing right
                    ! singular vectors of a in a if desired
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_cbdsqr( 'U', n, ncvt, 0_ilp, 0_ilp, s, rwork( ie ), a, lda,cdum, 1_ilp, cdum, &
                              1_ilp, rwork( irwork ), info )
                    ! if right singular vectors desired in vt, copy them there
                    if( wntvas )call stdlib_clacpy( 'F', n, n, a, lda, vt, ldvt )
                 else if( wntuo .and. wntvn ) then
                    ! path 2 (m much larger than n, jobu='o', jobvt='n')
                    ! n left singular vectors to be overwritten on a and
                    ! no right singular vectors to be computed
                    if( lwork>=n*n+3*n ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n )+lda*n ) then
                          ! work(iu) is lda by n, work(ir) is lda by n
                          ldwrku = lda
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n )+n*n ) then
                          ! work(iu) is lda by n, work(ir) is n by n
                          ldwrku = lda
                          ldwrkr = n
                       else
                          ! work(iu) is ldwrku by n, work(ir) is n by n
                          ldwrku = ( lwork-n*n ) / n
                          ldwrkr = n
                       end if
                       itau = ir + ldwrkr*n
                       iwork = itau + n
                       ! compute a=q*r
                       ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to work(ir) and zero out below it
                       call stdlib_clacpy( 'U', n, n, a, lda, work( ir ), ldwrkr )
                       call stdlib_claset( 'L', n-1, n-1, czero, czero,work( ir+1 ), ldwrkr )
                                 
                       ! generate q in a
                       ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in work(ir)
                       ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                       ! (rworkspace: need n)
                       call stdlib_cgebrd( n, n, work( ir ), ldwrkr, s, rwork( ie ),work( itauq ),&
                                  work( itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing r
                       ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                       ! (rworkspace: need 0)
                       call stdlib_cungbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       irwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of r in work(ir)
                       ! (cworkspace: need n*n)
                       ! (rworkspace: need bdspac)
                       call stdlib_cbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, rwork( ie ), cdum, 1_ilp,work( ir ), &
                                 ldwrkr, cdum, 1_ilp,rwork( irwork ), info )
                       iu = itauq
                       ! multiply q in a by left singular vectors of r in
                       ! work(ir), storing result in work(iu) and copying to a
                       ! (cworkspace: need n*n+n, prefer n*n+m*n)
                       ! (rworkspace: 0)
                       do i = 1, m, ldwrku
                          chunk = min( m-i+1, ldwrku )
                          call stdlib_cgemm( 'N', 'N', chunk, n, n, cone, a( i, 1_ilp ),lda, work( ir &
                                    ), ldwrkr, czero,work( iu ), ldwrku )
                          call stdlib_clacpy( 'F', chunk, n, work( iu ), ldwrku,a( i, 1_ilp ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       ie = 1_ilp
                       itauq = 1_ilp
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize a
                       ! (cworkspace: need 2*n+m, prefer 2*n+(m+n)*nb)
                       ! (rworkspace: n)
                       call stdlib_cgebrd( m, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing a
                       ! (cworkspace: need 3*n, prefer 2*n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungbr( 'Q', m, n, n, a, lda, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in a
                       ! (cworkspace: need 0)
                       ! (rworkspace: need bdspac)
                       call stdlib_cbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, rwork( ie ), cdum, 1_ilp,a, lda, cdum, &
                                 1_ilp, rwork( irwork ), info )
                    end if
                 else if( wntuo .and. wntvas ) then
                    ! path 3 (m much larger than n, jobu='o', jobvt='s' or 'a')
                    ! n left singular vectors to be overwritten on a and
                    ! n right singular vectors to be computed in vt
                    if( lwork>=n*n+3*n ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n )+lda*n ) then
                          ! work(iu) is lda by n and work(ir) is lda by n
                          ldwrku = lda
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n )+n*n ) then
                          ! work(iu) is lda by n and work(ir) is n by n
                          ldwrku = lda
                          ldwrkr = n
                       else
                          ! work(iu) is ldwrku by n and work(ir) is n by n
                          ldwrku = ( lwork-n*n ) / n
                          ldwrkr = n
                       end if
                       itau = ir + ldwrkr*n
                       iwork = itau + n
                       ! compute a=q*r
                       ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to vt, zeroing out below it
                       call stdlib_clacpy( 'U', n, n, a, lda, vt, ldvt )
                       if( n>1_ilp )call stdlib_claset( 'L', n-1, n-1, czero, czero,vt( 2_ilp, 1_ilp ), ldvt )
                                 
                       ! generate q in a
                       ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in vt, copying result to work(ir)
                       ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                       ! (rworkspace: need n)
                       call stdlib_cgebrd( n, n, vt, ldvt, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       call stdlib_clacpy( 'L', n, n, vt, ldvt, work( ir ), ldwrkr )
                       ! generate left vectors bidiagonalizing r in work(ir)
                       ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing r in vt
                       ! (cworkspace: need n*n+3*n-1, prefer n*n+2*n+(n-1)*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of r in work(ir) and computing right
                       ! singular vectors of r in vt
                       ! (cworkspace: need n*n)
                       ! (rworkspace: need bdspac)
                       call stdlib_cbdsqr( 'U', n, n, n, 0_ilp, s, rwork( ie ), vt,ldvt, work( ir ), &
                                 ldwrkr, cdum, 1_ilp,rwork( irwork ), info )
                       iu = itauq
                       ! multiply q in a by left singular vectors of r in
                       ! work(ir), storing result in work(iu) and copying to a
                       ! (cworkspace: need n*n+n, prefer n*n+m*n)
                       ! (rworkspace: 0)
                       do i = 1, m, ldwrku
                          chunk = min( m-i+1, ldwrku )
                          call stdlib_cgemm( 'N', 'N', chunk, n, n, cone, a( i, 1_ilp ),lda, work( ir &
                                    ), ldwrkr, czero,work( iu ), ldwrku )
                          call stdlib_clacpy( 'F', chunk, n, work( iu ), ldwrku,a( i, 1_ilp ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       itau = 1_ilp
                       iwork = itau + n
                       ! compute a=q*r
                       ! (cworkspace: need 2*n, prefer n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to vt, zeroing out below it
                       call stdlib_clacpy( 'U', n, n, a, lda, vt, ldvt )
                       if( n>1_ilp )call stdlib_claset( 'L', n-1, n-1, czero, czero,vt( 2_ilp, 1_ilp ), ldvt )
                                 
                       ! generate q in a
                       ! (cworkspace: need 2*n, prefer n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in vt
                       ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                       ! (rworkspace: n)
                       call stdlib_cgebrd( n, n, vt, ldvt, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! multiply q in a by left vectors bidiagonalizing r
                       ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_cunmbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), a, lda,&
                                  work( iwork ),lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing r in vt
                       ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in a and computing right
                       ! singular vectors of a in vt
                       ! (cworkspace: 0)
                       ! (rworkspace: need bdspac)
                       call stdlib_cbdsqr( 'U', n, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, a, lda, cdum,&
                                  1_ilp, rwork( irwork ),info )
                    end if
                 else if( wntus ) then
                    if( wntvn ) then
                       ! path 4 (m much larger than n, jobu='s', jobvt='n')
                       ! n left singular vectors to be computed in u and
                       ! no right singular vectors to be computed
                       if( lwork>=n*n+3*n ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(ir) is lda by n
                             ldwrkr = lda
                          else
                             ! work(ir) is n by n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(ir), zeroing out below it
                          call stdlib_clacpy( 'U', n, n, a, lda, work( ir ),ldwrkr )
                          call stdlib_claset( 'L', n-1, n-1, czero, czero,work( ir+1 ), ldwrkr )
                                    
                          ! generate q in a
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(ir)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_cgebrd( n, n, work( ir ), ldwrkr, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate left vectors bidiagonalizing r in work(ir)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(ir)
                          ! (cworkspace: need n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, rwork( ie ), cdum,1_ilp, work( ir ),&
                                     ldwrkr, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(ir), storing result in u
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, n, cone, a, lda,work( ir ), ldwrkr, &
                                    czero, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_claset( 'L', n-1, n-1, czero, czero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_cgebrd( n, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left vectors bidiagonalizing r
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, rwork( ie ), cdum,1_ilp, u, ldu, &
                                    cdum, 1_ilp, rwork( irwork ),info )
                       end if
                    else if( wntvo ) then
                       ! path 5 (m much larger than n, jobu='s', jobvt='o')
                       ! n left singular vectors to be computed in u and
                       ! n right singular vectors to be overwritten on a
                       if( lwork>=2_ilp*n*n+3*n ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*n ) then
                             ! work(iu) is lda by n and work(ir) is lda by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+n )*n ) then
                             ! work(iu) is lda by n and work(ir) is n by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          else
                             ! work(iu) is n by n and work(ir) is n by n
                             ldwrku = n
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (cworkspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_clacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_claset( 'L', n-1, n-1, czero, czero,work( iu+1 ), ldwrku )
                                    
                          ! generate q in a
                          ! (cworkspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to
                          ! work(ir)
                          ! (cworkspace: need   2*n*n+3*n,
                                       ! prefer 2*n*n+2*n+2*n*nb)
                          ! (rworkspace: need   n)
                          call stdlib_cgebrd( n, n, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_clacpy( 'U', n, n, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need 2*n*n+3*n, prefer 2*n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need   2*n*n+3*n-1,
                                       ! prefer 2*n*n+2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', n, n, n, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in work(ir)
                          ! (cworkspace: need 2*n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, n, n, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, &
                                    work( iu ),ldwrku, cdum, 1_ilp, rwork( irwork ),info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(iu), storing result in u
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, n, cone, a, lda,work( iu ), ldwrku, &
                                    czero, u, ldu )
                          ! copy right singular vectors of r to a
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_clacpy( 'F', n, n, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_claset( 'L', n-1, n-1, czero, czero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_cgebrd( n, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left vectors bidiagonalizing r
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right vectors bidiagonalizing r in a
                          ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in a
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, n, m, 0_ilp, s, rwork( ie ), a,lda, u, ldu, &
                                    cdum, 1_ilp, rwork( irwork ),info )
                       end if
                    else if( wntvas ) then
                       ! path 6 (m much larger than n, jobu='s', jobvt='s'
                               ! or 'a')
                       ! n left singular vectors to be computed in u and
                       ! n right singular vectors to be computed in vt
                       if( lwork>=n*n+3*n ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is n by n
                             ldwrku = n
                          end if
                          itau = iu + ldwrku*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_clacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_claset( 'L', n-1, n-1, czero, czero,work( iu+1 ), ldwrku )
                                    
                          ! generate q in a
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to vt
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_cgebrd( n, n, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_clacpy( 'U', n, n, work( iu ), ldwrku, vt,ldvt )
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (cworkspace: need   n*n+3*n-1,
                                       ! prefer n*n+2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in vt
                          ! (cworkspace: need n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, n, n, 0_ilp, s, rwork( ie ), vt,ldvt, work( iu )&
                                    , ldwrku, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(iu), storing result in u
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, n, cone, a, lda,work( iu ), ldwrku, &
                                    czero, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to vt, zeroing out below it
                          call stdlib_clacpy( 'U', n, n, a, lda, vt, ldvt )
                          if( n>1_ilp )call stdlib_claset( 'L', n-1, n-1, czero, czero,vt( 2_ilp, 1_ilp ), &
                                    ldvt )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in vt
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_cgebrd( n, n, vt, ldvt, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in vt
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    end if
                 else if( wntua ) then
                    if( wntvn ) then
                       ! path 7 (m much larger than n, jobu='a', jobvt='n')
                       ! m left singular vectors to be computed in u and
                       ! no right singular vectors to be computed
                       if( lwork>=n*n+max( n+m, 3_ilp*n ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(ir) is lda by n
                             ldwrkr = lda
                          else
                             ! work(ir) is n by n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, n, a, lda, u, ldu )
                          ! copy r to work(ir), zeroing out below it
                          call stdlib_clacpy( 'U', n, n, a, lda, work( ir ),ldwrkr )
                          call stdlib_claset( 'L', n-1, n-1, czero, czero,work( ir+1 ), ldwrkr )
                                    
                          ! generate q in u
                          ! (cworkspace: need n*n+n+m, prefer n*n+n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(ir)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_cgebrd( n, n, work( ir ), ldwrkr, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(ir)
                          ! (cworkspace: need n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, rwork( ie ), cdum,1_ilp, work( ir ),&
                                     ldwrkr, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(ir), storing result in a
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, n, cone, u, ldu,work( ir ), ldwrkr, &
                                    czero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_clacpy( 'F', m, n, a, lda, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need n+m, prefer n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_claset( 'L', n-1, n-1, czero, czero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_cgebrd( n, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in a
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, rwork( ie ), cdum,1_ilp, u, ldu, &
                                    cdum, 1_ilp, rwork( irwork ),info )
                       end if
                    else if( wntvo ) then
                       ! path 8 (m much larger than n, jobu='a', jobvt='o')
                       ! m left singular vectors to be computed in u and
                       ! n right singular vectors to be overwritten on a
                       if( lwork>=2_ilp*n*n+max( n+m, 3_ilp*n ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*n ) then
                             ! work(iu) is lda by n and work(ir) is lda by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+n )*n ) then
                             ! work(iu) is lda by n and work(ir) is n by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          else
                             ! work(iu) is n by n and work(ir) is n by n
                             ldwrku = n
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need 2*n*n+n+m, prefer 2*n*n+n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_clacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_claset( 'L', n-1, n-1, czero, czero,work( iu+1 ), ldwrku )
                                    
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to
                          ! work(ir)
                          ! (cworkspace: need   2*n*n+3*n,
                                       ! prefer 2*n*n+2*n+2*n*nb)
                          ! (rworkspace: need   n)
                          call stdlib_cgebrd( n, n, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_clacpy( 'U', n, n, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need 2*n*n+3*n, prefer 2*n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need   2*n*n+3*n-1,
                                       ! prefer 2*n*n+2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', n, n, n, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in work(ir)
                          ! (cworkspace: need 2*n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, n, n, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, &
                                    work( iu ),ldwrku, cdum, 1_ilp, rwork( irwork ),info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(iu), storing result in a
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, n, cone, u, ldu,work( iu ), ldwrku, &
                                    czero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_clacpy( 'F', m, n, a, lda, u, ldu )
                          ! copy right singular vectors of r from work(ir) to a
                          call stdlib_clacpy( 'F', n, n, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need n+m, prefer n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_claset( 'L', n-1, n-1, czero, czero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_cgebrd( n, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in a
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in a
                          ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in a
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, n, m, 0_ilp, s, rwork( ie ), a,lda, u, ldu, &
                                    cdum, 1_ilp, rwork( irwork ),info )
                       end if
                    else if( wntvas ) then
                       ! path 9 (m much larger than n, jobu='a', jobvt='s'
                               ! or 'a')
                       ! m left singular vectors to be computed in u and
                       ! n right singular vectors to be computed in vt
                       if( lwork>=n*n+max( n+m, 3_ilp*n ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is n by n
                             ldwrku = n
                          end if
                          itau = iu + ldwrku*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need n*n+n+m, prefer n*n+n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_clacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_claset( 'L', n-1, n-1, czero, czero,work( iu+1 ), ldwrku )
                                    
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to vt
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_cgebrd( n, n, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_clacpy( 'U', n, n, work( iu ), ldwrku, vt,ldvt )
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (cworkspace: need   n*n+3*n-1,
                                       ! prefer n*n+2*n+(n-1)*nb)
                          ! (rworkspace: need   0)
                          call stdlib_cungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in vt
                          ! (cworkspace: need n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, n, n, 0_ilp, s, rwork( ie ), vt,ldvt, work( iu )&
                                    , ldwrku, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(iu), storing result in a
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, n, cone, u, ldu,work( iu ), ldwrku, &
                                    czero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_clacpy( 'F', m, n, a, lda, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need n+m, prefer n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r from a to vt, zeroing out below it
                          call stdlib_clacpy( 'U', n, n, a, lda, vt, ldvt )
                          if( n>1_ilp )call stdlib_claset( 'L', n-1, n-1, czero, czero,vt( 2_ilp, 1_ilp ), &
                                    ldvt )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in vt
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_cgebrd( n, n, vt, ldvt, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in vt
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', n, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    end if
                 end if
              else
                 ! m < mnthr
                 ! path 10 (m at least n, but not much larger)
                 ! reduce to bidiagonal form without qr decomposition
                 ie = 1_ilp
                 itauq = 1_ilp
                 itaup = itauq + n
                 iwork = itaup + n
                 ! bidiagonalize a
                 ! (cworkspace: need 2*n+m, prefer 2*n+(m+n)*nb)
                 ! (rworkspace: need n)
                 call stdlib_cgebrd( m, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), &
                           work( iwork ), lwork-iwork+1,ierr )
                 if( wntuas ) then
                    ! if left singular vectors desired in u, copy result to u
                    ! and generate left bidiagonalizing vectors in u
                    ! (cworkspace: need 2*n+ncu, prefer 2*n+ncu*nb)
                    ! (rworkspace: 0)
                    call stdlib_clacpy( 'L', m, n, a, lda, u, ldu )
                    if( wntus )ncu = n
                    if( wntua )ncu = m
                    call stdlib_cungbr( 'Q', m, ncu, n, u, ldu, work( itauq ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntvas ) then
                    ! if right singular vectors desired in vt, copy result to
                    ! vt and generate right bidiagonalizing vectors in vt
                    ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                    ! (rworkspace: 0)
                    call stdlib_clacpy( 'U', n, n, a, lda, vt, ldvt )
                    call stdlib_cungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntuo ) then
                    ! if left singular vectors desired in a, generate left
                    ! bidiagonalizing vectors in a
                    ! (cworkspace: need 3*n, prefer 2*n+n*nb)
                    ! (rworkspace: 0)
                    call stdlib_cungbr( 'Q', m, n, n, a, lda, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvo ) then
                    ! if right singular vectors desired in a, generate right
                    ! bidiagonalizing vectors in a
                    ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                    ! (rworkspace: 0)
                    call stdlib_cungbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 irwork = ie + n
                 if( wntuas .or. wntuo )nru = m
                 if( wntun )nru = 0_ilp
                 if( wntvas .or. wntvo )ncvt = n
                 if( wntvn )ncvt = 0_ilp
                 if( ( .not.wntuo ) .and. ( .not.wntvo ) ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in vt
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_cbdsqr( 'U', n, ncvt, nru, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                              cdum, 1_ilp, rwork( irwork ),info )
                 else if( ( .not.wntuo ) .and. wntvo ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in a
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_cbdsqr( 'U', n, ncvt, nru, 0_ilp, s, rwork( ie ), a,lda, u, ldu, cdum,&
                               1_ilp, rwork( irwork ),info )
                 else
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in a and computing right singular
                    ! vectors in vt
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_cbdsqr( 'U', n, ncvt, nru, 0_ilp, s, rwork( ie ), vt,ldvt, a, lda, &
                              cdum, 1_ilp, rwork( irwork ),info )
                 end if
              end if
           else
              ! a has more columns than rows. if a has sufficiently more
              ! columns than rows, first reduce using the lq decomposition (if
              ! sufficient workspace available)
              if( n>=mnthr ) then
                 if( wntvn ) then
                    ! path 1t(n much larger than m, jobvt='n')
                    ! no right singular vectors to be computed
                    itau = 1_ilp
                    iwork = itau + m
                    ! compute a=l*q
                    ! (cworkspace: need 2*m, prefer m+m*nb)
                    ! (rworkspace: 0)
                    call stdlib_cgelqf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                              ierr )
                    ! zero out above l
                    if (m>1_ilp) call stdlib_claset( 'U', m-1, m-1, czero, czero, a( 1_ilp, 2_ilp ),lda )
                    ie = 1_ilp
                    itauq = 1_ilp
                    itaup = itauq + m
                    iwork = itaup + m
                    ! bidiagonalize l in a
                    ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                    ! (rworkspace: need m)
                    call stdlib_cgebrd( m, m, a, lda, s, rwork( ie ), work( itauq ),work( itaup ),&
                               work( iwork ), lwork-iwork+1,ierr )
                    if( wntuo .or. wntuas ) then
                       ! if left singular vectors desired, generate q
                       ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                    end if
                    irwork = ie + m
                    nru = 0_ilp
                    if( wntuo .or. wntuas )nru = m
                    ! perform bidiagonal qr iteration, computing left singular
                    ! vectors of a in a if desired
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_cbdsqr( 'U', m, 0_ilp, nru, 0_ilp, s, rwork( ie ), cdum, 1_ilp,a, lda, cdum, &
                              1_ilp, rwork( irwork ), info )
                    ! if left singular vectors desired in u, copy them there
                    if( wntuas )call stdlib_clacpy( 'F', m, m, a, lda, u, ldu )
                 else if( wntvo .and. wntun ) then
                    ! path 2t(n much larger than m, jobu='n', jobvt='o')
                    ! m right singular vectors to be overwritten on a and
                    ! no left singular vectors to be computed
                    if( lwork>=m*m+3*m ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n )+lda*m ) then
                          ! work(iu) is lda by n and work(ir) is lda by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n )+m*m ) then
                          ! work(iu) is lda by n and work(ir) is m by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = m
                       else
                          ! work(iu) is m by chunk and work(ir) is m by m
                          ldwrku = m
                          chunk = ( lwork-m*m ) / m
                          ldwrkr = m
                       end if
                       itau = ir + ldwrkr*m
                       iwork = itau + m
                       ! compute a=l*q
                       ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to work(ir) and zero out above it
                       call stdlib_clacpy( 'L', m, m, a, lda, work( ir ), ldwrkr )
                       call stdlib_claset( 'U', m-1, m-1, czero, czero,work( ir+ldwrkr ), ldwrkr )
                                 
                       ! generate q in a
                       ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_cunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in work(ir)
                       ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                       ! (rworkspace: need m)
                       call stdlib_cgebrd( m, m, work( ir ), ldwrkr, s, rwork( ie ),work( itauq ),&
                                  work( itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing l
                       ! (cworkspace: need m*m+3*m-1, prefer m*m+2*m+(m-1)*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       irwork = ie + m
                       ! perform bidiagonal qr iteration, computing right
                       ! singular vectors of l in work(ir)
                       ! (cworkspace: need m*m)
                       ! (rworkspace: need bdspac)
                       call stdlib_cbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, &
                                 cdum, 1_ilp, cdum, 1_ilp,rwork( irwork ), info )
                       iu = itauq
                       ! multiply right singular vectors of l in work(ir) by q
                       ! in a, storing result in work(iu) and copying to a
                       ! (cworkspace: need m*m+m, prefer m*m+m*n)
                       ! (rworkspace: 0)
                       do i = 1, n, chunk
                          blk = min( n-i+1, chunk )
                          call stdlib_cgemm( 'N', 'N', m, blk, m, cone, work( ir ),ldwrkr, a( 1_ilp, &
                                    i ), lda, czero,work( iu ), ldwrku )
                          call stdlib_clacpy( 'F', m, blk, work( iu ), ldwrku,a( 1_ilp, i ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       ie = 1_ilp
                       itauq = 1_ilp
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize a
                       ! (cworkspace: need 2*m+n, prefer 2*m+(m+n)*nb)
                       ! (rworkspace: need m)
                       call stdlib_cgebrd( m, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing a
                       ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + m
                       ! perform bidiagonal qr iteration, computing right
                       ! singular vectors of a in a
                       ! (cworkspace: 0)
                       ! (rworkspace: need bdspac)
                       call stdlib_cbdsqr( 'L', m, n, 0_ilp, 0_ilp, s, rwork( ie ), a, lda,cdum, 1_ilp, cdum, &
                                 1_ilp, rwork( irwork ), info )
                    end if
                 else if( wntvo .and. wntuas ) then
                    ! path 3t(n much larger than m, jobu='s' or 'a', jobvt='o')
                    ! m right singular vectors to be overwritten on a and
                    ! m left singular vectors to be computed in u
                    if( lwork>=m*m+3*m ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n )+lda*m ) then
                          ! work(iu) is lda by n and work(ir) is lda by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n )+m*m ) then
                          ! work(iu) is lda by n and work(ir) is m by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = m
                       else
                          ! work(iu) is m by chunk and work(ir) is m by m
                          ldwrku = m
                          chunk = ( lwork-m*m ) / m
                          ldwrkr = m
                       end if
                       itau = ir + ldwrkr*m
                       iwork = itau + m
                       ! compute a=l*q
                       ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to u, zeroing about above it
                       call stdlib_clacpy( 'L', m, m, a, lda, u, ldu )
                       if (m>1_ilp) call stdlib_claset( 'U', m-1, m-1, czero, czero, u( 1_ilp, 2_ilp ),ldu )
                       ! generate q in a
                       ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_cunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in u, copying result to work(ir)
                       ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                       ! (rworkspace: need m)
                       call stdlib_cgebrd( m, m, u, ldu, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       call stdlib_clacpy( 'U', m, m, u, ldu, work( ir ), ldwrkr )
                       ! generate right vectors bidiagonalizing l in work(ir)
                       ! (cworkspace: need m*m+3*m-1, prefer m*m+2*m+(m-1)*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing l in u
                       ! (cworkspace: need m*m+3*m, prefer m*m+2*m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + m
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of l in u, and computing right
                       ! singular vectors of l in work(ir)
                       ! (cworkspace: need m*m)
                       ! (rworkspace: need bdspac)
                       call stdlib_cbdsqr( 'U', m, m, m, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, u, &
                                 ldu, cdum, 1_ilp,rwork( irwork ), info )
                       iu = itauq
                       ! multiply right singular vectors of l in work(ir) by q
                       ! in a, storing result in work(iu) and copying to a
                       ! (cworkspace: need m*m+m, prefer m*m+m*n))
                       ! (rworkspace: 0)
                       do i = 1, n, chunk
                          blk = min( n-i+1, chunk )
                          call stdlib_cgemm( 'N', 'N', m, blk, m, cone, work( ir ),ldwrkr, a( 1_ilp, &
                                    i ), lda, czero,work( iu ), ldwrku )
                          call stdlib_clacpy( 'F', m, blk, work( iu ), ldwrku,a( 1_ilp, i ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       itau = 1_ilp
                       iwork = itau + m
                       ! compute a=l*q
                       ! (cworkspace: need 2*m, prefer m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to u, zeroing out above it
                       call stdlib_clacpy( 'L', m, m, a, lda, u, ldu )
                       if (m>1_ilp) call stdlib_claset( 'U', m-1, m-1, czero, czero, u( 1_ilp, 2_ilp ),ldu )
                       ! generate q in a
                       ! (cworkspace: need 2*m, prefer m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_cunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in u
                       ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                       ! (rworkspace: need m)
                       call stdlib_cgebrd( m, m, u, ldu, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! multiply right vectors bidiagonalizing l by q in a
                       ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_cunmbr( 'P', 'L', 'C', m, n, m, u, ldu,work( itaup ), a, lda, &
                                 work( iwork ),lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing l in u
                       ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_cungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + m
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in u and computing right
                       ! singular vectors of a in a
                       ! (cworkspace: 0)
                       ! (rworkspace: need bdspac)
                       call stdlib_cbdsqr( 'U', m, n, m, 0_ilp, s, rwork( ie ), a, lda,u, ldu, cdum, &
                                 1_ilp, rwork( irwork ), info )
                    end if
                 else if( wntvs ) then
                    if( wntun ) then
                       ! path 4t(n much larger than m, jobu='n', jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! no left singular vectors to be computed
                       if( lwork>=m*m+3*m ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(ir) is lda by m
                             ldwrkr = lda
                          else
                             ! work(ir) is m by m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(ir), zeroing out above it
                          call stdlib_clacpy( 'L', m, m, a, lda, work( ir ),ldwrkr )
                          call stdlib_claset( 'U', m-1, m-1, czero, czero,work( ir+ldwrkr ), &
                                    ldwrkr )
                          ! generate q in a
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(ir)
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_cgebrd( m, m, work( ir ), ldwrkr, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate right vectors bidiagonalizing l in
                          ! work(ir)
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of l in work(ir)
                          ! (cworkspace: need m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, &
                                    cdum, 1_ilp, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply right singular vectors of l in work(ir) by
                          ! q in a, storing result in vt
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, m, cone, work( ir ),ldwrkr, a, lda, &
                                    czero, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy result to vt
                          call stdlib_clacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_claset( 'U', m-1, m-1, czero, czero,a( 1_ilp, 2_ilp ), lda )
                          ! bidiagonalize l in a
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_cgebrd( m, m, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right vectors bidiagonalizing l by q in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'P', 'L', 'C', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, n, 0_ilp, 0_ilp, s, rwork( ie ), vt,ldvt, cdum, 1_ilp, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    else if( wntuo ) then
                       ! path 5t(n much larger than m, jobu='o', jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! m left singular vectors to be overwritten on a
                       if( lwork>=2_ilp*m*m+3*m ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*m ) then
                             ! work(iu) is lda by m and work(ir) is lda by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+m )*m ) then
                             ! work(iu) is lda by m and work(ir) is m by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          else
                             ! work(iu) is m by m and work(ir) is m by m
                             ldwrku = m
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (cworkspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(iu), zeroing out below it
                          call stdlib_clacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_claset( 'U', m-1, m-1, czero, czero,work( iu+ldwrku ), &
                                    ldwrku )
                          ! generate q in a
                          ! (cworkspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to
                          ! work(ir)
                          ! (cworkspace: need   2*m*m+3*m,
                                       ! prefer 2*m*m+2*m+2*m*nb)
                          ! (rworkspace: need   m)
                          call stdlib_cgebrd( m, m, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, m, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need   2*m*m+3*m-1,
                                       ! prefer 2*m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need 2*m*m+3*m, prefer 2*m*m+2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', m, m, m, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in work(ir) and computing
                          ! right singular vectors of l in work(iu)
                          ! (cworkspace: need 2*m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, m, m, 0_ilp, s, rwork( ie ),work( iu ), ldwrku, &
                                    work( ir ),ldwrkr, cdum, 1_ilp, rwork( irwork ),info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in a, storing result in vt
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, m, cone, work( iu ),ldwrku, a, lda, &
                                    czero, vt, ldvt )
                          ! copy left singular vectors of l to a
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_clacpy( 'F', m, m, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_claset( 'U', m-1, m-1, czero, czero,a( 1_ilp, 2_ilp ), lda )
                          ! bidiagonalize l in a
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_cgebrd( m, m, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right vectors bidiagonalizing l by q in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'P', 'L', 'C', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors of l in a
                          ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in a and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, a, lda, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    else if( wntuas ) then
                       ! path 6t(n much larger than m, jobu='s' or 'a',
                               ! jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! m left singular vectors to be computed in u
                       if( lwork>=m*m+3*m ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is lda by m
                             ldwrku = m
                          end if
                          itau = iu + ldwrku*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_clacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_claset( 'U', m-1, m-1, czero, czero,work( iu+ldwrku ), &
                                    ldwrku )
                          ! generate q in a
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to u
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_cgebrd( m, m, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, m, work( iu ), ldwrku, u,ldu )
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need   m*m+3*m-1,
                                       ! prefer m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in u and computing right
                          ! singular vectors of l in work(iu)
                          ! (cworkspace: need m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, m, m, 0_ilp, s, rwork( ie ),work( iu ), ldwrku, &
                                    u, ldu, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in a, storing result in vt
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, m, cone, work( iu ),ldwrku, a, lda, &
                                    czero, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to u, zeroing out above it
                          call stdlib_clacpy( 'L', m, m, a, lda, u, ldu )
                          if (m>1_ilp) call stdlib_claset( 'U', m-1, m-1, czero, czero,u( 1_ilp, 2_ilp ), ldu )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in u
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_cgebrd( m, m, u, ldu, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in u by q
                          ! in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'P', 'L', 'C', m, n, m, u, ldu,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    end if
                 else if( wntva ) then
                    if( wntun ) then
                       ! path 7t(n much larger than m, jobu='n', jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! no left singular vectors to be computed
                       if( lwork>=m*m+max( n+m, 3_ilp*m ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(ir) is lda by m
                             ldwrkr = lda
                          else
                             ! work(ir) is m by m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! copy l to work(ir), zeroing out above it
                          call stdlib_clacpy( 'L', m, m, a, lda, work( ir ),ldwrkr )
                          call stdlib_claset( 'U', m-1, m-1, czero, czero,work( ir+ldwrkr ), &
                                    ldwrkr )
                          ! generate q in vt
                          ! (cworkspace: need m*m+m+n, prefer m*m+m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(ir)
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_cgebrd( m, m, work( ir ), ldwrkr, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need   m*m+3*m-1,
                                       ! prefer m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of l in work(ir)
                          ! (cworkspace: need m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, &
                                    cdum, 1_ilp, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply right singular vectors of l in work(ir) by
                          ! q in vt, storing result in a
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, m, cone, work( ir ),ldwrkr, vt, ldvt,&
                                     czero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_clacpy( 'F', m, n, a, lda, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need m+n, prefer m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_claset( 'U', m-1, m-1, czero, czero,a( 1_ilp, 2_ilp ), lda )
                          ! bidiagonalize l in a
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_cgebrd( m, m, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in a by q
                          ! in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'P', 'L', 'C', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, n, 0_ilp, 0_ilp, s, rwork( ie ), vt,ldvt, cdum, 1_ilp, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    else if( wntuo ) then
                       ! path 8t(n much larger than m, jobu='o', jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! m left singular vectors to be overwritten on a
                       if( lwork>=2_ilp*m*m+max( n+m, 3_ilp*m ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*m ) then
                             ! work(iu) is lda by m and work(ir) is lda by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+m )*m ) then
                             ! work(iu) is lda by m and work(ir) is m by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          else
                             ! work(iu) is m by m and work(ir) is m by m
                             ldwrku = m
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need 2*m*m+m+n, prefer 2*m*m+m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_clacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_claset( 'U', m-1, m-1, czero, czero,work( iu+ldwrku ), &
                                    ldwrku )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to
                          ! work(ir)
                          ! (cworkspace: need   2*m*m+3*m,
                                       ! prefer 2*m*m+2*m+2*m*nb)
                          ! (rworkspace: need   m)
                          call stdlib_cgebrd( m, m, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, m, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need   2*m*m+3*m-1,
                                       ! prefer 2*m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need 2*m*m+3*m, prefer 2*m*m+2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', m, m, m, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in work(ir) and computing
                          ! right singular vectors of l in work(iu)
                          ! (cworkspace: need 2*m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, m, m, 0_ilp, s, rwork( ie ),work( iu ), ldwrku, &
                                    work( ir ),ldwrkr, cdum, 1_ilp, rwork( irwork ),info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in vt, storing result in a
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, m, cone, work( iu ),ldwrku, vt, ldvt,&
                                     czero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_clacpy( 'F', m, n, a, lda, vt, ldvt )
                          ! copy left singular vectors of a from work(ir) to a
                          call stdlib_clacpy( 'F', m, m, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need m+n, prefer m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_claset( 'U', m-1, m-1, czero, czero,a( 1_ilp, 2_ilp ), lda )
                          ! bidiagonalize l in a
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_cgebrd( m, m, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in a by q
                          ! in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'P', 'L', 'C', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in a
                          ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in a and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, a, lda, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    else if( wntuas ) then
                       ! path 9t(n much larger than m, jobu='s' or 'a',
                               ! jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! m left singular vectors to be computed in u
                       if( lwork>=m*m+max( n+m, 3_ilp*m ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(iu) is lda by m
                             ldwrku = lda
                          else
                             ! work(iu) is m by m
                             ldwrku = m
                          end if
                          itau = iu + ldwrku*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need m*m+m+n, prefer m*m+m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_clacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_claset( 'U', m-1, m-1, czero, czero,work( iu+ldwrku ), &
                                    ldwrku )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to u
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_cgebrd( m, m, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_clacpy( 'L', m, m, work( iu ), ldwrku, u,ldu )
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in u and computing right
                          ! singular vectors of l in work(iu)
                          ! (cworkspace: need m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, m, m, 0_ilp, s, rwork( ie ),work( iu ), ldwrku, &
                                    u, ldu, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in vt, storing result in a
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_cgemm( 'N', 'N', m, n, m, cone, work( iu ),ldwrku, vt, ldvt,&
                                     czero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_clacpy( 'F', m, n, a, lda, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_clacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need m+n, prefer m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to u, zeroing out above it
                          call stdlib_clacpy( 'L', m, m, a, lda, u, ldu )
                          if (m>1_ilp) call stdlib_claset( 'U', m-1, m-1, czero, czero,u( 1_ilp, 2_ilp ), ldu )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in u
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_cgebrd( m, m, u, ldu, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in u by q
                          ! in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_cunmbr( 'P', 'L', 'C', m, n, m, u, ldu,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_cungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_cbdsqr( 'U', m, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    end if
                 end if
              else
                 ! n < mnthr
                 ! path 10t(n greater than m, but not much larger)
                 ! reduce to bidiagonal form without lq decomposition
                 ie = 1_ilp
                 itauq = 1_ilp
                 itaup = itauq + m
                 iwork = itaup + m
                 ! bidiagonalize a
                 ! (cworkspace: need 2*m+n, prefer 2*m+(m+n)*nb)
                 ! (rworkspace: m)
                 call stdlib_cgebrd( m, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), &
                           work( iwork ), lwork-iwork+1,ierr )
                 if( wntuas ) then
                    ! if left singular vectors desired in u, copy result to u
                    ! and generate left bidiagonalizing vectors in u
                    ! (cworkspace: need 3*m-1, prefer 2*m+(m-1)*nb)
                    ! (rworkspace: 0)
                    call stdlib_clacpy( 'L', m, m, a, lda, u, ldu )
                    call stdlib_cungbr( 'Q', m, m, n, u, ldu, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvas ) then
                    ! if right singular vectors desired in vt, copy result to
                    ! vt and generate right bidiagonalizing vectors in vt
                    ! (cworkspace: need 2*m+nrvt, prefer 2*m+nrvt*nb)
                    ! (rworkspace: 0)
                    call stdlib_clacpy( 'U', m, n, a, lda, vt, ldvt )
                    if( wntva )nrvt = n
                    if( wntvs )nrvt = m
                    call stdlib_cungbr( 'P', nrvt, n, m, vt, ldvt, work( itaup ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntuo ) then
                    ! if left singular vectors desired in a, generate left
                    ! bidiagonalizing vectors in a
                    ! (cworkspace: need 3*m-1, prefer 2*m+(m-1)*nb)
                    ! (rworkspace: 0)
                    call stdlib_cungbr( 'Q', m, m, n, a, lda, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvo ) then
                    ! if right singular vectors desired in a, generate right
                    ! bidiagonalizing vectors in a
                    ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                    ! (rworkspace: 0)
                    call stdlib_cungbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 irwork = ie + m
                 if( wntuas .or. wntuo )nru = m
                 if( wntun )nru = 0_ilp
                 if( wntvas .or. wntvo )ncvt = n
                 if( wntvn )ncvt = 0_ilp
                 if( ( .not.wntuo ) .and. ( .not.wntvo ) ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in vt
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_cbdsqr( 'L', m, ncvt, nru, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                              cdum, 1_ilp, rwork( irwork ),info )
                 else if( ( .not.wntuo ) .and. wntvo ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in a
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_cbdsqr( 'L', m, ncvt, nru, 0_ilp, s, rwork( ie ), a,lda, u, ldu, cdum,&
                               1_ilp, rwork( irwork ),info )
                 else
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in a and computing right singular
                    ! vectors in vt
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_cbdsqr( 'L', m, ncvt, nru, 0_ilp, s, rwork( ie ), vt,ldvt, a, lda, &
                              cdum, 1_ilp, rwork( irwork ),info )
                 end if
              end if
           end if
           ! undo scaling if necessary
           if( iscl==1_ilp ) then
              if( anrm>bignum )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,&
                        ierr )
              if( info/=0_ilp .and. anrm>bignum )call stdlib_slascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn-1,&
                         1_ilp,rwork( ie ), minmn, ierr )
              if( anrm<smlnum )call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,&
                        ierr )
              if( info/=0_ilp .and. anrm<smlnum )call stdlib_slascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn-1,&
                         1_ilp,rwork( ie ), minmn, ierr )
           end if
           ! return optimal workspace in work(1)
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_cgesvd

     module subroutine stdlib_zgesvd( jobu, jobvt, m, n, a, lda, s, u, ldu,vt, ldvt, work, lwork, rwork, &
     !! ZGESVD computes the singular value decomposition (SVD) of a complex
     !! M-by-N matrix A, optionally computing the left and/or right singular
     !! vectors. The SVD is written
     !! A = U * SIGMA * conjugate-transpose(V)
     !! where SIGMA is an M-by-N matrix which is zero except for its
     !! min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
     !! V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
     !! are the singular values of A; they are real and non-negative, and
     !! are returned in descending order.  The first min(m,n) columns of
     !! U and V are the left and right singular vectors of A.
     !! Note that the routine returns V**H, not V.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu, jobvt
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*), s(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: u(ldu,*), vt(ldvt,*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, wntua, wntuas, wntun, wntuo, wntus, wntva, wntvas, wntvn, wntvo,&
                      wntvs
           integer(ilp) :: blk, chunk, i, ie, ierr, ir, irwork, iscl, itau, itaup, itauq, iu, &
           iwork, ldwrkr, ldwrku, maxwrk, minmn, minwrk, mnthr, ncu, ncvt, nru, nrvt, &
                     wrkbl
           integer(ilp) :: lwork_zgeqrf, lwork_zungqr_n, lwork_zungqr_m, lwork_zgebrd, &
                     lwork_zungbr_p, lwork_zungbr_q, lwork_zgelqf, lwork_zunglq_n, lwork_zunglq_m
           real(dp) :: anrm, bignum, eps, smlnum
           ! Local Arrays 
           real(dp) :: dum(1_ilp)
           complex(dp) :: cdum(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           minmn = min( m, n )
           wntua = stdlib_lsame( jobu, 'A' )
           wntus = stdlib_lsame( jobu, 'S' )
           wntuas = wntua .or. wntus
           wntuo = stdlib_lsame( jobu, 'O' )
           wntun = stdlib_lsame( jobu, 'N' )
           wntva = stdlib_lsame( jobvt, 'A' )
           wntvs = stdlib_lsame( jobvt, 'S' )
           wntvas = wntva .or. wntvs
           wntvo = stdlib_lsame( jobvt, 'O' )
           wntvn = stdlib_lsame( jobvt, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.( wntua .or. wntus .or. wntuo .or. wntun ) ) then
              info = -1_ilp
           else if( .not.( wntva .or. wntvs .or. wntvo .or. wntvn ) .or.( wntvo .and. wntuo ) ) &
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( ldu<1_ilp .or. ( wntuas .and. ldu<m ) ) then
              info = -9_ilp
           else if( ldvt<1_ilp .or. ( wntva .and. ldvt<n ) .or.( wntvs .and. ldvt<minmn ) ) &
                     then
              info = -11_ilp
           end if
           ! compute workspace
            ! (note: comments in the code beginning "workspace:" describe the
             ! minimal amount of workspace needed at that point in the code,
             ! as well as the preferred amount for good performance.
             ! cworkspace refers to complex workspace, and rworkspace to
             ! real workspace. nb refers to the optimal block size for the
             ! immediately following subroutine, as returned by stdlib_ilaenv.)
           if( info==0_ilp ) then
              minwrk = 1_ilp
              maxwrk = 1_ilp
              if( m>=n .and. minmn>0_ilp ) then
                 ! space needed for stdlib_zbdsqr is bdspac = 5*n
                 mnthr = stdlib_ilaenv( 6_ilp, 'ZGESVD', jobu // jobvt, m, n, 0_ilp, 0_ilp )
                 ! compute space needed for stdlib_zgeqrf
                 call stdlib_zgeqrf( m, n, a, lda, cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                 lwork_zgeqrf = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_zungqr
                 call stdlib_zungqr( m, n, n, a, lda, cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                 lwork_zungqr_n = int( cdum(1_ilp),KIND=ilp)
                 call stdlib_zungqr( m, m, n, a, lda, cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                 lwork_zungqr_m = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_zgebrd
                 call stdlib_zgebrd( n, n, a, lda, s, dum(1_ilp), cdum(1_ilp),cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                           
                 lwork_zgebrd = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_zungbr
                 call stdlib_zungbr( 'P', n, n, n, a, lda, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                 lwork_zungbr_p = int( cdum(1_ilp),KIND=ilp)
                 call stdlib_zungbr( 'Q', n, n, n, a, lda, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                 lwork_zungbr_q = int( cdum(1_ilp),KIND=ilp)
                 if( m>=mnthr ) then
                    if( wntun ) then
                       ! path 1 (m much larger than n, jobu='n')
                       maxwrk = n + lwork_zgeqrf
                       maxwrk = max( maxwrk, 2_ilp*n+lwork_zgebrd )
                       if( wntvo .or. wntvas )maxwrk = max( maxwrk, 2_ilp*n+lwork_zungbr_p )
                       minwrk = 3_ilp*n
                    else if( wntuo .and. wntvn ) then
                       ! path 2 (m much larger than n, jobu='o', jobvt='n')
                       wrkbl = n + lwork_zgeqrf
                       wrkbl = max( wrkbl, n+lwork_zungqr_n )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_q )
                       maxwrk = max( n*n+wrkbl, n*n+m*n )
                       minwrk = 2_ilp*n + m
                    else if( wntuo .and. wntvas ) then
                       ! path 3 (m much larger than n, jobu='o', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_zgeqrf
                       wrkbl = max( wrkbl, n+lwork_zungqr_n )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_q )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_p )
                       maxwrk = max( n*n+wrkbl, n*n+m*n )
                       minwrk = 2_ilp*n + m
                    else if( wntus .and. wntvn ) then
                       ! path 4 (m much larger than n, jobu='s', jobvt='n')
                       wrkbl = n + lwork_zgeqrf
                       wrkbl = max( wrkbl, n+lwork_zungqr_n )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_q )
                       maxwrk = n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    else if( wntus .and. wntvo ) then
                       ! path 5 (m much larger than n, jobu='s', jobvt='o')
                       wrkbl = n + lwork_zgeqrf
                       wrkbl = max( wrkbl, n+lwork_zungqr_n )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_q )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_p )
                       maxwrk = 2_ilp*n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    else if( wntus .and. wntvas ) then
                       ! path 6 (m much larger than n, jobu='s', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_zgeqrf
                       wrkbl = max( wrkbl, n+lwork_zungqr_n )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_q )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_p )
                       maxwrk = n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    else if( wntua .and. wntvn ) then
                       ! path 7 (m much larger than n, jobu='a', jobvt='n')
                       wrkbl = n + lwork_zgeqrf
                       wrkbl = max( wrkbl, n+lwork_zungqr_m )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_q )
                       maxwrk = n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    else if( wntua .and. wntvo ) then
                       ! path 8 (m much larger than n, jobu='a', jobvt='o')
                       wrkbl = n + lwork_zgeqrf
                       wrkbl = max( wrkbl, n+lwork_zungqr_m )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_q )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_p )
                       maxwrk = 2_ilp*n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    else if( wntua .and. wntvas ) then
                       ! path 9 (m much larger than n, jobu='a', jobvt='s' or
                       ! 'a')
                       wrkbl = n + lwork_zgeqrf
                       wrkbl = max( wrkbl, n+lwork_zungqr_m )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_q )
                       wrkbl = max( wrkbl, 2_ilp*n+lwork_zungbr_p )
                       maxwrk = n*n + wrkbl
                       minwrk = 2_ilp*n + m
                    end if
                 else
                    ! path 10 (m at least n, but not much larger)
                    call stdlib_zgebrd( m, n, a, lda, s, dum(1_ilp), cdum(1_ilp),cdum(1_ilp), cdum(1_ilp), -1_ilp, &
                              ierr )
                    lwork_zgebrd = int( cdum(1_ilp),KIND=ilp)
                    maxwrk = 2_ilp*n + lwork_zgebrd
                    if( wntus .or. wntuo ) then
                       call stdlib_zungbr( 'Q', m, n, n, a, lda, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                                 
                       lwork_zungbr_q = int( cdum(1_ilp),KIND=ilp)
                       maxwrk = max( maxwrk, 2_ilp*n+lwork_zungbr_q )
                    end if
                    if( wntua ) then
                       call stdlib_zungbr( 'Q', m, m, n, a, lda, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                                 
                       lwork_zungbr_q = int( cdum(1_ilp),KIND=ilp)
                       maxwrk = max( maxwrk, 2_ilp*n+lwork_zungbr_q )
                    end if
                    if( .not.wntvn ) then
                       maxwrk = max( maxwrk, 2_ilp*n+lwork_zungbr_p )
                    end if
                    minwrk = 2_ilp*n + m
                 end if
              else if( minmn>0_ilp ) then
                 ! space needed for stdlib_zbdsqr is bdspac = 5*m
                 mnthr = stdlib_ilaenv( 6_ilp, 'ZGESVD', jobu // jobvt, m, n, 0_ilp, 0_ilp )
                 ! compute space needed for stdlib_zgelqf
                 call stdlib_zgelqf( m, n, a, lda, cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                 lwork_zgelqf = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_zunglq
                 call stdlib_zunglq( n, n, m, cdum(1_ilp), n, cdum(1_ilp), cdum(1_ilp), -1_ilp,ierr )
                 lwork_zunglq_n = int( cdum(1_ilp),KIND=ilp)
                 call stdlib_zunglq( m, n, m, a, lda, cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                 lwork_zunglq_m = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_zgebrd
                 call stdlib_zgebrd( m, m, a, lda, s, dum(1_ilp), cdum(1_ilp),cdum(1_ilp), cdum(1_ilp), -1_ilp, ierr )
                           
                 lwork_zgebrd = int( cdum(1_ilp),KIND=ilp)
                  ! compute space needed for stdlib_zungbr p
                 call stdlib_zungbr( 'P', m, m, m, a, n, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                 lwork_zungbr_p = int( cdum(1_ilp),KIND=ilp)
                 ! compute space needed for stdlib_zungbr q
                 call stdlib_zungbr( 'Q', m, m, m, a, n, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                 lwork_zungbr_q = int( cdum(1_ilp),KIND=ilp)
                 if( n>=mnthr ) then
                    if( wntvn ) then
                       ! path 1t(n much larger than m, jobvt='n')
                       maxwrk = m + lwork_zgelqf
                       maxwrk = max( maxwrk, 2_ilp*m+lwork_zgebrd )
                       if( wntuo .or. wntuas )maxwrk = max( maxwrk, 2_ilp*m+lwork_zungbr_q )
                       minwrk = 3_ilp*m
                    else if( wntvo .and. wntun ) then
                       ! path 2t(n much larger than m, jobu='n', jobvt='o')
                       wrkbl = m + lwork_zgelqf
                       wrkbl = max( wrkbl, m+lwork_zunglq_m )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_p )
                       maxwrk = max( m*m+wrkbl, m*m+m*n )
                       minwrk = 2_ilp*m + n
                    else if( wntvo .and. wntuas ) then
                       ! path 3t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='o')
                       wrkbl = m + lwork_zgelqf
                       wrkbl = max( wrkbl, m+lwork_zunglq_m )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_p )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_q )
                       maxwrk = max( m*m+wrkbl, m*m+m*n )
                       minwrk = 2_ilp*m + n
                    else if( wntvs .and. wntun ) then
                       ! path 4t(n much larger than m, jobu='n', jobvt='s')
                       wrkbl = m + lwork_zgelqf
                       wrkbl = max( wrkbl, m+lwork_zunglq_m )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_p )
                       maxwrk = m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    else if( wntvs .and. wntuo ) then
                       ! path 5t(n much larger than m, jobu='o', jobvt='s')
                       wrkbl = m + lwork_zgelqf
                       wrkbl = max( wrkbl, m+lwork_zunglq_m )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_p )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_q )
                       maxwrk = 2_ilp*m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    else if( wntvs .and. wntuas ) then
                       ! path 6t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='s')
                       wrkbl = m + lwork_zgelqf
                       wrkbl = max( wrkbl, m+lwork_zunglq_m )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_p )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_q )
                       maxwrk = m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    else if( wntva .and. wntun ) then
                       ! path 7t(n much larger than m, jobu='n', jobvt='a')
                       wrkbl = m + lwork_zgelqf
                       wrkbl = max( wrkbl, m+lwork_zunglq_n )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_p )
                       maxwrk = m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    else if( wntva .and. wntuo ) then
                       ! path 8t(n much larger than m, jobu='o', jobvt='a')
                       wrkbl = m + lwork_zgelqf
                       wrkbl = max( wrkbl, m+lwork_zunglq_n )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_p )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_q )
                       maxwrk = 2_ilp*m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    else if( wntva .and. wntuas ) then
                       ! path 9t(n much larger than m, jobu='s' or 'a',
                       ! jobvt='a')
                       wrkbl = m + lwork_zgelqf
                       wrkbl = max( wrkbl, m+lwork_zunglq_n )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zgebrd )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_p )
                       wrkbl = max( wrkbl, 2_ilp*m+lwork_zungbr_q )
                       maxwrk = m*m + wrkbl
                       minwrk = 2_ilp*m + n
                    end if
                 else
                    ! path 10t(n greater than m, but not much larger)
                    call stdlib_zgebrd( m, n, a, lda, s, dum(1_ilp), cdum(1_ilp),cdum(1_ilp), cdum(1_ilp), -1_ilp, &
                              ierr )
                    lwork_zgebrd = int( cdum(1_ilp),KIND=ilp)
                    maxwrk = 2_ilp*m + lwork_zgebrd
                    if( wntvs .or. wntvo ) then
                      ! compute space needed for stdlib_zungbr p
                      call stdlib_zungbr( 'P', m, n, m, a, n, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                      lwork_zungbr_p = int( cdum(1_ilp),KIND=ilp)
                      maxwrk = max( maxwrk, 2_ilp*m+lwork_zungbr_p )
                    end if
                    if( wntva ) then
                      call stdlib_zungbr( 'P', n,  n, m, a, n, cdum(1_ilp),cdum(1_ilp), -1_ilp, ierr )
                      lwork_zungbr_p = int( cdum(1_ilp),KIND=ilp)
                      maxwrk = max( maxwrk, 2_ilp*m+lwork_zungbr_p )
                    end if
                    if( .not.wntun ) then
                       maxwrk = max( maxwrk, 2_ilp*m+lwork_zungbr_q )
                    end if
                    minwrk = 2_ilp*m + n
                 end if
              end if
              maxwrk = max( maxwrk, minwrk )
              work( 1_ilp ) = maxwrk
              if( lwork<minwrk .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGESVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              return
           end if
           ! get machine constants
           eps = stdlib_dlamch( 'P' )
           smlnum = sqrt( stdlib_dlamch( 'S' ) ) / eps
           bignum = one / smlnum
           ! scale a if max element outside range [smlnum,bignum]
           anrm = stdlib_zlange( 'M', m, n, a, lda, dum )
           iscl = 0_ilp
           if( anrm>zero .and. anrm<smlnum ) then
              iscl = 1_ilp
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, smlnum, m, n, a, lda, ierr )
           else if( anrm>bignum ) then
              iscl = 1_ilp
              call stdlib_zlascl( 'G', 0_ilp, 0_ilp, anrm, bignum, m, n, a, lda, ierr )
           end if
           if( m>=n ) then
              ! a has at least as many rows as columns. if a has sufficiently
              ! more rows than columns, first reduce using the qr
              ! decomposition (if sufficient workspace available)
              if( m>=mnthr ) then
                 if( wntun ) then
                    ! path 1 (m much larger than n, jobu='n')
                    ! no left singular vectors to be computed
                    itau = 1_ilp
                    iwork = itau + n
                    ! compute a=q*r
                    ! (cworkspace: need 2*n, prefer n+n*nb)
                    ! (rworkspace: need 0)
                    call stdlib_zgeqrf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                              ierr )
                    ! zero out below r
                    if( n > 1_ilp ) then
                       call stdlib_zlaset( 'L', n-1, n-1, czero, czero, a( 2_ilp, 1_ilp ),lda )
                    end if
                    ie = 1_ilp
                    itauq = 1_ilp
                    itaup = itauq + n
                    iwork = itaup + n
                    ! bidiagonalize r in a
                    ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                    ! (rworkspace: need n)
                    call stdlib_zgebrd( n, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ),&
                               work( iwork ), lwork-iwork+1,ierr )
                    ncvt = 0_ilp
                    if( wntvo .or. wntvas ) then
                       ! if right singular vectors desired, generate p'.
                       ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       ncvt = n
                    end if
                    irwork = ie + n
                    ! perform bidiagonal qr iteration, computing right
                    ! singular vectors of a in a if desired
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_zbdsqr( 'U', n, ncvt, 0_ilp, 0_ilp, s, rwork( ie ), a, lda,cdum, 1_ilp, cdum, &
                              1_ilp, rwork( irwork ), info )
                    ! if right singular vectors desired in vt, copy them there
                    if( wntvas )call stdlib_zlacpy( 'F', n, n, a, lda, vt, ldvt )
                 else if( wntuo .and. wntvn ) then
                    ! path 2 (m much larger than n, jobu='o', jobvt='n')
                    ! n left singular vectors to be overwritten on a and
                    ! no right singular vectors to be computed
                    if( lwork>=n*n+3*n ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n )+lda*n ) then
                          ! work(iu) is lda by n, work(ir) is lda by n
                          ldwrku = lda
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n )+n*n ) then
                          ! work(iu) is lda by n, work(ir) is n by n
                          ldwrku = lda
                          ldwrkr = n
                       else
                          ! work(iu) is ldwrku by n, work(ir) is n by n
                          ldwrku = ( lwork-n*n ) / n
                          ldwrkr = n
                       end if
                       itau = ir + ldwrkr*n
                       iwork = itau + n
                       ! compute a=q*r
                       ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to work(ir) and zero out below it
                       call stdlib_zlacpy( 'U', n, n, a, lda, work( ir ), ldwrkr )
                       call stdlib_zlaset( 'L', n-1, n-1, czero, czero,work( ir+1 ), ldwrkr )
                                 
                       ! generate q in a
                       ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in work(ir)
                       ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                       ! (rworkspace: need n)
                       call stdlib_zgebrd( n, n, work( ir ), ldwrkr, s, rwork( ie ),work( itauq ),&
                                  work( itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing r
                       ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                       ! (rworkspace: need 0)
                       call stdlib_zungbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       irwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of r in work(ir)
                       ! (cworkspace: need n*n)
                       ! (rworkspace: need bdspac)
                       call stdlib_zbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, rwork( ie ), cdum, 1_ilp,work( ir ), &
                                 ldwrkr, cdum, 1_ilp,rwork( irwork ), info )
                       iu = itauq
                       ! multiply q in a by left singular vectors of r in
                       ! work(ir), storing result in work(iu) and copying to a
                       ! (cworkspace: need n*n+n, prefer n*n+m*n)
                       ! (rworkspace: 0)
                       do i = 1, m, ldwrku
                          chunk = min( m-i+1, ldwrku )
                          call stdlib_zgemm( 'N', 'N', chunk, n, n, cone, a( i, 1_ilp ),lda, work( ir &
                                    ), ldwrkr, czero,work( iu ), ldwrku )
                          call stdlib_zlacpy( 'F', chunk, n, work( iu ), ldwrku,a( i, 1_ilp ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       ie = 1_ilp
                       itauq = 1_ilp
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize a
                       ! (cworkspace: need 2*n+m, prefer 2*n+(m+n)*nb)
                       ! (rworkspace: n)
                       call stdlib_zgebrd( m, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing a
                       ! (cworkspace: need 3*n, prefer 2*n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungbr( 'Q', m, n, n, a, lda, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in a
                       ! (cworkspace: need 0)
                       ! (rworkspace: need bdspac)
                       call stdlib_zbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, rwork( ie ), cdum, 1_ilp,a, lda, cdum, &
                                 1_ilp, rwork( irwork ), info )
                    end if
                 else if( wntuo .and. wntvas ) then
                    ! path 3 (m much larger than n, jobu='o', jobvt='s' or 'a')
                    ! n left singular vectors to be overwritten on a and
                    ! n right singular vectors to be computed in vt
                    if( lwork>=n*n+3*n ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n )+lda*n ) then
                          ! work(iu) is lda by n and work(ir) is lda by n
                          ldwrku = lda
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n )+n*n ) then
                          ! work(iu) is lda by n and work(ir) is n by n
                          ldwrku = lda
                          ldwrkr = n
                       else
                          ! work(iu) is ldwrku by n and work(ir) is n by n
                          ldwrku = ( lwork-n*n ) / n
                          ldwrkr = n
                       end if
                       itau = ir + ldwrkr*n
                       iwork = itau + n
                       ! compute a=q*r
                       ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to vt, zeroing out below it
                       call stdlib_zlacpy( 'U', n, n, a, lda, vt, ldvt )
                       if( n>1_ilp )call stdlib_zlaset( 'L', n-1, n-1, czero, czero,vt( 2_ilp, 1_ilp ), ldvt )
                                 
                       ! generate q in a
                       ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in vt, copying result to work(ir)
                       ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                       ! (rworkspace: need n)
                       call stdlib_zgebrd( n, n, vt, ldvt, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       call stdlib_zlacpy( 'L', n, n, vt, ldvt, work( ir ), ldwrkr )
                       ! generate left vectors bidiagonalizing r in work(ir)
                       ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing r in vt
                       ! (cworkspace: need n*n+3*n-1, prefer n*n+2*n+(n-1)*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of r in work(ir) and computing right
                       ! singular vectors of r in vt
                       ! (cworkspace: need n*n)
                       ! (rworkspace: need bdspac)
                       call stdlib_zbdsqr( 'U', n, n, n, 0_ilp, s, rwork( ie ), vt,ldvt, work( ir ), &
                                 ldwrkr, cdum, 1_ilp,rwork( irwork ), info )
                       iu = itauq
                       ! multiply q in a by left singular vectors of r in
                       ! work(ir), storing result in work(iu) and copying to a
                       ! (cworkspace: need n*n+n, prefer n*n+m*n)
                       ! (rworkspace: 0)
                       do i = 1, m, ldwrku
                          chunk = min( m-i+1, ldwrku )
                          call stdlib_zgemm( 'N', 'N', chunk, n, n, cone, a( i, 1_ilp ),lda, work( ir &
                                    ), ldwrkr, czero,work( iu ), ldwrku )
                          call stdlib_zlacpy( 'F', chunk, n, work( iu ), ldwrku,a( i, 1_ilp ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       itau = 1_ilp
                       iwork = itau + n
                       ! compute a=q*r
                       ! (cworkspace: need 2*n, prefer n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy r to vt, zeroing out below it
                       call stdlib_zlacpy( 'U', n, n, a, lda, vt, ldvt )
                       if( n>1_ilp )call stdlib_zlaset( 'L', n-1, n-1, czero, czero,vt( 2_ilp, 1_ilp ), ldvt )
                                 
                       ! generate q in a
                       ! (cworkspace: need 2*n, prefer n+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + n
                       iwork = itaup + n
                       ! bidiagonalize r in vt
                       ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                       ! (rworkspace: n)
                       call stdlib_zgebrd( n, n, vt, ldvt, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! multiply q in a by left vectors bidiagonalizing r
                       ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_zunmbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), a, lda,&
                                  work( iwork ),lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing r in vt
                       ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + n
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in a and computing right
                       ! singular vectors of a in vt
                       ! (cworkspace: 0)
                       ! (rworkspace: need bdspac)
                       call stdlib_zbdsqr( 'U', n, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, a, lda, cdum,&
                                  1_ilp, rwork( irwork ),info )
                    end if
                 else if( wntus ) then
                    if( wntvn ) then
                       ! path 4 (m much larger than n, jobu='s', jobvt='n')
                       ! n left singular vectors to be computed in u and
                       ! no right singular vectors to be computed
                       if( lwork>=n*n+3*n ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(ir) is lda by n
                             ldwrkr = lda
                          else
                             ! work(ir) is n by n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(ir), zeroing out below it
                          call stdlib_zlacpy( 'U', n, n, a, lda, work( ir ),ldwrkr )
                          call stdlib_zlaset( 'L', n-1, n-1, czero, czero,work( ir+1 ), ldwrkr )
                                    
                          ! generate q in a
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(ir)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_zgebrd( n, n, work( ir ), ldwrkr, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate left vectors bidiagonalizing r in work(ir)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(ir)
                          ! (cworkspace: need n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, rwork( ie ), cdum,1_ilp, work( ir ),&
                                     ldwrkr, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(ir), storing result in u
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, n, cone, a, lda,work( ir ), ldwrkr, &
                                    czero, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_zlaset( 'L', n-1, n-1, czero, czero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_zgebrd( n, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left vectors bidiagonalizing r
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, rwork( ie ), cdum,1_ilp, u, ldu, &
                                    cdum, 1_ilp, rwork( irwork ),info )
                       end if
                    else if( wntvo ) then
                       ! path 5 (m much larger than n, jobu='s', jobvt='o')
                       ! n left singular vectors to be computed in u and
                       ! n right singular vectors to be overwritten on a
                       if( lwork>=2_ilp*n*n+3*n ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*n ) then
                             ! work(iu) is lda by n and work(ir) is lda by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+n )*n ) then
                             ! work(iu) is lda by n and work(ir) is n by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          else
                             ! work(iu) is n by n and work(ir) is n by n
                             ldwrku = n
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (cworkspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_zlacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_zlaset( 'L', n-1, n-1, czero, czero,work( iu+1 ), ldwrku )
                                    
                          ! generate q in a
                          ! (cworkspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to
                          ! work(ir)
                          ! (cworkspace: need   2*n*n+3*n,
                                       ! prefer 2*n*n+2*n+2*n*nb)
                          ! (rworkspace: need   n)
                          call stdlib_zgebrd( n, n, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_zlacpy( 'U', n, n, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need 2*n*n+3*n, prefer 2*n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need   2*n*n+3*n-1,
                                       ! prefer 2*n*n+2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', n, n, n, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in work(ir)
                          ! (cworkspace: need 2*n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, n, n, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, &
                                    work( iu ),ldwrku, cdum, 1_ilp, rwork( irwork ),info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(iu), storing result in u
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, n, cone, a, lda,work( iu ), ldwrku, &
                                    czero, u, ldu )
                          ! copy right singular vectors of r to a
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_zlacpy( 'F', n, n, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_zlaset( 'L', n-1, n-1, czero, czero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_zgebrd( n, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left vectors bidiagonalizing r
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right vectors bidiagonalizing r in a
                          ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in a
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, n, m, 0_ilp, s, rwork( ie ), a,lda, u, ldu, &
                                    cdum, 1_ilp, rwork( irwork ),info )
                       end if
                    else if( wntvas ) then
                       ! path 6 (m much larger than n, jobu='s', jobvt='s'
                               ! or 'a')
                       ! n left singular vectors to be computed in u and
                       ! n right singular vectors to be computed in vt
                       if( lwork>=n*n+3*n ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is n by n
                             ldwrku = n
                          end if
                          itau = iu + ldwrku*n
                          iwork = itau + n
                          ! compute a=q*r
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_zlacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_zlaset( 'L', n-1, n-1, czero, czero,work( iu+1 ), ldwrku )
                                    
                          ! generate q in a
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, n, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to vt
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_zgebrd( n, n, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_zlacpy( 'U', n, n, work( iu ), ldwrku, vt,ldvt )
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (cworkspace: need   n*n+3*n-1,
                                       ! prefer n*n+2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in vt
                          ! (cworkspace: need n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, n, n, 0_ilp, s, rwork( ie ), vt,ldvt, work( iu )&
                                    , ldwrku, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply q in a by left singular vectors of r in
                          ! work(iu), storing result in u
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, n, cone, a, lda,work( iu ), ldwrku, &
                                    czero, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, n, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to vt, zeroing out below it
                          call stdlib_zlacpy( 'U', n, n, a, lda, vt, ldvt )
                          if( n>1_ilp )call stdlib_zlaset( 'L', n-1, n-1, czero, czero,vt( 2_ilp, 1_ilp ), &
                                    ldvt )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in vt
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_zgebrd( n, n, vt, ldvt, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in vt
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    end if
                 else if( wntua ) then
                    if( wntvn ) then
                       ! path 7 (m much larger than n, jobu='a', jobvt='n')
                       ! m left singular vectors to be computed in u and
                       ! no right singular vectors to be computed
                       if( lwork>=n*n+max( n+m, 3_ilp*n ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(ir) is lda by n
                             ldwrkr = lda
                          else
                             ! work(ir) is n by n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, n, a, lda, u, ldu )
                          ! copy r to work(ir), zeroing out below it
                          call stdlib_zlacpy( 'U', n, n, a, lda, work( ir ),ldwrkr )
                          call stdlib_zlaset( 'L', n-1, n-1, czero, czero,work( ir+1 ), ldwrkr )
                                    
                          ! generate q in u
                          ! (cworkspace: need n*n+n+m, prefer n*n+n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(ir)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_zgebrd( n, n, work( ir ), ldwrkr, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', n, n, n, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(ir)
                          ! (cworkspace: need n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, 0_ilp, n, 0_ilp, s, rwork( ie ), cdum,1_ilp, work( ir ),&
                                     ldwrkr, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(ir), storing result in a
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, n, cone, u, ldu,work( ir ), ldwrkr, &
                                    czero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_zlacpy( 'F', m, n, a, lda, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need n+m, prefer n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_zlaset( 'L', n-1, n-1, czero, czero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_zgebrd( n, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in a
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, 0_ilp, m, 0_ilp, s, rwork( ie ), cdum,1_ilp, u, ldu, &
                                    cdum, 1_ilp, rwork( irwork ),info )
                       end if
                    else if( wntvo ) then
                       ! path 8 (m much larger than n, jobu='a', jobvt='o')
                       ! m left singular vectors to be computed in u and
                       ! n right singular vectors to be overwritten on a
                       if( lwork>=2_ilp*n*n+max( n+m, 3_ilp*n ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*n ) then
                             ! work(iu) is lda by n and work(ir) is lda by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+n )*n ) then
                             ! work(iu) is lda by n and work(ir) is n by n
                             ldwrku = lda
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          else
                             ! work(iu) is n by n and work(ir) is n by n
                             ldwrku = n
                             ir = iu + ldwrku*n
                             ldwrkr = n
                          end if
                          itau = ir + ldwrkr*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n*n+2*n, prefer 2*n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need 2*n*n+n+m, prefer 2*n*n+n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_zlacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_zlaset( 'L', n-1, n-1, czero, czero,work( iu+1 ), ldwrku )
                                    
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to
                          ! work(ir)
                          ! (cworkspace: need   2*n*n+3*n,
                                       ! prefer 2*n*n+2*n+2*n*nb)
                          ! (rworkspace: need   n)
                          call stdlib_zgebrd( n, n, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_zlacpy( 'U', n, n, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need 2*n*n+3*n, prefer 2*n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need   2*n*n+3*n-1,
                                       ! prefer 2*n*n+2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', n, n, n, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in work(ir)
                          ! (cworkspace: need 2*n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, n, n, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, &
                                    work( iu ),ldwrku, cdum, 1_ilp, rwork( irwork ),info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(iu), storing result in a
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, n, cone, u, ldu,work( iu ), ldwrku, &
                                    czero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_zlacpy( 'F', m, n, a, lda, u, ldu )
                          ! copy right singular vectors of r from work(ir) to a
                          call stdlib_zlacpy( 'F', n, n, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need n+m, prefer n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! zero out below r in a
                          if( n > 1_ilp ) then
                             call stdlib_zlaset( 'L', n-1, n-1, czero, czero,a( 2_ilp, 1_ilp ), lda )
                                       
                          end if
                          ! bidiagonalize r in a
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_zgebrd( n, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in a
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'Q', 'R', 'N', m, n, n, a, lda,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in a
                          ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in a
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, n, m, 0_ilp, s, rwork( ie ), a,lda, u, ldu, &
                                    cdum, 1_ilp, rwork( irwork ),info )
                       end if
                    else if( wntvas ) then
                       ! path 9 (m much larger than n, jobu='a', jobvt='s'
                               ! or 'a')
                       ! m left singular vectors to be computed in u and
                       ! n right singular vectors to be computed in vt
                       if( lwork>=n*n+max( n+m, 3_ilp*n ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*n ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is n by n
                             ldwrku = n
                          end if
                          itau = iu + ldwrku*n
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need n*n+2*n, prefer n*n+n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need n*n+n+m, prefer n*n+n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r to work(iu), zeroing out below it
                          call stdlib_zlacpy( 'U', n, n, a, lda, work( iu ),ldwrku )
                          call stdlib_zlaset( 'L', n-1, n-1, czero, czero,work( iu+1 ), ldwrku )
                                    
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in work(iu), copying result to vt
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_zgebrd( n, n, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_zlacpy( 'U', n, n, work( iu ), ldwrku, vt,ldvt )
                          ! generate left bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need n*n+3*n, prefer n*n+2*n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', n, n, n, work( iu ), ldwrku,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (cworkspace: need   n*n+3*n-1,
                                       ! prefer n*n+2*n+(n-1)*nb)
                          ! (rworkspace: need   0)
                          call stdlib_zungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of r in work(iu) and computing
                          ! right singular vectors of r in vt
                          ! (cworkspace: need n*n)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, n, n, 0_ilp, s, rwork( ie ), vt,ldvt, work( iu )&
                                    , ldwrku, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply q in u by left singular vectors of r in
                          ! work(iu), storing result in a
                          ! (cworkspace: need n*n)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, n, cone, u, ldu,work( iu ), ldwrku, &
                                    czero, a, lda )
                          ! copy left singular vectors of a from a to u
                          call stdlib_zlacpy( 'F', m, n, a, lda, u, ldu )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + n
                          ! compute a=q*r, copying result to u
                          ! (cworkspace: need 2*n, prefer n+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgeqrf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, n, a, lda, u, ldu )
                          ! generate q in u
                          ! (cworkspace: need n+m, prefer n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungqr( m, m, n, u, ldu, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy r from a to vt, zeroing out below it
                          call stdlib_zlacpy( 'U', n, n, a, lda, vt, ldvt )
                          if( n>1_ilp )call stdlib_zlaset( 'L', n-1, n-1, czero, czero,vt( 2_ilp, 1_ilp ), &
                                    ldvt )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + n
                          iwork = itaup + n
                          ! bidiagonalize r in vt
                          ! (cworkspace: need 3*n, prefer 2*n+2*n*nb)
                          ! (rworkspace: need n)
                          call stdlib_zgebrd( n, n, vt, ldvt, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply q in u by left bidiagonalizing vectors
                          ! in vt
                          ! (cworkspace: need 2*n+m, prefer 2*n+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'Q', 'R', 'N', m, n, n, vt, ldvt,work( itauq ), u, &
                                    ldu, work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in vt
                          ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ),&
                                     lwork-iwork+1, ierr )
                          irwork = ie + n
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', n, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    end if
                 end if
              else
                 ! m < mnthr
                 ! path 10 (m at least n, but not much larger)
                 ! reduce to bidiagonal form without qr decomposition
                 ie = 1_ilp
                 itauq = 1_ilp
                 itaup = itauq + n
                 iwork = itaup + n
                 ! bidiagonalize a
                 ! (cworkspace: need 2*n+m, prefer 2*n+(m+n)*nb)
                 ! (rworkspace: need n)
                 call stdlib_zgebrd( m, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), &
                           work( iwork ), lwork-iwork+1,ierr )
                 if( wntuas ) then
                    ! if left singular vectors desired in u, copy result to u
                    ! and generate left bidiagonalizing vectors in u
                    ! (cworkspace: need 2*n+ncu, prefer 2*n+ncu*nb)
                    ! (rworkspace: 0)
                    call stdlib_zlacpy( 'L', m, n, a, lda, u, ldu )
                    if( wntus )ncu = n
                    if( wntua )ncu = m
                    call stdlib_zungbr( 'Q', m, ncu, n, u, ldu, work( itauq ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntvas ) then
                    ! if right singular vectors desired in vt, copy result to
                    ! vt and generate right bidiagonalizing vectors in vt
                    ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                    ! (rworkspace: 0)
                    call stdlib_zlacpy( 'U', n, n, a, lda, vt, ldvt )
                    call stdlib_zungbr( 'P', n, n, n, vt, ldvt, work( itaup ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntuo ) then
                    ! if left singular vectors desired in a, generate left
                    ! bidiagonalizing vectors in a
                    ! (cworkspace: need 3*n, prefer 2*n+n*nb)
                    ! (rworkspace: 0)
                    call stdlib_zungbr( 'Q', m, n, n, a, lda, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvo ) then
                    ! if right singular vectors desired in a, generate right
                    ! bidiagonalizing vectors in a
                    ! (cworkspace: need 3*n-1, prefer 2*n+(n-1)*nb)
                    ! (rworkspace: 0)
                    call stdlib_zungbr( 'P', n, n, n, a, lda, work( itaup ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 irwork = ie + n
                 if( wntuas .or. wntuo )nru = m
                 if( wntun )nru = 0_ilp
                 if( wntvas .or. wntvo )ncvt = n
                 if( wntvn )ncvt = 0_ilp
                 if( ( .not.wntuo ) .and. ( .not.wntvo ) ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in vt
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_zbdsqr( 'U', n, ncvt, nru, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                              cdum, 1_ilp, rwork( irwork ),info )
                 else if( ( .not.wntuo ) .and. wntvo ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in a
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_zbdsqr( 'U', n, ncvt, nru, 0_ilp, s, rwork( ie ), a,lda, u, ldu, cdum,&
                               1_ilp, rwork( irwork ),info )
                 else
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in a and computing right singular
                    ! vectors in vt
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_zbdsqr( 'U', n, ncvt, nru, 0_ilp, s, rwork( ie ), vt,ldvt, a, lda, &
                              cdum, 1_ilp, rwork( irwork ),info )
                 end if
              end if
           else
              ! a has more columns than rows. if a has sufficiently more
              ! columns than rows, first reduce using the lq decomposition (if
              ! sufficient workspace available)
              if( n>=mnthr ) then
                 if( wntvn ) then
                    ! path 1t(n much larger than m, jobvt='n')
                    ! no right singular vectors to be computed
                    itau = 1_ilp
                    iwork = itau + m
                    ! compute a=l*q
                    ! (cworkspace: need 2*m, prefer m+m*nb)
                    ! (rworkspace: 0)
                    call stdlib_zgelqf( m, n, a, lda, work( itau ), work( iwork ),lwork-iwork+1, &
                              ierr )
                    ! zero out above l
                    if (m>1_ilp) call stdlib_zlaset( 'U', m-1, m-1, czero, czero, a( 1_ilp, 2_ilp ),lda )
                    ie = 1_ilp
                    itauq = 1_ilp
                    itaup = itauq + m
                    iwork = itaup + m
                    ! bidiagonalize l in a
                    ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                    ! (rworkspace: need m)
                    call stdlib_zgebrd( m, m, a, lda, s, rwork( ie ), work( itauq ),work( itaup ),&
                               work( iwork ), lwork-iwork+1,ierr )
                    if( wntuo .or. wntuas ) then
                       ! if left singular vectors desired, generate q
                       ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                    end if
                    irwork = ie + m
                    nru = 0_ilp
                    if( wntuo .or. wntuas )nru = m
                    ! perform bidiagonal qr iteration, computing left singular
                    ! vectors of a in a if desired
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_zbdsqr( 'U', m, 0_ilp, nru, 0_ilp, s, rwork( ie ), cdum, 1_ilp,a, lda, cdum, &
                              1_ilp, rwork( irwork ), info )
                    ! if left singular vectors desired in u, copy them there
                    if( wntuas )call stdlib_zlacpy( 'F', m, m, a, lda, u, ldu )
                 else if( wntvo .and. wntun ) then
                    ! path 2t(n much larger than m, jobu='n', jobvt='o')
                    ! m right singular vectors to be overwritten on a and
                    ! no left singular vectors to be computed
                    if( lwork>=m*m+3*m ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n )+lda*m ) then
                          ! work(iu) is lda by n and work(ir) is lda by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n )+m*m ) then
                          ! work(iu) is lda by n and work(ir) is m by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = m
                       else
                          ! work(iu) is m by chunk and work(ir) is m by m
                          ldwrku = m
                          chunk = ( lwork-m*m ) / m
                          ldwrkr = m
                       end if
                       itau = ir + ldwrkr*m
                       iwork = itau + m
                       ! compute a=l*q
                       ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to work(ir) and zero out above it
                       call stdlib_zlacpy( 'L', m, m, a, lda, work( ir ), ldwrkr )
                       call stdlib_zlaset( 'U', m-1, m-1, czero, czero,work( ir+ldwrkr ), ldwrkr )
                                 
                       ! generate q in a
                       ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_zunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in work(ir)
                       ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                       ! (rworkspace: need m)
                       call stdlib_zgebrd( m, m, work( ir ), ldwrkr, s, rwork( ie ),work( itauq ),&
                                  work( itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing l
                       ! (cworkspace: need m*m+3*m-1, prefer m*m+2*m+(m-1)*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       irwork = ie + m
                       ! perform bidiagonal qr iteration, computing right
                       ! singular vectors of l in work(ir)
                       ! (cworkspace: need m*m)
                       ! (rworkspace: need bdspac)
                       call stdlib_zbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, &
                                 cdum, 1_ilp, cdum, 1_ilp,rwork( irwork ), info )
                       iu = itauq
                       ! multiply right singular vectors of l in work(ir) by q
                       ! in a, storing result in work(iu) and copying to a
                       ! (cworkspace: need m*m+m, prefer m*m+m*n)
                       ! (rworkspace: 0)
                       do i = 1, n, chunk
                          blk = min( n-i+1, chunk )
                          call stdlib_zgemm( 'N', 'N', m, blk, m, cone, work( ir ),ldwrkr, a( 1_ilp, &
                                    i ), lda, czero,work( iu ), ldwrku )
                          call stdlib_zlacpy( 'F', m, blk, work( iu ), ldwrku,a( 1_ilp, i ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       ie = 1_ilp
                       itauq = 1_ilp
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize a
                       ! (cworkspace: need 2*m+n, prefer 2*m+(m+n)*nb)
                       ! (rworkspace: need m)
                       call stdlib_zgebrd( m, n, a, lda, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! generate right vectors bidiagonalizing a
                       ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + m
                       ! perform bidiagonal qr iteration, computing right
                       ! singular vectors of a in a
                       ! (cworkspace: 0)
                       ! (rworkspace: need bdspac)
                       call stdlib_zbdsqr( 'L', m, n, 0_ilp, 0_ilp, s, rwork( ie ), a, lda,cdum, 1_ilp, cdum, &
                                 1_ilp, rwork( irwork ), info )
                    end if
                 else if( wntvo .and. wntuas ) then
                    ! path 3t(n much larger than m, jobu='s' or 'a', jobvt='o')
                    ! m right singular vectors to be overwritten on a and
                    ! m left singular vectors to be computed in u
                    if( lwork>=m*m+3*m ) then
                       ! sufficient workspace for a fast algorithm
                       ir = 1_ilp
                       if( lwork>=max( wrkbl, lda*n )+lda*m ) then
                          ! work(iu) is lda by n and work(ir) is lda by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = lda
                       else if( lwork>=max( wrkbl, lda*n )+m*m ) then
                          ! work(iu) is lda by n and work(ir) is m by m
                          ldwrku = lda
                          chunk = n
                          ldwrkr = m
                       else
                          ! work(iu) is m by chunk and work(ir) is m by m
                          ldwrku = m
                          chunk = ( lwork-m*m ) / m
                          ldwrkr = m
                       end if
                       itau = ir + ldwrkr*m
                       iwork = itau + m
                       ! compute a=l*q
                       ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to u, zeroing about above it
                       call stdlib_zlacpy( 'L', m, m, a, lda, u, ldu )
                       if (m>1_ilp) call stdlib_zlaset( 'U', m-1, m-1, czero, czero, u( 1_ilp, 2_ilp ),ldu )
                       ! generate q in a
                       ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_zunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in u, copying result to work(ir)
                       ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                       ! (rworkspace: need m)
                       call stdlib_zgebrd( m, m, u, ldu, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       call stdlib_zlacpy( 'U', m, m, u, ldu, work( ir ), ldwrkr )
                       ! generate right vectors bidiagonalizing l in work(ir)
                       ! (cworkspace: need m*m+3*m-1, prefer m*m+2*m+(m-1)*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), work( &
                                 iwork ),lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing l in u
                       ! (cworkspace: need m*m+3*m, prefer m*m+2*m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + m
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of l in u, and computing right
                       ! singular vectors of l in work(ir)
                       ! (cworkspace: need m*m)
                       ! (rworkspace: need bdspac)
                       call stdlib_zbdsqr( 'U', m, m, m, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, u, &
                                 ldu, cdum, 1_ilp,rwork( irwork ), info )
                       iu = itauq
                       ! multiply right singular vectors of l in work(ir) by q
                       ! in a, storing result in work(iu) and copying to a
                       ! (cworkspace: need m*m+m, prefer m*m+m*n))
                       ! (rworkspace: 0)
                       do i = 1, n, chunk
                          blk = min( n-i+1, chunk )
                          call stdlib_zgemm( 'N', 'N', m, blk, m, cone, work( ir ),ldwrkr, a( 1_ilp, &
                                    i ), lda, czero,work( iu ), ldwrku )
                          call stdlib_zlacpy( 'F', m, blk, work( iu ), ldwrku,a( 1_ilp, i ), lda )
                                    
                       end do
                    else
                       ! insufficient workspace for a fast algorithm
                       itau = 1_ilp
                       iwork = itau + m
                       ! compute a=l*q
                       ! (cworkspace: need 2*m, prefer m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-iwork+&
                                 1_ilp, ierr )
                       ! copy l to u, zeroing out above it
                       call stdlib_zlacpy( 'L', m, m, a, lda, u, ldu )
                       if (m>1_ilp) call stdlib_zlaset( 'U', m-1, m-1, czero, czero, u( 1_ilp, 2_ilp ),ldu )
                       ! generate q in a
                       ! (cworkspace: need 2*m, prefer m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_zunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                 iwork+1, ierr )
                       ie = 1_ilp
                       itauq = itau
                       itaup = itauq + m
                       iwork = itaup + m
                       ! bidiagonalize l in u
                       ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                       ! (rworkspace: need m)
                       call stdlib_zgebrd( m, m, u, ldu, s, rwork( ie ),work( itauq ), work( &
                                 itaup ),work( iwork ), lwork-iwork+1, ierr )
                       ! multiply right vectors bidiagonalizing l by q in a
                       ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                       ! (rworkspace: 0)
                       call stdlib_zunmbr( 'P', 'L', 'C', m, n, m, u, ldu,work( itaup ), a, lda, &
                                 work( iwork ),lwork-iwork+1, ierr )
                       ! generate left vectors bidiagonalizing l in u
                       ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                       ! (rworkspace: 0)
                       call stdlib_zungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                 lwork-iwork+1, ierr )
                       irwork = ie + m
                       ! perform bidiagonal qr iteration, computing left
                       ! singular vectors of a in u and computing right
                       ! singular vectors of a in a
                       ! (cworkspace: 0)
                       ! (rworkspace: need bdspac)
                       call stdlib_zbdsqr( 'U', m, n, m, 0_ilp, s, rwork( ie ), a, lda,u, ldu, cdum, &
                                 1_ilp, rwork( irwork ), info )
                    end if
                 else if( wntvs ) then
                    if( wntun ) then
                       ! path 4t(n much larger than m, jobu='n', jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! no left singular vectors to be computed
                       if( lwork>=m*m+3*m ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(ir) is lda by m
                             ldwrkr = lda
                          else
                             ! work(ir) is m by m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(ir), zeroing out above it
                          call stdlib_zlacpy( 'L', m, m, a, lda, work( ir ),ldwrkr )
                          call stdlib_zlaset( 'U', m-1, m-1, czero, czero,work( ir+ldwrkr ), &
                                    ldwrkr )
                          ! generate q in a
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(ir)
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_zgebrd( m, m, work( ir ), ldwrkr, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate right vectors bidiagonalizing l in
                          ! work(ir)
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of l in work(ir)
                          ! (cworkspace: need m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, &
                                    cdum, 1_ilp, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply right singular vectors of l in work(ir) by
                          ! q in a, storing result in vt
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, m, cone, work( ir ),ldwrkr, a, lda, &
                                    czero, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy result to vt
                          call stdlib_zlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_zlaset( 'U', m-1, m-1, czero, czero,a( 1_ilp, 2_ilp ), lda )
                          ! bidiagonalize l in a
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_zgebrd( m, m, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right vectors bidiagonalizing l by q in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'P', 'L', 'C', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, n, 0_ilp, 0_ilp, s, rwork( ie ), vt,ldvt, cdum, 1_ilp, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    else if( wntuo ) then
                       ! path 5t(n much larger than m, jobu='o', jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! m left singular vectors to be overwritten on a
                       if( lwork>=2_ilp*m*m+3*m ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*m ) then
                             ! work(iu) is lda by m and work(ir) is lda by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+m )*m ) then
                             ! work(iu) is lda by m and work(ir) is m by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          else
                             ! work(iu) is m by m and work(ir) is m by m
                             ldwrku = m
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (cworkspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(iu), zeroing out below it
                          call stdlib_zlacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_zlaset( 'U', m-1, m-1, czero, czero,work( iu+ldwrku ), &
                                    ldwrku )
                          ! generate q in a
                          ! (cworkspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to
                          ! work(ir)
                          ! (cworkspace: need   2*m*m+3*m,
                                       ! prefer 2*m*m+2*m+2*m*nb)
                          ! (rworkspace: need   m)
                          call stdlib_zgebrd( m, m, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, m, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need   2*m*m+3*m-1,
                                       ! prefer 2*m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need 2*m*m+3*m, prefer 2*m*m+2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', m, m, m, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in work(ir) and computing
                          ! right singular vectors of l in work(iu)
                          ! (cworkspace: need 2*m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, m, m, 0_ilp, s, rwork( ie ),work( iu ), ldwrku, &
                                    work( ir ),ldwrkr, cdum, 1_ilp, rwork( irwork ),info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in a, storing result in vt
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, m, cone, work( iu ),ldwrku, a, lda, &
                                    czero, vt, ldvt )
                          ! copy left singular vectors of l to a
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_zlacpy( 'F', m, m, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_zlaset( 'U', m-1, m-1, czero, czero,a( 1_ilp, 2_ilp ), lda )
                          ! bidiagonalize l in a
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_zgebrd( m, m, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right vectors bidiagonalizing l by q in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'P', 'L', 'C', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors of l in a
                          ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in a and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, a, lda, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    else if( wntuas ) then
                       ! path 6t(n much larger than m, jobu='s' or 'a',
                               ! jobvt='s')
                       ! m right singular vectors to be computed in vt and
                       ! m left singular vectors to be computed in u
                       if( lwork>=m*m+3*m ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(iu) is lda by n
                             ldwrku = lda
                          else
                             ! work(iu) is lda by m
                             ldwrku = m
                          end if
                          itau = iu + ldwrku*m
                          iwork = itau + m
                          ! compute a=l*q
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_zlacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_zlaset( 'U', m-1, m-1, czero, czero,work( iu+ldwrku ), &
                                    ldwrku )
                          ! generate q in a
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( m, n, m, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to u
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_zgebrd( m, m, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, m, work( iu ), ldwrku, u,ldu )
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need   m*m+3*m-1,
                                       ! prefer m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in u and computing right
                          ! singular vectors of l in work(iu)
                          ! (cworkspace: need m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, m, m, 0_ilp, s, rwork( ie ),work( iu ), ldwrku, &
                                    u, ldu, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in a, storing result in vt
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, m, cone, work( iu ),ldwrku, a, lda, &
                                    czero, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( m, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to u, zeroing out above it
                          call stdlib_zlacpy( 'L', m, m, a, lda, u, ldu )
                          if (m>1_ilp) call stdlib_zlaset( 'U', m-1, m-1, czero, czero,u( 1_ilp, 2_ilp ), ldu )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in u
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_zgebrd( m, m, u, ldu, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in u by q
                          ! in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'P', 'L', 'C', m, n, m, u, ldu,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    end if
                 else if( wntva ) then
                    if( wntun ) then
                       ! path 7t(n much larger than m, jobu='n', jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! no left singular vectors to be computed
                       if( lwork>=m*m+max( n+m, 3_ilp*m ) ) then
                          ! sufficient workspace for a fast algorithm
                          ir = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(ir) is lda by m
                             ldwrkr = lda
                          else
                             ! work(ir) is m by m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! copy l to work(ir), zeroing out above it
                          call stdlib_zlacpy( 'L', m, m, a, lda, work( ir ),ldwrkr )
                          call stdlib_zlaset( 'U', m-1, m-1, czero, czero,work( ir+ldwrkr ), &
                                    ldwrkr )
                          ! generate q in vt
                          ! (cworkspace: need m*m+m+n, prefer m*m+m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(ir)
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_zgebrd( m, m, work( ir ), ldwrkr, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          ! generate right bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need   m*m+3*m-1,
                                       ! prefer m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', m, m, m, work( ir ), ldwrkr,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of l in work(ir)
                          ! (cworkspace: need m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, m, 0_ilp, 0_ilp, s, rwork( ie ),work( ir ), ldwrkr, &
                                    cdum, 1_ilp, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply right singular vectors of l in work(ir) by
                          ! q in vt, storing result in a
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, m, cone, work( ir ),ldwrkr, vt, ldvt,&
                                     czero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_zlacpy( 'F', m, n, a, lda, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need m+n, prefer m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_zlaset( 'U', m-1, m-1, czero, czero,a( 1_ilp, 2_ilp ), lda )
                          ! bidiagonalize l in a
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_zgebrd( m, m, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in a by q
                          ! in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'P', 'L', 'C', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, n, 0_ilp, 0_ilp, s, rwork( ie ), vt,ldvt, cdum, 1_ilp, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    else if( wntuo ) then
                       ! path 8t(n much larger than m, jobu='o', jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! m left singular vectors to be overwritten on a
                       if( lwork>=2_ilp*m*m+max( n+m, 3_ilp*m ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+2*lda*m ) then
                             ! work(iu) is lda by m and work(ir) is lda by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = lda
                          else if( lwork>=wrkbl+( lda+m )*m ) then
                             ! work(iu) is lda by m and work(ir) is m by m
                             ldwrku = lda
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          else
                             ! work(iu) is m by m and work(ir) is m by m
                             ldwrku = m
                             ir = iu + ldwrku*m
                             ldwrkr = m
                          end if
                          itau = ir + ldwrkr*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m*m+2*m, prefer 2*m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need 2*m*m+m+n, prefer 2*m*m+m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_zlacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_zlaset( 'U', m-1, m-1, czero, czero,work( iu+ldwrku ), &
                                    ldwrku )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to
                          ! work(ir)
                          ! (cworkspace: need   2*m*m+3*m,
                                       ! prefer 2*m*m+2*m+2*m*nb)
                          ! (rworkspace: need   m)
                          call stdlib_zgebrd( m, m, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, m, work( iu ), ldwrku,work( ir ), ldwrkr )
                                    
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need   2*m*m+3*m-1,
                                       ! prefer 2*m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in work(ir)
                          ! (cworkspace: need 2*m*m+3*m, prefer 2*m*m+2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', m, m, m, work( ir ), ldwrkr,work( itauq ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in work(ir) and computing
                          ! right singular vectors of l in work(iu)
                          ! (cworkspace: need 2*m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, m, m, 0_ilp, s, rwork( ie ),work( iu ), ldwrku, &
                                    work( ir ),ldwrkr, cdum, 1_ilp, rwork( irwork ),info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in vt, storing result in a
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, m, cone, work( iu ),ldwrku, vt, ldvt,&
                                     czero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_zlacpy( 'F', m, n, a, lda, vt, ldvt )
                          ! copy left singular vectors of a from work(ir) to a
                          call stdlib_zlacpy( 'F', m, m, work( ir ), ldwrkr, a,lda )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need m+n, prefer m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! zero out above l in a
                          if (m>1_ilp) call stdlib_zlaset( 'U', m-1, m-1, czero, czero,a( 1_ilp, 2_ilp ), lda )
                          ! bidiagonalize l in a
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_zgebrd( m, m, a, lda, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in a by q
                          ! in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'P', 'L', 'C', m, n, m, a, lda,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in a
                          ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', m, m, m, a, lda, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in a and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, a, lda, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    else if( wntuas ) then
                       ! path 9t(n much larger than m, jobu='s' or 'a',
                               ! jobvt='a')
                       ! n right singular vectors to be computed in vt and
                       ! m left singular vectors to be computed in u
                       if( lwork>=m*m+max( n+m, 3_ilp*m ) ) then
                          ! sufficient workspace for a fast algorithm
                          iu = 1_ilp
                          if( lwork>=wrkbl+lda*m ) then
                             ! work(iu) is lda by m
                             ldwrku = lda
                          else
                             ! work(iu) is m by m
                             ldwrku = m
                          end if
                          itau = iu + ldwrku*m
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need m*m+2*m, prefer m*m+m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need m*m+m+n, prefer m*m+m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to work(iu), zeroing out above it
                          call stdlib_zlacpy( 'L', m, m, a, lda, work( iu ),ldwrku )
                          call stdlib_zlaset( 'U', m-1, m-1, czero, czero,work( iu+ldwrku ), &
                                    ldwrku )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in work(iu), copying result to u
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_zgebrd( m, m, work( iu ), ldwrku, s,rwork( ie ), work( &
                                    itauq ),work( itaup ), work( iwork ),lwork-iwork+1, ierr )
                          call stdlib_zlacpy( 'L', m, m, work( iu ), ldwrku, u,ldu )
                          ! generate right bidiagonalizing vectors in work(iu)
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+(m-1)*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'P', m, m, m, work( iu ), ldwrku,work( itaup ), &
                                    work( iwork ),lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (cworkspace: need m*m+3*m, prefer m*m+2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of l in u and computing right
                          ! singular vectors of l in work(iu)
                          ! (cworkspace: need m*m)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, m, m, 0_ilp, s, rwork( ie ),work( iu ), ldwrku, &
                                    u, ldu, cdum, 1_ilp,rwork( irwork ), info )
                          ! multiply right singular vectors of l in work(iu) by
                          ! q in vt, storing result in a
                          ! (cworkspace: need m*m)
                          ! (rworkspace: 0)
                          call stdlib_zgemm( 'N', 'N', m, n, m, cone, work( iu ),ldwrku, vt, ldvt,&
                                     czero, a, lda )
                          ! copy right singular vectors of a from a to vt
                          call stdlib_zlacpy( 'F', m, n, a, lda, vt, ldvt )
                       else
                          ! insufficient workspace for a fast algorithm
                          itau = 1_ilp
                          iwork = itau + m
                          ! compute a=l*q, copying result to vt
                          ! (cworkspace: need 2*m, prefer m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zgelqf( m, n, a, lda, work( itau ),work( iwork ), lwork-&
                                    iwork+1, ierr )
                          call stdlib_zlacpy( 'U', m, n, a, lda, vt, ldvt )
                          ! generate q in vt
                          ! (cworkspace: need m+n, prefer m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunglq( n, n, m, vt, ldvt, work( itau ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          ! copy l to u, zeroing out above it
                          call stdlib_zlacpy( 'L', m, m, a, lda, u, ldu )
                          if (m>1_ilp) call stdlib_zlaset( 'U', m-1, m-1, czero, czero,u( 1_ilp, 2_ilp ), ldu )
                          ie = 1_ilp
                          itauq = itau
                          itaup = itauq + m
                          iwork = itaup + m
                          ! bidiagonalize l in u
                          ! (cworkspace: need 3*m, prefer 2*m+2*m*nb)
                          ! (rworkspace: need m)
                          call stdlib_zgebrd( m, m, u, ldu, s, rwork( ie ),work( itauq ), work( &
                                    itaup ),work( iwork ), lwork-iwork+1, ierr )
                          ! multiply right bidiagonalizing vectors in u by q
                          ! in vt
                          ! (cworkspace: need 2*m+n, prefer 2*m+n*nb)
                          ! (rworkspace: 0)
                          call stdlib_zunmbr( 'P', 'L', 'C', m, n, m, u, ldu,work( itaup ), vt, &
                                    ldvt,work( iwork ), lwork-iwork+1, ierr )
                          ! generate left bidiagonalizing vectors in u
                          ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                          ! (rworkspace: 0)
                          call stdlib_zungbr( 'Q', m, m, m, u, ldu, work( itauq ),work( iwork ), &
                                    lwork-iwork+1, ierr )
                          irwork = ie + m
                          ! perform bidiagonal qr iteration, computing left
                          ! singular vectors of a in u and computing right
                          ! singular vectors of a in vt
                          ! (cworkspace: 0)
                          ! (rworkspace: need bdspac)
                          call stdlib_zbdsqr( 'U', m, n, m, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                                    cdum, 1_ilp,rwork( irwork ), info )
                       end if
                    end if
                 end if
              else
                 ! n < mnthr
                 ! path 10t(n greater than m, but not much larger)
                 ! reduce to bidiagonal form without lq decomposition
                 ie = 1_ilp
                 itauq = 1_ilp
                 itaup = itauq + m
                 iwork = itaup + m
                 ! bidiagonalize a
                 ! (cworkspace: need 2*m+n, prefer 2*m+(m+n)*nb)
                 ! (rworkspace: m)
                 call stdlib_zgebrd( m, n, a, lda, s, rwork( ie ), work( itauq ),work( itaup ), &
                           work( iwork ), lwork-iwork+1,ierr )
                 if( wntuas ) then
                    ! if left singular vectors desired in u, copy result to u
                    ! and generate left bidiagonalizing vectors in u
                    ! (cworkspace: need 3*m-1, prefer 2*m+(m-1)*nb)
                    ! (rworkspace: 0)
                    call stdlib_zlacpy( 'L', m, m, a, lda, u, ldu )
                    call stdlib_zungbr( 'Q', m, m, n, u, ldu, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvas ) then
                    ! if right singular vectors desired in vt, copy result to
                    ! vt and generate right bidiagonalizing vectors in vt
                    ! (cworkspace: need 2*m+nrvt, prefer 2*m+nrvt*nb)
                    ! (rworkspace: 0)
                    call stdlib_zlacpy( 'U', m, n, a, lda, vt, ldvt )
                    if( wntva )nrvt = n
                    if( wntvs )nrvt = m
                    call stdlib_zungbr( 'P', nrvt, n, m, vt, ldvt, work( itaup ),work( iwork ), &
                              lwork-iwork+1, ierr )
                 end if
                 if( wntuo ) then
                    ! if left singular vectors desired in a, generate left
                    ! bidiagonalizing vectors in a
                    ! (cworkspace: need 3*m-1, prefer 2*m+(m-1)*nb)
                    ! (rworkspace: 0)
                    call stdlib_zungbr( 'Q', m, m, n, a, lda, work( itauq ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 if( wntvo ) then
                    ! if right singular vectors desired in a, generate right
                    ! bidiagonalizing vectors in a
                    ! (cworkspace: need 3*m, prefer 2*m+m*nb)
                    ! (rworkspace: 0)
                    call stdlib_zungbr( 'P', m, n, m, a, lda, work( itaup ),work( iwork ), lwork-&
                              iwork+1, ierr )
                 end if
                 irwork = ie + m
                 if( wntuas .or. wntuo )nru = m
                 if( wntun )nru = 0_ilp
                 if( wntvas .or. wntvo )ncvt = n
                 if( wntvn )ncvt = 0_ilp
                 if( ( .not.wntuo ) .and. ( .not.wntvo ) ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in vt
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_zbdsqr( 'L', m, ncvt, nru, 0_ilp, s, rwork( ie ), vt,ldvt, u, ldu, &
                              cdum, 1_ilp, rwork( irwork ),info )
                 else if( ( .not.wntuo ) .and. wntvo ) then
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in u and computing right singular
                    ! vectors in a
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_zbdsqr( 'L', m, ncvt, nru, 0_ilp, s, rwork( ie ), a,lda, u, ldu, cdum,&
                               1_ilp, rwork( irwork ),info )
                 else
                    ! perform bidiagonal qr iteration, if desired, computing
                    ! left singular vectors in a and computing right singular
                    ! vectors in vt
                    ! (cworkspace: 0)
                    ! (rworkspace: need bdspac)
                    call stdlib_zbdsqr( 'L', m, ncvt, nru, 0_ilp, s, rwork( ie ), vt,ldvt, a, lda, &
                              cdum, 1_ilp, rwork( irwork ),info )
                 end if
              end if
           end if
           ! undo scaling if necessary
           if( iscl==1_ilp ) then
              if( anrm>bignum )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn, 1_ilp, s, minmn,&
                        ierr )
              if( info/=0_ilp .and. anrm>bignum )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, bignum, anrm, minmn-1,&
                         1_ilp,rwork( ie ), minmn, ierr )
              if( anrm<smlnum )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn, 1_ilp, s, minmn,&
                        ierr )
              if( info/=0_ilp .and. anrm<smlnum )call stdlib_dlascl( 'G', 0_ilp, 0_ilp, smlnum, anrm, minmn-1,&
                         1_ilp,rwork( ie ), minmn, ierr )
           end if
           ! return optimal workspace in work(1)
           work( 1_ilp ) = maxwrk
           return
     end subroutine stdlib_zgesvd




     module subroutine stdlib_sgesvdq( joba, jobp, jobr, jobu, jobv, m, n, a, lda,s, u, ldu, v, ldv, &
     !! SGESVDQ computes the singular value decomposition (SVD) of a real
     !! M-by-N matrix A, where M >= N. The SVD of A is written as
     !! [++]   [xx]   [x0]   [xx]
     !! A = U * SIGMA * V^*,  [++] = [xx] * [ox] * [xx]
     !! [++]   [xx]
     !! where SIGMA is an N-by-N diagonal matrix, U is an M-by-N orthonormal
     !! matrix, and V is an N-by-N orthogonal matrix. The diagonal elements
     !! of SIGMA are the singular values of A. The columns of U and V are the
     !! left and the right singular vectors of A, respectively.
               numrank, iwork, liwork,work, lwork, rwork, lrwork, info )
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: joba, jobp, jobr, jobu, jobv
           integer(ilp), intent(in) :: m, n, lda, ldu, ldv, liwork, lrwork
           integer(ilp), intent(out) :: numrank, info
           integer(ilp), intent(inout) :: lwork
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: u(ldu,*), v(ldv,*), work(*)
           real(sp), intent(out) :: s(*), rwork(*)
           integer(ilp), intent(out) :: iwork(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: ierr, iwoff, nr, n1, optratio, p, q
           integer(ilp) :: lwcon, lwqp3, lwrk_sgelqf, lwrk_sgesvd, lwrk_sgesvd2, lwrk_sgeqp3, &
           lwrk_sgeqrf, lwrk_sormlq, lwrk_sormqr, lwrk_sormqr2, lwlqf, lwqrf, lwsvd, lwsvd2, &
                     lworq, lworq2, lwunlq, minwrk, minwrk2, optwrk, optwrk2, iminwrk, rminwrk
           logical(lk) :: accla, acclm, acclh, ascaled, conda, dntwu, dntwv, lquery, lsvc0, lsvec,&
                      rowprm, rsvec, rtrans, wntua, wntuf, wntur, wntus, wntva, wntvr
           real(sp) :: big, epsln, rtmp, sconda, sfmin
           ! Local Arrays
           real(sp) :: rdummy(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           wntus  = stdlib_lsame( jobu, 'S' ) .or. stdlib_lsame( jobu, 'U' )
           wntur  = stdlib_lsame( jobu, 'R' )
           wntua  = stdlib_lsame( jobu, 'A' )
           wntuf  = stdlib_lsame( jobu, 'F' )
           lsvc0  = wntus .or. wntur .or. wntua
           lsvec  = lsvc0 .or. wntuf
           dntwu  = stdlib_lsame( jobu, 'N' )
           wntvr  = stdlib_lsame( jobv, 'R' )
           wntva  = stdlib_lsame( jobv, 'A' ) .or. stdlib_lsame( jobv, 'V' )
           rsvec  = wntvr .or. wntva
           dntwv  = stdlib_lsame( jobv, 'N' )
           accla  = stdlib_lsame( joba, 'A' )
           acclm  = stdlib_lsame( joba, 'M' )
           conda  = stdlib_lsame( joba, 'E' )
           acclh  = stdlib_lsame( joba, 'H' ) .or. conda
           rowprm = stdlib_lsame( jobp, 'P' )
           rtrans = stdlib_lsame( jobr, 'T' )
           if ( rowprm ) then
              if ( conda ) then
                 iminwrk = max( 1_ilp, n + m - 1_ilp + n )
              else
                 iminwrk = max( 1_ilp, n + m - 1_ilp )
              end if
              rminwrk = max( 2_ilp, m )
           else
              if ( conda ) then
                 iminwrk = max( 1_ilp, n + n )
              else
                 iminwrk = max( 1_ilp, n )
              end if
              rminwrk = 2_ilp
           end if
           lquery = (liwork == -1_ilp .or. lwork == -1_ilp .or. lrwork == -1_ilp)
           info  = 0_ilp
           if ( .not. ( accla .or. acclm .or. acclh ) ) then
              info = -1_ilp
           else if ( .not.( rowprm .or. stdlib_lsame( jobp, 'N' ) ) ) then
               info = -2_ilp
           else if ( .not.( rtrans .or. stdlib_lsame( jobr, 'N' ) ) ) then
               info = -3_ilp
           else if ( .not.( lsvec .or. dntwu ) ) then
              info = -4_ilp
           else if ( wntur .and. wntva ) then
              info = -5_ilp
           else if ( .not.( rsvec .or. dntwv )) then
              info = -5_ilp
           else if ( m<0_ilp ) then
              info = -6_ilp
           else if ( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -7_ilp
           else if ( lda<max( 1_ilp, m ) ) then
              info = -9_ilp
           else if ( ldu<1_ilp .or. ( lsvc0 .and. ldu<m ) .or.( wntuf .and. ldu<n ) ) then
              info = -12_ilp
           else if ( ldv<1_ilp .or. ( rsvec .and. ldv<n ) .or.( conda .and. ldv<n ) ) then
              info = -14_ilp
           else if ( liwork < iminwrk .and. .not. lquery ) then
              info = -17_ilp
           end if
           if ( info == 0_ilp ) then
              ! Compute The Minimal And The Optimal Workspace Lengths
              ! [[the expressions for computing the minimal and the optimal
              ! values of lwork are written with a lot of redundancy and
              ! can be simplified. however, this detailed form is easier for
              ! maintenance and modifications of the code.]]
              ! Minimal Workspace Length For Stdlib_Sgeqp3 Of An M X N Matrix
              lwqp3 = 3_ilp * n + 1_ilp
              ! Minimal Workspace Length For Stdlib_Sormqr To Build Left Singular Vectors
              if ( wntus .or. wntur ) then
                  lworq  = max( n  , 1_ilp )
              else if ( wntua ) then
                  lworq = max( m , 1_ilp )
              end if
              ! Minimal Workspace Length For Stdlib_Spocon Of An N X N Matrix
              lwcon = 3_ilp * n
              ! Stdlib_Sgesvd Of An N X N Matrix
              lwsvd = max( 5_ilp * n, 1_ilp )
              if ( lquery ) then
                  call stdlib_sgeqp3( m, n, a, lda, iwork, rdummy, rdummy, -1_ilp,ierr )
                  lwrk_sgeqp3 = int( rdummy(1_ilp),KIND=ilp)
                  if ( wntus .or. wntur ) then
                      call stdlib_sormqr( 'L', 'N', m, n, n, a, lda, rdummy, u,ldu, rdummy, -1_ilp, &
                                ierr )
                      lwrk_sormqr = int( rdummy(1_ilp),KIND=ilp)
                  else if ( wntua ) then
                      call stdlib_sormqr( 'L', 'N', m, m, n, a, lda, rdummy, u,ldu, rdummy, -1_ilp, &
                                ierr )
                      lwrk_sormqr = int( rdummy(1_ilp),KIND=ilp)
                  else
                      lwrk_sormqr = 0_ilp
                  end if
              end if
              minwrk = 2_ilp
              optwrk = 2_ilp
              if ( .not. (lsvec .or. rsvec )) then
                  ! Minimal And Optimal Sizes Of The Workspace If
                  ! only the singular values are requested
                  if ( conda ) then
                     minwrk = max( n+lwqp3, lwcon, lwsvd )
                  else
                     minwrk = max( n+lwqp3, lwsvd )
                  end if
                  if ( lquery ) then
                      call stdlib_sgesvd( 'N', 'N', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -1_ilp, &
                                ierr )
                      lwrk_sgesvd = int( rdummy(1_ilp),KIND=ilp)
                      if ( conda ) then
                         optwrk = max( n+lwrk_sgeqp3, n+lwcon, lwrk_sgesvd )
                      else
                         optwrk = max( n+lwrk_sgeqp3, lwrk_sgesvd )
                      end if
                  end if
              else if ( lsvec .and. (.not.rsvec) ) then
                  ! Minimal And Optimal Sizes Of The Workspace If The
                  ! singular values and the left singular vectors are requested
                  if ( conda ) then
                      minwrk = n + max( lwqp3, lwcon, lwsvd, lworq )
                  else
                      minwrk = n + max( lwqp3, lwsvd, lworq )
                  end if
                  if ( lquery ) then
                     if ( rtrans ) then
                        call stdlib_sgesvd( 'N', 'O', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -1_ilp, &
                                  ierr )
                     else
                        call stdlib_sgesvd( 'O', 'N', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -1_ilp, &
                                  ierr )
                     end if
                     lwrk_sgesvd = int( rdummy(1_ilp),KIND=ilp)
                     if ( conda ) then
                         optwrk = n + max( lwrk_sgeqp3, lwcon, lwrk_sgesvd,lwrk_sormqr )
                     else
                         optwrk = n + max( lwrk_sgeqp3, lwrk_sgesvd,lwrk_sormqr )
                     end if
                  end if
              else if ( rsvec .and. (.not.lsvec) ) then
                  ! Minimal And Optimal Sizes Of The Workspace If The
                  ! singular values and the right singular vectors are requested
                  if ( conda ) then
                      minwrk = n + max( lwqp3, lwcon, lwsvd )
                  else
                      minwrk = n + max( lwqp3, lwsvd )
                  end if
                  if ( lquery ) then
                      if ( rtrans ) then
                          call stdlib_sgesvd( 'O', 'N', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -&
                                    1_ilp, ierr )
                      else
                          call stdlib_sgesvd( 'N', 'O', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -&
                                    1_ilp, ierr )
                      end if
                      lwrk_sgesvd = int( rdummy(1_ilp),KIND=ilp)
                      if ( conda ) then
                          optwrk = n + max( lwrk_sgeqp3, lwcon, lwrk_sgesvd )
                      else
                          optwrk = n + max( lwrk_sgeqp3, lwrk_sgesvd )
                      end if
                  end if
              else
                  ! Minimal And Optimal Sizes Of The Workspace If The
                  ! full svd is requested
                  if ( rtrans ) then
                      minwrk = max( lwqp3, lwsvd, lworq )
                      if ( conda ) minwrk = max( minwrk, lwcon )
                      minwrk = minwrk + n
                      if ( wntva ) then
                         ! .. minimal workspace length for n x n/2 stdlib_sgeqrf
                         lwqrf  = max( n/2_ilp, 1_ilp )
                         ! .. minimal workspace length for n/2 x n/2 stdlib_sgesvd
                         lwsvd2 = max( 5_ilp * (n/2_ilp), 1_ilp )
                         lworq2 = max( n, 1_ilp )
                         minwrk2 = max( lwqp3, n/2_ilp+lwqrf, n/2_ilp+lwsvd2,n/2_ilp+lworq2, lworq )
                         if ( conda ) minwrk2 = max( minwrk2, lwcon )
                         minwrk2 = n + minwrk2
                         minwrk = max( minwrk, minwrk2 )
                      end if
                  else
                      minwrk = max( lwqp3, lwsvd, lworq )
                      if ( conda ) minwrk = max( minwrk, lwcon )
                      minwrk = minwrk + n
                      if ( wntva ) then
                         ! .. minimal workspace length for n/2 x n stdlib_sgelqf
                         lwlqf  = max( n/2_ilp, 1_ilp )
                         lwsvd2 = max( 5_ilp * (n/2_ilp), 1_ilp )
                         lwunlq = max( n , 1_ilp )
                         minwrk2 = max( lwqp3, n/2_ilp+lwlqf, n/2_ilp+lwsvd2,n/2_ilp+lwunlq, lworq )
                         if ( conda ) minwrk2 = max( minwrk2, lwcon )
                         minwrk2 = n + minwrk2
                         minwrk = max( minwrk, minwrk2 )
                      end if
                  end if
                  if ( lquery ) then
                     if ( rtrans ) then
                        call stdlib_sgesvd( 'O', 'A', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -1_ilp, &
                                  ierr )
                        lwrk_sgesvd = int( rdummy(1_ilp),KIND=ilp)
                        optwrk = max(lwrk_sgeqp3,lwrk_sgesvd,lwrk_sormqr)
                        if ( conda ) optwrk = max( optwrk, lwcon )
                        optwrk = n + optwrk
                        if ( wntva ) then
                            call stdlib_sgeqrf(n,n/2_ilp,u,ldu,rdummy,rdummy,-1_ilp,ierr)
                            lwrk_sgeqrf = int( rdummy(1_ilp),KIND=ilp)
                            call stdlib_sgesvd( 'S', 'O', n/2_ilp,n/2_ilp, v,ldv, s, u,ldu,v, ldv, rdummy,&
                                       -1_ilp, ierr )
                            lwrk_sgesvd2 = int( rdummy(1_ilp),KIND=ilp)
                            call stdlib_sormqr( 'R', 'C', n, n, n/2_ilp, u, ldu, rdummy,v, ldv, &
                                      rdummy, -1_ilp, ierr )
                            lwrk_sormqr2 = int( rdummy(1_ilp),KIND=ilp)
                            optwrk2 = max( lwrk_sgeqp3, n/2_ilp+lwrk_sgeqrf,n/2_ilp+lwrk_sgesvd2, n/2_ilp+&
                                      lwrk_sormqr2 )
                            if ( conda ) optwrk2 = max( optwrk2, lwcon )
                            optwrk2 = n + optwrk2
                            optwrk = max( optwrk, optwrk2 )
                        end if
                     else
                        call stdlib_sgesvd( 'S', 'O', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -1_ilp, &
                                  ierr )
                        lwrk_sgesvd = int( rdummy(1_ilp),KIND=ilp)
                        optwrk = max(lwrk_sgeqp3,lwrk_sgesvd,lwrk_sormqr)
                        if ( conda ) optwrk = max( optwrk, lwcon )
                        optwrk = n + optwrk
                        if ( wntva ) then
                           call stdlib_sgelqf(n/2_ilp,n,u,ldu,rdummy,rdummy,-1_ilp,ierr)
                           lwrk_sgelqf = int( rdummy(1_ilp),KIND=ilp)
                           call stdlib_sgesvd( 'S','O', n/2_ilp,n/2_ilp, v, ldv, s, u, ldu,v, ldv, rdummy,&
                                      -1_ilp, ierr )
                           lwrk_sgesvd2 = int( rdummy(1_ilp),KIND=ilp)
                           call stdlib_sormlq( 'R', 'N', n, n, n/2_ilp, u, ldu, rdummy,v, ldv, rdummy,&
                                     -1_ilp,ierr )
                           lwrk_sormlq = int( rdummy(1_ilp),KIND=ilp)
                           optwrk2 = max( lwrk_sgeqp3, n/2_ilp+lwrk_sgelqf,n/2_ilp+lwrk_sgesvd2, n/2_ilp+&
                                     lwrk_sormlq )
                            if ( conda ) optwrk2 = max( optwrk2, lwcon )
                            optwrk2 = n + optwrk2
                            optwrk = max( optwrk, optwrk2 )
                        end if
                     end if
                  end if
              end if
              minwrk = max( 2_ilp, minwrk )
              optwrk = max( 2_ilp, optwrk )
              if ( lwork < minwrk .and. (.not.lquery) ) info = -19_ilp
           end if
           if (info == 0_ilp .and. lrwork < rminwrk .and. .not. lquery) then
              info = -21_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGESVDQ', -info )
              return
           else if ( lquery ) then
           ! return optimal workspace
               iwork(1_ilp) = iminwrk
               work(1_ilp) = optwrk
               work(2_ilp) = minwrk
               rwork(1_ilp) = rminwrk
               return
           end if
           ! quick return if the matrix is void.
           if( ( m==0_ilp ) .or. ( n==0_ilp ) ) then
           ! All Output Is Void
              return
           end if
           big = stdlib_slamch('O')
           ascaled = .false.
           iwoff = 1_ilp
           if ( rowprm ) then
                 iwoff = m
                 ! Reordering The Rows In Decreasing Sequence In The
                 ! ell-infinity norm - this enhances numerical robustness in
                 ! the case of differently scaled rows.
                 do p = 1, m
                     ! rwork(p) = abs( a(p,stdlib_icamax(n,a(p,1),lda)) )
                     ! [[stdlib_slange will return nan if an entry of the p-th row is nan]]
                     rwork(p) = stdlib_slange( 'M', 1_ilp, n, a(p,1_ilp), lda, rdummy )
                     ! .. check for nan's and inf's
                     if ( ( rwork(p) /= rwork(p) ) .or.( (rwork(p)*zero) /= zero ) ) then
                         info = -8_ilp
                         call stdlib_xerbla( 'SGESVDQ', -info )
                         return
                     end if
                 end do
                 do p = 1, m - 1
                 q = stdlib_isamax( m-p+1, rwork(p), 1_ilp ) + p - 1_ilp
                 iwork(n+p) = q
                 if ( p /= q ) then
                    rtmp     = rwork(p)
                    rwork(p) = rwork(q)
                    rwork(q) = rtmp
                 end if
                 end do
                 if ( rwork(1_ilp) == zero ) then
                    ! quick return: a is the m x n zero matrix.
                    numrank = 0_ilp
                    call stdlib_slaset( 'G', n, 1_ilp, zero, zero, s, n )
                    if ( wntus ) call stdlib_slaset('G', m, n, zero, one, u, ldu)
                    if ( wntua ) call stdlib_slaset('G', m, m, zero, one, u, ldu)
                    if ( wntva ) call stdlib_slaset('G', n, n, zero, one, v, ldv)
                    if ( wntuf ) then
                        call stdlib_slaset( 'G', n, 1_ilp, zero, zero, work, n )
                        call stdlib_slaset( 'G', m, n, zero,  one, u, ldu )
                    end if
                    do p = 1, n
                        iwork(p) = p
                    end do
                    if ( rowprm ) then
                        do p = n + 1, n + m - 1
                            iwork(p) = p - n
                        end do
                    end if
                    if ( conda ) rwork(1_ilp) = -1_ilp
                    rwork(2_ilp) = -1_ilp
                    return
                 end if
                 if ( rwork(1_ilp) > big / sqrt(real(m,KIND=sp)) ) then
                     ! .. to prevent overflow in the qr factorization, scale the
                     ! matrix by 1/sqrt(m) if too large entry detected
                     call stdlib_slascl('G',0_ilp,0_ilp,sqrt(real(m,KIND=sp)),one, m,n, a,lda, ierr)
                               
                     ascaled = .true.
                 end if
                 call stdlib_slaswp( n, a, lda, 1_ilp, m-1, iwork(n+1), 1_ilp )
           end if
          ! .. at this stage, preemptive scaling is done only to avoid column
          ! norms overflows during the qr factorization. the svd procedure should
          ! have its own scaling to save the singular values from overflows and
          ! underflows. that depends on the svd procedure.
           if ( .not.rowprm ) then
               rtmp = stdlib_slange( 'M', m, n, a, lda, rdummy )
               if ( ( rtmp /= rtmp ) .or.( (rtmp*zero) /= zero ) ) then
                    info = -8_ilp
                    call stdlib_xerbla( 'SGESVDQ', -info )
                    return
               end if
               if ( rtmp > big / sqrt(real(m,KIND=sp)) ) then
                   ! .. to prevent overflow in the qr factorization, scale the
                   ! matrix by 1/sqrt(m) if too large entry detected
                   call stdlib_slascl('G',0_ilp,0_ilp, sqrt(real(m,KIND=sp)),one, m,n, a,lda, ierr)
                             
                   ascaled = .true.
               end if
           end if
           ! Qr Factorization With Column Pivoting
           ! a * p = q * [ r ]
                       ! [ 0 ]
           do p = 1, n
              ! All Columns Are Free Columns
              iwork(p) = 0_ilp
           end do
           call stdlib_sgeqp3( m, n, a, lda, iwork, work, work(n+1), lwork-n,ierr )
          ! if the user requested accuracy level allows truncation in the
          ! computed upper triangular factor, the matrix r is examined and,
          ! if possible, replaced with its leading upper trapezoidal part.
           epsln = stdlib_slamch('E')
           sfmin = stdlib_slamch('S')
           ! small = sfmin / epsln
           nr = n
           if ( accla ) then
              ! standard absolute error bound suffices. all sigma_i with
              ! sigma_i < n*eps*||a||_f are flushed to zero. this is an
              ! aggressive enforcement of lower numerical rank by introducing a
              ! backward error of the order of n*eps*||a||_f.
              nr = 1_ilp
              rtmp = sqrt(real(n,KIND=sp))*epsln
              do p = 2, n
                 if ( abs(a(p,p)) < (rtmp*abs(a(1,1))) ) go to 3002
                    nr = nr + 1_ilp
              end do
              3002 continue
           elseif ( acclm ) then
              ! .. similarly as above, only slightly more gentle (less aggressive).
              ! sudden drop on the diagonal of r is used as the criterion for being
              ! close-to-rank-deficient. the threshold is set to epsln=stdlib_slamch('e').
              ! [[this can be made more flexible by replacing this hard-coded value
              ! with a user specified threshold.]] also, the values that underflow
              ! will be truncated.
              nr = 1_ilp
              do p = 2, n
                 if ( ( abs(a(p,p)) < (epsln*abs(a(p-1,p-1))) ) .or.( abs(a(p,p)) < sfmin ) ) go to 3402
                 nr = nr + 1_ilp
              end do
              3402 continue
           else
              ! Rrqr Not Authorized To Determine Numerical Rank Except In The
              ! obvious case of zero pivots.
              ! .. inspect r for exact zeros on the diagonal;
              ! r(i,i)=0 => r(i:n,i:n)=0.
              nr = 1_ilp
              do p = 2, n
                 if ( abs(a(p,p)) == zero ) go to 3502
                 nr = nr + 1_ilp
              end do
              3502 continue
              if ( conda ) then
                 ! estimate the scaled condition number of a. use the fact that it is
                 ! the same as the scaled condition number of r.
                    ! V Is Used As Workspace
                    call stdlib_slacpy( 'U', n, n, a, lda, v, ldv )
                    ! only the leading nr x nr submatrix of the triangular factor
                    ! is considered. only if nr=n will this give a reliable error
                    ! bound. however, even for nr < n, this can be used on an
                    ! expert level and obtain useful information in the sense of
                    ! perturbation theory.
                    do p = 1, nr
                       rtmp = stdlib_snrm2( p, v(1_ilp,p), 1_ilp )
                       call stdlib_sscal( p, one/rtmp, v(1_ilp,p), 1_ilp )
                    end do
                    if ( .not. ( lsvec .or. rsvec ) ) then
                        call stdlib_spocon( 'U', nr, v, ldv, one, rtmp,work, iwork(n+iwoff), ierr &
                                  )
                    else
                        call stdlib_spocon( 'U', nr, v, ldv, one, rtmp,work(n+1), iwork(n+iwoff), &
                                  ierr )
                    end if
                    sconda = one / sqrt(rtmp)
                 ! for nr=n, sconda is an estimate of sqrt(||(r^* * r)^(-1)||_1),
                 ! n^(-1/4) * sconda <= ||r^(-1)||_2 <= n^(1/4) * sconda
                 ! see the reference [1] for more details.
              end if
           endif
           if ( wntur ) then
               n1 = nr
           else if ( wntus .or. wntuf) then
               n1 = n
           else if ( wntua ) then
               n1 = m
           end if
           if ( .not. ( rsvec .or. lsvec ) ) then
      ! .......................................................................
              ! Only The Singular Values Are Requested
      ! .......................................................................
              if ( rtrans ) then
               ! .. compute the singular values of r**t = [a](1:nr,1:n)**t
                 ! .. set the lower triangle of [a] to [a](1:nr,1:n)**t and
                 ! the upper triangle of [a] to zero.
                 do p = 1, min( n, nr )
                    do q = p + 1, n
                       a(q,p) = a(p,q)
                       if ( q <= nr ) a(p,q) = zero
                    end do
                 end do
                 call stdlib_sgesvd( 'N', 'N', n, nr, a, lda, s, u, ldu,v, ldv, work, lwork, info &
                           )
              else
                 ! .. compute the singular values of r = [a](1:nr,1:n)
                 if ( nr > 1_ilp )call stdlib_slaset( 'L', nr-1,nr-1, zero,zero, a(2_ilp,1_ilp), lda )
                 call stdlib_sgesvd( 'N', 'N', nr, n, a, lda, s, u, ldu,v, ldv, work, lwork, info &
                           )
              end if
           else if ( lsvec .and. ( .not. rsvec) ) then
      ! .......................................................................
             ! The Singular Values And The Left Singular Vectors Requested
      ! .......................................................................""""""""
              if ( rtrans ) then
                  ! .. apply stdlib_sgesvd to r**t
                  ! .. copy r**t into [u] and overwrite [u] with the right singular
                  ! vectors of r
                 do p = 1, nr
                    do q = p, n
                       u(q,p) = a(p,q)
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_slaset( 'U', nr-1,nr-1, zero,zero, u(1_ilp,2_ilp), ldu )
                 ! .. the left singular vectors not computed, the nr right singular
                 ! vectors overwrite [u](1:nr,1:nr) as transposed. these
                 ! will be pre-multiplied by q to build the left singular vectors of a.
                    call stdlib_sgesvd( 'N', 'O', n, nr, u, ldu, s, u, ldu,u, ldu, work(n+1), &
                              lwork-n, info )
                    do p = 1, nr
                        do q = p + 1, nr
                           rtmp   = u(q,p)
                           u(q,p) = u(p,q)
                           u(p,q) = rtmp
                        end do
                    end do
              else
                  ! Apply Stdlib_Sgesvd To R
                  ! .. copy r into [u] and overwrite [u] with the left singular vectors
                  call stdlib_slacpy( 'U', nr, n, a, lda, u, ldu )
                  if ( nr > 1_ilp )call stdlib_slaset( 'L', nr-1, nr-1, zero, zero, u(2_ilp,1_ilp), ldu )
                            
                  ! .. the right singular vectors not computed, the nr left singular
                  ! vectors overwrite [u](1:nr,1:nr)
                     call stdlib_sgesvd( 'O', 'N', nr, n, u, ldu, s, u, ldu,v, ldv, work(n+1), &
                               lwork-n, info )
                     ! .. now [u](1:nr,1:nr) contains the nr left singular vectors of
                     ! r. these will be pre-multiplied by q to build the left singular
                     ! vectors of a.
              end if
                 ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
              if ( ( nr < m ) .and. ( .not.wntuf ) ) then
                  call stdlib_slaset('A', m-nr, nr, zero, zero, u(nr+1,1_ilp), ldu)
                  if ( nr < n1 ) then
                     call stdlib_slaset( 'A',nr,n1-nr,zero,zero,u(1_ilp,nr+1), ldu )
                     call stdlib_slaset( 'A',m-nr,n1-nr,zero,one,u(nr+1,nr+1), ldu )
                  end if
              end if
                 ! the q matrix from the first qrf is built into the left singular
                 ! vectors matrix u.
              if ( .not.wntuf )call stdlib_sormqr( 'L', 'N', m, n1, n, a, lda, work, u,ldu, work(&
                        n+1), lwork-n, ierr )
              if ( rowprm .and. .not.wntuf )call stdlib_slaswp( n1, u, ldu, 1_ilp, m-1, iwork(n+1), -&
                        1_ilp )
           else if ( rsvec .and. ( .not. lsvec ) ) then
      ! .......................................................................
             ! The Singular Values And The Right Singular Vectors Requested
      ! .......................................................................
               if ( rtrans ) then
                  ! .. apply stdlib_sgesvd to r**t
                  ! .. copy r**t into v and overwrite v with the left singular vectors
                 do p = 1, nr
                    do q = p, n
                       v(q,p) = (a(p,q))
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_slaset( 'U', nr-1,nr-1, zero,zero, v(1_ilp,2_ilp), ldv )
                 ! .. the left singular vectors of r**t overwrite v, the right singular
                 ! vectors not computed
                 if ( wntvr .or. ( nr == n ) ) then
                    call stdlib_sgesvd( 'O', 'N', n, nr, v, ldv, s, u, ldu,u, ldu, work(n+1), &
                              lwork-n, info )
                    do p = 1, nr
                        do q = p + 1, nr
                           rtmp   = v(q,p)
                           v(q,p) = v(p,q)
                           v(p,q) = rtmp
                        end do
                    end do
                    if ( nr < n ) then
                        do p = 1, nr
                           do q = nr + 1, n
                               v(p,q) = v(q,p)
                           end do
                        end do
                    end if
                    call stdlib_slapmt( .false., nr, n, v, ldv, iwork )
                 else
                     ! .. need all n right singular vectors and nr < n
                     ! [!] this is simple implementation that augments [v](1:n,1:nr)
                     ! by padding a zero block. in the case nr << n, a more efficient
                     ! way is to first use the qr factorization. for more details
                     ! how to implement this, see the " full svd " branch.
                     call stdlib_slaset('G', n, n-nr, zero, zero, v(1_ilp,nr+1), ldv)
                     call stdlib_sgesvd( 'O', 'N', n, n, v, ldv, s, u, ldu,u, ldu, work(n+1), &
                               lwork-n, info )
                     do p = 1, n
                        do q = p + 1, n
                           rtmp   = v(q,p)
                           v(q,p) = v(p,q)
                           v(p,q) = rtmp
                        end do
                     end do
                     call stdlib_slapmt( .false., n, n, v, ldv, iwork )
                 end if
               else
                  ! Aply Stdlib_Sgesvd To R
                  ! Copy R Into V And Overwrite V With The Right Singular Vectors
                  call stdlib_slacpy( 'U', nr, n, a, lda, v, ldv )
                  if ( nr > 1_ilp )call stdlib_slaset( 'L', nr-1, nr-1, zero, zero, v(2_ilp,1_ilp), ldv )
                            
                  ! .. the right singular vectors overwrite v, the nr left singular
                  ! vectors stored in u(1:nr,1:nr)
                  if ( wntvr .or. ( nr == n ) ) then
                     call stdlib_sgesvd( 'N', 'O', nr, n, v, ldv, s, u, ldu,v, ldv, work(n+1), &
                               lwork-n, info )
                     call stdlib_slapmt( .false., nr, n, v, ldv, iwork )
                     ! .. now [v](1:nr,1:n) contains v(1:n,1:nr)**t
                  else
                     ! .. need all n right singular vectors and nr < n
                     ! [!] this is simple implementation that augments [v](1:nr,1:n)
                     ! by padding a zero block. in the case nr << n, a more efficient
                     ! way is to first use the lq factorization. for more details
                     ! how to implement this, see the " full svd " branch.
                      call stdlib_slaset('G', n-nr, n, zero,zero, v(nr+1,1_ilp), ldv)
                      call stdlib_sgesvd( 'N', 'O', n, n, v, ldv, s, u, ldu,v, ldv, work(n+1), &
                                lwork-n, info )
                      call stdlib_slapmt( .false., n, n, v, ldv, iwork )
                  end if
                  ! .. now [v] contains the transposed matrix of the right singular
                  ! vectors of a.
               end if
           else
      ! .......................................................................
             ! Full Svd Requested
      ! .......................................................................
              if ( rtrans ) then
                  ! .. apply stdlib_sgesvd to r**t [[this option is left for r
                 if ( wntvr .or. ( nr == n ) ) then
                  ! .. copy r**t into [v] and overwrite [v] with the left singular
                  ! vectors of r**t
                 do p = 1, nr
                    do q = p, n
                       v(q,p) = a(p,q)
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_slaset( 'U', nr-1,nr-1, zero,zero, v(1_ilp,2_ilp), ldv )
                 ! .. the left singular vectors of r**t overwrite [v], the nr right
                 ! singular vectors of r**t stored in [u](1:nr,1:nr) as transposed
                    call stdlib_sgesvd( 'O', 'A', n, nr, v, ldv, s, v, ldv,u, ldu, work(n+1), &
                              lwork-n, info )
                    ! Assemble V
                    do p = 1, nr
                       do q = p + 1, nr
                          rtmp   = v(q,p)
                          v(q,p) = v(p,q)
                          v(p,q) = rtmp
                       end do
                    end do
                    if ( nr < n ) then
                        do p = 1, nr
                           do q = nr+1, n
                              v(p,q) = v(q,p)
                           end do
                        end do
                    end if
                    call stdlib_slapmt( .false., nr, n, v, ldv, iwork )
                     do p = 1, nr
                        do q = p + 1, nr
                           rtmp   = u(q,p)
                           u(q,p) = u(p,q)
                           u(p,q) = rtmp
                        end do
                     end do
                     if ( ( nr < m ) .and. .not.(wntuf)) then
                       call stdlib_slaset('A', m-nr,nr, zero,zero, u(nr+1,1_ilp), ldu)
                       if ( nr < n1 ) then
                          call stdlib_slaset('A',nr,n1-nr,zero,zero,u(1_ilp,nr+1),ldu)
                          call stdlib_slaset( 'A',m-nr,n1-nr,zero,one,u(nr+1,nr+1), ldu )
                       end if
                    end if
                 else
                     ! .. need all n right singular vectors and nr < n
                  ! .. copy r**t into [v] and overwrite [v] with the left singular
                  ! vectors of r**t
                     ! [[the optimal ratio n/nr for using qrf instead of padding
                       ! with zeros. here hard coded to 2; it must be at least
                       ! two due to work space constraints.]]
                     ! optratio = stdlib_ilaenv(6, 'sgesvd', 's' // 'o', nr,n,0,0)
                     ! optratio = max( optratio, 2 )
                     optratio = 2_ilp
                     if ( optratio*nr > n ) then
                        do p = 1, nr
                           do q = p, n
                              v(q,p) = a(p,q)
                           end do
                        end do
                        if ( nr > 1_ilp )call stdlib_slaset('U',nr-1,nr-1, zero,zero, v(1_ilp,2_ilp),ldv)
                                  
                        call stdlib_slaset('A',n,n-nr,zero,zero,v(1_ilp,nr+1),ldv)
                        call stdlib_sgesvd( 'O', 'A', n, n, v, ldv, s, v, ldv,u, ldu, work(n+1), &
                                  lwork-n, info )
                        do p = 1, n
                           do q = p + 1, n
                              rtmp   = v(q,p)
                              v(q,p) = v(p,q)
                              v(p,q) = rtmp
                           end do
                        end do
                        call stdlib_slapmt( .false., n, n, v, ldv, iwork )
                    ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x n1), i.e. (m x n) or (m x m).
                        do p = 1, n
                           do q = p + 1, n
                              rtmp   = u(q,p)
                              u(q,p) = u(p,q)
                              u(p,q) = rtmp
                           end do
                        end do
                        if ( ( n < m ) .and. .not.(wntuf)) then
                           call stdlib_slaset('A',m-n,n,zero,zero,u(n+1,1_ilp),ldu)
                           if ( n < n1 ) then
                             call stdlib_slaset('A',n,n1-n,zero,zero,u(1_ilp,n+1),ldu)
                             call stdlib_slaset('A',m-n,n1-n,zero,one,u(n+1,n+1), ldu )
                           end if
                        end if
                     else
                        ! .. copy r**t into [u] and overwrite [u] with the right
                        ! singular vectors of r
                        do p = 1, nr
                           do q = p, n
                              u(q,nr+p) = a(p,q)
                           end do
                        end do
                        if ( nr > 1_ilp )call stdlib_slaset('U',nr-1,nr-1,zero,zero,u(1_ilp,nr+2),ldu)
                                  
                        call stdlib_sgeqrf( n, nr, u(1_ilp,nr+1), ldu, work(n+1),work(n+nr+1), lwork-&
                                  n-nr, ierr )
                        do p = 1, nr
                            do q = 1, n
                                v(q,p) = u(p,nr+q)
                            end do
                        end do
                       call stdlib_slaset('U',nr-1,nr-1,zero,zero,v(1_ilp,2_ilp),ldv)
                       call stdlib_sgesvd( 'S', 'O', nr, nr, v, ldv, s, u, ldu,v,ldv, work(n+nr+1)&
                                 ,lwork-n-nr, info )
                       call stdlib_slaset('A',n-nr,nr,zero,zero,v(nr+1,1_ilp),ldv)
                       call stdlib_slaset('A',nr,n-nr,zero,zero,v(1_ilp,nr+1),ldv)
                       call stdlib_slaset('A',n-nr,n-nr,zero,one,v(nr+1,nr+1),ldv)
                       call stdlib_sormqr('R','C', n, n, nr, u(1_ilp,nr+1), ldu,work(n+1),v,ldv,work(&
                                 n+nr+1),lwork-n-nr,ierr)
                       call stdlib_slapmt( .false., n, n, v, ldv, iwork )
                       ! Assemble The Left Singular Vector Matrix U Of Dimensions
                       ! (m x nr) or (m x n) or (m x m).
                       if ( ( nr < m ) .and. .not.(wntuf)) then
                          call stdlib_slaset('A',m-nr,nr,zero,zero,u(nr+1,1_ilp),ldu)
                          if ( nr < n1 ) then
                          call stdlib_slaset('A',nr,n1-nr,zero,zero,u(1_ilp,nr+1),ldu)
                          call stdlib_slaset( 'A',m-nr,n1-nr,zero,one,u(nr+1,nr+1),ldu)
                          end if
                       end if
                     end if
                 end if
              else
                  ! .. apply stdlib_sgesvd to r [[this is the recommended option]]
                  if ( wntvr .or. ( nr == n ) ) then
                      ! .. copy r into [v] and overwrite v with the right singular vectors
                      call stdlib_slacpy( 'U', nr, n, a, lda, v, ldv )
                     if ( nr > 1_ilp )call stdlib_slaset( 'L', nr-1,nr-1, zero,zero, v(2_ilp,1_ilp), ldv )
                               
                     ! .. the right singular vectors of r overwrite [v], the nr left
                     ! singular vectors of r stored in [u](1:nr,1:nr)
                     call stdlib_sgesvd( 'S', 'O', nr, n, v, ldv, s, u, ldu,v, ldv, work(n+1), &
                               lwork-n, info )
                     call stdlib_slapmt( .false., nr, n, v, ldv, iwork )
                     ! .. now [v](1:nr,1:n) contains v(1:n,1:nr)**t
                     ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
                    if ( ( nr < m ) .and. .not.(wntuf)) then
                       call stdlib_slaset('A', m-nr,nr, zero,zero, u(nr+1,1_ilp), ldu)
                       if ( nr < n1 ) then
                          call stdlib_slaset('A',nr,n1-nr,zero,zero,u(1_ilp,nr+1),ldu)
                          call stdlib_slaset( 'A',m-nr,n1-nr,zero,one,u(nr+1,nr+1), ldu )
                       end if
                    end if
                  else
                    ! .. need all n right singular vectors and nr < n
                    ! The Requested Number Of The Left Singular Vectors
                     ! is then n1 (n or m)
                     ! [[the optimal ratio n/nr for using lq instead of padding
                       ! with zeros. here hard coded to 2; it must be at least
                       ! two due to work space constraints.]]
                     ! optratio = stdlib_ilaenv(6, 'sgesvd', 's' // 'o', nr,n,0,0)
                     ! optratio = max( optratio, 2 )
                    optratio = 2_ilp
                    if ( optratio * nr > n ) then
                       call stdlib_slacpy( 'U', nr, n, a, lda, v, ldv )
                       if ( nr > 1_ilp )call stdlib_slaset('L', nr-1,nr-1, zero,zero, v(2_ilp,1_ilp),ldv)
                                 
                    ! .. the right singular vectors of r overwrite [v], the nr left
                       ! singular vectors of r stored in [u](1:nr,1:nr)
                       call stdlib_slaset('A', n-nr,n, zero,zero, v(nr+1,1_ilp),ldv)
                       call stdlib_sgesvd( 'S', 'O', n, n, v, ldv, s, u, ldu,v, ldv, work(n+1), &
                                 lwork-n, info )
                       call stdlib_slapmt( .false., n, n, v, ldv, iwork )
                       ! .. now [v] contains the transposed matrix of the right
                       ! singular vectors of a. the leading n left singular vectors
                       ! are in [u](1:n,1:n)
                       ! Assemble The Left Singular Vector Matrix U Of Dimensions
                       ! (m x n1), i.e. (m x n) or (m x m).
                       if ( ( n < m ) .and. .not.(wntuf)) then
                           call stdlib_slaset('A',m-n,n,zero,zero,u(n+1,1_ilp),ldu)
                           if ( n < n1 ) then
                             call stdlib_slaset('A',n,n1-n,zero,zero,u(1_ilp,n+1),ldu)
                             call stdlib_slaset( 'A',m-n,n1-n,zero,one,u(n+1,n+1), ldu )
                           end if
                       end if
                    else
                       call stdlib_slacpy( 'U', nr, n, a, lda, u(nr+1,1_ilp), ldu )
                       if ( nr > 1_ilp )call stdlib_slaset('L',nr-1,nr-1,zero,zero,u(nr+2,1_ilp),ldu)
                                 
                       call stdlib_sgelqf( nr, n, u(nr+1,1_ilp), ldu, work(n+1),work(n+nr+1), lwork-n-&
                                 nr, ierr )
                       call stdlib_slacpy('L',nr,nr,u(nr+1,1_ilp),ldu,v,ldv)
                       if ( nr > 1_ilp )call stdlib_slaset('U',nr-1,nr-1,zero,zero,v(1_ilp,2_ilp),ldv)
                       call stdlib_sgesvd( 'S', 'O', nr, nr, v, ldv, s, u, ldu,v, ldv, work(n+nr+&
                                 1_ilp), lwork-n-nr, info )
                       call stdlib_slaset('A',n-nr,nr,zero,zero,v(nr+1,1_ilp),ldv)
                       call stdlib_slaset('A',nr,n-nr,zero,zero,v(1_ilp,nr+1),ldv)
                       call stdlib_slaset('A',n-nr,n-nr,zero,one,v(nr+1,nr+1),ldv)
                       call stdlib_sormlq('R','N',n,n,nr,u(nr+1,1_ilp),ldu,work(n+1),v, ldv, work(n+&
                                 nr+1),lwork-n-nr,ierr)
                       call stdlib_slapmt( .false., n, n, v, ldv, iwork )
                     ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
                       if ( ( nr < m ) .and. .not.(wntuf)) then
                          call stdlib_slaset('A',m-nr,nr,zero,zero,u(nr+1,1_ilp),ldu)
                          if ( nr < n1 ) then
                          call stdlib_slaset('A',nr,n1-nr,zero,zero,u(1_ilp,nr+1),ldu)
                          call stdlib_slaset( 'A',m-nr,n1-nr,zero,one,u(nr+1,nr+1), ldu )
                          end if
                       end if
                    end if
                  end if
              ! .. end of the "r**t or r" branch
              end if
                 ! the q matrix from the first qrf is built into the left singular
                 ! vectors matrix u.
              if ( .not. wntuf )call stdlib_sormqr( 'L', 'N', m, n1, n, a, lda, work, u,ldu, work(&
                        n+1), lwork-n, ierr )
              if ( rowprm .and. .not.wntuf )call stdlib_slaswp( n1, u, ldu, 1_ilp, m-1, iwork(n+1), -&
                        1_ilp )
           ! ... end of the "full svd" branch
           end if
           ! check whether some singular values are returned as zeros, e.g.
           ! due to underflow, and update the numerical rank.
           p = nr
           do q = p, 1, -1
               if ( s(q) > zero ) go to 4002
               nr = nr - 1_ilp
           end do
           4002 continue
           ! .. if numerical rank deficiency is detected, the truncated
           ! singular values are set to zero.
           if ( nr < n ) call stdlib_slaset( 'G', n-nr,1_ilp, zero,zero, s(nr+1), n )
           ! .. undo scaling; this may cause overflow in the largest singular
           ! values.
           if ( ascaled )call stdlib_slascl( 'G',0_ilp,0_ilp, one,sqrt(real(m,KIND=sp)), nr,1_ilp, s, n, ierr &
                     )
           if ( conda ) rwork(1_ilp) = sconda
           rwork(2_ilp) = p - nr
           ! .. p-nr is the number of singular values that are computed as
           ! exact zeros in stdlib_sgesvd() applied to the (possibly truncated)
           ! full row rank triangular (trapezoidal) factor of a.
           numrank = nr
           return
     end subroutine stdlib_sgesvdq

     module subroutine stdlib_dgesvdq( joba, jobp, jobr, jobu, jobv, m, n, a, lda,s, u, ldu, v, ldv, &
     !! DGESVDQ computes the singular value decomposition (SVD) of a real
     !! M-by-N matrix A, where M >= N. The SVD of A is written as
     !! [++]   [xx]   [x0]   [xx]
     !! A = U * SIGMA * V^*,  [++] = [xx] * [ox] * [xx]
     !! [++]   [xx]
     !! where SIGMA is an N-by-N diagonal matrix, U is an M-by-N orthonormal
     !! matrix, and V is an N-by-N orthogonal matrix. The diagonal elements
     !! of SIGMA are the singular values of A. The columns of U and V are the
     !! left and the right singular vectors of A, respectively.
               numrank, iwork, liwork,work, lwork, rwork, lrwork, info )
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: joba, jobp, jobr, jobu, jobv
           integer(ilp), intent(in) :: m, n, lda, ldu, ldv, liwork, lrwork
           integer(ilp), intent(out) :: numrank, info
           integer(ilp), intent(inout) :: lwork
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: u(ldu,*), v(ldv,*), work(*)
           real(dp), intent(out) :: s(*), rwork(*)
           integer(ilp), intent(out) :: iwork(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: ierr, iwoff, nr, n1, optratio, p, q
           integer(ilp) :: lwcon, lwqp3, lwrk_dgelqf, lwrk_dgesvd, lwrk_dgesvd2, lwrk_dgeqp3, &
           lwrk_dgeqrf, lwrk_dormlq, lwrk_dormqr, lwrk_dormqr2, lwlqf, lwqrf, lwsvd, lwsvd2, &
                     lworq, lworq2, lworlq, minwrk, minwrk2, optwrk, optwrk2, iminwrk, rminwrk
           logical(lk) :: accla, acclm, acclh, ascaled, conda, dntwu, dntwv, lquery, lsvc0, lsvec,&
                      rowprm, rsvec, rtrans, wntua, wntuf, wntur, wntus, wntva, wntvr
           real(dp) :: big, epsln, rtmp, sconda, sfmin
           ! Local Arrays
           real(dp) :: rdummy(1_ilp)
           ! Intrinsic Functions 
           ! test the input arguments
           wntus  = stdlib_lsame( jobu, 'S' ) .or. stdlib_lsame( jobu, 'U' )
           wntur  = stdlib_lsame( jobu, 'R' )
           wntua  = stdlib_lsame( jobu, 'A' )
           wntuf  = stdlib_lsame( jobu, 'F' )
           lsvc0  = wntus .or. wntur .or. wntua
           lsvec  = lsvc0 .or. wntuf
           dntwu  = stdlib_lsame( jobu, 'N' )
           wntvr  = stdlib_lsame( jobv, 'R' )
           wntva  = stdlib_lsame( jobv, 'A' ) .or. stdlib_lsame( jobv, 'V' )
           rsvec  = wntvr .or. wntva
           dntwv  = stdlib_lsame( jobv, 'N' )
           accla  = stdlib_lsame( joba, 'A' )
           acclm  = stdlib_lsame( joba, 'M' )
           conda  = stdlib_lsame( joba, 'E' )
           acclh  = stdlib_lsame( joba, 'H' ) .or. conda
           rowprm = stdlib_lsame( jobp, 'P' )
           rtrans = stdlib_lsame( jobr, 'T' )
           if ( rowprm ) then
              if ( conda ) then
                 iminwrk = max( 1_ilp, n + m - 1_ilp + n )
              else
                 iminwrk = max( 1_ilp, n + m - 1_ilp )
              end if
              rminwrk = max( 2_ilp, m )
           else
              if ( conda ) then
                 iminwrk = max( 1_ilp, n + n )
              else
                 iminwrk = max( 1_ilp, n )
              end if
              rminwrk = 2_ilp
           end if
           lquery = (liwork == -1_ilp .or. lwork == -1_ilp .or. lrwork == -1_ilp)
           info  = 0_ilp
           if ( .not. ( accla .or. acclm .or. acclh ) ) then
              info = -1_ilp
           else if ( .not.( rowprm .or. stdlib_lsame( jobp, 'N' ) ) ) then
               info = -2_ilp
           else if ( .not.( rtrans .or. stdlib_lsame( jobr, 'N' ) ) ) then
               info = -3_ilp
           else if ( .not.( lsvec .or. dntwu ) ) then
              info = -4_ilp
           else if ( wntur .and. wntva ) then
              info = -5_ilp
           else if ( .not.( rsvec .or. dntwv )) then
              info = -5_ilp
           else if ( m<0_ilp ) then
              info = -6_ilp
           else if ( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -7_ilp
           else if ( lda<max( 1_ilp, m ) ) then
              info = -9_ilp
           else if ( ldu<1_ilp .or. ( lsvc0 .and. ldu<m ) .or.( wntuf .and. ldu<n ) ) then
              info = -12_ilp
           else if ( ldv<1_ilp .or. ( rsvec .and. ldv<n ) .or.( conda .and. ldv<n ) ) then
              info = -14_ilp
           else if ( liwork < iminwrk .and. .not. lquery ) then
              info = -17_ilp
           end if
           if ( info == 0_ilp ) then
              ! Compute The Minimal And The Optimal Workspace Lengths
              ! [[the expressions for computing the minimal and the optimal
              ! values of lwork are written with a lot of redundancy and
              ! can be simplified. however, this detailed form is easier for
              ! maintenance and modifications of the code.]]
              ! Minimal Workspace Length For Stdlib_Dgeqp3 Of An M X N Matrix
              lwqp3 = 3_ilp * n + 1_ilp
              ! Minimal Workspace Length For Stdlib_Dormqr To Build Left Singular Vectors
              if ( wntus .or. wntur ) then
                  lworq  = max( n  , 1_ilp )
              else if ( wntua ) then
                  lworq = max( m , 1_ilp )
              end if
              ! Minimal Workspace Length For Stdlib_Dpocon Of An N X N Matrix
              lwcon = 3_ilp * n
              ! Stdlib_Dgesvd Of An N X N Matrix
              lwsvd = max( 5_ilp * n, 1_ilp )
              if ( lquery ) then
                  call stdlib_dgeqp3( m, n, a, lda, iwork, rdummy, rdummy, -1_ilp,ierr )
                  lwrk_dgeqp3 = int( rdummy(1_ilp),KIND=ilp)
                  if ( wntus .or. wntur ) then
                      call stdlib_dormqr( 'L', 'N', m, n, n, a, lda, rdummy, u,ldu, rdummy, -1_ilp, &
                                ierr )
                      lwrk_dormqr = int( rdummy(1_ilp),KIND=ilp)
                  else if ( wntua ) then
                      call stdlib_dormqr( 'L', 'N', m, m, n, a, lda, rdummy, u,ldu, rdummy, -1_ilp, &
                                ierr )
                      lwrk_dormqr = int( rdummy(1_ilp),KIND=ilp)
                  else
                      lwrk_dormqr = 0_ilp
                  end if
              end if
              minwrk = 2_ilp
              optwrk = 2_ilp
              if ( .not. (lsvec .or. rsvec )) then
                  ! Minimal And Optimal Sizes Of The Workspace If
                  ! only the singular values are requested
                  if ( conda ) then
                     minwrk = max( n+lwqp3, lwcon, lwsvd )
                  else
                     minwrk = max( n+lwqp3, lwsvd )
                  end if
                  if ( lquery ) then
                      call stdlib_dgesvd( 'N', 'N', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -1_ilp, &
                                ierr )
                      lwrk_dgesvd = int( rdummy(1_ilp),KIND=ilp)
                      if ( conda ) then
                         optwrk = max( n+lwrk_dgeqp3, n+lwcon, lwrk_dgesvd )
                      else
                         optwrk = max( n+lwrk_dgeqp3, lwrk_dgesvd )
                      end if
                  end if
              else if ( lsvec .and. (.not.rsvec) ) then
                  ! Minimal And Optimal Sizes Of The Workspace If The
                  ! singular values and the left singular vectors are requested
                  if ( conda ) then
                      minwrk = n + max( lwqp3, lwcon, lwsvd, lworq )
                  else
                      minwrk = n + max( lwqp3, lwsvd, lworq )
                  end if
                  if ( lquery ) then
                     if ( rtrans ) then
                        call stdlib_dgesvd( 'N', 'O', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -1_ilp, &
                                  ierr )
                     else
                        call stdlib_dgesvd( 'O', 'N', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -1_ilp, &
                                  ierr )
                     end if
                     lwrk_dgesvd = int( rdummy(1_ilp),KIND=ilp)
                     if ( conda ) then
                         optwrk = n + max( lwrk_dgeqp3, lwcon, lwrk_dgesvd,lwrk_dormqr )
                     else
                         optwrk = n + max( lwrk_dgeqp3, lwrk_dgesvd,lwrk_dormqr )
                     end if
                  end if
              else if ( rsvec .and. (.not.lsvec) ) then
                  ! Minimal And Optimal Sizes Of The Workspace If The
                  ! singular values and the right singular vectors are requested
                  if ( conda ) then
                      minwrk = n + max( lwqp3, lwcon, lwsvd )
                  else
                      minwrk = n + max( lwqp3, lwsvd )
                  end if
                  if ( lquery ) then
                      if ( rtrans ) then
                          call stdlib_dgesvd( 'O', 'N', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -&
                                    1_ilp, ierr )
                      else
                          call stdlib_dgesvd( 'N', 'O', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -&
                                    1_ilp, ierr )
                      end if
                      lwrk_dgesvd = int( rdummy(1_ilp),KIND=ilp)
                      if ( conda ) then
                          optwrk = n + max( lwrk_dgeqp3, lwcon, lwrk_dgesvd )
                      else
                          optwrk = n + max( lwrk_dgeqp3, lwrk_dgesvd )
                      end if
                  end if
              else
                  ! Minimal And Optimal Sizes Of The Workspace If The
                  ! full svd is requested
                  if ( rtrans ) then
                      minwrk = max( lwqp3, lwsvd, lworq )
                      if ( conda ) minwrk = max( minwrk, lwcon )
                      minwrk = minwrk + n
                      if ( wntva ) then
                         ! .. minimal workspace length for n x n/2 stdlib_dgeqrf
                         lwqrf  = max( n/2_ilp, 1_ilp )
                         ! .. minimal workspace length for n/2 x n/2 stdlib_dgesvd
                         lwsvd2 = max( 5_ilp * (n/2_ilp), 1_ilp )
                         lworq2 = max( n, 1_ilp )
                         minwrk2 = max( lwqp3, n/2_ilp+lwqrf, n/2_ilp+lwsvd2,n/2_ilp+lworq2, lworq )
                         if ( conda ) minwrk2 = max( minwrk2, lwcon )
                         minwrk2 = n + minwrk2
                         minwrk = max( minwrk, minwrk2 )
                      end if
                  else
                      minwrk = max( lwqp3, lwsvd, lworq )
                      if ( conda ) minwrk = max( minwrk, lwcon )
                      minwrk = minwrk + n
                      if ( wntva ) then
                         ! .. minimal workspace length for n/2 x n stdlib_dgelqf
                         lwlqf  = max( n/2_ilp, 1_ilp )
                         lwsvd2 = max( 5_ilp * (n/2_ilp), 1_ilp )
                         lworlq = max( n , 1_ilp )
                         minwrk2 = max( lwqp3, n/2_ilp+lwlqf, n/2_ilp+lwsvd2,n/2_ilp+lworlq, lworq )
                         if ( conda ) minwrk2 = max( minwrk2, lwcon )
                         minwrk2 = n + minwrk2
                         minwrk = max( minwrk, minwrk2 )
                      end if
                  end if
                  if ( lquery ) then
                     if ( rtrans ) then
                        call stdlib_dgesvd( 'O', 'A', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -1_ilp, &
                                  ierr )
                        lwrk_dgesvd = int( rdummy(1_ilp),KIND=ilp)
                        optwrk = max(lwrk_dgeqp3,lwrk_dgesvd,lwrk_dormqr)
                        if ( conda ) optwrk = max( optwrk, lwcon )
                        optwrk = n + optwrk
                        if ( wntva ) then
                            call stdlib_dgeqrf(n,n/2_ilp,u,ldu,rdummy,rdummy,-1_ilp,ierr)
                            lwrk_dgeqrf = int( rdummy(1_ilp),KIND=ilp)
                            call stdlib_dgesvd( 'S', 'O', n/2_ilp,n/2_ilp, v,ldv, s, u,ldu,v, ldv, rdummy,&
                                       -1_ilp, ierr )
                            lwrk_dgesvd2 = int( rdummy(1_ilp),KIND=ilp)
                            call stdlib_dormqr( 'R', 'C', n, n, n/2_ilp, u, ldu, rdummy,v, ldv, &
                                      rdummy, -1_ilp, ierr )
                            lwrk_dormqr2 = int( rdummy(1_ilp),KIND=ilp)
                            optwrk2 = max( lwrk_dgeqp3, n/2_ilp+lwrk_dgeqrf,n/2_ilp+lwrk_dgesvd2, n/2_ilp+&
                                      lwrk_dormqr2 )
                            if ( conda ) optwrk2 = max( optwrk2, lwcon )
                            optwrk2 = n + optwrk2
                            optwrk = max( optwrk, optwrk2 )
                        end if
                     else
                        call stdlib_dgesvd( 'S', 'O', n, n, a, lda, s, u, ldu,v, ldv, rdummy, -1_ilp, &
                                  ierr )
                        lwrk_dgesvd = int( rdummy(1_ilp),KIND=ilp)
                        optwrk = max(lwrk_dgeqp3,lwrk_dgesvd,lwrk_dormqr)
                        if ( conda ) optwrk = max( optwrk, lwcon )
                        optwrk = n + optwrk
                        if ( wntva ) then
                           call stdlib_dgelqf(n/2_ilp,n,u,ldu,rdummy,rdummy,-1_ilp,ierr)
                           lwrk_dgelqf = int( rdummy(1_ilp),KIND=ilp)
                           call stdlib_dgesvd( 'S','O', n/2_ilp,n/2_ilp, v, ldv, s, u, ldu,v, ldv, rdummy,&
                                      -1_ilp, ierr )
                           lwrk_dgesvd2 = int( rdummy(1_ilp),KIND=ilp)
                           call stdlib_dormlq( 'R', 'N', n, n, n/2_ilp, u, ldu, rdummy,v, ldv, rdummy,&
                                     -1_ilp,ierr )
                           lwrk_dormlq = int( rdummy(1_ilp),KIND=ilp)
                           optwrk2 = max( lwrk_dgeqp3, n/2_ilp+lwrk_dgelqf,n/2_ilp+lwrk_dgesvd2, n/2_ilp+&
                                     lwrk_dormlq )
                            if ( conda ) optwrk2 = max( optwrk2, lwcon )
                            optwrk2 = n + optwrk2
                            optwrk = max( optwrk, optwrk2 )
                        end if
                     end if
                  end if
              end if
              minwrk = max( 2_ilp, minwrk )
              optwrk = max( 2_ilp, optwrk )
              if ( lwork < minwrk .and. (.not.lquery) ) info = -19_ilp
           end if
           if (info == 0_ilp .and. lrwork < rminwrk .and. .not. lquery) then
              info = -21_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGESVDQ', -info )
              return
           else if ( lquery ) then
           ! return optimal workspace
               iwork(1_ilp) = iminwrk
               work(1_ilp) = optwrk
               work(2_ilp) = minwrk
               rwork(1_ilp) = rminwrk
               return
           end if
           ! quick return if the matrix is void.
           if( ( m==0_ilp ) .or. ( n==0_ilp ) ) then
           ! All Output Is Void
              return
           end if
           big = stdlib_dlamch('O')
           ascaled = .false.
           iwoff = 1_ilp
           if ( rowprm ) then
                 iwoff = m
                 ! Reordering The Rows In Decreasing Sequence In The
                 ! ell-infinity norm - this enhances numerical robustness in
                 ! the case of differently scaled rows.
                 do p = 1, m
                     ! rwork(p) = abs( a(p,stdlib_icamax(n,a(p,1),lda)) )
                     ! [[stdlib_dlange will return nan if an entry of the p-th row is nan]]
                     rwork(p) = stdlib_dlange( 'M', 1_ilp, n, a(p,1_ilp), lda, rdummy )
                     ! .. check for nan's and inf's
                     if ( ( rwork(p) /= rwork(p) ) .or.( (rwork(p)*zero) /= zero ) ) then
                         info = -8_ilp
                         call stdlib_xerbla( 'DGESVDQ', -info )
                         return
                     end if
                 end do
                 do p = 1, m - 1
                 q = stdlib_idamax( m-p+1, rwork(p), 1_ilp ) + p - 1_ilp
                 iwork(n+p) = q
                 if ( p /= q ) then
                    rtmp     = rwork(p)
                    rwork(p) = rwork(q)
                    rwork(q) = rtmp
                 end if
                 end do
                 if ( rwork(1_ilp) == zero ) then
                    ! quick return: a is the m x n zero matrix.
                    numrank = 0_ilp
                    call stdlib_dlaset( 'G', n, 1_ilp, zero, zero, s, n )
                    if ( wntus ) call stdlib_dlaset('G', m, n, zero, one, u, ldu)
                    if ( wntua ) call stdlib_dlaset('G', m, m, zero, one, u, ldu)
                    if ( wntva ) call stdlib_dlaset('G', n, n, zero, one, v, ldv)
                    if ( wntuf ) then
                        call stdlib_dlaset( 'G', n, 1_ilp, zero, zero, work, n )
                        call stdlib_dlaset( 'G', m, n, zero,  one, u, ldu )
                    end if
                    do p = 1, n
                        iwork(p) = p
                    end do
                    if ( rowprm ) then
                        do p = n + 1, n + m - 1
                            iwork(p) = p - n
                        end do
                    end if
                    if ( conda ) rwork(1_ilp) = -1_ilp
                    rwork(2_ilp) = -1_ilp
                    return
                 end if
                 if ( rwork(1_ilp) > big / sqrt(real(m,KIND=dp)) ) then
                     ! .. to prevent overflow in the qr factorization, scale the
                     ! matrix by 1/sqrt(m) if too large entry detected
                     call stdlib_dlascl('G',0_ilp,0_ilp,sqrt(real(m,KIND=dp)),one, m,n, a,lda, ierr)
                               
                     ascaled = .true.
                 end if
                 call stdlib_dlaswp( n, a, lda, 1_ilp, m-1, iwork(n+1), 1_ilp )
           end if
          ! .. at this stage, preemptive scaling is done only to avoid column
          ! norms overflows during the qr factorization. the svd procedure should
          ! have its own scaling to save the singular values from overflows and
          ! underflows. that depends on the svd procedure.
           if ( .not.rowprm ) then
               rtmp = stdlib_dlange( 'M', m, n, a, lda, rdummy )
               if ( ( rtmp /= rtmp ) .or.( (rtmp*zero) /= zero ) ) then
                    info = -8_ilp
                    call stdlib_xerbla( 'DGESVDQ', -info )
                    return
               end if
               if ( rtmp > big / sqrt(real(m,KIND=dp)) ) then
                   ! .. to prevent overflow in the qr factorization, scale the
                   ! matrix by 1/sqrt(m) if too large entry detected
                   call stdlib_dlascl('G',0_ilp,0_ilp, sqrt(real(m,KIND=dp)),one, m,n, a,lda, ierr)
                             
                   ascaled = .true.
               end if
           end if
           ! Qr Factorization With Column Pivoting
           ! a * p = q * [ r ]
                       ! [ 0 ]
           do p = 1, n
              ! All Columns Are Free Columns
              iwork(p) = 0_ilp
           end do
           call stdlib_dgeqp3( m, n, a, lda, iwork, work, work(n+1), lwork-n,ierr )
          ! if the user requested accuracy level allows truncation in the
          ! computed upper triangular factor, the matrix r is examined and,
          ! if possible, replaced with its leading upper trapezoidal part.
           epsln = stdlib_dlamch('E')
           sfmin = stdlib_dlamch('S')
           ! small = sfmin / epsln
           nr = n
           if ( accla ) then
              ! standard absolute error bound suffices. all sigma_i with
              ! sigma_i < n*eps*||a||_f are flushed to zero. this is an
              ! aggressive enforcement of lower numerical rank by introducing a
              ! backward error of the order of n*eps*||a||_f.
              nr = 1_ilp
              rtmp = sqrt(real(n,KIND=dp))*epsln
              loop_3002: do p = 2, n
                 if ( abs(a(p,p)) < (rtmp*abs(a(1,1))) ) exit loop_3002
                    nr = nr + 1_ilp
              end do loop_3002
           elseif ( acclm ) then
              ! .. similarly as above, only slightly more gentle (less aggressive).
              ! sudden drop on the diagonal of r is used as the criterion for being
              ! close-to-rank-deficient. the threshold is set to epsln=stdlib_dlamch('e').
              ! [[this can be made more flexible by replacing this hard-coded value
              ! with a user specified threshold.]] also, the values that underflow
              ! will be truncated.
              nr = 1_ilp
              loop_3402: do p = 2, n
                 if ( ( abs(a(p,p)) < (epsln*abs(a(p-1,p-1))) ) .or.( abs(a(p,p)) < sfmin ) ) exit loop_3402
                 nr = nr + 1_ilp
              end do loop_3402
           else
              ! Rrqr Not Authorized To Determine Numerical Rank Except In The
              ! obvious case of zero pivots.
              ! .. inspect r for exact zeros on the diagonal;
              ! r(i,i)=0 => r(i:n,i:n)=0.
              nr = 1_ilp
              loop_3502: do p = 2, n
                 if ( abs(a(p,p)) == zero ) exit loop_3502
                 nr = nr + 1_ilp 
              end do loop_3502
              if ( conda ) then
                 ! estimate the scaled condition number of a. use the fact that it is
                 ! the same as the scaled condition number of r.
                    ! V Is Used As Workspace
                    call stdlib_dlacpy( 'U', n, n, a, lda, v, ldv )
                    ! only the leading nr x nr submatrix of the triangular factor
                    ! is considered. only if nr=n will this give a reliable error
                    ! bound. however, even for nr < n, this can be used on an
                    ! expert level and obtain useful information in the sense of
                    ! perturbation theory.
                    do p = 1, nr
                       rtmp = stdlib_dnrm2( p, v(1_ilp,p), 1_ilp )
                       call stdlib_dscal( p, one/rtmp, v(1_ilp,p), 1_ilp )
                    end do
                    if ( .not. ( lsvec .or. rsvec ) ) then
                        call stdlib_dpocon( 'U', nr, v, ldv, one, rtmp,work, iwork(n+iwoff), ierr &
                                  )
                    else
                        call stdlib_dpocon( 'U', nr, v, ldv, one, rtmp,work(n+1), iwork(n+iwoff), &
                                  ierr )
                    end if
                    sconda = one / sqrt(rtmp)
                 ! for nr=n, sconda is an estimate of sqrt(||(r^* * r)^(-1)||_1),
                 ! n^(-1/4) * sconda <= ||r^(-1)||_2 <= n^(1/4) * sconda
                 ! see the reference [1] for more details.
              end if
           endif
           if ( wntur ) then
               n1 = nr
           else if ( wntus .or. wntuf) then
               n1 = n
           else if ( wntua ) then
               n1 = m
           end if
           if ( .not. ( rsvec .or. lsvec ) ) then
      ! .......................................................................
              ! Only The Singular Values Are Requested
      ! .......................................................................
              if ( rtrans ) then
               ! .. compute the singular values of r**t = [a](1:nr,1:n)**t
                 ! .. set the lower triangle of [a] to [a](1:nr,1:n)**t and
                 ! the upper triangle of [a] to zero.
                 do p = 1, min( n, nr )
                    do q = p + 1, n
                       a(q,p) = a(p,q)
                       if ( q <= nr ) a(p,q) = zero
                    end do
                 end do
                 call stdlib_dgesvd( 'N', 'N', n, nr, a, lda, s, u, ldu,v, ldv, work, lwork, info &
                           )
              else
                 ! .. compute the singular values of r = [a](1:nr,1:n)
                 if ( nr > 1_ilp )call stdlib_dlaset( 'L', nr-1,nr-1, zero,zero, a(2_ilp,1_ilp), lda )
                 call stdlib_dgesvd( 'N', 'N', nr, n, a, lda, s, u, ldu,v, ldv, work, lwork, info &
                           )
              end if
           else if ( lsvec .and. ( .not. rsvec) ) then
      ! .......................................................................
             ! The Singular Values And The Left Singular Vectors Requested
      ! .......................................................................""""""""
              if ( rtrans ) then
                  ! .. apply stdlib_dgesvd to r**t
                  ! .. copy r**t into [u] and overwrite [u] with the right singular
                  ! vectors of r
                 do p = 1, nr
                    do q = p, n
                       u(q,p) = a(p,q)
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_dlaset( 'U', nr-1,nr-1, zero,zero, u(1_ilp,2_ilp), ldu )
                 ! .. the left singular vectors not computed, the nr right singular
                 ! vectors overwrite [u](1:nr,1:nr) as transposed. these
                 ! will be pre-multiplied by q to build the left singular vectors of a.
                    call stdlib_dgesvd( 'N', 'O', n, nr, u, ldu, s, u, ldu,u, ldu, work(n+1), &
                              lwork-n, info )
                    do p = 1, nr
                        do q = p + 1, nr
                           rtmp   = u(q,p)
                           u(q,p) = u(p,q)
                           u(p,q) = rtmp
                        end do
                    end do
              else
                  ! Apply Stdlib_Dgesvd To R
                  ! .. copy r into [u] and overwrite [u] with the left singular vectors
                  call stdlib_dlacpy( 'U', nr, n, a, lda, u, ldu )
                  if ( nr > 1_ilp )call stdlib_dlaset( 'L', nr-1, nr-1, zero, zero, u(2_ilp,1_ilp), ldu )
                            
                  ! .. the right singular vectors not computed, the nr left singular
                  ! vectors overwrite [u](1:nr,1:nr)
                     call stdlib_dgesvd( 'O', 'N', nr, n, u, ldu, s, u, ldu,v, ldv, work(n+1), &
                               lwork-n, info )
                     ! .. now [u](1:nr,1:nr) contains the nr left singular vectors of
                     ! r. these will be pre-multiplied by q to build the left singular
                     ! vectors of a.
              end if
                 ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
              if ( ( nr < m ) .and. ( .not.wntuf ) ) then
                  call stdlib_dlaset('A', m-nr, nr, zero, zero, u(nr+1,1_ilp), ldu)
                  if ( nr < n1 ) then
                     call stdlib_dlaset( 'A',nr,n1-nr,zero,zero,u(1_ilp,nr+1), ldu )
                     call stdlib_dlaset( 'A',m-nr,n1-nr,zero,one,u(nr+1,nr+1), ldu )
                  end if
              end if
                 ! the q matrix from the first qrf is built into the left singular
                 ! vectors matrix u.
              if ( .not.wntuf )call stdlib_dormqr( 'L', 'N', m, n1, n, a, lda, work, u,ldu, work(&
                        n+1), lwork-n, ierr )
              if ( rowprm .and. .not.wntuf )call stdlib_dlaswp( n1, u, ldu, 1_ilp, m-1, iwork(n+1), -&
                        1_ilp )
           else if ( rsvec .and. ( .not. lsvec ) ) then
      ! .......................................................................
             ! The Singular Values And The Right Singular Vectors Requested
      ! .......................................................................
               if ( rtrans ) then
                  ! .. apply stdlib_dgesvd to r**t
                  ! .. copy r**t into v and overwrite v with the left singular vectors
                 do p = 1, nr
                    do q = p, n
                       v(q,p) = (a(p,q))
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_dlaset( 'U', nr-1,nr-1, zero,zero, v(1_ilp,2_ilp), ldv )
                 ! .. the left singular vectors of r**t overwrite v, the right singular
                 ! vectors not computed
                 if ( wntvr .or. ( nr == n ) ) then
                    call stdlib_dgesvd( 'O', 'N', n, nr, v, ldv, s, u, ldu,u, ldu, work(n+1), &
                              lwork-n, info )
                    do p = 1, nr
                        do q = p + 1, nr
                           rtmp   = v(q,p)
                           v(q,p) = v(p,q)
                           v(p,q) = rtmp
                        end do
                    end do
                    if ( nr < n ) then
                        do p = 1, nr
                           do q = nr + 1, n
                               v(p,q) = v(q,p)
                           end do
                        end do
                    end if
                    call stdlib_dlapmt( .false., nr, n, v, ldv, iwork )
                 else
                     ! .. need all n right singular vectors and nr < n
                     ! [!] this is simple implementation that augments [v](1:n,1:nr)
                     ! by padding a zero block. in the case nr << n, a more efficient
                     ! way is to first use the qr factorization. for more details
                     ! how to implement this, see the " full svd " branch.
                     call stdlib_dlaset('G', n, n-nr, zero, zero, v(1_ilp,nr+1), ldv)
                     call stdlib_dgesvd( 'O', 'N', n, n, v, ldv, s, u, ldu,u, ldu, work(n+1), &
                               lwork-n, info )
                     do p = 1, n
                        do q = p + 1, n
                           rtmp   = v(q,p)
                           v(q,p) = v(p,q)
                           v(p,q) = rtmp
                        end do
                     end do
                     call stdlib_dlapmt( .false., n, n, v, ldv, iwork )
                 end if
               else
                  ! Aply Stdlib_Dgesvd To R
                  ! Copy R Into V And Overwrite V With The Right Singular Vectors
                  call stdlib_dlacpy( 'U', nr, n, a, lda, v, ldv )
                  if ( nr > 1_ilp )call stdlib_dlaset( 'L', nr-1, nr-1, zero, zero, v(2_ilp,1_ilp), ldv )
                            
                  ! .. the right singular vectors overwrite v, the nr left singular
                  ! vectors stored in u(1:nr,1:nr)
                  if ( wntvr .or. ( nr == n ) ) then
                     call stdlib_dgesvd( 'N', 'O', nr, n, v, ldv, s, u, ldu,v, ldv, work(n+1), &
                               lwork-n, info )
                     call stdlib_dlapmt( .false., nr, n, v, ldv, iwork )
                     ! .. now [v](1:nr,1:n) contains v(1:n,1:nr)**t
                  else
                     ! .. need all n right singular vectors and nr < n
                     ! [!] this is simple implementation that augments [v](1:nr,1:n)
                     ! by padding a zero block. in the case nr << n, a more efficient
                     ! way is to first use the lq factorization. for more details
                     ! how to implement this, see the " full svd " branch.
                      call stdlib_dlaset('G', n-nr, n, zero,zero, v(nr+1,1_ilp), ldv)
                      call stdlib_dgesvd( 'N', 'O', n, n, v, ldv, s, u, ldu,v, ldv, work(n+1), &
                                lwork-n, info )
                      call stdlib_dlapmt( .false., n, n, v, ldv, iwork )
                  end if
                  ! .. now [v] contains the transposed matrix of the right singular
                  ! vectors of a.
               end if
           else
      ! .......................................................................
             ! Full Svd Requested
      ! .......................................................................
              if ( rtrans ) then
                  ! .. apply stdlib_dgesvd to r**t [[this option is left for r
                 if ( wntvr .or. ( nr == n ) ) then
                  ! .. copy r**t into [v] and overwrite [v] with the left singular
                  ! vectors of r**t
                 do p = 1, nr
                    do q = p, n
                       v(q,p) = a(p,q)
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_dlaset( 'U', nr-1,nr-1, zero,zero, v(1_ilp,2_ilp), ldv )
                 ! .. the left singular vectors of r**t overwrite [v], the nr right
                 ! singular vectors of r**t stored in [u](1:nr,1:nr) as transposed
                    call stdlib_dgesvd( 'O', 'A', n, nr, v, ldv, s, v, ldv,u, ldu, work(n+1), &
                              lwork-n, info )
                    ! Assemble V
                    do p = 1, nr
                       do q = p + 1, nr
                          rtmp   = v(q,p)
                          v(q,p) = v(p,q)
                          v(p,q) = rtmp
                       end do
                    end do
                    if ( nr < n ) then
                        do p = 1, nr
                           do q = nr+1, n
                              v(p,q) = v(q,p)
                           end do
                        end do
                    end if
                    call stdlib_dlapmt( .false., nr, n, v, ldv, iwork )
                     do p = 1, nr
                        do q = p + 1, nr
                           rtmp   = u(q,p)
                           u(q,p) = u(p,q)
                           u(p,q) = rtmp
                        end do
                     end do
                     if ( ( nr < m ) .and. .not.(wntuf)) then
                       call stdlib_dlaset('A', m-nr,nr, zero,zero, u(nr+1,1_ilp), ldu)
                       if ( nr < n1 ) then
                          call stdlib_dlaset('A',nr,n1-nr,zero,zero,u(1_ilp,nr+1),ldu)
                          call stdlib_dlaset( 'A',m-nr,n1-nr,zero,one,u(nr+1,nr+1), ldu )
                       end if
                    end if
                 else
                     ! .. need all n right singular vectors and nr < n
                  ! .. copy r**t into [v] and overwrite [v] with the left singular
                  ! vectors of r**t
                     ! [[the optimal ratio n/nr for using qrf instead of padding
                       ! with zeros. here hard coded to 2; it must be at least
                       ! two due to work space constraints.]]
                     ! optratio = stdlib_ilaenv(6, 'dgesvd', 's' // 'o', nr,n,0,0)
                     ! optratio = max( optratio, 2 )
                     optratio = 2_ilp
                     if ( optratio*nr > n ) then
                        do p = 1, nr
                           do q = p, n
                              v(q,p) = a(p,q)
                           end do
                        end do
                        if ( nr > 1_ilp )call stdlib_dlaset('U',nr-1,nr-1, zero,zero, v(1_ilp,2_ilp),ldv)
                                  
                        call stdlib_dlaset('A',n,n-nr,zero,zero,v(1_ilp,nr+1),ldv)
                        call stdlib_dgesvd( 'O', 'A', n, n, v, ldv, s, v, ldv,u, ldu, work(n+1), &
                                  lwork-n, info )
                        do p = 1, n
                           do q = p + 1, n
                              rtmp   = v(q,p)
                              v(q,p) = v(p,q)
                              v(p,q) = rtmp
                           end do
                        end do
                        call stdlib_dlapmt( .false., n, n, v, ldv, iwork )
                    ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x n1), i.e. (m x n) or (m x m).
                        do p = 1, n
                           do q = p + 1, n
                              rtmp   = u(q,p)
                              u(q,p) = u(p,q)
                              u(p,q) = rtmp
                           end do
                        end do
                        if ( ( n < m ) .and. .not.(wntuf)) then
                           call stdlib_dlaset('A',m-n,n,zero,zero,u(n+1,1_ilp),ldu)
                           if ( n < n1 ) then
                             call stdlib_dlaset('A',n,n1-n,zero,zero,u(1_ilp,n+1),ldu)
                             call stdlib_dlaset('A',m-n,n1-n,zero,one,u(n+1,n+1), ldu )
                           end if
                        end if
                     else
                        ! .. copy r**t into [u] and overwrite [u] with the right
                        ! singular vectors of r
                        do p = 1, nr
                           do q = p, n
                              u(q,nr+p) = a(p,q)
                           end do
                        end do
                        if ( nr > 1_ilp )call stdlib_dlaset('U',nr-1,nr-1,zero,zero,u(1_ilp,nr+2),ldu)
                                  
                        call stdlib_dgeqrf( n, nr, u(1_ilp,nr+1), ldu, work(n+1),work(n+nr+1), lwork-&
                                  n-nr, ierr )
                        do p = 1, nr
                            do q = 1, n
                                v(q,p) = u(p,nr+q)
                            end do
                        end do
                       if (nr>1_ilp) call stdlib_dlaset('U',nr-1,nr-1,zero,zero,v(1_ilp,2_ilp),ldv)
                       call stdlib_dgesvd( 'S', 'O', nr, nr, v, ldv, s, u, ldu,v,ldv, work(n+nr+1)&
                                 ,lwork-n-nr, info )
                       call stdlib_dlaset('A',n-nr,nr,zero,zero,v(nr+1,1_ilp),ldv)
                       call stdlib_dlaset('A',nr,n-nr,zero,zero,v(1_ilp,nr+1),ldv)
                       call stdlib_dlaset('A',n-nr,n-nr,zero,one,v(nr+1,nr+1),ldv)
                       call stdlib_dormqr('R','C', n, n, nr, u(1_ilp,nr+1), ldu,work(n+1),v,ldv,work(&
                                 n+nr+1),lwork-n-nr,ierr)
                       call stdlib_dlapmt( .false., n, n, v, ldv, iwork )
                       ! Assemble The Left Singular Vector Matrix U Of Dimensions
                       ! (m x nr) or (m x n) or (m x m).
                       if ( ( nr < m ) .and. .not.(wntuf)) then
                          call stdlib_dlaset('A',m-nr,nr,zero,zero,u(nr+1,1_ilp),ldu)
                          if ( nr < n1 ) then
                          call stdlib_dlaset('A',nr,n1-nr,zero,zero,u(1_ilp,nr+1),ldu)
                          call stdlib_dlaset( 'A',m-nr,n1-nr,zero,one,u(nr+1,nr+1),ldu)
                          end if
                       end if
                     end if
                 end if
              else
                  ! .. apply stdlib_dgesvd to r [[this is the recommended option]]
                  if ( wntvr .or. ( nr == n ) ) then
                      ! .. copy r into [v] and overwrite v with the right singular vectors
                      call stdlib_dlacpy( 'U', nr, n, a, lda, v, ldv )
                     if ( nr > 1_ilp )call stdlib_dlaset( 'L', nr-1,nr-1, zero,zero, v(2_ilp,1_ilp), ldv )
                               
                     ! .. the right singular vectors of r overwrite [v], the nr left
                     ! singular vectors of r stored in [u](1:nr,1:nr)
                     call stdlib_dgesvd( 'S', 'O', nr, n, v, ldv, s, u, ldu,v, ldv, work(n+1), &
                               lwork-n, info )
                     call stdlib_dlapmt( .false., nr, n, v, ldv, iwork )
                     ! .. now [v](1:nr,1:n) contains v(1:n,1:nr)**t
                     ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
                    if ( ( nr < m ) .and. .not.(wntuf)) then
                       call stdlib_dlaset('A', m-nr,nr, zero,zero, u(nr+1,1_ilp), ldu)
                       if ( nr < n1 ) then
                          call stdlib_dlaset('A',nr,n1-nr,zero,zero,u(1_ilp,nr+1),ldu)
                          call stdlib_dlaset( 'A',m-nr,n1-nr,zero,one,u(nr+1,nr+1), ldu )
                       end if
                    end if
                  else
                    ! .. need all n right singular vectors and nr < n
                    ! The Requested Number Of The Left Singular Vectors
                     ! is then n1 (n or m)
                     ! [[the optimal ratio n/nr for using lq instead of padding
                       ! with zeros. here hard coded to 2; it must be at least
                       ! two due to work space constraints.]]
                     ! optratio = stdlib_ilaenv(6, 'dgesvd', 's' // 'o', nr,n,0,0)
                     ! optratio = max( optratio, 2 )
                    optratio = 2_ilp
                    if ( optratio * nr > n ) then
                       call stdlib_dlacpy( 'U', nr, n, a, lda, v, ldv )
                       if ( nr > 1_ilp )call stdlib_dlaset('L', nr-1,nr-1, zero,zero, v(2_ilp,1_ilp),ldv)
                                 
                    ! .. the right singular vectors of r overwrite [v], the nr left
                       ! singular vectors of r stored in [u](1:nr,1:nr)
                       call stdlib_dlaset('A', n-nr,n, zero,zero, v(nr+1,1_ilp),ldv)
                       call stdlib_dgesvd( 'S', 'O', n, n, v, ldv, s, u, ldu,v, ldv, work(n+1), &
                                 lwork-n, info )
                       call stdlib_dlapmt( .false., n, n, v, ldv, iwork )
                       ! .. now [v] contains the transposed matrix of the right
                       ! singular vectors of a. the leading n left singular vectors
                       ! are in [u](1:n,1:n)
                       ! Assemble The Left Singular Vector Matrix U Of Dimensions
                       ! (m x n1), i.e. (m x n) or (m x m).
                       if ( ( n < m ) .and. .not.(wntuf)) then
                           call stdlib_dlaset('A',m-n,n,zero,zero,u(n+1,1_ilp),ldu)
                           if ( n < n1 ) then
                             call stdlib_dlaset('A',n,n1-n,zero,zero,u(1_ilp,n+1),ldu)
                             call stdlib_dlaset( 'A',m-n,n1-n,zero,one,u(n+1,n+1), ldu )
                           end if
                       end if
                    else
                       call stdlib_dlacpy( 'U', nr, n, a, lda, u(nr+1,1_ilp), ldu )
                       if ( nr > 1_ilp )call stdlib_dlaset('L',nr-1,nr-1,zero,zero,u(nr+2,1_ilp),ldu)
                                 
                       call stdlib_dgelqf( nr, n, u(nr+1,1_ilp), ldu, work(n+1),work(n+nr+1), lwork-n-&
                                 nr, ierr )
                       call stdlib_dlacpy('L',nr,nr,u(nr+1,1_ilp),ldu,v,ldv)
                       if ( nr > 1_ilp )call stdlib_dlaset('U',nr-1,nr-1,zero,zero,v(1_ilp,2_ilp),ldv)
                       call stdlib_dgesvd( 'S', 'O', nr, nr, v, ldv, s, u, ldu,v, ldv, work(n+nr+&
                                 1_ilp), lwork-n-nr, info )
                       call stdlib_dlaset('A',n-nr,nr,zero,zero,v(nr+1,1_ilp),ldv)
                       call stdlib_dlaset('A',nr,n-nr,zero,zero,v(1_ilp,nr+1),ldv)
                       call stdlib_dlaset('A',n-nr,n-nr,zero,one,v(nr+1,nr+1),ldv)
                       call stdlib_dormlq('R','N',n,n,nr,u(nr+1,1_ilp),ldu,work(n+1),v, ldv, work(n+&
                                 nr+1),lwork-n-nr,ierr)
                       call stdlib_dlapmt( .false., n, n, v, ldv, iwork )
                     ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
                       if ( ( nr < m ) .and. .not.(wntuf)) then
                          call stdlib_dlaset('A',m-nr,nr,zero,zero,u(nr+1,1_ilp),ldu)
                          if ( nr < n1 ) then
                          call stdlib_dlaset('A',nr,n1-nr,zero,zero,u(1_ilp,nr+1),ldu)
                          call stdlib_dlaset( 'A',m-nr,n1-nr,zero,one,u(nr+1,nr+1), ldu )
                          end if
                       end if
                    end if
                  end if
              ! .. end of the "r**t or r" branch
              end if
                 ! the q matrix from the first qrf is built into the left singular
                 ! vectors matrix u.
              if ( .not. wntuf )call stdlib_dormqr( 'L', 'N', m, n1, n, a, lda, work, u,ldu, work(&
                        n+1), lwork-n, ierr )
              if ( rowprm .and. .not.wntuf )call stdlib_dlaswp( n1, u, ldu, 1_ilp, m-1, iwork(n+1), -&
                        1_ilp )
           ! ... end of the "full svd" branch
           end if
           ! check whether some singular values are returned as zeros, e.g.
           ! due to underflow, and update the numerical rank.
           p = nr
           do q = p, 1, -1
               if ( s(q) > zero ) go to 4002
               nr = nr - 1_ilp
           end do
           4002 continue
           ! .. if numerical rank deficiency is detected, the truncated
           ! singular values are set to zero.
           if ( nr < n ) call stdlib_dlaset( 'G', n-nr,1_ilp, zero,zero, s(nr+1), n )
           ! .. undo scaling; this may cause overflow in the largest singular
           ! values.
           if ( ascaled )call stdlib_dlascl( 'G',0_ilp,0_ilp, one,sqrt(real(m,KIND=dp)), nr,1_ilp, s, n, ierr &
                     )
           if ( conda ) rwork(1_ilp) = sconda
           rwork(2_ilp) = p - nr
           ! .. p-nr is the number of singular values that are computed as
           ! exact zeros in stdlib_dgesvd() applied to the (possibly truncated)
           ! full row rank triangular (trapezoidal) factor of a.
           numrank = nr
           return
     end subroutine stdlib_dgesvdq


     module subroutine stdlib_cgesvdq( joba, jobp, jobr, jobu, jobv, m, n, a, lda,s, u, ldu, v, ldv, &
     !! CGESVDQ computes the singular value decomposition (SVD) of a complex
     !! M-by-N matrix A, where M >= N. The SVD of A is written as
     !! [++]   [xx]   [x0]   [xx]
     !! A = U * SIGMA * V^*,  [++] = [xx] * [ox] * [xx]
     !! [++]   [xx]
     !! where SIGMA is an N-by-N diagonal matrix, U is an M-by-N orthonormal
     !! matrix, and V is an N-by-N unitary matrix. The diagonal elements
     !! of SIGMA are the singular values of A. The columns of U and V are the
     !! left and the right singular vectors of A, respectively.
               numrank, iwork, liwork,cwork, lcwork, rwork, lrwork, info )
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: joba, jobp, jobr, jobu, jobv
           integer(ilp), intent(in) :: m, n, lda, ldu, ldv, liwork, lrwork
           integer(ilp), intent(out) :: numrank, info
           integer(ilp), intent(inout) :: lcwork
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: u(ldu,*), v(ldv,*), cwork(*)
           real(sp), intent(out) :: s(*), rwork(*)
           integer(ilp), intent(out) :: iwork(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: ierr, nr, n1, optratio, p, q
           integer(ilp) :: lwcon, lwqp3, lwrk_cgelqf, lwrk_cgesvd, lwrk_cgesvd2, lwrk_cgeqp3, &
           lwrk_cgeqrf, lwrk_cunmlq, lwrk_cunmqr, lwrk_cunmqr2, lwlqf, lwqrf, lwsvd, lwsvd2, &
                     lwunq, lwunq2, lwunlq, minwrk, minwrk2, optwrk, optwrk2, iminwrk, rminwrk
           logical(lk) :: accla, acclm, acclh, ascaled, conda, dntwu, dntwv, lquery, lsvc0, lsvec,&
                      rowprm, rsvec, rtrans, wntua, wntuf, wntur, wntus, wntva, wntvr
           real(sp) :: big, epsln, rtmp, sconda, sfmin
           complex(sp) :: ctmp
           ! Local Arrays
           complex(sp) :: cdummy(1_ilp)
           real(sp) :: rdummy(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           wntus  = stdlib_lsame( jobu, 'S' ) .or. stdlib_lsame( jobu, 'U' )
           wntur  = stdlib_lsame( jobu, 'R' )
           wntua  = stdlib_lsame( jobu, 'A' )
           wntuf  = stdlib_lsame( jobu, 'F' )
           lsvc0  = wntus .or. wntur .or. wntua
           lsvec  = lsvc0 .or. wntuf
           dntwu  = stdlib_lsame( jobu, 'N' )
           wntvr  = stdlib_lsame( jobv, 'R' )
           wntva  = stdlib_lsame( jobv, 'A' ) .or. stdlib_lsame( jobv, 'V' )
           rsvec  = wntvr .or. wntva
           dntwv  = stdlib_lsame( jobv, 'N' )
           accla  = stdlib_lsame( joba, 'A' )
           acclm  = stdlib_lsame( joba, 'M' )
           conda  = stdlib_lsame( joba, 'E' )
           acclh  = stdlib_lsame( joba, 'H' ) .or. conda
           rowprm = stdlib_lsame( jobp, 'P' )
           rtrans = stdlib_lsame( jobr, 'T' )
           if ( rowprm ) then
              iminwrk = max( 1_ilp, n + m - 1_ilp )
              rminwrk = max( 2_ilp, m, 5_ilp*n )
           else
              iminwrk = max( 1_ilp, n )
              rminwrk = max( 2_ilp, 5_ilp*n )
           end if
           lquery = (liwork == -1_ilp .or. lcwork == -1_ilp .or. lrwork == -1_ilp)
           info  = 0_ilp
           if ( .not. ( accla .or. acclm .or. acclh ) ) then
              info = -1_ilp
           else if ( .not.( rowprm .or. stdlib_lsame( jobp, 'N' ) ) ) then
               info = -2_ilp
           else if ( .not.( rtrans .or. stdlib_lsame( jobr, 'N' ) ) ) then
               info = -3_ilp
           else if ( .not.( lsvec .or. dntwu ) ) then
              info = -4_ilp
           else if ( wntur .and. wntva ) then
              info = -5_ilp
           else if ( .not.( rsvec .or. dntwv )) then
              info = -5_ilp
           else if ( m<0_ilp ) then
              info = -6_ilp
           else if ( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -7_ilp
           else if ( lda<max( 1_ilp, m ) ) then
              info = -9_ilp
           else if ( ldu<1_ilp .or. ( lsvc0 .and. ldu<m ) .or.( wntuf .and. ldu<n ) ) then
              info = -12_ilp
           else if ( ldv<1_ilp .or. ( rsvec .and. ldv<n ) .or.( conda .and. ldv<n ) ) then
              info = -14_ilp
           else if ( liwork < iminwrk .and. .not. lquery ) then
              info = -17_ilp
           end if
           if ( info == 0_ilp ) then
           ! compute workspace
              ! Compute The Minimal And The Optimal Workspace Lengths
              ! [[the expressions for computing the minimal and the optimal
              ! values of lcwork are written with a lot of redundancy and
              ! can be simplified. however, this detailed form is easier for
              ! maintenance and modifications of the code.]]
              ! Minimal Workspace Length For Stdlib_Cgeqp3 Of An M X N Matrix
              lwqp3 = n+1
              ! Minimal Workspace Length For Stdlib_Cunmqr To Build Left Singular Vectors
              if ( wntus .or. wntur ) then
                  lwunq  = max( n  , 1_ilp )
              else if ( wntua ) then
                  lwunq = max( m , 1_ilp )
              end if
              ! Minimal Workspace Length For Stdlib_Cpocon Of An N X N Matrix
              lwcon = 2_ilp * n
              ! Stdlib_Cgesvd Of An N X N Matrix
              lwsvd = max( 3_ilp * n, 1_ilp )
              if ( lquery ) then
                  call stdlib_cgeqp3( m, n, a, lda, iwork, cdummy, cdummy, -1_ilp,rdummy, ierr )
                            
                  lwrk_cgeqp3 = int( cdummy(1_ilp),KIND=ilp)
                  if ( wntus .or. wntur ) then
                      call stdlib_cunmqr( 'L', 'N', m, n, n, a, lda, cdummy, u,ldu, cdummy, -1_ilp, &
                                ierr )
                      lwrk_cunmqr = int( cdummy(1_ilp),KIND=ilp)
                  else if ( wntua ) then
                      call stdlib_cunmqr( 'L', 'N', m, m, n, a, lda, cdummy, u,ldu, cdummy, -1_ilp, &
                                ierr )
                      lwrk_cunmqr = int( cdummy(1_ilp),KIND=ilp)
                  else
                      lwrk_cunmqr = 0_ilp
                  end if
              end if
              minwrk = 2_ilp
              optwrk = 2_ilp
              if ( .not. (lsvec .or. rsvec )) then
                  ! Minimal And Optimal Sizes Of The Complex Workspace If
                  ! only the singular values are requested
                  if ( conda ) then
                     minwrk = max( n+lwqp3, lwcon, lwsvd )
                  else
                     minwrk = max( n+lwqp3, lwsvd )
                  end if
                  if ( lquery ) then
                      call stdlib_cgesvd( 'N', 'N', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -1_ilp, &
                                rdummy, ierr )
                      lwrk_cgesvd = int( cdummy(1_ilp),KIND=ilp)
                      if ( conda ) then
                         optwrk = max( n+lwrk_cgeqp3, n+lwcon, lwrk_cgesvd )
                      else
                         optwrk = max( n+lwrk_cgeqp3, lwrk_cgesvd )
                      end if
                  end if
              else if ( lsvec .and. (.not.rsvec) ) then
                  ! Minimal And Optimal Sizes Of The Complex Workspace If The
                  ! singular values and the left singular vectors are requested
                  if ( conda ) then
                      minwrk = n + max( lwqp3, lwcon, lwsvd, lwunq )
                  else
                      minwrk = n + max( lwqp3, lwsvd, lwunq )
                  end if
                  if ( lquery ) then
                     if ( rtrans ) then
                        call stdlib_cgesvd( 'N', 'O', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -1_ilp, &
                                  rdummy, ierr )
                     else
                        call stdlib_cgesvd( 'O', 'N', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -1_ilp, &
                                  rdummy, ierr )
                     end if
                     lwrk_cgesvd = int( cdummy(1_ilp),KIND=ilp)
                     if ( conda ) then
                         optwrk = n + max( lwrk_cgeqp3, lwcon, lwrk_cgesvd,lwrk_cunmqr )
                     else
                         optwrk = n + max( lwrk_cgeqp3, lwrk_cgesvd,lwrk_cunmqr )
                     end if
                  end if
              else if ( rsvec .and. (.not.lsvec) ) then
                  ! Minimal And Optimal Sizes Of The Complex Workspace If The
                  ! singular values and the right singular vectors are requested
                  if ( conda ) then
                      minwrk = n + max( lwqp3, lwcon, lwsvd )
                  else
                      minwrk = n + max( lwqp3, lwsvd )
                  end if
                  if ( lquery ) then
                      if ( rtrans ) then
                          call stdlib_cgesvd( 'O', 'N', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -&
                                    1_ilp, rdummy, ierr )
                      else
                          call stdlib_cgesvd( 'N', 'O', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -&
                                    1_ilp, rdummy, ierr )
                      end if
                      lwrk_cgesvd = int( cdummy(1_ilp),KIND=ilp)
                      if ( conda ) then
                          optwrk = n + max( lwrk_cgeqp3, lwcon, lwrk_cgesvd )
                      else
                          optwrk = n + max( lwrk_cgeqp3, lwrk_cgesvd )
                      end if
                  end if
              else
                  ! Minimal And Optimal Sizes Of The Complex Workspace If The
                  ! full svd is requested
                  if ( rtrans ) then
                      minwrk = max( lwqp3, lwsvd, lwunq )
                      if ( conda ) minwrk = max( minwrk, lwcon )
                      minwrk = minwrk + n
                      if ( wntva ) then
                         ! .. minimal workspace length for n x n/2 stdlib_cgeqrf
                         lwqrf  = max( n/2_ilp, 1_ilp )
                         ! .. minimal workspace length for n/2 x n/2 stdlib_cgesvd
                         lwsvd2 = max( 3_ilp * (n/2_ilp), 1_ilp )
                         lwunq2 = max( n, 1_ilp )
                         minwrk2 = max( lwqp3, n/2_ilp+lwqrf, n/2_ilp+lwsvd2,n/2_ilp+lwunq2, lwunq )
                         if ( conda ) minwrk2 = max( minwrk2, lwcon )
                         minwrk2 = n + minwrk2
                         minwrk = max( minwrk, minwrk2 )
                      end if
                  else
                      minwrk = max( lwqp3, lwsvd, lwunq )
                      if ( conda ) minwrk = max( minwrk, lwcon )
                      minwrk = minwrk + n
                      if ( wntva ) then
                         ! .. minimal workspace length for n/2 x n stdlib_cgelqf
                         lwlqf  = max( n/2_ilp, 1_ilp )
                         lwsvd2 = max( 3_ilp * (n/2_ilp), 1_ilp )
                         lwunlq = max( n , 1_ilp )
                         minwrk2 = max( lwqp3, n/2_ilp+lwlqf, n/2_ilp+lwsvd2,n/2_ilp+lwunlq, lwunq )
                         if ( conda ) minwrk2 = max( minwrk2, lwcon )
                         minwrk2 = n + minwrk2
                         minwrk = max( minwrk, minwrk2 )
                      end if
                  end if
                  if ( lquery ) then
                     if ( rtrans ) then
                        call stdlib_cgesvd( 'O', 'A', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -1_ilp, &
                                  rdummy, ierr )
                        lwrk_cgesvd = int( cdummy(1_ilp),KIND=ilp)
                        optwrk = max(lwrk_cgeqp3,lwrk_cgesvd,lwrk_cunmqr)
                        if ( conda ) optwrk = max( optwrk, lwcon )
                        optwrk = n + optwrk
                        if ( wntva ) then
                            call stdlib_cgeqrf(n,n/2_ilp,u,ldu,cdummy,cdummy,-1_ilp,ierr)
                            lwrk_cgeqrf = int( cdummy(1_ilp),KIND=ilp)
                            call stdlib_cgesvd( 'S', 'O', n/2_ilp,n/2_ilp, v,ldv, s, u,ldu,v, ldv, cdummy,&
                                       -1_ilp, rdummy, ierr )
                            lwrk_cgesvd2 = int( cdummy(1_ilp),KIND=ilp)
                            call stdlib_cunmqr( 'R', 'C', n, n, n/2_ilp, u, ldu, cdummy,v, ldv, &
                                      cdummy, -1_ilp, ierr )
                            lwrk_cunmqr2 = int( cdummy(1_ilp),KIND=ilp)
                            optwrk2 = max( lwrk_cgeqp3, n/2_ilp+lwrk_cgeqrf,n/2_ilp+lwrk_cgesvd2, n/2_ilp+&
                                      lwrk_cunmqr2 )
                            if ( conda ) optwrk2 = max( optwrk2, lwcon )
                            optwrk2 = n + optwrk2
                            optwrk = max( optwrk, optwrk2 )
                        end if
                     else
                        call stdlib_cgesvd( 'S', 'O', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -1_ilp, &
                                  rdummy, ierr )
                        lwrk_cgesvd = int( cdummy(1_ilp),KIND=ilp)
                        optwrk = max(lwrk_cgeqp3,lwrk_cgesvd,lwrk_cunmqr)
                        if ( conda ) optwrk = max( optwrk, lwcon )
                        optwrk = n + optwrk
                        if ( wntva ) then
                           call stdlib_cgelqf(n/2_ilp,n,u,ldu,cdummy,cdummy,-1_ilp,ierr)
                           lwrk_cgelqf = int( cdummy(1_ilp),KIND=ilp)
                           call stdlib_cgesvd( 'S','O', n/2_ilp,n/2_ilp, v, ldv, s, u, ldu,v, ldv, cdummy,&
                                      -1_ilp, rdummy, ierr )
                           lwrk_cgesvd2 = int( cdummy(1_ilp),KIND=ilp)
                           call stdlib_cunmlq( 'R', 'N', n, n, n/2_ilp, u, ldu, cdummy,v, ldv, cdummy,&
                                     -1_ilp,ierr )
                           lwrk_cunmlq = int( cdummy(1_ilp),KIND=ilp)
                           optwrk2 = max( lwrk_cgeqp3, n/2_ilp+lwrk_cgelqf,n/2_ilp+lwrk_cgesvd2, n/2_ilp+&
                                     lwrk_cunmlq )
                            if ( conda ) optwrk2 = max( optwrk2, lwcon )
                            optwrk2 = n + optwrk2
                            optwrk = max( optwrk, optwrk2 )
                        end if
                     end if
                  end if
              end if
              minwrk = max( 2_ilp, minwrk )
              optwrk = max( 2_ilp, optwrk )
              if ( lcwork < minwrk .and. (.not.lquery) ) info = -19_ilp
           end if
           if (info == 0_ilp .and. lrwork < rminwrk .and. .not. lquery) then
              info = -21_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGESVDQ', -info )
              return
           else if ( lquery ) then
           ! return optimal workspace
               iwork(1_ilp) = iminwrk
               cwork(1_ilp) = optwrk
               cwork(2_ilp) = minwrk
               rwork(1_ilp) = rminwrk
               return
           end if
           ! quick return if the matrix is void.
           if( ( m==0_ilp ) .or. ( n==0_ilp ) ) then
           ! All Output Is Void
              return
           end if
           big = stdlib_slamch('O')
           ascaled = .false.
           if ( rowprm ) then
                 ! Reordering The Rows In Decreasing Sequence In The
                 ! ell-infinity norm - this enhances numerical robustness in
                 ! the case of differently scaled rows.
                 do p = 1, m
                     ! rwork(p) = abs( a(p,stdlib_icamax(n,a(p,1),lda)) )
                     ! [[stdlib_clange will return nan if an entry of the p-th row is nan]]
                     rwork(p) = stdlib_clange( 'M', 1_ilp, n, a(p,1_ilp), lda, rdummy )
                     ! .. check for nan's and inf's
                     if ( ( rwork(p) /= rwork(p) ) .or.( (rwork(p)*zero) /= zero ) ) then
                         info = - 8_ilp
                         call stdlib_xerbla( 'CGESVDQ', -info )
                         return
                     end if
                 end do
                 do p = 1, m - 1
                 q = stdlib_isamax( m-p+1, rwork(p), 1_ilp ) + p - 1_ilp
                 iwork(n+p) = q
                 if ( p /= q ) then
                    rtmp     = rwork(p)
                    rwork(p) = rwork(q)
                    rwork(q) = rtmp
                 end if
                 end do
                 if ( rwork(1_ilp) == zero ) then
                    ! quick return: a is the m x n zero matrix.
                    numrank = 0_ilp
                    call stdlib_slaset( 'G', n, 1_ilp, zero, zero, s, n )
                    if ( wntus ) call stdlib_claset('G', m, n, czero, cone, u, ldu)
                    if ( wntua ) call stdlib_claset('G', m, m, czero, cone, u, ldu)
                    if ( wntva ) call stdlib_claset('G', n, n, czero, cone, v, ldv)
                    if ( wntuf ) then
                        call stdlib_claset( 'G', n, 1_ilp, czero, czero, cwork, n )
                        call stdlib_claset( 'G', m, n, czero, cone, u, ldu )
                    end if
                    do p = 1, n
                        iwork(p) = p
                    end do
                    if ( rowprm ) then
                        do p = n + 1, n + m - 1
                            iwork(p) = p - n
                        end do
                    end if
                    if ( conda ) rwork(1_ilp) = -1_ilp
                    rwork(2_ilp) = -1_ilp
                    return
                 end if
                 if ( rwork(1_ilp) > big / sqrt(real(m,KIND=sp)) ) then
                     ! .. to prevent overflow in the qr factorization, scale the
                     ! matrix by 1/sqrt(m) if too large entry detected
                     call stdlib_clascl('G',0_ilp,0_ilp,sqrt(real(m,KIND=sp)),one, m,n, a,lda, ierr)
                               
                     ascaled = .true.
                 end if
                 call stdlib_claswp( n, a, lda, 1_ilp, m-1, iwork(n+1), 1_ilp )
           end if
          ! .. at this stage, preemptive scaling is done only to avoid column
          ! norms overflows during the qr factorization. the svd procedure should
          ! have its own scaling to save the singular values from overflows and
          ! underflows. that depends on the svd procedure.
           if ( .not.rowprm ) then
               rtmp = stdlib_clange( 'M', m, n, a, lda, rwork )
               if ( ( rtmp /= rtmp ) .or.( (rtmp*zero) /= zero ) ) then
                    info = - 8_ilp
                    call stdlib_xerbla( 'CGESVDQ', -info )
                    return
               end if
               if ( rtmp > big / sqrt(real(m,KIND=sp)) ) then
                   ! .. to prevent overflow in the qr factorization, scale the
                   ! matrix by 1/sqrt(m) if too large entry detected
                   call stdlib_clascl('G',0_ilp,0_ilp, sqrt(real(m,KIND=sp)),one, m,n, a,lda, ierr)
                             
                   ascaled = .true.
               end if
           end if
           ! Qr Factorization With Column Pivoting
           ! a * p = q * [ r ]
                       ! [ 0 ]
           do p = 1, n
              ! All Columns Are Free Columns
              iwork(p) = 0_ilp
           end do
           call stdlib_cgeqp3( m, n, a, lda, iwork, cwork, cwork(n+1), lcwork-n,rwork, ierr )
                     
          ! if the user requested accuracy level allows truncation in the
          ! computed upper triangular factor, the matrix r is examined and,
          ! if possible, replaced with its leading upper trapezoidal part.
           epsln = stdlib_slamch('E')
           sfmin = stdlib_slamch('S')
           ! small = sfmin / epsln
           nr = n
           if ( accla ) then
              ! standard absolute error bound suffices. all sigma_i with
              ! sigma_i < n*eps*||a||_f are flushed to zero. this is an
              ! aggressive enforcement of lower numerical rank by introducing a
              ! backward error of the order of n*eps*||a||_f.
              nr = 1_ilp
              rtmp = sqrt(real(n,KIND=sp))*epsln
              loop_3002: do p = 2, n
                 if ( abs(a(p,p)) < (rtmp*abs(a(1,1))) ) exit loop_3002
                    nr = nr + 1_ilp
              end do loop_3002
           elseif ( acclm ) then
              ! .. similarly as above, only slightly more gentle (less aggressive).
              ! sudden drop on the diagonal of r is used as the criterion for being
              ! close-to-rank-deficient. the threshold is set to epsln=stdlib_slamch('e').
              ! [[this can be made more flexible by replacing this hard-coded value
              ! with a user specified threshold.]] also, the values that underflow
              ! will be truncated.
              nr = 1_ilp
              loop_3402: do p = 2, n
                 if ( ( abs(a(p,p)) < (epsln*abs(a(p-1,p-1))) ) .or.( abs(a(p,p)) < sfmin ) ) exit loop_3402
                 nr = nr + 1_ilp
              end do loop_3402
           else
              ! Rrqr Not Authorized To Determine Numerical Rank Except In The
              ! obvious case of zero pivots.
              ! .. inspect r for exact zeros on the diagonal;
              ! r(i,i)=0 => r(i:n,i:n)=0.
              nr = 1_ilp
              loop_3502: do p = 2, n
                 if ( abs(a(p,p)) == zero ) exit loop_3502
                 nr = nr + 1_ilp
              end do loop_3502
              if ( conda ) then
                 ! estimate the scaled condition number of a. use the fact that it is
                 ! the same as the scaled condition number of r.
                    ! V Is Used As Workspace
                    call stdlib_clacpy( 'U', n, n, a, lda, v, ldv )
                    ! only the leading nr x nr submatrix of the triangular factor
                    ! is considered. only if nr=n will this give a reliable error
                    ! bound. however, even for nr < n, this can be used on an
                    ! expert level and obtain useful information in the sense of
                    ! perturbation theory.
                    do p = 1, nr
                       rtmp = stdlib_scnrm2( p, v(1_ilp,p), 1_ilp )
                       call stdlib_csscal( p, one/rtmp, v(1_ilp,p), 1_ilp )
                    end do
                    if ( .not. ( lsvec .or. rsvec ) ) then
                        call stdlib_cpocon( 'U', nr, v, ldv, one, rtmp,cwork, rwork, ierr )
                                  
                    else
                        call stdlib_cpocon( 'U', nr, v, ldv, one, rtmp,cwork(n+1), rwork, ierr )
                                  
                    end if
                    sconda = one / sqrt(rtmp)
                 ! for nr=n, sconda is an estimate of sqrt(||(r^* * r)^(-1)||_1),
                 ! n^(-1/4) * sconda <= ||r^(-1)||_2 <= n^(1/4) * sconda
                 ! see the reference [1] for more details.
              end if
           endif
           if ( wntur ) then
               n1 = nr
           else if ( wntus .or. wntuf) then
               n1 = n
           else if ( wntua ) then
               n1 = m
           end if
           if ( .not. ( rsvec .or. lsvec ) ) then
      ! .......................................................................
              ! Only The Singular Values Are Requested
      ! .......................................................................
              if ( rtrans ) then
               ! .. compute the singular values of r**h = [a](1:nr,1:n)**h
                 ! .. set the lower triangle of [a] to [a](1:nr,1:n)**h and
                 ! the upper triangle of [a] to zero.
                 do p = 1, min( n, nr )
                    a(p,p) = conjg(a(p,p))
                    do q = p + 1, n
                       a(q,p) = conjg(a(p,q))
                       if ( q <= nr ) a(p,q) = czero
                    end do
                 end do
                 call stdlib_cgesvd( 'N', 'N', n, nr, a, lda, s, u, ldu,v, ldv, cwork, lcwork, &
                           rwork, info )
              else
                 ! .. compute the singular values of r = [a](1:nr,1:n)
                 if ( nr > 1_ilp )call stdlib_claset( 'L', nr-1,nr-1, czero,czero, a(2_ilp,1_ilp), lda )
                           
                 call stdlib_cgesvd( 'N', 'N', nr, n, a, lda, s, u, ldu,v, ldv, cwork, lcwork, &
                           rwork, info )
              end if
           else if ( lsvec .and. ( .not. rsvec) ) then
      ! .......................................................................
             ! The Singular Values And The Left Singular Vectors Requested
      ! .......................................................................""""""""
              if ( rtrans ) then
                  ! .. apply stdlib_cgesvd to r**h
                  ! .. copy r**h into [u] and overwrite [u] with the right singular
                  ! vectors of r
                 do p = 1, nr
                    do q = p, n
                       u(q,p) = conjg(a(p,q))
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_claset( 'U', nr-1,nr-1, czero,czero, u(1_ilp,2_ilp), ldu )
                           
                 ! .. the left singular vectors not computed, the nr right singular
                 ! vectors overwrite [u](1:nr,1:nr) as conjugate transposed. these
                 ! will be pre-multiplied by q to build the left singular vectors of a.
                    call stdlib_cgesvd( 'N', 'O', n, nr, u, ldu, s, u, ldu,u, ldu, cwork(n+1), &
                              lcwork-n, rwork, info )
                    do p = 1, nr
                        u(p,p) = conjg(u(p,p))
                        do q = p + 1, nr
                           ctmp   = conjg(u(q,p))
                           u(q,p) = conjg(u(p,q))
                           u(p,q) = ctmp
                        end do
                    end do
              else
                  ! Apply Stdlib_Cgesvd To R
                  ! .. copy r into [u] and overwrite [u] with the left singular vectors
                  call stdlib_clacpy( 'U', nr, n, a, lda, u, ldu )
                  if ( nr > 1_ilp )call stdlib_claset( 'L', nr-1, nr-1, czero, czero, u(2_ilp,1_ilp), ldu )
                            
                  ! .. the right singular vectors not computed, the nr left singular
                  ! vectors overwrite [u](1:nr,1:nr)
                     call stdlib_cgesvd( 'O', 'N', nr, n, u, ldu, s, u, ldu,v, ldv, cwork(n+1), &
                               lcwork-n, rwork, info )
                     ! .. now [u](1:nr,1:nr) contains the nr left singular vectors of
                     ! r. these will be pre-multiplied by q to build the left singular
                     ! vectors of a.
              end if
                 ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
              if ( ( nr < m ) .and. ( .not.wntuf ) ) then
                  call stdlib_claset('A', m-nr, nr, czero, czero, u(nr+1,1_ilp), ldu)
                  if ( nr < n1 ) then
                     call stdlib_claset( 'A',nr,n1-nr,czero,czero,u(1_ilp,nr+1), ldu )
                     call stdlib_claset( 'A',m-nr,n1-nr,czero,cone,u(nr+1,nr+1), ldu )
                  end if
              end if
                 ! the q matrix from the first qrf is built into the left singular
                 ! vectors matrix u.
              if ( .not.wntuf )call stdlib_cunmqr( 'L', 'N', m, n1, n, a, lda, cwork, u,ldu, &
                        cwork(n+1), lcwork-n, ierr )
              if ( rowprm .and. .not.wntuf )call stdlib_claswp( n1, u, ldu, 1_ilp, m-1, iwork(n+1), -&
                        1_ilp )
           else if ( rsvec .and. ( .not. lsvec ) ) then
      ! .......................................................................
             ! The Singular Values And The Right Singular Vectors Requested
      ! .......................................................................
               if ( rtrans ) then
                  ! .. apply stdlib_cgesvd to r**h
                  ! .. copy r**h into v and overwrite v with the left singular vectors
                 do p = 1, nr
                    do q = p, n
                       v(q,p) = conjg(a(p,q))
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_claset( 'U', nr-1,nr-1, czero,czero, v(1_ilp,2_ilp), ldv )
                           
                 ! .. the left singular vectors of r**h overwrite v, the right singular
                 ! vectors not computed
                 if ( wntvr .or. ( nr == n ) ) then
                    call stdlib_cgesvd( 'O', 'N', n, nr, v, ldv, s, u, ldu,u, ldu, cwork(n+1), &
                              lcwork-n, rwork, info )
                    do p = 1, nr
                        v(p,p) = conjg(v(p,p))
                        do q = p + 1, nr
                           ctmp   = conjg(v(q,p))
                           v(q,p) = conjg(v(p,q))
                           v(p,q) = ctmp
                        end do
                    end do
                    if ( nr < n ) then
                        do p = 1, nr
                           do q = nr + 1, n
                               v(p,q) = conjg(v(q,p))
                           end do
                        end do
                    end if
                    call stdlib_clapmt( .false., nr, n, v, ldv, iwork )
                 else
                     ! .. need all n right singular vectors and nr < n
                     ! [!] this is simple implementation that augments [v](1:n,1:nr)
                     ! by padding a zero block. in the case nr << n, a more efficient
                     ! way is to first use the qr factorization. for more details
                     ! how to implement this, see the " full svd " branch.
                     call stdlib_claset('G', n, n-nr, czero, czero, v(1_ilp,nr+1), ldv)
                     call stdlib_cgesvd( 'O', 'N', n, n, v, ldv, s, u, ldu,u, ldu, cwork(n+1), &
                               lcwork-n, rwork, info )
                     do p = 1, n
                        v(p,p) = conjg(v(p,p))
                        do q = p + 1, n
                           ctmp   = conjg(v(q,p))
                           v(q,p) = conjg(v(p,q))
                           v(p,q) = ctmp
                        end do
                     end do
                     call stdlib_clapmt( .false., n, n, v, ldv, iwork )
                 end if
               else
                  ! Aply Stdlib_Cgesvd To R
                  ! Copy R Into V And Overwrite V With The Right Singular Vectors
                  call stdlib_clacpy( 'U', nr, n, a, lda, v, ldv )
                  if ( nr > 1_ilp )call stdlib_claset( 'L', nr-1, nr-1, czero, czero, v(2_ilp,1_ilp), ldv )
                            
                  ! .. the right singular vectors overwrite v, the nr left singular
                  ! vectors stored in u(1:nr,1:nr)
                  if ( wntvr .or. ( nr == n ) ) then
                     call stdlib_cgesvd( 'N', 'O', nr, n, v, ldv, s, u, ldu,v, ldv, cwork(n+1), &
                               lcwork-n, rwork, info )
                     call stdlib_clapmt( .false., nr, n, v, ldv, iwork )
                     ! .. now [v](1:nr,1:n) contains v(1:n,1:nr)**h
                  else
                     ! .. need all n right singular vectors and nr < n
                     ! [!] this is simple implementation that augments [v](1:nr,1:n)
                     ! by padding a zero block. in the case nr << n, a more efficient
                     ! way is to first use the lq factorization. for more details
                     ! how to implement this, see the " full svd " branch.
                      call stdlib_claset('G', n-nr, n, czero,czero, v(nr+1,1_ilp), ldv)
                      call stdlib_cgesvd( 'N', 'O', n, n, v, ldv, s, u, ldu,v, ldv, cwork(n+1), &
                                lcwork-n, rwork, info )
                      call stdlib_clapmt( .false., n, n, v, ldv, iwork )
                  end if
                  ! .. now [v] contains the adjoint of the matrix of the right singular
                  ! vectors of a.
               end if
           else
      ! .......................................................................
             ! Full Svd Requested
      ! .......................................................................
              if ( rtrans ) then
                  ! .. apply stdlib_cgesvd to r**h [[this option is left for r
                 if ( wntvr .or. ( nr == n ) ) then
                  ! .. copy r**h into [v] and overwrite [v] with the left singular
                  ! vectors of r**h
                 do p = 1, nr
                    do q = p, n
                       v(q,p) = conjg(a(p,q))
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_claset( 'U', nr-1,nr-1, czero,czero, v(1_ilp,2_ilp), ldv )
                           
                 ! .. the left singular vectors of r**h overwrite [v], the nr right
                 ! singular vectors of r**h stored in [u](1:nr,1:nr) as conjugate
                 ! transposed
                    call stdlib_cgesvd( 'O', 'A', n, nr, v, ldv, s, v, ldv,u, ldu, cwork(n+1), &
                              lcwork-n, rwork, info )
                    ! Assemble V
                    do p = 1, nr
                       v(p,p) = conjg(v(p,p))
                       do q = p + 1, nr
                          ctmp   = conjg(v(q,p))
                          v(q,p) = conjg(v(p,q))
                          v(p,q) = ctmp
                       end do
                    end do
                    if ( nr < n ) then
                        do p = 1, nr
                           do q = nr+1, n
                              v(p,q) = conjg(v(q,p))
                           end do
                        end do
                    end if
                    call stdlib_clapmt( .false., nr, n, v, ldv, iwork )
                     do p = 1, nr
                        u(p,p) = conjg(u(p,p))
                        do q = p + 1, nr
                           ctmp   = conjg(u(q,p))
                           u(q,p) = conjg(u(p,q))
                           u(p,q) = ctmp
                        end do
                     end do
                     if ( ( nr < m ) .and. .not.(wntuf)) then
                       call stdlib_claset('A', m-nr,nr, czero,czero, u(nr+1,1_ilp), ldu)
                       if ( nr < n1 ) then
                          call stdlib_claset('A',nr,n1-nr,czero,czero,u(1_ilp,nr+1),ldu)
                          call stdlib_claset( 'A',m-nr,n1-nr,czero,cone,u(nr+1,nr+1), ldu )
                                    
                       end if
                    end if
                 else
                     ! .. need all n right singular vectors and nr < n
                  ! .. copy r**h into [v] and overwrite [v] with the left singular
                  ! vectors of r**h
                     ! [[the optimal ratio n/nr for using qrf instead of padding
                       ! with zeros. here hard coded to 2; it must be at least
                       ! two due to work space constraints.]]
                     ! optratio = stdlib_ilaenv(6, 'cgesvd', 's' // 'o', nr,n,0,0)
                     ! optratio = max( optratio, 2 )
                     optratio = 2_ilp
                     if ( optratio*nr > n ) then
                        do p = 1, nr
                           do q = p, n
                              v(q,p) = conjg(a(p,q))
                           end do
                        end do
                        if ( nr > 1_ilp )call stdlib_claset('U',nr-1,nr-1, czero,czero, v(1_ilp,2_ilp),ldv)
                                  
                        call stdlib_claset('A',n,n-nr,czero,czero,v(1_ilp,nr+1),ldv)
                        call stdlib_cgesvd( 'O', 'A', n, n, v, ldv, s, v, ldv,u, ldu, cwork(n+1), &
                                  lcwork-n, rwork, info )
                        do p = 1, n
                           v(p,p) = conjg(v(p,p))
                           do q = p + 1, n
                              ctmp   = conjg(v(q,p))
                              v(q,p) = conjg(v(p,q))
                              v(p,q) = ctmp
                           end do
                        end do
                        call stdlib_clapmt( .false., n, n, v, ldv, iwork )
                    ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x n1), i.e. (m x n) or (m x m).
                        do p = 1, n
                           u(p,p) = conjg(u(p,p))
                           do q = p + 1, n
                              ctmp   = conjg(u(q,p))
                              u(q,p) = conjg(u(p,q))
                              u(p,q) = ctmp
                           end do
                        end do
                        if ( ( n < m ) .and. .not.(wntuf)) then
                           call stdlib_claset('A',m-n,n,czero,czero,u(n+1,1_ilp),ldu)
                           if ( n < n1 ) then
                             call stdlib_claset('A',n,n1-n,czero,czero,u(1_ilp,n+1),ldu)
                             call stdlib_claset('A',m-n,n1-n,czero,cone,u(n+1,n+1), ldu )
                           end if
                        end if
                     else
                        ! .. copy r**h into [u] and overwrite [u] with the right
                        ! singular vectors of r
                        do p = 1, nr
                           do q = p, n
                              u(q,nr+p) = conjg(a(p,q))
                           end do
                        end do
                        if ( nr > 1_ilp )call stdlib_claset('U',nr-1,nr-1,czero,czero,u(1_ilp,nr+2),ldu)
                                  
                        call stdlib_cgeqrf( n, nr, u(1_ilp,nr+1), ldu, cwork(n+1),cwork(n+nr+1), &
                                  lcwork-n-nr, ierr )
                        do p = 1, nr
                            do q = 1, n
                                v(q,p) = conjg(u(p,nr+q))
                            end do
                        end do
                       if (nr>1_ilp) call stdlib_claset('U',nr-1,nr-1,czero,czero,v(1_ilp,2_ilp),ldv)
                       call stdlib_cgesvd( 'S', 'O', nr, nr, v, ldv, s, u, ldu,v,ldv, cwork(n+nr+&
                                 1_ilp),lcwork-n-nr,rwork, info )
                       call stdlib_claset('A',n-nr,nr,czero,czero,v(nr+1,1_ilp),ldv)
                       call stdlib_claset('A',nr,n-nr,czero,czero,v(1_ilp,nr+1),ldv)
                       call stdlib_claset('A',n-nr,n-nr,czero,cone,v(nr+1,nr+1),ldv)
                       call stdlib_cunmqr('R','C', n, n, nr, u(1_ilp,nr+1), ldu,cwork(n+1),v,ldv,&
                                 cwork(n+nr+1),lcwork-n-nr,ierr)
                       call stdlib_clapmt( .false., n, n, v, ldv, iwork )
                       ! Assemble The Left Singular Vector Matrix U Of Dimensions
                       ! (m x nr) or (m x n) or (m x m).
                       if ( ( nr < m ) .and. .not.(wntuf)) then
                          call stdlib_claset('A',m-nr,nr,czero,czero,u(nr+1,1_ilp),ldu)
                          if ( nr < n1 ) then
                          call stdlib_claset('A',nr,n1-nr,czero,czero,u(1_ilp,nr+1),ldu)
                          call stdlib_claset( 'A',m-nr,n1-nr,czero,cone,u(nr+1,nr+1),ldu)
                          end if
                       end if
                     end if
                 end if
              else
                  ! .. apply stdlib_cgesvd to r [[this is the recommended option]]
                  if ( wntvr .or. ( nr == n ) ) then
                      ! .. copy r into [v] and overwrite v with the right singular vectors
                      call stdlib_clacpy( 'U', nr, n, a, lda, v, ldv )
                     if ( nr > 1_ilp )call stdlib_claset( 'L', nr-1,nr-1, czero,czero, v(2_ilp,1_ilp), ldv )
                               
                     ! .. the right singular vectors of r overwrite [v], the nr left
                     ! singular vectors of r stored in [u](1:nr,1:nr)
                     call stdlib_cgesvd( 'S', 'O', nr, n, v, ldv, s, u, ldu,v, ldv, cwork(n+1), &
                               lcwork-n, rwork, info )
                     call stdlib_clapmt( .false., nr, n, v, ldv, iwork )
                     ! .. now [v](1:nr,1:n) contains v(1:n,1:nr)**h
                     ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
                    if ( ( nr < m ) .and. .not.(wntuf)) then
                       call stdlib_claset('A', m-nr,nr, czero,czero, u(nr+1,1_ilp), ldu)
                       if ( nr < n1 ) then
                          call stdlib_claset('A',nr,n1-nr,czero,czero,u(1_ilp,nr+1),ldu)
                          call stdlib_claset( 'A',m-nr,n1-nr,czero,cone,u(nr+1,nr+1), ldu )
                                    
                       end if
                    end if
                  else
                    ! .. need all n right singular vectors and nr < n
                    ! The Requested Number Of The Left Singular Vectors
                     ! is then n1 (n or m)
                     ! [[the optimal ratio n/nr for using lq instead of padding
                       ! with zeros. here hard coded to 2; it must be at least
                       ! two due to work space constraints.]]
                     ! optratio = stdlib_ilaenv(6, 'cgesvd', 's' // 'o', nr,n,0,0)
                     ! optratio = max( optratio, 2 )
                    optratio = 2_ilp
                    if ( optratio * nr > n ) then
                       call stdlib_clacpy( 'U', nr, n, a, lda, v, ldv )
                       if ( nr > 1_ilp )call stdlib_claset('L', nr-1,nr-1, czero,czero, v(2_ilp,1_ilp),ldv)
                                 
                    ! .. the right singular vectors of r overwrite [v], the nr left
                       ! singular vectors of r stored in [u](1:nr,1:nr)
                       call stdlib_claset('A', n-nr,n, czero,czero, v(nr+1,1_ilp),ldv)
                       call stdlib_cgesvd( 'S', 'O', n, n, v, ldv, s, u, ldu,v, ldv, cwork(n+1), &
                                 lcwork-n, rwork, info )
                       call stdlib_clapmt( .false., n, n, v, ldv, iwork )
                       ! .. now [v] contains the adjoint of the matrix of the right
                       ! singular vectors of a. the leading n left singular vectors
                       ! are in [u](1:n,1:n)
                       ! Assemble The Left Singular Vector Matrix U Of Dimensions
                       ! (m x n1), i.e. (m x n) or (m x m).
                       if ( ( n < m ) .and. .not.(wntuf)) then
                           call stdlib_claset('A',m-n,n,czero,czero,u(n+1,1_ilp),ldu)
                           if ( n < n1 ) then
                             call stdlib_claset('A',n,n1-n,czero,czero,u(1_ilp,n+1),ldu)
                             call stdlib_claset( 'A',m-n,n1-n,czero,cone,u(n+1,n+1), ldu )
                           end if
                       end if
                    else
                       call stdlib_clacpy( 'U', nr, n, a, lda, u(nr+1,1_ilp), ldu )
                       if ( nr > 1_ilp )call stdlib_claset('L',nr-1,nr-1,czero,czero,u(nr+2,1_ilp),ldu)
                                 
                       call stdlib_cgelqf( nr, n, u(nr+1,1_ilp), ldu, cwork(n+1),cwork(n+nr+1), &
                                 lcwork-n-nr, ierr )
                       call stdlib_clacpy('L',nr,nr,u(nr+1,1_ilp),ldu,v,ldv)
                       if ( nr > 1_ilp )call stdlib_claset('U',nr-1,nr-1,czero,czero,v(1_ilp,2_ilp),ldv)
                                 
                       call stdlib_cgesvd( 'S', 'O', nr, nr, v, ldv, s, u, ldu,v, ldv, cwork(n+nr+&
                                 1_ilp), lcwork-n-nr, rwork, info )
                       call stdlib_claset('A',n-nr,nr,czero,czero,v(nr+1,1_ilp),ldv)
                       call stdlib_claset('A',nr,n-nr,czero,czero,v(1_ilp,nr+1),ldv)
                       call stdlib_claset('A',n-nr,n-nr,czero,cone,v(nr+1,nr+1),ldv)
                       call stdlib_cunmlq('R','N',n,n,nr,u(nr+1,1_ilp),ldu,cwork(n+1),v, ldv, cwork(n+&
                                 nr+1),lcwork-n-nr,ierr)
                       call stdlib_clapmt( .false., n, n, v, ldv, iwork )
                     ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
                       if ( ( nr < m ) .and. .not.(wntuf)) then
                          call stdlib_claset('A',m-nr,nr,czero,czero,u(nr+1,1_ilp),ldu)
                          if ( nr < n1 ) then
                          call stdlib_claset('A',nr,n1-nr,czero,czero,u(1_ilp,nr+1),ldu)
                          call stdlib_claset( 'A',m-nr,n1-nr,czero,cone,u(nr+1,nr+1), ldu )
                                    
                          end if
                       end if
                    end if
                  end if
              ! .. end of the "r**h or r" branch
              end if
                 ! the q matrix from the first qrf is built into the left singular
                 ! vectors matrix u.
              if ( .not. wntuf )call stdlib_cunmqr( 'L', 'N', m, n1, n, a, lda, cwork, u,ldu, &
                        cwork(n+1), lcwork-n, ierr )
              if ( rowprm .and. .not.wntuf )call stdlib_claswp( n1, u, ldu, 1_ilp, m-1, iwork(n+1), -&
                        1_ilp )
           ! ... end of the "full svd" branch
           end if
           ! check whether some singular values are returned as zeros, e.g.
           ! due to underflow, and update the numerical rank.
           p = nr
           do q = p, 1, -1
               if ( s(q) > zero ) go to 4002
               nr = nr - 1_ilp
           end do
           4002 continue
           ! .. if numerical rank deficiency is detected, the truncated
           ! singular values are set to zero.
           if ( nr < n ) call stdlib_slaset( 'G', n-nr,1_ilp, zero,zero, s(nr+1), n )
           ! .. undo scaling; this may cause overflow in the largest singular
           ! values.
           if ( ascaled )call stdlib_slascl( 'G',0_ilp,0_ilp, one,sqrt(real(m,KIND=sp)), nr,1_ilp, s, n, ierr &
                     )
           if ( conda ) rwork(1_ilp) = sconda
           rwork(2_ilp) = p - nr
           ! .. p-nr is the number of singular values that are computed as
           ! exact zeros in stdlib_cgesvd() applied to the (possibly truncated)
           ! full row rank triangular (trapezoidal) factor of a.
           numrank = nr
           return
     end subroutine stdlib_cgesvdq

     module subroutine stdlib_zgesvdq( joba, jobp, jobr, jobu, jobv, m, n, a, lda,s, u, ldu, v, ldv, &
     !! ZCGESVDQ computes the singular value decomposition (SVD) of a complex
     !! M-by-N matrix A, where M >= N. The SVD of A is written as
     !! [++]   [xx]   [x0]   [xx]
     !! A = U * SIGMA * V^*,  [++] = [xx] * [ox] * [xx]
     !! [++]   [xx]
     !! where SIGMA is an N-by-N diagonal matrix, U is an M-by-N orthonormal
     !! matrix, and V is an N-by-N unitary matrix. The diagonal elements
     !! of SIGMA are the singular values of A. The columns of U and V are the
     !! left and the right singular vectors of A, respectively.
               numrank, iwork, liwork,cwork, lcwork, rwork, lrwork, info )
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: joba, jobp, jobr, jobu, jobv
           integer(ilp), intent(in) :: m, n, lda, ldu, ldv, liwork, lrwork
           integer(ilp), intent(out) :: numrank, info
           integer(ilp), intent(inout) :: lcwork
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: u(ldu,*), v(ldv,*), cwork(*)
           real(dp), intent(out) :: s(*), rwork(*)
           integer(ilp), intent(out) :: iwork(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: ierr, nr, n1, optratio, p, q
           integer(ilp) :: lwcon, lwqp3, lwrk_zgelqf, lwrk_zgesvd, lwrk_zgesvd2, lwrk_zgeqp3, &
           lwrk_zgeqrf, lwrk_zunmlq, lwrk_zunmqr, lwrk_zunmqr2, lwlqf, lwqrf, lwsvd, lwsvd2, &
                     lwunq, lwunq2, lwunlq, minwrk, minwrk2, optwrk, optwrk2, iminwrk, rminwrk
           logical(lk) :: accla, acclm, acclh, ascaled, conda, dntwu, dntwv, lquery, lsvc0, lsvec,&
                      rowprm, rsvec, rtrans, wntua, wntuf, wntur, wntus, wntva, wntvr
           real(dp) :: big, epsln, rtmp, sconda, sfmin
           complex(dp) :: ctmp
           ! Local Arrays
           complex(dp) :: cdummy(1_ilp)
           real(dp) :: rdummy(1_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           wntus  = stdlib_lsame( jobu, 'S' ) .or. stdlib_lsame( jobu, 'U' )
           wntur  = stdlib_lsame( jobu, 'R' )
           wntua  = stdlib_lsame( jobu, 'A' )
           wntuf  = stdlib_lsame( jobu, 'F' )
           lsvc0  = wntus .or. wntur .or. wntua
           lsvec  = lsvc0 .or. wntuf
           dntwu  = stdlib_lsame( jobu, 'N' )
           wntvr  = stdlib_lsame( jobv, 'R' )
           wntva  = stdlib_lsame( jobv, 'A' ) .or. stdlib_lsame( jobv, 'V' )
           rsvec  = wntvr .or. wntva
           dntwv  = stdlib_lsame( jobv, 'N' )
           accla  = stdlib_lsame( joba, 'A' )
           acclm  = stdlib_lsame( joba, 'M' )
           conda  = stdlib_lsame( joba, 'E' )
           acclh  = stdlib_lsame( joba, 'H' ) .or. conda
           rowprm = stdlib_lsame( jobp, 'P' )
           rtrans = stdlib_lsame( jobr, 'T' )
           if ( rowprm ) then
              iminwrk = max( 1_ilp, n + m - 1_ilp )
              rminwrk = max( 2_ilp, m, 5_ilp*n )
           else
              iminwrk = max( 1_ilp, n )
              rminwrk = max( 2_ilp, 5_ilp*n )
           end if
           lquery = (liwork == -1_ilp .or. lcwork == -1_ilp .or. lrwork == -1_ilp)
           info  = 0_ilp
           if ( .not. ( accla .or. acclm .or. acclh ) ) then
              info = -1_ilp
           else if ( .not.( rowprm .or. stdlib_lsame( jobp, 'N' ) ) ) then
               info = -2_ilp
           else if ( .not.( rtrans .or. stdlib_lsame( jobr, 'N' ) ) ) then
               info = -3_ilp
           else if ( .not.( lsvec .or. dntwu ) ) then
              info = -4_ilp
           else if ( wntur .and. wntva ) then
              info = -5_ilp
           else if ( .not.( rsvec .or. dntwv )) then
              info = -5_ilp
           else if ( m<0_ilp ) then
              info = -6_ilp
           else if ( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -7_ilp
           else if ( lda<max( 1_ilp, m ) ) then
              info = -9_ilp
           else if ( ldu<1_ilp .or. ( lsvc0 .and. ldu<m ) .or.( wntuf .and. ldu<n ) ) then
              info = -12_ilp
           else if ( ldv<1_ilp .or. ( rsvec .and. ldv<n ) .or.( conda .and. ldv<n ) ) then
              info = -14_ilp
           else if ( liwork < iminwrk .and. .not. lquery ) then
              info = -17_ilp
           end if
           if ( info == 0_ilp ) then
              ! Compute The Minimal And The Optimal Workspace Lengths
              ! [[the expressions for computing the minimal and the optimal
              ! values of lcwork are written with a lot of redundancy and
              ! can be simplified. however, this detailed form is easier for
              ! maintenance and modifications of the code.]]
              ! Minimal Workspace Length For Stdlib_Zgeqp3 Of An M X N Matrix
              lwqp3 = n+1
              ! Minimal Workspace Length For Stdlib_Zunmqr To Build Left Singular Vectors
              if ( wntus .or. wntur ) then
                  lwunq  = max( n  , 1_ilp )
              else if ( wntua ) then
                  lwunq = max( m , 1_ilp )
              end if
              ! Minimal Workspace Length For Stdlib_Zpocon Of An N X N Matrix
              lwcon = 2_ilp * n
              ! Stdlib_Zgesvd Of An N X N Matrix
              lwsvd = max( 3_ilp * n, 1_ilp )
              if ( lquery ) then
                  call stdlib_zgeqp3( m, n, a, lda, iwork, cdummy, cdummy, -1_ilp,rdummy, ierr )
                            
                  lwrk_zgeqp3 = int( cdummy(1_ilp),KIND=ilp)
                  if ( wntus .or. wntur ) then
                      call stdlib_zunmqr( 'L', 'N', m, n, n, a, lda, cdummy, u,ldu, cdummy, -1_ilp, &
                                ierr )
                      lwrk_zunmqr = int( cdummy(1_ilp),KIND=ilp)
                  else if ( wntua ) then
                      call stdlib_zunmqr( 'L', 'N', m, m, n, a, lda, cdummy, u,ldu, cdummy, -1_ilp, &
                                ierr )
                      lwrk_zunmqr = int( cdummy(1_ilp),KIND=ilp)
                  else
                      lwrk_zunmqr = 0_ilp
                  end if
              end if
              minwrk = 2_ilp
              optwrk = 2_ilp
              if ( .not. (lsvec .or. rsvec ) ) then
                  ! Minimal And Optimal Sizes Of The Complex Workspace If
                  ! only the singular values are requested
                  if ( conda ) then
                     minwrk = max( n+lwqp3, lwcon, lwsvd )
                  else
                     minwrk = max( n+lwqp3, lwsvd )
                  end if
                  if ( lquery ) then
                      call stdlib_zgesvd( 'N', 'N', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -1_ilp, &
                                rdummy, ierr )
                      lwrk_zgesvd = int( cdummy(1_ilp),KIND=ilp)
                      if ( conda ) then
                         optwrk = max( n+lwrk_zgeqp3, n+lwcon, lwrk_zgesvd )
                      else
                         optwrk = max( n+lwrk_zgeqp3, lwrk_zgesvd )
                      end if
                  end if
              else if ( lsvec .and. (.not.rsvec) ) then
                  ! Minimal And Optimal Sizes Of The Complex Workspace If The
                  ! singular values and the left singular vectors are requested
                  if ( conda ) then
                      minwrk = n + max( lwqp3, lwcon, lwsvd, lwunq )
                  else
                      minwrk = n + max( lwqp3, lwsvd, lwunq )
                  end if
                  if ( lquery ) then
                     if ( rtrans ) then
                        call stdlib_zgesvd( 'N', 'O', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -1_ilp, &
                                  rdummy, ierr )
                     else
                        call stdlib_zgesvd( 'O', 'N', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -1_ilp, &
                                  rdummy, ierr )
                     end if
                     lwrk_zgesvd = int( cdummy(1_ilp),KIND=ilp)
                     if ( conda ) then
                         optwrk = n + max( lwrk_zgeqp3, lwcon, lwrk_zgesvd,lwrk_zunmqr )
                     else
                         optwrk = n + max( lwrk_zgeqp3, lwrk_zgesvd,lwrk_zunmqr )
                     end if
                  end if
              else if ( rsvec .and. (.not.lsvec) ) then
                  ! Minimal And Optimal Sizes Of The Complex Workspace If The
                  ! singular values and the right singular vectors are requested
                  if ( conda ) then
                      minwrk = n + max( lwqp3, lwcon, lwsvd )
                  else
                      minwrk = n + max( lwqp3, lwsvd )
                  end if
                  if ( lquery ) then
                      if ( rtrans ) then
                          call stdlib_zgesvd( 'O', 'N', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -&
                                    1_ilp, rdummy, ierr )
                      else
                          call stdlib_zgesvd( 'N', 'O', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -&
                                    1_ilp, rdummy, ierr )
                      end if
                      lwrk_zgesvd = int( cdummy(1_ilp),KIND=ilp)
                      if ( conda ) then
                          optwrk = n + max( lwrk_zgeqp3, lwcon, lwrk_zgesvd )
                      else
                          optwrk = n + max( lwrk_zgeqp3, lwrk_zgesvd )
                      end if
                  end if
              else
                  ! Minimal And Optimal Sizes Of The Complex Workspace If The
                  ! full svd is requested
                  if ( rtrans ) then
                      minwrk = max( lwqp3, lwsvd, lwunq )
                      if ( conda ) minwrk = max( minwrk, lwcon )
                      minwrk = minwrk + n
                      if ( wntva ) then
                         ! .. minimal workspace length for n x n/2 stdlib_zgeqrf
                         lwqrf  = max( n/2_ilp, 1_ilp )
                         ! .. minimal workspace length for n/2 x n/2 stdlib_zgesvd
                         lwsvd2 = max( 3_ilp * (n/2_ilp), 1_ilp )
                         lwunq2 = max( n, 1_ilp )
                         minwrk2 = max( lwqp3, n/2_ilp+lwqrf, n/2_ilp+lwsvd2,n/2_ilp+lwunq2, lwunq )
                         if ( conda ) minwrk2 = max( minwrk2, lwcon )
                         minwrk2 = n + minwrk2
                         minwrk = max( minwrk, minwrk2 )
                      end if
                  else
                      minwrk = max( lwqp3, lwsvd, lwunq )
                      if ( conda ) minwrk = max( minwrk, lwcon )
                      minwrk = minwrk + n
                      if ( wntva ) then
                         ! .. minimal workspace length for n/2 x n stdlib_zgelqf
                         lwlqf  = max( n/2_ilp, 1_ilp )
                         lwsvd2 = max( 3_ilp * (n/2_ilp), 1_ilp )
                         lwunlq = max( n , 1_ilp )
                         minwrk2 = max( lwqp3, n/2_ilp+lwlqf, n/2_ilp+lwsvd2,n/2_ilp+lwunlq, lwunq )
                         if ( conda ) minwrk2 = max( minwrk2, lwcon )
                         minwrk2 = n + minwrk2
                         minwrk = max( minwrk, minwrk2 )
                      end if
                  end if
                  if ( lquery ) then
                     if ( rtrans ) then
                        call stdlib_zgesvd( 'O', 'A', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -1_ilp, &
                                  rdummy, ierr )
                        lwrk_zgesvd = int( cdummy(1_ilp),KIND=ilp)
                        optwrk = max(lwrk_zgeqp3,lwrk_zgesvd,lwrk_zunmqr)
                        if ( conda ) optwrk = max( optwrk, lwcon )
                        optwrk = n + optwrk
                        if ( wntva ) then
                            call stdlib_zgeqrf(n,n/2_ilp,u,ldu,cdummy,cdummy,-1_ilp,ierr)
                            lwrk_zgeqrf = int( cdummy(1_ilp),KIND=ilp)
                            call stdlib_zgesvd( 'S', 'O', n/2_ilp,n/2_ilp, v,ldv, s, u,ldu,v, ldv, cdummy,&
                                       -1_ilp, rdummy, ierr )
                            lwrk_zgesvd2 = int( cdummy(1_ilp),KIND=ilp)
                            call stdlib_zunmqr( 'R', 'C', n, n, n/2_ilp, u, ldu, cdummy,v, ldv, &
                                      cdummy, -1_ilp, ierr )
                            lwrk_zunmqr2 = int( cdummy(1_ilp),KIND=ilp)
                            optwrk2 = max( lwrk_zgeqp3, n/2_ilp+lwrk_zgeqrf,n/2_ilp+lwrk_zgesvd2, n/2_ilp+&
                                      lwrk_zunmqr2 )
                            if ( conda ) optwrk2 = max( optwrk2, lwcon )
                            optwrk2 = n + optwrk2
                            optwrk = max( optwrk, optwrk2 )
                        end if
                     else
                        call stdlib_zgesvd( 'S', 'O', n, n, a, lda, s, u, ldu,v, ldv, cdummy, -1_ilp, &
                                  rdummy, ierr )
                        lwrk_zgesvd = int( cdummy(1_ilp),KIND=ilp)
                        optwrk = max(lwrk_zgeqp3,lwrk_zgesvd,lwrk_zunmqr)
                        if ( conda ) optwrk = max( optwrk, lwcon )
                        optwrk = n + optwrk
                        if ( wntva ) then
                           call stdlib_zgelqf(n/2_ilp,n,u,ldu,cdummy,cdummy,-1_ilp,ierr)
                           lwrk_zgelqf = int( cdummy(1_ilp),KIND=ilp)
                           call stdlib_zgesvd( 'S','O', n/2_ilp,n/2_ilp, v, ldv, s, u, ldu,v, ldv, cdummy,&
                                      -1_ilp, rdummy, ierr )
                           lwrk_zgesvd2 = int( cdummy(1_ilp),KIND=ilp)
                           call stdlib_zunmlq( 'R', 'N', n, n, n/2_ilp, u, ldu, cdummy,v, ldv, cdummy,&
                                     -1_ilp,ierr )
                           lwrk_zunmlq = int( cdummy(1_ilp),KIND=ilp)
                           optwrk2 = max( lwrk_zgeqp3, n/2_ilp+lwrk_zgelqf,n/2_ilp+lwrk_zgesvd2, n/2_ilp+&
                                     lwrk_zunmlq )
                            if ( conda ) optwrk2 = max( optwrk2, lwcon )
                            optwrk2 = n + optwrk2
                            optwrk = max( optwrk, optwrk2 )
                        end if
                     end if
                  end if
              end if
              minwrk = max( 2_ilp, minwrk )
              optwrk = max( 2_ilp, optwrk )
              if ( lcwork < minwrk .and. (.not.lquery) ) info = -19_ilp
           end if
           if (info == 0_ilp .and. lrwork < rminwrk .and. .not. lquery) then
              info = -21_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGESVDQ', -info )
              return
           else if ( lquery ) then
           ! return optimal workspace
               iwork(1_ilp) = iminwrk
               cwork(1_ilp) = optwrk
               cwork(2_ilp) = minwrk
               rwork(1_ilp) = rminwrk
               return
           end if
           ! quick return if the matrix is void.
           if( ( m==0_ilp ) .or. ( n==0_ilp ) ) then
           ! All Output Is Void
              return
           end if
           big = stdlib_dlamch('O')
           ascaled = .false.
           if ( rowprm ) then
                 ! Reordering The Rows In Decreasing Sequence In The
                 ! ell-infinity norm - this enhances numerical robustness in
                 ! the case of differently scaled rows.
                 do p = 1, m
                     ! rwork(p) = abs( a(p,stdlib_izamax(n,a(p,1),lda)) )
                     ! [[stdlib_zlange will return nan if an entry of the p-th row is nan]]
                     rwork(p) = stdlib_zlange( 'M', 1_ilp, n, a(p,1_ilp), lda, rdummy )
                     ! .. check for nan's and inf's
                     if ( ( rwork(p) /= rwork(p) ) .or.( (rwork(p)*zero) /= zero ) ) then
                         info = -8_ilp
                         call stdlib_xerbla( 'ZGESVDQ', -info )
                         return
                     end if
                 end do
                 do p = 1, m - 1
                 q = stdlib_idamax( m-p+1, rwork(p), 1_ilp ) + p - 1_ilp
                 iwork(n+p) = q
                 if ( p /= q ) then
                    rtmp     = rwork(p)
                    rwork(p) = rwork(q)
                    rwork(q) = rtmp
                 end if
                 end do
                 if ( rwork(1_ilp) == zero ) then
                    ! quick return: a is the m x n zero matrix.
                    numrank = 0_ilp
                    call stdlib_dlaset( 'G', n, 1_ilp, zero, zero, s, n )
                    if ( wntus ) call stdlib_zlaset('G', m, n, czero, cone, u, ldu)
                    if ( wntua ) call stdlib_zlaset('G', m, m, czero, cone, u, ldu)
                    if ( wntva ) call stdlib_zlaset('G', n, n, czero, cone, v, ldv)
                    if ( wntuf ) then
                        call stdlib_zlaset( 'G', n, 1_ilp, czero, czero, cwork, n )
                        call stdlib_zlaset( 'G', m, n, czero, cone, u, ldu )
                    end if
                    do p = 1, n
                        iwork(p) = p
                    end do
                    if ( rowprm ) then
                        do p = n + 1, n + m - 1
                            iwork(p) = p - n
                        end do
                    end if
                    if ( conda ) rwork(1_ilp) = -1_ilp
                    rwork(2_ilp) = -1_ilp
                    return
                 end if
                 if ( rwork(1_ilp) > big / sqrt(real(m,KIND=dp)) ) then
                     ! .. to prevent overflow in the qr factorization, scale the
                     ! matrix by 1/sqrt(m) if too large entry detected
                     call stdlib_zlascl('G',0_ilp,0_ilp,sqrt(real(m,KIND=dp)),one, m,n, a,lda, ierr)
                               
                     ascaled = .true.
                 end if
                 call stdlib_zlaswp( n, a, lda, 1_ilp, m-1, iwork(n+1), 1_ilp )
           end if
          ! .. at this stage, preemptive scaling is done only to avoid column
          ! norms overflows during the qr factorization. the svd procedure should
          ! have its own scaling to save the singular values from overflows and
          ! underflows. that depends on the svd procedure.
           if ( .not.rowprm ) then
               rtmp = stdlib_zlange( 'M', m, n, a, lda, rwork )
               if ( ( rtmp /= rtmp ) .or.( (rtmp*zero) /= zero ) ) then
                    info = -8_ilp
                    call stdlib_xerbla( 'ZGESVDQ', -info )
                    return
               end if
               if ( rtmp > big / sqrt(real(m,KIND=dp)) ) then
                   ! .. to prevent overflow in the qr factorization, scale the
                   ! matrix by 1/sqrt(m) if too large entry detected
                   call stdlib_zlascl('G',0_ilp,0_ilp, sqrt(real(m,KIND=dp)),one, m,n, a,lda, ierr)
                             
                   ascaled = .true.
               end if
           end if
           ! Qr Factorization With Column Pivoting
           ! a * p = q * [ r ]
                       ! [ 0 ]
           do p = 1, n
              ! All Columns Are Free Columns
              iwork(p) = 0_ilp
           end do
           call stdlib_zgeqp3( m, n, a, lda, iwork, cwork, cwork(n+1), lcwork-n,rwork, ierr )
                     
          ! if the user requested accuracy level allows truncation in the
          ! computed upper triangular factor, the matrix r is examined and,
          ! if possible, replaced with its leading upper trapezoidal part.
           epsln = stdlib_dlamch('E')
           sfmin = stdlib_dlamch('S')
           ! small = sfmin / epsln
           nr = n
           if ( accla ) then
              ! standard absolute error bound suffices. all sigma_i with
              ! sigma_i < n*eps*||a||_f are flushed to zero. this is an
              ! aggressive enforcement of lower numerical rank by introducing a
              ! backward error of the order of n*eps*||a||_f.
              nr = 1_ilp
              rtmp = sqrt(real(n,KIND=dp))*epsln
              loop_3002: do p = 2, n
                 if ( abs(a(p,p)) < (rtmp*abs(a(1,1))) ) exit loop_3002
                    nr = nr + 1_ilp
              end do loop_3002
           elseif ( acclm ) then
              ! .. similarly as above, only slightly more gentle (less aggressive).
              ! sudden drop on the diagonal of r is used as the criterion for being
              ! close-to-rank-deficient. the threshold is set to epsln=stdlib_dlamch('e').
              ! [[this can be made more flexible by replacing this hard-coded value
              ! with a user specified threshold.]] also, the values that underflow
              ! will be truncated.
              nr = 1_ilp
              loop_3402: do p = 2, n
                 if ( ( abs(a(p,p)) < (epsln*abs(a(p-1,p-1))) ) .or.( abs(a(p,p)) < sfmin ) ) exit loop_3402
                 nr = nr + 1_ilp
              end do loop_3402
           else
              ! Rrqr Not Authorized To Determine Numerical Rank Except In The
              ! obvious case of zero pivots.
              ! .. inspect r for exact zeros on the diagonal;
              ! r(i,i)=0 => r(i:n,i:n)=0.
              nr = 1_ilp
              loop_3502: do p = 2, n
                 if ( abs(a(p,p)) == zero ) exit loop_3502
                 nr = nr + 1_ilp
              end do loop_3502
              if ( conda ) then
                 ! estimate the scaled condition number of a. use the fact that it is
                 ! the same as the scaled condition number of r.
                    ! V Is Used As Workspace
                    call stdlib_zlacpy( 'U', n, n, a, lda, v, ldv )
                    ! only the leading nr x nr submatrix of the triangular factor
                    ! is considered. only if nr=n will this give a reliable error
                    ! bound. however, even for nr < n, this can be used on an
                    ! expert level and obtain useful information in the sense of
                    ! perturbation theory.
                    do p = 1, nr
                       rtmp = stdlib_dznrm2( p, v(1_ilp,p), 1_ilp )
                       call stdlib_zdscal( p, one/rtmp, v(1_ilp,p), 1_ilp )
                    end do
                    if ( .not. ( lsvec .or. rsvec ) ) then
                        call stdlib_zpocon( 'U', nr, v, ldv, one, rtmp,cwork, rwork, ierr )
                                  
                    else
                        call stdlib_zpocon( 'U', nr, v, ldv, one, rtmp,cwork(n+1), rwork, ierr )
                                  
                    end if
                    sconda = one / sqrt(rtmp)
                 ! for nr=n, sconda is an estimate of sqrt(||(r^* * r)^(-1)||_1),
                 ! n^(-1/4) * sconda <= ||r^(-1)||_2 <= n^(1/4) * sconda
                 ! see the reference [1] for more details.
              end if
           endif
           if ( wntur ) then
               n1 = nr
           else if ( wntus .or. wntuf) then
               n1 = n
           else if ( wntua ) then
               n1 = m
           end if
           if ( .not. ( rsvec .or. lsvec ) ) then
      ! .......................................................................
              ! Only The Singular Values Are Requested
      ! .......................................................................
              if ( rtrans ) then
               ! .. compute the singular values of r**h = [a](1:nr,1:n)**h
                 ! .. set the lower triangle of [a] to [a](1:nr,1:n)**h and
                 ! the upper triangle of [a] to zero.
                 do p = 1, min( n, nr )
                    a(p,p) = conjg(a(p,p))
                    do q = p + 1, n
                       a(q,p) = conjg(a(p,q))
                       if ( q <= nr ) a(p,q) = czero
                    end do
                 end do
                 call stdlib_zgesvd( 'N', 'N', n, nr, a, lda, s, u, ldu,v, ldv, cwork, lcwork, &
                           rwork, info )
              else
                 ! .. compute the singular values of r = [a](1:nr,1:n)
                 if ( nr > 1_ilp )call stdlib_zlaset( 'L', nr-1,nr-1, czero,czero, a(2_ilp,1_ilp), lda )
                           
                 call stdlib_zgesvd( 'N', 'N', nr, n, a, lda, s, u, ldu,v, ldv, cwork, lcwork, &
                           rwork, info )
              end if
           else if ( lsvec .and. ( .not. rsvec) ) then
      ! .......................................................................
             ! The Singular Values And The Left Singular Vectors Requested
      ! .......................................................................""""""""
              if ( rtrans ) then
                  ! .. apply stdlib_zgesvd to r**h
                  ! .. copy r**h into [u] and overwrite [u] with the right singular
                  ! vectors of r
                 do p = 1, nr
                    do q = p, n
                       u(q,p) = conjg(a(p,q))
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_zlaset( 'U', nr-1,nr-1, czero,czero, u(1_ilp,2_ilp), ldu )
                           
                 ! .. the left singular vectors not computed, the nr right singular
                 ! vectors overwrite [u](1:nr,1:nr) as conjugate transposed. these
                 ! will be pre-multiplied by q to build the left singular vectors of a.
                    call stdlib_zgesvd( 'N', 'O', n, nr, u, ldu, s, u, ldu,u, ldu, cwork(n+1), &
                              lcwork-n, rwork, info )
                    do p = 1, nr
                        u(p,p) = conjg(u(p,p))
                        do q = p + 1, nr
                           ctmp   = conjg(u(q,p))
                           u(q,p) = conjg(u(p,q))
                           u(p,q) = ctmp
                        end do
                    end do
              else
                  ! Apply Stdlib_Zgesvd To R
                  ! .. copy r into [u] and overwrite [u] with the left singular vectors
                  call stdlib_zlacpy( 'U', nr, n, a, lda, u, ldu )
                  if ( nr > 1_ilp )call stdlib_zlaset( 'L', nr-1, nr-1, czero, czero, u(2_ilp,1_ilp), ldu )
                            
                  ! .. the right singular vectors not computed, the nr left singular
                  ! vectors overwrite [u](1:nr,1:nr)
                     call stdlib_zgesvd( 'O', 'N', nr, n, u, ldu, s, u, ldu,v, ldv, cwork(n+1), &
                               lcwork-n, rwork, info )
                     ! .. now [u](1:nr,1:nr) contains the nr left singular vectors of
                     ! r. these will be pre-multiplied by q to build the left singular
                     ! vectors of a.
              end if
                 ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
              if ( ( nr < m ) .and. ( .not.wntuf ) ) then
                  call stdlib_zlaset('A', m-nr, nr, czero, czero, u(nr+1,1_ilp), ldu)
                  if ( nr < n1 ) then
                     call stdlib_zlaset( 'A',nr,n1-nr,czero,czero,u(1_ilp,nr+1), ldu )
                     call stdlib_zlaset( 'A',m-nr,n1-nr,czero,cone,u(nr+1,nr+1), ldu )
                  end if
              end if
                 ! the q matrix from the first qrf is built into the left singular
                 ! vectors matrix u.
              if ( .not.wntuf )call stdlib_zunmqr( 'L', 'N', m, n1, n, a, lda, cwork, u,ldu, &
                        cwork(n+1), lcwork-n, ierr )
              if ( rowprm .and. .not.wntuf )call stdlib_zlaswp( n1, u, ldu, 1_ilp, m-1, iwork(n+1), -&
                        1_ilp )
           else if ( rsvec .and. ( .not. lsvec ) ) then
      ! .......................................................................
             ! The Singular Values And The Right Singular Vectors Requested
      ! .......................................................................
               if ( rtrans ) then
                  ! .. apply stdlib_zgesvd to r**h
                  ! .. copy r**h into v and overwrite v with the left singular vectors
                 do p = 1, nr
                    do q = p, n
                       v(q,p) = conjg(a(p,q))
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_zlaset( 'U', nr-1,nr-1, czero,czero, v(1_ilp,2_ilp), ldv )
                           
                 ! .. the left singular vectors of r**h overwrite v, the right singular
                 ! vectors not computed
                 if ( wntvr .or. ( nr == n ) ) then
                    call stdlib_zgesvd( 'O', 'N', n, nr, v, ldv, s, u, ldu,u, ldu, cwork(n+1), &
                              lcwork-n, rwork, info )
                    do p = 1, nr
                        v(p,p) = conjg(v(p,p))
                        do q = p + 1, nr
                           ctmp   = conjg(v(q,p))
                           v(q,p) = conjg(v(p,q))
                           v(p,q) = ctmp
                        end do
                    end do
                    if ( nr < n ) then
                        do p = 1, nr
                           do q = nr + 1, n
                               v(p,q) = conjg(v(q,p))
                           end do
                        end do
                    end if
                    call stdlib_zlapmt( .false., nr, n, v, ldv, iwork )
                 else
                     ! .. need all n right singular vectors and nr < n
                     ! [!] this is simple implementation that augments [v](1:n,1:nr)
                     ! by padding a zero block. in the case nr << n, a more efficient
                     ! way is to first use the qr factorization. for more details
                     ! how to implement this, see the " full svd " branch.
                     call stdlib_zlaset('G', n, n-nr, czero, czero, v(1_ilp,nr+1), ldv)
                     call stdlib_zgesvd( 'O', 'N', n, n, v, ldv, s, u, ldu,u, ldu, cwork(n+1), &
                               lcwork-n, rwork, info )
                     do p = 1, n
                        v(p,p) = conjg(v(p,p))
                        do q = p + 1, n
                           ctmp   = conjg(v(q,p))
                           v(q,p) = conjg(v(p,q))
                           v(p,q) = ctmp
                        end do
                     end do
                     call stdlib_zlapmt( .false., n, n, v, ldv, iwork )
                 end if
               else
                  ! Aply Stdlib_Zgesvd To R
                  ! Copy R Into V And Overwrite V With The Right Singular Vectors
                  call stdlib_zlacpy( 'U', nr, n, a, lda, v, ldv )
                  if ( nr > 1_ilp )call stdlib_zlaset( 'L', nr-1, nr-1, czero, czero, v(2_ilp,1_ilp), ldv )
                            
                  ! .. the right singular vectors overwrite v, the nr left singular
                  ! vectors stored in u(1:nr,1:nr)
                  if ( wntvr .or. ( nr == n ) ) then
                     call stdlib_zgesvd( 'N', 'O', nr, n, v, ldv, s, u, ldu,v, ldv, cwork(n+1), &
                               lcwork-n, rwork, info )
                     call stdlib_zlapmt( .false., nr, n, v, ldv, iwork )
                     ! .. now [v](1:nr,1:n) contains v(1:n,1:nr)**h
                  else
                     ! .. need all n right singular vectors and nr < n
                     ! [!] this is simple implementation that augments [v](1:nr,1:n)
                     ! by padding a zero block. in the case nr << n, a more efficient
                     ! way is to first use the lq factorization. for more details
                     ! how to implement this, see the " full svd " branch.
                      call stdlib_zlaset('G', n-nr, n, czero,czero, v(nr+1,1_ilp), ldv)
                      call stdlib_zgesvd( 'N', 'O', n, n, v, ldv, s, u, ldu,v, ldv, cwork(n+1), &
                                lcwork-n, rwork, info )
                      call stdlib_zlapmt( .false., n, n, v, ldv, iwork )
                  end if
                  ! .. now [v] contains the adjoint of the matrix of the right singular
                  ! vectors of a.
               end if
           else
      ! .......................................................................
             ! Full Svd Requested
      ! .......................................................................
              if ( rtrans ) then
                  ! .. apply stdlib_zgesvd to r**h [[this option is left for r
                 if ( wntvr .or. ( nr == n ) ) then
                  ! .. copy r**h into [v] and overwrite [v] with the left singular
                  ! vectors of r**h
                 do p = 1, nr
                    do q = p, n
                       v(q,p) = conjg(a(p,q))
                    end do
                 end do
                 if ( nr > 1_ilp )call stdlib_zlaset( 'U', nr-1,nr-1, czero,czero, v(1_ilp,2_ilp), ldv )
                           
                 ! .. the left singular vectors of r**h overwrite [v], the nr right
                 ! singular vectors of r**h stored in [u](1:nr,1:nr) as conjugate
                 ! transposed
                    call stdlib_zgesvd( 'O', 'A', n, nr, v, ldv, s, v, ldv,u, ldu, cwork(n+1), &
                              lcwork-n, rwork, info )
                    ! Assemble V
                    do p = 1, nr
                       v(p,p) = conjg(v(p,p))
                       do q = p + 1, nr
                          ctmp   = conjg(v(q,p))
                          v(q,p) = conjg(v(p,q))
                          v(p,q) = ctmp
                       end do
                    end do
                    if ( nr < n ) then
                        do p = 1, nr
                           do q = nr+1, n
                              v(p,q) = conjg(v(q,p))
                           end do
                        end do
                    end if
                    call stdlib_zlapmt( .false., nr, n, v, ldv, iwork )
                     do p = 1, nr
                        u(p,p) = conjg(u(p,p))
                        do q = p + 1, nr
                           ctmp   = conjg(u(q,p))
                           u(q,p) = conjg(u(p,q))
                           u(p,q) = ctmp
                        end do
                     end do
                     if ( ( nr < m ) .and. .not.(wntuf)) then
                       call stdlib_zlaset('A', m-nr,nr, czero,czero, u(nr+1,1_ilp), ldu)
                       if ( nr < n1 ) then
                          call stdlib_zlaset('A',nr,n1-nr,czero,czero,u(1_ilp,nr+1),ldu)
                          call stdlib_zlaset( 'A',m-nr,n1-nr,czero,cone,u(nr+1,nr+1), ldu )
                                    
                       end if
                    end if
                 else
                     ! .. need all n right singular vectors and nr < n
                  ! .. copy r**h into [v] and overwrite [v] with the left singular
                  ! vectors of r**h
                     ! [[the optimal ratio n/nr for using qrf instead of padding
                       ! with zeros. here hard coded to 2; it must be at least
                       ! two due to work space constraints.]]
                     ! optratio = stdlib_ilaenv(6, 'zgesvd', 's' // 'o', nr,n,0,0)
                     ! optratio = max( optratio, 2 )
                     optratio = 2_ilp
                     if ( optratio*nr > n ) then
                        do p = 1, nr
                           do q = p, n
                              v(q,p) = conjg(a(p,q))
                           end do
                        end do
                        if ( nr > 1_ilp )call stdlib_zlaset('U',nr-1,nr-1, czero,czero, v(1_ilp,2_ilp),ldv)
                                  
                        call stdlib_zlaset('A',n,n-nr,czero,czero,v(1_ilp,nr+1),ldv)
                        call stdlib_zgesvd( 'O', 'A', n, n, v, ldv, s, v, ldv,u, ldu, cwork(n+1), &
                                  lcwork-n, rwork, info )
                        do p = 1, n
                           v(p,p) = conjg(v(p,p))
                           do q = p + 1, n
                              ctmp   = conjg(v(q,p))
                              v(q,p) = conjg(v(p,q))
                              v(p,q) = ctmp
                           end do
                        end do
                        call stdlib_zlapmt( .false., n, n, v, ldv, iwork )
                    ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x n1), i.e. (m x n) or (m x m).
                        do p = 1, n
                           u(p,p) = conjg(u(p,p))
                           do q = p + 1, n
                              ctmp   = conjg(u(q,p))
                              u(q,p) = conjg(u(p,q))
                              u(p,q) = ctmp
                           end do
                        end do
                        if ( ( n < m ) .and. .not.(wntuf)) then
                           call stdlib_zlaset('A',m-n,n,czero,czero,u(n+1,1_ilp),ldu)
                           if ( n < n1 ) then
                             call stdlib_zlaset('A',n,n1-n,czero,czero,u(1_ilp,n+1),ldu)
                             call stdlib_zlaset('A',m-n,n1-n,czero,cone,u(n+1,n+1), ldu )
                           end if
                        end if
                     else
                        ! .. copy r**h into [u] and overwrite [u] with the right
                        ! singular vectors of r
                        do p = 1, nr
                           do q = p, n
                              u(q,nr+p) = conjg(a(p,q))
                           end do
                        end do
                        if ( nr > 1_ilp )call stdlib_zlaset('U',nr-1,nr-1,czero,czero,u(1_ilp,nr+2),ldu)
                                  
                        call stdlib_zgeqrf( n, nr, u(1_ilp,nr+1), ldu, cwork(n+1),cwork(n+nr+1), &
                                  lcwork-n-nr, ierr )
                        do p = 1, nr
                            do q = 1, n
                                v(q,p) = conjg(u(p,nr+q))
                            end do
                        end do
                       if (nr>1_ilp) call stdlib_zlaset('U',nr-1,nr-1,czero,czero,v(1_ilp,2_ilp),ldv)
                       call stdlib_zgesvd( 'S', 'O', nr, nr, v, ldv, s, u, ldu,v,ldv, cwork(n+nr+&
                                 1_ilp),lcwork-n-nr,rwork, info )
                       call stdlib_zlaset('A',n-nr,nr,czero,czero,v(nr+1,1_ilp),ldv)
                       call stdlib_zlaset('A',nr,n-nr,czero,czero,v(1_ilp,nr+1),ldv)
                       call stdlib_zlaset('A',n-nr,n-nr,czero,cone,v(nr+1,nr+1),ldv)
                       call stdlib_zunmqr('R','C', n, n, nr, u(1_ilp,nr+1), ldu,cwork(n+1),v,ldv,&
                                 cwork(n+nr+1),lcwork-n-nr,ierr)
                       call stdlib_zlapmt( .false., n, n, v, ldv, iwork )
                       ! Assemble The Left Singular Vector Matrix U Of Dimensions
                       ! (m x nr) or (m x n) or (m x m).
                       if ( ( nr < m ) .and. .not.(wntuf)) then
                          call stdlib_zlaset('A',m-nr,nr,czero,czero,u(nr+1,1_ilp),ldu)
                          if ( nr < n1 ) then
                          call stdlib_zlaset('A',nr,n1-nr,czero,czero,u(1_ilp,nr+1),ldu)
                          call stdlib_zlaset( 'A',m-nr,n1-nr,czero,cone,u(nr+1,nr+1),ldu)
                          end if
                       end if
                     end if
                 end if
              else
                  ! .. apply stdlib_zgesvd to r [[this is the recommended option]]
                  if ( wntvr .or. ( nr == n ) ) then
                      ! .. copy r into [v] and overwrite v with the right singular vectors
                      call stdlib_zlacpy( 'U', nr, n, a, lda, v, ldv )
                     if ( nr > 1_ilp )call stdlib_zlaset( 'L', nr-1,nr-1, czero,czero, v(2_ilp,1_ilp), ldv )
                               
                     ! .. the right singular vectors of r overwrite [v], the nr left
                     ! singular vectors of r stored in [u](1:nr,1:nr)
                     call stdlib_zgesvd( 'S', 'O', nr, n, v, ldv, s, u, ldu,v, ldv, cwork(n+1), &
                               lcwork-n, rwork, info )
                     call stdlib_zlapmt( .false., nr, n, v, ldv, iwork )
                     ! .. now [v](1:nr,1:n) contains v(1:n,1:nr)**h
                     ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
                    if ( ( nr < m ) .and. .not.(wntuf)) then
                       call stdlib_zlaset('A', m-nr,nr, czero,czero, u(nr+1,1_ilp), ldu)
                       if ( nr < n1 ) then
                          call stdlib_zlaset('A',nr,n1-nr,czero,czero,u(1_ilp,nr+1),ldu)
                          call stdlib_zlaset( 'A',m-nr,n1-nr,czero,cone,u(nr+1,nr+1), ldu )
                                    
                       end if
                    end if
                  else
                    ! .. need all n right singular vectors and nr < n
                    ! The Requested Number Of The Left Singular Vectors
                     ! is then n1 (n or m)
                     ! [[the optimal ratio n/nr for using lq instead of padding
                       ! with zeros. here hard coded to 2; it must be at least
                       ! two due to work space constraints.]]
                     ! optratio = stdlib_ilaenv(6, 'zgesvd', 's' // 'o', nr,n,0,0)
                     ! optratio = max( optratio, 2 )
                    optratio = 2_ilp
                    if ( optratio * nr > n ) then
                       call stdlib_zlacpy( 'U', nr, n, a, lda, v, ldv )
                       if ( nr > 1_ilp )call stdlib_zlaset('L', nr-1,nr-1, czero,czero, v(2_ilp,1_ilp),ldv)
                                 
                    ! .. the right singular vectors of r overwrite [v], the nr left
                       ! singular vectors of r stored in [u](1:nr,1:nr)
                       call stdlib_zlaset('A', n-nr,n, czero,czero, v(nr+1,1_ilp),ldv)
                       call stdlib_zgesvd( 'S', 'O', n, n, v, ldv, s, u, ldu,v, ldv, cwork(n+1), &
                                 lcwork-n, rwork, info )
                       call stdlib_zlapmt( .false., n, n, v, ldv, iwork )
                       ! .. now [v] contains the adjoint of the matrix of the right
                       ! singular vectors of a. the leading n left singular vectors
                       ! are in [u](1:n,1:n)
                       ! Assemble The Left Singular Vector Matrix U Of Dimensions
                       ! (m x n1), i.e. (m x n) or (m x m).
                       if ( ( n < m ) .and. .not.(wntuf)) then
                           call stdlib_zlaset('A',m-n,n,czero,czero,u(n+1,1_ilp),ldu)
                           if ( n < n1 ) then
                             call stdlib_zlaset('A',n,n1-n,czero,czero,u(1_ilp,n+1),ldu)
                             call stdlib_zlaset( 'A',m-n,n1-n,czero,cone,u(n+1,n+1), ldu )
                           end if
                       end if
                    else
                       call stdlib_zlacpy( 'U', nr, n, a, lda, u(nr+1,1_ilp), ldu )
                       if ( nr > 1_ilp )call stdlib_zlaset('L',nr-1,nr-1,czero,czero,u(nr+2,1_ilp),ldu)
                                 
                       call stdlib_zgelqf( nr, n, u(nr+1,1_ilp), ldu, cwork(n+1),cwork(n+nr+1), &
                                 lcwork-n-nr, ierr )
                       call stdlib_zlacpy('L',nr,nr,u(nr+1,1_ilp),ldu,v,ldv)
                       if ( nr > 1_ilp )call stdlib_zlaset('U',nr-1,nr-1,czero,czero,v(1_ilp,2_ilp),ldv)
                                 
                       call stdlib_zgesvd( 'S', 'O', nr, nr, v, ldv, s, u, ldu,v, ldv, cwork(n+nr+&
                                 1_ilp), lcwork-n-nr, rwork, info )
                       call stdlib_zlaset('A',n-nr,nr,czero,czero,v(nr+1,1_ilp),ldv)
                       call stdlib_zlaset('A',nr,n-nr,czero,czero,v(1_ilp,nr+1),ldv)
                       call stdlib_zlaset('A',n-nr,n-nr,czero,cone,v(nr+1,nr+1),ldv)
                       call stdlib_zunmlq('R','N',n,n,nr,u(nr+1,1_ilp),ldu,cwork(n+1),v, ldv, cwork(n+&
                                 nr+1),lcwork-n-nr,ierr)
                       call stdlib_zlapmt( .false., n, n, v, ldv, iwork )
                     ! Assemble The Left Singular Vector Matrix U Of Dimensions
                    ! (m x nr) or (m x n) or (m x m).
                       if ( ( nr < m ) .and. .not.(wntuf)) then
                          call stdlib_zlaset('A',m-nr,nr,czero,czero,u(nr+1,1_ilp),ldu)
                          if ( nr < n1 ) then
                          call stdlib_zlaset('A',nr,n1-nr,czero,czero,u(1_ilp,nr+1),ldu)
                          call stdlib_zlaset( 'A',m-nr,n1-nr,czero,cone,u(nr+1,nr+1), ldu )
                                    
                          end if
                       end if
                    end if
                  end if
              ! .. end of the "r**h or r" branch
              end if
                 ! the q matrix from the first qrf is built into the left singular
                 ! vectors matrix u.
              if ( .not. wntuf )call stdlib_zunmqr( 'L', 'N', m, n1, n, a, lda, cwork, u,ldu, &
                        cwork(n+1), lcwork-n, ierr )
              if ( rowprm .and. .not.wntuf )call stdlib_zlaswp( n1, u, ldu, 1_ilp, m-1, iwork(n+1), -&
                        1_ilp )
           ! ... end of the "full svd" branch
           end if
           ! check whether some singular values are returned as zeros, e.g.
           ! due to underflow, and update the numerical rank.
           p = nr
           do q = p, 1, -1
               if ( s(q) > zero ) go to 4002
               nr = nr - 1_ilp
           end do
           4002 continue
           ! .. if numerical rank deficiency is detected, the truncated
           ! singular values are set to zero.
           if ( nr < n ) call stdlib_dlaset( 'G', n-nr,1_ilp, zero,zero, s(nr+1), n )
           ! .. undo scaling; this may cause overflow in the largest singular
           ! values.
           if ( ascaled )call stdlib_dlascl( 'G',0_ilp,0_ilp, one,sqrt(real(m,KIND=dp)), nr,1_ilp, s, n, ierr &
                     )
           if ( conda ) rwork(1_ilp) = sconda
           rwork(2_ilp) = p - nr
           ! .. p-nr is the number of singular values that are computed as
           ! exact zeros in stdlib_zgesvd() applied to the (possibly truncated)
           ! full row rank triangular (trapezoidal) factor of a.
           numrank = nr
           return
     end subroutine stdlib_zgesvdq



end submodule stdlib_lapack_eigv_svd_drivers
