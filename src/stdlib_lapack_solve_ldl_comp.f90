submodule(stdlib_lapack_solve) stdlib_lapack_solve_ldl_comp
  implicit none


  contains

     pure module subroutine stdlib_ssycon( uplo, n, a, lda, ipiv, anorm, rcond, work,iwork, info )
     !! SSYCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric matrix A using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by SSYTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, kase
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_ssytrs( uplo, n, 1_ilp, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_ssycon

     pure module subroutine stdlib_dsycon( uplo, n, a, lda, ipiv, anorm, rcond, work,iwork, info )
     !! DSYCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric matrix A using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by DSYTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, kase
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_dsytrs( uplo, n, 1_ilp, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_dsycon


     pure module subroutine stdlib_csycon( uplo, n, a, lda, ipiv, anorm, rcond, work,info )
     !! CSYCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex symmetric matrix A using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by CSYTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, kase
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_csytrs( uplo, n, 1_ilp, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_csycon

     pure module subroutine stdlib_zsycon( uplo, n, a, lda, ipiv, anorm, rcond, work,info )
     !! ZSYCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex symmetric matrix A using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by ZSYTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, kase
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_zsytrs( uplo, n, 1_ilp, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_zsycon




     pure module subroutine stdlib_ssytrf( uplo, n, a, lda, ipiv, work, lwork, info )
     !! SSYTRF computes the factorization of a real symmetric matrix A using
     !! the Bunch-Kaufman diagonal pivoting method.  The form of the
     !! factorization is
     !! A = U**T*D*U  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'SSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRF', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u**t*d*u using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_slasyf;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_slasyf( uplo, k, nb, kb, a, lda, ipiv, work, ldwork,iinfo )
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_ssytf2( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_slasyf;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_slasyf( uplo, n-k+1, nb, kb, a( k, k ), lda, ipiv( k ),work, ldwork, &
                           iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_ssytf2( uplo, n-k+1, a( k, k ), lda, ipiv( k ), iinfo )
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssytrf

     pure module subroutine stdlib_dsytrf( uplo, n, a, lda, ipiv, work, lwork, info )
     !! DSYTRF computes the factorization of a real symmetric matrix A using
     !! the Bunch-Kaufman diagonal pivoting method.  The form of the
     !! factorization is
     !! A = U**T*D*U  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'DSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRF', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u**t*d*u using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_dlasyf;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_dlasyf( uplo, k, nb, kb, a, lda, ipiv, work, ldwork,iinfo )
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_dsytf2( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_dlasyf;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_dlasyf( uplo, n-k+1, nb, kb, a( k, k ), lda, ipiv( k ),work, ldwork, &
                           iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_dsytf2( uplo, n-k+1, a( k, k ), lda, ipiv( k ), iinfo )
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsytrf


     pure module subroutine stdlib_csytrf( uplo, n, a, lda, ipiv, work, lwork, info )
     !! CSYTRF computes the factorization of a complex symmetric matrix A
     !! using the Bunch-Kaufman diagonal pivoting method.  The form of the
     !! factorization is
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'CSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTRF', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_clasyf;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_clasyf( uplo, k, nb, kb, a, lda, ipiv, work, n, iinfo )
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_csytf2( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_clasyf;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_clasyf( uplo, n-k+1, nb, kb, a( k, k ), lda, ipiv( k ),work, n, &
                           iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_csytf2( uplo, n-k+1, a( k, k ), lda, ipiv( k ), iinfo )
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_csytrf

     pure module subroutine stdlib_zsytrf( uplo, n, a, lda, ipiv, work, lwork, info )
     !! ZSYTRF computes the factorization of a complex symmetric matrix A
     !! using the Bunch-Kaufman diagonal pivoting method.  The form of the
     !! factorization is
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'ZSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTRF', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_zlasyf;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_zlasyf( uplo, k, nb, kb, a, lda, ipiv, work, n, iinfo )
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_zsytf2( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_zlasyf;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_zlasyf( uplo, n-k+1, nb, kb, a( k, k ), lda, ipiv( k ),work, n, &
                           iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_zsytf2( uplo, n-k+1, a( k, k ), lda, ipiv( k ), iinfo )
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zsytrf




     pure module subroutine stdlib_slasyf( uplo, n, nb, kb, a, lda, ipiv, w, ldw, info )
     !! SLASYF computes a partial factorization of a real symmetric matrix A
     !! using the Bunch-Kaufman diagonal pivoting method. The partial
     !! factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**T L21**T )  if UPLO = 'L'
     !! ( L21  I ) (  0  A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! SLASYF is an auxiliary routine called by SSYTRF. It uses blocked code
     !! (calling Level 3 BLAS) to update the submatrix A11 (if UPLO = 'U') or
     !! A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           ! Local Scalars 
           integer(ilp) :: imax, j, jb, jj, jmax, jp, k, kk, kkw, kp, kstep, kw
           real(sp) :: absakk, alpha, colmax, d11, d21, d22, r1, rowmax, t
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              ! kw is the column of w which corresponds to column k of a
              k = n
              10 continue
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              ! copy column k of a to column kw of w and update it
              call stdlib_scopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_sgemv( 'NO TRANSPOSE', k, n-k, -one, a( 1_ilp, k+1 ), lda,w( k, kw+&
                        1_ilp ), ldw, one, w( 1_ilp, kw ), 1_ilp )
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_isamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = abs( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! copy column imax to column kw-1 of w and update it
                    call stdlib_scopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                    call stdlib_scopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                              
                    if( k<n )call stdlib_sgemv( 'NO TRANSPOSE', k, n-k, -one, a( 1_ilp, k+1 ),lda, w( &
                              imax, kw+1 ), ldw, one,w( 1_ilp, kw-1 ), 1_ilp )
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = imax + stdlib_isamax( k-imax, w( imax+1, kw-1 ), 1_ilp )
                    rowmax = abs( w( jmax, kw-1 ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_isamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                       rowmax = max( rowmax, abs( w( jmax, kw-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( w( imax, kw-1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column kw-1 of w to column kw of w
                       call stdlib_scopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kkw of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k-1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = a( kk, kk )
                    call stdlib_scopy( kk-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    if( kp>1_ilp )call stdlib_scopy( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last k+1 to n columns of a
                    ! (columns k (or k and k-1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in last kkw to nb columns of w.
                    if( k<n )call stdlib_sswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                    call stdlib_sswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(kw) = u(k)*d(k),
                    ! where u(k) is the k-th column of u
                    ! store subdiag. elements of column u(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! note: diagonal element u(k,k) is a unit element
                    ! and not stored.
                       ! a(k,k) := d(k,k) = w(k,kw)
                       ! a(1:k-1,k) := u(1:k-1,k) = w(1:k-1,kw)/d(k,k)
                    call stdlib_scopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    r1 = one / a( k, k )
                    call stdlib_sscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now hold
                    ! ( w(kw-1) w(kw) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! store u(1:k-2,k-1) and u(1:k-2,k) and 2-by-2
                    ! block d(k-1:k,k-1:k) in columns k-1 and k of a.
                    ! note: 2-by-2 diagonal block u(k-1:k,k-1:k) is a unit
                    ! block and not stored.
                       ! a(k-1:k,k-1:k) := d(k-1:k,k-1:k) = w(k-1:k,kw-1:kw)
                       ! a(1:k-2,k-1:k) := u(1:k-2,k:k-1:k) =
                       ! = w(1:k-2,kw-1:kw) * ( d(k-1:k,k-1:k)**(-1) )
                    if( k>2_ilp ) then
                       ! compose the columns of the inverse of 2-by-2 pivot
                       ! block d in the following way to reduce the number
                       ! of flops when we myltiply panel ( w(kw-1) w(kw) ) by
                       ! this inverse
                       ! d**(-1) = ( d11 d21 )**(-1) =
                                 ! ( d21 d22 )
                       ! = 1/(d11*d22-d21**2) * ( ( d22 ) (-d21 ) ) =
                                              ! ( (-d21 ) ( d11 ) )
                       ! = 1/d21 * 1/((d11/d21)*(d22/d21)-1) *
                         ! * ( ( d22/d21 ) (      -1 ) ) =
                           ! ( (      -1 ) ( d11/d21 ) )
                       ! = 1/d21 * 1/(d22*d11-1) * ( ( d11 ) (  -1 ) ) =
                                                 ! ( ( -1  ) ( d22 ) )
                       ! = 1/d21 * t * ( ( d11 ) (  -1 ) )
                                     ! ( (  -1 ) ( d22 ) )
                       ! = d21 * ( ( d11 ) (  -1 ) )
                               ! ( (  -1 ) ( d22 ) )
                       d21 = w( k-1, kw )
                       d11 = w( k, kw ) / d21
                       d22 = w( k-1, kw-1 ) / d21
                       t = one / ( d11*d22-one )
                       d21 = t / d21
                       ! update elements in columns a(k-1) and a(k) as
                       ! dot products of rows of ( w(kw-1) w(kw) ) and columns
                       ! of d**(-1)
                       do j = 1, k - 2
                          a( j, k-1 ) = d21*( d11*w( j, kw-1 )-w( j, kw ) )
                          a( j, k ) = d21*( d22*w( j, kw )-w( j, kw-1 ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_sgemv( 'NO TRANSPOSE', jj-j+1, n-k, -one,a( j, k+1 ), lda, w( jj, &
                              kw+1 ), ldw, one,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb, n-k, -one,a( 1_ilp, k+1 ), &
                           lda, w( j, kw+1 ), ldw, one,a( 1_ilp, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in columns k+1:n looping backwards from k+1 to n
              j = k + 1_ilp
              60 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j + 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length n-j+1
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j + 1_ilp
                 if( jp/=jj .and. j<=n )call stdlib_sswap( n-j+1, a( jp, j ), lda, a( jj, j ), &
                           lda )
              if( j<n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              ! copy column k of a to column k of w and update it
              call stdlib_scopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              call stdlib_sgemv( 'NO TRANSPOSE', n-k+1, k-1, -one, a( k, 1_ilp ), lda,w( k, 1_ilp ), ldw, &
                        one, w( k, k ), 1_ilp )
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_isamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = abs( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! copy column imax to column k+1 of w and update it
                    call stdlib_scopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp )
                    call stdlib_scopy( n-imax+1, a( imax, imax ), 1_ilp, w( imax, k+1 ),1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', n-k+1, k-1, -one, a( k, 1_ilp ),lda, w( imax, &
                              1_ilp ), ldw, one, w( k, k+1 ), 1_ilp )
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = k - 1_ilp + stdlib_isamax( imax-k, w( k, k+1 ), 1_ilp )
                    rowmax = abs( w( jmax, k+1 ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_isamax( n-imax, w( imax+1, k+1 ), 1_ilp )
                       rowmax = max( rowmax, abs( w( jmax, k+1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( w( imax, k+1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column k+1 of w to column k of w
                       call stdlib_scopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k + kstep - 1_ilp
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kk of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k+1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = a( kk, kk )
                    call stdlib_scopy( kp-kk-1, a( kk+1, kk ), 1_ilp, a( kp, kk+1 ),lda )
                    if( kp<n )call stdlib_scopy( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    ! interchange rows kk and kp in first k-1 columns of a
                    ! (columns k (or k and k+1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in first kk columns of w.
                    if( k>1_ilp )call stdlib_sswap( k-1, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_sswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k),
                    ! where l(k) is the k-th column of l
                    ! store subdiag. elements of column l(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element l(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,k)
                       ! a(k+1:n,k) := l(k+1:n,k) = w(k+1:n,k)/d(k,k)
                    call stdlib_scopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       r1 = one / a( k, k )
                       call stdlib_sscal( n-k, r1, a( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! store l(k+2:n,k) and l(k+2:n,k+1) and 2-by-2
                    ! block d(k:k+1,k:k+1) in columns k and k+1 of a.
                    ! (note: 2-by-2 diagonal block l(k:k+1,k:k+1) is a unit
                    ! block and not stored)
                       ! a(k:k+1,k:k+1) := d(k:k+1,k:k+1) = w(k:k+1,k:k+1)
                       ! a(k+2:n,k:k+1) := l(k+2:n,k:k+1) =
                       ! = w(k+2:n,k:k+1) * ( d(k:k+1,k:k+1)**(-1) )
                    if( k<n-1 ) then
                       ! compose the columns of the inverse of 2-by-2 pivot
                       ! block d in the following way to reduce the number
                       ! of flops when we myltiply panel ( w(k) w(k+1) ) by
                       ! this inverse
                       ! d**(-1) = ( d11 d21 )**(-1) =
                                 ! ( d21 d22 )
                       ! = 1/(d11*d22-d21**2) * ( ( d22 ) (-d21 ) ) =
                                              ! ( (-d21 ) ( d11 ) )
                       ! = 1/d21 * 1/((d11/d21)*(d22/d21)-1) *
                         ! * ( ( d22/d21 ) (      -1 ) ) =
                           ! ( (      -1 ) ( d11/d21 ) )
                       ! = 1/d21 * 1/(d22*d11-1) * ( ( d11 ) (  -1 ) ) =
                                                 ! ( ( -1  ) ( d22 ) )
                       ! = 1/d21 * t * ( ( d11 ) (  -1 ) )
                                     ! ( (  -1 ) ( d22 ) )
                       ! = d21 * ( ( d11 ) (  -1 ) )
                               ! ( (  -1 ) ( d22 ) )
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = one / ( d11*d22-one )
                       d21 = t / d21
                       ! update elements in columns a(k) and a(k+1) as
                       ! dot products of rows of ( w(k) w(k+1) ) and columns
                       ! of d**(-1)
                       do j = k + 2, n
                          a( j, k ) = d21*( d11*w( j, k )-w( j, k+1 ) )
                          a( j, k+1 ) = d21*( d22*w( j, k+1 )-w( j, k ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_sgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -one,a( jj, 1_ilp ), lda, w( jj, &
                              1_ilp ), ldw, one,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           one, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ), ldw,one, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! of rows in columns 1:k-1 looping backwards from k-1 to 1
              j = k - 1_ilp
              120 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j - 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length j
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j - 1_ilp
                 if( jp/=jj .and. j>=1_ilp )call stdlib_sswap( j, a( jp, 1_ilp ), lda, a( jj, 1_ilp ), lda )
                           
              if( j>1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_slasyf

     pure module subroutine stdlib_dlasyf( uplo, n, nb, kb, a, lda, ipiv, w, ldw, info )
     !! DLASYF computes a partial factorization of a real symmetric matrix A
     !! using the Bunch-Kaufman diagonal pivoting method. The partial
     !! factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**T L21**T )  if UPLO = 'L'
     !! ( L21  I ) (  0  A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! DLASYF is an auxiliary routine called by DSYTRF. It uses blocked code
     !! (calling Level 3 BLAS) to update the submatrix A11 (if UPLO = 'U') or
     !! A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           ! Local Scalars 
           integer(ilp) :: imax, j, jb, jj, jmax, jp, k, kk, kkw, kp, kstep, kw
           real(dp) :: absakk, alpha, colmax, d11, d21, d22, r1, rowmax, t
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              ! kw is the column of w which corresponds to column k of a
              k = n
              10 continue
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              ! copy column k of a to column kw of w and update it
              call stdlib_dcopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_dgemv( 'NO TRANSPOSE', k, n-k, -one, a( 1_ilp, k+1 ), lda,w( k, kw+&
                        1_ilp ), ldw, one, w( 1_ilp, kw ), 1_ilp )
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_idamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = abs( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! copy column imax to column kw-1 of w and update it
                    call stdlib_dcopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                    call stdlib_dcopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                              
                    if( k<n )call stdlib_dgemv( 'NO TRANSPOSE', k, n-k, -one, a( 1_ilp, k+1 ),lda, w( &
                              imax, kw+1 ), ldw, one,w( 1_ilp, kw-1 ), 1_ilp )
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = imax + stdlib_idamax( k-imax, w( imax+1, kw-1 ), 1_ilp )
                    rowmax = abs( w( jmax, kw-1 ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_idamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                       rowmax = max( rowmax, abs( w( jmax, kw-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( w( imax, kw-1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column kw-1 of w to column kw of w
                       call stdlib_dcopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kkw of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k-1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = a( kk, kk )
                    call stdlib_dcopy( kk-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    if( kp>1_ilp )call stdlib_dcopy( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last k+1 to n columns of a
                    ! (columns k (or k and k-1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in last kkw to nb columns of w.
                    if( k<n )call stdlib_dswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                    call stdlib_dswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(kw) = u(k)*d(k),
                    ! where u(k) is the k-th column of u
                    ! store subdiag. elements of column u(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! note: diagonal element u(k,k) is a unit element
                    ! and not stored.
                       ! a(k,k) := d(k,k) = w(k,kw)
                       ! a(1:k-1,k) := u(1:k-1,k) = w(1:k-1,kw)/d(k,k)
                    call stdlib_dcopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    r1 = one / a( k, k )
                    call stdlib_dscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now hold
                    ! ( w(kw-1) w(kw) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! store u(1:k-2,k-1) and u(1:k-2,k) and 2-by-2
                    ! block d(k-1:k,k-1:k) in columns k-1 and k of a.
                    ! note: 2-by-2 diagonal block u(k-1:k,k-1:k) is a unit
                    ! block and not stored.
                       ! a(k-1:k,k-1:k) := d(k-1:k,k-1:k) = w(k-1:k,kw-1:kw)
                       ! a(1:k-2,k-1:k) := u(1:k-2,k:k-1:k) =
                       ! = w(1:k-2,kw-1:kw) * ( d(k-1:k,k-1:k)**(-1) )
                    if( k>2_ilp ) then
                       ! compose the columns of the inverse of 2-by-2 pivot
                       ! block d in the following way to reduce the number
                       ! of flops when we myltiply panel ( w(kw-1) w(kw) ) by
                       ! this inverse
                       ! d**(-1) = ( d11 d21 )**(-1) =
                                 ! ( d21 d22 )
                       ! = 1/(d11*d22-d21**2) * ( ( d22 ) (-d21 ) ) =
                                              ! ( (-d21 ) ( d11 ) )
                       ! = 1/d21 * 1/((d11/d21)*(d22/d21)-1) *
                         ! * ( ( d22/d21 ) (      -1 ) ) =
                           ! ( (      -1 ) ( d11/d21 ) )
                       ! = 1/d21 * 1/(d22*d11-1) * ( ( d11 ) (  -1 ) ) =
                                                 ! ( ( -1  ) ( d22 ) )
                       ! = 1/d21 * t * ( ( d11 ) (  -1 ) )
                                     ! ( (  -1 ) ( d22 ) )
                       ! = d21 * ( ( d11 ) (  -1 ) )
                               ! ( (  -1 ) ( d22 ) )
                       d21 = w( k-1, kw )
                       d11 = w( k, kw ) / d21
                       d22 = w( k-1, kw-1 ) / d21
                       t = one / ( d11*d22-one )
                       d21 = t / d21
                       ! update elements in columns a(k-1) and a(k) as
                       ! dot products of rows of ( w(kw-1) w(kw) ) and columns
                       ! of d**(-1)
                       do j = 1, k - 2
                          a( j, k-1 ) = d21*( d11*w( j, kw-1 )-w( j, kw ) )
                          a( j, k ) = d21*( d22*w( j, kw )-w( j, kw-1 ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_dgemv( 'NO TRANSPOSE', jj-j+1, n-k, -one,a( j, k+1 ), lda, w( jj, &
                              kw+1 ), ldw, one,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb, n-k, -one,a( 1_ilp, k+1 ), &
                           lda, w( j, kw+1 ), ldw, one,a( 1_ilp, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in columns k+1:n looping backwards from k+1 to n
              j = k + 1_ilp
              60 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j + 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length n-j+1
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j + 1_ilp
                 if( jp/=jj .and. j<=n )call stdlib_dswap( n-j+1, a( jp, j ), lda, a( jj, j ), &
                           lda )
              if( j<n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              ! copy column k of a to column k of w and update it
              call stdlib_dcopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              call stdlib_dgemv( 'NO TRANSPOSE', n-k+1, k-1, -one, a( k, 1_ilp ), lda,w( k, 1_ilp ), ldw, &
                        one, w( k, k ), 1_ilp )
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_idamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = abs( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! copy column imax to column k+1 of w and update it
                    call stdlib_dcopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp )
                    call stdlib_dcopy( n-imax+1, a( imax, imax ), 1_ilp, w( imax, k+1 ),1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', n-k+1, k-1, -one, a( k, 1_ilp ),lda, w( imax, &
                              1_ilp ), ldw, one, w( k, k+1 ), 1_ilp )
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = k - 1_ilp + stdlib_idamax( imax-k, w( k, k+1 ), 1_ilp )
                    rowmax = abs( w( jmax, k+1 ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_idamax( n-imax, w( imax+1, k+1 ), 1_ilp )
                       rowmax = max( rowmax, abs( w( jmax, k+1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( w( imax, k+1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column k+1 of w to column k of w
                       call stdlib_dcopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k + kstep - 1_ilp
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kk of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k+1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = a( kk, kk )
                    call stdlib_dcopy( kp-kk-1, a( kk+1, kk ), 1_ilp, a( kp, kk+1 ),lda )
                    if( kp<n )call stdlib_dcopy( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    ! interchange rows kk and kp in first k-1 columns of a
                    ! (columns k (or k and k+1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in first kk columns of w.
                    if( k>1_ilp )call stdlib_dswap( k-1, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_dswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k),
                    ! where l(k) is the k-th column of l
                    ! store subdiag. elements of column l(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element l(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,k)
                       ! a(k+1:n,k) := l(k+1:n,k) = w(k+1:n,k)/d(k,k)
                    call stdlib_dcopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       r1 = one / a( k, k )
                       call stdlib_dscal( n-k, r1, a( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! store l(k+2:n,k) and l(k+2:n,k+1) and 2-by-2
                    ! block d(k:k+1,k:k+1) in columns k and k+1 of a.
                    ! (note: 2-by-2 diagonal block l(k:k+1,k:k+1) is a unit
                    ! block and not stored)
                       ! a(k:k+1,k:k+1) := d(k:k+1,k:k+1) = w(k:k+1,k:k+1)
                       ! a(k+2:n,k:k+1) := l(k+2:n,k:k+1) =
                       ! = w(k+2:n,k:k+1) * ( d(k:k+1,k:k+1)**(-1) )
                    if( k<n-1 ) then
                       ! compose the columns of the inverse of 2-by-2 pivot
                       ! block d in the following way to reduce the number
                       ! of flops when we myltiply panel ( w(k) w(k+1) ) by
                       ! this inverse
                       ! d**(-1) = ( d11 d21 )**(-1) =
                                 ! ( d21 d22 )
                       ! = 1/(d11*d22-d21**2) * ( ( d22 ) (-d21 ) ) =
                                              ! ( (-d21 ) ( d11 ) )
                       ! = 1/d21 * 1/((d11/d21)*(d22/d21)-1) *
                         ! * ( ( d22/d21 ) (      -1 ) ) =
                           ! ( (      -1 ) ( d11/d21 ) )
                       ! = 1/d21 * 1/(d22*d11-1) * ( ( d11 ) (  -1 ) ) =
                                                 ! ( ( -1  ) ( d22 ) )
                       ! = 1/d21 * t * ( ( d11 ) (  -1 ) )
                                     ! ( (  -1 ) ( d22 ) )
                       ! = d21 * ( ( d11 ) (  -1 ) )
                               ! ( (  -1 ) ( d22 ) )
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = one / ( d11*d22-one )
                       d21 = t / d21
                       ! update elements in columns a(k) and a(k+1) as
                       ! dot products of rows of ( w(k) w(k+1) ) and columns
                       ! of d**(-1)
                       do j = k + 2, n
                          a( j, k ) = d21*( d11*w( j, k )-w( j, k+1 ) )
                          a( j, k+1 ) = d21*( d22*w( j, k+1 )-w( j, k ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_dgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -one,a( jj, 1_ilp ), lda, w( jj, &
                              1_ilp ), ldw, one,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           one, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ), ldw,one, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! of rows in columns 1:k-1 looping backwards from k-1 to 1
              j = k - 1_ilp
              120 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j - 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length j
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j - 1_ilp
                 if( jp/=jj .and. j>=1_ilp )call stdlib_dswap( j, a( jp, 1_ilp ), lda, a( jj, 1_ilp ), lda )
                           
              if( j>1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_dlasyf


     pure module subroutine stdlib_clasyf( uplo, n, nb, kb, a, lda, ipiv, w, ldw, info )
     !! CLASYF computes a partial factorization of a complex symmetric matrix
     !! A using the Bunch-Kaufman diagonal pivoting method. The partial
     !! factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) ( D    0  ) ( L11**T L21**T )  if UPLO = 'L'
     !! ( L21  I ) ( 0   A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! Note that U**T denotes the transpose of U.
     !! CLASYF is an auxiliary routine called by CSYTRF. It uses blocked code
     !! (calling Level 3 BLAS) to update the submatrix A11 (if UPLO = 'U') or
     !! A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           
           ! Local Scalars 
           integer(ilp) :: imax, j, jb, jj, jmax, jp, k, kk, kkw, kp, kstep, kw
           real(sp) :: absakk, alpha, colmax, rowmax
           complex(sp) :: d11, d21, d22, r1, t, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=sp) ) + abs( aimag( z ) )
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              ! kw is the column of w which corresponds to column k of a
              k = n
              10 continue
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              ! copy column k of a to column kw of w and update it
              call stdlib_ccopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_cgemv( 'NO TRANSPOSE', k, n-k, -cone, a( 1_ilp, k+1 ), lda,w( k, &
                        kw+1 ), ldw, cone, w( 1_ilp, kw ), 1_ilp )
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_icamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = cabs1( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! copy column imax to column kw-1 of w and update it
                    call stdlib_ccopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                    call stdlib_ccopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                              
                    if( k<n )call stdlib_cgemv( 'NO TRANSPOSE', k, n-k, -cone,a( 1_ilp, k+1 ), lda, w(&
                               imax, kw+1 ), ldw,cone, w( 1_ilp, kw-1 ), 1_ilp )
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = imax + stdlib_icamax( k-imax, w( imax+1, kw-1 ), 1_ilp )
                    rowmax = cabs1( w( jmax, kw-1 ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_icamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( w( jmax, kw-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( w( imax, kw-1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column kw-1 of w to column kw of w
                       call stdlib_ccopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kkw of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k-1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = a( kk, kk )
                    call stdlib_ccopy( kk-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    if( kp>1_ilp )call stdlib_ccopy( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last k+1 to n columns of a
                    ! (columns k (or k and k-1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in last kkw to nb columns of w.
                    if( k<n )call stdlib_cswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                    call stdlib_cswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(kw) = u(k)*d(k),
                    ! where u(k) is the k-th column of u
                    ! store subdiag. elements of column u(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! note: diagonal element u(k,k) is a unit element
                    ! and not stored.
                       ! a(k,k) := d(k,k) = w(k,kw)
                       ! a(1:k-1,k) := u(1:k-1,k) = w(1:k-1,kw)/d(k,k)
                    call stdlib_ccopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    r1 = cone / a( k, k )
                    call stdlib_cscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now hold
                    ! ( w(kw-1) w(kw) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! store u(1:k-2,k-1) and u(1:k-2,k) and 2-by-2
                    ! block d(k-1:k,k-1:k) in columns k-1 and k of a.
                    ! note: 2-by-2 diagonal block u(k-1:k,k-1:k) is a unit
                    ! block and not stored.
                       ! a(k-1:k,k-1:k) := d(k-1:k,k-1:k) = w(k-1:k,kw-1:kw)
                       ! a(1:k-2,k-1:k) := u(1:k-2,k:k-1:k) =
                       ! = w(1:k-2,kw-1:kw) * ( d(k-1:k,k-1:k)**(-1) )
                    if( k>2_ilp ) then
                       ! compose the columns of the inverse of 2-by-2 pivot
                       ! block d in the following way to reduce the number
                       ! of flops when we myltiply panel ( w(kw-1) w(kw) ) by
                       ! this inverse
                       ! d**(-1) = ( d11 d21 )**(-1) =
                                 ! ( d21 d22 )
                       ! = 1/(d11*d22-d21**2) * ( ( d22 ) (-d21 ) ) =
                                              ! ( (-d21 ) ( d11 ) )
                       ! = 1/d21 * 1/((d11/d21)*(d22/d21)-1) *
                         ! * ( ( d22/d21 ) (      -1 ) ) =
                           ! ( (      -1 ) ( d11/d21 ) )
                       ! = 1/d21 * 1/(d22*d11-1) * ( ( d11 ) (  -1 ) ) =
                                                 ! ( ( -1  ) ( d22 ) )
                       ! = 1/d21 * t * ( ( d11 ) (  -1 ) )
                                     ! ( (  -1 ) ( d22 ) )
                       ! = d21 * ( ( d11 ) (  -1 ) )
                               ! ( (  -1 ) ( d22 ) )
                       d21 = w( k-1, kw )
                       d11 = w( k, kw ) / d21
                       d22 = w( k-1, kw-1 ) / d21
                       t = cone / ( d11*d22-cone )
                       ! update elements in columns a(k-1) and a(k) as
                       ! dot products of rows of ( w(kw-1) w(kw) ) and columns
                       ! of d**(-1)
                       d21 = t / d21
                       do j = 1, k - 2
                          a( j, k-1 ) = d21*( d11*w( j, kw-1 )-w( j, kw ) )
                          a( j, k ) = d21*( d22*w( j, kw )-w( j, kw-1 ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_cgemv( 'NO TRANSPOSE', jj-j+1, n-k, -cone,a( j, k+1 ), lda, w( jj,&
                               kw+1 ), ldw, cone,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 call stdlib_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb, n-k,-cone, a( 1_ilp, k+1 ), &
                           lda, w( j, kw+1 ), ldw,cone, a( 1_ilp, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in columns k+1:n looping backwards from k+1 to n
              j = k + 1_ilp
              60 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j + 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length n-j+1
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j + 1_ilp
                 if( jp/=jj .and. j<=n )call stdlib_cswap( n-j+1, a( jp, j ), lda, a( jj, j ), &
                           lda )
              if( j<n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              ! copy column k of a to column k of w and update it
              call stdlib_ccopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              call stdlib_cgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ), lda,w( k, 1_ilp ), ldw,&
                         cone, w( k, k ), 1_ilp )
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_icamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = cabs1( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! copy column imax to column k+1 of w and update it
                    call stdlib_ccopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp )
                    call stdlib_ccopy( n-imax+1, a( imax, imax ), 1_ilp, w( imax, k+1 ),1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ),lda, w( imax, &
                              1_ilp ), ldw, cone, w( k, k+1 ),1_ilp )
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = k - 1_ilp + stdlib_icamax( imax-k, w( k, k+1 ), 1_ilp )
                    rowmax = cabs1( w( jmax, k+1 ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_icamax( n-imax, w( imax+1, k+1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( w( jmax, k+1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( w( imax, k+1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column k+1 of w to column k of w
                       call stdlib_ccopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k + kstep - 1_ilp
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kk of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k+1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = a( kk, kk )
                    call stdlib_ccopy( kp-kk-1, a( kk+1, kk ), 1_ilp, a( kp, kk+1 ),lda )
                    if( kp<n )call stdlib_ccopy( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    ! interchange rows kk and kp in first k-1 columns of a
                    ! (columns k (or k and k+1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in first kk columns of w.
                    if( k>1_ilp )call stdlib_cswap( k-1, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_cswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k),
                    ! where l(k) is the k-th column of l
                    ! store subdiag. elements of column l(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element l(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,k)
                       ! a(k+1:n,k) := l(k+1:n,k) = w(k+1:n,k)/d(k,k)
                    call stdlib_ccopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       r1 = cone / a( k, k )
                       call stdlib_cscal( n-k, r1, a( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! store l(k+2:n,k) and l(k+2:n,k+1) and 2-by-2
                    ! block d(k:k+1,k:k+1) in columns k and k+1 of a.
                    ! (note: 2-by-2 diagonal block l(k:k+1,k:k+1) is a unit
                    ! block and not stored)
                       ! a(k:k+1,k:k+1) := d(k:k+1,k:k+1) = w(k:k+1,k:k+1)
                       ! a(k+2:n,k:k+1) := l(k+2:n,k:k+1) =
                       ! = w(k+2:n,k:k+1) * ( d(k:k+1,k:k+1)**(-1) )
                    if( k<n-1 ) then
                       ! compose the columns of the inverse of 2-by-2 pivot
                       ! block d in the following way to reduce the number
                       ! of flops when we myltiply panel ( w(k) w(k+1) ) by
                       ! this inverse
                       ! d**(-1) = ( d11 d21 )**(-1) =
                                 ! ( d21 d22 )
                       ! = 1/(d11*d22-d21**2) * ( ( d22 ) (-d21 ) ) =
                                              ! ( (-d21 ) ( d11 ) )
                       ! = 1/d21 * 1/((d11/d21)*(d22/d21)-1) *
                         ! * ( ( d22/d21 ) (      -1 ) ) =
                           ! ( (      -1 ) ( d11/d21 ) )
                       ! = 1/d21 * 1/(d22*d11-1) * ( ( d11 ) (  -1 ) ) =
                                                 ! ( ( -1  ) ( d22 ) )
                       ! = 1/d21 * t * ( ( d11 ) (  -1 ) )
                                     ! ( (  -1 ) ( d22 ) )
                       ! = d21 * ( ( d11 ) (  -1 ) )
                               ! ( (  -1 ) ( d22 ) )
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       d21 = t / d21
                       ! update elements in columns a(k) and a(k+1) as
                       ! dot products of rows of ( w(k) w(k+1) ) and columns
                       ! of d**(-1)
                       do j = k + 2, n
                          a( j, k ) = d21*( d11*w( j, k )-w( j, k+1 ) )
                          a( j, k+1 ) = d21*( d22*w( j, k+1 )-w( j, k ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_cgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -cone,a( jj, 1_ilp ), lda, w( jj,&
                               1_ilp ), ldw, cone,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           cone, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ),ldw, cone, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! of rows in columns 1:k-1 looping backwards from k-1 to 1
              j = k - 1_ilp
              120 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j - 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length j
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j - 1_ilp
                 if( jp/=jj .and. j>=1_ilp )call stdlib_cswap( j, a( jp, 1_ilp ), lda, a( jj, 1_ilp ), lda )
                           
              if( j>1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_clasyf

     pure module subroutine stdlib_zlasyf( uplo, n, nb, kb, a, lda, ipiv, w, ldw, info )
     !! ZLASYF computes a partial factorization of a complex symmetric matrix
     !! A using the Bunch-Kaufman diagonal pivoting method. The partial
     !! factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**T U22**T )
     !! A  =  ( L11  0 ) ( D    0  ) ( L11**T L21**T )  if UPLO = 'L'
     !! ( L21  I ) ( 0   A22 ) (  0       I    )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! Note that U**T denotes the transpose of U.
     !! ZLASYF is an auxiliary routine called by ZSYTRF. It uses blocked code
     !! (calling Level 3 BLAS) to update the submatrix A11 (if UPLO = 'U') or
     !! A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           
           ! Local Scalars 
           integer(ilp) :: imax, j, jb, jj, jmax, jp, k, kk, kkw, kp, kstep, kw
           real(dp) :: absakk, alpha, colmax, rowmax
           complex(dp) :: d11, d21, d22, r1, t, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=dp) ) + abs( aimag( z ) )
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              ! kw is the column of w which corresponds to column k of a
              k = n
              10 continue
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              ! copy column k of a to column kw of w and update it
              call stdlib_zcopy( k, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              if( k<n )call stdlib_zgemv( 'NO TRANSPOSE', k, n-k, -cone, a( 1_ilp, k+1 ), lda,w( k, &
                        kw+1 ), ldw, cone, w( 1_ilp, kw ), 1_ilp )
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, kw ) )
              ! imax is the row-index of the largest off-diagonal element in
              if( k>1_ilp ) then
                 imax = stdlib_izamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = cabs1( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! copy column imax to column kw-1 of w and update it
                    call stdlib_zcopy( imax, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                    call stdlib_zcopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                              
                    if( k<n )call stdlib_zgemv( 'NO TRANSPOSE', k, n-k, -cone,a( 1_ilp, k+1 ), lda, w(&
                               imax, kw+1 ), ldw,cone, w( 1_ilp, kw-1 ), 1_ilp )
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = imax + stdlib_izamax( k-imax, w( imax+1, kw-1 ), 1_ilp )
                    rowmax = cabs1( w( jmax, kw-1 ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_izamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( w( jmax, kw-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( w( imax, kw-1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column kw-1 of w to column kw of w
                       call stdlib_zcopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kkw of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k-1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = a( kk, kk )
                    call stdlib_zcopy( kk-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    if( kp>1_ilp )call stdlib_zcopy( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last k+1 to n columns of a
                    ! (columns k (or k and k-1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in last kkw to nb columns of w.
                    if( k<n )call stdlib_zswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                    call stdlib_zswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(kw) = u(k)*d(k),
                    ! where u(k) is the k-th column of u
                    ! store subdiag. elements of column u(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! note: diagonal element u(k,k) is a unit element
                    ! and not stored.
                       ! a(k,k) := d(k,k) = w(k,kw)
                       ! a(1:k-1,k) := u(1:k-1,k) = w(1:k-1,kw)/d(k,k)
                    call stdlib_zcopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    r1 = cone / a( k, k )
                    call stdlib_zscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now hold
                    ! ( w(kw-1) w(kw) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! store u(1:k-2,k-1) and u(1:k-2,k) and 2-by-2
                    ! block d(k-1:k,k-1:k) in columns k-1 and k of a.
                    ! note: 2-by-2 diagonal block u(k-1:k,k-1:k) is a unit
                    ! block and not stored.
                       ! a(k-1:k,k-1:k) := d(k-1:k,k-1:k) = w(k-1:k,kw-1:kw)
                       ! a(1:k-2,k-1:k) := u(1:k-2,k:k-1:k) =
                       ! = w(1:k-2,kw-1:kw) * ( d(k-1:k,k-1:k)**(-1) )
                    if( k>2_ilp ) then
                       ! compose the columns of the inverse of 2-by-2 pivot
                       ! block d in the following way to reduce the number
                       ! of flops when we myltiply panel ( w(kw-1) w(kw) ) by
                       ! this inverse
                       ! d**(-1) = ( d11 d21 )**(-1) =
                                 ! ( d21 d22 )
                       ! = 1/(d11*d22-d21**2) * ( ( d22 ) (-d21 ) ) =
                                              ! ( (-d21 ) ( d11 ) )
                       ! = 1/d21 * 1/((d11/d21)*(d22/d21)-1) *
                         ! * ( ( d22/d21 ) (      -1 ) ) =
                           ! ( (      -1 ) ( d11/d21 ) )
                       ! = 1/d21 * 1/(d22*d11-1) * ( ( d11 ) (  -1 ) ) =
                                                 ! ( ( -1  ) ( d22 ) )
                       ! = 1/d21 * t * ( ( d11 ) (  -1 ) )
                                     ! ( (  -1 ) ( d22 ) )
                       ! = d21 * ( ( d11 ) (  -1 ) )
                               ! ( (  -1 ) ( d22 ) )
                       d21 = w( k-1, kw )
                       d11 = w( k, kw ) / d21
                       d22 = w( k-1, kw-1 ) / d21
                       t = cone / ( d11*d22-cone )
                       d21 = t / d21
                       ! update elements in columns a(k-1) and a(k) as
                       ! dot products of rows of ( w(kw-1) w(kw) ) and columns
                       ! of d**(-1)
                       do j = 1, k - 2
                          a( j, k-1 ) = d21*( d11*w( j, kw-1 )-w( j, kw ) )
                          a( j, k ) = d21*( d22*w( j, kw )-w( j, kw-1 ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**t = a11 - u12*w**t
              ! computing blocks of nb columns at a time
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_zgemv( 'NO TRANSPOSE', jj-j+1, n-k, -cone,a( j, k+1 ), lda, w( jj,&
                               kw+1 ), ldw, cone,a( j, jj ), 1_ilp )
                 end do
                 ! update the rectangular superdiagonal block
                 call stdlib_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb, n-k,-cone, a( 1_ilp, k+1 ), &
                           lda, w( j, kw+1 ), ldw,cone, a( 1_ilp, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in columns k+1:n looping backwards from k+1 to n
              j = k + 1_ilp
              60 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j + 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length n-j+1
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j + 1_ilp
                 if( jp/=jj .and. j<=n )call stdlib_zswap( n-j+1, a( jp, j ), lda, a( jj, j ), &
                           lda )
              if( j<n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              ! copy column k of a to column k of w and update it
              call stdlib_zcopy( n-k+1, a( k, k ), 1_ilp, w( k, k ), 1_ilp )
              call stdlib_zgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ), lda,w( k, 1_ilp ), ldw,&
                         cone, w( k, k ), 1_ilp )
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( w( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              if( k<n ) then
                 imax = k + stdlib_izamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = cabs1( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! copy column imax to column k+1 of w and update it
                    call stdlib_zcopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp )
                    call stdlib_zcopy( n-imax+1, a( imax, imax ), 1_ilp, w( imax, k+1 ),1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ),lda, w( imax, &
                              1_ilp ), ldw, cone, w( k, k+1 ),1_ilp )
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = k - 1_ilp + stdlib_izamax( imax-k, w( k, k+1 ), 1_ilp )
                    rowmax = cabs1( w( jmax, k+1 ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_izamax( n-imax, w( imax+1, k+1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( w( jmax, k+1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( w( imax, k+1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column k+1 of w to column k of w
                       call stdlib_zcopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k + kstep - 1_ilp
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kk of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k+1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = a( kk, kk )
                    call stdlib_zcopy( kp-kk-1, a( kk+1, kk ), 1_ilp, a( kp, kk+1 ),lda )
                    if( kp<n )call stdlib_zcopy( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    ! interchange rows kk and kp in first k-1 columns of a
                    ! (columns k (or k and k+1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in first kk columns of w.
                    if( k>1_ilp )call stdlib_zswap( k-1, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_zswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k),
                    ! where l(k) is the k-th column of l
                    ! store subdiag. elements of column l(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element l(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,k)
                       ! a(k+1:n,k) := l(k+1:n,k) = w(k+1:n,k)/d(k,k)
                    call stdlib_zcopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       r1 = cone / a( k, k )
                       call stdlib_zscal( n-k, r1, a( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! store l(k+2:n,k) and l(k+2:n,k+1) and 2-by-2
                    ! block d(k:k+1,k:k+1) in columns k and k+1 of a.
                    ! (note: 2-by-2 diagonal block l(k:k+1,k:k+1) is a unit
                    ! block and not stored)
                       ! a(k:k+1,k:k+1) := d(k:k+1,k:k+1) = w(k:k+1,k:k+1)
                       ! a(k+2:n,k:k+1) := l(k+2:n,k:k+1) =
                       ! = w(k+2:n,k:k+1) * ( d(k:k+1,k:k+1)**(-1) )
                    if( k<n-1 ) then
                       ! compose the columns of the inverse of 2-by-2 pivot
                       ! block d in the following way to reduce the number
                       ! of flops when we myltiply panel ( w(k) w(k+1) ) by
                       ! this inverse
                       ! d**(-1) = ( d11 d21 )**(-1) =
                                 ! ( d21 d22 )
                       ! = 1/(d11*d22-d21**2) * ( ( d22 ) (-d21 ) ) =
                                              ! ( (-d21 ) ( d11 ) )
                       ! = 1/d21 * 1/((d11/d21)*(d22/d21)-1) *
                         ! * ( ( d22/d21 ) (      -1 ) ) =
                           ! ( (      -1 ) ( d11/d21 ) )
                       ! = 1/d21 * 1/(d22*d11-1) * ( ( d11 ) (  -1 ) ) =
                                                 ! ( ( -1  ) ( d22 ) )
                       ! = 1/d21 * t * ( ( d11 ) (  -1 ) )
                                     ! ( (  -1 ) ( d22 ) )
                       ! = d21 * ( ( d11 ) (  -1 ) )
                               ! ( (  -1 ) ( d22 ) )
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       d21 = t / d21
                       ! update elements in columns a(k) and a(k+1) as
                       ! dot products of rows of ( w(k) w(k+1) ) and columns
                       ! of d**(-1)
                       do j = k + 2, n
                          a( j, k ) = d21*( d11*w( j, k )-w( j, k+1 ) )
                          a( j, k+1 ) = d21*( d22*w( j, k+1 )-w( j, k ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**t = a22 - l21*w**t
              ! computing blocks of nb columns at a time
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    call stdlib_zgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -cone,a( jj, 1_ilp ), lda, w( jj,&
                               1_ilp ), ldw, cone,a( jj, jj ), 1_ilp )
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           cone, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ),ldw, cone, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! of rows in columns 1:k-1 looping backwards from k-1 to 1
              j = k - 1_ilp
              120 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j - 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length j
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j - 1_ilp
                 if( jp/=jj .and. j>=1_ilp )call stdlib_zswap( j, a( jp, 1_ilp ), lda, a( jj, 1_ilp ), lda )
                           
              if( j>1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_zlasyf




     pure module subroutine stdlib_ssytf2( uplo, n, a, lda, ipiv, info )
     !! SSYTF2 computes the factorization of a real symmetric matrix A using
     !! the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**T is the transpose of U, and D is symmetric and
     !! block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kk, kp, kstep
           real(sp) :: absakk, alpha, colmax, d11, d12, d21, d22, r1, rowmax, t, wk, wkm1, &
                     wkp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTF2', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 70
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_isamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_sisnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = imax + stdlib_isamax( k-imax, a( imax, imax+1 ), lda )
                    rowmax = abs( a( imax, jmax ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_isamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                       rowmax = max( rowmax, abs( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( a( imax, imax ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_sswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_sswap( kk-kp-1, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**t = a - w(k)*1/d(k)*w(k)**t
                    r1 = one / a( k, k )
                    call stdlib_ssyr( uplo, k-1, -r1, a( 1_ilp, k ), 1_ilp, a, lda )
                    ! store u(k) in column k
                    call stdlib_sscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**t
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = one / ( d11*d22-one )
                       d12 = t / d12
                       do j = k - 2, 1, -1
                          wkm1 = d12*( d11*a( j, k-1 )-a( j, k ) )
                          wk = d12*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - a( i, k )*wk -a( i, k-1 )*wkm1
                          end do
                          a( j, k ) = wk
                          a( j, k-1 ) = wkm1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 70
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_isamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_sisnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = k - 1_ilp + stdlib_isamax( imax-k, a( imax, k ), lda )
                    rowmax = abs( a( imax, jmax ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_isamax( n-imax, a( imax+1, imax ), 1_ilp )
                       rowmax = max( rowmax, abs( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( a( imax, imax ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_sswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    call stdlib_sswap( kp-kk-1, a( kk+1, kk ), 1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**t = a - w(k)*(1/d(k))*w(k)**t
                       d11 = one / a( k, k )
                       call stdlib_ssyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                 
                       ! store l(k) in column k
                       call stdlib_sscal( n-k, d11, a( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k)
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( (a(k) a(k+1))*d(k)**(-1) ) * (a(k) a(k+1))**t
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = one / ( d11*d22-one )
                       d21 = t / d21
                       do j = k + 2, n
                          wk = d21*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = d21*( d22*a( j, k+1 )-a( j, k ) )
                          do i = j, n
                             a( i, j ) = a( i, j ) - a( i, k )*wk -a( i, k+1 )*wkp1
                          end do
                          a( j, k ) = wk
                          a( j, k+1 ) = wkp1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
           end if
           70 continue
           return
     end subroutine stdlib_ssytf2

     pure module subroutine stdlib_dsytf2( uplo, n, a, lda, ipiv, info )
     !! DSYTF2 computes the factorization of a real symmetric matrix A using
     !! the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**T is the transpose of U, and D is symmetric and
     !! block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kk, kp, kstep
           real(dp) :: absakk, alpha, colmax, d11, d12, d21, d22, r1, rowmax, t, wk, wkm1, &
                     wkp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTF2', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 70
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_idamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_disnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = imax + stdlib_idamax( k-imax, a( imax, imax+1 ), lda )
                    rowmax = abs( a( imax, jmax ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_idamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                       rowmax = max( rowmax, abs( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( a( imax, imax ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_dswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_dswap( kk-kp-1, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**t = a - w(k)*1/d(k)*w(k)**t
                    r1 = one / a( k, k )
                    call stdlib_dsyr( uplo, k-1, -r1, a( 1_ilp, k ), 1_ilp, a, lda )
                    ! store u(k) in column k
                    call stdlib_dscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**t
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = one / ( d11*d22-one )
                       d12 = t / d12
                       do j = k - 2, 1, -1
                          wkm1 = d12*( d11*a( j, k-1 )-a( j, k ) )
                          wk = d12*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - a( i, k )*wk -a( i, k-1 )*wkm1
                          end do
                          a( j, k ) = wk
                          a( j, k-1 ) = wkm1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 70
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_idamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = abs( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_disnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = k - 1_ilp + stdlib_idamax( imax-k, a( imax, k ), lda )
                    rowmax = abs( a( imax, jmax ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_idamax( n-imax, a( imax+1, imax ), 1_ilp )
                       rowmax = max( rowmax, abs( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( a( imax, imax ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_dswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    call stdlib_dswap( kp-kk-1, a( kk+1, kk ), 1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**t = a - w(k)*(1/d(k))*w(k)**t
                       d11 = one / a( k, k )
                       call stdlib_dsyr( uplo, n-k, -d11, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                 
                       ! store l(k) in column k
                       call stdlib_dscal( n-k, d11, a( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k)
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( (a(k) a(k+1))*d(k)**(-1) ) * (a(k) a(k+1))**t
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = one / ( d11*d22-one )
                       d21 = t / d21
                       do j = k + 2, n
                          wk = d21*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = d21*( d22*a( j, k+1 )-a( j, k ) )
                          do i = j, n
                             a( i, j ) = a( i, j ) - a( i, k )*wk -a( i, k+1 )*wkp1
                          end do
                          a( j, k ) = wk
                          a( j, k+1 ) = wkp1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
           end if
           70 continue
           return
     end subroutine stdlib_dsytf2


     pure module subroutine stdlib_csytf2( uplo, n, a, lda, ipiv, info )
     !! CSYTF2 computes the factorization of a complex symmetric matrix A
     !! using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**T is the transpose of U, and D is symmetric and
     !! block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kk, kp, kstep
           real(sp) :: absakk, alpha, colmax, rowmax
           complex(sp) :: d11, d12, d21, d22, r1, t, wk, wkm1, wkp1, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=sp) ) + abs( aimag( z ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTF2', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 70
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_icamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero .or. stdlib_sisnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = imax + stdlib_icamax( k-imax, a( imax, imax+1 ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_icamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( a( imax, imax ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_cswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_cswap( kk-kp-1, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**t = a - w(k)*1/d(k)*w(k)**t
                    r1 = cone / a( k, k )
                    call stdlib_csyr( uplo, k-1, -r1, a( 1_ilp, k ), 1_ilp, a, lda )
                    ! store u(k) in column k
                    call stdlib_cscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**t
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = cone / ( d11*d22-cone )
                       d12 = t / d12
                       do j = k - 2, 1, -1
                          wkm1 = d12*( d11*a( j, k-1 )-a( j, k ) )
                          wk = d12*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - a( i, k )*wk -a( i, k-1 )*wkm1
                          end do
                          a( j, k ) = wk
                          a( j, k-1 ) = wkm1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 70
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_icamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero .or. stdlib_sisnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = k - 1_ilp + stdlib_icamax( imax-k, a( imax, k ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_icamax( n-imax, a( imax+1, imax ), 1_ilp )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( a( imax, imax ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_cswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    call stdlib_cswap( kp-kk-1, a( kk+1, kk ), 1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**t = a - w(k)*(1/d(k))*w(k)**t
                       r1 = cone / a( k, k )
                       call stdlib_csyr( uplo, n-k, -r1, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                 
                       ! store l(k) in column k
                       call stdlib_cscal( n-k, r1, a( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k)
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**t
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**t
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       d21 = t / d21
                       do j = k + 2, n
                          wk = d21*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = d21*( d22*a( j, k+1 )-a( j, k ) )
                          do i = j, n
                             a( i, j ) = a( i, j ) - a( i, k )*wk -a( i, k+1 )*wkp1
                          end do
                          a( j, k ) = wk
                          a( j, k+1 ) = wkp1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
           end if
           70 continue
           return
     end subroutine stdlib_csytf2

     pure module subroutine stdlib_zsytf2( uplo, n, a, lda, ipiv, info )
     !! ZSYTF2 computes the factorization of a complex symmetric matrix A
     !! using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**T is the transpose of U, and D is symmetric and
     !! block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kk, kp, kstep
           real(dp) :: absakk, alpha, colmax, rowmax
           complex(dp) :: d11, d12, d21, d22, r1, t, wk, wkm1, wkp1, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=dp) ) + abs( aimag( z ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTF2', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 70
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_izamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero .or. stdlib_disnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = imax + stdlib_izamax( k-imax, a( imax, imax+1 ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_izamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( a( imax, imax ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_zswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    call stdlib_zswap( kk-kp-1, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**t = a - w(k)*1/d(k)*w(k)**t
                    r1 = cone / a( k, k )
                    call stdlib_zsyr( uplo, k-1, -r1, a( 1_ilp, k ), 1_ilp, a, lda )
                    ! store u(k) in column k
                    call stdlib_zscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**t
                    if( k>2_ilp ) then
                       d12 = a( k-1, k )
                       d22 = a( k-1, k-1 ) / d12
                       d11 = a( k, k ) / d12
                       t = cone / ( d11*d22-cone )
                       d12 = t / d12
                       do j = k - 2, 1, -1
                          wkm1 = d12*( d11*a( j, k-1 )-a( j, k ) )
                          wk = d12*( d22*a( j, k )-a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - a( i, k )*wk -a( i, k-1 )*wkm1
                          end do
                          a( j, k ) = wk
                          a( j, k-1 ) = wkm1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop
              if( k>n )go to 70
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( a( k, k ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_izamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero .or. stdlib_disnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = k - 1_ilp + stdlib_izamax( imax-k, a( imax, k ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_izamax( n-imax, a( imax+1, imax ), 1_ilp )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( a( imax, imax ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_zswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    call stdlib_zswap( kp-kk-1, a( kk+1, kk ), 1_ilp, a( kp, kk+1 ),lda )
                    t = a( kk, kk )
                    a( kk, kk ) = a( kp, kp )
                    a( kp, kp ) = t
                    if( kstep==2_ilp ) then
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**t = a - w(k)*(1/d(k))*w(k)**t
                       r1 = cone / a( k, k )
                       call stdlib_zsyr( uplo, n-k, -r1, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                 
                       ! store l(k) in column k
                       call stdlib_zscal( n-k, r1, a( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k)
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**t
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**t
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d21 = a( k+1, k )
                       d11 = a( k+1, k+1 ) / d21
                       d22 = a( k, k ) / d21
                       t = cone / ( d11*d22-cone )
                       d21 = t / d21
                       do j = k + 2, n
                          wk = d21*( d11*a( j, k )-a( j, k+1 ) )
                          wkp1 = d21*( d22*a( j, k+1 )-a( j, k ) )
                          do i = j, n
                             a( i, j ) = a( i, j ) - a( i, k )*wk -a( i, k+1 )*wkp1
                          end do
                          a( j, k ) = wk
                          a( j, k+1 ) = wkp1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 40
           end if
           70 continue
           return
     end subroutine stdlib_zsytf2




     pure module subroutine stdlib_ssytrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! SSYTRS solves a system of linear equations A*X = B with a real
     !! symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by SSYTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp
           real(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_sger( k-1, nrhs, -one, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_sscal( nrhs, one / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_sswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_sger( k-2, nrhs, -one, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 call stdlib_sger( k-2, nrhs, -one, a( 1_ilp, k-1 ), 1_ilp, b( k-1, 1_ilp ),ldb, b( 1_ilp, 1_ilp ), &
                           ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_sgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb, a( 1_ilp, k ),1_ilp, one, b( k, &
                           1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 call stdlib_sgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb, a( 1_ilp, k ),1_ilp, one, b( k, &
                           1_ilp ), ldb )
                 call stdlib_sgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb,a( 1_ilp, k+1 ), 1_ilp, one, b( &
                           k+1, 1_ilp ), ldb )
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_sger( n-k, nrhs, -one, a( k+1, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+&
                           1_ilp, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_sscal( nrhs, one / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_sswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_sger( n-k-1, nrhs, -one, a( k+2, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, 1_ilp &
                              ), ldb )
                    call stdlib_sger( n-k-1, nrhs, -one, a( k+2, k+1 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( k+&
                              2_ilp, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / akm1k
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_sgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+&
                           1_ilp, k ), 1_ilp, one, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_sgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+1, k ),&
                               1_ilp, one, b( k, 1_ilp ), ldb )
                    call stdlib_sgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+1, k-1 &
                              ), 1_ilp, one, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_ssytrs

     pure module subroutine stdlib_dsytrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! DSYTRS solves a system of linear equations A*X = B with a real
     !! symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by DSYTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp
           real(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_dger( k-1, nrhs, -one, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_dscal( nrhs, one / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_dswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_dger( k-2, nrhs, -one, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 call stdlib_dger( k-2, nrhs, -one, a( 1_ilp, k-1 ), 1_ilp, b( k-1, 1_ilp ),ldb, b( 1_ilp, 1_ilp ), &
                           ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_dgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb, a( 1_ilp, k ),1_ilp, one, b( k, &
                           1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 call stdlib_dgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb, a( 1_ilp, k ),1_ilp, one, b( k, &
                           1_ilp ), ldb )
                 call stdlib_dgemv( 'TRANSPOSE', k-1, nrhs, -one, b, ldb,a( 1_ilp, k+1 ), 1_ilp, one, b( &
                           k+1, 1_ilp ), ldb )
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_dger( n-k, nrhs, -one, a( k+1, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+&
                           1_ilp, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_dscal( nrhs, one / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_dswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_dger( n-k-1, nrhs, -one, a( k+2, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, 1_ilp &
                              ), ldb )
                    call stdlib_dger( n-k-1, nrhs, -one, a( k+2, k+1 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( k+&
                              2_ilp, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / akm1k
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - one
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_dgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+&
                           1_ilp, k ), 1_ilp, one, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_dgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+1, k ),&
                               1_ilp, one, b( k, 1_ilp ), ldb )
                    call stdlib_dgemv( 'TRANSPOSE', n-k, nrhs, -one, b( k+1, 1_ilp ),ldb, a( k+1, k-1 &
                              ), 1_ilp, one, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_dsytrs


     pure module subroutine stdlib_csytrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! CSYTRS solves a system of linear equations A*X = B with a complex
     !! symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by CSYTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_cgeru( k-1, nrhs, -cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb &
                           )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_cscal( nrhs, cone / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_cswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_cgeru( k-2, nrhs, -cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb &
                           )
                 call stdlib_cgeru( k-2, nrhs, -cone, a( 1_ilp, k-1 ), 1_ilp, b( k-1, 1_ilp ),ldb, b( 1_ilp, 1_ilp ), &
                           ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_cgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb, a( 1_ilp, k ),1_ilp, cone, b( &
                           k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 call stdlib_cgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb, a( 1_ilp, k ),1_ilp, cone, b( &
                           k, 1_ilp ), ldb )
                 call stdlib_cgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb,a( 1_ilp, k+1 ), 1_ilp, cone, b(&
                            k+1, 1_ilp ), ldb )
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_cgeru( n-k, nrhs, -cone, a( k+1, k ), 1_ilp, b( k, 1_ilp ),ldb, b( &
                           k+1, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_cscal( nrhs, cone / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_cswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_cgeru( n-k-1, nrhs, -cone, a( k+2, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, &
                              1_ilp ), ldb )
                    call stdlib_cgeru( n-k-1, nrhs, -cone, a( k+2, k+1 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( &
                              k+2, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / akm1k
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_cgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+&
                           1_ilp, k ), 1_ilp, cone, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_cgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+1, k )&
                              , 1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+1, k-&
                              1_ilp ), 1_ilp, cone, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_csytrs

     pure module subroutine stdlib_zsytrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! ZSYTRS solves a system of linear equations A*X = B with a complex
     !! symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by ZSYTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_zgeru( k-1, nrhs, -cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb &
                           )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_zscal( nrhs, cone / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_zswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_zgeru( k-2, nrhs, -cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb &
                           )
                 call stdlib_zgeru( k-2, nrhs, -cone, a( 1_ilp, k-1 ), 1_ilp, b( k-1, 1_ilp ),ldb, b( 1_ilp, 1_ilp ), &
                           ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / akm1k
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**t *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**t(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_zgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb, a( 1_ilp, k ),1_ilp, cone, b( &
                           k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**t(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 call stdlib_zgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb, a( 1_ilp, k ),1_ilp, cone, b( &
                           k, 1_ilp ), ldb )
                 call stdlib_zgemv( 'TRANSPOSE', k-1, nrhs, -cone, b, ldb,a( 1_ilp, k+1 ), 1_ilp, cone, b(&
                            k+1, 1_ilp ), ldb )
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**t.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_zgeru( n-k, nrhs, -cone, a( k+1, k ), 1_ilp, b( k, 1_ilp ),ldb, b( &
                           k+1, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 call stdlib_zscal( nrhs, cone / a( k, k ), b( k, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_zswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_zgeru( n-k-1, nrhs, -cone, a( k+2, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, &
                              1_ilp ), ldb )
                    call stdlib_zgeru( n-k-1, nrhs, -cone, a( k+2, k+1 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( &
                              k+2, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / akm1k
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / akm1k
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**t *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**t(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_zgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+&
                           1_ilp, k ), 1_ilp, cone, b( k, 1_ilp ), ldb )
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**t(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_zgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+1, k )&
                              , 1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'TRANSPOSE', n-k, nrhs, -cone, b( k+1, 1_ilp ),ldb, a( k+1, k-&
                              1_ilp ), 1_ilp, cone, b( k-1, 1_ilp ),ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_zsytrs




     pure module subroutine stdlib_ssytri( uplo, n, a, lda, ipiv, work, info )
     !! SSYTRI computes the inverse of a real symmetric indefinite matrix
     !! A using the factorization A = U*D*U**T or A = L*D*L**T computed by
     !! SSYTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kp, kstep
           real(sp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==zero )return
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 40
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / a( k, k )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_scopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_sdot( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k+1 ) )
                 ak = a( k, k ) / t
                 akp1 = a( k+1, k+1 ) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-one )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_scopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_sdot( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_sdot( k-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                    call stdlib_scopy( k-1, a( 1_ilp, k+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k+1 ), 1_ilp )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -stdlib_sdot( k-1, work, 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 call stdlib_sswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                 call stdlib_sswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
              end if
              k = k + kstep
              go to 30
              40 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              50 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 60
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / a( k, k )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_scopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_sdot( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k-1 ) )
                 ak = a( k-1, k-1 ) / t
                 akp1 = a( k, k ) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-one )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_scopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_sdot( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_sdot( n-k, a( k+1, k ), 1_ilp, a( k+1, k-1 ),1_ilp )
                              
                    call stdlib_scopy( n-k, a( k+1, k-1 ), 1_ilp, work, 1_ilp )
                    call stdlib_ssymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k-1 ), 1_ilp )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -stdlib_sdot( n-k, work, 1_ilp, a( k+1, k-1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 if( kp<n )call stdlib_sswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                 call stdlib_sswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
              end if
              k = k - kstep
              go to 50
              60 continue
           end if
           return
     end subroutine stdlib_ssytri

     pure module subroutine stdlib_dsytri( uplo, n, a, lda, ipiv, work, info )
     !! DSYTRI computes the inverse of a real symmetric indefinite matrix
     !! A using the factorization A = U*D*U**T or A = L*D*L**T computed by
     !! DSYTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kp, kstep
           real(dp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==zero )return
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 40
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / a( k, k )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_dcopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_ddot( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k+1 ) )
                 ak = a( k, k ) / t
                 akp1 = a( k+1, k+1 ) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-one )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_dcopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_ddot( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_ddot( k-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                    call stdlib_dcopy( k-1, a( 1_ilp, k+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, k-1, -one, a, lda, work, 1_ilp, zero,a( 1_ilp, k+1 ), 1_ilp )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -stdlib_ddot( k-1, work, 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 call stdlib_dswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                 call stdlib_dswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
              end if
              k = k + kstep
              go to 30
              40 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              50 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 60
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / a( k, k )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_dcopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_ddot( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k-1 ) )
                 ak = a( k-1, k-1 ) / t
                 akp1 = a( k, k ) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-one )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_dcopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_ddot( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_ddot( n-k, a( k+1, k ), 1_ilp, a( k+1, k-1 ),1_ilp )
                              
                    call stdlib_dcopy( n-k, a( k+1, k-1 ), 1_ilp, work, 1_ilp )
                    call stdlib_dsymv( uplo, n-k, -one, a( k+1, k+1 ), lda, work, 1_ilp,zero, a( k+1, &
                              k-1 ), 1_ilp )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -stdlib_ddot( n-k, work, 1_ilp, a( k+1, k-1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 if( kp<n )call stdlib_dswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                 call stdlib_dswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
              end if
              k = k - kstep
              go to 50
              60 continue
           end if
           return
     end subroutine stdlib_dsytri


     pure module subroutine stdlib_csytri( uplo, n, a, lda, ipiv, work, info )
     !! CSYTRI computes the inverse of a complex symmetric indefinite matrix
     !! A using the factorization A = U*D*U**T or A = L*D*L**T computed by
     !! CSYTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kp, kstep
           complex(sp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 40
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = cone / a( k, k )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_ccopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_cdotu( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = a( k, k+1 )
                 ak = a( k, k ) / t
                 akp1 = a( k+1, k+1 ) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-cone )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_ccopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_cdotu( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_cdotu( k-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                    call stdlib_ccopy( k-1, a( 1_ilp, k+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k+1 ), 1_ilp )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -stdlib_cdotu( k-1, work, 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 call stdlib_cswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                 call stdlib_cswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
              end if
              k = k + kstep
              go to 30
              40 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              50 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 60
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = cone / a( k, k )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_ccopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+&
                              1_ilp, k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_cdotu( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = a( k, k-1 )
                 ak = a( k-1, k-1 ) / t
                 akp1 = a( k, k ) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-cone )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_ccopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+&
                              1_ilp, k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_cdotu( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_cdotu( n-k, a( k+1, k ), 1_ilp, a( k+1, k-1 ),1_ilp &
                              )
                    call stdlib_ccopy( n-k, a( k+1, k-1 ), 1_ilp, work, 1_ilp )
                    call stdlib_csymv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+&
                              1_ilp, k-1 ), 1_ilp )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -stdlib_cdotu( n-k, work, 1_ilp, a( k+1, k-1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 if( kp<n )call stdlib_cswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                 call stdlib_cswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
              end if
              k = k - kstep
              go to 50
              60 continue
           end if
           return
     end subroutine stdlib_csytri

     pure module subroutine stdlib_zsytri( uplo, n, a, lda, ipiv, work, info )
     !! ZSYTRI computes the inverse of a complex symmetric indefinite matrix
     !! A using the factorization A = U*D*U**T or A = L*D*L**T computed by
     !! ZSYTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kp, kstep
           complex(dp) :: ak, akkp1, akp1, d, t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 40
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = cone / a( k, k )
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_zcopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_zdotu( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = a( k, k+1 )
                 ak = a( k, k ) / t
                 akp1 = a( k+1, k+1 ) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-cone )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_zcopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - stdlib_zdotu( k-1, work, 1_ilp, a( 1_ilp, k ),1_ilp )
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_zdotu( k-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                    call stdlib_zcopy( k-1, a( 1_ilp, k+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k+1 ), 1_ilp )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -stdlib_zdotu( k-1, work, 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 call stdlib_zswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                 call stdlib_zswap( k-kp-1, a( kp+1, k ), 1_ilp, a( kp, kp+1 ), lda )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
              end if
              k = k + kstep
              go to 30
              40 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**t.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              50 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 60
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = cone / a( k, k )
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_zcopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+&
                              1_ilp, k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_zdotu( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = a( k, k-1 )
                 ak = a( k-1, k-1 ) / t
                 akp1 = a( k, k ) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-cone )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_zcopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+&
                              1_ilp, k ), 1_ilp )
                    a( k, k ) = a( k, k ) - stdlib_zdotu( n-k, work, 1_ilp, a( k+1, k ),1_ilp )
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_zdotu( n-k, a( k+1, k ), 1_ilp, a( k+1, k-1 ),1_ilp &
                              )
                    call stdlib_zcopy( n-k, a( k+1, k-1 ), 1_ilp, work, 1_ilp )
                    call stdlib_zsymv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work, 1_ilp,czero, a( k+&
                              1_ilp, k-1 ), 1_ilp )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -stdlib_zdotu( n-k, work, 1_ilp, a( k+1, k-1 ), 1_ilp )
                              
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 if( kp<n )call stdlib_zswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                 call stdlib_zswap( kp-k-1, a( k+1, k ), 1_ilp, a( kp, k+1 ), lda )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
              end if
              k = k - kstep
              go to 50
              60 continue
           end if
           return
     end subroutine stdlib_zsytri




     pure module subroutine stdlib_ssyrfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! SSYRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric indefinite, and
     !! provides error bounds and backward error estimates for the solution.
               berr, work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
           real(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_scopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_ssymv( uplo, n, -one, a, lda, x( 1_ilp, j ), 1_ilp, one,work( n+1 ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    do i = 1, k - 1
                       work( i ) = work( i ) + abs( a( i, k ) )*xk
                       s = s + abs( a( i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + abs( a( k, k ) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    work( k ) = work( k ) + abs( a( k, k ) )*xk
                    do i = k + 1, n
                       work( i ) = work( i ) + abs( a( i, k ) )*xk
                       s = s + abs( a( i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_ssytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work( n+1 ), n,info )
                 call stdlib_saxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_slacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_slacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_ssytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work( n+1 ), n,info )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_ssytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work( n+1 ), n,info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_ssyrfs

     pure module subroutine stdlib_dsyrfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! DSYRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric indefinite, and
     !! provides error bounds and backward error estimates for the solution.
               berr, work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
           real(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_dcopy( n, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_dsymv( uplo, n, -one, a, lda, x( 1_ilp, j ), 1_ilp, one,work( n+1 ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    do i = 1, k - 1
                       work( i ) = work( i ) + abs( a( i, k ) )*xk
                       s = s + abs( a( i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + abs( a( k, k ) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = abs( x( k, j ) )
                    work( k ) = work( k ) + abs( a( k, k ) )*xk
                    do i = k + 1, n
                       work( i ) = work( i ) + abs( a( i, k ) )*xk
                       s = s + abs( a( i, k ) )*abs( x( i, j ) )
                    end do
                    work( k ) = work( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_dsytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work( n+1 ), n,info )
                 call stdlib_daxpy( n, one, work( n+1 ), 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_dlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_dlacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_dsytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work( n+1 ), n,info )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_dsytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work( n+1 ), n,info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_dsyrfs


     pure module subroutine stdlib_csyrfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! CSYRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric indefinite, and
     !! provides error bounds and backward error estimates for the solution.
               berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_ccopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_csymv( uplo, n, -cone, a, lda, x( 1_ilp, j ), 1_ilp, cone, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + cabs1( a( k, k ) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + cabs1( a( k, k ) )*xk
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_csytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 call stdlib_caxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_clacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_csytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_csytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_csyrfs

     pure module subroutine stdlib_zsyrfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! ZSYRFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is symmetric indefinite, and
     !! provides error bounds and backward error estimates for the solution.
               berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYRFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_zcopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_zsymv( uplo, n, -cone, a, lda, x( 1_ilp, j ), 1_ilp, cone, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + cabs1( a( k, k ) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + cabs1( a( k, k ) )*xk
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_zsytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 call stdlib_zaxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**t).
                    call stdlib_zsytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_zsytrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_zsyrfs




     pure module subroutine stdlib_ssyequb( uplo, n, a, lda, s, scond, amax, work, info )
     !! SSYEQUB computes row and column scalings intended to equilibrate a
     !! symmetric matrix A (with respect to the Euclidean norm) and reduce
     !! its condition number. The scale factors S are computed by the BIN
     !! algorithm (see references) so that the scaled matrix B with elements
     !! B(i,j) = S(i)*A(i,j)*S(j) has a condition number within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: amax, scond
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: s(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: max_iter = 100_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, j, iter
           real(sp) :: avg, std, tol, c0, c1, c2, t, u, si, d, base, smin, smax, smlnum, bignum, &
                     scale, sumsq
           logical(lk) :: up
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if ( .not. ( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -1_ilp
           else if ( n < 0_ilp ) then
              info = -2_ilp
           else if ( lda < max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if ( info /= 0_ilp ) then
              call stdlib_xerbla( 'SSYEQUB', -info )
              return
           end if
           up = stdlib_lsame( uplo, 'U' )
           amax = zero
           ! quick return if possible.
           if ( n == 0_ilp ) then
              scond = one
              return
           end if
           do i = 1, n
              s( i ) = zero
           end do
           amax = zero
           if ( up ) then
              do j = 1, n
                 do i = 1, j-1
                    s( i ) = max( s( i ), abs( a( i, j ) ) )
                    s( j ) = max( s( j ), abs( a( i, j ) ) )
                    amax = max( amax, abs( a( i, j ) ) )
                 end do
                 s( j ) = max( s( j ), abs( a( j, j ) ) )
                 amax = max( amax, abs( a( j, j ) ) )
              end do
           else
              do j = 1, n
                 s( j ) = max( s( j ), abs( a( j, j ) ) )
                 amax = max( amax, abs( a( j, j ) ) )
                 do i = j+1, n
                    s( i ) = max( s( i ), abs( a( i, j ) ) )
                    s( j ) = max( s( j ), abs( a( i, j ) ) )
                    amax = max( amax, abs( a( i, j ) ) )
                 end do
              end do
           end if
           do j = 1, n
              s( j ) = one / s( j )
           end do
           tol = one / sqrt( two * n )
           do iter = 1, max_iter
              scale = zero
              sumsq = zero
              ! beta = |a|s
              do i = 1, n
                 work( i ) = zero
              end do
              if ( up ) then
                 do j = 1, n
                    do i = 1, j-1
                       work( i ) = work( i ) + abs( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + abs( a( i, j ) ) * s( i )
                    end do
                    work( j ) = work( j ) + abs( a( j, j ) ) * s( j )
                 end do
              else
                 do j = 1, n
                    work( j ) = work( j ) + abs( a( j, j ) ) * s( j )
                    do i = j+1, n
                       work( i ) = work( i ) + abs( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + abs( a( i, j ) ) * s( i )
                    end do
                 end do
              end if
              ! avg = s^t beta / n
              avg = zero
              do i = 1, n
                 avg = avg + s( i )*work( i )
              end do
              avg = avg / n
              std = zero
              do i = n+1, 2*n
                 work( i ) = s( i-n ) * work( i-n ) - avg
              end do
              call stdlib_slassq( n, work( n+1 ), 1_ilp, scale, sumsq )
              std = scale * sqrt( sumsq / n )
              if ( std < tol * avg ) goto 999
              do i = 1, n
                 t = abs( a( i, i ) )
                 si = s( i )
                 c2 = ( n-1 ) * t
                 c1 = ( n-2 ) * ( work( i ) - t*si )
                 c0 = -(t*si)*si + 2_ilp*work( i )*si - n*avg
                 d = c1*c1 - 4_ilp*c0*c2
                 if ( d <= 0_ilp ) then
                    info = -1_ilp
                    return
                 end if
                 si = -2_ilp*c0 / ( c1 + sqrt( d ) )
                 d = si - s( i )
                 u = zero
                 if ( up ) then
                    do j = 1, i
                       t = abs( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = abs( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 else
                    do j = 1, i
                       t = abs( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = abs( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 end if
                 avg = avg + ( u + work( i ) ) * d / n
                 s( i ) = si
              end do
           end do
           999 continue
           smlnum = stdlib_slamch( 'SAFEMIN' )
           bignum = one / smlnum
           smin = bignum
           smax = zero
           t = one / sqrt( avg )
           base = stdlib_slamch( 'B' )
           u = one / log( base )
           do i = 1, n
              s( i ) = base ** int( u * log( s( i ) * t ),KIND=ilp)
              smin = min( smin, s( i ) )
              smax = max( smax, s( i ) )
           end do
           scond = max( smin, smlnum ) / min( smax, bignum )
     end subroutine stdlib_ssyequb

     pure module subroutine stdlib_dsyequb( uplo, n, a, lda, s, scond, amax, work, info )
     !! DSYEQUB computes row and column scalings intended to equilibrate a
     !! symmetric matrix A (with respect to the Euclidean norm) and reduce
     !! its condition number. The scale factors S are computed by the BIN
     !! algorithm (see references) so that the scaled matrix B with elements
     !! B(i,j) = S(i)*A(i,j)*S(j) has a condition number within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: amax, scond
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: s(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: max_iter = 100_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, j, iter
           real(dp) :: avg, std, tol, c0, c1, c2, t, u, si, d, base, smin, smax, smlnum, bignum, &
                     scale, sumsq
           logical(lk) :: up
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if ( .not. ( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -1_ilp
           else if ( n < 0_ilp ) then
              info = -2_ilp
           else if ( lda < max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if ( info /= 0_ilp ) then
              call stdlib_xerbla( 'DSYEQUB', -info )
              return
           end if
           up = stdlib_lsame( uplo, 'U' )
           amax = zero
           ! quick return if possible.
           if ( n == 0_ilp ) then
              scond = one
              return
           end if
           do i = 1, n
              s( i ) = zero
           end do
           amax = zero
           if ( up ) then
              do j = 1, n
                 do i = 1, j-1
                    s( i ) = max( s( i ), abs( a( i, j ) ) )
                    s( j ) = max( s( j ), abs( a( i, j ) ) )
                    amax = max( amax, abs( a( i, j ) ) )
                 end do
                 s( j ) = max( s( j ), abs( a( j, j ) ) )
                 amax = max( amax, abs( a( j, j ) ) )
              end do
           else
              do j = 1, n
                 s( j ) = max( s( j ), abs( a( j, j ) ) )
                 amax = max( amax, abs( a( j, j ) ) )
                 do i = j+1, n
                    s( i ) = max( s( i ), abs( a( i, j ) ) )
                    s( j ) = max( s( j ), abs( a( i, j ) ) )
                    amax = max( amax, abs( a( i, j ) ) )
                 end do
              end do
           end if
           do j = 1, n
              s( j ) = one / s( j )
           end do
           tol = one / sqrt( two * n )
           do iter = 1, max_iter
              scale = zero
              sumsq = zero
              ! beta = |a|s
              do i = 1, n
                 work( i ) = zero
              end do
              if ( up ) then
                 do j = 1, n
                    do i = 1, j-1
                       work( i ) = work( i ) + abs( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + abs( a( i, j ) ) * s( i )
                    end do
                    work( j ) = work( j ) + abs( a( j, j ) ) * s( j )
                 end do
              else
                 do j = 1, n
                    work( j ) = work( j ) + abs( a( j, j ) ) * s( j )
                    do i = j+1, n
                       work( i ) = work( i ) + abs( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + abs( a( i, j ) ) * s( i )
                    end do
                 end do
              end if
              ! avg = s^t beta / n
              avg = zero
              do i = 1, n
                 avg = avg + s( i )*work( i )
              end do
              avg = avg / n
              std = zero
              do i = n+1, 2*n
                 work( i ) = s( i-n ) * work( i-n ) - avg
              end do
              call stdlib_dlassq( n, work( n+1 ), 1_ilp, scale, sumsq )
              std = scale * sqrt( sumsq / n )
              if ( std < tol * avg ) goto 999
              do i = 1, n
                 t = abs( a( i, i ) )
                 si = s( i )
                 c2 = ( n-1 ) * t
                 c1 = ( n-2 ) * ( work( i ) - t*si )
                 c0 = -(t*si)*si + 2_ilp*work( i )*si - n*avg
                 d = c1*c1 - 4_ilp*c0*c2
                 if ( d <= 0_ilp ) then
                    info = -1_ilp
                    return
                 end if
                 si = -2_ilp*c0 / ( c1 + sqrt( d ) )
                 d = si - s( i )
                 u = zero
                 if ( up ) then
                    do j = 1, i
                       t = abs( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = abs( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 else
                    do j = 1, i
                       t = abs( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = abs( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 end if
                 avg = avg + ( u + work( i ) ) * d / n
                 s( i ) = si
              end do
           end do
           999 continue
           smlnum = stdlib_dlamch( 'SAFEMIN' )
           bignum = one / smlnum
           smin = bignum
           smax = zero
           t = one / sqrt( avg )
           base = stdlib_dlamch( 'B' )
           u = one / log( base )
           do i = 1, n
              s( i ) = base ** int( u * log( s( i ) * t ),KIND=ilp)
              smin = min( smin, s( i ) )
              smax = max( smax, s( i ) )
           end do
           scond = max( smin, smlnum ) / min( smax, bignum )
     end subroutine stdlib_dsyequb


     pure module subroutine stdlib_csyequb( uplo, n, a, lda, s, scond, amax, work, info )
     !! CSYEQUB computes row and column scalings intended to equilibrate a
     !! symmetric matrix A (with respect to the Euclidean norm) and reduce
     !! its condition number. The scale factors S are computed by the BIN
     !! algorithm (see references) so that the scaled matrix B with elements
     !! B(i,j) = S(i)*A(i,j)*S(j) has a condition number within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: amax, scond
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(out) :: s(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: max_iter = 100_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, j, iter
           real(sp) :: avg, std, tol, c0, c1, c2, t, u, si, d, base, smin, smax, smlnum, bignum, &
                     scale, sumsq
           logical(lk) :: up
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if ( .not. ( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -1_ilp
           else if ( n < 0_ilp ) then
              info = -2_ilp
           else if ( lda < max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if ( info /= 0_ilp ) then
              call stdlib_xerbla( 'CSYEQUB', -info )
              return
           end if
           up = stdlib_lsame( uplo, 'U' )
           amax = zero
           ! quick return if possible.
           if ( n == 0_ilp ) then
              scond = one
              return
           end if
           do i = 1, n
              s( i ) = zero
           end do
           amax = zero
           if ( up ) then
              do j = 1, n
                 do i = 1, j-1
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
              end do
           else
              do j = 1, n
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
                 do i = j+1, n
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
              end do
           end if
           do j = 1, n
              s( j ) = one / s( j )
           end do
           tol = one / sqrt( two * n )
           do iter = 1, max_iter
              scale = zero
              sumsq = zero
              ! beta = |a|s
              do i = 1, n
                  work( i ) = zero
              end do
              if ( up ) then
                 do j = 1, n
                    do i = 1, j-1
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                 end do
              else
                 do j = 1, n
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                    do i = j+1, n
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                 end do
              end if
              ! avg = s^t beta / n
              avg = zero
              do i = 1, n
                 avg = avg + real( s( i )*work( i ),KIND=sp)
              end do
              avg = avg / n
              std = zero
              do i = n+1, 2*n
                 work( i ) = s( i-n ) * work( i-n ) - avg
              end do
              call stdlib_classq( n, work( n+1 ), 1_ilp, scale, sumsq )
              std = scale * sqrt( sumsq / n )
              if ( std < tol * avg ) goto 999
              do i = 1, n
                 t = cabs1( a( i, i ) )
                 si = s( i )
                 c2 = ( n-1 ) * t
                 c1 = real( n-2,KIND=sp) * ( real( work( i ),KIND=sp) - t*si )
                 c0 = -(t*si)*si + 2_ilp * real( work( i ),KIND=sp) * si - n*avg
                 d = c1*c1 - 4_ilp*c0*c2
                 if ( d <= 0_ilp ) then
                    info = -1_ilp
                    return
                 end if
                 si = -2_ilp*c0 / ( c1 + sqrt( d ) )
                 d = si - s( i )
                 u = zero
                 if ( up ) then
                    do j = 1, i
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 else
                    do j = 1, i
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 end if
                 avg = avg + ( u + real( work( i ),KIND=sp) ) * d / n
                 s( i ) = si
              end do
           end do
           999 continue
           smlnum = stdlib_slamch( 'SAFEMIN' )
           bignum = one / smlnum
           smin = bignum
           smax = zero
           t = one / sqrt( avg )
           base = stdlib_slamch( 'B' )
           u = one / log( base )
           do i = 1, n
              s( i ) = base ** int( u * log( s( i ) * t ),KIND=ilp)
              smin = min( smin, s( i ) )
              smax = max( smax, s( i ) )
           end do
           scond = max( smin, smlnum ) / min( smax, bignum )
     end subroutine stdlib_csyequb

     pure module subroutine stdlib_zsyequb( uplo, n, a, lda, s, scond, amax, work, info )
     !! ZSYEQUB computes row and column scalings intended to equilibrate a
     !! symmetric matrix A (with respect to the Euclidean norm) and reduce
     !! its condition number. The scale factors S are computed by the BIN
     !! algorithm (see references) so that the scaled matrix B with elements
     !! B(i,j) = S(i)*A(i,j)*S(j) has a condition number within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: amax, scond
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(out) :: s(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: max_iter = 100_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, j, iter
           real(dp) :: avg, std, tol, c0, c1, c2, t, u, si, d, base, smin, smax, smlnum, bignum, &
                     scale, sumsq
           logical(lk) :: up
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if ( .not. ( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -1_ilp
           else if ( n < 0_ilp ) then
              info = -2_ilp
           else if ( lda < max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if ( info /= 0_ilp ) then
              call stdlib_xerbla( 'ZSYEQUB', -info )
              return
           end if
           up = stdlib_lsame( uplo, 'U' )
           amax = zero
           ! quick return if possible.
           if ( n == 0_ilp ) then
              scond = one
              return
           end if
           do i = 1, n
              s( i ) = zero
           end do
           amax = zero
           if ( up ) then
              do j = 1, n
                 do i = 1, j-1
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
              end do
           else
              do j = 1, n
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
                 do i = j+1, n
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
              end do
           end if
           do j = 1, n
              s( j ) = one / s( j )
           end do
           tol = one / sqrt( two * n )
           do iter = 1, max_iter
              scale = zero
              sumsq = zero
              ! beta = |a|s
              do i = 1, n
                 work( i ) = zero
              end do
              if ( up ) then
                 do j = 1, n
                    do i = 1, j-1
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                 end do
              else
                 do j = 1, n
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                    do i = j+1, n
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                 end do
              end if
              ! avg = s^t beta / n
              avg = zero
              do i = 1, n
                 avg = avg + s( i ) * real( work( i ),KIND=dp)
              end do
              avg = avg / n
              std = zero
              do i = n+1, 2*n
                 work( i ) = s( i-n ) * work( i-n ) - avg
              end do
              call stdlib_zlassq( n, work( n+1 ), 1_ilp, scale, sumsq )
              std = scale * sqrt( sumsq / n )
              if ( std < tol * avg ) goto 999
              do i = 1, n
                 t = cabs1( a( i, i ) )
                 si = s( i )
                 c2 = ( n-1 ) * t
                 c1 = ( n-2 ) * ( real( work( i ),KIND=dp) - t*si )
                 c0 = -(t*si)*si + 2_ilp * real( work( i ),KIND=dp) * si - n*avg
                 d = c1*c1 - 4_ilp*c0*c2
                 if ( d <= 0_ilp ) then
                    info = -1_ilp
                    return
                 end if
                 si = -2_ilp*c0 / ( c1 + sqrt( d ) )
                 d = si - s( i )
                 u = zero
                 if ( up ) then
                    do j = 1, i
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 else
                    do j = 1, i
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 end if
                 avg = avg + ( u + real( work( i ),KIND=dp) ) * d / n
                 s( i ) = si
              end do
           end do
           999 continue
           smlnum = stdlib_dlamch( 'SAFEMIN' )
           bignum = one / smlnum
           smin = bignum
           smax = zero
           t = one / sqrt( avg )
           base = stdlib_dlamch( 'B' )
           u = one / log( base )
           do i = 1, n
              s( i ) = base ** int( u * log( s( i ) * t ),KIND=ilp)
              smin = min( smin, s( i ) )
              smax = max( smax, s( i ) )
           end do
           scond = max( smin, smlnum ) / min( smax, bignum )
     end subroutine stdlib_zsyequb




     pure module subroutine stdlib_ssyconv( uplo, way, n, a, lda, ipiv, e, info )
     !! SSYCONV convert A given by TRF into L and D and vice-versa.
     !! Get Non-diag elements of D (returned in workspace) and
     !! apply or reverse permutation done in TRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip, j
           real(sp) :: temp
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYCONV', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
            ! a is upper
            ! convert a (a is upper)
              ! convert value
              if ( convert ) then
                 i=n
                 e(1_ilp)=zero
                 do while ( i > 1 )
                    if( ipiv(i) < 0_ilp ) then
                       e(i)=a(i-1,i)
                       e(i-1)=zero
                       a(i-1,i)=zero
                       i=i-1
                    else
                       e(i)=zero
                    endif
                    i=i-1
                 end do
              ! convert permutations
              i=n
              do while ( i >= 1 )
                 if( ipiv(i) > 0_ilp) then
                    ip=ipiv(i)
                    if( i < n) then
                       do j= i+1,n
                         temp=a(ip,j)
                         a(ip,j)=a(i,j)
                         a(i,j)=temp
                       end do
                    endif
                 else
                   ip=-ipiv(i)
                    if( i < n) then
                  do j= i+1,n
                      temp=a(ip,j)
                      a(ip,j)=a(i-1,j)
                      a(i-1,j)=temp
                  end do
                     endif
                     i=i-1
                endif
                i=i-1
             end do
              else
            ! revert a (a is upper)
              ! revert permutations
                 i=1_ilp
                 do while ( i <= n )
                    if( ipiv(i) > 0_ilp ) then
                       ip=ipiv(i)
                       if( i < n) then
                       do j= i+1,n
                         temp=a(ip,j)
                         a(ip,j)=a(i,j)
                         a(i,j)=temp
                       end do
                       endif
                    else
                      ip=-ipiv(i)
                      i=i+1
                      if( i < n) then
                         do j= i+1,n
                            temp=a(ip,j)
                            a(ip,j)=a(i-1,j)
                            a(i-1,j)=temp
                         end do
                      endif
                    endif
                    i=i+1
                 end do
              ! revert value
                 i=n
                 do while ( i > 1 )
                    if( ipiv(i) < 0_ilp ) then
                       a(i-1,i)=e(i)
                       i=i-1
                    endif
                    i=i-1
                 end do
              end if
           else
            ! a is lower
              if ( convert ) then
            ! convert a (a is lower)
              ! convert value
                 i=1_ilp
                 e(n)=zero
                 do while ( i <= n )
                    if( i<n .and. ipiv(i) < 0_ilp ) then
                       e(i)=a(i+1,i)
                       e(i+1)=zero
                       a(i+1,i)=zero
                       i=i+1
                    else
                       e(i)=zero
                    endif
                    i=i+1
                 end do
              ! convert permutations
              i=1_ilp
              do while ( i <= n )
                 if( ipiv(i) > 0_ilp ) then
                    ip=ipiv(i)
                    if (i > 1_ilp) then
                    do j= 1,i-1
                      temp=a(ip,j)
                      a(ip,j)=a(i,j)
                      a(i,j)=temp
                    end do
                    endif
                 else
                   ip=-ipiv(i)
                   if (i > 1_ilp) then
                   do j= 1,i-1
                      temp=a(ip,j)
                      a(ip,j)=a(i+1,j)
                      a(i+1,j)=temp
                   end do
                   endif
                   i=i+1
                endif
                i=i+1
             end do
              else
            ! revert a (a is lower)
              ! revert permutations
                 i=n
                 do while ( i >= 1 )
                    if( ipiv(i) > 0_ilp ) then
                       ip=ipiv(i)
                       if (i > 1_ilp) then
                          do j= 1,i-1
                             temp=a(i,j)
                             a(i,j)=a(ip,j)
                             a(ip,j)=temp
                          end do
                       endif
                    else
                       ip=-ipiv(i)
                       i=i-1
                       if (i > 1_ilp) then
                          do j= 1,i-1
                             temp=a(i+1,j)
                             a(i+1,j)=a(ip,j)
                             a(ip,j)=temp
                          end do
                       endif
                    endif
                    i=i-1
                 end do
              ! revert value
                 i=1_ilp
                 do while ( i <= n-1 )
                    if( ipiv(i) < 0_ilp ) then
                       a(i+1,i)=e(i)
                       i=i+1
                    endif
                    i=i+1
                 end do
              end if
           end if
           return
     end subroutine stdlib_ssyconv

     pure module subroutine stdlib_dsyconv( uplo, way, n, a, lda, ipiv, e, info )
     !! DSYCONV convert A given by TRF into L and D and vice-versa.
     !! Get Non-diag elements of D (returned in workspace) and
     !! apply or reverse permutation done in TRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip, j
           real(dp) :: temp
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYCONV', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
            ! a is upper
            ! convert a (a is upper)
              ! convert value
              if ( convert ) then
                 i=n
                 e(1_ilp)=zero
                 do while ( i > 1 )
                    if( ipiv(i) < 0_ilp ) then
                       e(i)=a(i-1,i)
                       e(i-1)=zero
                       a(i-1,i)=zero
                       i=i-1
                    else
                       e(i)=zero
                    endif
                    i=i-1
                 end do
              ! convert permutations
              i=n
              do while ( i >= 1 )
                 if( ipiv(i) > 0_ilp) then
                    ip=ipiv(i)
                    if( i < n) then
                       do j= i+1,n
                         temp=a(ip,j)
                         a(ip,j)=a(i,j)
                         a(i,j)=temp
                       end do
                    endif
                 else
                   ip=-ipiv(i)
                    if( i < n) then
                  do j= i+1,n
                      temp=a(ip,j)
                      a(ip,j)=a(i-1,j)
                      a(i-1,j)=temp
                  end do
                     endif
                     i=i-1
                endif
                i=i-1
             end do
              else
            ! revert a (a is upper)
              ! revert permutations
                 i=1_ilp
                 do while ( i <= n )
                    if( ipiv(i) > 0_ilp ) then
                       ip=ipiv(i)
                       if( i < n) then
                       do j= i+1,n
                         temp=a(ip,j)
                         a(ip,j)=a(i,j)
                         a(i,j)=temp
                       end do
                       endif
                    else
                      ip=-ipiv(i)
                      i=i+1
                      if( i < n) then
                         do j= i+1,n
                            temp=a(ip,j)
                            a(ip,j)=a(i-1,j)
                            a(i-1,j)=temp
                         end do
                      endif
                    endif
                    i=i+1
                 end do
              ! revert value
                 i=n
                 do while ( i > 1 )
                    if( ipiv(i) < 0_ilp ) then
                       a(i-1,i)=e(i)
                       i=i-1
                    endif
                    i=i-1
                 end do
              end if
           else
            ! a is lower
              if ( convert ) then
            ! convert a (a is lower)
              ! convert value
                 i=1_ilp
                 e(n)=zero
                 do while ( i <= n )
                    if( i<n .and. ipiv(i) < 0_ilp ) then
                       e(i)=a(i+1,i)
                       e(i+1)=zero
                       a(i+1,i)=zero
                       i=i+1
                    else
                       e(i)=zero
                    endif
                    i=i+1
                 end do
              ! convert permutations
              i=1_ilp
              do while ( i <= n )
                 if( ipiv(i) > 0_ilp ) then
                    ip=ipiv(i)
                    if (i > 1_ilp) then
                    do j= 1,i-1
                      temp=a(ip,j)
                      a(ip,j)=a(i,j)
                      a(i,j)=temp
                    end do
                    endif
                 else
                   ip=-ipiv(i)
                   if (i > 1_ilp) then
                   do j= 1,i-1
                      temp=a(ip,j)
                      a(ip,j)=a(i+1,j)
                      a(i+1,j)=temp
                   end do
                   endif
                   i=i+1
                endif
                i=i+1
             end do
              else
            ! revert a (a is lower)
              ! revert permutations
                 i=n
                 do while ( i >= 1 )
                    if( ipiv(i) > 0_ilp ) then
                       ip=ipiv(i)
                       if (i > 1_ilp) then
                          do j= 1,i-1
                             temp=a(i,j)
                             a(i,j)=a(ip,j)
                             a(ip,j)=temp
                          end do
                       endif
                    else
                       ip=-ipiv(i)
                       i=i-1
                       if (i > 1_ilp) then
                          do j= 1,i-1
                             temp=a(i+1,j)
                             a(i+1,j)=a(ip,j)
                             a(ip,j)=temp
                          end do
                       endif
                    endif
                    i=i-1
                 end do
              ! revert value
                 i=1_ilp
                 do while ( i <= n-1 )
                    if( ipiv(i) < 0_ilp ) then
                       a(i+1,i)=e(i)
                       i=i+1
                    endif
                    i=i+1
                 end do
              end if
           end if
           return
     end subroutine stdlib_dsyconv


     pure module subroutine stdlib_csyconv( uplo, way, n, a, lda, ipiv, e, info )
     !! CSYCONV convert A given by TRF into L and D and vice-versa.
     !! Get Non-diag elements of D (returned in workspace) and
     !! apply or reverse permutation done in TRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip, j
           complex(sp) :: temp
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYCONV', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
            ! a is upper
            ! convert a (a is upper)
              ! convert value
              if ( convert ) then
                 i=n
                 e(1_ilp)=czero
                 do while ( i > 1 )
                    if( ipiv(i) < 0_ilp ) then
                       e(i)=a(i-1,i)
                       e(i-1)=czero
                       a(i-1,i)=czero
                       i=i-1
                    else
                       e(i)=czero
                    endif
                    i=i-1
                 end do
              ! convert permutations
              i=n
              do while ( i >= 1 )
                 if( ipiv(i) > 0_ilp) then
                    ip=ipiv(i)
                    if( i < n) then
                       do j= i+1,n
                         temp=a(ip,j)
                         a(ip,j)=a(i,j)
                         a(i,j)=temp
                       end do
                    endif
                 else
                   ip=-ipiv(i)
                    if( i < n) then
                  do j= i+1,n
                      temp=a(ip,j)
                      a(ip,j)=a(i-1,j)
                      a(i-1,j)=temp
                  end do
                     endif
                     i=i-1
                endif
                i=i-1
             end do
              else
            ! revert a (a is upper)
              ! revert permutations
                 i=1_ilp
                 do while ( i <= n )
                    if( ipiv(i) > 0_ilp ) then
                       ip=ipiv(i)
                       if( i < n) then
                       do j= i+1,n
                         temp=a(ip,j)
                         a(ip,j)=a(i,j)
                         a(i,j)=temp
                       end do
                       endif
                    else
                      ip=-ipiv(i)
                      i=i+1
                      if( i < n) then
                         do j= i+1,n
                            temp=a(ip,j)
                            a(ip,j)=a(i-1,j)
                            a(i-1,j)=temp
                         end do
                      endif
                    endif
                    i=i+1
                 end do
              ! revert value
                 i=n
                 do while ( i > 1 )
                    if( ipiv(i) < 0_ilp ) then
                       a(i-1,i)=e(i)
                       i=i-1
                    endif
                    i=i-1
                 end do
              end if
           else
            ! a is lower
              if ( convert ) then
            ! convert a (a is lower)
              ! convert value
                 i=1_ilp
                 e(n)=czero
                 do while ( i <= n )
                    if( i<n .and. ipiv(i) < 0_ilp ) then
                       e(i)=a(i+1,i)
                       e(i+1)=czero
                       a(i+1,i)=czero
                       i=i+1
                    else
                       e(i)=czero
                    endif
                    i=i+1
                 end do
              ! convert permutations
              i=1_ilp
              do while ( i <= n )
                 if( ipiv(i) > 0_ilp ) then
                    ip=ipiv(i)
                    if (i > 1_ilp) then
                    do j= 1,i-1
                      temp=a(ip,j)
                      a(ip,j)=a(i,j)
                      a(i,j)=temp
                    end do
                    endif
                 else
                   ip=-ipiv(i)
                   if (i > 1_ilp) then
                   do j= 1,i-1
                      temp=a(ip,j)
                      a(ip,j)=a(i+1,j)
                      a(i+1,j)=temp
                   end do
                   endif
                   i=i+1
                endif
                i=i+1
             end do
              else
            ! revert a (a is lower)
              ! revert permutations
                 i=n
                 do while ( i >= 1 )
                    if( ipiv(i) > 0_ilp ) then
                       ip=ipiv(i)
                       if (i > 1_ilp) then
                          do j= 1,i-1
                             temp=a(i,j)
                             a(i,j)=a(ip,j)
                             a(ip,j)=temp
                          end do
                       endif
                    else
                       ip=-ipiv(i)
                       i=i-1
                       if (i > 1_ilp) then
                          do j= 1,i-1
                             temp=a(i+1,j)
                             a(i+1,j)=a(ip,j)
                             a(ip,j)=temp
                          end do
                       endif
                    endif
                    i=i-1
                 end do
              ! revert value
                 i=1_ilp
                 do while ( i <= n-1 )
                    if( ipiv(i) < 0_ilp ) then
                       a(i+1,i)=e(i)
                       i=i+1
                    endif
                    i=i+1
                 end do
              end if
           end if
           return
     end subroutine stdlib_csyconv

     pure module subroutine stdlib_zsyconv( uplo, way, n, a, lda, ipiv, e, info )
     !! ZSYCONV converts A given by ZHETRF into L and D or vice-versa.
     !! Get nondiagonal elements of D (returned in workspace) and
     !! apply or reverse permutation done in TRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, way
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: e(*)
        ! =====================================================================
           
           ! External Subroutines 
           logical(lk) :: upper, convert
           integer(ilp) :: i, ip, j
           complex(dp) :: temp
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           convert = stdlib_lsame( way, 'C' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.convert .and. .not.stdlib_lsame( way, 'R' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYCONV', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! a is upper
              if ( convert ) then
                 ! convert a (a is upper)
                 ! convert value
                 i=n
                 e(1_ilp)=czero
                 do while ( i > 1 )
                    if( ipiv(i) < 0_ilp ) then
                       e(i)=a(i-1,i)
                       e(i-1)=czero
                       a(i-1,i)=czero
                       i=i-1
                    else
                       e(i)=czero
                    endif
                    i=i-1
                 end do
                 ! convert permutations
                 i=n
                 do while ( i >= 1 )
                    if( ipiv(i) > 0_ilp) then
                       ip=ipiv(i)
                       if( i < n) then
                          do j= i+1,n
                            temp=a(ip,j)
                            a(ip,j)=a(i,j)
                            a(i,j)=temp
                          end do
                       endif
                    else
                       ip=-ipiv(i)
                       if( i < n) then
                          do j= i+1,n
                             temp=a(ip,j)
                             a(ip,j)=a(i-1,j)
                             a(i-1,j)=temp
                          end do
                       endif
                       i=i-1
                    endif
                    i=i-1
                 end do
              else
                 ! revert a (a is upper)
                 ! revert permutations
                 i=1_ilp
                 do while ( i <= n )
                    if( ipiv(i) > 0_ilp ) then
                       ip=ipiv(i)
                       if( i < n) then
                       do j= i+1,n
                         temp=a(ip,j)
                         a(ip,j)=a(i,j)
                         a(i,j)=temp
                       end do
                       endif
                    else
                      ip=-ipiv(i)
                      i=i+1
                      if( i < n) then
                         do j= i+1,n
                            temp=a(ip,j)
                            a(ip,j)=a(i-1,j)
                            a(i-1,j)=temp
                         end do
                      endif
                    endif
                    i=i+1
                 end do
                 ! revert value
                 i=n
                 do while ( i > 1 )
                    if( ipiv(i) < 0_ilp ) then
                       a(i-1,i)=e(i)
                       i=i-1
                    endif
                    i=i-1
                 end do
              end if
           else
              ! a is lower
              if ( convert ) then
                 ! convert a (a is lower)
                 ! convert value
                 i=1_ilp
                 e(n)=czero
                 do while ( i <= n )
                    if( i<n .and. ipiv(i) < 0_ilp ) then
                       e(i)=a(i+1,i)
                       e(i+1)=czero
                       a(i+1,i)=czero
                       i=i+1
                    else
                       e(i)=czero
                    endif
                    i=i+1
                 end do
                 ! convert permutations
                 i=1_ilp
                 do while ( i <= n )
                    if( ipiv(i) > 0_ilp ) then
                       ip=ipiv(i)
                       if (i > 1_ilp) then
                          do j= 1,i-1
                             temp=a(ip,j)
                             a(ip,j)=a(i,j)
                             a(i,j)=temp
                          end do
                       endif
                    else
                       ip=-ipiv(i)
                       if (i > 1_ilp) then
                          do j= 1,i-1
                             temp=a(ip,j)
                             a(ip,j)=a(i+1,j)
                             a(i+1,j)=temp
                          end do
                       endif
                       i=i+1
                    endif
                    i=i+1
                 end do
              else
                 ! revert a (a is lower)
                 ! revert permutations
                 i=n
                 do while ( i >= 1 )
                    if( ipiv(i) > 0_ilp ) then
                       ip=ipiv(i)
                       if (i > 1_ilp) then
                          do j= 1,i-1
                             temp=a(i,j)
                             a(i,j)=a(ip,j)
                             a(ip,j)=temp
                          end do
                       endif
                    else
                       ip=-ipiv(i)
                       i=i-1
                       if (i > 1_ilp) then
                          do j= 1,i-1
                             temp=a(i+1,j)
                             a(i+1,j)=a(ip,j)
                             a(ip,j)=temp
                          end do
                       endif
                    endif
                    i=i-1
                 end do
                 ! revert value
                 i=1_ilp
                 do while ( i <= n-1 )
                    if( ipiv(i) < 0_ilp ) then
                       a(i+1,i)=e(i)
                       i=i+1
                    endif
                    i=i+1
                 end do
              end if
           end if
           return
     end subroutine stdlib_zsyconv




     pure module subroutine stdlib_ssytrs2( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, info )
     !! SSYTRS2 solves a system of linear equations A*X = B with a real
     !! symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by SSYTRF and converted by SSYCONV.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, iinfo, j, k, kp
           real(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRS2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! convert a
           call stdlib_ssyconv( uplo, 'C', n, a, lda, ipiv, work, iinfo )
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
             ! p**t * b
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp==-ipiv( k-1 ) )call stdlib_sswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb &
                           )
                 k=k-2
              end if
             end do
        ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
             call stdlib_strsm('L','U','N','U',n,nrhs,one,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i=n
              do while ( i >= 1 )
                 if( ipiv(i) > 0_ilp ) then
                   call stdlib_sscal( nrhs, one / a( i, i ), b( i, 1_ilp ), ldb )
                 elseif ( i > 1_ilp) then
                    if ( ipiv(i-1) == ipiv(i) ) then
                       akm1k = work(i)
                       akm1 = a( i-1, i-1 ) / akm1k
                       ak = a( i, i ) / akm1k
                       denom = akm1*ak - one
                       do j = 1, nrhs
                          bkm1 = b( i-1, j ) / akm1k
                          bk = b( i, j ) / akm1k
                          b( i-1, j ) = ( ak*bkm1-bk ) / denom
                          b( i, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                    i = i - 1_ilp
                    endif
                 endif
                 i = i - 1_ilp
              end do
            ! compute (u**t \ b) -> b   [ u**t \ (d \ (u \p**t * b) ) ]
              call stdlib_strsm('L','U','T','U',n,nrhs,one,a,lda,b,ldb)
             ! p * b  [ p * (u**t \ (d \ (u \p**t * b) )) ]
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k < n .and. kp==-ipiv( k+1 ) )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp,&
                            1_ilp ), ldb )
                 k=k+2
              endif
             end do
           else
              ! solve a*x = b, where a = l*d*l**t.
             ! p**t * b
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k+1).
                 kp = -ipiv( k+1 )
                 if( kp==-ipiv( k ) )call stdlib_sswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                           
                 k=k+2
              endif
             end do
        ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
             call stdlib_strsm('L','L','N','U',n,nrhs,one,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i=1_ilp
              do while ( i <= n )
                 if( ipiv(i) > 0_ilp ) then
                   call stdlib_sscal( nrhs, one / a( i, i ), b( i, 1_ilp ), ldb )
                 else
                       akm1k = work(i)
                       akm1 = a( i, i ) / akm1k
                       ak = a( i+1, i+1 ) / akm1k
                       denom = akm1*ak - one
                       do j = 1, nrhs
                          bkm1 = b( i, j ) / akm1k
                          bk = b( i+1, j ) / akm1k
                          b( i, j ) = ( ak*bkm1-bk ) / denom
                          b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                       i = i + 1_ilp
                 endif
                 i = i + 1_ilp
              end do
        ! compute (l**t \ b) -> b   [ l**t \ (d \ (l \p**t * b) ) ]
             call stdlib_strsm('L','L','T','U',n,nrhs,one,a,lda,b,ldb)
             ! p * b  [ p * (l**t \ (d \ (l \p**t * b) )) ]
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k>1_ilp .and. kp==-ipiv( k-1 ) )call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, &
                           1_ilp ), ldb )
                 k=k-2
              endif
             end do
           end if
           ! revert a
           call stdlib_ssyconv( uplo, 'R', n, a, lda, ipiv, work, iinfo )
           return
     end subroutine stdlib_ssytrs2

     pure module subroutine stdlib_dsytrs2( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, info )
     !! DSYTRS2 solves a system of linear equations A*X = B with a real
     !! symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by DSYTRF and converted by DSYCONV.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, iinfo, j, k, kp
           real(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRS2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! convert a
           call stdlib_dsyconv( uplo, 'C', n, a, lda, ipiv, work, iinfo )
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
             ! p**t * b
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp==-ipiv( k-1 ) )call stdlib_dswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb &
                           )
                 k=k-2
              end if
             end do
        ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
             call stdlib_dtrsm('L','U','N','U',n,nrhs,one,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i=n
              do while ( i >= 1 )
                 if( ipiv(i) > 0_ilp ) then
                   call stdlib_dscal( nrhs, one / a( i, i ), b( i, 1_ilp ), ldb )
                 elseif ( i > 1_ilp) then
                    if ( ipiv(i-1) == ipiv(i) ) then
                       akm1k = work(i)
                       akm1 = a( i-1, i-1 ) / akm1k
                       ak = a( i, i ) / akm1k
                       denom = akm1*ak - one
                       do j = 1, nrhs
                          bkm1 = b( i-1, j ) / akm1k
                          bk = b( i, j ) / akm1k
                          b( i-1, j ) = ( ak*bkm1-bk ) / denom
                          b( i, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                    i = i - 1_ilp
                    endif
                 endif
                 i = i - 1_ilp
              end do
            ! compute (u**t \ b) -> b   [ u**t \ (d \ (u \p**t * b) ) ]
              call stdlib_dtrsm('L','U','T','U',n,nrhs,one,a,lda,b,ldb)
             ! p * b  [ p * (u**t \ (d \ (u \p**t * b) )) ]
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k < n .and. kp==-ipiv( k+1 ) )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp,&
                            1_ilp ), ldb )
                 k=k+2
              endif
             end do
           else
              ! solve a*x = b, where a = l*d*l**t.
             ! p**t * b
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k+1).
                 kp = -ipiv( k+1 )
                 if( kp==-ipiv( k ) )call stdlib_dswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                           
                 k=k+2
              endif
             end do
        ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
             call stdlib_dtrsm('L','L','N','U',n,nrhs,one,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i=1_ilp
              do while ( i <= n )
                 if( ipiv(i) > 0_ilp ) then
                   call stdlib_dscal( nrhs, one / a( i, i ), b( i, 1_ilp ), ldb )
                 else
                       akm1k = work(i)
                       akm1 = a( i, i ) / akm1k
                       ak = a( i+1, i+1 ) / akm1k
                       denom = akm1*ak - one
                       do j = 1, nrhs
                          bkm1 = b( i, j ) / akm1k
                          bk = b( i+1, j ) / akm1k
                          b( i, j ) = ( ak*bkm1-bk ) / denom
                          b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                       i = i + 1_ilp
                 endif
                 i = i + 1_ilp
              end do
        ! compute (l**t \ b) -> b   [ l**t \ (d \ (l \p**t * b) ) ]
             call stdlib_dtrsm('L','L','T','U',n,nrhs,one,a,lda,b,ldb)
             ! p * b  [ p * (l**t \ (d \ (l \p**t * b) )) ]
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k>1_ilp .and. kp==-ipiv( k-1 ) )call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, &
                           1_ilp ), ldb )
                 k=k-2
              endif
             end do
           end if
           ! revert a
           call stdlib_dsyconv( uplo, 'R', n, a, lda, ipiv, work, iinfo )
           return
     end subroutine stdlib_dsytrs2


     pure module subroutine stdlib_csytrs2( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, info )
     !! CSYTRS2 solves a system of linear equations A*X = B with a complex
     !! symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by CSYTRF and converted by CSYCONV.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, iinfo, j, k, kp
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTRS2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! convert a
           call stdlib_csyconv( uplo, 'C', n, a, lda, ipiv, work, iinfo )
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
             ! p**t * b
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp==-ipiv( k-1 ) )call stdlib_cswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb &
                           )
                 k=k-2
              end if
             end do
        ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
             call stdlib_ctrsm('L','U','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i=n
              do while ( i >= 1 )
                 if( ipiv(i) > 0_ilp ) then
                   call stdlib_cscal( nrhs, cone / a( i, i ), b( i, 1_ilp ), ldb )
                 elseif ( i > 1_ilp) then
                    if ( ipiv(i-1) == ipiv(i) ) then
                       akm1k = work(i)
                       akm1 = a( i-1, i-1 ) / akm1k
                       ak = a( i, i ) / akm1k
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i-1, j ) / akm1k
                          bk = b( i, j ) / akm1k
                          b( i-1, j ) = ( ak*bkm1-bk ) / denom
                          b( i, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                    i = i - 1_ilp
                    endif
                 endif
                 i = i - 1_ilp
              end do
            ! compute (u**t \ b) -> b   [ u**t \ (d \ (u \p**t * b) ) ]
              call stdlib_ctrsm('L','U','T','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (u**t \ (d \ (u \p**t * b) )) ]
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k < n .and. kp==-ipiv( k+1 ) )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp,&
                            1_ilp ), ldb )
                 k=k+2
              endif
             end do
           else
              ! solve a*x = b, where a = l*d*l**t.
             ! p**t * b
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k+1).
                 kp = -ipiv( k+1 )
                 if( kp==-ipiv( k ) )call stdlib_cswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                           
                 k=k+2
              endif
             end do
        ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
             call stdlib_ctrsm('L','L','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i=1_ilp
              do while ( i <= n )
                 if( ipiv(i) > 0_ilp ) then
                   call stdlib_cscal( nrhs, cone / a( i, i ), b( i, 1_ilp ), ldb )
                 else
                       akm1k = work(i)
                       akm1 = a( i, i ) / akm1k
                       ak = a( i+1, i+1 ) / akm1k
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i, j ) / akm1k
                          bk = b( i+1, j ) / akm1k
                          b( i, j ) = ( ak*bkm1-bk ) / denom
                          b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                       i = i + 1_ilp
                 endif
                 i = i + 1_ilp
              end do
        ! compute (l**t \ b) -> b   [ l**t \ (d \ (l \p**t * b) ) ]
             call stdlib_ctrsm('L','L','T','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (l**t \ (d \ (l \p**t * b) )) ]
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k>1_ilp .and. kp==-ipiv( k-1 ) )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, &
                           1_ilp ), ldb )
                 k=k-2
              endif
             end do
           end if
           ! revert a
           call stdlib_csyconv( uplo, 'R', n, a, lda, ipiv, work, iinfo )
           return
     end subroutine stdlib_csytrs2

     pure module subroutine stdlib_zsytrs2( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, info )
     !! ZSYTRS2 solves a system of linear equations A*X = B with a complex
     !! symmetric matrix A using the factorization A = U*D*U**T or
     !! A = L*D*L**T computed by ZSYTRF and converted by ZSYCONV.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, iinfo, j, k, kp
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTRS2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! convert a
           call stdlib_zsyconv( uplo, 'C', n, a, lda, ipiv, work, iinfo )
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**t.
             ! p**t * b
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp==-ipiv( k-1 ) )call stdlib_zswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb &
                           )
                 k=k-2
              end if
             end do
        ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
             call stdlib_ztrsm('L','U','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i=n
              do while ( i >= 1 )
                 if( ipiv(i) > 0_ilp ) then
                   call stdlib_zscal( nrhs, cone / a( i, i ), b( i, 1_ilp ), ldb )
                 elseif ( i > 1_ilp) then
                    if ( ipiv(i-1) == ipiv(i) ) then
                       akm1k = work(i)
                       akm1 = a( i-1, i-1 ) / akm1k
                       ak = a( i, i ) / akm1k
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i-1, j ) / akm1k
                          bk = b( i, j ) / akm1k
                          b( i-1, j ) = ( ak*bkm1-bk ) / denom
                          b( i, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                    i = i - 1_ilp
                    endif
                 endif
                 i = i - 1_ilp
              end do
            ! compute (u**t \ b) -> b   [ u**t \ (d \ (u \p**t * b) ) ]
              call stdlib_ztrsm('L','U','T','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (u**t \ (d \ (u \p**t * b) )) ]
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k < n .and. kp==-ipiv( k+1 ) )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp,&
                            1_ilp ), ldb )
                 k=k+2
              endif
             end do
           else
              ! solve a*x = b, where a = l*d*l**t.
             ! p**t * b
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k+1).
                 kp = -ipiv( k+1 )
                 if( kp==-ipiv( k ) )call stdlib_zswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                           
                 k=k+2
              endif
             end do
        ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
             call stdlib_ztrsm('L','L','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i=1_ilp
              do while ( i <= n )
                 if( ipiv(i) > 0_ilp ) then
                   call stdlib_zscal( nrhs, cone / a( i, i ), b( i, 1_ilp ), ldb )
                 else
                       akm1k = work(i)
                       akm1 = a( i, i ) / akm1k
                       ak = a( i+1, i+1 ) / akm1k
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i, j ) / akm1k
                          bk = b( i+1, j ) / akm1k
                          b( i, j ) = ( ak*bkm1-bk ) / denom
                          b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                       i = i + 1_ilp
                 endif
                 i = i + 1_ilp
              end do
        ! compute (l**t \ b) -> b   [ l**t \ (d \ (l \p**t * b) ) ]
             call stdlib_ztrsm('L','L','T','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (l**t \ (d \ (l \p**t * b) )) ]
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k>1_ilp .and. kp==-ipiv( k-1 ) )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, &
                           1_ilp ), ldb )
                 k=k-2
              endif
             end do
           end if
           ! revert a
           call stdlib_zsyconv( uplo, 'R', n, a, lda, ipiv, work, iinfo )
           return
     end subroutine stdlib_zsytrs2




     pure module subroutine stdlib_ssytrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb,info )
     !! SSYTRS_3 solves a system of linear equations A * X = B with a real
     !! symmetric matrix A using the factorization computed
     !! by SSYTRF_RK or SSYTRF_BK:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This algorithm is using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*), e(*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j, k, kp
           real(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRS_3', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! begin upper
              ! solve a*x = b, where a = u*d*u**t.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
              call stdlib_strsm( 'L', 'U', 'N', 'U', n, nrhs, one, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i = n
              do while ( i>=1 )
                 if( ipiv( i )>0_ilp ) then
                    call stdlib_sscal( nrhs, one / a( i, i ), b( i, 1_ilp ), ldb )
                 else if ( i>1_ilp ) then
                    akm1k = e( i )
                    akm1 = a( i-1, i-1 ) / akm1k
                    ak = a( i, i ) / akm1k
                    denom = akm1*ak - one
                    do j = 1, nrhs
                       bkm1 = b( i-1, j ) / akm1k
                       bk = b( i, j ) / akm1k
                       b( i-1, j ) = ( ak*bkm1-bk ) / denom
                       b( i, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i - 1_ilp
                 end if
                 i = i - 1_ilp
              end do
              ! compute (u**t \ b) -> b   [ u**t \ (d \ (u \p**t * b) ) ]
              call stdlib_strsm( 'L', 'U', 'T', 'U', n, nrhs, one, a, lda, b, ldb )
              ! p * b  [ p * (u**t \ (d \ (u \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
           else
              ! begin lower
              ! solve a*x = b, where a = l*d*l**t.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
              call stdlib_strsm( 'L', 'L', 'N', 'U', n, nrhs, one, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i = 1_ilp
              do while ( i<=n )
                 if( ipiv( i )>0_ilp ) then
                    call stdlib_sscal( nrhs, one / a( i, i ), b( i, 1_ilp ), ldb )
                 else if( i<n ) then
                    akm1k = e( i )
                    akm1 = a( i, i ) / akm1k
                    ak = a( i+1, i+1 ) / akm1k
                    denom = akm1*ak - one
                    do  j = 1, nrhs
                       bkm1 = b( i, j ) / akm1k
                       bk = b( i+1, j ) / akm1k
                       b( i, j ) = ( ak*bkm1-bk ) / denom
                       b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i + 1_ilp
                 end if
                 i = i + 1_ilp
              end do
              ! compute (l**t \ b) -> b   [ l**t \ (d \ (l \p**t * b) ) ]
              call stdlib_strsm('L', 'L', 'T', 'U', n, nrhs, one, a, lda, b, ldb )
              ! p * b  [ p * (l**t \ (d \ (l \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_sswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! end lower
           end if
           return
     end subroutine stdlib_ssytrs_3

     pure module subroutine stdlib_dsytrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb,info )
     !! DSYTRS_3 solves a system of linear equations A * X = B with a real
     !! symmetric matrix A using the factorization computed
     !! by DSYTRF_RK or DSYTRF_BK:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This algorithm is using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*), e(*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j, k, kp
           real(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRS_3', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! begin upper
              ! solve a*x = b, where a = u*d*u**t.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv( i ) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
              call stdlib_dtrsm( 'L', 'U', 'N', 'U', n, nrhs, one, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i = n
              do while ( i>=1 )
                 if( ipiv( i )>0_ilp ) then
                    call stdlib_dscal( nrhs, one / a( i, i ), b( i, 1_ilp ), ldb )
                 else if ( i>1_ilp ) then
                    akm1k = e( i )
                    akm1 = a( i-1, i-1 ) / akm1k
                    ak = a( i, i ) / akm1k
                    denom = akm1*ak - one
                    do j = 1, nrhs
                       bkm1 = b( i-1, j ) / akm1k
                       bk = b( i, j ) / akm1k
                       b( i-1, j ) = ( ak*bkm1-bk ) / denom
                       b( i, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i - 1_ilp
                 end if
                 i = i - 1_ilp
              end do
              ! compute (u**t \ b) -> b   [ u**t \ (d \ (u \p**t * b) ) ]
              call stdlib_dtrsm( 'L', 'U', 'T', 'U', n, nrhs, one, a, lda, b, ldb )
              ! p * b  [ p * (u**t \ (d \ (u \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
           else
              ! begin lower
              ! solve a*x = b, where a = l*d*l**t.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
              call stdlib_dtrsm( 'L', 'L', 'N', 'U', n, nrhs, one, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i = 1_ilp
              do while ( i<=n )
                 if( ipiv( i )>0_ilp ) then
                    call stdlib_dscal( nrhs, one / a( i, i ), b( i, 1_ilp ), ldb )
                 else if( i<n ) then
                    akm1k = e( i )
                    akm1 = a( i, i ) / akm1k
                    ak = a( i+1, i+1 ) / akm1k
                    denom = akm1*ak - one
                    do  j = 1, nrhs
                       bkm1 = b( i, j ) / akm1k
                       bk = b( i+1, j ) / akm1k
                       b( i, j ) = ( ak*bkm1-bk ) / denom
                       b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i + 1_ilp
                 end if
                 i = i + 1_ilp
              end do
              ! compute (l**t \ b) -> b   [ l**t \ (d \ (l \p**t * b) ) ]
              call stdlib_dtrsm('L', 'L', 'T', 'U', n, nrhs, one, a, lda, b, ldb )
              ! p * b  [ p * (l**t \ (d \ (l \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_dswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! end lower
           end if
           return
     end subroutine stdlib_dsytrs_3


     pure module subroutine stdlib_csytrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb,info )
     !! CSYTRS_3 solves a system of linear equations A * X = B with a complex
     !! symmetric matrix A using the factorization computed
     !! by CSYTRF_RK or CSYTRF_BK:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This algorithm is using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), e(*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j, k, kp
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYTRS_3', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! begin upper
              ! solve a*x = b, where a = u*d*u**t.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
              call stdlib_ctrsm( 'L', 'U', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i = n
              do while ( i>=1 )
                 if( ipiv( i )>0_ilp ) then
                    call stdlib_cscal( nrhs, cone / a( i, i ), b( i, 1_ilp ), ldb )
                 else if ( i>1_ilp ) then
                    akm1k = e( i )
                    akm1 = a( i-1, i-1 ) / akm1k
                    ak = a( i, i ) / akm1k
                    denom = akm1*ak - cone
                    do j = 1, nrhs
                       bkm1 = b( i-1, j ) / akm1k
                       bk = b( i, j ) / akm1k
                       b( i-1, j ) = ( ak*bkm1-bk ) / denom
                       b( i, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i - 1_ilp
                 end if
                 i = i - 1_ilp
              end do
              ! compute (u**t \ b) -> b   [ u**t \ (d \ (u \p**t * b) ) ]
              call stdlib_ctrsm( 'L', 'U', 'T', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (u**t \ (d \ (u \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv( i ) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
           else
              ! begin lower
              ! solve a*x = b, where a = l*d*l**t.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
              call stdlib_ctrsm( 'L', 'L', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i = 1_ilp
              do while ( i<=n )
                 if( ipiv( i )>0_ilp ) then
                    call stdlib_cscal( nrhs, cone / a( i, i ), b( i, 1_ilp ), ldb )
                 else if( i<n ) then
                    akm1k = e( i )
                    akm1 = a( i, i ) / akm1k
                    ak = a( i+1, i+1 ) / akm1k
                    denom = akm1*ak - cone
                    do  j = 1, nrhs
                       bkm1 = b( i, j ) / akm1k
                       bk = b( i+1, j ) / akm1k
                       b( i, j ) = ( ak*bkm1-bk ) / denom
                       b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i + 1_ilp
                 end if
                 i = i + 1_ilp
              end do
              ! compute (l**t \ b) -> b   [ l**t \ (d \ (l \p**t * b) ) ]
              call stdlib_ctrsm('L', 'L', 'T', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (l**t \ (d \ (l \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! end lower
           end if
           return
     end subroutine stdlib_csytrs_3

     pure module subroutine stdlib_zsytrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb,info )
     !! ZSYTRS_3 solves a system of linear equations A * X = B with a complex
     !! symmetric matrix A using the factorization computed
     !! by ZSYTRF_RK or ZSYTRF_BK:
     !! A = P*U*D*(U**T)*(P**T) or A = P*L*D*(L**T)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This algorithm is using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), e(*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j, k, kp
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYTRS_3', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! begin upper
              ! solve a*x = b, where a = u*d*u**t.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
              call stdlib_ztrsm( 'L', 'U', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i = n
              do while ( i>=1 )
                 if( ipiv( i )>0_ilp ) then
                    call stdlib_zscal( nrhs, cone / a( i, i ), b( i, 1_ilp ), ldb )
                 else if ( i>1_ilp ) then
                    akm1k = e( i )
                    akm1 = a( i-1, i-1 ) / akm1k
                    ak = a( i, i ) / akm1k
                    denom = akm1*ak - cone
                    do j = 1, nrhs
                       bkm1 = b( i-1, j ) / akm1k
                       bk = b( i, j ) / akm1k
                       b( i-1, j ) = ( ak*bkm1-bk ) / denom
                       b( i, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i - 1_ilp
                 end if
                 i = i - 1_ilp
              end do
              ! compute (u**t \ b) -> b   [ u**t \ (d \ (u \p**t * b) ) ]
              call stdlib_ztrsm( 'L', 'U', 'T', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (u**t \ (d \ (u \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
           else
              ! begin lower
              ! solve a*x = b, where a = l*d*l**t.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
              call stdlib_ztrsm( 'L', 'L', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i = 1_ilp
              do while ( i<=n )
                 if( ipiv( i )>0_ilp ) then
                    call stdlib_zscal( nrhs, cone / a( i, i ), b( i, 1_ilp ), ldb )
                 else if( i<n ) then
                    akm1k = e( i )
                    akm1 = a( i, i ) / akm1k
                    ak = a( i+1, i+1 ) / akm1k
                    denom = akm1*ak - cone
                    do  j = 1, nrhs
                       bkm1 = b( i, j ) / akm1k
                       bk = b( i+1, j ) / akm1k
                       b( i, j ) = ( ak*bkm1-bk ) / denom
                       b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i + 1_ilp
                 end if
                 i = i + 1_ilp
              end do
              ! compute (l**t \ b) -> b   [ l**t \ (d \ (l \p**t * b) ) ]
              call stdlib_ztrsm('L', 'L', 'T', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (l**t \ (d \ (l \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! end lower
           end if
           return
     end subroutine stdlib_zsytrs_3




     pure module subroutine stdlib_ssyswapr( uplo, n, a, lda, i1, i2)
     !! SSYSWAPR applies an elementary permutation on the rows and the columns of
     !! a symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: i1, i2, lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,n)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           real(sp) :: tmp
           ! Executable Statements 
           upper = stdlib_lsame( uplo, 'U' )
           if (upper) then
               ! upper
               ! first swap
                ! - swap column i1 and i2 from i1 to i1-1
              call stdlib_sswap( i1-1, a(1_ilp,i1), 1_ilp, a(1_ilp,i2), 1_ilp )
                ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap row i1 from i1+1 to i2-1 with col i2 from i1+1 to i2-1
              tmp=a(i1,i1)
              a(i1,i1)=a(i2,i2)
              a(i2,i2)=tmp
              do i=1,i2-i1-1
                 tmp=a(i1,i1+i)
                 a(i1,i1+i)=a(i1+i,i2)
                 a(i1+i,i2)=tmp
              end do
                ! third swap
                ! - swap row i1 and i2 from i2+1 to n
              do i=i2+1,n
                 tmp=a(i1,i)
                 a(i1,i)=a(i2,i)
                 a(i2,i)=tmp
              end do
             else
               ! lower
               ! first swap
                ! - swap row i1 and i2 from i1 to i1-1
              call stdlib_sswap( i1-1, a(i1,1_ilp), lda, a(i2,1_ilp), lda )
               ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap col i1 from i1+1 to i2-1 with row i2 from i1+1 to i2-1
               tmp=a(i1,i1)
               a(i1,i1)=a(i2,i2)
               a(i2,i2)=tmp
               do i=1,i2-i1-1
                  tmp=a(i1+i,i1)
                  a(i1+i,i1)=a(i2,i1+i)
                  a(i2,i1+i)=tmp
               end do
               ! third swap
                ! - swap col i1 and i2 from i2+1 to n
               do i=i2+1,n
                  tmp=a(i,i1)
                  a(i,i1)=a(i,i2)
                  a(i,i2)=tmp
               end do
           endif
     end subroutine stdlib_ssyswapr

     pure module subroutine stdlib_dsyswapr( uplo, n, a, lda, i1, i2)
     !! DSYSWAPR applies an elementary permutation on the rows and the columns of
     !! a symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: i1, i2, lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,n)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           real(dp) :: tmp
           ! Executable Statements 
           upper = stdlib_lsame( uplo, 'U' )
           if (upper) then
               ! upper
               ! first swap
                ! - swap column i1 and i2 from i1 to i1-1
              call stdlib_dswap( i1-1, a(1_ilp,i1), 1_ilp, a(1_ilp,i2), 1_ilp )
                ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap row i1 from i1+1 to i2-1 with col i2 from i1+1 to i2-1
              tmp=a(i1,i1)
              a(i1,i1)=a(i2,i2)
              a(i2,i2)=tmp
              do i=1,i2-i1-1
                 tmp=a(i1,i1+i)
                 a(i1,i1+i)=a(i1+i,i2)
                 a(i1+i,i2)=tmp
              end do
                ! third swap
                ! - swap row i1 and i2 from i2+1 to n
              do i=i2+1,n
                 tmp=a(i1,i)
                 a(i1,i)=a(i2,i)
                 a(i2,i)=tmp
              end do
             else
               ! lower
               ! first swap
                ! - swap row i1 and i2 from i1 to i1-1
              call stdlib_dswap( i1-1, a(i1,1_ilp), lda, a(i2,1_ilp), lda )
               ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap col i1 from i1+1 to i2-1 with row i2 from i1+1 to i2-1
               tmp=a(i1,i1)
               a(i1,i1)=a(i2,i2)
               a(i2,i2)=tmp
               do i=1,i2-i1-1
                  tmp=a(i1+i,i1)
                  a(i1+i,i1)=a(i2,i1+i)
                  a(i2,i1+i)=tmp
               end do
               ! third swap
                ! - swap col i1 and i2 from i2+1 to n
               do i=i2+1,n
                  tmp=a(i,i1)
                  a(i,i1)=a(i,i2)
                  a(i,i2)=tmp
               end do
           endif
     end subroutine stdlib_dsyswapr


     pure module subroutine stdlib_csyswapr( uplo, n, a, lda, i1, i2)
     !! CSYSWAPR applies an elementary permutation on the rows and the columns of
     !! a symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: i1, i2, lda, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,n)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           complex(sp) :: tmp
           ! Executable Statements 
           upper = stdlib_lsame( uplo, 'U' )
           if (upper) then
               ! upper
               ! first swap
                ! - swap column i1 and i2 from i1 to i1-1
              call stdlib_cswap( i1-1, a(1_ilp,i1), 1_ilp, a(1_ilp,i2), 1_ilp )
                ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap row i1 from i1+1 to i2-1 with col i2 from i1+1 to i2-1
              tmp=a(i1,i1)
              a(i1,i1)=a(i2,i2)
              a(i2,i2)=tmp
              do i=1,i2-i1-1
                 tmp=a(i1,i1+i)
                 a(i1,i1+i)=a(i1+i,i2)
                 a(i1+i,i2)=tmp
              end do
                ! third swap
                ! - swap row i1 and i2 from i2+1 to n
              do i=i2+1,n
                 tmp=a(i1,i)
                 a(i1,i)=a(i2,i)
                 a(i2,i)=tmp
              end do
             else
               ! lower
               ! first swap
                ! - swap row i1 and i2 from i1 to i1-1
              call stdlib_cswap ( i1-1, a(i1,1_ilp), lda, a(i2,1_ilp), lda )
               ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap col i1 from i1+1 to i2-1 with row i2 from i1+1 to i2-1
               tmp=a(i1,i1)
               a(i1,i1)=a(i2,i2)
               a(i2,i2)=tmp
               do i=1,i2-i1-1
                  tmp=a(i1+i,i1)
                  a(i1+i,i1)=a(i2,i1+i)
                  a(i2,i1+i)=tmp
               end do
               ! third swap
                ! - swap col i1 and i2 from i2+1 to n
               do i=i2+1,n
                  tmp=a(i,i1)
                  a(i,i1)=a(i,i2)
                  a(i,i2)=tmp
               end do
           endif
     end subroutine stdlib_csyswapr

     pure module subroutine stdlib_zsyswapr( uplo, n, a, lda, i1, i2)
     !! ZSYSWAPR applies an elementary permutation on the rows and the columns of
     !! a symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: i1, i2, lda, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,n)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           complex(dp) :: tmp
           ! Executable Statements 
           upper = stdlib_lsame( uplo, 'U' )
           if (upper) then
               ! upper
               ! first swap
                ! - swap column i1 and i2 from i1 to i1-1
              call stdlib_zswap( i1-1, a(1_ilp,i1), 1_ilp, a(1_ilp,i2), 1_ilp )
                ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap row i1 from i1+1 to i2-1 with col i2 from i1+1 to i2-1
              tmp=a(i1,i1)
              a(i1,i1)=a(i2,i2)
              a(i2,i2)=tmp
              do i=1,i2-i1-1
                 tmp=a(i1,i1+i)
                 a(i1,i1+i)=a(i1+i,i2)
                 a(i1+i,i2)=tmp
              end do
                ! third swap
                ! - swap row i1 and i2 from i2+1 to n
              do i=i2+1,n
                 tmp=a(i1,i)
                 a(i1,i)=a(i2,i)
                 a(i2,i)=tmp
              end do
             else
               ! lower
               ! first swap
                ! - swap row i1 and i2 from i1 to i1-1
              call stdlib_zswap( i1-1, a(i1,1_ilp), lda, a(i2,1_ilp), lda )
               ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap col i1 from i1+1 to i2-1 with row i2 from i1+1 to i2-1
               tmp=a(i1,i1)
               a(i1,i1)=a(i2,i2)
               a(i2,i2)=tmp
               do i=1,i2-i1-1
                  tmp=a(i1+i,i1)
                  a(i1+i,i1)=a(i2,i1+i)
                  a(i2,i1+i)=tmp
               end do
               ! third swap
                ! - swap col i1 and i2 from i2+1 to n
               do i=i2+1,n
                  tmp=a(i,i1)
                  a(i,i1)=a(i,i2)
                  a(i,i2)=tmp
               end do
           endif
     end subroutine stdlib_zsyswapr




     real(sp) module function stdlib_cla_herpvgrw( uplo, n, info, a, lda, af, ldaf, ipiv,work )
     !! CLA_HERPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, info, lda, ldaf
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: ncols, i, j, k, kp
           real(sp) :: amax, umax, rpvgrw, tmp
           logical(lk) :: upper
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag ( zdum ) )
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           if ( info==0_ilp ) then
              if (upper) then
                 ncols = 1_ilp
              else
                 ncols = n
              end if
           else
              ncols = info
           end if
           rpvgrw = one
           do i = 1, 2*n
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column of a.  compute the max
           ! for all n columns so we can apply the pivot permutation while
           ! looping below.  assume a full factorization is the common case.
           if ( upper ) then
              do j = 1, n
                 do i = 1, j
                    work( n+i ) = max( cabs1( a( i,j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i,j ) ), work( n+j ) )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    work( n+i ) = max( cabs1( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i, j ) ), work( n+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of u or l.  also
           ! permute the magnitudes of a above so they're in the same order as
           ! the factor.
           ! the iteration orders and permutations were copied from stdlib_csytrs.
           ! calls to stdlib_sswap would be severe overkill.
           if ( upper ) then
              k = n
              do while ( k < ncols .and. k>0 )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = 1, k
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k - 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k-1 )
                    work( n+k-1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = 1, k-1
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k-1 ) =max( cabs1( af( i, k-1 ) ), work( k-1 ) )
                    end do
                    work( k ) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k - 2_ilp
                 end if
              end do
              k = ncols
              do while ( k <= n )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k + 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k + 2_ilp
                 end if
              end do
           else
              k = 1_ilp
              do while ( k <= ncols )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = k, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k + 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k+1 )
                    work( n+k+1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = k+1, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k+1 ) =max( cabs1( af( i, k+1 ) ) , work( k+1 ) )
                    end do
                    work(k) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k + 2_ilp
                 end if
              end do
              k = ncols
              do while ( k >= 1 )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k - 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k - 2_ilp
                 endif
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( upper ) then
              do i = ncols, n
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_cla_herpvgrw = rpvgrw
     end function stdlib_cla_herpvgrw

     real(dp) module function stdlib_zla_herpvgrw( uplo, n, info, a, lda, af,ldaf, ipiv, work )
     !! ZLA_HERPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, info, lda, ldaf
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: ncols, i, j, k, kp
           real(dp) :: amax, umax, rpvgrw, tmp
           logical(lk) :: upper
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag ( zdum ) )
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           if ( info==0_ilp ) then
              if (upper) then
                 ncols = 1_ilp
              else
                 ncols = n
              end if
           else
              ncols = info
           end if
           rpvgrw = one
           do i = 1, 2*n
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column of a.  compute the max
           ! for all n columns so we can apply the pivot permutation while
           ! looping below.  assume a full factorization is the common case.
           if ( upper ) then
              do j = 1, n
                 do i = 1, j
                    work( n+i ) = max( cabs1( a( i,j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i,j ) ), work( n+j ) )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    work( n+i ) = max( cabs1( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i, j ) ), work( n+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of u or l.  also
           ! permute the magnitudes of a above so they're in the same order as
           ! the factor.
           ! the iteration orders and permutations were copied from stdlib_zsytrs.
           ! calls to stdlib_sswap would be severe overkill.
           if ( upper ) then
              k = n
              do while ( k < ncols .and. k>0 )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = 1, k
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k - 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k-1 )
                    work( n+k-1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = 1, k-1
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k-1 ) =max( cabs1( af( i, k-1 ) ), work( k-1 ) )
                    end do
                    work( k ) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k - 2_ilp
                 end if
              end do
              k = ncols
              do while ( k <= n )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k + 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k + 2_ilp
                 end if
              end do
           else
              k = 1_ilp
              do while ( k <= ncols )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = k, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k + 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k+1 )
                    work( n+k+1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = k+1, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k+1 ) =max( cabs1( af( i, k+1 ) ) , work( k+1 ) )
                    end do
                    work(k) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k + 2_ilp
                 end if
              end do
              k = ncols
              do while ( k >= 1 )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k - 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k - 2_ilp
                 endif
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( upper ) then
              do i = ncols, n
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_zla_herpvgrw = rpvgrw
     end function stdlib_zla_herpvgrw




     pure module subroutine stdlib_sspcon( uplo, n, ap, ipiv, anorm, rcond, work, iwork,info )
     !! SSPCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric packed matrix A using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by SSPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: ap(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, ip, kase
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              ip = n*( n+1 ) / 2_ilp
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip - i
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              ip = 1_ilp
              do i = 1, n
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip + n - i + 1_ilp
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_ssptrs( uplo, n, 1_ilp, ap, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_sspcon

     pure module subroutine stdlib_dspcon( uplo, n, ap, ipiv, anorm, rcond, work, iwork,info )
     !! DSPCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a real symmetric packed matrix A using the factorization
     !! A = U*D*U**T or A = L*D*L**T computed by DSPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: ap(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, ip, kase
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              ip = n*( n+1 ) / 2_ilp
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip - i
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              ip = 1_ilp
              do i = 1, n
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip + n - i + 1_ilp
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_dsptrs( uplo, n, 1_ilp, ap, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_dspcon


     pure module subroutine stdlib_cspcon( uplo, n, ap, ipiv, anorm, rcond, work, info )
     !! CSPCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex symmetric packed matrix A using the
     !! factorization A = U*D*U**T or A = L*D*L**T computed by CSPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, ip, kase
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              ip = n*( n+1 ) / 2_ilp
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip - i
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              ip = 1_ilp
              do i = 1, n
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip + n - i + 1_ilp
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_csptrs( uplo, n, 1_ilp, ap, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_cspcon

     pure module subroutine stdlib_zspcon( uplo, n, ap, ipiv, anorm, rcond, work, info )
     !! ZSPCON estimates the reciprocal of the condition number (in the
     !! 1-norm) of a complex symmetric packed matrix A using the
     !! factorization A = U*D*U**T or A = L*D*L**T computed by ZSPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, ip, kase
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              ip = n*( n+1 ) / 2_ilp
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip - i
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              ip = 1_ilp
              do i = 1, n
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip + n - i + 1_ilp
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**t) or inv(u*d*u**t).
              call stdlib_zsptrs( uplo, n, 1_ilp, ap, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_zspcon




     pure module subroutine stdlib_ssptrf( uplo, n, ap, ipiv, info )
     !! SSPTRF computes the factorization of a real symmetric matrix A stored
     !! in packed format using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kc, kk, knc, kp, kpc, kstep, kx, npp
           real(sp) :: absakk, alpha, colmax, d11, d12, d21, d22, r1, rowmax, t, wk, wkm1, &
                     wkp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPTRF', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              kc = ( n-1 )*n / 2_ilp + 1_ilp
              10 continue
              knc = kc
              ! if k < 1, exit from loop
              if( k<1 )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( ap( kc+k-1 ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k>1_ilp ) then
                 imax = stdlib_isamax( k-1, ap( kc ), 1_ilp )
                 colmax = abs( ap( kc+imax-1 ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    rowmax = zero
                    jmax = imax
                    kx = imax*( imax+1 ) / 2_ilp + imax
                    do j = imax + 1, k
                       if( abs( ap( kx ) )>rowmax ) then
                          rowmax = abs( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + j
                    end do
                    kpc = ( imax-1 )*imax / 2_ilp + 1_ilp
                    if( imax>1_ilp ) then
                       jmax = stdlib_isamax( imax-1, ap( kpc ), 1_ilp )
                       rowmax = max( rowmax, abs( ap( kpc+jmax-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( ap( kpc+imax-1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k - kstep + 1_ilp
                 if( kstep==2_ilp )knc = knc - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_sswap( kp-1, ap( knc ), 1_ilp, ap( kpc ), 1_ilp )
                    kx = kpc + kp - 1_ilp
                    do j = kp + 1, kk - 1
                       kx = kx + j - 1_ilp
                       t = ap( knc+j-1 )
                       ap( knc+j-1 ) = ap( kx )
                       ap( kx ) = t
                    end do
                    t = ap( knc+kk-1 )
                    ap( knc+kk-1 ) = ap( kpc+kp-1 )
                    ap( kpc+kp-1 ) = t
                    if( kstep==2_ilp ) then
                       t = ap( kc+k-2 )
                       ap( kc+k-2 ) = ap( kc+kp-1 )
                       ap( kc+kp-1 ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**t = a - w(k)*1/d(k)*w(k)**t
                    r1 = one / ap( kc+k-1 )
                    call stdlib_sspr( uplo, k-1, -r1, ap( kc ), 1_ilp, ap )
                    ! store u(k) in column k
                    call stdlib_sscal( k-1, r1, ap( kc ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**t
                    if( k>2_ilp ) then
                       d12 = ap( k-1+( k-1 )*k / 2_ilp )
                       d22 = ap( k-1+( k-2 )*( k-1 ) / 2_ilp ) / d12
                       d11 = ap( k+( k-1 )*k / 2_ilp ) / d12
                       t = one / ( d11*d22-one )
                       d12 = t / d12
                       do j = k - 2, 1, -1
                          wkm1 = d12*( d11*ap( j+( k-2 )*( k-1 ) / 2_ilp )-ap( j+( k-1 )*k / 2_ilp ) )
                                    
                          wk = d12*( d22*ap( j+( k-1 )*k / 2_ilp )-ap( j+( k-2 )*( k-1 ) / 2_ilp ) )
                                    
                          do i = j, 1, -1
                             ap( i+( j-1 )*j / 2_ilp ) = ap( i+( j-1 )*j / 2_ilp ) -ap( i+( k-1 )*k / 2_ilp )&
                                       *wk -ap( i+( k-2 )*( k-1 ) / 2_ilp )*wkm1
                          end do
                          ap( j+( k-1 )*k / 2_ilp ) = wk
                          ap( j+( k-2 )*( k-1 ) / 2_ilp ) = wkm1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              kc = knc - k
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              kc = 1_ilp
              npp = n*( n+1 ) / 2_ilp
              60 continue
              knc = kc
              ! if k > n, exit from loop
              if( k>n )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( ap( kc ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k<n ) then
                 imax = k + stdlib_isamax( n-k, ap( kc+1 ), 1_ilp )
                 colmax = abs( ap( kc+imax-k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    kx = kc + imax - k
                    do j = k, imax - 1
                       if( abs( ap( kx ) )>rowmax ) then
                          rowmax = abs( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + n - j
                    end do
                    kpc = npp - ( n-imax+1 )*( n-imax+2 ) / 2_ilp + 1_ilp
                    if( imax<n ) then
                       jmax = imax + stdlib_isamax( n-imax, ap( kpc+1 ), 1_ilp )
                       rowmax = max( rowmax, abs( ap( kpc+jmax-imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( ap( kpc ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k + kstep - 1_ilp
                 if( kstep==2_ilp )knc = knc + n - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_sswap( n-kp, ap( knc+kp-kk+1 ), 1_ilp, ap( kpc+1 ),1_ilp )
                              
                    kx = knc + kp - kk
                    do j = kk + 1, kp - 1
                       kx = kx + n - j + 1_ilp
                       t = ap( knc+j-kk )
                       ap( knc+j-kk ) = ap( kx )
                       ap( kx ) = t
                    end do
                    t = ap( knc )
                    ap( knc ) = ap( kpc )
                    ap( kpc ) = t
                    if( kstep==2_ilp ) then
                       t = ap( kc+1 )
                       ap( kc+1 ) = ap( kc+kp-k )
                       ap( kc+kp-k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**t = a - w(k)*(1/d(k))*w(k)**t
                       r1 = one / ap( kc )
                       call stdlib_sspr( uplo, n-k, -r1, ap( kc+1 ), 1_ilp,ap( kc+n-k+1 ) )
                       ! store l(k) in column k
                       call stdlib_sscal( n-k, r1, ap( kc+1 ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**t
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**t
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d21 = ap( k+1+( k-1 )*( 2_ilp*n-k ) / 2_ilp )
                       d11 = ap( k+1+k*( 2_ilp*n-k-1 ) / 2_ilp ) / d21
                       d22 = ap( k+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) / d21
                       t = one / ( d11*d22-one )
                       d21 = t / d21
                       do j = k + 2, n
                          wk = d21*( d11*ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp )-ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp ) )
                                    
                          wkp1 = d21*( d22*ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp )-ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp )&
                                     )
                          do i = j, n
                             ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) = ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) - ap( &
                                       i+( k-1 )*( 2_ilp*n-k ) /2_ilp )*wk - ap( i+k*( 2_ilp*n-k-1 ) / 2_ilp )*wkp1
                          end do
                          ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) = wk
                          ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp ) = wkp1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              kc = knc + n - k + 2_ilp
              go to 60
           end if
           110 continue
           return
     end subroutine stdlib_ssptrf

     pure module subroutine stdlib_dsptrf( uplo, n, ap, ipiv, info )
     !! DSPTRF computes the factorization of a real symmetric matrix A stored
     !! in packed format using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kc, kk, knc, kp, kpc, kstep, kx, npp
           real(dp) :: absakk, alpha, colmax, d11, d12, d21, d22, r1, rowmax, t, wk, wkm1, &
                     wkp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPTRF', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              kc = ( n-1 )*n / 2_ilp + 1_ilp
              10 continue
              knc = kc
              ! if k < 1, exit from loop
              if( k<1 )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( ap( kc+k-1 ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k>1_ilp ) then
                 imax = stdlib_idamax( k-1, ap( kc ), 1_ilp )
                 colmax = abs( ap( kc+imax-1 ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    rowmax = zero
                    jmax = imax
                    kx = imax*( imax+1 ) / 2_ilp + imax
                    do j = imax + 1, k
                       if( abs( ap( kx ) )>rowmax ) then
                          rowmax = abs( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + j
                    end do
                    kpc = ( imax-1 )*imax / 2_ilp + 1_ilp
                    if( imax>1_ilp ) then
                       jmax = stdlib_idamax( imax-1, ap( kpc ), 1_ilp )
                       rowmax = max( rowmax, abs( ap( kpc+jmax-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( ap( kpc+imax-1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k - kstep + 1_ilp
                 if( kstep==2_ilp )knc = knc - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_dswap( kp-1, ap( knc ), 1_ilp, ap( kpc ), 1_ilp )
                    kx = kpc + kp - 1_ilp
                    do j = kp + 1, kk - 1
                       kx = kx + j - 1_ilp
                       t = ap( knc+j-1 )
                       ap( knc+j-1 ) = ap( kx )
                       ap( kx ) = t
                    end do
                    t = ap( knc+kk-1 )
                    ap( knc+kk-1 ) = ap( kpc+kp-1 )
                    ap( kpc+kp-1 ) = t
                    if( kstep==2_ilp ) then
                       t = ap( kc+k-2 )
                       ap( kc+k-2 ) = ap( kc+kp-1 )
                       ap( kc+kp-1 ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**t = a - w(k)*1/d(k)*w(k)**t
                    r1 = one / ap( kc+k-1 )
                    call stdlib_dspr( uplo, k-1, -r1, ap( kc ), 1_ilp, ap )
                    ! store u(k) in column k
                    call stdlib_dscal( k-1, r1, ap( kc ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**t
                    if( k>2_ilp ) then
                       d12 = ap( k-1+( k-1 )*k / 2_ilp )
                       d22 = ap( k-1+( k-2 )*( k-1 ) / 2_ilp ) / d12
                       d11 = ap( k+( k-1 )*k / 2_ilp ) / d12
                       t = one / ( d11*d22-one )
                       d12 = t / d12
                       do j = k - 2, 1, -1
                          wkm1 = d12*( d11*ap( j+( k-2 )*( k-1 ) / 2_ilp )-ap( j+( k-1 )*k / 2_ilp ) )
                                    
                          wk = d12*( d22*ap( j+( k-1 )*k / 2_ilp )-ap( j+( k-2 )*( k-1 ) / 2_ilp ) )
                                    
                          do i = j, 1, -1
                             ap( i+( j-1 )*j / 2_ilp ) = ap( i+( j-1 )*j / 2_ilp ) -ap( i+( k-1 )*k / 2_ilp )&
                                       *wk -ap( i+( k-2 )*( k-1 ) / 2_ilp )*wkm1
                          end do
                          ap( j+( k-1 )*k / 2_ilp ) = wk
                          ap( j+( k-2 )*( k-1 ) / 2_ilp ) = wkm1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              kc = knc - k
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              kc = 1_ilp
              npp = n*( n+1 ) / 2_ilp
              60 continue
              knc = kc
              ! if k > n, exit from loop
              if( k>n )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( ap( kc ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k<n ) then
                 imax = k + stdlib_idamax( n-k, ap( kc+1 ), 1_ilp )
                 colmax = abs( ap( kc+imax-k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    kx = kc + imax - k
                    do j = k, imax - 1
                       if( abs( ap( kx ) )>rowmax ) then
                          rowmax = abs( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + n - j
                    end do
                    kpc = npp - ( n-imax+1 )*( n-imax+2 ) / 2_ilp + 1_ilp
                    if( imax<n ) then
                       jmax = imax + stdlib_idamax( n-imax, ap( kpc+1 ), 1_ilp )
                       rowmax = max( rowmax, abs( ap( kpc+jmax-imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( ap( kpc ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k + kstep - 1_ilp
                 if( kstep==2_ilp )knc = knc + n - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_dswap( n-kp, ap( knc+kp-kk+1 ), 1_ilp, ap( kpc+1 ),1_ilp )
                              
                    kx = knc + kp - kk
                    do j = kk + 1, kp - 1
                       kx = kx + n - j + 1_ilp
                       t = ap( knc+j-kk )
                       ap( knc+j-kk ) = ap( kx )
                       ap( kx ) = t
                    end do
                    t = ap( knc )
                    ap( knc ) = ap( kpc )
                    ap( kpc ) = t
                    if( kstep==2_ilp ) then
                       t = ap( kc+1 )
                       ap( kc+1 ) = ap( kc+kp-k )
                       ap( kc+kp-k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**t = a - w(k)*(1/d(k))*w(k)**t
                       r1 = one / ap( kc )
                       call stdlib_dspr( uplo, n-k, -r1, ap( kc+1 ), 1_ilp,ap( kc+n-k+1 ) )
                       ! store l(k) in column k
                       call stdlib_dscal( n-k, r1, ap( kc+1 ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**t
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**t
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d21 = ap( k+1+( k-1 )*( 2_ilp*n-k ) / 2_ilp )
                       d11 = ap( k+1+k*( 2_ilp*n-k-1 ) / 2_ilp ) / d21
                       d22 = ap( k+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) / d21
                       t = one / ( d11*d22-one )
                       d21 = t / d21
                       do j = k + 2, n
                          wk = d21*( d11*ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp )-ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp ) )
                                    
                          wkp1 = d21*( d22*ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp )-ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp )&
                                     )
                          do i = j, n
                             ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) = ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) - ap( &
                                       i+( k-1 )*( 2_ilp*n-k ) /2_ilp )*wk - ap( i+k*( 2_ilp*n-k-1 ) / 2_ilp )*wkp1
                          end do
                          ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) = wk
                          ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp ) = wkp1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              kc = knc + n - k + 2_ilp
              go to 60
           end if
           110 continue
           return
     end subroutine stdlib_dsptrf


     pure module subroutine stdlib_csptrf( uplo, n, ap, ipiv, info )
     !! CSPTRF computes the factorization of a complex symmetric matrix A
     !! stored in packed format using the Bunch-Kaufman diagonal pivoting
     !! method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kc, kk, knc, kp, kpc, kstep, kx, npp
           real(sp) :: absakk, alpha, colmax, rowmax
           complex(sp) :: d11, d12, d21, d22, r1, t, wk, wkm1, wkp1, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSPTRF', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              kc = ( n-1 )*n / 2_ilp + 1_ilp
              10 continue
              knc = kc
              ! if k < 1, exit from loop
              if( k<1 )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( ap( kc+k-1 ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k>1_ilp ) then
                 imax = stdlib_icamax( k-1, ap( kc ), 1_ilp )
                 colmax = cabs1( ap( kc+imax-1 ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    rowmax = zero
                    jmax = imax
                    kx = imax*( imax+1 ) / 2_ilp + imax
                    do j = imax + 1, k
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + j
                    end do
                    kpc = ( imax-1 )*imax / 2_ilp + 1_ilp
                    if( imax>1_ilp ) then
                       jmax = stdlib_icamax( imax-1, ap( kpc ), 1_ilp )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( ap( kpc+imax-1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k - kstep + 1_ilp
                 if( kstep==2_ilp )knc = knc - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_cswap( kp-1, ap( knc ), 1_ilp, ap( kpc ), 1_ilp )
                    kx = kpc + kp - 1_ilp
                    do j = kp + 1, kk - 1
                       kx = kx + j - 1_ilp
                       t = ap( knc+j-1 )
                       ap( knc+j-1 ) = ap( kx )
                       ap( kx ) = t
                    end do
                    t = ap( knc+kk-1 )
                    ap( knc+kk-1 ) = ap( kpc+kp-1 )
                    ap( kpc+kp-1 ) = t
                    if( kstep==2_ilp ) then
                       t = ap( kc+k-2 )
                       ap( kc+k-2 ) = ap( kc+kp-1 )
                       ap( kc+kp-1 ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**t = a - w(k)*1/d(k)*w(k)**t
                    r1 = cone / ap( kc+k-1 )
                    call stdlib_cspr( uplo, k-1, -r1, ap( kc ), 1_ilp, ap )
                    ! store u(k) in column k
                    call stdlib_cscal( k-1, r1, ap( kc ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**t
                    if( k>2_ilp ) then
                       d12 = ap( k-1+( k-1 )*k / 2_ilp )
                       d22 = ap( k-1+( k-2 )*( k-1 ) / 2_ilp ) / d12
                       d11 = ap( k+( k-1 )*k / 2_ilp ) / d12
                       t = cone / ( d11*d22-cone )
                       d12 = t / d12
                       do j = k - 2, 1, -1
                          wkm1 = d12*( d11*ap( j+( k-2 )*( k-1 ) / 2_ilp )-ap( j+( k-1 )*k / 2_ilp ) )
                                    
                          wk = d12*( d22*ap( j+( k-1 )*k / 2_ilp )-ap( j+( k-2 )*( k-1 ) / 2_ilp ) )
                                    
                          do i = j, 1, -1
                             ap( i+( j-1 )*j / 2_ilp ) = ap( i+( j-1 )*j / 2_ilp ) -ap( i+( k-1 )*k / 2_ilp )&
                                       *wk -ap( i+( k-2 )*( k-1 ) / 2_ilp )*wkm1
                          end do
                          ap( j+( k-1 )*k / 2_ilp ) = wk
                          ap( j+( k-2 )*( k-1 ) / 2_ilp ) = wkm1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              kc = knc - k
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              kc = 1_ilp
              npp = n*( n+1 ) / 2_ilp
              60 continue
              knc = kc
              ! if k > n, exit from loop
              if( k>n )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( ap( kc ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k<n ) then
                 imax = k + stdlib_icamax( n-k, ap( kc+1 ), 1_ilp )
                 colmax = cabs1( ap( kc+imax-k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    kx = kc + imax - k
                    do j = k, imax - 1
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + n - j
                    end do
                    kpc = npp - ( n-imax+1 )*( n-imax+2 ) / 2_ilp + 1_ilp
                    if( imax<n ) then
                       jmax = imax + stdlib_icamax( n-imax, ap( kpc+1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( ap( kpc ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k + kstep - 1_ilp
                 if( kstep==2_ilp )knc = knc + n - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_cswap( n-kp, ap( knc+kp-kk+1 ), 1_ilp, ap( kpc+1 ),1_ilp )
                              
                    kx = knc + kp - kk
                    do j = kk + 1, kp - 1
                       kx = kx + n - j + 1_ilp
                       t = ap( knc+j-kk )
                       ap( knc+j-kk ) = ap( kx )
                       ap( kx ) = t
                    end do
                    t = ap( knc )
                    ap( knc ) = ap( kpc )
                    ap( kpc ) = t
                    if( kstep==2_ilp ) then
                       t = ap( kc+1 )
                       ap( kc+1 ) = ap( kc+kp-k )
                       ap( kc+kp-k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**t = a - w(k)*(1/d(k))*w(k)**t
                       r1 = cone / ap( kc )
                       call stdlib_cspr( uplo, n-k, -r1, ap( kc+1 ), 1_ilp,ap( kc+n-k+1 ) )
                       ! store l(k) in column k
                       call stdlib_cscal( n-k, r1, ap( kc+1 ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**t
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**t
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d21 = ap( k+1+( k-1 )*( 2_ilp*n-k ) / 2_ilp )
                       d11 = ap( k+1+k*( 2_ilp*n-k-1 ) / 2_ilp ) / d21
                       d22 = ap( k+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) / d21
                       t = cone / ( d11*d22-cone )
                       d21 = t / d21
                       do j = k + 2, n
                          wk = d21*( d11*ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp )-ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp ) )
                                    
                          wkp1 = d21*( d22*ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp )-ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp )&
                                     )
                          do i = j, n
                             ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) = ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) - ap( &
                                       i+( k-1 )*( 2_ilp*n-k ) /2_ilp )*wk - ap( i+k*( 2_ilp*n-k-1 ) / 2_ilp )*wkp1
                          end do
                          ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) = wk
                          ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp ) = wkp1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              kc = knc + n - k + 2_ilp
              go to 60
           end if
           110 continue
           return
     end subroutine stdlib_csptrf

     pure module subroutine stdlib_zsptrf( uplo, n, ap, ipiv, info )
     !! ZSPTRF computes the factorization of a complex symmetric matrix A
     !! stored in packed format using the Bunch-Kaufman diagonal pivoting
     !! method:
     !! A = U*D*U**T  or  A = L*D*L**T
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kc, kk, knc, kp, kpc, kstep, kx, npp
           real(dp) :: absakk, alpha, colmax, rowmax
           complex(dp) :: d11, d12, d21, d22, r1, t, wk, wkm1, wkp1, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSPTRF', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**t using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              kc = ( n-1 )*n / 2_ilp + 1_ilp
              10 continue
              knc = kc
              ! if k < 1, exit from loop
              if( k<1 )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( ap( kc+k-1 ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k>1_ilp ) then
                 imax = stdlib_izamax( k-1, ap( kc ), 1_ilp )
                 colmax = cabs1( ap( kc+imax-1 ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    rowmax = zero
                    jmax = imax
                    kx = imax*( imax+1 ) / 2_ilp + imax
                    do j = imax + 1, k
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + j
                    end do
                    kpc = ( imax-1 )*imax / 2_ilp + 1_ilp
                    if( imax>1_ilp ) then
                       jmax = stdlib_izamax( imax-1, ap( kpc ), 1_ilp )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( ap( kpc+imax-1 ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k - kstep + 1_ilp
                 if( kstep==2_ilp )knc = knc - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_zswap( kp-1, ap( knc ), 1_ilp, ap( kpc ), 1_ilp )
                    kx = kpc + kp - 1_ilp
                    do j = kp + 1, kk - 1
                       kx = kx + j - 1_ilp
                       t = ap( knc+j-1 )
                       ap( knc+j-1 ) = ap( kx )
                       ap( kx ) = t
                    end do
                    t = ap( knc+kk-1 )
                    ap( knc+kk-1 ) = ap( kpc+kp-1 )
                    ap( kpc+kp-1 ) = t
                    if( kstep==2_ilp ) then
                       t = ap( kc+k-2 )
                       ap( kc+k-2 ) = ap( kc+kp-1 )
                       ap( kc+kp-1 ) = t
                    end if
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**t = a - w(k)*1/d(k)*w(k)**t
                    r1 = cone / ap( kc+k-1 )
                    call stdlib_zspr( uplo, k-1, -r1, ap( kc ), 1_ilp, ap )
                    ! store u(k) in column k
                    call stdlib_zscal( k-1, r1, ap( kc ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**t
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**t
                    if( k>2_ilp ) then
                       d12 = ap( k-1+( k-1 )*k / 2_ilp )
                       d22 = ap( k-1+( k-2 )*( k-1 ) / 2_ilp ) / d12
                       d11 = ap( k+( k-1 )*k / 2_ilp ) / d12
                       t = cone / ( d11*d22-cone )
                       d12 = t / d12
                       do j = k - 2, 1, -1
                          wkm1 = d12*( d11*ap( j+( k-2 )*( k-1 ) / 2_ilp )-ap( j+( k-1 )*k / 2_ilp ) )
                                    
                          wk = d12*( d22*ap( j+( k-1 )*k / 2_ilp )-ap( j+( k-2 )*( k-1 ) / 2_ilp ) )
                                    
                          do i = j, 1, -1
                             ap( i+( j-1 )*j / 2_ilp ) = ap( i+( j-1 )*j / 2_ilp ) -ap( i+( k-1 )*k / 2_ilp )&
                                       *wk -ap( i+( k-2 )*( k-1 ) / 2_ilp )*wkm1
                          end do
                          ap( j+( k-1 )*k / 2_ilp ) = wk
                          ap( j+( k-2 )*( k-1 ) / 2_ilp ) = wkm1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              kc = knc - k
              go to 10
           else
              ! factorize a as l*d*l**t using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              kc = 1_ilp
              npp = n*( n+1 ) / 2_ilp
              60 continue
              knc = kc
              ! if k > n, exit from loop
              if( k>n )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = cabs1( ap( kc ) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k<n ) then
                 imax = k + stdlib_izamax( n-k, ap( kc+1 ), 1_ilp )
                 colmax = cabs1( ap( kc+imax-k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    kx = kc + imax - k
                    do j = k, imax - 1
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + n - j
                    end do
                    kpc = npp - ( n-imax+1 )*( n-imax+2 ) / 2_ilp + 1_ilp
                    if( imax<n ) then
                       jmax = imax + stdlib_izamax( n-imax, ap( kpc+1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( cabs1( ap( kpc ) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k + kstep - 1_ilp
                 if( kstep==2_ilp )knc = knc + n - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_zswap( n-kp, ap( knc+kp-kk+1 ), 1_ilp, ap( kpc+1 ),1_ilp )
                              
                    kx = knc + kp - kk
                    do j = kk + 1, kp - 1
                       kx = kx + n - j + 1_ilp
                       t = ap( knc+j-kk )
                       ap( knc+j-kk ) = ap( kx )
                       ap( kx ) = t
                    end do
                    t = ap( knc )
                    ap( knc ) = ap( kpc )
                    ap( kpc ) = t
                    if( kstep==2_ilp ) then
                       t = ap( kc+1 )
                       ap( kc+1 ) = ap( kc+kp-k )
                       ap( kc+kp-k ) = t
                    end if
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**t = a - w(k)*(1/d(k))*w(k)**t
                       r1 = cone / ap( kc )
                       call stdlib_zspr( uplo, n-k, -r1, ap( kc+1 ), 1_ilp,ap( kc+n-k+1 ) )
                       ! store l(k) in column k
                       call stdlib_zscal( n-k, r1, ap( kc+1 ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**t
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**t
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d21 = ap( k+1+( k-1 )*( 2_ilp*n-k ) / 2_ilp )
                       d11 = ap( k+1+k*( 2_ilp*n-k-1 ) / 2_ilp ) / d21
                       d22 = ap( k+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) / d21
                       t = cone / ( d11*d22-cone )
                       d21 = t / d21
                       do j = k + 2, n
                          wk = d21*( d11*ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp )-ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp ) )
                                    
                          wkp1 = d21*( d22*ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp )-ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp )&
                                     )
                          do i = j, n
                             ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) = ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) - ap( &
                                       i+( k-1 )*( 2_ilp*n-k ) /2_ilp )*wk - ap( i+k*( 2_ilp*n-k-1 ) / 2_ilp )*wkp1
                          end do
                          ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) = wk
                          ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp ) = wkp1
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              kc = knc + n - k + 2_ilp
              go to 60
           end if
           110 continue
           return
     end subroutine stdlib_zsptrf



end submodule stdlib_lapack_solve_ldl_comp
