module ConjGradMethod
  use SparseMatrixForm
  implicit none

contains

  subroutine cgCalc(sparse_matrix, source, phi_guess, convergence_limit)
    class(sparseMatrix), intent(in)                      ::    sparse_matrix
    double precision, dimension(:), intent(in)          ::    source
    double precision, dimension(:), intent(inout)       ::    phi_guess
    double precision, intent(in)                        ::    convergence_limit
    double precision, allocatable, dimension(:)         ::    residuala
    double precision, allocatable, dimension(:)         ::    residualb
    double precision, allocatable, dimension(:)         ::    p
    double precision                                    ::    alpha

    allocate(residuala(size(phi_guess)))
    allocate(residualb(size(phi_guess)))
    allocate(p(size(phi_guess)))

    residuala = source - sparse_matrix%multDblVecSM(phi_guess)
    p = residuala
    write(*,*) residuala
    do while(residuala < convergence_limit)
      alpha = dot_product(residuala,residuala)/dot_product(p,sparse_matrix%multDblVecSM(p))
      phi_guess = phi_guess + alpha*p
      residualb = residuala - alpha*sparse_matrix%multDblVecSM(p)
      beta = dot_product(residualb,residualb) / dot_product(residuala,residuala)
      p = residualb + beta*p
      write(*,*) residualb
      residuala = residualb
    end do

    deallocate(residuala)
    deallocate(residualb)
    deallocate(p)

  end subroutine cgCalc
end module
