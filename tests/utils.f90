
!----------------------------------------------------------------------------------------------------------------------------------
program langford
  use :: mrkiss_config,          only: rk, istats_size
  use :: mrkiss_solvers_nt,      only: steps_fixed_stab
  use :: mrkiss_utils,           only: analyze_solution, print_istats, print_solution
  use :: mrkiss_erk_kutta_4, only: a, b, c

  implicit none

  integer,        parameter :: deq_dim       = 2
  integer,        parameter :: num_points    = 4
  real(kind=rk),  parameter :: param(2)      = [1.0_rk, -1.0_rk]
  real(kind=rk),  parameter :: y_iv(deq_dim) = [1.0_rk,  1.0_rk]
  real(kind=rk),  parameter :: t_delta       = 0.01_rk

  real(kind=rk)             :: solution(1+2*deq_dim, num_points)
  integer                   :: status, istats(istats_size), i
  character(len=512)        :: filename

  call steps_fixed_stab(status, istats, solution, eq, y_iv, param, a, b, c, t_delta_o=t_delta)

  call print_istats(status, istats,                   filename_o="utils_istats_def.out")
  call print_istats(status, istats,       fmt_w_o=8,  filename_o="utils_istats_w8.out")
  call analyze_solution(status, solution,             filename_o="utils_anal_def.out")
  call analyze_solution(status, solution, fmt_w_o=30, filename_o="utils_anal_w30.out")
  call analyze_solution(status, solution, fmt_e_o=2,  filename_o="utils_anal_e2.out")
  call analyze_solution(status, solution, fmt_d_o=5,  filename_o="utils_anal_d5.out")
  call print_solution(status, solution,               filename_o="utils_print_def.out")
  call print_solution(status, solution,   fmt_w_o=-1, filename_o="utils_print_wstd.out")
  call print_solution(status, solution,   fmt_w_o=30, filename_o="utils_print_w30.out")
  call print_solution(status, solution,   fmt_d_o=5,  filename_o="utils_print_d5.out")
  call print_solution(status, solution,   fmt_e_o=2,  filename_o="utils_print_e2.out")

contains
  
  subroutine eq(status, dydt, y, param)
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: y(:)
    real(kind=rk),    intent(in)  :: param(:)
    dydt = [param(1)*y(1), param(2)*y(2)]
    status = 0
  end subroutine eq

end program langford
