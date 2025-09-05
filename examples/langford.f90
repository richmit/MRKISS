! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      langford.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     MRKISS example solving Langford system.@EOL
!! @keywords  Runge Kutta ODE IVP initial value problem ordinary differential equation numerical
!! @std       F2023
!! @see       https://github.com/richmit/MRKISS
!! @copyright 
!!  @parblock
!!  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
!!  
!!  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
!!  conditions are met:
!!  
!!  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following
!!     disclaimer.
!!  
!!  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following
!!     disclaimer in the documentation and/or other materials provided with the distribution.
!!  
!!  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products
!!     derived from this software without specific prior written permission.
!!  
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
!!  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
!!  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
!!  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
!!  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
!!  OF THE POSSIBILITY OF SUCH DAMAGE.
!!  @endparblock
!! @filedetails   
!!
!!  The Langford attractor is governed by the following equations:
!!  
!!   @f[ \begin{align*}
!!        \frac{\mathrm{d}x}{\mathrm{d}t} & = (z - \beta) x - \omega y \\                                                  
!!        \frac{\mathrm{d}y}{\mathrm{d}t} & = \omega x + (z - \beta) y \\                                                  
!!        \frac{\mathrm{d}z}{\mathrm{d}t} & = \lambda + \alpha z - \frac{z^3}{3} - (x^2 + y^2) (1 + \rho z) + \epsilon z x^3
!!   \end{align*} @f]
!!  
!!  With the following parameter values:
!!  
!!   @f[ \begin{align*}
!!        \alpha   & = 0.95 \\
!!        \beta    & = 0.70 \\
!!        \lambda  & = 0.60 \\
!!        \omega   & = 3.50 \\
!!        \rho     & = 0.25 \\
!!        \epsilon & = 0.10
!!   \end{align*} @f]
!!  
!!  And with the following initial conditions:
!!  
!!   @f[ \begin{align*}
!!        x(0) & = 0.1 \\                                                  
!!        y(0) & = 0.0 \\                                                  
!!        z(0) & = 0.0
!!   \end{align*} @f]
!!
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program langford
  use :: mrkiss_config,      only: rk, istats_size
  use :: mrkiss_solvers_nt,  only: fixed_t_steps
  use :: mrkiss_utils,       only: print_solution
  use :: mrkiss_erk_kutta_4, only: a, b, c
  use :: omp_lib

  implicit none

  real(kind=rk),  parameter :: pi         = 4.0_rk * atan(1.0_rk)
  integer,        parameter :: num_lines  = 20
  integer,        parameter :: deq_dim    = 3
  integer,        parameter :: num_points = 100000
  real(kind=rk),  parameter :: param(6)   = [0.95_rk, 0.7_rk, 0.6_rk, 3.5_rk, 0.25_rk, 0.1_rk]
  real(kind=rk),  parameter :: t_delta    = 0.01_rk

  real(kind=rk)             :: solution(1+2*deq_dim, num_points), y_iv(deq_dim)
  integer                   :: status, istats(istats_size), i
  character(len=512)        :: filename

  ! SS-BEGIN:solver-call:
  y_iv = [0.1_rk, 0.0_rk, 0.0_rk]
  call fixed_t_steps(status, istats, solution, eq, y_iv, param, a, b, c, t_delta_o=t_delta, max_pts_o=15000)
  call print_solution(status, solution, filename_o="langford_fixed.csv", end_o=istats(1))
  ! SS-END:solver-call:

  ! SS-BEGIN:par-solver:
  !$OMP PARALLEL DO private(solution, status, istats, filename, i, y_iv)
  do i=0, num_lines-1
     y_iv(1) = cos(i * 2 * pi / num_lines) * 0.15_rk + 0.2_rk
     y_iv(2) = sin(i * 2 * pi / num_lines) * 0.15_rk
     y_iv(3) = 0.05_rk
     call fixed_t_steps(status, istats, solution, eq, y_iv, param, a, b, c, t_delta_o=t_delta, max_pts_o=350)
     write (filename, '("langford_",i2.2,".csv")') i
     call print_solution(status, solution, filename_o=trim(filename), end_o=istats(1), tag_o=i)
     print *, 'Line Complete: ', i
  end do
  !$OMP END PARALLEL DO
  ! SS-END:par-solver:

contains
  
  ! SS-BEGIN:eq:
  subroutine eq(status, dydt, y, param)
    integer,          intent(out) :: status
    real(kind=rk),    intent(out) :: dydt(:)
    real(kind=rk),    intent(in)  :: y(:), param(:)
    dydt = [(y(3) - param(2)) * y(1) - param(4) * y(2), & 
            param(4)*y(1) + (y(3) - param(2))*y(2),     &
            param(3) + param(1)*y(3) - (y(3)**3/3) - (y(1)**2+y(2)**2)*(1 + param(5)*y(3)) + param(6)*y(3)*y(1)**3]
    status = 0
  end subroutine eq
  ! SS-END:eq:

end program langford
