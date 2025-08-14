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
!!  Run with various thread counts on linux (eshell):
!!    bash -c 'export OMP_NUM_THREADS=16; time ./langford'
!!  Run with various thread counts on windows (eshell):
!!    bash -c 'export OMP_NUM_THREADS=16; time ./langford.exe '
!!  Convert CSVs to VTUs (eshell):
!!    for f in langford_??.csv {~/world/my_prog/learn/ex-VTK/xml_files/spaceCurveCSVtoVTU.rb $f points:4:5:6 time:3 tag:1 step:2 derivative:7:8:9 > $(file-name-sans-extension f).vtu}
!!    ~/world/my_prog/learn/ex-VTK/xml_files/spaceCurveCSVtoVTU.rb langford_fixed.csv points:3:4:5 time:2 step:1 derivative:6:7:8 > langford_fixed.vtu
!!
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program langford
  use :: mrkiss_config,          only: rk, istats_size
  use :: mrkiss_solvers_nt,      only: steps_fixed_stab
  use :: mrkiss_utils,           only: analyze_solution, print_istats, print_solution
  use :: mrkiss_eerk_verner_9_8, only: a, b=>b1, c
  use :: omp_lib

  implicit none

  integer,       parameter :: y_dim         = 3
  integer,       parameter :: np            = 10000000
  real(kind=rk), parameter :: param(6)      = [0.95_rk, 0.7_rk, 0.6_rk, 3.5_rk, 0.25_rk, 0.1_rk]
  real(kind=rk), parameter :: t_delta       = 0.01_rk
  real(kind=rk), parameter :: y_iv(y_dim)   = [0.1_rk, 0.0_rk, 0.0_rk]
  real(kind=rk)            :: solution(1+2*y_dim, np)
  integer                  :: status, istats(istats_size)

  call steps_fixed_stab(status, istats, solution, eq, y_iv, param, a, b, c, t_delta_o=t_delta)
  call print_solution(status, solution, fmt_w_o=-1, start_o=istats(1)-10)
  call analyze_solution(status, solution)
  call print_istats(status, istats)

contains
  
  subroutine eq(status, dydt, y, param)
    integer,       intent(out) :: status
    real(kind=rk), intent(out) :: dydt(:)
    real(kind=rk), intent(in)  :: y(:)
    real(kind=rk), intent(in)  :: param(:)
    dydt = [(y(3) - param(2)) * y(1) - param(4) * y(2), param(4) * y(1) + (y(3) - param(2)) * y(2), param(3) + param(1) * y(3) - (y(3)**3 / 3) - (y(1)**2 + y(2)**2) * (1 + param(5) * y(3)) + param(6) * y(3) * y(1)**3]
    status = 0
  end subroutine eq

end program langford
