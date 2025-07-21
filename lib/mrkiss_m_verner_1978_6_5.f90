! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_m_verner_1978_6_5.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for Verner's 8 stage, Order (6,5) Runge-Kutta method (1978).@EOL
!! @keywords  ode ivp differential equation initial value problem rk
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
!> Butcher tableau for Verner's 8 stage, Order (6,5) Runge-Kutta method (1978)
!!
!! IMO: This is a good general method; however, I think mrkiss_m_verner_2010_6_5 should be preferred.
!!
!! Known Aliases: 'ARKODE_VERNER_8_5_6' (SUNDIALS).
!!
!! References:
!!   Verner (1978); Explicit Runge-Kutta methods with estimates of the local truncation error; SIAM J. Numer. Anal 15 (4)l p772-790; zotero://select/items/0_9HLA6P9B
!!
module mrkiss_m_verner_1978_6_5
  use mrkiss_config, only: rk, ik
  implicit none
  public
  integer(kind=ik), parameter :: s      = 8
  real(kind=rk),    parameter :: a(s,s) = reshape([           0.0_rk,             0.0_rk,             0.0_rk,           0.0_rk,          0.0_rk,    0.0_rk,          0.0_rk,      0.0_rk, &
                                                     3026340000.0_rk,             0.0_rk,             0.0_rk,           0.0_rk,          0.0_rk,    0.0_rk,          0.0_rk,      0.0_rk, &
                                                      968428800.0_rk,    3873715200.0_rk,             0.0_rk,           0.0_rk,          0.0_rk,    0.0_rk,          0.0_rk,      0.0_rk, &
                                                    15131700000.0_rk,  -48421440000.0_rk,   45395100000.0_rk,           0.0_rk,          0.0_rk,    0.0_rk,          0.0_rk,      0.0_rk, &
                                                   -46813696875.0_rk,  166448700000.0_rk, -120580734375.0_rk, 16077431250.0_rk,          0.0_rk,    0.0_rk,          0.0_rk,      0.0_rk, &
                                                    43579296000.0_rk, -145264320000.0_rk,  119125050000.0_rk, -5548290000.0_rk, 6266304000.0_rk,    0.0_rk,          0.0_rk,      0.0_rk, &
                                                   -10002658968.0_rk,   30021292800.0_rk,  -17170029000.0_rk, -5883204960.0_rk, 4245136128.0_rk,    0.0_rk,          0.0_rk,      0.0_rk, &
                                                    36960057000.0_rk, -126684000000.0_rk,  102559875000.0_rk, -2494580000.0_rk, 5198688000.0_rk,    0.0_rk, 2618000000.0_rk,      0.0_rk], [s, s]) / 18158040000.0_rk
  real(kind=rk),    parameter :: c(s)   = [                   0.0_rk,             5.0_rk,             8.0_rk,          20.0_rk,         25.0_rk,   30.0_rk,          2.0_rk,     30.0_rk]          /          30.0_rk
  integer(kind=ik), parameter :: p1     = 6
  real(kind=rk),    parameter :: b1(s)  = [                2431.0_rk,             0.0_rk,         11875.0_rk,        9350.0_rk,       4224.0_rk, 2040.0_rk,          0.0_rk,      0.0_rk]          /       29920.0_rk
  integer(kind=ik), parameter :: p2     = 5
  real(kind=rk),    parameter :: b2(s)  = [              812889.0_rk,             0.0_rk,       4226250.0_rk,     3462305.0_rk,    1463616.0_rk,    0.0_rk,     116875.0_rk, 756585.0_rk]          /    10838520.0_rk
end module mrkiss_m_verner_1978_6_5
