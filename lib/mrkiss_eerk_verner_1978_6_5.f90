! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_eerk_verner_1978_6_5.f90
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
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
!> Butcher tableau for Verner's 8 stage, Order (6,5) Runge-Kutta method (1978)
!!
!! @image html eerk_verner_1978_6_5-stab.png
!!
!! @par IMO
!! This is a good general method; however, I think mrkiss_eerk_verner_2010_6_5 should be preferred.
!!
!! @par Known Aliases
!! 'ARKODE_VERNER_8_5_6' (SUNDIALS).
!!
!! @par Stability Image Links
!! <a href="eerk_verner_1978_6_5-stab.png">  <img src="eerk_verner_1978_6_5-stab.png"  width="256px"> </a>
!! <a href="eerk_verner_1978_6_5-astab.png"> <img src="eerk_verner_1978_6_5-astab.png" width="256px"> </a>
!! <a href="eerk_verner_1978_6_5-star1.png"> <img src="eerk_verner_1978_6_5-star1.png" width="256px"> </a>
!! <a href="eerk_verner_1978_6_5-star2.png"> <img src="eerk_verner_1978_6_5-star2.png" width="256px"> </a>
!!
!! @par References:
!!  - Verner (1978); Explicit Runge-Kutta methods with estimates of the local truncation error; SIAM J. Numer. Anal 15; 
!!    (4)1; p772-790; zotero://select/items/0_9HLA6P9B
!!
module mrkiss_eerk_verner_1978_6_5
  use mrkiss_config, only: rk
  implicit none
  public
  !> The order of the overall method
  integer,          parameter :: s      = 8
  !> Number of methods
  integer,          parameter :: m      = 2
  !> The @f$\mathbf{a}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: a(s,s) = reshape([           0.0_rk,             0.0_rk,             0.0_rk,           0.0_rk,          0.0_rk,       0.0_rk,          0.0_rk,       0.0_rk,  &
       &                                             3026340000.0_rk,             0.0_rk,             0.0_rk,           0.0_rk,          0.0_rk,       0.0_rk,          0.0_rk,       0.0_rk,  &
       &                                              968428800.0_rk,    3873715200.0_rk,             0.0_rk,           0.0_rk,          0.0_rk,       0.0_rk,          0.0_rk,       0.0_rk,  &
       &                                            15131700000.0_rk,  -48421440000.0_rk,   45395100000.0_rk,           0.0_rk,          0.0_rk,       0.0_rk,          0.0_rk,       0.0_rk,  &
       &                                           -46813696875.0_rk,  166448700000.0_rk, -120580734375.0_rk, 16077431250.0_rk,          0.0_rk,       0.0_rk,          0.0_rk,       0.0_rk,  &
       &                                            43579296000.0_rk, -145264320000.0_rk,  119125050000.0_rk, -5548290000.0_rk, 6266304000.0_rk,       0.0_rk,          0.0_rk,       0.0_rk,  &
       &                                           -10002658968.0_rk,   30021292800.0_rk,  -17170029000.0_rk, -5883204960.0_rk, 4245136128.0_rk,       0.0_rk,          0.0_rk,       0.0_rk,  &
       &                                            36960057000.0_rk, -126684000000.0_rk,  102559875000.0_rk, -2494580000.0_rk, 5198688000.0_rk,       0.0_rk, 2618000000.0_rk,       0.0_rk], [s, s]) / 18158040000.0_rk
  !> The @f$\mathbf{b}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource                                                                                     
  real(kind=rk),    parameter :: b(s,m) = reshape([     3251556.0_rk,             0.0_rk,      16905000.0_rk,    13849220.0_rk,    5854464.0_rk,       0.0_rk,     467500.0_rk, 3026340.0_rk,  &
       &                                                3522519.0_rk,             0.0_rk,      17206875.0_rk,    13548150.0_rk,    6120576.0_rk, 2955960.0_rk,          0.0_rk,       0.0_rk], [s, m]) /    43354080.0_rk
  !> The @f$\mathbf{c}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: c(s)   = [                   0.0_rk,             5.0_rk,             8.0_rk,          20.0_rk,         25.0_rk,      30.0_rk,          2.0_rk,      30.0_rk]          /          30.0_rk
  !> The method orders
  integer,          parameter :: p(m)   = [6, 5]
  !> Number of stages for each method
  integer,          parameter :: se(m)  = [8, 6]
end module mrkiss_eerk_verner_1978_6_5
