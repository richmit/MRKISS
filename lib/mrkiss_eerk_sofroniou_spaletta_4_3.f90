! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_eerk_sofroniou_spaletta_4_3.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for Sofroniou Spaletta's 5 step, order (4,3) Runge-Kutta method.@EOL
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
!> Butcher tableau for Sofroniou Spaletta's 5 step, order (4,3) Runge-Kutta method
!!
!! @image html eerk_sofroniou_spaletta_4_3-stab.png
!!
!! @par IMO
!! This is an interesting method in that one of the goals was to detect stiffness.  That said, it's a very good general
!! purpose method of order 4.  
!!
!! @par Known Aliases
!! 'ARKODE_SOFRONIOU_SPALETTA_5_3_4' (SUNDIALS)
!!
!! @par Stability Image Links
!! <a href="eerk_sofroniou_spaletta_4_3-stab.png">  <img src="eerk_sofroniou_spaletta_4_3-stab.png"  width="256px"> </a>
!! <a href="eerk_sofroniou_spaletta_4_3-astab.png"> <img src="eerk_sofroniou_spaletta_4_3-astab.png" width="256px"> </a>
!! <a href="eerk_sofroniou_spaletta_4_3-star1.png"> <img src="eerk_sofroniou_spaletta_4_3-star1.png" width="256px"> </a>
!! <a href="eerk_sofroniou_spaletta_4_3-star2.png"> <img src="eerk_sofroniou_spaletta_4_3-star2.png" width="256px"> </a>
!!
!! @par References:
!!  - Sofroniou & Spaletta (2004); Construction of Explicit Runge-Kutta Pairs with Stiffness Detection; Math. and Comp. Modelling; 
!!    40(11); p1157-1169; zotero://select/items/0_52X4WPJK
!!
module mrkiss_eerk_sofroniou_spaletta_4_3
  use mrkiss_config, only: rk
  implicit none
  public
  !> The order of the overall method
  integer,          parameter :: s      = 5
  !> Number of methods
  integer,          parameter :: m      = 2
  !> The @f$\mathbf{a}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: a(s,s) = reshape([      0.0_rk,        0.0_rk,       0.0_rk,       0.0_rk,        0.0_rk, &
                                                      1584.0_rk,        0.0_rk,       0.0_rk,       0.0_rk,        0.0_rk, &
                                                      -594.0_rk,     2970.0_rk,       0.0_rk,       0.0_rk,        0.0_rk, &
                                                      1710.0_rk,    -1350.0_rk,    3600.0_rk,       0.0_rk,        0.0_rk, &
                                                       605.0_rk,     1375.0_rk,    1375.0_rk,     605.0_rk,        0.0_rk], [s, s]) /    3960.0_rk
  !> The @f$\mathbf{b}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: b(s,m) = reshape([1370556.0_rk,  3114900.0_rk, 3114900.0_rk, 1370556.0_rk,        0.0_rk, &
       &                                           1251515.0_rk,  3710105.0_rk, 2519695.0_rk,   61105.0_rk, 1428492.0_rk], [s, m]) /  8970912.0_rk
  !> The @f$\mathbf{c}@f$ matrix for the Butcher Tableau. @hideinitializer @showinlinesource
  real(kind=rk),    parameter :: c(s)   = [             0.0_rk,         2.0_rk,       3.0_rk,       5.0_rk,       5.0_rk]          /       5.0_rk
  !> The method orders
  integer,          parameter :: p(m)   = [4, 3]
  !> Number of stages for each method
  integer,          parameter :: se(m)  = [4, 5]
end module mrkiss_eerk_sofroniou_spaletta_4_3
