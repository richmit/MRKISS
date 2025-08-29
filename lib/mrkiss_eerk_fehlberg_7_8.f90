! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_eerk_fehlberg_7_8.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for Fehlberg's 13 stage, Order (7,8) Runge-Kutta method.@EOL
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
!> Butcher tableau for Fehlberg's 13 stage, Order (7,8) Runge-Kutta method
!!
!! IMO: Included for historical reasons.  Performs poorly in local extrapolation mode.  This one was pretty broadly used, but not
!!      nearly as much as mrkiss_eerk_fehlberg_4_5.  It was largely replaced by mrkiss_eerk_dormand_prince_7_8.
!!
!! Known Aliases: 'RKF78'', 'ode78' (OrdinaryDiffEq.jl), 'ARKODE_FEHLBERG_13_7_8' (SUNDIALS).
!!
!! @image html eerk_fehlberg_7_8-stab.gif
!!
!! @par References:
!!  - Erwin Fehlberg (1972); Classical eight- and lower-order Runge-Kutta-Nystroem formulas with stepsize control for 
!!    special second-order differential equations; NASA Technical Report M-533
!!  - Butcher (2008); Numerical Methods for Ordinary Differential Equations. 2ed; p209; zotero://select/items/0_8V2GY73E
!!  - Butcher (2016); Numerical Methods for Ordinary Differential Equations. 3ed; p222; zotero://select/items/0_V7UTIRPT
!!  - Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p180; 
!!
module mrkiss_eerk_fehlberg_7_8
  use mrkiss_config, only: rk
  implicit none
  public
  !> The order of the overall method
  integer,          parameter :: s          = 13
  !> The @f$\mathbf{a}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: a(s,s) = reshape([      0.0_rk,     0.0_rk,       0.0_rk,        0.0_rk,        0.0_rk,        0.0_rk,       0.0_rk,       0.0_rk,      0.0_rk,      0.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                     32800.0_rk,     0.0_rk,       0.0_rk,        0.0_rk,        0.0_rk,        0.0_rk,       0.0_rk,       0.0_rk,      0.0_rk,      0.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                     12300.0_rk, 36900.0_rk,       0.0_rk,        0.0_rk,        0.0_rk,        0.0_rk,       0.0_rk,       0.0_rk,      0.0_rk,      0.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                     18450.0_rk,     0.0_rk,   55350.0_rk,        0.0_rk,        0.0_rk,        0.0_rk,       0.0_rk,       0.0_rk,      0.0_rk,      0.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                    184500.0_rk,     0.0_rk, -691875.0_rk,   691875.0_rk,        0.0_rk,        0.0_rk,       0.0_rk,       0.0_rk,      0.0_rk,      0.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                     22140.0_rk,     0.0_rk,       0.0_rk,   110700.0_rk,    88560.0_rk,        0.0_rk,       0.0_rk,       0.0_rk,      0.0_rk,      0.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                   -102500.0_rk,     0.0_rk,       0.0_rk,   512500.0_rk, -1066000.0_rk,  1025000.0_rk,       0.0_rk,       0.0_rk,      0.0_rk,      0.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                     45756.0_rk,     0.0_rk,       0.0_rk,        0.0_rk,   120048.0_rk,   -98400.0_rk,    6396.0_rk,       0.0_rk,      0.0_rk,      0.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                    885600.0_rk,     0.0_rk,       0.0_rk, -3911400.0_rk,  6927360.0_rk, -5264400.0_rk,  329640.0_rk, 1328400.0_rk,      0.0_rk,      0.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                   -373100.0_rk,     0.0_rk,       0.0_rk,    94300.0_rk, -3201280.0_rk,  2550200.0_rk, -140220.0_rk, 1254600.0_rk, -36900.0_rk,      0.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                    257364.0_rk,     0.0_rk,       0.0_rk,  -920700.0_rk,  1942272.0_rk, -1625400.0_rk,  230364.0_rk,  243000.0_rk, 121500.0_rk, 194400.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                      6480.0_rk,     0.0_rk,       0.0_rk,        0.0_rk,        0.0_rk,   -64800.0_rk,   -6480.0_rk,  -32400.0_rk,  32400.0_rk,  64800.0_rk,   0.0_rk,      0.0_rk,   0.0_rk, &
                                                   -191916.0_rk,     0.0_rk,       0.0_rk,  -920700.0_rk,  1942272.0_rk, -1560600.0_rk,  236844.0_rk,  275400.0_rk,  89100.0_rk, 129600.0_rk,   0.0_rk, 442800.0_rk,   0.0_rk], [s, s]) / 442800.0_rk
  !> The @f$\mathbf{c}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: c(s)   = [              0.0_rk,     8.0_rk,      12.0_rk,       18.0_rk,       45.0_rk,       54.0_rk,      90.0_rk,      18.0_rk,     72.0_rk,     36.0_rk, 108.0_rk,      0.0_rk, 108.0_rk]          /    108.0_rk
  !> The order of the @f$\mathbf{b_1}@f$ method
  integer,          parameter :: p1     = 7
  !> Number of stages for the @f$\mathbf{b_1}@f$ method
  integer,          parameter :: s1     = 11
  !> The @f$\mathbf{b_1}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: b1(s)  = [              41.0_rk,    0.0_rk,      0.0_rk,         0.0_rk,        0.0_rk,      272.0_rk,     216.0_rk,     216.0_rk,     27.0_rk,     27.0_rk,  41.0_rk,      0.0_rk,   0.0_rk]          /    840.0_rk
  !> The order of the @f$\mathbf{b_2}@f$ method
  integer,          parameter :: p2     = 8
  !> The @f$\mathbf{b_2}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: b2(s)  = [              0.0_rk,     0.0_rk,      0.0_rk,         0.0_rk,        0.0_rk,      272.0_rk,     216.0_rk,     216.0_rk,     27.0_rk,     27.0_rk,   0.0_rk,     41.0_rk,  41.0_rk]          /    840.0_rk
end module mrkiss_eerk_fehlberg_7_8
