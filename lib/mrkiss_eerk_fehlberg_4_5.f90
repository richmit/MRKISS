! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!.!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_eerk_fehlberg_4_5.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Butcher tableau for Fehlberg's 6 stage, Order (4,5) Runge-Kutta method.@EOL
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
!> Butcher tableau for Fehlberg's 6 stage, Order (4,5) Runge-Kutta method
!!
!! IMO: Included for historical reasons and for unit tests.  Performs poorly in local extrapolation mode.  This method was
!!      broadly implemented, and widely used.  As such it is a standard used for comparison frequently in older literature.  It
!!      was largely replaced by mrkiss_eerk_dormand_prince_5_4.
!!
!! Known Aliases: 'RKF78'', 'ode78' (OrdinaryDiffEq.jl), 'ARKODE_FEHLBERG_6_4_5' (SUNDIALS).
!!
!! @image html eerk_fehlberg_4_5-stab.png
!!
!! @par References:
!!  - Erwin Fehlberg (1972); Classical eight and lower-order Runge-Kutta-Nystroem formulas with stepsize control for 
!!    special second-order differential equations; NASA Tech. Rep. M-533
!!  - Butcher (2008); Numerical Methods for Ordinary Differential Equations. 2ed; p209; zotero://select/items/0_8V2GY73E
!!  - Butcher (2016); Numerical Methods for Ordinary Differential Equations. 3ed; p222; zotero://select/items/0_V7UTIRPT
!!  - Hairer, Norsett & Wanner (2009). Solving Ordinary Differential Equations. I: Nonstiff Problems. p180; 
!!    zotero://select/items/0_VLZWN2CT
!!
module mrkiss_eerk_fehlberg_4_5
  use mrkiss_config, only: rk
  implicit none
  public
  !> The order of the overall method
  integer,          parameter :: s      = 6
  !> The @f$\mathbf{a}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: a(s,s) = reshape([         0.0_rk,           0.0_rk,          0.0_rk,         0.0_rk,         0.0_rk,         0.0_rk, &
                                                     45082440.0_rk,           0.0_rk,          0.0_rk,         0.0_rk,         0.0_rk,         0.0_rk, &
                                                     16905915.0_rk,    50717745.0_rk,          0.0_rk,         0.0_rk,         0.0_rk,         0.0_rk, &
                                                    158578560.0_rk,  -590976000.0_rk,  598855680.0_rk,         0.0_rk,         0.0_rk,         0.0_rk, &
                                                    366503540.0_rk, -1442638080.0_rk, 1293593600.0_rk, -37129300.0_rk,         0.0_rk,         0.0_rk, &
                                                    -53431040.0_rk,   360659520.0_rk, -249157376.0_rk,  81684460.0_rk, -49590684.0_rk,         0.0_rk], [s, s]) / 180329760.0_rk
  !> The @f$\mathbf{c}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: c(s)   = [                 0.0_rk,          26.0_rk,         39.0_rk,        96.0_rk,       104.0_rk,       52.0_rk ]          /       104.0_rk
  !> The order of the @f$\mathbf{b_1}@f$ method
  integer,          parameter :: p1     = 4                                                                                                  
  !> The @f$\mathbf{b_1}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: b1(s)  = [              2375.0_rk,           0.0_rk,       11264.0_rk,    10985.0_rk,      -4104.0_rk,       0.0_rk ]          /     20520.0_rk
  !> The order of the @f$\mathbf{b_2}@f$ method
  integer,          parameter :: p2     = 5                                                                                              
  !> The @f$\mathbf{b_2}@f$ matrix for the Butcher Tableau. @hideinitializer @hideinlinesource
  real(kind=rk),    parameter :: b2(s)  = [             33440.0_rk,           0.0_rk,      146432.0_rk,   142805.0_rk,     -50787.0_rk,   10260.0_rk ]          /    282150.0_rk
end module mrkiss_eerk_fehlberg_4_5

