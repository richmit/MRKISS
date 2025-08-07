! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrkiss_config.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @brief     Configuration for MRKISS == MR RK KISS == Mitch Richling's Runge-Kutta Keep It Simple Stupid.@EOL
!! @std       F2023
!! @see       https://github.com/richmit/MRKISS/
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
!> Configuration for MRKISS == MR RK KISS == Mitch Richling's Runge-Kutta Keep It Simple Stupid
!!
module mrkiss_config
  use, intrinsic:: iso_c_binding, only: c_int32_t, c_double, c_float, c_bool
  implicit none
  private

  ! Real & integer types for externally viable interfaces and arguments
  integer,          parameter, public :: ik                 = c_int32_t                       !< Integer kinds used in interfaces
  integer,          parameter, public :: rk                 = c_double                        !< Real kind used in interfaces
  integer,          parameter, public :: bk                 = c_bool                          !< Integer kind for boolean

  ! Constants related to the library
  integer,          parameter, public :: istats_size        = 16                              !< Number of elements in istats(:)

  ! Absolute constants
  real(kind=rk),    parameter, public :: zero_epsilon       = 1.0e-12_rk                      !< Used to test for zero
  real(kind=rk),    parameter, public :: t_delta_tiny       = tiny(zero_epsilon) * 1.0e18_rk  !< Smallest value for t_delta

  ! ab initio parameters for defaults when we know nothing
  real(kind=rk),    parameter, public :: t_delta_ai         = 1.0e-4_rk                       !< t_delta ai default
  real(kind=rk),    parameter, public :: error_tol_abs_ai   = 1.0e-5_rk                       !< error_tol_abs ai default
  real(kind=rk),    parameter, public :: error_tol_rel_ai   = 1.0e-3_rk                       !< error_tol_rel ai default
  integer(kind=ik), parameter, public :: max_bisect_ai      = 1000                            !< max_bisect ai default
  real(kind=rk),    parameter, public :: t_delta_fac_min_ai = 0.5_rk                          !< t_delta_fac_min ai default
  real(kind=rk),    parameter, public :: t_delta_fac_max_ai = 2.0_rk                          !< t_delta_fac_max ai default
  real(kind=rk),    parameter, public :: t_delta_fac_fdg_ai = 0.5_rk                          !< t_delta_fac_fdg ai default
  real(kind=rk),    parameter, public :: sdf_tol_ai         = 1.0e-3                          !< sdf_tol ai default
end module mrkiss_config


!----------------------------------------------------------------------------------------------------------------------------------
!> \mainpage 
!! 
!! @image html MRKISS_logo_c_500x250.png
!!
!! MRKISS == MR RK KISS == Mitch Richling's Runge-Kutta Keep It Simple Stupid
!!
!! [MRKISS](https://github.com/richmit/MRKISS) is a *tiny* library with *zero dependencies* that aims to make it easy to *use*
!! and *experiment with* explicit Runge-Kutta methods.
!! 
!! For an overview of MRKISS and some tutorial information, check out the [documentation](https://richmit.github.io/MRKISS/index.html).
!! 
!! A couple of the examples in the repository have longer explanatory documentation:
!!    - [Three Body Problem](https://richmit.github.io/MRKISS/ex_three_body.html)
!!    - [Lornez Attracter](https://richmit.github.io/MRKISS/ex_lorenz.html)
!!    - [Step Size & Order Vs. Accuracy](https://richmit.github.io/MRKISS/ex_step_order_vs_error.html)
!! 
!! Generated [API documentation](https://www.mitchr.me/SS/MRKISS/doc-lib/html/index.html) is viable as well.
!! 
!! Development News:
!!   - [changelog](https://richmit.github.io/MRKISS/changelog.html)
!!   - [roadmap](https://richmit.github.io/MRKISS/roadmap.html)
!! 
!! This library powers the [Strange Attractor Zoo](https://richmit.github.io/StrangeAttractorZoo/).
!! 
!! The canonical URL for this documentation: https://www.mitchr.me/SS/MRKISS/doc-lib/html/index.html
!!
