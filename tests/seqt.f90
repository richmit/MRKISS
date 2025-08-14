

!----------------------------------------------------------------------------------------------------------------------------------
program seqt
  use            :: mrkiss_config,      only: rk
  use            :: mrkiss_utils,       only: seq

  implicit none
  integer,           parameter :: num_num   = 11
  real(kind=rk),     parameter :: in_from   = 0.0_rk
  real(kind=rk),     parameter :: in_to     = 1.0_rk
  real(kind=rk),     parameter :: in_step   = 0.1_rk
  real(kind=rk),     parameter :: out_step  = in_step + 1.0_rk
  real(kind=rk)                :: t(num_num)
  integer                      :: out_io_unit
  integer                      :: status

  open(newunit=out_io_unit, file="seqt.out", form='formatted', action='write')

  t = -1
  call seq(status, t, from_o=in_from, to_o=in_to)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, from_o=in_from, step_o=in_step)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, to_o=in_to, step_o=in_step)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, to_o=in_to, step_o=in_step, from_o=in_from)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  write (out_io_unit, fmt='(a)') repeat('*', 100)

  t = -1
  call seq(status, t, from_o=in_from, to_o=in_to, num_pts_o=16)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, from_o=in_from, step_o=in_step, num_pts_o=16)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, to_o=in_to, step_o=in_step, num_pts_o=16)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, to_o=in_to, step_o=in_step, from_o=in_from, num_pts_o=16)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  write (out_io_unit, fmt='(a)') repeat('*', 100)

  t = -1
  call seq(status, t, from_o=in_from, to_o=in_to, num_pts_o=6)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, from_o=in_from, step_o=in_step, num_pts_o=6)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, to_o=in_to, step_o=in_step, num_pts_o=6)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, to_o=in_to, step_o=in_step*2, from_o=in_from, num_pts_o=6)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  write (out_io_unit, fmt='(a)') repeat('*', 100)

  t = -1
  call seq(status, t, to_o=in_to, step_o=in_step, from_o=in_from, num_pts_o=6)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, to_o=in_to, step_o=out_step, from_o=in_from)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, to_o=in_to)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, step_o=out_step)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t

  t = -1
  call seq(status, t, from_o=in_from)
  write (out_io_unit, fmt='(i5)') status
  write (out_io_unit, fmt='(*(f8.3))') t


  close(unit=out_io_unit, status='keep')
end program seqt
