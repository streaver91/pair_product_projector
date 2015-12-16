program projector_calc

  use bspline_oo_module
  use, intrinsic :: iso_fortran_env, only: rk => real64

  implicit none

  ! Configuration
  integer, parameter :: TITLE_MAX_LENGTH = 16
  integer, parameter :: FILENAME_MAX_LENGTH = 64
  integer, parameter :: BUF_LENGTH = 64, BUF_LENGTH2 = 16
  real(rk), parameter :: E_T = -2.175_rk
  real(rk), parameter :: MU = 1.0_rk
  real(rk), parameter :: grid_z = 0.0_rk
  real(rk), parameter :: PI = 4.0_rk*atan(1._rk)

  ! Global variables
  character(TITLE_MAX_LENGTH) :: title
  integer :: num_type ! number of types of nucleis
  integer :: tot_nuclei
  integer, allocatable :: num_nuclei(:) ! number of nucleis each type
  integer, allocatable :: z_nuclei(:) ! Z of each type
  real(rk), allocatable :: pos_nuclei(:, :) ! position of nucleis
  integer :: num_elec
  real(rk), allocatable :: pos_elec_init(:, :) ! initial position of electrons
  real(rk), allocatable :: pos_elec_final(:, :) ! final position of electrons
  integer :: num_grid
  real(rk) :: grid_xmin, grid_xmax, grid_ymin, grid_ymax
  real(rk) :: grid_dx, grid_dy
  real(rk), allocatable :: proj(:, :)
  real(rk) :: tau
  character(BUF_LENGTH2) :: tau_str
  type(bspline_2d) :: pot_ee
  type(bspline_2d), allocatable :: pot_eZ(:)
  integer :: u_ee_method

  call get_cmd_input()
  call read_input()
  call load_potential()
  call evaluate_proj()
  call output_proj()

  contains

  subroutine get_cmd_input()

    implicit none

    integer :: i
    character(len = BUF_LENGTH) :: arg

    if (iargc() == 0) then
      write (*, '("Format: program_name u_ee_method#")')
      stop
    end if

    do i = 1, iargc()
      call getarg(i, arg)
      write (*, '("Command #", A, ": ", A)') trim(str(i)), arg

      if (i == 1) then
        read (arg, *) u_ee_method
        write (*, '("Anti Symmetrization Method: ", A)'), trim(str(u_ee_method))
      end if
    end do

  end subroutine get_cmd_input
!--------------------------------------------------------------------------------------------------

  subroutine output_proj()
    ! Output calcualted proj to {title}.proj

    implicit none

    character(FILENAME_MAX_LENGTH) :: filename
    integer :: i, j
    real(rk) :: tx, ty

    write(filename, '(A, "_", A, "_M", A, ".proj")') trim(title), trim(tau_str), trim(str(U_EE_METHOD))

    open(2, file = trim(filename))

    write (2, '(3A20)') "x", "y", "proj"

    do i = 1, num_grid
      tx = grid_dx * (i - 1) + grid_xmin
      do j = 1, num_grid
        ty = grid_dy * (j - 1) + grid_ymin
        write (2, '(3ES20.10)') tx, ty, proj(i, j)
      end do
      write (2, *)
    end do

    close(2)

    write (*, '("Results Saved To: ", A)') trim(filename)

  end subroutine output_proj
!--------------------------------------------------------------------------------------------------

  subroutine evaluate_proj()
    ! Calculate position corresponding to each grid point
    ! Change the position of last electron to position calculated
    ! Call evalutate_proj_sub to evaluate proj
    ! Save the result to proj

    implicit none

    integer :: i, j
    real(rk) :: tx, ty
    integer :: elec_id

    write (*, '("Evaluate Projector...")')

    elec_id = num_elec

    allocate(proj(num_grid, num_grid))

    grid_dx = (grid_xmax - grid_xmin) / num_grid
    grid_dy = (grid_ymax - grid_ymin) / num_grid

    do i = 1, num_grid
      tx = grid_xmin + grid_dx * (i - 1)
      do j = 1, num_grid
        ty = grid_ymin + grid_dy * (j - 1)
        pos_elec_final(num_elec, 1: 3) = (/tx, ty, grid_z/)
        proj(i, j) = evaluate_proj_sub()
      end do
    end do

  end subroutine evaluate_proj
!--------------------------------------------------------------------------------------------------

  function evaluate_proj_sub()
    ! Evaluate proj corresponding to the current configuration of particles

    implicit none

    real(rk) :: evaluate_proj_sub
    real(rk) :: u_ee
    real(rk), allocatable :: g_eZ(:, :), g_eZ_tmp(:, :)
    real(rk) :: g_eZ_det
    integer :: i, j
!   real(rk), save :: g_sav
    integer :: num_elec_tmp

    allocate(g_eZ(num_elec, num_elec))
    allocate(g_eZ_tmp(num_elec, num_elec))

    do i = 1, num_elec
      do j = 1, num_elec
        g_eZ(i, j) = get_g_eZ(i, j)
        g_eZ_tmp(i, j) = g_eZ(i, j)
      end do
    end do

    ! Calling matinv changes the matrix, so save it
    num_elec_tmp = num_elec
    call matinv(g_eZ_tmp, num_elec_tmp, g_eZ_det)
    ! write (*, *) "det: ", g_eZ_det

    if (u_ee_method == 3 .and. num_elec == 2) then
      evaluate_proj_sub = (g_eZ(1, 1) * g_eZ(2, 2) * exp(-get_u_ee(3, 1)) - g_eZ(1, 2) * g_eZ(2, 1) * exp(-get_u_ee(3, 2))) * exp(E_T * tau)
      write (*, *) "get_u_ee3", -get_u_ee(3, 1), -get_u_ee(3, 2)
    else
      u_ee = get_u_ee(u_ee_method, 0)
      evaluate_proj_sub = g_eZ_det * exp(E_T * tau - u_ee)
    end if

!   if(abs(g_eZ_det).gt.g_sav) then
!     write(6,'(''g_eZ_det, exp(E_T * tau - u_ee), 1/ (2*PI*tau)**(3._rk*num_elec/2._rk)'',9es12.4)') g_eZ_det, exp(E_T * tau - u_ee), 1/ (2*PI*tau)**(3._rk*num_elec/2._rk)
!     g_sav=abs(g_eZ_det)
!   endif

    evaluate_proj_sub = evaluate_proj_sub * (MU / (2 * PI * tau))**(1.5_rk * num_elec)

  end function evaluate_proj_sub
!--------------------------------------------------------------------------------------------------

  function get_g_eZ(elec_to, elec_from)

    implicit none

    integer :: elec_to, elec_from
    real(rk) :: get_g_eZ
    integer :: i, type_id, type_cnt
    real(rk) :: q, s, t
    real(rk) :: r_to_a(3), r_from_a(3)
    integer :: iflag

    get_g_eZ = 0.0_rk

    type_id = 1
    type_cnt = 0

    s = norm2(pos_elec_final(elec_to,1:3) - pos_elec_init(elec_from,1:3))

    do i = 1, tot_nuclei
      r_to_a = pos_elec_final(elec_to, 1:3) - pos_nuclei(i, 1:3)
      r_from_a = pos_elec_init(elec_from, 1:3) - pos_nuclei(i, 1:3)
      q = (norm2(r_to_a) + norm2(r_from_a)) / 2
      call pot_eZ(type_id)%evaluate(q + s / 2, q - s / 2, 0, 0, t, iflag)

      type_cnt = type_cnt + 1

      if (type_cnt == num_nuclei(type_id)) then
        type_id = type_id + 1
        type_cnt = 0
      end if

      get_g_eZ = get_g_eZ + t
    end do

    get_g_eZ = get_g_eZ + MU * s**2 / (2 * tau)

    get_g_eZ = exp(-get_g_eZ)

  end function get_g_eZ
!--------------------------------------------------------------------------------------------------

  function get_u_ee(method, arg)
    implicit none
    integer :: method
    real(rk) :: get_u_ee
    integer :: arg

    select case (method)
      case (1)
        get_u_ee = get_u_ee1()
      case (2)
        get_u_ee = get_u_ee2()
      case (3)
        if (num_elec == 2) then
          get_u_ee = get_u_ee3(arg)
        else
          write (*, '("Not Implemented Yet")')
          stop
        end if
        ! get_u_ee = get_u_ee3()
    end select

  end function get_u_ee
!--------------------------------------------------------------------------------------------------

  function get_u_ee1()
    ! Evaluate u_ee according to:
    ! Umrigar 2015. "Observation on variational and projector Monte Carlo Method", Eq. 17.

    implicit none

    real(rk) :: get_u_ee1
    integer :: i, j
    real(rk) :: q_prime, q, s
    real(rk) :: t_prime, t
    real(rk) :: r_ij(3), r_ij_prime(3)
    integer :: iflag

    get_u_ee1 = 0.0_rk
    s = 0.0_rk

    do i = 1, num_elec
      do j = i + 1, num_elec
        r_ij = pos_elec_init(j, 1:3) - pos_elec_init(i, 1:3)
        r_ij_prime = pos_elec_final(j, 1:3) - pos_elec_final(i, 1:3)
        q_prime = norm2(r_ij_prime)
        q = norm2(r_ij)
        call pot_ee%evaluate(q, q, 0, 0, t, iflag)
        call pot_ee%evaluate(q_prime, q_prime, 0, 0, t_prime, iflag)
        get_u_ee1 = get_u_ee1 + (t + t_prime) / 2
      end do
    end do

  end function get_u_ee1
!--------------------------------------------------------------------------------------------------

  function get_u_ee2()
    ! Evaluate u_ee according to:
    ! Umrigar 2015. "Observation on variational and projector Monte Carlo Method", Eq. 18.

    implicit none

    real(rk) :: get_u_ee2
    integer :: i, j
    real(rk) :: q, s
    real(rk) :: t
    real(rk) :: r_ij(3), r_ij_prime(3)
    integer :: iflag

    get_u_ee2 = 0.0_rk

    do i = 1, num_elec
      do j = i + 1, num_elec
        r_ij = pos_elec_init(j, 1:3) - pos_elec_init(i, 1:3)
        r_ij_prime = pos_elec_final(j, 1:3) - pos_elec_final(i, 1:3)
        q = (norm2(r_ij) + norm2(r_ij_prime)) / 2
        s = norm2(r_ij - r_ij_prime)
        call pot_ee%evaluate(q + s / 2, q - s / 2, 0, 0, t, iflag)
        get_u_ee2 = get_u_ee2 + t
      end do
    end do

  end function get_u_ee2
!--------------------------------------------------------------------------------------------------

  function get_u_ee3(arg)
    ! Evaluate u_ee according to:
    ! Umrigar 2015. "Observation on variational and projector Monte Carlo Method", Eq. 17.

    implicit none

    real(rk) :: get_u_ee3
    integer :: i, j
    real(rk) :: q, s
    real(rk) :: t
    real(rk) :: r_ij(3), r_ij_prime(3)
    integer :: iflag
    integer :: arg

    get_u_ee3 = 0.0_rk

    ! Only deals with 2 electrons for now
    if (num_elec .ne. 2) then
      return
    end if

    i = 1
    j = 2
    r_ij = pos_elec_init(j, 1:3) - pos_elec_init(i, 1:3)
    r_ij_prime = pos_elec_final(j, 1:3) - pos_elec_final(i, 1:3)
    q = (norm2(r_ij) + norm2(r_ij_prime)) / 2

    if (arg .eq. 1) then
      s = norm2(r_ij - r_ij_prime)
    else
      s = norm2(r_ij + r_ij_prime)
    end if

    call pot_ee%evaluate(q + s / 2, q - s / 2, 0, 0, t, iflag)
    get_u_ee3 = t

  end function get_u_ee3
!--------------------------------------------------------------------------------------------------

  subroutine load_potential()

    implicit none

    character(FILENAME_MAX_LENGTH) :: file_e, file_Z
    integer :: i

    write (*, '("==== Start Loading Potential ====")')
    file_e = get_filename(-1)
    call load_potential_sub(file_e, pot_ee)

    allocate(pot_eZ(num_type))
    do i = 1, num_type
      file_Z = get_filename(z_nuclei(i))
      call load_potential_sub(file_Z, pot_eZ(i))
    end do

    write (*, '("==== Finished Loading Potential ====")')

  end subroutine load_potential
!--------------------------------------------------------------------------------------------------

  subroutine load_potential_sub(filename, bspline_obj)
    implicit none

    integer :: grid_in
    real(rk) :: r_0, r_n
    integer, parameter :: LINE_SKIP = 1
    real(rk), allocatable :: x_in(:), y_in(:), u_in(:, :)
    real(rk) :: tx, ty, tu ! temp variables
    character(FILENAME_MAX_LENGTH) :: filename
    integer :: i, j
    type(bspline_2d) :: bspline_obj
    integer :: kx, ky
    integer :: iflag

    write (*, '("Loading Potential From: ", A)') trim(filename)

    ! read potential from filename
    open (1, file = trim(filename))
    rewind(1) ! move to the beginning of file
    read (1, *)
    read (1, *) grid_in, r_0, r_n

    allocate(x_in(grid_in))
    allocate(y_in(grid_in))
    allocate(u_in(grid_in, grid_in))

    do i = 1, LINE_SKIP
      read (1, *)
    end do

    do i = 1, grid_in
      do j = 1, grid_in
        read (1, *) tx, ty, tu
        if (isnan(tu) .or. isnan(tx) .or. isnan(ty)) then
          write (*, '("Invalid Potential (tx, ty, tu): ", 3F15.6)') tx, ty, tu
          stop
        end if
        u_in(i, j) = tu
        if (i == 1) then
          y_in(j) = ty
        end if
        if (j == 1) then
          x_in(i) = tx
        end if
      end do
    end do

    close(1)

    ! spline interpolation
    kx = 3 ! use cubic spline
    ky = 3
    call bspline_obj%initialize(x_in, y_in, u_in, kx, ky, iflag)

    if(iflag == 1) then
      write (*, '("Interpolation Succeeded!")')
    else
      write (*, '("Interpolation Failed!")')
      call exit(0)
    end if
  end subroutine load_potential_sub
!--------------------------------------------------------------------------------------------------

  function str(num_in)
    integer, intent(in) :: num_in
    character(BUF_LENGTH) :: str
    write (str, *) num_in
    str = adjustl(str)
  end function str
!--------------------------------------------------------------------------------------------------

  function get_filename(Z_in)

    implicit none

    integer :: Z_in
    character(FILENAME_MAX_LENGTH) :: get_filename
    character(FILENAME_MAX_LENGTH) :: buf

    ! obtain folder name
    !write (buf1, '("Z=", A, "_tau=", A, "_grid=400")'), trim(str(Z_nuclei)), trim(tau_str)

    ! obtain file name
    if(Z_in > 0) then
      write (buf, '("u_", A, "_tau=", A, "_ngrid=200")') trim(str(Z_in)), trim(tau_str)
    else if(Z_in == -1) then
      write (buf, '("u_e_tau=", A, "_ngrid=200")') trim(tau_str)
    end if

    ! combine folder and file name to filename with path
    write (get_filename, '(A)') trim(buf)

  end function get_filename
!--------------------------------------------------------------------------------------------------

  subroutine read_input()

    implicit none

    integer :: i

    write (*, '("==== Start Reading Input ====")')

    ! read info of nucleis
    read (*, '(A)') title
    write (*, '("Title: ", A)') title
    read (*, *) num_type
    write (*, '("Number of Types: ", A)') trim(str(num_type))
    allocate(num_nuclei(num_type))
    allocate(z_nuclei(num_type))

    read (*, *) (num_nuclei(i), i = 1, num_type)
    read (*, *) (z_nuclei(i), i = 1, num_type)
    tot_nuclei = 0
    do i = 1, num_type
      tot_nuclei = tot_nuclei + num_nuclei(i)
      write (*, '("Type, Count, Z: ", 3I5)') i, num_nuclei(i), z_nuclei(i)
    end do

    allocate(pos_nuclei(tot_nuclei, 3))
    write (*, '("Nuclei Positions:")')
    do i = 1, tot_nuclei
      read (*, *) pos_nuclei(i, 1:3)
      write (*, '("X, Y, Z: ", 3F10.6)') pos_nuclei(i, 1:3)
    end do

    ! read info of electrons
    read (*, *) num_elec
    write (*, '("Numbber of Electrons: ", A)') trim(str(num_elec))
    allocate(pos_elec_init(num_elec, 3))
    allocate(pos_elec_final(num_elec, 3))
    write (*, '("Initial Position of Electrons:")')
    do i = 1, num_elec
      read (*, *) pos_elec_init(i, 1:3)
      write (*, '("X, Y, Z: ", 3F10.6)') pos_elec_init(i, 1:3)
    end do
    write (*, '("Final Position of Electrons:")')
    do i = 1, num_elec - 1
      read (*, *) pos_elec_final(i, 1: 3)
      write (*, '("X, Y, Z: ", 3F10.6)') pos_elec_final(i, 1:3)
    end do

    ! read output grid
    read (*, *) num_grid
    read (*, *) grid_xmin, grid_xmax
    read (*, *) grid_ymin, grid_ymax
    write (*, '("Number of Grid Points: ", A)'), trim(str(num_grid))
    write (*, '("x_min, x_max: ", 2F10.6)') grid_xmin, grid_xmax
    write (*, '("y_min, y_max: ", 2F10.6)') grid_ymin, grid_ymax

    ! read tau
    read (*, '(A)') tau_str
    read (tau_str, *) tau
    write (*, '("Tau: ", F10.6)') tau

    write (*, '("==== Finished Reading Input ====")')

  end subroutine read_input

end program projector_calc
