#ifdef _FABM_

#include "cppdefs.h"
#include "fabm_driver.h"
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: gotm_fabm --- Interface to Framework for Aquatic Biogeochemical Models (FABM)
!
! !INTERFACE:
   module gotm_fabm_output
!
! !DESCRIPTION:
!  This module contains routines for creating and writing NetCDF variables for all FABM
!  state and diagnostic variables.
!
! !USES:
   use gotm_fabm
   use gotm_fabm_input
   use fabm
   use fabm_types

   implicit none

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm_fabm_output, do_gotm_fabm_output, clean_gotm_fabm_output

   REALTYPE,allocatable :: total0(:),total(:)
#ifdef _FABM_USE_1D_LOOP_
   REALTYPE,allocatable :: local(:,:)
#else
   REALTYPE,allocatable :: local(:)
#endif
!EOP
!-----------------------------------------------------------------------

contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize output
!
! !INTERFACE:
   subroutine init_gotm_fabm_output(nlev)
!
! !DESCRIPTION:
!  Initialize the output by defining biogeochemical variables.
!
! !USES:
   use output,  only: out_fmt
#ifdef NETCDF_FMT
   use ncdfout, only: ncid,lon_dim,lat_dim,z_dim,time_dim,dim3d,dim4d
   use ncdfout, only: define_mode,new_nc_variable,set_attributes,check_err
#endif
!
#ifdef NETCDF_FMT
   use netcdf
#endif

   integer, intent(in) :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer :: iret,n,rc
   type (type_input_variable),pointer :: cur_obs_variable
!
!-----------------------------------------------------------------------
!BOC
   if (.not. fabm_calc) return

   ! Allocate memory for conserved quantity totals
   allocate(total0(1:size(model%info%conserved_quantities)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (total0)'
   allocate(total(1:size(model%info%conserved_quantities)),stat=rc)
   if (rc /= 0) stop 'allocate_memory(): Error allocating (total)'
#ifdef _FABM_USE_1D_LOOP_
   allocate(local(1:nlev,1:size(model%info%conserved_quantities)),stat=rc)
#else
   allocate(local(1:size(model%info%conserved_quantities)),stat=rc)
#endif
   if (rc /= 0) stop 'allocate_memory(): Error allocating (local)'

   select case (out_fmt)
      case (NETCDF)
#ifdef NETCDF_FMT
         ! Put NetCDF library in define mode.
         iret = define_mode(ncid,.true.)

         dim4d(1) = lon_dim
         dim4d(2) = lat_dim
         dim4d(3) = z_dim
         dim4d(4) = time_dim

         dim3d(1) = lon_dim
         dim3d(2) = lat_dim
         dim3d(3) = time_dim

         ! Add a NetCDF variable for each 4D (longitude,latitude,depth,time) biogeochemical state variable.
         do n=1,size(model%info%state_variables)
            iret = new_nc_variable(ncid,trim(model%info%state_variables(n)%name),NF90_REAL, &
                                   dim4d,model%info%state_variables(n)%externalid)
            iret = set_attributes(ncid,model%info%state_variables(n)%externalid,       &
                                  units=trim(model%info%state_variables(n)%units),    &
                                  long_name=trim(model%info%state_variables(n)%long_name), &
                                  missing_value=model%info%state_variables(n)%missing_value)
         end do

         ! Add a NetCDF variable for each 4D (longitude,latitude,depth,time) biogeochemical diagnostic variable.
         do n=1,size(model%info%diagnostic_variables)
            iret = new_nc_variable(ncid,trim(model%info%diagnostic_variables(n)%name),NF90_REAL, &
                                   dim4d,model%info%diagnostic_variables(n)%externalid)
            iret = set_attributes(ncid,model%info%diagnostic_variables(n)%externalid,    &
                                  units=trim(model%info%diagnostic_variables(n)%units),        &
                                  long_name=trim(model%info%diagnostic_variables(n)%long_name), &
                                  missing_value=model%info%diagnostic_variables(n)%missing_value)
         end do

         ! Add a NetCDF variable for each 3D (longitude,latitude,time) biogeochemical state variable.
         do n=1,size(model%info%state_variables_ben)
            iret = new_nc_variable(ncid,trim(model%info%state_variables_ben(n)%name),NF90_REAL, &
                                   dim3d,model%info%state_variables_ben(n)%externalid)
            iret = set_attributes(ncid,model%info%state_variables_ben(n)%externalid,    &
                                  units=trim(model%info%state_variables_ben(n)%units),        &
                                  long_name=trim(model%info%state_variables_ben(n)%long_name), &
                                  missing_value=model%info%state_variables_ben(n)%missing_value)
         end do

         ! Add a NetCDF variable for each 3D (longitude,latitude,time) biogeochemical diagnostic variable.
         do n=1,size(model%info%diagnostic_variables_hz)
            iret = new_nc_variable(ncid,trim(model%info%diagnostic_variables_hz(n)%name),NF90_REAL, &
                                   dim3d,model%info%diagnostic_variables_hz(n)%externalid)
            iret = set_attributes(ncid,model%info%diagnostic_variables_hz(n)%externalid,    &
                                  units=trim(model%info%diagnostic_variables_hz(n)%units),        &
                                  long_name=trim(model%info%diagnostic_variables_hz(n)%long_name), &
                                  missing_value=model%info%diagnostic_variables_hz(n)%missing_value)
         end do

         ! Add a variable for each conserved quantity
         do n=1,size(model%info%conserved_quantities)
            iret = new_nc_variable(ncid,'int_change_in_'//trim(model%info%conserved_quantities(n)%name),NF90_REAL, &
                                   dim3d,model%info%conserved_quantities(n)%externalid)
            iret = set_attributes(ncid,model%info%conserved_quantities(n)%externalid,      &
                                  units=trim(model%info%conserved_quantities(n)%units)//'*m',    &
                                  long_name='integrated change in '//trim(model%info%conserved_quantities(n)%long_name))
         end do

         ! If requested, add a NetCDF variable for each variable read from an external source [input file].
         if (save_inputs) then
            ! Enumerate profile (depth-dependent) inputs and create corresponding NetCDF variables.
            cur_obs_variable => first_input_variable
            do while (associated(cur_obs_variable))
               if (allocated(cur_obs_variable%data_1d)) then
                  iret = new_nc_variable(ncid,trim(cur_obs_variable%name)//'_obs',NF90_REAL,dim4d,cur_obs_variable%ncid)
               else
                  iret = new_nc_variable(ncid,trim(cur_obs_variable%name)//'_obs',NF90_REAL,dim3d,cur_obs_variable%ncid)
               end if
               iret = nf90_put_att(ncid,cur_obs_variable%ncid,'source_file',trim(cur_obs_variable%path))
               call check_err(iret)
               iret = nf90_put_att(ncid,cur_obs_variable%ncid,'source_column',cur_obs_variable%index)
               call check_err(iret)
               cur_obs_variable => cur_obs_variable%next
            end do               
         end if

         ! Take NetCDF library out of define mode (ready for storing data).
         iret = define_mode(ncid,.false.)
#endif
   end select
   call calculate_conserved_quantities(nlev,total0)

   end subroutine init_gotm_fabm_output
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Save values of biogeochemical variables
!
! !INTERFACE:
   subroutine do_gotm_fabm_output(nlev,initial)

!
! !DESCRIPTION:
!  Save properties of biogeochemical model, including state variable
!  values, diagnostic variable values, and sums of conserved quantities.
!
! !USES:
   use output,  only: nsave,out_fmt
#ifdef NETCDF_FMT
   use ncdfout, only: ncid,set_no,store_data,check_err
#endif

#ifdef NETCDF_FMT
   use netcdf
#endif
!
! !INPUT PARAMETERS:
   integer, intent(in)                  :: nlev
   logical, intent(in)                  :: initial
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer :: iret,n,start(4),edges(4)
   type (type_input_variable),       pointer :: cur_obs_variable
!
!-----------------------------------------------------------------------
!BOC
   if (.not. fabm_calc) return

   select case (out_fmt)
      case (NETCDF)
#ifdef NETCDF_FMT
         
         ! ---------------------------
         ! Depth-dependent variables
         ! ---------------------------

         start(1) = 1;      edges(1) = 1
         start(2) = 1;      edges(2) = 1
         start(3) = 1;      edges(3) = nlev
         start(4) = set_no; edges(4) = 1

         ! Store pelagic biogeochemical state variables.
         do n=1,size(model%info%state_variables)
            iret = nf90_put_var(ncid,model%info%state_variables(n)%externalid,cc(1:nlev,n),start,edges)
            call check_err(iret)
         end do

         ! Process and store diagnostic variables defined on the full domain.
         do n=1,size(model%info%diagnostic_variables)
            ! Time-average diagnostic variable if needed.
            if (model%info%diagnostic_variables(n)%time_treatment==time_treatment_averaged.and..not.initial) &
               cc_diag(1:nlev,n) = cc_diag(1:nlev,n)/(nsave*dt)

            ! If we want time-step integrated values and have the initial field, multiply by output time step.
            if (model%info%diagnostic_variables(n)%time_treatment==time_treatment_step_integrated.and.initial) &
               cc_diag(1:nlev,n) = cc_diag(1:nlev,n)*(nsave*dt)

            ! Store diagnostic variable values.
            iret = nf90_put_var(ncid,model%info%diagnostic_variables(n)%externalid,cc_diag(1:nlev,n),start,edges)
            call check_err(iret)

            ! Reset diagnostic variables to zero if they will be time-integrated (or time-averaged).
            if (model%info%diagnostic_variables(n)%time_treatment==time_treatment_averaged .or. &
                model%info%diagnostic_variables(n)%time_treatment==time_treatment_step_integrated) &
               cc_diag(1:nlev,n) = _ZERO_
         end do

         ! Enumerate profile (depth-dependent) inputs and save corresponding data.
         cur_obs_variable => first_input_variable
         do while (associated(cur_obs_variable))
            if (cur_obs_variable%ncid/=-1.and.allocated(cur_obs_variable%data_1d)) then
               iret = nf90_put_var(ncid,cur_obs_variable%ncid,cur_obs_variable%data_1d(1:nlev),start,edges)
               call check_err(iret)
            end if
            cur_obs_variable => cur_obs_variable%next
         end do               

         ! ---------------------------
         ! Depth-independent variables
         ! ---------------------------

         ! Store benthic biogeochemical state variables.
         do n=1,size(model%info%state_variables_ben)
            iret = store_data(ncid,model%info%state_variables_ben(n)%externalid,XYT_SHAPE,1, &
                            & scalar=cc(1,size(model%info%state_variables)+n))
         end do

         ! Process and store diagnostic variables defined on horizontal slices of the domain.
         do n=1,size(model%info%diagnostic_variables_hz)
            ! Time-average diagnostic variable if needed.
            if (model%info%diagnostic_variables_hz(n)%time_treatment==time_treatment_averaged.and..not.initial) &
               cc_diag_hz(n) = cc_diag_hz(n)/(nsave*dt)

            ! If we want time-step integrated values and have the initial field, multiply by output time step.
            if (model%info%diagnostic_variables_hz(n)%time_treatment==time_treatment_step_integrated.and.initial) &
               cc_diag_hz(n) = cc_diag_hz(n)*(nsave*dt)

            ! Store diagnostic variable values.
            iret = store_data(ncid,model%info%diagnostic_variables_hz(n)%externalid,XYT_SHAPE,nlev,scalar=cc_diag_hz(n))

            ! Reset diagnostic variables to zero if they will be time-integrated (or time-averaged).
            if (model%info%diagnostic_variables_hz(n)%time_treatment==time_treatment_averaged .or. &
                model%info%diagnostic_variables_hz(n)%time_treatment==time_treatment_step_integrated) &
               cc_diag_hz(n) = _ZERO_
         end do

         ! Enumerate scalar (depth-independent) inputs and save corresponding data.
         cur_obs_variable => first_input_variable
         do while (associated(cur_obs_variable))
            if (cur_obs_variable%ncid/=-1.and..not.allocated(cur_obs_variable%data_1d)) &
               iret = store_data(ncid,cur_obs_variable%ncid,XYT_SHAPE,1,scalar=cur_obs_variable%data_0d)
            cur_obs_variable => cur_obs_variable%next
         end do

         ! Integrate conserved quantities over depth.
         call calculate_conserved_quantities(nlev,total)

         ! Compute difference with conserved quantity totals at t=0
         total = total - total0

         ! Store conserved quantity integrals.
         do n=1,size(model%info%conserved_quantities)
            iret = store_data(ncid,model%info%conserved_quantities(n)%externalid,XYT_SHAPE,1,scalar=total(n))
         end do
#endif
   end select

   end subroutine do_gotm_fabm_output
!EOC

   subroutine calculate_conserved_quantities(nlev,total)
      integer, intent(in)  :: nlev
      REALTYPE,intent(out) :: total(:)

      integer :: n

      ! Add conserved quantities at boundaries (in m-2)
      call fabm_get_horizontal_conserved_quantities(model,1,total)

#ifdef _FABM_USE_1D_LOOP_
      call fabm_get_conserved_quantities(model,1,nlev,local)
      do n=1,size(model%info%conserved_quantities)
         ! Note: our pointer to h has a lower bound of 1, while the original pointed-to data starts at 0.
         ! We therefore need to increment the index by 1 in order to address original elements >=1!
         total(n) = total(n) + sum(h(2:nlev+1)*local(1:nlev,n))
      end do
#else
      total = _ZERO_
      do n=1,nlev
         ! Note: our pointer to h has a lower bound of 1, while the original pointed-to data starts at 0.
         ! We therefore need to increment the index by 1 in order to address original elements >=1!
         call fabm_get_conserved_quantities(model,n,local)
         total = total + h(n+1)*local
      end do
#endif
   end subroutine

   subroutine clean_gotm_fabm_output()
      if (allocated(total0))           deallocate(total0)
      if (allocated(total))            deallocate(total)
      if (allocated(local))            deallocate(local)
   end subroutine

!-----------------------------------------------------------------------

end module gotm_fabm_output

#endif

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------

