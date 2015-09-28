#ifdef _FABM_

#include "cppdefs.h"

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
   use fabm_types,only:rk,get_safe_name,output_time_step_integrated,output_time_step_averaged,output_none
   use fabm,only:type_external_variable,fabm_get_conserved_quantities,fabm_get_horizontal_conserved_quantities

   use gotm_fabm_input
   use gotm_fabm

   implicit none

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm_fabm_output, do_gotm_fabm_output, clean_gotm_fabm_output

   REALTYPE,allocatable :: total0(:),total(:)
   REALTYPE,allocatable :: local(:,:)
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
   ! Allocate memory for conserved quantity totals
   allocate(total0(1:size(model%conserved_quantities)),stat=rc)
   if (rc /= 0) stop 'init_gotm_fabm_output: Error allocating (total0)'
   allocate(total(1:size(model%conserved_quantities)),stat=rc)
   if (rc /= 0) stop 'init_gotm_fabm_output: Error allocating (total)'
   allocate(local(1:nlev,1:size(model%conserved_quantities)),stat=rc)
   if (rc /= 0) stop 'init_gotm_fabm_output: Error allocating (local)'

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

         ! Add a NetCDF variable for each pelagic (longitude,latitude,depth,time) biogeochemical state variable.
         do n=1,size(model%state_variables)
            call add_variable(model%state_variables(n),dim4d)
         end do

         ! Add a NetCDF variable for each bottom (longitude,latitude,time) biogeochemical state variable.
         do n=1,size(model%bottom_state_variables)
            call add_variable(model%bottom_state_variables(n),dim3d)
         end do

         ! Add a NetCDF variable for each surface (longitude,latitude,time) biogeochemical state variable.
         do n=1,size(model%surface_state_variables)
            call add_variable(model%surface_state_variables(n),dim3d)
         end do

         ! Add a NetCDF variable for each 4D (longitude,latitude,depth,time) biogeochemical diagnostic variable.
         do n=1,size(model%diagnostic_variables)
            call add_variable(model%diagnostic_variables(n),dim4d)
         end do

         ! Add a NetCDF variable for each 3D (longitude,latitude,time) biogeochemical diagnostic variable.
         do n=1,size(model%horizontal_diagnostic_variables)
            call add_variable(model%horizontal_diagnostic_variables(n),dim3d)
         end do

         ! Add a variable for each conserved quantity
         do n=1,size(model%conserved_quantities)
            iret = new_nc_variable(ncid,'int_change_in_'//trim(model%conserved_quantities(n)%name),NCDF_FLOAT_PRECISION, &
                                   dim3d,model%conserved_quantities(n)%externalid)
            iret = set_attributes(ncid,model%conserved_quantities(n)%externalid,      &
                                  units=trim(model%conserved_quantities(n)%units)//'*m',    &
                                  long_name='integrated change in '//trim(model%conserved_quantities(n)%long_name), &
                                  missing_value=-1d20,FillValue=-1d20)
         end do

         ! If requested, add a NetCDF variable for each variable read from an external source [input file].
         if (save_inputs) then
            ! Enumerate profile (depth-dependent) inputs and create corresponding NetCDF variables.
            cur_obs_variable => first_input_variable
            do while (associated(cur_obs_variable))
               if (allocated(cur_obs_variable%data_1d)) then
                  iret = new_nc_variable(ncid,trim(get_safe_name(cur_obs_variable%name))//'_obs',NCDF_FLOAT_PRECISION,dim4d,cur_obs_variable%ncid)
               else
                  iret = new_nc_variable(ncid,trim(get_safe_name(cur_obs_variable%name))//'_obs',NCDF_FLOAT_PRECISION,dim3d,cur_obs_variable%ncid)
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

#ifdef NETCDF_FMT
   contains

   subroutine add_variable(variable,dims)
      class (type_external_variable),intent(inout) :: variable
      integer,                       intent(in)    :: dims(:)
      integer                                      :: iret

      if (variable%output==output_none) return
      iret = new_nc_variable(ncid,trim(variable%name),NCDF_FLOAT_PRECISION,dims,variable%externalid)
      iret = set_attributes(ncid,variable%externalid,units=trim(variable%units), &
                            long_name=trim(variable%long_name), &
                            FillValue=variable%missing_value,missing_value=variable%missing_value)
   end subroutine
#endif

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
         do n=1,size(model%state_variables)
            if (model%state_variables(n)%output==output_none) cycle
            iret = nf90_put_var(ncid,model%state_variables(n)%externalid,cc(1:nlev,n),start,edges)
            call check_err(iret)
         end do

         ! Process and store diagnostic variables defined on the full domain.
         do n=1,size(model%diagnostic_variables)
            if (model%diagnostic_variables(n)%output==output_none) cycle

            ! Time-average diagnostic variable if needed.
            if (model%diagnostic_variables(n)%output==output_time_step_averaged.and..not.initial) &
               cc_diag(:,n) = cc_diag(:,n)/(nsave*dt)

            ! If we want time-step integrated values and have the initial field, multiply by output time step.
            if (model%diagnostic_variables(n)%output==output_time_step_integrated.and.initial) &
               cc_diag(:,n) = cc_diag(:,n)*(nsave*dt)

            ! Store diagnostic variable values.
            iret = nf90_put_var(ncid,model%diagnostic_variables(n)%externalid,cc_diag(:,n),start,edges)
            call check_err(iret)

            ! Reset diagnostic variables to zero if they will be time-integrated (or time-averaged).
            if (model%diagnostic_variables(n)%output==output_time_step_averaged .or. &
                model%diagnostic_variables(n)%output==output_time_step_integrated) &
               cc_diag(:,n) = _ZERO_
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

         ! Store bottom-bound biogeochemical state variables.
         do n=1,size(model%bottom_state_variables)
            if (model%bottom_state_variables(n)%output==output_none) cycle
            iret = store_data(ncid,model%bottom_state_variables(n)%externalid,XYT_SHAPE,1, &
                            & scalar=cc(1,size(model%state_variables)+n))
         end do

         ! Store surface-bound biogeochemical state variables.
         do n=1,size(model%surface_state_variables)
            if (model%surface_state_variables(n)%output==output_none) cycle
            iret = store_data(ncid,model%surface_state_variables(n)%externalid,XYT_SHAPE,1, &
                            & scalar=cc(nlev,size(model%state_variables)+size(model%bottom_state_variables)+n))
         end do

         ! Process and store diagnostic variables defined on horizontal slices of the domain.
         do n=1,size(model%horizontal_diagnostic_variables)
            if (model%horizontal_diagnostic_variables(n)%output==output_none) cycle

            ! Time-average diagnostic variable if needed.
            if (model%horizontal_diagnostic_variables(n)%output==output_time_step_averaged.and..not.initial) &
               cc_diag_hz(n) = cc_diag_hz(n)/(nsave*dt)

            ! If we want time-step integrated values and have the initial field, multiply by output time step.
            if (model%horizontal_diagnostic_variables(n)%output==output_time_step_integrated.and.initial) &
               cc_diag_hz(n) = cc_diag_hz(n)*(nsave*dt)

            ! Store diagnostic variable values.
            iret = store_data(ncid,model%horizontal_diagnostic_variables(n)%externalid,XYT_SHAPE,nlev,scalar=cc_diag_hz(n))

            ! Reset diagnostic variables to zero if they will be time-integrated (or time-averaged).
            if (model%horizontal_diagnostic_variables(n)%output==output_time_step_averaged .or. &
                model%horizontal_diagnostic_variables(n)%output==output_time_step_integrated) &
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
         do n=1,size(model%conserved_quantities)
            iret = store_data(ncid,model%conserved_quantities(n)%externalid,XYT_SHAPE,1,scalar=total(n))
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

      call fabm_get_conserved_quantities(model,1,nlev,local)
      do n=1,size(model%conserved_quantities)
         ! Note: our pointer to h has a lower bound of 1, while the original pointed-to data starts at 0.
         ! We therefore need to increment the index by 1 in order to address original elements >=1!
         total(n) = total(n) + sum(h(2:nlev+1)*local(1:nlev,n))
      end do
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

