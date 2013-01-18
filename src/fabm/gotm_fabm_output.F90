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
   use fabm
   use fabm_types

   implicit none

!  default: all is private.
   private
!
! !PUBLIC MEMBER FUNCTIONS:
   public init_gotm_fabm_output, do_gotm_fabm_output
!EOP
!-----------------------------------------------------------------------

contains

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Initialize output
!
! !INTERFACE:
   subroutine init_gotm_fabm_output()

!
! !DESCRIPTION:
!  Initialize the output by defining biogeochemical variables.
!
! !USES:
   use output,  only: out_fmt
#ifdef NETCDF_FMT
   use ncdfout, only: ncid,lon_dim,lat_dim,z_dim,time_dim,dim3d,dim4d
   use ncdfout, only: define_mode,new_nc_variable,set_attributes
#endif
!
#ifdef NETCDF_FMT
   use netcdf
#endif
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer :: iret,n
   type (type_obs),pointer :: cur_obs
   character(len=64) :: name
!
!-----------------------------------------------------------------------
!BOC
   if (.not. fabm_calc) return

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
                                  long_name=trim(model%info%state_variables(n)%longname), &
                                  missing_value=model%info%state_variables(n)%missing_value)
         end do

         ! Add a NetCDF variable for each 4D (longitude,latitude,depth,time) biogeochemical diagnostic variable.
         do n=1,size(model%info%diagnostic_variables)
            iret = new_nc_variable(ncid,trim(model%info%diagnostic_variables(n)%name),NF90_REAL, &
                                   dim4d,model%info%diagnostic_variables(n)%externalid)
            iret = set_attributes(ncid,model%info%diagnostic_variables(n)%externalid,    &
                                  units=trim(model%info%diagnostic_variables(n)%units),        &
                                  long_name=trim(model%info%diagnostic_variables(n)%longname), &
                                  missing_value=model%info%diagnostic_variables(n)%missing_value)
         end do

         ! Add a NetCDF variable for each 3D (longitude,latitude,time) biogeochemical state variable.
         do n=1,size(model%info%state_variables_ben)
            iret = new_nc_variable(ncid,trim(model%info%state_variables_ben(n)%name),NF90_REAL, &
                                   dim3d,model%info%state_variables_ben(n)%externalid)
            iret = set_attributes(ncid,model%info%state_variables_ben(n)%externalid,    &
                                  units=trim(model%info%state_variables_ben(n)%units),        &
                                  long_name=trim(model%info%state_variables_ben(n)%longname), &
                                  missing_value=model%info%state_variables_ben(n)%missing_value)
         end do

         ! Add a NetCDF variable for each 3D (longitude,latitude,time) biogeochemical diagnostic variable.
         do n=1,size(model%info%diagnostic_variables_hz)
            iret = new_nc_variable(ncid,trim(model%info%diagnostic_variables_hz(n)%name),NF90_REAL, &
                                   dim3d,model%info%diagnostic_variables_hz(n)%externalid)
            iret = set_attributes(ncid,model%info%diagnostic_variables_hz(n)%externalid,    &
                                  units=trim(model%info%diagnostic_variables_hz(n)%units),        &
                                  long_name=trim(model%info%diagnostic_variables_hz(n)%longname), &
                                  missing_value=model%info%diagnostic_variables_hz(n)%missing_value)
         end do

         ! Add a variable for each conserved quantity
         do n=1,size(model%info%conserved_quantities)
            iret = new_nc_variable(ncid,trim(model%info%conserved_quantities(n)%name)//'_tot',NF90_REAL, &
                                   dim3d,model%info%conserved_quantities(n)%externalid)
            iret = set_attributes(ncid,model%info%conserved_quantities(n)%externalid,      &
                                  units='m*'//trim(model%info%conserved_quantities(n)%units),    &
                                  long_name=trim(model%info%conserved_quantities(n)%longname)//', depth-integrated')
         end do

         ! Add a NetCDF variable for each variable read from an external source [input file].
         if (save_inputs) then
            cur_obs => first_obs
            do while (associated(cur_obs))
               name = fabm_get_variable_name(model,cur_obs%id,cur_obs%shape)
               select case (cur_obs%shape)
                  case (shape_full)
                     iret = new_nc_variable(ncid,trim(name),NF90_REAL,dim4d,cur_obs%ncid)
                  case (shape_hz,shape_scalar)
                     iret = new_nc_variable(ncid,trim(name),NF90_REAL,dim3d,cur_obs%ncid)
               end select
               cur_obs => cur_obs%next
            end do
         end if

         ! Take NetCDF library out of define mode (ready for storing data).
         iret = define_mode(ncid,.false.)
#endif
   end select

   end subroutine init_gotm_fabm_output
!EOC

!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Save values of biogeochemical variables
!
! !INTERFACE:
   subroutine do_gotm_fabm_output(nlev)

!
! !DESCRIPTION:
!  Save properties of biogeochemical model, including state variable
!  values, diagnostic variable values, and sums of conserved quantities.
!
! !USES:
   use output,  only: nsave,out_fmt
#ifdef NETCDF_FMT
   use ncdfout, only: ncid
   use ncdfout, only: store_data
#endif

#ifdef NETCDF_FMT
   use netcdf
#endif
!
! !INPUT PARAMETERS:
   integer, intent(in)                  :: nlev
!
! !REVISION HISTORY:
!  Original author(s): Jorn Bruggeman
!
!EOP
!
! !LOCAL VARIABLES:
   integer :: iret,n
   type (type_obs),pointer :: cur_obs
!
!-----------------------------------------------------------------------
!BOC
   if (.not. fabm_calc) return

   select case (out_fmt)
      case (NETCDF)
#ifdef NETCDF_FMT
         ! Store pelagic biogeochemical state variables.
         do n=1,size(model%info%state_variables)
            iret = store_data(ncid,model%info%state_variables(n)%externalid,XYZT_SHAPE,nlev,array=cc(n,0:nlev))
         end do

         ! Store benthic biogeochemical state variables.
         do n=1,size(model%info%state_variables_ben)
            iret = store_data(ncid,model%info%state_variables_ben(n)%externalid,XYT_SHAPE,1, &
                            & scalar=cc(size(model%info%state_variables)+n,1))
         end do

         ! Process and store diagnostic variables defined on the full domain.
         do n=1,size(model%info%diagnostic_variables)
            ! Time-average diagnostic variable if needed.
            if (model%info%diagnostic_variables(n)%time_treatment==time_treatment_averaged) &
               cc_diag(n,1:nlev) = cc_diag(n,1:nlev)/(nsave*dt)

            ! Store diagnostic variable values.
            iret = store_data(ncid,model%info%diagnostic_variables(n)%externalid,XYZT_SHAPE,nlev,array=cc_diag(n,0:nlev))

            ! Reset diagnostic variables to zero if they will be time-integrated (or time-averaged).
            if (model%info%diagnostic_variables(n)%time_treatment==time_treatment_averaged .or. &
                model%info%diagnostic_variables(n)%time_treatment==time_treatment_step_integrated) &
               cc_diag(n,1:nlev) = _ZERO_
         end do

         ! Process and store diagnostic variables defined on horizontal slices of the domain.
         do n=1,size(model%info%diagnostic_variables_hz)
            ! Time-average diagnostic variable if needed.
            if (model%info%diagnostic_variables_hz(n)%time_treatment==time_treatment_averaged) &
               cc_diag_hz(n) = cc_diag_hz(n)/(nsave*dt)

            ! Store diagnostic variable values.
            iret = store_data(ncid,model%info%diagnostic_variables_hz(n)%externalid,XYT_SHAPE,nlev,scalar=cc_diag_hz(n))

            ! Reset diagnostic variables to zero if they will be time-integrated (or time-averaged).
            if (model%info%diagnostic_variables_hz(n)%time_treatment==time_treatment_averaged .or. &
                model%info%diagnostic_variables_hz(n)%time_treatment==time_treatment_step_integrated) &
               cc_diag_hz(n) = _ZERO_
         end do

         ! Store values for each variable read from an external source [input file].
         if (save_inputs) then
            cur_obs => first_obs
            do while (associated(cur_obs))
               select case (cur_obs%shape)
                  case (shape_full)
                     iret = store_data(ncid,cur_obs%ncid,XYZT_SHAPE,1,array=cur_obs%data_1d)
                  case (shape_hz,shape_scalar)
                     iret = store_data(ncid,cur_obs%ncid,XYT_SHAPE,1,scalar=cur_obs%data_0d)
               end select
               cur_obs => cur_obs%next
            end do
         end if

         ! Integrate conserved quantities over depth.
#ifdef _FABM_USE_1D_LOOP_
         call fabm_get_conserved_quantities(model,1,nlev,local)
         do n=1,size(model%info%conserved_quantities)
            ! Note: our pointer to h has a lower bound of 1, while the original pointed-to data starts at 0.
            ! We therefore need to increment the index by 1 in order to address original elements >=1!
            total(n) = sum(h(2:nlev+1)*local(1:nlev,n))
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

         ! Store conserved quantity integrals.
         do n=1,size(model%info%conserved_quantities)
            iret = store_data(ncid,model%info%conserved_quantities(n)%externalid,XYT_SHAPE,1,scalar=total(n))
         end do
#endif
   end select

   end subroutine do_gotm_fabm_output
!EOC

!-----------------------------------------------------------------------

end module gotm_fabm_output

#endif

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!----------------------------------------------------------------------

