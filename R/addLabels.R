#' @title addLabels
#' @description Given a data.frame, use the 'standard_name' and 'units' attributes
#' of columns in that data.frame to add 'label' attributes that can be used for
#' axis titles in plots.
#' @details Constructs an appropriate label for the variable, combining the
#' "measurand" and "units" attributes, often with translation to descriptive
#' labels that seem more appropriate. plotWAC() uses these label attributes,
#' possibly with translation to expressions where appropriate. Attributes 
#' including "standard_name" and "units" should be present for this to work.
#' This function is called by getNetCDF() and getProjectData() when they construct
#' data,franes from netCDF-format files.
#' @aliases addLabels
#' @author William Cooper
#' @export addLabels
#' @param .Data A Ranadu-style data.frame containing column vectors of measurements,
#' with attributes. Some subsetting removes these attributes, so it may be necessary
#' to use "transferAttributes()" to preserve them.
#' @examples 
#' addLabels (RAFdata)
## This function adds "label" attributes, with units, to a data.frame. 
addLabels <- function(.Data) {
  # print ('entry to addLabels.R')
  ## This defines translations for standard_names and units:
  transl <- data.frame(
    standard_name = c('platform_speed_wrt_air', 'upward_air_velocity', 'wind_from_direction',
      'air_temperature', 'atmosphere_number_content_of_aerosol_particles',
      'platform_speed_wrt_ground',
      'air_pressure', 'barometric_altitude', 'humidity_mixing_ratio', 'dew_point_temperature',
      'geopotential_height', 'height', 'atmosphere_cloud_liquid_water_content',
      'effective_radius_of_cloud_liquid_water_particle', 'platform_roll_angle', 
      'air_potential_temperature', 'pseudo_equivalent_potential_temperature',
      'platform_pitch_angle', 'platform_orientation', '#/cm3',
      'deg_C', 'm/s2', '#', 'counts/sec', 'g m-3', 'sec', 'degree_N', 'degree_T', 'deg_N', 'deg',
      'deg_T', 'Deg_C', 'gram/kg', 'um3/m3', 'degC', 'mb', 'msec', 'degree_E', '#/cc',
      'deg_E', 'std cm3/s', 'C', '#/L', 'vol cm3/s', 'G'
    ),
    replacement   = c('airspeed', 'updraft', 'wind direction',
      'temperature', 'condensation-nucleus concentration',
      'groundspeed',
      'pressure', 'pressure altitude', 'water mixing ratio', 'dew point',
      'geopotential altitude', 'height above terrain', 'liquid water content',
      'effective radius', 'roll', 
      'potential temperature', 'pseudo-equivalent potential temperature',
      'pitch', 'heading', 'cm^-3',
      'deg C', 'm/s^2', 'number', 'counts/s', 'g m^-3', 's', 'degree', 'degree', 'degree', 'degree',
      'degree', 'deg C', 'g/kg', 'um^3/m^3', 'deg C', 'hPa', 'ms', 'degree', 'cm^-3',
      'degree', 'std cm^3/s', 'deg C', 'L^-1', 'vol cm^3/s', 'm s^-2'
    )
  )
  for (V in names(.Data)) {
    if (!is.null (sname <- attr(.Data[,V], 'standard_name'))) {
      # print (sprintf ('Variable %s has standard_name %s', V, sname))
      # sname
      attr(.Data[,V], 'measurand') <- sname
      ix <- which(sname == transl$standard_name)
      if (length(ix) > 0) {
        sname <- as.vector (transl[ix, 'replacement'])
      }
      if (!is.null (uname <- attr(.Data[,V], 'units'))) {
        iu <- which (uname == transl$standard_name)
        if (length(iu) > 0) {
          uname <- as.vector (transl[iu, 'replacement'])
        }
      }
      attr(.Data[,V], 'label') <- paste0(gsub('_', ' ', sname), ' (', V, ') [', uname, ']')
    } else {
      ## This is the case with no standard_name. Try to construct a label with units, if available:
      if (!is.null (uname <- attr(.Data[,V], 'units'))) {
        iu <- which (uname == transl$standard_name)
        if (length(iu) > 0) {
          uname <- as.vector (transl[iu, 'replacement'])
        }
      }
      attr(.Data[,V], 'label') <- paste0(V, ' [', uname, ']')
    }
  }
  return(.Data)
}
