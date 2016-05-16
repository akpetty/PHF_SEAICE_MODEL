module global_data

implicit none
save

integer, parameter :: q = selected_real_kind(8)

	
real (kind=q), parameter :: &
	a_max = 0.95, & !maximum ice area
	alb_w = 0.06, & !albedo of ocean DIFF FROM 0.3
	alb_i = 0.63, & !albedo of ice from bitz1999
	alb_s = 0.8, & !albedo of snow
	alpha = 0.0000582, & ! thermal expansion coeff (1/C)
	beta = 0.0008, & ! saline contraction coeff (1/PSU)
	c1_hat = 0.8, & ! wind stirring constant 
	c_w = 4190.0, & ! specific heat of water (J/Kg/K)
	c_a = 1005, & ! specific heat of air (J/Kg/K)
	c_t = 0.001, & ! bulk transfer coefficient
	c_m = 0.003, & ! unsteadiness (m/s)
	e_w = 0.97, & !emmisivity of ocean
	e_i = 0.99, & !emmisivity of ice
	e_s = 0.99, & !emmisivity of snow all from EBERT
	d_grid = 0.5, & !depth resolution in m
	d_max = 500, & !max depth
	g = 9.8, & ! gravitational acceleration (m/s)
	h_min = 0.1, & ! minimum ice height
	i_rad_l = 0.45, & ! frac of radiation penetrating into interior see p5 ebert paper for values (0.18 - 0.35) 
    i_rad_i = 0.7, & !NCAR report has it as 0.7 for sea ice
	ki = 2.04, & ! thermal cond of ice (W/m/K)
	ks = 0.31, & ! thermal cond of snow (W/m/K) - should be a function of density as of ebert paper
	kappa_i = 1.5, & ! from the perovich optical properties of sea ice in bookmarks
	kappa_w = 0.1, & ! extinction of solar through ocean water kara
	lat_f = 334000.0, & ! Latent heat of freezing (J/kg)
	lat_v = 2501000.0, & ! Latent heat of vaporisation
	lat_s = 2834000.0, & ! Latent heat of sublimation
	p_atm = 100, & ! atmospheric pressure kPa
	ro_a = 1.2750, & ! air density (Kg/m^3)
	ro_w = 1026.0, & ! water density (kg/m^3)
	ro_i = 930.0, & ! ice density (kg/m^3)
	ro_s = 420.0, & ! snow density
	s_i = 5.0, & ! salanity at base of sea ice (PSU)
	sig = 0.0000000567 !stefan boltxman constant 
	
		
integer :: i, month, wtype, time, dt, io, lines, detraining, k, output, j, m, n, &
seaprofile, outputsize, relaxation, idealfile, testdo, outputdif, initial, years, outdmix, outscanml

real (kind=q), dimension (nint(d_max / d_grid) + 1) :: salinity, temp, salinityinit, tempinit

real (kind=q), dimension (31536000) :: t_air, q_a, f_lw, f_sw, u_a, va, precip, ua1

real (kind=q), dimension (100) :: density, salinityout, tempout

real (kind=q), dimension(13) :: exportarray

real (kind=q), dimension(6) :: snowarray


real (kind=q) :: a_ice , & ! ice cover fraction 10 per cent from parkinson table
		aveA , & ! average annual sea ice conc
		b_del , & ! buoyancy (m/s^2)
		b_hat , & ! buoyancy flux hat (m^2/s^3)
		b_flux , & ! buoyancy flux (m^2/s^3)
		b_solar , & !solar buoyancy
		blackbody_l, &
		c1 , & ! wind stirring magnitude
		c2 , & ! dissipation
		cd_l , &
		cd_i , &
		cd , &
		ch , &
		chl , &
		da_ice , &
		d_mix, & ! depth of mixed layer
		d_mix1 , & ! depth from monin-obukhov length
		d_mix_max , & !maximum depth of mixed layer
		d_mix_max1,  &
		evap_l , &
		evap_i , &
		exportrate , & !ice export rate
		flw , &
		flead , &
	 	fsw , &
	 	ft_solar , &
	 	h_ice , & ! water-ice heat flux (J/m^2/s)
	 	h_cond, & ! ice-atmosphere heat flux (J/m^2/s)
	 	h_flux, & ! ocean atmosphere heat flux
	 	hi , &
	 	hs , &
	 	ice_vol , &
	 	i_rad_is , &
	  	latheat , & ! heatflux difference from surface water freezing
	  	latent_l , &
	  	latent_i , &
	  	m_dec , & ! decay of short wave radiation 
	  	prec , &
	  	qa , &
	  	ridge , & ! ridging , rate of growth when ice is at a_max
	  	r_heat , & ! how much fraction of surface heat is used to warm ml
	  	r_base, & ! basal heat flux from fraction of surface mellt
	  	r_time , & ! relax time
	  	s_b , & ! Bottom salinity
     	s_dash, & ! Salinity time derivative (PSU/s)
	  	s_flux , & ! salt flux (m/s)
	 	sig_w , & ! basal melting or freezing
     	s_mix, & ! Salinity of mixed layer (PSU)
     	s_mix_max, & ! Salinity at max depth
     	sensible_i , &
     	sensible_l , &
     	t_mix , & ! Temperature of mixed layer (C)
     	t_mix_max , & !Temp at maximum mixed layer depth
     	t_b , & ! Bottom temperature
     	t_surface , &
     	t_surface_i , &
     	t_surface1 , &
     	t_dash, & ! Temperatuetime derivative (C/s)
	 	t_flux , & ! temperature flux (Km/s)
	 	t_f , & ! freezing temperature of salt water (K)
	 	ta, &
	  	ua, & !wind speed from data
	  	u_star , &
	  	u_star_i , &
	  	u_star_l , &
	 	w_e, & ! entrainment duuuhhh
      	w_e1
     

end module global_data
