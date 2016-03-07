! ALEKS MIXED LAYER MODEL

module ml_procs

use global_data

implicit none

contains

!========SECANT FUNCTION=============

function findroot(func, t_low, t_high)

	!use global_data, only : q
 implicit none
 
 real(kind=q) :: findroot, func
 real(kind=q), intent(in) :: t_low, t_high
 
 
 real (kind=q) :: t1, t2, f_diff, t_diff, t0
  
  !t_diff = ((t_guess + 30) - (t_guess - 20)) / 10.0
  !t0 = ((t_guess + 30) + (t_guess - 20)) / 2.0 
  t1 = t_high
  t0 = t_low
  !t1 = t0 + t_diff
  t_diff = t1 - t0
  do while (abs(t_diff).gt.1E-04)
  	f_diff = func(t1) - func(t0)
  	t2 = t1 - (func(t1)*(t1-t0)/f_diff)
  	t0=t1
  	t1=t2
  	t_diff = t1-t0
  end do
  
  findroot = t1
   
end function findroot

!============== SENSIBLE HEAT =================
function sensible(t_so)
!use global_data, only : ro_a, c_a, cd, ua, ta, q
	implicit none
	
	real(kind=q) :: sensible
	real (kind=q), intent (in) :: t_so
	sensible = (ro_a * c_a * cd * ua * (t_so - ta))

end function sensible
!==============================================

!=============== LATENT HEAT ===================
function latent(t_so, lat)
	!use global_data, only : ro_a, cd, ua, p_atm, q, qa
	implicit none
	real(kind=q) :: latent
	real (kind=q), intent (in) :: t_so, lat
	latent = (ro_a * lat * cd * ua * ((157600000  / ((p_atm * exp(5420 / t_so)) - 95600000)) - qa))

end function latent
!===============================================

!============= BLACKBODY RADIATION =============
function blackbody(t_so)
!use global_data, only : sig, q
	implicit none
	real(kind=q) :: blackbody
	real (kind=q), intent (in) :: t_so
	blackbody = (sig * (t_so ** 4))

end function blackbody
!===============================================

!=============== OCEAN HEAT FLUX ================
! got rid of the chl term as i think it is probs not needed!
 function oceanflux(t_so)
	!use global_data, only : ro_w,ro_a, c_w, chl, cd_l, ua, t_mix, q
	implicit none
	real(kind=q) :: oceanflux
	real (kind=q), intent (in) :: t_so
	!oceanflux = (ro_w * c_w * ch * c_d * ua * (t_mix - t_so)) ! Need to use diff values for ch and ustar for ocean
	oceanflux = (ro_w * c_w * sqrt(cd_l * ro_a / ro_w) * ua * (t_mix - t_so))

end function oceanflux
!================================================

!=============SNOW/ICE HEAT CONDUCTANCE===========
 function icesnowcond(t_so)
!use global_data, only : ki, ks, t_f, hs, hi, q
	implicit none
	real(kind=q) :: icesnowcond
	real (kind=q), intent (in) :: t_so
	if(hi>=0.0_q) then
	icesnowcond = (ki * ks * (t_f - t_so)) / ((ki * hs) + (ks * hi))
	else
	icesnowcond=0.0_q
	endif

end function icesnowcond
!=================================================

!======== NET HEAT EQUATION TO BE SOLVED =========

function fx(t_so)

!use global_data, only : alb_w, lat_v, e_w, alb_i, lat_s, i_rad_l, i_rad_i, e_i, alb_s, lat_s, &
!e_s, ta, qa, flw, fsw, q, wtype, cd
	implicit none
	
	real(kind=q) :: fx
	real (kind=q), intent (in) :: t_so
	real (kind=q) :: extflux, lat, e_isw, alb, i_rad1
	
	
	if (wtype == 0) then ! calculate surface temp for LEADS
	extflux = oceanflux(t_so) ! ocean heat flux nb. should be using diff values for ch and ustar for ocean
	i_rad1 = i_rad_l
	alb = alb_w
	lat = lat_v
	e_isw = e_w
	
	elseif (wtype == 1) then ! calculate for ICE COVERED surface temp
	extflux = icesnowcond(t_so)
	i_rad1 = i_rad_i !NCAR delta-eddington uses 0.7
	alb = alb_i
	lat = lat_s
	e_isw = e_i
	
	else ! caluclate for SNOW COVERED surface temp
	extflux = icesnowcond(t_so)
	i_rad1 = 0.0
	alb = alb_s
	lat=lat_s
	e_isw = e_s
	
	endif
	
	fx =  sensible(t_so) + & !sensible heat
	latent(t_so, lat) + & !latent heat term
	(e_isw * blackbody(t_so)) - & !blackbody radiation
	((1 - alb) * (1 - i_rad1) * fsw) - & !shortwave heat flux think i need to include the fraction absored below bottom is minus
	(e_isw * flw) - & !longwave heat flux
	extflux
	

end function fx



!===========Monin_Obukhov Length==================
!function monin(dmix)

!use global_data, only : c1_hat, u_star, b_flux, g, alpha, ro_w, c_w, kappa_w, a_ice, i_rad_l, alb_w, fsw, q, c2
!implicit none

!real(kind=q) :: monin
!real (kind=q), intent (in) :: dmix

!monin = (c1_hat * exp(-dmix / 10.0) * (u_star ** 3)) + (c2 * dmix * b_flux) &
 !       - (((g * alpha) / (ro_w * c_w)) * dmix * (1 - exp(-kappa_w*dmix)) * (1 - a_ice) * i_rad_l * (1 - alb_w) * fsw)	

!end function monin
!================================================



!==================== MAIN PROGRAM ============================
!
!
!
!
!===============================================================

subroutine mixedlayercalc()



implicit none
save



8 format (100F10.5)
9 format (6E10.3)


n=1

d_mix_max = d_mix
s_mix_max = s_mix
t_mix_max = t_mix

!RUN PROGRAM
do i=1, (lines -1) ! Do for whole years worth of timesteps

ta = t_air(i)
qa = q_a(i)
flw = f_lw(i)
fsw = f_sw(i)
ua = u_a(i)
!ua = u_a1(i)*u_a(i) + v
prec = 0.001*precip(i) !convert kg m^-2 s^-1 to m/s by dividing by 1000


!======== UPDATE SALINITY/TEMP PROFILE AND DECIDE ON BOTTOM VALUES=========

do j=1, nint(d_mix / d_grid)
salinity(j) = s_mix
temp(j) = t_mix
end do

!========= CREATE SHORT SALINITY/TEMP/DENSITY OUTPUT ARRAYS===============

!DO WE WANT TO OUTPUT DENSITY ARRAYS
if (output.eq.1) then

!Density output loop do for every 100th timestep
m = i/(n*100)


if (m.eq.1) then



do j=1, outputsize
density(j) = 1028.11 * (1.0 + (0.000786 * (salinity(-(outputdif/2) + (outputdif * j)) - 35)) &
- (0.0000387 * (temp(-(outputdif/2) + (outputdif * j)) - 273.15))) - 1000.0
salinityout(j) = salinity(-(outputdif/2) + (outputdif * j))
tempout(j) = temp(-(outputdif/2) + (outputdif * j)) - 273.15
end do

write(3, 8) density
write(4, 8) salinityout
write(5, 8) tempout
!print *, density

n=n+1
end if ! M LOOP

end if ! OUTPUT
!==========================================================================

!ASSIGN BOTTOM TEMP/SALINITY VALUES
s_b = salinity(nint(d_mix / d_grid) + 1) 
t_b = temp(nint(d_mix / d_grid) + 1)

!Apply relaxation to salinity and temp grids below the mixed layer
if (relaxation.eq.1) then
do j = (nint(d_mix / d_grid) + 1), (nint(d_max / d_grid) + 1)
salinity(j) = salinity(j) + ((salinityinit(j) - salinity(j)) * dt / (r_time*31536000))
temp(j) = temp(j) + ((tempinit(j) - temp(j)) * dt / (r_time*31536000))
end do
end if


!======= SET TRANSFER COEFFICIENTS ======================

!c_d = (0.78 + (0.475 * sqrt(0.0185)) * ua) * 0.001
cd_l = 0.001
cd_i = 0.0013
ch = 0.006
chl = 0.006
u_star_l = sqrt(cd_l * ro_a / ro_w) * ua
u_star_i = sqrt(cd_i * ro_a / ro_w) * ua
u_star = sqrt((a_ice * u_star_i * u_star_i) + ((1 - a_ice) * u_star_l * u_star_l))
!u_star = 0.01

!===============================================

!======== MIXED LAYER CALCULATION ==========

! make sure interpoalted data for incoming shortwave radition does not go below zero.
if (fsw.le.0.0) then
fsw = 0.0
endif

! recalculate freezing temp of water based on salinity of mixed layer
t_f = 273.15 - (0.054 * s_mix)	

! calculate wind stirring based on  mixed layer depth based on tang1998
c1 = c1_hat * exp(-d_mix / 10)
!c1 = c1_hat * exp(-d_mix / 30) * 3 TANG 

! grid volume
ice_vol = hi * a_ice

!=============OCEAN HEAT FLUX AND ICE FORMATION ==============

wtype = 0 ! means we are calculating surface temp for LEAD
cd = cd_l !FOR LEAD HEAT FLUX USE LEAD TRANSFER COEFF
t_surface = findroot(fx, t_mix - 20.0, t_mix + 20.0)

latent_l = latent(t_surface, lat_v) !To output heat fluxes for analysing
sensible_l = sensible(t_surface)
blackbody_l = blackbody(t_surface)
flead = oceanflux(t_surface)
evap_l = latent(t_surface, lat_v) / (lat_v * ro_w)
!evap = 0.0

if (t_surface > t_f.and. a_ice > 0.0) then ! when surface temp greater than freezing and with ice present the ice loss is..

! see maykut perovich 1987 paper
	
	h_flux = oceanflux(t_f)
	
	!total latent heat to melt ice. negative corresponds to melting
	latheat = oceanflux(t_surface) - oceanflux(t_f)
	!a fraction used to melt laterally 
    da_ice = (latheat * (1 - r_heat) * (1 - a_ice)) / (lat_f * ro_i * hi)
    
    !extra heat flux to base of ice
    r_base = -(latheat * r_heat * (1 - a_ice))
    
    a_ice = a_ice + (da_ice * dt) ! may need to put anohter if statement in saying if it goes negative then just set to 0
    t_surface1 = t_f 
    ridge =0.0
    
	elseif (t_surface < t_f.and. a_ice < (a_max)) then! when surface temp less than freezing

	h_flux = oceanflux(t_f)	
	latheat = oceanflux(t_surface) - oceanflux(t_f)
    da_ice = (latheat * (1 - a_ice)) / (lat_f * ro_i * hi) ! should this be density of water
    a_ice = a_ice + (da_ice * dt)
    t_surface1 = t_f
    ridge = 0.0
    r_base = 0.0
    
    elseif(t_surface < t_f.and.a_ice >= (a_max)) then
    
    h_flux = oceanflux(t_f)
    latheat = oceanflux(t_surface) - oceanflux(t_f)
    a_ice = a_ice
    ridge = (latheat * (1 - a_ice)) / (lat_f * ro_i)
    t_surface1 = t_f
        r_base = 0.0
        da_ice = 0.0

    else ! when surface temp greater than freezing and no ice 
    
    h_flux = oceanflux(t_surface)
	latheat = 0.0
	a_ice = a_ice
	t_surface1 = t_surface
	ridge = 0.0
        r_base = 0.0
        da_ice = 0.0

end if

! ===== ICE DIVERGENCE ==============
! there are 31536000 seconds in a year.
! If we wish to see a full ice area removed in a year then we have 1/time in a year * time
! so that for every time interval within a year we remove enough so that once a year passes
! we have effectively removed the whole ice cover

a_ice = a_ice - (exportrate*aveA*0.0000000317*dt)

!=====================================

if (a_ice > a_max) then
	a_ice = a_max
elseif (a_ice.le.0.0) then
	a_ice = 0.0_q
	hi = h_min
endif
!==================================================================

!===============CALC ICE/SNOW SURFACE TEMP (see bitz1999)============================

! If there is a snow layer then set to snow covered ice for surface flux calculations
! and decide if we have penetrating solar radiation (0 pen for snow covered ice)
if (hs > 0.0001) then
wtype=2
i_rad_is = 0.0
else
wtype=1
i_rad_is = i_rad_i
endif

cd = cd_i !FOR ICE FLUX USE ICE TRANSFER COEFF 

if(hi>0) then
t_surface_i = findroot(fx, t_mix - 20.0, t_mix + 20.0)
else
t_surface_i = 273.15
endif

!calc evaporation over ice
evap_i =  0.0 !latent(t_surface_i, lat_s) / (lat_s * ro_s)

!hs = hs + (prec - evap_i) * a_ice
if (hs < 0.0) then
hs = 0.0
end if

!if (t_surface_i > t_f) then
!t_surface_i = t_f

!else
!t_surface_i = t_surface_i

!endif

!should now calculate the amount of surface ablation we get based on this temp, perhaps fixing at 0degrees C
!=====================================================================


! calculate water-ice heat flux
!h_ice = ro_w * c_w * ch * u_star * (t_mix - t_f)

if (hi.gt.0.0.and.a_ice.gt.0.0) then
h_ice = ro_w * c_w * ch * sqrt(0.0013*ro_a/ro_w) * ua * (t_mix - t_f)
else
h_ice = 0.0
endif

! calculate ice-atmosphere heat flux based on 0-layer semtner model

if (hi.gt.0.0.and.a_ice.gt.0.0) then
h_cond = (ki * ks * (t_f - t_surface_i)) / ((ki * hs) + (ks * hi))
else
h_cond = 0.0_q
endif

! calculate  if we have basal melting or freezing
sig_w = (h_ice + r_base - h_cond) / (ro_i * lat_f)


! caluclate new ice thickness based on basal melting/freezing
hi = hi - (sig_w * dt) + (ridge * dt)


if (hi<h_min) then
hi = h_min
endif



! short wavelangth decay
!m_dec = 1 - ((2 * d_att) / d_mix) + (1 + ((2 * d_att) / d_mix)) * exp(-d_mix / d_att)
m_dec = 1 - exp(-kappa_w * d_mix)

!Caculate temp flux
t_flux = (1 / (ro_w * c_w)) * (((1-a_ice) * h_flux) + (a_ice * h_ice))

!Calculate salt flux
s_flux = (ro_i / ro_w) * (s_mix - s_i) * (((sig_w - ridge) * a_ice) - (hi* da_ice)) &
		+ ((prec - evap_l) * s_mix * (1 - a_ice))
		
		!- ((ro_i / ro_w) * da_ice * hi * (s_mix - s_i))
		!- ((latheat * (s_mix - s_i)  * (1 - a_ice)) / (lat_f * ro_i))
		!- ((ro_i / ro_w) * (s_mix - s_i) * ridge * a_ice)
		
!buoyancy
if (d_mix <d_max) then
b_del = (g * alpha * (t_mix - t_b)) - (g * beta * (s_mix - s_b))
else
b_del = 0.0
end if

ft_solar =  (fsw / (ro_w * c_w * d_mix)) * ((m_dec * (1 - a_ice) * (1 - alb_w) * i_rad_l ) &
+ ( i_rad_is * m_dec * exp(-kappa_i  * hi) * (1 - alb_i) * a_ice))

b_solar = - g * alpha * ft_solar

!buoyancy flux
b_flux = (g * alpha * t_flux)  - (g * beta * s_flux)

if (b_flux > 0) then
	c2 = 0.8 !magnitude of buoyancy flux i guess is something to do with dissipation
else
	c2 = 0.8
end if	

! calculate entrainment rate
w_e = (1.0 / ((d_mix * b_del) + (c_m ** 2))) &
	* ((c1 * (u_star ** 3)) + (c2 * d_mix * b_flux) &
        + (b_solar * d_mix))
        
!===================== DETRAINMENT CALCULATIONS =================================

if ((d_max - d_mix) < 0.0001.and.w_e > 0.0) then
w_e = 0.0
end if

!old way of calculating when we have detrainment

!print *, d_mix, w_e
!w_e = 0.0_q

!d_mix = findroot(monin, 0.0001, d_mix)
!print *, d_mix, b_del,time, fsw, a_ice, s_b, s_mix, t_mix - 273.15 , t_b - 273.15, t_f - 273.15, t_surface - 273.15, a_ice


!endif
!=======================================================


d_mix = d_mix + dt * w_e


if (d_mix.lt.1.0) then
d_mix = 1.0
end if


w_e1 = w_e

if (d_mix > d_max) then
d_mix = d_max
end if

if (w_e < 0.0) then
w_e = 0.0
end if

s_dash =  - (w_e / d_mix) * (s_mix - s_b) - (s_flux / d_mix)
t_dash = (w_e / d_mix) * (t_b - t_mix) - (t_flux / d_mix) + &
 ft_solar




! EULER STEP calculate salinity of mixed layer
s_mix = s_mix + s_dash * dt

! EULER STEP calculate temperature of mixed layer
t_mix = t_mix + t_dash * dt



if (outdmix.eq.1) then
write(1,6) time, d_mix, a_ice, hi, s_mix
6 format(I10, " ", E15.8, " ", E15.8, " ", E15.8, " ", E15.8)
end if

time = time + dt

if (outscanml.eq.1) then

write(10,5) time, d_mix, s_mix, t_mix, w_e, b_del, b_flux, s_flux, t_flux, t_f, &
s_i, h_ice, h_cond, c1, sig_w, t_surface, latheat, a_ice, t_surface_i, flw, fsw, &
hi, ua, qa, ta, s_b, t_b, ft_solar, u_star, da_ice, prec, evap_l, r_base

5 format(I10, " ", E15.8," ", F15.6," ", E15.8," ",E15.8," ",E15.8, &
" ",E15.8," ",E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," "," ", E15.8, &
" ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", &
E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8," ", E15.8)

end if

!=====================================
if (d_mix.ge.d_mix_max) then
d_mix_max = d_mix
s_mix_max = s_mix
t_mix_max = t_mix
end if
!====================================
end do !i-loop








 
 end subroutine mixedlayercalc
 
 end module ml_procs









