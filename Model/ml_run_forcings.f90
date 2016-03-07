program ml_run_forcings

use global_data
use ml_procs

implicit none
save


CHARACTER (LEN=7) :: filename 
integer :: forcetype


! The interpolated data sets
!open(20, file="Forcings/climSmIntA.txt")

! The idealised forcing data sets
!open(20, file="Forcings/Amundsenbase/ideala.txt")

do forcetype = 1, 10



if (forcetype.eq.1) then
open(20, file="Forcings/Weddellbase/idealw.txt")
open(21, file="Forcings/Weddellbase/idealwta2.txt")
open(22, file="Forcings/Weddellbase/idealwta3.txt")
open(23, file="Forcings/Weddellbase/idealwta4.txt")
open(24, file="Forcings/Weddellbase/idealwta5.txt")
filename = "WTatest"
initial = 2

elseif (forcetype.eq.2) then 
open(20, file="Forcings/Weddellbase/idealw.txt")
open(21, file="Forcings/Weddellbase/idealwqa2.txt")
open(22, file="Forcings/Weddellbase/idealwqa3.txt")
open(23, file="Forcings/Weddellbase/idealwqa4.txt")
open(24, file="Forcings/Weddellbase/idealwqa5.txt")
filename = "WQatest"
initial = 2

elseif (forcetype.eq.3) then
open(20, file="Forcings/Weddellbase/idealw.txt")
open(21, file="Forcings/Weddellbase/idealwua2.txt")
open(22, file="Forcings/Weddellbase/idealwua3.txt")
open(23, file="Forcings/Weddellbase/idealwua4.txt")
open(24, file="Forcings/Weddellbase/idealwua5.txt")
filename = "WUatest"
initial = 2

elseif (forcetype.eq.4) then
open(20, file="Forcings/Weddellbase/idealw.txt")
open(21, file="Forcings/Weddellbase/idealwflw2.txt")
open(22, file="Forcings/Weddellbase/idealwflw3.txt")
open(23, file="Forcings/Weddellbase/idealwflw4.txt")
open(24, file="Forcings/Weddellbase/idealwflw5.txt")
filename = "WLwtest"
initial = 2

elseif (forcetype.eq.5) then
open(20, file="Forcings/Weddellbase/idealw.txt")
open(21, file="Forcings/Weddellbase/idealwprec2.txt")
open(22, file="Forcings/Weddellbase/idealwprec3.txt")
open(23, file="Forcings/Weddellbase/idealwprec4.txt")
open(24, file="Forcings/Weddellbase/idealwprec5.txt")
filename = "WPrtest"
initial = 2


elseif (forcetype.eq.6) then
open(20, file="Forcings/Amundsenbase/ideala.txt")
open(21, file="Forcings/Amundsenbase/idealata4.txt")
open(22, file="Forcings/Amundsenbase/idealata3.txt")
open(23, file="Forcings/Amundsenbase/idealata2.txt")
open(24, file="Forcings/Amundsenbase/idealata1.txt")
filename = "ATatest"
initial = 1

elseif (forcetype.eq.7) then 
open(20, file="Forcings/Amundsenbase/ideala.txt")
open(21, file="Forcings/Amundsenbase/idealaqa4.txt")
open(22, file="Forcings/Amundsenbase/idealaqa3.txt")
open(23, file="Forcings/Amundsenbase/idealaqa2.txt")
open(24, file="Forcings/Amundsenbase/idealaqa1.txt")
filename = "AQatest"
initial = 1


elseif (forcetype.eq.8) then
open(20, file="Forcings/Amundsenbase/ideala.txt")
open(21, file="Forcings/Amundsenbase/idealaua4.txt")
open(22, file="Forcings/Amundsenbase/idealaua3.txt")
open(23, file="Forcings/Amundsenbase/idealaua2.txt")
open(24, file="Forcings/Amundsenbase/idealaua1.txt")
filename = "AUatest"
initial = 1


elseif (forcetype.eq.9) then
open(20, file="Forcings/Amundsenbase/ideala.txt")
open(21, file="Forcings/Amundsenbase/idealaflw4.txt")
open(22, file="Forcings/Amundsenbase/idealaflw3.txt")
open(23, file="Forcings/Amundsenbase/idealaflw2.txt")
open(24, file="Forcings/Amundsenbase/idealaflw1.txt")
filename = "ALwtest"
initial = 1


elseif (forcetype.eq.10) then
open(20, file="Forcings/Amundsenbase/ideala.txt")
open(21, file="Forcings/Amundsenbase/idealaprec4.txt")
open(22, file="Forcings/Amundsenbase/idealaprec3.txt")
open(23, file="Forcings/Amundsenbase/idealaprec2.txt")
open(24, file="Forcings/Amundsenbase/idealaprec1.txt")
filename = "APrtest"
initial = 1

end if

open(30, file="./Reports/mixedlayer_report/Recent/forcings/" // filename //".csv")
open(31, file="./Reports/mixedlayer_report/Recent/forcings/" // filename // "PARAMETERS.csv")


print *, filename

!open(25, file="Forcings/Weddellbase/idealwua5.txt")
!open(26, file="Forcings/Weddellbase/idealwprec5.txt")
!idealfile = 21


do idealfile = 20, 24

!If reading in actual data need to change below to include the u and v winds etc, note the int data has the u wind in last column already

lines = 1 ! initialise number of lines before the loop as dont want to rest this value

do 
read(idealfile, *, iostat = io) t_air(lines), f_lw(lines), f_sw(lines),q_a(lines), precip(lines),&
!ua1(lines), va(lines),  &
u_a(lines)

if (io < 0) exit
lines = lines + 1
end do

close(idealfile)

print *, "Forcing:" ,idealfile
print *, lines
!================================================================


!====== OPEN FILE TO READ DATA IN TO AND INITIALISE DATA ===========

open(1, file="mixedlayer.txt")

!WHICH SETUP
!1 = Amundsen
!2 = Weddell
!3 = Wong


!NO OF YEARS
years = 1

!===========AMUNDSEN=============
if (initial.eq.1) then

d_mix = 10.0_q
relaxation = 1.0 ! 0 for no relaxation 1 for relaxation
r_time = 1.0 ! so 1* is one year relaxation time period for water below the mixed layer
t_mix = 273.15 + 1.0_q
s_mix = 33.0_q
exportrate = 1.0_q !how much sea ice export in a year in units of area
aveA = 0.30_q ! average sea ice conc for export rate calculation
hs = 0.2_q

a_ice = 0.0_q
hi = h_min

seaprofile = 4

r_heat = 0.4
!================================================================

!==========WEDDELL==============
elseif (initial.eq.2) then

d_mix = 10.0_q
relaxation = 1 ! 0 for no relaxation 1 for relaxation
r_time = 0.5 ! so 1* is one year relaxation time period for water below the mixed layer
t_mix = 273.15 + 1.0_q
s_mix = 34.0_q
exportrate = 1.0_q !how much sea ice export in a year in units of area
aveA = 0.7_q ! average sea ice conc for export rate calculation
hs = 0.3_q

!a_ice = 0.0_q
!hi = 0
a_ice = 0.8_q
hi = 1.4


seaprofile = 1

r_heat = 0.75
!r_heat = 0.0
!================================================================

end if

ridge = 0.0
time = 0.0
dt= 31536000 / (lines-1)

output = 0 ! 1 to output density 0 not to bother
outdmix = 0 ! do not output dmix, smix info etc

150 format (A, F10.2)
151 format (A, I2)
write(31, 150) "Initial dmix:",  d_mix
write(31, 150) "Initial smix:",  s_mix
write(31, 150) "Initial tmix:",  t_mix
write(31, 150) "Export Rate:",  exportrate
write(31, 150) "AveA:",  aveA
write(31, 150) "Initial ice thickness:",  hi
write(31, 150) "Initial ice conc:",  a_ice
write(31, 150) "Snow thicnkess:",  hs
write(31, 150) "Relaxation:",  r_time
write(31, 151) "Profile:",  seaprofile
write(31, 150) "Rside:",  r_heat


close(31)
!===============================================================

!================ CREATE LINEAR SALINITY/TEMP ARRAYS =========================

print *, "Profile:", seaprofile

if (d_max.eq.500) then

if (seaprofile.eq.1) then
	do j = 1, nint((d_max /  d_grid) + 1)
	salinity(j) = 34.2 + (0.35 * j / (nint(d_max / (d_grid))))
	temp(j) = (273.15 - 1.8) + (0.3 * j / (nint(d_max /  d_grid)))
	end do
	
elseif (seaprofile.eq.4) then
	do j = 1, nint(d_max /  d_grid)
	salinity(j) = 34.0 + (0.5 * j / (nint(d_max / d_grid)))
	temp(j) = 273.15 - 1.8 + (2.8 * j / (nint(d_max / d_grid)))
	end do

end if

elseif (d_max.eq.1000) then

if (seaprofile.eq.1) then
	do j = 1, nint(d_max / (2 * d_grid))
	salinity(j) = 34.2 + (0.35 * j / (nint(d_max / (2 * d_grid))))
	temp(j) = 273.15 - 1.8 + (0.3 * j / (nint(d_max / (2 * d_grid))))
	end do
	do j = nint(d_max/(2*d_grid)) + 1, (nint(d_max / d_grid) + 1)
	salinity(j) = 34.55
	temp(j) = (273.15 - 1.8) + 0.3
	end do
	
elseif (seaprofile.eq.4) then
	do j = 1, nint(d_max / (2 * d_grid))
	salinity(j) = 34 + (0.5 * j / (nint(d_max / (2 * d_grid))))
	temp(j) = 273.15 - 1.8 + (2.8 * j / (nint(d_max / (2 * d_grid))))
	end do
	do j = nint(d_max/(2*d_grid)) + 1, (nint(d_max / d_grid) + 1)
	salinity(j) = 34.5
	temp(j) = 273.15 - 1.8 + 2.8
	end do

end if

end if
!===============================================================	

! create an initial temp and salinity grid as a reference to relax back to
do j=1, (nint(d_max / d_grid) + 1)
salinityinit(j) = salinity(j)
tempinit(j) = temp(j)
end do

!=========================================================================

	call mixedlayercalc()

	write(30, 40) idealfile  ,d_mix_max,  s_mix_max, t_mix_max - 273.15
	40 format(I2, ",", F7.2,",", F6.2,",", F6.2)

	print *, "Maximum Depth Conditions"
	print *, "Depth:",d_mix_max, "Salinity:", s_mix_max

end do !Forcings

close(1)
close(30)


end do !forcetype


end program ml_run_forcings

