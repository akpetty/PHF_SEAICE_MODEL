

program ml_run

use global_data
use ml_procs

implicit none
save



CHARACTER (LEN=7) :: filename
CHARACTER (LEN=7) :: filepath
integer :: forcetype


! The interpolated data sets
!open(20, file="Forcings/climSmIntA.txt")

! The idealised forcing data sets
!open(20, file="Forcings/Amundsenbase/ideala.txt")

forcetype = 1

years = 10

output = 1 ! 1 to output density 0 not to bother
outdmix = 1 !do we want to output dmix, smix file?
outscanml = 0

if (forcetype.eq.1) then
	open(20, file="Forcings/Weddellbase/idealw.txt")
	

	if(output.eq.1) then



	open(3, file="../Output/Weddell10year/density.txt")
	open(4, file="../Output/Weddell10year/salinity.txt")
	open(5, file="../Output/Weddell10year/temperature.txt")

	open(30, file="../Output/Weddell10year/test1.csv")
	open(31, file="../Output/Weddell10year/test1PARAMETERS.csv")

	end if

	initial = 2
	
	open(1, file="../Output/Weddell10year/mixedlayerdmix.txt")

elseif (forcetype.eq.2) then
	
	open(20, file="../Forcings/Weddellbase/idealw.txt")
		
	if(output.eq.1) then

	open(3, file="../Output/AmundsenW/density.txt")
	open(4, file="../Output/AmundsenW/salinity.txt")
	open(5, file="../Output/AmundsenW/temperature.txt")

	open(30, file="../Output/AmundsenW/test.csv")
	open(31, file="../Output/AmundsenW/testPARAMETERS.csv")

	end if

	initial = 1
	
	open(1, file="../Output/AmundsenW/mixedlayerdmix.txt")
	
elseif (forcetype.eq.3) then
	!open(20, file="../Forcings/Amundsenbase/ideala.txt")
	
	open(20, file="../Forcings/climSmIntE.txt")
	
	if(output.eq.1) then

	open(3, file="density.txt")
	open(4, file="salinity.txt")
	open(5, file="temperature.txt")

	open(30, file="../Output/Wong/test.csv")
	open(31, file="../Output/Wong/testPARAMETERS.csv")

	end if

	initial = 3
	
	open(1, file="mixedlayerdmix.txt")

end if



! The interpolated data sets
!open(20, file="Forcings/climSmIntA.txt")

idealfile = 20




!If reading in actual data need to change below to include the u and v winds etc, note the int data has the u wind in last column already

!===========================================================================
lines = 1 ! initialise number of lines before the loop as dont want to rest this value

do 
read(idealfile, *, iostat = io) t_air(lines), f_lw(lines), f_sw(lines),q_a(lines), precip(lines),&
!ua1(lines), va(lines),  &
u_a(lines)

if (io < 0) exit
lines = lines + 1
end do

close(idealfile)

!================================================================



!====== OPEN FILE TO READ DATA IN TO AND INITIALISE DATA ===========
open(10, file="mixedlayer.txt")

!WHICH SETUP

!1 = Amundsen
!2 = Weddell
!3 = Wong
!initial = 2

!NO OF YEARS


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
hs = 0.30_q

!a_ice = 0.0_q
!hi = 0
a_ice = 0.8_q
hi = 1.4


seaprofile = 1

r_heat = 0.75
!r_heat = 0.0
!================================================================

!===========Wong=============
elseif (initial.eq.3) then

d_mix = 10.0_q
relaxation = 1 ! 0 for no relaxation 1 for relaxation
r_time = 0.5 ! so 1* is one year relaxation time period for water below the mixed layer
t_mix = 273.15 + 1.0_q
s_mix = 34.0_q
exportrate = 1.0_q !how much sea ice export in a year in units of area
aveA = 0.7_q ! average sea ice conc for export rate calculation
hs = 0.2_q

seaprofile = 7

a_ice = 0.0_q
hi = h_min


r_heat = 0.4
!================================================================

end if

ridge = 0.0
time = 0.0
dt= 31536000 / (lines-1)

n = 1 !for density output loop

outputsize = 100 ! how many points we want for density output of water column


outputdif = nint(d_max / (outputsize* d_grid)) ! diffrence in index from original density arrays to output arrays


150 format (A, F10.2)
151 format (A, I2)
write(31, 150) "Initial dmix:",  d_mix
write(31, 150) "Initial smix:",  s_mix
write(31, 150) "Initial tmix:",  t_mix
write(31, 150) "Export Rate:",  exportrate
write(31, 150) "AveA:",  aveA
write(31, 150) "Initial ice thickness:",  hi
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
	
elseif (seaprofile.eq.7) then
	 
	do j = 1, nint(150 /  d_grid)
	salinity(j) = 34.0 + (0.5 * j / (nint(150 /  d_grid)))
	temp(j) = 273.15 - 1.8 + (2.8 * j / (nint(150 /  d_grid)))
	end do
	do j = nint(150/d_grid) + 1, (nint(d_max / d_grid) + 1)
	salinity(j) = 34.5 + (0.1 * (j - nint(150/d_grid))  / (nint(350 /  d_grid)))!check this
	temp(j) = 273.15 +1.0 + (0.8 * (j - nint(150/d_grid)) / (nint(350 /  d_grid)))
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

! create an initial temp and salinity grid as a reference to relax back to
do j=1, (nint(d_max / d_grid) + 1)
salinityinit(j) = salinity(j)
tempinit(j) = temp(j)
end do
!=========================================================================


!============== RUN PROGRAM & INITIALISE FORCINGS ====================================
do k=1, years ! NUMBER OF YEARS

	call mixedlayercalc()

	print *,"Year:", k, d_mix_max, hi, a_ice

	write(30, 40) k ,d_mix_max,  s_mix_max, t_mix_max - 273.15, exportrate, hi
	40 format(I2, ",", F7.2,",", F6.2,",", F6.2,",", F6.2,",", F6.2)

end do !k=loop

print *, "Maximum Depth Conditions"
print *, "Depth:",d_mix_max, "Salinity:", s_mix_max


close(3)
close(4)
close(5)
close(1)
close(30)



end program ml_run