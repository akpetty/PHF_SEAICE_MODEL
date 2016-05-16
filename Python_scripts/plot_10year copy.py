import os
import matplotlib 
matplotlib.use("AGG")

from pylab import *

rcParams['axes.labelsize'] =9
rcParams['xtick.labelsize']=9
rcParams['ytick.labelsize']=9
rcParams['legend.fontsize']=9
rcParams['font.size']=9
rc('font',**{'family':'sans-serif','sans-serif':['Arial']})

dmax = 500;
dspace = 5;
depth = arange(dspace/2,dmax,dspace)
smin=34.
smax=34.5
tmin=-1.9
tmax=1.
yrs=['1','2','3','4','5','6','7','8','9','10']


filepath = '../Output/Amundsen10year/'

temp = loadtxt(filepath+'temperature.txt', unpack=True)
salinity = loadtxt(filepath+'salinity.txt', unpack=True)
density = loadtxt(filepath+'density.txt', unpack=True)
time, d_mix, a_ice, hi, s_mix = loadtxt(filepath+'mixedlayerdmix.txt', unpack=True)
numpts = temp.shape[1]
time_col=np.linspace(0, 31536000*10, numpts)

print('loaded files')
print('plotting..')
#######################################################
fig = figure(figsize=(5.5,4.5))
ax1 = subplot(3,1,1)

pl1 =ax1.plot(time, a_ice,label="$A$",color='b')
ax1.set_ylabel('Ice concentration', color='b')
for tl in ax1.get_yticklabels():
  tl.set_color('b')
ax1.set_ylim(0,1)

ax11 = ax1.twinx()
pl11=ax11.plot(time, hi,label="$h_i$", color='g')
ax11.set_ylabel('Ice thickness (m)', color='g')
ax11.set_ylim(0, 2)
for tl in ax11.get_yticklabels():
  tl.set_color('g')
  
xlim(xmax=max(time))
print('change xticks')
xticks(linspace(max(time)/10, max(time),10),yrs)
ax1.set_xticklabels([])
ax1.yaxis.grid(True)
ax1.xaxis.grid(True)
 

ax2 = subplot(3,1,2)

plot(time, d_mix,color='w', linewidth = 2)
xlim(xmax=max(time))
xticks(linspace(max(time)/10, max(time),10),yrs)
ax2.set_ylabel('Depth (m)')
ax2.set_xticklabels([])
ax2.xaxis.grid(True)
hold(True)
p1 = pcolormesh(time_col, depth, temp, vmin=tmin, vmax=tmax, cmap = cm.RdYlBu_r)
gca().invert_yaxis()
cax = fig.add_axes([0.88, 0.4, 0.025, 0.2])
#
cbar1 = colorbar(p1, cax=cax, extend='both',orientation='vertical')
cbar1.set_label(u'Potential temperature (\u00B0C)')
cbar1.set_ticks(np.linspace(tmin, tmax, 5))
cbar1.solids.set_rasterized(True)

ax3 = subplot(3,1,3)

plot(time, d_mix,color='w', linewidth = 2)
xlim(xmax=max(time))
xticks(linspace(max(time)/10, max(time),10),yrs)
ax3.set_ylabel('Depth (m)')

hold(True)
p3 = pcolormesh(time_col, depth, salinity, vmin=smin, vmax=smax, cmap = cm.YlGnBu)
gca().invert_yaxis()

cax2 = fig.add_axes([0.88, 0.12, 0.025, 0.2])
#use_gridspec=True
cbar2 = colorbar(p3, cax=cax2, extend='both',orientation='vertical')
cbar2.set_label('Salinity')
cbar2.set_ticks(np.linspace(smin, smax, 5))
cbar2.solids.set_rasterized(True)
ax3.xaxis.grid(True)
ax3.set_xlabel('Years')


subplots_adjust(hspace=0.15, right=0.87, bottom=0.09, left=0.1)
print('saving figure..')
savefig(filepath+'10year_run.png',dpi=300)