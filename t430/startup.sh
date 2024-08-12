# remap keys
xmodmap .Xmodmap
# make trackpoint less insanely fast
xinput --set-prop 'TPPS/2 IBM TrackPoint' 'libinput Accel Speed' -0.4
