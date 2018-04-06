sed -e "s/sYYYYMMDDHH/2018013100/"               \
    -e "s/eYYYYMMDDHH/2018013100/"               \
    -e "s/TEST/xzinit/"                            \
    -e "s/HPS_PTMP/hps/"                              \
    -e "s/First/Xiaqiong/"                       \
    -e "s/Last/Zhou/"                             \
    user.conf_protype                            \
    > user.conf
