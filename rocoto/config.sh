sed -e "s/sYYYYMMDDHH/2018013100/"               \
    -e "s/eYYYYMMDDHH/2018013100/"               \
    -e "s/TEST/XXXX/"                            \
    -e "s/HPS/hps/"                              \
    -e "s/First/XXXX/"                       \
    -e "s/Last/XXXX/"                             \
    user.conf_protype                            \
    > user.conf
