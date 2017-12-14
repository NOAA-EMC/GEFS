sed -e "s/sYYYYMMDDHH/2017121312/"               \
    -e "s/eYYYYMMDDHH/2017121312/"               \
    -e "s/TEST/tgBd/"                            \
    -e "s/HPS/hps/"                              \
    -e "s/First/Dingchen/"                       \
    -e "s/Last/Hou/"                             \
    user.conf_protype                            \
    > user.conf
