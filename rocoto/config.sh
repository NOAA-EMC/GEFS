sed -e "s/sYYYYMMDDHH/2018013100/"               \
    -e "s/eYYYYMMDDHH/2018013100/"               \
    -e "s/TEST/BF44960/"                            \
    -e "s/HPS/hps3/"                              \
    -e "s/First/Bing/"                       \
    -e "s/Last/Fu/"                             \
    user.conf_protype                            \
    > user.conf
