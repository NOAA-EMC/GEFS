sed -e "s/sYYYYMMDDHH/2018013100/"            \
    -e "s/eYYYYMMDDHH/2018013100/"            \
    -e "s/TEST/XXXX/"                         \
    -e "s/HPS_PTMP/hps/"                      \
    -e "s/First/XXXX/"                        \
    -e "s/Last/XXXX/"                         \
    -e "s/sACCOUNT/GEN-T20/"                     \
    -e "s/sCUE2RUN/devhigh/"                     \
    user.conf_protype                         \
    > user.conf
