sed -e "s/sYYYYMMDDHH/2018013100/"            \
    -e "s/eYYYYMMDDHH/2018013100/"            \
    -e "s/TEST/XXXX/"                         \
    -e "s/HPS_PTMP/hps/"                      \
    -e "s/First/XXXX/"                        \
    -e "s/Last/XXXX/"                         \
    -e "s/sACCOUNT/GEN-T20/"                  \ GEN will be avaiable 
    -e "s/sCUE2RUN/dev/"                      \ dev for GEN and devhigh for GEN-T2O
    user.conf_protype                         \
    > user.conf
