sed -e "s/sYYYYMMDDHH/2018013100/"            \
    -e "s/eYYYYMMDDHH/2018013100/"            \
    -e "s/TEST/XXXX/"                         \
    -e "s/HPS_PTMP/hps/"                      \
    -e "s/First/XXXX/"                        \
    -e "s/Last/XXXX/"                         \
    -e "s/sACCOUNT/GEN-T2O/"                  \
    -e "s/sCUE2RUN/devhigh/"                  \
    user.conf_protype                         \
    > user.conf
### Note for WCOSS: For sACCOUNT, GEN is suggested (if available) except devhigh users
### Note for WCOSS: For sCUE2RUN, dev for GEN and devhigh for GEN-T2O is recommended 
