#!/bin/bash
#OAR -n 1
#OAR -l /nodes=1
#OAR --project futureweb
#OAR --stdout InferStatus_Sc4_aout2023_CAZ_maskSea.out
#OAR --stderr InferStatus_Sc4_aout2023_CAZ_maskSea.err

#load modules
source /applis/site/nix.sh

#directories
dir=/hdd/home/lvelez/zonation/ZonationIUCN
bin=/hdd/home/lvelez/zonation/build/zig4/zig4

time $bin -r $dir/CAZ_maskSea.dat $dir/InferStatus_Sc4_aout2023.spp $dir/ZonationOUT/InferStatus_Sc4_aout2023_CAZ_maskSea 0.0 0 1.0 1 &

wait