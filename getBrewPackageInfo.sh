brewpacks="$(brew list)"
for pack in ${brewpacks}
do
    brew deps --tree $pack >> brewPackageInfo.txt
done
