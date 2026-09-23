./clean.sh 
mkdir -p gradients palettes
wget --no-hsts -O cpt-city.zip https://phillips.shef.ac.uk/pub/cpt-city/resource/packages/188
fname=`unzip -Z -1 cpt-city.zip | head -n1`
unzip -q cpt-city.zip
mv $fname cpt-city
cd cpt-city
find . -name "*.c3g" > cpt-city-names.txt
cd ..

