./clean.sh 
mkdir -p gradients palettes
wget --no-hsts -O cpt-city.zip http://soliton.vm.bytemark.co.uk/pub/cpt-city/pkg/cpt-city-c3g-2.24.zip
unzip -q cpt-city.zip
cd cpt-city
find . -name "*.c3g" > cpt-city-names.txt
cd ..

