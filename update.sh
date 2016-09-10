hugo
rm -rf ~/Development/temp/site
cp -r public/ ~/Development/temp/site
rm -rf public/*
git checkout gh-pages
cp -r ~/Development/temp/site/ .
