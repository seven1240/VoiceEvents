#/bin/bash 
sudo gem uninstall telegraph
gem build telegraph.gemspec
sudo gem install telegraph-0.0.1.gem
