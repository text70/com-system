#Add your packages below and uncomment --

sudo nix-shell -p "ghc.withPackages(pkgs: with pkgs; [


--text 
--random
--your-package-name-here



])"
