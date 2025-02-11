#Add your packages below and uncomment --

sudo nix-shell -p "ghc.withPackages(pkgs: with pkgs; [


--text 
--lists
--your-package-here





])"
