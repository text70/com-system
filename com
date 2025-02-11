sudo nix-shell --packages 'ghc.withPackages (pkgs: [ 

--Add your packages below and uncomment --

--networking 
--your-hackage-packages






])'
