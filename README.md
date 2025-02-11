# The COM System
## An ephemeral ghc dev environment based on nix-pkgs as a replacement for cabal and stack 

  
### What it do:
  
Provides ghc and the required haskell packages for building and compiling the haskell language into static executeables, 
without the need for cabal, stack, or ghcup. This system formalizes what developers have been
working with for a while in NixOS, and makes the system available to all linux flavors. (Maybe WSL).    
  
### The COM file
  
The ```com``` file is used to call the specific packages needed for each build.
As of this version the ```com``` file simply provides a shell entry point to build 
a directory level instance of ```ghc``` with the requried packages. It is run simply with 
```sh com```
  
### Helper scripts
  
A helper script SearchPackages.hs is provided, and does not need to be compiled to run.  
```runhaskell SeachPackages.hs <my-haskell-program.hs>```  

This provides a reasonable list to modify the ```com``` file for haskell packages necessary, 
for build environments at the directory level.  
  
### The Build environment

There is no need to call cabal or stack, or search through endless hackage or hoogle
databases with the helper scripts installed. The ```com``` system itself is based on 
Nixpkgs and nix-shell. This can be installed on any linux environment with the 
```haskell_nix_build.sh``` file. This is a daemon that will install the nix-shell, nix-packages and access
to the nix-store on your system at the root level (if you don't have a nix-shell already).  
This will also modify your path to contain the root executable  
```nix-shell``` in the terminal.  

Once in the com shell after executing ```sh com``` you have a shell built, specifically for building
your .hs files using ghc. No more .cabal, no more stack build, no more ghcup-tui.   
Just build with ```ghc WhyDidntIThinkofThis.hs```. 


Acknowledgements:

For further reading on the Nixpkgs installer check out (https://itsfoss.com/ubuntu-install-nix-package-manager/)  
For Arch specific: https://wiki.archlinux.org/title/Nix  
For Debian: https://ariya.io/2020/05/nix-package-manager-on-ubuntu-or-debian  
For the determinate installer MacOS etc...: https://github.com/DeterminateSystems/nix-installer  
