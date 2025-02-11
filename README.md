# The COM System (v0.1)
## An ephemeral ghc dev environment based on nix-pkgs as a replacement for cabal and stack 

  
### What it do:
  
✦Provides a directory level environment ghc compiler and the required haskell packages for building and compiling the haskell language (.hs) files into easy-to-run static executeables, 
without the need to engage with cabal, stack, or ghcup (Yay!🎉🥳🎈)   
✦Provides a way to search for the nixpkgs equivalent of the hackage packages from any .hs file and modules listed.   
✦This system formalizes what developers have been
working with for a while in NixOS, and makes the system available to all linux flavors. (Maybe WSL).    
  
### The COM file
  
The ```com``` file is a directory level file, like Dockerifle or .yaml, used to call the specific packages needed for each build.
As of this version the ```com``` file simply provides a shell entry point to build 
a directory level instance of ```ghc``` with the requried packages. Future versions may have a more ordered installable executeable, 
that can handle cli commands and passing lists to the package search function, but I am lazy, so this will do for version (v0.1).  
It is run simply with 
```sh com``` and can be modified with the results from ```SearchPackages.hs``` or any named hackage package.
  
### Helper scripts
  
A helper script SearchPackages.hs is provided, and does not need to be compiled to run provided you have ghc version already. 
```runhaskell SeachPackages.hs <my-haskell-program.hs>```  

This provides a reasonable list to modify the ```com``` file for haskell packages at build time, 
for build environments at the directory level.  See [The Build Environment]

## Running and compiling an executeable

1.  Add your haskell package names to the ```com```file.  

2.   ```$ sh com```  to start the nix-shell

3.   ```[nix-shell~]# ghc MyFavioriteHaskellProgram.hs```

4.   ```[nix-shell~]# exit``` to escape the nix-shell  

5.   ```$ ./MyFavoriteHaskellProgram```

## Requirements

✦A Linux/BSD/GNU/MacOS/(Maybe WSL) OS 
  
✦Bourne shell   
  
✦~10GB(+/-3GB) space for nix-packages and the nix-store local cache   
(nix-store is ephemeral and can be cleared with ```nix-store --gc```)


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


### Example
If you already have GHC and nix-shell installed, you can try it out:  
```runhaskell SeachPackages.hs Cookies.hs```  
Add results to the ```com``` file pkgs list  
```$ sh com```  
```~$ ghc Cookies.hs```  
```~$ exit```  
```$ ./Cookies```    
  

>[!NOTE]   
>The nix-packages channel provided by COM(v0.1) is the recommended channel nixpkgs-unstable.
>Please be aware, that while the repository tries to keep the latest version of pkgs in the database,
>it is a rolling release version of linux and some pkgs may cause packages with outdated dependencies from hackage to fail compilation. If this is the case,
>please contact the package maintainers. 



>[!TIP]
>If you would also like to customize the GHC compiler version, there are several versions availible from the [nix-store](https://search.nixos.org/packages).

#### Acknowledgements:

For further reading on the Nixpkgs installer check out [ItsFOSS](https://itsfoss.com/ubuntu-install-nix-package-manager/)  
For Arch specific: https://wiki.archlinux.org/title/Nix  
For Debian: https://ariya.io/2020/05/nix-package-manager-on-ubuntu-or-debian  
For the determinate installer MacOS etc...: https://github.com/DeterminateSystems/nix-installer  
More on clearing the NixStore: https://nixos.wiki/wiki/Cleaning_the_nix_store
