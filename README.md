# The COM System (v0.1)
## An ephemeral GHC dev environment based on nix-pkgs as a replacement for cabal and stack 

  
### What it do:
  
✦Provides a comfy directory level dev environment for the GHC compiler and the required haskell packages for *your* project in *your* native OS. It is used for building and compiling haskell language (.hs) files into easy-to-run static executables, 
without the need to engage with cabal, stack, ghcup, flakes, hix, or shell.nix (Yay!🎉🥳🎈)   
✦Provides a way to search for the nixpkgs equivalent of the hackage packages from any .hs file and modules listed.   
✦This system formalizes what developers have been
working with for a while in NixOS, and makes the system available to all linux flavors. (Maybe WSL too).    
  
### The COM file
  
The ```com``` file is a directory level file, like Dockerifle or .yaml, used to call the specific packages needed for each build.
As of this version the ```com``` file simply provides a shell entry point to build 
a directory level instance of the most recent version of ```ghc``` with the requried packages. Future versions may have a more ordered installable executable, 
that can handle cli commands and passing lists to the package search function, but I am lazy, so this will do for version (v0.1).  
It is run simply with 
```sh com``` and can be modified with the results from ```SearchPackages.hs``` or any named hackage package to build your project with, free from the worry of env variables or paths or modules package dependencies. If it is not listed in ```SearchPackges.hs```, please modify pull and push and become a contributor as this is an active project and needs *your* help to make it the bestest ever for goodness sake. 
  
### Helper scripts
  
A helper script SearchPackages.hs is provided, and does not need to be compiled to run provided you have a GHC version already. 
```runhaskell SeachPackages.hs <my-haskell-program.hs>```  

This provides a reasonable list to modify the ```com``` file for haskell packages at build time, 
for build environments at the directory level.  See [The Build Environment]

## Running and compiling an executable

1.  Add your haskell package names to the ```com```file in the ```--Add your packages below and uncomment --``` section, and put the ```com``` file your directory that you want to compile from, usually the same directory as your .hs file.  

2.   ```$ sh com```  to start the nix-shell

3.   ```[nix-shell~]# ghc MyFavioriteHaskellProgram.hs```

4.   ```[nix-shell~]# exit``` to escape the nix-shell  

5.   run your program ```$ ./MyFavoriteHaskellProgram```

If your program fails to compile at (3) you can check the error dependencies, modify your ```com``` file with the necessary hackage packages, and recompile, and test run without ever leaving the environment as you can call ```sh com``` inside of ```sh com```to host a new shell. After compilation, you should be able to run the program from most modern os systems with a binary read capacity.  If you want to pass custom flags to the ghc compiler like ```-O2, -threaded, or +RTS -N{#of cores} ``` go for it! It works just like all your other ghc builds except its waaay faster because you aren't spending precious (T&*$) on setting up your build envs. 

## Requirements

✦Your favorite Linux/BSD/GNU/MacOS/(Maybe WSL) OS 
  
✦Bourne shell (if you use zsh just add some Zzz<sub>zz</sub>)   
  
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
your .hs files using GHC. No more .cabal, no more stack build, no more ghcup-tui.   
Just build with ```ghc WhyDidntIThinkofThis.hs```.   

Each directory should have its own ```com``` file for building the .hs files into their executable versions. So, say you had 15 different (.hs) files in one directory that you needed to put into an executeable. Well you would use the helper script, add all the packages to the ```com```file, then ```sh com``` your way to victory, because you just saved so much time that you can now make that date, game, lifelong dream a real possibility now my friend. 

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
>If you would also like to customize the GHC compiler version, there are several versions availible from the [nix-store](https://search.nixos.org/packages). Just modify the ```com``` file at ```"ghc_my_version.withPackages...```

#### Acknowledgements:

For further reading on the Nixpkgs installer check out [ItsFOSS](https://itsfoss.com/ubuntu-install-nix-package-manager/)  
For Arch specific: https://wiki.archlinux.org/title/Nix  
For Debian: https://ariya.io/2020/05/nix-package-manager-on-ubuntu-or-debian  
For the determinate installer MacOS etc...: https://github.com/DeterminateSystems/nix-installer  
More on clearing the NixStore: https://nixos.wiki/wiki/Cleaning_the_nix_store
