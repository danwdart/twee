{ 
  nixpkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/haskell-updates.tar.gz") {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {},
  compiler ? "ghc922"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      twee = lib.dontHaddock (self.callCabal2nix "twee" (gitignore ./.) {});
      twee-lib = lib.dontHaddock (self.callCabal2nix "twee-lib" (gitignore ./src) {});
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.twee
      p.twee-lib
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find -type f | grep -v dist-newstyle); do krank $i; done
    '';
    buildInputs = tools.defaultBuildTools;
    withHoogle = false;
  };
  exe = nixpkgs.haskell.lib.justStaticExecutables (myHaskellPackages.twee);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  twee = myHaskellPackages.twee;
  twee-lib = myHaskellPackages.twee-lib;
}

