{
  # Fetch the latest haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/05f999da8041cfc0a0265ea071c317ad51be81b3.tar.gz") {}

  # haskell.nix provides access to the nixpkgs pins which are used by our CI,
  # hence you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003

  # haskell.nix provides some arguments to be passed to nixpkgs, including some
  # patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

  # import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs
}:
let
  isDir = path: builtins.pathExists (builtins.toPath path + "/.");
in
pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src =
    if isDir ./.git
    then
      pkgs.haskell-nix.haskellLib.cleanGit {
        name = "suimin";
        src = ./.;
      }
    else
      ./.;
  # For `cabal.project` based projects specify the GHC version to use.
  compiler-nix-name = "ghc883"; # Not used for `stack.yaml` based projects.
}
