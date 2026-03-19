{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;

  # Use same pinned nixpkgs as scrappy for compatible haskell packages
  myPkgs =
    let
      hostPkgs = import <nixpkgs> {};
      pinnedPkgs = hostPkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "b4372c4924d9182034066c823df76d6eaf1f4ec4";
        sha256 = "TJv2srXt6fYPUjxgLAL0cy4nuf1OZD4KuA1TrCiQqg0=";
      };
    in
      import pinnedPkgs {};

  hp = myPkgs.haskellPackages;
  haskellLib = myPkgs.haskell.lib;

  nix-thunkSrc = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "nix-thunk";
    rev = "8fe6f2de2579ea3f17df2127f6b9f49db1be189f";
    sha256 = "14l2k6wipam33696v3dr3chysxhqcy0j7hxfr10c0bxd1pxv7s8b";
  };
  nix-thunk = import nix-thunkSrc {};

  webdriverSrc = nix-thunk.thunkSource ../scrappy/deps/haskell-webdriver;
  webdriver = haskellLib.doJailbreak (hp.callCabal2nix "webdriver" webdriverSrc {});

  # Build scrappy from local source, adding missing cabal deps via override
  scrappy = haskellLib.dontCheck (haskellLib.overrideCabal (hp.callCabal2nix "scrappy" ../scrappy {}) (old: {
    libraryHaskellDepends = (old.libraryHaskellDepends or []) ++ [
      webdriver
      hp.witherable
    ];
    librarySystemDepends = (old.librarySystemDepends or []) ++ [
      myPkgs.nodejs
      pkgs.zlib
      pkgs.gmp
    ];
  }));

  drv = hp.callCabal2nix "table-demo" ./. {
    inherit scrappy;
  };

in
pkgs.mkShell {
  buildInputs = [ myPkgs.cabal-install ];
  inputsFrom = [ (if pkgs.lib.inNixShell then drv.env else drv) ];
}
