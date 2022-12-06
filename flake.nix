{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , devshell
    , flake-utils
    }:
    flake-utils.lib.eachDefaultSystem (system: (
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            devshell.overlay
          ];
        };
      in
      {
        devShells.default = pkgs.devshell.mkShell {
          motd = "";
          devshell.packages = [
            pkgs.hyperfine
            pkgs.nixpkgs-fmt
            pkgs.ruby_3_1
          ];
        };
      }
    ));
}
