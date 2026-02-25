{
  description = "Kiro Emacs modes with tests and Lean4 proofs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages.default = pkgs.stdenv.mkDerivation {
          name = "kiro-el";
          src = ./.;
          
          buildInputs = [ pkgs.emacs pkgs.lean4 ];
          
          buildPhase = ''
            emacs --batch -L . -l kiro-task-mode.el -l kiro-meta-mode.el -l kiro-tenfold.el -l kiro-test.el -f ert-run-tests-batch-and-exit
          '';
          
          installPhase = ''
            mkdir -p $out/share/emacs/site-lisp
            cp *.el $out/share/emacs/site-lisp/
            mkdir -p $out/share/lean4
            cp *.lean lakefile.lean $out/share/lean4/
          '';
        };
      });
}
