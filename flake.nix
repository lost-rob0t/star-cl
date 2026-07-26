{
  description = "Star-cl: StarIntel v0.9.0 document implementation";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      sbcl' = pkgs.sbcl.withOverrides (self: super: {
        cms-ulid = pkgs.sbcl.buildASDFSystem rec {
          pname = "cms-ulid";
          version = "latest";
          src = pkgs.fetchgit {
            url = "https://gitlab.com/colinstrickland/cms-ulid.git";
            rev = "fff84302dee5db42fb90aafd834af3ffbfd6c2bb";
            hash = "sha256-B5rekME60bWHk47kDepQpOr9drjgXjZBiRpA+Ob1CuU=";
          };

          lispLibs = [
            self.local-time
            self.ironclad
            self.bit-smasher
            self.serapeum
          ];

          dontStrip = true;
        };

        starintel = pkgs.sbcl.buildASDFSystem rec {
          pname = "starintel";
          version = "0.9.0";
          src = ./.;

          lispLibs = [
            self.jsown
            self.jzon
            self.cl-ppcre
            self.ironclad
            self.local-time
            self.cms-ulid
            self.str
            self.closer-mop
          ];

          systems = [ "starintel" "starintel-v090" ];
          asdFilesToKeep = [ "src/starintel.asd" "starintel-v090.asd" "starintel-test.asd" ];
          dontStrip = true;
        };

        starintel-test = pkgs.sbcl.buildASDFSystem rec {
          pname = "starintel-test";
          version = "0.9.0";
          src = ./.;

          lispLibs = [
            self.starintel
            self.fiveam
          ];

          systems = [ "starintel-test" ];
          dontStrip = true;
        };
      });

      starintel = sbcl'.pkgs.starintel;
      starintel-test = sbcl'.pkgs.starintel-test;
      cms-ulid = sbcl'.pkgs.cms-ulid;

      sbcl-wrapped = sbcl'.withPackages (ps: [
        ps.starintel
      ]);

      sbcl-test-wrapped = sbcl'.withPackages (ps: [
        ps.starintel-test
      ]);
    in
    {
      packages.${system} = {
        default = starintel;
        starintel = starintel;
        starintel-test = starintel-test;
        cms-ulid = cms-ulid;
        sbcl-wrapped = sbcl-wrapped;
        sbcl-test-wrapped = sbcl-test-wrapped;
      };

      checks.${system} = {
        starintel-tests = pkgs.stdenv.mkDerivation {
          name = "starintel-tests-check";
          src = ./.;
          nativeBuildInputs = [ sbcl-test-wrapped ];

          buildPhase = ''
            export HOME=$TMPDIR
            export XDG_CACHE_HOME="$HOME/.cache"

            cp -r $src $TMPDIR/source
            chmod -R u+w $TMPDIR/source
            cd $TMPDIR/source

            ${sbcl-test-wrapped}/bin/sbcl --non-interactive --no-userinit --no-sysinit \
              --eval "(require :asdf)" \
              --eval "(push (truename \".\") asdf:*central-registry*)" \
              --eval "(asdf:load-system :starintel-test)" \
              --eval "(handler-case
                        (progn
                          (asdf:test-system :starintel-test)
                          (uiop:quit 0))
                        (error (e)
                          (format t \"~%Test error: ~a~%\" e)
                          (uiop:quit 1)))" \
              2>&1 | tee $TMPDIR/test-output.log

            TEST_EXIT_CODE=''${PIPESTATUS[0]}
            if [ $TEST_EXIT_CODE -ne 0 ]; then
              exit $TEST_EXIT_CODE
            fi
          '';

          installPhase = ''
            mkdir -p $out
            cp $TMPDIR/test-output.log $out/test-results.log
          '';
        };
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          sbcl-test-wrapped
          pkgs.pkg-config
        ];

        shellHook = ''
          echo "StarIntel v0.9.0 development environment ready"
          echo "Test with: nix flake check"
        '';
      };
    };
}
