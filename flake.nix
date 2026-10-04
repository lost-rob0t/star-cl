{
  description = "Star-cl: StarLang-generated StarIntel 0.10.1 contract and legacy compatibility";

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
          version = "0.10.1";
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

          systems = [ "starintel" "starintel-0101" "starintel-v090" "starintel-legacy" ];
          asdFilesToKeep = [ "src/starintel.asd" "starintel-legacy.asd" "starintel-0101.asd" "starintel-v090.asd" "starintel-test.asd" ];
          dontStrip = true;
        };

        starintel-archive = pkgs.sbcl.buildASDFSystem rec {
          pname = "starintel-archive";
          version = "0.1.0";
          src = ./.;

          lispLibs = [
            self.starintel
          ];

          systems = [ "starintel-archive" ];
          asdFilesToKeep = [ "starintel-archive.asd" "starintel-archive-test.asd" ];
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
        starintel-archive-test = pkgs.sbcl.buildASDFSystem rec {
          pname = "starintel-archive-test";
          version = "0.1.0";
          src = ./.;

          lispLibs = [
            self.starintel-archive
            self.fiveam
          ];

          systems = [ "starintel-archive-test" ];
          dontStrip = true;
        };

      });

      starintel = sbcl'.pkgs.starintel;
      starintel-archive = sbcl'.pkgs.starintel-archive;
      starintel-test = sbcl'.pkgs.starintel-test;
      starintel-archive-test = sbcl'.pkgs.starintel-archive-test;
      cms-ulid = sbcl'.pkgs.cms-ulid;

      sbcl-wrapped = sbcl'.withPackages (ps: [
        ps.starintel
        ps.starintel-archive
      ]);

      sbcl-test-wrapped = sbcl'.withPackages (ps: [
        ps.starintel-test
        ps.starintel-archive-test
      ]);
    in
    {
      packages.${system} = {
        default = starintel;
        starintel = starintel;
        starintel-archive = starintel-archive;
        starintel-test = starintel-test;
        starintel-archive-test = starintel-archive-test;
        cms-ulid = cms-ulid;
        sbcl-wrapped = sbcl-wrapped;
        sbcl-test-wrapped = sbcl-test-wrapped;
      };

      checks.${system} = {
        canonical-contract = pkgs.runCommand "starintel-canonical-contract" {
          nativeBuildInputs = [ pkgs.python3 sbcl-test-wrapped ];
        } ''
          export XDG_CACHE_HOME="$TMPDIR/.cache"
          cp -r ${self} source
          chmod -R u+w source
          cd source
          python3 -m unittest discover -s tests -p test_v0101_runtime.py -v
          python3 -m unittest discover -s tests -p test_public_api.py -v
          python3 -m unittest discover -s tests -p test_exact_json.py -v
          mkdir -p $out
        '';
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
              --eval "(asdf:initialize-source-registry (list :source-registry (list :tree (truename \".\")) :inherit-configuration))" \
              --eval "(handler-case
                        (progn
                          (asdf:load-system :starintel-test)
                          (asdf:test-system :starintel-test)
                          (asdf:load-system :starintel-archive-test)
                          (asdf:test-system :starintel-archive-test)
                          (uiop:quit 0))
                        (error (condition)
                          (format *error-output* \"~&StarIntel tests failed: ~a~%\" condition)
                          (uiop:quit 1)))" \
              2>&1 | tee $TMPDIR/test-output.log

            TEST_EXIT_CODE=''${PIPESTATUS[0]}

            if [ $TEST_EXIT_CODE -eq 0 ]; then
              echo ""
              echo "Test check passed"
            else
              echo ""
              echo "Test check failed with exit code $TEST_EXIT_CODE"
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
