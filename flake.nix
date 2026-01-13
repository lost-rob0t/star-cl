{
  description = "Star-cl: Document Spec for handling osint data";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      # Custom CMS-ULID dependency
      cms-ulid = pkgs.sbcl.buildASDFSystem rec {
        pname = "cms-ulid";
        version = "latest";
        src = pkgs.fetchgit {
          url = "https://gitlab.com/colinstrickland/cms-ulid.git";
          rev = "fff84302dee5db42fb90aafd834af3ffbfd6c2bb";
          hash = "sha256-B5rekME60bWHk47kDepQpOr9drjgXjZBiRpA+Ob1CuU=";
        };
        lispLibs = with pkgs.sbclPackages; [ local-time ironclad bit-smasher serapeum ];
      };

      # Create an SBCL with our custom packages
      sbcl' = pkgs.sbcl.withOverrides (self: super: {
        inherit cms-ulid;
      });

      # Build the main starintel system
      starintel = sbcl'.buildASDFSystem rec {
        pname = "starintel";
        version = "0.7.2";
        src = ./.;

        lispLibs = with sbcl'.pkgs; [
          jsown ironclad local-time cms-ulid str closer-mop
        ];

        systems = [ "starintel" ];

        # Keep test asd file so tests can be built
        asdFilesToKeep = [ "src/starintel.asd" "starintel-test.asd" ];

        dontStrip = true;
      };

      # Build the test system
      starintel-test = sbcl'.buildASDFSystem {
        pname = "starintel-test";
        version = "0.7.2";
        src = ./.;

        lispLibs = with sbcl'.pkgs; [
          starintel
          fiveam
        ];

        systems = [ "starintel-test" ];

        dontStrip = true;
      };

      # Create wrapper with all dependencies
      sbcl-wrapped = sbcl'.withPackages (ps: with ps; [
        starintel
      ]);

      # Create wrapper for tests
      sbcl-test-wrapped = sbcl'.withPackages (ps: with ps; [
        starintel-test
      ]);

    in {
      packages.${system} = {
        default = starintel;
        starintel = starintel;
        starintel-test = starintel-test;
      };

      # Add test checks
      checks.${system} = {
        starintel-tests = pkgs.stdenv.mkDerivation {
          name = "starintel-tests-check";
          src = ./.;

          nativeBuildInputs = [ sbcl-test-wrapped ];

          buildPhase = ''
            export HOME=$TMPDIR
            export XDG_CACHE_HOME="$HOME/.cache"

            # Copy source to writable location
            cp -r $src $TMPDIR/source
            chmod -R u+w $TMPDIR/source
            cd $TMPDIR/source

            echo "=========================================="
            echo "  Running StarIntel Tests"
            echo "=========================================="
            echo ""

            ${sbcl-test-wrapped}/bin/sbcl --non-interactive --no-userinit --no-sysinit \
              --eval "(require :asdf)" \
              --eval "(push (truename \".\") asdf:*central-registry*)" \
              --eval "(asdf:load-system :starintel-test)" \
              --eval "(handler-case
                        (progn
                          (asdf:test-system :starintel-test)
                          (uiop:quit 0))
                        (error (e)
                          (format t \"~%~%========================================~%\")
                          (format t \"  Test Error~%\")
                          (format t \"========================================~%\")
                          (format t \"~%Error: ~a~%~%\" e)
                          (uiop:quit 1)))" \
              2>&1 | tee $TMPDIR/test-output.log

            TEST_EXIT_CODE=''${PIPESTATUS[0]}

            if [ $TEST_EXIT_CODE -eq 0 ]; then
              echo ""
              echo "✓ Test check passed"
            else
              echo ""
              echo "✗ Test check failed with exit code $TEST_EXIT_CODE"
              exit $TEST_EXIT_CODE
            fi
          '';

          installPhase = ''
            mkdir -p $out
            cp $TMPDIR/test-output.log $out/test-results.log
            echo "Test results saved to $out/test-results.log"
          '';
        };
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          sbcl-wrapped pkg-config
        ];

        shellHook = ''
          echo "StarIntel dev environment ready"
          echo "Use: sbcl to start SBCL with all dependencies"
          echo "Test with: nix flake check"
        '';
      };
    };
}
