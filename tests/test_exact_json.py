"""Raw JSON enters SDK-owned readers before any possible binary-float coercion."""
from decimal import Decimal
import json
import os
from pathlib import Path
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[1]
NUMBERS = ["0.12345678901234567890123456789", "9007199254740993",
           "-1234567890123456789012345678901234567890", "1e10000", "1e-10000",
           "-7.00000000000000000000000000001E+900", "-0.0000", "1.00e0"]
DOCUMENT = ('{"id":"person:exact","dataset":"conformance","dtype":"person",'
            '"schemaVersion":"0.10.1","extensions":{"numbers":[' + ','.join(NUMBERS) +
            '],"nested":{"false":false,"null":null,"empty":[],"object":{}},'
            '"unicode":"雪😀 café","numericString":"1e10000",'
            '"escaped":"quote \\\" and slash \\\\"}}')


def decoded(text):
    return json.loads(text, parse_float=Decimal)


def run(script, text):
    return subprocess.run([os.environ.get("STARINTEL_SBCL", "sbcl"), "--script", str(ROOT / script)],
                          input=text, text=True, capture_output=True, timeout=90)


def boundary(text, mode="parse", **kwargs):
    return run("tests/exact-json-smoke.lisp", json.dumps({"input": text, "mode": mode, **kwargs}))


class ExactJSONTests(unittest.TestCase):
    def test_public_json_and_root_boundaries(self):
        for mode in ["parse", "root", "root-decode", "root-create"]:
            with self.subTest(mode=mode):
                result = boundary(DOCUMENT, mode)
                self.assertEqual(result.returncode, 0, result.stderr[-4000:])
                self.assertEqual(decoded(result.stdout), decoded(DOCUMENT))
                for token in NUMBERS:
                    self.assertIn(token, result.stdout)

    def test_cli_roundtrip_and_generated_binding(self):
        for command in ["roundtrip", "binding-roundtrip"]:
            with self.subTest(command=command):
                result = run("bin/starintel-canonical.lisp", '{"command":' + json.dumps(command) + ',"document":' + DOCUMENT + '}')
                self.assertEqual(result.returncode, 0, result.stderr[-4000:])
                self.assertEqual(decoded(result.stdout)["document"], decoded(DOCUMENT))
                for token in NUMBERS:
                    self.assertIn(token, result.stdout)

    def test_writer_cycles_depth_and_non_finite_values(self):
        result = run("tests/exact-json-invariants.lisp", "")
        self.assertEqual(result.returncode, 0, result.stderr[-4000:])
        self.assertIn("Exact JSON invariants passed", result.stdout)

    def test_bignum_beyond_typical_digit_limits(self):
        token = "9" * 5000
        result = boundary(token)
        self.assertEqual(result.returncode, 0, result.stderr[-4000:])
        self.assertEqual(result.stdout.strip(), token)

    def test_extreme_exponents_do_not_expand(self):
        for token in ["1e999999999999999999999", "1e-999999999999999999999", "-1e-9999999999999999999999"]:
            result = boundary(token)
            self.assertEqual(result.returncode, 0, result.stderr[-4000:])
            self.assertEqual(result.stdout.strip(), token)

    def test_exact_integer_validation_and_bounds(self):
        integer = {"type": "integer", "minimum": 0, "maximum": 65535}
        for token in ["0", "-0.0", "1.0", "1e0", "655350e-1"]:
            self.assertEqual(boundary(token, "bounds", schema=integer).returncode, 0, token)
        for token in ["1.00000000000000000000000000001", "-1e-10000", "1e10000", "65535.00000000000000001", "true", "\"1\""]:
            self.assertNotEqual(boundary(token, "bounds", schema=integer).returncode, 0, token)
        number = {"type": "number", "minimum": -1, "maximum": 1}
        for token in ["0.99999999999999999999999999", "-0.99999999999999999999999999", "1e-10000"]:
            self.assertEqual(boundary(token, "bounds", schema=number).returncode, 0, token)
        for token in ["1.00000000000000000000001", "-1.00000000000000000000001", "1e999999999", "-1e999999999"]:
            self.assertNotEqual(boundary(token, "bounds", schema=number).returncode, 0, token)

    def test_root_integer_field_uses_exact_validation(self):
        for token, valid in [("1.0", True), ("1e0", True), ("1e999999999999999999999", True), ("1e-999999999999999999999", False), ("1.00000000000000000000000001", False), ("-1", False), ("true", False)]:
            raw = DOCUMENT[:-1] + ',"createdAt":' + token + '}'
            result = boundary(raw, "root")
            self.assertEqual(result.returncode == 0, valid, (token, result.stderr[-2000:]))

    def test_malformed_json_stays_rejected(self):
        for text in ["", "01", "-01", "+1", ".1", "1.", "1e", "1e+", "--1", "NaN", "Infinity", "-Infinity", "1 2",
                     "[1,]", "{\"x\":1,}", "{\"x\" 1}", "{x:1}", "[1 2]", "truefalse", '"bad\\q"', '"bad\n"', "/*comment*/1"]:
            with self.subTest(text=text):
                self.assertNotEqual(boundary(text).returncode, 0)


if __name__ == "__main__":
    unittest.main()
