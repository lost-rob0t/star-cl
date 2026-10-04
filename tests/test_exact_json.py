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

    def test_shared_raw_duplicate_key_corpus(self):
        result = run("tests/duplicate-json-keys.lisp", "")
        self.assertEqual(result.returncode, 0, result.stderr[-4000:])
        self.assertIn("27 shared raw JSON key cases passed", result.stdout)

    def test_historical_cli_checks_raw_keys_before_decoding(self):
        cases = json.loads((ROOT / "tests/fixtures/raw-json-unique-keys.json").read_text())["cases"]
        for case in cases:
            # The historical Jzon decoder cannot represent 1e10000. Preserve
            # that existing limit; canonical exact-number coverage is separate.
            if case["name"] == "exact_values_control":
                continue
            request = '{"command":"version","probe":' + case["wire"] + '}'
            result = run("bin/starintel-conformance.lisp", request)
            with self.subTest(case=case["name"]):
                self.assertEqual(result.returncode == 0, case["valid"], result.stderr[-2000:])
                if not case["valid"]:
                    self.assertIn("duplicate json key", (result.stdout + result.stderr).lower())
                else:
                    self.assertTrue(json.loads(result.stdout)["ok"])

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

    def test_all_zero_lexemes_survive_and_validate(self):
        zeros = ["0", "-0", "0.0", "-0.0", "0e0", "-0e0", "0.000E+10000", "-0.000E-10000"]
        for token in zeros:
            with self.subTest(token=token):
                result = boundary(token)
                self.assertEqual(result.returncode, 0, result.stderr[-2000:])
                self.assertEqual(result.stdout.strip(), token)
                exact_zero = {"type": "integer", "minimum": 0, "maximum": 0}
                checked = boundary(token, "bounds", schema=exact_zero)
                self.assertEqual(checked.returncode, 0, checked.stderr[-2000:])
                self.assertEqual(checked.stdout.strip(), token)
        nested = '{"zeros":[' + ','.join(zeros) + ']}'
        self.assertEqual(boundary(nested).stdout.strip(), nested)
        raw = DOCUMENT[:-1] + ',"createdAt":-0}'
        for mode in ["root", "root-decode"]:
            result = boundary(raw, mode)
            self.assertEqual(result.returncode, 0, result.stderr[-2000:])
            self.assertIn('"createdAt":-0', result.stdout)

    def test_malformed_json_stays_rejected(self):
        for text in ["", "01", "-01", "+1", ".1", "1.", "1e", "1e+", "--1", "NaN", "Infinity", "-Infinity", "1 2",
                     "[1,]", "{\"x\":1,}", "{\"x\" 1}", "{x:1}", "[1 2]", "truefalse", '"bad\\q"', '"bad\n"', "/*comment*/1"]:
            with self.subTest(text=text):
                self.assertNotEqual(boundary(text).returncode, 0)


if __name__ == "__main__":
    unittest.main()
