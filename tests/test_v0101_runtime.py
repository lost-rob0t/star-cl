"""Execute the generated CL boundary for every canonical document contract."""
import json
import os
from pathlib import Path
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[1]
SCHEMA = json.loads((ROOT / "schemas/starintel-0.10.1/generated/schema.json").read_text())
MANIFEST = json.loads((ROOT / "schemas/starintel-0.10.1/generated/portable-manifest.json").read_text())


def sample(schema):
    if "$ref" in schema:
        return sample(SCHEMA["$defs"][schema["$ref"].split("/")[-1]])
    if "enum" in schema:
        return schema["enum"][0]
    if "anyOf" in schema:
        return sample(schema["anyOf"][0])
    kind = schema.get("type")
    if kind == "object":
        return {key: sample(schema["properties"][key]) for key in schema.get("required", [])}
    if kind == "array":
        return []
    if kind in ("integer", "number"):
        return schema.get("minimum", 0)
    if kind == "boolean":
        return False
    if schema.get("format") == "date-time":
        return "2026-10-03T12:00:00Z"
    if schema.get("format") == "date":
        return "2026-10-03"
    if schema.get("format") == "uri":
        return "https://example.test/"
    if "pattern" in schema:
        if "@" in schema["pattern"]:
            return "fixture@example.test"
        if "0-9()." in schema["pattern"]:
            return "+123456789"
        return "0"
    return "fixture"


def document(dtype):
    definition = "".join(word.capitalize() for word in dtype.split("-"))
    value = sample(SCHEMA["$defs"][definition])
    value.update(id="fixture:" + dtype, dataset="conformance", dtype=dtype, schemaVersion="0.10.1")
    return value


def run(command, value=None):
    process = subprocess.run([os.environ.get("STARINTEL_SBCL", "sbcl"), "--script", str(ROOT / "bin/starintel-canonical.lisp")],
                             input=json.dumps({"command": command, "document": value}), text=True, capture_output=True)
    if not process.stdout:
        raise AssertionError(process.stderr[-4000:])
    return json.loads(process.stdout), process.returncode


class CanonicalRuntimeTests(unittest.TestCase):
    def test_all_generated_document_types_validate_and_binding_roundtrip(self):
        types = [entry["name"].split("/")[-1] for entry in MANIFEST["types"] if entry["kind"] == "document"]
        capabilities, status = run("capabilities")
        self.assertEqual(status, 0)
        self.assertEqual(set(capabilities["documentTypes"]), set(types))
        for dtype in types:
            with self.subTest(dtype=dtype):
                value = document(dtype)
                response, status = run("binding-roundtrip", value)
                self.assertEqual(status, 0, response)
                self.assertEqual(response["document"], value)

    def test_referenced_enums_scalars_required_fields_and_unknowns_rejected(self):
        cases = []
        value = document("wireless-network"); value["security"] = "wpa4"; cases.append((value, "invalid_enum"))
        value = document("person"); value["id"] = "invalid space"; cases.append((value, "pattern_mismatch"))
        value = document("person"); value["createdAt"] = -1; cases.append((value, "below_minimum"))
        value = document("geo-point"); value["latitude"] = "90.00000001"; cases.append((value, "above_maximum"))
        value = document("person"); value["confidence"] = "0.12345"; cases.append((value, "invalid_scale"))
        value = document("person"); value["dob"] = "2026-02-30"; cases.append((value, "invalid_date"))
        value = document("url"); value["url"] = "invalid URL with spaces"; cases.append((value, "invalid_uri"))
        value = document("wireless-station"); del value["mac"]; cases.append((value, "missing_required_field"))
        value = document("mission"); value["dtype"] = "made-up"; cases.append((value, "unknown_object_type"))
        value = document("person"); value["schemaVersion"] = "0.10.2"; cases.append((value, "unsupported_spec_version"))
        value = document("person"); value["data"] = {}; cases.append((value, "undeclared_field"))
        value = document("person"); value["_id"] = value["id"]; cases.append((value, "undeclared_field"))
        value = document("person"); value["sources"] = [{"id": "source"}]; cases.append((value, "missing_required_field"))
        for value, category in cases:
            with self.subTest(category=category):
                response, status = run("validate", value)
                self.assertEqual(status, 1)
                self.assertEqual(response["error"], category)

    def test_unknown_extension_and_false_values_survive(self):
        value = document("person")
        value.update(deleted=False, extensions={"example.vendor": {"opaque_key": None, "flag": False, "items": []}})
        response, status = run("binding-roundtrip", value)
        self.assertEqual(status, 0, response)
        self.assertEqual(response["document"], value)


if __name__ == "__main__":
    unittest.main()
