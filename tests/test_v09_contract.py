from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def balanced_lisp(text: str) -> bool:
    depth = 0
    in_string = False
    escaped = False
    in_comment = False
    for char in text:
        if in_comment:
            if char == "\n":
                in_comment = False
            continue
        if in_string:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            continue
        if char == ";":
            in_comment = True
        elif char == '"':
            in_string = True
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth < 0:
                return False
    return depth == 0 and not in_string


class CommonLispV09ContractTests(unittest.TestCase):
    def test_changed_lisp_files_are_balanced(self) -> None:
        paths = [
            ROOT / "src" / "schema-org.lisp",
            ROOT / "src" / "types.lisp",
            ROOT / "src" / "documents.lisp",
            ROOT / "src" / "json.lisp",
            ROOT / "src" / "json-v09.lisp",
            ROOT / "src" / "exports-v09.lisp",
            ROOT / "t" / "documents-test.lisp",
            ROOT / "t" / "json-test.lisp",
        ]
        for path in paths:
            with self.subTest(path=path.name):
                self.assertTrue(balanced_lisp(path.read_text(encoding="utf-8")))

    def test_system_and_document_versions_are_v09(self) -> None:
        asd = (ROOT / "src" / "starintel.asd").read_text(encoding="utf-8")
        documents = (ROOT / "src" / "documents.lisp").read_text(encoding="utf-8")
        self.assertIn(':version "0.9.0"', asd)
        self.assertIn('+starintel-doc-version+ "0.9.0"', documents)
        self.assertIn("schema-version", documents)
        self.assertIn("schema-org", documents)
        self.assertRegex(documents, r"date-added\s+:accessor doc-added\s+:type string")
        self.assertRegex(documents, r"version\s+:accessor doc-version\s+:type integer")

    def test_schema_org_map_covers_49_types(self) -> None:
        source = (ROOT / "src" / "schema-org.lisp").read_text(encoding="utf-8")
        mapping = re.findall(r'\("([a-z0-9-]+)"\s+\.\s+\("[A-Za-z]+"\)\)', source)
        self.assertEqual(len(mapping), 49)
        self.assertEqual(len(set(mapping)), 49)

    def test_wire_codec_nests_subtype_slots(self) -> None:
        codec = (ROOT / "src" / "json-v09.lisp").read_text(encoding="utf-8")
        self.assertIn('(jsown:val json-object "data")', codec)
        self.assertIn("document-envelope-slot-p", codec)
        self.assertIn("encode-source-v09", codec)
        self.assertIn("decode-document-v09", codec)

    def test_codec_is_typed_and_portable(self) -> None:
        codec = (ROOT / "src" / "json.lisp").read_text(encoding="utf-8")
        self.assertIn("codec-validation-error", codec)
        self.assertIn("nullable-type-p", codec)
        self.assertIn("encode-collection-value", codec)
        self.assertIn("decode-collection-value", codec)
        self.assertIn("closer-mop:class-slots", codec)
        self.assertNotIn("sb-mop:", codec)
        self.assertNotIn("(declare (ignore type-spec))", codec)

    def test_document_decode_uses_registered_classes(self) -> None:
        codec = (ROOT / "src" / "json-v09.lisp").read_text(encoding="utf-8")
        self.assertIn("*document-class-registry*", codec)
        self.assertIn("registered-document-class", codec)
        self.assertIn("unknown-document-dtype", codec)
        self.assertIn("document-class-mismatch", codec)
        self.assertNotIn("intern", codec.lower())

    def test_encode_contract_returns_jsown(self) -> None:
        codec = (ROOT / "src" / "json-v09.lisp").read_text(encoding="utf-8")
        tests = (ROOT / "t" / "json-test.lisp").read_text(encoding="utf-8")
        self.assertIn("Return a JSOWN object", codec)
        self.assertIn("encode-returns-jsown-object", tests)
        self.assertNotIn("encode-returns-string", tests)
        self.assertNotIn(":pretty", tests)

    def test_required_dtype_fields_are_normalized(self) -> None:
        codec = (ROOT / "src" / "json-v09.lisp").read_text(encoding="utf-8")
        self.assertIn("normalize-required-data-v09", codec)
        self.assertIn('data "subject"', codec)
        self.assertIn('data "object"', codec)
        self.assertIn('data "domain"', codec)
        self.assertIn('"address"', codec)
        self.assertIn('(jsown:val data "to")', codec)
        self.assertIn('(jsown:val data "headers")', codec)

    def test_hashing_is_sha256(self) -> None:
        source = (ROOT / "src" / "documents.lisp").read_text(encoding="utf-8")
        self.assertIn("*default-hash-algo* :sha256", source)
        self.assertNotIn("*default-hash-algo* :md5", source)


if __name__ == "__main__":
    unittest.main()
