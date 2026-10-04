import json
import os
from pathlib import Path
import subprocess
import unittest
from test_v0101_runtime import document, MANIFEST

ROOT = Path(__file__).resolve().parents[1]
class PublicApiTests(unittest.TestCase):
    def test_fresh_canonical_only_root_families(self):
        values=[document(t['name'].split('/')[-1]) for t in MANIFEST['types']
                if t['kind']=='document' and t['persistence']=='persistent']
        values += [f['document'] for f in json.loads((ROOT/'schemas/starintel-0.10.1/supported-workflow-fixtures.json').read_text())]
        values.append({'id':'person:scalars','dataset':'test','dtype':'person','schemaVersion':'0.10.1',
                       'deleted':False,'extensions':{'opaque_key':None,'flag':False}})
        result=subprocess.run([os.environ.get('STARINTEL_SBCL','sbcl'),'--script',str(ROOT/'tests/root-family-smoke.lisp')],
                              input=json.dumps(values),text=True,capture_output=True)
        self.assertEqual(result.returncode,0,result.stderr[-6000:])
        # ASDF may print compilation notes on first load; JSON is the final line.
        self.assertEqual(json.loads(result.stdout.splitlines()[-1]),values)

    def test_fresh_explicit_legacy_opt_in(self):
        result=subprocess.run([os.environ.get('STARINTEL_SBCL','sbcl'),'--script',str(ROOT/'tests/legacy-api-smoke.lisp')],text=True,capture_output=True)
        self.assertEqual(result.returncode,0,result.stderr[-6000:])
        self.assertIn('Fresh explicit legacy opt-in',result.stdout)
