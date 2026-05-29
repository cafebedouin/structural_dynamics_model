"""Verify the inject→generate→pop→write keystone path in save_story_tagged.

Tests two silent-failure modes:
  Check 1: pop-before-read — if pop runs before generate_pl, cs_kernel_id/2 is absent from .pl
  Check 2: skip-of-pop    — if pop doesn't run, _kernel_id pollutes the JSON

Note on scope:
  This script exercises save-path (half 2 of the B1+B2 keystone) by execution.
  Orchestrator extraction (entry.get('kernel_id') → story_dict['_kernel_id'], half 1)
  was verified by Phase 1 static audit (Trail 1+2: key at top level, entry in scope),
  not by execution — labeled acceptance, not silent gap.
"""

import json, sys, tempfile, pathlib

REPO = pathlib.Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO))
from agent.story_generator_base import save_story_tagged  # noqa: E402

story_path = REPO / "json" / "sanctity_reading.json"
story = json.loads(story_path.read_text())
cid = story["header"]["constraint_id"]   # "sanctity_reading"
test_kernel = "end_of_life_decision_authority"

with tempfile.TemporaryDirectory() as tmp:
    jdir = pathlib.Path(tmp) / "json"
    pdir = pathlib.Path(tmp) / "pl"
    jdir.mkdir(); pdir.mkdir()

    story["_kernel_id"] = test_kernel       # simulate orchestrator injection (B1)
    save_story_tagged(story, jdir, pdir, overwrite=True)

    pl_text = (pdir / f"{cid}.pl").read_text()
    j_data  = json.loads((jdir / f"{cid}.json").read_text())

    # Check 1: cs_kernel_id/2 fact is present in the .pl file
    # Emit format: narrative_ontology:cs_kernel_id({cid}, {kernel_id}).
    # Substring check tolerates module qualifier and trailing period.
    expected_pl = f"cs_kernel_id({cid}, {test_kernel})"
    assert expected_pl in pl_text, (
        f"FAIL (pop-before-read): '{expected_pl}' not in .pl\n"
        f"  → pop ran before generate_pl, so cs_kernel_id/2 was never emitted"
    )
    print(f"Check 1 PASS: '{expected_pl}' found in .pl")

    # Check 2: _kernel_id is NOT present in the written JSON
    assert "_kernel_id" not in j_data, (
        "FAIL (skip-of-pop): '_kernel_id' present in JSON\n"
        "  → pop did not run before json.dumps in save_story_tagged"
    )
    print("Check 2 PASS: '_kernel_id' absent from JSON")

print("PASS: inject→generate→pop→write sequence correct")
