#!/usr/bin/env python3
"""OQ-344 wire fix — the two-sided control, run through the REAL process_batch_results.

Not a copy of the logic: this drives `agent.generate_kernel_corpus.process_batch_results`
itself with an Anthropic-result-shaped stub (exactly the shim the no_scope drivers use), so
the code under test is the shipped code on its shipped path.

FIRES  on a story authoring `role: "victim"`   -> counter increments, values list records it,
                                                  repair_stats.json is written.
DECLINES on the same story authoring `role: "payer"` -> counter absent, and (because
                                                  repair_stats is then empty) NO file at all.

The stories are deliberately invalid after repair, so they are rejected before the .pl write
path — nothing is written into the live prolog/testsets/.
"""
import json, sys, tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO))
from agent.generate_kernel_corpus import process_batch_results   # noqa: E402


class _Block:
    def __init__(self, text): self.type, self.text = "text", text


class _Msg:
    def __init__(self, text):
        self.content, self.usage, self.model = [_Block(text)], None, "control/stub"


class _Inner:
    def __init__(self, text): self.type, self.message = "succeeded", _Msg(text)


class _Res:
    def __init__(self, cid, text): self.custom_id, self.result = cid, _Inner(text)


class _Batches:
    def __init__(self, items): self._items = items
    def results(self, _bid): return iter(self._items)


class _Messages:
    def __init__(self, items): self.batches = _Batches(items)


class _StubClient:
    def __init__(self, items): self.messages = _Messages(items)


def story_with_role(role):
    """Minimal JSON that parses, carries a stakeholders[] with `role`, and stays INVALID
    after repair (no base_properties) so it never reaches the .pl writer."""
    return json.dumps({
        "header": {"constraint_id": "oq344_wire_control"},
        "stakeholders": [{"name": "affected_group", "role": role, "situation": "bears costs"}],
    })


def run(role, tag):
    with tempfile.TemporaryDirectory() as td:
        td = Path(td)
        (td / "json").mkdir(); (td / "pl").mkdir()
        rej = td / "rejections.json"
        s, f, _, _ = process_batch_results(
            _StubClient([_Res("oq344_wire_control", story_with_role(role))]),
            "control-batch", td / "json", td / "pl", td / "ladder.log",
            rejections_path=rej, overwrite=True,
            provenance_source=f"oq344_control+{tag}")
        stats_path = rej.parent / "repair_stats.json"
        payload = json.loads(stats_path.read_text()) if stats_path.exists() else None
        return {"role": role, "succeeded": s, "failed": f,
                "repair_stats_json_exists": stats_path.exists(), "payload": payload}


def main():
    fires = run("victim", "fires")
    declines = run("payer", "declines")

    print("=== ARM 1 (FIRES): stakeholders[].role = 'victim' ===")
    print(json.dumps(fires, indent=2))
    print("\n=== ARM 2 (DECLINES): stakeholders[].role = 'payer' ===")
    print(json.dumps(declines, indent=2))

    tot = (fires["payload"] or {}).get("totals", {})
    checks = [
        ("fires: repair_stats.json written", fires["repair_stats_json_exists"] is True),
        ("fires: stakeholder_role_remapped == 1", tot.get("stakeholder_role_remapped") == 1),
        ("fires: values list records the field+value",
         tot.get("stakeholder_role_remapped_values") == ["role=victim"]),
        ("fires: run entry carries provenance_source",
         (fires["payload"] or {}).get("runs", [{}])[0].get("provenance_source")
         == "oq344_control+fires"),
        ("declines: no stakeholder_role_remapped key",
         "stakeholder_role_remapped" not in
         ((declines["payload"] or {}).get("totals", {}))),
        ("both arms rejected before the .pl writer (0 succeeded)",
         fires["succeeded"] == 0 and declines["succeeded"] == 0),
    ]
    print("\n=== CONTROL VERDICT ===")
    ok = True
    for name, passed in checks:
        print(f"  {'PASS' if passed else 'FAIL'}  {name}")
        ok &= passed
    print("\n" + ("CONTROL GREEN — the wire fires and declines two-sided."
                  if ok else "CONTROL RED"))
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
