#!/usr/bin/env python3
"""4b — opposition-authoring GATE (OQ-83 step 4).

Generate the stakeholder arm (Gemini, pilot prompt) for the PINNED topics, compile
with constant scaffold, load via stakeholder_seats, witness in_contention BOTH halves:
emerges in contention-labeled, absent in non-contention-labeled.

GATE: opposition absent in contention topics => renamed-not-escaped (caller stops; no
prompt-tuning). No stderr suppression.
"""
import json
import pathlib
import subprocess
import sys

REPO = pathlib.Path("/home/scott/bin/structural_dynamics_model")
AUD = REPO / "audits/2026-06-07_stakeholder_layer_migration"
sys.path.insert(0, str(AUD))
from step4_harness import (apply_scaffold, compile_to_temp,  # noqa: E402
                           generate_stakeholder_arm, PINNED_GEMINI_MODEL)

TOPICS = json.loads((AUD / "step4_topics_pinned.json").read_text())["topics"]

MEASURE = r"""
gate_measure(C) :-
    findall(N-R-P, narrative_ontology:constraint_stakeholder(C, N, R, P, _, _, _), Seats),
    format("SEATS ~q~n", [Seats]),
    findall(A-B, stakeholder_seats:in_contention(C, A, B), Ps),
    format("CONTENTION ~q~n", [Ps]).
"""

def measure(cid, pl_path):
    probe = AUD / f".tmp_gate_{cid}.pl"
    probe.write_text(":- [stack].\n:- corpus_loader:ensure_corpus_loaded.\n"
                     f":- consult('{pl_path}').\n" + MEASURE)
    r = subprocess.run(
        ["swipl", "-g", f"consult('{probe}'), gate_measure({cid}), halt", "-t", "halt(1)"],
        cwd=str(REPO / "prolog"), capture_output=True, text=True)
    probe.unlink(missing_ok=True)
    seats = next((l for l in r.stdout.splitlines() if l.startswith("SEATS")), "SEATS (none)")
    cont = next((l for l in r.stdout.splitlines() if l.startswith("CONTENTION")), None)
    if cont is None:
        sys.stderr.write(f"[{cid}] no CONTENTION line; stderr tail:\n"
                         + "\n".join(r.stderr.splitlines()[-8:]) + "\n")
        cont = "CONTENTION (probe-error)"
    return seats, cont

print(f"model pinned: {PINNED_GEMINI_MODEL}\n")
results = []
for t in TOPICS:
    cid = t["id"]
    print(f"--- {cid} [{t['label']}] generating ...", flush=True)
    story = generate_stakeholder_arm(t["topic"], cid)
    (AUD / f"{cid}.stakeholder.json").write_text(json.dumps(story, indent=2))
    scaffolded = apply_scaffold(story)
    pl = compile_to_temp(scaffolded, "gate")
    seats, cont = measure(cid, pl)
    pl.unlink(missing_ok=True)
    has_contention = "CONTENTION []" not in cont and "probe-error" not in cont
    results.append((cid, t["label"], has_contention, seats, cont))
    print(f"    {seats}")
    print(f"    {cont}  -> in_contention={'YES' if has_contention else 'no'}")

print("\n=== 4b GATE TABLE ===")
for cid, label, hc, _, _ in results:
    print(f"  {label:22} {cid:38} in_contention={'YES' if hc else 'no '}")

contention = [r for r in results if r[1] == "contention"]
noncont = [r for r in results if r[1] != "contention"]
emerges = all(r[2] for r in contention)
absent = all(not r[2] for r in noncont)
print(f"\nemerges in ALL contention topics: {emerges}")
print(f"absent  in ALL non_contention topics: {absent}")
if not emerges:
    print("GATE: FAIL -> RENAMED-NOT-ESCAPED (schema escaped; generator re-imposed). STOP.")
elif not absent:
    print("GATE: CONTAMINATED -> opposition in non-contention topics (prompt may manufacture). STOP, report.")
else:
    print("GATE: PASS -> proceed to 4c.")
