#!/usr/bin/env python3
"""Scaffold-leak witness (OQ-83 step 4, operator-strengthened, TWO axes).

Claim: the constant scaffold is inert for the stakeholder arm's measurement.
A1 proved the engine ignores authored perspectives, but A1 ran before this
session's scaffold/adapter code existed — so re-prove it WITH the scaffold
present, on BOTH the type axis and the tuple axis.

Variants (same stakeholder story, perspectives stripped, scaffold re-injected):
  A  constant scaffold (type rope, tuple canonical/powerless)
  B  scaffold TYPE varied  (rope->snare)              expect measurement == A
  C  scaffold TUPLE varied (different P,T,E,S)         expect measurement == A
  PC positive control: scaffold A BUT a REAL stakeholder tuple perturbed
     (publisher_consortium exit mobile->trapped)       expect measurement != A

Measurement (what the stakeholder arm actually reads) = canonical-4 dr_type +
per-stakeholder dr_type_for_stakeholder + in_contention pairs. No stderr
suppression (step-3 trap).
"""
import copy
import json
import pathlib
import subprocess
import sys

REPO = pathlib.Path("/home/scott/bin/structural_dynamics_model")
AUD = REPO / "audits/2026-06-07_stakeholder_layer_migration"
sys.path.insert(0, str(AUD))
from step4_harness import apply_scaffold, compile_to_temp, CONSTANT_SCAFFOLD  # noqa: E402

# Stakeholder-arm substrate: the step-3 hand-authored contention story, perspectives stripped.
base = json.loads((AUD / "phase_a_contention_demo.json").read_text())
base.pop("perspectives", None)
CID = base["header"]["constraint_id"]

MEASURE = r"""
measure(C) :-
    constraint_indexing:site_contexts_canonical(C4),
    forall(member(Ctx, C4),
           ( (drl_core:dr_type(C, Ctx, T) -> true ; T = no_type),
             format("ORBIT ~q ~q~n", [Ctx, T]) )),
    forall(narrative_ontology:constraint_stakeholder(C, N, _, _, _, _, _),
           ( stakeholder_seats:dr_type_for_stakeholder(C, N, ST),
             stakeholder_seats:derive_directionality_for_stakeholder(C, N, SD),
             format("SEAT ~q d=~q type=~q~n", [N, SD, ST]) )),
    findall(A-B, stakeholder_seats:in_contention(C, A, B), Ps),
    format("CONTENTION ~q~n", [Ps]).
"""

def signature(story, tag):
    pl = compile_to_temp(story, tag)
    probe = AUD / f".tmp_measure_{tag}.pl"
    probe.write_text(":- [stack].\n:- corpus_loader:ensure_corpus_loaded.\n"
                     f":- consult('{pl}').\n" + MEASURE)
    r = subprocess.run(
        ["swipl", "-g", f"consult('{probe}'), measure({CID}), halt", "-t", "halt(1)"],
        cwd=str(REPO / "prolog"), capture_output=True, text=True)
    pl.unlink(missing_ok=True)
    probe.unlink(missing_ok=True)
    lines = [l for l in r.stdout.splitlines()
             if l.startswith(("ORBIT", "SEAT", "CONTENTION"))]
    if not lines:
        # surface the error channel (no swallowing) — empty measurement is a probe failure
        sys.stderr.write(f"[{tag}] EMPTY measurement; stderr tail:\n")
        sys.stderr.write("\n".join(r.stderr.splitlines()[-8:]) + "\n")
    return "\n".join(lines)

# A — constant scaffold
sigA = signature(apply_scaffold(base), "leakA")
# B — scaffold type varied
scaf_b = copy.deepcopy(CONSTANT_SCAFFOLD)
for p in scaf_b:
    p["classification_type"] = "snare"
sigB = signature(apply_scaffold(base, scaf_b), "leakB")
# C — scaffold tuple varied
scaf_c = copy.deepcopy(CONSTANT_SCAFFOLD)
scaf_c[0]["agent_power"] = "institutional"; scaf_c[0]["exit_options"] = "arbitrage"
scaf_c[1]["agent_power"] = "moderate"; scaf_c[1]["spatial_scope"] = "national"
sigC = signature(apply_scaffold(base, scaf_c), "leakC")
# PC — positive control: real stakeholder tuple perturbed
pc = copy.deepcopy(base)
for s in pc["stakeholders"]:
    if s["name"] == "publisher_consortium":
        s["exit_options"] = "trapped"
sigPC = signature(apply_scaffold(pc), "leakPC")

print("=== scaffold-leak witness ===")
print(f"A == B (type axis inert):  {sigA == sigB}")
print(f"A == C (tuple axis inert): {sigA == sigC}")
print(f"A != PC (probe sees real-tuple movement): {sigA != sigPC}")
print()
print("--- signature A (canonical):")
print(sigA)
if sigA != sigPC:
    print("\n--- positive control PC delta (real publisher_consortium tuple moved):")
    a, p = sigA.splitlines(), sigPC.splitlines()
    for x, y in zip(a, p):
        if x != y:
            print(f"  A : {x}\n  PC: {y}")
ok = (sigA == sigB) and (sigA == sigC) and (sigA != sigPC) and bool(sigA)
print(f"\nVERDICT: {'PASS — scaffold inert on both axes, probe live' if ok else 'FAIL'}")
sys.exit(0 if ok else 1)
