#!/usr/bin/env python3
"""Two-axis witness (for the Claude-web claim "the engine has no framing-sensitive
classification layer at all").

Claim refined: the OBSERVER axis (dr_type orbit) is framing-blind (A1) — TRUE. But the
engine has a SECOND, separate classification surface — the COMMITTER / commitment-system
axis (cs_structure → cs_axiom_engine / cs_drift_engine), which the stakeholder migration
never touched and the 4c pilot (zero cs_structure) structurally could not exercise.

Demonstration: two constraints IDENTICAL on the observer axis (same metrics, same
beneficiary/victim, same scaffold perspectives) differing ONLY in committer structure
(one axiom's grounding + the drift_state). Expect: observer orbit BYTE-IDENTICAL across
both (orbit reads no cs_*), committer verdict cs_axiom_foreclosed DIFFERS. That is a
structure-sensitive classification layer the observer orbit is blind to → refutes
"no framing-sensitive classification layer at all."
"""
import json, pathlib, subprocess, sys
REPO = pathlib.Path("/home/scott/bin/structural_dynamics_model")
AUD = REPO / "audits/2026-06-07_stakeholder_layer_migration"
sys.path.insert(0, str(AUD))
from step4_harness import apply_scaffold, compile_to_temp  # noqa: E402

def story(cid, uid, grounding, drift_dir, drift_mag):
    return {
        "header": {"constraint_id": cid, "version": "1.0", "generated_date": "2026-06-07",
                   "status": "ACTIVE", "story_uid": uid},
        # OBSERVER AXIS — identical across both
        "base_properties": {"extractiveness": 0.55, "suppression": 0.45, "theater_ratio": 0.4,
                            "claimed_type": "tangled_rope", "human_readable": "two-axis demo",
                            "topic_domain": "test", "requires_active_enforcement": True,
                            "beneficiaries": ["incumbent_institution"], "victims": ["bound_party"]},
        "interval": {"start": 0, "end": 10},
        # COMMITTER AXIS — the ONLY difference between base and variant
        "cs_structure": {
            "kernel_codification": "formalized",
            "authority_grounding": "lineage",
            "axioms": [{"atom": "core_premise", "role": "foundational", "status": "holdable",
                        "grounding_type": grounding}],
            "reference_frame": "founding_frame",
            "drift_state": {"moment": "contemporary", "direction": drift_dir,
                            "magnitude": drift_mag, "acknowledged": False},
        },
    }

# base: deontological + practice_drift  -> NOT foreclosed
# variant: empirically_contingent + axiom_overriding/substantial -> foreclosed
base = story("twoaxis_base", "aaaaaaaa-0000-4000-8000-000000000001",
             "deontological", "practice_drift", "substantial")
variant = story("twoaxis_variant", "bbbbbbbb-0000-4000-8000-000000000002",
                "empirically_contingent", "axiom_overriding", "substantial")

MEAS = (":- [stack].\n:- corpus_loader:ensure_corpus_loaded.\n:- consult('%s').\n"
        "m(C,U) :- constraint_indexing:site_contexts_canonical(C4), "
        "forall(member(X,C4),((drl_core:dr_type(C,X,T)->true;T=na),format(\"ORB ~q~n\",[T]))), "
        "(setof(A, cs_axiom_engine:cs_axiom_foreclosed(U,A), Fs) -> true ; Fs=[]), "
        "format(\"FORECLOSED ~q~n\",[Fs]).\n")

def measure(s):
    pl = compile_to_temp(apply_scaffold(s), "twoaxis")
    cid = s["header"]["constraint_id"]; uid = s["header"]["story_uid"]
    pr = AUD / f".tmp_twoaxis_{cid}.pl"
    pr.write_text(MEAS % pl)
    r = subprocess.run(["swipl", "-g", f"consult('{pr}'), m({cid}, '{uid}'), halt", "-t", "halt(1)"],
                       cwd=str(REPO / "prolog"), capture_output=True, text=True)
    pl.unlink(missing_ok=True); pr.unlink(missing_ok=True)
    orb = [l[4:].strip() for l in r.stdout.splitlines() if l.startswith("ORB ")]
    fc = next((l[11:].strip() for l in r.stdout.splitlines() if l.startswith("FORECLOSED ")), None)
    if not orb or fc is None:
        sys.stderr.write("PROBE FAIL; stderr tail:\n" + "\n".join(r.stderr.splitlines()[-10:]) + "\n")
    return orb, fc

ob, fb = measure(base)
ov, fv = measure(variant)
print("OBSERVER AXIS (dr_type orbit, canonical-4):")
print(f"  base    {ob}")
print(f"  variant {ov}")
print(f"  identical: {ob == ov and bool(ob)}  <- observer orbit blind to committer structure (expected)")
print("\nCOMMITTER AXIS (cs_axiom_foreclosed — computed verdict from cs_structure):")
print(f"  base    {fb}  (deontological + practice_drift)")
print(f"  variant {fv}  (empirically_contingent + axiom_overriding)")
print(f"  differs: {fb != fv and fb is not None}  <- committer axis IS structure-sensitive classification")
ok = (ob == ov and bool(ob)) and (fb != fv) and fb is not None
print(f"\nVERDICT: {'two-axis separation witnessed — observer-blind, committer-sensitive' if ok else 'INCONCLUSIVE (see stderr)'}")
sys.exit(0 if ok else 1)
