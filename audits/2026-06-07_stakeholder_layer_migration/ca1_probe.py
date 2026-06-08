#!/usr/bin/env python3
"""CA-1 — committer field-partition CROSS-CHECK (synthetic; not a fresh architectural finding).

Hold committer CONTENT fixed (axiom grounding + drift), vary in-engine FRAMING knobs
(reference_frame; story_uid), recompute the committer verdicts. Expected: verdicts invariant to
framing (partition confirmed by a 2nd method — perturbation, vs the static read). The interesting
outcome is a SURPRISE (a verdict moves on a framing knob = a mis-binned field). Content-move
positive control proves the probe is live. (Reading prose is emitted as comments, read by no
verdict — trivially invariant, noted not run.)
"""
import copy, json, pathlib, subprocess, sys
REPO = pathlib.Path("/home/scott/bin/structural_dynamics_model")
AUD = REPO / "audits/2026-06-07_stakeholder_layer_migration"
sys.path.insert(0, str(AUD))
from step4_harness import apply_scaffold, compile_to_temp  # noqa: E402

def story(cid, uid, ref_frame, grounding):
    return {
        "header": {"constraint_id": cid, "version": "1.0", "generated_date": "2026-06-07",
                   "status": "ACTIVE", "story_uid": uid},
        "base_properties": {"extractiveness": 0.55, "suppression": 0.45, "theater_ratio": 0.4,
                            "claimed_type": "tangled_rope", "human_readable": "CA-1 demo",
                            "topic_domain": "test", "requires_active_enforcement": True,
                            "beneficiaries": ["inst"], "victims": ["bound"]},
        "interval": {"start": 0, "end": 10},
        "cs_structure": {
            "kernel_codification": "formalized", "authority_grounding": "lineage",
            "axioms": [{"atom": "core_premise", "role": "foundational", "status": "holdable",
                        "grounding_type": grounding}],
            "reference_frame": ref_frame,
            "drift_state": {"moment": "contemporary", "direction": "axiom_overriding",
                            "magnitude": "substantial", "acknowledged": False}},
    }

MEAS = (":- [stack].\n:- corpus_loader:ensure_corpus_loaded.\n:- consult('%s').\n"
        "v(U) :- (cs_axiom_engine:cs_axiom_foreclosed(U,A)->true;A=none), "
        "(cs_drift_engine:cs_drift_trajectory(U,_,T)->true;T=none), "
        "(cs_drift_mismatch:cs_drift_mismatch(U,M)->true;M=none), "
        "format(\"VERDICT foreclosed=~w drift_terminal=~w mismatch=~w~n\",[A,T,M]).\n")

def verdict(s):
    pl = compile_to_temp(apply_scaffold(s), "ca1")
    cid=s["header"]["constraint_id"]; uid=s["header"]["story_uid"]
    pr=AUD/f".tmp_ca1_{cid}.pl"; pr.write_text(MEAS % pl)
    r=subprocess.run(["swipl","-g",f"consult('{pr}'), v('{uid}'), halt","-t","halt(1)"],
                     cwd=str(REPO/"prolog"), capture_output=True, text=True)
    pl.unlink(missing_ok=True); pr.unlink(missing_ok=True)
    line=next((l for l in r.stdout.splitlines() if l.startswith("VERDICT")), None)
    if line is None:
        sys.stderr.write("PROBE FAIL; stderr tail:\n"+"\n".join(r.stderr.splitlines()[-8:])+"\n")
    return line

base = verdict(story("ca1_base","uid-aaaa-0000-4000-8000-000000000001","frame_alpha","empirically_contingent"))
fr   = verdict(story("ca1_refframe","uid-aaaa-0000-4000-8000-000000000001","frame_BETA","empirically_contingent"))
uidv = verdict(story("ca1_uid","uid-bbbb-0000-4000-8000-000000000002","frame_alpha","empirically_contingent"))
ctl  = verdict(story("ca1_content","uid-aaaa-0000-4000-8000-000000000001","frame_alpha","deontological"))

print("CA-1 committer field-partition cross-check (synthetic):")
print(f"  base                  {base}")
print(f"  framing: reference_frame frame_alpha->frame_BETA   {fr}")
print(f"  framing: story_uid (relabel)                       {uidv}")
print(f"  CONTENT control: grounding empcont->deontological  {ctl}")
inv = (base==fr) and (base==uidv) and bool(base)
ctl_moved = (ctl!=base) and ctl is not None
print(f"\n  framing-invariant (base==refframe==uid): {inv}")
print(f"  content-control moved (probe live):      {ctl_moved}")
if inv and ctl_moved:
    print("  VERDICT: partition confirmed by 2nd method (perturbation) — no mis-binned field; committer verdicts framing-invariant, content-sensitive. (NOT a fresh architectural finding — the read established this; this is the cross-check.)")
elif not inv:
    print("  VERDICT: SURPRISE — a committer verdict MOVED on a framing knob = MIS-BINNED FIELD. STOP, report.")
else:
    print("  VERDICT: INCONCLUSIVE (content control did not move — probe not live; see stderr).")
sys.exit(0 if (inv and ctl_moved) else 1)
