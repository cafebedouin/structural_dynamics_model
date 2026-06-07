#!/usr/bin/env python3
"""OQ-85 decomposition audit — execution witnesses W1-W4 (read-only; audit-scoped).

W1 positive control: same-power beneficiary+payer story -> in_contention fires; silent on the
   streaming/hospital antagonist pairs (located absence, not probe-blindness).
W2 type corroboration: streaming+hospital compute an extractive dr_type, not the claimed rope.
W3 recovery-probe: structured beneficiary/payer split for the two antagonist pairs (expect none).
W4 anchor test + blind-spot positive control: powerless structured payer per story; a
   co-equals-extract-from-each-other story with NO powerless anchor.

Compiles to temp testsets, loads, measures, removes temp. stderr surfaced (no swallowing).
"""
import copy
import json
import pathlib
import subprocess
import sys

REPO = pathlib.Path("/home/scott/bin/structural_dynamics_model")
AUD = REPO / "audits/2026-06-07_stakeholder_layer_migration"
sys.path.insert(0, str(AUD))
from step4_harness import apply_scaffold, compile_to_temp  # noqa: E402


def run(cid, pl_path, goal_body):
    probe = AUD / f".tmp_oq85_{cid}.pl"
    probe.write_text(":- [stack].\n:- corpus_loader:ensure_corpus_loaded.\n"
                     f":- consult('{pl_path}').\n"
                     f"go :- {goal_body}.\n")
    r = subprocess.run(["swipl", "-g", f"consult('{probe}'), go, halt", "-t", "halt(1)"],
                       cwd=str(REPO / "prolog"), capture_output=True, text=True)
    probe.unlink(missing_ok=True)
    out = [l for l in r.stdout.splitlines() if l.startswith(("W", "  "))]
    if not out:
        sys.stderr.write(f"[{cid}] empty; stderr tail:\n" + "\n".join(r.stderr.splitlines()[-10:]) + "\n")
    return "\n".join(out)


def base_story(cid, hr, stakeholders, eps=0.55, supp=0.45):
    return {
        "header": {"constraint_id": cid, "version": "1.0", "generated_date": "2026-06-07", "status": "ACTIVE"},
        "base_properties": {"extractiveness": eps, "suppression": supp, "theater_ratio": 0.4,
                            "claimed_type": "tangled_rope", "human_readable": hr,
                            "topic_domain": "test", "requires_active_enforcement": True},
        "stakeholders": stakeholders,
        "six_questions": {"disappearance_verdict": "world_rearranges",
                          "disappearance_rationale": "t", "founding_problem": "t",
                          "founding_problem_status": "live", "founding_problem_corroboration": "external t"},
        "interval": {"start": 0, "end": 10},
    }


def sk(name, role, power, exit="mobile"):
    return {"name": name, "role": role, "power": power, "time_horizon": "biographical",
            "exit_options": exit, "spatial_scope": "national",
            "situation": "test stakeholder"}


def measure_block(cid):
    return (f"format('W in_contention ~w: ', [{cid}]), "
            f"(findall(A-B, stakeholder_seats:in_contention({cid}, A, B), Ps), format('~q~n', [Ps])), "
            f"forall(narrative_ontology:constraint_stakeholder({cid}, N, R, P, _, _, _), "
            f"(stakeholder_seats:derive_directionality_for_stakeholder({cid}, N, D), "
            f"format('  SEAT ~w role=~w power=~w d=~q~n', [N, R, P, D]))), "
            f"(drl_core:dr_type({cid}, T) -> true ; T = no_type), format('  COMPUTED_TYPE ~w~n', [T])")


print("=" * 70)
print("W1 — positive control: same-power beneficiary+payer => in_contention fires")
print("=" * 70)
w1 = base_story("oq85_pc_clean", "PC clean asymmetric pair",
                [sk("acquirer_institution", "beneficiary", "institutional"),
                 sk("supplier_institution", "payer", "institutional"),
                 sk("retail_customers", "payer", "powerless")])
pl = compile_to_temp(apply_scaffold(w1), "oq85pc")
print(run("oq85_pc_clean", pl, measure_block("oq85_pc_clean")))
pl.unlink(missing_ok=True)

print("\n" + "=" * 70)
print("W2 + W3 + W4-anchor — the two failing stories")
print("=" * 70)
for cid in ["pilot_streaming_royalty_split", "pilot_hospital_insurer_reimbursement"]:
    story = json.loads((AUD / f"{cid}.stakeholder.json").read_text())
    story.pop("perspectives", None)
    pl = compile_to_temp(apply_scaffold(story), "oq85fail")
    # W3 recovery probe: any structured beneficiary AND victim among the institutional pair?
    body = (measure_block(cid) + ", "
            f"findall(B, narrative_ontology:constraint_beneficiary({cid}, B), Bens), "
            f"findall(V, narrative_ontology:constraint_victim({cid}, V), Vics), "
            f"format('  STRUCT_BENEFICIARIES ~q~n', [Bens]), "
            f"format('  STRUCT_VICTIMS ~q~n', [Vics])")
    print(f"\n--- {cid}")
    print(run(cid, pl, body))
    pl.unlink(missing_ok=True)

print("\n" + "=" * 70)
print("W4 blind-spot positive control: two co-equals extract from each other, NO powerless anchor")
print("=" * 70)
# A pays B and B pays A across the relation; both institutional; NO powerless payer at all.
w4 = base_story("oq85_noanchor", "co-equals extract from each other, no powerless seat",
                [sk("trading_house_a", "beneficiary", "institutional"),
                 sk("trading_house_b", "payer", "institutional"),
                 sk("market_oversight", "observer", "institutional")])
pl = compile_to_temp(apply_scaffold(w4), "oq85na")
body = (measure_block("oq85_noanchor") + ", "
        f"findall(V-PV, (narrative_ontology:constraint_victim(oq85_noanchor, V), "
        f"narrative_ontology:constraint_stakeholder(oq85_noanchor, V, _, PV, _, _, _)), Vics), "
        f"format('  VICTIMS_WITH_POWER ~q~n', [Vics])")
print(run("oq85_noanchor", pl, body))
pl.unlink(missing_ok=True)
print("\n(no temp testsets should remain:)")
