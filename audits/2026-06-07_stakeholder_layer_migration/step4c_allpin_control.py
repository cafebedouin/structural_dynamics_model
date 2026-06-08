#!/usr/bin/env python3
"""Per-flip positive control: pin ALL THREE metrics (ε, supp, theater); do the two flips
collapse to identical orbits? If yes, the flip was metric-drift, not framing residual.
Empty orbit = probe failure (surface stderr), NOT a pass."""
import json, pathlib, copy, subprocess, sys
REPO = pathlib.Path("/home/scott/bin/structural_dynamics_model")
A = REPO / "audits/2026-06-07_stakeholder_layer_migration"
sys.path.insert(0, str(A))
from step4_harness import apply_scaffold, compile_to_temp

MEAS = (":- [stack].\n:- corpus_loader:ensure_corpus_loaded.\n:- consult('%s').\n"
        "m(C) :- constraint_indexing:site_contexts_canonical(C4), "
        "forall(member(X,C4),((drl_core:dr_type(C,X,T)->true;T=na),format(\"ORB ~q~n\",[T]))).\n")

def orbit(story, tag, metrics=None):
    s = copy.deepcopy(apply_scaffold(story))
    if metrics:
        s["base_properties"].update(metrics)
    pl = compile_to_temp(s, tag); cid = s["header"]["constraint_id"]
    pr = A / f".tmp_allpin_{tag}.pl"
    pr.write_text(MEAS % pl)
    r = subprocess.run(["swipl", "-g", f"consult('{pr}'), m({cid}), halt", "-t", "halt(1)"],
                       cwd=str(REPO / "prolog"), capture_output=True, text=True)
    pl.unlink(missing_ok=True); pr.unlink(missing_ok=True)
    orb = [l[4:].strip() for l in r.stdout.splitlines() if l.startswith("ORB ")]
    if not orb:
        sys.stderr.write(f"[{tag} {cid}] EMPTY orbit; stderr tail:\n"
                         + "\n".join(r.stderr.splitlines()[-8:]) + "\n")
    return orb

for cid in ["pilot_app_store_commission", "pilot_streaming_royalty_split"]:
    st = json.loads((A / f"{cid}.stakeholder.json").read_text()); st.pop("perspectives", None)
    ft = json.loads((A / f"{cid}.fourtuple.json").read_text())
    fbp = ft["base_properties"]
    pin = {"extractiveness": fbp["extractiveness"], "suppression": fbp["suppression"],
           "theater_ratio": fbp["theater_ratio"]}
    o_ft = orbit(ft, "ft")
    o_st = orbit(st, "stall", pin)
    ok = bool(o_ft) and bool(o_st)
    print(f"TOPIC {cid}")
    print(f"  four-tuple orbit          {o_ft}")
    print(f"  stake @all-3-metrics-pin  {o_st}")
    if not ok:
        print("  PROBE FAILED (empty) — result void, not a pass")
    else:
        print(f"  identical: {o_ft == o_st}  -> "
              + ("flip = 100% metric-drift, zero framing residual" if o_ft == o_st
                 else "framing residual REMAINS after pinning all metrics"))
