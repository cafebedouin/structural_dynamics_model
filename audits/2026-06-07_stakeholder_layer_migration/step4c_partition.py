#!/usr/bin/env python3
"""4c — per-topic PARTITION (survived / flipped / unevaluable-by-resolution). Not a verdict.

Discipline (operator-pinned, OQ-83):
 - evaluability BIN-BLIND: (a) same primary object [recorded judgment] + (b) same
   (HasBeneficiaries, HasVictims) profile [substrate diff]; computed/recorded before bins.
 - ε-pinned is the believed figure; raw is the contaminated upper bound on movement.
 - per-flip scrutiny: a flip with large residual supp/theater delta is suspect (noted), since
   uncontrolled metric drift inflates flips toward Type-B.
 - structure pass: does bin membership track ε / signature / type.
No stderr suppression. Temp .pl removed. No json/ or live testsets/ writes.
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

TOPICS = json.loads((AUD / "step4_topics_pinned.json").read_text())["topics"]

MEAS = r"""
meas(C) :-
    config:param(extractiveness_metric_name, EN), config:param(suppression_metric_name, SN),
    (narrative_ontology:constraint_metric(C, EN, Eps) -> true ; Eps = na),
    (narrative_ontology:constraint_metric(C, SN, Sup) -> true ; Sup = na),
    (narrative_ontology:constraint_metric(C, theater_ratio, Th) -> true ; Th = na),
    (narrative_ontology:constraint_beneficiary(C, _) -> HB = true ; HB = false),
    (narrative_ontology:constraint_victim(C, _) -> HV = true ; HV = false),
    format("MET eps=~w sup=~w th=~w hasB=~w hasV=~w~n", [Eps, Sup, Th, HB, HV]),
    constraint_indexing:site_contexts_canonical(C4),
    forall(member(Ctx, C4), ((drl_core:dr_type(C, Ctx, T) -> true ; T = no_type),
        format("ORB ~q~n", [T]))),
    (signature_detection:constraint_signature(C, Sig) -> true ; Sig = none),
    format("SIG ~w~n", [Sig]),
    (drl_core:dr_type(C, TA) -> true ; TA = no_type), format("ANALYTIC ~w~n", [TA]).
"""

def measure(story, tag, eps_override=None):
    s = apply_scaffold(story)
    cid = s["header"]["constraint_id"]
    if eps_override is not None:
        s = copy.deepcopy(s)
        s["base_properties"]["extractiveness"] = eps_override
    pl = compile_to_temp(s, tag)
    probe = AUD / f".tmp_4c_{tag}_{cid}.pl"
    probe.write_text(":- [stack].\n:- corpus_loader:ensure_corpus_loaded.\n"
                     f":- consult('{pl}').\n" + MEAS)
    r = subprocess.run(["swipl", "-g", f"consult('{probe}'), meas({cid}), halt", "-t", "halt(1)"],
                       cwd=str(REPO / "prolog"), capture_output=True, text=True)
    pl.unlink(missing_ok=True); probe.unlink(missing_ok=True)
    out = {"orbit": [], "met": None, "sig": None, "analytic": None}
    for l in r.stdout.splitlines():
        if l.startswith("ORB "): out["orbit"].append(l[4:].strip())
        elif l.startswith("MET "): out["met"] = l[4:].strip()
        elif l.startswith("SIG "): out["sig"] = l[4:].strip()
        elif l.startswith("ANALYTIC "): out["analytic"] = l[9:].strip()
    if not out["orbit"]:
        sys.stderr.write(f"[{tag} {cid}] empty; stderr tail:\n" + "\n".join(r.stderr.splitlines()[-8:]) + "\n")
    return out

def parse_met(m):
    d = {}
    for kv in m.split():
        k, v = kv.split("=")
        d[k] = v
    return d

rows = []
for t in TOPICS:
    cid = t["id"]
    st = json.loads((AUD / f"{cid}.stakeholder.json").read_text()); st.pop("perspectives", None)
    ft_path = AUD / f"{cid}.fourtuple.json"
    if not ft_path.exists():
        print(f"!! {cid}: four-tuple arm missing — gen not complete?"); continue
    ft = json.loads(ft_path.read_text())
    s_m = measure(st, "stake")
    f_m = measure(ft, "four")
    sd, fd = parse_met(s_m["met"]), parse_met(f_m["met"])
    # (b) substrate diff — bin-blind: (HasBenef,HasVic) profile equality
    b_match = (sd["hasB"], sd["hasV"]) == (fd["hasB"], fd["hasV"])
    # (a) same object — recorded judgment (human_readable both arms), bin-blind
    s_hr = st["base_properties"].get("human_readable", "")
    f_hr = ft["base_properties"].get("human_readable", "")
    rows.append({"cid": cid, "label": t["label"],
                 "a_stake_hr": s_hr, "a_four_hr": f_hr,
                 "b_stake_profile": (sd["hasB"], sd["hasV"]),
                 "b_four_profile": (fd["hasB"], fd["hasV"]),
                 "b_match": b_match,
                 "eps_stake": sd["eps"], "eps_four": fd["eps"],
                 "sup_stake": sd["sup"], "sup_four": fd["sup"],
                 "th_stake": sd["th"], "th_four": fd["th"],
                 "orbit_stake": s_m["orbit"], "orbit_four": f_m["orbit"],
                 "sig_stake": s_m["sig"], "sig_four": f_m["sig"],
                 "analytic_stake": s_m["analytic"], "analytic_four": f_m["analytic"],
                 "_st": st, "_ft": ft})

# ---- BIN-BLIND evaluability first (printed before any bin) ----
print("=" * 78)
print("EVALUABILITY (bin-blind: computed from (a) same-object + (b) HasB/HasV profile only)")
print("=" * 78)
for r in rows:
    a_ok = "?"  # recorded for human ruling; printed for review, not auto-failed unless clearly different object
    print(f"\n{r['cid']} [{r['label']}]")
    print(f"  (a) object  stake='{r['a_stake_hr'][:55]}'  four='{r['a_four_hr'][:55]}'")
    print(f"  (b) profile stake={r['b_stake_profile']} four={r['b_four_profile']}  match={r['b_match']}")

# ---- bins (only after evaluability shown) ----
print("\n" + "=" * 78)
print("BINS (ε-pinned primary; raw = contaminated upper bound on movement)")
print("=" * 78)
for r in rows:
    # ε-pin: recompute stakeholder orbit at the four-tuple arm's ε (common ε)
    eps_common = float(r["eps_four"])
    st_pinned = measure(r["_st"], "stakepin", eps_override=eps_common)
    raw_same = r["orbit_stake"] == r["orbit_four"]
    pin_same = st_pinned["orbit"] == r["orbit_four"]
    supdelta = abs(float(r["sup_stake"]) - float(r["sup_four"]))
    thdelta = abs(float(r["th_stake"]) - float(r["th_four"]))
    r["raw_same"] = raw_same; r["pin_same"] = pin_same
    r["orbit_stake_pinned"] = st_pinned["orbit"]
    r["supdelta"] = supdelta; r["thdelta"] = thdelta
    print(f"\n{r['cid']} [{r['label']}] eval(b)={r['b_match']}")
    print(f"  eps stake={r['eps_stake']} four={r['eps_four']}  supΔ={supdelta:.2f} thΔ={thdelta:.2f}")
    print(f"  orbit stake     {r['orbit_stake']}")
    print(f"  orbit stake-pin {st_pinned['orbit']}  (at ε={eps_common})")
    print(f"  orbit four      {r['orbit_four']}")
    print(f"  sig stake={r['sig_stake']} four={r['sig_four']} | analytic stake={r['analytic_stake']} four={r['analytic_four']}")
    bin_ = ("UNEVALUABLE(b-profile-mismatch)" if not r["b_match"]
            else "SURVIVED" if pin_same else "FLIPPED")
    note = ""
    if bin_ == "FLIPPED" and (supdelta > 0.15 or thdelta > 0.15):
        note = f"  <-- per-flip scrutiny: large residual metric drift (supΔ={supdelta:.2f} thΔ={thdelta:.2f}); flip SUSPECT"
    print(f"  BIN(ε-pinned): {bin_}{note}")
    r["bin"] = bin_

json.dump([{k: v for k, v in r.items() if not k.startswith("_")} for r in rows],
          open(AUD / "step4c_partition.json", "w"), indent=2)
print("\nsaved step4c_partition.json")
