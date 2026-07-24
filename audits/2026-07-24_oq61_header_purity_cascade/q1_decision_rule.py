"""OQ-61 Q1 pre-registered decision rule (denominators fixed before the run):
'no residual signal beyond type composition' IFF
  (a) for every type, |severe-in-type/drifting-in-type - severe-overall/n_drifting| <= 15pp, AND
  (b) off-diagonal severe mass (severe in pristine|sound-expected types: rope,mountain)
      / n_drifting < 5%.
If either fails -> a residual exists -> ESCALATE to operator (revisiting the Q1
ruling is operator territory). If both hold, the header change is measurement-backed.

Evaluated on the drifting stratum via severity_by_type (drifting-in-type,
severe-in-type) + network counts. PRISTINE_SOUND_TYPES = the types whose
reporting-convention expected band is pristine|sound."""
import sys, json

PRISTINE_SOUND_TYPES = {"rope", "mountain"}
PP_TOL = 15.0    # percentage points, rule (a)
OFFDIAG_TOL = 5.0  # percent, rule (b)


def evaluate(path):
    d = json.load(open(path))
    diag = d["diagnostic"]
    sbt = diag.get("severity_by_type", {})
    n_drift = diag.get("network_n_drifting", 0)
    n_sev = diag.get("network_n_severe", 0)
    print(f"\n### {path.split('/')[-1]}  (n_drifting={n_drift}, n_severe={n_sev}, "
          f"token={diag.get('network_stability')})")
    if n_drift == 0:
        print("  n_drifting == 0 — decision rule N/A (nothing drifting).")
        return None
    overall = n_sev / n_drift * 100
    print(f"  severe-overall / n_drifting = {n_sev}/{n_drift} = {overall:.1f}%")
    # rule (a)
    a_fail = []
    print("  per-type severe/drifting (rule a, tol 15pp):")
    for t in sorted(sbt):
        r = sbt[t]
        dt = r.get("drifting", 0); st = r.get("severe", 0)
        if dt == 0:
            continue
        frac = st / dt * 100
        delta = abs(frac - overall)
        flag = "  <-- FAIL" if delta > PP_TOL else ""
        if delta > PP_TOL:
            a_fail.append((t, delta))
        print(f"    {t:14} {st:4}/{dt:<4} = {frac:5.1f}%  |Δ|={delta:5.1f}pp{flag}")
    # rule (b)
    offdiag_sev = sum(sbt.get(t, {}).get("severe", 0) for t in PRISTINE_SOUND_TYPES)
    offdiag_pct = offdiag_sev / n_drift * 100
    b_fail = offdiag_pct >= OFFDIAG_TOL
    print(f"  off-diagonal severe mass (rule b, tol 5%): "
          f"{offdiag_sev}/{n_drift} = {offdiag_pct:.1f}%"
          f"{'  <-- FAIL' if b_fail else '  ok'}")
    verdict = not a_fail and not b_fail
    print(f"  => {'BOTH HOLD — measurement-backed' if verdict else 'RESIDUAL — ESCALATE to operator'}")
    return dict(measurement_backed=verdict, a_fail=a_fail, b_fail=b_fail,
                overall=overall, offdiag_pct=offdiag_pct)


if __name__ == "__main__":
    out = {}
    for p in sys.argv[1:]:
        out[p] = evaluate(p)
    json.dump(out, open("outputs/oq61_q1_decision_rule.json", "w"), indent=2, default=str)
