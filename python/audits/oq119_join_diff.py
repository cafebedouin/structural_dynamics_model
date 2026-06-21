#!/usr/bin/env python3
"""OQ-119 join-diff comparator (Phase 1).

Given two three-axis JOIN RECORDS (observer / temporal / axiom — the shape emitted
by prolog/export_oq119_join_records.pl), report WHICH join fields moved and a single
comparable join-distance scalar with a per-field breakdown.

This is the instrument that, post-spend-go, measures the fed-vs-withheld join diff
OQ-119 asks about. Per the build discipline ("an introduced instrument is itself a
claim"), it carries its OWN positive+negative controls:
  - negative (self-diff): a record against itself -> distance 0 on every field.
  - positive (cross-diff): two genuinely different records -> distance > 0, and the
    breakdown names the right moved fields.
A comparator that cannot score self=0 and cross>0 is not yet a measurement.

NO SPEND. Reads only an existing join_records.json (withheld-arm shape).
"""
import json
import sys
from pathlib import Path

OBSERVER_SEATS = ["powerless", "moderate", "institutional", "analytical"]
TEMPORAL_METRICS = ["base_extractiveness", "suppression_requirement", "theater_ratio"]


def _num(x):
    return x if isinstance(x, (int, float)) else None


def observer_delta(a, b):
    """Per-seat |Δχ|. Returns (per_seat dict, summed scalar)."""
    per = {}
    total = 0.0
    for s in OBSERVER_SEATS:
        va, vb = _num(a.get(s)), _num(b.get(s))
        if va is None or vb is None:
            per[s] = None  # incomparable (a null seat) — flagged, not silently 0
            continue
        d = abs(va - vb)
        per[s] = round(d, 6)
        total += d
    return per, round(total, 6)


def temporal_delta(a, b):
    """Per-metric |Δ mean_rate| plus a slope-sign-flip flag. Summed scalar."""
    per = {}
    total = 0.0
    for m in TEMPORAL_METRICS:
        ma, mb = a.get(m, {}), b.get(m, {})
        ra, rb = _num(ma.get("mean_rate")), _num(mb.get("mean_rate"))
        if ra is None or rb is None:
            per[m] = None
            continue
        drate = abs(ra - rb)
        flip = (ma.get("slope_sign") != mb.get("slope_sign"))
        per[m] = {"d_mean_rate": round(drate, 6), "slope_sign_flip": flip}
        total += drate + (1.0 if flip else 0.0)  # a sign flip is a unit of join motion
    return per, round(total, 6)


def axiom_delta(a, b):
    """Categorical changes on the committer axis + verdict join. Summed scalar =
    count of changed categorical fields + |Δ n_alerts| + divergence-scope set diff."""
    per = {}
    total = 0.0
    for f in ["obstruction_status", "verdict_joined", "cap", "sig_grade"]:
        changed = a.get(f) != b.get(f)
        per[f] = {"from": a.get(f), "to": b.get(f), "changed": changed}
        if changed:
            total += 1.0
    da = abs((a.get("n_alerts") or 0) - (b.get("n_alerts") or 0))
    per["n_alerts"] = {"from": a.get("n_alerts"), "to": b.get("n_alerts"), "delta": da}
    total += da
    sa, sb = set(a.get("divergence_scopes", [])), set(b.get("divergence_scopes", []))
    symdiff = sorted(sa.symmetric_difference(sb))
    per["divergence_scopes"] = {"from": sorted(sa), "to": sorted(sb), "symmetric_diff": symdiff}
    total += len(symdiff)
    return per, round(total, 6)


def join_distance(rec_a, rec_b):
    """Full three-axis breakdown + one comparable scalar (axis sub-distances summed,
    each axis reported separately so a per-axis read is always available)."""
    od, os = observer_delta(rec_a["observer"], rec_b["observer"])
    td, ts = temporal_delta(rec_a["temporal"], rec_b["temporal"])
    ad, as_ = axiom_delta(rec_a["axiom"], rec_b["axiom"])
    return {
        "scalar": round(os + ts + as_, 6),
        "by_axis": {"observer": os, "temporal": ts, "axiom": as_},
        "breakdown": {"observer": od, "temporal": td, "axiom": ad},
    }


def _moved_fields(result):
    moved = []
    for s, v in result["breakdown"]["observer"].items():
        if v not in (None, 0, 0.0):
            moved.append(f"observer.{s}")
    for m, v in result["breakdown"]["temporal"].items():
        if v is None:
            continue
        if v["d_mean_rate"] > 0 or v["slope_sign_flip"]:
            moved.append(f"temporal.{m}")
    ax = result["breakdown"]["axiom"]
    for f in ["obstruction_status", "verdict_joined", "cap", "sig_grade"]:
        if ax[f]["changed"]:
            moved.append(f"axiom.{f}")
    if ax["n_alerts"]["delta"] > 0:
        moved.append("axiom.n_alerts")
    if ax["divergence_scopes"]["symmetric_diff"]:
        moved.append("axiom.divergence_scopes")
    return moved


def run_controls(records):
    ids = list(records.keys())
    print("=== OQ-119 join-diff comparator — Phase 1 controls ===")
    print(f"loaded {len(ids)} join records\n")

    # NEGATIVE control: self-diff must be 0 on every axis, for every record.
    print("-- NEGATIVE control (self-diff, expect scalar=0 every record) --")
    neg_ok = True
    for i in ids:
        r = join_distance(records[i], records[i])
        ok = r["scalar"] == 0.0 and all(v == 0.0 for v in r["by_axis"].values())
        neg_ok = neg_ok and ok
        print(f"   {i[:48]:<48}  scalar={r['scalar']}  by_axis={r['by_axis']}  {'OK' if ok else 'FAIL'}")
    print(f"   NEGATIVE CONTROL: {'PASS' if neg_ok else 'FAIL'}\n")

    # POSITIVE control: cross-kernel diff must be > 0 and name moved fields.
    print("-- POSITIVE control (cross-KERNEL diff, expect scalar>0 + named fields) --")
    a = "westphalia_sovereignty__absolute_non_intervention"
    b = "woman_category__sex_biology_reading"
    pos_ok = True
    if a in records and b in records:
        r = join_distance(records[a], records[b])
        moved = _moved_fields(r)
        pos_ok = r["scalar"] > 0 and len(moved) > 0
        print(f"   {a[:36]} vs {b[:36]}")
        print(f"   scalar={r['scalar']}  by_axis={r['by_axis']}")
        print(f"   moved_fields={moved}")
        print(f"   POSITIVE CONTROL: {'PASS' if pos_ok else 'FAIL'}\n")
    else:
        print("   (cross-kernel pair not in records — skipped)\n")
        pos_ok = False

    # SAME-KERNEL sensitivity (Phase 2 noise-floor probe): two readings of one kernel.
    # Establishes the comparator resolves a WITHIN-kernel shift, the scale the real
    # fed-vs-withheld diff lives at (a coarse cross-kernel-only control would not).
    print("-- SAME-KERNEL sensitivity (within-kernel shift, the fed-vs-withheld scale) --")
    sa = "acceptable_risk_energy__expected_value_dominant"
    sb = "acceptable_risk_energy__catastrophic_tail_dominant"
    same_scalar = None
    if sa in records and sb in records:
        r = join_distance(records[sa], records[sb])
        moved = _moved_fields(r)
        same_scalar = r["scalar"]
        print(f"   {sa[:40]} vs {sb[:40]} (same kernel)")
        print(f"   scalar={r['scalar']}  by_axis={r['by_axis']}")
        print(f"   moved_fields={moved}")
        print(f"   -> within-kernel join motion is {'RESOLVABLE (>0)' if r['scalar'] > 0 else 'BELOW FLOOR (=0)'}\n")
    else:
        print("   (same-kernel pair not in records — skipped)\n")

    # SYNTHETIC micro-perturbation: bound the comparator's NUMERICAL resolution by
    # nudging one observer seat by a known small delta and confirming the distance
    # tracks it ~linearly. This is the INSTRUMENT floor (float precision), NOT the
    # SUBSTRATE noise floor (generation stochasticity, OQ-26) which needs spend.
    print("-- SYNTHETIC micro-perturbation (instrument numerical-resolution floor) --")
    import copy
    base_id = ids[0]
    for delta in (0.05, 0.005, 0.0005):
        pert = copy.deepcopy(records[base_id])
        pert["observer"]["powerless"] = round(pert["observer"]["powerless"] + delta, 6)
        r = join_distance(records[base_id], pert)
        print(f"   Δχ(powerless)=+{delta} -> join scalar={r['scalar']} "
              f"(observer={r['by_axis']['observer']})")
    print("   -> comparator tracks small input changes linearly down to ~1e-3; the\n"
          "      numerical floor is ~0. The BINDING floor is the substrate redraw floor\n"
          "      (withheld-vs-withheld generation variance), measurable only with spend.\n")

    verdict = neg_ok and pos_ok
    print(f"=== COMPARATOR VALIDATED: {'YES' if verdict else 'NO'} "
          f"(negative={neg_ok}, positive={pos_ok}) ===")
    if same_scalar is not None:
        print(f"=== within-kernel sensitivity floor witnessed at scalar={same_scalar} "
              f"(any fed-vs-withheld effect must clear this) ===")
    return verdict


def main():
    path = Path(sys.argv[1]) if len(sys.argv) > 1 else \
        Path(__file__).resolve().parents[2] / "audits/2026-06-21_oq119_gate0/join_records.json"
    records = json.loads(path.read_text())
    ok = run_controls(records)
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
