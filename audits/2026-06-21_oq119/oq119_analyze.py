#!/usr/bin/env python3
"""OQ-119 three-axis fed-vs-withheld analysis (post-spend).

Reads the 6 exported draw join-record files (oq119_{arm}_d{n}), and for each kernel
computes the per-axis floor F_A (withheld-vs-withheld pairwise) and effect D_A
(fed-vs-withheld pairwise), then applies the FROZEN rule from PREDICTION.md:
  axis moved  iff  median(D_A) > max(F_A)   [scalar axes]
  categorical iff  majority of D pairs differ AND withheld pairs agree (max F = 0)
Observer + temporal-rate-magnitude are SOFT (reported, cannot headline); committer
obstruction, temporal SIGN flips, and verdict grade/alert are HIGH-information.

Blind to PREDICTION.md's sub-predictions at compute time — applies the rule, reports.
"""
import json
import statistics
import sys
from itertools import product, combinations
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
AUDIT = REPO / "audits/2026-06-21_oq119"
GATE0 = REPO / "audits/2026-06-21_oq119_gate0"
DRAWS = [1, 2, 3]
SEATS = ["powerless", "moderate", "institutional", "analytical"]
METRICS = ["base_extractiveness", "suppression_requirement", "theater_ratio"]


def load_draws():
    """Returns {arm: {draw: {constraint_id: record}}}."""
    out = {}
    for arm in ["withheld", "fed"]:
        out[arm] = {}
        for d in DRAWS:
            p = AUDIT / f"join_{arm}_d{d}.json"
            out[arm][d] = json.loads(p.read_text()) if p.exists() else {}
    return out


def kernels_in(records):
    ks = {}
    for cid, rec in records.items():
        k = rec["axiom"]["kernel"]
        ks.setdefault(k, []).append(cid)
    return ks


# ---- per-axis distances between two draws (each draw = {cid: record}) ----
def observer_dist(dA, dB, cids):
    tot = 0.0
    for c in cids:
        if c not in dA or c not in dB:
            continue
        oa, ob = dA[c]["observer"], dB[c]["observer"]
        for s in SEATS:
            va, vb = oa.get(s), ob.get(s)
            if isinstance(va, (int, float)) and isinstance(vb, (int, float)):
                tot += abs(va - vb)
    return round(tot, 6)


def temporal_rate_dist(dA, dB, cids):
    tot = 0.0
    for c in cids:
        if c not in dA or c not in dB:
            continue
        ta, tb = dA[c]["temporal"], dB[c]["temporal"]
        for m in METRICS:
            ra = ta.get(m, {}).get("mean_rate", 0.0)
            rb = tb.get(m, {}).get("mean_rate", 0.0)
            tot += abs((ra or 0.0) - (rb or 0.0))
    return round(tot, 6)


def temporal_signflips(dA, dB, cids):
    n = 0
    for c in cids:
        if c not in dA or c not in dB:
            continue
        ta, tb = dA[c]["temporal"], dB[c]["temporal"]
        for m in METRICS:
            if ta.get(m, {}).get("slope_sign") != tb.get(m, {}).get("slope_sign"):
                n += 1
    return n


def committer_dist(dA, dB, cids):
    """Kernel-level: obstruction_status change (1/0) + divergence-scope set symdiff.
    Both draws share the kernel; read from any of its readings (kernel-level facts)."""
    def kfacts(d):
        for c in cids:
            if c in d:
                ax = d[c]["axiom"]
                return ax["obstruction_status"], frozenset(ax.get("divergence_scopes", []))
        return None, frozenset()
    sa, scA = kfacts(dA)
    sb, scB = kfacts(dB)
    return (1 if sa != sb else 0) + len(scA ^ scB), (sa, sb)


def verdict_dist(dA, dB, cids):
    """Per-reading verdict grade/alert changes summed (high-info)."""
    tot = 0
    for c in cids:
        if c not in dA or c not in dB:
            continue
        a, b = dA[c]["axiom"], dB[c]["axiom"]
        tot += (a["verdict_joined"] != b["verdict_joined"])
        tot += (a["sig_grade"] != b["sig_grade"])
        tot += abs((a["n_alerts"] or 0) - (b["n_alerts"] or 0))
    return tot


def pairwise(within, distfn, cids):
    """within = {draw: records}. Returns list of distances over draw-pairs."""
    return [distfn(within[i], within[j], cids) for i, j in combinations(DRAWS, 2)]


def cross(fed, wh, distfn, cids):
    return [distfn(fed[i], wh[j], cids) for i, j in product(DRAWS, DRAWS)]


def scalar_verdict(F, D):
    """Frozen: moved iff median(D) > max(F)."""
    if not F or not D:
        return None
    return statistics.median(D) > max(F)


def main():
    data = load_draws()
    # union of kernels across all draws
    allk = {}
    for arm in data:
        for d in data[arm]:
            for k, cids in kernels_in(data[arm][d]).items():
                allk.setdefault(k, set()).update(cids)

    lines = []
    headline_moves = {"committer": 0, "temporal_sign": 0, "verdict": 0}
    soft_moves = {"observer": 0, "temporal_rate": 0}
    n_kernels = 0
    for k in sorted(allk):
        wh = data["withheld"]
        fed = data["fed"]
        # Restrict to FULL-COVERAGE readings: present in all 6 draws (fair pairwise distances).
        cids = sorted(c for c in allk[k]
                      if all(c in data[a][d] for a in ["withheld", "fed"] for d in DRAWS))
        if len(cids) < 2:
            lines.append(f"### {k}\n  INCOMPLETE — <2 full-coverage readings; skipped.\n")
            continue
        n_kernels += 1
        rec = {"kernel": k, "axes": {}}
        # scalar axes
        for name, fn, high in [("observer", observer_dist, False),
                               ("temporal_rate", temporal_rate_dist, False),
                               ("verdict", verdict_dist, True)]:
            F = pairwise(wh, fn, cids)
            D = cross(fed, wh, fn, cids)
            mv = scalar_verdict(F, D)
            rec["axes"][name] = {"F": F, "maxF": max(F) if F else None,
                                 "medD": round(statistics.median(D), 4) if D else None, "moved": mv}
            if mv:
                (headline_moves if high else soft_moves).setdefault(name, 0)
                if high:
                    headline_moves[name] = headline_moves.get(name, 0) + 1
                else:
                    soft_moves[name] = soft_moves.get(name, 0) + 1
        # temporal sign flips (high-info categorical-ish: count)
        Fs = pairwise(wh, temporal_signflips, cids)
        Ds = cross(fed, wh, temporal_signflips, cids)
        sign_moved = (statistics.median(Ds) > max(Fs)) if (Fs and Ds) else None
        rec["axes"]["temporal_sign"] = {"F": Fs, "maxF": max(Fs), "medD": statistics.median(Ds), "moved": sign_moved}
        if sign_moved:
            headline_moves["temporal_sign"] += 1
        # committer (categorical): majority of D pairs differ AND withheld agree
        Fc = [committer_dist(wh[i], wh[j], cids)[0] for i, j in combinations(DRAWS, 2)]
        Dc = [committer_dist(fed[i], wh[j], cids)[0] for i, j in product(DRAWS, DRAWS)]
        comm_moved = (sum(1 for x in Dc if x > 0) > len(Dc) / 2) and (max(Fc) == 0) if (Fc and Dc) else None
        rec["axes"]["committer"] = {"F": Fc, "maxF": max(Fc) if Fc else None,
                                    "D_nonzero": sum(1 for x in Dc if x > 0), "D_n": len(Dc), "moved": comm_moved}
        if comm_moved:
            headline_moves["committer"] += 1

        lines.append(f"### {k}  ({len(cids)} readings)")
        for ax, v in rec["axes"].items():
            tag = "HIGH" if ax in ("committer", "temporal_sign", "verdict") else "soft"
            lines.append(f"  [{tag}] {ax}: moved={v['moved']}  {({kk: vv for kk, vv in v.items() if kk != 'moved'})}")
        lines.append("")

    # headline
    any_high = sum(headline_moves.values())
    verdict = "JOIN MOVES" if any_high > 0 else "JOIN INVARIANT (on informative axes)"
    soft_only = (any_high == 0) and (sum(soft_moves.values()) > 0)
    summary = [
        "# OQ-119 RESULTS — does feeding move the join?\n",
        f"Kernels analyzed (all 6 draws present): {n_kernels}",
        f"HIGH-information axis moves: {headline_moves}",
        f"SOFT axis moves (cannot headline): {soft_moves}",
        f"\n## HEADLINE: {verdict}",
    ]
    if soft_only:
        summary.append("(SOFT-ONLY: feeding nudges ε/observer but does not move the cross-examination.)")
    if headline_moves["committer"] > 0:
        summary.append("** committer obstruction_status MOVED under feeding — Theorem-7 "
                       "detection-independence VIOLATION, top-line finding. **")
    summary.append("\n## Per-kernel per-axis\n")
    out = "\n".join(summary + lines)
    (AUDIT / "RESULTS.md").write_text(out)
    print(out)


if __name__ == "__main__":
    main()
