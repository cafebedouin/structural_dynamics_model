#!/usr/bin/env python3
"""OQ-188 fire-rate census (pre-registered: PREREG.md Blocks 1-2, commit 57159a36).

Over the live outputs/pipeline_output.json: for each of the four seats, bucket the
serialized perspective_chi d as matched (== a stakeholder_role_d_* constant, tol 1e-6),
unmatched (non-null, matches none), canonical (d == 0.0), or null. For matched seats,
apply the zero-free-parameter flip predicate: sign(f(d)) != sign(f(d_nearest_alt)).
Everything (root, role ladder, sigmoid) comes from the SERIALIZED config section.
"""
import json
import math
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PIPELINE = ROOT / "outputs/pipeline_output.json"

TOL = 1e-6
SEATS = ("powerless", "moderate", "institutional", "analytical")


def sigmoid_f(d, cfg):
    L, U = cfg["sigmoid_lower"], cfg["sigmoid_upper"]
    d0, k = cfg["sigmoid_midpoint"], cfg["sigmoid_steepness"]
    return L + (U - L) / (1 + math.exp(-k * (d - d0)))


def main():
    data = json.loads(PIPELINE.read_text(encoding="utf-8"))
    man = data.get("manifest", {})
    cfg = data["config"]
    roles = {k.replace("stakeholder_role_d_", ""): v
             for k, v in cfg.items() if k.startswith("stakeholder_role_d_")}
    L, U, d0, k = (cfg["sigmoid_lower"], cfg["sigmoid_upper"],
                   cfg["sigmoid_midpoint"], cfg["sigmoid_steepness"])
    root = d0 - math.log((U - L) / (-L) - 1) / k

    print(f"manifest: run_at={man.get('pipeline_run_at')} n={man.get('n_constraints')} "
          f"commit={man.get('code_commit_short')} dirty={man.get('code_dirty')}")
    print(f"serialized roles: {roles}")
    print(f"f(d) sign root from serialized params: {root:.5f}")
    for r, d in sorted(roles.items(), key=lambda kv: kv[1]):
        print(f"  f({r}={d}) = {sigmoid_f(d, cfg):+.6f}")

    per = data["per_constraint"]  # a LIST, not a dict
    print(f"\nper_constraint entries: {len(per)}")

    for seat in SEATS:
        buckets = {"matched": [], "unmatched": [], "canonical": [], "null": []}
        fired = []
        unmatched_vals = {}
        for e in per:
            pc = (e.get("perspective_chi") or {}).get(seat) or {}
            d = pc.get("d")
            if d is None:
                buckets["null"].append(e["id"])
                continue
            if abs(d - 0.0) <= TOL:
                buckets["canonical"].append(e["id"])
                continue
            match = next((r for r, rd in roles.items() if abs(d - rd) <= TOL), None)
            if match is None:
                buckets["unmatched"].append(e["id"])
                unmatched_vals[round(d, 6)] = unmatched_vals.get(round(d, 6), 0) + 1
                continue
            buckets["matched"].append(e["id"])
            alt_role, alt_d = min(((r, rd) for r, rd in roles.items() if r != match),
                                  key=lambda kv: abs(kv[1] - d))
            flips = (sigmoid_f(d, cfg) > 0) != (sigmoid_f(alt_d, cfg) > 0)
            if flips:
                fired.append((e["id"], match, alt_role))
        n_matched = len(buckets["matched"])
        rate = (len(fired) / n_matched * 100) if n_matched else float("nan")
        print(f"\n[{seat}] matched={n_matched} unmatched={len(buckets['unmatched'])} "
              f"canonical={len(buckets['canonical'])} null={len(buckets['null'])}")
        if unmatched_vals:
            print(f"  unmatched d values: {dict(sorted(unmatched_vals.items()))}")
        print(f"  flip-predicate fires: {len(fired)}/{n_matched} matched "
              f"({rate:.1f}% of matched)")
        if seat == "institutional":
            role_counts = {}
            for _, m, a in fired:
                role_counts[(m, a)] = role_counts.get((m, a), 0) + 1
            print(f"  fired (role -> nearest-alt) counts: {role_counts}")
            print(f"  sample fired ids: {[f[0] for f in fired[:5]]}")
            if n_matched == 0:
                print("  GATE: no matched institutional seats — census inconclusive")
            elif rate >= 50:
                print("  GATE: >=50% -> STANDING type-level form (legend sentence + glyph)")
            elif rate <= 25:
                print("  GATE: <=25% -> per-constraint conditional flag")
            else:
                print("  GATE: 25-50% -> ESCALATE to operator (blocked_on_human)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
