#!/usr/bin/env python3
"""OQ-186 epsilon-clause discrimination census (pre-registered: PREREG.md Block 4
subordinate gate, commit 57159a36).

Over all live corpus-derived agent edges (edge_type in {shared_beneficiary,
shared_victim}) in outputs/pipeline_output.json: partition edge pairs into
both-sides (beneficiary overlap >=1 AND victim overlap >=1) vs non-both-sides,
and report the |d-eps| <= 0.02 rate in each. Gate: if a MAJORITY of
non-both-sides pairs also satisfy |d-eps| <= 0.02, the eps clause does not
discriminate -> drop it, key on both-sides overlap alone.
"""
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PIPELINE = ROOT / "outputs/pipeline_output.json"

AGENT_EDGES = ("shared_beneficiary", "shared_victim")
EPS_MARGIN = 0.02


def main():
    data = json.loads(PIPELINE.read_text(encoding="utf-8"))
    man = data.get("manifest", {})
    print(f"manifest: run_at={man.get('pipeline_run_at')} n={man.get('n_constraints')} "
          f"commit={man.get('code_commit_short')} dirty={man.get('code_dirty')}")

    per = data["per_constraint"]  # a LIST
    index = {e["id"]: e for e in per}

    pairs = set()
    dangling = 0
    for e in per:
        cn = e.get("contamination_network") or {}
        for n in cn.get("neighbors") or []:
            if n.get("edge_type") not in AGENT_EDGES:
                continue
            nid = n.get("constraint_id")
            if nid not in index:
                dangling += 1
                continue
            pairs.add(tuple(sorted((e["id"], nid))))

    both_sides, non_both = [], []
    null_eps = 0
    for a, b in sorted(pairs):
        ea, eb = index[a], index[b]
        bens_a, bens_b = set(ea.get("beneficiaries") or []), set(eb.get("beneficiaries") or [])
        vics_a, vics_b = set(ea.get("victims") or []), set(eb.get("victims") or [])
        is_both = bool(bens_a & bens_b) and bool(vics_a & vics_b)
        eps_a, eps_b = ea.get("base_extractiveness"), eb.get("base_extractiveness")
        if eps_a is None or eps_b is None:
            null_eps += 1
            close = None
        else:
            close = abs(eps_a - eps_b) <= EPS_MARGIN
        (both_sides if is_both else non_both).append((a, b, close))

    print(f"\nunique agent-edge pairs: {len(pairs)} (dangling neighbor refs skipped: {dangling}; "
          f"pairs with null eps on either side: {null_eps})")

    for label, group in (("both-sides", both_sides), ("non-both-sides", non_both)):
        n = len(group)
        known = [g for g in group if g[2] is not None]
        close = sum(1 for g in known if g[2])
        rate = (close / len(known) * 100) if known else float("nan")
        print(f"[{label}] pairs={n} eps-known={len(known)} "
              f"|d-eps|<={EPS_MARGIN}: {close} ({rate:.1f}% of eps-known)")

    known_nb = [g for g in non_both if g[2] is not None]
    close_nb = sum(1 for g in known_nb if g[2])
    if not known_nb:
        print("\nGATE: no eps-known non-both-sides pairs — clause undecidable on live data; "
              "KEEP the clause (authored margin stands, decided by the A/B fixture semantics)")
    elif close_nb / len(known_nb) > 0.5:
        print("\nGATE: eps clause NON-discriminating (majority of non-both-sides pairs inside "
              "margin) -> DROP eps clause, key on both-sides overlap alone")
    else:
        print("\nGATE: eps clause discriminates -> KEEP |d-eps| <= 0.02 clause")
    return 0


if __name__ == "__main__":
    sys.exit(main())
