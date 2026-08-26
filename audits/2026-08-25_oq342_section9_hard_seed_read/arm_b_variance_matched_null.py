#!/usr/bin/env python3
"""SENSITIVITY, added at execution AFTER seeing the primary result and declared as such.

The pre-registered C2 null is NOISE-ASYMMETRIC with its own observed arm:
  observed = L1(target_FIXED_m, firstpass_RANDOM_m)   -- ONE random side
  null     = L1(firstpass_RANDOM_m, firstpass_RANDOM_m) -- TWO random sides
Two random sides carry more sampling noise than one, so the null distribution is wider and
T95 is inflated. That biases R downward, i.e. toward row 4 -- the outcome the pre-registration
calls "the valuable null". A criterion that is biased toward the author's convenient answer is
one to test, not to name and keep.

VARIANCE-MATCHED NULL: fix a first-pass subset A of size m (J different choices), draw B
randomly from first-pass \\ A. Now the null has exactly the observed arm's structure -- one
fixed side, one random side, disjoint populations -- so T95 is estimated on the same footing.
Verdict is recomputed under it. Same rng seed and K.
"""
import json, sys
from pathlib import Path
import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
from arm_b_hard_seed import (REPO, SEATS, VOCAB, K, SEED, strata_of, records,  # noqa: E402
                             seat_matrix, l1)

J = 50                      # fixed-A choices; K // J random B draws each -> K total
SPECS = [
    ("nemotron", "nemotron", "no_scope_rebuild_nemotron+rescue1", "no_scope_rebuild_nemotron"),
    ("stealth", "stealth", "no_scope_rebuild_stealth+rescue1", "no_scope_rebuild_stealth"),
    ("haiku(C1)", "haiku", "no_scope_rebuild_haiku+stakeholder_backfill", "no_scope_rebuild"),
    ("flash(C1)", "flash", "no_scope_rebuild_gemini+stakeholder_backfill", "no_scope_rebuild_gemini"),
]


def main():
    rng = np.random.default_rng(SEED)
    out = {"design": "variance-matched null (one fixed side, one random side)",
           "J_fixed_A": J, "K": K, "seed": SEED, "arms": {}}
    for name, suf, tgt_tag, fst_tag in SPECS:
        recs, _ = records(suf)
        st = strata_of(suf if suf.startswith("testsets") else f"testsets_{suf}")
        groups = {}
        for cid, s in st.items():
            groups.setdefault(s, []).append(cid)
        tgt, fst = sorted(groups[tgt_tag]), sorted(groups[fst_tag])
        m = min(len(tgt), len(fst) // 2)
        Mt, Mf = seat_matrix(recs, tgt), seat_matrix(recs, fst)

        observed, null = [], []
        for _ in range(K):
            ti = rng.choice(len(tgt), m, replace=False) if len(tgt) > m else np.arange(len(tgt))
            ri = rng.choice(len(fst), m, replace=False)
            observed.append(l1(Mt[ti], Mf[ri]))
        for _ in range(J):
            perm = rng.permutation(len(fst))
            A, rest = perm[:m], perm[m:]
            MA = Mf[A]
            for _ in range(K // J):
                B = rng.choice(rest, m, replace=False)
                null.append(l1(MA, Mf[B]))

        observed, null = np.array(observed), np.array(null)
        t95 = float(np.percentile(null, 95))
        out["arms"][name] = {
            "m": m, "observed_mean": float(observed.mean()), "observed_sd": float(observed.std()),
            "null_mean": float(null.mean()), "null_sd": float(null.std()), "T95": t95,
            "R": float(observed.mean() / t95), "exceedance_frac": float((observed > t95).mean()),
        }

    Rn, Rc1 = out["arms"]["nemotron"]["R"], out["arms"]["haiku(C1)"]["R"]
    out["verdict_under_variance_matched_null"] = (
        "row 2 regeneration_effect" if Rn > 1 and Rc1 >= 1 else
        "row 3 hard_seeds_differ" if Rn > 1 else "row 4 just_misauthored")
    print(json.dumps(out, indent=2))


if __name__ == "__main__":
    main()
