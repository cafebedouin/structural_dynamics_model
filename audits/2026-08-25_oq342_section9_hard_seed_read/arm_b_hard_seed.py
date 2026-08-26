#!/usr/bin/env python3
"""OQ-342 §9 Arm B — are a model's hard seeds structurally different stories?

Design frozen in PREREGISTRATION.md (md5 1989b5536578a4e7ce2503baacbfd4ad) BEFORE this ran.
Primary statistic: L1 between per-seat type-share vectors (4 seats x 8 types, zero-filled).
Controls C1 (regeneration-without-hardness, own null), C2 (size-matched permutation null),
C3 (seed-vs-model hypergeometric). Ratios R = observed mean L1 / that leg's own T95.
"""
import json, sys
from pathlib import Path
from collections import Counter
import numpy as np
from scipy.stats import hypergeom

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "python"))
from audits.leg_diagnostic_table import PROV_RE          # noqa: E402

SEATS = ["powerless", "moderate", "institutional", "analytical"]
# PRE-REGISTERED vocabulary: union across all strata compared, zero-filled.
VOCAB = ["mountain", "naturalized", "piton", "rope",
         "scaffold", "snare", "tangled_rope", "unknown"]
K = 1000
SEED = 20260825


def strata_of(leg):
    """id -> provenance stratum (field 5), from the leg's own .pl files."""
    out = {}
    for f in (REPO / "prolog" / leg).glob("*.pl"):
        m = PROV_RE.search(f.read_text(encoding="utf-8", errors="replace"))
        out[f.stem] = m.group(5) if m else "PROVENANCE_ABSENT"
    return out


def records(suffix):
    d = json.load(open(REPO / "outputs" / f"pipeline_output.{suffix}.json"))
    man = d["manifest"]
    return {r["id"]: r for r in d["per_constraint"]}, man


def seat_matrix(recs, ids):
    """(n, 4, 8) one-hot. Raises on an unexpected type or a missing seat -- absence
    must never be silently zero-filled into a share."""
    idx = {t: i for i, t in enumerate(VOCAB)}
    M = np.zeros((len(ids), len(SEATS), len(VOCAB)))
    for i, cid in enumerate(ids):
        p = recs[cid].get("perspectives") or {}
        for j, s in enumerate(SEATS):
            if s not in p:
                raise ValueError(f"{cid}: seat {s} absent -- vocabulary/seat premise broken")
            t = p[s]
            if t not in idx:
                raise ValueError(f"{cid}: seat type {t!r} outside pre-registered vocabulary")
            M[i, j, idx[t]] = 1.0
    return M


def l1(Ma, Mb):
    """L1 between two strata's per-seat type-share vectors. Range [0, 8]."""
    return float(np.abs(Ma.mean(axis=0) - Mb.mean(axis=0)).sum())


def arm(name, recs, target_ids, first_ids, rng):
    """C2 on one leg: observed mean L1 and the size-matched disjoint null's T95."""
    m = min(len(target_ids), len(first_ids) // 2)
    tgt = np.array(sorted(target_ids))
    fst = np.array(sorted(first_ids))
    Mt_full = seat_matrix(recs, tgt)
    Mf_full = seat_matrix(recs, fst)

    observed, null = [], []
    for _ in range(K):
        # observed: target(m) vs a fresh m-sample of first-pass
        ti = rng.choice(len(tgt), m, replace=False) if len(tgt) > m else np.arange(len(tgt))
        ri = rng.choice(len(fst), m, replace=False)
        observed.append(l1(Mt_full[ti], Mf_full[ri]))
        # null: two DISJOINT m-samples of first-pass
        perm = rng.permutation(len(fst))
        null.append(l1(Mf_full[perm[:m]], Mf_full[perm[m:2 * m]]))

    observed, null = np.array(observed), np.array(null)
    t95 = float(np.percentile(null, 95))
    return {
        "arm": name, "m": m, "n_target": len(tgt), "n_firstpass": len(fst),
        "subsampled_target": len(tgt) > m,
        "observed_mean": float(observed.mean()), "observed_sd": float(observed.std()),
        "null_mean": float(null.mean()), "null_sd": float(null.std()), "T95": t95,
        "R": float(observed.mean() / t95) if t95 > 0 else float("inf"),
        "exceedance_frac": float((observed > t95).mean()),
    }


def descriptives(recs, ids, label):
    """Secondary, descriptive only. Nulls never coerced (OQ-51, OQ-60)."""
    eps, h1, verdict, sig, pband, claimed = [], Counter(), Counter(), Counter(), Counter(), Counter()
    n_scored = 0
    for cid in ids:
        r = recs[cid]
        e = ((r.get("perspective_chi") or {}).get("powerless") or {}).get("epsilon")
        if isinstance(e, (int, float)):
            eps.append(float(e))
        h1.update(["null" if r.get("h1_band") is None else str(r["h1_band"])])
        verdict.update([(r.get("verdict_join") or {}).get("verdict", "ABSENT")])
        sig.update([r.get("signature") or "ABSENT"])
        pb = r.get("purity_band")
        pband.update(["null" if pb is None else str(pb)])
        if isinstance(r.get("purity_score"), (int, float)) and r.get("purity_score") >= 0.0:
            n_scored += 1
        claimed.update([r.get("claimed_type") or "ABSENT"])
    a = np.array(eps) if eps else np.array([])
    return {
        "label": label, "n": len(ids),
        "eps_n": int(a.size),
        "eps_mean": float(a.mean()) if a.size else None,
        "eps_sd": float(a.std()) if a.size else None,
        "eps_band3_share": float((a >= 0.46).mean()) if a.size else None,
        "h1_band": dict(h1), "h1_null_share": h1["null"] / len(ids),
        "verdict": dict(verdict), "signature_top5": dict(sig.most_common(5)),
        "purity_band": dict(pband), "purity_n_scored": n_scored, "purity_n_total": len(ids),
        "claimed_type": dict(claimed),
    }


def main():
    rng = np.random.default_rng(SEED)
    out = {"seed": SEED, "K": K, "vocab": VOCAB, "seats": SEATS,
           "arm_order": ["nemotron", "stealth", "haiku(C1)", "flash(C1)", "MDE_anchor"]}

    legs = {}
    for leg, suf in [("testsets_nemotron", "nemotron"), ("testsets_stealth", "stealth"),
                     ("testsets_haiku", "haiku"), ("testsets_flash", "flash")]:
        recs, man = records(suf)
        st = strata_of(leg)
        assert set(st) == set(recs), f"{leg}: .pl/json id set mismatch"
        groups = {}
        for cid, s in st.items():
            groups.setdefault(s, []).append(cid)
        legs[suf] = {"recs": recs, "manifest": man, "groups": groups}
        out.setdefault("manifests", {})[suf] = {
            k: man.get(k) for k in ("code_commit_short", "code_dirty", "n_stories", "pipeline_run_at")}
        out.setdefault("strata", {})[suf] = {k: len(v) for k, v in sorted(groups.items())}

    # ---- primary + corroboration + C1 ----
    specs = [
        ("nemotron", "nemotron", "no_scope_rebuild_nemotron+rescue1", "no_scope_rebuild_nemotron"),
        ("stealth", "stealth", "no_scope_rebuild_stealth+rescue1", "no_scope_rebuild_stealth"),
        ("haiku(C1)", "haiku", "no_scope_rebuild_haiku+stakeholder_backfill", "no_scope_rebuild"),
        ("flash(C1)", "flash", "no_scope_rebuild_gemini+stakeholder_backfill", "no_scope_rebuild_gemini"),
    ]
    out["arms"] = {}
    for name, suf, tgt_tag, fst_tag in specs:
        L = legs[suf]
        out["arms"][name] = arm(name, L["recs"], L["groups"][tgt_tag], L["groups"][fst_tag], rng)
        out["arms"][name]["target_stratum"] = tgt_tag
        out["arms"][name]["firstpass_stratum"] = fst_tag

    # ---- MDE anchor: between-model first-pass L1, computed the SAME way, at m=144 ----
    m = out["arms"]["nemotron"]["m"]
    Mn = seat_matrix(legs["nemotron"]["recs"],
                     sorted(legs["nemotron"]["groups"]["no_scope_rebuild_nemotron"]))
    Ms = seat_matrix(legs["stealth"]["recs"],
                     sorted(legs["stealth"]["groups"]["no_scope_rebuild_stealth"]))
    anchor = [l1(Mn[rng.choice(len(Mn), m, replace=False)],
                 Ms[rng.choice(len(Ms), m, replace=False)]) for _ in range(K)]
    out["MDE_anchor"] = {
        "what": "mean L1(nemotron first-pass m-sample, stealth first-pass m-sample)",
        "m": m, "mean": float(np.mean(anchor)), "sd": float(np.std(anchor)),
        "p05": float(np.percentile(anchor, 5)),
    }

    # ---- C3: seed-vs-model hypergeometric over the shared 1005 pool ----
    nem = {c.rsplit("__", 1)[0] if False else c
           for c in legs["nemotron"]["groups"]["no_scope_rebuild_nemotron+rescue1"]}
    ste = set(legs["stealth"]["groups"]["no_scope_rebuild_stealth+rescue1"])
    overlap = nem & ste
    POOL = 1005
    out["C3"] = {
        "pool": POOL, "n_nemotron_rescue": len(nem), "n_stealth_rescue": len(ste),
        "observed_overlap": len(overlap),
        "expected_overlap": len(nem) * len(ste) / POOL,
        "p_one_sided_enrichment": float(hypergeom.sf(len(overlap) - 1, POOL, len(nem), len(ste))),
        "overlap_ids": sorted(overlap),
        "declared": "low-powered by construction; directional only. A null here is "
                    "`underpowered`, not `no enrichment`.",
    }

    # ---- descriptives ----
    out["descriptives"] = []
    for name, suf, tgt_tag, fst_tag in specs:
        L = legs[suf]
        out["descriptives"].append(descriptives(L["recs"], L["groups"][tgt_tag], f"{suf}:{tgt_tag}"))
        out["descriptives"].append(descriptives(L["recs"], L["groups"][fst_tag], f"{suf}:{fst_tag}"))

    # ---- pre-registered outcome table, evaluated IN ORDER ----
    Rn = out["arms"]["nemotron"]["R"]
    Rc1 = out["arms"]["haiku(C1)"]["R"]
    t95n = out["arms"]["nemotron"]["T95"]
    if t95n >= out["MDE_anchor"]["mean"]:
        row, verdict = 1, "underpowered"
    elif Rn > 1 and Rc1 >= 1:
        row, verdict = 2, "regeneration_effect"
    elif Rn > 1 and Rc1 < 1:
        row, verdict = 3, "hard_seeds_differ"
    else:
        row, verdict = 4, "just_misauthored"
    out["verdict"] = {
        "row": row, "outcome": verdict,
        "R_nemotron": Rn, "R_C1_haiku": Rc1, "R_C1_flash": out["arms"]["flash(C1)"]["R"],
        "R_stealth_corroboration": out["arms"]["stealth"]["R"],
        "T95_nemotron": t95n, "MDE_anchor_mean": out["MDE_anchor"]["mean"],
        "power_check": ("PASS - T95 is below the between-model anchor, so the instrument "
                        "resolves a known-real difference at this m")
                       if t95n < out["MDE_anchor"]["mean"] else
                       "FAIL - T95 >= the between-model anchor; rows 2-4 not evaluated",
    }
    print(json.dumps(out, indent=2, sort_keys=False))


if __name__ == "__main__":
    main()
