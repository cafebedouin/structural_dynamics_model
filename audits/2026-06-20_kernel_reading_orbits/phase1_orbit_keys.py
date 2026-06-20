#!/usr/bin/env python3
"""Phase 1 — cross-twin orbit-key diagnosticity (OQ-150).

Deterministic Python over the pre-computed twin outputs
outputs/pipeline_output.{haiku,flash}.json. No engine re-run. Design and decision rule are
pre-registered in PRE_REGISTRATION.md; this script must not deviate from it.
"""
import json
import random
from collections import defaultdict, Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT = Path(__file__).resolve().parent / "phase1_results.json"
SEATS = ["powerless", "moderate", "institutional", "analytical"]
PERMUTE_N = 1000
SEED = 20260620

# priority cascade (CLAUDE.md) — higher index = higher priority, for R4 tie-breaks
CASCADE = ["unknown", "naturalized", "piton", "tangled_rope", "rope", "scaffold",
           "snare", "mountain"]
def cascade_rank(t):
    return CASCADE.index(t) if t in CASCADE else -1


def load(twin):
    d = json.load(open(ROOT / f"outputs/pipeline_output.{twin}.json"))
    pc = d["per_constraint"]
    items = pc.values() if isinstance(pc, dict) else pc
    return {v["id"]: v for v in items}


def wilson_lo(k, n, z=1.96):
    if n == 0:
        return 0.0
    p = k / n
    d = 1 + z * z / n
    c = p + z * z / (2 * n)
    m = z * ((p * (1 - p) / n + z * z / (4 * n * n)) ** 0.5)
    return (c - m) / d


# ---- reading-orbit key extractors (unit = id) ----
def key_R1(v):  # observer orbit (signature proxy for gauge_orbit)
    return v.get("signature")

def key_R2(v):  # commitment-apparatus [axis 1]
    return v.get("cs_pattern")

def key_R3(v):  # terminal projection, committer side [axis 2]
    return v.get("cs_drift_terminal")

def key_R4(v):  # terminal projection, observer side: dominant dr_type over 4 seats
    p = v.get("perspectives") or {}
    types = [p.get(s) for s in SEATS if p.get(s)]
    if not types:
        return None
    cnt = Counter(types)
    top = max(cnt.values())
    winners = [t for t, c in cnt.items() if c == top]
    return max(winners, key=cascade_rank)

def key_R5(v):  # seat-signature / role-vector (4-tuple)
    p = v.get("perspectives") or {}
    return tuple(p.get(s) for s in SEATS)

READING_KEYS = {"R1_observer_signature": key_R1,
                "R2_apparatus_cs_pattern": key_R2,
                "R3_terminal_committer": key_R3,
                "R4_terminal_observer": key_R4,
                "R5_seat_role_vector": key_R5}


def agreement_test(H, F, extractor, common, rng):
    """Cross-twin membership-agreement test for one key. Returns the full result dict."""
    lblH = {i: extractor(H[i]) for i in common}
    lblF = {i: extractor(F[i]) for i in common}
    # exclude ids where either side is None (un-keyed) — report coverage
    keyed = [i for i in common if lblH[i] is not None and lblF[i] is not None]
    n = len(keyed)
    if n == 0:
        return {"n_keyed": 0, "verdict": "NO_COVERAGE"}
    agree = sum(1 for i in keyed if lblH[i] == lblF[i])
    obs = agree / n
    lo = wilson_lo(agree, n)
    # permutation null: shuffle F labels among keyed ids
    fvals = [lblF[i] for i in keyed]
    hvals = [lblH[i] for i in keyed]
    perm = []
    for _ in range(PERMUTE_N):
        sh = fvals[:]
        rng.shuffle(sh)
        perm.append(sum(1 for a, b in zip(hvals, sh) if a == b) / n)
    perm.sort()
    band95 = perm[int(0.95 * PERMUTE_N)]
    # per-twin non-degeneracy
    distinctH = len(set(lblH[i] for i in keyed))
    distinctF = len(set(lblF[i] for i in keyed))
    largestH = max(Counter(lblH[i] for i in keyed).values()) / n
    largestF = max(Counter(lblF[i] for i in keyed).values()) / n
    nondegen = (distinctH >= 2 and distinctF >= 2 and largestH < 0.95 and largestF < 0.95)
    robust = (lo > band95) and (band95 < 1.0) and nondegen
    return {
        "n_keyed": n, "observed_agreement": round(obs, 4), "wilson95_lo": round(lo, 4),
        "permute_band95": round(band95, 4), "permute_mean": round(sum(perm) / len(perm), 4),
        "distinct_labels_haiku": distinctH, "distinct_labels_flash": distinctF,
        "largest_orbit_frac_haiku": round(largestH, 4),
        "largest_orbit_frac_flash": round(largestF, 4),
        "non_degenerate": nondegen,
        "verdict": "DRAW_ROBUST" if robust else "DRAW_SENSITIVE",
    }


# ---- kernel-orbit key K1 (unit = kernel) ----
def multi_kernels(rows):
    by = defaultdict(list)
    for i in rows:
        by[i.split("__", 1)[0]].append(i)
    return {k: sorted(v) for k, v in by.items() if len(v) >= 2}

def depth_vector(rows, ids):
    out = []
    for s in SEATS:
        types = {(rows[c].get("perspectives") or {}).get(s) for c in ids}
        types.discard(None)
        out.append(1 if len(types) >= 2 else 0)
    return tuple(out)

def k1_signature(rows, ids):
    return (len(ids), depth_vector(rows, ids),
            tuple(sorted(Counter(rows[c].get("claimed_type") for c in ids).items())))


def main():
    H, F = load("haiku"), load("flash")
    common = sorted(set(H) & set(F))
    rng = random.Random(SEED)
    res = {"meta": {"haiku_n": len(H), "flash_n": len(F), "common": len(common),
                    "permute_n": PERMUTE_N, "seed": SEED}}

    # ---- positive controls ----
    ct = sum(1 for i in common if H[i]["claimed_type"] == F[i]["claimed_type"]) / len(common)
    res["control_claimed_type_agreement"] = round(ct, 4)  # expect ~0.721
    kH = {i.split("__", 1)[0] for i in H}
    kF = {i.split("__", 1)[0] for i in F}
    res["control_kernel_set_identical"] = (kH == kF)        # expect True (=1.000 agreement)
    if abs(ct - 0.721) > 0.01 or kH != kF:
        res["HALT"] = "positive control failed — probe mis-wired, do not trust orbit numbers"
        json.dump(res, open(OUT, "w"), indent=2)
        print(json.dumps(res, indent=2)); return

    # ---- reading-orbit keys ----
    res["reading_orbits"] = {}
    for name, ext in READING_KEYS.items():
        res["reading_orbits"][name] = agreement_test(H, F, ext, common, rng)

    # ---- kernel-orbit key K1 ----
    KH, KF = multi_kernels(H), multi_kernels(F)
    ck = sorted(set(KH) & set(KF))
    sigH = {k: k1_signature(H, KH[k]) for k in ck}
    sigF = {k: k1_signature(F, KF[k]) for k in ck}
    agree = sum(1 for k in ck if sigH[k] == sigF[k])
    n = len(ck)
    fvals = [sigF[k] for k in ck]
    hvals = [sigH[k] for k in ck]
    perm = []
    for _ in range(PERMUTE_N):
        sh = fvals[:]; rng.shuffle(sh)
        perm.append(sum(1 for a, b in zip(hvals, sh) if a == b) / n)
    perm.sort()
    band95 = perm[int(0.95 * PERMUTE_N)]
    lo = wilson_lo(agree, n)
    res["kernel_orbits"] = {"K1_structure_signature": {
        "n_kernels": n, "observed_agreement": round(agree / n, 4),
        "wilson95_lo": round(lo, 4), "permute_band95": round(band95, 4),
        "distinct_signatures_haiku": len(set(sigH.values())),
        "distinct_signatures_flash": len(set(sigF.values())),
        "largest_orbit_haiku": max(Counter(sigH.values()).values()),
        "verdict": "DRAW_ROBUST" if (lo > band95 and band95 < 1.0) else "DRAW_SENSITIVE",
    }}

    # ---- menu summary (the OQ-56 candidate vocabulary) ----
    robust = [k for k, v in res["reading_orbits"].items() if v.get("verdict") == "DRAW_ROBUST"]
    res["MENU"] = {"draw_robust_reading_orbits": robust,
                   "empty_menu": len(robust) == 0}

    json.dump(res, open(OUT, "w"), indent=2)
    print(json.dumps(res, indent=2))


if __name__ == "__main__":
    main()
