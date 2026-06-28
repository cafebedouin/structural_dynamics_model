#!/usr/bin/env python3
"""Phase 0 steps 2-3 — reproduce positive controls + raw divergences at one commit.

Reads the freshly re-classified twin outputs (both at bbf5c92) for the engine-computed
fields (claimed_type, signature) and the authored .pl facts for the committer-axis fields
(cs_kernel_id, cs_reading_relation, cs_axiom_status).

Positive controls (must reproduce or the read is broken — halt):
  - claimed_type per-id agreement ~0.721
  - cs_kernel_id membership 1.000 (sets identical)

Raw divergences (must survive the clean re-classification):
  - Field A: constructed_high_extraction <-> false_ci_rope fork rate (both directions)
  - Field B: cs_reading_relation per-slot multiset agreement (~0.392)
  - Field C: cs_axiom_status overridden 51 (haiku) vs 4 (flash)
"""
import json
import re
import glob
from collections import Counter
from pathlib import Path

A = Path(__file__).resolve().parent
REPO = A.parents[1]
OUT = REPO / "outputs"


def load_pc(name):
    d = json.load(open(OUT / name))
    return {e["id"]: e for e in d["per_constraint"]}, d["manifest"]


# ---- authored .pl parsers (commit-independent; hash-pinned separately) ----
def parse_pl_field(twin, regex, per_uid=False):
    """Return {cid: [captures...]} for a regex over each .pl file in the twin dir."""
    out = {}
    for f in sorted(glob.glob(str(REPO / "prolog" / f"testsets_{twin}" / "*.pl"))):
        cid = Path(f).stem
        txt = open(f).read()
        out[cid] = regex.findall(txt)
    return out


RE_KERNEL = re.compile(r"cs_kernel_id\([^,]+,\s*([a-z0-9_]+)\)")
RE_RELATION = re.compile(r"cs_reading_relation\([^,]+,\s*[^,]+,\s*([a-z_]+)\)")
RE_AXIOM_STATUS = re.compile(r"cs_axiom_status\([^,]+,\s*([a-z_]+)\)")


def per_id_agreement(H, F, label):
    common = sorted(set(H) & set(F))
    agree = sum(1 for i in common if H[i] == F[i])
    n = len(common)
    return {"field": label, "n_common": n, "agree": agree,
            "agreement": round(agree / n, 4) if n else None}


def main():
    res = {}
    H, mH = load_pc("pipeline_output.haiku.json")
    F, mF = load_pc("pipeline_output.flash.json")
    res["manifest"] = {
        "haiku": {k: mH.get(k) for k in ("code_commit_short", "n_constraints", "pipeline_run_at")},
        "flash": {k: mF.get(k) for k in ("code_commit_short", "n_constraints", "pipeline_run_at")},
        "same_commit": mH.get("code_commit_short") == mF.get("code_commit_short"),
    }

    # --- Positive control 1: claimed_type per-id agreement (target 0.721) ---
    ct_H = {i: e.get("claimed_type") for i, e in H.items()}
    ct_F = {i: e.get("claimed_type") for i, e in F.items()}
    res["ctrl_claimed_type"] = per_id_agreement(ct_H, ct_F, "claimed_type")

    # --- Positive control 2: cs_kernel_id membership 1.000 (sets identical) ---
    kH = {c: (v[0] if v else None) for c, v in parse_pl_field("haiku", RE_KERNEL).items()}
    kF = {c: (v[0] if v else None) for c, v in parse_pl_field("flash", RE_KERNEL).items()}
    res["ctrl_cs_kernel_id"] = per_id_agreement(kH, kF, "cs_kernel_id")
    res["ctrl_cs_kernel_id"]["sets_identical"] = (set(kH) == set(kF))

    # --- Field A: signature fork (CHE <-> FCR, both directions) ---
    sH = {i: e.get("signature") for i, e in H.items()}
    sF = {i: e.get("signature") for i, e in F.items()}
    res["fieldA_signature_dist_haiku"] = dict(Counter(sH.values()))
    res["fieldA_signature_dist_flash"] = dict(Counter(sF.values()))
    common = sorted(set(sH) & set(sF))
    CHE, FCR = "constructed_high_extraction", "false_ci_rope"
    fork_h_che_f_fcr = [i for i in common if sH[i] == CHE and sF[i] == FCR]
    fork_h_fcr_f_che = [i for i in common if sH[i] == FCR and sF[i] == CHE]
    res["fieldA_fork"] = {
        "haiku_CHE_flash_FCR": len(fork_h_che_f_fcr),
        "haiku_FCR_flash_CHE": len(fork_h_fcr_f_che),
        "total_bidirectional_fork": len(fork_h_che_f_fcr) + len(fork_h_fcr_f_che),
        "signature_agreement": per_id_agreement(sH, sF, "signature")["agreement"],
    }
    res["fieldA_fork_ids"] = {
        "haiku_CHE_flash_FCR": fork_h_che_f_fcr,
        "haiku_FCR_flash_CHE": fork_h_fcr_f_che,
    }

    # --- Field B: cs_reading_relation per-slot multiset agreement ---
    rH = {c: tuple(sorted(v)) for c, v in parse_pl_field("haiku", RE_RELATION).items()}
    rF = {c: tuple(sorted(v)) for c, v in parse_pl_field("flash", RE_RELATION).items()}
    # only slots where BOTH authored at least one relation
    both = sorted(i for i in set(rH) & set(rF) if rH[i] and rF[i])
    agreeB = sum(1 for i in both if rH[i] == rF[i])
    res["fieldB_reading_relation"] = {
        "n_both_authored": len(both),
        "multiset_agree": agreeB,
        "multiset_agreement": round(agreeB / len(both), 4) if both else None,
        "dist_haiku": dict(Counter(x for v in rH.values() for x in v)),
        "dist_flash": dict(Counter(x for v in rF.values() for x in v)),
    }

    # --- Field C: cs_axiom_status overridden counts ---
    aH = parse_pl_field("haiku", RE_AXIOM_STATUS)
    aF = parse_pl_field("flash", RE_AXIOM_STATUS)
    cH = Counter(x for v in aH.values() for x in v)
    cF = Counter(x for v in aF.values() for x in v)
    res["fieldC_axiom_status"] = {
        "haiku": dict(cH), "flash": dict(cF),
        "overridden_haiku": cH.get("overridden", 0),
        "overridden_flash": cF.get("overridden", 0),
    }

    json.dump(res, open(A / "recon_reproduce.json", "w"), indent=2)
    # print everything except the long id lists
    printable = {k: v for k, v in res.items() if k != "fieldA_fork_ids"}
    print(json.dumps(printable, indent=2))
    print(f"\nfork id counts: CHE->FCR={len(fork_h_che_f_fcr)} FCR->CHE={len(fork_h_fcr_f_che)}")


if __name__ == "__main__":
    main()
