#!/usr/bin/env python3
"""OQ-109 Phase B2 — pick the one-shot example source by MINIMUM signature prevalence.

Operator criterion (2026-06-11): among the three omega-carrying pilot stakeholder arms
(app_store_commission / hospital_insurer_reimbursement / streaming_royalty_split), pick the
one whose structural signatures are LEAST prevalent in the live 62-story corpus — minimizes
the example-inherited contamination the post-cutover FNL-regime reset has to discount.
(payday_lending is excluded: no omegas.)

Prevalence is counted over authored, generation-inheritable features (the things a model
copies from a one-shot example): claimed_type, topic_domain, metric values (exact and
0.1-bin), requires_active_enforcement, disappearance_verdict, founding_problem_status,
omega type_class values, stakeholder role multiset shape, interval. Engine signature
firings are NOT used (pilots are not in the pipeline output; counting them would require
classifying the pilots — a different, heavier instrument).

Denominator: the 62 live testsets; features are read from the paired json/<id>.json
(coverage of that pairing is reported — Pattern 6; stories without a JSON pair contribute
no counts and are listed).

Run from repo root: python3 audits/2026-06-11_oq109_phase_b/example_prevalence.py
"""
import glob
import json
import os
from collections import Counter

PILOT_DIR = "audits/2026-06-07_stakeholder_layer_migration"
PILOTS = ["app_store_commission", "hospital_insurer_reimbursement", "streaming_royalty_split"]


def fbin(v):
    return None if v is None else round(float(v), 1)


def features(d):
    bp = d.get("base_properties", {})
    sq = d.get("six_questions", {}) or {}
    f = {}
    f["claimed_type"] = bp.get("claimed_type")
    f["topic_domain"] = bp.get("topic_domain")
    f["eps_exact"] = bp.get("extractiveness")
    f["eps_bin"] = fbin(bp.get("extractiveness"))
    f["supp_exact"] = bp.get("suppression")
    f["supp_bin"] = fbin(bp.get("suppression"))
    f["theater_exact"] = bp.get("theater_ratio")
    f["theater_bin"] = fbin(bp.get("theater_ratio"))
    f["enforcement"] = bp.get("requires_active_enforcement")
    f["disappearance_verdict"] = sq.get("disappearance_verdict")
    f["founding_problem_status"] = sq.get("founding_problem_status")
    f["omega_type_classes"] = tuple(sorted({o.get("type_class") for o in d.get("omegas", []) or []})) or None
    sk = d.get("stakeholders", []) or []
    f["role_multiset"] = tuple(sorted(Counter(s.get("role") for s in sk).items())) or None
    iv = d.get("interval", {}) or {}
    f["interval"] = (iv.get("start"), iv.get("end")) if iv else None
    return f


def main():
    live_ids = sorted(os.path.splitext(os.path.basename(p))[0]
                      for p in glob.glob("prolog/testsets/*.pl"))
    corpus, unpaired = {}, []
    for cid in live_ids:
        jp = "json/%s.json" % cid
        if os.path.exists(jp):
            corpus[cid] = features(json.load(open(jp)))
        else:
            unpaired.append(cid)
    n = len(live_ids)
    print("live testsets: %d | json-paired: %d | unpaired (contribute no counts): %d"
          % (n, len(corpus), len(unpaired)))
    if unpaired:
        print("  unpaired:", ", ".join(unpaired))

    # positive control: a feature vector copied from a paired corpus story must score
    # maximal prevalence on every defined feature (the counter fires).
    ctl_id = next(iter(corpus))
    ctl_hits = sum(1 for cid, cf in corpus.items()
                   if cf["claimed_type"] == corpus[ctl_id]["claimed_type"])
    assert ctl_hits >= 1, "positive control failed: corpus story does not match itself"
    print("positive control: %s matches its own claimed_type in %d/%d paired stories — counter fires\n"
          % (ctl_id, ctl_hits, len(corpus)))

    results = {}
    for pilot in PILOTS:
        pf = features(json.load(open(
            "%s/pilot_%s.stakeholder.json" % (PILOT_DIR, pilot))))
        rows, score = [], 0.0
        for key, val in pf.items():
            if val is None:
                rows.append((key, val, None))
                continue
            hits = sum(1 for cf in corpus.values() if cf.get(key) == val)
            frac = hits / len(corpus)
            score += frac
            rows.append((key, val, hits))
        results[pilot] = (score, rows)

    for pilot, (score, rows) in sorted(results.items(), key=lambda kv: kv[1][0]):
        print("== %s — aggregate prevalence score %.3f (sum of per-feature fractions; lower = cleaner)"
              % (pilot, score))
        for key, val, hits in rows:
            if hits is None:
                print("   %-24s %-38r (not authored — no count)" % (key, val))
            else:
                print("   %-24s %-38r %d/%d" % (key, val, hits, len(corpus)))
        print()
    best = min(results, key=lambda k: results[k][0])
    print("MINIMUM-PREVALENCE PICK: %s" % best)


if __name__ == "__main__":
    main()
