#!/usr/bin/env python3
"""OQ-60 C-FLOOR witness: edited-engine classify_corpus on ALL FOUR legs (serialized),
then per-constraint census join per leg.

Join rule per census row (disposition):
  scored   -> JSON purity_score numeric AND == census P rounded to JSON precision (6dp)
  unknown  -> JSON purity_score null  (the flip)
  sentinel -> JSON purity_score null  (gate-fail, was already null)
Over-flip  = scored row whose JSON is null or value-changed  -> census falsified, HALT.
Under-flip = unknown row whose JSON is numeric               -> missed caller, HALT.
Screen: scorable-mean delta vs pre-registered prediction (tolerance 1e-5).
"""
import sys, os, json, csv, glob, hashlib

REPO = "/home/scott/bin/structural_dynamics_model"
AUD = f"{REPO}/audits/2026-07-17_oq60_purity_absence"
sys.path.insert(0, f"{REPO}/python")
os.chdir(REPO)

LEGS = [
    # (corpus_path, out_name, expected_model, census_tsv)
    ("testsets", "oq60_cfloor_testsets.json", None, "census_testsets_v2_2026-07-23.tsv"),
    ("testsets_haiku", "oq60_cfloor_haiku.json", "claude-haiku-4-5", "census_testsets_haiku.tsv"),
    ("testsets_flash", "oq60_cfloor_flash.json", "gemini-2.5-flash", "census_testsets_flash.tsv"),
    ("archives/datasets/kernel_v1", "oq60_cfloor_kernel_v1.json", None, "census_kernel_v1.tsv"),
]

# Pre-registered delta-mean screen (computed from census BEFORE this run)
PRED = {
    "testsets":  (153, 0.5450032680),
    "testsets_haiku": (492, 0.4915548780),
    "testsets_flash": (668, 0.5711254990),
    "archives/datasets/kernel_v1": (1102, 0.4812507562),
}

def corpus_md5(leg):
    files = sorted(glob.glob(os.path.join(REPO, "prolog", leg, "*.pl")))
    h = hashlib.md5()
    for f in files:
        h.update(open(f, "rb").read())
    return len(files), h.hexdigest()

if sys.argv[1] == "run":
    from run_pipeline import classify_corpus
    for leg, out, model, _ in LEGS:
        n, d = corpus_md5(leg)
        print(f"[fingerprint-pre] {leg} n={n} md5={d}", flush=True)
        m = classify_corpus(leg, out, model)
        print(f"[done] {leg}: n={m.get('n_constraints')} commit={m.get('code_commit_short')} dirty={m.get('code_dirty')}", flush=True)
        n2, d2 = corpus_md5(leg)
        assert (n, d) == (n2, d2), f"CORPUS DRIFT during {leg} run"
        print(f"[fingerprint-post] {leg} stable", flush=True)

elif sys.argv[1] == "join":
    halt = False
    for leg, out, _, tsv in LEGS:
        rows = list(csv.DictReader(open(f"{AUD}/{tsv}"), delimiter="\t"))
        data = json.load(open(f"{REPO}/outputs/{out}"))
        js = {r["id"]: r.get("purity_score") for r in data["per_constraint"]}
        cids, jids = {r["constraint"] for r in rows}, set(js)
        over, under, valdiff, flips = [], [], [], []
        for r in rows:
            cid, disp = r["constraint"], r["disposition"]
            if cid not in js:
                continue
            v = js[cid]
            if disp == "scored":
                if v is None:
                    over.append(cid)
                elif abs(v - round(float(r["purity"]), 6)) > 1e-6:
                    valdiff.append((cid, r["purity"], v))
            elif disp == "unknown":
                if v is None:
                    flips.append(cid)
                else:
                    under.append((cid, v))
            elif disp == "sentinel":
                if v is not None:
                    valdiff.append((cid, "-1.0(sentinel)", v))
        scorable = [v for v in js.values() if v is not None]
        n_sc, mean = len(scorable), (sum(scorable) / len(scorable) if scorable else 0.0)
        pn, pm = PRED[leg]
        mean_ok = (n_sc == pn) and abs(mean - pm) < 1e-5
        print(f"[{leg}] census_rows={len(rows)} json_rows={len(js)} "
              f"membership: only_census={len(cids-jids)} only_json={len(jids-cids)}")
        print(f"  flips={len(flips)} over_flip={over[:5]} under_flip={under[:5]} value_diff={valdiff[:5]}")
        print(f"  screen: scorable n={n_sc} (pred {pn}) mean={mean:.10f} (pred {pm:.10f}) -> {'OK' if mean_ok else '*** MISMATCH'}")
        if over or under or valdiff:
            halt = True
            print(f"  *** [{leg}] JOIN FAILED — HALT branch")
        else:
            print(f"  [{leg}] JOIN CLEAN: flip set exactly the census unknown rows")
    # money pair
    js = {r["id"]: r.get("purity_score") for r in json.load(open(f"{REPO}/outputs/oq60_cfloor_testsets.json"))["per_constraint"]}
    print(f"[money pair] conceptual_framework_reading: 0.972 -> {js.get('conceptual_framework_reading')}")
    print(f"[money pair] vocabulary_collision_reading: 0.948 -> {js.get('vocabulary_collision_reading')}")
    sys.exit(1 if halt else 0)
