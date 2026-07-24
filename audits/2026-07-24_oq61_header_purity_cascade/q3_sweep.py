"""OQ-61 Q3 verification sweep: classify each leg + archive via classify_corpus
(fresh process, asserta overlay, serialized), record the scored/gate_fail/
no_data split, assert the sum-invariant per leg, and compare unscored totals to
the census reproduction targets where they exist."""
import sys, json
sys.path.insert(0, "python")
import run_pipeline as rp

LEGS = [
    ("testsets",         "testsets",                          199),
    ("testsets_haiku",   "testsets_haiku",                    960),
    ("testsets_flash",   "testsets_flash",                    960),
    ("testsets_kimi",    "testsets_kimi",                    1005),
    ("testsets_sonnet",  "testsets_sonnet",                  1001),
    ("kernel_v1",        "archives/datasets/kernel_v1",      1106),
]
# census reproduction targets: unscored == gate_fail + no_data (raw sentinel+flip)
REPRO = {
    "testsets":       (46, 35, 11),
    "testsets_haiku": (468, 466, 2),
    "testsets_flash": (292, 212, 80),
    "kernel_v1":      (4, 2, 2),
}

results = {}
for name, path, expect_n in LEGS:
    out = f"oq61_q3_{name}.json"
    rp.classify_corpus(path, out, None)
    d = json.load(open(f"outputs/{out}"))
    diag = d["diagnostic"]
    s = diag["purity_n_scored"]; g = diag["purity_n_gate_fail"]
    nd = diag["purity_n_no_data"]; t = diag["purity_n_total"]
    assert s + g + nd == t, f"{name}: split {s}+{g}+{nd} != total {t}"
    assert t == expect_n, f"{name}: n_total {t} != glob {expect_n}"
    unscored = g + nd
    tag = ""
    if name in REPRO:
        eu, eg, end_ = REPRO[name]
        ok = (unscored == eu and g == eg and nd == end_)
        tag = f"REPRO {'OK' if ok else 'MISMATCH'} (target {eu}={eg}+{end_})"
    else:
        tag = "FRESH (no prior witness)"
    results[name] = dict(scored=s, gate_fail=g, no_data=nd, total=t,
                         unscored=unscored, tag=tag)
    print(f"[{name:16}] scored={s:5} gate_fail={g:4} no_data={nd:4} "
          f"total={t:5} unscored={unscored:4}  {tag}", flush=True)

json.dump(results, open("outputs/oq61_q3_sweep_summary.json", "w"), indent=2)
print("\nDONE")
