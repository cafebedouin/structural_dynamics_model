#!/usr/bin/env python3
"""bisect_batch.py — which converted FILE moved the corpus?

The batch six-leg pair came back NOT output-preserving (129/279 testsets, 1106/1106
kernel_v1). This attributes it per file, independently rather than cumulatively: revert every
converted file, convert exactly ONE, classify `testsets` (~35 s), diff against the batch
baseline. Independent runs, so a file that only moves the corpus in company with another still
shows up as clean here — that residual is reported, not assumed away: the per-file changes are
summed and compared against the whole-batch number at the end.
"""
from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
sys.path.insert(0, str(REPO / "python"))
sys.path.insert(0, str(HERE))
from dispatch_head_check import DECLARED  # noqa: E402  (post-retirement: latent-B is empty)

BASE_JSON = REPO / "outputs" / "oq303b_testsets_batchclean.json"
PROBE_OUT = "oq303b_bisect_probe.json"

# The worklist as it was BEFORE the retirement commit — read from git so this script does not
# depend on the registry it just emptied.
def worklist() -> dict[str, list[str]]:
    src = subprocess.run(["git", "show", "6c1bfa44:python/dispatch_head_check.py"],
                         cwd=REPO, capture_output=True, text=True, timeout=120).stdout
    # Parse the DECLARED literal out of the historical source rather than exec'ing the
    # module (it references __file__ and would import at load time).
    import ast
    tree = ast.parse(src)
    declared = None
    for node in ast.walk(tree):
        if isinstance(node, ast.AnnAssign) and getattr(node.target, "id", "") == "DECLARED":
            declared = node.value
        elif isinstance(node, ast.Assign) and any(
                getattr(t, "id", "") == "DECLARED" for t in node.targets):
            declared = node.value
    if declared is None:
        raise SystemExit("bisect: RED — no DECLARED literal in the historical source")
    pairs = {}
    for k, v in zip(declared.keys, declared.values):
        key = tuple(ast.literal_eval(e) for e in k.elts)
        pairs[key] = v.value if isinstance(v, ast.Constant) else "MUST-NOT-FIRE"
    by_file: dict[str, list[str]] = {}
    for (f, pi), cls in pairs.items():
        if cls == "latent-B":
            by_file.setdefault(f, []).append(pi)
    return by_file


def revert_all(files: list[str]) -> None:
    subprocess.run(["git", "checkout", "--"] + [f"prolog/{f}" for f in files],
                   cwd=REPO, check=True, timeout=120)


def baseline_keys() -> dict:
    d = json.loads(BASE_JSON.read_text())
    return {r["id"]: r for r in d["per_constraint"]}


def classify() -> dict:
    from run_pipeline import classify_corpus
    classify_corpus("testsets", PROBE_OUT, None, timeout=1800, soft_timeout=900)
    d = json.loads((REPO / "outputs" / PROBE_OUT).read_text())
    return {r["id"]: r for r in d["per_constraint"]}


def main() -> int:
    by_file = worklist()
    files = sorted(by_file)
    base = baseline_keys()
    revert_all(files)

    # Control: with EVERYTHING reverted the probe must match the baseline exactly. If it does
    # not, the baseline is not the pre-batch state and no attribution below means anything.
    got = classify()
    zero = sum(1 for k in set(base) | set(got) if base.get(k) != got.get(k))
    print(f"CONTROL all-reverted vs batchclean: changed={zero}", flush=True)
    if zero != 0:
        print("bisect: RED — the reverted tree does not reproduce the baseline; attribution "
              "would be meaningless", flush=True)
        return 1

    results = []
    for f in files:
        revert_all(files)
        conv = subprocess.run(
            [sys.executable, str(HERE / "convert.py"), "--file", f,
             *sum((["--pred", pi] for pi in by_file[f]), [])],
            cwd=REPO, capture_output=True, text=True, timeout=900)
        # convert.py takes one --pred; run it once per predicate instead
        revert_all(files)
        for pi in by_file[f]:
            subprocess.run([sys.executable, str(HERE / "convert.py"), "--file", f,
                            "--pred", pi], cwd=REPO, capture_output=True, text=True,
                           timeout=900)
        got = classify()
        n = sum(1 for k in set(base) | set(got) if base.get(k) != got.get(k))
        results.append((f, n, by_file[f]))
        print(f"  {f:34} changed={n:4}  ({', '.join(by_file[f])})", flush=True)

    revert_all(files)
    results.sort(key=lambda r: -r[1])
    (HERE / "bisect_batch.json").write_text(json.dumps(
        [{"file": f, "changed": n, "preds": p} for f, n, p in results], indent=1))
    movers = [r for r in results if r[1] > 0]
    print(f"\n{len(movers)} of {len(files)} files move the corpus on their own; "
          f"sum of independent changes = {sum(r[1] for r in results)} "
          f"(whole batch moved 129 — a shortfall means files interact)", flush=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
