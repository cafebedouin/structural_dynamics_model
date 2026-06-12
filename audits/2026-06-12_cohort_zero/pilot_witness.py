#!/usr/bin/env python3
"""Pilot witness (the launch's positive control — operator: 'paste-or-untag applied to
the driver itself'). Checks, per generated story: (1) provenance stamps complete and
correct (incl. seeded_from/draw, prompt/schema commits match the repo); (2) schema
validation; (3) compile + lint; (4) deterministic id; (5) seed-echo sanity (title/domain
present; no archived metrics smuggled — spot: extractiveness differs or matches by chance,
reported not judged). Replicates checked for draw indices 2,3 in the probe dir.

Run from repo root: python3 audits/2026-06-12_cohort_zero/pilot_witness.py
"""
import glob
import json
import os
import subprocess
import sys

sys.path.insert(0, "python")
from generate_constraint_pl import validate_json, generate_pl  # noqa: E402
from linter import lint_file  # noqa: E402

ARCHIVE = "prolog/archives/datasets/kernel_v2_test2/json"


def commit_of(path):
    return subprocess.run(["git", "log", "-1", "--format=%H", "--", path],
                          capture_output=True, text=True).stdout.strip()


def main():
    prompt_c = commit_of("prompts/constraint_story_generation_prompt_json.md")
    schema_c = commit_of("schemas/constraint_story_schema.json")
    ok = True
    staged = sorted(glob.glob("json_cohort0/*.json"))
    reps = sorted(glob.glob("audits/2026-06-12_cohort_zero/replicates/*.json"))
    print("staged corpus stories: %d | replicate extras: %d" % (len(staged), len(reps)))
    for f in staged + reps:
        d = json.load(open(f))
        cid = d["header"]["constraint_id"]
        p = d.get("provenance", {})
        seed = p.get("seeded_from")
        rows = []
        rows.append(("provenance complete",
                     all(p.get(k) for k in ("prompt_commit", "schema_commit",
                                            "generated_date", "source_essay",
                                            "one_shot_example", "model",
                                            "sampling_params", "seeded_from"))
                     and isinstance(p.get("draw"), int)))
        rows.append(("prompt_commit matches repo", p.get("prompt_commit") == prompt_c))
        rows.append(("schema_commit matches repo", p.get("schema_commit") == schema_c))
        expected_id = ("%s_c0" % seed) if p.get("draw") == 1 else \
                      ("%s_c0_d%d" % (seed, p.get("draw", -1)))
        rows.append(("deterministic id", cid == expected_id))
        errs = validate_json(d)
        rows.append(("schema validation", not errs))
        try:
            pl = generate_pl(d)
            tmp = "prolog/testsets/.tmp_pilot_%s.pl" % cid
            open(tmp, "w").write(pl)
            lint = lint_file(tmp)
            os.unlink(tmp)
            rows.append(("compiles + story_seed emitted", "story_seed(" in pl))
            rows.append(("lint clean", not lint))
        except Exception as e:
            rows.append(("compiles", False))
            lint = [str(e)]
        arch = json.load(open("%s/%s.json" % (ARCHIVE, seed)))
        old_eps = arch.get("base_properties", {}).get("extractiveness")
        new_eps = d.get("base_properties", {}).get("extractiveness")
        bad = [n for n, v in rows if not v]
        ok &= not bad
        print("%s  %s  (eps archived=%s drawn=%s%s)" % (
            "PASS" if not bad else "FAIL[%s]" % ",".join(bad), cid, old_eps, new_eps,
            "" if not bad else " :: " + "; ".join(map(str, (errs or []) + (lint or [])))[:200]))
    print("PILOT WITNESS:", "ALL PASS" if ok else "FAILURES — driver not yet trusted")
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
