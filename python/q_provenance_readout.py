#!/usr/bin/env python3
"""
OQ-254 standing Q-provenance readout (mold: epsilon_authorship_readout.py).

The SCOPE decomposition manifest is the Q-choice record — which axes were asked,
which were deferred and why, the kernel verdict. This readout answers: for each
story in the pipeline output, does its epsilon_provenance generation_run_id JOIN
a manifest, and where? Loud-null obliged: "no run id authored" and "manifest
unreachable" are DIFFERENT tokens, never collapsed (Pattern 6).

Story-side buckets (exactly one per story):
  joined                              run_id resolves in the LIVE tracked dir
                                      (agent/decompose_manifests/, non-archive)
  joined_archive_not_authoritative    run_id resolves only in the archive
                                      (archive_pre_*/ — declared non-read-surface;
                                      never certified as `joined`, never alarmed
                                      as unreachable: the manifest exists)
  no_run_id_authored                  the pre-wiring loud-null stratum, with a
                                      counted sub-breakdown (fact authored with
                                      run_id 'none' / no epsilon_provenance fact /
                                      pre-U6 emission missing) — distinguishable
                                      absences stay distinguished
  run_id_authored_manifest_unreachable  the Pattern-6 bucket: an id names a
                                      manifest nothing can read — must never
                                      collapse into any neighbor

Manifest-side: live-dir census (self-provenance coverage, selected/deferred
counts; empty-deferred_axes classified by the audited mechanical discriminator
from audits/2026-07-25_oq254_q_provenance/ — its own counted bucket, never
averaged into coverage).

Planted positive controls run on EVERY invocation (temp-dir fixtures, one per
non-null token, two-sided): a readout whose controls did not pass is not
trusted, and says so by exiting non-zero.

Usage:
    python3 python/q_provenance_readout.py
        [--pipeline outputs/pipeline_output.json]
        [--manifests-dir agent/decompose_manifests]
        [--out-prefix outputs/q_provenance_readout]
        [--selftest]        # run only the planted controls
"""

import argparse
import json
import sys
import tempfile
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

ARCHIVE_PREFIX = "archive_pre_"


def build_resolver(manifests_root):
    """Map run_id -> 'live' | 'archive' by manifest filename stem.

    Resolver order: live tracked dirs first, then archive — a stem present in
    both resolves live (the authoritative copy wins).
    """
    live, archive = {}, {}
    root = Path(manifests_root)
    if root.is_dir():
        for f in root.rglob("*.manifest.json"):
            rel = f.relative_to(root)
            stem = f.name[: -len(".manifest.json")]
            in_archive = any(part.startswith(ARCHIVE_PREFIX) for part in rel.parts)
            (archive if in_archive else live)[stem] = str(rel)
    return live, archive


def bucket_story(entry, live, archive):
    """(bucket, sub_stratum, run_id) for one per_constraint entry."""
    ep = entry.get("epsilon_provenance")
    if not isinstance(ep, dict):
        return ("no_run_id_authored", "emission_missing", None)
    if ep.get("status") != "authored":
        return ("no_run_id_authored", "no_epsilon_provenance_fact", None)
    run_id = ep.get("generation_run_id")
    if not run_id or run_id == "none":
        return ("no_run_id_authored", "fact_authored_run_id_none", None)
    if run_id in live:
        return ("joined", None, run_id)
    if run_id in archive:
        return ("joined_archive_not_authoritative", None, run_id)
    return ("run_id_authored_manifest_unreachable", None, run_id)


def classify_empty_deferred(manifest):
    """The audited discriminator (OQ-254 Step 0, validated two-sided):
    empty deferred_axes + non-empty fracture_scan.notes narrating the scan
    => legitimately-nothing-deferred; both empty => unauthored."""
    fs = manifest.get("fracture_scan") or {}
    notes = fs.get("notes") if isinstance(fs, dict) else None
    return "legit_nothing_deferred" if notes else "unauthored"


def manifest_census(manifests_root):
    """Live-dir (non-archive) manifest census; archive counted separately."""
    root = Path(manifests_root)
    rows = {"n_live": 0, "n_archive": 0, "live_with_provenance_stamp": 0,
            "live_with_run_id": 0, "axes_total": 0, "selection_reason_present": 0,
            "deferred_entries_total": 0, "deferral_reason_present": 0,
            "deferred_empty": Counter(), "parse_errors": []}
    if not root.is_dir():
        return rows
    for f in root.rglob("*.manifest.json"):
        rel = f.relative_to(root)
        if any(part.startswith(ARCHIVE_PREFIX) for part in rel.parts):
            rows["n_archive"] += 1
            continue
        try:
            d = json.loads(f.read_text(encoding="utf-8"))
        except (json.JSONDecodeError, OSError) as e:
            rows["parse_errors"].append({"file": str(rel), "error": repr(e)})
            continue
        rows["n_live"] += 1
        if isinstance(d.get("_provenance"), dict):
            rows["live_with_provenance_stamp"] += 1
        if d.get("_generation_run_id"):
            rows["live_with_run_id"] += 1
        for ax in d.get("axes", []) or []:
            rows["axes_total"] += 1
            if isinstance(ax, dict) and ax.get("selection_reason"):
                rows["selection_reason_present"] += 1
        da = d.get("deferred_axes")
        if isinstance(da, list) and not da:
            rows["deferred_empty"][classify_empty_deferred(d)] += 1
        elif isinstance(da, list):
            rows["deferred_entries_total"] += len(da)
            rows["deferral_reason_present"] += sum(
                1 for e in da if isinstance(e, dict) and e.get("deferral_reason"))
    rows["deferred_empty"] = dict(rows["deferred_empty"])
    return rows


def run_controls():
    """Planted fixtures, one per non-null token, two-sided. Returns (ok, rows)."""
    rows = []
    with tempfile.TemporaryDirectory() as td:
        mroot = Path(td)
        (mroot / "flat").mkdir()
        (mroot / f"{ARCHIVE_PREFIX}x" / "old").mkdir(parents=True)
        (mroot / "flat" / "ctl_live_fam_1.manifest.json").write_text("{}")
        (mroot / f"{ARCHIVE_PREFIX}x" / "old" / "ctl_arch_fam_1.manifest.json").write_text("{}")
        live, archive = build_resolver(mroot)
        cases = [
            ({"epsilon_provenance": {"status": "authored",
                                     "generation_run_id": "ctl_live_fam_1"}},
             "joined"),
            ({"epsilon_provenance": {"status": "authored",
                                     "generation_run_id": "ctl_arch_fam_1"}},
             "joined_archive_not_authoritative"),
            ({"epsilon_provenance": {"status": "authored",
                                     "generation_run_id": "ctl_missing_fam_1"}},
             "run_id_authored_manifest_unreachable"),
            # two-sided: the null stratum must NOT leak into any join bucket
            ({"epsilon_provenance": {"status": "authored",
                                     "generation_run_id": "none"}},
             "no_run_id_authored"),
            ({"epsilon_provenance": {"status": "none_authored"}},
             "no_run_id_authored"),
            ({}, "no_run_id_authored"),
        ]
        ok = True
        for entry, expect in cases:
            got, sub, _ = bucket_story(entry, live, archive)
            rows.append({"expected": expect, "got": got, "sub": sub,
                         "pass": got == expect})
            ok = ok and got == expect
    return ok, rows


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--pipeline", default=str(ROOT / "outputs" / "pipeline_output.json"))
    ap.add_argument("--manifests-dir", default=str(ROOT / "agent" / "decompose_manifests"))
    ap.add_argument("--out-prefix", default=str(ROOT / "outputs" / "q_provenance_readout"))
    ap.add_argument("--selftest", action="store_true",
                    help="run only the planted controls")
    a = ap.parse_args()

    controls_ok, control_rows = run_controls()
    if not controls_ok or a.selftest:
        for r in control_rows:
            print(f"  control {r['expected']:40} -> {r['got']:40} "
                  f"{'PASS' if r['pass'] else 'FAIL'}")
        if not controls_ok:
            print("CONTROLS FAILED — readout not trusted", file=sys.stderr)
            sys.exit(1)
        if a.selftest:
            print("all controls PASS")
            return

    pipe = json.loads(Path(a.pipeline).read_text(encoding="utf-8"))
    per = pipe["per_constraint"]
    if isinstance(per, dict):
        per = list(per.values())
    manifest = pipe.get("manifest", {})

    live, archive = build_resolver(a.manifests_dir)

    buckets = Counter()
    subs = Counter()
    by_bucket_ids = {}
    for e in per:
        b, sub, run_id = bucket_story(e, live, archive)
        buckets[b] += 1
        if sub:
            subs[sub] += 1
        by_bucket_ids.setdefault(b, []).append(
            {"id": e.get("id"), "run_id": run_id} if run_id else e.get("id"))
    # The Pattern-6 bucket and both join buckets list members in full — they are
    # the actionable strata; the loud-null stratum is counted (ids omitted at
    # volume, sub-breakdown carried).
    for b in ("no_run_id_authored",):
        ids = by_bucket_ids.get(b, [])
        if len(ids) > 20:
            by_bucket_ids[b] = {"n": len(ids), "ids_omitted_over": 20}

    mrows = manifest_census(a.manifests_dir)

    result = {
        "pipeline_manifest": {k: manifest.get(k) for k in
                              ("pipeline_run_at", "n_constraints",
                               "code_commit_short", "corpus_path")},
        "controls": {"state": "pass", "rows": control_rows},
        "story_side": {
            "n_stories": len(per),
            "buckets": dict(buckets),
            "no_run_id_authored_breakdown": dict(subs),
            "members": by_bucket_ids,
        },
        "manifest_side": mrows,
    }
    out_json = Path(a.out_prefix + ".json")
    out_json.parent.mkdir(parents=True, exist_ok=True)
    out_json.write_text(json.dumps(result, indent=2), encoding="utf-8")

    lines = ["# Q-provenance standing readout (OQ-254)", ""]
    lines.append(f"Pipeline run: {manifest.get('pipeline_run_at')} "
                 f"(n={manifest.get('n_constraints')}, "
                 f"code {manifest.get('code_commit_short')})")
    lines.append(f"Controls: pass ({len(control_rows)} planted, two-sided)")
    lines.append("")
    lines.append("| bucket | n |")
    lines.append("|---|---|")
    for b in ("joined", "joined_archive_not_authoritative",
              "no_run_id_authored", "run_id_authored_manifest_unreachable"):
        lines.append(f"| {b} | {buckets.get(b, 0)} |")
    lines.append("")
    lines.append(f"no_run_id_authored breakdown: {dict(subs)}")
    lines.append(f"Manifest side: live={mrows['n_live']} "
                 f"(stamped={mrows['live_with_provenance_stamp']}, "
                 f"run_id={mrows['live_with_run_id']}), archive={mrows['n_archive']}; "
                 f"empty-deferred classes={mrows['deferred_empty']}")
    Path(a.out_prefix + ".md").write_text("\n".join(lines) + "\n", encoding="utf-8")

    for b in ("joined", "joined_archive_not_authoritative",
              "no_run_id_authored", "run_id_authored_manifest_unreachable"):
        print(f"{b:40} {buckets.get(b, 0)}")
    print(f"no_run_id_authored breakdown: {dict(subs)}")
    print(f"manifests: live={mrows['n_live']} archive={mrows['n_archive']}")
    print(f"wrote {out_json} and {a.out_prefix}.md")


if __name__ == "__main__":
    main()
