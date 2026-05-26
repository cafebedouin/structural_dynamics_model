#!/usr/bin/env python3
"""
CS Drift Mismatch H¹ distribution analysis.

Invokes the Prolog cs_drift_mismatch module, collects flagged readings,
maps to H¹ band from pipeline_output.json, and reports the distribution.

Run from repo root:
  python3 python/run_drift_mismatch.py
"""

import json
import re
import subprocess
import sys
from collections import Counter
from pathlib import Path

REPO_ROOT = Path(__file__).parent.parent
PROLOG_DIR = REPO_ROOT / "prolog"
PIPELINE_OUTPUT = REPO_ROOT / "outputs" / "pipeline_output.json"


def load_h1_bands():
    with open(PIPELINE_OUTPUT) as f:
        data = json.load(f)
    manifest = data.get("manifest", {})
    h1 = {item["id"]: item.get("h1_band", 0) for item in data["per_constraint"]}
    drift_detected = {
        item["id"]: any(
            e.get("type") == "network_drift"
            for e in item.get("drift_events", [])
        )
        for item in data["per_constraint"]
    }
    return h1, drift_detected, manifest


def run_prolog_mismatch():
    goal = "[cs_drift_mismatch], run_drift_mismatch_report, halt"
    result = subprocess.run(
        ["swipl", "-g", goal, "-t", "halt(1)"],
        capture_output=True, text=True, cwd=str(PROLOG_DIR),
        timeout=120
    )
    return result.stdout, result.stderr, result.returncode


def parse_mismatch_lines(stdout):
    mismatches = []
    for line in stdout.splitlines():
        if line.startswith("MISMATCH: "):
            rest = line[len("MISMATCH: "):]
            parts = rest.split(" | ", 1)
            cid = parts[0].strip()
            source = parts[1].strip() if len(parts) > 1 else "unknown"
            mismatches.append((cid, source))
    return mismatches


def main():
    print("Running cs_drift_mismatch via Prolog...\n")
    try:
        stdout, stderr, rc = run_prolog_mismatch()
    except subprocess.TimeoutExpired:
        print("ERROR: Prolog timed out after 120s", file=sys.stderr)
        sys.exit(1)

    # Print Prolog stdout
    print(stdout, end="")
    if stderr.strip():
        # Filter out routine SWI-Prolog load messages; show only warnings/errors
        err_lines = [l for l in stderr.splitlines()
                     if any(kw in l.lower() for kw in ["warning", "error", "undefined"])]
        if err_lines:
            print("--- Prolog warnings/errors ---")
            print("\n".join(err_lines[:30]))
            print()

    mismatches = parse_mismatch_lines(stdout)

    if not mismatches:
        print("\nNo mismatches found in Prolog output — nothing to map to H¹.")
        print("(If this is unexpected, check that testsets with CS drift state are loaded.)")
        return

    h1_bands, drift_detected, manifest = load_h1_bands()

    print("\n" + "=" * 70)
    print("H¹ BAND DISTRIBUTION — CS DRIFT MISMATCH SET")
    print("=" * 70)
    print(f"\nPipeline manifest: {manifest.get('pipeline_run_at', 'unknown')} | "
          f"n={manifest.get('n_constraints', '?')} | commit={manifest.get('code_commit_short', '?')}")
    print(f"Total mismatch readings (Prolog): {len(mismatches)}\n")

    # Classify each mismatch
    in_pipeline = []
    not_in_pipeline = []
    for cid, source in mismatches:
        if cid in h1_bands:
            band = h1_bands[cid]
            nd = drift_detected.get(cid, False)
            in_pipeline.append((cid, source, band, nd))
        else:
            not_in_pipeline.append((cid, source))

    print(f"In pipeline output (H¹ available): {len(in_pipeline)}")
    print(f"Not in pipeline output (H¹ unavailable): {len(not_in_pipeline)}\n")

    if in_pipeline:
        # Separate metric-stable (no network_drift in pipeline) from those with drift
        # The Prolog already checked metric stability via the engine; the pipeline flag
        # provides independent corroboration.
        stable_in_pipeline = [(c, s, b) for c, s, b, nd in in_pipeline if not nd]
        drifting_in_pipeline = [(c, s, b) for c, s, b, nd in in_pipeline if nd]

        print("-- In-pipeline readings (engine called them metric-stable) --\n")
        print(f"  Corroborated stable (no pipeline network_drift event): {len(stable_in_pipeline)}")
        print(f"  Pipeline shows network_drift event (Prolog engine vs pipeline disagree): "
              f"{len(drifting_in_pipeline)}\n")

        if stable_in_pipeline:
            band_counts = Counter(b for _, _, b in stable_in_pipeline)
            print("  H¹ distribution (corroborated metric-stable mismatches):")
            for band in sorted(band_counts):
                print(f"    H¹={band}: {band_counts[band]}")
            print()
            print("  Per-reading detail:")
            for cid, src, band in sorted(stable_in_pipeline, key=lambda x: x[2]):
                print(f"    {cid:<50} H¹={band}  source={src}")
            print()

        if drifting_in_pipeline:
            print("  H¹ distribution (pipeline-contradicted: engine=stable, pipeline=drifting):")
            band_counts_d = Counter(b for _, _, b in drifting_in_pipeline)
            for band in sorted(band_counts_d):
                print(f"    H¹={band}: {band_counts_d[band]}")
            print()
            print("  Per-reading detail:")
            for cid, src, band in sorted(drifting_in_pipeline, key=lambda x: x[2]):
                print(f"    {cid:<50} H¹={band}  source={src}")
            print()

    if not_in_pipeline:
        print(f"-- Not in pipeline output ({len(not_in_pipeline)} readings, H¹ unknown) --\n")
        for cid, src in sorted(not_in_pipeline):
            print(f"  {cid:<50} source={src}")
        print()

    # Full H¹ distribution over all in-pipeline mismatches (regardless of pipeline drift flag)
    if in_pipeline:
        print("-- Full H¹ distribution (all in-pipeline mismatches) --\n")
        all_bands = Counter(b for _, _, b, _ in in_pipeline)
        for band in sorted(all_bands):
            pct = all_bands[band] * 100 // len(in_pipeline)
            print(f"  H¹={band}: {all_bands[band]}  ({pct}%)")
        print()


if __name__ == "__main__":
    main()
