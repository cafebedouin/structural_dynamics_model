#!/usr/bin/env python3
"""
perturb(param, values) -> re-export -> fold-survival per kernel

Single primitive that unifies the type-stability sweep family.
See: docs/technical/build_discipline.md, python/tests/cross_reading_diff.py

Usage:
    from python.sweeps.perturb import perturb
    result = perturb("tangled_rope_chi_floor", [0.40, 0.41, 0.42])
    # or as a CLI:
    python3 python/sweeps/perturb.py --param tangled_rope_chi_floor --values 0.40 0.41 0.42
"""

import argparse
import hashlib
import json
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"
ORBITS_PATH = ROOT / "outputs" / "product_site_orbits.json"
PIPELINE_PATH = ROOT / "outputs" / "pipeline_output.json"


def _compute_corpus_hash(testsets_dir: Path) -> str:
    """sha256 of sorted (filename, file_content) pairs.

    Detects both membership changes (add/remove testset) AND in-place content edits.
    Filename-only would miss in-place edits; mtime is cheaper but not git-reproducible.
    Known limit: does not detect changes in testsets/<run_tag>/ subdirs (not loaded by
    corpus_loader). Documented in OQ-29.
    """
    pairs = []
    for p in sorted(testsets_dir.glob("*.pl")):
        pairs.append(p.name + "\n" + p.read_text(encoding="utf-8", errors="replace"))
    return hashlib.sha256("\n---\n".join(pairs).encode()).hexdigest()[:12]

# ---------------------------------------------------------------------------
# Overlay template (A1 dialect: retract/assert param/2)
# ---------------------------------------------------------------------------

_OVERLAY_TMPL = """\
%% Auto-generated perturb overlay — DO NOT EDIT
%% Perturbs param({name}, {original}) -> {perturbed}
:- use_module(config).
:- (   retract(config:param({name}, _))
   ->  true
   ;   true
   ),
   asserta(config:param({name}, {perturbed})).
:- [stack].
:- use_module(product_site_export, [run_product_export_to/1]).
:- product_site_export:run_product_export_to('{outpath}'), halt.
"""

# ---------------------------------------------------------------------------
# Zone inference — determines which chi values this param "touches"
#
# A context is TOUCHED if the param participates in its type decision path.
# For threshold params (chi_floor / chi_ceil / epsilon_floor), this is
# contexts whose chi falls in the param's zone of influence.
# For other params: touched = flipped (lower bound; documented limitation).
#
# This distinction prevents "blind = stable" — the defect that stale
# bifurcation_results.json demonstrated (7 constraints absent from live
# corpus, reporting clean results because subjects were absent).
# ---------------------------------------------------------------------------

def _infer_zone(param: str, old_val: float, new_val: float) -> tuple[float, float] | None:
    """Chi zone [lo, hi] for threshold params; None = use flipped as lower bound."""
    lo = min(old_val, new_val)
    hi = max(old_val, new_val)
    margin = abs(hi - lo) * 0.5 + 0.01  # conservative buffer
    if "chi_floor" in param or "chi_ceil" in param:
        return (lo - margin, hi + margin)
    if "epsilon_floor" in param or "epsilon_ceil" in param:
        return (lo - margin, hi + margin)
    return None


def _load_chi_data(pipeline_path: Path) -> dict[str, dict[str, float]]:
    """Load {reading_id: {power_level: chi}} from pipeline_output.json."""
    try:
        data = json.loads(pipeline_path.read_text())
    except (FileNotFoundError, json.JSONDecodeError):
        return {}
    out: dict[str, dict[str, float]] = {}
    for entry in data.get("per_constraint", []):
        rid = entry.get("id")
        pchi = entry.get("perspective_chi") or {}
        if rid and pchi:
            out[rid] = {obs: v["chi"] for obs, v in pchi.items()
                        if isinstance(v, dict) and v.get("chi") is not None}
    return out


def _power_from_ctx(ctx_key: str) -> str:
    """Extract power level (first component) from a context key."""
    return ctx_key.split("_")[0]


def _compute_touched(
    reading: str,
    all_ctx_keys: list[str],
    chi_data: dict,
    zone: tuple[float, float] | None,
) -> set[str]:
    """Return set of context keys touched by this param."""
    if zone is None:
        return set()  # caller should union with flipped
    lo, hi = zone
    reading_chi = chi_data.get(reading, {})
    touched = set()
    for ck in all_ctx_keys:
        power = _power_from_ctx(ck)
        chi = reading_chi.get(power)
        if chi is not None and lo <= chi <= hi:
            touched.add(ck)
    return touched


# ---------------------------------------------------------------------------
# Core: run one perturbed export and parse it
# ---------------------------------------------------------------------------

def _run_perturbed_export(
    param: str,
    original: float,
    perturbed: float,
    timeout: int,
) -> dict | None:
    """Write overlay, run swipl, return parsed JSON dict or None on error."""
    with tempfile.NamedTemporaryFile(
        suffix=".pl", dir=PROLOG_DIR, mode="w", delete=False
    ) as ovf:
        ovf.write(_OVERLAY_TMPL.format(
            name=param,
            original=original,
            perturbed=perturbed,
            outpath=str(PROLOG_DIR.parent / "outputs" / "perturb_tmp.json"),
        ))
        overlay_path = ovf.name

    out_path = ROOT / "outputs" / "perturb_tmp.json"
    try:
        r = subprocess.run(
            ["swipl", "-g", f"consult('{overlay_path}'), halt."],
            cwd=PROLOG_DIR,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        if not out_path.exists():
            print(f"  [perturb] swipl produced no output for {param}={perturbed}", file=sys.stderr)
            print(f"  stderr: {r.stderr[-500:]}", file=sys.stderr)
            return None
        data = json.loads(out_path.read_text())
        out_path.unlink(missing_ok=True)
        return data
    except subprocess.TimeoutExpired:
        print(f"  [perturb] timeout for {param}={perturbed}", file=sys.stderr)
        return None
    finally:
        Path(overlay_path).unlink(missing_ok=True)


# ---------------------------------------------------------------------------
# Kernel discovery from testsets (cs_kernel_id)
# ---------------------------------------------------------------------------

def _load_kernel_map(testsets_dir: Path) -> dict[str, str]:
    """Return {reading_id: kernel_id} from cs_kernel_id/2 facts in testsets."""
    import re
    pattern = re.compile(r"cs_kernel_id\(\s*(\w+)\s*,\s*(\w+)\s*\)")
    mapping: dict[str, str] = {}
    for pl_file in testsets_dir.glob("*.pl"):
        for m in pattern.finditer(pl_file.read_text()):
            mapping[m.group(1)] = m.group(2)
    return mapping


# ---------------------------------------------------------------------------
# Per-kernel fold-survival computation
# ---------------------------------------------------------------------------

def _compute_kernel_results(
    baseline: dict,
    perturbed_export: dict,
    kernel_map: dict[str, str],
    chi_data: dict,
    zone: tuple[float, float] | None,
) -> dict:
    """
    Compare baseline vs perturbed_export, grouped by kernel.

    Returns {kernel_id: {fold_survival, stable, flipped, touched, coverage, per_reading}}
    """
    # Guard 1B: empty export — zero-reading export produces {} (confirmed from
    # product_site_export.pl write_entries/4 base case). If empty, every reading would
    # appear "not in export" → pert_ctxs={} → all stable → fold_survival=1.0 silently.
    if not perturbed_export:
        raise RuntimeError(
            "perturb: perturbed export is empty — swipl produced a zero-reading JSON. "
            "Corpus may not have loaded (check cwd, corpus_path config, "
            "product_site_export load). Refusing to return fold_survival from empty substrate."
        )

    # Group reading_ids by kernel
    kernel_to_readings: dict[str, list[str]] = {}
    for reading, kernel in kernel_map.items():
        kernel_to_readings.setdefault(kernel, []).append(reading)

    results: dict = {}
    for kernel, readings in sorted(kernel_to_readings.items()):
        total_stable = 0
        total_flipped = 0
        total_touched = 0
        total_all = 0
        per_reading: dict = {}

        for reading in sorted(readings):
            if reading not in baseline:
                continue  # not in export
            base_ctxs: dict = baseline[reading].get("contexts", {})
            pert_data = perturbed_export.get(reading, {})
            pert_ctxs: dict = pert_data.get("contexts", {})
            all_keys = list(base_ctxs.keys())

            # Classify each context
            stable_ctxs, flipped_ctxs = [], []
            for ck in all_keys:
                base_t = base_ctxs.get(ck)
                pert_t = pert_ctxs.get(ck, base_t)  # absent → unchanged
                if pert_t == base_t:
                    stable_ctxs.append(ck)
                else:
                    flipped_ctxs.append((ck, base_t, pert_t))

            # Compute touched (chi-based for threshold params; flipped as fallback)
            touched_set = _compute_touched(reading, all_keys, chi_data, zone)
            # Always include flipped contexts in touched (they definitely participated)
            flipped_keys = {f[0] for f in flipped_ctxs}
            touched_set |= flipped_keys

            n_all = len(all_keys)
            n_flipped = len(flipped_ctxs)
            n_stable = len(stable_ctxs)
            n_touched = len(touched_set)

            total_all += n_all
            total_stable += n_stable
            total_flipped += n_flipped
            total_touched += n_touched

            per_reading[reading] = {
                "stable_contexts": stable_ctxs,
                "flipped_contexts": flipped_ctxs,
                "touched_contexts": sorted(touched_set),
                "n_all": n_all,
            }

        if total_all == 0:
            continue

        results[kernel] = {
            "fold_survival": total_stable / total_all if total_all else 1.0,
            "stable": total_stable,
            "flipped": total_flipped,
            "touched": total_touched,
            "coverage": total_touched / total_all if total_all else 0.0,
            "per_reading": per_reading,
        }

    return results


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------

def perturb(
    param: str,
    values: list[float],
    *,
    baseline: dict | None = None,
    kernels: list[str] | None = None,
    timeout: int = 300,
) -> dict:
    """
    For each value in `values`:
      1. Write Prolog overlay (A1 dialect: retract/asserta on config:param/2)
      2. swipl → product_site_export:run_product_export_to(tmp_path)
      3. Parse tmp JSON → per-reading contexts for each cs_kernel_id-linked kernel
      4. fold_survival(kernel, value) = stable / all_contexts
         touched(kernel, value) = contexts where param is on the decision path
         coverage(kernel, value) = touched / all_contexts

    Coverage > 0 with fold_survival = 1.0 means "param reached decision boundary,
    type held" (ε-intercept or similar). Coverage = 0 means "param did not reach
    this kernel's decision path at this value" — blind, not stable.
    """
    if baseline is None:
        baseline = json.loads(ORBITS_PATH.read_text())

    # Guard 2: stale orbits check — compare corpus_hash stamped in orbits file against
    # current testsets. Mismatch means orbits were computed against a different corpus.
    stored_hash = baseline.get("corpus_hash")
    if stored_hash is None:
        print(
            f"[perturb] WARNING: {ORBITS_PATH.name} has no corpus_hash — "
            "orbits predate this guard. Staleness unverifiable. "
            "Regenerate orbits and re-run python3 python/run_pipeline.py to stamp.",
            file=sys.stderr,
        )
    else:
        current_hash = _compute_corpus_hash(PROLOG_DIR / "testsets")
        if stored_hash != current_hash:
            raise RuntimeError(
                f"perturb: product_site_orbits.json is stale — "
                f"stored corpus_hash={stored_hash!r} but current testsets hash={current_hash!r}. "
                "Regenerate orbits: cd prolog && swipl -g "
                "'[stack],[product_site_export],run_product_export,halt'. "
                "Then re-run python3 python/run_pipeline.py to re-stamp corpus_hash."
            )

    baseline_hash = hashlib.sha256(
        json.dumps(baseline, sort_keys=True).encode()
    ).hexdigest()[:12]

    # Read current param value from config.pl
    import re
    config_text = (PROLOG_DIR / "config.pl").read_text()
    m = re.search(rf"^\s*param\(\s*{re.escape(param)}\s*,\s*(-?\d+(?:\.\d+)?)\s*\)\.",
                  config_text, re.MULTILINE)
    if not m:
        raise ValueError(f"param({param}, ...) not found in config.pl")
    original = float(m.group(1))

    chi_data = _load_chi_data(PIPELINE_PATH)
    kernel_map = _load_kernel_map(PROLOG_DIR / "testsets")
    if kernels is not None:
        kernel_map = {r: k for r, k in kernel_map.items() if k in kernels}

    # Guard 1A: empty kernel_map — no cs_kernel_id facts found. Silently returning
    # empty results here would look like "all stable" on zero kernels.
    if not kernel_map:
        raise ValueError(
            "perturb: kernel_map is empty — no cs_kernel_id facts found in testsets/. "
            "Check that prolog/testsets/*.pl files have cs_kernel_id/2 facts and "
            "that PROLOG_DIR points to the correct directory. "
            "If kernels= was passed, verify the kernel IDs exist in testsets."
        )

    all_results: dict = {}
    for val in values:
        print(f"  [perturb] {param}: {original} → {val}", file=sys.stderr)
        zone = _infer_zone(param, original, val)
        if val == original:
            # Identity: compare baseline against itself
            kr = _compute_kernel_results(baseline, baseline, kernel_map, chi_data, zone)
        else:
            exported = _run_perturbed_export(param, original, val, timeout)
            if exported is None:
                all_results[val] = {"error": "export_failed"}
                continue
            kr = _compute_kernel_results(baseline, exported, kernel_map, chi_data, zone)
        all_results[val] = kr

    return {
        "param": param,
        "original": original,
        "baseline_hash": baseline_hash,
        "corpus_hash": _compute_corpus_hash(PROLOG_DIR / "testsets"),
        "values": values,
        "results": all_results,
    }


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def _summarize(result: dict) -> None:
    param = result["param"]
    print(f"\nParam: {param}  (original={result['original']})")
    print(f"Baseline hash: {result['baseline_hash']}")
    for val, kr in result["results"].items():
        if isinstance(kr, dict) and "error" in kr:
            print(f"\n  value={val}: ERROR — {kr['error']}")
            continue
        # Show kernels sorted by fold_survival asc (most affected first)
        kernels_sorted = sorted(
            ((k, v) for k, v in kr.items()),
            key=lambda x: (x[1]["fold_survival"], -x[1]["coverage"])
        )
        affected = [(k, v) for k, v in kernels_sorted if v["fold_survival"] < 1.0]
        touched_but_stable = [(k, v) for k, v in kernels_sorted
                              if v["fold_survival"] == 1.0 and v["coverage"] > 0.0]
        blind = [(k, v) for k, v in kernels_sorted
                 if v["fold_survival"] == 1.0 and v["coverage"] == 0.0]
        print(f"\n  value={val}: {len(affected)} kernels affected, "
              f"{len(touched_but_stable)} touched-but-stable, "
              f"{len(blind)} blind/stable")
        if affected:
            print("  Affected kernels (fold_survival < 1.0):")
            for k, v in affected[:10]:
                print(f"    {k}: fold_survival={v['fold_survival']:.3f} "
                      f"coverage={v['coverage']:.3f} "
                      f"flipped={v['flipped']} touched={v['touched']}")
        if touched_but_stable:
            print(f"  Touched-but-stable kernels (showing ≤5): "
                  + ", ".join(k for k, _ in touched_but_stable[:5]))


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--param", required=True, help="config.pl param name")
    ap.add_argument("--values", nargs="+", type=float, required=True,
                    help="param values to sweep (include current value for identity check)")
    ap.add_argument("--kernels", nargs="*", help="kernel_ids to restrict to (default: all)")
    ap.add_argument("--timeout", type=int, default=300, help="swipl timeout per run (s)")
    ap.add_argument("--json-out", help="write full result JSON to this path")
    a = ap.parse_args()

    result = perturb(
        a.param,
        a.values,
        kernels=a.kernels if a.kernels else None,
        timeout=a.timeout,
    )
    _summarize(result)
    if a.json_out:
        Path(a.json_out).write_text(json.dumps(result, indent=2))
        print(f"\nFull results written to {a.json_out}")


if __name__ == "__main__":
    main()
