#!/usr/bin/env python3
"""Kernel/reading orbit operator (OQ-150 → OQ-53 transpose surface).

The first-class cross-kernel grouping operation OQ-53 asks for, grounded in the
OQ-150 twin measurement (audits/2026-06-20_kernel_reading_orbits/). Groups the
loaded corpus's readings (across kernels) and kernels by each declared orbit-key
and materializes outputs/{reading,kernel}_orbits.json.

DISPOSITION (operator ruling 2026-06-20, OQ-56 / OQ-53):
  - Tier 1 (membership-reproducible at the extraction baseline ~0.72) is the BUILT,
    DECLARED surface: observer-signature (reading) + obstruction-class (kernel).
  - Tier 2 (above-chance but membership-fragile) is REPORTED model-relative: every
    Tier-2 orbit record carries its own twin_agreement number INLINE (not a legend),
    per the operator's [EDGE] — the model-relative flag must not degrade to a heading.

SOURCES (single-canonical, no re-derivation — Build Discipline Pattern 2):
  - outputs/pipeline_output.json : R1/R2/R3/R4/R5 + K1 (the keys the pipeline already
    serialises per constraint). Read, never recomputed here.
  - outputs/kernel_obstruction.json : the obstruction-class + grounding-profile keys
    that pipeline_output.json does not serialise (produced by the Prolog step
    kernel_orbit_export.pl). Absent ⇒ those keys emit source_missing, NOT a silent
    empty orbit (fail-closed, Pattern 5).

Freshness (Pattern 1 + OQ-29): the outputs stamp corpus_hash and the source run's
pipeline_run_at; a consumer asserts same-run before joining.
"""
import json
from collections import Counter, defaultdict
from pathlib import Path

from corpus_hash import compute_corpus_hash

ROOT = Path(__file__).resolve().parents[1]
OUTPUTS = ROOT / "outputs"
TESTSETS = ROOT / "prolog" / "testsets"
SEATS = ["powerless", "moderate", "institutional", "analytical"]
# priority cascade (CLAUDE.md) for R4 dominant-seat-type tie-breaks
CASCADE = ["unknown", "naturalized", "piton", "tangled_rope", "rope", "scaffold",
           "snare", "mountain"]

# OQ-150 twin-measured draw-stability constants — the declared metadata each orbit
# carries inline. agreement = cross-twin per-unit membership agreement (haiku/flash,
# n=960); baseline = extraction reproducibility ~0.721 (the declarability floor).
BASELINE = 0.721
KEY_META = {
    # reading-orbit keys (unit = constraint id)
    "observer_signature":      dict(tier=1, twin_agreement=0.722, family="observer"),
    "terminal_observer":       dict(tier=2, twin_agreement=0.566, family="observer"),
    "apparatus_cs_pattern":    dict(tier=2, twin_agreement=0.487, family="committer"),
    "terminal_committer":      dict(tier=2, twin_agreement=0.300, family="committer"),
    "axiom_grounding_profile": dict(tier=2, twin_agreement=0.272, family="committer"),
    "seat_role_vector":        dict(tier=2, twin_agreement=0.245, family="stakeholder"),
    # kernel-orbit keys (unit = kernel)
    "obstruction_class":       dict(tier=1, twin_agreement=0.734, family="committer"),
    "kernel_structure_signature": dict(tier=2, twin_agreement=0.134, family="structure"),
}
for _m in KEY_META.values():
    _m["declarable"] = _m["twin_agreement"] >= 0.70  # baseline floor
    _m["model_relative"] = not _m["declarable"]


def _cascade_rank(t):
    return CASCADE.index(t) if t in CASCADE else -1


def _sources(twin=None):
    """Resolve (pipeline_path, obstruction_path, testsets_dir, out_reading, out_kernel).

    twin=None -> the live run (pipeline_output.json + testsets/), the wired default.
    twin in {haiku,flash} -> the twin outputs + twin testsets, for testing on the
    meaningful orbit populations (the live corpus has only ~3 multi-reading kernels).
    """
    if twin is None:
        return (OUTPUTS / "pipeline_output.json", OUTPUTS / "kernel_obstruction.json",
                TESTSETS, OUTPUTS / "reading_orbits.json", OUTPUTS / "kernel_orbits.json")
    return (OUTPUTS / f"pipeline_output.{twin}.json",
            OUTPUTS / f"kernel_obstruction.{twin}.json",
            ROOT / "prolog" / f"testsets_{twin}",
            OUTPUTS / f"reading_orbits.{twin}.json",
            OUTPUTS / f"kernel_orbits.{twin}.json")


def _load_pipeline(path):
    d = json.loads(Path(path).read_text(encoding="utf-8"))
    pc = d["per_constraint"]
    items = list(pc.values()) if isinstance(pc, dict) else pc
    return d.get("manifest", {}), {v["id"]: v for v in items}


def _load_obstruction(path):
    p = Path(path)
    if not p.exists():
        return None
    return json.loads(p.read_text(encoding="utf-8"))


# ---- reading-orbit key extractors (unit = constraint id) ----
def _k_observer_signature(v, obs):
    return v.get("signature")

def _k_terminal_observer(v, obs):
    p = v.get("perspectives") or {}
    ts = [p.get(s) for s in SEATS if p.get(s)]
    if not ts:
        return None
    cnt = Counter(ts)
    top = max(cnt.values())
    return max((t for t, c in cnt.items() if c == top), key=_cascade_rank)

def _k_apparatus(v, obs):
    return v.get("cs_pattern")

def _k_terminal_committer(v, obs):
    return v.get("cs_drift_terminal")

def _k_seat_role_vector(v, obs):
    p = v.get("perspectives") or {}
    return "|".join(str(p.get(s)) for s in SEATS)

def _k_grounding_profile(v, obs):
    if obs is None:
        return None  # source_missing handled by caller
    return (obs.get("grounding") or {}).get(v["id"])

READING_KEYS = {
    "observer_signature": _k_observer_signature,
    "terminal_observer": _k_terminal_observer,
    "apparatus_cs_pattern": _k_apparatus,
    "terminal_committer": _k_terminal_committer,
    "seat_role_vector": _k_seat_role_vector,
    "axiom_grounding_profile": _k_grounding_profile,
}


def _group(rows, extractor, obs):
    orbits = defaultdict(list)
    unkeyed = 0
    for cid, v in rows.items():
        lab = extractor(v, obs)
        if lab is None:
            unkeyed += 1
            continue
        orbits[lab].append(cid)
    return orbits, unkeyed


def _orbit_records(orbits, meta, source_missing=False):
    """One record per orbit; meta (tier, twin_agreement, ...) stamped INLINE on each."""
    recs = []
    for lab, ids in sorted(orbits.items(), key=lambda kv: -len(kv[1])):
        recs.append({
            "label": lab, "size": len(ids), "members": sorted(ids),
            "tier": meta["tier"], "twin_agreement": meta["twin_agreement"],
            "model_relative": meta["model_relative"],
        })
    return recs


def _kernels(rows):
    by = defaultdict(list)
    for cid in rows:
        by[cid.split("__", 1)[0]].append(cid)
    return {k: sorted(v) for k, v in by.items()}


def _depth_vector(rows, ids):
    out = []
    for s in SEATS:
        ts = {(rows[c].get("perspectives") or {}).get(s) for c in ids}
        ts.discard(None)
        out.append(1 if len(ts) >= 2 else 0)
    return tuple(out)


def build(twin=None):
    pipeline_path, obstruction_path, testsets_dir, out_reading, out_kernel = _sources(twin)
    manifest, rows = _load_pipeline(pipeline_path)
    obs = _load_obstruction(obstruction_path)
    # Same-run guard (Pattern 1: assert same-run before joining). A kernel_obstruction.json
    # left from a prior run would silently mis-join; fail-closed by dropping it to source_missing
    # when its n_constraints disagrees with the pipeline manifest.
    obs_stale = None
    if obs is not None:
        oc = obs.get("n_constraints")
        if oc is not None and oc != manifest.get("n_constraints"):
            obs_stale = f"stale: obstruction n_constraints {oc} != pipeline {manifest.get('n_constraints')}"
            obs = None
    corpus_hash = compute_corpus_hash(testsets_dir)
    common_stamp = {
        "corpus_hash": corpus_hash,
        "source_pipeline_run_at": manifest.get("pipeline_run_at"),
        "source_n_constraints": manifest.get("n_constraints"),
        "code_commit_short": manifest.get("code_commit_short"),
        "baseline": BASELINE,
        "disposition": "tier1=declared; tier2=reported model-relative (numbers inline) "
                       "(operator ruling 2026-06-20, OQ-56/OQ-53)",
    }

    # ---- reading orbits ----
    reading = dict(common_stamp); reading["keys"] = {}
    for name, ext in READING_KEYS.items():
        meta = KEY_META[name]
        if name == "axiom_grounding_profile" and obs is None:
            reading["keys"][name] = {"tier": meta["tier"],
                                     "twin_agreement": meta["twin_agreement"],
                                     "model_relative": meta["model_relative"],
                                     "source_missing": obs_stale or "kernel_obstruction.json absent"}
            continue
        orbits, unkeyed = _group(rows, ext, obs)
        reading["keys"][name] = {
            "tier": meta["tier"], "twin_agreement": meta["twin_agreement"],
            "model_relative": meta["model_relative"], "family": meta["family"],
            "n_orbits": len(orbits), "unkeyed": unkeyed,
            "orbits": _orbit_records(orbits, meta),
        }

    # ---- kernel orbits ----
    kernels = _kernels(rows)
    multi = {k: v for k, v in kernels.items() if len(v) >= 2}
    kernel = dict(common_stamp)
    kernel["n_kernels"] = len(kernels)
    kernel["n_multi_reading_kernels"] = len(multi)
    kernel["keys"] = {}

    # K1 structure-signature (from pipeline_output)
    meta = KEY_META["kernel_structure_signature"]
    k1 = defaultdict(list)
    for k, ids in kernels.items():
        sig = (len(ids), _depth_vector(rows, ids),
               tuple(sorted(Counter(rows[c].get("claimed_type") for c in ids).items())))
        k1[str(sig)].append(k)
    kernel["keys"]["kernel_structure_signature"] = {
        "tier": meta["tier"], "twin_agreement": meta["twin_agreement"],
        "model_relative": meta["model_relative"], "family": meta["family"],
        "n_orbits": len(k1),
        "orbits": _orbit_records(k1, meta),
    }

    # obstruction-class (Tier 1; from kernel_obstruction.json)
    meta = KEY_META["obstruction_class"]
    if obs is None:
        kernel["keys"]["obstruction_class"] = {
            "tier": meta["tier"], "twin_agreement": meta["twin_agreement"],
            "model_relative": meta["model_relative"],
            "source_missing": obs_stale or "kernel_obstruction.json absent (run kernel_orbit_export.pl)"}
    else:
        oc = defaultdict(list)
        for k, status in (obs.get("obstruction") or {}).items():
            oc[status].append(k)
        kernel["keys"]["obstruction_class"] = {
            "tier": meta["tier"], "twin_agreement": meta["twin_agreement"],
            "model_relative": meta["model_relative"], "family": meta["family"],
            "n_orbits": len(oc),
            "orbits": _orbit_records(oc, meta),
        }

    out_reading.write_text(json.dumps(reading, indent=2), encoding="utf-8")
    out_kernel.write_text(json.dumps(kernel, indent=2), encoding="utf-8")
    return reading, kernel


def main():
    import sys
    twin = None
    if "--twin" in sys.argv:
        twin = sys.argv[sys.argv.index("--twin") + 1]
    reading, kernel = build(twin)
    tag = twin or "live"
    print(f"[{tag}] reading_orbits: {len(reading['keys'])} keys; "
          f"kernel_orbits: {kernel['n_multi_reading_kernels']} multi-reading kernels, "
          f"{len(kernel['keys'])} keys. corpus_hash={reading['corpus_hash']}")


if __name__ == "__main__":
    main()
