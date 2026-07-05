#!/usr/bin/env python3
"""OQ-88 sweep: false-mountain as kernel-false-negative detector.

Pre-registered in audits/2026-07-04_oq88_false_mountain_detector/PROPOSAL.md (frozen
before this script ran). Phases implemented here:

  1. Live routing partition of the Layer-B set (flat / kernel-routed / routing-unknown /
     routing-ambiguous), fail-closed on no-manifest.
  2. Controls: positive (full-D N=2), dispatch (two-sided), discriminator sanity.
  3. kernel_v1 flinch-tail base rate (Pin 1: base rate, NOT a negative control) —
     reads outputs/pipeline_output_kernel_v1_oq88.json produced by classify_corpus;
     reports NOT RUN if absent.
  4. Three-cell verdict skeleton with per-cell evidence grades.

Read-only over pipeline outputs, manifests, and testset .pl files; writes results JSON
to the audit directory only.
"""
import json
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent.parent
sys.path.insert(0, str(REPO / "python"))
from migrate_kernel_linkage import build_constraint_map  # noqa: E402

OUTPUTS = REPO / "outputs"
MANIFESTS_ROOT = OUTPUTS / "kernel_manifests"
TESTSETS = REPO / "prolog" / "testsets"
KV1 = REPO / "prolog" / "archives" / "datasets" / "kernel_v1"
AUDIT_DIR = REPO / "audits" / "2026-07-04_oq88_false_mountain_detector"

LIVE_OUTPUT = OUTPUTS / "pipeline_output.json"
KV1_OUTPUT = OUTPUTS / "pipeline_output_kernel_v1_oq88.json"
WORLD3_ARCHIVE = REPO / "audits" / "2026-06-11_oq90_piton_refinement" / "pipeline_output.preedit.json"
WORLD3_SIBLINGS = [
    REPO / "audits" / "2026-06-11_oq90_piton_refinement" / p
    for p in ("pipeline_output.phase4_postremoval.json",
              "pipeline_output.refine0.json", "pipeline_output.refine1.json")
]
WORLD3_FLAT_MANIFEST = MANIFESTS_ROOT / "flat" / "world3_recalibration_2024_20260608_171605.manifest.json"
WORLD3_REGIME_DRAW = MANIFESTS_ROOT / "world3_kernel_probe" / "world3_recalibration_2024_20260608_183123.manifest.json"
CHINA_FLAT_MANIFEST = MANIFESTS_ROOT / "flat" / "china_blue_collar_wage_convergence_2026_20260608_163143.manifest.json"
WORLD3_PL = REPO / "prolog" / "archives" / "datasets" / "kernel_v2_test2" / "pl" / "collapse_mechanism_ambiguity.pl"

# Pre-registered discriminator tiers (PROPOSAL.md — anchor-derivation caveat applies).
TIER1_RE = re.compile(r"paradigm|regime|belief[ _-]?system|worldview|ideolog|reorganiz|social[ _-]?order", re.I)
TIER2_RE = re.compile(r"natural|construct|contingen|inevitab|necessit|irreducib", re.I)

SUFFIX_RE = re.compile(r"_c\d+$")


def norm_id(cid):
    return SUFFIX_RE.sub("", cid)


# ---------------------------------------------------------------------------
# Layer B
# ---------------------------------------------------------------------------

def layer_b(pipeline_path):
    """Partition a pipeline output's mountain->rope entries.

    Returns dict: manifest, firing (alert-keyed predicate holds), undetermined
    (mountain->rope, no type_1_false_summit alert — the OQ-51 h1-null edge class).
    """
    d = json.loads(pipeline_path.read_text(encoding="utf-8"))
    firing, undetermined = [], []
    for c in d.get("per_constraint", []):
        cid = c.get("id") or c.get("constraint_id")
        if c.get("claimed_type") != "mountain":
            continue
        top = (c.get("maxent") or {}).get("top_type") if isinstance(c.get("maxent"), dict) else None
        top = top or c.get("maxent_top_type")
        if top != "rope":
            continue
        alerts = ((c.get("verdict_join") or {}).get("alerts")) or []
        fs = [a for a in alerts if a.get("type") == "type_1_false_summit"]
        row = {"id": cid, "norm_id": norm_id(cid),
               "alerts": [(a.get("type"), a.get("severity")) for a in alerts],
               "h1_band": c.get("h1_band", "KEY-MISSING")}
        (firing if fs else undetermined).append(row)
    return {"manifest": d.get("manifest", {}), "firing": firing, "undetermined": undetermined,
            "n_per_constraint": len(d.get("per_constraint", []))}


# ---------------------------------------------------------------------------
# Layer A: manifest walk
# ---------------------------------------------------------------------------

def flat_walk():
    """All manifests with falsy is_contested_kernel: norm claim_id -> [manifest paths].

    Complement of build_constraint_map(): plain-string generation_sequence entries.
    Dict-shaped axes inside a falsy-CSR manifest would be anomalous; collected too
    (by claim_id) so nothing is silently dropped.
    """
    fmap = {}
    for mf in sorted(MANIFESTS_ROOT.glob("*/*.manifest.json")):
        try:
            m = json.loads(mf.read_text(encoding="utf-8"))
        except Exception as e:
            print(f"WARNING: cannot parse {mf}: {e}", file=sys.stderr)
            continue
        csr = m.get("commitment_system_recognition") or {}
        if isinstance(csr, dict) and csr.get("is_contested_kernel"):
            continue  # contested: build_constraint_map territory
        for axis in m.get("generation_sequence") or []:
            if isinstance(axis, str):
                cid = axis
            elif isinstance(axis, dict):
                cid = axis.get("claim_id") or axis.get("constraint_id")
            else:
                continue
            if cid:
                fmap.setdefault(norm_id(cid), []).append(str(mf.relative_to(REPO)))
    return fmap


def manifest_omegas(mf_path):
    m = json.loads((REPO / mf_path).read_text(encoding="utf-8")) if not Path(mf_path).is_absolute() \
        else json.loads(Path(mf_path).read_text(encoding="utf-8"))
    out = []
    for o in m.get("omegas") or []:
        if isinstance(o, dict):
            out.append((o.get("id") or "", o.get("description") or ""))
    return out


# ---------------------------------------------------------------------------
# Discriminator: omega sources + tier match
# ---------------------------------------------------------------------------

def pl_omega_spans(pl_path):
    """All omega_variable(...) term texts in a .pl file (qualified /3 facts and the
    rich multi-line blocks). Returns list of raw span strings."""
    text = pl_path.read_text(encoding="utf-8", errors="replace")
    spans = []
    for m in re.finditer(r"omega_variable\s*\(", text):
        i, depth = m.end(), 1
        while i < len(text) and depth:
            if text[i] == "(":
                depth += 1
            elif text[i] == ")":
                depth -= 1
            i += 1
        spans.append(text[m.start():i])
    return spans


def tier_match(texts):
    """texts: iterable of strings. Returns dict with matched tokens per tier."""
    joined = "\n".join(texts)
    return {"tier1": sorted(set(t.lower() for t in TIER1_RE.findall(joined))),
            "tier2": sorted(set(t.lower() for t in TIER2_RE.findall(joined))),
            "present": bool(TIER1_RE.search(joined) or TIER2_RE.search(joined))}


def live_pl_path(cid, testsets_dir=TESTSETS):
    p = testsets_dir / f"{cid}.pl"
    return p if p.exists() else None


def discriminator(cid, flat_manifest_paths, testsets_dir=TESTSETS):
    """Union of manifest omegas (flat manifests carrying this id) + testset .pl
    omega_variable facts. Sources reported separately."""
    mtexts = []
    for mp in flat_manifest_paths:
        for oid, desc in manifest_omegas(mp):
            mtexts.append(f"{oid} {desc}")
    ptexts = []
    plp = live_pl_path(cid, testsets_dir)
    if plp:
        ptexts = pl_omega_spans(plp)
    return {"manifest_omega_match": tier_match(mtexts) if mtexts else None,
            "pl_omega_match": tier_match(ptexts) if ptexts else None,
            "union": tier_match(mtexts + ptexts),
            "n_manifest_omegas": len(mtexts), "n_pl_omega_spans": len(ptexts),
            "pl_source": str(plp.relative_to(REPO)) if plp else None}


# ---------------------------------------------------------------------------
# Per-corpus sweep (twin legs etc.) — same predicate, same maps, same tiers
# ---------------------------------------------------------------------------

def sweep_corpus(label, output_json, testsets_dir):
    """Layer-B + routing partition + discriminator over an arbitrary
    (pipeline output, testsets dir) pair. Per-instrument liveness controls are
    built in (claimed-type distribution + alert-channel coverage), so an empty
    firing set is distinguishable from a reader that never looked."""
    d = json.loads(Path(output_json).read_text(encoding="utf-8"))
    pc = d.get("per_constraint", [])
    lb = layer_b(Path(output_json))

    # liveness controls (didn't-look guards for a possibly-empty firing set)
    claimed = {}
    for c in pc:
        claimed[c.get("claimed_type")] = claimed.get(c.get("claimed_type"), 0) + 1
    n_alert_bearing = sum(1 for c in pc if ((c.get("verdict_join") or {}).get("alerts")))
    n_claimed_mountain = claimed.get("mountain", 0)

    kmap_raw = build_constraint_map()
    kmap = {}
    for cid, (kid, tag) in kmap_raw.items():
        kmap.setdefault(norm_id(cid), []).append((kid, tag))
    fmap = flat_walk()

    partition = {"flat": [], "kernel_routed": [], "routing_unknown": [], "routing_ambiguous": []}
    for row in lb["firing"]:
        nid = row["norm_id"]
        in_k, in_f = nid in kmap, nid in fmap
        entry = dict(row)
        if in_k and in_f:
            entry["kernel"] = kmap[nid]
            entry["flat_manifests"] = fmap[nid]
            partition["routing_ambiguous"].append(entry)
        elif in_k:
            entry["kernel"] = kmap[nid]
            partition["kernel_routed"].append(entry)
        elif in_f:
            entry["flat_manifests"] = fmap[nid]
            entry["discriminator"] = discriminator(row["id"], fmap[nid], testsets_dir)
            partition["flat"].append(entry)
        else:
            entry["discriminator_pl_only"] = discriminator(row["id"], [], testsets_dir)
            partition["routing_unknown"].append(entry)
    und = []
    for row in lb["undetermined"]:
        nid = row["norm_id"]
        row = dict(row)
        row["routing"] = ("ambiguous" if nid in kmap and nid in fmap else
                          "kernel" if nid in kmap else "flat" if nid in fmap else "unknown")
        row["discriminator_pl_only"] = discriminator(row["id"], [], testsets_dir)
        und.append(row)

    results = {
        "corpus_label": label,
        "manifest": {k: lb["manifest"].get(k) for k in
                     ("pipeline_run_at", "n_constraints", "code_commit_short",
                      "code_dirty", "corpus_path")},
        "liveness_controls": {
            "claimed_type_distribution": claimed,
            "n_claimed_mountain": n_claimed_mountain,
            "n_alert_bearing": n_alert_bearing,
            "n_per_constraint": len(pc),
        },
        "partition": partition,
        "partition_counts": {k: len(v) for k, v in partition.items()},
        "undetermined_bucket": und,
        "pin1_note": "twin/archive legs without manifest lineage fail-close to "
                     "routing_unknown; they contribute to base-rate characterization, "
                     "never to D",
    }
    out = AUDIT_DIR / f"oq88_sweep_{label}.json"
    out.write_text(json.dumps(results, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"results -> {out.relative_to(REPO)}")
    summary = {k: v for k, v in results.items() if k not in ("partition", "undetermined_bucket")}
    summary["firing_ids"] = [r["id"] for r in lb["firing"]]
    summary["undetermined_ids"] = [r["id"] for r in lb["undetermined"]]
    print(json.dumps(summary, indent=2))
    return results


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    results = {"proposal": "PROPOSAL.md (frozen 2026-07-04, pre-run)"}

    # ---- Live Layer B ----
    live = layer_b(LIVE_OUTPUT)
    results["live_manifest"] = {k: live["manifest"].get(k) for k in
                                ("pipeline_run_at", "n_constraints", "code_commit_short", "code_dirty")}
    results["live_layer_b_firing"] = live["firing"]
    results["live_layer_b_undetermined"] = live["undetermined"]

    # ---- Layer A maps ----
    kmap_raw = build_constraint_map()
    kmap = {}
    for cid, (kid, tag) in kmap_raw.items():
        kmap.setdefault(norm_id(cid), []).append((kid, tag))
    fmap = flat_walk()

    # ---- Phase 1: routing partition ----
    partition = {"flat": [], "kernel_routed": [], "routing_unknown": [], "routing_ambiguous": []}
    for row in live["firing"]:
        nid = row["norm_id"]
        in_k, in_f = nid in kmap, nid in fmap
        entry = dict(row)
        if in_k and in_f:
            entry["kernel"] = kmap[nid]
            entry["flat_manifests"] = fmap[nid]
            partition["routing_ambiguous"].append(entry)
        elif in_k:
            entry["kernel"] = kmap[nid]
            partition["kernel_routed"].append(entry)
        elif in_f:
            entry["flat_manifests"] = fmap[nid]
            entry["discriminator"] = discriminator(row["id"], fmap[nid])
            partition["flat"].append(entry)
        else:
            # fail-closed: no manifest anywhere — never assumed flat
            entry["discriminator_pl_only"] = discriminator(row["id"], [])
            partition["routing_unknown"].append(entry)
    results["phase1_partition"] = partition
    results["phase1_counts"] = {k: len(v) for k, v in partition.items()}
    # undetermined-seat bucket routing (reported, not folded)
    und = []
    for row in live["undetermined"]:
        nid = row["norm_id"]
        row = dict(row)
        row["routing"] = ("ambiguous" if nid in kmap and nid in fmap else
                          "kernel" if nid in kmap else "flat" if nid in fmap else "unknown")
        und.append(row)
    results["phase1_undetermined_bucket"] = und

    # ---- Phase 2 controls ----
    controls = {}

    # (a) Positive control, full-D N=2, each layer stated.
    demo = next((r for r in live["firing"] if r["id"] == "demographic_skill_mismatch_c0"), None)
    demo_flat = "demographic_skill_mismatch" in fmap
    demo_not_kernel = "demographic_skill_mismatch" not in kmap
    controls["positive_demographic"] = {
        "layer_b_live": bool(demo), "layer_a_flat": demo_flat,
        "not_kernel_routed": demo_not_kernel,
        "full_D": bool(demo) and demo_flat and demo_not_kernel,
        "flat_manifests": fmap.get("demographic_skill_mismatch"),
    }

    w3 = layer_b(WORLD3_ARCHIVE)
    w3row = next((r for r in w3["firing"] if r["id"] == "collapse_mechanism_ambiguity"), None)
    w3_flat = "collapse_mechanism_ambiguity" in fmap
    w3_not_kernel = "collapse_mechanism_ambiguity" not in kmap
    w3_siblings = {}
    for sp in WORLD3_SIBLINGS:
        sb = layer_b(sp)
        w3_siblings[sp.name] = bool(next((r for r in sb["firing"]
                                          if r["id"] == "collapse_mechanism_ambiguity"), None))
    controls["positive_world3"] = {
        "layer_b_archived": bool(w3row),
        "layer_b_source": str(WORLD3_ARCHIVE.relative_to(REPO)),
        "layer_b_row": w3row,
        "layer_b_sibling_consistency": w3_siblings,
        "layer_a_flat": w3_flat,
        "layer_a_source_present": str(WORLD3_FLAT_MANIFEST.relative_to(REPO)) in (fmap.get("collapse_mechanism_ambiguity") or []),
        "not_kernel_routed": w3_not_kernel,
        "full_D": bool(w3row) and w3_flat and w3_not_kernel,
    }
    controls["positive_full_D_N"] = sum(1 for k in ("positive_demographic", "positive_world3")
                                        if controls[k]["full_D"])

    # (b) Dispatch control, two-sided.
    known_kernel_live = None
    live_ids = {norm_id(r) for r in
                (c.get("id") or c.get("constraint_id")
                 for c in json.loads(LIVE_OUTPUT.read_text(encoding="utf-8"))["per_constraint"])}
    for nid in sorted(kmap):
        if nid in live_ids:
            known_kernel_live = (nid, kmap[nid])
            break
    controls["dispatch"] = {
        "reader_found_known_mountain_rope": bool(demo),
        "join_resolved_china_manifest": str(CHINA_FLAT_MANIFEST.relative_to(REPO)) in (fmap.get("demographic_skill_mismatch") or []),
        "join_resolved_world3_manifest": str(WORLD3_FLAT_MANIFEST.relative_to(REPO)) in (fmap.get("collapse_mechanism_ambiguity") or []),
        "suffix_normalization_exercised": bool(demo) and demo["id"] != demo["norm_id"] and demo_flat,
        "known_kernel_routed_in_kernel_bucket": known_kernel_live,
        "n_kernel_map": len(kmap), "n_flat_map": len(fmap),
    }

    # (c) Discriminator sanity: regime trace capturable on the 183123 draw
    # (cross-draw — representability only, NOT D'-validated-on-World3).
    regime_draw = tier_match([f"{i} {d}" for i, d in manifest_omegas(WORLD3_REGIME_DRAW)])
    full_d_draw = tier_match([f"{i} {d}" for i, d in manifest_omegas(WORLD3_FLAT_MANIFEST)])
    w3_pl = tier_match(pl_omega_spans(WORLD3_PL)) if WORLD3_PL.exists() else None
    controls["discriminator_sanity"] = {
        "world3_183123_draw": regime_draw,
        "world3_171605_full_D_draw": full_d_draw,
        "world3_pl_kernel_v2_test2": w3_pl,
        "note": "183123 = representability witness only (cross-draw, Pin 3); "
                "171605 + .pl reported for the within-draw picture",
    }
    results["phase2_controls"] = controls

    probe_broken = controls["positive_full_D_N"] < 2 or not all(
        v for k, v in controls["dispatch"].items() if isinstance(v, bool))
    results["probe_broken"] = probe_broken

    # ---- Phase 3: kernel_v1 base rate (Pin 1: NOT a negative control) ----
    if KV1_OUTPUT.exists():
        kv1 = layer_b(KV1_OUTPUT)
        kv1_rows = []
        for row in kv1["firing"]:
            plp = KV1 / f"{row['id']}.pl"
            spans = pl_omega_spans(plp) if plp.exists() else []
            row = dict(row)
            row["omega_match"] = tier_match(spans)
            row["n_omega_spans"] = len(spans)
            row["pl_found"] = plp.exists()
            kv1_rows.append(row)
        n_fire = len(kv1_rows)
        n_regime = sum(1 for r in kv1_rows if r["omega_match"]["present"])
        results["phase3_kernel_v1"] = {
            "manifest": {k: kv1["manifest"].get(k) for k in
                         ("pipeline_run_at", "n_constraints", "code_commit_short", "code_dirty")},
            "n_per_constraint": kv1["n_per_constraint"],
            "layer_b_firing_n": n_fire,
            "layer_b_undetermined_n": len(kv1["undetermined"]),
            "regime_omega_present_n": n_regime,
            "flinch_tail_n": n_fire - n_regime,
            "tier1_only_n": sum(1 for r in kv1_rows
                                if r["omega_match"]["tier1"] and not r["omega_match"]["tier2"]),
            "tier2_only_n": sum(1 for r in kv1_rows
                                if r["omega_match"]["tier2"] and not r["omega_match"]["tier1"]),
            "both_tiers_n": sum(1 for r in kv1_rows
                                if r["omega_match"]["tier1"] and r["omega_match"]["tier2"]),
            "rows": kv1_rows,
            "pin1": "base-rate measurement; Layer A uncomputable (no manifests); contributes ZERO to D",
        }
    else:
        results["phase3_kernel_v1"] = {"status": "NOT RUN (classify_corpus output absent)"}

    # ---- write ----
    out = AUDIT_DIR / "oq88_sweep_results.json"
    out.write_text(json.dumps(results, indent=2, ensure_ascii=False), encoding="utf-8")
    print(f"results -> {out.relative_to(REPO)}")
    print(json.dumps({"phase1_counts": results["phase1_counts"],
                      "positive_full_D_N": controls["positive_full_D_N"],
                      "probe_broken": probe_broken,
                      "dispatch": {k: v for k, v in controls["dispatch"].items()},
                      "phase3": {k: v for k, v in results["phase3_kernel_v1"].items()
                                 if k != "rows"}}, indent=2, default=str))


if __name__ == "__main__":
    if len(sys.argv) == 4 and sys.argv[1] == "sweep_corpus":
        # sweep_corpus <label> <pipeline_output_json>; testsets dir = prolog/<label>
        sweep_corpus(sys.argv[2], sys.argv[3], REPO / "prolog" / sys.argv[2])
    else:
        main()
