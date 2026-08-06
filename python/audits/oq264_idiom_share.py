#!/usr/bin/env python3
"""OQ-264 — k-redraw variance floor of the pooled idiom-SHARE observable.

Subcommands (run from repo root; all deterministic, no network, no corpus writes):

  control    Mechanical denominator control: compute D per manifest from the
             pre-registered formula, compare against expected values (plan +
             SCORING.md baselines). Non-halting on mismatch (pre-registered fix
             path: wrong-formula hypothesis -> fix -> re-commit with note);
             only an unresolvable mismatch halts the audit.
  calibrate  Print the achievable share lattice per gate draw, the achievable
             range lattice, and the quantization simulation (stable-null +
             exactly-one-scorer-error) band probabilities for the candidate
             gate thresholds. No judged input consumed. Gate numbers in
             PROPOSAL.md are set only after this output is pasted.
  packet     Emit the pooled cross-file blinded packet (packet.md) + the
             label->unit mapping (mapping.json). Labels are a shuffle seeded
             from the packet content hash; a seeded subset of entries is
             silently duplicated (scorer-variance instrument); planted-control
             entries from planted_control.manifest.json are mixed in.
             The mapping file must NOT be read by the scorer until calls.json
             is committed.
  compute    Unblind: read calls.json + mapping.json; run the planted-control
             HALT check; compute duplicate agreement, per-draw shares, TAG/D/
             share ranges, component residuals, sensitivity, gate verdict with
             the sensitivity modifier, mechanical comparator ranges, and
             (optionally, --holdout) holdout reliability vs SCORING.md calls.

Provenance: OQ-264 (ISSUES.md), plan rev 2; pre-registration in
audits/2026-08-06_oq264_kredraw_variance/PROPOSAL.md.
"""

import argparse
import hashlib
import json
import random
import sys
from fractions import Fraction
from itertools import product
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
AUDIT_DIR = REPO / "audits/2026-08-06_oq264_kredraw_variance"

# (file_key, draw_key, path, gate_draw) -- gate draws are the Biopower triple.
MANIFESTS = [
    ("biopower", "base",
     "audits/2026-08-03_kritik_ingest/biopower_k_nhi_debate_2026_20260803_102652.manifest.json",
     True),
    ("biopower", "r1",
     "audits/2026-08-05_oq259_emphasis_discriminator/biopower_healthcare_kernel_2026_20260805_144612.manifest.json",
     True),
    ("biopower", "r2",
     "audits/2026-08-05_oq259_emphasis_discriminator/biopower_nhi_debate_2026_20260805_144823.manifest.json",
     True),
    ("capk", "base",
     "audits/2026-08-03_kritik_ingest/capitalism_k_ndi2026_20260803_102445.manifest.json",
     False),
    ("capk", "r1",
     "audits/2026-08-05_oq259_emphasis_discriminator/capitalism_k_debate_2026_20260805_145017.manifest.json",
     False),
    ("capk", "r2",
     "audits/2026-08-05_oq259_emphasis_discriminator/capitalism_kritik_ndi2026_20260805_145128.manifest.json",
     False),
]

# Mechanical control expectations (pre-registered; plan "Draw denominators").
EXPECTED_D = {
    ("biopower", "base"): 6, ("biopower", "r1"): 4, ("biopower", "r2"): 6,
    ("capk", "base"): 6, ("capk", "r1"): 4, ("capk", "r2"): 3,
}
# SCORING.md 2026-08-03 baseline denominators (its per-file scoring tables):
# Cap K NW "manifest = 5 kernel readings + 1 selected axis" -> 6;
# Biopower NW "manifest = 6 kernel readings selected" -> 6.
SCORING_MD_BASELINE_D = {"biopower": 6, "capk": 6}

# Gate bands (decimals; range lattice is k/12). Set from the calibrate output,
# per PROPOSAL.md section 5 -- do not edit without re-running calibrate.
PASS_MAX = Fraction(3, 12)   # 0.25  -- explainable by quantization + <=1 flip
FAIL_MIN = Fraction(6, 12)   # 0.50  -- unreachable under stable null + 1 flip
COMPONENT_RESIDUAL_MAX = 1.0  # max |TAG(d) - s_pooled*D(d)| allowed for PASS

TAG_CLASSES = {"tag", "tag-leaning"}
ALL_CLASSES = {"tag", "tag-leaning", "mixed", "card"}
N_DUPLICATES = 6


def load_manifest(relpath):
    return json.loads((REPO / relpath).read_text())


def manifest_units(m):
    """Pre-registered denominator formula.

    Units = kernel readings UNION selected axes whose claim_id is not among
    kernel reading_ids. Deferred axes excluded. Zero-kernel manifests (no
    contested kernel / empty readings) use the selected-axes FALLBACK
    population: a different unit population, excluded from the share range,
    reported as a categorical outcome (kernel-minting churn).

    Returns (units, zero_kernel) where each unit is a dict with kind/name/
    text fields.
    """
    csr = m.get("commitment_system_recognition") or {}
    readings = csr.get("readings") or []
    kernel_ok = bool(csr.get("is_contested_kernel")) and bool(readings)
    units = []
    if kernel_ok:
        kr_ids = {r["reading_id"] for r in readings}
        for r in readings:
            units.append({
                "kind": "kernel_reading",
                "name": r["reading_id"],
                "text_a": r.get("commitment", ""),
                "text_b": r.get("authority_grounding", ""),
                "text_c": r.get("expected_structural_delta", ""),
            })
        for a in m.get("axes", []):
            if a.get("selected") and a["claim_id"] not in kr_ids:
                units.append({
                    "kind": "selected_axis",
                    "name": a["claim_id"],
                    "text_a": a.get("human_readable", ""),
                    "text_b": a.get("structural_delta", ""),
                    "text_c": a.get("hypothesis", ""),
                })
        return units, False
    # zero-kernel fallback population
    for a in m.get("axes", []):
        if a.get("selected"):
            units.append({
                "kind": "selected_axis",
                "name": a["claim_id"],
                "text_a": a.get("human_readable", ""),
                "text_b": a.get("structural_delta", ""),
                "text_c": a.get("hypothesis", ""),
            })
    return units, True


def mechanical_observables(m):
    csr = m.get("commitment_system_recognition") or {}
    return {
        "contested_kernel": bool(csr.get("is_contested_kernel")),
        "n_kernel_readings": len(csr.get("readings") or []),
        "n_selected_axes": sum(1 for a in m.get("axes", []) if a.get("selected")),
        "n_deferred_axes": len(m.get("deferred_axes") or []),
        "scope_model": (m.get("_provenance") or {}).get("scope_model"),
        "scope_prompt_commit": (m.get("_provenance") or {}).get("scope_prompt_commit"),
        "scope_schema_commit": (m.get("_provenance") or {}).get("scope_schema_commit"),
    }


def cmd_control(_args):
    print("== Mechanical denominator control (pre-registered formula) ==")
    ok = True
    per_file_base = {}
    for fk, dk, path, _gate in MANIFESTS:
        m = load_manifest(path)
        units, zero_kernel = manifest_units(m)
        d = len(units)
        exp = EXPECTED_D[(fk, dk)]
        mark = "PASS" if d == exp else "MISMATCH"
        if d != exp:
            ok = False
        zk = " [ZERO-KERNEL fallback population]" if zero_kernel else ""
        print(f"  {fk}/{dk}: D={d} expected={exp} -> {mark}{zk}")
        if dk == "base":
            per_file_base[fk] = d
    print("-- SCORING.md 2026-08-03 baseline denominators --")
    for fk, exp in SCORING_MD_BASELINE_D.items():
        d = per_file_base.get(fk)
        mark = "PASS" if d == exp else "MISMATCH"
        if d != exp:
            ok = False
        print(f"  {fk}/base vs SCORING.md: D={d} expected={exp} -> {mark}")
    print("-- Model-version / provenance identity check --")
    provs = set()
    for fk, dk, path, _gate in MANIFESTS:
        mo = mechanical_observables(load_manifest(path))
        provs.add((mo["scope_model"], mo["scope_prompt_commit"], mo["scope_schema_commit"]))
        print(f"  {fk}/{dk}: model={mo['scope_model']} prompt={str(mo['scope_prompt_commit'])[:8]} "
              f"schema={str(mo['scope_schema_commit'])[:8]}")
    print(f"  distinct (model,prompt,schema) triples: {len(provs)}"
          + (" -> identical across all six" if len(provs) == 1 else " -> DIFFER (version effect candidate)"))
    print(f"== control: {'ALL PASS' if ok else 'MISMATCH (pre-registered fix path applies)'} ==")
    return 0 if ok else 1


def gate_draw_denoms():
    return [EXPECTED_D[(fk, dk)] for fk, dk, _p, gate in MANIFESTS if gate]


def share_range(tags, denoms):
    shares = [Fraction(t, d) for t, d in zip(tags, denoms)]
    return max(shares) - min(shares)


def band_of(rng):
    if rng <= PASS_MAX:
        return "PASS"
    if rng >= FAIL_MIN:
        return "FAIL"
    return "INDET"


def component_ok(tags, denoms):
    """Pass-branch component rule: per-draw TAG counts must be consistent with
    ONE pooled share (max |TAG(d) - s_pooled*D(d)| <= COMPONENT_RESIDUAL_MAX).
    Blocks ratio-stability produced by compensating churn."""
    s_pooled = Fraction(sum(tags), sum(denoms))
    return all(abs(t - s_pooled * d) <= COMPONENT_RESIDUAL_MAX
               for t, d in zip(tags, denoms))


def verdict_raw(tags, denoms):
    """Band verdict before the sensitivity modifier."""
    b = band_of(share_range(tags, denoms))
    if b == "PASS" and not component_ok(tags, denoms):
        return "INDET(component)"
    return b


def verdict_final(tags, denoms, dup_clean, rule="recal"):
    """Final verdict = raw band + sensitivity modifier.

    rule="rev1": plan text as drafted (sensitivity==1 -> INDET regardless of
    band). REJECTED by the quantization simulation (P(INDET)=1.0 under every
    stable null + one flip, and INDET under a perfect scorer at stable
    non-representable s*) -- kept selectable so the calibrate output witnesses
    the rejection.

    rule="recal" (pre-registered, PROPOSAL section 5): FAIL with sensitivity 1
    downgrades to INDET always; PASS with sensitivity 1 stands only if the
    duplicate-measured scorer variance is zero at TAG-side (dup_clean), else
    downgrades to INDET. Sensitivity>=2 verdicts stand.
    """
    raw = verdict_raw(tags, denoms)
    if raw.startswith("INDET"):
        return raw
    s = sensitivity(tags, denoms)
    if s != 1:
        return raw
    if rule == "rev1":
        return "INDET(sens1)"
    if raw == "FAIL":
        return "INDET(sens1-FAIL)"
    return "PASS(sens1)" if dup_clean else "INDET(sens1+scorer-var)"


def sensitivity(tags, denoms):
    """Minimum number of single-unit TAG flips changing the raw verdict."""
    base = verdict_raw(tags, denoms)
    frontier = {tuple(tags)}
    seen = set(frontier)
    for depth in range(1, sum(denoms) + 1):
        nxt = set()
        for cfg in frontier:
            for i, d in enumerate(denoms):
                for delta in (-1, 1):
                    t2 = list(cfg)
                    t2[i] += delta
                    if 0 <= t2[i] <= d:
                        t2 = tuple(t2)
                        if t2 not in seen:
                            seen.add(t2)
                            nxt.add(t2)
        for cfg in nxt:
            if verdict_raw(list(cfg), denoms) != base:
                return depth
        frontier = nxt
    return None


def cmd_calibrate(_args):
    denoms = gate_draw_denoms()
    print(f"== Gate draws (Biopower triple): D = {denoms} ==")
    print("-- Achievable share lattice per draw --")
    for d in sorted(set(denoms)):
        vals = ", ".join(f"{k}/{d}={float(Fraction(k, d)):.3f}" for k in range(d + 1))
        print(f"  D={d}: {vals}")
    ranges = sorted({share_range(list(t), denoms)
                     for t in product(*[range(d + 1) for d in denoms])})
    print("-- Achievable RANGE lattice (max-min over the triple) --")
    print("  " + ", ".join(f"{r.numerator}/{r.denominator}={float(r):.3f}" for r in ranges))
    print(f"-- Candidate bands: PASS range<={float(PASS_MAX)}; FAIL range>={float(FAIL_MIN)}; "
          f"INDET between; component residual max {COMPONENT_RESIDUAL_MAX} --")

    print("-- Quantization simulation: null = share perfectly stable at s*, "
          "scorer makes EXACTLY ONE class flip on one uniformly-chosen unit --")
    print("   rev1 = plan-as-drafted modifier (sens==1 -> INDET regardless of band)")
    print("   recal = recalibrated modifier (FAIL+sens1 -> INDET; PASS+sens1 stands "
          "iff duplicates clean; clean assumed here)")
    print(f"  {'s*':>6} {'TAG_true':>10} |  zero-error verdict (rev1/recal) | "
          f"one-error rev1: P/I/F | one-error recal: P/I/F")
    grid = [Fraction(k, 24) for k in range(0, 25)]
    worst_fail = Fraction(0)
    for s in grid:
        t_true = [min(d, max(0, round(s * d))) for d in denoms]
        v0_rev1 = verdict_final(t_true, denoms, True, rule="rev1")
        v0_recal = verdict_final(t_true, denoms, True, rule="recal")
        outcomes = []  # (prob, tags)
        n_units = sum(denoms)
        for i, d in enumerate(denoms):
            if t_true[i] > 0:  # flip a TAG unit down
                t2 = list(t_true)
                t2[i] -= 1
                outcomes.append((Fraction(t_true[i], n_units), t2))
            if t_true[i] < d:  # flip a non-TAG unit up
                t2 = list(t_true)
                t2[i] += 1
                outcomes.append((Fraction(d - t_true[i], n_units), t2))
        dist = {"rev1": {"PASS": Fraction(0), "INDET": Fraction(0), "FAIL": Fraction(0)},
                "recal": {"PASS": Fraction(0), "INDET": Fraction(0), "FAIL": Fraction(0)}}
        for p, cfg in outcomes:
            for rule in ("rev1", "recal"):
                v = verdict_final(cfg, denoms, True, rule=rule)
                key = "INDET" if v.startswith("INDET") else ("PASS" if v.startswith("PASS") else "FAIL")
                dist[rule][key] += p
        worst_fail = max(worst_fail, dist["recal"]["FAIL"])
        r1, rc = dist["rev1"], dist["recal"]
        print(f"  {float(s):>6.3f} {str(t_true):>10} |  {v0_rev1:>14}/{v0_recal:<12} | "
              f"{float(r1['PASS']):.2f}/{float(r1['INDET']):.2f}/{float(r1['FAIL']):.2f} | "
              f"{float(rc['PASS']):.2f}/{float(rc['INDET']):.2f}/{float(rc['FAIL']):.2f}")
    print(f"-- max P(FAIL) under any stable null + one flip (recal rule): {float(worst_fail):.4f} "
          f"({'calibrated: FAIL unreachable under the null' if worst_fail == 0 else 'MISCALIBRATED -- recalibrate before committing numbers'}) --")

    print("-- Sensitivity examples (flips to change raw verdict; final under recal, clean dups) --")
    for tags in ([3, 2, 3], [4, 2, 3], [4, 2, 4], [4, 1, 3], [5, 1, 3], [6, 1, 3]):
        rng = share_range(tags, denoms)
        print(f"  TAG={tags} shares={[f'{t}/{d}' for t, d in zip(tags, denoms)]} "
              f"range={float(rng):.3f} raw={verdict_raw(tags, denoms)} "
              f"sensitivity={sensitivity(tags, denoms)} "
              f"final={verdict_final(tags, denoms, True)}")
    return 0


def render_entry(label, u):
    lines = [f"### {label}  [{u['kind']}]  `{u['name']}`"]
    field_names = {
        "kernel_reading": ("commitment", "authority_grounding", "expected_structural_delta"),
        "selected_axis": ("human_readable", "structural_delta", "hypothesis"),
    }[u["kind"]]
    for fn, key in zip(field_names, ("text_a", "text_b", "text_c")):
        lines.append(f"- **{fn}:** {u[key]}")
    lines.append("- **idiom call:** _____")
    return "\n".join(lines)


def cmd_packet(_args):
    planted_path = AUDIT_DIR / "planted_control.manifest.json"
    planted = json.loads(planted_path.read_text())["entries"]
    pool = []  # (source_tag, unit)
    for fk, dk, path, _gate in MANIFESTS:
        m = load_manifest(path)
        units, zero_kernel = manifest_units(m)
        for i, u in enumerate(units):
            pool.append({"source": f"{fk}/{dk}", "unit_index": i,
                         "zero_kernel": zero_kernel, "planted": False,
                         "dup_of": None, "unit": u})
    for p in planted:
        pool.append({"source": "planted", "unit_index": p["plant_id"],
                     "zero_kernel": False, "planted": True,
                     "expected_side": p["expected_side"], "dup_of": None,
                     "unit": {"kind": p["style"], "name": p["name"],
                              "text_a": p["text_a"], "text_b": p["text_b"],
                              "text_c": p["text_c"]}})
    # Deterministic seed from the canonical content of the pool.
    canon = json.dumps([e["unit"] for e in pool], sort_keys=True).encode()
    seed = int(hashlib.sha256(canon).hexdigest(), 16) % (2 ** 32)
    rng = random.Random(seed)
    # Seeded silent duplicates (never duplicate a planted entry).
    real_idx = [i for i, e in enumerate(pool) if not e["planted"]]
    dup_sources = rng.sample(real_idx, N_DUPLICATES)
    for i in dup_sources:
        src = pool[i]
        pool.append({"source": src["source"], "unit_index": src["unit_index"],
                     "zero_kernel": src["zero_kernel"], "planted": False,
                     "dup_of": i, "unit": dict(src["unit"])})
    order = list(range(len(pool)))
    rng.shuffle(order)
    labels = [f"E{k + 1:02d}" for k in range(len(pool))]
    entries_md, mapping = [], {}
    for lab, idx in zip(labels, order):
        e = pool[idx]
        entries_md.append(render_entry(lab, e["unit"]))
        mapping[lab] = {k: e[k] for k in e if k != "unit"} | {"name": e["unit"]["name"]}
    header = (
        "# OQ-264 pooled blinded scoring packet\n\n"
        "Score EVERY entry with one idiom class: tag | tag-leaning | mixed | card.\n"
        "Definitions and anchors: PROPOSAL.md section 3 (rubric). File identity is\n"
        "unmaskable from content (score against that file's tag inventory); the\n"
        "blind covers DRAW identity. Do not read mapping.json until calls.json\n"
        f"is committed.\n\nEntries: {len(pool)} (order seeded from content hash; "
        "a seeded subset are silent duplicates; planted controls included).\n")
    packet_text = header + "\n\n".join(entries_md) + "\n"
    (AUDIT_DIR / "packet.md").write_text(packet_text)
    (AUDIT_DIR / "mapping.json").write_text(json.dumps(
        {"seed": seed, "n_entries": len(pool), "labels": mapping}, indent=2))
    print(f"packet.md written: {len(pool)} entries "
          f"({len(real_idx)} real, {len(planted)} planted, {N_DUPLICATES} duplicates)")
    print(f"packet sha256: {hashlib.sha256(packet_text.encode()).hexdigest()}")
    print(f"shuffle seed (content-derived): {seed}")
    print("mapping.json written -- scorer: do NOT open before committing calls.json")
    return 0


def cmd_compute(args):
    calls = json.loads((AUDIT_DIR / "calls.json").read_text())["calls"]
    mapping = json.loads((AUDIT_DIR / "mapping.json").read_text())["labels"]
    assert set(calls) == set(mapping), (
        f"label sets differ: calls-only={set(calls) - set(mapping)}, "
        f"mapping-only={set(mapping) - set(calls)}")
    for lab, c in calls.items():
        assert c["class"] in ALL_CLASSES, f"{lab}: bad class {c['class']}"

    print("== Planted-control check (HALT on failure) ==")
    halt = False
    for lab, info in sorted(mapping.items()):
        if info.get("planted"):
            side = "TAG" if calls[lab]["class"] in TAG_CLASSES else "NON-TAG"
            exp = info["expected_side"]
            mark = "PASS" if side == exp else "FAIL -> HALT"
            if side != exp:
                halt = True
            print(f"  {lab} ({info['name']}): expected {exp}, scored "
                  f"{calls[lab]['class']} ({side}) -> {mark}")
    if halt:
        print("== HALT: planted judged control failed; no further computation ==")
        return 2

    print("== Duplicate-item scorer variance ==")
    n_pairs = agree_exact = agree_side = 0
    orig_label = {}
    for lab, info in mapping.items():
        if info.get("dup_of") is None and not info.get("planted"):
            orig_label[(info["source"], info["unit_index"])] = lab
    for lab, info in sorted(mapping.items()):
        if info.get("dup_of") is not None:
            o = orig_label[(info["source"], info["unit_index"])]
            a, b = calls[o]["class"], calls[lab]["class"]
            n_pairs += 1
            ex = a == b
            sd = (a in TAG_CLASSES) == (b in TAG_CLASSES)
            agree_exact += ex
            agree_side += sd
            print(f"  {o}<->{lab} ({info['name']}): {a} vs {b} "
                  f"exact={'Y' if ex else 'N'} TAG-side={'Y' if sd else 'N'}")
    print(f"  duplicate pairs: {n_pairs}; exact agreement {agree_exact}/{n_pairs}; "
          f"TAG-side agreement {agree_side}/{n_pairs}")

    print("== Per-draw shares (all six) ==")
    per_draw = {}
    for fk, dk, path, gate in MANIFESTS:
        m = load_manifest(path)
        units, zero_kernel = manifest_units(m)
        d = len(units)
        tag = 0
        for i in range(d):
            lab = orig_label[(f"{fk}/{dk}", i)]
            if calls[lab]["class"] in TAG_CLASSES:
                tag += 1
        per_draw[(fk, dk)] = {"D": d, "TAG": tag, "share": Fraction(tag, d),
                              "zero_kernel": zero_kernel, "gate": gate}
        note = " [ZERO-KERNEL: categorical outcome kernel-minting churn; "\
               "fallback share CONTRAST-ONLY, excluded from range]" if zero_kernel else ""
        print(f"  {fk}/{dk}: TAG={tag}/{d} share={float(Fraction(tag, d)):.3f}"
              f"{' [gate draw]' if gate else ''}{note}")

    print("== Gate computation (Biopower triple) ==")
    gate_keys = [(fk, dk) for fk, dk, _p, g in MANIFESTS if g]
    tags = [per_draw[k]["TAG"] for k in gate_keys]
    denoms = [per_draw[k]["D"] for k in gate_keys]
    rng_share = share_range(tags, denoms)
    tag_range = max(tags) - min(tags)
    d_range = max(denoms) - min(denoms)
    s_pooled = Fraction(sum(tags), sum(denoms))
    resid = [float(abs(t - s_pooled * d)) for t, d in zip(tags, denoms)]
    raw = verdict_raw(tags, denoms)
    sens = sensitivity(tags, denoms)
    dup_clean = agree_side == n_pairs  # zero TAG-side disagreement on duplicates
    final = verdict_final(tags, denoms, dup_clean)
    print(f"  TAG counts: {tags}  (TAG range = {tag_range})")
    print(f"  D:          {denoms}  (D range = {d_range})")
    print(f"  shares:     {[f'{t}/{d}={float(Fraction(t, d)):.3f}' for t, d in zip(tags, denoms)]}")
    print(f"  share range = {rng_share.numerator}/{rng_share.denominator} = {float(rng_share):.3f}")
    print(f"  pooled share = {float(s_pooled):.3f}; component residuals = "
          f"{[f'{r:.2f}' for r in resid]} (max allowed {COMPONENT_RESIDUAL_MAX})")
    print(f"  raw band = {raw}; sensitivity (flips to change raw verdict) = {sens}; "
          f"duplicates TAG-side clean = {dup_clean}")
    print(f"  FINAL GATE VERDICT = {final}"
          + ("  [k=3: any PASS is PROVISIONAL by the k-monotonicity clause]"
             if final.startswith("PASS") else ""))

    print("== Cap K triple (churn-extreme CONTRAST; feeds no gate) ==")
    cap_keys = [(fk, dk) for fk, dk, _p, g in MANIFESTS if fk == "capk"]
    non_zk = [k for k in cap_keys if not per_draw[k]["zero_kernel"]]
    for k in cap_keys:
        pd = per_draw[k]
        print(f"  {k[0]}/{k[1]}: TAG={pd['TAG']}/{pd['D']} share={float(pd['share']):.3f}"
              + (" [zero-kernel, contrast-only]" if pd["zero_kernel"] else ""))
    if len(non_zk) >= 2:
        c_tags = [per_draw[k]["TAG"] for k in non_zk]
        c_denoms = [per_draw[k]["D"] for k in non_zk]
        print(f"  non-zero-kernel Cap share range = "
              f"{float(share_range(c_tags, c_denoms)):.3f} (contrast only, n={len(non_zk)})")

    print("== Mechanical comparator observables (no judgment) ==")
    for fk in ("biopower", "capk"):
        rows = [(dk, mechanical_observables(load_manifest(path)))
                for f2, dk, path, _g in MANIFESTS if f2 == fk]
        for name in ("n_kernel_readings", "n_selected_axes", "n_deferred_axes"):
            vals = [mo[name] for _dk, mo in rows]
            print(f"  {fk} {name}: {vals} (range {max(vals) - min(vals)})")
        ck = [mo["contested_kernel"] for _dk, mo in rows]
        print(f"  {fk} contested_kernel: {ck} "
              f"({'stable' if len(set(ck)) == 1 else 'FLIPS'})")

    if args.holdout:
        print("== Holdout reliability (contaminated -- see PROPOSAL section 3) ==")
        expected = json.loads(Path(args.holdout).read_text())["holdout"]
        n = ex = sd = 0
        for item in expected:
            lab = orig_label[(item["source"], item["unit_index"])]
            got = calls[lab]["class"]
            want = item["class"]
            n += 1
            e = got == want
            s = (got in TAG_CLASSES) == (want in TAG_CLASSES)
            ex += e
            sd += s
            print(f"  {item['name']} ({lab}): SCORING.md={want} blinded={got} "
                  f"exact={'Y' if e else 'N'} TAG-side={'Y' if s else 'N'}")
        print(f"  holdout: exact {ex}/{n}; TAG-side {sd}/{n}")
    return 0


def cmd_denom_table(_args):
    """PROPOSAL_ADDENDUM section 1-2: EXPLORATORY, NON-GATING. Denominator-convention
    sensitivity table + Cap concordance check, from the committed calls/mapping."""
    calls = json.loads((AUDIT_DIR / "calls.json").read_text())["calls"]
    mapping = json.loads((AUDIT_DIR / "mapping.json").read_text())["labels"]
    orig_label = {}
    for lab, info in mapping.items():
        if info.get("dup_of") is None and not info.get("planted"):
            orig_label[(info["source"], info["unit_index"])] = lab

    def tag_over(fk, dk, kinds):
        """(TAG, D) counting only units whose kind is in `kinds`."""
        path = next(p for f2, d2, p, _g in MANIFESTS if (f2, d2) == (fk, dk))
        units, zero_kernel = manifest_units(load_manifest(path))
        tag = d = 0
        for i, u in enumerate(units):
            if u["kind"] in kinds:
                d += 1
                if calls[orig_label[(f"{fk}/{dk}", i)]]["class"] in TAG_CLASSES:
                    tag += 1
        return tag, d, zero_kernel

    conventions = [
        ("A per-draw D (committed)", ("kernel_reading", "selected_axis"), None),
        ("B fixed baseline D=6", ("kernel_reading", "selected_axis"), 6),
        ("C kernel-readings-only", ("kernel_reading",), None),
        ("D selected-axes-only", ("selected_axis",), None),
        ("E raw TAG count", ("kernel_reading", "selected_axis"), "raw"),
    ]
    print("== Denominator-convention sensitivity table (EXPLORATORY, NON-GATING) ==")
    print("   (zero-kernel capk/r2 excluded from every range, per the committed rule)")
    for fk, keys in (("biopower", ["base", "r1", "r2"]), ("capk", ["base", "r1", "r2"])):
        print(f"-- {fk} --")
        for name, kinds, fixed in conventions:
            vals, disp = [], []
            for dk in keys:
                tag, d, zk = tag_over(fk, dk, kinds)
                if fixed == "raw":
                    v, txt = Fraction(tag), f"{tag}"
                elif fixed:
                    v, txt = Fraction(tag, fixed), f"{tag}/{fixed}={tag / fixed:.3f}"
                elif d == 0:
                    v, txt = None, "D=0 (n/a)"
                else:
                    v, txt = Fraction(tag, d), f"{tag}/{d}={tag / d:.3f}"
                if zk:
                    txt += " [ZK excl]"
                    v = None
                vals.append(v)
                disp.append(txt)
            ranged = [v for v in vals if v is not None]
            rng = (max(ranged) - min(ranged)) if len(ranged) >= 2 else None
            pair = ""
            if rng is not None and len(ranged) >= 2:
                hi = vals.index(max(ranged))
                lo = vals.index(min(ranged))
                th, _, _ = tag_over(fk, keys[hi], kinds)
                tl, _, _ = tag_over(fk, keys[lo], kinds)
                pair = (f"; max-pair {keys[lo]}<->{keys[hi]}"
                        f"{' NUMERATOR-IDENTICAL' if th == tl and fixed != 'raw' else ''}")
            print(f"  {name:26} {', '.join(disp):44} range="
                  f"{('%.3f' % float(rng)) if rng is not None else 'n/a'}{pair}")

    print("== Cap concordance check (base->r1, base->r2 directions; both files) ==")
    obs_names = ("n_kernel_readings", "n_selected_axes", "n_deferred_axes")
    concordant = []
    for name in obs_names + ("share",):
        dirs = {}
        for fk in ("biopower", "capk"):
            row = {}
            for dk in ("base", "r1", "r2"):
                path = next(p for f2, d2, p, _g in MANIFESTS if (f2, d2) == (fk, dk))
                if name == "share":
                    tag, d, zk = tag_over(fk, dk, ("kernel_reading", "selected_axis"))
                    row[dk] = None if (zk or d == 0) else tag / d
                else:
                    row[dk] = mechanical_observables(load_manifest(path))[name]
            def sgn(a, b):
                if a is None or b is None:
                    return "n/a"
                return "+" if b > a else ("-" if b < a else "0")
            dirs[fk] = (sgn(row["base"], row["r1"]), sgn(row["base"], row["r2"]))
        match = dirs["biopower"] == dirs["capk"] and "n/a" not in dirs["biopower"] + dirs["capk"]
        concordant.append(match)
        print(f"  {name:20} biopower {dirs['biopower']}  capk {dirs['capk']}"
              f"  -> {'CONCORDANT' if match else 'discordant/mixed'}")
    print(f"  verdict (rule fixed in PROPOSAL_ADDENDUM section 2): "
          f"{'concordant cross-file direction — cheapest drift evidence PRESENT' if all(concordant) else 'no consistent cross-file direction — drift UNSUPPORTED by this check (not excluded; weak check)'}")

    ub = 1 - 0.05 ** (1 / 6)
    print(f"== Duplicate-agreement bound (ADDENDUM section 3) ==")
    print(f"  0 disagreements in 6 pairs -> one-sided 95% binomial upper bound on "
          f"per-item disagreement rate = 1 - 0.05^(1/6) = {ub:.3f}")
    return 0


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("control")
    sub.add_parser("calibrate")
    sub.add_parser("packet")
    sub.add_parser("denom-table")
    pc = sub.add_parser("compute")
    pc.add_argument("--holdout", default=None,
                    help="holdout_expected.json (written AFTER calls commit)")
    args = ap.parse_args()
    return {"control": cmd_control, "calibrate": cmd_calibrate,
            "packet": cmd_packet, "compute": cmd_compute,
            "denom-table": cmd_denom_table}[args.cmd](args)


if __name__ == "__main__":
    sys.exit(main())
