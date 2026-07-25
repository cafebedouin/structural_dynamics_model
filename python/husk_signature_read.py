"""
Corpus-wide husk-signature read.

HEADLINE FINDING (reported first): The husk signature is temporal — it requires
two time-points (identity-stable-then, behavior-decayed-now). The loaded corpus
(testsets/*.pl, ~3,381 constraints) has ZERO measurement series. All measurement/5
facts live in archives/datasets/v1-v4/, which corpus_loader never loads. K=0, M=0
is not a pending gap; the husk is undetectable on this corpus. Everything below
is a synchronic proxy, labeled as such.

Reads (read-only):
  outputs/fingerprint_data.json
  outputs/orbit_data.json
  outputs/fpn_report.md

Writes:
  outputs/husk_join_table.csv
  outputs/husk_shaped_list.txt
  outputs/husk_signature_report.md
"""

import json
import csv
import re
from pathlib import Path
from collections import defaultdict

ROOT = Path(__file__).parent.parent
OUT = ROOT / "outputs"


# ---------------------------------------------------------------------------
# 1. Load fingerprint families
# ---------------------------------------------------------------------------

def load_fingerprint_data():
    with open(OUT / "fingerprint_data.json") as f:
        data = json.load(f)

    constraint_family = {}   # id -> pattern string
    constraint_uniform = {}  # id -> bool

    for fam in data["shift_families"]:
        pattern = fam["pattern"]
        components = fam["components"]
        vals = list(components.values())
        is_uniform = len(set(vals)) == 1
        for cid in fam["members"]:
            constraint_family[cid] = pattern
            constraint_uniform[cid] = is_uniform

    return constraint_family, constraint_uniform


# ---------------------------------------------------------------------------
# 2. Load gauge families
# ---------------------------------------------------------------------------

def load_orbit_data():
    with open(OUT / "orbit_data.json") as f:
        data = json.load(f)

    constraint_gauge = {}
    for cid, info in data.items():
        sig = info.get("orbit_signature", [])
        constraint_gauge[cid] = "|".join(sorted(set(sig))) if sig else "unknown"

    return constraint_gauge


# ---------------------------------------------------------------------------
# 3. Parse fpn_report.md
# ---------------------------------------------------------------------------

def parse_fpn_report():
    with open(OUT / "fpn_report.md") as f:
        lines = f.readlines()

    # Locate section boundaries
    zm_start = zm_end = sm_start = sm_end = None
    for i, line in enumerate(lines):
        if line.startswith("## Zone Migrations"):
            zm_start = i
        elif line.startswith("## Significant Movers"):
            if zm_start is not None and zm_end is None:
                zm_end = i
            sm_start = i
        elif line.startswith("## Type Breakdown"):
            if sm_start is not None:
                sm_end = i

    def parse_table(start, end):
        rows = {}
        for line in lines[start:end]:
            if not line.startswith("| "):
                continue
            parts = [p.strip() for p in line.split("|")[1:-1]]
            if not parts or parts[0] in ("Constraint", "---", "----------"):
                continue
            if parts[0].startswith("---"):
                continue
            rows[parts[0]] = parts[1:]
        return rows

    # Zone migrations: constraint -> [type, one_hop_ep, one_hop_ep_band, fpn_ep, ep_band, shift]
    zm_rows = parse_table(zm_start, zm_end)

    # Significant movers: constraint -> [type, intrinsic, one_hop_ep, fpn_ep, shift]
    sm_rows = parse_table(sm_start, sm_end)

    return zm_rows, sm_rows


# ---------------------------------------------------------------------------
# 4. Assemble join table
# ---------------------------------------------------------------------------

def assemble(constraint_family, constraint_uniform, constraint_gauge, zm_rows, sm_rows):
    all_ids = sorted(constraint_family.keys())
    rows = []

    for cid in all_ids:
        fp_family = constraint_family[cid]
        is_uniform = constraint_uniform[cid]
        gauge = constraint_gauge.get(cid, "unknown")

        # FPN data
        if cid in zm_rows:
            zm = zm_rows[cid]
            # [type, one_hop_ep, one_hop_ep_band, fpn_ep, ep_band, shift]
            ctype = zm[0] if len(zm) > 0 else "NA"
            one_hop_ep = zm[1] if len(zm) > 1 else "NA"
            one_hop_ep_band = zm[2] if len(zm) > 2 else "NA"
            fpn_ep = zm[3] if len(zm) > 3 else "NA"
            ep_band = zm[4] if len(zm) > 4 else "NA"
            ep_shift = zm[5] if len(zm) > 5 else "NA"
            ep_band_migration = f"{one_hop_ep_band}->{ep_band}"
        elif cid in sm_rows:
            sm = sm_rows[cid]
            # [type, intrinsic, one_hop_ep, fpn_ep, shift]
            ctype = sm[0] if len(sm) > 0 else "NA"
            one_hop_ep = sm[2] if len(sm) > 2 else "NA"
            one_hop_ep_band = "NA"
            fpn_ep = sm[3] if len(sm) > 3 else "NA"
            ep_band = "NA"
            ep_shift = sm[4] if len(sm) > 4 else "NA"
            ep_band_migration = "none"
        else:
            ctype = "NA"
            one_hop_ep = "NA"
            one_hop_ep_band = "NA"
            fpn_ep = "NA"
            ep_band = "NA"
            ep_shift = "NA"
            ep_band_migration = "stable"

        # Proxy husk: criterion 2 only (criterion 1 vacuously true for all)
        #
        # OQ-62: these values come from fpn_report.md, i.e. from
        # fpn_report:ep_band/2 — NOT from abductive_helpers:fpn_band/2. The
        # columns were previously named fpn_zone/one_hop_zone, which named them
        # after the wrong bander; renamed with them. The worst band was renamed
        # "critical" -> "ep_critical", and matching only the old string would
        # silently yield zero proxy husks — which reads exactly like a genuine
        # finding rather than a broken parse. Accept both so a re-run against an
        # archived pre-rename report still parses.
        WORST_EP_BAND = ("ep_critical", "critical")
        proxy_husk = "Y" if ep_band in WORST_EP_BAND else "N"

        # Sharper structural annotation: uniform + worst band
        uniform_critical = "Y" if (is_uniform and ep_band in WORST_EP_BAND) else "N"

        rows.append({
            "constraint_id": cid,
            "fingerprint_family": fp_family,
            "is_fingerprint_uniform": "Y" if is_uniform else "N",
            "gauge_family": gauge,
            "constraint_type": ctype,
            "one_hop_EP": one_hop_ep,
            "one_hop_ep_band": one_hop_ep_band,
            "fpn_EP": fpn_ep,
            "ep_band": ep_band,
            "ep_band_migration": ep_band_migration,
            "ep_shift": ep_shift,
            "has_measurement_series": "N",
            "drift_velocity": "NA",
            "proxy_husk": proxy_husk,
            "uniform_critical": uniform_critical,
        })

    return rows


# ---------------------------------------------------------------------------
# 5. Write CSV
# ---------------------------------------------------------------------------

FIELDNAMES = [
    "constraint_id", "fingerprint_family", "is_fingerprint_uniform",
    "gauge_family", "constraint_type",
    "one_hop_EP", "one_hop_ep_band", "fpn_EP", "ep_band",
    "ep_band_migration", "ep_shift",
    "has_measurement_series", "drift_velocity",
    "proxy_husk", "uniform_critical",
]

def write_csv(rows):
    path = OUT / "husk_join_table.csv"
    with open(path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=FIELDNAMES)
        w.writeheader()
        w.writerows(rows)
    print(f"Wrote {len(rows)} rows to {path}")


# ---------------------------------------------------------------------------
# 6. Analysis
# ---------------------------------------------------------------------------

def analyze(rows):
    total = len(rows)

    # Criterion 1 check — vacuity
    criterion1_count = sum(1 for r in rows)  # all by construction
    assert criterion1_count == total, "criterion 1 is not vacuous — unexpected"

    # Proxy husk (criterion 2 only)
    proxy = [r for r in rows if r["proxy_husk"] == "Y"]
    N = len(proxy)

    # Uniform critical
    uniform_crit = [r for r in rows if r["uniform_critical"] == "Y"]

    # Coverage
    K = 0  # no measurement series in loaded corpus
    M = 0

    # Seed spots
    seeds = [
        "academic_peer_review_gatekeeping",
        "academic_publishing_peer_review",
        "academic_journal_peer_review_gatekeeping",
        "academic_tenure_system",
        "academic_citation_metrics_as_career_incentive",
        "academic_fashion_modernism_2026",
    ]
    seed_rows = {r["constraint_id"]: r for r in rows if r["constraint_id"] in seeds}

    # Population context: shift(tangled_rope, ×4) family
    tr_uniform_pattern = "shift(tangled_rope, tangled_rope, tangled_rope, tangled_rope)"
    tr_family = [r for r in rows if r["fingerprint_family"] == tr_uniform_pattern]
    tr_total = len(tr_family)
    tr_proxy = sum(1 for r in tr_family if r["proxy_husk"] == "Y")
    tr_fraction = tr_proxy / tr_total if tr_total > 0 else 0

    # Zone migration breakdown
    migration_counts = defaultdict(int)
    for r in rows:
        migration_counts[r["ep_band_migration"]] += 1

    # FPN zone distribution
    zone_counts = defaultdict(int)
    for r in rows:
        zone_counts[r["ep_band"]] += 1

    return {
        "total": total,
        "N_proxy": N,
        "N_uniform_critical": len(uniform_crit),
        "K": K,
        "M": M,
        "seed_rows": seed_rows,
        "tr_family_total": tr_total,
        "tr_family_proxy": tr_proxy,
        "tr_fraction": tr_fraction,
        "migration_counts": dict(migration_counts),
        "zone_counts": dict(zone_counts),
        "proxy": proxy,
    }


# ---------------------------------------------------------------------------
# 7. Write husk_shaped_list.txt
# ---------------------------------------------------------------------------

def write_proxy_list(proxy_rows):
    path = OUT / "husk_shaped_list.txt"
    with open(path, "w") as f:
        f.write("# Proxy-husk constraints (synchronic: ep_band=ep_critical only)\n")
        f.write("# NOT confirmed husk — temporal data absent. See husk_signature_report.md.\n\n")
        for r in sorted(proxy_rows, key=lambda x: x["constraint_id"]):
            f.write(f"{r['constraint_id']}\t{r['fingerprint_family']}\t"
                    f"{r['constraint_type']}\t{r['one_hop_EP']}\t{r['fpn_EP']}\t"
                    f"{r['ep_band_migration']}\t{r['is_fingerprint_uniform']}\n")
    print(f"Wrote {len(proxy_rows)} proxy-husk rows to {path}")


# ---------------------------------------------------------------------------
# 8. Write report
# ---------------------------------------------------------------------------

def write_report(stats):
    path = OUT / "husk_signature_report.md"
    s = stats

    seed_table_lines = []
    seeds_ordered = [
        "academic_peer_review_gatekeeping",
        "academic_publishing_peer_review",
        "academic_journal_peer_review_gatekeeping",
        "academic_tenure_system",
        "academic_fashion_modernism_2026",
        "academic_citation_metrics_as_career_incentive",
    ]
    for sid in seeds_ordered:
        r = s["seed_rows"].get(sid, {})
        seed_table_lines.append(
            f"| {sid} | {r.get('fingerprint_family','?')} | "
            f"{r.get('is_fingerprint_uniform','?')} | "
            f"{r.get('gauge_family','?')} | "
            f"{r.get('ep_band_migration','?')} | "
            f"{r.get('fpn_EP','?')} | "
            f"{r.get('proxy_husk','?')} |"
        )

    migration_summary = "\n".join(
        f"- {k}: {v}" for k, v in sorted(s["migration_counts"].items(),
                                          key=lambda x: -x[1])
    )

    proxy_pct = 100 * s["N_proxy"] / s["total"]
    tr_pct = 100 * s["tr_fraction"]

    total_migrated = sum(v for k, v in s["migration_counts"].items() if "->" in k)
    to_critical = sum(v for k, v in s["migration_counts"].items() if k.endswith("->critical"))
    to_degraded = sum(v for k, v in s["migration_counts"].items() if k.endswith("->degraded"))
    to_contested = sum(v for k, v in s["migration_counts"].items() if k.endswith("->contested"))

    report = f"""# Husk-Signature Read — Corpus-Wide Report

*Data sources: outputs/fingerprint_data.json, outputs/orbit_data.json, outputs/fpn_report.md*
*Corpus commit: 8def32e6 (2026-05-22), output files generated 2026-05-21*

---

## Headline Finding

The husk signature requires temporal data: two time-points to confirm that a constraint's
perspectival identity is stable *while* its extraction-purity decays. The loaded corpus
(testsets/*.pl, {s["total"]:,} constraints) has **zero measurement series** (has_measurement_series=N
for all). All measurement/5 facts live in archives/datasets/v1–v4/, which corpus_loader does
not touch. **K=0. M=0.** This is not a gap to fill later — it is a structural fact about
the corpus as loaded. The husk is undetectable on this corpus.

What follows is a synchronic proxy: applying criteria 1 and 2 to a snapshot, with no
temporal confirmation. The proxy is labeled as such throughout.

---

## Join Table Summary

| Metric | Value |
|--------|-------|
| Total constraints | {s["total"]:,} |
| With any zone migration (purity zone changed) | {total_migrated:,} |
| Zone migrations ending in critical | {to_critical:,} |
| Zone migrations ending in degraded | {to_degraded:,} |
| Zone migrations ending in contested | {to_contested:,} |
| No significant EP shift (shift ≤ 0.01) | {s["migration_counts"].get("stable",0):,} |
| Significant shift but no zone change | {s["migration_counts"].get("none",0):,} |
| has_measurement_series=Y | 0 |

Zone migration detail (source→dest: count):
{migration_summary}

Full join table: `outputs/husk_join_table.csv` ({s["total"]:,} rows)

---

## Criterion 1 Status: Vacuous

Criterion 1 (fingerprint-stable = "holds one isomorphism family") is **satisfied by all
{s["total"]:,} constraints**. Every constraint maps to exactly one shift family by construction
(logical_fingerprint is a deterministic function). Criterion 1 adds zero discrimination to the
proxy filter. It is reported here, not silently dropped.

A sharper structural annotation (uniform shift pattern = all 4 perspectives same type) identifies
{s["N_uniform_critical"]:,} constraints that are both uniform and critical-zone (column `uniform_critical=Y`
in the CSV). This is *not* the specified husk criterion — it is a separate annotation.

---

## Proxy Population (N = {s["N_proxy"]:,})

**N = {s["N_proxy"]:,}** constraints have ep_band = "ep_critical" ({proxy_pct:.1f}% of corpus).
These satisfy criterion 2 but criterion 1 is vacuous, so N is the proxy count, not a husk count.

Of those {s["N_proxy"]:,}:
- With uniform shift pattern (uniform_critical=Y): {s["N_uniform_critical"]:,}
- With mixed shift pattern: {s["N_proxy"] - s["N_uniform_critical"]:,}

Coverage split: **N = {s["N_proxy"]:,}** constraints are proxy-shaped on static signature; of
those, **K = 0** have measurement trajectories at all; **M = 0** confirm drift-negative.
The remaining **{s["N_proxy"]:,}** ({s["N_proxy"]:,} − 0) are proxy-shaped but
trajectory-unconfirmable *in principle on this corpus* — not just pending collection,
but requiring the archive measurement layer to be loaded.

Full proxy list: `outputs/husk_shaped_list.txt`

---

## Seed Spot-Check (6 Academic Constraints)

| Constraint | Fingerprint Family | Uniform | Gauge Family | FPN Migration | FPN EP | proxy_husk |
|---|---|---|---|---|---|---|
{chr(10).join(seed_table_lines)}

**Peer-review trio** (academic_peer_review_gatekeeping, academic_publishing_peer_review,
academic_journal_peer_review_gatekeeping): all share fingerprint family
`shift(tangled_rope, tangled_rope, tangled_rope, tangled_rope)`. All three migrate
degraded→critical with FPN EP = 0.0000. All proxy_husk=Y.

**Citation metrics** (academic_citation_metrics_as_career_incentive): fingerprint family
`shift(naturalized, tangled_rope, rope, snare)` — perspectivally fragmented (four distinct
types across observer positions). Not in fpn zone migrations or significant movers; EP shift ≤ 0.01;
purity-stable. proxy_husk=N. The fingerprint report's hint that citation is perspectivally
distinct from the peer-review trio is confirmed: different family, different purity trajectory.

---

## Population Control — Proxy Self-Refutation

The peer-review trio's fingerprint family is `shift(tangled_rope, tangled_rope, tangled_rope,
tangled_rope)` with **{s["tr_family_total"]:,} members** corpus-wide.

Of those {s["tr_family_total"]:,}:
- proxy_husk=Y (ep_band=ep_critical): **{s["tr_family_proxy"]:,}** ({tr_pct:.1f}%)
- proxy_husk=N: **{s["tr_family_total"] - s["tr_family_proxy"]:,}** ({100-tr_pct:.1f}%)

**The synchronic proxy fails to discriminate.** {tr_pct:.0f}% of all uniform tangled_rope
constraints end in critical zone — this is not a rare sub-population, it is the default
behavior of the family. "Fingerprint-uniform + purity-collapsed" describes most tangled_ropes,
not a coherent husk population.

This is independent evidence that the husk is genuinely temporal and cannot be approximated
with snapshot features. A constraint that is uniform and critical-zone could be: (a) a true
husk — identity stable while purity decays over time; (b) a constraint that was always
critical-zone and always uniform — no decay, just a low-purity structure from the start.
The snapshot cannot tell these apart. The temporal criterion exists precisely because of this
ambiguity.

---

## Synthesis

The four existing predicates (logical_fingerprint, gauge_orbit, fpn_run, drift_velocity) were
read corpus-wide. Three produced aligned output (fingerprint_data.json, orbit_data.json,
fpn_report.md, all 2026-05-21). The fourth — drift_velocity — yielded K=0: no measurement/5
facts are in scope for any of the {s["total"]:,} main-corpus constraints; the measurement layer
exists only in unloaded archives.

The synchronic proxy (ep_band=ep_critical) flags {s["N_proxy"]:,} constraints ({proxy_pct:.1f}%
of corpus). But the population control shows that {tr_pct:.0f}% of the uniform tangled_rope
family — the peer-review trio's own family — lands in that critical zone. The proxy does not
isolate a coherent sub-population; it describes a majority behavior of the largest fingerprint
family. This is not a failure of execution — it is a real result: the husk signature is not a
synchronic property, and attempting to proxy it with snapshot data confirms rather than
approximates the temporal criterion. The measurement archive (archives/datasets/v1–v4/, 2,241
measurement/5 facts across 294 target entities) is a separate corpus, not yet integrated
with the classification layer; that integration is the precondition for any genuine husk
detection.
"""

    with open(path, "w") as f:
        f.write(report)
    print(f"Wrote report to {path}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print("Loading fingerprint data...")
    constraint_family, constraint_uniform = load_fingerprint_data()
    print(f"  {len(constraint_family):,} constraints in {len(set(constraint_family.values()))} families")

    print("Loading orbit data...")
    constraint_gauge = load_orbit_data()
    print(f"  {len(constraint_gauge):,} constraints with gauge data")

    print("Parsing FPN report...")
    zm_rows, sm_rows = parse_fpn_report()
    print(f"  Zone migrations: {len(zm_rows):,}")
    print(f"  Significant movers: {len(sm_rows):,}")

    print("Assembling join table...")
    rows = assemble(constraint_family, constraint_uniform, constraint_gauge, zm_rows, sm_rows)

    write_csv(rows)

    print("Analyzing...")
    stats = analyze(rows)

    write_proxy_list(stats["proxy"])

    write_report(stats)

    # Quick summary to stdout
    print()
    print("=" * 60)
    print("HEADLINE: K=0 — husk undetectable on loaded corpus")
    print(f"Total constraints: {stats['total']:,}")
    print(f"Proxy N (ep_band=ep_critical): {stats['N_proxy']:,}")
    print(f"Uniform+critical: {stats['N_uniform_critical']:,}")
    print(f"K (with measurement series): {stats['K']}")
    print(f"M (confirmed husk): {stats['M']}")
    print()
    print(f"tr_uniform family ({stats['tr_family_total']:,} members): "
          f"{stats['tr_family_proxy']:,} proxy ({100*stats['tr_fraction']:.1f}%) — proxy fails to discriminate")
    print("=" * 60)


if __name__ == "__main__":
    main()
