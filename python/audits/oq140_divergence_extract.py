#!/usr/bin/env python3
"""OQ-140 — characterize the author_engine_divergence population.

Subcommands:
  recon     Read-only. Re-witness counts, G-class decomposition (record-level Sigma),
            pair x G-class crosstab, silent-axis census, the confound quantity
            (epsilon-uniformity + institutional-d), and the D-ladder feasibility
            witness. Writes a JSON summary + prints a human report. No engine calls,
            no writes to prolog/. Re-runnable (staleness handling per plan).
  extract   [PHASE 2 — after PROPOSAL ratification] Build membership.tsv with the
            descriptor tuple, author/engine columns INDEPENDENTLY sourced by direct
            Prolog query, cross-checked against routing_sink.json (control 6).
  controls  [PHASE 2] Paste all pre-registered control firings (Sigma, drop-one,
            planted-mountain, planted eps-band, D-ladder).
  sample    [PHASE 2] Seeded stratified sampler for the hand-read.

The two Phase-2 subcommands are gated: they refuse to run until the audit dir
contains a ratified PROPOSAL (a `RATIFIED` marker file), so the read-only/decide
pass cannot silently slide into the write/spend pass before the operator rules.

Everything `recon` reports is sourced from the two live JSON artifacts:
  outputs/routing_sink.json      (the 512-record population; 277 divergence)
  outputs/pipeline_output.json   (the per-seat epsilon/chi/d join)
so it re-witnesses whatever is on disk NOW. Regenerate them with a serialized
`run_pipeline.py` before trusting the numbers (routing_sink manifest carries NO
code_commit/pipeline_run_at — coherence is witnessed by the run, not the file).
"""
import argparse
import collections
import json
import os
import re
import sys

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
ROUTING_SINK = os.path.join(REPO, "outputs", "routing_sink.json")
PIPELINE = os.path.join(REPO, "outputs", "pipeline_output.json")
AUDIT_DIR = os.path.join(
    REPO, "audits", "2026-07-04_oq140_divergence_characterization"
)
DIVERGENCE = "author_engine_divergence"


def _author_type(rec):
    """The author's claimed type, parsed out of the `author` field
    (e.g. 'supplied=[tangled_rope]-seat_blind_claim' -> 'tangled_rope')."""
    m = re.search(r"\[([^\]]*)\]", rec["author"])
    return m.group(1) if m else rec["author"]


def _engine_type(rec):
    """Per-seat engine dr_type, parsed from 'dr_type=snare' -> 'snare'."""
    return rec["engine"].split("=")[-1]


def _load():
    with open(ROUTING_SINK) as f:
        rs = json.load(f)
    with open(PIPELINE) as f:
        pl = json.load(f)
    pc = {c["id"]: c for c in pl["per_constraint"]}
    return rs, pc


def _gclass(rs, pc):
    """Per divergence-constraint granularity class, computed from the ENGINE
    field only (independence note in the plan): G-A uniform orbit; G-B non-uniform
    + author matches >=1 seat; G-C non-uniform + author matches no seat."""
    by_c = collections.defaultdict(list)
    for r in rs["records"]:
        by_c[r["constraint"]].append(r)
    div = [r for r in rs["records"] if r["address"] == DIVERGENCE]
    div_c = sorted({r["constraint"] for r in div})
    gcls = {}
    for c in div_c:
        all4 = by_c[c]
        engines = {_engine_type(r) for r in all4}
        authors = {_author_type(r) for r in all4}
        if len(engines) == 1:
            gcls[c] = "G-A"
        elif authors & engines:
            gcls[c] = "G-B"
        else:
            gcls[c] = "G-C"
    return div, div_c, gcls, by_c


def cmd_recon(args):
    rs, pc = _load()
    m = rs["manifest"]
    div, div_c, gcls, by_c = _gclass(rs, pc)
    out = {"regime": {}, "sigma_checks": {}}

    print("=" * 70)
    print("OQ-140 RECON — author_engine_divergence characterization")
    print("=" * 70)
    print("\n[regime — routing_sink manifest]")
    for k in ("n_constraints", "n_seats", "n_records", "per_seat_invariant_holds"):
        print(f"  {k} = {m.get(k)}")
        out["regime"][k] = m.get(k)
    # routing_sink manifest carries NO code_commit/pipeline_run_at — flag it.
    out["regime"]["manifest_carries_code_commit"] = "code_commit" in m
    print(f"  manifest_carries_code_commit = {'code_commit' in m}  "
          "(NB: coherence witnessed by the run, not the file)")

    print("\n[address counts]")
    for a in m["address_counts"]:
        print(f"  {a['address']:28s} {a['count']}")
    out["address_counts"] = {a["address"]: a["count"] for a in m["address_counts"]}

    n_div = len(div)
    print(f"\n[divergence population] {n_div} records on {len(div_c)} constraints")
    out["n_divergence_records"] = n_div
    out["n_divergence_constraints"] = len(div_c)

    # --- G-class, record-level Sigma ---
    grc = collections.Counter(gcls[c] for c in div_c)
    grr = collections.Counter()
    for r in div:
        grr[gcls[r["constraint"]]] += 1
    print("\n[G-class decomposition — record-level Sigma]")
    for g in ("G-A", "G-B", "G-C"):
        print(f"  {g}: {grc[g]:3d} constraints / {grr[g]:3d} records")
    s = grr["G-A"] + grr["G-B"] + grr["G-C"]
    print(f"  sum = {s}   (Sigma-check == n_divergence: {s == n_div})")
    out["gclass"] = {g: {"constraints": grc[g], "records": grr[g]}
                     for g in ("G-A", "G-B", "G-C")}
    out["sigma_checks"]["gclass_records_sum_eq_divergence"] = (s == n_div)

    # --- pair x G-class crosstab ---
    pc_ct = collections.Counter()
    pairs = collections.Counter()
    for r in div:
        pair = f"{_author_type(r)}->{_engine_type(r)}"
        pairs[pair] += 1
        pc_ct[(pair, gcls[r["constraint"]])] += 1
    print("\n[pair x G-class crosstab]")
    xt = {}
    for pair, tot in pairs.most_common():
        ga, gb, gc = (pc_ct[(pair, g)] for g in ("G-A", "G-B", "G-C"))
        print(f"  {pair:30s} tot={tot:3d}  G-A={ga:2d} G-B={gb:2d} G-C={gc:2d}")
        xt[pair] = {"total": tot, "G-A": ga, "G-B": gb, "G-C": gc}
    out["pair_x_gclass"] = xt
    # tail cells (pairs the plan did not name as candidate kinds)
    named = {"tangled_rope->snare", "rope->scaffold", "tangled_rope->rope",
             "snare->rope", "scaffold->piton", "snare->tangled_rope"}
    tail = {p: n for p, n in pairs.items() if p not in named}
    tail_n = sum(tail.values())
    print(f"  [tail] {len(tail)} unnamed cells, {tail_n} records: "
          + ", ".join(f"{p}={n}" for p, n in sorted(tail.items(),
                                                     key=lambda x: -x[1])))
    out["tail_cells"] = tail
    out["tail_records"] = tail_n

    # --- silent axes census ---
    print("\n[silent-axis census over the 277]")
    for axis, fn in (("provenance.mismatch", lambda r: r["provenance"].get("mismatch")),
                     ("author_mode", lambda r: r["provenance"].get("author_mode")),
                     ("detector", lambda r: r["detector"])):
        cnt = collections.Counter(fn(r) for r in div)
        print(f"  {axis}: " + ", ".join(f"{k}={v}" for k, v in cnt.most_common()))
        out.setdefault("silent_axes", {})[axis] = dict(cnt)

    # --- seat distribution ---
    seatd = collections.Counter(r["seat"] for r in div)
    print("\n[seat distribution of divergence records]")
    for k, v in seatd.most_common():
        print(f"  {k}: {v}")
    out["seat_distribution"] = dict(seatd)

    # --- CONFOUND QUANTITY (committed number for the reframe gate) ---
    print("\n[CONFOUND QUANTITY — feeds the reframe gate]")
    # epsilon uniformity across seats within each divergence constraint
    nonuniform = []
    eps_status = collections.Counter()
    inst_lowest = 0
    for c in div_c:
        p = pc[c]["perspective_chi"]
        eps = {round(v["epsilon"], 6) for v in p.values()}
        if len(eps) > 1:
            nonuniform.append(c)
        eps_status[pc[c]["epsilon_provenance"].get("status")] += 1
        ds = {s: v["d"] for s, v in p.items()}
        if ds.get("institutional") == min(ds.values()):
            inst_lowest += 1
    inst_div = len({r["constraint"] for r in div if r["seat"] == "institutional"})
    print(f"  epsilon non-uniform across seats: {len(nonuniform)}/{len(div_c)} "
          f"constraints  => per-seat orbit variation is d-driven at fixed eps")
    print(f"  epsilon_provenance: " + ", ".join(f"{k}={v}"
          for k, v in eps_status.most_common()))
    print(f"  institutional has lowest d in: {inst_lowest}/{len(div_c)}")
    print(f"  institutional diverges on: {inst_div}/{len(div_c)}")
    confound_free = grr["G-A"]
    confound_exposed = grr["G-B"] + grr["G-C"]
    print(f"  confound-FREE population   (G-A, uniform orbit): "
          f"{confound_free}/{n_div} ({100*confound_free/n_div:.1f}%)")
    print(f"  confound-EXPOSED population (G-B+G-C, d-fractured): "
          f"{confound_exposed}/{n_div} ({100*confound_exposed/n_div:.1f}%)")
    out["confound"] = {
        "epsilon_nonuniform_constraints": len(nonuniform),
        "epsilon_provenance": dict(eps_status),
        "institutional_lowest_d": inst_lowest,
        "institutional_diverges": inst_div,
        "confound_free_records": confound_free,
        "confound_exposed_records": confound_exposed,
        "confound_exposed_share": round(confound_exposed / n_div, 4),
    }

    # --- D-ladder feasibility (JSON side only; Prolog witness in RECON.md) ---
    print("\n[D-ladder note] axis-D raw!=final witness lives in RECON.md "
          "(Prolog: 49 baseline signature-overwrite seats = natural positive control).")

    # --- write JSON summary ---
    outpath = os.path.join(AUDIT_DIR, "recon_summary.json")
    with open(outpath, "w") as f:
        json.dump(out, f, indent=2)
    print(f"\n[wrote] {outpath}")


def _require_ratified():
    marker = os.path.join(AUDIT_DIR, "RATIFIED")
    if not os.path.exists(marker):
        sys.exit(
            "REFUSED: this subcommand is Phase-2 (post-ratification). "
            f"Create {marker} only after the operator ratifies PROPOSAL.md "
            "(escalation 1). Recon/decide precedes write/spend."
        )


def cmd_extract(args):
    _require_ratified()
    sys.exit("extract: implemented in Phase 2 — independent Prolog sourcing of "
             "author (narrative_ontology:constraint_claim/2) + engine (dr_type/3), "
             "cross-checked against routing_sink.json (control 6).")


def cmd_controls(args):
    _require_ratified()
    sys.exit("controls: implemented in Phase 2 — Sigma, drop-one, planted-mountain, "
             "planted eps-band, D-ladder firings.")


def cmd_sample(args):
    _require_ratified()
    sys.exit("sample: implemented in Phase 2 — seeded stratified hand-read sampler.")


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("recon").set_defaults(fn=cmd_recon)
    sub.add_parser("extract").set_defaults(fn=cmd_extract)
    sub.add_parser("controls").set_defaults(fn=cmd_controls)
    sub.add_parser("sample").set_defaults(fn=cmd_sample)
    args = ap.parse_args()
    args.fn(args)


if __name__ == "__main__":
    main()
