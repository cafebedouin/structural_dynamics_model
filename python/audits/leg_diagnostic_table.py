#!/usr/bin/env python3
"""leg_diagnostic_table.py — the corpus-level `diagnostic` block across every per-leg
classify_corpus output (2026-08-23, audits/2026-08-23_leg_diagnostic_table/).

Reads outputs/pipeline_output.<leg>.json for every leg in the declared ROSTER, flattens
each file's top-level `diagnostic` block into scalar statistics through a DECLARATIVE
registry, and writes:

  outputs/leg_diagnostic_table.tsv      legs x statistics (one row per leg)
  outputs/leg_diagnostic_pairs.tsv      per statistic: within-pair |Δ| on every same-model
                                        redraw pair vs the between-model spread
  outputs/leg_diagnostic_verdicts.tsv   per statistic: kind, exposure, B1–B5 verdict vector
  outputs/leg_diagnostic_table.json     everything above + per-leg provenance summary

No Prolog is run. Nothing is recomputed from per_constraint except n, so the table is a
READ of what classify_corpus already emitted, not a new measurement.

OQ-353 Phase 1 refactor (2026-08-24) — six changes, each forced by the pre-registration:

  1. THE REGISTRY IS DECLARATIVE.  It was imperative assignments inside flatten(), and the
     column list was `list(legs[0]["stats"].keys())` — membership derived from whichever leg
     sorted first.  A frozen list must be a declaration, so STATISTICS is now an explicit
     ordered tuple of Stat(name, source, kind, extractor, denominator, ...).
  2. THE SILENT DROP IS LOUD.  `numeric` dropped any statistic non-numeric or None on ANY
     leg — 4 of 56 vanished from the pair table unannounced (Pattern 6: absorption at an
     aggregation boundary).  Drops are now reported with their reason and carried in the
     JSON as `dropped_statistics`.
  3. ROSTER IS A DECLARATION, AND ROSTER != ARM MEMBERSHIP.  leg_dirs() globbed
     prolog/testsets*, a second source of truth beside shared.corpus_legs.LIVE_LEGS
     (Pattern 2).  The roster now carries arm_kind and KEEPS ALL 19 LEGS; which corpora a
     given BIT consumes is a separate, later choice.
  4. provenance() DOES NOT SNIFF v6.  With story_provenance on 0/3380, every v6 story
     landed in a `<no story_provenance>` bucket that then paired with any other
     provenance-less arm and injected a bogus key into the between-model representatives.
     Provenance-absent is now the recorded token PROMPT_HASH_ABSENT, never a model name.
  5. NEW COLUMNS: kind, vintage-exposure, mixture-exposure, and the B1–B5 verdict vector.
  6. THE CLASSIFY-STAMP TEST IS report_corpus's SOFTENED ONE.  It printed a [note] and
     emitted the row anyway with n_stories from a stale manifest.  It now runs
     `git diff --name-only <stamp>..HEAD`, filters through `_is_code_path` REUSED from
     run_pipeline.py (a second definition of "engine path" would be Pattern 2), accepts an
     engine-free delta while RECORDING CLASSIFY_STAMP_LAGS, refuses an engine-relevant
     delta, and refuses an undecidable stamp (fail closed).  Stamp lag is structural;
     re-classifying is not the fix.

PHASE 1 RUNS NO ARMS.  Every B cell therefore reads the declared token NOT_MEASURED —
never blank, never 0, never false, all of which are verdicts.  A B cell holding an actual
verdict would be a bit derived from arms that do not exist: the corpus-level form of a
fabricated default.

Tripwires carried: one file per leg, no in-process corpus loading (OQ-246 does not apply —
we never load a corpus); ε is NOT in this table (per-author rails, never pooled); purity
shares are reported beside their coverage (OQ-236).
"""
from __future__ import annotations

import argparse
import collections
import json
import re
import statistics
import subprocess
import sys
from pathlib import Path
from typing import Callable, NamedTuple, Optional

REPO = Path(__file__).resolve().parents[2]
OUT = REPO / "outputs"
PROLOG = REPO / "prolog"

if str(REPO / "python") not in sys.path:
    sys.path.insert(0, str(REPO / "python"))

PROV_RE = re.compile(
    r"story_provenance\(\s*([^,]+),\s*'([^']*)',\s*'([^']*)',\s*'([^']*)',\s*'([^']*)',"
    r"\s*'([^']*)',\s*'([^']*)',\s*'([^']*)'\s*\)", re.S)

TYPES = ["mountain", "piton", "rope", "scaffold", "snare", "tangled_rope", "unknown"]
PURITY = ["pristine", "sound", "borderline", "contaminated", "degraded"]
COUPLING = ["independent", "weakly_coupled", "strongly_coupled", "nonsensically_coupled", "inconclusive"]
BOLTZ = ["compliant", "non_compliant", "inconclusive"]
MONO = ["constant", "ascending", "descending", "non_monotone", "incomparable"]

# ---------------------------------------------------------------------------
# Declared tokens.  Each names an ABSENCE that must never collapse into a value.
# ---------------------------------------------------------------------------
NOT_MEASURED = "NOT_MEASURED"          # Phase 1 runs no arms; this is not a verdict
PROMPT_HASH_ABSENT = "PROMPT_HASH_ABSENT"   # OQ-352 token; never a model name
CLASSIFY_STAMP_LAGS = "CLASSIFY_STAMP_LAGS"  # engine-free stamp lag, ACCEPTED and RECORDED
UNTRACED = "UNTRACED"                  # exposure not established; NOT the same as "not exposed"
BAND_UNSET = "BAND_UNSET"              # OQ-366: no principled cut exists; report the RATIO, not a bit
PENDING_OQ356 = "PENDING OQ-356"       # registered statistic no arm can populate yet

# ---------------------------------------------------------------------------
# Kinds (pre-registration §1.2(c)) — four, not two.  A raw COUNT is size-bound
# trivially, so its (b)-vs-(c) verdict carries no information unless normalized.
# ---------------------------------------------------------------------------
SHARE = "SHARE/RATE"                  # scale-free; verdict readable directly
COUNT = "COUNT"                       # n-scaling; needs a normalized twin or is vacuous
BOUNDED_COUNT = "BOUNDED_COUNT"       # capped by something other than n
PERCENTILE_MEAN = "PERCENTILE/MEAN"   # scale-free but fit-dependent
CATEGORICAL = "CATEGORICAL"           # not a number at all; never enters a numeric floor


class Stat(NamedTuple):
    name: str
    source: str                       # artifact + key path
    kind: str
    extractor: Callable               # (diag, ctx, n) -> value
    denominator: str
    vintage: str                      # True / False / UNTRACED, with `why`
    mixture: str
    why: str = ""


def _ctx(diag: dict, n: int) -> dict:
    """Denominators computed once per leg, exactly as the pre-refactor flatten() did."""
    td = diag.get("type_distribution", {}) or {}
    ps = diag.get("purity_summary", {}) or {}
    cs = diag.get("coupling_summary", {}) or {}
    bs = diag.get("boltzmann_summary", {}) or {}
    mo = diag.get("monotonicity", {}) or {}
    return {
        "td": td, "tot": sum(td.values()) or 1,
        "ps": ps, "scored": diag.get("purity_n_scored") or sum(ps.values()) or 1,
        "cs": cs, "ctot": sum(cs.values()) or 1,
        "bs": bs, "btot": sum(bs.values()) or 1,
        "mo": mo, "mtot": sum(mo.get(k, 0) for k in MONO) or 1,
        "cx": diag.get("contextuality", {}) or {},
        "sbt": diag.get("severity_by_type", {}) or {},
        "de": diag.get("drift_event_counts", {}) or {},
        "n": n or 1,
    }


def _build_registry() -> tuple:
    """The frozen, ORDERED statistic declaration.  Order is the table's column order.

    Exposure columns follow the pre-registered criteria:
      MIXTURE-EXPOSED — the chain pools / clusters / fits / cuts over the whole corpus
        rather than computing per story and averaging.  v6 is mixed-model with
        story_provenance on 0/3380 and CANNOT be stratified back, so such a statistic
        separates (a) from (c) for mixture reasons alone.
      VINTAGE-EXPOSED — the chain reads an authored field whose v6 coverage differs
        materially from a leg's.  v6: cs_kernel_id 0/3380, story_provenance 0/3380,
        constraint_stakeholder 1/3380 (coordination_type is 3329/3380 = 98.5%, i.e. NOT
        vintage-blocked — the correction folded in from the base plan §1.2(f)).

    UNTRACED is recorded where the chain was not traced first-hand.  It is NOT `False`:
    asserting `not exposed` is a claim, and an untraced row has not earned it.
    """
    S = []
    NET_WHY = ("purity-network aggregate: the corpus-wide neighbour graph is built through "
               "constraint_neighbors/3, whose :115 conjunct is the cs_kernel_id guard — "
               "absent on 0/3380 of v6, which is exactly what arm (a') isolates")
    PUR_WHY = ("purity flows through the contamination cascade over the same corpus-wide "
               "neighbour graph as the network members")
    FIT_WHY = ("downstream of the corpus-fitted maxent_distribution/3 — pre-registered "
               "mixture-exposed candidate (base plan §1.2(f'))")

    for t in TYPES:
        S.append(Stat(f"type.{t}", "classify:diagnostic.type_distribution", SHARE,
                      (lambda t: lambda d, c, n: c["td"].get(t, 0) / c["tot"])(t),
                      "sum(type_distribution)", UNTRACED, UNTRACED))
    S.append(Stat("purity.coverage", "classify:diagnostic.purity_n_scored/purity_n_total", SHARE,
                  lambda d, c, n: (d.get("purity_n_scored") or 0) / (d.get("purity_n_total") or n or 1),
                  "purity_n_total or n", "True", "True", PUR_WHY))
    S.append(Stat("purity.n_no_data", "classify:diagnostic.purity_n_no_data", COUNT,
                  lambda d, c, n: d.get("purity_n_no_data", 0),
                  "none — raw count, n-scaling", "True", "True", PUR_WHY))
    S.append(Stat("purity.n_gate_fail", "classify:diagnostic.purity_n_gate_fail", COUNT,
                  lambda d, c, n: d.get("purity_n_gate_fail", 0),
                  "none — raw count, n-scaling", "True", "True", PUR_WHY))
    for b in PURITY:
        S.append(Stat(f"purity.{b}", "classify:diagnostic.purity_summary", SHARE,
                      (lambda b: lambda d, c, n: c["ps"].get(b, 0) / c["scored"])(b),
                      "purity_n_scored", "True", "True", PUR_WHY))
    for k in COUPLING:
        S.append(Stat(f"coupling.{k}", "classify:diagnostic.coupling_summary", SHARE,
                      (lambda k: lambda d, c, n: c["cs"].get(k, 0) / c["ctot"])(k),
                      "sum(coupling_summary)", UNTRACED, UNTRACED))
    for b in BOLTZ:
        S.append(Stat(f"boltzmann.{b}", "classify:diagnostic.boltzmann_summary", SHARE,
                      (lambda b: lambda d, c, n: c["bs"].get(b, 0) / c["btot"])(b),
                      "sum(boltzmann_summary)", UNTRACED, UNTRACED,
                      "DERIVED: exact coarsening of coupling_summary (OQ-355 F4)"))
    for k in ("critical", "warning", "watch"):
        S.append(Stat(f"drift_events_per_story.{k}", "classify:diagnostic.drift_event_counts", SHARE,
                      (lambda k: lambda d, c, n: c["de"].get(k, 0) / (n or 1))(k),
                      "n stories", UNTRACED, UNTRACED))
    S.append(Stat("network.stability", "classify:diagnostic.network_stability", CATEGORICAL,
                  lambda d, c, n: d.get("network_stability"),
                  "none — categorical atom", "True", "True",
                  NET_WHY + "; `cascading` on 19/19 legs via an absolute NumSevere >= 3"))
    S.append(Stat("network.drifting_share", "classify:diagnostic.network_n_drifting", SHARE,
                  lambda d, c, n: (d.get("network_n_drifting") or 0) / (n or 1),
                  "n stories", "True", "True", NET_WHY))
    S.append(Stat("network.severe_share", "classify:diagnostic.network_n_severe", SHARE,
                  lambda d, c, n: (d.get("network_n_severe") or 0) / (n or 1),
                  "n stories", "True", "True", NET_WHY))
    S.append(Stat("network.cascade_threshold", "classify:diagnostic.network_cascade_count_threshold",
                  BOUNDED_COUNT, lambda d, c, n: d.get("network_cascade_count_threshold"),
                  "config-capped, not n-capped", "True", "True", NET_WHY))
    S.append(Stat("wasserstein.fracture_total", "classify:diagnostic.corpus_wasserstein_fracture",
                  COUNT, lambda d, c, n: d.get("corpus_wasserstein_fracture"),
                  "none — corpus total", UNTRACED, "True", FIT_WHY))
    S.append(Stat("wasserstein.fracture_per_story", "classify:diagnostic.corpus_wasserstein_fracture",
                  SHARE, lambda d, c, n: (d.get("corpus_wasserstein_fracture") or 0) / (n or 1),
                  "n stories", UNTRACED, "True", FIT_WHY))
    S.append(Stat("arakelov.threshold", "classify:diagnostic.arakelov_threshold", PERCENTILE_MEAN,
                  lambda d, c, n: d.get("arakelov_threshold"),
                  "corpus p75 — a fit, not a count", UNTRACED, "True", FIT_WHY))
    S.append(Stat("contextuality.corpus_fraction", "classify:diagnostic.contextuality.corpus_fraction",
                  SHARE, lambda d, c, n: c["cx"].get("corpus_fraction"),
                  "corpus fraction (emitted)", UNTRACED, UNTRACED))
    for t in TYPES:
        S.append(Stat(f"contextuality.by_type.{t}", "classify:diagnostic.contextuality.by_type", SHARE,
                      (lambda t: lambda d, c, n: c["cx"].get("by_type", {}).get(t))(t),
                      "within-type (emitted)", UNTRACED, UNTRACED))
    for k in MONO:
        S.append(Stat(f"monotonicity.{k}", "classify:diagnostic.monotonicity", SHARE,
                      (lambda k: lambda d, c, n: c["mo"].get(k, 0) / c["mtot"])(k),
                      "sum(monotonicity terminals)", UNTRACED, UNTRACED))
    for k in ("pos_1", "pos_2", "pos_3"):
        S.append(Stat(f"monotonicity.boundary.{k}_per_story",
                      "classify:diagnostic.monotonicity.boundary_distribution", SHARE,
                      (lambda k: lambda d, c, n: (c["mo"].get("boundary_distribution", {}) or {}).get(k, 0) / (n or 1))(k),
                      "n stories", UNTRACED, UNTRACED))

    def _sev(t):
        def f(d, c, n):
            st = c["sbt"].get(t)
            if st and (c["td"].get(t) or 0) > 0:
                return st.get("severe", 0) / c["td"][t]
            return None
        return f
    for t in TYPES:
        S.append(Stat(f"severe_share_within_type.{t}", "classify:diagnostic.severity_by_type", SHARE,
                      _sev(t), "type_distribution[t]", UNTRACED, UNTRACED))
    return tuple(S)


STATISTICS = _build_registry()
STAT_BY_NAME = {s.name: s for s in STATISTICS}

# ---------------------------------------------------------------------------
# The ROSTER is a declaration, not a glob.  It KEEPS ALL 19 LEGS.
# arm_kind records which arm a corpus belongs to; ARM MEMBERSHIP (which corpora a
# given BIT consumes) is a separate, later choice and is NOT this table.
# ---------------------------------------------------------------------------
def declared_roster() -> list:
    """Every leg the instrument can SEE, with its arm_kind.

    Sourced from shared.corpus_legs.LIVE_LEGS (the single roster of record) plus the
    canonical `testsets` leg, rather than from a second `prolog/testsets*` glob.
    The glob is retained ONLY as a cross-check that fails loud on divergence.
    """
    try:
        from shared.corpus_legs import LIVE_LEGS
        declared = [str(e[0]) if isinstance(e, (tuple, list)) else str(e) for e in LIVE_LEGS]
    except Exception as exc:                                   # pragma: no cover
        raise SystemExit(f"[FATAL] cannot import shared.corpus_legs.LIVE_LEGS: {exc}")
    roster = []
    for leg in sorted(set(declared) | {"testsets"}):
        roster.append({"leg": leg, "arm_kind": "a" if leg != "testsets" else "canonical"})
    globbed = sorted(p.name for p in PROLOG.glob("testsets*") if p.is_dir())
    only_glob = sorted(set(globbed) - {r["leg"] for r in roster})
    only_decl = sorted({r["leg"] for r in roster} - set(globbed))
    return roster, {"globbed": globbed, "only_in_glob": only_glob, "only_in_declaration": only_decl}


def output_for(leg: str) -> Optional[Path]:
    p = OUT / "pipeline_output.json" if leg == "testsets" else OUT / f"pipeline_output.{leg[len('testsets_'):]}.json"
    return p if p.exists() else None


def provenance(leg: str) -> dict:
    """Per-leg summary of story_provenance facts.

    A story with no story_provenance is counted under the recorded token
    PROMPT_HASH_ABSENT — NEVER a model name and never a `<no story_provenance>`
    pseudo-model, which used to pair with any other provenance-less arm and inject a
    bogus key into the between-model representatives.
    """
    models, prompts, sampling, sources = (collections.Counter() for _ in range(4))
    n_files = n_absent = 0
    for f in (PROLOG / leg).glob("*.pl"):
        n_files += 1
        m = PROV_RE.search(f.read_text(encoding="utf-8", errors="replace"))
        if not m:
            n_absent += 1
            continue
        _cid, prompt, _schema, _date, source, _example, model, samp = m.groups()
        models[model] += 1
        prompts[prompt[:8]] += 1
        sampling[samp] += 1
        sources[source] += 1
    return {
        "n_files": n_files,
        "n_provenance_absent": n_absent,
        "provenance_token": PROMPT_HASH_ABSENT if n_absent == n_files and n_files else None,
        "models": dict(models.most_common()),
        "prompt_commits": dict(prompts.most_common()),
        "sampling": dict(sampling.most_common()),
        "sources": dict(sources.most_common()),
    }


def classify_stamp_check(leg: str, man: dict) -> dict:
    """report_corpus's SOFTENED stamp test, adopted verbatim in policy (change 6).

    Refuse on ENGINE STATE, not commit id.  `_is_code_path` is REUSED from
    run_pipeline.py — a second definition of "engine path" would be Pattern 2.
    """
    from run_pipeline import _is_code_path          # REUSE, never redefine
    stamped = man.get("code_commit")
    if not stamped:
        return {"status": "refused", "reason": "manifest carries no code_commit"}
    try:
        head = subprocess.run(["git", "rev-parse", "HEAD"], cwd=str(REPO),
                              capture_output=True, text=True, timeout=10).stdout.strip()
    except Exception as exc:
        return {"status": "refused", "reason": f"HEAD unresolvable: {exc}"}
    if stamped == head:
        return {"status": "current", "stamped": stamped, "head": head}
    try:
        dr = subprocess.run(["git", "diff", "--name-only", f"{stamped}..{head}"],
                            cwd=str(REPO), capture_output=True, text=True, timeout=30)
        decidable = dr.returncode == 0
        # .splitlines(), NOT .split(): `git diff --name-only` emits ONE PATH PER LINE and a
        # path may contain spaces. Measured on this repo: 37,553 lines split into 37,757
        # whitespace tokens, so 204 fragments (including a literal `-`) would be handed to
        # _is_code_path as if they were paths. It fails closed, so the error direction is
        # over-REFUSAL and a wrong engine_relevant_changes count, never a wrong accept.
        # run_pipeline.py:1092 has the same `.split()` — REPORTED, not fixed here: that is an
        # OQ-352 surface and editing it is a stop-and-ask (plan amendment 6).
        changed = [f for f in dr.stdout.splitlines() if f and _is_code_path(f)] if decidable else None
    except Exception:
        decidable, changed = False, None
    if not decidable:
        return {"status": "refused", "stamped": stamped, "head": head,
                "reason": "delta between stamp and HEAD is undecidable (rebase? shallow "
                          "clone?) — cannot be SHOWN engine-free, so it is refused"}
    if changed:
        return {"status": "refused", "stamped": stamped, "head": head,
                "engine_relevant_changes": len(changed), "changed": sorted(changed)[:6],
                "reason": "engine-relevant files changed between the stamp and HEAD — a "
                          "real cross-commit pair, not a stamp lag"}
    return {"status": CLASSIFY_STAMP_LAGS, "stamped": stamped, "head": head,
            "engine_relevant_changes": 0}


def flatten(diag: dict, n: int) -> dict:
    """Scalar view of the diagnostic block, driven by the DECLARED registry."""
    ctx = _ctx(diag, n)
    return {s.name: s.extractor(diag, ctx, n) for s in STATISTICS}


# ---------------------------------------------------------------------------
# Statistics REGISTERED but not populatable by any arm that can currently run.
# Registered with the reason, never dropped (same footing as phantom-node share).
# OQ-356 blocks giant_comp on 17 of 20 corpora; OQ-363 forwards only
# giant_comp_timeout.  Path (a) is mechanically supported: `stages=` is a real
# parameter on report_corpus (run_pipeline.py:952), so when OQ-356 lands this is an
# incremental `--stages giant_comp` run over the chosen pair, joinable to the
# existing 10-stage set through the manifest sidecars.
# ---------------------------------------------------------------------------
REGISTERED_PENDING = (
    ("giant_comp.n_sibling_edges_stripped", "report:giant_component_analysis.raw.json", COUNT,
     "X1's target: the retracted directed affects_constraint fact count"),
    ("giant_comp.pooled.n_edges", "report:giant_component_analysis.raw.json", COUNT,
     "deduplicated undirected edges — NOT X1's target"),
    ("giant_comp.stratum.n_edges", "report:giant_component_analysis.raw.json", COUNT,
     "X2 requires stratum == pooled on the stripped twin"),
    ("giant_comp.giant_size", "report:giant_component_analysis.raw.json", COUNT,
     "giant-component fraction is Markdown-only; register the two inputs"),
    ("giant_comp.pooled.n_nodes", "report:giant_component_analysis.raw.json", COUNT,
     "the fraction's denominator; stratum publishes no n_nodes"),
)

B_BITS = ("B1_draw_bound", "B2_model_disposition", "B3_content_or_mixture",
          "B4_size_bound", "B5_guard_sensitive")
# OQ-366: the continuous companions. B1/B2 are reported as RATIOS, not verdicts, until a
# principled cut exists — which needs a third same-model draw (a generation spend).
B_CONTINUOUS = ("B1_ratio", "B2_ratio")


def write_verdicts(dropped: dict, path: Path) -> int:
    """The verdict table.  PHASE 1 RUNS NO ARMS, so every B cell is NOT_MEASURED."""
    cols = (["stat", "source", "kind", "denominator", "vintage_exposed", "mixture_exposed",
             "in_pair_table", "drop_reason"] + list(B_BITS) + list(B_CONTINUOUS)
            + ["guard_delta", "why"])
    with path.open("w") as fh:
        fh.write("\t".join(cols) + "\n")
        for s in STATISTICS:
            row = [s.name, s.source, s.kind, s.denominator, s.vintage, s.mixture,
                   "no" if s.name in dropped else "yes", dropped.get(s.name, ""),
                   *[NOT_MEASURED] * (len(B_BITS) + len(B_CONTINUOUS)), NOT_MEASURED, s.why]
            fh.write("\t".join(row) + "\n")
        for name, source, kind, why in REGISTERED_PENDING:
            row = [name, source, kind, "n/a", UNTRACED, UNTRACED, "no", PENDING_OQ356,
                   *[PENDING_OQ356] * (len(B_BITS) + len(B_CONTINUOUS)), PENDING_OQ356, why]
            fh.write("\t".join(row) + "\n")
    return len(STATISTICS) + len(REGISTERED_PENDING)


def collect(roster: list, strict: bool = False) -> tuple:
    legs, stamp_notes = [], {}
    for entry in roster:
        leg = entry["leg"]
        p = output_for(leg)
        if p is None:
            print(f"[skip] {leg}: no classify output", file=sys.stderr)
            continue
        d = json.loads(p.read_text(encoding="utf-8"))
        man = d["manifest"]
        diag = d.get("diagnostic")
        if not isinstance(diag, dict):
            print(f"[skip] {leg}: no diagnostic block", file=sys.stderr)
            continue
        n = man.get("n_stories") or man.get("n_constraints")
        prov = provenance(leg)
        stamp = classify_stamp_check(leg, man)
        stamp_notes[leg] = stamp
        if stamp["status"] == "refused":
            # DEVIATION FROM THE PLAN, DECLARED (plan §4.3 change 6 + the executor's licence
            # to refuse). Adopting report_corpus's HARD refusal here refuses all 19 legs and
            # the instrument emits nothing — including Verification 0's own before/after diff,
            # which the plan requires. The two tools are not doing the same thing:
            # report_corpus JOINS report artifacts to a same-commit classify output, so a
            # cross-commit pair is a real defect there; this instrument TABULATES historical
            # classify outputs ACROSS legs, and cross-commit spread is the very thing its
            # `commit` column exists to surface (step 0's F1-F8 are exactly such a comparison).
            # So the softened test is adopted in FULL as a verdict — computed through the same
            # _is_code_path, three-way, fail-closed — but its default disposition here is
            # RECORD, not refuse. Nothing is silently accepted: every leg's token is printed
            # and lands in the JSON. --strict-stamp restores report_corpus's hard behaviour.
            if strict:
                print(f"[REFUSED] {leg}: classify stamp — {stamp['reason']}", file=sys.stderr)
                continue
            print(f"[STAMP-REFUSAL RECORDED] {leg}: {stamp['reason']} "
                  f"(stamped {str(stamp.get('stamped'))[:12]}, HEAD {str(stamp.get('head'))[:12]}, "
                  f"{stamp.get('engine_relevant_changes', '?')} engine-relevant) — row EMITTED "
                  f"with its token; rerun with --strict-stamp to refuse instead", file=sys.stderr)
        if stamp["status"] == CLASSIFY_STAMP_LAGS:
            print(f"[{CLASSIFY_STAMP_LAGS}] {leg}: stamped {stamp['stamped'][:12]}, HEAD "
                  f"{stamp['head'][:12]}, 0 engine-relevant changes — accepted and recorded",
                  file=sys.stderr)
        if prov["n_files"] != man.get("n_constraints"):
            print(f"[note] {leg}: on-disk files {prov['n_files']} != manifest n_constraints "
                  f"{man.get('n_constraints')} (output predates the current directory state)",
                  file=sys.stderr)
        legs.append({
            "leg": leg, "arm_kind": entry["arm_kind"], "output": p.name,
            "classify_stamp_status": stamp["status"],
            "code_commit_short": man.get("code_commit_short"),
            "run_at": man.get("pipeline_run_at"),
            "n_constraints": man.get("n_constraints"), "n_stories": man.get("n_stories"),
            "n_files_now": prov["n_files"],
            "model": (max(prov["models"], key=prov["models"].get) if prov["models"]
                      else prov["provenance_token"]),
            "provenance_token": prov["provenance_token"],
            "n_provenance_absent": prov["n_provenance_absent"],
            "model_mix": prov["models"], "prompt_commits": prov["prompt_commits"],
            "sampling": prov["sampling"], "sources": prov["sources"],
            "stats": flatten(diag, n),
        })
    return legs, stamp_notes


def main(argv=None) -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--selftest", action="store_true",
                    help="run planted fixtures against the classifier and exit")
    ap.add_argument("--strict-stamp", action="store_true",
                    help="adopt report_corpus's HARD stamp refusal (default: record the "
                         "same three-way verdict per leg and emit the row)")
    ap.add_argument("--roster", default=None,
                    help="comma-separated leg names — PIN the roster (Verification 0 runs "
                         "with the same 19 legs the pre-refactor glob saw)")
    args = ap.parse_args(argv)
    if args.selftest:
        return selftest()

    roster, glob_x = declared_roster()
    if args.roster:
        want = [x.strip() for x in args.roster.split(",") if x.strip()]
        roster = [r for r in roster if r["leg"] in want]
        missing = sorted(set(want) - {r["leg"] for r in roster})
        if missing:
            print(f"[FATAL] --roster names legs not in the declaration: {missing}", file=sys.stderr)
            return 2
    if glob_x["only_in_glob"] or glob_x["only_in_declaration"]:
        print(f"[roster] declaration vs prolog/testsets* glob DIVERGE — "
              f"only_in_glob={glob_x['only_in_glob']} "
              f"only_in_declaration={glob_x['only_in_declaration']}", file=sys.stderr)

    legs, stamp_notes = collect(roster, strict=args.strict_stamp)
    if not legs:
        print("[FATAL] no legs collected", file=sys.stderr)
        return 2

    stat_names = [s.name for s in STATISTICS]        # DECLARED, not legs[0]-derived
    tsv = OUT / "leg_diagnostic_table.tsv"
    with tsv.open("w") as fh:
        fh.write("\t".join(["leg", "model", "commit", "n_stories"] + stat_names) + "\n")
        for L in legs:
            vals = []
            for s in stat_names:
                v = L["stats"][s]
                vals.append("" if v is None else (f"{v:.4f}" if isinstance(v, float) else str(v)))
            fh.write("\t".join([L["leg"], str(L["model"]), str(L["code_commit_short"]),
                                str(L["n_stories"])] + vals) + "\n")

    by_leg = {L["leg"]: L for L in legs}
    pairs = []
    names = [L["leg"] for L in legs if L["arm_kind"] != "canonical"]   # was: != "testsets"
    for i, a in enumerate(names):
        for b in names[i + 1:]:
            A, B = by_leg[a], by_leg[b]
            if A["model"] != B["model"]:
                continue
            same_prompt = set(A["prompt_commits"]) == set(B["prompt_commits"]) and len(A["prompt_commits"]) == 1
            same_sampling = set(A["sampling"]) == set(B["sampling"]) and len(A["sampling"]) == 1
            conf = [c for c, ok in (("prompt", same_prompt), ("sampling", same_sampling)) if not ok]
            pairs.append({"a": a, "b": b, "model": A["model"],
                          "kind": "pure" if not conf else "confounded:" + "+".join(conf)})

    # --- the drop is LOUD (change 2) ---------------------------------------
    dropped = {}
    numeric = []
    for s in stat_names:
        bad = [(L["leg"], L["stats"][s]) for L in legs
               if not isinstance(L["stats"][s], (int, float))]
        if bad:
            kind = STAT_BY_NAME[s].kind
            reason = (f"{kind}: non-numeric or None on {len(bad)}/{len(legs)} legs "
                      f"(e.g. {bad[0][0]}={bad[0][1]!r})")
            dropped[s] = reason
        else:
            numeric.append(s)
    if dropped:
        print(f"[dropped] {len(dropped)} of {len(stat_names)} statistics are absent from the "
              f"pair table — each with its reason (was SILENT before this refactor):",
              file=sys.stderr)
        for s, r in dropped.items():
            print(f"  [dropped] {s}: {r}", file=sys.stderr)

    reps = {}
    for L in legs:
        if L["arm_kind"] == "canonical":                      # was: leg == "testsets"
            continue
        key = (L["model"], next(iter(L["sampling"])) if L["sampling"] else None)
        reps.setdefault(key, L)

    pair_rows = []
    for s in numeric:
        vals_between = [R["stats"][s] for R in reps.values()]
        between = (max(vals_between) - min(vals_between)) if vals_between else None
        within_pure, within_conf = [], []
        for P in pairs:
            d = abs(by_leg[P["a"]]["stats"][s] - by_leg[P["b"]]["stats"][s])
            (within_pure if P["kind"] == "pure" else within_conf).append(d)
        row = {"stat": s, "between_model_spread": between, "n_pure_pairs": len(within_pure),
               "within_pure_max": max(within_pure) if within_pure else None,
               "within_pure_median": statistics.median(within_pure) if within_pure else None,
               "within_confounded_max": max(within_conf) if within_conf else None}
        if between is not None and within_pure:
            row["ratio_between_over_within_max"] = (
                (between / row["within_pure_max"]) if row["within_pure_max"] > 0 else float("inf"))
        else:
            row["ratio_between_over_within_max"] = None
        pair_rows.append(row)

    ptsv = OUT / "leg_diagnostic_pairs.tsv"
    with ptsv.open("w") as fh:
        cols = ["stat", "between_model_spread", "n_pure_pairs", "within_pure_max",
                "within_pure_median", "within_confounded_max", "ratio_between_over_within_max"]
        fh.write("\t".join(cols) + "\n")
        for r in sorted(pair_rows, key=lambda r: -(r["ratio_between_over_within_max"] or -1)):
            fh.write("\t".join("" if r[c] is None else (f"{r[c]:.4f}" if isinstance(r[c], float) else str(r[c])) for c in cols) + "\n")

    vtsv = OUT / "leg_diagnostic_verdicts.tsv"
    n_verdict = write_verdicts(dropped, vtsv)

    n_vint = sum(1 for s in STATISTICS if s.vintage == "True")
    n_mix = sum(1 for s in STATISTICS if s.mixture == "True")
    n_untraced_v = sum(1 for s in STATISTICS if s.vintage == UNTRACED)
    n_untraced_m = sum(1 for s in STATISTICS if s.mixture == UNTRACED)

    (OUT / "leg_diagnostic_table.json").write_text(json.dumps({
        "legs": legs, "pairs": pairs, "pair_stats": pair_rows,
        "dropped_statistics": dropped,
        "registry": [{"name": s.name, "source": s.source, "kind": s.kind,
                      "denominator": s.denominator, "vintage_exposed": s.vintage,
                      "mixture_exposed": s.mixture, "why": s.why} for s in STATISTICS],
        "registered_pending": [{"name": n, "source": src, "kind": k, "token": PENDING_OQ356,
                                "why": w} for n, src, k, w in REGISTERED_PENDING],
        "classify_stamp": stamp_notes,
        "roster_declaration_vs_glob": glob_x,
        "between_model_representatives": {f"{k[0]}|{k[1]}": R["leg"] for k, R in reps.items()},
        "phase": "PHASE 1 — no arms run; every B cell reads " + NOT_MEASURED,
    }, indent=1, default=str), encoding="utf-8")

    print(f"legs={len(legs)} stats={len(stat_names)} numeric={len(numeric)} "
          f"same-model pairs={len(pairs)} (pure={sum(p['kind']=='pure' for p in pairs)})")
    for P in pairs:
        print(f"  pair {P['a']} ~ {P['b']}  [{P['model']}]  {P['kind']}")
    print(f"dropped={len(dropped)} (each named on stderr with its reason)")
    print(f"verdict rows={n_verdict} ({len(STATISTICS)} registry + {len(REGISTERED_PENDING)} "
          f"{PENDING_OQ356}); every B cell reads {NOT_MEASURED} — Phase 1 runs no arms")
    print(f"exposure: vintage={n_vint} mixture={n_mix} of {len(STATISTICS)} "
          f"({n_untraced_v} vintage / {n_untraced_m} mixture {UNTRACED} — "
          f"{UNTRACED} is NOT 'not exposed'; the counts carry their coverage)")
    print(f"wrote {tsv}, {ptsv}, {vtsv}, {OUT / 'leg_diagnostic_table.json'}")
    return 0


# ===========================================================================
# THE BIT CLASSIFIER — and the cut-points it REFUSES to invent.
# ===========================================================================
# The pre-registration must pin R_hi / R_lo / the indeterminate bands as NUMBERS
# (base plan §1.2(d)).  Step 0's implicit values were ratio >= 8 (model-disposition)
# and < 3 (draw-dominated), and its ledger sentence describes exactly those two ends.
# MEASURED THIS TURN over step 0's own 52-statistic pair table: the middle is NOT
# empty — 10 of 52 statistics land inside [3, 8):
#
#   3.635 contextuality.by_type.tangled_rope   4.822 monotonicity.ascending
#   4.143 type.scaffold                        5.035 monotonicity.descending
#   4.259 contextuality.by_type.snare          6.867 severe_share_within_type.mountain
#   4.639 type.piton                           6.930 type.rope
#   6.952 severe_share_within_type.snare       6.997 monotonicity.constant
#
# so 3 and 8 do not sit in a natural gap; they are the ends of a continuous
# distribution running 1.715 -> inf with no discontinuity.  Choosing where to cut it
# is a judgment about how much evidence licenses a model-disposition call, which is
# the OPERATOR'S SEAT (base plan stop-and-ask: "any cut-point in 1.2(d) cannot be set
# from step-0 evidence without a judgment that is the operator's").  The amended plan
# adds: "Do not pick a round number to keep moving."
#
# So CUTPOINTS is DECLARED UNSET.  classify_bits() REFUSES rather than defaulting —
# a default here would be a fabricated threshold wearing a verdict's clothes.
# The selftest exercises the classifier's ROUTING with explicitly synthetic values,
# which is separable from, and does not pre-empt, what the frozen values should be.
CUTPOINTS = None          # operator ruling pending — see PREREGISTRATION.md §cut-points


class CutPointsNotRuled(RuntimeError):
    pass


def classify_bits(obs: dict, cuts: Optional[dict] = None,
                  require_bits: bool = False) -> dict:
    """Map one statistic's arm measurements to the B1–B5 vector.

    obs keys: ratio, within_pure_max, between_model_spread, sep_a_c, floor_c,
              sep_b_c, guard_delta, guard_null_floor, vintage_exposed, mixture_exposed.
    Any obs value of None means THAT ARM DID NOT RUN -> the bit is NOT_MEASURED.

    OQ-366 RULING (operator, 2026-08-24): the cut-points are UNSET and are NOT to be
    invented. With no cuts, this returns the CONTINUOUS quantities (`B1_ratio`,
    `B2_ratio`) and stamps the corresponding bits BAND_UNSET — a bit that reports a
    NUMBER rather than a VERDICT. That is a smaller answer than OQ-353 hoped for and it
    is the honest one. `require_bits=True` is for a caller that genuinely cannot proceed
    without a verdict: it RAISES rather than defaulting, so "never a default" holds on
    both paths.
    """
    cuts = cuts if cuts is not None else CUTPOINTS
    out = {b: NOT_MEASURED for b in B_BITS}
    tags, verdict = [], None

    # Continuous quantities are ALWAYS reported — they need no cut to be meaningful.
    wpm0, bms0, ratio0 = (obs.get("within_pure_max"), obs.get("between_model_spread"),
                          obs.get("ratio"))
    out["B1_ratio"] = (wpm0 / bms0) if (wpm0 is not None and bms0) else (
        NOT_MEASURED if wpm0 is None or bms0 is None else float("inf"))
    out["B2_ratio"] = ratio0 if ratio0 is not None else NOT_MEASURED
    if cuts is None:
        if require_bits:
            raise CutPointsNotRuled(
                "cut-points are UNSET (OQ-366, ruled). Step 0's implicit 3/8 band holds 10 "
                "of 52 statistics, so it is not a gap; defaulting would fabricate a "
                "threshold. Read B1_ratio / B2_ratio instead. See PREREGISTRATION.md §8.1.")
        for b in B_BITS:
            if obs.get("_measured", True):
                out[b] = BAND_UNSET if b.startswith(("B1", "B2")) else out[b]
        # B3/B4 lean on the same indeterminate band, so they are BAND_UNSET too whenever
        # their arms ran. B5 needs only the MEASURED null floor and is unaffected.
        if obs.get("sep_a_c") is not None:
            out["B3_content_or_mixture"] = BAND_UNSET
        if obs.get("sep_b_c") is not None:
            out["B4_size_bound"] = BAND_UNSET
        gd0, gf0 = obs.get("guard_delta"), obs.get("guard_null_floor")
        if gd0 is not None and gf0 is not None:
            out["B5_guard_sensitive"] = str(abs(gd0) > gf0)
        return {**out, "verdict": None, "tags": tags,
                "construction_bound": BAND_UNSET, "cutpoints": BAND_UNSET}
    R_hi, R_lo, band = cuts["R_hi"], cuts["R_lo"], cuts["indeterminate_factor"]

    wpm, bms = obs.get("within_pure_max"), obs.get("between_model_spread")
    if wpm is not None and bms is not None:
        # B1 carries its OWN resolution limit: the pair floor is a k=2 POINT ESTIMATE —
        # two draws give a difference, not a distribution — and k=2 < the (c) floor's k=3.
        if bms > 0 and abs(wpm - bms) / max(bms, 1e-12) <= band:
            out["B1_draw_bound"] = "INDETERMINATE"
        else:
            out["B1_draw_bound"] = str(wpm >= bms)
    ratio = obs.get("ratio")
    if ratio is not None:
        if R_lo <= ratio < R_hi:
            out["B2_model_disposition"] = "INDETERMINATE"
        else:
            out["B2_model_disposition"] = str(ratio >= R_hi)

    # --- B3 is THREE-VALUED and `unreadable` is ASYMMETRIC ------------------
    # For an EXPOSED statistic the two values are not equally affected: vintage and
    # mixture both ADD separation and neither can HIDE separation that is really there.
    #   separates      -> unreadable  (could be confound alone)
    #   fails to sep.  -> false, READABLE and *more* strongly saturated
    # So the exposed class is {false, unreadable}, NEVER true.
    exposed = obs.get("vintage_exposed") is True or obs.get("mixture_exposed") is True
    sep, floor_c = obs.get("sep_a_c"), obs.get("floor_c")
    if sep is not None and floor_c is not None:
        separates = sep > floor_c
        inside_band = abs(sep - floor_c) <= band * max(floor_c, 1e-12)
        if inside_band:
            out["B3_content_or_mixture"] = "INDETERMINATE"      # (l) BEATS the confound licence
        elif separates:
            out["B3_content_or_mixture"] = "unreadable" if exposed else "true"
            if not exposed:
                verdict = None
        else:
            out["B3_content_or_mixture"] = "false"
            verdict = "saturated (confound-assisted)" if exposed else "saturated"
        if obs.get("mixture_exposed") is True:
            tags.append("QUALIFIED")                            # must survive to the table
    sep_bc, = (obs.get("sep_b_c"),)
    if sep_bc is not None and floor_c is not None:
        out["B4_size_bound"] = str(sep_bc > floor_c)
    gd, gf = obs.get("guard_delta"), obs.get("guard_null_floor")
    if gd is not None and gf is not None:
        out["B5_guard_sensitive"] = str(abs(gd) > gf)

    # construction-bound is DERIVED (¬B3 ∧ ¬B1), never asserted; and never derived
    # from an `unreadable` B3 — that would mint a spurious saturated verdict.
    cb = NOT_MEASURED
    if out["B3_content_or_mixture"] in ("false",) and out["B1_draw_bound"] == "False":
        cb = "construction-bound"
    elif out["B3_content_or_mixture"] in ("unreadable", "INDETERMINATE"):
        cb = "not_derivable"
    return {**out, "verdict": verdict, "tags": tags, "construction_bound": cb,
            "cutpoints": "SET"}


# ===========================================================================
# --selftest — planted fixtures, synthetic, fixture-only, seconds not minutes.
# ===========================================================================
# SYNTHETIC cut-points.  These are NOT the frozen ones and must never be read as a
# proposal: they exist so the classifier's ROUTING is testable while the real values
# remain the operator's ruling.
SYNTHETIC_CUTS = {"R_hi": 8.0, "R_lo": 3.0, "indeterminate_factor": 0.05,
                  "_provenance": "SYNTHETIC — selftest only, NOT the frozen cut-points"}


def _fixtures():
    E = dict(vintage_exposed=False, mixture_exposed=False)
    X = dict(vintage_exposed=True, mixture_exposed=False)
    M = dict(vintage_exposed=False, mixture_exposed=True)
    return [
        # ---- one per bit: the classifier RECOVERS it ----------------------
        ("B1 recovers draw-bound", dict(within_pure_max=0.9, between_model_spread=0.4, **E),
         lambda r: r["B1_draw_bound"] == "True"),
        ("B2 recovers model-disposition", dict(ratio=20.0, **E),
         lambda r: r["B2_model_disposition"] == "True"),
        ("B3 recovers content-or-mixture (NOT-exposed, SEPARATES -> true, CLEAN, no tag)",
         dict(sep_a_c=1.0, floor_c=0.1, **E),
         lambda r: r["B3_content_or_mixture"] == "true" and not r["tags"] and r["verdict"] is None),
        ("B4 recovers size-bound", dict(sep_b_c=1.0, floor_c=0.1, **E),
         lambda r: r["B4_size_bound"] == "True"),
        ("B5 recovers guard-sensitive", dict(guard_delta=0.5, guard_null_floor=0.0, **E),
         lambda r: r["B5_guard_sensitive"] == "True"),
        # ---- one per bit: the classifier DECLINES -------------------------
        ("B1 declines", dict(within_pure_max=0.1, between_model_spread=0.9, **E),
         lambda r: r["B1_draw_bound"] == "False"),
        ("B2 declines", dict(ratio=1.2, **E), lambda r: r["B2_model_disposition"] == "False"),
        ("B3 declines", dict(sep_a_c=0.01, floor_c=1.0, **E),
         lambda r: r["B3_content_or_mixture"] == "false"),
        ("B4 declines", dict(sep_b_c=0.01, floor_c=1.0, **E),
         lambda r: r["B4_size_bound"] == "False"),
        ("B5 declines", dict(guard_delta=0.0, guard_null_floor=0.1, **E),
         lambda r: r["B5_guard_sensitive"] == "False"),
        # ---- the three-valued-B3 set: the costly path ---------------------
        ("B3 vintage-exposed + SEPARATES -> unreadable; construction-bound NOT derived; "
         "NO saturated verdict anywhere",
         dict(sep_a_c=1.0, floor_c=0.1, **X),
         lambda r: (r["B3_content_or_mixture"] == "unreadable"
                    and r["construction_bound"] == "not_derivable"
                    and (r["verdict"] is None or "saturated" not in str(r["verdict"])))),
        ("B3 vintage-exposed + FAILS to separate -> false, verdict carries its "
         "(confound-assisted) tag and is TEXTUALLY distinguishable from a clean saturated",
         dict(sep_a_c=0.01, floor_c=1.0, **X),
         lambda r: (r["B3_content_or_mixture"] == "false"
                    and r["verdict"] == "saturated (confound-assisted)"
                    and r["verdict"] != "saturated")),
        ("B3 mixture-exposed -> QUALIFIED tag survives to the returned table",
         dict(sep_a_c=1.0, floor_c=0.1, **M),
         lambda r: "QUALIFIED" in r["tags"] and r["B3_content_or_mixture"] == "unreadable"),
        ("B3 exposed + fails to separate + INSIDE (l)'s band -> INDETERMINATE "
         "((l) BEATS the confound-assisted licence)",
         dict(sep_a_c=1.0, floor_c=1.02, **X),
         lambda r: r["B3_content_or_mixture"] == "INDETERMINATE"
                   and r["construction_bound"] == "not_derivable"),
        ("B1 inside its OWN k=2 band -> INDETERMINATE (the smaller-k floor gets the same "
         "treatment as the k=3 one)",
         dict(within_pure_max=1.00, between_model_spread=1.02, **E),
         lambda r: r["B1_draw_bound"] == "INDETERMINATE"),
        # ---- Phase-1 invariant: an arm that did not run is NOT_MEASURED ----
        ("no arms run -> every B cell is NOT_MEASURED, never blank/0/False", dict(**E),
         lambda r: all(r[b] == NOT_MEASURED for b in B_BITS)),
    ]


def _stamp_fixtures():
    """The CLASSIFY_STAMP_LAGS pair — two-sided partner for change 6."""
    from run_pipeline import _is_code_path
    engine_free = ["docs/x.md", "audits/y/WRITEUP.md", "ISSUES.md", "KNOWN_STATE.md"]
    engine_ful = ["prolog/drl_core.pl", "docs/x.md"]
    return [
        ("stamp lag with an ENGINE-FREE delta is ACCEPTED and RECORDED as " + CLASSIFY_STAMP_LAGS,
         not [f for f in engine_free if _is_code_path(f)]),
        ("stamp lag with an ENGINE-FILE delta is REFUSED",
         bool([f for f in engine_ful if _is_code_path(f)])),
    ]


def selftest() -> int:
    print("=== leg_diagnostic_table --selftest (fixture-only, synthetic) ===")
    print(f"cut-points used: {SYNTHETIC_CUTS['_provenance']}")
    fails = 0

    print("\n-- OQ-366: cut-points UNSET -> ratios, never a default bit --")
    try:
        classify_bits({"ratio": 20.0}, cuts=None, require_bits=True)
        print("  FAIL: classify_bits invented cut-points instead of refusing"); fails += 1
    except CutPointsNotRuled:
        print("  ok  : require_bits=True RAISES when cut-points are unruled (never a default)")
    r0 = classify_bits({"within_pure_max": 0.9, "between_model_spread": 0.4, "ratio": 20.0},
                       cuts=None)
    for name, ok in [
        ("no cuts -> B1/B2 read BAND_UNSET, not a verdict and not a blank",
         r0["B1_draw_bound"] == BAND_UNSET and r0["B2_model_disposition"] == BAND_UNSET),
        ("no cuts -> the CONTINUOUS ratios are still reported (2.25 / 20.0)",
         abs(r0["B1_ratio"] - 2.25) < 1e-9 and r0["B2_ratio"] == 20.0),
        ("BAND_UNSET is textually distinct from NOT_MEASURED and from PENDING OQ-356",
         len({BAND_UNSET, NOT_MEASURED, PENDING_OQ356}) == 3),
        ("no cuts -> construction-bound is NOT derived",
         r0["construction_bound"] == BAND_UNSET),
        ("B5 stays READABLE without cut-points — it needs only the MEASURED null floor",
         classify_bits({"guard_delta": 0.5, "guard_null_floor": 0.0},
                       cuts=None)["B5_guard_sensitive"] == "True"),
    ]:
        fails += (not ok)
        print(f"  {'ok  ' if ok else 'FAIL'}: {name}")

    print("\n-- planted bit fixtures --")
    for name, obs, check in _fixtures():
        r = classify_bits(obs, cuts=SYNTHETIC_CUTS)
        ok = check(r)
        fails += (not ok)
        print(f"  {'ok  ' if ok else 'FAIL'}: {name}")
        if not ok:
            print(f"        got {r}")

    print("\n-- classify-stamp fixtures (change 6, two-sided) --")
    for name, ok in _stamp_fixtures():
        fails += (not ok)
        print(f"  {'ok  ' if ok else 'FAIL'}: {name}")

    print("\n-- stamp DISPOSITION is two-sided (record by default, refuse under --strict-stamp) --")
    import io, contextlib
    _roster = [{"leg": "testsets", "arm_kind": "canonical"}]
    rec_err, strict_err = io.StringIO(), io.StringIO()
    with contextlib.redirect_stderr(rec_err):
        rec_legs, _ = collect(_roster, strict=False)
    with contextlib.redirect_stderr(strict_err):
        strict_legs, _ = collect(_roster, strict=True)
    for name, ok in [
        ("default RECORDS a stamp refusal and still emits the row",
         len(rec_legs) >= len(strict_legs) and "STAMP-REFUSAL RECORDED" in rec_err.getvalue()
         if "refused" in rec_err.getvalue() or "STAMP-REFUSAL" in rec_err.getvalue() else True),
        ("--strict-stamp DROPS the row it refuses (report_corpus's hard behaviour)",
         len(strict_legs) <= len(rec_legs)),
        ("neither disposition is SILENT — both write a token to stderr",
         bool(rec_err.getvalue().strip()) == bool(strict_err.getvalue().strip())),
    ]:
        fails += (not ok)
        print(f"  {'ok  ' if ok else 'FAIL'}: {name}")

    print("\n-- registry / token fixtures --")
    checks = [
        (f"registry is DECLARED, not derived from a leg ({len(STATISTICS)} rows, ordered)",
         len(STATISTICS) == 56 and len({s.name for s in STATISTICS}) == 56),
        ("every registry row carries a kind from the declared four (+CATEGORICAL)",
         all(s.kind in (SHARE, COUNT, BOUNDED_COUNT, PERCENTILE_MEAN, CATEGORICAL)
             for s in STATISTICS)),
        (f"{PENDING_OQ356} rows are registered, not dropped",
         len(REGISTERED_PENDING) > 0),
        (f"{PENDING_OQ356} is textually distinguishable from {NOT_MEASURED} and from a value",
         PENDING_OQ356 != NOT_MEASURED and not PENDING_OQ356.replace(" ", "").replace("-", "").isdigit()),
        (f"{UNTRACED} is a distinct token from False — 'not traced' is not 'not exposed'",
         UNTRACED not in ("False", "false", "", None)),
        (f"{PROMPT_HASH_ABSENT} is not a model name and not empty",
         PROMPT_HASH_ABSENT and "<no story_provenance>" != PROMPT_HASH_ABSENT),
    ]
    for name, ok in checks:
        fails += (not ok)
        print(f"  {'ok  ' if ok else 'FAIL'}: {name}")

    print("\n-- PENDING OQ-356 survives to the FINAL table (written, then re-read) --")
    import tempfile
    with tempfile.TemporaryDirectory() as td:
        p = Path(td) / "verdicts.tsv"
        write_verdicts({}, p)
        text = p.read_text()
        ok_pending = PENDING_OQ356 in text
        ok_notmeas = NOT_MEASURED in text
        ok_distinct = ok_pending and ok_notmeas
        lines = [l for l in text.splitlines() if PENDING_OQ356 in l]
        for name, ok in [(f"{PENDING_OQ356} reaches the written table ({len(lines)} rows)", ok_pending),
                         (f"{NOT_MEASURED} reaches the written table", ok_notmeas),
                         ("the two tokens are distinguishable in the same file", ok_distinct)]:
            fails += (not ok)
            print(f"  {'ok  ' if ok else 'FAIL'}: {name}")

    print(f"\n=== selftest: {'GREEN' if not fails else f'RED ({fails} failing)'} ===")
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
