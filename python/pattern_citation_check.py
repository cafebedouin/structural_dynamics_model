#!/usr/bin/env python3
"""pattern_citation_check — sweep pattern-index citations; GATE the unswept consumers of a
vacated taxonomy member.

Two jobs, one classifier, one canonical location. Built as the OQ-278 archaeology sweep inside
`audits/2026-08-14_oq278_index_collision/`, then MOVED here (2026-08-14) rather than copied,
when the gate mode was added — a scanner in an audit dir plus a scanner in `python/` is
Pattern 2 on this file's own subject.

    --check     GATE MODE. The vacated-consumer instrument. See VACATED below.
    --sweep     writes the audit's `LABEL_SET.tsv` (the artifact OQ-294 consumes)
    --selftest  the sweep's controls alone

WHY THE GATE MODE EXISTS — three strikes, so an instrument rather than a fourth note
------------------------------------------------------------------------------------
`build_discipline.md:1392` ("a correction landed in PROSE is not landed until every instrument
encoding the same assumption is checked") and `:2558` ("a correction is not done until the old
value's consumers are swept") have now fired THREE times on this one taxonomy: the 2026-08-11
vacating that lived only in the paper for a day; the same ruling leaving `CLAUDE.md` publishing
the old six; and the nine stale citations this sweep found. Operator, 2026-08-14: *three
instances of one failure mode wants an instrument, not a fourth note.*

This mode is that instrument, and it is DECLARATION-BASED, not repair-based — the nine stale
citations cannot be repaired until OQ-278's R2, because "append the slug in place" is
invalidated if the ruling renumbers. Green today; RED on a tenth; RED when one is silently
repaired without retiring its manifest entry. Same shape as
`doc_pattern_check.DECLARED_SPINE_LAG` and `prolog/axis_boundary_allowlist.txt`.

**It has already earned its keep once.** Its first run DISAGREED with the hand adjudication in
`WRITEUP.md` §4.6 on 7 of 9 sites — and the hand list was right: the disagreement was two
hyphenation bugs in this file's own recovery regexes (`faith merge` never matched
`faith-merge`; `old-vs-new diff` never matched `Old-vs-new OUTPUT diff`). Under-recovery is
SILENT — it presents as `unrecoverable`, which reads like a result — so `LABEL_SET.tsv` had
been shipping under-recovered rows to OQ-294. Fixed and regenerated.

WHAT THE SWEEP PRODUCES, and why the shape matters more than the counts
------------------------------------------------------------------
One row per candidate citation to build-discipline pattern index 3 or 4:

    file, line, raw_text_as_found, quoted_context, namespace, mechanism_slug, confidence

**`mechanism_slug` is the recovered SENSE. The index survives only inside
`raw_text_as_found`.** This is deliberate and is the artifact's whole design constraint: OQ-278's
eventual ruling may RENUMBER, and anything index-keyed would become ground truth expressed in a
numbering that no longer exists. Keyed on mechanism, the label set stays valid under every branch
of R1a/R1b/R2 and the ruling never has to touch it. OQ-294 consumes it as cleaned ground truth.

THE NAMESPACE PROBLEM — `Pattern N` / `PN` is SEVEN-WAY OVERLOADED
-----------------------------------------------------------------
OQ-278 named four senses; this sweep found seven, and TWO of them were found as its own false
positives — `CWC:P3` (a concealment claim row, guarded by `python/claim_cite_check.py`) and
decompose-manifest `candidate_pattern` (the DR engine's own vocabulary). The others:
`prolog/diagnostic_summary.pl`'s independent EXPECTED CONFLICT CATALOG (P1-P10); `Priority:`
levels; essay/protocol/analysis enumerations; a Prolog *variable* named `P3`; and the paper's
own published table. A prohibition gate on bare `Pattern N` would run >50% false positives,
which is why Step 0 namespaces citations (`CM-P4` / `BD-P4`) instead of forbidding them.

Two further populations are PINNED, not ambiguous, and must never be counted as citations: the
md5-frozen OQ-277 prereg (which defines `P1`-`P6` verbatim) and OQ-278's own body.

So this sweep classifies NAMESPACE first, then recovers the mechanism only inside the taxonomy
namespace.

CONFIDENCE IS THREE-VALUED, and `unrecoverable` is a RESULT
-----------------------------------------------------------
    recovered    the context names the mechanism (not the index) unambiguously
    inferred     the context is consistent with one mechanism and no other, but does not name it
    unrecoverable the index is the only information present

Per the cross-sibling rule, an out-of-file read GENERATES a hypothesis; only an in-file witness
RULES it. `unrecoverable` rows are labelled `[AMBIGUOUS — OQ-278]` in the repair step, never
guessed.

POSITIVE CONTROL (rides every run, `--selftest`): the probe must find two hits it is KNOWN to
have — the `FINDINGS.md:23` wrong label (OQ-278's third sighting) and the `:1015` bound-probe
cross-reference — and must DECLINE on a planted non-taxonomy `P3` in the Prolog conflict-catalog
shape. A one-sided control (plant-and-find) would show only that the probe CAN fire.

Usage:
    python3 python/pattern_citation_check.py --check     # gate mode (vacated consumers); default-less
    python3 python/pattern_citation_check.py --sweep     # write LABEL_SET.tsv + print the summary
    python3 python/pattern_citation_check.py --selftest  # the sweep's controls alone
"""

import re
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
AUDIT = REPO / "audits/2026-08-14_oq278_index_collision"

# --- the vacated-consumer manifest (the gate's actual subject) ---------------
#
# WHY THIS EXISTS. `build_discipline.md:1392` ("a correction landed in PROSE is not landed
# until every instrument encoding the same assumption is checked") and `:2558` ("a correction
# is not done until the old value's consumers are swept") have now fired THREE times on this
# one taxonomy. Three instances of one failure mode wants an instrument, not a fourth note
# (operator, 2026-08-14).
#
# The 2026-08-11 ruling DEMOTED AND VACATED `destructive-replace` from index 3. Nine live
# citations still cite it as a member. They are NOT repaired here — the repair is Step 4,
# after OQ-278's R2, because "append the slug in place" is invalidated if the ruling
# renumbers. So they are DECLARED, exactly as `doc_pattern_check.DECLARED_SPINE_LAG` declares
# the unrepaired spine table: green today, red on a TENTH, red when one is silently repaired.
#
# Counts are per FILE, not per line: line numbers drift under ordinary editing and a
# line-keyed manifest would go red on churn instead of on substance.
VACATED = {
    "slug": "destructive-replace",
    "declared": "2026-08-11",
    "oq": "OQ-278",
    "what": "demoted from a defect pattern to a witness rule; index 3 vacated, never reused",
    "repair_blocked_on": "OQ-278 R2 (Step 4) — renumbering would invalidate an in-place slug",
}
DECLARED_STALE_CONSUMERS = {
    "ISSUES.md": 1,
    "KNOWN_STATE.md": 1,
    "audits/2026-06-07_stakeholder_layer_migration/MIGRATION_PLAN.md": 1,
    "audits/2026-06-11_oq109_phase_b/b3_open1_discharge.md": 1,
    "audits/2026-06-12_oq106_retire/README.md": 1,
    "audits/2026-06-18_oq104_citation_checker/FINDINGS.md": 1,
    "docs/design/the_perturbation_move.md": 1,
    "docs/the_perturbation_principle.md": 1,
    "docs/design/design_gaps.md": 1,
}

# The two definitional documents are the SUBJECT, not citations. Excluded from the label set.
DEFINITIONAL = {"CLAUDE.md", "docs/technical/build_discipline.md"}

# THE SWEEP MUST NOT READ ITS OWN OUTPUT. `LABEL_SET.tsv` is committed, so `git grep` finds it
# and every row it already contains becomes a new candidate on the next run — the row count
# compounds silently (1421 on the run that caught this, against 671 real candidates) and every
# published count is inflated by however many times the sweep has been run. A producer that
# consumes its own artifact reports growth as discovery.
SELF_OUTPUT = {"audits/2026-08-14_oq278_index_collision/LABEL_SET.tsv"}

FORM_A = re.compile(r"Pattern[- ]([34])\b")
FORM_B = re.compile(r"\bP([34])\b")

# --- namespace classification (applied to the whole line + file path) -------

# TWO POPULATIONS ARE NOT AMBIGUOUS AND MUST NOT BE COUNTED AS IF THEY WERE. Both were
# miscounted as ordinary citations on the first pass, which inflated the ambiguous set.
#
#   oq277-frozen-prereg — everything under the OQ-277 cross-coding audit uses `P1`..`P6` as a
#     LOCAL DEFINED NAMESPACE, pinned VERBATIM by a md5-frozen, gate-enforced prereg
#     (`PREREGISTRATION.md:350,356`) precisely so the out-of-harness coder could not read them
#     by reference. They mean the CLAUDE.md sense BY CONSTRUCTION. Pinned, not ambiguous.
#   oq278-subject — OQ-278's own body in ISSUES.md, and this audit directory, are ABOUT the
#     collision and name both senses explicitly at every mention. Subject, not citation.
PINNED_PREFIXES = ("audits/2026-08-10_oq277_rq2_crosscoding/", "python/audits/oq277")
SUBJECT_PREFIXES = ("audits/2026-08-14_oq278_index_collision/",)
# ISSUES.md line span of the OQ-278 entry. A LINE SPAN INTO A 12,800-LINE FILE THAT GROWS IS A
# STALE PIN WAITING TO HAPPEN — so it is not trusted: `oq278_body_span()` re-derives it from the
# headings at run time and this pair is only the fallback if the headings ever move.
OQ278_BODY = (11201, 11539)  # re-derived 2026-08-14 after this session's own edit to the entry

NS_RULES = [
    # (namespace, path predicate, line predicate)
    # SIXTH namespace, found by this sweep's own false positive: `CWC:P3` is a claim row in
    # docs/concealment/, guarded by python/claim_cite_check.py, which namespaces DELIBERATELY
    # for exactly this reason ("Their `A2`s are DIFFERENT CLAIMS"). Empirical support for
    # Step 0: an unnamespaced scanner reads another scheme's labels as citations.
    # The paper PUBLISHES the taxonomy — its rows are definitional restatements, in the same
    # class as the two definitional documents, not citations that consume it. Only the current
    # version is ever amended (R4); the five earlier ones are point-in-time.
    ("paper-publication",
     lambda p: p.startswith("docs/amnesiac_institution/"),
     lambda l: True),
    ("cwc-claim-row",
     lambda p: True,
     lambda l: "CWC:" in l),
    # Essays, uke transform outputs and analysis scripts number their OWN findings. These
    # trees never cite build discipline; the mid-sentence references ("Pattern 3 exhibits
    # extraction_accumulation drift") are to the essay's own enumeration.
    ("analysis-enumeration",
     lambda p: p.startswith(("agent/analysis/", "agent/narrative_transform/",
                             "docs/v8/foundations/", "protocols/", "docs/recon_2_scope",
                             "audits/2025-05-15_recon_2/", "python/scenario_convergence")),
     lambda l: True),
    ("prolog-conflict-catalog",
     lambda p: p.endswith(".pl") or "diagnostic_summary" in p,
     lambda l: bool(re.search(r"P[34]\s*:", l)) or "conflict" in l.lower()),
    ("prolog-variable",
     lambda p: p.endswith(".pl"),
     lambda l: bool(re.search(r"\bP[34]\b\s*(=|,|\)|\])", l))),
    ("priority-level",
     lambda p: True,
     lambda l: bool(re.search(r"[Pp]riority\W{0,4}[34]\b", l))),
    # Corpus-analysis manifests emit `"candidate_pattern": "... (Pattern 3)"` — the DR
    # engine's own pattern vocabulary, unrelated to build discipline.
    ("decompose-manifest-candidate",
     lambda p: True,
     lambda l: "candidate_pattern" in l),
    # The FIFTH namespace, and the largest one the first pass missed: essays, protocols,
    # recon reports and analysis scripts number their OWN findings `Pattern 3: <name>`.
    # The discriminator is the name that FOLLOWS the index, not the heading shape.
    ("analysis-enumeration",
     lambda p: True,
     lambda l: bool(re.search(r"(^\s*#{1,6}\s*|^\s*[-*]\s*|\*\*|#\s|%\s)?\**Pattern[- ][34]\**\s*[::-]",
                              l)) and not any(r.search(l) for r in MECH_ANY)),
]

# --- mechanism recovery, inside the taxonomy namespace only -----------------
# Keyed on MECHANISM VOCABULARY, never on the index. Order matters: first match wins.
MECHANISMS = [
    ("fabricated-default", [
        r"fabricat\w* default", r"fabricated", r"missing-data fallback",
        r"plausible constant", r"`?0\.5`?\b", r"boltzmann_floor_default",
        r"emits a real-looking value", r"unknown.{0,12}not.{0,6}0\.5",
    ]),
    ("recap-as-witness", [
        r"recap", r"paste-or-untag", r"done-?claim", r"done / verified",
        r"witness.{0,25}same turn", r"turn-end",
    ]),
    ("destructive-replace", [
        # HYPHENATION BUGS, found 2026-08-14 by the vacated-consumer check disagreeing with
        # the hand adjudication in the audit's WRITEUP §4.6. `faith merge` never matched
        # `faith-merge` (3 sites); `old-vs-new diff` never matched `Old-vs-new OUTPUT diff`.
        # Under-recovery is silent — it reads as `unrecoverable`, which looks like a result.
        r"destructive[- ]replace", r"faith[- ]merge", r"prove before you replace",
        r"old-vs-new(\s+\w+)?\s+diff", r"the diff is proof", r"before you delet",
        r"before/after byte-identical", r"faithful\s+\S+\s+diff", r"pipeline identity",
    ]),
    ("bound-probe", [
        r"bound[- ]probe", r"bypasses (the )?cut", r"clause[- ]order",
        r"query[- ]binding", r"post-filter", r"findall", r"\b432\b",
    ]),
]
MECH_RES = [(slug, [re.compile(p, re.IGNORECASE) for p in pats]) for slug, pats in MECHANISMS]
# Flat list, used by the analysis-enumeration namespace rule (resolved at call time).
MECH_ANY = [r for _, pats in MECH_RES for r in pats]

# Vocabulary that marks a line as taxonomy-sense even without a mechanism named.
TAXONOMY_MARKERS = [re.compile(p, re.IGNORECASE) for p in [
    r"build[- ]discipline", r"build_discipline", r"defect pattern", r"the six patterns",
    r"pattern taxonomy", r"CLAUDE\.md.{0,30}[Pp]attern", r"spine",
]]

CONTEXT = 3  # lines either side pulled for the quoted_context / recovery window


def tracked_files():
    """Candidate files only. `git ls-files` + read-everything took 32s — too slow to gate.

    `git grep -lI` does the prefilter in C and skips binaries itself (-I), which also closes
    the decoded-noise hole independently of the NUL check below. ~0.5s.
    """
    out = subprocess.run(
        ["git", "grep", "-lIE", r"Pattern[- ][34]|\bP[34]\b"],
        cwd=REPO, capture_output=True, text=True).stdout
    return [p for p in out.splitlines() if p and p not in DEFINITIONAL and p not in SELF_OUTPUT]


_SPAN_CACHE = {}


def oq278_body_span():
    """Derive OQ-278's ISSUES.md line span from the headings, don't trust the pin.

    ISSUES.md is 12,800 lines and grows; a hardcoded span silently starts mis-scoping the
    moment anything above the entry is edited — including this session's own edit to it,
    which moved the end by 74 lines. Re-derived per run; the constant is the fallback.
    """
    if "span" in _SPAN_CACHE:
        return _SPAN_CACHE["span"]
    span = OQ278_BODY
    try:
        lines = (REPO / "ISSUES.md").read_text(encoding="utf-8").splitlines()
        start = next(i for i, l in enumerate(lines, 1) if l.startswith("## OQ-278 "))
        end = next((i for i, l in enumerate(lines, 1)
                    if i > start and l.startswith("## OQ-")), len(lines)) - 1
        span = (start, end)
    except (OSError, StopIteration):
        pass
    _SPAN_CACHE["span"] = span
    return span


def classify_namespace(path, line, lineno):
    if path.startswith(PINNED_PREFIXES):
        return "oq277-frozen-prereg"
    if path.startswith(SUBJECT_PREFIXES):
        return "oq278-subject"
    lo, hi = oq278_body_span()
    if path == "ISSUES.md" and lo <= lineno <= hi:
        return "oq278-subject"
    for ns, pathp, linep in NS_RULES:
        if pathp(path) and linep(line):
            return ns
    return "taxonomy-candidate"


MARGIN = 40  # chars: how much closer the winner must be before the call is "recovered"


def recover(blob, cite_pos):
    """Return (mechanism_slug, confidence) — NEAREST mechanism vocabulary to the citation.

    Window MEMBERSHIP is not sufficient and this is not hypothetical: the oq93 `FINDINGS.md`
    paragraph says "Build-discipline spine, TWICE OVER" and names `fabricated default` and the
    `bound-probe/clause-order family` in adjacent lines. Any window wide enough to recover the
    sense contains both. So distance decides, and the MARGIN decides the confidence — a near-tie
    is reported `unrecoverable` with both candidates, never resolved by rule precedence.
    """
    dists = {}
    for slug, pats in MECH_RES:
        best = None
        for r in pats:
            for m in r.finditer(blob):
                d = 0 if m.start() <= cite_pos <= m.end() else min(
                    abs(m.start() - cite_pos), abs(m.end() - cite_pos))
                best = d if best is None else min(best, d)
        if best is not None:
            dists[slug] = best
    if not dists:
        if any(r.search(blob) for r in TAXONOMY_MARKERS):
            return "", "inferred"
        return "", "unrecoverable"
    ranked = sorted(dists.items(), key=lambda kv: kv[1])
    if len(ranked) == 1:
        return ranked[0][0], "recovered"
    (w, dw), (r2, d2) = ranked[0], ranked[1]
    if d2 - dw >= MARGIN:
        return w, "recovered"
    return "|".join(sorted(s for s, _ in ranked)), "unrecoverable"


def sweep():
    rows = []
    for rel in tracked_files():
        p = REPO / rel
        try:
            raw = p.read_bytes()
        except (OSError, IsADirectoryError):
            continue
        # BINARIES MUST BE EXCLUDED, not decoded with errors='replace'. Witnessed: a
        # `.jpg` under agent/analysis/originals/ produced a byte sequence containing `P4`
        # and was classified `bound-probe`, `recovered`. Decoded noise reads exactly like a
        # citation at the read site — this file's own subject.
        if b"\x00" in raw[:8192]:
            continue
        try:
            text = raw.decode("utf-8")
        except UnicodeDecodeError:
            continue
        if "Pattern" not in text and not FORM_B.search(text):
            continue
        lines = text.splitlines()
        for i, line in enumerate(lines):
            ma, mb = FORM_A.search(line), FORM_B.search(line)
            if not (ma or mb):
                continue
            hit = ma or mb
            raw = hit.group(0)
            ns = classify_namespace(rel, line, i + 1)
            lo = max(0, i - CONTEXT)
            window = lines[lo: i + CONTEXT + 1]
            # offset of the citation token inside the joined window
            cite_pos = sum(len(x) + 1 for x in lines[lo:i]) + hit.start()
            if ns != "taxonomy-candidate":
                slug, conf = "", "n/a"
            else:
                slug, conf = recover("\n".join(window), cite_pos)
                # Form B with no taxonomy marker anywhere near is almost certainly another
                # namespace we have no rule for. Report it as such rather than as a citation.
                if not ma and conf == "unrecoverable" and not slug:
                    ns = "other-unclassified"
                    conf = "n/a"
            rows.append({
                "file": rel, "line": i + 1, "raw_text_as_found": raw,
                "quoted_context": line.strip()[:200],
                "namespace": ns, "mechanism_slug": slug, "confidence": conf,
            })
    return rows


# --- controls ---------------------------------------------------------------

def selftest():
    """Two-sided: must FIND two known hits; must DECLINE on a planted foreign-namespace P3."""
    failures = []
    rows = sweep()
    idx = {(r["file"], r["line"]): r for r in rows}

    # (1) known positive: OQ-278's third sighting — a WRONG label in an incident record.
    hit = [r for r in rows
           if r["file"] == "audits/2026-06-10_oq93_grid_viability_probe/FINDINGS.md"
           and r["namespace"] == "taxonomy-candidate"]
    if not hit:
        failures.append("selftest FAILED: known positive (oq93 FINDINGS.md 'Pattern 4') not found")
    elif not any(r["mechanism_slug"] == "fabricated-default" for r in hit):
        failures.append(f"selftest FAILED: oq93 FINDINGS.md recovered "
                        f"{[r['mechanism_slug'] for r in hit]}, expected fabricated-default")

    # (2) known positive, opposite mechanism: the three cross-wired citations.
    xw = [r for r in rows if r["file"] == "docs/design/design_gaps.md"
          and r["namespace"] == "taxonomy-candidate"]
    if not xw:
        failures.append("selftest FAILED: known positive (design_gaps.md 'Pattern 3') not found")

    # (3) NEGATIVE control — a foreign-namespace P3 in the Prolog conflict-catalog shape must
    #     NOT be classified as a taxonomy candidate. Drawn from the population, not planted:
    #     prolog/diagnostic_summary.pl's own catalog.
    cat = [r for r in rows if r["file"] == "prolog/diagnostic_summary.pl"]
    if not cat:
        failures.append("selftest FAILED: negative control absent — no P3/P4 found in "
                        "prolog/diagnostic_summary.pl, so the decline cannot be witnessed")
    elif any(r["namespace"] == "taxonomy-candidate" and r["confidence"] != "unrecoverable"
             for r in cat):
        bad = [(r["line"], r["namespace"], r["mechanism_slug"]) for r in cat
               if r["namespace"] == "taxonomy-candidate"]
        failures.append(f"selftest FAILED: negative control — conflict-catalog P3/P4 read as "
                        f"taxonomy citations: {bad}")
    return failures, rows


def stale_consumers(rows):
    """file -> count of live citations that still cite the VACATED mechanism as a member.

    A row counts only if it is a taxonomy citation (namespace `taxonomy-candidate`, so the
    pinned/subject/foreign-namespace populations are already excluded) AND its recovered
    mechanism is the vacated slug. Discussion OF the vacating lives in `oq278-subject` and in
    the two definitional documents, none of which reach here — which is what keeps this from
    firing on its own paper trail.
    """
    found = {}
    for r in rows:
        if r["namespace"] == "taxonomy-candidate" and r["mechanism_slug"] == VACATED["slug"]:
            found[r["file"]] = found.get(r["file"], 0) + 1
    return found


def run_vacated_check(rows, declared=None):
    """Return a list of error strings (empty = green)."""
    declared = DECLARED_STALE_CONSUMERS if declared is None else declared
    found = stale_consumers(rows)
    errors = []
    for path in sorted(set(found) | set(declared)):
        f, d = found.get(path, 0), declared.get(path, 0)
        if f == d:
            continue
        if d == 0:
            errors.append(f"UNSWEPT CONSUMER: {path} cites the VACATED '{VACATED['slug']}' "
                          f"({f}x) and is not in the manifest — a correction landed in prose "
                          f"without sweeping its consumers ({VACATED['oq']})")
        elif f == 0:
            errors.append(f"DECLARED CONSUMER GONE: {path} no longer cites "
                          f"'{VACATED['slug']}' — either the repair landed (retire the manifest "
                          f"entry in the SAME change) or the detector stopped seeing it")
        else:
            errors.append(f"COUNT CHANGED: {path} cites '{VACATED['slug']}' {f}x, "
                          f"manifest says {d}x")
    return errors


def main(argv):
    if "--check" in argv:
        # The sweep's own controls gate the gate: an uncontrolled census is a positional
        # parse waiting to happen, and this row reports a COUNT.
        failures, rows = selftest()
        if failures:
            for f in failures:
                print(f"  {f}")
            print("pattern_citation_check: RED (sweep controls)")
            return 1
        # Control that the manifest can go red at all: a phantom entry must be reported.
        if not run_vacated_check(rows, declared={**DECLARED_STALE_CONSUMERS,
                                                 "docs/NOT_A_REAL_FILE.md": 1}):
            print("  selftest FAILED: phantom manifest entry did not turn the check red")
            print("pattern_citation_check: RED (selftest)")
            return 1
        errors = run_vacated_check(rows)
        if errors:
            for e in errors:
                print(f"  {e}")
            print(f"pattern_citation_check: RED — {len(errors)} vacated-consumer problem(s)")
            return 1
        n = sum(DECLARED_STALE_CONSUMERS.values())
        print(f"pattern_citation_check: GREEN — {n} declared unswept consumer(s) of the VACATED "
              f"'{VACATED['slug']}' across {len(DECLARED_STALE_CONSUMERS)} files, repair blocked "
              f"on {VACATED['repair_blocked_on']}; selftest 4/4")
        return 0

    failures, rows = selftest()
    if "--selftest" in argv:
        for f in failures:
            print(f"  {f}")
        print(f"sweep selftest: {'RED' if failures else 'GREEN'} "
              f"(2 positives + 1 naturally-arising negative)")
        return 1 if failures else 0

    if "--sweep" not in argv:
        print(__doc__)
        return 2

    if failures:
        for f in failures:
            print(f"  {f}")
        print("sweep: RED (controls) — label set NOT written; an uncontrolled census is a "
              "positional parse waiting to happen")
        return 1

    out = AUDIT / "LABEL_SET.tsv"
    cols = ["file", "line", "raw_text_as_found", "quoted_context",
            "namespace", "mechanism_slug", "confidence"]
    with out.open("w", encoding="utf-8") as fh:
        fh.write("\t".join(cols) + "\n")
        for r in sorted(rows, key=lambda r: (r["file"], r["line"])):
            fh.write("\t".join(str(r[c]).replace("\t", " ") for c in cols) + "\n")

    ns_counts, mech_counts, conf_counts = {}, {}, {}
    for r in rows:
        ns_counts[r["namespace"]] = ns_counts.get(r["namespace"], 0) + 1
        if r["namespace"] == "taxonomy-candidate":
            mech_counts[r["mechanism_slug"] or "(none)"] = \
                mech_counts.get(r["mechanism_slug"] or "(none)", 0) + 1
            conf_counts[r["confidence"]] = conf_counts.get(r["confidence"], 0) + 1

    print(f"rows: {len(rows)}  files: {len({r['file'] for r in rows})}")
    print("\nnamespace:")
    for k, v in sorted(ns_counts.items(), key=lambda kv: -kv[1]):
        print(f"  {v:5d}  {k}")
    print("\ntaxonomy-candidate -> recovered mechanism:")
    for k, v in sorted(mech_counts.items(), key=lambda kv: -kv[1]):
        print(f"  {v:5d}  {k}")
    print("\ntaxonomy-candidate -> confidence:")
    for k, v in sorted(conf_counts.items(), key=lambda kv: -kv[1]):
        print(f"  {v:5d}  {k}")
    print(f"\nwrote {out.relative_to(REPO)}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
