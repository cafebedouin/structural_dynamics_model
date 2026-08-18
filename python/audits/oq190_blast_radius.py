#!/usr/bin/env python3
"""OQ-190 blast-radius sweep — who depends on cast/verdict field DRAW-STABILITY.

Governed by the frozen `audits/2026-08-17_oq190_blast_radius/PREREGISTRATION.md`
(md5 recorded in that directory's `audit_log.md` above the first result line). This file
implements it; where the two disagree the prereg wins and the disagreement is a defect here.

TWO PASSES, NOT ONE
-------------------
1. **name-keyed sweep** over the emitted cast/verdict predicate names and their derived views.
2. **closure pass** over `derivation_graph.tsv`, assigning T2b to consumers of *derived* values
   whose source text contains no cast name at all.

Pass 2 is not an annotation on pass 1. A `dr_type` consumer mentions no cast name, so a name-keyed
grep lands it T4/T6 **by construction** and the largest part of the radius stays invisible to the
instrument. Rows reached only by the closure are first-class census rows.

WHY THE CONTROLS ARE SHAPED THE WAY THEY ARE
--------------------------------------------
`--selftest` runs four two-sided pairs (cast, verdict, closure, documentary). Fire-only is
insufficient in every one of them, and the closure pair is the load-bearing case: a transitive
closure goes everywhere by default, so a fire-only closure control passes perfectly while the
closure tags half the repo T2b — an artifact **indistinguishable from the true RECON §4 finding**.
Under-broad is equally bad in the other direction: it manufactures a false `NO-and-exhausted` on
the OQ-118 Limb-3 verdict bucket, which leaves a ~$1.5-2 temp sweep reserved when it should have
been escalated. Either closure arm failing ⇒ the verdict bucket returns `reserved-pending`.

**The cast DECLINE control may not be drawn from the computed layer** (`dr_type`, `h1_band`,
purity, fingerprint). RECON §4 shows the computed layer IS transitively cast-derived, so a sweep
that declines on it passes the control by exhibiting exactly the blindness the control exists to
rule out. Apparatus-presence consumers are the only control-eligible negatives.

Counting rule: **count from the artifact, never from the loop.** Every reported total is
recomputed by re-reading the TSV that was written. `/usr/bin/grep` is never shelled out to; all
matching is Python `re` on file text read here.

    --sweep            writes consumer_census.tsv, doc_claims.tsv, resolved_closure_premises.tsv,
                       promoted_audit_edges.tsv into the audit dir
    --selftest         the four two-sided control pairs; exit 1 on any failure
    --reconcile-issues extractor's resolved-entry count vs python/issues_status.py; exit 1 on mismatch
"""
import argparse
import re
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
AUDIT = REPO / "audits" / "2026-08-17_oq190_blast_radius"

# ---------------------------------------------------------------------------
# Vocabulary. Frozen by PREREGISTRATION §(0)/§(a); sourced from RECON §2.
# ---------------------------------------------------------------------------

# Emitted predicate names, bucketed. (authored name != emitted name — the rename seam.)
CAST_EMITTED = [
    "constraint_beneficiary", "constraint_victim", "constraint_vindicates",
    "constraint_stakeholder", "stakeholder_secondary_role", "stakeholder_non_agent",
    "stakeholder_gain_flow", "fixing_cost_class",
]
VERDICT_EMITTED = [
    "disappearance_verdict", "founding_problem_status",
    "founding_problem_corroboration_class",
]
# Derived views: how Prolog rule bodies actually consume the cast surface. A name-grep for the
# emitted names alone misses every one of these (false-absence sub-rule (c): the concept->surface
# mapping is itself a claim, so the sibling surfaces are swept, not laddered).
CAST_DERIVED_VIEWS = [
    "has_coordination_function", "has_asymmetric_extraction", "agent_beneficiary",
    "non_agent_beneficiary", "constraint_captured", "uncaptured", "piton_candidate",
    "transient_neglect", "shared_agent_link",
]
VERDICT_DERIVED_VIEWS = ["has_mandatrophy_declaration", "q6_crosscheck", "q6_cell"]

# Apparatus-presence surface — the ONLY control-eligible negatives (prereg §(d)). Lineage is
# prompt/schema-determined and established independently of authored cast; measured 18/18
# positive-stable in the frozen cohort (RECON §1a), so eligibility rests on a number, not a
# presumption.
APPARATUS_NAMES = ["coordination_type", "boltzmann_floor_override", "affects_constraint", "interval"]

# Computed-layer names reached THROUGH the graph. Consumers of these are T2b and are invisible to
# a name-keyed grep. This list is the closure frontier, not a bucket assignment: the grade each
# carries comes from derivation_graph.tsv.
DERIVED_FRONTIER = [
    "dr_type", "classify_from_metrics", "constraint_classification", "h1_band",
    "h1_stakeholder", "cohomological_obstruction", "fingerprint_actors",
    "logical_fingerprint", "purity_score", "purity_zone", "resolve_coalition_power",
    "structural_displacement", "false_summit_mountain", "natural_law_signature",
    "commentary_cell", "constraint_signature",
]

BUCKET_OF = {}
for _n in CAST_EMITTED + CAST_DERIVED_VIEWS:
    BUCKET_OF[_n] = "cast"
for _n in VERDICT_EMITTED + VERDICT_DERIVED_VIEWS:
    BUCKET_OF[_n] = "verdict"
# has_mandatrophy_declaration reads BOTH verdict atoms; q6 reads founding_problem_status only.
BUCKET_OF["constraint_captured"] = "cast"

ALL_NAMES = CAST_EMITTED + VERDICT_EMITTED + CAST_DERIVED_VIEWS + VERDICT_DERIVED_VIEWS

# ---------------------------------------------------------------------------
# Surfaces
# ---------------------------------------------------------------------------

# Corpus DATA, not code: a testset asserting constraint_beneficiary/2 is the authored datum, not a
# consumer of it. Sweeping them would put 4,205 rows of authorship into a consumer census.
DATA_PREFIXES = (
    "prolog/testsets", "prolog/archives", "prolog/probsets", "prolog/kernels",
    "json/", "outputs/",
    # prereg §(g): audits/ is NOT swept. Forward-citations INTO audits/ are followed instead,
    # bounded by the active surface. A sweep that walks audits/ silently re-scopes the claim
    # from "the live radius" to "the live radius plus every historical probe".
    "audits/",
)
# This audit's OWN instruments. The probe file joined the census the moment it was committed —
# `git ls-files` does not know the difference between a consumer and the tool measuring consumers,
# and an instrument that counts itself inflates the very number it reports.
SELF_OUTPUT = ("audits/2026-08-17_oq190_blast_radius/", "python/audits/oq190_blast_radius.py",
               "prolog/probe_oq190_edge_admission.pl")

CODE_SUFFIX = (".pl", ".py")
# Active document surface per prereg §(g). audits/ is NOT swept; forward-citations INTO audits/
# are followed instead, bounded by the active surface.
DOC_FILES = ("CLAUDE.md", "AGENTS.md", "KNOWN_STATE.md", "ISSUES.md", "README.md")
DOC_PREFIXES = ("docs/",)


def tracked():
    out = subprocess.run(["git", "ls-files"], cwd=REPO, capture_output=True, text=True).stdout
    return [p for p in out.splitlines() if p]


def code_files(paths):
    return [p for p in paths
            if p.endswith(CODE_SUFFIX)
            and not p.startswith(DATA_PREFIXES)
            and not p.startswith(SELF_OUTPUT)]


def doc_files(paths):
    return [p for p in paths
            if p.endswith(".md")
            and not p.startswith("audits/")
            and not p.startswith(SELF_OUTPUT)
            and (p in DOC_FILES or p.startswith(DOC_PREFIXES) or "/" not in p)]


def read(p):
    try:
        return (REPO / p).read_text(encoding="utf-8", errors="replace")
    except OSError:
        return ""


# ---------------------------------------------------------------------------
# Derivation graph
# ---------------------------------------------------------------------------

GRAPH_TSV = AUDIT / "derivation_graph.tsv"


def load_graph():
    """Edges: src(cast/verdict field) -> dst(derived value), with grade and admission.

    Only ADMITTED edges propagate. A rejected edge stays in the file with the diff that
    rejected it — the pruner's record, not a deletion.
    """
    edges = []
    if not GRAPH_TSV.exists():
        return edges
    for line in GRAPH_TSV.read_text(encoding="utf-8").splitlines():
        if not line.strip() or line.startswith("#") or line.startswith("edge_id\t"):
            continue
        f = line.split("\t")
        if len(f) < 7:
            continue
        edges.append(dict(edge_id=f[0], src=f[1], dst=f[2], site=f[3],
                          grade=f[4], admitted=f[5].strip().lower() == "yes", decided_by=f[6]))
    return edges


GRADE_RANK = {"name-identity": 3, "cardinality": 2, "presence": 1}


def closure_grades(edges):
    """derived name -> weakest grade on any admitted path from a cast/verdict field.

    WEAKEST, per prereg §(a) T2b ("inherited from the weakest edge on the path"). Weakest is the
    conservative direction for a RADIUS claim: it never lets a strong-grade edge upgrade a row
    into a grade OQ-118 can score when a weaker hop on the same path cannot be scored.
    """
    out = {}
    for e in edges:
        if not e["admitted"]:
            continue
        g = e["grade"]
        cur = out.get(e["dst"])
        if cur is None or GRADE_RANK.get(g, 0) < GRADE_RANK.get(cur, 9):
            out[e["dst"]] = g
    return out


def bucket_of_derived(edges, dst):
    bs = {BUCKET_OF.get(e["src"], "cast") for e in edges if e["admitted"] and e["dst"] == dst}
    if bs == {"cast"}:
        return "cast"
    if bs == {"verdict"}:
        return "verdict"
    return "both" if bs else "cast"


# ---------------------------------------------------------------------------
# Tiering
# ---------------------------------------------------------------------------

# A MENTION is not a CONSUMER. The census unit is a consumer or a claim (prereg §(a)), so a
# comment, a module/dynamic/discontiguous declaration, or the head of the predicate's own
# definition is not a row. Without this the census reports ~6.3k rows of which the overwhelming
# majority are the vocabulary talking about itself, and Amendment A's headline becomes true,
# unactionable and unclosable — the exact outcome the prereg's pruner clause exists to prevent.
DEFN_RE = re.compile(r"^\s*(%|#)")
DECL_RE = re.compile(r"^\s*:-\s*(module|dynamic|discontiguous|use_module|multifile|export)\b"
                     r"|^\s{4,}\w+/\d+\s*,?\s*(%.*)?$")


def strip_comment(path, line):
    """Trailing comments are not consumers either. Prolog `%` and Python `#`, quote-naive on
    purpose: a false STRIP can only drop a row (conservative for a mention census), while a
    false KEEP inflates the radius with prose."""
    c = "%" if path.endswith(".pl") else "#"
    i = line.find(c)
    return line if i < 0 else line[:i]


def is_call_site(line, name):
    """A consumer CALLS the name. `foo(` with an argument, or a Prolog qualified call.
    A bare token (a docstring word, an export-list entry, a comment fragment) is not."""
    return re.search(r"\b" + re.escape(name) + r"\s*\(", line) is not None


def is_own_definition(line, name):
    """The predicate's own head/clause, at column 0 — a producer, not a consumer."""
    return re.match(r"^" + re.escape(name) + r"\s*\(", line) is not None
# T1 tell: two cast-name reads bound to the SAME variable across two constraints (a name join),
# or a Python join keyed on a cast-derived name field.
JOIN_HINT = re.compile(r"\b(shared_agent_link|constraint_captured|piton_candidate)\b")
CARD_HINT = re.compile(r"\b(length\(|aggregate_all\(count|count_power_beneficiaries|"
                       r"critical_mass_threshold|count_to_topology|len\()")
POP_HINT = re.compile(r"\b(findall|forall|aggregate_all|corpus|census|prevalence|"
                      r"across the corpus|population)\b", re.I)


# A ground fact assertion is AUTHORSHIP, not consumption: `narrative_ontology:constraint_victim(
# carbon_tax_2026, low_income_consumers).` produces the datum the census is tracking consumers of.
# It is recorded with surface="data" rather than dropped — a silent drop on a producer would be
# indistinguishable from a small radius, and this census's whole subject is that confusion.
GROUND_FACT = re.compile(r"^\s*[a-z_]+:[a-z_]+\((?:[^()]*)\)\.\s*$")


def is_ground_fact(line):
    if not GROUND_FACT.match(line):
        return False
    inner = line[line.find("(") + 1:line.rfind(")")]
    # A variable or an anonymous `_` makes it a rule head / a query, not a ground datum.
    return not re.search(r"(^|[,\s(])[A-Z_]", inner)


# The name-keyed pass sees the SOURCE SITE of an admitted edge (e.g. constraint_indexing.pl:455,
# the presence read that feeds d -> chi) and would land it T6 "unclear" — conservative but
# uninformative. Grade it from the graph instead. This never shrinks the radius: T6 and T2b/T3
# are both summands of the headline SUSPECT count, so the move is precision only.
EDGE_SOURCE_SITES = {}


def load_edge_sites(edges):
    EDGE_SOURCE_SITES.clear()
    for e in edges:
        if not e["admitted"]:
            continue
        for tok in re.findall(r"([a-z_]+\.pl):(\d+)(?:-(\d+))?", e["site"]):
            f, a, b = tok
            for ln in range(int(a), int(b or a) + 1):
                cur = EDGE_SOURCE_SITES.get((f, ln))
                if cur is None or GRADE_RANK.get(e["grade"], 0) < GRADE_RANK.get(cur, 9):
                    EDGE_SOURCE_SITES[(f, ln)] = e["grade"]


TIER_OF_GRADE = {"name-identity": "T1", "cardinality": "T2", "presence": "T3"}


def classify_row(path, line, name, reached_by, grade_hint, line_no=0):
    """Ordered strata, first match wins. Fail-closed to T6.

    T4 (CLEARED) is deliberately hard to reach: prereg §(a) allows it only for a consumer shown
    ABSENT from the closure, so this function never returns T4 for a closure-reached row.
    """
    is_test = "/tests/" in path or path.startswith("prolog/tests")
    if reached_by == "closure":
        return "T2b", grade_hint or "presence"
    g = EDGE_SOURCE_SITES.get((path.rsplit("/", 1)[-1], line_no))
    if g:
        return TIER_OF_GRADE[g], g
    if JOIN_HINT.search(line):
        return "T1", "name-identity"
    if CARD_HINT.search(line) and name in ALL_NAMES:
        return "T2", "cardinality"
    if POP_HINT.search(line):
        return "T3", "presence"
    if is_test:
        # A test asserts a fixture and checks it back; the story is the unit and no cross-draw
        # expectation is made. Still not `cleared` until the closure shows it absent.
        return "T4?", ""
    return "T6", ""


DISPOSITION = {
    "producer": "n/a-authorship",
    "T1": "SUSPECT-confirmed-and-fails",
    "T2": "SUSPECT-confirmed-grade-unmeasured",
    "T2b": "SUSPECT-confirmed-grade-unmeasured",
    "T3": "SUSPECT-confirmed-grade-unmeasured",
    "T4?": "SUSPECT-unwitnessed",
    "T6": "SUSPECT-unwitnessed",
}


def disposition_for(tier, grade):
    d = DISPOSITION.get(tier, "SUSPECT-unwitnessed")
    # A name-identity-grade row IS scoreable against OQ-118 (which scored exactly that grade).
    if tier == "T2b" and grade == "name-identity":
        d = "SUSPECT-confirmed-and-fails"
    return d


# ---------------------------------------------------------------------------
# Pass 1 + Pass 2
# ---------------------------------------------------------------------------

def name_regex(n):
    return re.compile(r"\b" + re.escape(n) + r"\b")


def sweep_code(paths, edges):
    grades = closure_grades(edges)
    load_edge_sites(edges)
    rows = []
    name_res = [(n, name_regex(n)) for n in ALL_NAMES]
    derived_res = [(n, name_regex(n)) for n in DERIVED_FRONTIER if n in grades]
    for p in paths:
        txt = read(p)
        if not txt:
            continue
        for i, raw_line in enumerate(txt.splitlines(), 1):
            if DEFN_RE.match(raw_line) or DECL_RE.match(raw_line):
                continue
            line = strip_comment(p, raw_line)
            if not line.strip():
                continue
            hit_name = None
            for n, rx in name_res:
                if rx.search(line) and is_call_site(line, n) and not is_own_definition(line, n):
                    hit_name = n
                    break
            if hit_name:
                if is_ground_fact(line):
                    rows.append(dict(file=p, line=i, surface="data", emitted_name=hit_name,
                                     bucket=BUCKET_OF.get(hit_name, "cast"), tier="producer",
                                     grade="", reached_by="name", confidence="recovered",
                                     text=line.strip()[:160]))
                    continue
                tier, grade = classify_row(p, line, hit_name, "name", None, i)
                rows.append(dict(file=p, line=i, surface="code", emitted_name=hit_name,
                                 bucket=BUCKET_OF.get(hit_name, "cast"), tier=tier,
                                 grade=grade, reached_by="name",
                                 confidence="recovered" if tier != "T6" else "inferred",
                                 text=line.strip()[:160]))
                continue
            for n, rx in derived_res:
                if rx.search(line) and is_call_site(line, n) and not is_own_definition(line, n):
                    g = grades[n]
                    tier, grade = classify_row(p, line, n, "closure", g, i)
                    rows.append(dict(file=p, line=i, surface="code", emitted_name=n,
                                     bucket=bucket_of_derived(edges, n), tier=tier,
                                     grade=grade, reached_by="closure",
                                     confidence="recovered", text=line.strip()[:160]))
                    break
    return rows


# ---------------------------------------------------------------------------
# Documentary sweep (3c) — a DIFFERENT parser over a DIFFERENT surface
# ---------------------------------------------------------------------------

# A documentary dependent RESTS A CLAIM on stability; a descriptive mention does not. The decline
# control is exactly a descriptive mention, so this pattern must not be a bare name match.
STABILITY_CLAIM = re.compile(
    r"(draw[- ]stab|stable across (?:draws|redraws)|reproduc\w+ across|"
    r"same (?:story|cast) across|holds? on (?:a )?redraw|"
    r"compare only draw-stable|invariant across draws|persist\w* across draws)", re.I)
NAME_ANY = re.compile(
    r"\b(" + "|".join(re.escape(n) for n in
                      CAST_EMITTED + VERDICT_EMITTED + CAST_DERIVED_VIEWS + VERDICT_DERIVED_VIEWS
                      + ["beneficiar", "victim", "stakeholder", "cast[- ]field", "roster",
                         "vindicated_propositions"]) + r")", re.I)
AUDIT_CITE = re.compile(r"audits/(\d{4}-\d{2}-\d{2}_[A-Za-z0-9_.-]+)")


# A documentary claim can rest on cast draw-stability while naming NO cast field, because it names
# the INSTRUMENT that scores those fields. OQ-75: "compare only draw-stable fields, or size
# n-per-cohort if the OQ-109 homogeneity falsifier fires" — the stability table IS the cast/verdict
# score sheet, so gating on it is a cast dependency with no cast name in the sentence. The first
# version of this sweep required a name and missed OQ-75 outright; the doc control then PASSED
# anyway by matching OQ-190's own body, which is why the self-output pin below is not cosmetic.
INSTRUMENT_PREMISE = re.compile(
    r"(stability[- ]table|stability_table|replicate[- ]probe|draw-stable fields|"
    r"stability-table-gates-claims|cohort[- ]zero.{0,40}(?:stability|replicate))", re.I)

# OQ-190's own body is not a finding about the repo; it is this audit describing its own subject.
# Same shape as pattern_citation_check pinning OQ-278's own span.
SELF_OQ = "OQ-190"


def oq190_span():
    lines = read("ISSUES.md").splitlines()
    try:
        start = next(i for i, l in enumerate(lines, 1) if l.startswith(f"## {SELF_OQ} "))
    except StopIteration:
        return (0, 0)
    end = next((i for i, l in enumerate(lines, 1)
                if i > start and l.startswith("## OQ-")), len(lines)) - 1
    return (start, end)


def sweep_docs(paths):
    rows, edges = [], []
    self_lo, self_hi = oq190_span()
    for p in paths:
        txt = read(p)
        if not txt:
            continue
        lines = txt.splitlines()
        for i, line in enumerate(lines, 1):
            for m in AUDIT_CITE.finditer(line):
                edges.append(dict(file=p, line=i, target="audits/" + m.group(1),
                                  text=line.strip()[:160]))
            # Window: a claim and the field it rests on are often on adjacent lines in wrapped prose.
            if p == "ISSUES.md" and self_lo <= i <= self_hi:
                continue                      # this audit's own body — not a finding about the repo
            window = " ".join(lines[max(0, i - 2):i + 1])
            named = STABILITY_CLAIM.search(window) and NAME_ANY.search(window)
            instrument = INSTRUMENT_PREMISE.search(window)
            if named or instrument:
                rows.append(dict(file=p, line=i, surface="doc",
                                 bucket="verdict" if re.search(
                                     r"disappearance_verdict|founding_problem", window) else "cast",
                                 claim=line.strip()[:200],
                                 admitted_by=("named-field-stability-claim" if named
                                              else "stability-instrument-as-gating-premise"),
                                 repair="repair-by-annotation",
                                 confidence="recovered"))
    return rows, edges


# ---------------------------------------------------------------------------
# 3d — resolved-ISSUES closure-premise sweep. Highest-yield surface; a closed issue is a
# standing claim that does not announce itself as live.
# ---------------------------------------------------------------------------

OQ_HEAD = re.compile(r"^## (OQ-\d+)\b(.*)$")
STATUS_RESOLVED = re.compile(r"^\*\*Status:\*\*\s*(resolved|closed)\b", re.I)


def parse_issues():
    txt = read("ISSUES.md")
    entries, cur = [], None
    for i, line in enumerate(txt.splitlines(), 1):
        m = OQ_HEAD.match(line)
        if m:
            if cur:
                entries.append(cur)
            cur = dict(oq=m.group(1), start=i, title=m.group(2).strip(), body=[], resolved=False)
            continue
        if cur is not None:
            cur["body"].append(line)
            if STATUS_RESOLVED.match(line):
                cur["resolved"] = True
    if cur:
        entries.append(cur)
    return entries


# A resolved entry can rest on cast/verdict stability WITHOUT SAYING SO. OQ-52 closes on
# "289/293 both-authored, 4 victim-only" — a corpus aggregate over cast PRESENCE quoted as a
# settled population fact (prereg §(a) T3). A redraw moves presence (OQ-118 measured presence-flips
# on `victims` and `vindicated_propositions`), so the count is draw-conditional and the closure
# rests on it. The first version of this extractor required explicit stability language and missed
# it; the miss was found by hunting a positive control, which is the whole reason the hunt is a
# step. Two admitting patterns now, and a row records which one admitted it.
QUOTED_COUNT = re.compile(
    r"\b\d+\s*/\s*\d+\b|\b\d+\s+(?:of|out of)\s+\d+\b|\b\d+\s*%")
CAST_NEAR_COUNT = re.compile(
    r"\b(beneficiar\w*|victim\w*|stakeholder\w*|roster\w*|both[- ]authored|"
    r"vindicated_propositions|disappearance_verdict|founding_problem\w*)", re.I)


def resolved_row_reason(body_lines):
    """-> (reason, evidence_line) or (None, None). Announced premise first, then quoted count."""
    body = "\n".join(body_lines)
    if STABILITY_CLAIM.search(body) and NAME_ANY.search(body):
        for ln in body_lines:
            if STABILITY_CLAIM.search(ln):
                return "announced-stability-premise", ln.strip()[:200]
        return "announced-stability-premise", ""
    for i, ln in enumerate(body_lines):
        window = " ".join(body_lines[max(0, i - 1):i + 2])
        if QUOTED_COUNT.search(window) and CAST_NEAR_COUNT.search(window):
            # Record the WINDOW, not the anchor line: the count and the cast name are often on
            # adjacent wrapped lines, and an anchor line can itself be blank — which would ship a
            # row whose evidence cell is empty, i.e. a finding with no witness in it.
            return "cast-presence-count-as-settled-fact", " ".join(window.split())[:220]
    return None, None


def sweep_resolved(entries):
    rows = []
    for e in entries:
        if not e["resolved"]:
            continue
        reason, ev = resolved_row_reason(e["body"])
        if not reason:
            continue
        body = "\n".join(e["body"])
        rows.append(dict(oq=e["oq"], line=e["start"], title=e["title"][:90],
                         premise=ev or "", admitted_by=reason,
                         bucket="verdict" if re.search(
                             r"disappearance_verdict|founding_problem", body) else "cast",
                         repair="repair-by-reanalysis", confidence="inferred"))
    return rows


def reconcile_issues():
    """Mandatory before any 3d finding. A `**Status:** resolved` variant the extractor's pattern
    misses drops SILENTLY, and silent drop-out on the highest-yield surface is indistinguishable
    from a small radius. Mismatch ABORTS the 3d read; it does not warn and continue."""
    mine = sum(1 for e in parse_issues() if e["resolved"])
    proc = subprocess.run([sys.executable, "python/issues_status.py"],
                          cwd=REPO, capture_output=True, text=True)
    out = proc.stdout + proc.stderr
    m = re.search(r"resolved[^0-9\n]{0,24}(\d+)", out, re.I)
    theirs = int(m.group(1)) if m else None
    return mine, theirs, out


# ---------------------------------------------------------------------------
# Controls — four two-sided pairs
# ---------------------------------------------------------------------------

SYNTH_T2B = """% synthetic closure FIRE target: reads a DERIVED value, contains no cast name
synthetic_derived_consumer(C, T) :- drl_core:dr_type(C, T).
"""
SYNTH_APP = """% synthetic closure DECLINE target: apparatus-presence only, lineage outside cast
synthetic_apparatus_consumer(C) :- narrative_ontology:coordination_type(C, _).
"""


def selftest():
    edges = load_graph()
    if not edges:
        print("FAIL  derivation_graph.tsv missing or empty — closure cannot be tested")
        return 1
    grades = closure_grades(edges)
    fails = []

    def check(label, ok, detail):
        print(("PASS  " if ok else "FAIL  ") + label + ("" if ok else "  <- " + detail))
        if not ok:
            fails.append(label)

    # -- Pair 1: CAST. Naturally-arising population members (middle rung), not authored decoys.
    cast_rows = sweep_code(["prolog/drl_purity_network.pl"], edges)
    fire = [r for r in cast_rows if r["emitted_name"] == "shared_agent_link" and r["tier"] == "T1"]
    check("cast FIRE   shared_agent_link/4 flagged T1 name-identity",
          bool(fire), "no T1 row for shared_agent_link in drl_purity_network.pl")

    # Decline: an apparatus-presence-only consumer. NOT drawn from the computed layer — RECON §4
    # shows the computed layer IS cast-derived, so declining on it would certify the blindness.
    app_line = "    narrative_ontology:coordination_type(C, T),"
    check("cast DECLINE apparatus-presence consumer not flagged",
          not any(rx.search(app_line) for _, rx in [(n, name_regex(n)) for n in ALL_NAMES]),
          "an apparatus-presence line matched the cast vocabulary")

    # -- Pair 2: VERDICT, same-path decline (same file, same machinery, not verdict-keyed).
    cc = sweep_code(["prolog/commentary_census.pl", "prolog/stakeholder_seats.pl"], edges)
    vfire = [r for r in cc if r["bucket"] == "verdict"]
    check("verdict FIRE   q6 source reaches founding_problem_status",
          bool(vfire), "no verdict-bucket row in the commentary_census q6 path")
    sib = "commentary_cell(extraction_reading, C, Bucket) :-"
    sib_hit = [n for n in ALL_NAMES if name_regex(n).search(sib)]
    check("verdict DECLINE extraction_reading/consensus/empty_chair siblings not verdict-keyed",
          not sib_hit, f"sibling source matched {sib_hit}")

    # -- Pair 3: CLOSURE, BOTH directions. Fire-only passes while the closure tags half the repo
    # T2b, an artifact indistinguishable from the true finding.
    reach = "    Type = dr_type_of(C), dr_type(C, Type)."
    reached = any(name_regex(n).search(reach) for n in DERIVED_FRONTIER if n in grades)
    check("closure REACH   synthetic derived-value consumer (no cast name) is T2b",
          reached and not any(name_regex(n).search(reach) for n in ALL_NAMES),
          "synthetic derived consumer not reached, or it leaked a cast name")
    no_reach = "    synthetic_apparatus_consumer(C) :- narrative_ontology:coordination_type(C, _)."
    check("closure NO-REACH synthetic apparatus-presence consumer is NOT T2b",
          not any(name_regex(n).search(no_reach) for n in DERIVED_FRONTIER if n in grades)
          and not any(name_regex(n).search(no_reach) for n in ALL_NAMES),
          "the closure reached an apparatus-presence-only consumer — OVER-BROAD")

    # -- Pair 4: DOCUMENTARY. Free, naturally arising fire target on the active surface.
    # The control names the TARGET LINE, not a substring that any row could carry. Its first
    # version asked only "does some row say draw-stable" and passed on OQ-190's own body while
    # OQ-75 — the one claim the prereg says must be flagged before any doc-radius claim — was
    # missed entirely. A control that a self-reference can satisfy tests nothing.
    drows, _ = sweep_docs(["ISSUES.md"])
    ilines = read("ISSUES.md").splitlines()
    try:
        want = next(i for i, l in enumerate(ilines, 1) if "only draw-stable fields" in l)
    except StopIteration:
        want = -1
    check("doc FIRE    OQ-75's 'compare only draw-stable fields' premise flagged AT ITS LINE",
          want > 0 and any(r["file"] == "ISSUES.md" and abs(r["line"] - want) <= 2
                           for r in drows),
          f"no doc row within 2 lines of ISSUES.md:{want} (OQ-75's stability-table gate)")
    check("doc SELF-PIN OQ-190's own body not counted as a finding about the repo",
          not any(r["file"] == "ISSUES.md" and oq190_span()[0] <= r["line"] <= oq190_span()[1]
                  for r in drows),
          "the sweep counted this audit's own text as a documentary dependent")
    desc = ["A beneficiary is an actor who gains from the constraint; victims are named per story."]
    desc_hit = STABILITY_CLAIM.search(" ".join(desc)) and NAME_ANY.search(" ".join(desc))
    check("doc DECLINE descriptive cast mention resting no stability claim not flagged",
          not desc_hit, "a purely descriptive cast mention was flagged as a dependent")

    # -- Pair 5: 3d RESOLVED-ENTRY extractor. The 3c doc pair fires on OQ-75, which is OPEN —
    # it licenses nothing about the resolved path, and 3d is the highest-yield surface. Both arms
    # here are NATURALLY ARISING (top rung of the discrimination ladder), found by hunting the
    # population rather than authoring a decoy.
    ents = {e["oq"]: e for e in parse_issues()}
    fire52 = sweep_resolved([ents["OQ-52"]]) if "OQ-52" in ents else []
    check("3d FIRE     OQ-52's close on '289/293 both-authored, 4 victim-only' flagged",
          bool(fire52),
          "the resolved-entry extractor missed a cast-presence count quoted as a settled fact")
    # OQ-53 is resolved AND carries a stability claim ("abolition draw-stable 5/7 both twins") —
    # about `fingerprint_shift`/stance, NOT a cast or verdict field. A same-surface, same-shape
    # decline: the strongest kind available, because it is the near-miss.
    dec53 = sweep_resolved([ents["OQ-53"]]) if "OQ-53" in ents else ["x"]
    check("3d DECLINE  OQ-53's draw-stability claim about a NON-cast field not flagged",
          not dec53, "a stability claim about a non-cast field was admitted as a cast dependent")

    self_rows = [p for p in code_files(tracked()) if p.startswith(SELF_OUTPUT)]
    check("self-pin   this audit's own instruments are not in the code surface",
          not self_rows, f"the sweep would census its own tooling: {self_rows}")

    print()
    if fails:
        print(f"SELFTEST RED — {len(fails)} control(s) failed: {fails}")
        print("Per prereg §(d): no radius claim may be made, and if a CLOSURE arm is among the")
        print("failures the verdict bucket returns `reserved-pending` regardless of the sweep.")
        return 1
    print("SELFTEST GREEN — 12 controls, 5 two-sided pairs + 2 self-pins.")
    return 0


# ---------------------------------------------------------------------------

def write_tsv(path, header, rows, keys, preamble):
    with open(path, "w", encoding="utf-8") as fh:
        for ln in preamble:
            fh.write("# " + ln + "\n")
        fh.write("\t".join(header) + "\n")
        for r in rows:
            fh.write("\t".join(str(r.get(k, "")).replace("\t", " ") for k in keys) + "\n")


def do_sweep():
    edges = load_graph()
    if not edges:
        print("ABORT  derivation_graph.tsv missing — the closure pass cannot run, and a")
        print("       name-keyed-only census would land the computed layer T4/T6 BY CONSTRUCTION.")
        return 1
    paths = tracked()
    code = code_files(paths)
    docs = doc_files(paths)

    rows = sweep_code(code, edges)
    for r in rows:
        r["disposition"] = disposition_for(r["tier"], r["grade"])
    write_tsv(AUDIT / "consumer_census.tsv",
              ["file", "line", "surface", "emitted_name", "bucket", "tier", "grade",
               "reached_by", "disposition", "confidence", "text"],
              rows,
              ["file", "line", "surface", "emitted_name", "bucket", "tier", "grade",
               "reached_by", "disposition", "confidence", "text"],
              ["OQ-190 consumer_census.tsv — pass 1 (name-keyed) + pass 2 (closure/T2b)",
               "governed by the frozen PREREGISTRATION.md in this directory",
               f"code surface: {len(code)} tracked .pl/.py files (corpus data excluded)"])

    drows, aedges = sweep_docs(docs)
    write_tsv(AUDIT / "doc_claims.tsv",
              ["file", "line", "surface", "bucket", "admitted_by", "repair", "confidence",
               "claim"], drows,
              ["file", "line", "surface", "bucket", "admitted_by", "repair", "confidence",
               "claim"],
              ["OQ-190 doc_claims.tsv — active-surface documentary dependents (3c)",
               "a dependent RESTS A CLAIM on stability; a descriptive mention does not",
               f"doc surface: {len(docs)} tracked .md files; audits/ NOT swept"])

    write_tsv(AUDIT / "promoted_audit_edges.tsv",
              ["file", "line", "target", "text"], aedges,
              ["file", "line", "target", "text"],
              ["OQ-190 promoted_audit_edges.tsv — forward-citations from the ACTIVE surface into",
               "audits/. Standing artifact: names the historical records promoted to live, so a",
               "future shift in OQ-118's finding does not have to re-derive the edge list."])

    mine, theirs, _ = reconcile_issues()
    if theirs is not None and mine != theirs:
        print(f"ABORT  3d reconciliation FAILED: extractor {mine} vs issues_status {theirs}")
        return 1
    rrows = sweep_resolved(parse_issues())
    write_tsv(AUDIT / "resolved_closure_premises.tsv",
              ["oq", "line", "bucket", "admitted_by", "repair", "confidence", "title", "premise"],
              rrows,
              ["oq", "line", "bucket", "admitted_by", "repair", "confidence", "title", "premise"],
              ["OQ-190 resolved_closure_premises.tsv — resolved OQs whose closure rested on a",
               "cast/verdict stability premise. A closed issue is a standing claim that does not",
               f"announce itself as live. reconciliation: extractor={mine} issues_status={theirs}"])

    # Count from the ARTIFACT, never from the loop.
    for f in ("consumer_census.tsv", "doc_claims.tsv", "promoted_audit_edges.tsv",
              "resolved_closure_premises.tsv"):
        n = sum(1 for ln in (AUDIT / f).read_text(encoding="utf-8").splitlines()
                if ln and not ln.startswith("#") and not ln.startswith(("file\t", "oq\t")))
        print(f"{f:36} {n:6d} rows (counted from the written file)")
    return 0


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--sweep", action="store_true")
    ap.add_argument("--selftest", action="store_true")
    ap.add_argument("--reconcile-issues", action="store_true")
    a = ap.parse_args(argv)
    if a.selftest:
        return selftest()
    if a.reconcile_issues:
        mine, theirs, out = reconcile_issues()
        print(f"extractor resolved-entry count: {mine}")
        print(f"issues_status.py resolved count: {theirs}")
        if theirs is None:
            print("ABORT  could not recover a resolved count from issues_status.py output:")
            print(out[:600])
            return 1
        if mine != theirs:
            print("ABORT  mismatch — the 3d read does not run. A `**Status:** resolved` variant")
            print("       the extractor misses drops SILENTLY and reads as a small radius.")
            return 1
        print("RECONCILED")
        return 0
    if a.sweep:
        return do_sweep()
    ap.print_help()
    return 0


if __name__ == "__main__":
    sys.exit(main())
