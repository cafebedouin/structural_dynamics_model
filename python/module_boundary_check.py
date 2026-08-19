#!/usr/bin/env python3
"""module_boundary_check.py — the module-boundary bypass made a declared, checked fact.

A cross-module call written `other_module:pred(...)` reaches PAST `other_module`'s export
list. SWI permits it unconditionally, so the export list — which this repo's own notes call
"the only honest API statement there is" (swipl_load_path_and_probe_gotchas.md §1, module
boundaries here being already porous via non-module report files importing into `user`) —
stops describing what the module actually promises. An internal signature change then fails
SILENTLY at every bypass site: nothing errors at edit time, and the blast radius is not
enumerable without a sweep like this one. That is OQ-68.

THE RULING THIS ENCODES (operator, 2026-08-18). The deciding axis is WHO OWNS THE WRITE,
not exported-vs-qualified:

  * The module asserts and outsiders only read (maxent_dist/3, fpn_neighbors_cache/3):
    encapsulation is real, so the leak is real -> ACCESSOR. `ROLE=internal-state` is
    deliberately NOT a legal value below — a store whose module owns the write gets an
    accessor, not an allowlist row.
  * Outsiders assert into a namespace the module merely HOSTS (narrative_ontology's
    corpus-schema family): there is no encapsulation to breach -> QUALIFICATION IS THE
    IDIOM. Declare it, do not export it. Exporting was ruled against: writers would still
    have to qualify their heads or declare `multifile` locally, so it changes name
    resolution across 100+ modules and still leaves qualified writes — all cost, no
    consolidation.
  * FOURTH DISPOSITION, an EXTENSION of the ruling rather than an application of it
    (recorded 2026-08-18): an outsider writes, BUT the namespace owner reads and enforces an
    INVARIANT over what was written -> WRITE ACCESSOR, fail-loud. The hosting test is not
    "who asserts" alone; it is whether the module merely holds the facts or also means
    something by them. diagnostic_summary:maxent_attempted/1 is the instance —
    json_report does the retractall/assertz, but diagnostic_summary interprets the marker
    (maxent_stage_attempted_but_void/2), so an unrecognised stage would sit in the store
    unread and the void gate would report a clean run over it. Shipped as
    maxent_attempt_reset/0 + maxent_mark_attempted/1, the latter throwing on an unknown
    stage. Do not read the axis as having three branches: a pure host takes a row, a host
    with an invariant takes a write accessor.

WHY A GATE ROW RATHER THAN A DOCUMENTED RULE. The corpus-schema set is opt-in in exactly
the way `reading_registry` registration and the `spec_enum_check` sentinels are: a new
predicate is unguarded until someone remembers it. Two members had already fallen out of
the set undetected (see ARM B below), and the mechanism has fired here before —
narrative_ontology.pl carries its own tombstone for story_provenance/8 + story_seed/3,
which "MUST be multifile like every sibling above — declared dynamic-only, they loaded 1/N
('Redefined static procedure' on each testset consult, last-file-wins). Fixed 2026-06-13."

THREE ARMS
  A — bypass closure. Every cross-module qualified reference to a NON-EXPORTED predicate
      needs an allowlist row. Undeclared -> RED. A row with no surviving site is a
      `note:` and not red (dispatch_head_check.py precedent) — a stale row is clutter,
      an undeclared bypass is the defect.
  B — corpus-schema multifile completeness. Every ROLE=corpus-schema row needs a
      `:- multifile` in a PRODUCTION engine module. A declaration under tests/ does NOT
      count: the production load chain never consults those files, so a tests/-only
      declaration defends nothing at pipeline time.
  D — unwired-schema consumer tripwire. A corpus-schema predicate declared for
      load-correctness but with no engine consumer is listed in UNWIRED_SCHEMA; the arm goes
      RED when one ACQUIRES a reference. Declaring such a predicate turns it from UNDEFINED
      (call throws) into DEFINED-BUT-EMPTY (call fails silently) on any leg with no writers,
      which is the OQ-66 shape; "no consumer exists" is the mitigation AND the thing that
      stops being true silently. The arm fires exactly when that happens, so the first
      consumer is forced to decide what an empty read means. See the registry below.
  C — corpus-schema closure (the restored typo detector). Every `narrative_ontology:P(...)`
      clause head written by a testset must appear as a ROLE=corpus-schema row. THIS ARM
      IS WHY THE LIST MUST BE CLOSED: once a predicate is `multifile`, SWI stops warning on
      redefinition, so declaring the set costs us the "Redefined static procedure" warning
      that is currently doing accidental duty as a typo detector for qualified heads. A
      closed, gated list buys it back — an unlisted or MISSPELLED qualified head still
      trips this check.

DISCRIMINATION RECORD — ANCHORED TO CONTENT, NOT A COMMIT (build_discipline: a SHA-only
record dangles if the anchoring commit is amended or rebased).

  ARM B, from git rather than a fixture — a naturally-arising positive and a naturally-
  arising negative, neither authored to be found, which is the top of the ladder. Run
  2026-08-18 by swapping ONLY prolog/narrative_ontology.pl to each side and re-running
  --check against today's allowlist:

    narrative_ontology.pl @ dc12bf5a^  -> arm B fires on 5:
        epsilon_provenance/5, flat_control_of/2, has_sunset_clause/1,
        story_provenance/8, story_seed/3
    narrative_ontology.pl @ dc12bf5a   -> arm B fires on 3:
        epsilon_provenance/5, flat_control_of/2, has_sunset_clause/1

  The DIFFERENCE is exactly {story_provenance/8, story_seed/3} — precisely the pair that
  commit repaired, and the commit message says so. The three CONSTANT fires are what make
  this a discrimination rather than a detection: the check is not returning everything or
  nothing on either side, it is tracking the one thing that changed. (Those three were
  declared later — epsilon_provenance/5 after June 2026, the other two in the change that
  introduced this checker — so they are correctly red against a June substrate.)

  To re-verify without trusting a SHA: `git show dc12bf5a^:prolog/narrative_ontology.pl`
  into place, run --check, restore. The anchor is the CONTENT above, not the SHA.

  ARM B, live at introduction (2026-08-18): the sweep that motivated this row found the
  set was NOT closed. flat_control_of/2 had NO engine-side declaration anywhere, and
  has_sunset_clause/1 was declared `:- dynamic` but never `:- multifile`, so both were
  correct on disk only because every writing testset happens to emit a local
  `:- multifile narrative_ontology:P.` — a generation-time habit, not an engine guarantee.
  The forcing witness, on a scratch copy of the leg with the local declaration stripped
  from all 28 loaded writers (the ONLY variable being the central declaration):
      no central decl : flat_control_of loaded 1/28, multifile=no, 27 warnings
      central decl    : flat_control_of loaded 28/28, multifile=yes, 0 warnings
  i.e. 27 of 28 authored rows silently lost. Both predicates were added to
  narrative_ontology.pl's multifile block in the same change that introduced this checker.

  ARM A: fires on a planted bypass; declines on the ~2,900 existing cross-module qualified
  calls to EXPORTED predicates — a naturally-arising negative drawn from the population, not
  an authored decoy. Verified live 2026-08-18 by plant-and-restore on the real tree (an added
  engine file reaching maxent_classifier:maxent_dist/3 -> RED naming it; removed -> GREEN).

  ARM C: verified the same way and in the same run — a planted testset head
  narrative_ontology:constraint_victimm/2 -> RED naming it; removed -> GREEN, with the
  testsets md5 byte-identical before and after.

  ARM D: verified 2026-08-18 — a planted engine file calling
  narrative_ontology:flat_control_of/2 -> "RED - 1 boundary problem(s)" naming the file and
  line; removed -> GREEN. Note this arm's two-sidedness is structural rather than incidental:
  it is GREEN today precisely because the predicate has no consumer, so its green state and
  its red state are the two states of the fact it is watching.

  ARM C tokenizer control: the first version of this sweep was line-based and reported 17
  undeclared corpus-schema predicates; 15 were artifacts of multi-line facts and commas
  inside quoted strings. Both parsers stay in the selftest and the assertion is on the
  returned SET, not its cardinality — a cardinality-only assertion lets a future miscount
  cancel against a compensating error. (build_discipline: "pin /usr/bin/grep, not grep, in
  any script computing a reported count" — this checker computes its counts in-process for
  the same reason.)

CENSUS RECONCILIATION — WHY 116 ROWS AND NOT ~279 (added 2026-08-18 at operator request).
Arm A claims CLOSURE ("every non-exported cross-module reference has a row"), so a reader who
finds two different census numbers and no bridge is entitled to disbelieve the claim. The
bridge, measured by toggling each defect in this instrument back on:

    239   naive: predicate indicators counted as calls, closures unresolved, reexport ignored
    132   - 107  predicate INDICATORS (`mod:pred/2` in a directive) excluded
    115   -  17  meta-predicate CLOSURES resolved to their real arity
     98   -  17  FACADE reexports resolved transitively
    +18   write-only corpus-schema heads (arm C requires a row; no read site to find)
    ----
    116   allowlist rows

THE PLAN'S 279 IS SUPERSEDED, NOT BRIDGED. The recon this work was executed from reported 279
(mod,pred) pairs from a different sweep. Scope does not explain the gap — including probsets/
moves this instrument from 239 to 242, not to 279 — so the difference is the PARSER, and that
recon documented its own arity defects ("mis-arity'd multi-line facts and commas inside quoted
strings"). A mis-arity'd parse inflates the count of distinct (module, predicate, arity)
triples, each wrong arity becoming its own row. That census is therefore not reproducible here
and should not be cited; this instrument's numbers supersede it, and the table above is the
only bridge that exists.

WHAT THE CENTRAL DECLARATION ACTUALLY BUYS (operator, 2026-08-18). The 1/28-vs-28/28 witness
demonstrates the MECHANISM, not the EXPOSURE — the corpus loads 28/28 either way today,
because every writer happens to self-declare. The real property gained is stronger and is not
a count: with a central `:- multifile`, LOAD ORDER STOPS MATTERING AT ALL. "28/28 loads today"
is a fact about the current corpus and the current generator; "no load order can lose a row" is
a fact about the engine. That is the justification for the change. (Note also that the control
must strip ALL loaded writers: `multifile` is a property of the PREDICATE, set by whichever
file declares it first, so stripping one writer of 28 changes nothing and licenses nothing.)

Usage:
    python3 python/module_boundary_check.py --check      # selftest, then live sweep
    python3 python/module_boundary_check.py --selftest   # fixtures only
    python3 python/module_boundary_check.py --list       # every bypass, classified
    python3 python/module_boundary_check.py --full       # arm C over all five legs
"""

from __future__ import annotations

import re
import sys
from collections import defaultdict
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
PROLOG = REPO / "prolog"
ALLOWLIST = PROLOG / "module_boundary_allowlist.txt"

# ---------------------------------------------------------------------------
# ROLES. Each is a CLASSIFICATION of why a bypass is sanctioned, never a bare
# silencer. `internal-state` is deliberately absent — see the ruling above.
# ---------------------------------------------------------------------------
LEGAL_ROLES = {
    "corpus-schema":     "Written by testset story files as qualified heads; narrative_ontology "
                         "HOSTS the namespace and does not own the write. Requires a "
                         "production-side :- multifile (arm B) and closure (arm C).",
    "multifile-registry": "Written by another ENGINE module into a multifile namespace "
                          "(axiom_concept_registry -> axiom_diff, sweeps -> "
                          "constraint_indexing:directionality_override/3). Same "
                          "outsider-owns-the-write shape as corpus-schema, non-corpus writer.",
    "test-whitebox":     "Sole consumers are tests/ asserting on private memo state. "
                         "Legitimate white-box testing — an accessor would widen the "
                         "production API to serve a test.",
    "derived-view":      "A RULE (not a store) carrying a vestigial `:- dynamic` declaration, "
                         "read cross-module. There is no stored state to encapsulate, so no "
                         "accessor is owed — but the shape is recorded because a derived view "
                         "that LOOKS like a store is how a caller comes to believe an argument "
                         "means something the rule ignores.",
    "helper-static":     "A non-exported STATIC helper predicate read cross-module. Recorded, "
                         "NOT repaired in the OQ-68 pass (out of scope by ruling).",
    "unruled":           "Recorded by the introducing sweep and not yet adjudicated. NOT a "
                         "verdict — a placeholder that keeps the row visible. Reclassify on "
                         "sight; never add a NEW row with this role.",
}

SCHEMA_ROLE = "corpus-schema"

# ---------------------------------------------------------------------------
# ARM D — corpus-schema predicates DECLARED FOR LOAD-CORRECTNESS BUT UNWIRED.
#
# Adding a predicate to narrative_ontology's multifile/dynamic block is not free: on a leg
# with no writers it changes the predicate from UNDEFINED (a call throws existence_error) to
# DEFINED-BUT-EMPTY (a call fails silently). That is the OQ-66 shape — a consumer measuring
# NOTHING becomes indistinguishable from a consumer measuring ZERO — and the mitigation
# usually offered, "no consumer exists," is exactly the condition that stops being true
# without anyone noticing.
#
# So this arm fires at the moment the hazard goes live: a listed predicate ACQUIRING a
# reference anywhere in engine code is RED. That is not a complaint about wiring it — it is a
# demand that whoever wires it decides, in that change, what an empty read means on a leg
# with no authored data, and then removes the entry here.
#
# Witnessed 2026-08-18 for flat_control_of/2 across all five legs: counts identical before
# and after (28/0/0/0/0), and on the four twin legs the predicate went undefined ->
# defined-but-empty. Related open question: OQ-308 (arity/shape safety for the schema set).
# ---------------------------------------------------------------------------
UNWIRED_SCHEMA: dict[tuple[str, int], str] = {
    ("flat_control_of", 2):
        "28 facts in testsets/ only; ZERO writers on the four twin legs and ZERO engine "
        "consumers (checked 2026-08-18). Declared in narrative_ontology.pl for "
        "load-correctness. A first consumer MUST treat an empty result as 'no data authored "
        "on this leg', never as 'no flat-control relation exists' — on four of five legs "
        "those are now the same token. Decide that, then delete this entry.",
}

# Files where naming an UNWIRED_SCHEMA predicate is not a consumer: the declaration itself,
# the allowlist that records it, and this checker.
UNWIRED_EXEMPT_FILES = {
    "prolog/narrative_ontology.pl": "the declaration site itself",
    "prolog/module_boundary_allowlist.txt": "the record, not a consumer",
    "python/module_boundary_check.py": "this checker's own registry",
}

# Directories that hold CORPUS DATA, not engine code: story files author facts, they do not
# call the engine. Declared with reasons — a bare list decays into "places someone stopped
# looking" — and PRINTED in the green line so the narrowing is never silent.
CORPUS_DIRS = {
    "testsets":         "LIVE leg — the deliberately singleton topical working set.",
    "testsets_haiku":   "Reconciled twin leg.",
    "testsets_flash":   "Reconciled twin leg.",
    "testsets_kimi":    "Model-named leg (kimi-k2.6).",
    "testsets_sonnet":  "Model-named leg (claude-sonnet-5).",
    "probsets":         "Probe story packs — data, same shape as a testset leg.",
    "archives":         "Archived corpora and point-in-time probes; per audits/README.md "
                        "point-in-time documents are not retro-edited.",
}
DEFAULT_ARM_C_LEGS = ["testsets"]
ALL_ARM_C_LEGS = ["testsets", "testsets_haiku", "testsets_flash", "testsets_kimi", "testsets_sonnet"]


# ---------------------------------------------------------------------------
# CLAUSE TOKENIZER. Quote-aware and comment-stripping, preserving line numbers.
# The naive line-based version this replaced produced 17 false "undeclared" rows.
# ---------------------------------------------------------------------------
def strip_comments(text: str) -> str:
    """Remove % line comments and /* */ blocks, respecting quoted atoms/strings.

    Newlines are preserved so line numbers survive — a stripper that collapses lines
    reports the wrong site and sends the reader to innocent code.
    """
    out, i, n, in_q = [], 0, len(text), None
    while i < n:
        c = text[i]
        if in_q:
            out.append(c)
            if c == "\\" and i + 1 < n:
                out.append(text[i + 1]); i += 2; continue
            if c == in_q:
                in_q = None
            i += 1; continue
        if c in ("'", '"', "`"):
            in_q = c; out.append(c); i += 1; continue
        if c == "%":
            while i < n and text[i] != "\n":
                i += 1
            continue
        if c == "/" and i + 1 < n and text[i + 1] == "*":
            j = text.find("*/", i + 2)
            if j == -1:
                j = n
            out.append("\n" * text.count("\n", i, j))
            i = j + 2; continue
        out.append(c); i += 1
    return "".join(out)


def naive_strip(text: str) -> str:
    """The REJECTED line-based stripper, kept as a selftest control (see arm C above)."""
    return "\n".join(line.split("%")[0] for line in text.splitlines())


def module_of(text: str) -> str | None:
    m = re.search(r":-\s*module\s*\(\s*([a-z][A-Za-z0-9_]*)\s*,", text)
    return m.group(1) if m else None


def export_list(text: str) -> set[tuple[str, int]]:
    m = re.search(r":-\s*module\s*\(\s*[a-z][A-Za-z0-9_]*\s*,\s*\[", text)
    if not m:
        return set()
    i, depth, n = m.end(), 1, len(text)
    while i < n and depth > 0:
        if text[i] == "[":
            depth += 1
        elif text[i] == "]":
            depth -= 1
        i += 1
    body = text[m.end():i - 1]
    return {(nm, int(ar)) for nm, ar in re.findall(r"\b([a-z][A-Za-z0-9_]*)\s*/\s*(\d+)", body)}


REEXPORT = re.compile(r":-\s*reexport\s*\(\s*([a-z][A-Za-z0-9_]*)\s*(,\s*\[)?")


def reexports(text: str) -> list[tuple[str, set[tuple[str, int]] | None]]:
    """[(module, explicit_list_or_None)] for each `:- reexport(...)` directive.

    A FACADE module re-exports another module's API under its own name, and a qualified
    call through the facade is correct BY DESIGN — drl_lifecycle.pl says so in as many
    words: "All original exports remain accessible via drl_lifecycle:predicate/N."

    Missing this is not a cosmetic parse bug: drl_lifecycle declares `:- module(drl_lifecycle,
    [])`, an EMPTY export list, so every one of its four facades' predicates looked like an
    undeclared bypass. The first live run of this checker reported
    `drl_lifecycle:generate_drift_report` as a wrong-qualifier defect on exactly that
    reasoning. It is not one — check_stack agrees, listing three undefined references and
    not this. Effective exports are therefore computed TRANSITIVELY below.
    """
    out = []
    for m in REEXPORT.finditer(text):
        mod = m.group(1)
        if not m.group(2):
            out.append((mod, None))                    # whole API
            continue
        i, depth, n = m.end() - 1, 1, len(text)
        while i < n and depth > 0:
            if text[i] == "[":
                depth += 1
            elif text[i] == "]":
                depth -= 1
            i += 1
        body = text[m.end():i - 1]
        out.append((mod, {(nm, int(ar)) for nm, ar
                          in re.findall(r"\b([a-z][A-Za-z0-9_]*)\s*/\s*(\d+)", body)}))
    return out


def effective_exports(mods: dict, name: str, _seen: frozenset = frozenset()) -> set[tuple[str, int]]:
    """A module's own exports PLUS everything it reexports, transitively.

    Cycle-guarded via _seen: a reexport loop would otherwise recurse forever, and this
    check must not be able to hang the gate.
    """
    if name in _seen or name not in mods:
        return set()
    seen = _seen | {name}
    out = set(mods[name]["exports"])
    for target, explicit in mods[name]["reexports"]:
        inner = effective_exports(mods, target, seen)
        out |= inner if explicit is None else (explicit & inner) | explicit
    return out


def directive_preds(text: str, kind: str) -> set[tuple[str, int]]:
    """Predicate indicators named by a `:- <kind> ...` directive (possibly multi-line)."""
    out = set()
    for m in re.finditer(rf":-\s*{kind}\s+(.*?)\.\s*(?:\n|$)", text, re.S):
        for nm, ar in re.findall(r"\b([a-z][A-Za-z0-9_]*)\s*/\s*(\d+)", m.group(1)):
            out.add((nm, int(ar)))
    return out


CLAUSE_HEAD = re.compile(r"^([a-z][A-Za-z0-9_]*)\s*(\()?", re.M)


def defined_preds(text: str) -> set[tuple[str, int]]:
    """(name, arity) for every predicate this module DEFINES or declares.

    Clause heads at column 0, plus dynamic/multifile/discontiguous/table directives. Used
    only to resolve the arity of a PAREN-LESS qualified reference — see closure_arity().
    """
    out = set()
    for m in CLAUSE_HEAD.finditer(text):
        if m.group(2):
            ar = arity_at(text, m.end() - 1)
            if ar is not None:
                out.add((m.group(1), ar))
        else:
            tail = text[m.end():m.end() + 4].lstrip()
            if tail.startswith(":-") or tail.startswith("."):
                out.add((m.group(1), 0))
    for kind in ("dynamic", "multifile", "discontiguous", "table"):
        out |= directive_preds(text, kind)
    return out


def closure_arity(mods: dict, mod: str, pred: str) -> int | None:
    """Real arity of a paren-less `mod:pred` reference, or None if unresolvable.

    A paren-less reference is EITHER a genuine 0-arity call (corpus_loader:corpus_loaded,
    cache_registry:clear_hook) OR a CLOSURE handed to a meta-predicate and called with
    extra arguments later:

        maplist(maxent_classifier:pair_snd, Dist, Probs)                     -> pair_snd/2
        run_trigger_over_constraints(abductive_triggers:trigger_x, Cs, Ctx)  -> trigger_x/N

    Recording a closure at arity 0 names a predicate that exists at NO arity — a fabricated
    census row, not a conservative one. So: prefer 0 when the module really defines pred/0;
    else, if the module defines the name at exactly ONE other arity, use it; if ambiguous,
    return None so the caller reports it UNRESOLVED rather than guessing.
    """
    # Pool = what the module DEFINES plus what it effectively EXPORTS. The export half
    # matters for facades: drl_lifecycle defines nothing of its own, so a paren-less
    # `drl_lifecycle:generate_drift_report` is resolvable only through the reexported API.
    pool = set(mods[mod]["defined"]) | set(mods[mod].get("effective_exports", set()))
    if (pred, 0) in pool:
        return 0
    cands = sorted({a for (n, a) in pool if n == pred})
    if len(cands) == 1:
        return cands[0]
    return None


def _scan_args(text: str, open_paren: int, values: bool):
    """THE argument-grammar scan. One loop, two modes; None if unterminated.

    `values=False` counts arguments, `values=True` materialises their texts. The two modes
    share every branch that decides where an argument BEGINS and ENDS, so the arity a row
    is keyed by and the values a conformance arm reads cannot disagree — two parsers for
    one grammar is how a checker comes to enforce a shape the corpus does not have. The
    flag exists only because materialising a substring per argument across the ~4,200-file
    corpus costs ~11s of wall time to build strings the head census never looks at.

    Quote- and nesting-aware: this is the half the naive parser got wrong — a comma inside
    a quoted string, or a fact spanning several lines, both inflate a line-based count.

    `saw` distinguishes `foo()` (arity 0) from `foo(X)` (arity 1) — an empty argument list
    from a one-argument one. It must therefore be set by BRACKETS as well as by bare tokens:
    a term whose arguments are all bracket structures (`adjacent_pairs([], [])`) is a real
    2-argument term, and reading it as 0 records a predicate at an arity it does not have —
    the fabricated-census-row shape this file's own qualified_refs() docstring warns about.
    23 engine sites scanned that way before this was fixed.
    """
    i, n, depth, in_q = open_paren + 1, len(text), 1, None
    start, out, count, saw = i, [], 1, False
    while i < n:
        c = text[i]
        if in_q:
            if c == "\\":
                i += 2; continue
            if c == in_q:
                in_q = None
            i += 1; continue
        if c in ("'", '"'):
            in_q = c; saw = True; i += 1; continue
        if c in "([{":
            depth += 1; saw = True
        elif c in ")]}":
            depth -= 1
            if depth != 0:
                saw = True
            else:
                if not saw:
                    return [] if values else 0
                if values:
                    out.append(text[start:i])
                    return out
                return count
        elif c == "," and depth == 1:
            if values:
                out.append(text[start:i]); start = i + 1
            else:
                count += 1
        elif not c.isspace():
            saw = True
        i += 1
    return None


def split_args(text: str, open_paren: int) -> list[str] | None:
    """Argument TEXTS of the term whose '(' sits at open_paren; None if unterminated."""
    return _scan_args(text, open_paren, True)


def arity_at(text: str, open_paren: int) -> int | None:
    """Arity of the term whose '(' sits at open_paren. Quote- and nesting-aware."""
    return _scan_args(text, open_paren, False)


QUALIFIED = re.compile(
    r"(?<![A-Za-z0-9_])([a-z][A-Za-z0-9_]*)\s*:\s*([a-z][A-Za-z0-9_]*)\s*(\(|/\s*\d+)?")


def qualified_refs(text: str) -> list[tuple[str, str, int, int]]:
    r"""[(module, pred, arity, lineno)] for every Module:pred CALL reference.

    Three shapes, and telling them apart is the first defect this checker had:

      `mod:pred(A, B)`  a CALL              -> arity from the argument list.
      `mod:pred`        a 0-ARITY CALL      -> arity 0. This is why the pattern is not a
                                               `\w+:\w+\(` regex — corpus_loader:corpus_loaded/0
                                               is invisible to that version.
      `mod:pred/2`      a PREDICATE INDICATOR inside a `:- multifile` / `:- dynamic`
                        directive — NOT a call, and excluded here. The first draft read
                        these as 0-arity calls and manufactured 20+ phantom rows
                        (narrative_ontology:constraint_beneficiary/0, cache_registry:
                        clear_hook/0, logical_fingerprint:purity_zone/0, ...), every one
                        naming a predicate that exists at NO arity. A census whose members
                        cannot exist is a positional parse — the exact shape
                        build_discipline warns about — so it is fixture-controlled below.
    """
    out = []
    for m in QUALIFIED.finditer(text):
        tail = m.group(3)
        if tail and tail.startswith("/"):
            continue                                  # predicate indicator, not a call
        ar = arity_at(text, m.end() - 1) if tail else 0
        if ar is None:
            continue
        out.append((m.group(1), m.group(2), ar, text.count("\n", 0, m.start()) + 1))
    return out


SCHEMA_HEAD = re.compile(r"(?:^|(?<=[.\s]))narrative_ontology\s*:\s*([a-z][A-Za-z0-9_]*)\s*(\()?")


def opens_a_clause(text: str, start: int) -> bool:
    """True iff the term at `start` opens a CLAUSE, rather than sitting in a clause BODY.

    SCHEMA_HEAD accepts any preceding whitespace, so before this guard a GOAL inside a
    plunit test body counted as a clause head:

        test(mountain_threshold_validation) :-
            config:param(extractiveness_metric_name, ExtMetricName),
            narrative_ontology:constraint_metric(collapse_inevitability_reading, ExtMetricName, E),

    810 such occurrences across 270 files, all constraint_metric/3. The (name, arity) key
    survives either way — that predicate has 20,895 real heads — so arm C never noticed and
    its 40 do not move. A conformance arm does notice: it would have harvested the Prolog
    VARIABLE `ExtMetricName` as an authored value of argument 2 and gone red on the corpus
    for a value no story ever wrote. A parse reused by a second consumer has to mean what
    its name says.

    A clause opens at the start of the text or just after a clause-terminating `.`; a body
    goal is preceded by `,` `;` `->` or `:-`.
    """
    i = start - 1
    while i >= 0 and text[i].isspace():
        i -= 1
    return i < 0 or text[i] == "."


def schema_head_terms(text: str, values: bool = False) -> list[tuple[str, int, list[str], int]]:
    """[(pred, arity, [argument texts], lineno)] for `narrative_ontology:P(...)` HEADS.

    Excludes `:- multifile narrative_ontology:P/N.` style declarations, which name a
    predicate indicator (P/N) rather than opening a term — those are the writer's local
    self-declaration, not a head.

    Carries the argument TEXTS as well as the arity because arm F conforms authored
    values and arm C counts heads, and both must be reading the same parse of the same
    byte range. schema_heads() is the arity-only projection of this, not a second parser.
    """
    out = []
    for m in SCHEMA_HEAD.finditer(text):
        if not m.group(2):
            continue  # `P/N` in a directive, or a bare atom — not a head
        if not opens_a_clause(text, m.start()):
            continue  # a GOAL in a clause body, not a head — see opens_a_clause()
        args = _scan_args(text, m.end() - 1, values)
        if args is None:
            continue
        out.append((m.group(1), len(args) if values else args,
                    args if values else [], text.count("\n", 0, m.start()) + 1))
    return out


def schema_heads(text: str) -> set[tuple[str, int]]:
    """The (name, arity) projection of schema_head_terms()."""
    return {(p, a) for p, a, _args, _ln in schema_head_terms(text)}


# ---------------------------------------------------------------------------
# Allowlist
# ---------------------------------------------------------------------------
ROW = re.compile(r"^([a-z][A-Za-z0-9_]*):([a-z][A-Za-z0-9_]*)/(\d+)\s+ROLE=(\S+)\s+(.*\S)\s*$")


def parse_allowlist(path: Path = ALLOWLIST) -> tuple[dict, list[str]]:
    """-> ({(mod,pred,arity): (role, reason)}, [problems])

    Fails CLOSED: a malformed row is a problem, never a skipped line. A row whose reason
    is empty is malformed too — the reason column is the whole point of the file.
    """
    entries, problems = {}, []
    if not path.exists():
        return entries, [f"allowlist missing: {path.relative_to(REPO)}"]
    for lineno, raw in enumerate(path.read_text(encoding="utf-8").splitlines(), start=1):
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        m = ROW.match(line)
        if not m:
            problems.append(f"{path.name}:{lineno}: malformed row (want "
                            f"`mod:pred/arity  ROLE=<role>  <reason>`) | {line}")
            continue
        mod, pred, ar, role, reason = m.group(1), m.group(2), int(m.group(3)), m.group(4), m.group(5)
        if role not in LEGAL_ROLES:
            problems.append(f"{path.name}:{lineno}: illegal ROLE={role} "
                            f"(legal: {', '.join(sorted(LEGAL_ROLES))}). "
                            f"NOTE: `internal-state` is not legal by ruling — a store whose "
                            f"module owns the write gets an ACCESSOR, not a row.")
            continue
        key = (mod, pred, ar)
        if key in entries:
            problems.append(f"{path.name}:{lineno}: duplicate row for {mod}:{pred}/{ar}")
            continue
        entries[key] = (role, reason)
    return entries, problems


# ---------------------------------------------------------------------------
# Engine scan
# ---------------------------------------------------------------------------
def engine_files() -> list[Path]:
    return sorted(p for p in PROLOG.rglob("*.pl")
                  if not any(part in CORPUS_DIRS for part in p.parts))


def engine_bodies(files: list[Path]) -> dict:
    """{path: comment-stripped body}, read ONCE per invocation.

    Three consumers used to read and strip the same ~180 files independently: the module
    table, the bypass sweep, and arm D — and arm D did it INSIDE its per-predicate loop,
    so its cost was files x watched-predicates. With one entry in UNWIRED_SCHEMA that is
    invisible; it becomes visible exactly when the registry grows, which is the moment
    someone is least inclined to look at the checker.
    """
    return {p: strip_comments(p.read_text(encoding="utf-8", errors="replace"))
            for p in files}


def build_module_table(files: list[Path], bodies: dict) -> dict:
    mods = {}
    for p in files:
        t = bodies[p]
        name = module_of(t)
        if name:
            mods[name] = {
                "path": p,
                "exports": export_list(t),
                "multifile": directive_preds(t, "multifile"),
                "dynamic": directive_preds(t, "dynamic"),
                "defined": defined_preds(t),
                "reexports": reexports(t),
            }
    for name in mods:
        mods[name]["effective_exports"] = effective_exports(mods, name)
    return mods


def find_bypasses(files: list[Path], mods: dict, bodies: dict) -> dict:
    """-> {(mod,pred,arity): [(relpath, lineno)]} for non-exported cross-module refs."""
    found = defaultdict(list)
    for p in files:
        t = bodies[p]
        self_mod = module_of(t)
        for mod, pred, ar, lineno in qualified_refs(t):
            if mod not in mods or mod == self_mod:
                continue
            if ar == 0:
                # Paren-less: a genuine 0-arity call OR a closure. Resolve before judging,
                # or a maplist closure gets filed at an arity the predicate never has.
                resolved = closure_arity(mods, mod, pred)
                if resolved is None:
                    found[(mod, pred, -1)].append((str(p.relative_to(REPO)), lineno))
                    continue
                ar = resolved
            if (pred, ar) in mods[mod]["effective_exports"]:
                continue  # the declining population: ~2,900 sites over ~310 predicates
            found[(mod, pred, ar)].append((str(p.relative_to(REPO)), lineno))
    return found


def scan_story_legs(legs: list[str], want_args: frozenset = frozenset()) -> dict:
    """ONE read-and-strip per story file, feeding every corpus-facing arm.

    Before this, arm C read and comment-stripped the whole corpus on its own. A second
    corpus-facing arm would have read it a second time and a third — but cost is the
    lesser reason. Two arms parsing the same file SEPARATELY can disagree about what it
    says, and a per-arm parse is precisely where that divergence hides: arm C would count
    a head this arm never saw the arguments of, and each would be internally consistent.

    Returns:
      heads    {(pred, arity): n_files}                  — arm C, unchanged semantics:
                                                           counted once per FILE, not once
                                                           per fact (a set per file).
      per_leg  {leg: {(pred, arity): n_files}}           — arm G's declared-vs-actual
      args     {(pred, arity): {argpos: {value texts}}}  — arm F's conformance, and
                 collected ONLY for the (pred, arity, argpos) triples in `want_args`.
                 Demand-driven on purpose: arm F conforms CLOSED positions only, and
                 harvesting every value at every position instead cost ~11s of corpus
                 wall time to build sets nothing reads. A producer sized to its consumer
                 also cannot quietly become a dangling one.
      sites    {(pred, arity): (relpath, lineno)}        — first site, for naming a file
      files    int
    """
    heads: dict = defaultdict(int)
    per_leg: dict = {leg: defaultdict(int) for leg in legs}
    args: dict = defaultdict(lambda: defaultdict(set))
    sites: dict = {}
    nfiles = 0
    for leg in legs:
        d = PROLOG / leg
        if not d.is_dir():
            continue
        for p in sorted(d.glob("*.pl")):  # non-recursive: run-tagged subdirs are not loaded
            t = strip_comments(p.read_text(encoding="utf-8", errors="replace"))
            nfiles += 1
            seen = set()
            rel = None
            for pred, ar, argv, lineno in schema_head_terms(t, values=bool(want_args)):
                key = (pred, ar)
                seen.add(key)
                if key not in sites:
                    # relative_to() is not free and this loop runs once per FACT, not once
                    # per file: computing it eagerly inside setdefault() cost ~11s of the
                    # corpus sweep to build a path that was thrown away every time.
                    rel = rel if rel is not None else str(p.relative_to(REPO))
                    sites[key] = (rel, lineno)
                for i, a in enumerate(argv, start=1):
                    if (pred, ar, i) in want_args:
                        args[key][i].add(a.strip())
            for key in seen:
                heads[key] += 1
                per_leg[leg][key] += 1
    return {"heads": dict(heads),
            "per_leg": {k: dict(v) for k, v in per_leg.items()},
            "args": {k: dict(v) for k, v in args.items()},
            "sites": sites,
            "files": nfiles}


def live_sweep(legs: list[str]) -> tuple[list[str], list[str], dict]:
    problems, notes = [], []
    entries, ap = parse_allowlist()
    problems.extend(ap)

    files = engine_files()
    if not files:
        raise SystemExit("module_boundary_check: RED — scanned 0 engine files")
    bodies = engine_bodies(files)
    mods = build_module_table(files, bodies)
    if not mods:
        raise SystemExit("module_boundary_check: RED — resolved 0 modules")
    bypasses = find_bypasses(files, mods, bodies)
    scan = scan_story_legs(legs)

    # --- ARM A: bypass closure -------------------------------------------------
    for key in sorted(bypasses):
        if key in entries:
            continue
        mod, pred, ar = key
        sites = bypasses[key]
        where = ", ".join(f"{f}:{l}" for f, l in sites[:3]) + (" ..." if len(sites) > 3 else "")
        owns_write = (pred, ar) not in mods[mod]["multifile"]
        hint = ("the module appears to OWN this predicate (no :- multifile) — the ruling "
                "asks for an ACCESSOR, not a row" if owns_write else
                "outsider-written multifile namespace — a row with ROLE=multifile-registry "
                "or corpus-schema is the idiom")
        problems.append(f"ARM A undeclared bypass {mod}:{pred}/{ar} "
                        f"({len(sites)} site(s)): {where}\n"
                        f"           -> {hint}")

    # --- ARM B: corpus-schema multifile completeness ----------------------------
    for (mod, pred, ar), (role, _reason) in sorted(entries.items()):
        if role != SCHEMA_ROLE:
            continue
        prod = [name for name, info in mods.items()
                if (pred, ar) in info["multifile"] and "tests" not in info["path"].parts]
        if not prod:
            test_only = [name for name, info in mods.items()
                         if (pred, ar) in info["multifile"]]
            extra = (f" (declared ONLY under tests/: {', '.join(sorted(test_only))} — the "
                     f"production load chain never consults those files)"
                     if test_only else " (declared NOWHERE)")
            problems.append(
                f"ARM B corpus-schema {mod}:{pred}/{ar} has no PRODUCTION-side "
                f":- multifile{extra}. Writers self-declaring is a generation-time habit, "
                f"not an engine guarantee: drop it and the predicate loads 1/N, "
                f"last-file-wins (the story_provenance/8 tombstone).")

    # --- ARM C: corpus-schema closure ------------------------------------------
    schema_rows = {(p, a) for (m, p, a), (r, _) in entries.items()
                   if r == SCHEMA_ROLE and m == "narrative_ontology"}
    heads = scan["heads"]
    for (pred, ar), nfiles in sorted(heads.items()):
        if (pred, ar) not in schema_rows:
            problems.append(
                f"ARM C unlisted corpus-schema head narrative_ontology:{pred}/{ar} "
                f"written by {nfiles} story file(s) but absent from the ROLE=corpus-schema "
                f"list. Either it is a NEW schema predicate (add the row AND a "
                f"production-side :- multifile in this change) or it is a TYPO — this arm "
                f"is the typo detector that `:- multifile` silences.")

    # --- ARM D: an unwired schema predicate acquiring a consumer ----------------
    for (pred, ar), reason in sorted(UNWIRED_SCHEMA.items()):
        sightings = []
        for f in files:
            rel = str(f.relative_to(REPO))
            if rel in UNWIRED_EXEMPT_FILES:
                continue
            for i, line in enumerate(bodies[f].splitlines(), start=1):
                if re.search(rf"(?<![A-Za-z0-9_]){re.escape(pred)}\s*[(/]", line):
                    sightings.append(f"{rel}:{i}")
        if sightings:
            where = ", ".join(sightings[:4]) + (" ..." if len(sightings) > 4 else "")
            problems.append(
                f"ARM D unwired schema predicate narrative_ontology:{pred}/{ar} has ACQUIRED "
                f"a consumer: {where}\n"
                f"           -> This is the moment the loud->quiet change becomes live. On a "
                f"leg with no writers this predicate is DEFINED-BUT-EMPTY, so a failed read "
                f"means 'no data authored here', NOT 'the relation does not hold'. Decide "
                f"which your consumer needs, handle absence explicitly, then remove the "
                f"UNWIRED_SCHEMA entry in the same change.\n"
                f"           -> registry note: {reason}")

    # --- stale rows (a `note:`, never red) -------------------------------------
    # ROLE-AWARE, because a corpus-schema row is justified by arm C — a testset WRITES the
    # head — not by a cross-module read. The first version noted 18 write-only schema rows
    # as "stale, prune it"; following that advice would have deleted exactly the rows arm C
    # requires and turned the gate red. A note that instructs the reader to break the check
    # is worse than no note.
    full_legs = set(legs) == set(ALL_ARM_C_LEGS)
    for key, (role, _r) in sorted(entries.items()):
        mod, pred, ar = key
        if key in bypasses:
            continue
        if role == SCHEMA_ROLE:
            if (pred, ar) in heads:
                continue                       # write-only but LIVE: arm C keeps it
            if not full_legs:
                continue                       # cannot tell from a partial leg scan
            notes.append(f"note: corpus-schema row {mod}:{pred}/{ar} is neither read "
                         f"cross-module nor written by any story file in any leg — stale.")
            continue
        notes.append(f"note: allowlist row {mod}:{pred}/{ar} has no surviving call site "
                     f"— stale, prune it (not red: a stale row is clutter, an "
                     f"undeclared bypass is the defect)")

    stats = {"files": len(files), "modules": len(mods), "bypasses": len(bypasses),
             "sites": sum(len(v) for v in bypasses.values()), "rows": len(entries),
             "heads": len(heads), "legs": legs}
    return problems, notes, stats


# ---------------------------------------------------------------------------
# Selftest
# ---------------------------------------------------------------------------
TOKENIZER_FIXTURE = """\
% a comment with an unbalanced ( paren and a , comma
narrative_ontology:constraint_victim(alpha, 'a victim, with a comma').
narrative_ontology:stakeholder_gain_flow(
    beta,
    diffuse).
/* block comment: narrative_ontology:ghost_pred(a, b).
   narrative_ontology:ghost_head(c, d). */
:- multifile narrative_ontology:has_sunset_clause/1.
narrative_ontology:has_sunset_clause(gamma).
"""

BODY_GOAL_FIXTURE = """\
narrative_ontology:constraint_metric(alpha, extractiveness, 0.4).
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(alpha, ExtMetricName, E),
    E =< 0.25.
"""

FIXTURES = [
    ("tokenizer: quoted comma does not inflate arity",
     lambda: schema_heads(strip_comments(TOKENIZER_FIXTURE)),
     {("constraint_victim", 2), ("stakeholder_gain_flow", 2), ("has_sunset_clause", 1)}),
    ("tokenizer: multi-line fact keeps its true arity",
     lambda: {a for p, a in schema_heads(strip_comments(TOKENIZER_FIXTURE))
              if p == "stakeholder_gain_flow"},
     {2}),
    ("tokenizer: block-commented heads are not heads",
     lambda: {p for p, _ in schema_heads(strip_comments(TOKENIZER_FIXTURE))
              if p.startswith("ghost")},
     set()),
    ("tokenizer: `:- multifile P/N` is a declaration, not a head",
     lambda: len([1 for p, a in schema_heads(strip_comments(TOKENIZER_FIXTURE))
                  if (p, a) == ("has_sunset_clause", 1)]),
     1),
    ("NAIVE parser control: line-based stripping keeps a block-commented clause head",
     lambda: schema_heads(naive_strip(TOKENIZER_FIXTURE)) ==
             schema_heads(strip_comments(TOKENIZER_FIXTURE)),
     False),
    ("0-arity qualified reference is seen (corpus_loaded/0 was invisible to a `\\w+:\\w+\\(` regex)",
     lambda: ("corpus_loader", "corpus_loaded", 0, 1) in
             qualified_refs("foo :- corpus_loader:corpus_loaded, bar."),
     True),
    ("PREDICATE INDICATOR in a directive is NOT a call (phantom-row control)",
     lambda: qualified_refs(":- multifile narrative_ontology:constraint_beneficiary/2."),
     []),
    ("...but a genuine 0-arity call in the SAME text is still seen (two-sided)",
     lambda: [(m, p_, a) for m, p_, a, _ in qualified_refs(
         ":- multifile narrative_ontology:constraint_beneficiary/2.\ngo :- corpus_loader:corpus_loaded.")],
     [("corpus_loader", "corpus_loaded", 0)]),
    ("reexport(Mod) with no list pulls the whole API through the facade",
     lambda: sorted(effective_exports(
         {"facade": {"exports": set(), "reexports": [("inner", None)]},
          "inner":  {"exports": {("g", 0), ("h", 2)}, "reexports": []}}, "facade")),
     [("g", 0), ("h", 2)]),
    ("reexport(Mod, [List]) pulls ONLY the listed predicates",
     lambda: sorted(effective_exports(
         {"facade": {"exports": set(), "reexports": [("inner", {("g", 0)})]},
          "inner":  {"exports": {("g", 0), ("h", 2)}, "reexports": []}}, "facade")),
     [("g", 0)]),
    ("a reexport CYCLE terminates instead of hanging the gate",
     lambda: sorted(effective_exports(
         {"a": {"exports": {("x", 1)}, "reexports": [("b", None)]},
          "b": {"exports": {("y", 1)}, "reexports": [("a", None)]}}, "a")),
     [("x", 1), ("y", 1)]),
    ("reexports() parses both the bare and the explicit-list form",
     lambda: reexports(":- reexport(foo).\n:- reexport(bar, [baz/2, qux/0]).\n"),
     [("foo", None), ("bar", {("baz", 2), ("qux", 0)})]),
    ("closure arity resolves to the module's single defining arity",
     lambda: closure_arity({"m": {"defined": {("pair_snd", 2)}}}, "m", "pair_snd"),
     2),
    ("genuine 0-arity wins over a same-name higher arity",
     lambda: closure_arity({"m": {"defined": {("clear_hook", 0), ("clear_hook", 1)}}},
                           "m", "clear_hook"),
     0),
    ("ambiguous closure arity is UNRESOLVED, not guessed",
     lambda: closure_arity({"m": {"defined": {("f", 2), ("f", 3)}}}, "m", "f"),
     None),
    ("a FACADE that defines nothing resolves through its reexported API",
     lambda: closure_arity(
         {"f": {"defined": set(), "effective_exports": {("generate_drift_report", 0)}}},
         "f", "generate_drift_report"),
     0),
    ("defined_preds sees column-0 heads and 0-arity clauses, not indented ones",
     lambda: defined_preds("foo(A, B) :- bar.\nbaz :- qux.\n  indented(X).\n"),
     {("foo", 2), ("baz", 0)}),
    ("qualified reference WITH args resolves arity",
     lambda: [(m, p, a) for m, p, a, _ in
              qualified_refs("foo :- maxent_classifier:maxent_dist(C, Ctx, D).")],
     [("maxent_classifier", "maxent_dist", 3)]),
    ("export list parses name/arity pairs",
     lambda: ("maxent_fitted", 1) in export_list(
         ":- module(m, [\n  foo/2,  % c\n  maxent_fitted/1\n])."),
     True),
    ("multi-line :- multifile directive is parsed whole",
     lambda: directive_preds(":- multifile\n    a/1,\n    b/2.\n", "multifile"),
     {("a", 1), ("b", 2)}),
    ("ROLE=internal-state is REJECTED by ruling",
     lambda: "internal-state" in LEGAL_ROLES,
     False),
    ("allowlist row grammar accepts a well-formed row",
     lambda: bool(ROW.match("narrative_ontology:constraint_victim/2  ROLE=corpus-schema  "
                            "written by testsets")),
     True),
    ("a GOAL in a clause body is NOT a head (the ExtMetricName variable control)",
     lambda: sorted(a.strip() for _p, _ar, args, _ln in
                    schema_head_terms(strip_comments(BODY_GOAL_FIXTURE), values=True)
                    for a in args),
     ["0.4", "alpha", "extractiveness"]),
    ("...and the real head in the SAME text is still counted (two-sided)",
     lambda: schema_heads(strip_comments(BODY_GOAL_FIXTURE)),
     {("constraint_metric", 3)}),
    ("bracket-only arguments are ARGUMENTS, not an empty list (phantom /0 control)",
     lambda: arity_at("adjacent_pairs([], []).", len("adjacent_pairs")),
     2),
    ("...and the empty argument list is still 0 (two-sided against that fix)",
     lambda: arity_at("clear_kb().", len("clear_kb")),
     0),
    ("an ALL-bracket nested term keeps its true arity (the first rewrite read 3 here)",
     lambda: arity_at("context([],[],[],[]).", len("context")),
     4),
    ("the two scan modes agree by construction (one grammar, not two parsers)",
     lambda: all(len(split_args(t, t.index("("))) == arity_at(t, t.index("("))
                 for t in ["f()", "f(a)", "f([], [])", "f('x, y')", "f(a, [b, c], d)",
                           "f(context([],[],[],[]))", "f(g(h(1,2)), [x|Y])"]),
     True),
    ("allowlist row grammar REJECTS a reasonless row",
     lambda: bool(ROW.match("narrative_ontology:constraint_victim/2  ROLE=corpus-schema")),
     False),
]


def selftest() -> list[str]:
    fails = []
    for label, fn, expect in FIXTURES:
        try:
            got = fn()
        except Exception as e:                        # a throwing probe is a failing probe
            fails.append(f"SELFTEST {label}: raised {e!r}")
            continue
        if got != expect:
            fails.append(f"SELFTEST {label}: expected {expect!r}, got {got!r}")
    return fails


def main(argv: list[str]) -> int:
    legs = ALL_ARM_C_LEGS if "--full" in argv else DEFAULT_ARM_C_LEGS

    st = selftest()
    if st:
        for f in st:
            print(f"  {f}")
        print("module_boundary_check: RED (selftest)")
        return 1
    if "--selftest" in argv:
        print(f"module_boundary_check: selftest {len(FIXTURES)}/{len(FIXTURES)} "
              f"(tokenizer, 0-arity, grammar, and the rejected naive-parser control)")
        return 0

    if "--list" in argv:
        entries, _ = parse_allowlist()
        files = engine_files()
        bodies = engine_bodies(files)
        mods = build_module_table(files, bodies)
        for key, sites in sorted(find_bypasses(files, mods, bodies).items()):
            role = entries.get(key, ("UNDECLARED", ""))[0]
            print(f"{role:20} {key[0]}:{key[1]}/{key[2]:<2} {len(sites):3d} site(s)")
        return 0

    problems, notes, stats = live_sweep(legs)
    for n in notes:
        print(f"  {n}")
    if problems:
        for e in problems:
            print(f"  {e}")
        print(f"module_boundary_check: RED — {len(problems)} boundary problem(s)")
        return 1
    print(f"module_boundary_check: GREEN — {stats['files']} engine files, "
          f"{stats['modules']} modules, {stats['sites']} bypass site(s) over "
          f"{stats['bypasses']} predicate(s), all declared in {stats['rows']} allowlist "
          f"row(s); arm C scanned leg(s): {', '.join(stats['legs'])} "
          f"({stats['heads']} schema heads); {len(notes)} stale-row note(s); "
          f"{len(UNWIRED_SCHEMA)} unwired schema predicate(s) watched "
          f"({', '.join(f'{p}/{a}' for p, a in sorted(UNWIRED_SCHEMA))}); "
          f"selftest {len(FIXTURES)}/{len(FIXTURES)}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
