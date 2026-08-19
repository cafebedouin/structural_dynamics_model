#!/usr/bin/env python3
"""codewalk_caller_check.py — the CALL-SITE arm of the bound-caller instrument pair.

Wraps prolog/codewalk_caller.pl (library(prolog_codewalk) over the LOADED program) and
feeds it python/dispatch_head_check.py's DECLARED registry — imported, not copied, so the
worklist and the checker cannot fork (build_discipline Pattern 2).

WHY IT EXISTS. The `latent-B` class label in that registry means "shape present, NO LIVE
BOUND CALLER FOUND in the 2026-08-17 caller sweep". That sweep is a single-line regex
(audits/2026-08-17_bound_dispatch_hardening/caller_sweep.py) whose blind spots are readable
in its own source at :44 and :72-73 — one physical line at a time, no nested-term arguments
(conceded in its docstring), bare lowercase atoms only, name/arity textual matching with no
module resolution. So the premise arm (a) of OQ-303 rides on is not "none exist", it is
"none found by one instrument with undeclared blind spots". This is the second instrument.

THE TWO ARMS ARE NOT NESTED — this one is NOT a strict superset:
  * regex-only: source in modules the load chain never reaches; goal strings embedded in
    Python/shell; any file outside the loaded program.
  * codewalk-only: module-resolved callees; multi-line clause bodies; nested-term
    arguments; goals reached through meta-predicates.
  * codewalk-only, AND THIS ONE CORRECTS THE PREMISE THE CHECKER WAS BUILT ON: a selector
    bound by UNIFICATION before the call (`T = rope, ..., p(C, T)`). This was expected to
    be residue invisible to both arms. It is not — library(prolog_codewalk) EXECUTES `A=B`
    while walking (`evaluate/2`, prolog_codewalk.pl:663-664, on by default), so the binding
    propagates and the call reports BOUND. Witnessed 2026-08-18; SWI does not compile the
    unification away (`clause/2` still shows `_G=alpha, q(a,_G)`), so it is the walker's
    abstract interpretation doing it.
  * SHARED RESIDUE, seen by NEITHER, and narrower than expected: a selector bound by
    RUNTIME COMPUTATION — `member(T, Types)`, a helper predicate's output, arithmetic,
    findall. `evaluate/2` handles unification only, so these stay free under the walker and
    are a variable in last position to the regex.
A zero from this checker means "this arm declined", never "no caller exists".

The unification-bound stratum is therefore MEASURABLE, not merely declared: run the walker
with evaluate(true) and evaluate(false) and diff (`--evaluate false`).

DISCRIMINATION RECORD — ANCHORED TO CONTENT, not to a commit (operator, 2026-08-17). The
record is the output text below; run --check to reproduce it. Both halves are required in
the SAME process before any zero from that process is readable (PREREGISTRATION §4,
audits/2026-08-18_bound_caller_rewitness/):

On the chain --check actually uses ([stack] + every registry file except LOAD_EXCLUSIONS):

  FIRES    drl_core.pl dr_type/3 module=drl_core sites=80 bound=20
  DECLINES signature_detection.pl constraint_signature/2 module=signature_detection sites=29 bound=0

On `[stack]` alone the same pair reads sites=67 bound=19 and sites=18 bound=0. Both are
recorded because the FIGURES ARE CHAIN-RELATIVE and a reader reproducing one on the other
chain would otherwise read a real difference as drift. The discriminating property is
neither integer: it is bound>0 on the fires half and (sites>0, bound=0) on the declines half.

The DECLINES half is the informative one: the arm LOOKED (sites=29) and declined on the
bound question (bound=0). A declines-control reporting sites=0 means it never looked, and
invalidates every zero in that run — enforced by check_controls(), not merely documented.

Usage:
    python3 python/codewalk_caller_check.py --check     # selftest, then live sweep
    python3 python/codewalk_caller_check.py --selftest  # fixtures only
    python3 python/codewalk_caller_check.py --list      # every call site, classified
    python3 python/codewalk_caller_check.py --json      # machine-readable per-predicate rows

ALLOWLIST (prolog/codewalk_caller_allowlist.txt). A `latent-B` row asserts "no live bound
caller". When this arm finds one, the assertion is wrong and the row is RED unless it carries
an adjudication there — with its ATOMS and its REMOVE condition. Neither red-forever nor
unwired: a knowingly-red row trains readers to ignore the channel, and an unwired checker
cannot catch the NEXT one (operator ruling, 2026-08-18). ATOMS is enforced per-atom because
over-permissiveness is atom-specific — signature_grade/2 is exactly identical to the
unbound-then-filter form at `correction` on all five legs and diverges by 29-167 constraints
per leg at `commentary`.

RECOVERY PASS. LOAD_EXCLUSIONS is specific to evaluate(true); the excluded file walks fine at
evaluate(false). Rows it costs are re-walked there and graded
`converts-clean-minus-dataflow` — module-resolved, multi-line-body, meta-call-aware, minus
only the unification-bound stratum. The missing stratum is named in the grade, not rounded off.
"""
from __future__ import annotations

import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
PROLOG = REPO / "prolog"
WALKER = PROLOG / "codewalk_caller.pl"
ALLOWLIST = PROLOG / "codewalk_caller_allowlist.txt"

sys.path.insert(0, str(REPO / "python"))
from dispatch_head_check import DECLARED, MUST_NOT_FIRE  # the worklist, imported not copied

# The two-sided control pair. dr_type/3 is deliberately NOT a registry member — it is the
# converted immune idiom, which is exactly why it is the fires-control: the shape is gone
# but the bound CALLERS are not, so a caller instrument must still see them.
FIRES_CONTROL = ("drl_core.pl", "dr_type/3")
DECLINES_CONTROL = ("signature_detection.pl", "constraint_signature/2")

# Declared blind spots — PRINTED in the green line so none of them is silent.
DECLARED_BLIND_SPOTS = [
    "unloaded-modules (walks the [stack] program only; unresolved rows reported, not scored 0)",
    "embedded-goal-strings (python/shell literals — the regex arm's territory)",
    "runtime-bound selectors (`member(T,Ts), p(C,T)` and helper-predicate output read as "
    "free; unification-bound selectors DO resolve, via prolog_codewalk evaluate/2)",
    "output-not-last (last argument assumed to be the output, per dispatch_head_check.pl:9-11)",
]

ALLOW_RE = re.compile(
    r"^(?P<file>\S+\.pl):(?P<pi>\S+/\d+)\s+ATOMS=(?P<atoms>\S+)\s+"
    r"REMOVE=(?P<remove>.+?)\s{2,}(?P<reason>\S.*)$")


def read_allowlist(path: Path = ALLOWLIST) -> dict[tuple[str, str], dict]:
    """Parse the allowlist. A MALFORMED row is RED, never a skipped line.

    Absence of the file is also RED: an allowlist-shaped guard whose allowlist vanished
    would otherwise go green by having nothing to compare against (Pattern 5).
    """
    if not path.exists():
        raise SystemExit(f"codewalk_caller_check: RED — allowlist missing: {path}")
    entries: dict[tuple[str, str], dict] = {}
    for lineno, raw in enumerate(path.read_text().splitlines(), 1):
        line = raw.rstrip()
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        m = ALLOW_RE.match(line)
        if not m:
            raise SystemExit(
                f"codewalk_caller_check: RED — malformed allowlist row {path.name}:{lineno}: "
                f"{line[:120]!r} (grammar: <file.pl>:<name>/<arity>  ATOMS=<a[,b]>  "
                f"REMOVE=<condition>  <reason>; two+ spaces separate the columns)")
        key = (m.group("file"), m.group("pi"))
        if key in entries:
            raise SystemExit(f"codewalk_caller_check: RED — duplicate allowlist row for "
                             f"{key[0]} {key[1]} at {path.name}:{lineno}")
        entries[key] = {"atoms": set(m.group("atoms").split(",")),
                        "remove": m.group("remove").strip(),
                        "reason": m.group("reason").strip(), "line": lineno}
    return entries


PRED_RE = re.compile(
    r"^CWC_PRED: (\S+) (\S+/\d+) module=(\S+) sites=(\d+) bound=(\d+)\s*$")
SITE_RE = re.compile(
    r"^CWC_SITE: (\S+) (\S+/\d+) (bound|free) (\S+):(\d+) caller=(\S+) atom=(\S+)\s*$")
UNRES_RE = re.compile(r"^CWC_UNRESOLVED: (\S+) (\S+/\d+) reason=(\S+)\s*$")


class WalkResult:
    def __init__(self) -> None:
        self.preds: dict[tuple[str, str], dict] = {}
        self.sites: list[dict] = []
        self.unresolved: list[tuple[str, str, str]] = []
        self.walked = 0
        self.scanned = 0
        self.modules = 0
        self.raw = ""


def run_walker(specs: list[tuple[str, str]], load_goals: list[str],
               cwd: Path = PROLOG, evaluate: bool = True) -> WalkResult:
    """Run the walker over `specs` after consulting `load_goals`. Fails closed.

    evaluate=False switches off prolog_codewalk's `A=B` propagation, so the run sees only
    selectors written literally at the call site. The difference between the two runs IS
    the unification-bound stratum.
    """
    with tempfile.NamedTemporaryFile("w", suffix=".spec", delete=False) as fh:
        for deffile, pi in specs:
            fh.write(f"{deffile} {pi}\n")
        specpath = fh.name
    cmd = ["swipl", "-q"]
    for g in load_goals:
        cmd += ["-g", g]
    ev = "true" if evaluate else "false"
    cmd += ["-l", str(WALKER),
            "-g", f"run_codewalk_caller('{specpath}', {ev}), halt", "-t", "halt(1)"]
    proc = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True, timeout=600)
    Path(specpath).unlink(missing_ok=True)

    r = WalkResult()
    r.raw = proc.stdout
    for ln in proc.stdout.splitlines():
        if (m := PRED_RE.match(ln)):
            r.preds[(m.group(1), m.group(2))] = {
                "module": m.group(3), "sites": int(m.group(4)), "bound": int(m.group(5))}
        elif (m := SITE_RE.match(ln)):
            r.sites.append({"deffile": m.group(1), "pi": m.group(2), "kind": m.group(3),
                            "file": m.group(4), "line": int(m.group(5)),
                            "caller": m.group(6), "atom": m.group(7)})
        elif (m := UNRES_RE.match(ln)):
            r.unresolved.append((m.group(1), m.group(2), m.group(3)))
        elif ln.startswith("CWC_WALKED:"):
            r.walked = int(ln.split()[1])
        elif ln.startswith("CWC_SCANNED:"):
            r.scanned = int(ln.split()[1])
        elif ln.startswith("CWC_MODULES:"):
            r.modules = int(ln.split()[1])
    if proc.returncode != 0 or r.scanned == 0:
        raise SystemExit(
            f"codewalk_caller_check: RED — walker did not complete (rc={proc.returncode}, "
            f"scanned={r.scanned}, modules={r.modules}). stderr: {proc.stderr[-800:]}")
    return r


# ---------------------------------------------------------------------------
# Selftest fixtures — real Prolog files, consulted and swept by the REAL walker on the
# SAME code path as the live run. Four of the five positive shapes are ones the regex arm
# structurally cannot see; that is the point of the pair.
# ---------------------------------------------------------------------------
FIXTURE_MAIN = """
:- module(fx_main, [fx_bound_line/0]).

% The dispatch shape under study (definition side).
fx_pred(_, foo) :- fail, !.
fx_pred(_, bar) :- fail, !.
fx_pred(_, baz).

fx_quiet(_, alpha) :- fail, !.
fx_quiet(_, beta).

% (1) plain bound caller — both arms see this one
fx_bound_line :- fx_pred(a, rope).

% (2) bound caller split across physical lines — REGEX-BLIND
fx_bound_multiline :-
    fx_pred(
        a,
        snare).

% (3) bound caller with a nested-term argument — REGEX-BLIND (conceded at caller_sweep.py:44)
fx_bound_nested :- fx_pred(f(x, y), mountain).

% (4) bound caller reached through a meta-predicate — REGEX-BLIND
fx_bound_meta :- forall(member(X, [1, 2]), fx_pred(X, scaffold)).

% (5) NEGATIVE: free last argument, post-filtered — the conforming idiom
fx_free :- fx_pred(a, T), T == rope.

% (6) NEGATIVE: a comment is not a call site: fx_pred(a, comment_only_never_a_site)

% (7) UNIFICATION-BOUND selector. prolog_codewalk EXECUTES `A=B` while walking
%     (evaluate/2, prolog_codewalk.pl:663-664), so this resolves to BOUND under the
%     default and to FREE under evaluate(false). The pair of runs measures the stratum.
fx_unify :- T = alpha, fx_quiet(a, T).

% (8) RUNTIME-BOUND selector — the genuine shared residue. `evaluate/2` handles
%     unification only, so this stays FREE under both flags and is a variable in last
%     position to the regex arm. If this ever reads BOUND, the residue claim is false.
fx_runtime :- member(T, [alpha, beta]), fx_runtime_sink(a, T).

fx_runtime_sink(_, alpha) :- fail, !.
fx_runtime_sink(_, beta).
"""

FIXTURE_OTHER = """
:- module(fx_other, []).

% Same predicate NAME AND ARITY in a different module. A name/arity textual matcher
% attributes this caller to fx_main; a module-resolved walker must not.
fx_pred(_, foo) :- fail, !.
fx_pred(_, baz).

fx_other_bound :- fx_pred(a, tangled_rope).
"""


def selftest() -> list[str]:
    fails: list[str] = []
    with tempfile.TemporaryDirectory(prefix="cwc_selftest_") as td:
        tdp = Path(td)
        (tdp / "fx_main.pl").write_text(FIXTURE_MAIN)
        (tdp / "fx_other.pl").write_text(FIXTURE_OTHER)
        specs = [("fx_main.pl", "fx_pred/2"),
                 ("fx_main.pl", "fx_quiet/2"),
                 ("fx_main.pl", "fx_runtime_sink/2"),
                 ("fx_other.pl", "fx_pred/2"),
                 ("nonexistent_module.pl", "never_loaded/2")]
        loads = [f"['{tdp / 'fx_main.pl'}']", f"['{tdp / 'fx_other.pl'}']"]
        try:
            r = run_walker(specs, loads, cwd=tdp)
            r_noeval = run_walker(specs, loads, cwd=tdp, evaluate=False)
        except SystemExit as e:
            return [f"SELFTEST walker failed on fixtures: {e}"]

        main_pred = r.preds.get(("fx_main.pl", "fx_pred/2"))
        other_pred = r.preds.get(("fx_other.pl", "fx_pred/2"))
        quiet = r.preds.get(("fx_main.pl", "fx_quiet/2"))

        if main_pred is None:
            fails.append("SELFTEST fx_main:fx_pred/2 not reported at all")
        else:
            atoms = {s["atom"] for s in r.sites
                     if s["deffile"] == "fx_main.pl" and s["pi"] == "fx_pred/2"
                     and s["kind"] == "bound"}
            for want, label in [("rope", "plain bound caller"),
                                ("snare", "MULTI-LINE bound caller (regex-blind)"),
                                ("mountain", "NESTED-TERM argument (regex-blind)"),
                                ("scaffold", "META-CALLED bound caller (regex-blind)")]:
                if want not in atoms:
                    fails.append(f"SELFTEST {label}: expected bound atom `{want}`, "
                                 f"got {sorted(atoms)}")
            if "tangled_rope" in atoms:
                fails.append("SELFTEST module resolution: fx_other's caller was attributed "
                             "to fx_main — the walker is matching name/arity, not module")
            if not any(s["kind"] == "free" for s in r.sites
                       if s["deffile"] == "fx_main.pl" and s["pi"] == "fx_pred/2"):
                fails.append("SELFTEST conforming free-variable call site was not seen at "
                             "all — a caller instrument that misses free calls cannot "
                             "produce the informative `looked and declined` result")

        if other_pred is None:
            fails.append("SELFTEST fx_other:fx_pred/2 not reported at all")
        elif other_pred["bound"] != 1:
            fails.append(f"SELFTEST fx_other:fx_pred/2 expected bound=1, "
                         f"got {other_pred['bound']}")

        # UNIFICATION-BOUND selector: bound under evaluate(true), free under
        # evaluate(false). This pair is what makes the stratum measurable rather than
        # merely declared — and it is the control that corrected this unit's premise.
        quiet_ne = r_noeval.preds.get(("fx_main.pl", "fx_quiet/2"))
        if quiet is None:
            fails.append("SELFTEST fx_quiet/2 not reported at all")
        else:
            if quiet["sites"] == 0:
                fails.append("SELFTEST fx_quiet/2 sites=0 — the walker did not look, so "
                             "its bound=0 witnesses nothing (the didn't-look/measured-empty "
                             "collapse this control exists to prevent)")
            if quiet["bound"] != 1:
                fails.append(f"SELFTEST unification-bound selector: expected bound=1 under "
                             f"evaluate(true), got {quiet['bound']} — prolog_codewalk's "
                             f"evaluate/2 no longer propagates `A=B`, so the measured "
                             f"unification stratum in this audit is invalid")
        if quiet_ne is None:
            fails.append("SELFTEST fx_quiet/2 not reported under evaluate(false)")
        elif quiet_ne["bound"] != 0:
            fails.append(f"SELFTEST evaluate(false) still resolved a unification-bound "
                         f"selector (bound={quiet_ne['bound']}) — the two runs do not "
                         f"differ, so their difference measures nothing")

        # RUNTIME-BOUND selector: the genuine shared residue. Free under BOTH flags.
        for label, res in (("evaluate(true)", r), ("evaluate(false)", r_noeval)):
            sink = res.preds.get(("fx_main.pl", "fx_runtime_sink/2"))
            if sink is None:
                fails.append(f"SELFTEST fx_runtime_sink/2 not reported under {label}")
            elif sink["sites"] == 0:
                fails.append(f"SELFTEST fx_runtime_sink/2 sites=0 under {label} — did not look")
            elif sink["bound"] != 0:
                fails.append(f"SELFTEST shared-residue control: a RUNTIME-bound selector "
                             f"read as BOUND under {label} — the residue claim is false "
                             f"and must be re-adjudicated before shipping")

        # A comment is not a call site.
        if any(s["atom"] == "comment_only_never_a_site" for s in r.sites):
            fails.append("SELFTEST comment/text was read as a call site")

        if not any(u[0] == "nonexistent_module.pl" for u in r.unresolved):
            fails.append("SELFTEST an unloadable spec did not report CWC_UNRESOLVED — "
                         "unloaded modules would be silently scored zero")

    # Empty-spec fail-loud (Pattern 5): an empty sweep is a broken sweep, not a clean one.
    try:
        run_walker([], ["true"], cwd=PROLOG)
        fails.append("SELFTEST empty spec did not fail loud")
    except SystemExit:
        pass

    fails.extend(allowlist_selftest())
    return fails


# ---------------------------------------------------------------------------
# The allowlist is itself a claim, so it owes its own two-sided controls: the parser must
# ACCEPT a well-formed row and REFUSE each way a row can be wrong. An allowlist that silently
# skips a line it cannot parse is an exemption channel with no floor.
# ---------------------------------------------------------------------------
ALLOWLIST_FIXTURES: list[tuple[str, str, bool]] = [
    # (label, text, expect_parse_ok)
    ("well-formed row parses",
     "foo.pl:bar/2  ATOMS=alpha  REMOVE=when bar/2 is converted  because reasons\n", True),
    ("multi-atom row parses",
     "foo.pl:bar/2  ATOMS=alpha,beta  REMOVE=on conversion  because reasons\n", True),
    ("comments and blanks are skipped, not parsed",
     "# a comment\n\n   \nfoo.pl:bar/2  ATOMS=alpha  REMOVE=x  y\n", True),
    ("REFUSE missing ATOMS",
     "foo.pl:bar/2  REMOVE=on conversion  because reasons\n", False),
    ("REFUSE missing REMOVE",
     "foo.pl:bar/2  ATOMS=alpha  because reasons\n", False),
    ("REFUSE missing reason",
     "foo.pl:bar/2  ATOMS=alpha  REMOVE=on conversion\n", False),
    ("REFUSE bare predicate indicator with no file",
     "bar/2  ATOMS=alpha  REMOVE=x  y\n", False),
    ("REFUSE duplicate rows for one predicate",
     "foo.pl:bar/2  ATOMS=alpha  REMOVE=x  y\nfoo.pl:bar/2  ATOMS=beta  REMOVE=x  y\n",
     False),
]


def allowlist_selftest() -> list[str]:
    fails: list[str] = []
    with tempfile.TemporaryDirectory(prefix="cwc_allow_") as td:
        for i, (label, text, ok) in enumerate(ALLOWLIST_FIXTURES):
            f = Path(td) / f"allow_{i}.txt"
            f.write_text(text)
            try:
                read_allowlist(f)
                got = True
            except SystemExit:
                got = False
            if got != ok:
                fails.append(f"SELFTEST allowlist {label}: expected parse_ok={ok}, got {got}")
        # A missing allowlist must be RED, not an empty (and therefore permissive) one.
        try:
            read_allowlist(Path(td) / "does_not_exist.txt")
            fails.append("SELFTEST allowlist absence did not fail loud — a vanished "
                         "allowlist would go GREEN by having nothing to compare against")
        except SystemExit:
            pass
        # The LIVE allowlist must parse, and every row must name a real registry entry.
        try:
            live = read_allowlist()
        except SystemExit as e:
            return fails + [f"SELFTEST live allowlist does not parse: {e}"]
        for key, entry in live.items():
            if key not in DECLARED:
                fails.append(f"SELFTEST live allowlist row {entry['line']} names {key[0]} "
                             f"{key[1]}, absent from the dispatch_head_check registry")
            if not entry["remove"]:
                fails.append(f"SELFTEST live allowlist row {entry['line']} has an empty "
                             f"REMOVE condition — a permanent exemption in temporary clothes")
    return fails


# ---------------------------------------------------------------------------

def live_specs() -> list[tuple[str, str]]:
    specs = {(f, pi) for (f, pi) in DECLARED}
    specs.add(FIRES_CONTROL)
    return sorted(specs)


# DECLARED LOAD EXCLUSION — printed in the green line, never silent.
LOAD_EXCLUSIONS = {
    "json_report.pl":
        "prolog_walk_code does not terminate on it under evaluate(true): >90s vs 0.5-0.7s "
        "for every other registry file (measured 2026-08-18, per-file budget sweep). It "
        "walks in 0.6s under evaluate(false), so the non-termination is in the `A=B` "
        "propagation itself — the same feature that resolves unification-bound selectors. "
        "Cost: boltzmann_label/2 and live_index_label/3 (latent-B) plus write_json_number/2 "
        "(input-key) stay UNRESOLVED under this arm and are scored by the regex arm only.",
}


def live_goals() -> list[str]:
    """`[stack]` PLUS every registry-named file, explicitly, minus the declared exclusions.

    `[stack]` alone loads 11 of the registry's 26 files NOT AT ALL (witnessed 2026-08-18:
    json_report, fpn_report, maxent_report, orbit_report, diagnostic_summary, routing_sink,
    giant_component_analysis, invertibility_analysis, gap_diagnostic, context_profile_mining,
    probe_oq197_controls). Left at `[stack]`, 17 of the 58 latent-B rows are unscoreable by
    this arm and would read as a clean zero. `ensure_loaded/1`, not `use_module/1`: four of
    those files carry no module header at all and use_module refuses them.
    """
    files = sorted({f for (f, _pi) in live_specs()} - set(LOAD_EXCLUSIONS))
    loads = ", ".join(f"catch(ensure_loaded('{f}'), _, true)" for f in files)
    return ["['stack']", f"forall(member(G, [{loads}]), (call(G) -> true ; true))"]


def live_run(evaluate: bool = True) -> WalkResult:
    return run_walker(live_specs(), live_goals(), cwd=PROLOG, evaluate=evaluate)


def recovery_run() -> WalkResult:
    """Second pass at evaluate(false), WITH the excluded file loaded.

    The exclusion in LOAD_EXCLUSIONS is specific to `evaluate(true)` — json_report.pl walks
    in 0.6s once `A=B` propagation is off. So the rows it costs need not stay
    single-instrument: at evaluate(false) they get a module-resolved, multi-line-body,
    meta-call-aware codewalk verdict — everything the second arm buys EXCEPT the
    unification-bound stratum. That is a materially different epistemic state from
    regex-only, and it is one subprocess away.

    Rows recovered here are graded `converts-clean-minus-dataflow`, never `converts-clean`:
    the missing stratum is declared in the grade rather than rounded off.
    """
    files = sorted({f for (f, _pi) in live_specs()})
    loads = ", ".join(f"catch(ensure_loaded('{f}'), _, true)" for f in files)
    goals = ["['stack']", f"forall(member(G, [{loads}]), (call(G) -> true ; true))"]
    return run_walker(live_specs(), goals, cwd=PROLOG, evaluate=False)


def check_controls(r: WalkResult) -> list[str]:
    """The zero rule, enforced rather than documented (PREREGISTRATION §4)."""
    problems = []
    fires = r.preds.get(FIRES_CONTROL)
    decl = r.preds.get(DECLINES_CONTROL)
    if fires is None or fires["bound"] == 0:
        problems.append(
            f"CONTROL (fires) {FIRES_CONTROL[1]}: expected bound > 0, got "
            f"{fires and fires['bound']} — every zero in this run is uninterpretable")
    if decl is None:
        problems.append(f"CONTROL (declines) {DECLINES_CONTROL[1]}: not reported")
    else:
        if decl["sites"] == 0:
            problems.append(
                f"CONTROL (declines) {DECLINES_CONTROL[1]}: sites=0 — the arm did not "
                f"look, so its bound=0 is a didn't-look, not a decline")
        if decl["bound"] != 0:
            problems.append(
                f"CONTROL (declines) {DECLINES_CONTROL[1]}: bound={decl['bound']}, "
                f"expected 0 — a converted predicate acquired a live bound caller")
    return problems


def main(argv: list[str]) -> int:
    st = selftest()
    if st:
        for f in st:
            print(f"  {f}")
        print("codewalk_caller_check: RED (selftest)")
        return 1
    if "--selftest" in argv:
        print(f"codewalk_caller_check: selftest OK (4 regex-blind positive shapes, "
              f"module-resolution control, unification-stratum control both ways "
              f"[evaluate true/false], runtime-residue control both ways, free-call "
              f"looked-and-declined control, comment negative, unresolved-spec control, "
              f"empty-scan control, {len(ALLOWLIST_FIXTURES)} allowlist-grammar fixtures "
              f"[3 accept / 5 refuse] + allowlist-absence control + live-allowlist "
              f"registry-membership and REMOVE-nonempty checks)")
        return 0

    evaluate = not ("--evaluate" in argv and "false" in argv)
    allow = read_allowlist()
    r = live_run(evaluate=evaluate)
    control_problems = check_controls(r)

    # RECOVERY: fill in rows the primary pass could not resolve (LOAD_EXCLUSIONS), at
    # evaluate(false), which is the setting the exclusion is specific to.
    recovered: dict[tuple[str, str], dict] = {}
    if evaluate and r.unresolved:
        rec = recovery_run()
        rec_problems = check_controls(rec)
        if rec_problems:
            control_problems += [f"RECOVERY {p}" for p in rec_problems]
        else:
            rec_pred = {(x["deffile"], x["pi"]): x for x in
                        [{"deffile": k[0], "pi": k[1], **v} for k, v in rec.preds.items()]}
            for key in [(a, b) for a, b, _ in r.unresolved]:
                if key in rec_pred:
                    recovered[key] = rec_pred[key]

    latentb_problems: list[str] = []
    stale_notes: list[str] = []
    # Computed BEFORE the --list/--json branches so every mode reports the same verdict.
    for (f, pi), cls in sorted(DECLARED.items()):
        if cls != "latent-B":
            continue
        row = r.preds.get((f, pi)) or recovered.get((f, pi))
        if row is None or row["bound"] == 0:
            continue
        atoms = {s["atom"] for s in r.sites
                 if (s["deffile"], s["pi"]) == (f, pi) and s["kind"] == "bound"}
        entry = allow.get((f, pi))
        if entry is None:
            latentb_problems.append(
                f"latent-B {f} {pi}: {row['bound']} bound call site(s) under the codewalk "
                f"arm — the class label says none was found. Adjudicate and allowlist (with "
                f"ATOMS + REMOVE) in the same change, or convert.")
            continue
        rogue = sorted(atoms - entry["atoms"])
        if rogue:
            latentb_problems.append(
                f"latent-B {f} {pi}: bound caller(s) on atom(s) {rogue}, which the allowlist "
                f"({ALLOWLIST.name}:{entry['line']}) does NOT cover — it adjudicates only "
                f"{sorted(entry['atoms'])}. Over-permissiveness is atom-specific; the "
                f"listed atom's evidence does not transfer. Adjudicate this atom.")

    # A listed row that stopped firing is a stale entry: note, exit 0, prune it.
    for (f, pi), entry in sorted(allow.items()):
        if (f, pi) not in DECLARED:
            latentb_problems.append(
                f"allowlist row {ALLOWLIST.name}:{entry['line']} names {f} {pi}, which is "
                f"not in the dispatch_head_check registry at all — stale or misspelled")
            continue
        row = r.preds.get((f, pi)) or recovered.get((f, pi))
        if row is not None and row["bound"] == 0:
            stale_notes.append(
                f"stale allowlist row ({ALLOWLIST.name}:{entry['line']}): {f} {pi} no longer "
                f"has a bound caller — prune it. REMOVE condition was: {entry['remove']}")

    problems = control_problems + latentb_problems

    if "--json" in argv:
        rows = []
        for (f, pi), d in sorted(r.preds.items()):
            rows.append({"deffile": f, "pi": pi, "cls": DECLARED.get((f, pi), "CONTROL"),
                         **d})
        print(json.dumps({"preds": rows,
                          "recovered": [{"deffile": k[0], "pi": k[1],
                                         "grade": "converts-clean-minus-dataflow",
                                         **{x: y for x, y in v.items()
                                            if x not in ("deffile", "pi")}}
                                        for k, v in sorted(recovered.items())],
                          "allowlist": [{"deffile": k[0], "pi": k[1], **v}
                                        for k, v in sorted(allow.items())
                                        for v in [{**v, "atoms": sorted(v["atoms"])}]],
                          "stale_notes": stale_notes,
                          "unresolved": [{"deffile": a, "pi": b, "reason": c}
                                         for a, b, c in r.unresolved],
                          "sites": r.sites,
                          "walked": r.walked, "scanned": r.scanned,
                          "modules": r.modules,
                          "control_problems": control_problems,
                          "latentb_problems": latentb_problems}, indent=1))
        return 1 if problems else 0

    if "--list" in argv:
        for (f, pi), d in sorted(r.preds.items()):
            cls = DECLARED.get((f, pi), "CONTROL")
            print(f"{cls:14} {f:34} {pi:34} module={d['module']:26} "
                  f"sites={d['sites']:3} bound={d['bound']:3}")
        for f, pi, reason in sorted(r.unresolved):
            cls = DECLARED.get((f, pi), "CONTROL")
            print(f"{cls:14} {f:34} {pi:34} UNRESOLVED reason={reason}")
        for s in sorted(r.sites, key=lambda s: (s["deffile"], s["pi"], s["file"], s["line"])):
            if s["kind"] == "bound":
                print(f"    site {s['deffile']} {s['pi']:28} {s['file']}:{s['line']} "
                      f"caller={s['caller']} atom={s['atom']}")
        return 1 if problems else 0

    n_latentb = sum(1 for v in DECLARED.values() if v == "latent-B")
    allow_summary = ", ".join(f"{pi}@{sorted(e['atoms'])}"
                              for (_f, pi), e in sorted(allow.items()))
    for n in stale_notes:
        print(f"  note: {n}")
    if problems:
        for p in problems:
            print(f"  {p}")
        print(f"codewalk_caller_check: RED — {len(problems)} problem(s)")
        return 1
    print(f"codewalk_caller_check: GREEN — {r.scanned} registry spec(s) "
          f"({n_latentb} latent-B), {r.modules} loaded module(s), {r.walked} traced goal(s), "
          f"{len(r.preds)} resolved / {len(r.unresolved)} unresolved, "
          f"controls two-sided (fires {FIRES_CONTROL[1]} bound="
          f"{r.preds[FIRES_CONTROL]['bound']}, declines {DECLINES_CONTROL[1]} sites="
          f"{r.preds[DECLINES_CONTROL]['sites']} bound=0); "
          f"{len(allow)} adjudicated allowlist row(s) "
          f"({allow_summary}); "
          f"{len(LOAD_EXCLUSIONS)} declared load exclusion(s) under evaluate(true) "
          f"({', '.join(sorted(LOAD_EXCLUSIONS))}), {len(recovered)} row(s) recovered at "
          f"evaluate(false) as converts-clean-minus-dataflow; declared blind spots: "
          + "; ".join(DECLARED_BLIND_SPOTS) + "; selftest OK")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
