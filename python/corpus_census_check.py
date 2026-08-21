#!/usr/bin/env python3
"""corpus_census_check.py — gate row `corpus census` (OQ-306).

Guards the SHARE OVER TIME of non-story members, which is the thing that
actually went wrong. The `*_contradictions.pl` stratum grew 5 -> 22 -> 26 -> 27
inside `manifest.n_constraints` with nothing going red. A *growing* contaminant
does not bias a rate by a constant — it silently rewrites a time series, so
historical rates stop being comparable to current ones even when each was
correct when computed. Presence was never the defect; unwatched growth was.

TWO ARMS (D4 declares TWO, not three — see "The dropped arm" below):

  1. TOTALITY, fail-closed. Every member of every live leg must kind `story` or
     a KNOWN non-story kind. Any `unknown` or `dual_family` is RED, naming the
     files. These are discoveries about the corpus, not tooling faults.

  2. STRATUM PIN. Per-leg, per-kind non-story counts are pinned against
     corpus_census_baseline.json. RED on ANY delta, in either direction.
     Story counts are NOT pinned — the corpus grows daily by design, and
     pinning growth would make the row red every week for the wrong reason.

ONE DEFINITION, NOT TWO (R-G, ruled 2026-08-21). Every leg derives its kinds
from `corpus_loader:corpus_member_kind/2` via swipl. This checker owns ZERO
classification logic. An earlier design had the four twin legs use a cheap
textual ratchet instead, to dodge per-gate swipl latency — that would have
forked canonicity, which is the exact defect this OQ exists to close, and the
measurement did not support it:

    MEASURED 2026-08-21: kinding costs +143.8 ms over a 285-member load
    (590.71 -> 734.51 ms median, 3 runs/side, warm, corpus md5-pinned)
    = ~0.5 ms per member. Extrapolated across all five legs at their
    2026-08-21 file counts (285 / 960 / 960 / 1005 / 1001 = 4211 members):
    ~2.1 s of kinding.

RE-MEASURE TRIGGER, not a bare wall-clock number (R-G). A one-time number
governing a permanent choice over growing legs goes stale silently, and nobody
revisits a threshold. So the trigger is stamped to GROWTH: re-measure when any
leg exceeds 2x its stamped file count below. That fires on the thing that would
actually invalidate the measurement.

    STAMPED FILE COUNTS (2026-08-21): testsets 285 (live, moves — this one is
    informational only, never a pin), testsets_haiku 960, testsets_flash 960,
    testsets_kimi 1005, testsets_sonnet 1001.

RE-PINNING (R-A, ruled 2026-08-21). Ordinary corpus authoring legitimately
moves the stratum: the generator is live and R2 keeps its output in testsets/.
So re-pinning is EXECUTOR-LICENSED, but every re-pin MUST record a cause and an
authorizer:

    python3 python/corpus_census_check.py --repin \\
        --cause "<what moved it>" --authorized-by "<who>"

The cause field deliberately accepts an ORCHESTRATOR RUN OR TOPIC IDENTIFIER,
not only a commit hash. This is not a convenience: the stratum demonstrably
moves WITHOUT a commit. `f32fe86b` committed a topic run's 5 story cids and left
the `*_contradictions.pl` it emitted untracked, because a contradictions file is
not a run cid and the auto-commit's pathspec is cid-scoped. A schema that only
accepts hashes would be filled with "n/a" within a week, and the cause field is
the ONLY record that will ever exist for such a move.

THE DROPPED ARM (R-E, ruled 2026-08-21). An earlier design had a third arm: a
loud loader warning when the live leg resolves to `testsets` with zero
`cs_axiom_contradiction` clauses, to catch a bulk relocation. It is DROPPED, not
demoted to an ignored info line. Two reasons, the second decisive:

  * It was never enforceable. `load_warning_gate.py` runs `swipl -g "[stack],
    halt"` and never loads the corpus, and its regex is `^(Warning|ERROR):`,
    which `[corpus] WARNING:` does not match. There was no gate arm to scope.
    An unenforced stderr line is a Pattern-6 demotion, which the design forbids.
  * It was redundant anyway. Its trigger — live leg, zero contradiction clauses
    — is a strict SUBSET of arm 2's. Files leaving `testsets/` drive the pin to
    0, which reddens against any nonzero baseline regardless.

Relocation coverage is therefore: files moved out -> arm 2 (pin goes to 0).
Facts moved/renamed with stub files left behind -> arm 1 (stubs kind `unknown`).
Generator redirected so new files stop arriving -> NOT covered by any arm while
the stratum is static; that residue is DECLARED, and its marker lives at
`agent/generate_kernel_corpus.py:emit_axiom_contradiction_facts`, where the
person doing the redirecting is actually reading.

Usage:
    python3 python/corpus_census_check.py --check
    python3 python/corpus_census_check.py --repin --cause "..." --authorized-by "..."
    python3 python/corpus_census_check.py --selftest
"""
from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
PROLOG_DIR = REPO_ROOT / "prolog"
BASELINE = REPO_ROOT / "python" / "corpus_census_baseline.json"

sys.path.insert(0, str(Path(__file__).resolve().parent))
from shared.corpus_legs import LIVE_LEGS  # noqa: E402

# Kinds that are legitimate, known non-story members. `unknown` and
# `dual_family` are deliberately ABSENT: they are not kinds of thing the corpus
# contains, they are failures to determine what it contains, and they belong to
# arm 1 (RED), never to the pin.
KNOWN_NONSTORY_KINDS = ("axiom_contradiction",)
UNCLASSIFIED_KINDS = ("unknown", "dual_family")

# Growth-based re-measure trigger (R-G). Stamped 2026-08-21.
STAMPED_FILE_COUNTS = {
    "testsets_haiku": 960,
    "testsets_flash": 960,
    "testsets_kimi": 1005,
    "testsets_sonnet": 1001,
}
REMEASURE_GROWTH_FACTOR = 2.0


def kind_census(corpus_path: str, timeout: int = 900) -> dict[str, list[str]]:
    """Kinds for one leg, derived from corpus_loader:corpus_member_kind/2.

    Returns {kind: [sorted member ids]}. This checker owns no classification
    logic — the engine predicate is the single definition (R-G).
    """
    goal = (
        "asserta(config:param(corpus_path, '%s')), "
        "corpus_loader:load_all_testsets, "
        "findall(K-C, (corpus_loader:corpus_constraint(C), "
        "              corpus_loader:corpus_member_kind(C, K)), Pairs), "
        "forall(member(K2-C2, Pairs), format('CENSUS ~w ~w~n', [K2, C2])), "
        "halt." % corpus_path
    )
    proc = subprocess.run(
        ["swipl", "-q", "-g", goal, "-t", "halt(1)", "stack.pl"],
        cwd=PROLOG_DIR, capture_output=True, text=True, timeout=timeout,
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"corpus_census_check: swipl failed for leg {corpus_path!r} "
            f"(exit {proc.returncode}). stderr tail:\n{proc.stderr[-1500:]}"
        )
    out: dict[str, list[str]] = {}
    for line in proc.stdout.splitlines():
        if line.startswith("CENSUS "):
            _, kind, cid = line.split(None, 2)
            out.setdefault(kind, []).append(cid.strip())
    if not out:
        # A leg that produced no rows at all is a didn't-look, not an empty
        # corpus: load_all_testsets throws corpus_empty on a zero glob.
        raise RuntimeError(
            f"corpus_census_check: leg {corpus_path!r} produced ZERO census rows. "
            "The corpus loads or it throws — zero rows means the query did not "
            "dispatch, not that the corpus is empty."
        )
    for k in out:
        out[k].sort()
    return out


def load_baseline() -> dict:
    if not BASELINE.exists():
        raise SystemExit(
            f"corpus_census_check: baseline {BASELINE} not found — "
            "run with --repin --cause ... --authorized-by ... to create it."
        )
    return json.loads(BASELINE.read_text(encoding="utf-8"))


def check(legs=LIVE_LEGS) -> tuple[bool, list[str]]:
    """Both arms. Returns (ok, problem lines)."""
    baseline = load_baseline()
    pinned = baseline.get("legs", {})
    problems: list[str] = []

    for leg in legs:
        census = kind_census(leg)

        # ARM 1 — totality, fail-closed.
        for kind in UNCLASSIFIED_KINDS:
            if census.get(kind):
                ids = census[kind]
                problems.append(
                    f"[totality] {leg}: {len(ids)} member(s) kinded `{kind}` — "
                    f"{', '.join(ids[:8])}{' ...' if len(ids) > 8 else ''}. "
                    "This is a finding about the corpus (re-key to filename, or a "
                    "member satisfying both fact families), not a tooling fault."
                )
        for kind in census:
            if kind != "story" and kind not in KNOWN_NONSTORY_KINDS \
                    and kind not in UNCLASSIFIED_KINDS:
                problems.append(
                    f"[totality] {leg}: unrecognized kind `{kind}` "
                    f"({len(census[kind])} member(s)). A new kind needs a "
                    "kind-taxonomy ruling and a baseline update."
                )

        # ARM 2 — stratum pin. COUNTS ONLY; cause/authorized_by are metadata and
        # are never compared, so editing a cause note cannot turn the row red.
        want = pinned.get(leg)
        if want is None:
            problems.append(f"[pin] {leg}: no baseline entry. Re-pin to adopt it.")
            continue
        got = {k: len(v) for k, v in census.items() if k in KNOWN_NONSTORY_KINDS}
        want_counts = {k: v for k, v in want.items() if k in KNOWN_NONSTORY_KINDS}
        if got != want_counts:
            problems.append(
                f"[pin] {leg}: non-story stratum moved — baseline {want_counts or '{}'} "
                f"vs actual {got or '{}'}. If this is ordinary corpus authoring, re-pin "
                "WITH a recorded cause and authorizer; the cause field is the only record "
                "a commitless move will ever have."
            )

        # R-G re-measure trigger.
        stamped = STAMPED_FILE_COUNTS.get(leg)
        if stamped:
            n = sum(len(v) for v in census.values())
            if n > stamped * REMEASURE_GROWTH_FACTOR:
                problems.append(
                    f"[remeasure] {leg}: {n} members vs stamped {stamped} "
                    f"(>{REMEASURE_GROWTH_FACTOR}x). Re-measure the kinding cost and "
                    "re-stamp, per R-G — the one-definition choice rests on that number."
                )

    return (not problems), problems


def repin(cause: str, authorized_by: str, legs=LIVE_LEGS) -> int:
    """Re-pin the stratum. R-A: cause AND authorizer are MANDATORY."""
    if not cause.strip() or not authorized_by.strip():
        raise SystemExit(
            "corpus_census_check: --repin requires BOTH --cause and --authorized-by "
            "(R-A, ruled 2026-08-21). A re-pin with no recorded cause makes the time "
            "series unexplainable, and the stratum can move with no commit to point at."
        )
    existing = json.loads(BASELINE.read_text(encoding="utf-8")) if BASELINE.exists() else {}
    new_legs = {}
    for leg in legs:
        census = kind_census(leg)
        new_legs[leg] = {k: len(v) for k, v in sorted(census.items())
                         if k in KNOWN_NONSTORY_KINDS}
    history = existing.get("provenance", [])
    history.append({
        "cause": cause.strip(),
        "authorized_by": authorized_by.strip(),
        "previous": existing.get("legs", {}),
        "new": new_legs,
    })
    BASELINE.write_text(json.dumps({
        "_comment": (
            "OQ-306 stratum pin. Per-leg counts of KNOWN non-story kinds. Story counts "
            "are deliberately NOT pinned (the corpus grows by design). `unknown` and "
            "`dual_family` never appear here — they are arm-1 REDs, not a stratum. "
            "Only the `legs` counts are compared; `provenance` is metadata, so editing a "
            "cause note can never turn the gate row red. Cause accepts an orchestrator "
            "run or topic identifier, not just a commit hash: the stratum moves without "
            "commits (see f32fe86b)."
        ),
        "legs": new_legs,
        "provenance": history,
    }, indent=2) + "\n", encoding="utf-8")
    print(f"corpus_census_check: re-pinned {len(new_legs)} leg(s) — {new_legs}")
    return 0


# ---------------------------------------------------------------------------
# SELFTEST — rides every --check run.
#
# The story/axiom arms get a NATURALLY-ARISING two-sided control: a real
# contradictions file and a real story file, both picked from the live corpus at
# runtime, neither authored to be found. The DECLINE is the informative half.
#
# The `dual_family` and pin arms get PLANTED fixtures only — the bottom rung of
# the discrimination ladder. Reported at that altitude: a plant shows the
# instrument CAN fire on authored drift, nothing more. The live corpus supplies
# no natural instance of either.
#
# WRITE-FREE with respect to all five legs. This runs at every gate invocation
# and must never race an operator topic run, so every plant lives in a tempdir.
# ---------------------------------------------------------------------------
def selftest() -> tuple[bool, list[str]]:
    fails: list[str] = []
    checks = 0

    live = kind_census("testsets")
    checks += 1
    if not live.get("axiom_contradiction"):
        fails.append("selftest: live leg has no axiom_contradiction member to control on")
    if not live.get("story"):
        fails.append("selftest: live leg has no story member to control on")

    # NATURALLY-ARISING two-sided control.
    if live.get("axiom_contradiction") and live.get("story"):
        checks += 2
        a = live["axiom_contradiction"][0]
        s = live["story"][0]
        if not a.endswith("_contradictions"):
            fails.append(f"selftest: {a} kinded axiom_contradiction but is not a "
                         "*_contradictions file — the instrument fires off-target")
        if s.endswith("_contradictions"):
            fails.append(f"selftest: {s} kinded story but IS a *_contradictions file — "
                         "the instrument declines on the wrong population")

    # PLANTED: a leg carrying an unknown-shape member must go RED (arm 1).
    with tempfile.TemporaryDirectory(dir=str(PROLOG_DIR)) as td:
        tdp = Path(td)
        src = PROLOG_DIR / "testsets"
        for f in sorted(src.glob("*.pl"))[:6]:
            shutil.copy2(f, tdp / f.name)
        (tdp / "planted_unknown_shape.pl").write_text(
            "% planted fixture: carries NEITHER fact family (OQ-306 selftest)\n"
            ":- multifile narrative_ontology:human_readable/2.\n"
            "narrative_ontology:human_readable(planted_unknown_shape, 'planted').\n",
            encoding="utf-8")
        checks += 1
        planted = kind_census(tdp.name)
        if "planted_unknown_shape" not in planted.get("unknown", []):
            fails.append("selftest: PLANTED unknown-shape member did NOT kind `unknown` "
                         f"(got {[k for k, v in planted.items() if 'planted_unknown_shape' in v]})")
        # two-sided: the real files copied alongside it must still kind correctly
        checks += 1
        if not planted.get("story"):
            fails.append("selftest: planted leg lost its real story members — the "
                         "instrument fires on everything, which discriminates nothing")

    # PLANTED: baseline off-by-one must go RED (arm 2).
    #
    # This control was VACUOUS until 2026-08-21 (caught by the OQ-306
    # post-implementation evaluation): it built two dicts guaranteed to differ and
    # asserted they differed. No value of N could make it fire, it exercised
    # nothing in check()'s comparison, and it still counted toward the advertised
    # control total — control count rising while coverage stayed flat, which is
    # the orphaned-control shape (build_discipline -> "A control must witness that
    # it is CALLED"). It now drives the REAL comparison path and is two-sided.
    global BASELINE
    _saved_baseline = BASELINE
    try:
        with tempfile.TemporaryDirectory() as bt:
            n_live = len(live.get("axiom_contradiction", []))
            # (a) a CORRECT baseline must produce no [pin] problem
            BASELINE = Path(bt) / "ok.json"
            BASELINE.write_text(json.dumps(
                {"legs": {"testsets": {"axiom_contradiction": n_live}}}))
            checks += 1
            _, probs_ok = check(["testsets"])
            if any(p.startswith("[pin]") for p in probs_ok):
                fails.append("selftest: correct baseline produced a [pin] problem "
                             "— the pin arm fires on a matching stratum")
            # (b) an off-by-one baseline MUST produce one
            BASELINE = Path(bt) / "off.json"
            BASELINE.write_text(json.dumps(
                {"legs": {"testsets": {"axiom_contradiction": n_live + 1}}}))
            checks += 1
            _, probs_off = check(["testsets"])
            if not any(p.startswith("[pin]") for p in probs_off):
                fails.append("selftest: off-by-one baseline did NOT produce a [pin] "
                             "problem — the pin arm cannot detect a stratum move")
    finally:
        BASELINE = _saved_baseline

    return (not fails), fails + [f"({checks} controls)"]


def main(argv: list[str]) -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--check", action="store_true")
    ap.add_argument("--repin", action="store_true")
    ap.add_argument("--selftest", action="store_true")
    ap.add_argument("--cause", default="")
    ap.add_argument("--authorized-by", dest="authorized_by", default="")
    args = ap.parse_args(argv)

    if args.repin:
        return repin(args.cause, args.authorized_by)

    if args.selftest:
        ok, notes = selftest()
        print(f"corpus_census_check selftest: {'OK' if ok else 'FAILED'} — {'; '.join(notes)}")
        return 0 if ok else 1

    # --check (default): selftest rides every run, so a green line is never a didn't-look.
    st_ok, st_notes = selftest()
    if not st_ok:
        for n in st_notes:
            print(f"  {n}", file=sys.stderr)
        print(f"corpus_census_check: RED — selftest failed ({'; '.join(st_notes)})")
        return 1

    ok, problems = check()
    for p in problems:
        print(f"  {p}", file=sys.stderr)
    baseline = load_baseline()
    strata = {leg: v.get("axiom_contradiction", 0) for leg, v in baseline["legs"].items()}
    if ok:
        print(f"corpus_census_check: GREEN — {len(LIVE_LEGS)} leg(s), totality holds, "
              f"stratum pinned {strata}, selftest {st_notes[-1]}")
        return 0
    print(f"corpus_census_check: RED — {len(problems)} problem(s); "
          f"stratum baseline {strata}")
    return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
