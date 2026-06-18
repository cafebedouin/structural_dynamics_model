#!/usr/bin/env python3
"""
OQ-48 threshold-distribution probe — read-only metric dump for recalibration-readiness.

For OQ-48 (ISSUES.md, Priority 1, Ω_E): the χ / ε / suppression classification thresholds
in config.pl were calibrated on the 691-constraint corpus (2024-2026, logic_thresholds.md:15),
which predates the 2026-06-05 reset. They have never been recalibrated against the rebuilt
corpus. The live testsets/ (80 readings) is far below the recalibration bar, but the matched
twin corpora (testsets_haiku=960, testsets_flash=960) each clear it.

This script is the PROBE half (plan step 1): one swipl process per twin, overlay corpus_path
via `asserta`, load the engine + corpus, HARD-STOP if LOADCOUNT != 960, then emit one
greppable ROW per corpus_constraint at the default analytical context:

    ROW <id> <eps> <supp> <tr> <chi> <MT> <FT>

  eps  = drl_core:base_extractiveness/2
  supp = drl_core:suppression_score/2            (same metric `suppression_requirement` the
         suppression gates read via get_raw_suppression/2; differs only on absence)
  tr   = drl_core:effective_theater_ratio/3      (theater_metric_name = theater_ratio)
  chi  = constraint_indexing:extractiveness_for_agent/3   (canonical sigmoid χ used by the gates)
  MT   = drl_core:metric_based_type_indexed/3    (raw metric class, override suppressed)
  FT   = drl_core:dr_type/3                       (final class, override active)

Absent values print the atom `unknown` (never a fabricated 0 — OQ-44 fail-closed discipline).

DISCIPLINE (CLAUDE.md):
  * ONE swipl process per corpus — positive controls are PER-PROCESS.
  * Corpus overlaid with `asserta` (plain assertz is silently ignored -> loads default 80,
    witnessed hazard). Load count confirmed from corpus_loader:corpus_constraint/1.
  * HARD STOP: if LOADCOUNT != 960 the run aborts for that twin with no ROW analysis — a wrong
    count must never produce misleading output.
  * Records each twin's loaded-id-set sha256 (corpus_hash) for reproducibility.

Read-only: loads engine + corpora, computes, prints. Writes NOTHING to engine or corpus.
The analysis half (histograms, KDE antimodes, verdict rule) is oq48_analyze.py.

Usage:
    python3 python/audits/oq48_threshold_distributions.py                 # both twins
    python3 python/audits/oq48_threshold_distributions.py testsets_haiku  # one twin
"""

import hashlib
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"

TWINS = ["testsets_haiku", "testsets_flash"]
EXPECTED_LOADCOUNT = 960

# ---------------------------------------------------------------------------
# Prolog probe goal. Greppable token-prefixed lines on stdout:
#   LOADCOUNT <n>
#   ROW <id> <eps> <supp> <tr> <chi> <MT> <FT>
#   PROBE_ERROR <msg>
# Everything else (engine load chatter) goes to stderr.
# ---------------------------------------------------------------------------
_GOAL_TMPL = r"""
:- asserta(config:param(corpus_path, '{corpus}')).
:- [stack], corpus_loader:load_all_testsets.

num_or_unknown(G, V) :- ( call(G) -> true ; V = unknown ).

probe_row(C) :-
    constraint_indexing:default_context(Ctx),
    num_or_unknown(drl_core:base_extractiveness(C, Eps), Eps),
    num_or_unknown(drl_core:suppression_score(C, Supp), Supp),
    num_or_unknown(drl_core:effective_theater_ratio(C, theater_ratio, TR), TR),
    num_or_unknown(constraint_indexing:extractiveness_for_agent(C, Ctx, Chi), Chi),
    ( drl_core:metric_based_type_indexed(C, Ctx, MT0) -> true ; MT0 = unknown ),
    ( drl_core:dr_type(C, Ctx, FT0) -> true ; FT0 = unknown ),
    format("ROW ~w ~w ~w ~w ~w ~w ~w~n", [C, Eps, Supp, TR, Chi, MT0, FT0]).

run_probe :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    format("LOADCOUNT ~w~n", [N]),
    forall(corpus_loader:corpus_constraint(C), probe_row(C)).

:- ( run_probe -> true ; format("PROBE_ERROR run_probe failed~n", []) ), halt.
"""


def run_twin(corpus):
    """Run one swipl process for `corpus`; return (loadcount, rows, raw_stdout, stderr)."""
    with tempfile.NamedTemporaryFile("w", suffix=".pl", dir=PROLOG_DIR, delete=False) as tf:
        tf.write(_GOAL_TMPL.format(corpus=corpus))
        goal_path = tf.name
    try:
        proc = subprocess.run(
            ["swipl", "-q", goal_path],
            cwd=PROLOG_DIR,
            capture_output=True,
            text=True,
            timeout=1800,
        )
    finally:
        Path(goal_path).unlink(missing_ok=True)

    loadcount = None
    rows = []
    probe_error = None
    for line in proc.stdout.splitlines():
        t = line.split()
        if not t:
            continue
        if t[0] == "LOADCOUNT":
            loadcount = int(t[1])
        elif t[0] == "ROW" and len(t) == 8:
            rows.append({
                "id": t[1], "eps": t[2], "supp": t[3], "tr": t[4],
                "chi": t[5], "mt": t[6], "ft": t[7],
            })
        elif t[0] == "PROBE_ERROR":
            probe_error = line
    return loadcount, rows, probe_error, proc.stdout, proc.stderr


def corpus_hash(rows):
    """sha256 over the sorted loaded-id set (OQ-29 corpus_hash idea)."""
    ids = sorted(r["id"] for r in rows)
    h = hashlib.sha256()
    for i in ids:
        h.update(i.encode())
        h.update(b"\n")
    return h.hexdigest()


def main(argv):
    twins = argv[1:] if len(argv) > 1 else TWINS
    out_dir = ROOT / "audits" / "2026-06-18_oq48_recalibration"
    out_dir.mkdir(parents=True, exist_ok=True)

    overall_ok = True
    for corpus in twins:
        print(f"\n{'='*78}\nTWIN: {corpus}\n{'='*78}")
        loadcount, rows, probe_error, raw_stdout, stderr = run_twin(corpus)

        if probe_error:
            print(f"  !! {probe_error}")
            print("  stderr tail:")
            print("\n".join("    " + l for l in stderr.splitlines()[-15:]))
            overall_ok = False
            continue

        print(f"LOADCOUNT {loadcount}")
        # ---- HARD STOP: wrong count never produces analysis output ----
        if loadcount != EXPECTED_LOADCOUNT:
            print(f"  !! HARD STOP: LOADCOUNT {loadcount} != {EXPECTED_LOADCOUNT} "
                  f"(overlay likely ignored — must use asserta). Aborting this twin; "
                  f"no rows written.")
            print("  stderr tail:")
            print("\n".join("    " + l for l in stderr.splitlines()[-15:]))
            overall_ok = False
            continue

        if len(rows) != EXPECTED_LOADCOUNT:
            print(f"  !! HARD STOP: {len(rows)} ROW lines != {EXPECTED_LOADCOUNT}. Aborting twin.")
            overall_ok = False
            continue

        chash = corpus_hash(rows)
        print(f"ROWS {len(rows)}   corpus_hash(sha256) {chash}")

        # ---- write raw row TSV sidecar (per-id, the audit's reusable raw evidence) ----
        tsv = out_dir / f"rows_{corpus}.tsv"
        header = "id\teps\tsupp\ttr\tchi\tmt\tft\n"
        body = "\n".join(
            "\t".join((r["id"], r["eps"], r["supp"], r["tr"], r["chi"], r["mt"], r["ft"]))
            for r in rows
        )
        tsv.write_text(header + body + "\n")
        (out_dir / f"corpus_hash_{corpus}.txt").write_text(chash + "\n")
        print(f"  wrote {tsv.relative_to(ROOT)}")

    print(f"\n{'='*78}")
    print("PROBE STATUS:", "PASS" if overall_ok else "FAIL")
    return 0 if overall_ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
