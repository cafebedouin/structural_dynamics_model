#!/usr/bin/env python3
"""
OQ-22 Hub-1/Hub-2 starvation census — Phase-0 (χ→type map) + Phase-1 (per-corpus census).

OQ-22 (ISSUES.md, Priority 1, bundled_with OQ-01): when χ across observer positions is so
compressed that it stays inside a single TYPE-BAND of the cascade, Hub 1 (the χ-driven gates)
cannot move the constraint across any type boundary by changing observer — so the final type
(and any cross-observer variation) is decided by Hub 2 (effective_immutability) alone, yet is
reported as a normal two-hub decision. This script measures whether any constraint enters that
Hub-1 starvation band under the DEFAULT sigmoid on the live corpora.

CRITICAL design point (plan §"the type-band is not the config-gate partition"):
  The four config gate values {rope_ceiling 0.35, scaffold/piton_ceiling 0.45, snare_floor 0.66,
  tangled_rope_ceil 0.90} are NOT four clean type boundaries — tangled_rope (0.35<χ≤0.90) overlaps
  scaffold (χ≤0.45) and snare (χ≥0.66). The cascade resolves the overlap by PRIORITY, so the
  REALIZED χ→type map per (constraint, observer) is single-valued, but its band boundaries are
  emergent (depend on which clauses' non-χ gates pass) and are generally NOT {0.35,0.45,0.66,0.90}.
  `starved` is therefore defined against the REALIZED per-(constraint,observer) χ→type map, swept
  here, never against the config-gate partition.

This is the PROBE/measurement half. It is READ-ONLY w.r.t. engine + corpus (writes only TSVs under
the audit dir). It does NOT pre-commit any engine change (a code change is conditional on findings;
that is a follow-up OQ, decided by the operator).

Per corpus it runs ONE swipl process (positive controls are per-process), overlays corpus_path via
`asserta` (plain assertz is silently ignored -> loads default testsets, witnessed hazard), HARD-STOPS
if the load count != on-disk *.pl count, records a corpus_hash, then per constraint emits:

  BASE <id> <baseeps> <supp>
      base_extractiveness/2 and get_raw_suppression/2 — both observer-INVARIANT (verified against
      the metric_based_type_indexed/3 call path, drl_core.pl:479-483).
  OBS  <id> <obs> <chi> <immut> <mtype> <ftype>
      per observer (powerless/moderate/institutional/analytical = the 4 standard_context positions):
      χ = extractiveness_for_agent/3 (canonical sigmoid, Hub 1), immutability perception (Hub 2),
      metric type = classify_from_metrics/6 (the cascade, where the χ gates live),
      final type = dr_type/3 (after signature overrides — recorded for context, not the band axis).
  BAND <id> <obs> <lo> <hi> <type>
      the REALIZED χ→type map: synthetic χ swept through classify_from_metrics/6 holding the
      constraint's actual BaseEps/Supp and this observer's Context fixed, collapsed to maximal
      same-type χ-intervals (the constraint's true type-bands at that observer).
  GATEOWN <id> <obs> <gateval> <type>
      Phase-0 boundary test: the type the cascade ACTUALLY returns with χ placed exactly on each
      config gate value {0.0,0.35,0.45,0.66,0.90}. Discovers (never ratifies) boundary ownership.

Analysis (starved flag, subsets (a)/(b), the band-width floor, the χ-path bridge) is done by
oq22_analyze.py from these TSVs — evidence first, analysis second (CLAUDE.md audit discipline).

Usage:
    python3 python/audits/oq22_starvation_census.py                 # all four legs
    python3 python/audits/oq22_starvation_census.py testsets        # one leg (e.g. bridge check)
"""

import hashlib
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"
OUT_DIR = ROOT / "audits" / "2026-06-28_oq22_hub_starvation"

# (name -> corpus_path relative to prolog/). All four legs: the live singleton, the two reconciled
# twins, and the kernel_v1 breadth archive (the residual is literally "which ε/d distributions",
# so breadth matters). kernel_v1 is a pre-reset CONFOUNDED arm (template guidance + unknown model +
# regime) — corroboration only, never pooled (OQ-26 single-regime scoping).
CORPUS_SPECS = {
    "testsets": "testsets",
    "testsets_haiku": "testsets_haiku",
    "testsets_flash": "testsets_flash",
    "kernel_v1": "archives/datasets/kernel_v1",
}
DEFAULT_RUN = ["testsets", "testsets_haiku", "testsets_flash", "kernel_v1"]

# ---------------------------------------------------------------------------
# Prolog probe goal.  Greppable token-prefixed lines on stdout; engine chatter on stderr.
#   χ sweep: integer index I from -500..1600, Chi is I/1000.0  -> step 0.001 over [-0.5, 1.6].
#   Bands collapsed in-Prolog to keep output small (kernel_v1 = 1106*4 maps).
# ---------------------------------------------------------------------------
_GOAL_TMPL = r"""
:- asserta(config:param(corpus_path, '{corpus}')).
:- [stack].
{preamble}
:- corpus_loader:load_all_testsets.

% --- observer set: the 4 standard_context positions, named by power atom ---
obs_ctx(powerless,     Ctx) :- Ctx = context(agent_power(powerless),     time_horizon(biographical),  exit_options(trapped),   spatial_scope(local)).
obs_ctx(moderate,      Ctx) :- Ctx = context(agent_power(moderate),      time_horizon(biographical),  exit_options(mobile),    spatial_scope(national)).
obs_ctx(institutional, Ctx) :- Ctx = context(agent_power(institutional), time_horizon(generational),  exit_options(arbitrage), spatial_scope(national)).
obs_ctx(analytical,    Ctx) :- Ctx = context(agent_power(analytical),    time_horizon(civilizational), exit_options(analytical),spatial_scope(global)).

num_or_unknown(G, V) :- ( call(G) -> true ; V = unknown ).

% type the cascade returns for a SYNTHETIC chi, holding this constraint's real BaseEps/Supp and
% this observer's Context fixed.  classify_from_metrics is first-solution (every clause cut).
synth_type(C, BaseEps, Supp, Ctx, Chi, Type) :-
    ( drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Ctx, T) -> Type = T ; Type = fail ).

% sweep I=Lo..Hi (chi = I/1000), collapse runs of equal type into maximal [lo,hi] bands.
emit_bands(C, Obs, BaseEps, Supp, Ctx) :-
    Lo = -500, Hi = 1600,
    ChiLo is Lo/1000.0,
    synth_type(C, BaseEps, Supp, Ctx, ChiLo, T0),
    sweep_bands(C, Obs, BaseEps, Supp, Ctx, Lo, Hi, Lo, T0).

sweep_bands(C, Obs, BaseEps, Supp, Ctx, RunStart, Hi, I, CurT) :-
    ( I >= Hi
    ->  % close final band at Hi
        LoChi is RunStart/1000.0, HiChi is Hi/1000.0,
        format("BAND ~w ~w ~4f ~4f ~w~n", [C, Obs, LoChi, HiChi, CurT])
    ;   I1 is I + 1,
        Chi1 is I1/1000.0,
        synth_type(C, BaseEps, Supp, Ctx, Chi1, T1),
        ( T1 == CurT
        ->  sweep_bands(C, Obs, BaseEps, Supp, Ctx, RunStart, Hi, I1, CurT)
        ;   LoChi is RunStart/1000.0, HiChi is I/1000.0,
            format("BAND ~w ~w ~4f ~4f ~w~n", [C, Obs, LoChi, HiChi, CurT]),
            sweep_bands(C, Obs, BaseEps, Supp, Ctx, I1, Hi, I1, T1)
        )
    ).

% Phase-0 boundary ownership: type with chi placed EXACTLY on each config gate value.
gate_val(0.00). gate_val(0.35). gate_val(0.45). gate_val(0.66). gate_val(0.90).
emit_gateown(C, Obs, BaseEps, Supp, Ctx) :-
    forall(gate_val(G),
        ( synth_type(C, BaseEps, Supp, Ctx, G, T),
          format("GATEOWN ~w ~w ~4f ~w~n", [C, Obs, G, T]) )).

probe_constraint(C) :-
    num_or_unknown(drl_core:base_extractiveness(C, BaseEps0), BaseEps0),
    num_or_unknown(drl_core:get_raw_suppression(C, Supp0), Supp0),
    format("BASE ~w ~w ~w~n", [C, BaseEps0, Supp0]),
    ( number(BaseEps0), number(Supp0)
    ->  forall(obs_ctx(Obs, Ctx),
            ( num_or_unknown(constraint_indexing:extractiveness_for_agent(C, Ctx, Chi0), Chi0),
              ( constraint_indexing:effective_immutability_for_context(Ctx, Imm) -> true ; Imm = none ),
              ( number(Chi0), drl_core:classify_from_metrics(C, BaseEps0, Chi0, Supp0, Ctx, MT0) -> true ; MT0 = unknown ),
              ( drl_core:dr_type(C, Ctx, FT0) -> true ; FT0 = unknown ),
              format("OBS ~w ~w ~w ~w ~w ~w~n", [C, Obs, Chi0, Imm, MT0, FT0]),
              emit_bands(C, Obs, BaseEps0, Supp0, Ctx),
              emit_gateown(C, Obs, BaseEps0, Supp0, Ctx)
            ))
    ;   % absent eps/supp: cascade fail-closes (OQ-44). Emit observer rows w/o bands (no map).
        forall(obs_ctx(Obs, Ctx),
            ( num_or_unknown(constraint_indexing:extractiveness_for_agent(C, Ctx, Chi0), Chi0),
              ( constraint_indexing:effective_immutability_for_context(Ctx, Imm) -> true ; Imm = none ),
              format("OBS ~w ~w ~w ~w unknown unknown~n", [C, Obs, Chi0, Imm]) ))
    ).

run_probe :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    format("LOADCOUNT ~w~n", [N]),
    forall(corpus_loader:corpus_constraint(C), probe_constraint(C)).

:- ( catch(run_probe, E, (format("PROBE_ERROR ~w~n", [E]), true)) -> true
   ; format("PROBE_ERROR run_probe failed~n", []) ), halt.
"""


def run_corpus(corpus_path, preamble=""):
    with tempfile.NamedTemporaryFile("w", suffix=".pl", dir=PROLOG_DIR, delete=False) as tf:
        tf.write(_GOAL_TMPL.format(corpus=corpus_path, preamble=preamble))
        goal_path = tf.name
    try:
        proc = subprocess.run(
            ["swipl", "-q", goal_path],
            cwd=PROLOG_DIR, capture_output=True, text=True, timeout=5400,
        )
    finally:
        Path(goal_path).unlink(missing_ok=True)
    return proc


def corpus_hash(ids):
    h = hashlib.sha256()
    for i in sorted(ids):
        h.update(i.encode()); h.update(b"\n")
    return h.hexdigest()


def process_corpus(name, corpus_path, preamble="", expected=None):
    """Run one swipl probe, hard-stop on load-count mismatch, write the four TSVs under `name`.
    Returns True on success. `expected` defaults to the on-disk *.pl count for corpus_path."""
    if expected is None:
        expected = len(list((PROLOG_DIR / corpus_path).glob("*.pl")))
    print(f"\n{'='*78}\nCORPUS: {name}  (path {corpus_path}, on-disk *.pl = {expected})\n{'='*78}")
    proc = run_corpus(corpus_path, preamble=preamble)

    loadcount = None
    base_lines, obs_lines, band_lines, gate_lines = [], [], [], []
    probe_error = None
    for line in proc.stdout.splitlines():
        t = line.split()
        if not t:
            continue
        tag = t[0]
        if tag == "LOADCOUNT":
            loadcount = int(t[1])
        elif tag == "BASE":
            base_lines.append(line)
        elif tag == "OBS":
            obs_lines.append(line)
        elif tag == "BAND":
            band_lines.append(line)
        elif tag == "GATEOWN":
            gate_lines.append(line)
        elif tag == "PROBE_ERROR":
            probe_error = line

    if probe_error:
        print(f"  !! {probe_error}")
        print("  stderr tail:\n" + "\n".join("    " + l for l in proc.stderr.splitlines()[-15:]))
        return False
    if loadcount is None:
        print("  !! no LOADCOUNT emitted (probe never reached run_probe)")
        print("  stderr tail:\n" + "\n".join("    " + l for l in proc.stderr.splitlines()[-20:]))
        return False

    print(f"LOADCOUNT {loadcount}")
    if loadcount != expected:
        print(f"  !! HARD STOP: LOADCOUNT {loadcount} != on-disk {expected} "
              f"(overlay likely ignored — must use asserta). Aborting corpus; no TSV written.")
        print("  stderr tail:\n" + "\n".join("    " + l for l in proc.stderr.splitlines()[-15:]))
        return False

    ids = [l.split()[1] for l in base_lines]
    if len(set(ids)) != expected:
        print(f"  !! HARD STOP: {len(set(ids))} distinct BASE ids != {expected}. Aborting.")
        return False

    chash = corpus_hash(ids)
    print(f"BASE rows {len(base_lines)}  OBS {len(obs_lines)}  BAND {len(band_lines)}  "
          f"GATEOWN {len(gate_lines)}   corpus_hash {chash}")

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    (OUT_DIR / f"base_{name}.tsv").write_text(
        "id\tbaseeps\tsupp\n" + "\n".join("\t".join(l.split()[1:]) for l in base_lines) + "\n")
    (OUT_DIR / f"obs_{name}.tsv").write_text(
        "id\tobs\tchi\timmut\tmtype\tftype\n" + "\n".join(
            "\t".join(l.split()[1:]) for l in obs_lines) + "\n")
    (OUT_DIR / f"bands_{name}.tsv").write_text(
        "id\tobs\tlo\thi\ttype\n" + "\n".join(
            "\t".join(l.split()[1:]) for l in band_lines) + "\n")
    (OUT_DIR / f"gateown_{name}.tsv").write_text(
        "id\tobs\tgateval\ttype\n" + "\n".join(
            "\t".join(l.split()[1:]) for l in gate_lines) + "\n")
    (OUT_DIR / f"corpus_hash_{name}.txt").write_text(chash + "\n")
    print(f"  wrote base/obs/bands/gateown_{name}.tsv + corpus_hash_{name}.txt")
    return True


def main(argv):
    names = argv[1:] if len(argv) > 1 else DEFAULT_RUN
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    overall_ok = True
    for name in names:
        corpus_path = CORPUS_SPECS.get(name, name)
        if not process_corpus(name, corpus_path):
            overall_ok = False
    print(f"\n{'='*78}\nPROBE STATUS:", "PASS" if overall_ok else "FAIL")
    return 0 if overall_ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
