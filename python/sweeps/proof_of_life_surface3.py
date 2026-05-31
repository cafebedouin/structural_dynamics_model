#!/usr/bin/env python3
"""
Surface 3 proof-of-life: confirm constraint_history/3 is readable and the
type-over-time vector moves when a measurement/5 fact is overlaid.

Observable: drl_composition:constraint_history(C, Context, Timeline)
  where Timeline = [state(T0, Type0), state(T2, Type2), state(T4, Type4)]
Overlay:    retract measurement(civi_be_t4, C, base_extractiveness, 4, 0.68)
            assertz measurement(civi_be_t4, C, base_extractiveness, 4, 0.95)
Constraint: civic_eugenic_reading
  measurement facts at T=0,2,4 for base_extractiveness and theater_ratio
  default_context: context(agent_power(analytical), time_horizon(civilizational),
                           exit_options(analytical), spatial_scope(global))

Pass condition: "moved" = state(4, Type_baseline) vs state(4, Type_perturbed) differ.
  No arithmetic threshold prediction — the actual types from the engine are the verdict.
  Not-moved with diagnostic (computed Chi at T=4, binding threshold) is also a valid
  scoping output: it identifies the correct overlay target for the full primitive build.

Surface 3 is independent of Surface 2 — each surface reports its own moved/not-moved.
"""

import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"

_PROLOG_GOAL = """\
:- [stack].
:- corpus_loader:ensure_corpus_loaded.
:- use_module(drl_composition).

% Helper: format a timeline as text
print_timeline([], _) :- nl.
print_timeline([state(T, Type)|Rest], Prefix) :-
    format("  ~w  state(~w, ~w)~n", [Prefix, T, Type]),
    print_timeline(Rest, Prefix).

% Diff two timelines, report which entries changed
diff_timelines([], [], []).
diff_timelines([state(T, Type1)|R1], [state(T, Type2)|R2], Diffs) :-
    diff_timelines(R1, R2, Rest),
    ( Type1 \= Type2 ->
        Diffs = [changed(T, Type1, Type2)|Rest]
    ;
        Diffs = Rest
    ).
diff_timelines(L1, L2, [length_mismatch(L1, L2)]) :-
    length(L1, N1), length(L2, N2), N1 \= N2.

pol_run :-
    C = civic_eugenic_reading,

    % Confirm measurement/5 facts exist for this constraint
    findall(m(ID,Metric,T,V),
            narrative_ontology:measurement(ID, C, Metric, T, V),
            AllMeasurements),
    length(AllMeasurements, MCount),

    % Baseline timeline (default_context: analytical perspective)
    constraint_indexing:default_context(Ctx),
    drl_composition:constraint_history(C, Ctx, Timeline1),

    % Extract T=4 type from baseline (actual engine output, no prediction)
    ( member(state(4, T4_baseline), Timeline1) ->
        true
    ;
        T4_baseline = not_found
    ),

    % Confirm the measurement fact we will retract actually exists
    ( narrative_ontology:measurement(civi_be_t4, C, base_extractiveness, 4, 0.68) ->
        FactExists = true
    ;
        FactExists = false
    ),

    % Overlay: retract T=4 base_extractiveness 0.68, assert 0.95
    OldVal = 0.68,
    NewVal = 0.95,
    ( FactExists = true ->
        retract(narrative_ontology:measurement(civi_be_t4, C, base_extractiveness, 4, OldVal)),
        assertz(narrative_ontology:measurement(civi_be_t4, C, base_extractiveness, 4, NewVal))
    ;
        true
    ),

    % Perturbed timeline
    drl_composition:constraint_history(C, Ctx, Timeline2),

    % Extract T=4 type from perturbed
    ( member(state(4, T4_perturbed), Timeline2) ->
        true
    ;
        T4_perturbed = not_found
    ),

    % Diff
    diff_timelines(Timeline1, Timeline2, Diffs),
    ( Diffs = [] -> Moved = false ; Moved = true ),

    % If not moved: diagnostic — compute Chi at T=4 with the perturbed ε
    ( Moved = false ->
        % Chi = BaseX * PowerMod * ScopeMod  (from classify_at_time implementation)
        BaseX = NewVal,
        Ctx = context(_, _, exit_options(ExitOpt), spatial_scope(Scope)),
        constraint_indexing:scope_modifier(Scope, ScopeMod),
        constraint_indexing:derive_directionality(C, Ctx, D),
        constraint_indexing:sigmoid_f(D, PowerMod),
        Chi is BaseX * PowerMod * ScopeMod,
        % Read relevant thresholds
        config:param(snare_epsilon_floor, SEF),
        config:param(snare_chi_floor, SCF),
        config:param(rope_chi_ceiling, RCC),
        format("  [diagnostic] baseline not disturbed despite ε=~6f~n", [NewVal]),
        format("  [diagnostic] computed Chi at T=4: ~6f~n", [Chi]),
        format("  [diagnostic] scope_modifier(~w) = ~6f~n", [Scope, ScopeMod]),
        format("  [diagnostic] sigmoid_f(D=~6f) = ~6f~n", [D, PowerMod]),
        format("  [diagnostic] snare_epsilon_floor = ~6f (ε ~w this)~n",
               [SEF, (BaseX >= SEF -> '≥' ; '<')]),
        format("  [diagnostic] snare_chi_floor    = ~6f (Chi ~w this)~n",
               [SCF, (Chi >= SCF -> '≥' ; '<')]),
        format("  [diagnostic] rope_chi_ceiling   = ~6f~n", [RCC]),
        DiagChi = Chi,
        DiagSEF = SEF,
        DiagSCF = SCF
    ;
        DiagChi = na, DiagSEF = na, DiagSCF = na,
        ExitOpt = na
    ),

    format("~`-t~60|~n", []),
    format("Surface 3 proof-of-life: constraint_history~n", []),
    format("  constraint         : ~w~n", [C]),
    format("  measurement_count  : ~w~n", [MCount]),
    format("  measurement_fact_exists (civi_be_t4, 0.68): ~w~n", [FactExists]),
    format("  overlay            : base_extractiveness T=4: ~6f -> ~6f~n", [OldVal, NewVal]),
    format("  baseline_timeline  :~n", []),
    print_timeline(Timeline1, "baseline"),
    format("  perturbed_timeline :~n", []),
    print_timeline(Timeline2, "perturbed"),
    format("  T=4 type baseline  : ~w~n", [T4_baseline]),
    format("  T=4 type perturbed : ~w~n", [T4_perturbed]),
    format("  diffs              : ~w~n", [Diffs]),
    format("  moved              : ~w~n", [Moved]),
    ( Moved = false ->
        format("  diag_chi_at_t4    : ~w~n", [DiagChi]),
        format("  diag_snare_eps_fl : ~w~n", [DiagSEF]),
        format("  diag_snare_chi_fl : ~w~n", [DiagSCF]),
        format("  (not-moved = valid scoping output: identifies binding threshold)~n", [])
    ;
        true
    ),
    format("~`-t~60|~n", []).

:- catch(pol_run, E, (format(user_error, "ERROR: ~w~n", [E]), halt(1))), halt.
:- halt(1).
"""


def run() -> dict:
    with tempfile.NamedTemporaryFile(
        suffix=".pl", dir=PROLOG_DIR, mode="w", delete=False
    ) as f:
        f.write(_PROLOG_GOAL)
        pl_path = f.name

    try:
        r = subprocess.run(
            ["swipl", "-g", f"consult('{pl_path}'), halt.", "-t", "halt(1)"],
            cwd=PROLOG_DIR,
            capture_output=True,
            text=True,
            timeout=120,
        )
    finally:
        Path(pl_path).unlink(missing_ok=True)

    print(r.stdout, end="")
    if r.stderr.strip():
        for line in r.stderr.splitlines():
            if not line.startswith("[") and "WARNING" not in line:
                print(f"  [stderr] {line}", file=sys.stderr)
    if r.returncode != 0:
        print(f"\n[S3] swipl exited {r.returncode}", file=sys.stderr)
    return {"stdout": r.stdout, "returncode": r.returncode}


if __name__ == "__main__":
    run()
