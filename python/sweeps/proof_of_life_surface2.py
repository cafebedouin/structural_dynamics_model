#!/usr/bin/env python3
"""
Surface 2 proof-of-life: confirm excess_extraction/2 is readable and moves
under boltzmann_floor_* param overlay.

Observable: boltzmann_compliance:excess_extraction(C, ExcessEps)
Overlay:    config:param(boltzmann_floor_identity_coordination, _) retract/assertz
Constraint: civic_eugenic_reading
  - coordination_type: identity_coordination
  - floor path: clause 2 (coordination_type), NOT override, NOT default
  - boltzmann_floor_identity_coordination = 0.08
  - extractiveness (ε) = 0.68 from constraint_metric/3
  - hypothesis: baseline excess = max(0.0, 0.68 - 0.08) = 0.60

Reconciliation with prior session:
  The prior session stated "boltzmann_floor_override dead-ends at line 453."
  That is correct for Surface 1: product_site_export never calls excess_extraction
  or boltzmann_floor_for, so perturbing any boltzmann param produces zero change
  in the product-site re-export (the Surface-1 observable). The control break holds.
  On Surface 2, boltzmann_floor_for/2's output IS consumed by excess_extraction/2
  and 14+ callers in metric_drift_events.pl, drl_boltzmann_analysis.pl, etc. The surface
  is live for its own observable. Both claims are true; they operate at different
  surface granularities.

S2 coverage analog:
  If boltzmann_floor_for/2 takes the boltzmann_floor_override path (clause 1),
  perturbing boltzmann_floor_identity_coordination is shadowed (coverage=0) — the
  same blind-green trap as Surface 1's coverage field. This script confirms which
  path the chosen constraint actually takes before trusting the result.

Cache discipline:
  boltzmann_compliance.pl declares cached_classification/3 and cached_coupling/2.
  excess_extraction/2 does NOT write to these caches (reads constraint_metric/3
  and config:param directly). clear_classification_cache/0 is called as a
  precaution; the script confirms cache count = 0 after clearing.
"""

import json
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"

_PROLOG_GOAL = """\
:- use_module(library(aggregate)).
:- [stack].
:- corpus_loader:ensure_corpus_loaded.
:- use_module(boltzmann_compliance).

pol_run :-
    C = civic_eugenic_reading,

    % Read static epsilon
    config:param(extractiveness_metric_name, ExtName),
    narrative_ontology:constraint_metric(C, ExtName, Eps),

    % Diagnose boltzmann_floor_for/2 path (determines overlay validity)
    % Override path (clause 1): per-constraint boltzmann_floor_override fact
    % Coordination-type path (clause 2): coordination_type -> floor param
    % Default path (clause 3): boltzmann_floor_default
    ( catch(narrative_ontology:boltzmann_floor_override(C, _), _, fail) ->
        FloorPath = override,
        FloorParam = boltzmann_floor_override,
        CoordType = na
    ; catch((narrative_ontology:coordination_type(C, CoordType),
             atom_concat('boltzmann_floor_', CoordType, FloorParam),
             config:param(FloorParam, _)), _, fail) ->
        FloorPath = coordination_type
    ;
        FloorPath = default,
        FloorParam = boltzmann_floor_default,
        CoordType = na
    ),
    config:param(FloorParam, FloorBaseline),

    % Baseline read
    boltzmann_compliance:excess_extraction(C, E1),

    % Cache audit: count before and after clear.
    % excess_extraction/2 reads constraint_metric/3 and config:param directly --
    % it does not write to cached_classification/3 or cached_coupling/2.
    % clear_classification_cache/0 is called as a precaution; CacheAfter must be 0.
    aggregate_all(count, boltzmann_compliance:cached_classification(_, _, _), CacheBefore),
    boltzmann_compliance:clear_classification_cache,
    aggregate_all(count, boltzmann_compliance:cached_classification(_, _, _), CacheAfter),

    % Overlay: raise identity_coordination floor 0.08 -> 0.60
    % Valid only when FloorPath = coordination_type (confirmed above).
    % If FloorPath = override, perturbing this param is shadowed (coverage=0).
    FloorPerturbed = 0.60,
    retractall(config:param(boltzmann_floor_identity_coordination, _)),
    assertz(config:param(boltzmann_floor_identity_coordination, FloorPerturbed)),

    % Perturbed read
    boltzmann_compliance:excess_extraction(C, E2),

    Diff is E2 - E1,
    ( abs(Diff) > 1.0e-9 -> Moved = true ; Moved = false ),

    % Overlay validity: coordination_type path means overlay reached the floor.
    % If override path had fired, boltzmann_floor_identity_coordination would
    % be shadowed -- the Surface-1 coverage=0 lesson at Surface-2 granularity.
    ( FloorPath = coordination_type ->
        OverlayValid = true
    ;
        OverlayValid = false
    ),

    format("~`-t~60|~n", []),
    format("Surface 2 proof-of-life: excess_extraction/2~n", []),
    format("  constraint         : ~w~n", [C]),
    format("  floor_path         : ~w~n", [FloorPath]),
    format("  floor_param        : ~w~n", [FloorParam]),
    format("  coord_type         : ~w~n", [CoordType]),
    format("  floor_baseline     : ~6f~n", [FloorBaseline]),
    format("  floor_overlay      : ~6f~n", [FloorPerturbed]),
    format("  eps (static fact)  : ~6f~n", [Eps]),
    format("  [hypothesis] baseline_excess = max(0, ~6f - ~6f) = ~6f~n",
           [Eps, FloorBaseline, E1]),
    format("  baseline_excess (actual)    : ~6f~n", [E1]),
    format("  perturbed_excess (actual)   : ~6f~n", [E2]),
    format("  diff               : ~6f~n", [Diff]),
    format("  moved              : ~w~n", [Moved]),
    format("  overlay_valid      : ~w  (false = shadowed, coverage=0)~n", [OverlayValid]),
    format("  cache_before_clear : ~w~n", [CacheBefore]),
    format("  cache_after_clear  : ~w  (must be 0; nonzero = clear did not hold)~n", [CacheAfter]),
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
        print(f"\n[S2] swipl exited {r.returncode}", file=sys.stderr)
    return {"stdout": r.stdout, "returncode": r.returncode}


if __name__ == "__main__":
    run()
