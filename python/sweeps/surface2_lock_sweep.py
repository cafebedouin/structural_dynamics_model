#!/usr/bin/env python3
"""
Surface-2 lock sweep: does perturbing the Boltzmann subsystem flip the final
dr_type of the signature-locked kernels?

This is the Surface-2 *primitive* (the proof-of-life graduated to an instrument).
It does NOT extend perturb.py (Surface 1, product_site_export round-trip). It runs
its observables directly against the engine in one swipl process — corpus loaded
ONCE — and overlays config params in-memory via retract/assertz + cache clear.

THREE LEVERS, swept INDEPENDENTLY (never bundled — bundling confounds attribution):

  1. boltzmann_floor_*  (+ boltzmann_floor_default)
       observable: boltzmann_compliance:excess_extraction(C, Excess)
       Phase-0/1 read-only probe showed this is the WRONG lever for the lock:
       it moves excess_extraction but NOT boltzmann_compliant, and the FNL /
       CI_rope override gates DO NOT consume excess_extraction
       (signature_detection.pl:927-930 — gating on it was removed). Swept here to
       CONFIRM THE NULL per-kernel (set-not-count: a non-uniform floor can't hide).

  2. boltzmann_coupling_threshold  (base param, config.pl:332 = 0.25)
       gate: boltzmann_compliant(C, compliant|non_compliant) via
             complexity_adjusted_threshold = boltzmann_coupling_threshold + offset,
             compared to cross_index_coupling (the coupling SCORE).
       This is the lever the read-only probe witnessed flipping the lock
       (abolition_reading: tangled_rope -> snare at threshold 1.5). Swept across a
       RANGE both directions to find each kernel's BOUNDARY ("flips at X"), not a bit.

  3. coordination_type_offset  (complexity_offset_* + complexity_offset_default)
       gate: same complexity_adjusted_threshold, additively. Swept across a range;
       a flat result is reported as "inert in swept range" (range-bound), NOT
       "structurally inert" — the algebra (additive into the same gate) is noted.

Coverage (Surface-2 sense): a lever has coverage>0 on a reading iff the observable
it controls actually MOVES somewhere in the swept range (floor->excess_extraction;
coupling/offset->boltzmann_compliant). If the observable never moves, coverage=0 and
we report the lever as inert/shadowed, never "type stable" (the blind-green trap).

Target set: derived in-Prolog, NOT inherited from any doc. All readings with a
cs_kernel_id whose constraint_signature is a Boltzmann-gated override
(false_natural_law | coupling_invariant_rope | false_ci_rope). Each row is flagged
LOAD-BEARING when metric_based_type_indexed != final dr_type (the override actually
changes the final type) vs redundant (final == metric) — the latter is the built-in
contrast / over-inclusion control.

Context = constraint_indexing:default_context (analytical), matching dr_type/2 and
the Phase-1 census.

Usage:
    python3 python/sweeps/surface2_lock_sweep.py [--json-out PATH] [--only C1 C2 ...]
"""

import argparse
import json
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"

# Sweep grids (sized from Phase-1: coupScore in {0, 0.75, 0.875, 1.0}; offsets 0.0-0.15;
# base coupling_threshold = 0.25). The coupScore=1.0 cluster flips when base+offset>=1.0
# (base ~0.85-1.0); the coupScore=0 cluster (CI_rope/FCR-scaffold, already compliant)
# only breaks when base+offset<0 (base negative). Both directions required.
FLOOR_VALUES = [0.0, 0.5, 0.99]
COUPLING_VALUES = [-0.5, -0.2, -0.1, -0.05, 0.0, 0.25, 0.5, 0.67, 0.75,
                   0.83, 0.9, 0.95, 1.0, 1.1, 1.5]
OFFSET_VALUES = [-0.5, -0.3, -0.25, -0.1, 0.0, 0.04, 0.25, 0.5, 0.75, 0.85, 1.0]

FLOOR_PARAMS = [
    "boltzmann_floor_information_standard",
    "boltzmann_floor_attachment_coordination",
    "boltzmann_floor_resource_allocation",
    "boltzmann_floor_identity_coordination",
    "boltzmann_floor_enforcement_mechanism",
    "boltzmann_floor_global_infrastructure",
    "boltzmann_floor_default",
]
OFFSET_PARAMS = [
    "complexity_offset_information_standard",
    "complexity_offset_attachment_coordination",
    "complexity_offset_resource_allocation",
    "complexity_offset_identity_coordination",
    "complexity_offset_enforcement_mechanism",
    "complexity_offset_global_infrastructure",
    "complexity_offset_default",
]
GATED_SIGNATURES = ["false_natural_law", "coupling_invariant_rope", "false_ci_rope"]


def _pl_floatlist(vals):
    return "[" + ",".join(f"{v:.6f}" for v in vals) + "]"


def _pl_atomlist(names):
    return "[" + ",".join(names) + "]"


def _build_goal(only=None):
    only_clause = ""
    if only:
        only_clause = (
            "    , ( OnlyList == [] -> true ; memberchk(C, OnlyList) )\n"
        )
    only_list = _pl_atomlist(only) if only else "[]"
    return f"""\
:- use_module(library(aggregate)).
:- [stack].
:- corpus_loader:ensure_corpus_loaded.
:- use_module(boltzmann_compliance).
:- use_module(signature_detection).

floor_params({_pl_atomlist(FLOOR_PARAMS)}).
offset_params({_pl_atomlist(OFFSET_PARAMS)}).
floor_values({_pl_floatlist(FLOOR_VALUES)}).
coupling_values({_pl_floatlist(COUPLING_VALUES)}).
offset_values({_pl_floatlist(OFFSET_VALUES)}).
only_list({only_list}).

% restore a param family to a captured original alist [Name-Val,...]
restore(Pairs) :-
    forall(member(N-V, Pairs),
        ( retractall(config:param(N,_)), assertz(config:param(N,V)) )).

capture(Names, Pairs) :-
    findall(N-V, (member(N, Names), config:param(N,V)), Pairs).

set_all(Names, V) :-
    forall(member(N, Names),
        ( retractall(config:param(N,_)), assertz(config:param(N,V)) )).

set_one(N, V) :-
    retractall(config:param(N,_)), assertz(config:param(N,V)).

compliant_atom(C, A) :-
    ( catch(boltzmann_compliance:boltzmann_compliant(C, R),_,fail)
    -> ( R = compliant(_) -> A = compliant
       ; R = non_compliant(_,_) -> A = non_compliant
       ; A = R )
    ; A = err ).

final_type(C, T) :- ( catch(drl_core:dr_type(C, T0),_,fail) -> T = T0 ; T = err ).
metric_type(C, Ctx, T) :- ( catch(drl_core:metric_based_type_indexed(C, Ctx, T0),_,fail) -> T = T0 ; T = err ).
excess(C, E) :- ( catch(boltzmann_compliance:excess_extraction(C, E0),_,fail) -> E = E0 ; E = none ).

floor_path(C, Path) :-
    ( catch(narrative_ontology:boltzmann_floor_override(C,_),_,fail) -> Path = override
    ; catch(narrative_ontology:coordination_type(C,_),_,fail) -> Path = coordination_type
    ; Path = default ).

run :-
    constraint_indexing:default_context(Ctx),
    only_list(OnlyList),
    floor_params(FloorPs), offset_params(OffsetPs),
    capture(FloorPs, FloorOrig),
    capture(OffsetPs, OffsetOrig),
    config:param(boltzmann_coupling_threshold, BaseOrig),
    % target set: kernel-linked readings with a Boltzmann-gated override signature
    findall(K-C-Sig,
        ( narrative_ontology:cs_kernel_id(C,K),
          catch(signature_detection:constraint_signature(C,Sig),_,fail),
          memberchk(Sig, {_pl_atomlist(GATED_SIGNATURES)})
{only_clause}        ),
        Trips0),
    sort(Trips0, Trips),
    length(Trips, NT),
    format("META target_readings ~w~n", [NT]),
    format("META coupling_base_orig ~6f~n", [BaseOrig]),
    forall(member(K-C-Sig, Trips),
        sweep_reading(Ctx, K, C, Sig, FloorPs, FloorOrig, OffsetPs, OffsetOrig, BaseOrig)).

sweep_reading(Ctx, K, C, Sig, FloorPs, FloorOrig, OffsetPs, OffsetOrig, BaseOrig) :-
    % ---- baseline (all params at original) ----
    restore(FloorOrig), restore(OffsetOrig), set_one(boltzmann_coupling_threshold, BaseOrig),
    boltzmann_compliance:clear_classification_cache,
    metric_type(C, Ctx, MT), final_type(C, FT),
    ( MT == FT -> LB = redundant ; LB = load_bearing ),
    compliant_atom(C, BC0), excess(C, Ex0), floor_path(C, FP),
    ( Ex0 == none -> Ex0f = 0.0 ; Ex0f = Ex0 ),
    ( catch(boltzmann_compliance:cross_index_coupling(C, CplScore),_,fail) -> true ; CplScore = err ),
    format("READING ~w ~w ~w ~w ~w ~w ~w ~6f ~w~n",
        [K, C, Sig, MT, FT, LB, BC0, Ex0f, FP]),
    format("BASE ~w cplscore ~w compliant ~w excess ~w final ~w~n", [C, CplScore, BC0, Ex0, FT]),

    % ---- LEVER 1: floor (observable excess; expect null on type) ----
    restore(OffsetOrig), set_one(boltzmann_coupling_threshold, BaseOrig),
    floor_values(FVs),
    forall(member(FV, FVs),
        ( set_all(FloorPs, FV),
          boltzmann_compliance:clear_classification_cache,
          excess(C, ExF), final_type(C, FTF),
          format("FLOOR ~w ~6f excess ~w final ~w~n", [C, FV, ExF, FTF]) )),
    restore(FloorOrig),

    % ---- LEVER 2: coupling_threshold base (range -> boundary) ----
    restore(FloorOrig), restore(OffsetOrig),
    coupling_values(CVs),
    forall(member(CV, CVs),
        ( set_one(boltzmann_coupling_threshold, CV),
          boltzmann_compliance:clear_classification_cache,
          compliant_atom(C, BCc), final_type(C, FTc),
          format("COUP ~w ~6f compliant ~w final ~w~n", [C, CV, BCc, FTc]) )),
    set_one(boltzmann_coupling_threshold, BaseOrig),

    % ---- LEVER 3: coordination_type_offset (range; additive into same gate) ----
    restore(FloorOrig), set_one(boltzmann_coupling_threshold, BaseOrig),
    offset_values(OVs),
    forall(member(OV, OVs),
        ( set_all(OffsetPs, OV),
          boltzmann_compliance:clear_classification_cache,
          compliant_atom(C, BCo), final_type(C, FTo),
          format("OFFSET ~w ~6f compliant ~w final ~w~n", [C, OV, BCo, FTo]) )),
    restore(OffsetOrig),
    boltzmann_compliance:clear_classification_cache.

:- catch(run, E, (format(user_error, "ERR: ~w~n", [E]), halt(1))), halt.
:- halt(1).
"""


def run(only=None, json_out=None):
    goal = _build_goal(only=only)
    with tempfile.NamedTemporaryFile(suffix=".pl", dir=PROLOG_DIR, mode="w", delete=False) as f:
        f.write(goal)
        pl_path = f.name
    try:
        r = subprocess.run(
            ["swipl", "-g", f"consult('{pl_path}'), halt.", "-t", "halt(1)"],
            cwd=PROLOG_DIR, capture_output=True, text=True, timeout=1200,
        )
    finally:
        Path(pl_path).unlink(missing_ok=True)

    if r.returncode != 0 and "META target_readings" not in r.stdout:
        sys.stderr.write(r.stderr[-2000:])
        raise SystemExit(f"swipl exited {r.returncode} with no data")

    parsed = _parse(r.stdout)
    if json_out:
        Path(json_out).write_text(json.dumps(parsed, indent=2))
    return parsed, r.stdout, r.stderr


def _parse(stdout):
    readings = {}
    meta = {}
    order = []
    for line in stdout.splitlines():
        t = line.split()
        if not t:
            continue
        if t[0] == "META":
            meta[t[1]] = t[2]
        elif t[0] == "READING":
            # READING K C Sig MT FT LB BC0 Excess FloorPath
            _, k, c, sig, mt, ft, lb, bc0, ex, fp = t[:10]
            readings[c] = {
                "kernel": k, "constraint": c, "signature": sig,
                "metric_type": mt, "final_type": ft, "load_bearing": lb,
                "baseline_compliant": bc0, "baseline_excess": float(ex),
                "floor_path": fp,
                "floor": [], "coupling": [], "offset": [],
            }
            order.append(c)
        elif t[0] == "FLOOR":
            _, c, v, _kw, ex, _kw2, ft = t
            readings[c]["floor"].append({"value": float(v), "excess": ex, "final": ft})
        elif t[0] == "COUP":
            _, c, v, _kw, comp, _kw2, ft = t
            readings[c]["coupling"].append({"value": float(v), "compliant": comp, "final": ft})
        elif t[0] == "OFFSET":
            _, c, v, _kw, comp, _kw2, ft = t
            readings[c]["offset"].append({"value": float(v), "compliant": comp, "final": ft})
    return {"meta": meta, "order": order, "readings": readings}


def _excess_to_float(s):
    try:
        return float(s)
    except ValueError:
        return None


def _crossing_nearest(seq, base_final, baseline):
    """Return the adjacent-value interval (lo, hi) where `final` transitions away
    from base_final, choosing the crossing whose interval is closest to `baseline`
    (the param's resting value). Returns None if no transition. This reports the
    real boundary regardless of sweep direction (upward for non-compliant readings,
    downward/near-0 for the already-compliant CI_rope cluster)."""
    crossings = []
    for a, b in zip(seq, seq[1:]):
        if (a["final"] == base_final) != (b["final"] == base_final):
            crossings.append((a["value"], b["value"]))
    if not crossings:
        return None
    # pick crossing whose midpoint is nearest baseline
    crossings.sort(key=lambda lh: abs((lh[0] + lh[1]) / 2 - baseline))
    return list(crossings[0])


def analyze(parsed):
    """Per-reading lever verdicts with coverage and boundaries."""
    out = []
    for c in parsed["order"]:
        r = parsed["readings"][c]
        base_final = r["final_type"]

        # --- floor lever ---
        fl = r["floor"]
        excess_vals = [_excess_to_float(x["excess"]) for x in fl]
        excess_moved = len({v for v in excess_vals if v is not None}) > 1
        floor_type_flip = any(x["final"] != base_final for x in fl)
        floor_cov = excess_moved
        if r["floor_path"] == "override":
            floor_cov = False  # shadowed: type/default floor params don't reach excess

        # --- coupling lever ---
        cp = sorted(r["coupling"], key=lambda x: x["value"])
        compliant_set = {x["compliant"] for x in cp}
        coup_gate_moved = len(compliant_set) > 1
        coup_finals = {x["final"] for x in cp}
        coup_type_flip = any(x["final"] != base_final for x in cp)
        coup_boundary = _crossing_nearest(cp, base_final, baseline=0.25)
        coup_cov = coup_gate_moved

        # --- offset lever ---
        of = sorted(r["offset"], key=lambda x: x["value"])
        off_compliant_set = {x["compliant"] for x in of}
        off_gate_moved = len(off_compliant_set) > 1
        off_type_flip = any(x["final"] != base_final for x in of)
        off_boundary = _crossing_nearest(of, base_final, baseline=0.0)
        off_cov = off_gate_moved

        out.append({
            **{k: r[k] for k in ("kernel", "constraint", "signature", "metric_type",
                                 "final_type", "load_bearing", "baseline_excess",
                                 "baseline_compliant", "floor_path")},
            "floor_coverage": floor_cov,
            "floor_excess_moved": excess_moved,
            "floor_type_flip": floor_type_flip,
            "coupling_coverage": coup_cov,
            "coupling_gate_moved": coup_gate_moved,
            "coupling_type_flip": coup_type_flip,
            "coupling_boundary": coup_boundary,
            "coupling_finals": sorted(coup_finals),
            "offset_coverage": off_cov,
            "offset_gate_moved": off_gate_moved,
            "offset_type_flip": off_type_flip,
            "offset_boundary": off_boundary,
        })
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--json-out", help="write full result JSON")
    ap.add_argument("--only", nargs="*", help="restrict to these constraint ids")
    ap.add_argument("--raw", action="store_true", help="dump raw swipl stdout")
    a = ap.parse_args()

    parsed, stdout, stderr = run(only=a.only, json_out=a.json_out)
    if a.raw:
        print(stdout)
        return
    rows = analyze(parsed)
    meta = parsed["meta"]
    print(f"target_readings={meta.get('target_readings')} "
          f"coupling_base_orig={meta.get('coupling_base_orig')}\n")
    hdr = (f"{'kernel':28} {'reading':34} {'sig':18} {'LB':12} "
           f"{'floor:exM/fl/cov':16} {'coup:boundary/fl/cov':26} {'offset:boundary/fl/cov':26} "
           f"{'coup_finals':16}")
    print(hdr)
    print("-" * len(hdr))
    def _bd(iv):
        return f"({iv[0]:+.2f},{iv[1]:+.2f}]" if iv else "none"
    for r in rows:
        fl = f"{str(r['floor_excess_moved'])[0]}/{str(r['floor_type_flip'])[0]}/{str(r['floor_coverage'])[0]}"
        cp = f"{_bd(r['coupling_boundary'])}/{str(r['coupling_type_flip'])[0]}/{str(r['coupling_coverage'])[0]}"
        op = f"{_bd(r['offset_boundary'])}/{str(r['offset_type_flip'])[0]}/{str(r['offset_coverage'])[0]}"
        finals = ">".join(r['coupling_finals'])
        print(f"{r['kernel']:28.28} {r['constraint']:34.34} {r['signature']:18.18} "
              f"{r['load_bearing']:12} {fl:16} {cp:26} {op:26} {finals:16.16}")
    if a.json_out:
        print(f"\nFull JSON: {a.json_out}")


if __name__ == "__main__":
    main()
