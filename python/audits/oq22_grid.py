#!/usr/bin/env python3
"""
OQ-22 Phase-2 — the (observer x immutability) type grid that WITNESSES each subset (a)/(b) member.

The Phase-1 band screen is a SCREEN; this grid is the arbiter (plan §Phase 2). It separates the two
sources of cross-observer type variation onto ORTHOGONAL axes:

  observer axis  -> χ = ε·f(d)·σ varies (Hub 1)
  immutability axis -> effective_immutability perception varies (Hub 2)

MECHANISM (no retract — effective_immutability/3 is a STATIC procedure; with_overlay can't pin it).
Inside classify_from_metrics/6 the passed Context is used ONLY for the immutability checks (χ is
passed in, already computed; snare_immutability_check reads standard_context independently). So we
compute each observer's NATURAL χ once, then call classify_from_metrics with a context that keeps the
observer's agent_power + spatial_scope (hence the natural χ stays the right Hub-1 value) but pins
(time_horizon, exit_options) to force a chosen immutability:
    pin mountain  <- (immediate, trapped)     [effective_immutability(immediate,trapped,mountain)]
    pin rope      <- (immediate, mobile)       [effective_immutability(immediate,mobile,rope)]
The NATURAL cell uses the observer's real (T,E) and must reproduce the Phase-1 metric type — a grid
self-consistency check.

Per (constraint, observer) emits:
    GRID <id> <obs> <chi> <nat_immut> <nat_type> <pin_mountain_type> <pin_rope_type>

WITNESS RULES (analysis, oq22_grid_analyze inline below):
  (a) Hub-2-SOURCED variation: natural type varies across observers, AND under a CONSTANT-immutability
      pin (the pin_rope column AND the pin_mountain column) the type is constant across observers —
      i.e. holding immutability fixed, the observer's χ variation alone cannot move the type, so the
      real variation was Hub 2's.  (For a non-starved constraint χ has range, so a pinned column still
      varies -> NOT (a) -> the screen's contrast set.)
  (b) immutability-DRIVEN at fixed observer: pin_mountain_type != pin_rope_type at some observer
      (type tracks immutability).
  negative control: pin_mountain_type == pin_rope_type at an observer = invariant across the
      immutability axis = Hub-1/χ-sourced (not a Hub-2 decision).
  persists-under-pin: natural type varies but pinned columns ALSO vary -> Hub-1-sourced (NOT a halt;
      the variation was χ via d/σ).

GRID SELF-CONTROL (plan / operator finding 3): the a-priori (a) members are the mountain<->scaffold
flippers (mountain clause ignores χ, drl_core.pl:330-336, so their flip CANNOT be Hub-1). If the grid
does not collapse THEM under the immutability pin, the grid is broken before we trust it on marginals.

Read-only. Usage:  python3 python/audits/oq22_grid.py [corpus ...]   (default: testsets)
"""

import csv
import subprocess
import sys
import tempfile
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"
OUT_DIR = ROOT / "audits" / "2026-06-28_oq22_hub_starvation"
OBSERVERS = ["powerless", "moderate", "institutional", "analytical"]
CORPUS_SPECS = {
    "testsets": "testsets", "testsets_haiku": "testsets_haiku",
    "testsets_flash": "testsets_flash", "kernel_v1": "archives/datasets/kernel_v1",
}

_GOAL_TMPL = r"""
:- asserta(config:param(corpus_path, '{corpus}')).
:- [stack].
:- corpus_loader:load_all_testsets.

obs_ctx(powerless,     context(agent_power(powerless),     time_horizon(biographical),  exit_options(trapped),    spatial_scope(local))).
obs_ctx(moderate,      context(agent_power(moderate),      time_horizon(biographical),  exit_options(mobile),     spatial_scope(national))).
obs_ctx(institutional, context(agent_power(institutional), time_horizon(generational),  exit_options(arbitrage),  spatial_scope(national))).
obs_ctx(analytical,    context(agent_power(analytical),    time_horizon(civilizational),exit_options(analytical), spatial_scope(global))).

% pin (T,E) to force a chosen immutability, keeping the observer's power + scope (hence natural χ).
pinned_ctx(context(agent_power(P),_,_,spatial_scope(S)), mountain, context(agent_power(P), time_horizon(immediate), exit_options(trapped), spatial_scope(S))).
pinned_ctx(context(agent_power(P),_,_,spatial_scope(S)), rope,     context(agent_power(P), time_horizon(immediate), exit_options(mobile),  spatial_scope(S))).

classify_fixed_chi(C, BaseEps, Supp, Ctx, Chi, Type) :-
    ( drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Ctx, T) -> Type = T ; Type = fail ).

grid_constraint(C) :-
    ( drl_core:base_extractiveness(C, BaseEps), drl_core:get_raw_suppression(C, Supp),
      number(BaseEps), number(Supp)
    ->  forall(obs_ctx(Obs, NatCtx),
            ( ( constraint_indexing:extractiveness_for_agent(C, NatCtx, Chi) -> true ; Chi = unknown ),
              ( constraint_indexing:effective_immutability_for_context(NatCtx, NatImm) -> true ; NatImm = none ),
              ( number(Chi)
              ->  classify_fixed_chi(C, BaseEps, Supp, NatCtx, Chi, NatT),
                  pinned_ctx(NatCtx, mountain, MCtx), classify_fixed_chi(C, BaseEps, Supp, MCtx, Chi, MT),
                  pinned_ctx(NatCtx, rope, RCtx),     classify_fixed_chi(C, BaseEps, Supp, RCtx, Chi, RT)
              ;   NatT = unknown, MT = unknown, RT = unknown ),
              format("GRID ~w ~w ~w ~w ~w ~w ~w~n", [C, Obs, Chi, NatImm, NatT, MT, RT])
            ))
    ;   true ).

run_grid :- forall(corpus_loader:corpus_constraint(C), grid_constraint(C)).
:- ( catch(run_grid, E, (format("GRID_ERROR ~w~n",[E]),true)) -> true ; format("GRID_ERROR failed~n",[]) ), halt.
"""


def run(corpus, corpus_path):
    with tempfile.NamedTemporaryFile("w", suffix=".pl", dir=PROLOG_DIR, delete=False) as tf:
        tf.write(_GOAL_TMPL.format(corpus=corpus_path))
        gp = tf.name
    try:
        proc = subprocess.run(["swipl", "-q", gp], cwd=PROLOG_DIR, capture_output=True,
                              text=True, timeout=5400)
    finally:
        Path(gp).unlink(missing_ok=True)
    rows = []
    err = None
    for line in proc.stdout.splitlines():
        t = line.split()
        if not t:
            continue
        if t[0] == "GRID" and len(t) == 8:
            rows.append(dict(id=t[1], obs=t[2], chi=t[3], nat_immut=t[4],
                             nat_type=t[5], pin_mountain=t[6], pin_rope=t[7]))
        elif t[0] == "GRID_ERROR":
            err = line
    if err:
        print(f"  !! {err}\n" + "\n".join("    " + l for l in proc.stderr.splitlines()[-12:]))
    return rows


def analyze(corpus, rows):
    # group by id, ordered observers
    by_id = defaultdict(dict)
    for r in rows:
        by_id[r["id"]][r["obs"]] = r
    # cross-check vs Phase-1 census metric type vector (diagonal must reproduce it)
    census = {}
    cpath = OUT_DIR / f"census_{corpus}.tsv"
    if cpath.exists():
        for r in csv.DictReader(cpath.open(), delimiter="\t"):
            census[r["id"]] = {"starved": r["starved"], "subset_a": r["subset_a"],
                               "subset_b": r["subset_b"], "mtype_vec": r["mtype_vec"].split("|")}

    diag_mismatch = []
    a_witnessed, b_witnessed, persist, neg_control = [], [], [], []
    out_rows = []
    for cid, cells in by_id.items():
        if not all(o in cells for o in OBSERVERS):
            continue
        nat = [cells[o]["nat_type"] for o in OBSERVERS]
        colm = [cells[o]["pin_mountain"] for o in OBSERVERS]
        colr = [cells[o]["pin_rope"] for o in OBSERVERS]
        # diagonal vs census metric vector
        if cid in census and census[cid]["mtype_vec"] != ["NA"]:
            if nat != census[cid]["mtype_vec"]:
                diag_mismatch.append((cid, nat, census[cid]["mtype_vec"]))
        nat_varies = len(set(nat)) > 1
        colm_flat = len(set(colm)) == 1
        colr_flat = len(set(colr)) == 1
        # (a): natural variation vanishes under BOTH constant-immutability pins
        is_a = nat_varies and colm_flat and colr_flat
        # persists: natural varies but a pinned column still varies -> Hub-1-sourced
        is_persist = nat_varies and not (colm_flat and colr_flat)
        # (b): immutability changes the type at some fixed observer
        is_b = any(cells[o]["pin_mountain"] != cells[o]["pin_rope"] for o in OBSERVERS)
        # negative control: invariant across immutability axis at some observer
        is_neg = any(cells[o]["pin_mountain"] == cells[o]["pin_rope"] for o in OBSERVERS)
        if is_a:
            a_witnessed.append(cid)
        if is_persist:
            persist.append(cid)
        if is_b:
            b_witnessed.append(cid)
        if is_neg:
            neg_control.append(cid)
        out_rows.append(dict(id=cid, nat="|".join(nat), pin_mountain="|".join(colm),
                             pin_rope="|".join(colr), a=is_a, b=is_b, persist=is_persist,
                             neg=is_neg,
                             screen_a=census.get(cid, {}).get("subset_a"),
                             screen_starved=census.get(cid, {}).get("starved")))
    # write grid tsv
    with (OUT_DIR / f"grid_{corpus}.tsv").open("w") as f:
        f.write("id\tnat\tpin_mountain\tpin_rope\tgrid_a\tgrid_b\tpersist\tneg_control\t"
                "screen_subset_a\tscreen_starved\n")
        for r in out_rows:
            f.write("\t".join(str(r[k]) for k in
                    ["id", "nat", "pin_mountain", "pin_rope", "a", "b", "persist", "neg",
                     "screen_a", "screen_starved"]) + "\n")
    return dict(corpus=corpus, n=len(out_rows), diag_mismatch=diag_mismatch,
                a=a_witnessed, b=b_witnessed, persist=persist, neg=neg_control,
                out_rows=out_rows, census=census)


def main(argv):
    corpora = argv[1:] if len(argv) > 1 else ["testsets"]
    for corpus in corpora:
        cp = CORPUS_SPECS.get(corpus, corpus)
        print(f"\n{'='*78}\nGRID {corpus} (path {cp})\n{'='*78}")
        rows = run(corpus, cp)
        if not rows:
            print("  !! no grid rows"); continue
        s = analyze(corpus, rows)
        print(f"  constraints with full grid: {s['n']}")
        n_mm = len(s["diag_mismatch"])
        print(f"  diagonal vs Phase-1 census metric vector: "
              f"{'CONSISTENT' if n_mm == 0 else str(n_mm) + ' MISMATCHES'}")
        for m in s["diag_mismatch"][:8]:
            print(f"    MISMATCH {m[0]}: grid {m[1]} vs census {m[2]}")
        # grid self-control: the screen's subset_a members must be grid-(a) (or persist, explicitly)
        screen_a = [r["id"] for r in s["out_rows"] if str(r["screen_a"]) == "True"]
        confirmed = [cid for cid in screen_a if cid in set(s["a"])]
        persisted = [cid for cid in screen_a if cid in set(s["persist"])]
        print(f"  GRID-(a) Hub-2-sourced (vanish under pin): {len(s['a'])}")
        print(f"  GRID-(b) immutability-driven at fixed obs:  {len(s['b'])}")
        print(f"  persists-under-pin (Hub-1-sourced):         {len(s['persist'])}")
        print(f"  negative-control cells (invariant/immut):   {len(s['neg'])}")
        print(f"  --- screen subset_a reconciliation ({len(screen_a)} screened) ---")
        print(f"    confirmed grid-(a): {len(confirmed)}   reclassified persist/Hub-1: {len(persisted)}")
        for cid in screen_a:
            r = next(rr for rr in s["out_rows"] if rr["id"] == cid)
            tag = "CONFIRM-a" if r["a"] else ("PERSIST->Hub1" if r["persist"] else "OTHER")
            print(f"    [{tag}] {cid}\n        nat={r['nat']}  pinM={r['pin_mountain']}  pinR={r['pin_rope']}")
        print(f"  wrote grid_{corpus}.tsv")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
