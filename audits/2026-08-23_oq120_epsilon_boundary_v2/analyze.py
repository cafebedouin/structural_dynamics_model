#!/usr/bin/env python3
"""OQ-120 Phase 0 Step C — v2 gate.

v2 changes vs v1, each forced by a defect v1 surfaced:
  * "attributes to" == DECISIVE (operator ruling 2026-08-23). MOVED kept and
    reported so a cold reader can re-derive under either.
  * the pooled `N_rail >= 10` floor is RETIRED as vacuous (v1 observed 9191).
    Replaced by, per stratum: N_rail >= 10 AND N_rail/(stories*4seats) >= 0.5%,
    in >=2 distinct MODEL strata.
  * stratum = (model, regime, prompt_hash, schema_hash) — legs are backfilled.
  * G1 subtypes exhaustive by construction: G1a underpowered / G1b uncorroborated
    / G1c pair-falsified.
"""
import json, sys
from collections import Counter, defaultdict
from pathlib import Path

D = Path(__file__).resolve().parent
RAW = D / "raw"
EPS_GATES = {"snare_epsilon_floor", "rope_epsilon_ceiling"}
PAIR_TYPES = {"rope", "snare", "tangled_rope", "naturalized"}
ARCHIVE_LEGS = {"archives/datasets/kernel_v1"}
MIN_COUNT, MIN_RATE, MIN_MODELS = 10, 0.005, 2


def rail_cell(x):
    k = int(round(x * 100 - 1e-7))
    return k if abs(x * 100 - k) < 1e-6 else int(x * 100 + 1e-9)


def ft_timeline(trs):
    trs = sorted(trs, key=lambda t: t["eps_lo"])
    def at(x):
        if not trs: return None
        if x <= trs[0]["eps_lo"] + 1e-9: return trs[0]["ft_lo"]
        for t in trs:
            if x <= t["eps_lo"] + 1e-9: return t["ft_lo"]
        return trs[-1]["ft_hi"]
    return at


def main():
    legs = {}
    for f in sorted(RAW.glob("tm_*.json")):
        j = json.loads(f.read_text()); legs[j["corpus_path"]] = j
    strata = json.loads((RAW / "strata.json").read_text())

    stratum_eps, stratum_stories, story_meta = defaultdict(list), Counter(), {}
    for leg, j in legs.items():
        smap = strata.get(leg, {})
        for s in j["stories"]:
            st = smap.get(s["id"], {}).get("stratum", "unprovenanced")
            story_meta[(leg, s["id"])] = dict(s, stratum=st, leg=leg)
            stratum_stories[st] += 1
            if s["claimed_type"] in ("rope", "snare") and isinstance(s["epsilon"], float):
                stratum_eps[st].append(s["epsilon"])
    stratum_range = {k: (min(v), max(v), len(v)) for k, v in stratum_eps.items()}

    rows = []
    for leg, j in legs.items():
        by_cell = defaultdict(list)
        for t in j["transitions"]:
            by_cell[(t["id"], t["seat"])].append(t)
        for (sid, seat), trs in by_cell.items():
            at = ft_timeline(trs)
            meta = story_meta.get((leg, sid), {})
            st = meta.get("stratum", "unprovenanced")
            rng = stratum_range.get(st)
            for t in trs:
                gates = set(t["deciding_gates"])
                pair = {t["ft_lo"], t["ft_hi"]}
                mid = (t["eps_lo"] + t["eps_hi"]) / 2.0
                k = rail_cell(mid)
                snare_dec = "snare_epsilon_floor" in gates and "snare" in (t["mt_lo"], t["mt_hi"])
                rope_dec = "rope_epsilon_ceiling" in gates and "rope" in (t["mt_lo"], t["mt_hi"])
                rows.append({
                    "leg": leg, "id": sid, "seat": seat, "stratum": st,
                    "model": st.split("|")[0],
                    "eps_lo": t["eps_lo"], "eps_hi": t["eps_hi"], "eps_mid": mid,
                    "mt_lo": t["mt_lo"], "ft_lo": t["ft_lo"],
                    "mt_hi": t["mt_hi"], "ft_hi": t["ft_hi"], "gates": sorted(gates),
                    "eps_moved": bool(gates & EPS_GATES),
                    "eps_decisive": snare_dec or rope_dec,
                    "snare_eps_moved": "snare_epsilon_floor" in gates,
                    "snare_eps_decisive": snare_dec, "rope_eps_decisive": rope_dec,
                    "pair_ok": bool(pair & PAIR_TYPES),
                    "has_unknown": "unknown" in pair or "err" in pair,
                    "mt_invariant": t["mt_lo"] == t["mt_hi"] and t["ft_lo"] != t["ft_hi"],
                    "rail_visible": at(k / 100.0) != at((k + 1) / 100.0),
                    "in_stratum_range": bool(rng) and (rng[0]-1e-9 <= mid <= rng[1]+1e-9),
                })

    live = [r for r in rows if r["leg"] not in ARCHIVE_LEGS]
    arch = [r for r in rows if r["leg"] in ARCHIVE_LEGS]

    # --- DECISIVE-scored gate (the ruled reading)
    q = [r for r in live if r["eps_decisive"] and r["pair_ok"]]
    q_clean = [r for r in q if not r["has_unknown"]]
    q_unk = [r for r in q if r["has_unknown"]]
    reach = [r for r in q_clean if r["in_stratum_range"]]
    rail = [r for r in reach if r["rail_visible"]]
    cells = lambda rs: {(r["leg"], r["id"], r["seat"]) for r in rs}

    # --- the RE-SET per-stratum floor
    per_stratum = {}
    rail_by_st = Counter()
    for r in rail:
        rail_by_st[r["stratum"]] += 1
    for st, nst in sorted(stratum_stories.items()):
        nrail = rail_by_st.get(st, 0)
        denom = nst * 4
        rate = nrail / denom if denom else 0.0
        per_stratum[st] = {"stories": nst, "seat_cells": denom, "N_rail": nrail,
                           "rate": rate,
                           "passes": nrail >= MIN_COUNT and rate >= MIN_RATE}
    passing = {st for st, v in per_stratum.items() if v["passes"]}
    passing_models = {st.split("|")[0] for st in passing}
    floor_met = len(passing_models) >= MIN_MODELS

    snare_dec = [r for r in live if r["snare_eps_decisive"]]
    snare_dec_models = {r["model"] for r in snare_dec}
    pair_exact = [r for r in snare_dec if {r["ft_lo"], r["ft_hi"]} == {"rope", "snare"}]

    # --- branch, tested in the frozen order
    if not floor_met:
        branch = "G1a"; why = f"floor met in only {len(passing_models)} model strata (<{MIN_MODELS})"
    elif len(snare_dec) == 0:
        branch = "G0"; why = "floor met and ZERO snare_epsilon_floor-DECISIVE transitions"
    elif len(snare_dec_models) < MIN_MODELS:
        branch = "G1b"; why = (f"floor met, {len(snare_dec)} snare_epsilon_floor-DECISIVE "
                               f"transition(s) but from {len(snare_dec_models)} model stratum "
                               f"({sorted(snare_dec_models)}) — fails >=2 distinct MODELS")
    elif len(pair_exact) == 0:
        branch = "G1c"; why = "floor + models met, but NO observed FT pair is exactly {rope, snare}"
    else:
        branch = "G2"; why = f"{len(pair_exact)} transition(s) with FT pair exactly {{rope, snare}}"

    L = []; A = L.append
    A(f"BRANCH: {branch}")
    A(f"  because: {why}")
    A("")
    A("POOLED MEMO LINE (satisfies nothing on its own — the per-stratum table below is primary):")
    A(f"  N_eps {len(cells(q_clean))}  N_reach {len(cells(reach))}  N_rail {len(cells(rail))}")
    A(f"  qualifying transitions {len(q_clean)} (+{len(q_unk)} unknown-endpoint, separate)")
    A(f"  all located live transitions {len(live)}")
    A(f"  MT-invariant / FT-only among qualifying {sum(1 for r in q_clean if r['mt_invariant'])}")
    A("")
    A("MOVED vs DECISIVE (both reported; DECISIVE is the ruled reading):")
    A(f"  snare_epsilon_floor  MOVED {sum(1 for r in live if r['snare_eps_moved'])}   DECISIVE {len(snare_dec)}")
    A(f"  rope_epsilon_ceiling DECISIVE {sum(1 for r in live if r['rope_eps_decisive'])}")
    A(f"  snare DECISIVE models: {sorted(snare_dec_models)}")
    A(f"  FT pair exactly {{rope,snare}} among snare-DECISIVE: {len(pair_exact)}")
    A("")
    A(f"RE-SET FLOOR — per stratum, N_rail>={MIN_COUNT} AND rate>={MIN_RATE:.1%}, in >={MIN_MODELS} MODEL strata")
    A(f"  strata passing: {len(passing)}   distinct models passing: {len(passing_models)} {sorted(passing_models)}")
    A(f"  FLOOR MET: {floor_met}")
    A("")
    A(f"  {'stratum':<62}{'stories':>8}{'N_rail':>8}{'rate':>8}  pass")
    for st, v in sorted(per_stratum.items(), key=lambda kv: -kv[1]["N_rail"]):
        A(f"  {st[:62]:<62}{v['stories']:>8}{v['N_rail']:>8}{v['rate']:>7.2%}  {'YES' if v['passes'] else 'no'}")
    A("")
    A("FT pairs among N_rail (decisive-scored):")
    for p, n in Counter(f"{r['ft_lo']} -> {r['ft_hi']}" for r in rail).most_common(12):
        A(f"  {p:<40}{n}")
    A("")
    A(f"kernel_v1 (archive, separate, never corroboration): transitions {len(arch)}, "
      f"snare DECISIVE {sum(1 for r in arch if r['snare_eps_decisive'])}, "
      f"rope DECISIVE {sum(1 for r in arch if r['rope_eps_decisive'])}")
    out = "\n".join(L)
    print(out)
    (D / "gate_numbers.txt").write_text(out, encoding="utf-8")
    (D / "gate_readout.json").write_text(json.dumps(
        {"branch": branch, "why": why, "floor_met": floor_met,
         "per_stratum": per_stratum, "rows": rows}, indent=1), encoding="utf-8")


if __name__ == "__main__":
    main()
