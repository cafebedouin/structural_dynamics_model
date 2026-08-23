#!/usr/bin/env python3
"""OQ-120 Phase 0 Step C — the decision gate, computed from the frozen
PREREGISTRATION.md definitions.

Reads raw/tm_*.json (one per swept leg) + raw/strata.json, emits gate_readout.md
and gate_readout.json. Computes N_eps / N_reach / N_rail exactly as frozen, and
reports the categories the prereg requires kept separate: `unknown`-endpoint
transitions, and MT-invariant / FT-only transitions.
"""
import json, sys
from collections import Counter, defaultdict
from pathlib import Path

D = Path(__file__).resolve().parent
RAW = D / "raw"

EPS_GATES = {"snare_epsilon_floor", "rope_epsilon_ceiling"}
PAIR_TYPES = {"rope", "snare", "tangled_rope", "naturalized"}
# kernel_v1 is pre-reset, a different generation regime: swept and reported
# separately, NEVER counted toward the >=2-distinct-models corroboration.
ARCHIVE_LEGS = {"archives/datasets/kernel_v1"}


def rail_cell(x):
    """The 0.01 rail cell [k/100, (k+1)/100] containing x. Returns k."""
    k = int(round(x * 100 - 1e-7))
    if abs(x * 100 - k) < 1e-6:      # x IS a rail point
        return k
    return int(x * 100 + 1e-9)


def load():
    legs = {}
    for f in sorted(RAW.glob("tm_*.json")):
        j = json.loads(f.read_text())
        legs[j["corpus_path"]] = j
    return legs


def ft_timeline(trs):
    """Piecewise-constant FT for one (story, seat), from its ordered transitions."""
    trs = sorted(trs, key=lambda t: t["eps_lo"])
    def at(x):
        if not trs:
            return None
        if x <= trs[0]["eps_lo"] + 1e-9:
            return trs[0]["ft_lo"]
        for t in trs:
            if x <= t["eps_lo"] + 1e-9:
                return t["ft_lo"]
        return trs[-1]["ft_hi"]
    return at


def main():
    legs = load()
    strata = json.loads((RAW / "strata.json").read_text())

    # ---- per-stratum authored-ε range over the claimed-rope-or-snare population
    stratum_eps = defaultdict(list)
    story_meta = {}                      # (leg, id) -> dict
    for leg, j in legs.items():
        smap = strata.get(leg, {})
        for s in j["stories"]:
            st = smap.get(s["id"], {}).get("stratum", "unprovenanced")
            story_meta[(leg, s["id"])] = dict(s, stratum=st, leg=leg)
            if s["claimed_type"] in ("rope", "snare") and isinstance(s["epsilon"], float):
                stratum_eps[st].append(s["epsilon"])
    stratum_range = {k: (min(v), max(v), len(v)) for k, v in stratum_eps.items()}

    # ---- classify every transition
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
                eps_attr = bool(gates & EPS_GATES)
                pair = {t["ft_lo"], t["ft_hi"]}
                has_unknown = "unknown" in pair or "err" in pair
                pair_ok = bool(pair & PAIR_TYPES)
                mid = (t["eps_lo"] + t["eps_hi"]) / 2.0
                k = rail_cell(mid)
                lo_r, hi_r = k / 100.0, (k + 1) / 100.0
                rail_visible = at(lo_r) != at(hi_r)
                in_range = bool(rng) and (rng[0] - 1e-9 <= mid <= rng[1] + 1e-9)
                rows.append({
                    "leg": leg, "id": sid, "seat": seat, "stratum": st,
                    "eps_lo": t["eps_lo"], "eps_hi": t["eps_hi"], "eps_mid": mid,
                    "mt_lo": t["mt_lo"], "ft_lo": t["ft_lo"],
                    "mt_hi": t["mt_hi"], "ft_hi": t["ft_hi"],
                    "gates": sorted(gates),
                    "eps_attributed": eps_attr,
                    "snare_eps_attributed": "snare_epsilon_floor" in gates,
                    "rope_eps_attributed": "rope_epsilon_ceiling" in gates,
                    # DECISIVE vs MOVED. A gate BIT flipping across the bracket is not
                    # the same proposition as that gate DECIDING the outcome. The bit
                    # snare_epsilon_floor flips f->t at EVERY transition located at
                    # 0.46, by construction. It is decisive only if the type its clause
                    # produces is actually an endpoint. Read on MT: the gate lives in
                    # classify_from_metrics/6, which produces MT, not FT.
                    "snare_eps_decisive": ("snare_epsilon_floor" in gates
                                           and "snare" in (t["mt_lo"], t["mt_hi"])),
                    "rope_eps_decisive": ("rope_epsilon_ceiling" in gates
                                          and "rope" in (t["mt_lo"], t["mt_hi"])),
                    "pair_ok": pair_ok, "has_unknown": has_unknown,
                    "mt_invariant": t["mt_lo"] == t["mt_hi"] and t["ft_lo"] != t["ft_hi"],
                    "rail_visible": rail_visible,
                    "in_stratum_range": in_range,
                    "chi_lo": t["lo"]["chi"], "chi_hi": t["hi"]["chi"],
                    "coal_lo": t["lo"]["coalition_fired"],
                    "coal_hi": t["hi"]["coalition_fired"],
                    "claimed_type": meta.get("claimed_type"),
                    "authored_eps": meta.get("epsilon"),
                })

    live = [r for r in rows if r["leg"] not in ARCHIVE_LEGS]
    arch = [r for r in rows if r["leg"] in ARCHIVE_LEGS]

    def gate_numbers(rs):
        q = [r for r in rs if r["eps_attributed"] and r["pair_ok"]]
        q_clean = [r for r in q if not r["has_unknown"]]
        q_unk = [r for r in q if r["has_unknown"]]
        cells = {(r["leg"], r["id"], r["seat"]) for r in q_clean}
        reach = [r for r in q_clean if r["in_stratum_range"]]
        reach_cells = {(r["leg"], r["id"], r["seat"]) for r in reach}
        rail = [r for r in reach if r["rail_visible"]]
        rail_cells = {(r["leg"], r["id"], r["seat"]) for r in rail}
        return dict(q=q, q_clean=q_clean, q_unk=q_unk,
                    N_eps=len(cells), N_reach=len(reach_cells), N_rail=len(rail_cells),
                    cells=cells, reach=reach, rail=rail, rail_cells=rail_cells)

    G = gate_numbers(live)
    A = gate_numbers(arch)

    snare_rail = [r for r in G["rail"] if r["snare_eps_attributed"]]
    snare_rail_dec = [r for r in G["rail"] if r["snare_eps_decisive"]]
    rope_rail_dec = [r for r in G["rail"] if r["rope_eps_decisive"]]
    snare_dec_pair = [r for r in snare_rail_dec
                      if {r["ft_lo"], r["ft_hi"]} == {"rope", "snare"}]
    snare_dec_mt_pair = Counter(f"{r['mt_lo']} -> {r['mt_hi']}" for r in snare_rail_dec)
    snare_any = [r for r in live if r["snare_eps_attributed"]]
    snare_pair_exact = [r for r in snare_rail
                        if {r["ft_lo"], r["ft_hi"]} == {"rope", "snare"}]
    models_rail = {r["stratum"].split("|")[0] for r in G["rail"]}
    models_snare = {r["stratum"].split("|")[0] for r in snare_rail}
    legs_rail = {r["leg"] for r in G["rail"]}

    # ---- the DECISIVE predicate is itself an introduced instrument, so it owes
    # its own two-sided control (build_discipline: "An introduced instrument is
    # itself a claim"). Positive: C1's planted transition shape
    # (mt tangled_rope -> snare, gates carrying snare_epsilon_floor) MUST score
    # decisive. Negative: the naturally-arising moved-but-not-decisive rows.
    def _decisive(mt_lo, mt_hi, gates):
        return "snare_epsilon_floor" in gates and "snare" in (mt_lo, mt_hi)
    ctl_pos = _decisive("tangled_rope", "snare", {"snare_epsilon_floor"})
    ctl_neg = _decisive("tangled_rope", "naturalized", {"snare_epsilon_floor"})
    assert ctl_pos and not ctl_neg, "decisive predicate is not two-sided"

    snare_dec_all = [r for r in live if r["snare_eps_decisive"]]
    rope_dec_all = [r for r in live if r["rope_eps_decisive"]]
    snare_moved_all = [r for r in live if r["snare_eps_attributed"]]

    out = {
        "decisive_predicate_control": {
            "positive_C1_shape_scores_decisive": ctl_pos,
            "negative_moved_not_decisive_shape_declines": not ctl_neg,
            "note": ("positive arm is C1's PLANTED shape (authored-decoy grade); "
                     "the naturally-arising arm is the decline count below"),
        },
        "snare_eps_MOVED_all_live_transitions": len(snare_moved_all),
        "snare_eps_DECISIVE_all_live_transitions": len(snare_dec_all),
        "rope_eps_DECISIVE_all_live_transitions": len(rope_dec_all),
        "N_eps": G["N_eps"], "N_reach": G["N_reach"], "N_rail": G["N_rail"],
        "n_qualifying_transitions": len(G["q_clean"]),
        "n_unknown_endpoint_transitions": len(G["q_unk"]),
        "n_transitions_total_live": len(live),
        "n_snare_eps_attributed_any": len(snare_any),
        "n_snare_eps_attributed_in_N_rail": len(snare_rail),
        "n_snare_eps_pair_exactly_rope_snare": len(snare_pair_exact),
        "n_snare_eps_DECISIVE_in_N_rail": len(snare_rail_dec),
        "n_rope_eps_DECISIVE_in_N_rail": len(rope_rail_dec),
        "n_snare_eps_DECISIVE_pair_exactly_rope_snare": len(snare_dec_pair),
        "snare_eps_DECISIVE_mt_pairs": dict(snare_dec_mt_pair),
        "models_with_snare_eps_DECISIVE": sorted(
            {r["stratum"].split("|")[0] for r in snare_rail_dec}),
        "models_contributing_to_N_rail": sorted(models_rail),
        "legs_contributing_to_N_rail": sorted(legs_rail),
        "models_with_snare_eps_transition": sorted(models_snare),
        "archive_kernel_v1": {"N_eps": A["N_eps"], "N_reach": A["N_reach"],
                              "N_rail": A["N_rail"],
                              "n_snare_eps": len([r for r in arch if r["snare_eps_attributed"]])},
        "stratum_authored_eps_range_rope_or_snare": {
            k: {"min": v[0], "max": v[1], "n": v[2]} for k, v in sorted(stratum_range.items())},
    }
    (D / "gate_readout.json").write_text(json.dumps(
        {"summary": out, "rows": rows}, indent=1), encoding="utf-8")

    # ---------------- report ----------------
    L = []
    A_ = L.append
    A_(f"N_eps  = {out['N_eps']}")
    A_(f"N_reach= {out['N_reach']}")
    A_(f"N_rail = {out['N_rail']}")
    A_("")
    A_(f"qualifying transitions (clean)      {len(G['q_clean'])}")
    A_(f"qualifying with unknown endpoint    {len(G['q_unk'])}  (counted, reported separately)")
    A_(f"all located transitions, live legs  {len(live)}")
    A_(f"snare_epsilon_floor-attributed, any {len(snare_any)}")
    A_(f"  ... inside N_rail                 {len(snare_rail)}")
    A_(f"  ... with FT pair exactly {{rope,snare}} {len(snare_pair_exact)}")
    A_("")
    A_("MOVED vs DECISIVE (the bit flips at every 0.46 crossing by construction;")
    A_("  decisive = the gate's own output type is actually an MT endpoint):")
    A_(f"  snare_epsilon_floor MOVED    in N_rail  {len(snare_rail)}")
    A_(f"  snare_epsilon_floor DECISIVE in N_rail  {len(snare_rail_dec)}")
    A_(f"     ... FT pair exactly {{rope,snare}}     {len(snare_dec_pair)}")
    A_(f"     ... MT pairs                         {dict(snare_dec_mt_pair)}")
    A_(f"     ... models                           {sorted({r[chr(34)+chr(34)] if False else r['stratum'].split('|')[0] for r in snare_rail_dec})}")
    A_(f"  rope_epsilon_ceiling DECISIVE in N_rail {len(rope_rail_dec)}")
    A_("")
    A_("unfiltered over EVERY located live transition (no N_eps/N_reach/N_rail filter):")
    A_(f"  snare_epsilon_floor MOVED     {len(snare_moved_all)}")
    A_(f"  snare_epsilon_floor DECISIVE  {len(snare_dec_all)}")
    A_(f"  rope_epsilon_ceiling DECISIVE {len(rope_dec_all)}")
    A_(f"  decisive-predicate control: plant-fires={ctl_pos} decoy-declines={not ctl_neg}")
    A_(f"models contributing to N_rail       {sorted(models_rail)}")
    A_(f"legs contributing to N_rail         {len(legs_rail)}")
    A_("")
    A_("deciding-gate census over qualifying transitions:")
    gc = Counter(g for r in G["q_clean"] for g in r["gates"])
    for g, n in gc.most_common():
        A_(f"  {g:<34} {n}")
    A_("")
    A_("FT pairs among N_rail transitions:")
    for p, n in Counter(f"{r['ft_lo']} -> {r['ft_hi']}" for r in G["rail"]).most_common(20):
        A_(f"  {p:<40} {n}")
    A_("")
    A_("MT-invariant / FT-only among qualifying: "
       f"{sum(1 for r in G['q_clean'] if r['mt_invariant'])}")
    A_("")
    A_("per-leg N_rail:")
    for leg, n in sorted(Counter(r["leg"] for r in G["rail"]).items()):
        A_(f"  {leg:<38} {n}")
    print("\n".join(L))
    (D / "gate_numbers.txt").write_text("\n".join(L), encoding="utf-8")


if __name__ == "__main__":
    main()
