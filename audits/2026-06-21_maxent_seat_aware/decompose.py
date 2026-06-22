#!/usr/bin/env python3
"""Decompose maxent/verdict fields for the OQ-138 maxent-seat-aware audit.
Usage: decompose.py <pipeline_output.json> <out.json>
Reads routed-seat lists from this file (pinned from baseline_routed_seats.txt probe).
"""
import json, sys

FCR_ROUTED = [
    "basic_law_interpretive_boundary__parliamentary_sovereignty_reading",
    "conceptual_framework_reading",
    "divine_legitimacy_substrate__folk_syncretistic_reading",
    "fictional_construct_reading",
    "jewish_sovereignty_palestine__cultural_zionist_reading",
    "lausanne_minority_protections__guarantor_reading",
    "llm_synthesis_capacity",
    "neutron_star_bombardment_reading",
    "press_reformation_causation__strategic_deployment",
]
CONSTRUCTED_ROUTED = [
    "equal_protection_kernel__colorblind_reading",
    "institutional_trust_erosion_c0",
    "shinbutsu_ontological_commitment__incoherence_reading",
]
ROUTED = set(FCR_ROUTED) | set(CONSTRUCTED_ROUTED)

def argmax(probs):
    if not probs:
        return None
    # probs is dict type->p
    return max(probs.items(), key=lambda kv: kv[1])[0]

def vj(e):
    v = e.get("verdict_join") or {}
    return {
        "verdict": v.get("verdict"),
        "base_verdict": v.get("base_verdict"),
    }

def slice_seat(e):
    rmp = e.get("raw_maxent_probs") or {}
    return {
        "id": e["id"],
        "signature": e.get("signature"),
        "maxent_top_type": e.get("maxent_top_type"),
        "maxent_indexed": e.get("maxent_indexed"),
        "raw_maxent_argmax": argmax(rmp),
        "raw_maxent_probs": rmp,
        "verdict_join": vj(e),
    }

def main():
    d = json.load(open(sys.argv[1]))
    pc = d["per_constraint"]
    by_id = {e["id"]: e for e in pc}
    out = {"manifest": d["manifest"], "fcr_routed": {}, "constructed_routed": {},
           "non_routed_fcr": {}, "non_routed_constructed": {}}
    for cid in FCR_ROUTED:
        if cid in by_id:
            out["fcr_routed"][cid] = slice_seat(by_id[cid])
    for cid in CONSTRUCTED_ROUTED:
        if cid in by_id:
            out["constructed_routed"][cid] = slice_seat(by_id[cid])
    # non-routed control: every seat whose signature is false_ci_rope / constructed_high but NOT routed
    for e in pc:
        if e.get("signature") == "false_ci_rope" and e["id"] not in ROUTED:
            out["non_routed_fcr"][e["id"]] = slice_seat(e)
        elif e.get("signature") == "constructed_high_extraction" and e["id"] not in ROUTED:
            out["non_routed_constructed"][e["id"]] = slice_seat(e)
    json.dump(out, open(sys.argv[2], "w"), indent=2, sort_keys=True)
    print(f"fcr_routed={len(out['fcr_routed'])} constructed_routed={len(out['constructed_routed'])} "
          f"non_routed_fcr={len(out['non_routed_fcr'])} non_routed_constructed={len(out['non_routed_constructed'])}")
    # quick echo of routed maxent_top
    print("--- routed maxent_top_type / raw_argmax / verdict_join.verdict ---")
    for grp in ("fcr_routed", "constructed_routed"):
        for cid, s in sorted(out[grp].items()):
            print(f"  [{grp}] {cid}: top={s['maxent_top_type']} idx={s['maxent_indexed']} "
                  f"rawarg={s['raw_maxent_argmax']} vj={s['verdict_join']['verdict']} "
                  f"base={s['verdict_join']['base_verdict']}")

if __name__ == "__main__":
    main()
