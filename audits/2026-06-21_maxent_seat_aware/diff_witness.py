#!/usr/bin/env python3
"""Full-corpus pre/post diff witness for the maxent seat-aware fix.
Classifies every seat's delta; runs the negative-half discriminator
(raw_maxent_probs must be byte-stable on non-routed seats)."""
import json, sys

BASE = json.load(open(sys.argv[1]))
POST = json.load(open(sys.argv[2]))

FCR_ROUTED = {
    "basic_law_interpretive_boundary__parliamentary_sovereignty_reading",
    "conceptual_framework_reading", "divine_legitimacy_substrate__folk_syncretistic_reading",
    "fictional_construct_reading", "jewish_sovereignty_palestine__cultural_zionist_reading",
    "lausanne_minority_protections__guarantor_reading", "llm_synthesis_capacity",
    "neutron_star_bombardment_reading", "press_reformation_causation__strategic_deployment"}
CON_ROUTED = {"equal_protection_kernel__colorblind_reading", "institutional_trust_erosion_c0",
              "shinbutsu_ontological_commitment__incoherence_reading"}
ROUTED = FCR_ROUTED | CON_ROUTED

def by_id(d): return {e["id"]: e for e in d["per_constraint"]}
B, P = by_id(BASE), by_id(POST)

def argmax(p): return max(p.items(), key=lambda kv: kv[1])[0] if p else None
def idxtop(e): return (e.get("maxent_indexed") or {}).get("top_type")
def vj(e): return (e.get("verdict_join") or {}).get("verdict")
def vjb(e): return (e.get("verdict_join") or {}).get("base_verdict")

# --- Negative-half discriminator: raw_maxent_probs stability on NON-routed seats ---
raw_moved_nonrouted = []
maxprobs_moved_nonrouted = []
top_moved_nonrouted = []
idxtop_moved_nonrouted = []
vj_moved_nonrouted = []
for cid in B:
    if cid in ROUTED or cid not in P:
        continue
    b, p = B[cid], P[cid]
    if (b.get("raw_maxent_probs") or {}) != (p.get("raw_maxent_probs") or {}):
        raw_moved_nonrouted.append(cid)
    if (b.get("maxent_probs") or {}) != (p.get("maxent_probs") or {}):
        maxprobs_moved_nonrouted.append(cid)
    if b.get("maxent_top_type") != p.get("maxent_top_type"):
        top_moved_nonrouted.append((cid, b.get("maxent_top_type"), p.get("maxent_top_type")))
    if idxtop(b) != idxtop(p):
        idxtop_moved_nonrouted.append((cid, idxtop(b), idxtop(p)))
    if vj(b) != vj(p):
        vj_moved_nonrouted.append((cid, vj(b), vj(p)))

# --- Routed seats: maxent_probs should now == raw (boost removed); classical top unchanged ---
print("=" * 70)
print("ROUTED SEATS (12) — expected: classical top unchanged; maxent_probs==raw; verdict_join unchanged")
print("=" * 70)
routed_classical_top_changed = []
routed_maxprobs_now_raw = []
routed_maxprobs_was_boosted = []
routed_idxtop_flips = []
routed_vj_changed = []
for cid in sorted(ROUTED):
    if cid not in P:
        print(f"  MISSING in post: {cid}"); continue
    b, p = B[cid], P[cid]
    bt, pt = b.get("maxent_top_type"), p.get("maxent_top_type")
    if bt != pt:
        routed_classical_top_changed.append((cid, bt, pt))
    p_max = p.get("maxent_probs") or {}
    p_raw = p.get("raw_maxent_probs") or {}
    b_max = b.get("maxent_probs") or {}
    b_raw = b.get("raw_maxent_probs") or {}
    if p_max == p_raw:
        routed_maxprobs_now_raw.append(cid)
    if b_max != b_raw:
        routed_maxprobs_was_boosted.append(cid)
    if idxtop(b) != idxtop(p):
        routed_idxtop_flips.append((cid, idxtop(b), idxtop(p)))
    if vj(b) != vj(p) or vjb(b) != vjb(p):
        routed_vj_changed.append((cid, (vj(b), vjb(b)), (vj(p), vjb(p))))
    print(f"  {cid}")
    print(f"     classical top: {bt} -> {pt} | maxent_probs==raw(post): {p_max==p_raw} | "
          f"was_boosted(base): {b_max!=b_raw}")
    print(f"     indexed top: {idxtop(b)} -> {idxtop(p)} | verdict_join: "
          f"{vj(b)}/{vjb(b)} -> {vj(p)}/{vjb(p)}")

print("\n" + "=" * 70)
print("PRE-REGISTRATION CHECKS")
print("=" * 70)
print(f"[movers] routed maxent_probs now == raw (boost removed): {len(routed_maxprobs_now_raw)}/12")
print(f"[movers] routed seats that WERE boosted in baseline (maxent_probs!=raw): {len(routed_maxprobs_was_boosted)}/12")
print(f"         -> {sorted(routed_maxprobs_was_boosted)}")
print(f"[movers] routed indexed-top flips: {routed_idxtop_flips}")
print(f"[non-movers] routed CLASSICAL top changed (expect 0): {routed_classical_top_changed}")
print(f"[non-movers] routed verdict_join changed (expect 0): {routed_vj_changed}")
print()
print(f"[NEG HALF] non-routed seats with raw_maxent_probs MOVED (MUST be 0 — else too-wide): {len(raw_moved_nonrouted)}")
print(f"           -> {raw_moved_nonrouted}")
print(f"[NEG HALF] non-routed maxent_probs moved (ensemble ripple allowed IF raw stable): {len(maxprobs_moved_nonrouted)}")
print(f"           -> {maxprobs_moved_nonrouted}")
print(f"[NEG HALF] non-routed classical top moved: {top_moved_nonrouted}")
print(f"[NEG HALF] non-routed indexed top moved: {idxtop_moved_nonrouted}")
print(f"[NEG HALF] non-routed verdict_join moved: {vj_moved_nonrouted}")

# verdict: overall pass/fail
ok = (not routed_classical_top_changed and not routed_vj_changed
      and not raw_moved_nonrouted and len(routed_maxprobs_now_raw) == 12)
print("\nWITNESS RESULT:", "PASS" if ok else "REVIEW")
