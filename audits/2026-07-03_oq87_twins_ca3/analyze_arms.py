#!/usr/bin/env python3
"""OQ-87 twins characterization — Phase-2 analysis over raw arm ROWs (PLAN.md 2026-07-03).

Computes ONLY the pre-registered metrics, in the pre-registered order:
  per-arm tables -> C2 check -> C5 exclusion BEFORE new kernel_v1 diverge-A ->
  C7 join-rate BEFORE any cross-twin agreement -> core agreement, substantial-rate
  delta, conditioned direction-conjunct residual.
Reads: arm_*.rows (this dir) + banked ca3_rows.txt (2026-06-07 audit). Writes: stdout only.
"""
import re
import sys
from collections import Counter
from pathlib import Path

HERE = Path(__file__).parent
BANKED = HERE.parent / "2026-06-07_stakeholder_layer_migration" / "ca3_rows.txt"

GAP_RE = re.compile(r"gap\(([a-z_]+),([a-z_]+),(true|false)\)")
DEATH_SET = {"axiom_foreclosure", "husk", "extinction", "repudiation"}
SMALL_DELTA = 0.10          # pinned in PLAN.md
OBS_POOL_FLOOR = 300        # pinned (per twin, /960)
CORE_N_FLOOR = 20           # pinned (per twin)


def parse_rows(path):
    rows = {}
    meta = {}
    for line in path.read_text().splitlines():
        if line.startswith("C4 "):
            meta["loaded"] = int(line.split("=")[1].split()[0])
        elif line.startswith("POOL "):
            m = re.match(r"POOL kernel_bearing=(\d+) no_uid=(\d+)", line)
            meta["pool"], meta["no_uid"] = int(m.group(1)), int(m.group(2))
        elif line.startswith("ROW|"):
            _, kernel, cid, obs, com, axfc, terminals, gaps = line.split("|")
            fired = axfc != "axfc=[]"
            terms = re.findall(r"[a-z_]+", terminals.split("=", 1)[1])
            gap_list = GAP_RE.findall(gaps)
            rows[cid] = {
                "kernel": kernel, "obs": obs, "com": com, "fired": fired,
                "terminals": [t for t in terms if t in DEATH_SET],
                "gaps": gap_list,
            }
    return meta, rows


def nonminor_unack(r):
    return any(m != "minor" and a == "false" for _, m, a in r["gaps"])


def ao_nonminor_unack(r):
    return any(d == "axiom_overriding" and m != "minor" and a == "false"
               for d, m, a in r["gaps"])


def arm_report(name, meta, rows, expect_loaded):
    print(f"\n===== ARM {name} (loaded={meta['loaded']}, expect {expect_loaded}; "
          f"pool={meta['pool']}, no_uid={meta['no_uid']}) =====")
    if meta["loaded"] != expect_loaded:
        print(f"  !! C4 FAIL: loaded {meta['loaded']} != {expect_loaded}")
    obs_c = Counter(r["obs"] for r in rows.values())
    print(f"  observer buckets: {dict(obs_c)}")
    two = Counter((r["obs"], r["com"]) for r in rows.values()
                  if r["obs"] in ("coherent", "incoherent"))
    print("  2x2 (obs x com):")
    for o in ("coherent", "incoherent"):
        for c in ("dead", "live"):
            print(f"    {o:10s} {c:4s}: {two.get((o, c), 0)}")
    print(f"    undetermined (excluded from 2x2): {obs_c.get('undetermined', 0)}; "
          f"err/fail: {sum(v for k, v in obs_c.items() if k not in ('coherent', 'incoherent', 'undetermined'))}")
    core = [cid for cid, r in rows.items() if r["fired"]]
    print(f"  fired core (cs_axiom_foreclosed fires): {len(core)}")
    dirs = Counter(d for r in rows.values() for d, _, _ in r["gaps"])
    mags = Counter(m for r in rows.values() for _, m, _ in r["gaps"])
    print(f"  gap direction marginal: {dict(dirs.most_common())}")
    print(f"  gap magnitude marginal: {dict(mags.most_common())}")
    n = len(rows)
    for label, pred in (("substantial", lambda r: any(m == "substantial" for _, m, _ in r["gaps"])),
                        ("severe", lambda r: any(m == "severe" for _, m, _ in r["gaps"])),
                        ("non-minor", lambda r: any(m != "minor" for _, m, _ in r["gaps"])),
                        ("non-minor+unack", nonminor_unack),
                        ("ao+non-minor+unack", ao_nonminor_unack)):
        k = sum(1 for r in rows.values() if pred(r))
        print(f"  {label}-rate: {k}/{n} = {k / n:.3f}")
    dead = [r for r in rows.values() if r["com"] == "dead"]
    term_c = Counter(t for r in dead for t in set(r["terminals"]))
    print(f"  death terminals (dead stories, N={len(dead)}): {dict(term_c.most_common())}")
    da = [cid for cid, r in rows.items() if r["obs"] == "coherent" and r["com"] == "dead"]
    return {"core": set(core), "diverge_a": set(da), "two": two, "obs_c": obs_c, "rows": rows}


def main():
    arms = {}
    for tag, expect in (("testsets", 119), ("kernel_v1", 1106),
                        ("haiku", 960), ("flash", 960)):
        meta, rows = parse_rows(HERE / f"arm_{tag}.rows")
        arms[tag] = arm_report(tag, meta, rows, expect)

    # --- C2: conjunction-cell control (pinned story must be diverge-A on testsets)
    print("\n===== C2 conjunction-cell control =====")
    c2 = "propagation_speed_asymmetry"
    r = arms["testsets"]["rows"].get(c2)
    ok = r and r["obs"] == "coherent" and r["com"] == "dead"
    print(f"  {c2}: obs={r['obs'] if r else 'MISSING'} com={r['com'] if r else '-'} "
          f"fired={r['fired'] if r else '-'} -> C2 {'PASS' if ok else 'FAIL'}")
    if not ok:
        print("  !! HALT per PLAN.md: no arm numbers citable until fixed")

    # --- C5: null-exclusion computed BEFORE reading new kernel_v1 diverge-A
    print("\n===== C5 kernel_v1 anchor (pool stage: 906 kernel-bearing) =====")
    banked_da = set()
    for line in BANKED.read_text().splitlines():
        m = re.match(r"ROW \S+ \| (\S+) \| coherent \| dead", line)
        if m:
            banked_da.add(m.group(1))
    print(f"  banked diverge-A (2026-06-07): {len(banked_da)} (banked headline 74)")
    kv = arms["kernel_v1"]["rows"]
    now_undet = {cid for cid, r in kv.items() if r["obs"] == "undetermined"}
    excl = banked_da & now_undet
    print(f"  OQ-51 null-exclusion delta: {len(excl)} banked-diverge-A now undetermined")
    target = len(banked_da) - len(excl)
    print(f"  reproduction target = {len(banked_da)} - {len(excl)} = {target}")
    new_da = arms["kernel_v1"]["diverge_a"]
    print(f"  NEW kernel_v1 diverge-A at HEAD: {len(new_da)}")
    print(f"  deviation from target: {len(new_da) - target:+d}")
    gained = sorted(new_da - banked_da)
    lost = sorted(banked_da - new_da - excl)
    print(f"  gained (not banked-diverge-A): {len(gained)} {gained[:6]}")
    print(f"  lost (banked, not excluded, not diverge-A now): {len(lost)} {lost[:6]}")

    # --- coverage gates (pinned)
    print("\n===== coverage gates =====")
    for t in ("haiku", "flash"):
        oc = arms[t]["obs_c"]
        pool = oc.get("coherent", 0) + oc.get("incoherent", 0)
        print(f"  {t}: observer pool {pool}/960 (floor {OBS_POOL_FLOOR}) -> "
              f"{'OK' if pool >= OBS_POOL_FLOOR else 'UNDERPOWERED - stop 2x2 reads'}")
        core_n = len(arms[t]["core"])
        print(f"  {t}: core N {core_n} (floor {CORE_N_FLOOR}) -> "
              f"{'citable' if core_n >= CORE_N_FLOOR else 'DESCRIPTIVE-ONLY'}")

    # --- C7 join-rate BEFORE any cross-twin agreement number
    print("\n===== cross-twin (haiku x flash) =====")
    h, f = arms["haiku"]["rows"], arms["flash"]["rows"]
    joined = set(h) & set(f)
    print(f"  C7 join-rate: |haiku ∩ flash| = {len(joined)} "
          f"(haiku {len(h)}, flash {len(f)})")
    hs = sum(1 for c in h.values() if any(m == "substantial" for _, m, _ in c["gaps"])) / len(h)
    fs = sum(1 for c in f.values() if any(m == "substantial" for _, m, _ in c["gaps"])) / len(f)
    delta = abs(hs - fs)
    print(f"  substantial-rate: haiku {hs:.3f} vs flash {fs:.3f}; |delta| = {delta:.3f} "
          f"({'SMALL (<%.2f) -> consequence 5 ACTIVE: raw core/diverge-A agreement disarmed' % SMALL_DELTA if delta < SMALL_DELTA else 'LARGE -> raw agreement retains discriminating value'})")
    hc, fc = arms["haiku"]["core"] & joined, arms["flash"]["core"] & joined
    both, honly, fonly = len(hc & fc), len(hc - fc), len(fc - hc)
    neither = len(joined) - both - honly - fonly
    print(f"  core membership over join: both={both} haiku-only={honly} "
          f"flash-only={fonly} neither={neither}")
    hda, fda = arms["haiku"]["diverge_a"] & joined, arms["flash"]["diverge_a"] & joined
    print(f"  diverge-A membership over join: both={len(hda & fda)} "
          f"haiku-only={len(hda - fda)} flash-only={len(fda - hda)}")
    # conditioned direction-conjunct residual (the pre-registered payoff metric)
    cond = [c for c in joined if nonminor_unack(h[c]) and nonminor_unack(f[c])]
    print(f"  conditioning set (non-minor+unack in BOTH twins): {len(cond)}")
    agree = Counter((ao_nonminor_unack(h[c]), ao_nonminor_unack(f[c])) for c in cond)
    print(f"  direction=axiom_overriding within conditioned set "
          f"(haiku,flash): {dict(agree)}")
    if cond:
        a = agree.get((True, True), 0) + agree.get((False, False), 0)
        print(f"  conditioned direction agreement: {a}/{len(cond)} = {a / len(cond):.3f}")
        hb = agree.get((True, True), 0) + agree.get((True, False), 0)
        fb = agree.get((True, True), 0) + agree.get((False, True), 0)
        print(f"  base rates within conditioned set: haiku ao {hb}/{len(cond)}={hb/len(cond):.3f}, "
              f"flash ao {fb}/{len(cond)}={fb/len(cond):.3f}; "
              f"chance agreement = {(hb/len(cond))*(fb/len(cond)) + (1-hb/len(cond))*(1-fb/len(cond)):.3f}")


if __name__ == "__main__":
    sys.exit(main())
