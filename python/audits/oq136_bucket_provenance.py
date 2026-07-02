#!/usr/bin/env python3
"""OQ-136: census absence buckets x generation provenance (pre-registered join).

Executes the design frozen in
audits/2026-07-02_oq136_census_bucket_provenance/PROPOSAL.md — read it first;
every statistic, threshold, and axis rule here is pre-committed there.

Pipeline:
  1. One swipl run emits MEMBER/PROV/KERNEL/MCC_EXCL lines (membership via
     commentary_cell/3, provenance via story_provenance/8, both from the SAME
     loaded corpus — no cross-load skew). Sigma + exactly-one checks, with a
     drop-one positive control on the checker itself.
  2. Axis derivation: model / prompt_commit from provenance
     (provenance_unauthored stratum where absent); topic_family by the frozen
     rule (cs_kernel_id -> __-prefix -> batch-tag strip). json/ twins
     cross-checked against the .pl provenance.
  3. K x 2 raw-count contingency per (in-scope bucket x axis); Fisher exact for
     2x2, permutation chi^2 (N=10,000, seed=20260702) for K>2; Holm over the
     rule-defined family (powered buckets n>=8 x artifact axes). Two statistic
     positive controls run BEFORE any real table is read.

Writes only to the audit dir. Records its own git rev + dirty flag + loaded
count (RECON.md) — never cites the pipeline manifest.
"""

import json
import re
import subprocess
import sys
from collections import Counter, defaultdict
from pathlib import Path

import numpy as np
from scipy.stats import fisher_exact

REPO = Path(__file__).resolve().parents[2]
PROLOG_DIR = REPO / "prolog"
TESTSETS_DIR = PROLOG_DIR / "testsets"
JSON_DIR = REPO / "json"
AUDIT_DIR = REPO / "audits" / "2026-07-02_oq136_census_bucket_provenance"

SEED = 20260702
N_PERM = 10_000
UNAUTHORED = "provenance_unauthored"

# Frozen scope (PROPOSAL.md): bucket -> census source.
SCOPE = {
    "q6_unmeasured": "q6",
    "q6_signature_unknown": "q6",
    "extraction_unnameable": "extraction_reading",
    "no_agent_seats": "consensus",
    "manufactured_consensus_candidate": "consensus",
}
ARTIFACT_AXES = ["model", "prompt_commit"]
ALL_AXES = ARTIFACT_AXES + ["topic_family"]
POWER_MIN = 8          # powered bucket: n >= 8 at the re-witnessed count
ENRICH_MIN_MEMBERS = 3  # most-enriched stratum needs >= 3 in-bucket members
ENRICH_MIN_RATIO = 2.0  # ...and >= 2x enrichment
ALPHA = 0.05

PROLOG_GOAL = """
corpus_loader:load_all_testsets,
aggregate_all(count, corpus_loader:corpus_constraint(_), N),
format('MEMBER_META n_corpus ~w~n', [N]),
forall(commentary_census:commentary_source(S),
  forall(( corpus_loader:corpus_constraint(C),
           commentary_census:commentary_cell(S, C, B) ),
         format('MEMBER ~w ~w ~w~n', [S, C, B]))),
forall(( corpus_loader:corpus_constraint(C),
         narrative_ontology:story_provenance(C, PC, _, GD, _, _, M, _) ),
       format('PROV ~w ~w ~w ~w~n', [C, PC, M, GD])),
forall(( corpus_loader:corpus_constraint(C),
         narrative_ontology:cs_kernel_id(C, K) ),
       format('KERNEL ~w ~w~n', [C, K])),
forall(( corpus_loader:corpus_constraint(C),
         stakeholder_seats:consensus_provenance(C,
             manufactured_consensus_candidate(Excl)) ),
       format('MCC_EXCL ~w ~w~n', [C, Excl]))
"""


def run_prolog_extract():
    cmd = ["swipl", "-l", "stack.pl", "-l", "commentary_census.pl",
           "-g", PROLOG_GOAL.replace("\n", " ") + ", halt.", "-t", "halt(1)"]
    res = subprocess.run(cmd, cwd=str(PROLOG_DIR), capture_output=True,
                         text=True, timeout=600)
    if res.returncode != 0:
        sys.exit(f"swipl extract failed (rc={res.returncode}):\n{res.stderr[-2000:]}")
    return res.stdout


def parse_extract(raw):
    n_corpus = None
    members = []            # (source, cid, bucket)
    prov = {}               # cid -> {prompt_commit, model, generated_date}
    kernel = {}             # cid -> kernel_id
    mcc_excl = {}           # cid -> raw excl list text
    for line in raw.splitlines():
        parts = line.split(None, 3)
        if not parts:
            continue
        if parts[0] == "MEMBER_META" and parts[1] == "n_corpus":
            n_corpus = int(parts[2])
        elif parts[0] == "MEMBER" and len(parts) == 4:
            members.append((parts[1], parts[2], parts[3]))
        elif parts[0] == "PROV":
            _, rest = line.split(None, 1)
            cid, pc, model, gd = rest.split()
            prov[cid] = {"prompt_commit": pc, "model": model, "generated_date": gd}
        elif parts[0] == "KERNEL":
            kernel[parts[1]] = parts[2]
        elif parts[0] == "MCC_EXCL":
            mcc_excl[parts[1]] = parts[3] if len(parts) == 4 else parts[2]
    if n_corpus is None:
        sys.exit("extract: no MEMBER_META n_corpus line — census did not run.")
    if n_corpus <= 0:
        sys.exit(f"extract: n_corpus={n_corpus} — corpus did not load; refusing "
                 "(a 0==0 sigma check would pass vacuously).")
    return n_corpus, members, prov, kernel, mcc_excl


def check_membership(n_corpus, members):
    """Sigma == n_corpus per source AND exactly one bucket per (source, cid).
    Raises AssertionError naming the defect."""
    per_source = defaultdict(list)
    for s, c, b in members:
        per_source[s].append((c, b))
    if not per_source:
        raise AssertionError("no MEMBER lines at all")
    for s, rows in per_source.items():
        cids = [c for c, _ in rows]
        dupes = [c for c, k in Counter(cids).items() if k != 1]
        if dupes:
            raise AssertionError(f"[{s}] non-exactly-one cids: {dupes[:5]}")
        if len(rows) != n_corpus:
            raise AssertionError(
                f"[{s}] sigma members ({len(rows)}) != n_corpus ({n_corpus})")


def extractor_positive_control(n_corpus, members):
    """The checker must FIRE on a dropped member (and on a duplicated one)."""
    dropped = members[1:]
    try:
        check_membership(n_corpus, dropped)
    except AssertionError as e:
        drop_msg = str(e)
    else:
        sys.exit("EXTRACTOR CONTROL FAILED: sigma check did not fire on a "
                 "dropped member — the check is not discriminating; halting.")
    duped = members + [members[0]]
    try:
        check_membership(n_corpus, duped)
    except AssertionError as e:
        dup_msg = str(e)
    else:
        sys.exit("EXTRACTOR CONTROL FAILED: exactly-one check did not fire on "
                 "a duplicated member; halting.")
    return drop_msg, dup_msg


def topic_family(cid, kernel):
    """Frozen rule (PROPOSAL.md): cs_kernel_id > __-prefix > one batch-tag strip."""
    if cid in kernel:
        return kernel[cid]
    if "__" in cid:
        return cid.split("__", 1)[0]
    for tag in ("_contradictions", "_flat_control", "_c0"):
        if cid.endswith(tag):
            return cid[: -len(tag)]
    return cid


def axis_value(cid, axis, prov, kernel):
    if axis == "topic_family":
        return topic_family(cid, kernel)
    return prov.get(cid, {}).get(axis, UNAUTHORED)


def chi2_stat(table):
    """Pearson chi^2 on a K x 2 table, skipping zero-expected cells."""
    t = np.asarray(table, dtype=float)
    total = t.sum()
    rows = t.sum(axis=1, keepdims=True)
    cols = t.sum(axis=0, keepdims=True)
    expected = rows @ cols / total
    mask = expected > 0
    return float((((t - expected) ** 2)[mask] / expected[mask]).sum())


def perm_pvalue(strata_of_all, in_bucket_mask, rng):
    """Permutation test on chi^2: shuffle bucket membership over the corpus."""
    strata = np.asarray(strata_of_all)
    labels = np.unique(strata)
    obs = chi2_stat(_table(strata, in_bucket_mask, labels))
    n_ge = 0
    mask = in_bucket_mask.copy()
    for _ in range(N_PERM):
        rng.shuffle(mask)
        if chi2_stat(_table(strata, mask, labels)) >= obs - 1e-12:
            n_ge += 1
    return (1 + n_ge) / (N_PERM + 1), obs


def _table(strata, mask, labels):
    return [[int(((strata == s) & mask).sum()),
             int(((strata == s) & ~mask).sum())] for s in labels]


def bucket_axis_test(cids_all, bucket_cids, axis, prov, kernel, rng):
    strata_all = [axis_value(c, axis, prov, kernel) for c in cids_all]
    mask = np.array([c in bucket_cids for c in cids_all])
    labels = sorted(set(strata_all))
    table = _table(np.asarray(strata_all), mask, labels)
    k = len(labels)
    if k < 2:
        p, method = 1.0, "degenerate_single_stratum"
    elif k == 2:
        p = float(fisher_exact(np.asarray(table))[1])
        method = "fisher_exact"
    else:
        p, _ = perm_pvalue(strata_all, mask, rng)
        method = f"permutation_chi2_N{N_PERM}"
    # enrichment: stratum share among bucket vs among rest, from raw counts.
    # Gate reading (PROPOSAL's "most-enriched stratum has >=3 members and >=2x"):
    # EXISTENTIAL — gates pass iff some stratum meets BOTH; a 1-member stratum
    # with infinite ratio must not mask (or fake) a real multi-member cluster.
    # `most_enriched` reported = the max-ratio stratum among gate-passers, else
    # the max-(members, ratio) stratum for description.
    n_in = int(mask.sum())
    n_rest = len(cids_all) - n_in
    strata_stats = []
    for lab, (a, b) in zip(labels, table):
        if n_in == 0:
            continue
        share_in = a / n_in
        share_rest = (b / n_rest) if n_rest else 0.0
        ratio = float("inf") if (share_rest == 0 and share_in > 0) else (
            share_in / share_rest if share_rest else 0.0)
        strata_stats.append({"stratum": lab, "members": int(a), "ratio": ratio})
    passers = [s for s in strata_stats
               if s["members"] >= ENRICH_MIN_MEMBERS
               and s["ratio"] >= ENRICH_MIN_RATIO]
    enrich_ok = bool(passers)
    if passers:
        best = max(passers, key=lambda s: (s["ratio"], s["members"]))
    elif strata_stats:
        best = max(strata_stats, key=lambda s: (s["members"], s["ratio"]))
    else:
        best = None
    return {"axis": axis, "k_strata": k, "labels": labels, "table": table,
            "p_raw": p, "method": method, "most_enriched": best,
            "enrichment_gates_pass": enrich_ok}


def holm(pvals):
    """Holm step-down: list of (key, p) -> dict key -> p_holm."""
    m = len(pvals)
    out = {}
    running = 0.0
    for i, (key, p) in enumerate(sorted(pvals, key=lambda kp: kp[1])):
        adj = (m - i) * p
        running = max(running, adj)
        out[key] = min(1.0, running)
    return out


def statistic_positive_controls():
    """(1) synthetic all-one-stratum bucket MUST flag; (2) seeded uniform draw
    must NOT. Run before any real table is read; halt if either fails."""
    rng = np.random.default_rng(SEED)
    cids = [f"ctl_{i}" for i in range(119)]
    # control 1: 30 of 119 in stratum A; a 10-member bucket entirely inside A
    prov1 = {c: {"model": ("A" if i < 30 else "B")} for i, c in enumerate(cids)}
    r1 = bucket_axis_test(cids, set(cids[:10]), "model", prov1, {}, rng)
    flag1 = r1["p_raw"] < ALPHA and r1["enrichment_gates_pass"]
    if not flag1:
        sys.exit(f"STATISTIC CONTROL 1 FAILED: planted all-one-stratum bucket "
                 f"not flagged ({r1}); halting before real tables.")
    # control 2: same corpus, 10-member bucket drawn uniformly (seeded)
    draw = set(rng.choice(cids, size=10, replace=False).tolist())
    r2 = bucket_axis_test(cids, draw, "model", prov1, {}, rng)
    flag2 = r2["p_raw"] < ALPHA and r2["enrichment_gates_pass"]
    if flag2:
        sys.exit(f"STATISTIC CONTROL 2 FAILED: seeded uniform draw flagged as "
                 f"clustered ({r2}); halting before real tables.")
    return {"control_planted_cluster": {"p_raw": r1["p_raw"],
                                        "enrich": r1["most_enriched"],
                                        "flagged": flag1},
            "control_uniform_draw": {"p_raw": r2["p_raw"],
                                     "enrich": r2["most_enriched"],
                                     "flagged": flag2}}


def json_twin_crosscheck(prov):
    """json/<cid>.json provenance blocks vs the .pl story_provenance."""
    mismatches, twin_missing, checked = [], [], 0
    for cid, p in prov.items():
        jf = JSON_DIR / f"{cid}.json"
        if not jf.exists():
            twin_missing.append(cid)
            continue
        try:
            jp = json.loads(jf.read_text(encoding="utf-8")).get("provenance", {})
        except Exception as e:
            mismatches.append((cid, f"json unreadable: {e}"))
            continue
        checked += 1
        for key in ("prompt_commit", "model"):
            if str(jp.get(key)) != p[key]:
                mismatches.append((cid, f"{key}: pl={p[key]} json={jp.get(key)}"))
    return {"checked": checked, "mismatches": mismatches,
            "twin_missing": twin_missing}


def git_state():
    rev = subprocess.run(["git", "rev-parse", "HEAD"], cwd=REPO,
                         capture_output=True, text=True).stdout.strip()
    dirty = subprocess.run(["git", "status", "--porcelain"], cwd=REPO,
                           capture_output=True, text=True).stdout.strip() != ""
    return rev, dirty


def main():
    AUDIT_DIR.mkdir(parents=True, exist_ok=True)
    rev, dirty = git_state()

    raw = run_prolog_extract()
    (AUDIT_DIR / "extract_raw.log").write_text(raw, encoding="utf-8")
    n_corpus, members, prov, kernel, mcc_excl = parse_extract(raw)

    # --- extractor positive control BEFORE trusting the clean check ---------
    drop_msg, dup_msg = extractor_positive_control(n_corpus, members)
    check_membership(n_corpus, members)   # clean run

    # --- statistic positive controls BEFORE any real table ------------------
    stat_controls = statistic_positive_controls()

    cids_all = sorted({c for s, c, b in members if s == "q6"})
    bucket_members = defaultdict(set)
    for s, c, b in members:
        if b in SCOPE and SCOPE[b] == s:
            bucket_members[b].add(c)

    twin = json_twin_crosscheck(prov)

    # --- membership.tsv ------------------------------------------------------
    with open(AUDIT_DIR / "membership.tsv", "w", encoding="utf-8") as f:
        f.write("source\tcid\tbucket\tprompt_commit\tmodel\tgenerated_date"
                "\ttopic_family\tmcc_excl\n")
        for s, c, b in sorted(members):
            p = prov.get(c, {})
            f.write("\t".join([
                s, c, b,
                p.get("prompt_commit", UNAUTHORED),
                p.get("model", UNAUTHORED),
                p.get("generated_date", UNAUTHORED),
                topic_family(c, kernel),
                mcc_excl.get(c, "") if b == "manufactured_consensus_candidate" else "",
            ]) + "\n")

    # --- per (bucket x axis) tables + tests ----------------------------------
    rng = np.random.default_rng(SEED)
    results = {}
    for bucket in SCOPE:
        mem = bucket_members.get(bucket, set())
        powered = len(mem) >= POWER_MIN
        results[bucket] = {"n": len(mem), "powered": powered, "axes": {}}
        for axis in ALL_AXES:
            results[bucket]["axes"][axis] = bucket_axis_test(
                cids_all, mem, axis, prov, kernel, rng)

    # Holm family: powered buckets x artifact axes (rule-defined).
    family = [((b, a), results[b]["axes"][a]["p_raw"])
              for b in SCOPE for a in ARTIFACT_AXES if results[b]["powered"]]
    holm_p = holm(family)
    for (b, a), _ in family:
        r = results[b]["axes"][a]
        r["p_holm"] = holm_p[(b, a)]
        r["clustered"] = bool(r["p_holm"] < ALPHA and r["enrichment_gates_pass"])
    for b in SCOPE:   # topic: descriptive labeling only (outside the family)
        r = results[b]["axes"]["topic_family"]
        r["descriptive_clustered"] = bool(
            r["p_raw"] < ALPHA and r["enrichment_gates_pass"])

    # --- axis-confounding cross-tab: topic_family x prompt_commit -----------
    confound = defaultdict(Counter)
    for c in cids_all:
        confound[topic_family(c, kernel)][
            prov.get(c, {}).get("prompt_commit", UNAUTHORED)] += 1
    multi_commit_families = {t: dict(cnt) for t, cnt in confound.items()
                             if len(cnt) > 1}

    # --- stats_output.json ----------------------------------------------------
    out = {
        "audit": "oq136_bucket_provenance",
        "proposal": "audits/2026-07-02_oq136_census_bucket_provenance/PROPOSAL.md",
        "git_rev": rev, "git_dirty": dirty,
        "n_corpus_loaded": n_corpus,
        "seed": SEED, "n_perm": N_PERM,
        "extractor_controls": {"drop_one_fired": drop_msg,
                               "duplicate_fired": dup_msg},
        "statistic_controls": stat_controls,
        "json_twin_crosscheck": {
            "checked": twin["checked"],
            "n_mismatches": len(twin["mismatches"]),
            "mismatches": twin["mismatches"][:20],
            "twin_missing": twin["twin_missing"]},
        "holm_family_size": len(family),
        "results": results,
        "confounding_multi_commit_families": multi_commit_families,
    }
    (AUDIT_DIR / "stats_output.json").write_text(
        json.dumps(out, indent=2, default=str), encoding="utf-8")

    # --- contingency_tables.md ------------------------------------------------
    lines = ["# OQ-136 contingency tables (raw counts — no rates; see PROPOSAL.md)",
             "", f"git `{rev[:12]}` dirty={dirty}; corpus n={n_corpus}; "
             f"seed={SEED}; Holm family size={len(family)}", ""]
    for bucket, rb in results.items():
        lines.append(f"## {bucket} (n={rb['n']}, "
                     f"{'POWERED' if rb['powered'] else 'UNPOWERED — descriptive only'})")
        for axis in ALL_AXES:
            r = rb["axes"][axis]
            lines.append(f"\n### axis: {axis} ({r['method']}, K={r['k_strata']})")
            lines.append("| stratum | in-bucket | rest |")
            lines.append("|---|---|---|")
            for lab, (a, bb) in zip(r["labels"], r["table"]):
                lines.append(f"| {lab} | {a} | {bb} |")
            verdict = (f"p_raw={r['p_raw']:.4g}"
                       + (f", p_holm={r['p_holm']:.4g}, clustered={r['clustered']}"
                          if "p_holm" in r else
                          (f", descriptive_clustered={r['descriptive_clustered']}"
                           if axis == "topic_family" else ", (unpowered: no test in family)")))
            e = r["most_enriched"]
            lines.append(f"\n{verdict}; most-enriched={e['stratum']} "
                         f"(members={e['members']}, ratio={e['ratio']:.3g}); "
                         f"enrichment gates pass={r['enrichment_gates_pass']}")
        lines.append("")
    lines.append("## Axis-confounding cross-tab: topic_family x prompt_commit")
    lines.append("(families spanning >1 prompt_commit; a family echoing a single "
                 "generation batch is the confound to read jointly)")
    lines.append(f"\nfamilies with >1 commit: {len(multi_commit_families)} "
                 f"of {len(confound)}")
    for t, cnt in sorted(multi_commit_families.items()):
        lines.append(f"- {t}: {cnt}")
    (AUDIT_DIR / "contingency_tables.md").write_text(
        "\n".join(lines) + "\n", encoding="utf-8")

    # --- RECON.md ---------------------------------------------------------------
    bucket_counts = {b: results[b]["n"] for b in SCOPE}
    recon = [
        "# OQ-136 RECON — re-witnessed counts at execution time",
        "",
        f"- git rev: `{rev}` (dirty={dirty}) — the audit's own stamp; the pipeline",
        "  manifest is NOT cited (it may be stale relative to this run).",
        f"- corpus loaded: n={n_corpus} (corpus_loader count from the extract run)",
        f"- bucket counts: {json.dumps(bucket_counts)}",
        f"- powered (n>={POWER_MIN}): "
        f"{[b for b in SCOPE if results[b]['powered']]}",
        f"- unpowered: {[b for b in SCOPE if not results[b]['powered']]}",
        f"- provenance authored: {len(prov)}/{n_corpus}; missing: "
        f"{sorted(set(cids_all) - set(prov))}",
        f"- json twin crosscheck: {twin['checked']} checked, "
        f"{len(twin['mismatches'])} mismatches, "
        f"{len(twin['twin_missing'])} twins missing",
        f"- extractor controls fired: drop-one [{drop_msg}]; duplicate [{dup_msg}]",
        f"- statistic controls: planted-cluster flagged="
        f"{stat_controls['control_planted_cluster']['flagged']} "
        f"(p={stat_controls['control_planted_cluster']['p_raw']:.4g}); "
        f"uniform-draw flagged={stat_controls['control_uniform_draw']['flagged']} "
        f"(p={stat_controls['control_uniform_draw']['p_raw']:.4g})",
    ]
    (AUDIT_DIR / "RECON.md").write_text("\n".join(recon) + "\n", encoding="utf-8")

    print(f"[oq136] done. corpus n={n_corpus}; buckets={bucket_counts}; "
          f"holm family={len(family)}; outputs in {AUDIT_DIR}")
    for b in SCOPE:
        for a in ARTIFACT_AXES:
            r = results[b]["axes"][a]
            if "clustered" in r:
                print(f"  {b} x {a}: p_holm={r['p_holm']:.4g} "
                      f"clustered={r['clustered']}")
        rt = results[b]["axes"]["topic_family"]
        print(f"  {b} x topic: p_raw={rt['p_raw']:.4g} "
              f"descriptive_clustered={rt['descriptive_clustered']}")


if __name__ == "__main__":
    main()
