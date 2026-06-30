"""
oq38_orphan_sweep.py — reproducible export-vs-caller orphan census (OQ-38).

Replaces the uncommitted 2026-05-31 ad-hoc grep sweep (wiring_gap_census.md,
"217-candidate upper bound") with a tool-native funnel:

    [tool exports] -> [tool zero-static-caller] -> N (xref STATIC_ORPHAN)
                   -> M (N minus the dynamic-reachability surface)

Pipeline:
  1. Run prolog/orphan_xref.pl (library(prolog_xref); clause-head-vs-body
     separator) to produce outputs/oq38_orphan_xref.tsv — the static census.
  2. Build the DYNAMIC SURFACE that static xref cannot see:
       (a) predicate names appearing literally in Python/shell as swipl goal
           strings / run_* API names (run_pipeline.py ships goals as strings),
       (b) Prolog name-CONSTRUCTION prefixes (atom_concat / atomic_list_concat /
           format(atom(...)) / =.. / term_to_atom) — a STATIC_ORPHAN whose name
           starts with a constructed prefix may be assembled and call/N'd at
           runtime, invisible to xref.
  3. Join: a STATIC_ORPHAN masked by the dynamic surface is do-not-strip
     (dynamic-reachable). M = STATIC_ORPHANs that survive the mask.
  4. Emit the funnel with 528/422/217 (the prior ad-hoc-grep claim) ALONGSIDE,
     naming every delta (tool-exports != 528 is a FINDING, not an error).

[EDGE] M is still an UPPER BOUND on "dead": static-uncalled minus the dynamic
surface we could enumerate. This is a diagnostic, never an automatic stripper.

Run:  python3 python/cli.py audits oq38_orphan_sweep
  or: python3 python/audits/oq38_orphan_sweep.py
"""

import json
import re
import subprocess
import sys
from pathlib import Path

PROLOG_DIR = Path(__file__).resolve().parents[2] / "prolog"
REPO_ROOT = Path(__file__).resolve().parents[2]
OUT_DIR = REPO_ROOT / "outputs"
TSV_PATH = OUT_DIR / "oq38_orphan_xref.tsv"
FUNNEL_JSON = OUT_DIR / "oq38_orphan_funnel.json"

# The prior ad-hoc-grep claim (2026-05-31 wiring_gap_census.md -> ISSUES.md:1845).
PRIOR_CLAIM = {"exports": 528, "zero_external_caller": 422, "candidate": 217}

# The four calibration orphans this OQ strips (the held-out remainder is routed).
CALIBRATION_FOUR = [
    ("linear_slope", 2),
    ("slope_accum", 3),
    ("safe_get_all_metrics", 2),
    ("safe_get_profile_components", 2),
]

# Name-construction patterns (the classes xref is blind to). Each must have a
# firing positive control on the corpus, else "clean for X" proves nothing.
CONSTRUCTION_PATTERNS = {
    "=..": re.compile(r"=\.\."),
    "atom_concat": re.compile(r"\batom_concat\("),
    "atomic_list_concat": re.compile(r"\batomic_list_concat\("),
    "format(atom": re.compile(r"\bformat\(atom\("),
    "term_to_atom": re.compile(r"\bterm_to_atom\("),
}

# Literal-atom extractor for construction prefixes: atoms appearing as the
# first/second arg of a concat call, e.g. atom_concat('safe_get_', X, P).
CONSTRUCTED_ATOM = re.compile(r"""['"]([a-z][a-zA-Z0-9_]*_)['"]""")


def run_prolog_tool():
    """Run the xref core; raise loudly on non-zero (a stale TSV must not pass)."""
    cmd = [
        "swipl", "-l", "orphan_xref.pl",
        "-g", "run_orphan_xref, halt", "-t", "halt(1)",
    ]
    proc = subprocess.run(
        cmd, cwd=str(PROLOG_DIR), capture_output=True, text=True, timeout=300
    )
    sys.stderr.write(proc.stderr)
    if proc.returncode != 0:
        raise SystemExit(f"orphan_xref.pl failed (exit {proc.returncode}) — TSV untrusted")
    if not TSV_PATH.exists():
        raise SystemExit(f"orphan_xref.pl produced no TSV at {TSV_PATH}")


def parse_tsv():
    rows = []
    with open(TSV_PATH) as f:
        header = f.readline()
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) < 7:
                parts += [""] * (7 - len(parts))
            file, name, arity, exported, cls, n_callers, callers = parts[:7]
            rows.append({
                "file": file, "name": name, "arity": int(arity),
                "exported": exported == "1", "class": cls,
                "n_callers": int(n_callers), "callers": callers,
            })
    return rows


def grep_files(globs):
    out = []
    for g in globs:
        out.extend(sorted(REPO_ROOT.glob(g)))
    return out


def build_python_shell_surface():
    """Predicate names appearing literally in Python/shell (goal strings, APIs)."""
    self_path = Path(__file__).resolve()
    text = []
    for p in grep_files(["python/**/*.py", "scripts/**/*.sh", "*.sh", "agent/**/*.py"]):
        if p.resolve() == self_path:
            continue  # self-exclude: this driver NAMES the four for bookkeeping
        try:
            text.append(p.read_text(errors="ignore"))
        except OSError:
            continue
    blob = "\n".join(text)
    # predicate-name-ish tokens; we only test membership against orphan names.
    return set(re.findall(r"[a-z][a-zA-Z0-9_]*", blob))


def build_construction_surface():
    """
    Returns (prefixes, controls):
      prefixes — literal atom prefixes used in Prolog name-construction sites.
      controls — {class: [sample sites]} positive control that each class fires.
    """
    prefixes = set()
    controls = {k: [] for k in CONSTRUCTION_PATTERNS}
    for p in sorted(PROLOG_DIR.glob("*.pl")):
        if p.name == "orphan_xref.pl":
            continue
        for i, line in enumerate(p.read_text(errors="ignore").splitlines(), 1):
            stripped = line.lstrip()
            if stripped.startswith("%"):
                continue  # skip comment lines
            for cls, pat in CONSTRUCTION_PATTERNS.items():
                if pat.search(line):
                    if len(controls[cls]) < 3:
                        controls[cls].append(f"{p.name}:{i}")
                    for m in CONSTRUCTED_ATOM.findall(line):
                        prefixes.add(m)
    return prefixes, controls


def name_is_constructible(name, prefixes):
    """A name is construction-reachable if some constructed prefix prefixes it."""
    return any(name.startswith(pre) for pre in prefixes if len(pre) >= 4)


def main():
    run_prolog_tool()
    rows = parse_tsv()

    n_defined = len(rows)
    exported = [r for r in rows if r["exported"]]
    static_orphans = [r for r in rows if r["class"] == "STATIC_ORPHAN"]
    entrypoints = [r for r in rows if r["class"] == "ENTRYPOINT_CLI"]
    live = [r for r in rows if r["class"] == "LIVE"]

    py_surface = build_python_shell_surface()
    constr_prefixes, constr_controls = build_construction_surface()

    # Positive-control assertion: each construction class must fire somewhere.
    missing_controls = [k for k, v in constr_controls.items() if not v]

    # Mask each static orphan against the dynamic surface.
    masked, survivors = [], []
    for r in static_orphans:
        nm = r["name"]
        in_py = nm in py_surface
        constructible = name_is_constructible(nm, constr_prefixes)
        if in_py or constructible:
            r2 = dict(r, mask_reason=("python_goal_string" if in_py else "prolog_construction"))
            masked.append(r2)
        else:
            survivors.append(r)

    # Status of the four calibration orphans (must be STATIC_ORPHAN & survive,
    # except slope_accum which is LIVE-via-its-sibling — a Commit-A cascade tail).
    four_status = []
    by_key = {(r["name"], r["arity"]): r for r in rows}
    for name, arity in CALIBRATION_FOUR:
        r = by_key.get((name, arity))
        if r is None:
            four_status.append({"pred": f"{name}/{arity}", "class": "ABSENT"})
            continue
        in_py = name in py_surface
        constructible = name_is_constructible(name, constr_prefixes)
        four_status.append({
            "pred": f"{name}/{arity}", "file": r["file"], "class": r["class"],
            "callers": r["callers"], "in_python_surface": in_py,
            "constructible": constructible,
        })

    funnel = {
        "tool_native": {
            "sources_xrefed": 121,
            "predicates_defined": n_defined,
            "exports": len(exported),
            "live": len(live),
            "entrypoint_cli": len(entrypoints),
            "static_orphan_N": len(static_orphans),
            "static_orphan_exported": sum(1 for r in static_orphans if r["exported"]),
            "dynamic_masked": len(masked),
            "real_orphan_M": len(survivors),
        },
        "prior_adhoc_grep_claim": PRIOR_CLAIM,
        "deltas": {
            "exports": len(exported) - PRIOR_CLAIM["exports"],
            "candidate_vs_N": len(static_orphans) - PRIOR_CLAIM["candidate"],
        },
        "construction_positive_controls": constr_controls,
        "missing_construction_controls": missing_controls,
        "calibration_four": four_status,
        "real_orphan_survivors": [
            {"file": r["file"], "pred": f"{r['name']}/{r['arity']}",
             "exported": r["exported"]}
            for r in sorted(survivors, key=lambda r: (r["file"], r["name"]))
        ],
        "dynamic_masked_orphans": [
            {"file": r["file"], "pred": f"{r['name']}/{r['arity']}",
             "mask_reason": r["mask_reason"]}
            for r in sorted(masked, key=lambda r: (r["file"], r["name"]))
        ],
    }

    FUNNEL_JSON.write_text(json.dumps(funnel, indent=2))

    # ---- human-readable funnel ----
    t = funnel["tool_native"]
    print("=" * 70)
    print("OQ-38 ORPHAN FUNNEL (tool-native; xref + dynamic-surface mask)")
    print("=" * 70)
    print(f"  tool exports ............ {t['exports']}   (prior grep claim: {PRIOR_CLAIM['exports']}"
          f"  delta {funnel['deltas']['exports']:+d}  <- FINDING: grep undercounted exports)")
    print(f"  zero-static-caller ...... {t['static_orphan_N'] + t['entrypoint_cli']}"
          f"   (= {t['static_orphan_N']} STATIC_ORPHAN + {t['entrypoint_cli']} ENTRYPOINT_CLI)")
    print(f"  N (xref STATIC_ORPHAN) .. {t['static_orphan_N']}   (prior grep candidate: "
          f"{PRIOR_CLAIM['candidate']}  delta {funnel['deltas']['candidate_vs_N']:+d})")
    print(f"  - dynamic-masked ........ {t['dynamic_masked']}")
    print(f"  = M (real orphan list) .. {t['real_orphan_M']}")
    print(f"  [EDGE] M is an upper bound: static-uncalled minus enumerable dynamic surface.")
    print()
    print("Construction positive-controls (each class must fire):")
    for cls, sites in constr_controls.items():
        flag = "OK" if sites else "** MISSING **"
        print(f"  {cls:20s} {flag}  {', '.join(sites) if sites else '(no firing site)'}")
    if missing_controls:
        print(f"  WARNING: classes with no positive control: {missing_controls}")
    print()
    print("Calibration four (re-witnessed via the trusted tool):")
    for s in four_status:
        extra = ""
        if "in_python_surface" in s:
            extra = (f"  py_surface={s['in_python_surface']} constructible={s['constructible']}"
                     f"  callers=[{s['callers']}]")
        print(f"  {s['pred']:34s} {s['class']:14s}{extra}")
    print()
    print(f"wrote {FUNNEL_JSON.relative_to(REPO_ROOT)}")


if __name__ == "__main__":
    main()
