#!/usr/bin/env python3
"""Gate row `python env` — assert the RUNNING interpreter can import what the repo imports.

Why this exists (2026-08-18). An OS upgrade moved the system interpreter 3.10 -> 3.12 and
stranded every pip-installed package. The gate showed exactly ONE red
(`gap surfaces: ModuleNotFoundError: pandas`) while ~20 tools were broken, because the other
affected tools are not gate rows. One red row read as one broken check. It was not.

Two design choices follow from that:

1. **The required set is DERIVED, not declared.** An AST scan of `python/` + `agent/` finds
   every third-party import. A hand-maintained manifest would be a second canonical list
   (Build Discipline Pattern 2) and would go stale the first time someone adds an import.
   Consequence worth knowing: adding a new third-party import to any script turns this row
   RED until the dep is installed or declared below. That is the intent.

2. **The interpreter is REPORTED, always.** The failure above was interpreter selection, not
   content: the same checker was red under `python3` and green under `.venv/bin/python`. A
   gate row that does not say which interpreter it ran under cannot distinguish those.

`OPTIONAL` below is the escape hatch: modules allowed to be absent, each with a reason.
Absent-and-declared prints as a note; absent-and-undeclared is RED.
"""
import argparse
import ast
import importlib.util
import pathlib
import sys
import warnings

ROOT = pathlib.Path(__file__).resolve().parent.parent
SCAN_DIRS = ("python", "agent")

# Modules allowed to be absent. Reason is mandatory — an entry without one is a silent carve-out.
OPTIONAL = {
    "sentence_transformers": "heavy (pulls torch, GB-scale) for 2 audit scripts "
                             "(cluster_space_audit, g_orbit_proximity_probe); install on demand",
}

# Scripts invoked by `.claude/settings.json` hooks. Those hook commands run bare `python3`
# (the SYSTEM interpreter), not the venv, and deliberately so: a JSON-string hook cannot
# reliably resolve a venv, and $CLAUDE_PROJECT_DIR may not be the cwd. That is safe ONLY
# while these stay stdlib-only. Adding a third-party import to one of them would break the
# hook silently in a fresh shell while every venv-run check stayed green — so the invariant
# is checked here rather than remembered. If you must add one, fix the hook command in the
# same change.
HOOK_SCRIPTS = (
    "python/omega_resolver.py",       # SessionStart: activations
    "python/pretooluse_tripwires.py", # PreToolUse:  tripwire injection
    "python/issues_status.py",        # PostToolUse: ISSUES.md grammar gate
)


def hook_scripts_stdlib_only():
    """-> {script: sorted[third-party modules it imports]} — empty dict is the healthy state."""
    std = set(sys.stdlib_module_names)
    local = _local_names()
    bad = {}
    for rel in HOOK_SCRIPTS:
        f = ROOT / rel
        if not f.exists():
            bad[rel] = ["<MISSING FILE>"]
            continue
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", SyntaxWarning)
            tree = ast.parse(f.read_text(encoding="utf-8", errors="replace"))
        ext = set()
        for n in ast.walk(tree):
            if isinstance(n, ast.Import):
                ext |= {a.name.split(".")[0] for a in n.names}
            elif isinstance(n, ast.ImportFrom) and n.level == 0 and n.module:
                ext.add(n.module.split(".")[0])
        ext -= std | local
        if ext:
            bad[rel] = sorted(ext)
    return bad


def _local_names():
    """Top-level names that resolve inside the repo, so they are not third-party."""
    names = set(SCAN_DIRS)
    for d in SCAN_DIRS:
        base = ROOT / d
        if not base.is_dir():
            continue
        for p in base.rglob("*.py"):
            names.add(p.stem)
        for p in base.rglob("*"):
            if p.is_dir():
                names.add(p.name)
    return names


def scan():
    """-> {module: sorted[relative paths that import it]} for third-party imports only."""
    std = set(sys.stdlib_module_names)
    local = _local_names()
    hits, unreadable = {}, []
    for d in SCAN_DIRS:
        for p in sorted((ROOT / d).rglob("*.py")) if (ROOT / d).is_dir() else []:
            try:
                with warnings.catch_warnings():
                    # Some scripts carry invalid escape sequences; that is a lint issue for
                    # those files, not a finding for this row. Do not let it pollute output.
                    warnings.simplefilter("ignore", SyntaxWarning)
                    tree = ast.parse(p.read_text(encoding="utf-8", errors="replace"))
            except SyntaxError:
                unreadable.append(str(p.relative_to(ROOT)))
                continue
            for n in ast.walk(tree):
                mods = []
                if isinstance(n, ast.Import):
                    mods = [a.name.split(".")[0] for a in n.names]
                elif isinstance(n, ast.ImportFrom) and n.level == 0 and n.module:
                    mods = [n.module.split(".")[0]]
                for m in mods:
                    if m in std or m in local:
                        continue
                    hits.setdefault(m, set()).add(str(p.relative_to(ROOT)))
    return {m: sorted(v) for m, v in hits.items()}, unreadable


def importable(mod):
    """Findable WITHOUT executing it — find_spec, never import (imports have side effects)."""
    try:
        return importlib.util.find_spec(mod) is not None
    except (ImportError, ValueError):
        return False


def check():
    hits, unreadable = scan()
    missing_hard, missing_opt = {}, {}
    for m in sorted(hits):
        if importable(m):
            continue
        (missing_opt if m in OPTIONAL else missing_hard)[m] = hits[m]

    interp = sys.executable
    try:
        rel = pathlib.Path(interp).relative_to(ROOT)
        where = f".../{rel}"
    except ValueError:
        where = interp

    for m, files in missing_hard.items():
        print(f"MISSING: {m} — imported by {len(files)} file(s): {', '.join(files[:3])}"
              + (" …" if len(files) > 3 else ""))
    for m in missing_opt:
        print(f"note: {m} absent (declared optional: {OPTIONAL[m]})")
    for f in unreadable:
        print(f"WARN: unparseable, not scanned: {f}")

    hook_bad = hook_scripts_stdlib_only()
    for rel, mods in hook_bad.items():
        print(f"HOOK-BREAK: {rel} imports third-party {', '.join(mods)} but its "
              f".claude/settings.json hook runs bare `python3` — fix the hook command too")

    ok = not missing_hard and not hook_bad
    print(f"python env: {'GREEN' if ok else 'RED'} — interpreter {where} (py"
          f"{sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}); "
          f"{len(hits)} third-party import(s) required, {len(missing_hard)} missing, "
          f"{len(missing_opt)}/{len(OPTIONAL)} declared-optional absent, "
          f"{len(HOOK_SCRIPTS) - len(hook_bad)}/{len(HOOK_SCRIPTS)} hook scripts stdlib-only, "
          f"{len(unreadable)} unparseable")
    return 0 if ok else 1


def selftest():
    """Discrimination, not detection: the instrument must DECLINE as well as fire."""
    fails = []

    def ck(name, cond):
        print(f"  {'PASS' if cond else 'FAIL'}  {name}")
        if not cond:
            fails.append(name)

    hits, _ = scan()
    # Fires: a name that cannot possibly be installed is reported missing.
    ck("nonexistent module is not importable",
       not importable("zzz_definitely_not_a_module_20260818"))
    # Declines: stdlib and a known-present third-party are not flagged.
    ck("stdlib is importable", importable("json"))
    ck("scan excludes stdlib", "json" not in hits and "pathlib" not in hits)
    # Declines: local packages are not mistaken for third-party (the false-positive direction).
    ck("scan excludes local modules", "reports" not in hits and "shared" not in hits)
    # Fires: the scan actually finds real third-party imports (a scan returning {} would
    # otherwise pass every check above vacuously — Pattern 5).
    ck("scan is non-empty (not vacuous)", len(hits) >= 5)
    ck("scan finds a known third-party import", "anthropic" in hits or "numpy" in hits)
    # Every optional carve-out carries a reason.
    ck("all OPTIONAL entries carry a reason", all(bool(v.strip()) for v in OPTIONAL.values()))
    # OPTIONAL may not name something nothing imports (a stale carve-out hides a real gap).
    stale = [m for m in OPTIONAL if m not in hits]
    ck(f"no stale OPTIONAL entries{' — ' + ', '.join(stale) if stale else ''}", not stale)
    # Hook invariant: declines on the real (healthy) state...
    ck("hook scripts are stdlib-only", not hook_scripts_stdlib_only())
    # ...and FIRES on a planted violation. Without this the check above passes whether the
    # detector works or is a stub that always returns {} (Pattern 5 / vacuous pass).
    _real = globals()["HOOK_SCRIPTS"]
    try:
        globals()["HOOK_SCRIPTS"] = ("python/query.py",)  # known to import pandas
        planted = hook_scripts_stdlib_only()
        ck("hook detector FIRES on a planted third-party import",
           "python/query.py" in planted and "pandas" in planted["python/query.py"])
        globals()["HOOK_SCRIPTS"] = ("python/does_not_exist_20260818.py",)
        ck("hook detector FIRES on a missing hook script",
           bool(hook_scripts_stdlib_only()))
    finally:
        globals()["HOOK_SCRIPTS"] = _real
    ck("HOOK_SCRIPTS restored after plant", globals()["HOOK_SCRIPTS"] is _real)

    print(f"python_env_check selftest: {len(fails)} failure(s)" if fails
          else "python_env_check selftest: OK (12/12 controls)")
    return 1 if fails else 0


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true")
    ap.add_argument("--selftest", action="store_true")
    a = ap.parse_args()
    if a.selftest:
        sys.exit(selftest())
    sys.exit(check())
