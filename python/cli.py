#!/usr/bin/env python3
"""cli.py — single discoverable entry point for the python/ toolset (OQ-163).

Lists every tool grouped logically and runs any of them. Grouping is a property
of the COMMAND TREE, not the directory layout: scripts are dispatched wherever
they physically sit, so no files move (the OQ-32 path-fragility risk class is
already drained at the root by paths.py). See ISSUES.md OQ-163.

Usage:
  python3 python/cli.py [list]                 grouped tree of every command
  python3 python/cli.py <group>                list the scripts in <group>
  python3 python/cli.py <group> <name> [args]  run a script (argv forwarded verbatim)
  python3 python/cli.py <name> [args]          run by unique basename (errors if ambiguous)
  python3 python/cli.py report <name>|--all|--list   delegate to the reports package
  python3 python/cli.py menu                   delegate to omega_resolver.py menu (what next)
  python3 python/cli.py selftest               positive-controlled self-check (gate-wired)

EXECUTION is subprocess everywhere: every target runs in its own interpreter
with its own sys.path[0], inherited cwd, forwarded argv, and a propagated exit
code. The dispatcher is a transparent pass-through and cannot change behavior.
"""
from __future__ import annotations

import ast
import subprocess
import sys
from pathlib import Path

# Locate script roots from the canonical source of truth; never re-derive.
sys.path.insert(0, str(Path(__file__).resolve().parent))
from paths import PYTHON_DIR  # noqa: E402

# --- discovery model -------------------------------------------------------
#
# Two tiers with DIFFERENT rot profiles (be honest about which):
#
#   1. Physical subdir groups DO NOT ROT. Drop a file in the dir and it appears
#      in the group automatically.
#   2. Top-level scripts use a prefix/suffix -> group table that ROTS GRACEFULLY
#      into `misc`: a new top-level script with a novel prefix stays reachable
#      (correctness holds) but lands in `misc` (discoverability degrades
#      *visibly* — selftest reports the misc count). The table is one obvious
#      edit to extend.

# Tier 1: physical subdir groups (group name -> directory).
PHYSICAL_GROUPS = {
    "audit": PYTHON_DIR / "audits",
    "sweep": PYTHON_DIR / "sweeps",
    "test": PYTHON_DIR / "tests",
}

# Tier 2a: explicit overrides for named entry points (highest precedence).
TOPLEVEL_OVERRIDES = {
    "run_pipeline": "pipeline",
    "omega_resolver": "omega",
    "enhanced_report": "report-gen",
}

# Tier 2b: ordered prefix rules (first match wins).
TOPLEVEL_PREFIXES = [
    ("game_theory_", "game-theory"),
    ("cluster_space_", "cluster"),
    ("harvest_b", "harvest"),
    ("sotu_", "sotu"),
    ("orbit_", "orbit"),
    ("omega_", "omega"),
    ("tangled_", "tangled"),
    ("generate_", "generate"),
    ("migrate_", "maintenance"),
    ("fix_", "maintenance"),
    ("verify_", "maintenance"),
    ("lineage_", "lineage"),
    ("cs_", "cs"),
]

# Tier 2c: ordered suffix rules (checked after prefixes).
TOPLEVEL_SUFFIXES = [
    ("_sweep", "sweep"),
    ("_report", "report-gen"),
    ("_reporter", "report-gen"),
    ("_analysis", "analysis"),
]

MISC_GROUP = "misc"

# Files at top level that are libraries / this dispatcher, not runnable tools.
SKIP_TOPLEVEL = {"cli.py"}


class Entry:
    __slots__ = ("name", "path", "summary")

    def __init__(self, name: str, path: Path, summary: str):
        self.name = name
        self.path = path
        self.summary = summary


def docstring_summary(path: Path) -> str:
    """First line of the module docstring, or '' if none / unparseable."""
    try:
        tree = ast.parse(path.read_text(encoding="utf-8", errors="replace"))
        doc = ast.get_docstring(tree)
        if doc:
            return doc.strip().splitlines()[0].strip()
    except Exception:
        pass
    return ""


def group_for_toplevel(name: str) -> str:
    """Logical group for a top-level script basename (no .py)."""
    if name in TOPLEVEL_OVERRIDES:
        return TOPLEVEL_OVERRIDES[name]
    for prefix, group in TOPLEVEL_PREFIXES:
        if name.startswith(prefix):
            return group
    for suffix, group in TOPLEVEL_SUFFIXES:
        if name.endswith(suffix):
            return group
    return MISC_GROUP


def build_index() -> dict[str, list[Entry]]:
    """Return {group: [Entry, ...]} over physical subdirs + top-level scripts."""
    index: dict[str, list[Entry]] = {}

    def add(group: str, path: Path):
        index.setdefault(group, []).append(
            Entry(path.stem, path, docstring_summary(path))
        )

    # Tier 1: physical subdir groups.
    for group, directory in PHYSICAL_GROUPS.items():
        for path in sorted(directory.glob("*.py")):
            if path.name == "__init__.py":
                continue
            add(group, path)

    # Tier 2: top-level scripts.
    for path in sorted(PYTHON_DIR.glob("*.py")):
        if path.name in SKIP_TOPLEVEL or path.name == "__init__.py":
            continue
        add(group_for_toplevel(path.stem), path)

    for entries in index.values():
        entries.sort(key=lambda e: e.name)
    return index


# --- resolution ------------------------------------------------------------

class Ambiguous(Exception):
    def __init__(self, name: str, groups: list[str]):
        self.name = name
        self.groups = groups
        super().__init__(f"'{name}' is ambiguous across groups: {', '.join(groups)}")


class NotFound(Exception):
    pass


def resolve_bare(index: dict[str, list[Entry]], name: str) -> Entry:
    """Resolve a unique basename across all groups (no silent first-match)."""
    hits = [(g, e) for g, entries in index.items() for e in entries if e.name == name]
    if not hits:
        raise NotFound(name)
    if len(hits) > 1:
        raise Ambiguous(name, sorted(g for g, _ in hits))
    return hits[0][1]


# --- presentation ----------------------------------------------------------

def _fmt(entry: Entry) -> str:
    summary = entry.summary or "(no docstring)"
    if len(summary) > 70:
        summary = summary[:67] + "..."
    return f"    {entry.name:34s} {summary}"


def print_tree(index: dict[str, list[Entry]]):
    print("Python toolset — `python3 python/cli.py <group> <name> [args]`\n")
    for group in sorted(index):
        entries = index[group]
        tag = " (physical subdir)" if group in PHYSICAL_GROUPS else ""
        print(f"{group}{tag}  [{len(entries)}]")
        for e in entries:
            print(_fmt(e))
        print()
    print("special commands (delegate to existing CLIs):")
    print(f"    {'report':34s} python -m reports <name>|--all|--list")
    print(f"    {'menu':34s} omega_resolver.py menu (what to work on next)")


def print_group(index: dict[str, list[Entry]], group: str):
    print(f"{group}  [{len(index[group])}]")
    for e in index[group]:
        print(_fmt(e))


# --- subprocess dispatch ---------------------------------------------------

def _run(argv: list[str], cwd: Path | None = None) -> int:
    return subprocess.run(argv, cwd=str(cwd) if cwd else None).returncode


def run_script(path: Path, args: list[str]) -> int:
    return _run([sys.executable, str(path), *args])


def run_reports(args: list[str]) -> int:
    # The reports package supports `python -m reports` via reports/__main__.py;
    # it must run with python/ on sys.path, so cwd=PYTHON_DIR.
    return _run([sys.executable, "-m", "reports", *args], cwd=PYTHON_DIR)


def run_menu(args: list[str]) -> int:
    return _run([sys.executable, str(PYTHON_DIR / "omega_resolver.py"), "menu", *args])


# --- selftest (positive control, gate-wired) -------------------------------

def selftest() -> int:
    """Positive-controlled self-check. Exit non-zero on any failure.

    Guards Build-Discipline Patterns 5/6: a glob that silently finds nothing
    must FAIL, not pass green; a known resolution must hold; an ambiguous
    basename must be caught (synthetic planted collision = the positive control).
    """
    failures: list[str] = []

    def check(cond: bool, msg: str):
        status = "ok  " if cond else "FAIL"
        print(f"  [{status}] {msg}")
        if not cond:
            failures.append(msg)

    index = build_index()

    # 1. Each physical group must discover N>0 scripts (absence-satisfies-the-gate guard).
    for group in PHYSICAL_GROUPS:
        n = len(index.get(group, []))
        check(n > 0, f"physical group '{group}' discovers N>0 scripts (found {n})")

    # 2. A known entry point resolves to its expected path.
    try:
        e = resolve_bare(index, "run_pipeline")
        check(
            e.path == PYTHON_DIR / "run_pipeline.py",
            f"run_pipeline resolves to {e.path}",
        )
    except (NotFound, Ambiguous) as exc:
        check(False, f"run_pipeline resolution: {exc}")

    # 3. Ambiguity is detected (synthetic planted collision = positive control).
    synthetic = {
        "alpha": [Entry("dup", PYTHON_DIR / "a.py", "")],
        "beta": [Entry("dup", PYTHON_DIR / "b.py", "")],
    }
    try:
        resolve_bare(synthetic, "dup")
        check(False, "planted basename collision is flagged ambiguous")
    except Ambiguous:
        check(True, "planted basename collision is flagged ambiguous")
    except NotFound:
        check(False, "planted basename collision is flagged ambiguous (got NotFound)")

    # 4. `list` produces non-empty grouped output.
    total = sum(len(v) for v in index.values())
    check(total > 0, f"list emits non-empty grouped output ({total} commands)")

    # Visibility: report misc growth (the graceful-rot vector — not a failure).
    n_misc = len(index.get(MISC_GROUP, []))
    print(f"  [info] {n_misc} script(s) in '{MISC_GROUP}' "
          f"(novel top-level prefixes; extend TOPLEVEL_PREFIXES to group them)")

    if failures:
        print(f"\ncli selftest: FAIL ({len(failures)} failure(s))")
        return 1
    print(f"\ncli selftest: OK ({total} commands across {len(index)} groups)")
    return 0


# --- main ------------------------------------------------------------------

def main(argv: list[str]) -> int:
    index = build_index()

    if not argv or argv[0] == "list":
        print_tree(index)
        return 0

    cmd, rest = argv[0], argv[1:]

    if cmd == "selftest":
        return selftest()
    if cmd == "report":
        return run_reports(rest)
    if cmd == "menu":
        return run_menu(rest)

    # Group dispatch.
    if cmd in index:
        if not rest:
            print_group(index, cmd)
            return 0
        name, script_args = rest[0], rest[1:]
        for e in index[cmd]:
            if e.name == name:
                return run_script(e.path, script_args)
        print(f"No '{name}' in group '{cmd}'. Available: "
              f"{', '.join(e.name for e in index[cmd])}", file=sys.stderr)
        return 1

    # Bare-name dispatch.
    try:
        entry = resolve_bare(index, cmd)
    except Ambiguous as exc:
        print(f"Error: {exc}\nUse: python3 python/cli.py <group> {cmd} [args]",
              file=sys.stderr)
        return 1
    except NotFound:
        print(f"Error: no command or group named '{cmd}'. "
              f"Run `python3 python/cli.py list`.", file=sys.stderr)
        return 1
    return run_script(entry.path, rest)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
