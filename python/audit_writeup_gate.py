#!/usr/bin/env python3
"""audit_writeup_gate.py — machine-check of the audits/ writeup-format convention.

Convention (adopted 2026-08-06, forward-only; spec: audits/README.md → Writeup
format): every audits/<YYYY-MM-DD>_<slug>/ directory dated on or after the
adoption date must carry exactly one WRITEUP.md entry point (uppercase) and no
rival pre-registration spellings (PRE_REGISTRATION.md / PREREG.md — the single
canonical spelling is PREREGISTRATION.md). Directories dated before adoption
are point-in-time documents and exempt. Directory names that do not parse as
<YYYY-MM-DD>_<slug> FAIL (fail-closed: an unparseable name would otherwise
silently escape enforcement — Build Discipline Pattern 5).

--check runs the fixture selftest FIRST, then the live sweep: the probe carries
its positive controls on every invocation, so a clean live result is never a
didn't-look. Wired into scripts/gate.sh.
"""
from __future__ import annotations

import re
import sys
import tempfile
from datetime import date
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from paths import AUDITS  # noqa: E402

ADOPTION = date(2026, 8, 6)
NAME_RE = re.compile(r"^(\d{4})-(\d{2})-(\d{2})_.+$")
RIVAL_PREREG = {"pre_registration.md", "prereg.md"}


def check_dir(d: Path) -> list[str]:
    """Problems for one audit directory (empty list = conforming)."""
    m = NAME_RE.match(d.name)
    if not m:
        return [f"{d.name}: malformed directory name (want <YYYY-MM-DD>_<slug>)"]
    try:
        dir_date = date(int(m.group(1)), int(m.group(2)), int(m.group(3)))
    except ValueError:
        return [f"{d.name}: date component does not parse as a real date"]
    if dir_date < ADOPTION:
        return []  # grandfathered: point-in-time, never retro-edited
    problems = []
    names = {p.name for p in d.iterdir()}
    if "WRITEUP.md" not in names:
        problems.append(f"{d.name}: no WRITEUP.md entry point (required for dirs dated >= {ADOPTION})")
    for n in sorted(names):
        low = n.lower()
        if low == "writeup.md" and n != "WRITEUP.md":
            problems.append(f"{d.name}: rival-case entry point {n} (canonical is WRITEUP.md)")
        if low in RIVAL_PREREG:
            problems.append(f"{d.name}: rival pre-registration spelling {n} (canonical is PREREGISTRATION.md)")
    return problems


def check_root(root: Path) -> tuple[int, int, list[str]]:
    """(n_dirs, n_enforced, problems) over one audits root."""
    n_dirs = n_enforced = 0
    problems: list[str] = []
    for d in sorted(root.iterdir()):
        if not d.is_dir():
            continue  # top-level files (README.md) are not audit dirs
        n_dirs += 1
        ps = check_dir(d)
        m = NAME_RE.match(d.name)
        if ps or (m and _parses(m) and _date_of(m) >= ADOPTION):
            n_enforced += 1
        problems.extend(ps)
    return n_dirs, n_enforced, problems


def _parses(m: re.Match) -> bool:
    try:
        _date_of(m)
        return True
    except ValueError:
        return False


def _date_of(m: re.Match) -> date:
    return date(int(m.group(1)), int(m.group(2)), int(m.group(3)))


def selftest() -> None:
    """Fixture controls: assert the checker flags each violation shape and
    passes each conforming/grandfathered shape. Raises on any miss."""
    with tempfile.TemporaryDirectory() as td:
        root = Path(td)
        adoption = ADOPTION.isoformat()

        def mk(dirname: str, *files: str) -> Path:
            d = root / dirname
            d.mkdir()
            for f in files:
                (d / f).write_text("x\n")
            return d

        # positive controls — each MUST be flagged
        must_flag = [
            mk(f"{adoption}_missing_writeup", "FINDINGS.md"),
            mk(f"{adoption}_rival_prereg", "WRITEUP.md", "PRE_REGISTRATION.md"),
            mk(f"{adoption}_prereg_short", "WRITEUP.md", "PREREG.md"),
            mk(f"{adoption}_lowercase_only", "writeup.md"),
            mk("notadate_dir", "WRITEUP.md"),
            mk("2026-13-01_bad_date", "WRITEUP.md"),
        ]
        # negative controls — each MUST pass
        must_pass = [
            mk(f"{adoption}_conforming", "WRITEUP.md", "PREREGISTRATION.md", "RECON.md"),
            mk("2026-06-01_grandfathered", "FINDINGS.md"),
        ]
        for d in must_flag:
            if not check_dir(d):
                raise AssertionError(f"selftest: checker FAILED to flag {d.name}")
        for d in must_pass:
            ps = check_dir(d)
            if ps:
                raise AssertionError(f"selftest: false positive on {d.name}: {ps}")
    n_controls = len(must_flag) + len(must_pass)
    print(f"selftest: OK ({n_controls} fixture controls: {len(must_flag)} flagged, {len(must_pass)} clean)")


def main(argv: list[str]) -> int:
    mode = argv[1] if len(argv) > 1 else "--check"
    if mode == "--selftest":
        selftest()
        return 0
    if mode != "--check":
        print(__doc__)
        return 2
    selftest()  # controls ride with every live check
    n_dirs, n_enforced, problems = check_root(AUDITS)
    for p in problems:
        print(f"PROBLEM: {p}")
    status = "OK" if not problems else "RED"
    print(f"audit writeup gate: {status} ({n_dirs} dirs, {n_enforced} enforced, {len(problems)} problems)")
    return 0 if not problems else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
