#!/usr/bin/env python3
"""apparatus_instrument.py — the apparatus's own catch-rate + channel instrument.

Two checks, both answering the standing meta-question ("is the verification
apparatus earning its weight?") with data instead of more apparatus
(build_discipline.md → "Don't answer 'does the apparatus pay for itself?' by
producing more apparatus" — this IS the kill-question installed in substrate,
honest move (4) of that section; operator ruling 2026-08-10).

1. CATCH BIT. Every audits/<YYYY-MM-DD>_<slug>/ dated after the adoption date
   must carry, in its WRITEUP.md, exactly one line of the form

       **Fired:** live — <what control fired / claim flipped>
       **Fired:** latent — <defect found but conditional on inputs not produced>
       **Fired:** no

   `live`  = a control fired, a claim flipped, or a verdict a consumer actually
             saw changed (the one-fix-bite sense).
   `latent`= a real defect/hazard found, but conditional on an input the system
             does not currently produce (hardened-before-live).
   `no`    = pure confirmation — nothing fired, nothing flipped.
   Pre-adoption dirs are exempt; a voluntary backfilled Fired: line still counts
   toward the rolling rate. The rate itself is REPORTED, never gated — reading
   it is the operator's seat; the gate only enforces that the bit exists.

2. CHANNEL CAP (finite channel, gated exchange — operator ruling 2026-08-10).
   The memory index's "## Feedback" sections hold at most FEEDBACK_CAP entries,
   the end state of the 2026-08-10 prune. Admitting a new feedback rule requires
   retiring or merging one in the same edit. Over-cap turns the gate RED with
   the exchange message. If the memory index is absent (fresh machine), the
   check SKIPS with a declared line — never a silent pass posing as a check.

--check runs the fixture selftest FIRST, then the live sweep (probe carries its
positive controls on every invocation). Wired into scripts/gate.sh.
"""
from __future__ import annotations

import re
import sys
import tempfile
from datetime import date
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from paths import AUDITS  # noqa: E402

# Dirs dated strictly after this date must carry the Fired: bit.
CATCH_ADOPTION = date(2026, 8, 10)
FEEDBACK_CAP = 33  # end state of the 2026-08-10 memory prune (78 -> 33)
MEMORY_MD = Path.home() / (
    ".claude/projects/-home-scott-bin-structural-dynamics-model/memory/MEMORY.md"
)

NAME_RE = re.compile(r"^(\d{4})-(\d{2})-(\d{2})_.+$")
FIRED_RE = re.compile(r"^\*\*Fired:\*\*\s+(live|latent|no)\b", re.MULTILINE)
FIRED_ANY_RE = re.compile(r"^\*\*Fired:\*\*", re.MULTILINE)


def fired_token(writeup_text: str) -> str | None:
    """The Fired: token in a WRITEUP body, or None if absent/malformed."""
    m = FIRED_RE.search(writeup_text)
    if m:
        return m.group(1)
    return None


def check_catch_bits(root: Path) -> tuple[list[str], dict[str, int]]:
    """(problems, tally live/latent/no over every Fired: line found)."""
    problems: list[str] = []
    tally = {"live": 0, "latent": 0, "no": 0}
    if not root.is_dir():
        return problems, tally
    for d in sorted(root.iterdir()):
        if not d.is_dir():
            continue
        m = NAME_RE.match(d.name)
        if not m:
            continue  # malformed names are audit_writeup_gate's finding, not ours
        try:
            dir_date = date(int(m.group(1)), int(m.group(2)), int(m.group(3)))
        except ValueError:
            continue
        wu = d / "WRITEUP.md"
        text = wu.read_text(encoding="utf-8", errors="replace") if wu.is_file() else ""
        tok = fired_token(text)
        if tok:
            tally[tok] += 1
        elif FIRED_ANY_RE.search(text):
            problems.append(
                f"{d.name}: malformed **Fired:** line (want live|latent|no)"
            )
        elif dir_date > CATCH_ADOPTION:
            problems.append(
                f"{d.name}: WRITEUP.md missing its **Fired:** line "
                f"(required for dirs dated > {CATCH_ADOPTION})"
            )
    return problems, tally


def count_feedback_entries(memory_text: str) -> int:
    """Bullet entries inside '## Feedback*' sections of MEMORY.md."""
    n = 0
    in_feedback = False
    for line in memory_text.splitlines():
        if line.startswith("## "):
            in_feedback = line.startswith("## Feedback")
        elif in_feedback and line.startswith("- "):
            n += 1
    return n


def check_channel(memory_path: Path) -> tuple[list[str], str]:
    """(problems, summary line) for the feedback-channel cap."""
    if not memory_path.is_file():
        return [], f"channel SKIPPED (no memory index at {memory_path})"
    n = count_feedback_entries(memory_path.read_text(encoding="utf-8", errors="replace"))
    if n > FEEDBACK_CAP:
        return [
            f"feedback channel over cap: {n} > {FEEDBACK_CAP} — gated exchange: "
            "retire or merge one feedback memory to admit the new one"
        ], f"channel {n}/{FEEDBACK_CAP} OVER"
    return [], f"channel {n}/{FEEDBACK_CAP}"


def selftest() -> int:
    """Positive controls: each check must FIRE on a planted violation and stay
    quiet on a conforming twin. want=fail/pass pairs per check."""
    failures: list[str] = []

    # -- catch bit: planted-missing must fail; conforming twin must pass -------
    with tempfile.TemporaryDirectory() as td:
        root = Path(td)
        bad = root / "2026-08-12_planted_missing_fired"
        bad.mkdir()
        (bad / "WRITEUP.md").write_text("# writeup with no bit\n")
        good = root / "2026-08-12_planted_conforming"
        good.mkdir()
        (good / "WRITEUP.md").write_text("**Fired:** live — planted control\n")
        mal = root / "2026-08-12_planted_malformed"
        mal.mkdir()
        (mal / "WRITEUP.md").write_text("**Fired:** maybe — bad token\n")
        pre = root / "2026-08-01_planted_preadoption"
        pre.mkdir()
        (pre / "WRITEUP.md").write_text("# exempt, no bit\n")
        problems, tally = check_catch_bits(root)
        if not any("planted_missing_fired" in p for p in problems):
            failures.append("catch-bit control: planted-missing did NOT fire")
        if any("planted_conforming" in p for p in problems):
            failures.append("catch-bit control: conforming twin false-fired")
        if not any("planted_malformed" in p for p in problems):
            failures.append("catch-bit control: malformed token did NOT fire")
        if any("planted_preadoption" in p for p in problems):
            failures.append("catch-bit control: pre-adoption dir was not exempt")
        if tally["live"] != 1:
            failures.append(f"catch-bit tally: want live=1, got {tally}")

    # -- channel cap: over-cap must fail; at-cap must pass; absent must skip ---
    over = "## Feedback — planted\n" + "- entry\n" * (FEEDBACK_CAP + 1)
    at = ("## Feedback — planted\n" + "- entry\n" * FEEDBACK_CAP
          + "## Projects\n" + "- non-feedback entry\n" * 5)
    if count_feedback_entries(over) != FEEDBACK_CAP + 1:
        failures.append("channel control: over-cap count wrong")
    with tempfile.TemporaryDirectory() as td:
        p_over = Path(td) / "over.md"
        p_over.write_text(over)
        probs, _ = check_channel(p_over)
        if not probs:
            failures.append("channel control: over-cap did NOT fire")
        p_at = Path(td) / "at.md"
        p_at.write_text(at)
        probs, _ = check_channel(p_at)
        if probs:
            failures.append("channel control: at-cap false-fired (or counted non-Feedback sections)")
        probs, summary = check_channel(Path(td) / "absent.md")
        if probs or "SKIPPED" not in summary:
            failures.append("channel control: absent index must SKIP declaredly")

    for f in failures:
        print(f"SELFTEST FAIL: {f}")
    return 1 if failures else 0


def main() -> int:
    argv = sys.argv[1:]
    if argv == ["--selftest"]:
        rc = selftest()
        print("apparatus selftest " + ("GREEN" if rc == 0 else "RED"))
        return rc
    if argv and argv != ["--check"]:
        print(__doc__)
        return 2
    # --check (default): selftest rides every run
    if selftest() != 0:
        print("apparatus: selftest RED — live result not trustworthy")
        return 1
    problems, tally = check_catch_bits(AUDITS)
    chan_problems, chan_summary = check_channel(MEMORY_MD)
    problems += chan_problems
    n_bits = sum(tally.values())
    rate = (
        f"catch-rate {tally['live']}L/{tally['latent']}l/{tally['no']}n of {n_bits}"
        if n_bits else "catch-rate: no Fired: bits yet"
    )
    for p in problems:
        print(f"PROBLEM: {p}")
    print(f"apparatus: {rate}; {chan_summary}; "
          + ("RED" if problems else "GREEN"))
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
