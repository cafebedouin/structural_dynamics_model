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

3. DELIVERY FRACTION — **REPORTING ONLY, DELIBERATELY NOT ENFORCING** (2026-08-12,
   ISSUES OQ-289/OQ-290). The channel cap above is an ATTENTION cap: it bounds
   what a fresh instance must hold. The memory files it produces are delivered
   through a second channel with a per-file DELIVERY cap, and the two pull in
   opposite directions — consolidating for attention is what created exposure to
   the delivery limit (amnesiac_institution_v0_6.md §8.5).

   This prints the delivered-fraction table beside the channel cap. It does NOT
   gate, for two reasons and both are load-bearing:

     (a) WHICH CAP BINDS IS UNSETTLED. The harness carries two candidate constant
         pairs, and they disagree by a factor of nineteen about how many files are
         affected. Enforcing either would encode an unwitnessed prediction into an
         instrument. OQ-289 is the run that settles it.
     (b) A CHECK RED BY CONSTRUCTION AT INTRODUCTION TEACHES THE INSTITUTION TO
         ROUTE AROUND IT. Enforcing today would go red on nineteen files the moment
         it landed and stay red until an Ω_P ruling nobody has scheduled — §2.6's
         green check inverted, and no better.

   PROMOTE TO ENFORCING WHEN OQ-290 LANDS, with the cap OQ-289 witnessed. Until
   then this is a readout, and it is labelled as one everywhere it prints.
   No spend-bearing probe belongs in scripts/gate.sh; this reads the filesystem.

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
MEMORY_DIR = Path.home() / (
    ".claude/projects/-home-scott-bin-structural-dynamics-model/memory"
)
MEMORY_MD = MEMORY_DIR / "MEMORY.md"

#: Candidate per-file DELIVERY caps for recalled memory (OQ-289). REPORTING ONLY —
#: which pair binds is exactly what OQ-289 runs to settle, so both are printed and
#: neither is enforced. Recorded as (bytes, lines), harness 2.1.229.
DELIVERY_CAPS = {"NSp": (4096, 200), "kae": (25000, 200)}
#: How many rows of the over-cap table to print. The count is ALWAYS printed in full;
#: only the per-file listing is capped, and the elision is stated rather than silent.
DELIVERY_ROWS = 5

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


def delivery_report(memory_dir: Path) -> tuple[dict, list[str]]:
    """Per-file delivered fraction against each CANDIDATE cap. REPORTING ONLY.

    Returns ({cap_name: [(name, bytes, lines, delivered_fraction), ...]}, lines_to_print).
    Returns no problems by construction — this is a readout, not a gate, and it says so
    in its own output. See module docstring item 3 for why that is deliberate and what
    would license promoting it.

    MEMORY.md is excluded from the sibling census: it is the always-loaded index and
    travels the other path. Counting it as a sibling is how "20 of 54" got reported for
    what is 19 of 53 siblings — a small conflation, and exactly the kind this instrument
    exists to stop repeating.
    """
    if not memory_dir.is_dir():
        return {}, [f"delivery SKIPPED (no memory dir at {memory_dir})"]
    sibs = sorted(p for p in memory_dir.glob("*.md") if p.name != "MEMORY.md")
    out: dict = {}
    for cap_name, (cap_bytes, cap_lines) in DELIVERY_CAPS.items():
        rows = []
        for p in sibs:
            b = p.stat().st_size
            n = p.read_text(encoding="utf-8", errors="replace").count("\n") + 1
            if b > cap_bytes or n > cap_lines:
                rows.append((p.name, b, n, min(1.0, cap_bytes / b) if b else 1.0))
        out[cap_name] = sorted(rows, key=lambda r: r[3])
    printed = [f"delivery (REPORTING ONLY, not gated — OQ-289 settles which cap binds; "
               f"promote to enforcing when OQ-290 lands): {len(sibs)} sibling files"]
    for cap_name, rows in sorted(out.items()):
        cb, cl = DELIVERY_CAPS[cap_name]
        printed.append(f"  under {cap_name} ({cb} B / {cl} lines): "
                       f"{len(rows)} of {len(sibs)} over cap")
        for name, b, n, frac in rows[:DELIVERY_ROWS]:
            printed.append(f"    {frac:5.0%}  {b:6d} B  {n:4d} ln  {name}")
        if len(rows) > DELIVERY_ROWS:
            printed.append(f"    ... and {len(rows) - DELIVERY_ROWS} more "
                           f"(listing capped at {DELIVERY_ROWS}; the COUNT above is full)")
    return out, printed


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

    # -- delivery readout: must SELECT the over-cap files and EXCLUDE MEMORY.md ---
    # Two-sided per cap: a file over the small cap but under the large one must appear
    # in exactly one of the two tables. A readout that reported the same set under both
    # would look like a working instrument while measuring nothing cap-specific.
    with tempfile.TemporaryDirectory() as td:
        d = Path(td)
        (d / "MEMORY.md").write_text("x" * 90_000)          # must be EXCLUDED
        (d / "small.md").write_text("x" * 1_000)            # under both caps
        (d / "mid.md").write_text("x" * 10_000)             # over NSp, under kae
        (d / "big.md").write_text("x" * 30_000)             # over both
        (d / "longlines.md").write_text("y\n" * 300)        # over both LINE caps
        tables, printed = delivery_report(d)
        nsp = {r[0] for r in tables["NSp"]}
        kae = {r[0] for r in tables["kae"]}
        if "MEMORY.md" in nsp | kae:
            failures.append("delivery control: MEMORY.md must be EXCLUDED from the "
                            "sibling census (it travels the always-loaded path)")
        if "small.md" in nsp | kae:
            failures.append("delivery control: an under-cap file false-fired")
        if "mid.md" not in nsp:
            failures.append("delivery control: a file over NSp did NOT appear")
        if "mid.md" in kae:
            failures.append("delivery control: a file UNDER kae appeared in the kae "
                            "table — the two caps are not being applied separately")
        if not ({"big.md", "longlines.md"} <= nsp & kae):
            failures.append("delivery control: files over BOTH caps must appear in both")
        if "longlines.md" not in kae:
            failures.append("delivery control: the LINE cap is not being applied")
        if not any("REPORTING ONLY" in ln for ln in printed):
            failures.append("delivery control: the readout must label itself "
                            "REPORTING ONLY wherever it prints")
        _, skipped = delivery_report(d / "nonexistent")
        if not any("SKIPPED" in ln for ln in skipped):
            failures.append("delivery control: an absent memory dir must SKIP declaredly")

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
    # Reporting-only readout. Contributes NOTHING to the return code by design —
    # see module docstring item 3.
    _, delivery_lines = delivery_report(MEMORY_DIR)
    for ln in delivery_lines:
        print(ln)
    print(f"apparatus: {rate}; {chan_summary}; "
          + ("RED" if problems else "GREEN"))
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
