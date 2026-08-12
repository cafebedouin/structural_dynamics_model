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


def delivered_fraction(text: str, cap_bytes: int, cap_lines: int) -> float:
    """Fraction of a file that survives truncation. LINE CUT FIRST, THEN BYTES.

    The order is not cosmetic and getting it wrong is not a rounding error. `WEr` does:

        a = lineCount > iJ ? first-iJ-lines : text
        if (a.length > kae) a = a[:kae]

    so a file over BOTH caps is cut to `cap_lines` first, and the byte cap then applies to
    the RESULT -- frequently never binding at all. A byte-only estimate reported
    `feedback_prereg_review_riders.md` (25,373 B, 359 lines) as delivering ~98.5%; it
    actually delivers 60.9%, because the 200-line cap binds and the 373-byte overage never
    applies. This instrument reproduced that exact error until 2026-08-12.
    """
    if not text:
        return 1.0
    total = len(text.encode())
    lines = text.split("\n")
    cut = "\n".join(lines[:cap_lines]) if len(lines) > cap_lines else text
    if len(cut.encode()) > cap_bytes:
        cut = cut.encode()[:cap_bytes].decode("utf-8", errors="ignore")
    return len(cut.encode()) / total


def is_front_loaded(text: str) -> bool:
    """True if the file declares a front_loaded: stamp in its frontmatter.

    NEEDED THE MOMENT THE OQ-290 RULING LANDED, because front-loading DEPRESSES the
    delivered fraction (the file grows; the delivered prefix does not). Without this
    flag the readout reports a successful front-load as a WORSE number than the raw
    file it replaced -- an instrument that inverts the sign of the fix it was built to
    motivate. The declared stamp is the discriminator; a low fraction on a stamped file
    is the design working.
    """
    s = text.lstrip()
    if not s.startswith("---"):
        return False
    end = s.find("\n---", 3)          # close of the frontmatter block
    if end == -1:
        return False
    return "front_loaded:" in s[3:end]


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
            txt = p.read_text(encoding="utf-8", errors="replace")
            n = txt.count("\n") + 1
            if b > cap_bytes or n > cap_lines:
                rows.append((p.name, b, n, delivered_fraction(txt, cap_bytes, cap_lines),
                             is_front_loaded(txt)))
        out[cap_name] = sorted(rows, key=lambda r: r[3])
    n_fl = sum(1 for p in sibs if is_front_loaded(
        p.read_text(encoding="utf-8", errors="replace")))
    printed = [f"delivery (REPORTING ONLY, not gated): {len(sibs)} sibling files, "
               f"{n_fl} front-loaded per the OQ-290 ruling"]
    if n_fl:
        printed.append("  NOTE: for a [front-loaded] file the delivered FRACTION is not a "
                       "health metric — the delivered prefix is self-sufficient by "
                       "construction, so a low number is the design working, not a loss. "
                       "Front-loading LOWERS this percentage on purpose.")
    for cap_name, rows in sorted(out.items()):
        cb, cl = DELIVERY_CAPS[cap_name]
        printed.append(f"  under {cap_name} ({cb} B / {cl} lines): "
                       f"{len(rows)} of {len(sibs)} over cap")
        for name, b, n, frac, fl in rows[:DELIVERY_ROWS]:
            printed.append(f"    {frac:5.0%}  {b:6d} B  {n:4d} ln  "
                           f"{'[front-loaded] ' if fl else ''}{name}")
        if len(rows) > DELIVERY_ROWS:
            printed.append(f"    ... and {len(rows) - DELIVERY_ROWS} more "
                           f"(listing capped at {DELIVERY_ROWS}; the COUNT above is full)")
    return out, printed


#: PASSIVE TRUNCATION TRIPWIRE (2026-08-12, OQ-289). The two delivery paths append
#: DIFFERENT notices, so any truncated memory file that ever arrives ANNOUNCES which path
#: cut it. That needs no designed trigger and no spend — it needs capture during ordinary
#: work, which is the production transport, free and cumulative. It is the passive
#: alternative to building an interactive harness to drive a stochastic relevance selector.
NOTICE_WER = "Only part of it was loaded"          # WEr / kae=25000 / iJ=200 — NO pointer
NOTICE_PIE = "This memory file was truncated"      # PIe / NSp=4096 / Npa=200 — HAS pointer
TRUNCATION_SIGHTINGS = Path(__file__).resolve().parents[1] / "audits" / "truncation_sightings.md"


def check_truncation_tripwire(path: Path) -> tuple[list[str], str]:
    """Report any recorded sighting of a delivery-truncation notice.

    REPORTING ONLY, and it is HALF AN INSTRUMENT BY CONSTRUCTION — stated here because a
    quiet tripwire is exactly the shape that reads as reassurance.

    **No sighting is UNINFORMATIVE.** Smoke run 2 proved that harder than assumed: a
    report can be suppressed by prompt wording even when the content is plainly visible
    (OQ-292, absence-shaped success). So silence here is not evidence that nothing
    truncates. The standing POSITIVE that makes this quiet meaningful is the static
    delivery-fraction readout below — it says how many files are over cap on disk, which
    is a fact about the substrate rather than about anyone's search.

    The pair is the instrument: static readout says *exposure exists*; the tripwire says
    *a truncation was actually observed, and by which path*. Neither alone licenses a
    conclusion.
    """
    if not path.is_file():
        return [], ("truncation tripwire: no sightings file (no truncation notice has been "
                    "recorded; NOT evidence that none occurred — see OQ-292)")
    text = path.read_text(encoding="utf-8", errors="replace")
    wer, pie = text.count(NOTICE_WER), text.count(NOTICE_PIE)
    if not (wer or pie):
        return [], ("truncation tripwire: sightings file present, 0 notices recorded "
                    "(NOT evidence that none occurred — see OQ-292)")
    return [], (f"truncation tripwire: {pie} PIe-path (NSp=4096) / {wer} WEr-path "
                f"(kae=25000) sighting(s) recorded — OQ-289 is answerable from these")


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
        # LINE-CUT-FIRST arithmetic. A byte-only estimate reported the real over-cap
        # file as ~98.5% delivered when it actually delivers 60.9%; this instrument
        # carried that same error until 2026-08-12.
        long_file = "x" * 60 + "\n"
        long_file = long_file * 400            # 400 lines, ~24,400 B
        f_kae = delivered_fraction(long_file, 25000, 200)
        if not (0.49 < f_kae < 0.51):
            failures.append(f"delivery control: line-cut-first arithmetic wrong "
                            f"(400 lines under a 200-line cap should deliver ~50%, "
                            f"got {f_kae:.1%})")
        if delivered_fraction("a" * 10000, 25000, 200) != 1.0:
            failures.append("delivery control: an under-cap file must deliver 100%")
        f_b = delivered_fraction("a" * 10000, 4096, 200)
        if not (0.40 < f_b < 0.42):
            failures.append(f"delivery control: byte cap must bind when lines do not "
                            f"(got {f_b:.1%})")
        # front-load stamp: the readout must not report a successful front-load as a
        # worse number than the raw file it replaced.
        if not is_front_loaded("---\nname: x\nmetadata:\n  front_loaded: 2026-08-12\n---\nbody"):
            failures.append("front-load control: a stamped file was not detected")
        if is_front_loaded("---\nname: x\n---\nbody mentioning front_loaded: later"):
            failures.append("front-load control: the stamp must be in FRONTMATTER, not body")
        if is_front_loaded("no frontmatter at all"):
            failures.append("front-load control: an unstamped file false-fired")
        _, skipped = delivery_report(d / "nonexistent")
        if not any("SKIPPED" in ln for ln in skipped):
            failures.append("delivery control: an absent memory dir must SKIP declaredly")

    # -- truncation tripwire: must distinguish the two paths, and must NEVER report a
    # -- quiet channel as reassurance. Two-sided plus a wording check on the silence.
    with tempfile.TemporaryDirectory() as td:
        d = Path(td)
        _, quiet = check_truncation_tripwire(d / "absent.md")
        if "NOT evidence" not in quiet:
            failures.append("tripwire control: a quiet tripwire must SAY it is not "
                            "evidence of absence (OQ-292) -- silence reads as "
                            "reassurance otherwise")
        empty = d / "empty.md"; empty.write_text("# sightings\n")
        _, s = check_truncation_tripwire(empty)
        if "0 notices" not in s or "NOT evidence" not in s:
            failures.append("tripwire control: an empty sightings file must report 0 AND "
                            "disclaim")
        hit = d / "hit.md"
        hit.write_text(f"seen: {NOTICE_PIE} (4096 byte limit). Use the Read tool\n")
        _, s = check_truncation_tripwire(hit)
        if "1 PIe-path" not in s:
            failures.append("tripwire control: a PIe notice did not register")
        if "0 WEr-path" not in s:
            failures.append("tripwire control: a PIe notice must NOT register as WEr -- "
                            "the whole value is that the paths are distinguishable")
        hit2 = d / "hit2.md"
        hit2.write_text(f"> WARNING: this memory file is 359 lines. {NOTICE_WER}.\n")
        _, s = check_truncation_tripwire(hit2)
        if "1 WEr-path" not in s or "0 PIe-path" not in s:
            failures.append("tripwire control: a WEr notice did not register cleanly")

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
    # The tripwire's PAIR. Printed immediately after the static readout deliberately:
    # the readout is the standing positive that makes a quiet tripwire mean anything.
    _, trip = check_truncation_tripwire(TRUNCATION_SIGHTINGS)
    print("  " + trip)
    print(f"apparatus: {rate}; {chan_summary}; "
          + ("RED" if problems else "GREEN"))
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
