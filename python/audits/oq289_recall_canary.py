#!/usr/bin/env python3
"""OQ-289 recall-channel canary driver — does an over-limit memory file arrive truncated?

STATUS 2026-08-12 — **NO MODEL CALL HAS EVER BEEN MADE BY THIS DRIVER.** `payloads/` and
`responses/` do not exist yet. The `--live` path is structurally refused until a dated
audit dir carries a frozen PREREGISTRATION.md whose md5 sits above the first-result
sentinel in audit_log.md.

If you change this driver's spend state, change this header IN THE SAME COMMIT. The
predecessor driver's header read "NO MODEL CALL HAS EVER BEEN MADE" for a week after a
219-call run — every word true when written, every word false the moment the run
happened, in the file that guards against exactly that.

======================================================================================
WHAT THIS MEASURES, AND WHY THE OBVIOUS VERSION OF IT MEASURES NOTHING
======================================================================================
OQ-286 asked whether the ALWAYS-LOADED instruction set is silently truncated. It is not:
`CLAUDE.md` is 91,029 B against a documented limit of 4,194,304 B (46x headroom), and
above that limit the harness SKIPS THE FILE WHOLE and logs a telemetry event rather than
tail-truncating. A canary at the end of `CLAUDE.md` would have arrived every time, under
a perfectly valid freeze, carrying no information. OQ-286 is retracted; this is OQ-289.

The channel that plausibly truncates today is PER-TURN RECALLED MEMORY. Sibling files
under the project's memory dir arrive as `relevant_memories` attachments, and the binary
carries two candidate constant pairs that disagree by a factor of nineteen about live
exposure:

    NSp = 4096 B / Npa = 200 lines   (PIe, truncateOnByteLimit) -> 19 of 53 files over
    kae = 25000 B / iJ  = 200 lines  (WEr, and note WEr(s.content,"memory") exists) -> 1 of 53

WHICH PAIR GOVERNS IS UNSETTLED, and the constants are recorded in the pre-registration
as PREDICTIONS TO BE FALSIFIED, not as findings. Binary strings witness shipped code; a
canary witnesses the path actually taken. If the run contradicts them, the run wins.

======================================================================================
GATE THE OUTPUT, NOT ONLY THE INPUT
======================================================================================
Gates 0/0b/1/2/3 are INPUT gates. A driver can pass every one of them and persist
nothing: the predecessor did, for 219 calls. Gate 4 is their mirror and is deliberately
stronger than a count, because a count alone passes when every file is written empty.

  - Raw stdout is persisted BEFORE any parsing. The text is the datum; the verdict is
    derived. A parse bug then degrades to RECOVERABLE rather than total.
  - Write-then-verify is PER UNIT, not per run: a run that dies at unit 40 leaves 40
    recoverable answers instead of zero.
  - Counts come from `glob` over the artifacts, never from `len(results)` — a count
    sourced from the loop is a claim about persistence sourced from the thing that is
    not persistence.

======================================================================================
THE ARMS
======================================================================================
A       THRESHOLD. Scratch memory files just-over and just-under each candidate cap,
        fresh 16-hex canaries at START / MIDDLE / END, tokens required VERBATIM.
        THE JUST-OVER/JUST-UNDER PAIR IS THE DISCRIMINATION RECORD: same instrument,
        same path, one fires and one declines, and the decline is forced by a documented
        constant rather than authored. Line count varies independently of bytes (separates
        Npa from NSp); batch size varies (per-file vs per-batch); both brackets run
        (settles which pair governs).

A_PRIME BEHAVIOURAL, same round trip. Canary in the TRUNCATED TAIL of a scratch copy of
        a real over-limit file, with `Read` RE-ENABLED and the canary placed where only a
        pointer-follow reaches it. Delivery-only runs cannot touch the question that
        decides the disposition: does an instance follow the `Read` pointer, or does the
        truncated fragment read as complete guidance? Measured as an OBSERVED TOOL CALL,
        never a self-report. This is the arm OQ-290 waits on.

INJECT  REPORTING-CHANNEL CONTROL. An END canary delivered via `--append-system-prompt`
        with none in the file. Proves the model CAN and WILL emit a verbatim 16-hex token
        that is genuinely in context. Without it an all-ABSENT run is ambiguous between
        "the harness dropped the payload" and "the model won't echo a random-looking
        token" — and that ambiguity would void the expensive verdict.

LEAK    UNINTENDED-PAYLOAD-PATH CONTROL, and its purpose is RESTATED from the usual one.
        It does NOT guard the false positive the OQ names: a fresh 16-hex token forecloses
        inference at 2^-64, so this control cannot fire on the threat that justification
        cites, and a reviewer who spots the mismatch discounts the whole control
        discipline. Its real function is to witness a canary reaching context by a path
        nobody intended — environment, prompt assembly, or a tool call that `--tools ""`
        did not actually suppress. Kept for that, labelled for that. It also supplies
        BASELINE (see below).

Arm B (a `CLAUDE.md` size ladder) IS DELIBERATELY ABSENT — 46x headroom. That absence is
OQ-286's retraction, not an oversight.

======================================================================================
PRIMARY INSTRUMENT = HARNESS-SIDE TOKEN SLOPE, NOT THE MODEL'S WORD
======================================================================================
    delivered = input_tokens + cache_creation_input_tokens + cache_read_input_tokens
    slope     = d(delivered) / (d(bytes)/BYTES_PER_TOKEN)

All three components, summed. `cache_read_input_tokens == 0` is ASSERTED per unit —
nonzero means isolation failed and the count is corrupt, so that rung is VOID rather
than reported.

Self-report and slope answer different questions and are both required, because under
the slope instrument alone `TRUNCATED` and `ATTACHMENT NEVER FIRED` are indistinguishable
— both plateau. Two discriminators are pre-registered and they do different jobs:

    START canary          separates TRUNCATED from the all-ABSENT rows
    delivered vs BASELINE separates ATTACHMENT_NEVER_FIRED from DROPPED

======================================================================================
ISOLATION — `--add-dir` IS AN INSTRUCTION-INJECTION CHANNEL, BY DEFAULT, EVERY UNIT
======================================================================================
`CLAUDE_CODE_ADDITIONAL_DIRECTORIES_CLAUDE_MD=1` is in `~/.claude/settings.json` as of
2026-08-12 and was verified by a three-arm before/after test. It is ON, GLOBALLY, and it
gates `.claude/CLAUDE.md` and `.claude/rules/` in the added directory as well as the
top-level `CLAUDE.md`. Under a token-slope primary instrument that is an uncontrolled
payload landing IN THE EXACT QUANTITY BEING MEASURED. So:

  - assert per unit that no CLAUDE.md / .claude/CLAUDE.md / .claude/rules/ exists under
    any scratch or added directory;
  - pin `~/.claude/settings.json`'s md5 in the prereg and assert it unchanged per unit,
    ON THE SAME FOOTING as the live `CLAUDE.md` guard. Context assembly is now a function
    of two files and only one of them was being watched.

Do NOT relocate `CLAUDE_CONFIG_DIR` — `~/.credentials.json` lives under it and moving it
likely breaks auth, producing a failure unrelated to the hypothesis. Use a fresh scratch
cwd, which is enough: the harness keys the memory dir off the cwd, so a scratch cwd gets
its OWN, EMPTY memory dir (verified 2026-08-12 against the `--add-dir` test fixture's
leftover project dir, which has a memory/ holding 0 files).

DEVIATION FROM THE PLAN, STATED RATHER THAN SILENTLY SUBSTITUTED. The plan asks that
"every write path is under outdir/scratch/". That is not achievable: the harness derives
the memory dir from the cwd under ~/.claude/projects/, so the payload MUST be written
there for the channel under test to see it. The substituted guard is stronger where it
matters — it pins the LIVE dir rather than the scratch one:
  (a) the derived project key must differ from the live project's key;
  (b) the derived key must be one this driver computed from a scratch cwd;
  (c) the live memory dir's full manifest (names + per-file md5) must be byte-identical
      before and after every unit.
A guard that watches the scratch dir would pass while the live dir burned.

Usage:
  python3 python/audits/oq289_recall_canary.py --selftest
  python3 python/audits/oq289_recall_canary.py --dry-run
  python3 python/audits/oq289_recall_canary.py --smoke      # 1 live call, settles 3 unknowns
  python3 python/audits/oq289_recall_canary.py --live       # refused without the freeze
"""
from __future__ import annotations
import argparse, ast, glob, hashlib, json, os, pathlib, re, secrets, shutil, subprocess
import sys, tempfile
from datetime import datetime, timezone

_here = pathlib.Path(__file__).resolve()
_root = next(c for c in (_here, *_here.parents) if (c / "pyproject.toml").is_file())
sys.path.insert(0, str(_root / "python"))
from paths import REPO_ROOT, AUDITS  # noqa: E402

# --- pinned environment (0b asserts these; prereg records them) ---------------------
PINNED_CLI_VERSION = "2.1.229"
MODEL = "claude-sonnet-5"
K = 3
BYTES_PER_TOKEN = 3.8

# --- the two candidate constant pairs, as PREDICTIONS ------------------------------
#: (label, byte cap, line cap). Recorded to be falsified. If the run contradicts a row,
#: the run wins: these witness shipped code, not the path taken.
CAPS = {
    "NSp": (4096, 200),      # PIe / relevant_memories, truncateOnByteLimit
    "kae": (25000, 200),     # WEr, incl. the WEr(s.content,"memory") call site
}
#: Just-under / just-over deltas around each cap. The PAIR is the discrimination record.
BRACKET_DELTA = 512

SLOPE_PASS_THROUGH = 0.80
SLOPE_PLATEAU = 0.10
#: Tolerance on BASELINE. BASELINE itself is not a constant: it is MEASURED on the LEAK
#: arm (a unit with no memory payload) and the pre-registration declares the METHOD plus
#: this epsilon, so `elevated` is a frozen rule rather than a judgement made at analysis
#: time. That is the whole reason rows 3 and 4 can be disjoint.
BASELINE_EPS_TOKENS = 50.0

CANARY_RE = re.compile(r"\b[0-9a-f]{16}\b")
IMPORT_RE = re.compile(r"@[A-Za-z0-9_./-]+")

#: SECONDARY DISCRIMINATOR, added 2026-08-12 after reading the full `WEr` body.
#: The two truncation paths append DIFFERENT notices, so a truncated file SAYS which
#: path cut it. That is a direct, self-identifying answer to underdetermined question
#: (a) — far stronger than inferring the governing pair from where a threshold lands.
#:
#:   WEr  -> "\n\n> WARNING: this memory file is <what> . Only part of it was loaded.
#:            Keep each memory file focused on one topic."          NO Read pointer.
#:   PIe  -> "This memory file was truncated (<N> byte limit | first <N> lines).
#:            Use the Read tool to view the complete file at: <path>"   HAS a pointer.
#:
#: The asymmetry is load-bearing for OQ-290: **under WEr there is no pointer at all**,
#: so "accept truncate-plus-pointer" is not an available disposition on that branch,
#: and Arm A′'s question (does an instance follow the pointer?) is only well-posed on
#: the PIe branch. Recorded here so the run cannot quietly answer a question that does
#: not arise.
NOTICE_WER = "Only part of it was loaded"
NOTICE_PIE = "This memory file was truncated"
#: WEr's notice also names the AXIS that fired: byte-only, line-only, or both.
NOTICE_AXIS = {
    "line_only": re.compile(r"\d+ lines \(limit: \d+\)"),
    "byte_only": re.compile(r"\(limit: [^)]+\) — its lines are too long"),
    "both": re.compile(r"\d+ lines and "),
}

LIVE_PROJECT_KEY = "-home-scott-bin-structural-dynamics-model"
LIVE_MEMORY_DIR = pathlib.Path.home() / ".claude" / "projects" / LIVE_PROJECT_KEY / "memory"
LIVE_CLAUDE_MD = REPO_ROOT / "CLAUDE.md"
LIVE_SETTINGS = pathlib.Path.home() / ".claude" / "settings.json"

#: Where the pre-registration is STAGED before run day. It must be MOVED (not copied)
#: into the dated audit dir at freeze time — a copy leaves two live versions of the
#: frozen document with no queryable fact of canonicity, which is Pattern 2 on the
#: freeze itself. assert_spend_go() refuses if the staging file still exists.
PREREG_STAGING = pathlib.Path(__file__).resolve().parent / "oq289_prereg_draft.md"
#: Smoke's own scope declaration. Smoke deliberately does NOT require the frozen prereg
#: — it runs first, by ruling — but "no freeze" must not mean "no stated scope". This
#: file states what smoke may and may not conclude, and its md5 is persisted with every
#: smoke artifact, so the scope is on the record BEFORE the probe rather than after.
SMOKE_SCOPE = pathlib.Path(__file__).resolve().parent / "oq289_smoke_scope.md"
AUDIT_SLUG = "oq289_recall_canary"
SENTINEL = "<!--OQ289-FIRST-RESULT-->"

#: Filler vocabulary. Every word contains at least one letter outside [0-9a-f] and is
#: <= 15 chars, so NO 16-hex run can form even across a word boundary (spaces are not
#: hex, and no single word is long enough or hex enough). No '@' anywhere, because the
#: loader follows @-imports and would add an uncontrolled file to the payload. Both
#: properties are ASSERTED after generation, not merely intended — a construction-time
#: guarantee that is never checked is the same shape as a gate over an empty table.
FILLER_WORDS = ("the", "quiet", "ledger", "of", "records", "keeps", "its", "scope",
                "wherever", "the", "witness", "is", "not", "yet", "spent", "or",
                "promoted", "into", "the", "always", "loaded", "stack")


def md5(s: str) -> str:
    return hashlib.md5(s.encode("utf-8")).hexdigest()


def md5_file(p: pathlib.Path) -> str:
    return hashlib.md5(p.read_bytes()).hexdigest()


def now() -> str:
    return datetime.now(timezone.utc).isoformat()


# ---------------------------------------------------------------------------
# Payload construction
# ---------------------------------------------------------------------------
def mint_canary() -> str:
    """A fresh 16-hex token, per unit, never reused and never placed in the prompt.

    Freshness is what forecloses inference: the model cannot guess a token it has never
    seen at 2^-64, so a verbatim hit is delivery and not reconstruction. This is also
    exactly why the LEAK arm cannot fire on the false positive usually cited for it —
    see the module docstring.
    """
    return secrets.token_hex(8)


def build_filler(target_bytes: int, target_lines: int) -> str:
    """Filler of EXACTLY target_bytes across EXACTLY target_lines, clean by assertion.

    Byte length is exact because gate 0 checks the ladder against declared lengths: a
    prereg naming 11 rungs while 4 exist on disk looks exactly like a freeze, and an
    md5 over it would be a success-shaped token.

    Byte count and line count are set INDEPENDENTLY, which is the point: the line axis has
    to vary without the byte axis moving, or `Npa` and `NSp` stay confounded and a file
    over both caps cannot say which one fired.

    Method: lay down exactly `target_bytes` ASCII characters of filler, then overwrite
    `target_lines - 1` evenly spaced characters with newlines. Byte count is exact by
    construction (overwriting is length-preserving) and line count is exact by counting.
    An earlier version padded and trimmed only the LAST line, which could not reach a
    high line count inside a small byte budget — gate 0 caught it on the line-axis rungs.
    """
    if target_lines < 1:
        raise ValueError("target_lines must be >= 1")
    if target_bytes < max(target_lines, 1):
        raise ValueError(f"target_bytes={target_bytes} too small for "
                         f"target_lines={target_lines}")
    reps = target_bytes // (sum(len(w) + 1 for w in FILLER_WORDS)) + 2
    stream = (" ".join(FILLER_WORDS) + " ") * reps
    chars = list(stream[:target_bytes])
    step = target_bytes // target_lines
    for i in range(1, target_lines):
        chars[i * step] = "\n"
    text = "".join(chars)
    if len(text.encode()) != target_bytes:
        raise AssertionError(f"filler is {len(text.encode())} B, wanted {target_bytes}")
    if text.count("\n") + 1 != target_lines:
        raise AssertionError(f"filler has {text.count(chr(10)) + 1} lines, "
                             f"wanted {target_lines}")
    assert_filler_clean(text)
    return text


def assert_filler_clean(text: str) -> None:
    """Two properties the payload must have, CHECKED rather than assumed.

    A 16-hex run in the filler would be indistinguishable from a delivered canary at the
    read site; an @-import would add an uncontrolled file to the exact quantity the slope
    instrument is measuring.
    """
    stray = CANARY_RE.findall(text)
    if stray:
        raise AssertionError(f"filler contains {len(stray)} 16-hex run(s): {stray[:3]} — "
                             f"indistinguishable from a delivered canary at the read site")
    imports = IMPORT_RE.findall(text)
    if imports:
        raise AssertionError(f"filler contains @-import(s): {imports[:3]} — the loader "
                             f"follows these and would add uncontrolled payload")


def build_memory_file(nbytes: int, nlines: int, canaries: dict) -> str:
    """A scratch memory file with canaries at START / MIDDLE / END.

    START is not merely one leg of a two-sided pair: it is NAMED IN THE PREREG AS THE
    ATTACHMENT-FIRED CONTROL. Under the slope instrument, `truncated` and `attachment
    never fired` both plateau; START is what separates them on the self-report side.
    """
    for slot in ("start", "middle", "end"):
        if slot not in canaries:
            raise AssertionError(f"canary slot {slot!r} missing — the three-slot layout "
                                 f"is what makes TRUNCATED separable from DROPPED")
    start_line = f"CANARY-START {canaries['start']}"
    mid_line = f"CANARY-MIDDLE {canaries['middle']}"
    end_line = f"CANARY-END {canaries['end']}"
    fixed = len(start_line) + len(mid_line) + len(end_line)
    # Three canary lines join into the body with 3 extra newlines; the body supplies the
    # remaining nlines-3 lines. Solving for the body budget gives an EXACT total, which
    # gate 0 then re-checks off the filesystem rather than trusting this arithmetic.
    body_lines = nlines - 3
    body_bytes = nbytes - fixed - 3
    if body_lines < 1 or body_bytes < body_lines:
        raise ValueError(f"nbytes={nbytes}/nlines={nlines} leaves no room for a body "
                         f"after the three canary lines")
    body = build_filler(body_bytes, body_lines).split("\n")
    half = len(body) // 2
    parts = [start_line] + body[:half] + [mid_line] + body[half:] + [end_line]
    text = "\n".join(parts)
    if len(text.encode()) != nbytes:
        raise AssertionError(f"memory file is {len(text.encode())} B, wanted {nbytes}")
    if text.count("\n") + 1 != nlines:
        raise AssertionError(f"memory file has {text.count(chr(10)) + 1} lines, "
                             f"wanted {nlines}")
    return text


# ---------------------------------------------------------------------------
# The ladder
# ---------------------------------------------------------------------------
def build_ladder() -> list[dict]:
    """Every unit the run will make, with its declared byte/line length.

    Rungs come in JUST-UNDER / JUST-OVER pairs around each candidate cap. The pair is
    the discrimination record — a control that only ever fires shows the instrument CAN
    fire; the witness that its firing carries information is a case it DECLINED, and here
    the decline is forced by a documented constant rather than authored by us.
    """
    units: list[dict] = []
    for cap_name, (cap_bytes, cap_lines) in sorted(CAPS.items()):
        for side, nbytes in (("under", cap_bytes - BRACKET_DELTA),
                             ("over", cap_bytes + BRACKET_DELTA)):
            units.append({"arm": "A", "rung": f"{cap_name}_bytes_{side}",
                          "cap": cap_name, "axis": "bytes", "side": side,
                          "bytes": nbytes, "lines": 40, "batch": 1})
        # Line axis varied INDEPENDENTLY of bytes. Without this, Npa and NSp are
        # confounded: a file over both caps cannot say which one fired.
        for side, nlines in (("under", cap_lines - 20), ("over", cap_lines + 20)):
            units.append({"arm": "A", "rung": f"{cap_name}_lines_{side}",
                          "cap": cap_name, "axis": "lines", "side": side,
                          "bytes": cap_bytes - BRACKET_DELTA, "lines": nlines, "batch": 1})
    # Batch axis: per-file vs per-recall-batch. Same per-file size, more files.
    units.append({"arm": "A", "rung": "batch_x4", "cap": "NSp", "axis": "batch",
                  "side": "under", "bytes": CAPS["NSp"][0] - BRACKET_DELTA,
                  "lines": 40, "batch": 4})
    units.append({"arm": "A_PRIME", "rung": "pointer_follow", "cap": "NSp",
                  "axis": "behaviour", "side": "over",
                  "bytes": CAPS["NSp"][0] + 8192, "lines": 60, "batch": 1})
    units.append({"arm": "INJECT", "rung": "append_system_prompt", "cap": None,
                  "axis": "control", "side": None, "bytes": 0, "lines": 0, "batch": 0})
    units.append({"arm": "LEAK", "rung": "no_payload", "cap": None,
                  "axis": "control", "side": None, "bytes": 0, "lines": 0, "batch": 0})
    return units


#: Smoke payload size. Chosen to be FAR under every candidate constant on both axes
#: (4,096 / 25,000 bytes; 200 lines) so the probe cannot carry threshold information.
SMOKE_BYTES = 512
SMOKE_LINES = 10
#: A distinct marker prefix so smoke artifacts can never be confused with run canaries.
SMOKE_PREFIX = "SMOKE-MARKER"


def build_smoke_units() -> list[dict]:
    """The feasibility probe. IT IS NOT A RUNG OF THE LADDER, and that is deliberate.

    ORDERING (operator ruling 2026-08-12): smoke runs BEFORE the pre-registration is
    frozen, because smoke settles whether Arm A is runnable at all. Freezing a prereg
    that names an unrunnable test forces an amendment, and an amended freeze is a weaker
    instrument than one frozen a day later.

    That reordering is only legitimate if smoke carries NO INFORMATION ABOUT THE
    HYPOTHESIS, so:

      - the payload is 512 B / 10 lines — far under every candidate constant on both
        axes, so nothing about it bears on WHERE truncation begins;
      - it carries ONE marker with a distinct prefix, not a START/MIDDLE/END triple, so
        there is no position signal to read;
      - the question asked is **does the attachment arrive**, never **how much of it**.

    Without that discipline, §7.5's seeded-draw problem eats the first real run: a
    prompt written after seeing a threshold result is a seeded draw, and "run it again"
    is unavailable in its usual form.

    THE TOOLS/NO-TOOLS PAIR IS ONE DISCRIMINATOR. If the no-tools arm declines while the
    tools arm fires, `--tools ""` suppresses the recall attachment and Arm A as designed
    would return a null that means nothing.

    THREE ARMS AS OF THE 2026-08-12 RE-DESIGN. The first smoke ran two arms and returned
    0/3 on BOTH — a null that, as run, was UNINTERPRETABLE, because nothing in it showed
    the memory channel could deliver anything at all. "I didn't find it" is a fact about
    the search until the search is shown to find. Two confounds were live:

      - the scratch memory dir contained ONLY the sibling file, with **no `MEMORY.md`
        index**, while the live dir has one. Recall is plausibly index-driven (`WEr`'s
        own default tag is "index"), so there may have been nothing to select FROM;
      - `relevant_memories` is relevance-selected per turn, and the smoke prompt had no
        semantic overlap with the payload's filler.

    SMOKE_INDEX is the positive control that makes a 0 mean something. Its marker goes
    in the scratch `MEMORY.md` itself — the ALWAYS-LOADED path, not the attachment path
    — at a size far under every candidate constant, so it still carries no threshold
    information. It splits the null that was previously unreadable:

        INDEX fires, siblings do not  -> the ATTACHMENT path specifically is the problem
        INDEX also does not fire      -> the memory subsystem is not engaging under -p;
                                         the transport is wrong, not the flag
    """
    common = {"cap": None, "axis": "smoke", "side": None,
              "bytes": SMOKE_BYTES, "lines": SMOKE_LINES, "batch": 1}
    return [
        {"arm": "SMOKE_NOTOOLS", "rung": "smoke_notools", **common},
        {"arm": "SMOKE_TOOLS", "rung": "smoke_tools", **common},
        {"arm": "SMOKE_INDEX", "rung": "smoke_index", **common},
    ]


#: Mirrors the live memory dir's structure. Without an index the recall system may have
#: nothing to select from — the confound that made the first smoke's null unreadable.
SMOKE_INDEX_TEMPLATE = """# Scratch Memory Index

One line per memory; content lives in the files.

## Feedback — scratch
- [{stem}]({stem}.md) — scratch probe file for OQ-289 feasibility
"""


def build_smoke_file(nbytes: int, nlines: int, marker: str) -> str:
    """One marker, one small file. No position structure to read."""
    head = f"{SMOKE_PREFIX} {marker}"
    body = build_filler(nbytes - len(head) - 1, nlines - 1)
    return head + "\n" + body


def expected_calls(units: list[dict]) -> int:
    return len(units) * K


# ---------------------------------------------------------------------------
# Isolation
# ---------------------------------------------------------------------------
def project_key(cwd: pathlib.Path) -> str:
    """The harness's cwd -> ~/.claude/projects/<key> transform.

    Verified 2026-08-12 against two live dirs: `/home/scott/bin/structural_dynamics_model`
    -> `-home-scott-bin-structural-dynamics-model`, and the `--add-dir` test fixture's
    scratch path. Non-alphanumerics collapse to '-'.
    """
    return re.sub(r"[^A-Za-z0-9]", "-", str(cwd))


def memory_dir_for(cwd: pathlib.Path) -> pathlib.Path:
    return pathlib.Path.home() / ".claude" / "projects" / project_key(cwd) / "memory"


def live_manifest() -> dict:
    """Names + per-file md5 of the LIVE memory dir. The thing that must not change."""
    if not LIVE_MEMORY_DIR.is_dir():
        return {}
    return {p.name: md5_file(p) for p in sorted(LIVE_MEMORY_DIR.glob("*.md"))}


def assert_isolation(scratch_cwd: pathlib.Path, baseline: dict, errors: list) -> None:
    """Per-unit isolation. Every clause here has cost a real result somewhere.

    Ordered cheapest-to-most-expensive, but ALL of them run — an early return would make
    the later clauses vacuous, and a gate that stops at the first hit reports one problem
    where there are three.
    """
    key = project_key(scratch_cwd)
    if key == LIVE_PROJECT_KEY:
        errors.append(f"isolation: scratch cwd resolves to the LIVE project key {key!r}. "
                      f"The payload would be written into the live memory dir.")
    # The instruction-injection channel. --add-dir ingests these as Project-tier content,
    # landing an uncontrolled payload in the exact quantity the slope is measuring.
    for probe in ("CLAUDE.md", ".claude/CLAUDE.md", ".claude/rules"):
        p = scratch_cwd / probe
        if p.exists():
            errors.append(f"isolation: stray {probe} under the scratch cwd ({p}). "
                          f"CLAUDE_CODE_ADDITIONAL_DIRECTORIES_CLAUDE_MD=1 is ON globally, "
                          f"so this manufactures a PASS_THROUGH slope with nothing to do "
                          f"with recall.")
    # Compare ONLY the memory-file portion. `baseline` also carries the two `__`-prefixed
    # md5 pins, which live_manifest() does not produce — comparing the dicts whole made
    # this clause fire unconditionally, i.e. a gate that can never pass, which is as
    # uninformative as one that can never fail. Caught by the CONVERSE control below.
    want_mem = {k: v for k, v in baseline.items() if not k.startswith("__")}
    if live_manifest() != want_mem:
        errors.append("isolation: the LIVE memory dir manifest changed. Hard abort — "
                      "the run is writing where it must never write.")
    if LIVE_CLAUDE_MD.exists() and md5_file(LIVE_CLAUDE_MD) != baseline.get("__claude_md__"):
        errors.append("isolation: live CLAUDE.md md5 changed mid-run. Hard abort.")
    if LIVE_SETTINGS.exists() and md5_file(LIVE_SETTINGS) != baseline.get("__settings__"):
        errors.append("isolation: ~/.claude/settings.json md5 changed mid-run. Hard abort — "
                      "context assembly is a function of TWO files and this is the one "
                      "that was not being watched.")


def assert_removable_project_dir(d: pathlib.Path) -> None:
    """Refuse to remove anything that is not a scratch project dir this run created.

    A full run mints one ~/.claude/projects/<key> per unit (36 of them) and they are
    litter outside outdir if nobody removes them. Cleanup is therefore owed — but a
    cleanup that can reach the LIVE project dir is a far worse defect than the litter,
    so the removal is guarded and the guard is two-sided in the selftest.
    """
    root = pathlib.Path.home() / ".claude" / "projects"
    if d.resolve().parent != root.resolve():
        raise AssertionError(f"refusing to remove {d}: not a direct child of {root}")
    if d.name == LIVE_PROJECT_KEY:
        raise AssertionError(f"refusing to remove the LIVE project dir {d}")
    if "oq289" not in d.name:
        raise AssertionError(f"refusing to remove {d}: not an oq289 scratch project dir")


def cleanup_scratch_project_dirs(created: list) -> int:
    n = 0
    for d in created:
        try:
            assert_removable_project_dir(d)
        except AssertionError as e:
            print(f"  cleanup skipped: {e}")
            continue
        shutil.rmtree(d, ignore_errors=True)
        n += 1
    return n


def isolation_baseline() -> dict:
    b = live_manifest()
    b["__claude_md__"] = md5_file(LIVE_CLAUDE_MD) if LIVE_CLAUDE_MD.exists() else None
    b["__settings__"] = md5_file(LIVE_SETTINGS) if LIVE_SETTINGS.exists() else None
    return b


# ---------------------------------------------------------------------------
# Spend gates
# ---------------------------------------------------------------------------
def cli_version() -> str:
    try:
        out = subprocess.run(["claude", "--version"], capture_output=True, text=True,
                             timeout=30).stdout
    except Exception as e:                                              # noqa: BLE001
        return f"<unavailable: {e}>"
    m = re.search(r"(\d+\.\d+\.\d+)", out)
    return m.group(1) if m else out.strip()


def gate_0_ladder(units: list[dict], payload_dir: pathlib.Path, errors: list) -> None:
    """Every declared rung has a payload on disk at EXACTLY its declared byte length.

    An md5 over a pre-registration naming 11 rungs while 4 exist on disk looks exactly
    like a freeze and passes every check below it. Checked before the md5, for the same
    reason the predecessor checks leg-completeness before the md5.
    """
    print("\n  [gate 0] ladder — declared rungs vs payloads on disk, at exact length")
    for u in units:
        if u["arm"] in ("INJECT", "LEAK"):
            continue                                   # no file payload by design
        for b in range(u["batch"]):
            p = payload_dir / u["rung"] / f"mem_{b:02d}.md"
            if not p.exists():
                errors.append(f"gate 0: rung {u['rung']} file {b} has no payload on disk")
                continue
            actual = p.stat().st_size
            if actual != u["bytes"]:
                errors.append(f"gate 0: rung {u['rung']} file {b} is {actual} B, "
                              f"declared {u['bytes']} B")
    if not errors:
        print(f"           OK — {len(units)} declared rungs, every payload at declared length")


def gate_0b_environment(errors: list) -> None:
    """Pin the CLI version AND the config. Five versions shipped in six days.

    A result attributed to 2.1.229 but produced by 2.1.230 is unattributable and
    unnoticeable after the fact. `~/.claude/settings.json` is pinned on the same footing
    as the live `CLAUDE.md`, because context assembly is now a function of both.
    """
    print("\n  [gate 0b] environment — CLI version and config md5 against the freeze")
    v = cli_version()
    print(f"            claude --version = {v}   pinned = {PINNED_CLI_VERSION}")
    if v != PINNED_CLI_VERSION:
        errors.append(f"gate 0b: CLI is {v}, pre-registration pins {PINNED_CLI_VERSION}. "
                      f"Refusing — a result attributed to the wrong version is "
                      f"unattributable and unnoticeable after the fact.")
    if not LIVE_SETTINGS.exists():
        errors.append("gate 0b: ~/.claude/settings.json is absent; its md5 cannot be pinned")
    else:
        print(f"            settings.json md5 = {md5_file(LIVE_SETTINGS)}")


def audit_dir_for(date_str: str) -> pathlib.Path:
    return AUDITS / f"{date_str}_{AUDIT_SLUG}"


def assert_spend_go(audit: pathlib.Path) -> None:
    """Refuse the live path until the freeze ordering physically holds on disk."""
    if PREREG_STAGING.exists():
        sys.exit(f"REFUSED: the staging pre-registration still exists at {PREREG_STAGING}. "
                 f"MOVE it into {audit}/PREREGISTRATION.md — do not copy. Two live copies "
                 f"of a frozen document with no queryable fact of canonicity is Pattern 2 "
                 f"on the freeze itself.")
    prereg = audit / "PREREGISTRATION.md"
    log = audit / "audit_log.md"
    if not prereg.exists():
        sys.exit(f"REFUSED: {prereg} does not exist. The spend-go is at prereg freeze, "
                 f"not at plan approval.")
    want = md5(prereg.read_text())
    text = log.read_text() if log.exists() else ""
    if want not in text:
        sys.exit(f"REFUSED: the current PREREGISTRATION.md md5 ({want}) is not recorded in "
                 f"{log}. Freeze it first.")
    if SENTINEL not in text:
        sys.exit(f"REFUSED: audit_log.md carries no {SENTINEL} marker, so 'the md5 is above "
                 f"the first result line' cannot be checked. A boundary that cannot be "
                 f"located is not a boundary — and a check that cannot find its own input "
                 f"passes VACUOUSLY.")
    if text.index(want) > text.index(SENTINEL):
        sys.exit("REFUSED: the prereg md5 is recorded BELOW the first-result sentinel. The "
                 "ordering is the entire point of the freeze.")


# ---------------------------------------------------------------------------
# Transport
# ---------------------------------------------------------------------------
def assert_smoke_go() -> str:
    """Smoke's gate. Weaker than the freeze BY RULING, not by omission — and it still
    requires a stated scope on disk before the probe, whose md5 is persisted with the
    artifacts. Returns that md5.

    Smoke must NOT require PREREGISTRATION.md: the whole point of the reordering is that
    smoke settles whether the prereg's Arm A is runnable. Requiring the freeze here would
    reinstate exactly the ordering the ruling removed.
    """
    if not SMOKE_SCOPE.exists():
        sys.exit(f"REFUSED: {SMOKE_SCOPE} does not exist. Smoke runs without the freeze "
                 f"by ruling, but 'no freeze' is not 'no stated scope' — write what this "
                 f"probe may and may not conclude BEFORE running it.")
    text = SMOKE_SCOPE.read_text()
    for required in ("feasibility", "may NOT conclude"):
        if required not in text:
            sys.exit(f"REFUSED: {SMOKE_SCOPE} does not state {required!r}. The scope "
                     f"declaration is the only thing separating a feasibility probe from "
                     f"an unregistered first look at the hypothesis.")
    return md5(text)


def build_argv(unit: dict, scratch_cwd: pathlib.Path, prompt: str,
               session_id: str, budget: float) -> list[str]:
    """The exact command line. Built here so the selftest can assert its shape without
    spending — a transport whose argv is assembled inline can only be checked by running
    it."""
    argv = ["claude", "-p", prompt,
            "--model", MODEL,
            "--output-format", "json",
            "--session-id", session_id,
            "--no-session-persistence",
            "--max-budget-usd", str(budget)]
    if unit["arm"] in ("A_PRIME", "SMOKE_TOOLS"):
        # A' MUST re-enable Read: the canary sits where only a pointer-follow reaches it,
        # and the measurement is the observed tool call, not the answer.
        # SMOKE_TOOLS is the paired arm that makes SMOKE_NOTOOLS' decline readable.
        argv += ["--tools", "Read"]
    else:
        argv += ["--tools", ""]
    if unit["arm"] == "INJECT":
        argv += ["--append-system-prompt",
                 f"CANARY-END {unit['inject_canary']}"]
    return argv


def live_transport(argv: list[str], cwd: pathlib.Path, timeout: int = 300) -> dict:
    """One real call. Returns the RAW record; no parsing of the answer happens here.

    Reached only after assert_spend_go(). stdout is captured verbatim and persisted by
    the caller BEFORE anything reads it.
    """
    proc = subprocess.run(argv, cwd=str(cwd), capture_output=True, text=True,
                          timeout=timeout)
    return {"rc": proc.returncode, "stdout": proc.stdout, "stderr": proc.stderr,
            "argv": argv, "cwd": str(cwd), "at": now()}


def stub_transport(argv: list[str], cwd: pathlib.Path, timeout: int = 300) -> dict:
    """Deterministic canned transport. Exercises everything that can actually be wrong
    at this stage — payload assembly, isolation asserts, persistence, the count gates,
    slope arithmetic, verdict partitioning — none of which needs the network.

    Deliberately deterministic rather than random: a stubbed run must be reproducible,
    and luck-driven variation would make the k=3 bookkeeping pass or fail by chance.
    """
    prompt = argv[argv.index("-p") + 1] if "-p" in argv else ""
    body = {"type": "result", "subtype": "success", "is_error": False,
            "result": "STUB no canaries visible",
            "usage": {"input_tokens": 1000 + len(prompt) // 4,
                      "cache_creation_input_tokens": 0,
                      "cache_read_input_tokens": 0}}
    return {"rc": 0, "stdout": json.dumps(body), "stderr": "", "argv": argv,
            "cwd": str(cwd), "at": now()}


# ---------------------------------------------------------------------------
# Persistence — raw first, always
# ---------------------------------------------------------------------------
def write_response(resp_dir: pathlib.Path, unit: dict, k: int, rec: dict) -> pathlib.Path:
    """Persist ONE raw response and verify it landed, BEFORE the next call issues.

    Raw stdout is the datum; every verdict is derived from it. A run that dies at unit 40
    leaves 40 recoverable answers instead of zero. Verifying at the end would have the
    same failure profile as not verifying at all, for every call that never got made.
    """
    d = resp_dir / unit["arm"]
    d.mkdir(parents=True, exist_ok=True)
    p = d / f"{unit['rung']}__k{k}.json"
    p.write_text(json.dumps({"unit": unit, "k": k, **rec}, ensure_ascii=False) + "\n")
    if not p.exists() or p.stat().st_size == 0:
        sys.exit(f"ABORT: response for {unit['rung']} k={k} did not land at {p}. Halting "
                 f"before the next call — a run that cannot persist must not keep spending.")
    return p


def parse_usage(stdout: str) -> dict | None:
    """delivered = input + cache_creation + cache_read. All three, summed.

    Returns None rather than a plausible zero when the shape is not found: a default-on-
    empty here would make "the harness reported nothing" byte-identical to "the harness
    reported a small number", which is the absorption this whole run exists to avoid.
    """
    try:
        obj = json.loads(stdout)
    except Exception:                                                   # noqa: BLE001
        return None
    u = obj.get("usage") or (obj.get("message") or {}).get("usage")
    if not isinstance(u, dict):
        return None
    keys = ("input_tokens", "cache_creation_input_tokens", "cache_read_input_tokens")
    if not any(k in u for k in keys):
        return None
    vals = {k: u.get(k, 0) for k in keys}
    vals["delivered"] = sum(int(v or 0) for v in vals.values())
    return vals


def notice_path(text: str) -> str | None:
    """Which truncation path cut this file, read off its own appended notice.

    Returns "WEr" | "PIe" | None. None means NO truncation notice was seen, which is
    NOT the same as "not truncated" — the notice may simply not have been echoed by the
    model. Never coerce None to "untruncated"; it is reported as its own value.
    """
    if NOTICE_PIE in text:
        return "PIe"
    if NOTICE_WER in text:
        return "WEr"
    return None


def notice_axis(text: str) -> str | None:
    """Which AXIS fired, per WEr's own message. Only meaningful when path == 'WEr'."""
    for axis, rx in NOTICE_AXIS.items():
        if rx.search(text):
            return axis
    return None


def observed_tool_calls(stdout: str) -> int | None:
    """Count tool_use blocks. Used two ways and they are opposite.

    On `--tools ""` arms ANY tool call voids the unit — the suppression was assumed, not
    verified, and an unsuppressed Read could have fetched the canary off disk.
    On A_PRIME a tool call is THE MEASUREMENT: pointer-following observed rather than
    self-reported.
    """
    try:
        obj = json.loads(stdout)
    except Exception:                                                   # noqa: BLE001
        return None
    return json.dumps(obj).count('"type": "tool_use"') + json.dumps(obj).count('"type":"tool_use"')


# ---------------------------------------------------------------------------
# Gates on the output side
# ---------------------------------------------------------------------------
def gate_1_payloads(payload_dir: pathlib.Path, expected: int, errors: list) -> int:
    """COUNT FIRST. A capture bug that writes zero payloads yields a perfectly clean
    sweep and a green everything — a success-shaped absence, which is the exact defect
    class this run codes for. Counted from glob, never from the loop."""
    got = glob.glob(str(payload_dir / "*" / "*.md"))
    print(f"\n  [gate 1] payloads on disk = {len(got)}   expected = {expected}")
    if len(got) != expected:
        errors.append(f"gate 1: {len(got)} payload files on disk, expected {expected}. "
                      f"A clean sweep over a short capture is a success-shaped absence.")
    return len(got)


def gate_2_clean(payload_dir: pathlib.Path, minted: set, errors: list) -> None:
    """Every 16-hex run on disk is one we minted, and every payload is @-free.

    The converse direction matters as much as the forward one: an unminted token in a
    payload means the filler generator regressed, and every ABSENT in the run would then
    be uninterpretable.
    """
    print("\n  [gate 2] payload sweep — @-free, and every 16-hex run is one we minted")
    for path in sorted(glob.glob(str(payload_dir / "*" / "*.md"))):
        text = pathlib.Path(path).read_text()
        for tok in CANARY_RE.findall(text):
            if tok not in minted:
                errors.append(f"gate 2: unminted 16-hex token {tok} in {path}")
        for imp in IMPORT_RE.findall(text):
            errors.append(f"gate 2: @-import {imp} in {path} — the loader follows these")
    if not errors:
        print("           OK")


def gate_3_responses(resp_dir: pathlib.Path, expected: int, errors: list) -> None:
    """The mirror of gate 1, on the OUTPUT side, and deliberately stronger than a count.

    Count alone passes when every file is written empty. So: present, non-empty, parses,
    and carries a usage block. Out-of-vocabulary values are REPORTED, never coerced, and
    this runs after every response is on disk — a failure here is a finding with its
    evidence retained, not a second loss.
    """
    print(f"\n  [gate 3] OUTPUT SIDE — persisted responses vs expected calls")
    files = glob.glob(str(resp_dir / "*" / "*.json"))
    print(f"           persisted = {len(files)}   expected = {expected}")
    if len(files) != expected:
        errors.append(f"gate 3: persisted {len(files)} responses, expected {expected}. "
                      f"Gate the output, not only the input — a pipeline verified "
                      f"end-to-end on what it CONSUMES can produce nothing and report green.")
    empty, no_usage = [], []
    for f in files:
        if os.path.getsize(f) == 0:
            empty.append(os.path.basename(f)); continue
        try:
            rec = json.loads(open(f).read())
        except Exception:                                               # noqa: BLE001
            empty.append(os.path.basename(f)); continue
        if not (rec.get("stdout") or "").strip():
            empty.append(os.path.basename(f)); continue
        if parse_usage(rec["stdout"]) is None:
            no_usage.append(os.path.basename(f))
    if empty:
        errors.append(f"gate 3: {len(empty)} response file(s) empty or unparseable "
                      f"({empty[:5]}). A file count alone passes on zero-byte writes.")
    if no_usage:
        errors.append(f"gate 3: {len(no_usage)} response(s) carry no usage block "
                      f"({no_usage[:5]}) — the PRIMARY instrument has no input there. "
                      f"Reported, never defaulted to zero.")
    if not (len(files) != expected or empty or no_usage):
        print("           OK — every call left a non-empty, usage-bearing record on disk")


# ---------------------------------------------------------------------------
# Instrument
# ---------------------------------------------------------------------------
def slope(d_delivered: float, d_bytes: float) -> float | None:
    """Δdelivered / (Δbytes / BYTES_PER_TOKEN). None when Δbytes is 0 — a slope over a
    zero denominator is undefined, and returning 0.0 there would read as PLATEAU, which
    is a verdict."""
    if d_bytes == 0:
        return None
    return d_delivered / (d_bytes / BYTES_PER_TOKEN)


def slope_band(s: float | None) -> str:
    if s is None:
        return "undefined"
    if s >= SLOPE_PASS_THROUGH:
        return "PASS_THROUGH"
    if s < SLOPE_PLATEAU:
        return "PLATEAU"
    return "PARTIAL"


def classify(unit_reports: list[dict], baseline: float, eps: float) -> str:
    """The pre-registered outcome table. ROWS PARTITION — they do not merely cover.

    Total-over-the-space is NOT sufficient: if two rows can both be true, the analyst
    picks between two valid readings AFTER seeing data, which is precisely the discretion
    the freeze exists to remove. Evaluated IN ORDER; the first matching row wins, and
    every row's condition is mutually exclusive with those above it.

    FABRICATED is deliberately ABSENT here. It is a HALT, not a verdict — a unit emitting
    a regex-matching token that was never minted has broken the self-report channel, and
    the breakage is NOT LOCAL to that unit: it discredits every ABSENT and every verbatim
    hit in the run.
    """
    rcs = [r.get("rc", 0) for r in unit_reports]
    errs = " ".join((r.get("stderr") or "") for r in unit_reports).lower()
    if any(rc != 0 for rc in rcs) and ("input too long" in errs or "too long" in errs
                                       or "context" in errs and "limit" in errs):
        return "LOUD_REFUSAL"

    bands = [r.get("slope_band") for r in unit_reports]
    seen = [set(r.get("seen", [])) for r in unit_reports]
    all_absent = all(not s for s in seen)
    any_verbatim = any(s for s in seen)
    delivered = [r.get("delivered") for r in unit_reports if r.get("delivered") is not None]
    elevated = bool(delivered) and (sum(delivered) / len(delivered)) > baseline + eps

    if all(b == "PLATEAU" for b in bands) and any_verbatim:
        return "CONTRADICTION"          # instrument is wrong; refuse to render a verdict
    if all_absent and not elevated:
        return "ATTACHMENT_NEVER_FIRED"  # the payload never arrived at all
    if all_absent and elevated:
        return "DROPPED"                 # it arrived and its content did not survive
    if all(s == {"start", "middle", "end"} for s in seen):
        return "DELIVERED"
    if all("start" in s and "end" not in s for s in seen):
        return "TRUNCATED"
    return "BOUNDARY"                    # its own row, never coerced into a neighbour


HALTS = {
    "LEAK_FIRED": "leak arm reported any canary -> run VOID (unintended payload path)",
    "INJECT_SILENT": "inject arm failed to echo verbatim in >=1 of 3 -> run VOID "
                     "(the reporting channel is broken; every ABSENT is uninterpretable)",
    "FABRICATED": "a unit emitted a regex-matching token that was never minted -> run VOID "
                  "(self-report channel broken, and the breakage is not local)",
    #: REPLACED 2026-08-12 by the first smoke. `cache_read_input_tokens == 0` is
    #: UNSATISFIABLE under this transport: the CLI caches the system prompt, and all six
    #: smoke units returned cache_read of 3,289 / 4,479 with input_tokens = 2. As
    #: specified, that HALT would have voided EVERY rung of the real run — a gate that
    #: cannot pass, which is as uninformative as one that cannot fail.
    #:
    #: The isolation worry it encoded was cross-unit contamination of `delivered`. The
    #: smoke supplies a satisfiable and strictly better check for that: with the payload
    #: held identical across k, `delivered` must be IDENTICAL across k. It was, exactly
    #: (9,002 x3 and 10,262 x3, zero variance), and the 1,260-token gap between the two
    #: arms is precisely the Read tool definitions — so the instrument is demonstrably
    #: sensitive at the scale the run needs. cache_read is legitimately delivered context
    #: and stays inside `delivered`; it is reported, not gated.
    "DELIVERED_UNSTABLE_ACROSS_K": "delivered varies across k for an IDENTICAL payload -> "
                                   "that rung VOID (the count is not a function of the "
                                   "payload, so no slope over it means anything)",
    "CLAUDE_MD_CHANGED": "live CLAUDE.md md5 changed -> hard abort",
    "SETTINGS_CHANGED": "~/.claude/settings.json md5 changed -> hard abort",
    "STRAY_INSTRUCTION_FILE": "CLAUDE.md/.claude/CLAUDE.md/.claude/rules under a scratch or "
                              "added dir -> hard abort",
    "TOOL_CALL_ON_SUPPRESSED_ARM": "tool call observed on a --tools '' arm -> unit VOID",
}


def analyze(reports: list[dict], units: list[dict]) -> dict:
    """Attach a slope band to every A-arm report and assign each rung its verdict.

    Deliberately done HERE and not left to the writeup. A verdict computed by hand from
    reports.json is a verdict computed after seeing the data, which is exactly the
    discretion the freeze exists to remove; and an instrument the driver never calls is
    an orphan (see orphaned_controls — this function is what wires slope/classify).

    BASELINE is measured on the LEAK arm, which is the unit with no memory payload. If
    the LEAK arm produced no usable delivered count, BASELINE is None and the two
    all-ABSENT rows CANNOT be separated — that is reported as such, never defaulted.
    """
    leak = [r["delivered"] for r in reports
            if r["arm"] == "LEAK" and r.get("delivered") is not None]
    baseline = (sum(leak) / len(leak)) if leak else None

    by_rung: dict = {}
    for r in reports:
        by_rung.setdefault(r["rung"], []).append(r)
    rung_bytes = {u["rung"]: u["bytes"] for u in units}
    rung_group = {u["rung"]: (u.get("cap"), u.get("axis")) for u in units}

    # Slope is between ADJACENT RUNGS in the same (cap, axis) family — a slope computed
    # across families would compare a byte ladder against a line ladder.
    mean_delivered = {}
    for rung, rs in by_rung.items():
        vals = [r["delivered"] for r in rs if r.get("delivered") is not None]
        mean_delivered[rung] = (sum(vals) / len(vals)) if vals else None
    families: dict = {}
    for rung, grp in rung_group.items():
        if grp[0] is not None:
            families.setdefault(grp, []).append(rung)
    slopes: dict = {}
    for grp, rungs in families.items():
        ordered = sorted(rungs, key=lambda r: rung_bytes.get(r, 0))
        for lo, hi in zip(ordered, ordered[1:]):
            if mean_delivered.get(lo) is None or mean_delivered.get(hi) is None:
                slopes[hi] = None
                continue
            slopes[hi] = slope(mean_delivered[hi] - mean_delivered[lo],
                               rung_bytes[hi] - rung_bytes[lo])
    for r in reports:
        r["slope"] = slopes.get(r["rung"])
        r["slope_band"] = slope_band(r["slope"]) if r["rung"] in slopes else "undefined"

    verdicts = {}
    for rung, rs in by_rung.items():
        if baseline is None:
            verdicts[rung] = "UNSEPARABLE_NO_BASELINE"
            continue
        verdicts[rung] = classify(rs, baseline, BASELINE_EPS_TOKENS)
    return {"baseline": baseline, "eps": BASELINE_EPS_TOKENS,
            "slopes": slopes, "verdicts": verdicts}


def check_halts(reports: list[dict], minted: set) -> list[str]:
    """Numeric HALTs, evaluated over persisted reports. Returns the fired HALT names."""
    fired = []
    for r in reports:
        arm = r.get("arm")
        seen = set(r.get("seen", []))
        if arm == "LEAK" and seen:
            fired.append("LEAK_FIRED")
        # A_PRIME and SMOKE_TOOLS have tools ON by design — there a tool call is the
        # measurement (A_PRIME) or the paired condition (SMOKE_TOOLS), not a violation.
        if arm not in ("A_PRIME", "SMOKE_TOOLS") and (r.get("tool_calls") or 0) > 0:
            fired.append("TOOL_CALL_ON_SUPPRESSED_ARM")
        for tok in r.get("tokens_emitted", []):
            if tok not in minted:
                fired.append("FABRICATED")
    # Stability of `delivered` across k, per rung, at identical payload. Replaces the
    # unsatisfiable cache_read==0 assertion; see the HALTS entry for why.
    by_rung: dict = {}
    for r in reports:
        if r.get("delivered") is not None:
            by_rung.setdefault(r["rung"], []).append(r["delivered"])
    for rung, vals in by_rung.items():
        if len(vals) > 1 and len(set(vals)) > 1:
            fired.append("DELIVERED_UNSTABLE_ACROSS_K")
    inject = [r for r in reports if r.get("arm") == "INJECT"]
    if inject and sum(1 for r in inject if "end" not in set(r.get("seen", []))) >= 1:
        fired.append("INJECT_SILENT")
    return sorted(set(fired))


# ---------------------------------------------------------------------------
# Wiring witness
# ---------------------------------------------------------------------------
def orphaned_controls(src: str | None = None) -> list[str]:
    """Guarded functions that NOTHING outside the selftest calls.

    The selftest exercises the FUNCTION; this exercises the WIRING. A control must
    witness that it is CALLED, not only that it works — the predecessor kept four green
    selftest lines for two functions `run()` had stopped calling: code correct,
    assertions would have fired, selftests real, wired to nothing.

    That is worse than a red light in one specific respect. A red light recruits
    attention, while green lines from a disconnected control are indistinguishable from
    connected ones AND add to the control count, so control count rises while coverage
    falls.

    Orphaning arrives through REPAIRS: a replacement lands and the old control stays,
    because removal was not part of the fix. Minting has a constituency; retirement does
    not. This function is the forcing mechanism.

    `orphaned_controls` itself is deliberately NOT guarded: it is a selftest instrument
    by design and its wiring witness is that --selftest fails without it. Stated here
    rather than silently excluded — an exemption nobody wrote down is how the always-
    loaded canary got aimed at a channel with 46x headroom.
    """
    tree = ast.parse(src if src is not None else pathlib.Path(__file__).read_text())
    guarded_prefixes = ("gate_", "assert_", "check_halts", "write_response",
                        "parse_usage", "observed_tool_calls", "classify", "slope",
                        "build_argv", "isolation_baseline", "memory_dir_for")
    top = [n for n in tree.body if isinstance(n, ast.FunctionDef)]
    guarded = [n.name for n in top if n.name.startswith(guarded_prefixes)]

    def calls_within(node) -> set:
        return {s.func.id for s in ast.walk(node)
                if isinstance(s, ast.Call) and isinstance(s.func, ast.Name)}

    callers: dict = {}
    for fn in top:
        for callee in calls_within(fn):
            callers.setdefault(callee, set()).add(fn.name)
    for node in tree.body:
        if not isinstance(node, (ast.FunctionDef, ast.ClassDef)):
            for callee in calls_within(node):
                callers.setdefault(callee, set()).add("<module>")
    return sorted(n for n in guarded
                  if callers.get(n, set()) <= {"selftest", "orphaned_controls"})


# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------
PROMPT_DELIVERY = (
    "Report VERBATIM every token you can see in your context that matches the pattern "
    "CANARY-<SLOT> <16 hex characters>. Output one line per token, in the form "
    "SLOT=<token>. If you can see none, output exactly NONE. Do not guess, do not "
    "reconstruct, and do not describe what such a token would look like — only report "
    "tokens actually present."
)
PROMPT_SMOKE = (
    "Report VERBATIM any token you can see in your context that appears after the label "
    "SMOKE-MARKER. Output exactly one line, in the form MARKER=<token>. If you can see "
    "none, output exactly NONE. Do not guess and do not reconstruct — only report a "
    "token actually present."
)
PROMPT_POINTER = (
    "Your recalled memory may include a file that was truncated, with a note pointing at "
    "the full file on disk. Answer this question completely: what does the memory file "
    "say under the heading FINAL RULE? If the information is not in what you were given, "
    "use the tools available to obtain it."
)


def run(mode: str, outdir: pathlib.Path, budget: float) -> int:
    """mode in {dry, stub, smoke, live}. Only `live` and `smoke` spend."""
    # ORDERING (operator ruling 2026-08-12): SMOKE RUNS BEFORE THE FREEZE, because smoke
    # settles whether Arm A is runnable at all; a prereg naming an unrunnable test would
    # have to be amended, and an amended freeze is weaker than one frozen a day later.
    # `live` still checks the freeze BEFORE any work — building a ladder and spending on
    # it is the most natural thing in the world and would put a result on disk first.
    smoke_scope_md5 = None
    if mode == "live":
        assert_spend_go(audit_dir_for(datetime.now(timezone.utc).strftime("%Y-%m-%d")))
    if mode == "smoke":
        smoke_scope_md5 = assert_smoke_go()
        print(f"  smoke scope md5 = {smoke_scope_md5} "
              f"({SMOKE_SCOPE.relative_to(REPO_ROOT)})")

    if mode == "smoke":
        units = build_smoke_units()
    else:
        units = build_ladder()
    exp_calls = expected_calls(units)
    payload_dir, resp_dir = outdir / "payloads", outdir / "responses"
    payload_dir.mkdir(parents=True, exist_ok=True)
    resp_dir.mkdir(parents=True, exist_ok=True)
    errors: list = []
    baseline_iso = isolation_baseline()
    minted: set = set()

    print(f"OQ-289 recall canary — mode={mode}")
    print(f"  units={len(units)}  k={K}  expected calls={exp_calls}")
    print(f"  outdir={outdir}")

    # --- build every payload BEFORE any call, so gate 0 can check the whole ladder ---
    for u in units:
        if u["arm"] in ("INJECT", "LEAK"):
            u["inject_canary"] = mint_canary()
            minted.add(u["inject_canary"])
            continue
        d = payload_dir / u["rung"]
        d.mkdir(parents=True, exist_ok=True)
        u["canaries"] = []
        for b in range(u["batch"]):
            if u["arm"].startswith("SMOKE"):
                # ONE marker, no position structure. See build_smoke_units().
                marker = mint_canary()
                minted.add(marker)
                u["canaries"].append({"smoke": marker})
                (d / f"mem_{b:02d}.md").write_text(
                    build_smoke_file(u["bytes"], u["lines"], marker))
                continue
            cans = {slot: mint_canary() for slot in ("start", "middle", "end")}
            minted.update(cans.values())
            u["canaries"].append(cans)
            (d / f"mem_{b:02d}.md").write_text(
                build_memory_file(u["bytes"], u["lines"], cans))

    gate_0_ladder(units, payload_dir, errors)
    gate_0b_environment(errors)
    gate_1_payloads(payload_dir, sum(u["batch"] for u in units), errors)
    gate_2_clean(payload_dir, minted, errors)

    if errors:
        print("\nGATES RED — refusing to proceed:")
        for e in errors:
            print(f"  - {e}")
        return 1

    if mode == "dry":
        print("\n--dry-run: gates green, ladder built, NOTHING SENT.")
        print("  NOTE: a --dry-run does not witness the write path. One live call must "
              "land a parseable response file before the sweep is authorized (--smoke).")
        return 0

    transport = stub_transport if mode == "stub" else live_transport
    todo = units
    reports: list = []
    minted_project_dirs: list = []
    for u in todo:
        for k in range(1, K + 1):
            scratch = outdir / "scratch" / f"{u['rung']}__k{k}" / "work"
            scratch.mkdir(parents=True, exist_ok=True)
            memdir = memory_dir_for(scratch)
            iso_errors: list = []
            assert_isolation(scratch, baseline_iso, iso_errors)
            if iso_errors:
                print("\nISOLATION RED — hard abort:")
                for e in iso_errors:
                    print(f"  - {e}")
                return 2
            if u["arm"] not in ("INJECT", "LEAK"):
                memdir.mkdir(parents=True, exist_ok=True)
                minted_project_dirs.append(memdir.parent)
                stems = []
                for b in range(u["batch"]):
                    stem = f"scratch_{u['rung']}_{b:02d}"
                    stems.append(stem)
                    shutil.copy(payload_dir / u["rung"] / f"mem_{b:02d}.md",
                                memdir / f"{stem}.md")
                if u["arm"] == "SMOKE_INDEX":
                    # POSITIVE CONTROL: the marker rides the ALWAYS-LOADED index itself,
                    # not the attachment path. If this does not arrive either, the memory
                    # subsystem is not engaging at all and a sibling miss says nothing.
                    (memdir / "MEMORY.md").write_text(
                        (payload_dir / u["rung"] / "mem_00.md").read_text())
                else:
                    # Every other unit gets an index NAMING its siblings — the live dir
                    # has one, and recall is plausibly index-driven. Its absence is what
                    # made the first smoke's null unreadable.
                    (memdir / "MEMORY.md").write_text(
                        "".join(SMOKE_INDEX_TEMPLATE.format(stem=s) for s in stems[:1])
                        + "".join(f"- [{s}]({s}.md) — scratch probe file\n"
                                  for s in stems[1:]))
            prompt = (PROMPT_POINTER if u["arm"] == "A_PRIME"
                      else PROMPT_SMOKE if u["arm"].startswith("SMOKE")
                      else PROMPT_DELIVERY)
            sid = f"{secrets.token_hex(4)}-{secrets.token_hex(2)}-4{secrets.token_hex(2)[1:]}" \
                  f"-a{secrets.token_hex(2)[1:]}-{secrets.token_hex(6)}"
            argv = build_argv(u, scratch, prompt, sid, budget)
            rec = transport(argv, scratch)
            write_response(resp_dir, u, k, rec)          # RAW FIRST, always
            usage = parse_usage(rec["stdout"])
            emitted = CANARY_RE.findall(rec["stdout"])
            cans = u.get("canaries", [{}])[0]
            reports.append({
                "arm": u["arm"], "rung": u["rung"], "k": k, "rc": rec["rc"],
                "stderr": rec["stderr"][:400],
                "delivered": (usage or {}).get("delivered"),
                "cache_read_input_tokens": (usage or {}).get("cache_read_input_tokens"),
                "tool_calls": observed_tool_calls(rec["stdout"]),
                "tokens_emitted": emitted,
                "seen": [slot for slot, tok in cans.items() if tok in emitted],
                # Secondary discriminator: a truncated file SAYS which path cut it.
                # None means no notice was seen, which is NOT "untruncated".
                "notice_path": notice_path(rec["stdout"]),
                "notice_axis": notice_axis(rec["stdout"]),
            })

    gate_3_responses(resp_dir, len(todo) * K, errors)
    n_cleaned = cleanup_scratch_project_dirs(minted_project_dirs)
    print(f"\n  cleanup: removed {n_cleaned} scratch project dir(s) under "
          f"~/.claude/projects/ (guarded; the live key can never be reached)")
    fired = check_halts(reports, minted)
    print(f"\n  HALTs fired: {fired or 'none'}")
    for h in fired:
        print(f"    {h}: {HALTS[h]}")

    if mode == "smoke":
        # SMOKE RENDERS NO VERDICT AGAINST THE OUTCOME TABLE, deliberately. The table
        # is about thresholds; smoke has no threshold in it. Reporting one here would
        # be the seeded draw the reordering exists to avoid.
        summary = {"feasibility": {}, "smoke_scope_md5": smoke_scope_md5}
        print("\n  FEASIBILITY READOUT (no verdict — smoke carries no threshold "
              "information; see oq289_smoke_scope.md):")
        for arm in ("SMOKE_NOTOOLS", "SMOKE_TOOLS"):
            rs = [r for r in reports if r["arm"] == arm]
            hits = sum(1 for r in rs if r["seen"])
            deliv = [r["delivered"] for r in rs if r.get("delivered") is not None]
            summary["feasibility"][arm] = {
                "marker_verbatim": f"{hits}/{len(rs)}",
                "usage_present": f"{len(deliv)}/{len(rs)}",
                "mean_delivered": (sum(deliv) / len(deliv)) if deliv else None,
                "tool_calls": [r.get("tool_calls") for r in rs],
            }
            print(f"    {arm:16s} marker {hits}/{len(rs)}   usage {len(deliv)}/{len(rs)}"
                  f"   mean delivered "
                  f"{(sum(deliv) / len(deliv)) if deliv else 'n/a'}")
        a = summary["feasibility"]["SMOKE_NOTOOLS"]["marker_verbatim"]
        b = summary["feasibility"]["SMOKE_TOOLS"]["marker_verbatim"]
        print(f"\n    The PAIR is the discriminator: no-tools {a} vs tools {b}.")
        print("    no-tools 0/n WHILE tools n/n  => --tools \"\" suppresses recall; "
              "Arm A as designed returns a null that means nothing.")
        print("    BOTH 0/n                      => recall may not fire under -p at all; "
              "the transport, not the flag, is the problem.")
        print("    BOTH n/n                      => attachment arrives under both; "
              "Arm A is runnable as designed.")
    else:
        # Verdicts are computed HERE, from persisted data against the frozen table — not
        # assigned by hand in the writeup after the numbers are visible.
        summary = analyze(reports, units)
        print(f"\n  BASELINE (LEAK arm) = {summary['baseline']}  eps = {summary['eps']}")
        for rung, v in sorted(summary["verdicts"].items()):
            print(f"    {rung:28s} {v}")
    paths = {r["rung"]: r.get("notice_path") for r in reports if r.get("notice_path")}
    if paths:
        print(f"\n  truncation-notice path (secondary discriminator): {paths}")
    (outdir / "reports.json").write_text(json.dumps(reports, indent=2) + "\n")
    (outdir / "summary.json").write_text(json.dumps(
        {**summary, "mode": mode, "halts": fired, "cli_version": cli_version(),
         "settings_md5": md5_file(LIVE_SETTINGS) if LIVE_SETTINGS.exists() else None,
         "notice_paths": paths, "at": now()}, indent=2) + "\n")
    if errors:
        print("\nGATES RED after the run — evidence retained:")
        for e in errors:
            print(f"  - {e}")
        return 1
    print("\nRun complete. No expected verdict is written into this driver or the "
          "deliverable template; any prior about which outcome is likely belongs in the "
          "pre-registration, where the freeze covers it.")
    return 0


# ---------------------------------------------------------------------------
# Selftest — every control two-sided
# ---------------------------------------------------------------------------
def selftest() -> int:
    ok = True

    def check(label, cond):
        nonlocal ok
        print(f"  {'PASS' if cond else 'FAIL'}  {label}")
        ok = ok and cond

    print("payload construction — the properties are ASSERTED, not merely intended:\n")
    f = build_filler(4096, 40)
    check("(1) filler hits the exact declared byte length", len(f.encode()) == 4096)
    check("(1) filler hits the exact declared line count", f.count("\n") + 1 == 40)
    check("(1) filler carries no 16-hex run", not CANARY_RE.findall(f))
    check("(1) filler carries no @-import", not IMPORT_RE.findall(f))
    try:
        assert_filler_clean("deadbeefdeadbeef is a 16-hex run")
        fired = False
    except AssertionError:
        fired = True
    check("(1) CONVERSE — a planted 16-hex run FIRES the cleanliness assert", fired)
    try:
        assert_filler_clean("see @docs/other.md for detail")
        fired = False
    except AssertionError:
        fired = True
    check("(1) CONVERSE — a planted @-import FIRES the cleanliness assert", fired)

    mem = build_memory_file(6000, 50, {"start": "a" * 16, "middle": "b" * 16, "end": "c" * 16})
    check("(2) memory file carries all three canary slots",
          all(s in mem for s in ("CANARY-START", "CANARY-MIDDLE", "CANARY-END")))
    try:
        build_memory_file(6000, 50, {"start": "x", "middle": "y"})
        fired = False
    except AssertionError:
        fired = True
    check("(2) CONVERSE — a missing canary slot FIRES", fired)

    print("\nsmoke — the probe must carry NO threshold information (operator ruling):")
    su = build_smoke_units()
    check("(2s) smoke is THREE arms — the tools pair, plus the always-loaded positive "
          "control that makes a 0 mean anything at all",
          {u["arm"] for u in su} == {"SMOKE_NOTOOLS", "SMOKE_TOOLS", "SMOKE_INDEX"})
    check("(2s) smoke payload is far under EVERY candidate byte cap",
          all(u["bytes"] < min(c[0] for c in CAPS.values()) / 4 for u in su))
    check("(2s) smoke payload is far under EVERY candidate line cap",
          all(u["lines"] < min(c[1] for c in CAPS.values()) / 4 for u in su))
    check("(2s) smoke is NOT a rung of the ladder — no ladder rung shares its name",
          not ({u["rung"] for u in su} & {u["rung"] for u in build_ladder()}))
    sf = build_smoke_file(SMOKE_BYTES, SMOKE_LINES, "a" * 16)
    check("(2s) smoke file carries exactly ONE marker — no position signal to read",
          sf.count(SMOKE_PREFIX) == 1
          and not any(s in sf for s in ("CANARY-START", "CANARY-MIDDLE", "CANARY-END")))
    check("(2s) smoke file hits its declared byte length", len(sf.encode()) == SMOKE_BYTES)
    check("(2s) smoke arms differ ONLY in the tools flag — the pair is the discriminator",
          build_argv(su[0], pathlib.Path("/tmp/x"), "p", "s", 1.0)[
              build_argv(su[0], pathlib.Path("/tmp/x"), "p", "s", 1.0).index("--tools") + 1] == ""
          and build_argv(su[1], pathlib.Path("/tmp/x"), "p", "s", 1.0)[
              build_argv(su[1], pathlib.Path("/tmp/x"), "p", "s", 1.0).index("--tools") + 1] == "Read")
    check("(2s) a tool call on SMOKE_TOOLS does NOT fire the suppressed-arm HALT",
          "TOOL_CALL_ON_SUPPRESSED_ARM" not in check_halts(
              [{"arm": "SMOKE_TOOLS", "tool_calls": 1}], set()))
    check("(2s) a tool call on SMOKE_NOTOOLS DOES fire it",
          "TOOL_CALL_ON_SUPPRESSED_ARM" in check_halts(
              [{"arm": "SMOKE_NOTOOLS", "tool_calls": 1}], set()))
    check("(2s) the scope file exists and states what smoke may NOT conclude",
          SMOKE_SCOPE.exists() and "may NOT conclude" in SMOKE_SCOPE.read_text())

    print("\nnotice discriminator — a truncated file SAYS which path cut it:")
    check("(2n) the PIe notice is recognised", notice_path(
        "This memory file was truncated (4096 byte limit). Use the Read tool") == "PIe")
    check("(2n) the WEr notice is recognised", notice_path(
        "> WARNING: this memory file is 359 lines (limit: 200). "
        "Only part of it was loaded.") == "WEr")
    check("(2n) PIe wins when BOTH strings appear — it is the more specific claim",
          notice_path("This memory file was truncated ... Only part of it was loaded")
          == "PIe")
    check("(2n) no notice returns None, NOT 'untruncated' — absence is its own value",
          notice_path("MARKER=deadbeefdeadbeef") is None)
    check("(2n) WEr's line-only axis is read off its own message",
          notice_axis("is 359 lines (limit: 200). Only part") == "line_only")
    check("(2n) WEr's both-axes message is distinguished from line-only",
          notice_axis("is 359 lines and 25.4KB. Only part") == "both")

    print("\nisolation — the guard must fire on each way the live substrate can be touched:")
    errs: list = []
    assert_isolation(pathlib.Path("/home/scott/bin/structural_dynamics_model"),
                     isolation_baseline(), errs)
    check("(3) scratch cwd resolving to the LIVE project key fires",
          any("LIVE project key" in e for e in errs))

    tmp = pathlib.Path(tempfile.mkdtemp())
    try:
        (tmp / ".claude").mkdir()
        (tmp / ".claude" / "CLAUDE.md").write_text("stray")
        errs = []
        assert_isolation(tmp, isolation_baseline(), errs)
        check("(3) a stray .claude/CLAUDE.md under the scratch cwd fires",
              any(".claude/CLAUDE.md" in e for e in errs))
        shutil.rmtree(tmp / ".claude")
        errs = []
        assert_isolation(tmp, isolation_baseline(), errs)
        check("(3) CONVERSE — a clean scratch cwd does NOT fire", not errs)
        errs = []
        assert_isolation(tmp, {"__claude_md__": "wrong", "__settings__": "wrong"}, errs)
        check("(3) a changed live CLAUDE.md md5 fires",
              any("CLAUDE.md md5 changed" in e for e in errs))
        check("(3) a changed settings.json md5 fires",
              any("settings.json md5 changed" in e for e in errs))
        # This clause had NO two-sided control until 2026-08-12, and it shipped
        # comparing a dict against a differently-shaped dict — so it fired on every
        # unit. A clause that can never pass is as uninformative as one that can never
        # fail, and only the CONVERSE above could see it. Both directions now pinned.
        base = isolation_baseline()
        mutated = {**base, "__PLANTED_EXTRA__.md": "0" * 32}
        mutated.pop("__PLANTED_EXTRA__.md")
        mutated["planted_extra.md"] = "0" * 32
        errs = []
        assert_isolation(tmp, mutated, errs)
        check("(3) a changed LIVE memory-dir manifest fires",
              any("LIVE memory dir manifest changed" in e for e in errs))
        errs = []
        assert_isolation(tmp, base, errs)
        check("(3) CONVERSE — the unmodified live manifest does NOT fire",
              not any("LIVE memory dir manifest" in e for e in errs))
    finally:
        shutil.rmtree(tmp, ignore_errors=True)

    print("\ncleanup guard — the litter must be removable and the live dir must not be:")
    root = pathlib.Path.home() / ".claude" / "projects"
    for label, target, want_fire in (
            ("the LIVE project dir", root / LIVE_PROJECT_KEY, True),
            ("a non-oq289 sibling project dir", root / "-some-other-project", True),
            ("a path outside ~/.claude/projects", pathlib.Path("/tmp/oq289-x"), True),
            ("a genuine oq289 scratch project dir", root / "-tmp-oq289-scratch-r1", False)):
        try:
            assert_removable_project_dir(target)
            fired = False
        except AssertionError:
            fired = True
        check(f"(3) cleanup {'REFUSES' if want_fire else 'ALLOWS'} {label}",
              fired == want_fire)
    check("(3) cleanup_scratch_project_dirs removes nothing it was refused",
          cleanup_scratch_project_dirs([root / LIVE_PROJECT_KEY]) == 0
          and (root / LIVE_PROJECT_KEY).is_dir())

    check("(3) project_key reproduces the LIVE dir name",
          project_key(pathlib.Path("/home/scott/bin/structural_dynamics_model"))
          == LIVE_PROJECT_KEY)

    print("\ngate 0 / 0b — a short ladder must not look like a freeze:")
    tmp = pathlib.Path(tempfile.mkdtemp())
    try:
        errs = []
        gate_0_ladder([{"arm": "A", "rung": "r1", "bytes": 100, "batch": 1}],
                      tmp, errs)
        check("(4) a declared rung with no payload on disk fires",
              any("no payload on disk" in e for e in errs))
        (tmp / "r1").mkdir()
        (tmp / "r1" / "mem_00.md").write_text("x" * 99)
        errs = []
        gate_0_ladder([{"arm": "A", "rung": "r1", "bytes": 100, "batch": 1}], tmp, errs)
        check("(4) a payload at the WRONG byte length fires",
              any("declared 100 B" in e for e in errs))
        (tmp / "r1" / "mem_00.md").write_text("x" * 100)
        errs = []
        gate_0_ladder([{"arm": "A", "rung": "r1", "bytes": 100, "batch": 1}], tmp, errs)
        check("(4) CONVERSE — an exact-length payload does NOT fire", not errs)
    finally:
        shutil.rmtree(tmp, ignore_errors=True)

    errs = []
    gate_0b_environment(errs)
    live_v = cli_version()
    check(f"(5) gate 0b reads the CLI version ({live_v}) and compares to the pin",
          (live_v == PINNED_CLI_VERSION) == (not any("CLI is" in e for e in errs)))

    print("\ngate 1/2/3 — output-side gates, each way a result can be lost:")
    tmp = pathlib.Path(tempfile.mkdtemp())
    try:
        errs = []
        gate_1_payloads(tmp, 12, errs)
        check("(6) a short payload capture fires", any("gate 1" in e for e in errs))
        (tmp / "r1").mkdir()
        (tmp / "r1" / "a.md").write_text("cafebabecafebabe here")
        errs = []
        gate_2_clean(tmp, set(), errs)
        check("(6) an UNMINTED 16-hex token in a payload fires",
              any("unminted" in e for e in errs))
        errs = []
        gate_2_clean(tmp, {"cafebabecafebabe"}, errs)
        check("(6) CONVERSE — a minted token does NOT fire", not errs)
        (tmp / "r1" / "b.md").write_text("see @other.md")
        errs = []
        gate_2_clean(tmp, {"cafebabecafebabe"}, errs)
        check("(6) an @-import in a payload fires", any("@-import" in e for e in errs))
    finally:
        shutil.rmtree(tmp, ignore_errors=True)

    def responses_under(build) -> list:
        d = pathlib.Path(tempfile.mkdtemp())
        try:
            build(d)
            e: list = []
            gate_3_responses(d, 3, e)
            return e
        finally:
            shutil.rmtree(d, ignore_errors=True)

    good = json.dumps({"result": "NONE", "usage": {"input_tokens": 10,
                                                   "cache_creation_input_tokens": 0,
                                                   "cache_read_input_tokens": 0}})

    def resp(d, arm, rung, k, stdout, blank=False):
        (d / arm).mkdir(parents=True, exist_ok=True)
        f = d / arm / f"{rung}__k{k}.json"
        f.write_text("" if blank else json.dumps({"unit": {}, "k": k, "stdout": stdout}) + "\n")

    check("(7) a MISSING response file fires (2 of 3 present)",
          bool(responses_under(lambda d: [resp(d, "A", "r", k, good) for k in (1, 2)])))
    check("(7) all present but one ZERO-BYTE fires — a count alone would pass",
          bool(responses_under(lambda d: [resp(d, "A", "r", k, good, blank=(k == 3))
                                          for k in (1, 2, 3)])))
    check("(7) a response with NO usage block fires — the primary instrument has no input",
          any("usage block" in e for e in responses_under(
              lambda d: [resp(d, "A", "r", k, json.dumps({"result": "NONE"}))
                         for k in (1, 2, 3)])))
    check("(7) CONVERSE — three complete usage-bearing responses do NOT fire",
          not responses_under(lambda d: [resp(d, "A", "r", k, good) for k in (1, 2, 3)]))

    print("\ninstrument — slope, and the refusal to render 0.0 for undefined:")
    check("(8) pass-through slope bands PASS_THROUGH",
          slope_band(slope(1000, 3800)) == "PASS_THROUGH")
    check("(8) flat slope bands PLATEAU", slope_band(slope(1, 3800)) == "PLATEAU")
    check("(8) mid slope bands PARTIAL", slope_band(slope(400, 3800)) == "PARTIAL")
    check("(8) zero Δbytes yields undefined, NOT 0.0/PLATEAU — a plateau is a verdict",
          slope(5, 0) is None and slope_band(None) == "undefined")
    check("(8) parse_usage sums all THREE components",
          parse_usage(json.dumps({"usage": {"input_tokens": 1,
                                            "cache_creation_input_tokens": 2,
                                            "cache_read_input_tokens": 4}}))["delivered"] == 7)
    check("(8) parse_usage returns None (not 0) when no usage block exists — "
          "'reported nothing' must not equal 'reported a small number'",
          parse_usage(json.dumps({"result": "x"})) is None)

    print("\noutcome table — the rows must PARTITION, not merely cover:")

    def rep(**kw):
        base = {"rc": 0, "stderr": "", "slope_band": "PASS_THROUGH", "seen": [],
                "delivered": 100.0}
        base.update(kw)
        return base

    three = lambda **kw: [rep(**kw) for _ in range(3)]                   # noqa: E731
    check("(9) all three slots verbatim -> DELIVERED",
          classify(three(seen=["start", "middle", "end"]), 100.0, 5.0) == "DELIVERED")
    check("(9) START present, END absent -> TRUNCATED",
          classify(three(seen=["start", "middle"]), 100.0, 5.0) == "TRUNCATED")
    check("(9) all-ABSENT at BASELINE -> ATTACHMENT_NEVER_FIRED",
          classify(three(seen=[], delivered=100.0), 100.0, 5.0) == "ATTACHMENT_NEVER_FIRED")
    check("(9) all-ABSENT with delivered ELEVATED -> DROPPED (the discriminator that "
          "used to be missing)",
          classify(three(seen=[], delivered=900.0), 100.0, 5.0) == "DROPPED")
    check("(9) PLATEAU while canaries are verbatim -> CONTRADICTION, verdict REFUSED",
          classify(three(seen=["start"], slope_band="PLATEAU"), 100.0, 5.0) == "CONTRADICTION")
    check("(9) a loud 'input too long' -> LOUD_REFUSAL",
          classify(three(rc=1, stderr="Error: input too long"), 100.0, 5.0) == "LOUD_REFUSAL")
    check("(9) a mixed split lands in BOUNDARY, never coerced into a neighbour",
          classify([rep(seen=["start", "middle", "end"]), rep(seen=["start"]),
                    rep(seen=["start"])], 100.0, 5.0) == "BOUNDARY")
    check("(9) FABRICATED is NOT a verdict row — it is a HALT",
          "FABRICATED" not in {classify(three(seen=s), 100.0, 5.0)
                               for s in ([], ["start"], ["start", "middle", "end"])})

    print("\nHALTs — each must fire, and the run-voiding ones must not be local:")
    check("(10) leak arm reporting a canary fires LEAK_FIRED",
          "LEAK_FIRED" in check_halts([{"arm": "LEAK", "seen": ["end"]}], set()))
    check("(10) CONVERSE — a silent leak arm does NOT fire",
          "LEAK_FIRED" not in check_halts([{"arm": "LEAK", "seen": []}], set()))
    check("(10) inject arm failing to echo fires INJECT_SILENT",
          "INJECT_SILENT" in check_halts([{"arm": "INJECT", "seen": []}], set()))
    check("(10) CONVERSE — an inject arm that echoes does NOT fire",
          "INJECT_SILENT" not in check_halts([{"arm": "INJECT", "seen": ["end"]}], set()))
    check("(10) an unminted emitted token fires FABRICATED",
          "FABRICATED" in check_halts(
              [{"arm": "A", "tokens_emitted": ["0123456789abcdef"]}], set()))
    check("(10) CONVERSE — a minted emitted token does NOT fire FABRICATED",
          "FABRICATED" not in check_halts(
              [{"arm": "A", "tokens_emitted": ["0123456789abcdef"]}], {"0123456789abcdef"}))
    check("(10) delivered varying across k at identical payload fires",
          "DELIVERED_UNSTABLE_ACROSS_K" in check_halts(
              [{"arm": "A", "rung": "r", "delivered": 100.0},
               {"arm": "A", "rung": "r", "delivered": 105.0}], set()))
    check("(10) CONVERSE — delivered IDENTICAL across k does NOT fire (this is the "
          "shape the first smoke actually produced: 9002 x3, zero variance)",
          "DELIVERED_UNSTABLE_ACROSS_K" not in check_halts(
              [{"arm": "A", "rung": "r", "delivered": 9002.0} for _ in range(3)], set()))
    check("(10) nonzero cache_read does NOT fire — it is delivered context, and the "
          "old cache_read==0 HALT was unsatisfiable under this transport",
          not check_halts([{"arm": "A", "rung": "r",
                            "cache_read_input_tokens": 3289}], set()))
    check("(10) a tool call on a --tools '' arm fires",
          "TOOL_CALL_ON_SUPPRESSED_ARM" in check_halts(
              [{"arm": "A", "tool_calls": 1}], set()))
    check("(10) CONVERSE — a tool call on A_PRIME does NOT fire (there it is the "
          "MEASUREMENT, not a violation)",
          "TOOL_CALL_ON_SUPPRESSED_ARM" not in check_halts(
              [{"arm": "A_PRIME", "tool_calls": 1}], set()))
    check("(10) every HALT name emitted by check_halts has an entry in HALTS",
          all(h in HALTS for h in check_halts(
              [{"arm": "LEAK", "seen": ["e"]}, {"arm": "INJECT", "seen": []},
               {"arm": "A", "tool_calls": 1, "cache_read_input_tokens": 1,
                "tokens_emitted": ["0123456789abcdef"]}], set())))

    print("\ntransport shape — asserted without spending:")
    a = build_argv({"arm": "A", "rung": "r"}, pathlib.Path("/tmp/x"), "p", "sid", 1.0)
    check("(11) delivery arms disable ALL tools",
          a[a.index("--tools") + 1] == "" and "--no-session-persistence" in a)
    ap = build_argv({"arm": "A_PRIME", "rung": "r"}, pathlib.Path("/tmp/x"), "p", "sid", 1.0)
    check("(11) A_PRIME RE-ENABLES Read — the pointer-follow is the measurement",
          ap[ap.index("--tools") + 1] == "Read")
    inj = build_argv({"arm": "INJECT", "rung": "r", "inject_canary": "f" * 16},
                     pathlib.Path("/tmp/x"), "p", "sid", 1.0)
    check("(11) INJECT delivers its canary via --append-system-prompt, not via a file",
          "--append-system-prompt" in inj and "f" * 16 in " ".join(inj))
    check("(11) every arm carries --max-budget-usd", "--max-budget-usd" in a)

    print("\nspend gate — the live path is structurally refused, not merely discouraged:")
    d = pathlib.Path(tempfile.mkdtemp())
    try:
        rc = subprocess.run(
            [sys.executable, __file__, "--live", "--outdir", str(d / "o")],
            capture_output=True, text=True, timeout=180)
        combined = rc.stdout + rc.stderr
        check("(12) --live REFUSES without the freeze on disk",
              rc.returncode != 0 and "REFUSED" in combined)
        check("(12) and it names the staging-file MOVE, so a copy cannot fork the freeze",
              "MOVE it into" in combined or "PREREGISTRATION.md does not exist" in combined)
    finally:
        shutil.rmtree(d, ignore_errors=True)

    print("\nwiring witness — a control must witness that it is CALLED:")
    orph = orphaned_controls()
    check(f"(13) no orphaned controls (found: {orph or 'none'})", not orph)
    planted = pathlib.Path(__file__).read_text() + (
        "\n\ndef gate_planted_orphan(x):\n    return x\n")
    check("(13) CONVERSE — a planted orphan IS detected",
          "gate_planted_orphan" in orphaned_controls(planted))
    check("(13) and the planted orphan does NOT displace the real result — the detector "
          "reports exactly the planted name and nothing else",
          set(orphaned_controls(planted)) == {"gate_planted_orphan"})

    print("\nanalysis wiring — slope and classify must be reachable from run(), not "
          "only from the selftest:")
    u = [{"rung": "lo", "bytes": 1000, "cap": "NSp", "axis": "bytes"},
         {"rung": "hi", "bytes": 4800, "cap": "NSp", "axis": "bytes"}]
    reps = ([{"arm": "A", "rung": "lo", "delivered": 100.0, "seen": ["start", "middle", "end"],
              "rc": 0, "stderr": ""} for _ in range(3)]
            + [{"arm": "A", "rung": "hi", "delivered": 1100.0, "seen": ["start"],
                "rc": 0, "stderr": ""} for _ in range(3)]
            + [{"arm": "LEAK", "rung": "no_payload", "delivered": 90.0, "seen": [],
                "rc": 0, "stderr": ""} for _ in range(3)])
    s = analyze(reps, u)
    check("(14) BASELINE is measured from the LEAK arm, not assumed", s["baseline"] == 90.0)
    check("(14) slope is computed between adjacent rungs of the SAME family",
          s["slopes"]["hi"] is not None and s["slopes"]["hi"] > SLOPE_PASS_THROUGH)
    check("(14) each rung receives a verdict from the frozen table",
          s["verdicts"]["hi"] == "TRUNCATED" and s["verdicts"]["lo"] == "DELIVERED")
    s2 = analyze([{"arm": "A", "rung": "lo", "delivered": None, "seen": [], "rc": 0,
                   "stderr": ""}], u)
    check("(14) no LEAK arm -> UNSEPARABLE_NO_BASELINE, never a defaulted baseline",
          s2["baseline"] is None
          and s2["verdicts"]["lo"] == "UNSEPARABLE_NO_BASELINE")

    print(f"\nselftest: {'all controls fired' if ok else 'FAILURES ABOVE'}")
    return 0 if ok else 1


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    g = ap.add_mutually_exclusive_group()
    g.add_argument("--selftest", action="store_true")
    g.add_argument("--dry-run", action="store_true")
    g.add_argument("--stub", action="store_true")
    g.add_argument("--smoke", action="store_true",
                   help="one live call; settles the three --smoke unknowns")
    g.add_argument("--live", action="store_true")
    ap.add_argument("--outdir", default=None)
    ap.add_argument("--budget", type=float, default=0.50,
                    help="--max-budget-usd per call")
    a = ap.parse_args()
    if a.selftest:
        return selftest()
    out = pathlib.Path(a.outdir) if a.outdir else \
        pathlib.Path(tempfile.mkdtemp(prefix="oq289_"))
    mode = ("live" if a.live else "smoke" if a.smoke else "stub" if a.stub else "dry")
    return run(mode, out, a.budget)


if __name__ == "__main__":
    sys.exit(main())
