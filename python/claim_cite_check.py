#!/usr/bin/env python3
"""claim_cite_check — pin cross-document claim citations to the row they cite.

Mirrors `python/spec_enum_check.py`: sentinel blocks + a checker, wired into `scripts/gate.sh`,
carrying the same opt-in hazard (a NEW citation is unguarded until its digest lands in the same
change) and a red-capable selftest.

WHAT A CITATION LOOKS LIKE
    <!-- PIN-RECORD-BEGIN: illustration, not a citation -->
    CWC:A2@31548228     CWC = Concealment Without a Concealer; A2 = Appendix A row; @ = digest.
    <!-- PIN-RECORD-END -->
  (The example is sentinel-wrapped because this file is inside the corpus it scans. Documentation
  ABOUT a citation otherwise registers AS one — the same shape, three times now.)

  Namespaced deliberately. Two claim namespaces with the same shape and different arity exist:
  v0.6's own §0 table runs A1-A4 (and cross-references its own labels in the Premises column),
  while concealment's Appendix A runs A1-A5 + C1, E1-E5, P1-P5. Their `A2`s are DIFFERENT CLAIMS.
  An unnamespaced scanner would read v0.6's own table rows as citations - so the checker requires
  the `CWC:` prefix and the namespace ambiguity never arises. The label class set is OPEN: A, E, P
  and now C. Never hardcode [AEP].

--- CORPUS-SCOPE-RATIONALE-BEGIN ------------------------------------------------------------
THE CHECKER SCANS THE WHOLE REPOSITORY, NOT JUST docs/. This is the instrument's own framing and
it is stated rather than left implicit, because an unstated selection rule is the defect the cited
paper is about.

Three scopes were available:
  1. docs/ only            - misses audits/, where EXTRACTION_PROMPT.md will carry the pins a
                             receiver reads months later with no live context: the pins most
                             likely to go stale and least likely to be noticed.
  2. everything, flat      - fires on every historical draft that correctly RECORDS a superseded
                             pin, until someone silences it by narrowing scope, invisibly.
  3. everything + sentinel - a pin inside a PIN-RECORD block is a record of a past state; a pin
                             outside one is a claim about a present state. Only the latter is
                             checked. This is row 1's containment mechanism, already proven this
                             pass, and it is the only option that does not trade coverage against
                             false fires.
Scope 3 is taken.

It also keeps a declared falsifier well-formed. The stopping point declared in
`audits/2026-08-13_oq287_defork/claim_digest.sh` says the instrument regress stops unless "a stale
pin reaches a citing document undetected". Under scope 1 that falsifier would be unfalsifiable for
exactly the documents where it is most likely to fire.
--- CORPUS-SCOPE-RATIONALE-END --------------------------------------------------------------

--- DECLARED-RESIDUAL-BEGIN: aptness is not checked -----------------------------------------
THIS CHECKER VERIFIES THAT A PIN MATCHES ITS ROW. IT CANNOT VERIFY THAT THE ROW IS THE RIGHT ONE
TO CITE AT THAT SITE.

A citation aimed at A2 where the argument actually needs A4 reads green forever, and stays green
through every future narrowing of either row - because both the pin and the row remain internally
consistent. The mechanical relation is guarded; the semantic one is not. With 16 sites today and
roughly double after A3, that is a real surface, not a theoretical one.

This is the same shape as the gloss problem that got the citation glosses struck: an unpinned
judgement sitting beside a pinned fact, where the instrument sees the fact and not the judgement.

MITIGATION, and it is partial: aptness was recorded once, by hand, in
`audits/2026-08-13_oq287_defork/COVERAGE_DIFF.md` - the claim-by-claim mapping table that decided
which concealment claim each vacated v0.6 unit maps to. NOTHING CURRENTLY RE-CHECKS IT, and it was
built against the pre-C1 label set. A future pass that adds citation sites should extend that table
in the same change, and a future instrument that wants to close this residual should start there
rather than from the citation text.
--- DECLARED-RESIDUAL-END -------------------------------------------------------------------

Usage:
    python3 python/claim_cite_check.py --check      # gate mode: exit 1 on any stale/bad pin
    python3 python/claim_cite_check.py --list       # every live citation and its status
    python3 python/claim_cite_check.py --selftest   # red-capable controls
"""

import re
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
CWC = REPO / "docs/concealment/concealment_without_a_concealer_v0_4.md"
DIGEST_TOOL = REPO / "audits/2026-08-13_oq287_defork/claim_digest.sh"

# Label class set is OPEN by design (A, E, P, C). See the header.
CITE_RE = re.compile(r"CWC:([A-Z]+[0-9]+)@([0-9a-f]{8})")
PIN_BEGIN = "PIN-RECORD-BEGIN"
PIN_END = "PIN-RECORD-END"
PIN_INLINE = "PIN-RECORD-INLINE"

SCAN_SUFFIXES = {".md", ".sh", ".py", ".pl", ".txt", ".json"}
SKIP_DIRS = {".git", "node_modules", "__pycache__", ".claude"}


def digest_of(label, cwc_path=CWC):
    """Delegate to claim_digest.sh. NEVER reimplement the recipe here.

    The recipe was once written as prose and implemented twice from that prose in one turn; the
    two implementations disagreed on a trailing newline and every digest was wrong. One executable
    definition, one call site.
    """
    res = subprocess.run(
        [str(DIGEST_TOOL), label, str(cwc_path)],
        capture_output=True, text=True,
    )
    if res.returncode != 0:
        return None          # label does not resolve; caller reports it
    return res.stdout.strip()


def scan_file(path):
    """Yield (lineno, label, pin, is_record) for every citation in one file."""
    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return
    in_record = False
    for lineno, line in enumerate(text.split("\n"), 1):
        if PIN_BEGIN in line:
            in_record = True
        if PIN_END in line:
            in_record = False
            continue
        for m in CITE_RE.finditer(line):
            yield lineno, m.group(1), m.group(2), (in_record or PIN_INLINE in line)


def iter_files(root=REPO):
    """Candidate files: git-tracked where possible, else a walk; then a fast prefilter.

    CORPUS DEFINITION, stated because it is this instrument's own framing: *every git-tracked file
    with a scannable suffix that contains the literal `CWC:`*. Tracked-ness is a checkable property,
    not a judgement about which directories "could plausibly" hold a citation - that judgement would
    be an unstated selection rule, which is the defect the cited paper is about.

    The prefilter is a performance device ONLY and must stay conservative: it selects on `CWC:`,
    which is a strict superset of what CITE_RE can match, so no file the regex would have matched is
    excluded. A naive rglob over this repository walks 85k files and takes minutes; that cost is
    what tempts a later maintainer to narrow the scope instead.
    """
    # --cached AND --others: tracked files PLUS untracked-but-not-ignored ones.
    #
    # `ls-files` alone lists only TRACKED files, and that shipped as a silent hole: a brand-new
    # document carrying a stale pin was invisible until someone committed it - which is precisely
    # backwards, since a new citation is at its most fragile before review. Witnessed on this file:
    # arm 9 (no-self-fire) passed while claim_cite_check.py was untracked and went red the moment
    # it was committed, because its own docstring example became visible. The arm was right both
    # times; the corpus was wrong once.
    #
    # --exclude-standard keeps .gitignore'd build output out, which is a DECLARED exclusion rather
    # than an incidental one: ignored paths are not authored documents.
    try:
        listed = subprocess.run(
            ["git", "-C", str(root), "ls-files", "--cached", "--others", "--exclude-standard", "-z"],
            capture_output=True, text=True, check=True)
        candidates = [root / f for f in listed.stdout.split("\0") if f]
    except (subprocess.CalledProcessError, FileNotFoundError):
        candidates = [p for p in root.rglob("*") if p.is_file()]

    for p in candidates:
        if p.suffix not in SCAN_SUFFIXES or any(part in SKIP_DIRS for part in p.parts):
            continue
        try:
            if not p.is_file():
                continue
            with open(p, "rb") as fh:
                if b"CWC:" not in fh.read():
                    continue
        except OSError:
            continue
        yield p


def collect(root=REPO, cwc_path=CWC):
    """Return (live, records). live entries carry a `status`."""
    live, records = [], []
    cache = {}
    for path in iter_files(root):
        for lineno, label, pin, is_record in scan_file(path):
            entry = {
                "file": str(path.relative_to(root)), "line": lineno,
                "label": label, "pin": pin,
            }
            if is_record:
                records.append(entry)
                continue
            if label not in cache:
                cache[label] = digest_of(label, cwc_path)
            actual = cache[label]
            if actual is None:
                entry["status"] = "UNRESOLVED"
            elif actual != pin:
                entry["status"] = "STALE"
                entry["actual"] = actual
            else:
                entry["status"] = "OK"
            live.append(entry)
    return live, records


def run_check(root=REPO, cwc_path=CWC, quiet=False):
    live, records = collect(root, cwc_path)
    bad = [e for e in live if e["status"] != "OK"]
    if not quiet:
        print(f"claim_cite_check: {len(live)} live citation(s), "
              f"{len(records)} recorded (superseded, not checked)")
        for e in bad:
            extra = f" (actual {e['actual']})" if "actual" in e else ""
            print(f"  {e['status']}: {e['file']}:{e['line']} CWC:{e['label']}@{e['pin']}{extra}")
        # Coverage carried to the read site: a zero-citation run is NOT a pass.
        if not live:
            print("  VACUOUS: no live citations found - this run checked nothing. "
                  "If citations exist, the scan or the corpus scope is wrong.")
    return 1 if bad else (2 if not live else 0)


def _fixture_root(td, cwc_text=None):
    """A minimal fake repo for an arm. Copies only the one file the checker resolves against.

    (The first version copied all of docs/ - 34 MB - per arm. Slow, and it also dragged the real
    v0.6 and its live pins into every fixture, so an arm's result depended on documents the arm was
    not about.)
    """
    root = Path(td) / "r"
    dest = root / "docs/concealment/concealment_without_a_concealer_v0_4.md"
    dest.parent.mkdir(parents=True)
    dest.write_text(cwc_text if cwc_text is not None else CWC.read_text(encoding="utf-8"),
                    encoding="utf-8")
    return root, dest


def selftest():
    """Red-capable controls. Each names the value that would make it fail.

    NOTE ON THIS FUNCTION'S OWN FIXTURES: the planted pins below are wrapped in PIN-RECORD
    sentinels, because the corpus is the whole repository and that includes THIS FILE. Without the
    sentinels the checker reads its own test data as live citations and fires on itself - which it
    did, on the first run. Same shape as the sentinel-rationale comment that quoted the string it
    was explaining: prose (or fixtures) ABOUT a checked thing register AS the checked thing. The
    mechanism chosen for the corpus scope turns out to be the mechanism that fixes it.
    """
    import tempfile
    ok = True

    def p(m): print(f"  PASS  {m}")

    def b(m):
        nonlocal ok
        ok = False
        print(f"  FAIL  {m}")

    live, records = collect()

    # 1 - declines on the repository as it stands (naturally-arising negative)
    if all(e["status"] == "OK" for e in live):
        p(f"declines on the real repository ({len(live)} live citations, all resolving)")
    else:
        b("fired on the real repository")

    # 2 - the corpus actually reaches audits/, not only docs/. This is the scope decision,
    #     witnessed rather than asserted: if it silently narrowed, the declared falsifier for the
    #     stopping point would become unfalsifiable exactly where it matters.
    if any(e["file"].startswith("audits/") for e in records):
        p("corpus reaches audits/ (recorded pins found there)")
    else:
        b("corpus does NOT reach audits/ - scope narrowed, records invisible")

    # 3 - records are excluded from checking but COUNTED. A silent exclusion is the invisible
    #     tuning this design exists to avoid.
    if records:
        p(f"{len(records)} recorded pin(s) excluded from checking and reported, not hidden")
    else:
        b("no recorded pins found - the sentinel path is untested")

    # --- PIN-RECORD-BEGIN: the planted fixtures below are test data, never live citations ---
    STALE_PIN = "CWC:A2@" + "deadbeef"       # split so the literal never sits in one token
    BAD_LABEL = "CWC:Z9@" + "00000000"
    # --- PIN-RECORD-END ---

    # 4 - FIRES on a stale pin planted outside a record block (the defect it exists for)
    with tempfile.TemporaryDirectory() as td:
        root, cwc = _fixture_root(td)
        (root / "x.md").write_text(f"cites {STALE_PIN} here\n", encoding="utf-8")
        rc = run_check(root, cwc, quiet=True)
        p("fires on a planted stale pin") if rc == 1 else b("MISSED a planted stale pin")

    # 5 - DECLINES on the same stale pin wrapped in a record block. This is the arm that
    #     distinguishes containment from blanket-scanning; a flat scope-2 checker fires here.
    #
    #     The fixture carries a LIVE valid pin alongside the recorded stale one, deliberately. The
    #     first version had only the recorded pin, so the corpus had zero live citations and exited
    #     2 (VACUOUS) - and the arm read that as a failure to decline. It was neither a decline nor
    #     a fire: nothing had been checked. "Clean" and "checked nothing" are different states and
    #     an arm that cannot tell them apart tests neither. With a live pin present the corpus is
    #     non-vacuous, so exit 0 means specifically: the recorded pin was excluded and the live one
    #     verified.
    with tempfile.TemporaryDirectory() as td:
        root, cwc = _fixture_root(td)
        (root / "x.md").write_text(
            f"live: CWC:A2@{digest_of('A2')}\n"
            f"<!-- {PIN_BEGIN}: t -->\nrecords {STALE_PIN}\n<!-- {PIN_END} -->\n",
            encoding="utf-8")
        rc = run_check(root, cwc, quiet=True)
        p("declines on a recorded pin while still verifying a live one alongside it") if rc == 0 \
            else b(f"rc={rc}: recorded pin fired, or the live pin was not checked")

    # 6 - FIRES on an unresolvable label (a rename or deletion upstream)
    with tempfile.TemporaryDirectory() as td:
        root, cwc = _fixture_root(td)
        (root / "x.md").write_text(f"cites {BAD_LABEL} here\n", encoding="utf-8")
        rc = run_check(root, cwc, quiet=True)
        p("fires on an unresolvable label") if rc == 1 else b("MISSED an unresolvable label")

    # 7 - FIRES on a content change under an UNCHANGED label. The A4-narrowing case: label
    #     resolution alone passes silently through exactly this, which is why digests exist.
    with tempfile.TemporaryDirectory() as td:
        mutated_text = CWC.read_text(encoding="utf-8").replace(
            "| A2 | Framing non-identifiability: the framing",
            "| A2 | Framing NON-IDENTIFIABILITY: the framing", 1)
        root, cwc = _fixture_root(td, mutated_text)
        (root / "x.md").write_text(f"cites CWC:A2@{digest_of('A2')} here\n", encoding="utf-8")
        rc = run_check(root, cwc, quiet=True)
        p("fires when a row's content changes under an unchanged label") if rc == 1 else \
            b("MISSED a content change under an unchanged label - the whole point of digests")

    # 8 - a zero-citation corpus reports VACUOUS (exit 2), never a pass.
    with tempfile.TemporaryDirectory() as td:
        root, cwc = _fixture_root(td)
        (root / "x.md").write_text("no citations here\n", encoding="utf-8")
        rc = run_check(root, cwc, quiet=True)
        p("zero-citation corpus exits 2 (VACUOUS), not 0") if rc == 2 else \
            b(f"zero-citation corpus exited {rc} - absence satisfied the gate")

    # 8b - an UNTRACKED file is scanned. The corpus once used `git ls-files` alone, so a new
    #      document carrying a stale pin was invisible until committed - backwards, since a
    #      citation is most fragile before review. This arm is the discrimination record for that
    #      repair: it plants a stale pin in an untracked file and requires a fire.
    with tempfile.TemporaryDirectory() as td:
        root, cwc = _fixture_root(td)
        subprocess.run(["git", "-C", str(root), "init", "-q"], capture_output=True)
        (root / "x.md").write_text(f"live: CWC:A2@{digest_of('A2')}\n", encoding="utf-8")
        subprocess.run(["git", "-C", str(root), "add", "x.md"], capture_output=True)
        (root / "untracked.md").write_text(f"stale: {STALE_PIN}\n", encoding="utf-8")
        rc = run_check(root, cwc, quiet=True)
        p("fires on a stale pin in an UNTRACKED file") if rc == 1 else \
            b(f"rc={rc}: untracked file invisible - a new citation is unguarded until committed")

    # 9 - the checker does not fire on ITSELF. Its fixtures live in the scanned corpus; without
    #     the sentinels above they read as live citations, which is how the first run went red.
    self_live = [e for e in live if e["file"] == "python/claim_cite_check.py"]
    if not self_live:
        p("checker's own fixtures are records, not live citations (no self-fire)")
    else:
        b(f"checker reads {len(self_live)} of its own fixtures as LIVE citations")

    return 0 if ok else 1


def main(argv):
    if "--selftest" in argv:
        return selftest()
    if "--list" in argv:
        live, records = collect()
        for e in sorted(live, key=lambda x: (x["file"], x["line"])):
            print(f"  {e['status']:10s} {e['file']}:{e['line']}  CWC:{e['label']}@{e['pin']}")
        for e in sorted(records, key=lambda x: (x["file"], x["line"])):
            print(f"  {'RECORD':10s} {e['file']}:{e['line']}  CWC:{e['label']}@{e['pin']}")
        return 0
    if "--check" in argv:
        rc = run_check()
        # gate semantics: 0 green, 1 red, 2 vacuous-and-therefore-red (nothing was checked)
        return 0 if rc == 0 else 1
    print(__doc__.strip().split("\n\n")[0])
    print("usage: claim_cite_check.py [--check | --list | --selftest]")
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
