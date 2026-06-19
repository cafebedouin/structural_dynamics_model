#!/usr/bin/env python3
"""audit_citation_status.py — verify paths cited from audit writeups survive a fresh clone.

Sibling of issues_status.py / known_state_status.py (OQ-104). Audit writeups in
audits/<date>_<slug>/*.md cite repo-relative paths as evidence, but nothing checks
that those paths resolve on a *fresh clone*: a typo'd path, evidence left in a
worktree, a gitignored output, or the next .gitignore rule all read identically
(writeup cites it; fresh clone lacks it).

THE INVARIANT (operator ruling, OQ-104): a cited path must
    EXIST AND be git-tracked, OR be allowlisted-ephemeral.
"Missing" and "untracked-not-allowlisted" are the SAME violation class. WARN is a
staging area split by *why* a path is not yet an ERROR — three sublabels, three
destinies:

  WARN[untracked-pending]   real file-evidence citation: exists, untracked,
                            awaiting copy-into-audit-dir vs allowlist. Promoted to
                            ERROR by --promote-untracked. Promotion condition: this
                            set empty-or-allowlisted on a fresh clone.
  WARN[missing-pending-M]   survives exclusion, normalizes in-repo, still missing,
                            but the missing-class grammar is not yet FP-clean.
                            Promoted by --promote-missing. Condition: every survivor
                            classified (FP-excluded or surfaced), NOT zero survivors.
  WARN[grammar-ambiguous]   grammar is not confident the token is a real evidence
                            citation (globs, ellipsis, descriptive directory/location
                            mentions). NEVER promotes — promoting would gate on
                            maybe-not-citations.

A gitignored path INSIDE the repo root (outputs/, .claude/) is NEVER allowlisted —
it is the OQ-104 signature. Allowlist is for by-design-ephemeral/external paths only
(/tmp/..., ~/..., paths escaping the repo root, decided AFTER normalization).

NOT wired into scripts/gate.sh until the false-positive rate is witnessed.

Usage:
    python3 python/audit_citation_status.py                 # table of WARN/ERROR
    python3 python/audit_citation_status.py --check         # exit 1 on any ERROR
    python3 python/audit_citation_status.py --promote-untracked   # untracked-pending -> ERROR
    python3 python/audit_citation_status.py --promote-missing     # missing-pending-M -> ERROR
    python3 python/audit_citation_status.py --file <audit-dir>    # restrict to one audit dir
    python3 python/audit_citation_status.py --pass          # also list tracked/allowlisted passes
"""
import posixpath
import re
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
AUDITS_DIR = REPO_ROOT / "audits"

# Severity sublabels.
PASS_TRACKED = "pass-tracked"
PASS_ALLOWLIST = "pass-allowlist"
UNTRACKED = "untracked-pending"
MISSING = "missing-pending-M"
AMBIG = "grammar-ambiguous"

# Inline code spans on a single line: `...`.  (Fenced ``` blocks are multi-line and
# excluded by construction — a fence open/close line is `````, no inner `/`.)
INLINE = re.compile(r"`([^`]+)`")

# Arity / predicate indicator: ends with /N or /N-M after a name or mod:pred.
#   write_entries/3, diagnostic_summary:verdict_join/3, assess_scaffold_need/2-3
ARITY = re.compile(r"/\d+(-\d+)?$")

# Line / range / symbol anchor at the end, INCLUDING en-dash (–) and em-dash (—) ranges,
# comma-separated line lists, and a trailing :symbol code reference.
#   foo.pl:100, foo.pl:100-111, foo.pl:100–111, foo.py:171,207, foo.py:_grid_alignment_errors
ANCHOR = re.compile(r":(\d[\d,–—-]*|[A-Za-z_]\w*)$")

# Single brace-expansion group: trap.{json,pl} -> trap.json, trap.pl
BRACE = re.compile(r"^(.*)\{([^{}]*)\}(.*)$")

# A trailing file extension on the last path segment (.json, .pl, .md, ...).
EXT = re.compile(r"\.[A-Za-z0-9]+$")

# Shell / operator / template debris that is never a citation.
#   () | < > $  shell/operators;  [ ]  template placeholders (prolog/[core], stakeholders[].x)
_OPERATOR_CHARS = set("()|<>$[]")
_OPERATOR_SUBSTR = ("assertz", "2>", "retractall", "asserta", "use_module")


def repo_top_heads():
    """Set of valid first-segment heads = tracked top-level dirs + tracked top-level files."""
    out = subprocess.run(
        ["git", "ls-files"], cwd=REPO_ROOT, capture_output=True, text=True, check=True
    ).stdout.splitlines()
    heads = set()
    for p in out:
        seg = p.split("/", 1)
        heads.add(seg[0])  # both 'prolog' (dir) and 'README.md' (top-level file)
    return heads


def tracked_sets():
    """Return (tracked_files, tracked_dirs): exact tracked paths + every parent prefix."""
    out = subprocess.run(
        ["git", "ls-files"], cwd=REPO_ROOT, capture_output=True, text=True, check=True
    ).stdout.splitlines()
    files = set(out)
    dirs = set()
    for p in out:
        parts = p.split("/")
        for i in range(1, len(parts)):
            dirs.add("/".join(parts[:i]))
    return files, dirs


def expand_braces(token):
    """trap.{json,pl} -> [trap.json, trap.pl]; no brace -> [token]. Single group only."""
    m = BRACE.match(token)
    if not m:
        return [token]
    pre, body, post = m.group(1), m.group(2), m.group(3)
    members = [s.strip() for s in body.split(",") if s.strip()]
    if not members:
        return [token]
    return [pre + mem + post for mem in members]


def looks_like_arity(token):
    return bool(ARITY.search(token))


def looks_like_operator(token):
    if any(c in _OPERATOR_CHARS for c in token):
        return True
    return any(s in token for s in _OPERATOR_SUBSTR)


def looks_like_field_list(token, heads):
    """>=2 segments, first not a repo-top head, no extension anywhere, no trailing slash.
    Catches `accessibility_collapse/resistance`, NOT `evidence/summary.json` (extension)
    nor `outputs/essays/` (trailing slash) nor `prolog/testsets` (repo-top head)."""
    if token.endswith("/") or token.startswith("/") or token.startswith("~"):
        return False  # dir mention / absolute / home — handled downstream, not a field-list
    segs = token.split("/")
    if len(segs) < 2:
        return False
    if segs[0] in heads:
        return False
    if any(EXT.search(s) for s in segs):
        return False
    return True


def normalize(token):
    """Strip trailing punctuation and line/range anchors; map absolute-inside-repo and
    leading-slash-repo-root to repo-relative. Returns the normalized token (still possibly
    relative; resolution against repo-root vs writeup-dir happens in classify)."""
    t = token.strip()
    # Trailing prose punctuation (not '/').
    t = t.rstrip(",;")
    while t and t[-1] in ".,;:" and not t.endswith("/"):
        # Only strip a trailing '.' if it is not part of an extension we still need.
        if t[-1] == "." and EXT.search(t[:-0] if False else t) and not t.endswith("."):
            break
        # Strip trailing ':' / ';' / ',' always; trailing '.' only if it is dangling.
        if t[-1] == ".":
            # dangling sentence period: 'foo.json.' -> keep extension, drop only the dot
            # but a bare 'foo/.' is junk; simplest: drop a single trailing dot.
            t = t[:-1]
        else:
            t = t[:-1]
    # Line / range anchor (after punctuation strip so 'foo.pl:10,' already lost the comma).
    t = ANCHOR.sub("", t)
    # Absolute path inside this repo -> repo-relative.
    repo_prefix = str(REPO_ROOT) + "/"
    if t.startswith(repo_prefix):
        t = t[len(repo_prefix):]
    return t


def classify(token, writeup_dir, heads, files, dirs):
    """Return (status, normalized_path) for one already-brace-expanded token, or None to drop.

    writeup_dir is the audit dir of the citing .md, relative to repo root (e.g.
    'audits/2026-06-18_x'), used for writeup-relative resolution."""
    if "/" not in token:
        return None  # not a path
    if any(ch.isspace() for ch in token):
        # command illustration / prose with a path inside, not a single path citation
        return (AMBIG, token)
    if looks_like_arity(token):
        return None
    if looks_like_operator(token):
        return None
    if token.startswith("../") or token.startswith("./../"):
        # leading parent-traversal: the resolution anchor (repo-root? prolog/? writeup?)
        # is ambiguous — these point at gitignored outputs via a cwd-relative '..'. Do not
        # assert a clean "missing"; the grammar is not confident.
        return (AMBIG, token)

    # Home-dir and obvious ephemeral -> allowlist (by-design external).
    if token.startswith("~/") or token.startswith("~"):
        return (PASS_ALLOWLIST, token)
    if token.startswith("/tmp/") or token.startswith("tmp/"):
        return (PASS_ALLOWLIST, token)

    # Glob / ellipsis -> grammar-ambiguous (illustrative pattern, not a hard per-file citation).
    if "*" in token or "..." in token:
        return (AMBIG, token)

    if looks_like_field_list(token, heads):
        return None

    norm = normalize(token)
    if not norm or norm == "/":
        return None

    # Leading-slash: /prolog/... -> prolog/... iff first segment is a repo head; else escapes repo.
    if norm.startswith("/"):
        stripped = norm.lstrip("/")
        head = stripped.split("/", 1)[0]
        if head in heads:
            norm = stripped
        else:
            return (PASS_ALLOWLIST, token)  # absolute path escaping the repo

    is_dir_cite = norm.endswith("/")
    rel = norm.rstrip("/") if is_dir_cite else norm

    # Resolve against BOTH repo-root and the writeup dir. Resolution picks WHICH path;
    # the tracked-check still runs on it. Collapse '.'/'..' segments so './scripts/gate.sh'
    # and 'audits/a/../b/x' compare against the tracked set correctly; a candidate that
    # escapes the repo root after collapsing is allowlisted (external).
    candidates = []
    for base in ([rel] + ([f"{writeup_dir}/{rel}"] if writeup_dir else [])):
        collapsed = posixpath.normpath(base)
        if collapsed.startswith("..") or collapsed.startswith("/"):
            continue  # this resolution escapes the repo root; try the other
        candidates.append(collapsed)
    if not candidates:
        return (PASS_ALLOWLIST, token)  # every resolution escapes repo root (external)

    # 1) tracked (file or dir) on any candidate -> pass.
    for c in candidates:
        if c in files:
            return (PASS_TRACKED, c)
        if c in dirs:  # tracked directory, cited with or without a trailing slash
            return (PASS_TRACKED, c + ("/" if is_dir_cite else ""))
    # 2) A bare directory/location mention that is NOT a tracked dir reads as descriptive
    #    ("the pipeline writes to outputs/essays/"), not load-bearing file-evidence —
    #    grammar-ambiguous (not-a-citation disposition), whether or not it exists on disk.
    if is_dir_cite:
        return (AMBIG, candidates[0] + "/")
    # 3) a file that exists but is untracked -> the OQ-104 gitignored-evidence bucket.
    for c in candidates:
        if (REPO_ROOT / c).exists():
            return (UNTRACKED, c)
    # 4) neither resolves -> missing.
    return (MISSING, candidates[0])


def scan(restrict_dir=None):
    """Walk audits/*.md, extract citations, classify. Return (citations, problems).

    citations: list of dict(audit, lineno, raw, token, status, path).
    problems: parse-time issues (currently none expected; reserved for parity)."""
    heads = repo_top_heads()
    files, dirs = tracked_sets()
    citations, problems = [], []

    md_files = sorted(AUDITS_DIR.rglob("*.md"))
    for md in md_files:
        audit_rel = md.parent.relative_to(REPO_ROOT).as_posix()
        if restrict_dir and restrict_dir not in audit_rel and restrict_dir not in md.as_posix():
            continue
        try:
            text = md.read_text(encoding="utf-8", errors="replace")
        except OSError as e:
            problems.append(f"{md}: cannot read ({e})")
            continue
        for lineno, line in enumerate(text.splitlines(), 1):
            for raw in INLINE.findall(line):
                if "/" not in raw:
                    continue
                for token in expand_braces(raw):
                    res = classify(token, audit_rel, heads, files, dirs)
                    if res is None:
                        continue
                    status, path = res
                    citations.append({
                        "audit": audit_rel, "lineno": lineno, "raw": raw,
                        "token": token, "status": status, "path": path,
                    })
    return citations, problems


# Statuses that --promote-* can lift to ERROR (grammar-ambiguous is NEVER promotable).
PROMOTABLE = {UNTRACKED, MISSING}


def main():
    args = sys.argv[1:]
    restrict = None
    if "--file" in args:
        try:
            restrict = args[args.index("--file") + 1]
        except IndexError:
            print("usage: audit_citation_status.py --file <audit-dir>", file=sys.stderr)
            sys.exit(2)

    citations, problems = scan(restrict)

    promote = set()
    if "--promote-untracked" in args:
        promote.add(UNTRACKED)
    if "--promote-missing" in args:
        promote.add(MISSING)

    def effective(status):
        return "ERROR" if status in promote else status

    # Dedup to distinct (path, status) for the summary headline (a path cited in
    # several audits is one violation to remediate) while keeping per-citation rows.
    counts = {}
    for c in citations:
        eff = effective(c["status"])
        counts[eff] = counts.get(eff, 0) + 1

    show_pass = "--pass" in args
    is_check = "--check" in args

    # Per-citation rows (skip passes unless --pass).
    rows = []
    for c in citations:
        eff = effective(c["status"])
        if eff in (PASS_TRACKED, PASS_ALLOWLIST) and not show_pass:
            continue
        rows.append(c)

    if not is_check:
        for c in sorted(rows, key=lambda r: (r["status"], r["audit"], r["lineno"])):
            print(f"{effective(c['status'])}\t{c['audit']}\t{c['lineno']}\t{c['path']}")

    distinct_untracked = sorted({c["path"] for c in citations if c["status"] == UNTRACKED})
    distinct_missing = sorted({c["path"] for c in citations if c["status"] == MISSING})
    distinct_ambig = sorted({c["path"] for c in citations if c["status"] == AMBIG})

    n_errors = sum(1 for c in citations if effective(c["status"]) == "ERROR")

    print("---", file=sys.stderr)
    print(f"audit .md scanned: {len(set(c['audit'] for c in citations))} dirs; "
          f"{len(citations)} path-citations", file=sys.stderr)
    print(" ".join(f"{k}:{v}" for k, v in sorted(counts.items())), file=sys.stderr)
    print(f"distinct untracked-pending: {len(distinct_untracked)}", file=sys.stderr)
    print(f"distinct missing-pending-M: {len(distinct_missing)}", file=sys.stderr)
    print(f"distinct grammar-ambiguous: {len(distinct_ambig)}", file=sys.stderr)
    for p in problems:
        print(f"PROBLEM: {p}", file=sys.stderr)

    if is_check:
        print(f"ERRORS: {n_errors}", file=sys.stderr)
        sys.exit(1 if (n_errors or problems) else 0)


if __name__ == "__main__":
    main()
