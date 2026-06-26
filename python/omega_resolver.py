#!/usr/bin/env python3
"""omega_resolver.py — read-only catalog loader + authority control + frontier
view over ISSUES.md (the omega-resolver pilot; OQ-129 family).

This is the pilot apparatus described in
`~/.claude/plans/brief-the-omega-glittery-wozniak.md` §B/§D/§E. It does NOT
migrate ISSUES.md to an `issues/` directory (scale-time only); it reads the
existing prose plus the one authored field `**Deps:**`.

What it does (all READ-ONLY; never writes ISSUES.md):
  * parse  — walk `## OQ-NNN` headers, extract status, Ω-type, references,
             witnesses, and the authored `**Deps:**` typed edges.
  * authority-control — validate each token fail-closed against its authority
             (Deps target -> existing OQ set; File -> filesystem; witness ->
             git/audits/KNOWN_STATE; predicate -> current_predicate dump).
             Each authority enumeration is positive-controlled non-empty before
             it may judge (Build Discipline Pattern 5).
  * frontier — bucket the active OQs over the SCC CONDENSATION of the Deps
             blocking graph (§D), so a mutual block is one `standoff`, never a
             pair of `blocked` dead-ends and never a hang.
  * check  — flag a dangling `Deps:` target and a `resolved` entry whose witness
             no longer resolves on disk/git; pass the clean set.
  * selftest — planted-fixture positive controls (§D 2-cycle -> exactly one
             standoff; dangling-dep flagged; rotted witness flagged; authority
             lists non-empty). A loader is not trusted until its controls fire.

Every view stamps a manifest (§1b): store git HEAD, schema_version, generated_at.

Usage:
    python3 python/omega_resolver.py menu         # ← "what should I work on next" (run THIS, not the file)
    python3 python/omega_resolver.py frontier     # the routing view as JSON (machine artifact)
    python3 python/omega_resolver.py check        # checker (exit 1 on problems)
    python3 python/omega_resolver.py selftest     # positive controls (exit 1 on fail)
    python3 python/omega_resolver.py dump         # parsed access points, per OQ
    python3 python/omega_resolver.py index        # (re)write issues/INDEX.{md,json} router
    python3 python/omega_resolver.py index --check # exit 1 if the router is stale vs ISSUES.md

The `index` command derives a compact ROUTER over ISSUES.md (a scan surface +
machine artifact under `issues/`) — generated FROM ISSUES.md, never authoritative
itself. It mirrors current resolver/parser behavior; it does not adjudicate. The
`index --check` line in scripts/gate.sh is the regenerate-after-edit hook: editing
ISSUES.md without re-running `index` turns [GATE] red. This is the index-only path;
the `issues/OQ-NN.md` per-entry split is deferred to scale-time (see OQ-141 note and
the per-file split threshold in cmd_index).
"""
import datetime
import hashlib
import json
import re
import subprocess
import sys
from pathlib import Path

import issues_status  # canonical status authority (does NOT fork a third parser)

SCHEMA_VERSION = "omega-resolver/1"
ROOT = Path(__file__).resolve().parents[1]
ISSUES = ROOT / "ISSUES.md"
ISSUES_DIR = ROOT / "issues"

HEADER = re.compile(r"^## (OQ-\d+)\b(.*)$")
STATUS = re.compile(r"^\*\*Status:\*\* (\w+)(?: — .*)?$")
OMEGA = re.compile(r"^\*\*Ω-type:\*\* (Ω_[ECP])\b")
FILES = re.compile(r"^\*\*Files?:\*\*\s*(.+)$")
DEPS = re.compile(r"^\*\*Deps:\*\*\s*(.+)$")
# Authored priority hint (1-10, 1 = highest). The OQ's author declares it to help
# the operator judge; it is a DECLARED estimate surfaced by `menu`, never a computed
# value (priority/value is the operator's seat — see OQ-130 #1). Absent => unranked.
PRIORITY = re.compile(r"^\*\*Priority:\*\*\s*(\d{1,2})\b")
WHATCHANGES = re.compile(r"^\*\*What resolution (?:would )?changes?:\*\*\s*(.+)$", re.I)

ACTIVE = {"open", "investigating", "partial"}
# relators whose edge BLOCKS (contributes to reachability); others are grouping.
BLOCKING_RELATORS = {"blocked_on", "gates"}
GROUPING_RELATORS = {"bundled_with", "splits_from"}
# blocked_on_human: a live human/operator/substrate gate that is NOT an OQ edge
# (e.g. "gated on operator spend-go", "blocked on substrate"). Surfaced by the
# pilot: active Ω_E entries can be human-gated without any Ω_P blocker OQ, which
# the OQ->OQ model alone mis-buckets as workable_now. Target is free text.
HUMAN_RELATOR = "blocked_on_human"
ALL_RELATORS = BLOCKING_RELATORS | GROUPING_RELATORS | {HUMAN_RELATOR}

# Signature of ONE edge: `blocked_on_human` (free-text target) OR an OQ-relator
# immediately followed by an OQ ref. Used to catch edges PACKED into a single
# comma-chunk (e.g. `bundled_with OQ-1; bundled_with OQ-2`), which the comma-only
# splitter silently DROPS to its first edge — the exact failure that let OQ-136/
# 137/138 register fewer deps than authored with no error. `blocked_on_human`
# first (it has no OQ ref) so it isn't shadowed by the alternation.
_EDGE_SIG = re.compile(
    r"\bblocked_on_human\b|\b(?:blocked_on|gates|bundled_with|splits_from)\b\s+OQ-\d+")

OQREF = re.compile(r"OQ-\d+")
COMMIT = re.compile(r"\b[0-9a-f]{8,40}\b")


class Entry:
    def __init__(self, oq, lineno, title=""):
        self.oq = oq
        self.lineno = lineno
        self.title = title       # the header text after "OQ-NN —"
        self.status = None
        self.omega = None
        self.priority = None     # authored 1-10 hint (1=highest); None => unranked
        self.whatchanges = None  # first "what resolution changes" line (menu context)
        self.files = []          # raw file/line ref strings
        self.deps = []           # list of (relator, target_oq)
        self.body = []           # all body lines (for witness scan)

    @property
    def active(self):
        return self.status in ACTIVE


# --------------------------------------------------------------------------- #
# parse
# --------------------------------------------------------------------------- #
def parse_entries(text=None):
    text = text if text is not None else ISSUES.read_text()
    entries = {}
    cur = None
    problems = []
    for lineno, line in enumerate(text.splitlines(), 1):
        m = HEADER.match(line)
        if m:
            title = m.group(2).strip().lstrip("—-").strip()
            cur = Entry(m.group(1), lineno, title=title)
            if cur.oq in entries:
                problems.append(f"{cur.oq} (line {lineno}): duplicate OQ label")
            entries[cur.oq] = cur
            continue
        if cur is None:
            continue
        cur.body.append(line)
        if cur.status is None:
            sm = STATUS.match(line)
            if sm:
                cur.status = sm.group(1)
        if cur.omega is None:
            om = OMEGA.match(line)
            if om:
                cur.omega = om.group(1)
        fm = FILES.match(line)
        if fm:
            cur.files.extend(_split_file_refs(fm.group(1)))
        dm = DEPS.match(line)
        if dm:
            deps, dp = _parse_deps(cur.oq, dm.group(1))
            cur.deps.extend(deps)
            problems.extend(dp)
        if cur.priority is None:
            pm = PRIORITY.match(line)
            if pm:
                cur.priority = int(pm.group(1))
        if cur.whatchanges is None:
            wm = WHATCHANGES.match(line)
            if wm:
                cur.whatchanges = wm.group(1).strip()
    return entries, problems


def _split_file_refs(s):
    # files are backtick-quoted or bare paths, comma/space separated
    refs = re.findall(r"`([^`]+)`", s)
    if refs:
        return [r.strip() for r in refs]
    return [tok.strip() for tok in re.split(r"[,\s]+", s) if "/" in tok or "." in tok]


def _chunk_deps(s):
    """Split a `**Deps:**` value into per-edge chunks on commas, BUT keep commas
    that belong to a `blocked_on_human` free-text target (its target is prose and
    may legitimately contain commas). Rule: a comma starts a new edge only if the
    text after it begins with a known relator keyword; otherwise the comma is
    free text and is re-joined into the current (human) edge. This preserves the
    unknown-relator / no-OQ-target / packed-edge detection below — a stray
    non-relator chunk that does NOT follow a human edge is still passed through to
    be flagged."""
    chunks, prev_is_human = [], False
    for raw in s.split(","):
        toks = raw.split()
        first = toks[0] if toks else ""
        if first in ALL_RELATORS:
            chunks.append(raw)
            prev_is_human = (first == HUMAN_RELATOR)
        elif prev_is_human and chunks:
            # comma inside a blocked_on_human free-text target — re-join, stay human
            chunks[-1] = chunks[-1] + "," + raw
        else:
            chunks.append(raw)   # let the parser flag malformed / unknown-relator
            prev_is_human = False
    return chunks


def _parse_deps(oq, s):
    """`**Deps:** blocked_on OQ-122, bundled_with OQ-50, blocked_on_human <prose>`.
    Returns (list[(relator, target)], problems). A `blocked_on_human` free-text
    target may contain commas (see _chunk_deps)."""
    deps, problems = [], []
    for chunk in _chunk_deps(s):
        chunk = chunk.strip()
        if not chunk:
            continue
        parts = chunk.split()
        if len(parts) < 2:
            problems.append(f"{oq}: malformed Deps chunk {chunk!r}")
            continue
        relator = parts[0]
        if relator not in ALL_RELATORS:
            problems.append(f"{oq}: unknown relator {relator!r} in {chunk!r}")
            continue
        if relator == HUMAN_RELATOR:
            # free-text target (a human/external gate, not an OQ); commas allowed
            target = " ".join(parts[1:]) or "<unspecified>"
            deps.append((relator, target))
            continue
        # Silent-drop guard (OQ-target relators only — a human target is prose and
        # may legitimately mention a relator word or OQ ref): comma is the chunk
        # delimiter, so >1 edge in a single chunk (typically `;`-joined) parses to
        # the first and DROPS the rest with no error. Flag it — comma-separate.
        if len(_EDGE_SIG.findall(chunk)) > 1:
            problems.append(
                f"{oq}: multiple edges packed in one comma-chunk (only the first "
                f"registers; separate each with a comma): {chunk!r}")
        target_m = OQREF.search(chunk)
        if not target_m:
            problems.append(f"{oq}: Deps chunk has no OQ target: {chunk!r}")
            continue
        deps.append((relator, target_m.group(0)))
    return deps, problems


# --------------------------------------------------------------------------- #
# authority control (fail-closed; each list positive-controlled non-empty)
# --------------------------------------------------------------------------- #
def predicate_authority():
    """current_predicate dump if present, else None (skipped, loudly)."""
    dump = ROOT / "outputs" / "current_predicates.txt"
    if dump.exists():
        preds = {l.strip() for l in dump.read_text().splitlines() if l.strip()}
        return preds or None
    return None


def authority_report(entries):
    """Validate tokens fail-closed. Returns dict of {kind: {ok, bad, empty}}."""
    oqset = set(entries)
    rep = {}

    # --- Deps target authority: the existing OQ set ---
    assert oqset, "authority list EMPTY (OQ set) — would pass everything (Pattern 5)"
    dep_bad = []
    for e in entries.values():
        for relator, target in e.deps:
            if relator == HUMAN_RELATOR:
                continue                         # free-text human gate, not an OQ
            if target not in oqset:
                dep_bad.append(f"{e.oq}: dangling Deps target {target} ({relator})")
    rep["deps_target"] = {"authority_size": len(oqset), "bad": dep_bad}

    # --- File authority: filesystem ---
    file_bad = []
    for e in entries.values():
        for ref in e.files:
            path = ref.split(":")[0]            # strip :line-range (soft)
            if not (ROOT / path).exists() and not Path(path).exists():
                file_bad.append(f"{e.oq}: File not found: {path}")
    rep["file"] = {"authority_size": "filesystem", "bad": file_bad}

    return rep


# --------------------------------------------------------------------------- #
# witness resolution (for the checker's resolved-with-rotted-witness rule, §3)
# --------------------------------------------------------------------------- #
def _commit_exists(h):
    try:
        return subprocess.run(["git", "cat-file", "-e", h + "^{commit}"],
                              cwd=ROOT, capture_output=True).returncode == 0
    except Exception:
        return False


def witness_status(entry):
    """Scan an entry body for DURABLE witness tokens (the ones a fresh clone
    can verify) and whether each resolves. Returns list of (token, kind, ok).

    Durable = audit dirs (`audits/DATE_slug/`) and git commit hashes. Crucially
    NOT `outputs/*.json`: those are gitignored/regenerable (gone on a fresh
    clone), which is the whole reason audit dirs carry evidence — so an outputs/
    path is never the durable witness and must not flag a resolved entry. The
    entry-level rule (in check()) is `>=1 durable witness resolves`, not
    `every token resolves`: a stray truncated prose fragment must not fail an
    entry that also cites a real audit dir (the OQ-92 over-fire, witnessed)."""
    out = []
    body = "\n".join(entry.body)
    for m in re.finditer(r"audits/(\d{4}-\d{2}-\d{2}_[A-Za-z0-9_]+)/?", body):
        d = ROOT / "audits" / m.group(1)
        out.append((m.group(0), "audit_dir", d.is_dir()))
    for m in re.finditer(COMMIT, body):
        h = m.group(0)
        if _commit_exists(h):
            out.append((h, "commit", True))
    return out


# --------------------------------------------------------------------------- #
# §D — SCC condensation of the blocking graph + reachability buckets
# --------------------------------------------------------------------------- #
def blocking_graph(entries, active_only=True):
    """Directed blocking edges S->T meaning 'S waits for T'.
    blocked_on TARGET -> S->TARGET ; gates TARGET -> TARGET->S (reverse)."""
    nodes = {oq for oq, e in entries.items() if (e.active or not active_only)}
    adj = {n: set() for n in nodes}
    for e in entries.values():
        for relator, target in e.deps:
            if relator not in BLOCKING_RELATORS:
                continue
            if relator == "blocked_on":
                s, t = e.oq, target
            else:  # gates: e enables target -> target waits for e
                s, t = target, e.oq
            if s in nodes and t in nodes:
                adj[s].add(t)
    return nodes, adj


def tarjan_scc(nodes, adj):
    """Tarjan SCC. Returns list of components (each a list of nodes)."""
    index = {}
    low = {}
    onstack = {}
    stack = []
    sccs = []
    counter = [0]
    sys.setrecursionlimit(10000)

    def strongconnect(v):
        index[v] = low[v] = counter[0]
        counter[0] += 1
        stack.append(v)
        onstack[v] = True
        for w in adj.get(v, ()):
            if w not in index:
                strongconnect(w)
                low[v] = min(low[v], low[w])
            elif onstack.get(w):
                low[v] = min(low[v], index[w])
        if low[v] == index[v]:
            comp = []
            while True:
                w = stack.pop()
                onstack[w] = False
                comp.append(w)
                if w == v:
                    break
            sccs.append(comp)

    for v in nodes:
        if v not in index:
            strongconnect(v)
    return sccs


def frontier(entries):
    """Bucket active OQs over the SCC condensation (§D)."""
    nodes, adj = blocking_graph(entries, active_only=True)
    sccs = tarjan_scc(nodes, adj)

    # map node -> scc id; build condensation adjacency
    comp_of = {}
    for i, comp in enumerate(sccs):
        for n in comp:
            comp_of[n] = i
    nontrivial = {}     # scc id -> members (>=2 or self-loop)
    for i, comp in enumerate(sccs):
        selfloop = len(comp) == 1 and comp[0] in adj.get(comp[0], set())
        if len(comp) >= 2 or selfloop:
            nontrivial[i] = comp

    # cross-SCC blockers per scc
    cond_adj = {i: set() for i in range(len(sccs))}
    for s in nodes:
        for t in adj[s]:
            if comp_of[s] != comp_of[t]:
                cond_adj[comp_of[s]].add(comp_of[t])

    def omega_of(oq):
        return entries[oq].omega

    buckets = {"workable_now": [], "blocked_on_human": [], "blocked": [],
               "standoff": []}

    def human_gated(oq):
        return any(rel == HUMAN_RELATOR for rel, _ in entries[oq].deps)

    for i, comp in enumerate(sccs):
        if i in nontrivial:
            buckets["standoff"].append(sorted(comp))
            continue
        oq = comp[0]
        if human_gated(oq):                          # authored live human gate
            buckets["blocked_on_human"].append(oq)
            continue
        blockers = cond_adj[i]                       # super-nodes this waits on
        if not blockers:
            # leaf: workable now, unless it is itself an Ω_P (routes to a human)
            if omega_of(oq) == "Ω_P":
                buckets["blocked_on_human"].append(oq)
            else:
                buckets["workable_now"].append(oq)
        else:
            # read the Ω-type of the REMAINING blockers (§5)
            blocker_oqs = [m for b in blockers for m in sccs[b]]
            if all(omega_of(b) == "Ω_P" for b in blocker_oqs):
                buckets["blocked_on_human"].append(oq)
            else:
                buckets["blocked"].append(oq)
    return buckets, sccs, nontrivial


# --------------------------------------------------------------------------- #
# manifest (§1b)
# --------------------------------------------------------------------------- #
def manifest():
    try:
        head = subprocess.run(["git", "rev-parse", "HEAD"], cwd=ROOT,
                              capture_output=True, text=True).stdout.strip()
    except Exception:
        head = "unknown"
    return {"store_version": head, "schema_version": SCHEMA_VERSION,
            "generated_at": datetime.datetime.now().isoformat(timespec="seconds")}


# --------------------------------------------------------------------------- #
# checker (§3)
# --------------------------------------------------------------------------- #
def check(entries, parse_problems=()):
    # Parse-layer problems (unknown relator, no OQ target, malformed/packed Deps,
    # duplicate OQ label) are real authority failures, not just stderr noise: a
    # dropped/mis-typed edge silently corrupts the frontier. Fail the gate on them.
    problems = list(parse_problems)
    auth = authority_report(entries)
    problems += auth["deps_target"]["bad"]
    # resolved-with-rotted-witness, entry-level: flag only when the entry CITES
    # a durable audit-dir witness and NONE of its durable witnesses resolve.
    for e in entries.values():
        if e.status not in ("resolved", "disposed"):
            continue
        ws = witness_status(e)
        audit_toks = [t for t, k, ok in ws if k == "audit_dir"]
        if not audit_toks:
            continue                              # no durable audit witness cited (advisory only)
        if not any(ok for _, _, ok in ws):        # cited audit dir(s) but nothing resolves
            problems.append(
                f"{e.oq} ({e.status}): rotted witness — cites {audit_toks} but no "
                f"durable witness (audit dir / commit) resolves")
    return problems


# --------------------------------------------------------------------------- #
# selftest — planted-fixture positive controls (§D, step 2)
# --------------------------------------------------------------------------- #
FIXTURE = """\
## OQ-9001 planted leaf workable
**Status:** open
**Ω-type:** Ω_C (test)
**Deps:** blocked_on OQ-9002

## OQ-9002 planted leaf resolved
**Status:** resolved
**Ω-type:** Ω_C (test)
witness audits/2026-06-14_extraction_blindness_existential_label/ should resolve

## OQ-9003 planted human-gated
**Status:** open
**Ω-type:** Ω_P (test)

## OQ-9004 planted cycle member A
**Status:** open
**Ω-type:** Ω_C (test)
**Deps:** blocked_on OQ-9005

## OQ-9005 planted cycle member B
**Status:** open
**Ω-type:** Ω_C (test)
**Deps:** blocked_on OQ-9004

## OQ-9006 planted dangling dep
**Status:** open
**Ω-type:** Ω_C (test)
**Deps:** blocked_on OQ-9999

## OQ-9007 planted rotted witness
**Status:** resolved
**Ω-type:** Ω_C (test)
see audits/2026-01-01_this_dir_does_not_exist/ for proof

## OQ-9008 planted human-gated Ω_E (no Ω_P blocker OQ)
**Status:** open
**Ω-type:** Ω_E (test)
**Deps:** blocked_on_human operator-spend-go

## OQ-9009 resolved with one real + one truncated/fake audit dir (must NOT flag)
**Status:** resolved
**Ω-type:** Ω_C (test)
real witness audits/2026-06-14_extraction_blindness_existential_label/ plus a
stray truncated mention audits/2026-06-14_extraction_blindness_existential_ that
formatting clipped — the entry IS witnessed by the first, must not flag.

## OQ-9010 planted prose-on-Deps (unknown relator after comma — must be flagged)
**Status:** open
**Ω-type:** Ω_C (test)
**Deps:** splits_from OQ-9001, the rest of this line is prose not a typed relator

## OQ-9011 planted packed edges (`;`-joined — silently drops, must be flagged)
**Status:** open
**Ω-type:** Ω_C (test)
**Deps:** bundled_with OQ-9001; bundled_with OQ-9002

## OQ-9012 planted comma in blocked_on_human free text (must parse CLEAN, comma kept)
**Status:** open
**Ω-type:** Ω_E (test)
**Deps:** blocked_on_human GAP-08 §7 immovability signal, routed to a design gap, not an OQ edge
"""


def selftest():
    entries, parse_probs = parse_entries(FIXTURE)
    fails = []

    # control 1: §D 2-cycle -> exactly one standoff naming both members
    buckets, sccs, nontrivial = frontier(entries)
    standoffs = buckets["standoff"]
    cyc = [s for s in standoffs if set(s) == {"OQ-9004", "OQ-9005"}]
    if len(cyc) != 1:
        fails.append(f"§D control FAILED: expected one standoff {{9004,9005}}, got {standoffs}")
    # negative side of the control: a real leaf must NOT be in standoff
    if any("OQ-9001" in s for s in standoffs):
        fails.append("§D control FAILED: non-cycle node landed in standoff")

    # control 2: leaf with a resolved blocker is workable_now
    if "OQ-9001" not in buckets["workable_now"]:
        fails.append(f"workable control FAILED: OQ-9001 not workable_now ({_where(buckets,'OQ-9001')})")
    # control 3: Ω_P leaf -> blocked_on_human
    if "OQ-9003" not in buckets["blocked_on_human"]:
        fails.append(f"Ω_P control FAILED: OQ-9003 not blocked_on_human ({_where(buckets,'OQ-9003')})")

    # control 4: dangling dep flagged
    auth = authority_report(entries)
    if not any("OQ-9999" in b for b in auth["deps_target"]["bad"]):
        fails.append("dangling-dep control FAILED: OQ-9999 not flagged")

    # control 5: rotted witness flagged; live witness NOT flagged; and the
    # over-fire negative control (one real + one truncated audit dir) NOT flagged.
    probs = check(entries, parse_probs)
    rotted = [p for p in probs if "rotted" in p]   # scope to rotted-witness problems
    if not any(p.startswith("OQ-9007") for p in rotted):
        fails.append("rotted-witness control FAILED: OQ-9007 not flagged")
    if any(p.startswith("OQ-9002") for p in rotted):
        fails.append("witness false-positive: live OQ-9002 witness flagged as rotted")
    if any(p.startswith("OQ-9009") for p in rotted):
        fails.append("over-fire control FAILED: OQ-9009 (has a real audit dir) flagged as rotted")

    # control 6: authority lists non-empty (Pattern 5)
    if auth["deps_target"]["authority_size"] < 1:
        fails.append("authority-nonempty control FAILED: OQ set empty")

    # control 7: human-gated Ω_E (no Ω_P blocker) -> blocked_on_human, NOT workable_now
    if "OQ-9008" not in buckets["blocked_on_human"]:
        fails.append(f"human-gate control FAILED: OQ-9008 not blocked_on_human ({_where(buckets,'OQ-9008')})")
    if "OQ-9008" in buckets["workable_now"]:
        fails.append("human-gate control FAILED: OQ-9008 mis-bucketed workable_now")

    # control 8: malformed Deps caught at BOTH layers, two-sided.
    #  (a) prose-after-comma (unknown relator) surfaces as a parse problem;
    #  (b) `;`-packed edges (the SILENT-drop case) surface as a parse problem;
    #  (c) check() now FAILS on those parse problems (folded in, exit 1);
    #  (d) negative side: no WELL-FORMED fixture entry (OQ-9001..9009) trips it.
    # match on the problem's OWNING entry (prefix), not substring — a packed-edge
    # problem quotes the chunk text, which contains other OQ refs that would
    # collide with substring checks (this bit the first draft of this control).
    if not any(p.startswith("OQ-9010") for p in parse_probs):
        fails.append("malformed-Deps control FAILED: OQ-9010 prose-on-Deps not parse-flagged")
    if not any(p.startswith("OQ-9011") and "packed" in p for p in parse_probs):
        fails.append("packed-Deps control FAILED: OQ-9011 silent-drop not parse-flagged")
    if not any(p.startswith("OQ-9010") for p in probs):
        fails.append("malformed-Deps control FAILED: check() did not fail on OQ-9010")
    wellformed = {f"OQ-900{n}" for n in range(1, 10)}   # 9001..9009 are all well-formed
    overfire = [p for p in parse_probs if any(p.startswith(w) for w in wellformed)]
    if overfire:
        fails.append(f"malformed-Deps over-fire: well-formed entry parse-flagged: {overfire}")

    # control 9: a blocked_on_human free-text target may contain commas — the
    # comma is kept as free text, NOT mis-split into a new edge / unknown relator
    # (the bug fixed 2026-06-18). Positive: OQ-9012 parses clean AND registers
    # exactly one blocked_on_human edge whose target carries the comma'd prose.
    if any(p.startswith("OQ-9012") for p in parse_probs):
        bad = [p for p in parse_probs if p.startswith("OQ-9012")]
        fails.append(f"human-comma control FAILED: OQ-9012 free-text comma parse-flagged: {bad}")
    e12 = entries.get("OQ-9012")
    human12 = [t for (r, t) in (e12.deps if e12 else []) if r == HUMAN_RELATOR]
    if not (len(human12) == 1 and "," in human12[0]):
        fails.append(f"human-comma control FAILED: OQ-9012 did not register one comma-bearing human edge ({human12})")

    return fails, parse_probs


def _where(buckets, oq):
    for k, v in buckets.items():
        flat = [x for item in v for x in (item if isinstance(item, list) else [item])]
        if oq in flat:
            return k
    return "<none>"


# --------------------------------------------------------------------------- #
# CLI
# --------------------------------------------------------------------------- #
def cmd_dump(entries):
    for oq in sorted(entries, key=lambda o: int(o.split("-")[1])):
        e = entries[oq]
        print(f"{oq}\tstatus={e.status}\tomega={e.omega}\t"
              f"deps={e.deps}\tfiles={len(e.files)}")


def cmd_frontier(entries):
    buckets, sccs, nontrivial = frontier(entries)
    out = {"manifest": manifest(),
           "n_active": sum(1 for e in entries.values() if e.active),
           "buckets": buckets,
           "n_scc": len(sccs),
           "n_standoff": len(nontrivial)}
    print(json.dumps(out, indent=2, ensure_ascii=False))


def _pri(e):
    return e.priority if e.priority is not None else 99


def _by_pri(entries):
    return lambda oq: (_pri(entries[oq]), int(oq.split("-")[1]))


def bundled_components(entries):
    """Union-find over the UNDIRECTED `bundled_with` graph across ALL entries.
    Returns {oq -> root_oq}. `bundled_with` is a symmetric family relator (often
    authored on one side only), so we union both directions and let two workable
    items group via a shared hub even if the hub is itself blocked/resolved.
    `splits_from` is deliberately NOT folded in here — this prototype groups the
    `bundled_with` family only (the question asked)."""
    parent = {}

    def find(x):
        parent.setdefault(x, x)
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    def union(a, b):
        ra, rb = find(a), find(b)
        if ra != rb:
            parent[max(ra, rb)] = min(ra, rb)  # deterministic root

    for e in entries.values():
        for rel, tgt in e.deps:
            if rel == "bundled_with":
                union(e.oq, tgt)
    return {oq: find(oq) for oq in parent}


def cmd_menu(entries):
    """Human-readable 'what should I work on next' surface. Run THIS instead of
    reading ISSUES.md whole. Workable items are sorted by AUTHORED Priority
    (1=highest); the ranking is the operator's declared seat, surfaced — not
    computed. The coverage footer states how trustworthy the frontier is."""
    buckets, sccs, nontrivial = frontier(entries)
    man = manifest()
    n_active = sum(1 for e in entries.values() if e.active)
    print("# Omega frontier — what's workable now  (authored Priority; 1=highest)")
    print(f"# store={man['store_version'][:8]} generated={man['generated_at']} "
          f"active={n_active}\n")

    wk = sorted(buckets["workable_now"], key=_by_pri(entries))

    def _print_item(oq, indent="  "):
        e = entries[oq]
        pri = f"P{e.priority}" if e.priority is not None else "P–"
        print(f"{indent}[{pri:>3}] {oq}  {e.title[:72]}")
        wc = (e.whatchanges or "").split(". ")[0][:96]
        line = f"{indent}       {e.omega or 'Ω?'}"
        if wc:
            line += f" · changes: {wc}"
        print(line)

    # group workable items by bundled_with family (connected component over the
    # bundled_with graph). Items sharing a component print together under a ⧉
    # header; singletons fall through to a flat unbundled list.
    comp_of = bundled_components(entries)
    fams = {}
    for oq in wk:
        root = comp_of.get(oq)
        if root is not None:
            fams.setdefault(root, []).append(oq)
    multi = {r: m for r, m in fams.items() if len(m) >= 2}   # >=2 WORKABLE members
    grouped = {oq for m in multi.values() for oq in m}
    singles = [oq for oq in wk if oq not in grouped]
    n_fam = len(multi)

    print(f"## WORKABLE NOW ({len(wk)}) — pick from these"
          + (f"; ⧉ = bundled family ({n_fam})" if n_fam else ""))
    # families first, ordered by their best (lowest) member priority
    for root in sorted(multi, key=lambda r: _by_pri(entries)(
            min(multi[r], key=_by_pri(entries)))):
        members = sorted(multi[root], key=_by_pri(entries))
        # surface non-workable siblings in the same family for context
        sibs = sorted(o for o, r in comp_of.items()
                      if r == root and o not in set(members))
        sib_note = f"  (+ {', '.join(sibs)})" if sibs else ""
        print(f"  ⧉ family {', '.join(members)}{sib_note}")
        for oq in members:
            _print_item(oq, indent="    ")
    if multi and singles:
        print("  · unbundled")
    for oq in singles:
        _print_item(oq, indent="  ")

    if buckets["blocked_on_human"]:
        b = sorted(buckets["blocked_on_human"], key=_by_pri(entries))
        print(f"\n## BLOCKED ON YOU ({len(b)}) — needs a ruling / spend-go / Ω_P decision")
        for oq in b:
            e = entries[oq]
            gate = next((t for r, t in e.deps if r == HUMAN_RELATOR), e.omega or "")
            print(f"  {oq}  {e.title[:58]}  ({gate})")

    if buckets["standoff"]:
        print(f"\n## STANDOFFS ({len(buckets['standoff'])}) — mutually-blocked; you cut the cycle")
        for grp in buckets["standoff"]:
            print(f"  {' ↔ '.join(grp)}")

    if buckets["blocked"]:
        print(f"\n## BLOCKED ({len(buckets['blocked'])}) — waiting on another OQ")
        for oq in sorted(buckets["blocked"], key=lambda o: int(o.split("-")[1])):
            e = entries[oq]
            waits = [t for r, t in e.deps if r in BLOCKING_RELATORS]
            print(f"  {oq}  {e.title[:52]}  → waits on {', '.join(waits) or '?'}")

    n_dep = sum(1 for e in entries.values() if e.active and e.deps)
    n_pri = sum(1 for e in entries.values() if e.active and e.priority is not None)
    print(f"\n# coverage: {n_dep}/{n_active} active OQs have authored Deps · "
          f"{n_pri}/{n_active} have a Priority.")
    print("# Unranked sort last (P–); edge-free OQs default workable_now and may "
          "overstate workability until Deps are authored. Frontier is only as good "
          "as the authored edges — this footer is the honesty check.")


def cmd_activations(entries):
    """SessionStart-hook output: emit JSON whose `additionalContext` injects the
    activation menu (with LIVE state) into Claude's context, so Claude opens the
    session by showing the user their options. Always emits valid JSON (a broken
    hook must not break session start)."""
    live = "[NEXT] — what to work on next (run `python3 python/omega_resolver.py menu`)"
    try:
        buckets, _sccs, _nt = frontier(entries)
        wk = sorted(buckets["workable_now"], key=_by_pri(entries))
        n_wk, n_h = len(wk), len(buckets["blocked_on_human"])
        top = ""
        if wk:
            e = entries[wk[0]]
            top = f"; top: {wk[0]} {e.title[:48]}"
        live = (f"[NEXT] — what to work on next: {n_wk} workable now, {n_h} blocked on "
                f"you{top}  (runs `python3 python/omega_resolver.py menu`)")
    except Exception:
        pass
    # Monthly-consolidation reminder: deterministic date-check against CLAUDE.md's
    # declared due date (no reliance on the model noticing the date itself).
    consolidation = ""
    try:
        import datetime as _dt
        cm = (ROOT / "CLAUDE.md").read_text()
        m = re.search(r"on or after \*\*(\d{4}-\d{2}-\d{2})\*\*", cm)
        if m and _dt.date.today().isoformat() >= m.group(1):
            consolidation = (f"\n⚠ MONTHLY CONSOLIDATION DUE (since {m.group(1)}): before other "
                             "work, prompt the operator to run the memory / KNOWN_STATE roll-off / "
                             "ISSUES compress pass (CLAUDE.md → Memory Consolidation Review).\n")
    except Exception:
        pass
    ctx = (
        "Open EVERY session by showing the user the ACTIVATION MENU below FIRST, before any "
        "other output — this comes first even when the user's opening message is already a "
        "concrete task or plan. If that opening message IS a task, show the menu first, then "
        "proceed with the task. If it is empty, a greeting, or just an activation token, show "
        "the menu and wait for their pick (do not start work unprompted).\n\n"
        "Activations — the user types one of these (exact, case-sensitive, with brackets):\n"
        f"  {live}\n"
        "  [GATE] — run all project gate checks (./scripts/gate.sh), report green/red.\n"
        "  [PUSH] — pre-push ritual: [GATE] green + docs current, then push (see CLAUDE.md).\n"
        f"{consolidation}\n"
        "When the user sends [NEXT], run the menu command and present WORKABLE NOW for "
        "them to pick. The flow is sequential: after they finish an item, [NEXT] again "
        "gives the next. Full activation docs are in CLAUDE.md."
    )
    print(json.dumps({"hookSpecificOutput": {
        "hookEventName": "SessionStart", "additionalContext": ctx}}, ensure_ascii=False))


# --------------------------------------------------------------------------- #
# index — derived router over ISSUES.md (index-only path; OQ-141)
#
# A compact scan surface (issues/INDEX.md) + machine artifact (issues/INDEX.json)
# generated FROM ISSUES.md, regenerated after every edit, never authoritative
# itself. INVARIANT: the index is a derived routing aid; it never resolves policy
# disputes. It mirrors current resolver/parser behavior; it does not adjudicate.
#
# Status authority stays with issues_status.py (its token is canonical per row);
# the partition mirrors omega_resolver.ACTIVE by IMPORT (this module's own
# constant) so it cannot drift a third time. A dropped/malformed row can never
# silently shrink the index — the join asserts the two parsers' ID sets are
# identical and aborts loudly + classified on any mismatch (Build Discipline
# Pattern 4/5).
#
# Per-file split threshold (when to revisit): this index-only path holds while one
# agent can `grep OQ-NN ISSUES.md` cheaply. Revisit the `issues/OQ-NN.md` per-entry
# split at SCALE-TIME — when a single grep body read is too large to scan, or when
# parallel-worktree write contention on the one ISSUES.md recurs. Deferred per this
# module's docstring; recorded in OQ-141.
# --------------------------------------------------------------------------- #
def _index_abort(kind, detail):
    """Loud, named, classified abort — a repairable message, not a bare exit, so
    the failure is fixable without weakening strictness (Pattern 4/5)."""
    print(f"index: ABORT {kind}: {detail}", file=sys.stderr)
    sys.exit(2)


def _oqnum(oq):
    return int(oq.split("-")[1])


def _body_for_edges(body):
    """parse_entries has NO footer-awareness: every line after the LAST `## OQ-NN`
    header is appended to that trailing entry's body, so the trailing entry absorbs
    the document footer (the `*Last updated:* …` block, which itself cites OQ refs —
    e.g. OQ-51). Trim that footer here so the last entry's DERIVED edges don't
    inherit footer OQ refs. Scoped to the index's edge view only; ISSUES.md and the
    shared parser stay untouched. No-op on every non-trailing entry."""
    out = []
    for line in body:
        if line.startswith("*Last updated:"):
            while out and out[-1].strip() in ("", "---"):
                out.pop()
            break
        out.append(line)
    return out


def _index_row(oq, entry, status):
    """One canonical row dict (the JSON shape; markdown is rendered FROM this)."""
    body = _body_for_edges(entry.body)
    deps = [f"{rel} {tgt}" for rel, tgt in entry.deps]
    # cross_refs: OQ-NN targets parsed from **Cross-refs:** lines (freeform; some
    # carry no OQ target -> empty, which is fine), self-excluded, deduped.
    cross = []
    for line in body:
        if line.startswith("**Cross-refs:**"):
            cross.extend(OQREF.findall(line))
    cross_refs = sorted(set(cross) - {oq}, key=_oqnum)
    # mentioned_in: ALL inline OQ-NN in the body, deduped, self-excluded (keep
    # simple — the derived secondary "mentioned-in" edge per the plan).
    mentioned = sorted(set(OQREF.findall("\n".join(body))) - {oq}, key=_oqnum)
    return {
        "id": oq,
        "status": status,
        "priority": entry.priority if entry.priority is not None else "–",
        "summary": entry.title,            # title verbatim (the heading after "OQ-NN —")
        "deps": deps,                      # typed declared edges (Entry.deps)
        "cross_refs": cross_refs,          # declared primary, from **Cross-refs:**
        "mentioned_in": mentioned,         # derived secondary, inline body mentions
    }


def _index_rows(entries):
    """JOIN omega_resolver's rich parse with issues_status's canonical status,
    assert ID-set/count integrity, and build the canonical row list. Shared by the
    write and --check paths so they cannot diverge.
    Returns (rows_active, rows_archive, per_status_counts)."""
    status_entries, status_problems = issues_status.scan()
    # (1) malformed status -> abort named (first 1-3 OQ IDs)
    if status_problems:
        ids = []
        for p in status_problems:
            tok = p.split(":", 1)[0].split()[0]
            if tok.startswith("OQ-") and tok not in ids:
                ids.append(tok)
        _index_abort("MALFORMED",
                     f"{ids[:3] or status_problems[:1]} — fix ISSUES.md until "
                     f"`issues_status.py --check` passes")
    status_by_oq = dict(status_entries)
    # (2) ID-set mismatch -> abort (never silently drop a row)
    issues_ids, status_ids = set(entries), set(status_by_oq)
    if issues_ids != status_ids:
        _index_abort("ID-MISMATCH",
                     f"in-issues-only={sorted(issues_ids - status_ids)}, "
                     f"in-status-only={sorted(status_ids - issues_ids)}")
    # build rows from the agreed-upon ID set
    rows_active, rows_archive = [], []
    per_status = {}
    for oq in sorted(entries, key=_oqnum):
        status = status_by_oq[oq]
        per_status[status] = per_status.get(status, 0) + 1
        row = _index_row(oq, entries[oq], status)
        (rows_active if status in ACTIVE else rows_archive).append(row)
    # (3) count integrity: headings vs status rows vs built rows must agree
    n_headings, n_status = len(entries), len(status_entries)
    n_rows = len(rows_active) + len(rows_archive)
    if not (n_headings == n_status == n_rows):
        _index_abort("COUNT-MISMATCH",
                     f"headings={n_headings}, status={n_status}, rows={n_rows}")
    return rows_active, rows_archive, per_status


def _index_doc(entries):
    """Full JSON document (the single source the markdown is rendered from)."""
    rows_active, rows_archive, per_status = _index_rows(entries)
    return {
        "manifest": manifest(),
        "issues_sha": hashlib.sha256(ISSUES.read_bytes()).hexdigest(),
        "n_entries": len(rows_active) + len(rows_archive),
        "active_token_set": sorted(ACTIVE),
        "per_status": per_status,
        "active": rows_active,
        "archive": rows_archive,
    }


def _md_cell(s):
    return str(s).replace("|", "\\|").replace("\n", " ")


def _index_md(doc):
    """Render the compact scan surface FROM the same row list. Intentionally
    minimal: banner + two partition definitions + table. No prose adjudication of
    the `mitigated` membership question — only a one-line linked OQ reference."""
    sha = doc["issues_sha"][:12]
    active_set = ", ".join(doc["active_token_set"])
    n_a, n_b = len(doc["active"]), len(doc["archive"])
    L = []
    L.append("# OQ Router Index — DERIVED from ISSUES.md (do not edit by hand)")
    L.append("")
    L.append(f"Generated by `python3 python/omega_resolver.py index` (issues_sha "
             f"`{sha}`). Regenerate after EVERY edit to ISSUES.md — the `omega index` "
             f"check in `scripts/gate.sh` turns [GATE] red if this is stale. This "
             f"index ROUTES (scan it, then read with `grep OQ-NN ISSUES.md`); it is a "
             f"derived routing aid and never resolves policy disputes.")
    L.append("")
    L.append("**Partition** (descriptive, not normative):")
    L.append(f"- **Active Frontier** — *current resolver-defined frontier*: status ∈ "
             f"`omega_resolver.ACTIVE` (`{{{active_set}}}`), imported, not re-encoded.")
    L.append(f"- **Archive** — every other status (resolved, disposed, future, "
             f"mitigated, …).")
    L.append(f"- Whether `mitigated` belongs in the active frontier is open: see OQ-141.")
    L.append("")
    L.append("Per-status counts: " +
             ", ".join(f"`{t}`={n}" for t, n in sorted(doc["per_status"].items())) +
             f" (total {doc['n_entries']}).")

    def table(rows):
        out = ["| OQ | P | status | summary | deps | cross-refs | mentioned-in |",
               "|----|---|--------|---------|------|------------|--------------|"]
        for r in rows:
            out.append("| {id} | {p} | {st} | {summ} | {deps} | {cr} | {mi} |".format(
                id=r["id"], p=_md_cell(r["priority"]), st=r["status"],
                summ=_md_cell(r["summary"]),
                deps=_md_cell("; ".join(r["deps"])),
                cr=_md_cell(", ".join(r["cross_refs"])),
                mi=_md_cell(", ".join(r["mentioned_in"]))))
        return out

    L.append("")
    L.append(f"## Active Frontier (current resolver-defined frontier) — {n_a}")
    L.append("")
    L.extend(table(doc["active"]))
    L.append("")
    L.append(f"## Archive — {n_b}")
    L.append("")
    L.extend(table(doc["archive"]))
    L.append("")
    return "\n".join(L)


def cmd_index(entries, check=False):
    doc = _index_doc(entries)
    md = _index_md(doc)
    json_path = ISSUES_DIR / "INDEX.json"
    md_path = ISSUES_DIR / "INDEX.md"
    if check:
        # Recompute and diff row content + issues_sha + schema_version against the
        # on-disk files; generated_at (and the git-HEAD store_version) are EXCLUDED
        # — they change without ISSUES.md changing and would false-fire.
        drift = []
        if not json_path.exists() or not md_path.exists():
            drift.append("issues/INDEX.{json,md} missing — run `omega_resolver.py index`")
        else:
            on_disk = json.loads(json_path.read_text())
            for key in ("issues_sha", "n_entries", "per_status", "active", "archive"):
                if on_disk.get(key) != doc[key]:
                    drift.append(f"INDEX.json {key} differs from current ISSUES.md")
            if on_disk.get("manifest", {}).get("schema_version") != SCHEMA_VERSION:
                drift.append("INDEX.json schema_version differs")
            if md_path.read_text() != md:
                drift.append("INDEX.md differs from re-render of current ISSUES.md")
        if drift:
            for d in drift:
                print(f"index --check: STALE: {d}", file=sys.stderr)
            print(f"index --check: {len(drift)} drift(s) — regenerate with "
                  f"`python3 python/omega_resolver.py index`")
            sys.exit(1)
        print(f"index --check: fresh ({doc['n_entries']} rows, "
              f"{len(doc['active'])} active / {len(doc['archive'])} archive)")
        return
    ISSUES_DIR.mkdir(exist_ok=True)
    json_path.write_text(json.dumps(doc, indent=2, ensure_ascii=False) + "\n")
    md_path.write_text(md)
    print(f"index: wrote {json_path.relative_to(ROOT)} + {md_path.relative_to(ROOT)} "
          f"({doc['n_entries']} rows: {len(doc['active'])} active / "
          f"{len(doc['archive'])} archive)")


def main():
    args = sys.argv[1:]
    cmd = args[0] if args else "frontier"
    if cmd == "selftest":
        fails, parse_probs = selftest()
        for p in parse_probs:
            print(f"PARSE: {p}")
        for f in fails:
            print(f"FAIL: {f}")
        if fails:
            print(f"selftest: {len(fails)} FAILED")
            sys.exit(1)
        print("selftest: all positive controls fired (10/10)")
        return
    entries, problems = parse_entries()
    for p in problems:
        print(f"PARSE-PROBLEM: {p}", file=sys.stderr)
    if cmd == "dump":
        cmd_dump(entries)
    elif cmd == "check":
        probs = check(entries, problems)
        for p in probs:
            print(f"PROBLEM: {p}")
        print(f"{len(probs)} problems")
        sys.exit(1 if probs else 0)
    elif cmd == "frontier":
        cmd_frontier(entries)
    elif cmd == "menu":
        cmd_menu(entries)
    elif cmd == "index":
        cmd_index(entries, check=("--check" in args))
    elif cmd == "activations":
        cmd_activations(entries)
    else:
        print(__doc__)
        sys.exit(2)


if __name__ == "__main__":
    main()
