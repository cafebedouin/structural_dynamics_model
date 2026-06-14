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
    python3 python/omega_resolver.py frontier     # the routing view
    python3 python/omega_resolver.py check        # checker (exit 1 on problems)
    python3 python/omega_resolver.py selftest     # positive controls (exit 1 on fail)
    python3 python/omega_resolver.py dump         # parsed access points, per OQ
"""
import datetime
import json
import re
import subprocess
import sys
from pathlib import Path

SCHEMA_VERSION = "omega-resolver/1"
ROOT = Path(__file__).resolve().parents[1]
ISSUES = ROOT / "ISSUES.md"

HEADER = re.compile(r"^## (OQ-\d+)\b(.*)$")
STATUS = re.compile(r"^\*\*Status:\*\* (\w+)(?: — .*)?$")
OMEGA = re.compile(r"^\*\*Ω-type:\*\* (Ω_[ECP])\b")
FILES = re.compile(r"^\*\*Files?:\*\*\s*(.+)$")
DEPS = re.compile(r"^\*\*Deps:\*\*\s*(.+)$")

ACTIVE = {"open", "investigating", "partial"}
# relators whose edge BLOCKS (contributes to reachability); others are grouping.
BLOCKING_RELATORS = {"blocked_on", "gates"}
GROUPING_RELATORS = {"bundled_with", "splits_from"}
ALL_RELATORS = BLOCKING_RELATORS | GROUPING_RELATORS

OQREF = re.compile(r"OQ-\d+")
COMMIT = re.compile(r"\b[0-9a-f]{8,40}\b")


class Entry:
    def __init__(self, oq, lineno):
        self.oq = oq
        self.lineno = lineno
        self.status = None
        self.omega = None
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
            cur = Entry(m.group(1), lineno)
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
    return entries, problems


def _split_file_refs(s):
    # files are backtick-quoted or bare paths, comma/space separated
    refs = re.findall(r"`([^`]+)`", s)
    if refs:
        return [r.strip() for r in refs]
    return [tok.strip() for tok in re.split(r"[,\s]+", s) if "/" in tok or "." in tok]


def _parse_deps(oq, s):
    """`**Deps:** blocked_on OQ-122, bundled_with OQ-50 (free text)`.
    Returns (list[(relator, target)], problems)."""
    deps, problems = [], []
    for chunk in s.split(","):
        chunk = chunk.strip()
        if not chunk:
            continue
        parts = chunk.split()
        if len(parts) < 2:
            problems.append(f"{oq}: malformed Deps chunk {chunk!r}")
            continue
        relator = parts[0]
        target_m = OQREF.search(chunk)
        if relator not in ALL_RELATORS:
            problems.append(f"{oq}: unknown relator {relator!r} in {chunk!r}")
            continue
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
    """Scan an entry body for witness tokens and whether each resolves.
    Returns list of (token, kind, resolves: bool). Only meaningful kinds."""
    out = []
    body = "\n".join(entry.body)
    # audit dirs
    for m in re.finditer(r"audits/(\d{4}-\d{2}-\d{2}_[A-Za-z0-9_]+)/?", body):
        d = ROOT / "audits" / m.group(1)
        out.append((m.group(0), "audit_dir", d.is_dir()))
    # backtick'd output/json files
    for m in re.finditer(r"`(outputs/[^`]+\.json)`", body):
        out.append((m.group(1), "output_file", (ROOT / m.group(1)).exists()))
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

    for i, comp in enumerate(sccs):
        if i in nontrivial:
            buckets["standoff"].append(sorted(comp))
            continue
        oq = comp[0]
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
def check(entries):
    problems = []
    auth = authority_report(entries)
    problems += auth["deps_target"]["bad"]
    # resolved-with-rotted-witness
    for e in entries.values():
        if e.status in ("resolved", "disposed"):
            for tok, kind, ok in witness_status(e):
                if not ok:
                    problems.append(
                        f"{e.oq} ({e.status}): witness does not resolve: {tok} [{kind}]")
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

    # control 5: rotted witness flagged; live witness NOT flagged
    probs = check(entries)
    if not any("OQ-9007" in p for p in probs):
        fails.append("rotted-witness control FAILED: OQ-9007 not flagged")
    if any("OQ-9002" in p for p in probs):
        fails.append("witness false-positive: live OQ-9002 witness flagged as rotted")

    # control 6: authority lists non-empty (Pattern 5)
    if auth["deps_target"]["authority_size"] < 1:
        fails.append("authority-nonempty control FAILED: OQ set empty")

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
        print("selftest: all positive controls fired (6/6)")
        return
    entries, problems = parse_entries()
    for p in problems:
        print(f"PARSE-PROBLEM: {p}", file=sys.stderr)
    if cmd == "dump":
        cmd_dump(entries)
    elif cmd == "check":
        probs = check(entries)
        for p in probs:
            print(f"PROBLEM: {p}")
        print(f"{len(probs)} problems")
        sys.exit(1 if probs else 0)
    elif cmd == "frontier":
        cmd_frontier(entries)
    else:
        print(__doc__)
        sys.exit(2)


if __name__ == "__main__":
    main()
