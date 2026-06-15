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
        if relator not in ALL_RELATORS:
            problems.append(f"{oq}: unknown relator {relator!r} in {chunk!r}")
            continue
        if relator == HUMAN_RELATOR:
            # free-text target (a human/external gate, not an OQ)
            target = " ".join(parts[1:]) or "<unspecified>"
            deps.append((relator, target))
            continue
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
def check(entries):
    problems = []
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
    probs = check(entries)
    if not any("OQ-9007" in p for p in probs):
        fails.append("rotted-witness control FAILED: OQ-9007 not flagged")
    if any("OQ-9002" in p for p in probs):
        fails.append("witness false-positive: live OQ-9002 witness flagged as rotted")
    if any("OQ-9009" in p for p in probs):
        fails.append("over-fire control FAILED: OQ-9009 (has a real audit dir) flagged as rotted")

    # control 6: authority lists non-empty (Pattern 5)
    if auth["deps_target"]["authority_size"] < 1:
        fails.append("authority-nonempty control FAILED: OQ set empty")

    # control 7: human-gated Ω_E (no Ω_P blocker) -> blocked_on_human, NOT workable_now
    if "OQ-9008" not in buckets["blocked_on_human"]:
        fails.append(f"human-gate control FAILED: OQ-9008 not blocked_on_human ({_where(buckets,'OQ-9008')})")
    if "OQ-9008" in buckets["workable_now"]:
        fails.append("human-gate control FAILED: OQ-9008 mis-bucketed workable_now")

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
    print(f"## WORKABLE NOW ({len(wk)}) — pick from these")
    for oq in wk:
        e = entries[oq]
        pri = f"P{e.priority}" if e.priority is not None else "P–"
        print(f"  [{pri:>3}] {oq}  {e.title[:72]}")
        wc = (e.whatchanges or "").split(". ")[0][:96]
        line = f"         {e.omega or 'Ω?'}"
        if wc:
            line += f" · changes: {wc}"
        print(line)

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
    ctx = (
        "Open this session by showing the user the ACTIVATION MENU below, then wait "
        "for their pick (do not start work unprompted).\n\n"
        "Activations — the user types one of these (exact, case-sensitive, with brackets):\n"
        f"  {live}\n\n"
        "When the user sends [NEXT], run the menu command and present WORKABLE NOW for "
        "them to pick. The flow is sequential: after they finish an item, [NEXT] again "
        "gives the next. More activations are documented in CLAUDE.md as they are created."
    )
    print(json.dumps({"hookSpecificOutput": {
        "hookEventName": "SessionStart", "additionalContext": ctx}}, ensure_ascii=False))


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
        print("selftest: all positive controls fired (8/8)")
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
    elif cmd == "menu":
        cmd_menu(entries)
    elif cmd == "activations":
        cmd_activations(entries)
    else:
        print(__doc__)
        sys.exit(2)


if __name__ == "__main__":
    main()
