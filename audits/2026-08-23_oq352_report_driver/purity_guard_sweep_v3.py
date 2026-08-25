#!/usr/bin/env python3
"""OQ-356 — REPAIRED class sweep (v3). Supersedes purity_guard_sweep.py (v2).

WHAT WAS WRONG WITH v2, and why it matters more than a regex typo
================================================================
v2's find-criterion was IDIOM-KEYED, not REACHABILITY-KEYED. Two independent
failures, of different severity:

  (i)  FALSE MATCHES. Its ARITH regex carried a bare `>` alternative, so it
       matched the `->` of every if-then-else. "does arithmetic within 5 lines"
       actually meant "has an if-then-else nearby". It also never tracked the
       bound variable, so arithmetic on ANY value counted.

  (ii) A STRUCTURAL BLIND SPOT — the serious one. v2 only ever inspected lines
       that CONTAIN a purity call, so any hazard one predicate downstream is
       invisible at ANY window width. Widening the window (the improvement the
       OQ itself proposed) would never have found it. The held-out case is
       giant_component_analysis.pl:596, `in_float_range(Lo,Hi,V) :- V >= Lo,
       V < Hi.` — a second unguarded arithmetic on the same value, reached from
       count_by_action_band via count_in_zone/4 through include/3, and found BY
       HAND, not by the instrument.

THE REPAIRED CRITERION (verbatim from OQ-356, because this is the part that
generalizes): *model whether an `unknown` REACHES an arithmetic comparison on
this path* — bound-variable tracking through to the operation, plus upstream
reachability gates — rather than the `catch(...)` idiom or absence counts.

Concretely v3:
  (a) never matches `->` / `-->` as arithmetic;
  (b) binds the OUTPUT variable at the purity call and flags arithmetic only on
      THAT variable (or on a value derived from it);
  (c) has no window: taint is tracked to the end of the clause;
  (d) treats a dominating `number(V)` / `integer(V)` / `float(V)` guard, or a
      catch-recovery that rebinds V to a number AND is then number-checked, as
      making the value unreachable as a non-number;
  (e) FOLLOWS THE VALUE ACROSS PREDICATE BOUNDARIES — into user-defined callees
      by argument position, and from a list of tainted values into the
      element-level arithmetic reached through include/exclude/maplist/member
      and the list arithmetic builtins.

POSITIVE CONTROL — AND WHY IT SCANS A FROZEN FIXTURE
====================================================
v2's control asserted "fires on giant_component_analysis.pl:1278, declines on
both guarded siblings". OQ-356's Commit 1 FIXES :1278. Scanned against the live
tree afterwards, the "fires" half CANNOT FIRE, the control passes VACUOUSLY, and
it still prints `=> sweep DISCRIMINATES` — the same defect class this OQ is
about, reproduced inside the instrument built to detect it. So the "fires" half
is pointed at a frozen PRE-FIX snapshot of the predicate,
audits/2026-08-24_oq356_purity_guard/fixtures/count_by_action_band_prefix.pl,
captured before the edit. The "declines" half stays on the live tree: those
sites are unchanged by this work, and a decline that can be checked against the
real thing should be.

This is criterion 4's rule ("availability is not automatic; when a detector will
follow, preserve the defective state deliberately") applied to the INSTRUMENT
rather than to the corpus.

THE HELD-OUT ACCEPTANCE TEST
============================
`:596` is deliberately NOT in the expected-findings list. The repaired sweep
must surface it ON ITS OWN. If it does not, the criterion is still wrong, the
re-run's result is NOT evidence, and the sweep-candidate adjudication cannot be
trusted. Run with --acceptance to check exactly that.
"""
import re, sys
from pathlib import Path

PROLOG = Path("prolog")
FIXTURE = Path("audits/2026-08-24_oq356_purity_guard/fixtures/count_by_action_band_prefix.pl")

# The purity producers whose output may be a non-number (OQ-60 path 0a /
# the -1.0 sentinel). Value is the 1-based OUTPUT argument position per arity.
PRODUCERS = {
    ("purity_score", 2): 2,
    ("effective_purity", 4): 3,
    ("effective_purity", 3): 2,
}

# Arithmetic / ordering that THROWS or MISORDERS on an atom. `->` and `-->` are
# NOT here: that was v2's false-match engine.
CMP_OPS = [">=", "=<", "=:=", "=\\=", "<", ">"]
LIST_ARITH = {"sum_list", "sumlist", "max_list", "min_list", "msort", "sort",
              "predsort", "last", "nth0", "nth1"}
# list-consuming meta-calls that hand ELEMENTS to a closure
# name -> (closure argument, 1-BASED; list argument, 0-BASED).
# The mixed bases are a footgun and cost a debugging round: (1, 2) read the
# RESULT list instead of the input list for include/3, so the element-level
# hop never fired and `:596` stayed invisible while everything else looked
# right. Spelled out here rather than "fixed" silently.
ELEMENT_META = {"include": (1, 1), "exclude": (1, 1),
                "partition": (1, 1), "maplist": (1, 1)}

VAR = r"[A-Z_][A-Za-z0-9_]*"


ARROW = re.compile(r"-->|->|:-|\?-|=\.\.")


def mask_arrows(code):
    r"""Blank out `->`, `-->`, `:-` etc. BEFORE scanning for arithmetic.

    This is v2's defect, and it is easy to reintroduce in a new form: v2 matched
    the `>` of `->` with a bare alternative in one big regex; a first draft of
    v3 matched `>\s*Score` against `->  Score = unknown` and reported
    purity_scoring.pl:61 as a comparison. Same bug, different surface. Masking
    (rather than negative lookaround at each op) keeps line offsets intact and
    removes the whole class in one place."""
    return ARROW.sub(lambda m: "#" * len(m.group(0)), code)


def strip_comment(line):
    """Drop a % comment, but not a % inside quotes (format strings use ~w, and
    quoted atoms can contain %)."""
    out, i, q = [], 0, None
    while i < len(line):
        c = line[i]
        if q:
            if c == "\\":
                out.append(c)
                i += 1
                if i < len(line):
                    out.append(line[i]); i += 1
                continue
            if c == q:
                q = None
            out.append(c); i += 1; continue
        if c in "'\"":
            q = c; out.append(c); i += 1; continue
        if c == "%":
            break
        out.append(c); i += 1
    return "".join(out)


def clauses(path):
    """Yield (start_line, [(lineno, code)]) per clause. A clause starts at a
    non-blank line in column 0 and runs to a line whose code ends with '.'."""
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    cur, start = [], None
    for i, raw in enumerate(lines, 1):
        code = strip_comment(raw)
        if not code.strip():
            if cur and start is not None:
                continue
            continue
        if start is None:
            if raw[:1].strip() == "" :   # continuation with no open clause
                continue
            start = i
        cur.append((i, code))
        if code.rstrip().endswith("."):
            yield start, cur
            cur, start = [], None
    if cur:
        yield start, cur


def head_of(clause):
    txt = " ".join(c for _, c in clause).strip()
    m = re.match(r"([a-z][A-Za-z0-9_]*)\s*\(", txt)
    if m:
        name = m.group(1)
        args = split_args(txt[m.end():])
        return name, len(args), args
    m = re.match(r"([a-z][A-Za-z0-9_]*)\s*(:-|\.)", txt)
    if m:
        return m.group(1), 0, []
    return None, 0, []


def split_args(s):
    """Split the argument list starting just after an opening paren."""
    args, depth, cur, q = [], 0, [], None
    for ch in s:
        if q:
            cur.append(ch)
            if ch == q:
                q = None
            continue
        if ch in "'\"":
            q = ch; cur.append(ch); continue
        if ch in "([{":
            depth += 1; cur.append(ch); continue
        if ch in ")]}":
            if depth == 0:
                args.append("".join(cur).strip()); return args
            depth -= 1; cur.append(ch); continue
        if ch == "," and depth == 0:
            args.append("".join(cur).strip()); cur = []; continue
        cur.append(ch)
    args.append("".join(cur).strip())
    return args


def find_calls(text, name, arity):
    """All (index_after_paren, args) for name/arity in text."""
    out = []
    for m in re.finditer(r"(?<![A-Za-z0-9_])" + re.escape(name) + r"\s*\(", text):
        args = split_args(text[m.end():])
        if len(args) == arity:
            out.append((m.start(), args))
    return out


def build_index(paths):
    """name/arity -> list of (path, start_line, clause)."""
    idx = {}
    for p in paths:
        for start, cl in clauses(p):
            n, a, _ = head_of(cl)
            if n:
                idx.setdefault((n, a), []).append((p, start, cl))
    return idx


class Finding:
    def __init__(self, path, line, kind, var, snippet, chain):
        self.path, self.line, self.kind = str(path), line, kind
        self.var, self.snippet, self.chain = var, snippet, chain

    def key(self):
        return (self.path, self.line)


GOAL = re.compile(r"(?<![A-Za-z0-9_])([a-z][A-Za-z0-9_]*)\s*\(")
# NOTE: the lookbehind deliberately does NOT exclude `:` — a first draft did,
# and silently skipped EVERY module-qualified call
# (`drl_purity_network:effective_purity(...)`), i.e. every producer in the
# codebase. The sweep then reported 2 findings and its own positive control
# caught it.


def clause_goals(clause):
    """Every (lineno, name, arity, args, snippet) call in the clause body.

    THE CLAUSE IS JOINED FIRST, and this is load-bearing rather than tidy. A
    per-line version of this function silently mis-parses every goal that spans
    lines — `findall(EP,` on its own line parses as findall/1, so the
    findall-template -> collection propagation never fires, and with it the
    whole interprocedural chain into count_in_zone/4 and in_float_range/3 dies.
    The sweep then reports a clean 4 findings and misses the one site the
    acceptance test exists to catch. Extracted ONCE per clause and cached; a
    first draft instead iterated the entire predicate index per clause and did
    not terminate in two minutes."""
    joined, offs, pos = [], [], 0
    for ln, code in clause:
        joined.append(code)
        offs.append((pos, ln))
        pos += len(code) + 1
    text = "\n".join(joined)

    def line_at(off):
        cur = clause[0][0]
        for start, ln in offs:
            if start <= off:
                cur = ln
            else:
                break
        return cur

    out = []
    for m in GOAL.finditer(text):
        args = split_args(text[m.end():])
        ln = line_at(m.start())
        snippet = " ".join(text[m.start():m.start() + 90].split())
        out.append((ln, m.group(1), len(args), args, snippet))
    return out


_GOALCACHE = {}


def goals_of(path, start, clause):
    k = (str(path), start)
    if k not in _GOALCACHE:
        _GOALCACHE[k] = clause_goals(clause)
    return _GOALCACHE[k]


def guarded_before(clause, var, upto_line):
    for ln, code in clause:
        if ln > upto_line:
            break
        for g in ("number", "integer", "float"):
            for m in re.finditer(r"(?<![A-Za-z0-9_])" + g + r"\s*\(", code):
                a = split_args(code[m.end():])
                if len(a) == 1 and a[0].strip() == var:
                    return True
    return False


def guarded_anywhere(clause, var):
    """Is there a number/integer/float check on var anywhere in this clause?

    Used for the findall-template -> collection propagation only. The guard
    that matters there sits INSIDE the findall goal, i.e. on a line AFTER the
    findall call's own start line, so guarded_before/3 structurally cannot see
    it. Scalar arithmetic still uses guarded_before/3, because for a scalar the
    ORDER is exactly what is load-bearing (OQ-60: `>=` throws on the atom before
    a later number/1 could run)."""
    for ln, code in clause:
        for g in ("number", "integer", "float"):
            for m in re.finditer(r"(?<![A-Za-z0-9_])" + g + r"\s*\(", code):
                a = split_args(code[m.end():])
                if len(a) == 1 and a[0].strip() == var:
                    return True
    return False


NEG_GUARD = re.compile(r"\\\+\s*(number|integer|float)\s*\(")


def clause_guarded(path, start, idx, var):
    r"""Is `var` this clause's own parameter, guarded by an EARLIER clause of the
    same predicate that cuts on `\+ number(...)`?

    The fail-closed idiom

        ep_base_severity(EP, T) :- \+ number(EP), !, T = undetermined.
        ep_base_severity(EP, T) :- EP < 0.30, !, T = critical.

    means the comparison in clause 2 is UNREACHABLE with a non-number. A
    criterion that only recognises a positive `number(V)` call in the SAME
    clause reports clause 2 as unguarded — which a first draft of this sweep
    did, twice, on network_dynamics.pl:285 and :287. Modelling reachability
    means modelling the clause order, not just the conjunction."""
    _n, _a, params = head_of(_clause_at(path, start, idx))
    if var not in [p.strip() for p in params]:
        return False
    k = [p.strip() for p in params].index(var)
    for (p2, s2, c2) in idx.get((_n, _a), []):
        if str(p2) != str(path) or s2 >= start:
            continue                        # only EARLIER clauses guard
        _n2, _a2, params2 = head_of(c2)
        if len(params2) != _a:
            continue
        pv = params2[k].strip()
        body = " ".join(code for _, code in c2)
        if NEG_GUARD.search(body) and "!" in body:
            for m in NEG_GUARD.finditer(body):
                if split_args(body[m.end():])[0].strip() == pv:
                    return True
    return False


_CLAUSE_AT = {}


def _clause_at(path, start, idx):
    k = (str(path), start)
    if k not in _CLAUSE_AT:
        for defs in idx.values():
            for (p2, s2, c2) in defs:
                _CLAUSE_AT[(str(p2), s2)] = c2
    return _CLAUSE_AT.get(k, [(start, "")])


def add(findings, seen, f):
    if f.key() in seen:
        return
    seen.add(f.key())
    findings.append(f)


_VISITED = set()


def scan_clause(path, start, clause, idx, tainted, listtainted, chain, depth,
                findings, seen):
    memo = (str(path), start, frozenset(tainted), frozenset(listtainted))
    if memo in _VISITED or depth > 4:
        return
    _VISITED.add(memo)

    goals = goals_of(path, start, clause)

    # WHERE each taint enters. Taint flows FORWARD only: arithmetic on a line
    # BEFORE the producer that binds the variable is not reachable from it.
    # Without this, a variable bound by a producer in ONE branch of an
    # if-then-else is treated as tainted in the OTHER branch too -- which is
    # exactly how a first draft reported drl_fpn.pl:311
    # (`TotalContam is max(0.0, IP - EP)`, in the then-branch, where EP comes
    # from the fpn_ep/3 cache) as a finding, because effective_purity/4 binds
    # the same variable name three lines LATER in the else-branch.
    entry = {v: clause[0][0] for v in tainted}
    entry.update({v: clause[0][0] for v in listtainted})

    # 1. purity producers introduce taint (deeper frames get their taint from
    #    the caller's argument position instead)
    for ln, n, a, args, code in goals:
        if (n, a) in PRODUCERS:
            v = args[PRODUCERS[(n, a)] - 1].strip()
            if re.fullmatch(VAR, v) and v != "_":
                tainted.add(v)
                entry.setdefault(v, ln)
    if not tainted and not listtainted:
        return

    # findall/bagof/setof: a tainted TEMPLATE makes the COLLECTION list-tainted
    for ln, n, a, args, code in goals:
        if n in ("findall", "bagof", "setof") and a == 3:
            tmpl, _g, coll = [x.strip() for x in args]
            if (tmpl in tainted and re.fullmatch(VAR, coll)
                    and not guarded_anywhere(clause, tmpl)):
                listtainted.add(coll)
                entry.setdefault(coll, ln)
        if n == "member" and a == 2 and args[1].strip() in listtainted:
            x = args[0].strip()
            if re.fullmatch(VAR, x):
                tainted.add(x)
                entry.setdefault(x, ln)

    for ln, rawcode in clause:
        code = mask_arrows(rawcode)
        # 2. scalar arithmetic on a tainted var
        for v in sorted(tainted):
            if ln < entry.get(v, clause[0][0]):
                continue                      # arithmetic precedes the producer
            if not re.search(r"(?<![A-Za-z0-9_])" + re.escape(v) + r"(?![A-Za-z0-9_])", code):
                continue
            if guarded_before(clause, v, ln):
                continue
            if clause_guarded(path, start, idx, v):
                continue
            for op in CMP_OPS:
                pat = (r"(?<![A-Za-z0-9_])" + re.escape(v) + r"\s*" + re.escape(op)
                       + r"|" + re.escape(op) + r"\s*" + re.escape(v) + r"(?![A-Za-z0-9_])")
                if re.search(pat, code):
                    add(findings, seen, Finding(path, ln, "comparison", v,
                                                rawcode.strip()[:90], chain))
                    break
            if re.search(r"(?<![A-Za-z0-9_])is\s+.*(?<![A-Za-z0-9_])" + re.escape(v)
                         + r"(?![A-Za-z0-9_])", code):
                add(findings, seen, Finding(path, ln, "is/2", v, rawcode.strip()[:90], chain))

    # 3/4/5/6 -- goal-driven, so no scan of the whole index
    for ln, n, a, args, code in goals:
        # 3. list arithmetic
        if n in LIST_ARITH and args and args[0].strip() in listtainted:
            add(findings, seen, Finding(path, ln, "list-arith(%s)" % n,
                                        args[0].strip(), code.strip()[:90], chain))
        # 4. element-level meta-calls: include(Closure, List, _)
        if n in ELEMENT_META and a >= 3:
            cpos, lpos = ELEMENT_META[n]
            if len(args) > lpos and args[lpos].strip() in listtainted:
                closure = args[cpos - 1].strip()
                m = re.match(r"([a-z][A-Za-z0-9_]*)\s*(\((.*)\))?$", closure)
                if m:
                    cname = m.group(1)
                    given = len(split_args(m.group(3) + ")")) if m.group(3) else 0
                    for (p2, s2, c2) in idx.get((cname, given + 1), []):
                        _n, _a, params = head_of(c2)
                        if len(params) != given + 1:
                            continue
                        ev = params[given].strip()
                        if re.fullmatch(VAR, ev):
                            scan_clause(p2, s2, c2, idx, {ev}, set(),
                                        chain + ["%s/%d applied by %s/3 at %s:%d"
                                                 % (cname, given + 1, n, path, ln)],
                                        depth + 1, findings, seen)
        # 6. interprocedural by argument position
        if (n, a) in PRODUCERS or n in ("number", "integer", "float", "findall",
                                        "bagof", "setof", "catch", "format"):
            continue
        defs = idx.get((n, a))
        if not defs:
            continue
        for k, arg in enumerate(args):
            arg = arg.strip()
            kind = ("scalar" if arg in tainted else
                    "list" if arg in listtainted else None)
            if kind is None or guarded_before(clause, arg, ln):
                continue
            for (p2, s2, c2) in defs:
                _n, _a, params = head_of(c2)
                if len(params) != a:
                    continue
                pv = params[k].strip()
                if not re.fullmatch(VAR, pv):
                    continue
                scan_clause(p2, s2, c2, idx,
                            {pv} if kind == "scalar" else set(),
                            set() if kind == "scalar" else {pv},
                            chain + ["%s/%d arg%d from %s:%d" % (n, a, k + 1, path, ln)],
                            depth + 1, findings, seen)


def sweep(paths):
    idx = build_index(paths)
    findings, seen = [], set()
    for p in paths:
        for start, cl in clauses(p):
            scan_clause(p, start, cl, idx, set(), set(), [], 0, findings, seen)
    return findings


def live_paths():
    out = []
    for f in sorted(PROLOG.rglob("*.pl")):
        s = str(f)
        if "/tests/" in s or "/testsets" in s or "/archives/" in s:
            continue
        out.append(f)
    return out


def main():
    acceptance = "--acceptance" in sys.argv
    paths = live_paths()
    if FIXTURE.exists():
        paths = paths + [FIXTURE]
    else:
        print("WARNING: frozen pre-fix fixture %s is MISSING — the positive control's"
              " 'fires' half cannot fire and would pass VACUOUSLY. Refusing." % FIXTURE)
        return 2

    findings = sweep(paths)
    findings.sort(key=lambda f: (f.path, f.line))

    print("REPAIRED SWEEP v3 — reachability-keyed, interprocedural\n")
    print("sites where a possibly-non-number purity value REACHES arithmetic: %d\n"
          % len(findings))
    for f in findings:
        print("  %s:%d  [%s on %s]" % (f.path, f.line, f.kind, f.var))
        print("      %s" % f.snippet)
        for c in f.chain:
            print("      via %s" % c)

    # ---------------- POSITIVE CONTROL ------------------------------------
    print("\n--- positive control: does this sweep DISCRIMINATE? ---")
    keys = {(f.path, f.line) for f in findings}
    fixture_hit = any(p == str(FIXTURE) for p, _ in keys)
    print("  FIRES on the FROZEN PRE-FIX count_by_action_band fixture      : %s" % fixture_hit)
    print("    (%s)" % FIXTURE)
    print("    scanned frozen, NOT live: Commit 1 fixed the live :1278, so a")
    print("    live-tree scan would pass this half vacuously.")
    guarded_siblings = [("prolog/drl_purity_network.pl", 352),
                        ("prolog/giant_component_analysis.pl", 365)]
    decl = []
    for gp, gl in guarded_siblings:
        hit = any(p == gp and abs(l - gl) <= 6 for p, l in keys)
        decl.append(not hit)
        print("  DECLINES (guarded) %s:%d%s: %s" % (gp, gl, " " * max(0, 28 - len(gp)), not hit))
    live_1278 = any(p == "prolog/giant_component_analysis.pl" and abs(l - 1278) <= 40
                    for p, l in keys)
    print("  DECLINES the LIVE (now-guarded) count_by_action_band          : %s" % (not live_1278))
    ok = fixture_hit and all(decl) and not live_1278
    print("  => sweep %s" % ("DISCRIMINATES" if ok else "IS UNTESTED — do not cite its zeros"))

    # ---------------- HELD-OUT ACCEPTANCE TEST ----------------------------
    if acceptance:
        print("\n--- HELD-OUT ACCEPTANCE TEST (pre-committed in the OQ-356 plan) ---")
        print("  `:596` (in_float_range/3) was found BY HAND, and is deliberately NOT")
        print("  in any expected-findings list here. A criterion with real")
        print("  interprocedural reach must surface it unaided; no window width can.")
        hit596 = any(p == "prolog/giant_component_analysis.pl" and abs(l - 596) <= 2
                     for p, l in keys)
        print("  repaired sweep surfaces giant_component_analysis.pl:596 on its own: %s"
              % hit596)
        if not hit596:
            print("  => CRITERION STILL WRONG. The re-run is NOT evidence, the")
            print("     sweep-candidate adjudication cannot be trusted, and the")
            print("     gate-promotion trigger stays unmet.")
        return 0 if (ok and hit596) else 1
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
