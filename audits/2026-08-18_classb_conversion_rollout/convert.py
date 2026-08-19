#!/usr/bin/env python3
"""convert.py — apply the class-B conversion template to a named predicate, in place.

TEMPLATE (2026-08-17 pilot, fixed in PREREGISTRATION §4): fresh-variable heads +
unify-after-cut.

    p(X, foo) :- guard(X), !.      ->   p(X, T) :- guard(X), !, T = foo.
    p(_, baz).                     ->   p(_, T) :- T = baz.

WHY A TRANSFORMER AND NOT 55 HAND EDITS. Not speed — uniformity. A hand edit can silently
drop a guard or mistype an atom in a way no structural check catches; a transformer makes the
SAME mistake everywhere, where the checks below will see it. The checks are what license this,
not the code:

  1. `scan_file_clauses/2` before and after: same clause count, same guard text, and every
     output argument a fresh VARIABLE afterwards. Asserted per predicate, in-process.
  2. the clause-order census must read steal_risk 0 for the predicate afterwards.
  3. the file must still load under swipl.
  4. (batch level) six-leg corpus diff + gate + Prolog suites.

NOT A REGEX OVER LINES. It finds the clause's head line, then reads FORWARD balancing
parens/brackets/braces and respecting quoted atoms, strings, `0'c` char codes, line comments
and block comments, to the terminating `.`. The head is everything up to the first top-level
`:-`. That is the same class of work `caller_sweep.py` did with one regex per line, and the
reason this arc exists — so the parser's own assumptions are asserted, not assumed:
a clause it cannot parse is REFUSED, never silently skipped.

Usage:
    convert.py --file signature_detection.pl --pred signature_grade/2 [--dry-run]
    convert.py --all                      # every latent-B row in the registry
"""
from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
PROLOG = REPO / "prolog"
sys.path.insert(0, str(REPO / "python"))
from dispatch_head_check import DECLARED  # noqa: E402

ATOM_RE = re.compile(r"^[a-z][A-Za-z0-9_]*$")


class Refused(Exception):
    pass


def _is_char_code_quote(text: str, i: int) -> bool:
    """True if text[i] is the quote of a `0'c` character-code literal, not a quoted atom.

    The first version of this parser tested `text[i-2:i] == "0'"` — off by one, so `0'.`
    read as the start of a quoted atom and swallowed the rest of the file. The selftest
    fixture for exactly that shape refused the whole run, which is the behaviour wanted
    (refuse, never silently skip), and is why the fixture is there.
    """
    if text[i] != "'" or i < 1 or text[i - 1] != "0":
        return False
    return i < 2 or not (text[i - 2].isalnum() or text[i - 2] == "_")


def _skip_char_code(text: str, i: int) -> int:
    """Return the offset just past a `0'c` literal whose quote is at i."""
    j = i + 1
    if j < len(text) and text[j] == "\\":
        j += 1
    return min(j + 1, len(text))


def scan_clauses(path: Path) -> list[tuple[str, list, bool]]:
    """(PI, arg-strings, has_cut) per clause, in order — via the Prolog reader, not the text."""
    goal = (f"scan_file_clauses('{path}', Cs), "
            "forall(member(PI-clause(Args,Cut), Cs), "
            "  ( PI = N/A, format('C ~w/~w ~w ', [N,A,Cut]), "
            "    last(Args, L), "
            "    ( var(L) -> write('$VAR') "
            "    ; atom(L) -> write('$ATOM ') , writeq(L) "
            "    ; write('$OTHER ') , writeq(L) ), nl ))")
    proc = subprocess.run(
        ["swipl", "-q", "-l", str(PROLOG / "dispatch_head_check.pl"), "-g", f"{goal}, halt",
         "-t", "halt(1)"], cwd=REPO, capture_output=True, text=True, timeout=300)
    if proc.returncode != 0:
        raise Refused(f"clause scan failed for {path}: {proc.stderr[-300:]}")
    out = []
    for ln in proc.stdout.splitlines():
        parts = ln.split(None, 3)
        if len(parts) == 4 and parts[0] == "C":
            out.append((parts[1], parts[2] == "true", parts[3]))
    return out


def split_clauses(text: str) -> list[tuple[int, int]]:
    """(start, end) character offsets of each top-level term, terminating '.' included.

    COMMENTS ARE SKIPPED BEFORE a clause start is recorded. The first version set `start` at
    the first non-space character and only then handled `%`, so a comment sitting above a
    clause was absorbed into that clause's span and the clause was then skipped as
    unrecognised. On signature_detection.pl that produced the RIGHT answer by accident (the
    two convertible clauses happened to have no comment above them), which is exactly the kind
    of luck the fixtures exist to remove.

    Refuses rather than guesses: unterminated quote or block comment raises.
    """
    spans, i, n, start = [], 0, len(text), None
    depth = 0
    while i < n:
        c = text[i]
        # --- comments and whitespace: never part of a clause span ---
        if c == "%":
            j = text.find("\n", i)
            i = n if j < 0 else j + 1
            continue
        if text.startswith("/*", i):
            j = text.find("*/", i + 2)
            if j < 0:
                raise Refused("unterminated block comment")
            i = j + 2
            continue
        if start is None and c.isspace():
            i += 1
            continue
        if start is None:
            start = i
        # --- quoted atoms / strings / char codes ---
        if c in "'\"`":
            if _is_char_code_quote(text, i):
                i = _skip_char_code(text, i)
                continue
            q, j = c, i + 1
            while j < n:
                if text[j] == "\\":
                    j += 2
                    continue
                if text[j] == q:
                    if j + 1 < n and text[j + 1] == q:
                        j += 2
                        continue
                    break
                j += 1
            if j >= n:
                raise Refused(f"unterminated quote at offset {i}")
            i = j + 1
            continue
        if c in "([{":
            depth += 1
        elif c in ")]}":
            depth -= 1
        elif c == "." and depth == 0:
            nxt = text[i + 1] if i + 1 < n else "\n"
            prv = text[i - 1] if i else " "
            if nxt in " \t\r\n" and not prv.isdigit():
                spans.append((start, i + 1))
                start = None
                i += 1
                continue
        i += 1
    return spans


def head_body(clause: str) -> tuple[str, str | None]:
    """Split a clause into head text and body text at the top-level `:-`."""
    depth, i, n = 0, 0, len(clause)
    while i < n:
        c = clause[i]
        if c == "%":
            j = clause.find("\n", i)
            i = n if j < 0 else j + 1
            continue
        if clause.startswith("/*", i):
            j = clause.find("*/", i + 2)
            i = n if j < 0 else j + 2
            continue
        if c in "'\"`":
            if _is_char_code_quote(clause, i):
                i = _skip_char_code(clause, i)
                continue
            q, j = c, i + 1
            while j < n and clause[j] != q:
                j += 2 if clause[j] == "\\" else 1
            i = j + 1
            continue
        if c in "([{":
            depth += 1
        elif c in ")]}":
            depth -= 1
        elif depth == 0 and clause.startswith(":-", i):
            return clause[:i], clause[i:]
        i += 1
    return clause, None


def split_args(argtext: str) -> list[str]:
    """Top-level comma split of an argument list."""
    args, depth, cur, i, n = [], 0, [], 0, len(argtext)
    while i < n:
        c = argtext[i]
        if c in "'\"`" and _is_char_code_quote(argtext, i):
            cur.append(argtext[i:_skip_char_code(argtext, i)])
            i = _skip_char_code(argtext, i)
            continue
        if c in "'\"`":
            q, j = c, i + 1
            while j < n:
                if argtext[j] == "\\":
                    j += 2
                    continue
                if argtext[j] == q:
                    if j + 1 < n and argtext[j + 1] == q:
                        j += 2
                        continue
                    break
                j += 1
            cur.append(argtext[i:j + 1])
            i = j + 1
            continue
        if c in "([{":
            depth += 1
        elif c in ")]}":
            depth -= 1
        if c == "," and depth == 0:
            args.append("".join(cur))
            cur = []
            i += 1
            continue
        cur.append(c)
        i += 1
    args.append("".join(cur))
    return args


def fresh_var(existing: str) -> str:
    for cand in ("T", "T0", "Out", "Out0", "Val", "Val0", "Res", "Res0"):
        if not re.search(r"\b" + cand + r"\b", existing):
            return cand
    raise Refused("no fresh variable name available")


def convert_predicate(path: Path, name: str, arity: int) -> tuple[str, int]:
    """Return (new_text, n_clauses_converted). Refuses on anything it cannot parse."""
    text = path.read_text()
    spans = split_clauses(text)
    head_re = re.compile(r"^" + re.escape(name) + r"\s*\(")
    out, converted, pos = [], 0, 0
    for (s, e) in spans:
        clause = text[s:e]
        stripped = clause.lstrip()
        if not head_re.match(stripped):
            continue
        head, body = head_body(clause)
        open_i = head.index("(")
        close_i = head.rindex(")")
        args = split_args(head[open_i + 1:close_i])
        if len(args) != arity:
            continue
        last = args[-1].strip()
        if not ATOM_RE.match(last):
            continue                                    # already fresh, or not a bare atom
        var = fresh_var(clause)
        args[-1] = args[-1].replace(last, var, 1)
        new_head = head[:open_i + 1] + ",".join(args) + head[close_i:]
        if body is None:                                # a FACT becomes a one-goal rule
            new_clause = new_head.rstrip().rstrip(".") + f" :- {var} = {last}."
        else:
            b = body.rstrip()
            assert b.endswith("."), b
            new_clause = new_head + b[:-1].rstrip() + f",\n    {var} = {last}."
        out.append(text[pos:s])
        out.append(new_clause)
        pos = e
        converted += 1
    out.append(text[pos:])
    return "".join(out), converted


def verify(path: Path, name: str, arity: int, before: list) -> list[str]:
    """Structural post-conditions, asserted rather than assumed."""
    problems = []
    after = scan_clauses(path)
    pi = f"{name}/{arity}"
    b = [c for c in before if c[0] == pi]
    a = [c for c in after if c[0] == pi]
    if len(a) != len(b):
        problems.append(f"{pi}: clause count changed {len(b)} -> {len(a)}")
    for i, (bc, ac) in enumerate(zip(b, a), 1):
        if bc[1] != ac[1]:
            problems.append(f"{pi} clause {i}: cut presence changed {bc[1]} -> {ac[1]}")
    # POST-CONDITION IS "no BARE ATOM in the output position", not "every output is a
    # variable". A clause whose output argument is a COMPOUND (p(X, f(y))) is deliberately
    # left alone — the bound-probe hazard this template retires is about atoms, and rewriting
    # a compound head would be a different, unwitnessed change. Getting this assertion wrong
    # in the strong direction would have reverted every such file with a misleading message.
    still_atom = [i for i, c in enumerate(a, 1) if c[2].startswith("$ATOM")]
    if still_atom:
        problems.append(f"{pi}: output argument still a bare atom in clause(s) {still_atom}")
    compounds = [i for i, c in enumerate(a, 1) if c[2].startswith("$OTHER")]
    if compounds:
        problems.append(f"{pi}: NOTE compound output argument left unconverted in clause(s) "
                        f"{compounds} — declared, not a failure, but the predicate is not "
                        f"fully fresh-headed and the census will still report it")
    return problems


# ---------------------------------------------------------------------------
# The transformer is itself a claim (build_discipline -> *An introduced instrument is itself
# a claim*), so it owes fixtures — and specifically the shapes its parser could get wrong:
# facts, multi-line bodies, nested-term and quoted arguments, 0'c char codes, comments that
# look like clauses, and same-name-different-arity neighbours it must LEAVE ALONE.
# ---------------------------------------------------------------------------
SELFTEST_SRC = r"""
:- module(fx_conv, []).

% a fact -> becomes a one-goal rule
p(_, baz).

% a plain cut clause
p(X, foo) :- q(X), !.

% multi-line body
p(X, bar) :-
    q(X),
    r(X),
    !.

% nested-term argument in the head, atom still last
p(f(X, [a, b]), qux) :- q(X), !.

% quoted atom argument containing a comma and a paren
p('a,b)c', zap) :- !.

% ALREADY CONFORMING: fresh variable output, must be left untouched
p(X, T) :- q(X), !, T = already.

% last argument is a COMPOUND, not a bare atom: leave alone
p(X, f(y)) :- q(X), !.

% char code 0'. must not terminate the clause early
p(X, dot) :- X =:= 0'., !.

% DIFFERENT ARITY, same name: must be left alone
p(a, b, c).

% p(X, decoy) :- this is a comment, not a clause.
/* p(X, decoy2) :- nor is this. */
"""

SELFTEST_EXPECT_CONVERTED = 7   # baz foo bar qux zap dot  + ... see assertions below


def selftest() -> list[str]:
    import tempfile
    fails: list[str] = []
    with tempfile.TemporaryDirectory(prefix="cv_selftest_") as td:
        f = Path(td) / "fx_conv.pl"
        f.write_text(SELFTEST_SRC)
        before = scan_clauses(f)
        try:
            new_text, n = convert_predicate(f, "p", 2)
        except Refused as e:
            return [f"SELFTEST refused on fixtures: {e}"]
        f.write_text(new_text)
        after = scan_clauses(f)

        b2 = [c for c in before if c[0] == "p/2"]
        a2 = [c for c in after if c[0] == "p/2"]
        if len(a2) != len(b2):
            fails.append(f"SELFTEST clause count changed {len(b2)} -> {len(a2)}")
        if [c[1] for c in a2] != [c[1] for c in b2]:
            fails.append("SELFTEST cut presence changed across the conversion")
        still_atom = [i for i, c in enumerate(a2, 1) if c[2].startswith("$ATOM")]
        if still_atom:
            fails.append(f"SELFTEST output still a bare atom in p/2 clause(s) {still_atom}")
        compounds = [i for i, c in enumerate(a2, 1) if c[2].startswith("$OTHER")]
        if compounds != [7]:
            fails.append(f"SELFTEST expected exactly clause 7 (compound output f(y)) to be "
                         f"left unconverted, got {compounds}")

        # the compound-output clause must NOT have been rewritten
        if "p(X, f(y)) :- q(X), !." not in new_text:
            fails.append("SELFTEST a COMPOUND last argument was rewritten — only bare atoms "
                         "are in scope")
        # the already-conforming clause must be byte-identical
        if "p(X, T) :- q(X), !, T = already." not in new_text:
            fails.append("SELFTEST an already-conforming clause was rewritten")
        # different arity untouched
        if "p(a, b, c)." not in new_text:
            fails.append("SELFTEST p/3 was rewritten by a p/2 conversion")
        # comments untouched
        if "% p(X, decoy) :- this is a comment, not a clause." not in new_text:
            fails.append("SELFTEST a line comment was rewritten as a clause")
        if "/* p(X, decoy2) :- nor is this. */" not in new_text:
            fails.append("SELFTEST a block comment was rewritten as a clause")
        # the quoted-atom clause survived intact in its head
        if "p('a,b)c', " not in new_text:
            fails.append("SELFTEST a quoted atom containing , and ) was mis-parsed")
        # the char-code clause survived
        if "0'." not in new_text:
            fails.append("SELFTEST the 0'. char code was mis-parsed as a clause terminator")
        if n != 6:
            fails.append(f"SELFTEST expected 6 converted p/2 clauses "
                         f"(baz foo bar qux zap dot), got {n}")

        # and it must still load
        load = subprocess.run(["swipl", "-q", "-g", "halt", "-l", str(f)],
                              capture_output=True, text=True, timeout=120)
        if load.returncode != 0:
            fails.append(f"SELFTEST converted fixture does not load: {load.stderr[-300:]}")

    # A file the parser cannot read must be REFUSED, not silently skipped.
    with tempfile.TemporaryDirectory(prefix="cv_refuse_") as td:
        f = Path(td) / "broken.pl"
        f.write_text("p(X, foo) :- q(X), /* unterminated\n")
        try:
            convert_predicate(f, "p", 2)
            fails.append("SELFTEST unterminated block comment did not refuse")
        except Refused:
            pass
    return fails


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--file")
    ap.add_argument("--pred")
    ap.add_argument("--all", action="store_true")
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--selftest", action="store_true")
    args = ap.parse_args()

    st = selftest()
    if st:
        for s in st:
            print(f"  {s}")
        print("convert: RED (selftest)")
        return 1
    if args.selftest:
        print("convert: selftest OK (fact->rule, multi-line body, nested-term arg, quoted "
              "atom with , and ), 0'. char code, already-conforming left alone, compound "
              "output left alone, p/3 left alone, line+block comments left alone, "
              "unterminated-comment refusal, converted fixture loads)")
        return 0

    if args.all:
        targets = [(f, pi) for (f, pi), c in sorted(DECLARED.items()) if c == "latent-B"]
    elif args.file and args.pred:
        targets = [(args.file, args.pred)]
    else:
        return int(bool(ap.error("need --all or --file+--pred")))

    total, failed = 0, []
    by_file: dict[str, list[str]] = {}
    for f, pi in targets:
        by_file.setdefault(f, []).append(pi)

    for f, pis in sorted(by_file.items()):
        path = PROLOG / f
        try:
            before = scan_clauses(path)
        except Refused as e:
            failed.append(f"{f}: {e}")
            continue
        original = path.read_text()
        n_here = 0
        for pi in pis:
            name, ar = pi.rsplit("/", 1)
            try:
                new_text, n = convert_predicate(path, name, int(ar))
            except Refused as e:
                failed.append(f"{f} {pi}: REFUSED — {e}")
                path.write_text(original)
                break
            if n == 0:
                print(f"  {f} {pi}: 0 clauses to convert (already fresh-headed?)")
                continue
            path.write_text(new_text)
            n_here += n
            print(f"  {f} {pi}: {n} clause(s)")
        if args.dry_run:
            path.write_text(original)
            continue
        probs = []
        for pi in pis:
            name, ar = pi.rsplit("/", 1)
            probs += verify(path, name, int(ar), before)
        load = subprocess.run(["swipl", "-q", "-g", "halt", "-l", str(path)],
                              cwd=PROLOG, capture_output=True, text=True, timeout=300)
        if load.returncode != 0:
            probs.append(f"{f}: does not load after conversion: {load.stderr[-400:]}")
        if probs:
            path.write_text(original)          # revert the whole file, loudly
            failed += [f"{p} (file REVERTED)" for p in probs]
        else:
            total += n_here
    for p in failed:
        print(f"  FAILED: {p}")
    print(f"convert: {total} clause(s) converted across {len(by_file)} file(s), "
          f"{len(failed)} failure(s)")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
