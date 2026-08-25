#!/usr/bin/env python3
"""recon.py — the generic ID-keyed cross-artifact reconciler.

The library behind `docs/technical/build_discipline.md` -> *Cross-artifact
reconciliation: two artifacts naming the same units owe an ID-keyed join (fork
found, ruling owed)*. Read that section before using this: it states the rule,
the two rulings automation CANNOT make, and why the output is never a winner.

WHAT THIS IS FOR
    Two artifacts describe the same observational units -- a spec and its
    implementation, a catalog and a dataset, a tracker and its derived index.
    Their aggregates agree. That is not evidence they agree, because compensating
    per-unit differences leave every marginal identical while most rows disagree.
    This joins them on the unit ID and reports what the marginals conceal.

WHAT THIS DELIBERATELY DOES NOT DO
    It never picks a canonical side. Canonicity is an operator ruling over a
    modelling question ("which artifact is authoritative?"), and so is unit
    choice ("what counts as the same unit?"). A reconciler that answered either
    would have manufactured a verdict. The report ends "fork found, ruling owed"
    and names the forks; a human rules.

TWO DESIGN DECISIONS THAT LOOK LIKE FUSSINESS AND ARE NOT

  1. INPUTS ARE SEQUENCES OF PAIRS, NOT DICTS.
     `reconcile(a_pairs, b_pairs, fields)` takes `[(unit_id, {field: value}), ...]`.
     A dict-keyed input would collapse duplicate unit IDs last-write-wins IN THE
     CALLER, before this code ever saw them -- so the duplicate refusal below
     would be unreachable, guarding against a state that the input type had
     already made impossible to express. Pairs keep the duplicates visible.

  2. THE DUPLICATE DOWNGRADE IS MECHANICAL, NOT A NORM.
     Duplicate unit IDs within one side raise by default. The downgrade takes the
     EXPECTED duplicate IDs as its value -- `allow_duplicates={"i-07", "i-11"}` --
     so naming them IS the report, and any duplicate NOT named still raises. A
     boolean `allow_duplicates=True` would leave "and report them" as caller
     discipline this library cannot enforce, which is the same shape as a gate
     that passes because its input was missing.

ABSENCE IS A THIRD BUCKET.
    A field present on one side and absent on the other is neither agreement nor
    disagreement. Coercing it into either is `Absence satisfies the gate` wearing
    a join. It gets its own bucket, and its presence alone is enough to make the
    report say "ruling owed".

AN INTRODUCED RECONCILER OWES ITS OWN CONTROL.
    A join returning "no disagreement" is indistinguishable from a join that
    never dispatched. `selftest()` below is the two-sided pair: a planted
    agreement that must yield zero disagreements, and a planted disagreement that
    must NAME the mutated unit rather than merely go red. An adapter built on this
    library owes the same pair over ITS OWN parse -- this selftest covers the join
    algebra, not anybody's parser.
"""
from __future__ import annotations

from collections import Counter
from dataclasses import dataclass, field as _dc_field
from typing import Any, Iterable, Mapping, Sequence

ABSENT = object()   # sentinel: this side did not carry the field at all


class DuplicateUnitIDs(ValueError):
    """Raised when one side carries a unit ID more than once, un-declared."""


@dataclass
class FieldResult:
    """One field's join outcome across the shared units."""
    name: str
    agree: list[str] = _dc_field(default_factory=list)
    disagree: list[tuple[str, Any, Any]] = _dc_field(default_factory=list)
    absent: list[tuple[str, str]] = _dc_field(default_factory=list)  # (unit, which side)

    @property
    def forked(self) -> bool:
        return bool(self.disagree or self.absent)


@dataclass
class ReconResult:
    """The full join. Nothing here is a verdict; every list is an observation."""
    fields: list[str]
    shared: list[str]
    only_a: list[str]
    only_b: list[str]
    per_field: dict[str, FieldResult]
    marginals_a: dict[str, Counter]
    marginals_b: dict[str, Counter]
    dup_a: dict[str, int]
    dup_b: dict[str, int]
    label_a: str
    label_b: str

    @property
    def forked(self) -> bool:
        """True if ANY fork exists: membership, disagreement, or one-sided absence."""
        return bool(self.only_a or self.only_b
                    or any(f.forked for f in self.per_field.values()))


def _ingest(pairs: Sequence[tuple[str, Mapping[str, Any]]], side: str,
            allow_duplicates: Iterable[str] | None) -> tuple[dict, dict]:
    """Pairs -> (by_id, duplicate_counts), refusing un-declared duplicates.

    The refusal is reachable precisely because the input is a sequence: a dict
    would have collapsed these in the caller.
    """
    declared = set(allow_duplicates or ())
    counts = Counter(uid for uid, _ in pairs)
    dups = {u: c for u, c in counts.items() if c > 1}
    undeclared = sorted(set(dups) - declared)
    if undeclared:
        raise DuplicateUnitIDs(
            f"{side}: {len(undeclared)} unit ID(s) appear more than once and are not "
            f"declared: " + ", ".join(f"{u}(x{dups[u]})" for u in undeclared) +
            ". If these are expected, pass allow_duplicates={...} NAMING them -- "
            "naming them is the report. Later occurrences win."
        )
    by_id: dict[str, Mapping[str, Any]] = {}
    for uid, rec in pairs:
        by_id[uid] = rec
    return by_id, dups


def reconcile(a_pairs: Sequence[tuple[str, Mapping[str, Any]]],
              b_pairs: Sequence[tuple[str, Mapping[str, Any]]],
              fields: Sequence[str],
              *,
              label_a: str = "A",
              label_b: str = "B",
              allow_duplicates_a: Iterable[str] | None = None,
              allow_duplicates_b: Iterable[str] | None = None) -> ReconResult:
    """Join two artifacts on unit ID and compare `fields`. Adjudicates nothing."""
    a, dup_a = _ingest(a_pairs, label_a, allow_duplicates_a)
    b, dup_b = _ingest(b_pairs, label_b, allow_duplicates_b)

    shared = sorted(set(a) & set(b))
    per_field: dict[str, FieldResult] = {}
    for f in fields:
        fr = FieldResult(name=f)
        for uid in shared:
            av = a[uid].get(f, ABSENT)
            bv = b[uid].get(f, ABSENT)
            if av is ABSENT and bv is ABSENT:
                fr.absent.append((uid, "both"))
            elif av is ABSENT:
                fr.absent.append((uid, label_a))
            elif bv is ABSENT:
                fr.absent.append((uid, label_b))
            elif av == bv:
                fr.agree.append(uid)
            else:
                fr.disagree.append((uid, av, bv))
        per_field[f] = fr

    def marg(side: dict) -> dict[str, Counter]:
        return {f: Counter(r[f] for r in side.values() if f in r) for f in fields}

    return ReconResult(
        fields=list(fields), shared=shared,
        only_a=sorted(set(a) - set(b)), only_b=sorted(set(b) - set(a)),
        per_field=per_field, marginals_a=marg(a), marginals_b=marg(b),
        dup_a=dup_a, dup_b=dup_b, label_a=label_a, label_b=label_b,
    )


def render_report(r: ReconResult, *, unit_noun: str = "unit") -> str:
    """The text form: matrix, named mismatch rows, and BOTH marginals.

    The marginals are printed beside the per-unit result on purpose. They are
    what a reader would otherwise have compared by eye, and printing them next to
    the rows they conceal is what makes the concealment visible rather than
    arguable.
    """
    L: list[str] = []
    add = L.append
    add(f"Cross-artifact reconciliation: {r.label_a}  vs  {r.label_b}")
    add("=" * 72)
    add(f"membership: {len(r.shared)} shared {unit_noun}(s); "
        f"{len(r.only_a)} only in {r.label_a}; {len(r.only_b)} only in {r.label_b}")
    for lbl, ids in ((r.label_a, r.only_a), (r.label_b, r.only_b)):
        if ids:
            add(f"  only in {lbl}: " + ", ".join(ids))
    for side, dups in ((r.label_a, r.dup_a), (r.label_b, r.dup_b)):
        if dups:
            add(f"  DECLARED duplicate ids in {side}: "
                + ", ".join(f"{u}(x{c})" for u, c in sorted(dups.items())))

    for f in r.fields:
        fr = r.per_field[f]
        add("")
        n = len(r.shared)
        pct = f"{100*len(fr.agree)/n:.0f}% agreement" if n else "no shared units"
        add(f"field `{f}`: {len(fr.agree)} of {n} agree ({pct}); "
            f"{len(fr.disagree)} disagree; {len(fr.absent)} absent on one side")
        if fr.disagree:
            add(f"  mismatch rows ({unit_noun} | {r.label_a} | {r.label_b}):")
            for uid, av, bv in fr.disagree:
                add(f"    {uid:<52s} {av!s:<12s} {bv!s}")
        if fr.absent:
            add("  absent-on-one-side (NOT counted as agreement or disagreement):")
            for uid, which in fr.absent:
                add(f"    {uid:<52s} missing on {which}")
        ma, mb = r.marginals_a.get(f, Counter()), r.marginals_b.get(f, Counter())
        keys = sorted(set(ma) | set(mb), key=str)
        add(f"  marginals  {r.label_a}: " + "/".join(f"{k}{ma.get(k,0)}" for k in keys))
        add(f"  marginals  {r.label_b}: " + "/".join(f"{k}{mb.get(k,0)}" for k in keys))
        if keys and ma == mb:
            add("  NOTE: the marginals are IDENTICAL. That is not agreement -- "
                "see the per-unit rows above.")

    add("")
    if r.forked:
        add("FORK FOUND, RULING OWED.")
        add("  Two rulings this tool cannot make, and did not: (a) unit choice -- what")
        add("  counts as the same unit; (b) which artifact is canonical where they fork.")
        add("  Both are the operator's seat. Nothing above is a verdict about either.")
    else:
        add("No fork on the compared fields. (This licenses a claim about THESE fields")
        add("  over THESE shared units -- not about the artifacts as wholes.)")
    return "\n".join(L)


def selftest(verbose: bool = True) -> int:
    """Two-sided control on the join algebra. Returns 0 on pass.

    Planted agreement must find nothing; planted disagreement must NAME the
    mutated unit (going red is not enough -- a detector that fires without
    localising cannot be distinguished from one that always fires).
    """
    fails: list[str] = []
    def chk(name: str, ok: bool) -> None:
        if verbose:
            print(f"  {'PASS' if ok else 'FAIL'}  {name}")
        if not ok:
            fails.append(name)

    base = [(f"u{i}", {"cls": c}) for i, c in enumerate("ABCDE" * 3)]

    # (1) planted agreement: one artifact against itself.
    r = reconcile(base, list(base), ["cls"], label_a="self", label_b="self'")
    chk("planted agreement -> 0 disagreements", not r.per_field["cls"].disagree)
    chk("planted agreement -> not forked", not r.forked)

    # (2) planted disagreement: mutate exactly one unit; the tool must NAME it.
    mut = [(u, dict(rec)) for u, rec in base]
    mut[7] = (mut[7][0], {"cls": "Z"})
    r2 = reconcile(base, mut, ["cls"], label_a="orig", label_b="mutated")
    d = r2.per_field["cls"].disagree
    chk("planted disagreement -> exactly 1 disagreement", len(d) == 1)
    chk("planted disagreement -> NAMES u7 with both values",
        bool(d) and d[0][0] == "u7" and d[0][1] == "C" and d[0][2] == "Z")
    chk("planted disagreement -> report says fork found",
        "FORK FOUND, RULING OWED." in render_report(r2))

    # (3) marginals-identical trap: swap two units' classes between the sides.
    #     Every marginal is preserved; two rows disagree. This is the whole point.
    sw = [(u, dict(rec)) for u, rec in base]
    sw[0] = (sw[0][0], {"cls": base[1][1]["cls"]})
    sw[1] = (sw[1][0], {"cls": base[0][1]["cls"]})
    r3 = reconcile(base, sw, ["cls"], label_a="orig", label_b="swapped")
    chk("compensating swap -> marginals IDENTICAL",
        r3.marginals_a["cls"] == r3.marginals_b["cls"])
    chk("compensating swap -> 2 per-unit disagreements found anyway",
        len(r3.per_field["cls"].disagree) == 2)
    chk("compensating swap -> report flags the identical marginals",
        "the marginals are IDENTICAL" in render_report(r3))

    # (4) absence is a third bucket, never coerced.
    miss = [(u, dict(rec)) for u, rec in base]
    miss[3] = (miss[3][0], {})
    r4 = reconcile(base, miss, ["cls"], label_a="full", label_b="holed")
    fr = r4.per_field["cls"]
    chk("absence -> lands in the absent bucket", len(fr.absent) == 1)
    chk("absence -> NOT counted as agreement", "u3" not in fr.agree)
    chk("absence -> NOT counted as disagreement",
        all(x[0] != "u3" for x in fr.disagree))
    chk("absence alone -> still 'ruling owed'", r4.forked)

    # (5) membership fork is reported, not silently intersected away.
    r5 = reconcile(base, base[:-2], ["cls"], label_a="long", label_b="short")
    chk("membership fork -> only_a names the 2 dropped units", len(r5.only_a) == 2)
    chk("membership fork -> forked", r5.forked)

    # (6) the duplicate refusal is REACHABLE and the downgrade is per-id.
    dup = base + [("u3", {"cls": "Q"})]
    try:
        reconcile(dup, base, ["cls"]); raised = False
    except DuplicateUnitIDs as e:
        raised = "u3" in str(e)
    chk("undeclared duplicate -> raises AND names the id", raised)
    try:
        reconcile(dup, base, ["cls"], allow_duplicates_a={"u3"}); ok = True
    except DuplicateUnitIDs:
        ok = False
    chk("declared duplicate -> accepted", ok)
    try:
        reconcile(dup + [("u5", {"cls": "Q"})], base, ["cls"],
                  allow_duplicates_a={"u3"}); still = False
    except DuplicateUnitIDs as e:
        still = "u5" in str(e) and "u3" not in str(e)
    chk("declaring one duplicate does NOT license another", still)

    if verbose:
        print(f"\nrecon selftest: {'GREEN' if not fails else 'RED'} "
              f"({'all controls fired' if not fails else '; '.join(fails)})")
    return 0 if not fails else 1


if __name__ == "__main__":
    import sys
    sys.exit(selftest())
