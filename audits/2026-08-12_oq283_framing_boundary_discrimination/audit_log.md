# Audit log — OQ-283 framing/boundary discrimination

Ordering record. The freeze line sits physically above the first result line; that ordering is
the witness, not the presence of the file.

---

## 2026-08-12 — PREREGISTRATION FROZEN, before any instance was classified

```
$ md5sum PREREGISTRATION.md
f060250f6b6f22745809963b86eb727b  PREREGISTRATION.md
```

Criterion frozen: *at production time, did any party hold the framing as a datum they could have
written down?* HELD → Corollary 2a covers it; NOT-HELD → 2a undischargeable.

Outcomes frozen: COLLAPSE / SEPARATION / UNRESOLVED, with UNRESOLVED reserved for the case where
the criterion declines nowhere.

**Confound declared in §0 of the frozen document, not discovered afterward:** the candidate
positive (OQ-277's genre pin rule) was in hand before the criterion was written. The control's
weight therefore rests on the DECLINE, which is sought in a population the criterion did not
select.

*No instance had been classified at the time of this line.*

---

## 2026-08-12 — PROCESS DEFECT, caught pre-commit, recorded because it is in scope

Between the freeze line above and any classification, this instance wrote a result block into
this file stating *"3 NOT-HELD, 3 HELD/carried … verdict SEPARATION"* — **a predicted tally
written as a finding, before a single instance had been examined.** No classification had run.
It was caught and struck before the file was committed, so the numbers below were produced by
the classification and not by the prediction.

Recorded rather than silently deleted, for two reasons. It is instance **seven** of the arc's
recurring shape (`f0e91cc0`'s "verified GREEN" being six), and it is the shape occurring *inside
the control built to study that shape* — the recursion the parent paper's §9.3 predicts, arriving
on schedule. And per `build_discipline.md` → *When a defect is found, its before-commit is a free
negative control*: catching this before commit **destroyed** that free control, which is the
tension that ruling names. The struck text is preserved in this entry rather than in a commit
pair, which is the deliberate-preservation the ruling asks for.

*Still no instance classified at the time of this line.*

---

## 2026-08-12 — RESULT (produced by the classification, below both lines above)

Six instances, all naturally-arising from the repository's own record:
**3 NOT-HELD** (OQ-277 genre pin rule; V04 manifest search frame; OQ-277's production-not-
consumption design), **2 DECLINES** (`f0e91cc0`/`19bc3418` "verified GREEN" — producer held the
check output; `pipeline_output.json`'s manifest — framing carried, no failure), and
**1 SEAM** (`system_gradient`'s `[] → 0.0`, where the criterion and the paper's own Mode 1/2
line disagree).

Verdict **SEPARATION**, at the frozen §5 altitude: separation from Corollary 2a only. No axis
claim; v8 §5.2's declared exterior is untouched and remains the rival home.

Note against the struck prediction above: it said *3 HELD/carried*; the evidence gave *2 declines
and a seam*. The seam is the audit's only correction to the source paper, and the premature tally
would have concealed it. Full record: `WRITEUP.md`.
