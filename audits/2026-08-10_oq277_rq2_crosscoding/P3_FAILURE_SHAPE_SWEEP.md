# P3 failure-shape sweep — the search the ruling was waiting on

**Executed:** 2026-08-11 · **OQ:** OQ-278 (P3 disposition) · **Fired:** live
**Verdict at its scoped altitude:** **no post-discipline failure instance; three pre-discipline
delete→restore episodes, whose status is the one thing the pre-registered branches do not settle.**
**Manifest:** repository git history at HEAD `ede866c7`.

Pre-registered branches (operator, 2026-08-11, **before** this sweep ran): witnessed failure
instance → **specify**; only prevention records → **demote**; nothing found and search shown able
to find → **demote with residue declared**. **Retire is off the table in all branches.**

---

## 1. What was searched — the shape, not the vocabulary

P3 (published) is *destructive-replace without proof*: before deleting, retiring or overwriting a
script, sweep, data file or generator that something relies on, run old and new, **paste both
outputs**, diff them, show identity or justify every difference in the same change.

The prior search was a **vocabulary** search with a vocabulary control; it found `oq106_retire`,
which records the discipline being *followed*. This sweep searches the **failure shape** instead,
by two independent probes:

- **Probe A — destructive commits lacking paired proof.** Every commit deleting a `.py`/`.pl`/`.sh`
  file, classified by whether its record carries witness language.
- **Probe B — deletions that had to be undone.** Files deleted and later re-added: the strongest
  available failure signature, because it is a *consequence* rather than a missing artifact.

**Discipline boundary: 2026-05-29** (`7af6b945`, the commit that authored `build_discipline.md` and
first wrote the words *Destructive-replace*). Commits before it are not violations of a rule that
did not exist.

## 2. Probe A — 19 destructive commits, 5 post-discipline

| commit | date | disposition | evidence |
|---|---|---|---|
| `578c5f85` | 2026-07-25 | **prevention record** | witnesses in `audits/2026-07-25_oq67_legacy_chi_retire/`; "per_constraint BYTE-IDENTICAL at n=199"; "check_stack byte-identical to a pristine HEAD extract" |
| `0259c8bd` | 2026-06-21 | **prevention record** | message is explicitly labelled *"Pattern-3 dead-code proof"* with the import-graph witness |
| `7ca48e0b` | 2026-06-12 | **prevention record** | battery witnessed; old set **retired to `kernel_v2_test2`**, i.e. archived, not destroyed |
| `29889e50` | 2026-06-05 | **not a deletion** | `report_generator.pl` still present in the tree at that commit — a move the rename detector missed |
| `ef92a61d` | 2026-06-02 | **prevention record, weaker form** | see §2.1 |

*Pre-discipline: 14 commits, outside the rule's scope, retained in §3.*

### 2.1 The one close call, recorded rather than rounded

`ef92a61d` ("remove superseded observer-axis husk") carries **no witness vocabulary at all** and is
the only post-discipline destructive commit that does not. What it does carry: provenance (the husk
was superseded ~4h after it landed), **zero engine consumers**, a stated complete blast radius
(*the only behavioral change is that reports no longer emit the `--- HUSK SIGNATURE ---` section*),
and "engine loads clean … with zero dangling references."

**P3's own text permits this**: *"show identity **or justify every difference** in the same
change."* It justifies, and it names the one difference. **But P3 also says "structurally
equivalent is a code-read, not proof — the diff is proof,"** and every claim in that message is a
code-read. **No old-vs-new output was pasted.**

**Classified as a prevention record in the weaker of the two permitted forms, not as a failure.**
Nothing broke, nothing was restored, no consequence was witnessed. Recorded here because it is the
closest the post-discipline population comes to the shape, and a later reader should see the call
rather than the conclusion.

## 3. Probe B — deletions that had to be undone

**135 files, collapsing to 3 distinct episodes.** (135 was never treated as an incident count; one
episode covers 133 files.)

| files | deleted | restored | side of the boundary |
|---|---|---|---|
| **133** | 2026-02-25 `2726506e` "Framework audit…" | 2026-02-27 `828ad085` "Haiku into testsets…" | **pre-discipline** |
| 1 | 2026-02-14 `669ab97a` "Clean prolog and linter run…" | 2026-02-27 `828ad085` | **pre-discipline** |
| 1 | 2026-02-06 `c6e8969b` "Major refactoring…" | 2026-02-08 `9b3e4fe9` | **pre-discipline** |

**Post-discipline delete→restore episodes: ZERO.**

**The 133-file episode has the signature of an undone mistake, not a move**: identical paths before
and after, and the deleting commit removed **166** files of which **133** came back — a selective
deletion with partial restoration. That is the P3 failure shape. It is also three months before the
rule existed, in `prolog/archives/prolog_v3/`, during the corpus-rebuild churn.

## 4. Controls — the sweep is shown able to find

| control | result | grade |
|---|---|---|
| Probe A declines on genuine proof | `578c5f85`, `0259c8bd` both classify **prevention record** | naturally-arising negative ×2 |
| Probe A fires when proof is absent | same commit with witness language stripped → **flags** | authored (weakest) |
| Probe B is not an empty search | returns **3 real episodes** on real history | naturally-arising positive ×3 |
| Probe B distinguishes moves from deletions | `29889e50` correctly excluded — file survives in-tree | naturally-arising negative |

**"I didn't find it" is therefore a fact about the world here, not only about the search** — for
the post-discipline window. Probe B demonstrably finds restorations; there are none after
2026-05-29.

## 5. Where the evidence lands, and the one thing it does not settle

**Post-discipline population: branch 2.** Five destructive commits, four prevention records, one
non-deletion, zero restorations, zero witnessed failures.

**The pre-registered branches do not say whether pre-discipline instances count**, and this is
decision-relevant rather than academic:

- **If they do not count** → branch 2 outright: P3 has no failure instance in the window where it
  was a rule, and *demote* follows.
- **If they do count** → the 133-file episode is a witnessed failure instance of the shape, and
  branch 1 (*specify*) is live — with the complication that P3's authoring cites no incident, so
  the rule would be one written three months after an episode nobody connected to it.

**This sweep does not resolve that, deliberately.** It is a scope question about what the branches
range over, which is the operator's seat, and resolving it here would be an instance answering the
question it was sent to gather evidence for.

**One datum that bears on it:** P3's text, at authoring and today, cites **no dated instance** —
alone among the six. If the Feb episode had motivated it, the citation was available and unused.
