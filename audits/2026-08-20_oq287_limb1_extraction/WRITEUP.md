# OQ-287 Limb 1 — the practice paper is extracted, and its acceptance test passes on a real deletion

**Executed:** 2026-08-20
**OQ:** OQ-287 (first limb)
**Verdict:** `docs/practice/practice_paper_v0_1.md` is authored (§I–§V, III ungated by OQ-278's
resolution) and canonical per `docs/practice/README.md`. The pre-registered acceptance condition —
**v0.6 §2.8 and §2.9 must be LOAD-BEARING in III and V, not appended** — was tested by **deleting
them from a scratch copy**, and is **MET**: after deletion, four anchors are absent while **eight
surviving references to them dangle**, §III retains a taxonomy heading over no taxonomy, and §V
retains a heading naming a negative control it no longer contains. **Scope: this is a v0.1 draft
that passes its own acceptance test — not a claim that the paper is good, reviewed, or placeable.**
**Caveat the body carries:** aptness of every citation is hand-checked and machine-unverifiable
(R2), and one ruling is routed back rather than resolved (§5).
**Substrate:** no pipeline run — paper substrate only. Code state: `git HEAD c3667f75` plus this
pass; gate GREEN before and after.
**Fired:** live — the acceptance test's **control arm caught two defects in the test's own probe**
before the test arm was believed: (1) an anchor pattern written against v0.6's capitalisation
(`EXCLUDE`) returned ABSENT on the *intact* paper, which read exactly like the deletion working; (2)
a second anchor straddled a hard line-break, so a line-based grep returned 0 on a phrase plainly
present — the wrap trap. Both would have inflated the test's verdict in the direction I wanted. A
third live fire: `checks.sh` row 3's forward-pointer arm went RED when the extraction landed, because
the marker's casing changed — a genuine demonstration that the arm added earlier today can fail.
**Evidence map:**
- `WRITEUP.md` — this file.
- `APTNESS.md` — the one-line aptness note per citation that R2 requires; the gate's green tick does
  not discharge this and reads as though it does. Also records the one citation deliberately **not**
  made, since a missing citation is otherwise indistinguishable from an oversight.
- `dangle_count.sh` — the mechanical half of the acceptance test, two-sided by construction. Run it
  against `docs/practice/practice_paper_v0_1.md` (control: all anchors PRESENT) and against
  `deletion_test_arm.md` (test: all anchors ABSENT, references surviving).
- `deletion_test_arm.md` — the scratch copy with §2.8/§2.9 material actually removed. Retained as the
  witness that the deletion was performed rather than imagined.

---

## 1. Gates, checked rather than assumed

`EXTRACTION_PROMPT.md` §0 requires these be *run*, not taken as context. All four green at HEAD:

| gate | check | result |
|---|---|---|
| G1 | OQ-278's index ruling landed; CLAUDE.md and `build_discipline.md` **agree** at index 4 | GREEN — `doc_pattern_check`: 8 indices, 0 collisions, selftest 7/7. **§III authorable.** |
| G2 | repo gate | GREEN — 26 rows |
| G3 | citation apparatus live | GREEN — `claim_cite_check` 64 live, selftest passes |
| G4 | v0.6 structure as the prompt assumes | GREEN — `checks.sh` 35 PASS / 0 FAIL |

**The prompt predicted a 14-row gate; the observed baseline is 26.** Reported as observed, per the
executor's half of the pre-authorized-dismissal rule — the mismatch is a finding about the prompt,
not about the repo.

## 2. What was authored

`docs/practice/` created with a README naming its canonical file **before** the paper needed it — a
new directory without one repeats the defect OQ-287 closed. The README states what the paper is
canonical for, what it is **not** (a table pointing at the other two papers), and — added because
§3's split showed it was the likeliest error — **what moved versus what did not**.

The paper is §I–§V per the prompt's outline. Two structural decisions worth recording:

**III is built ON the perturbation account, not around it.** The section's spine is the identity
*a failure of type X is what happens when axis X varied without your holding it; the method is the
same perturbation run on purpose*. Every practice in §III.3 is then **derived** from it and tagged
with the axis it holds (*perturb the position axis*, *perturb the probe itself*, *perturb time*).
This is what the acceptance condition is testing for, and it is why the test bites: remove the
identity and the tags point at nothing.

**V is built ON the negative control, not concluded by it.** §V.2 states why a unification claim owes
a control *at all* — a unification claim is itself an invariance claim, so by §III's own rule it
carries no information until the framing is shown able to fail to fit. §V.3 is the exclusion half,
§V.4 the break. §V.5's honest limits then sit **downstream** of a control that ran, rather than being
a caveats list.

## 3. The acceptance test, run as a real deletion

> **Delete §2.8's and §2.9's material from your draft. Do III and V still stand? If yes, they are
> appended and the acceptance condition is NOT met.**

Performed on a scratch copy: 37 lines cut from §III.1–III.2, 11 from §III.4, 63 from §V.2–V.4.

**Judgement half.** §III survives as its title plus §III.3 — a heading promising a *failure taxonomy*
over a section containing no taxonomy, introducing practices as *"the same move performed deliberately
on a named axis"* where neither the move nor the axes exist. That is precisely the **"folklore"** its
own first line disclaims. §V survives as its title plus the hazard and the limits — a section named
*"and the negative control"* containing no negative control, and a closing sentence asserting *"§V
(that it cannot certify itself)"* whose entire support was §V.4.

**Mechanical half, because the judgement half is made by the author.** `dangle_count.sh` counts
surviving references against the anchors they point at:

```
CONTROL ARM (intact paper)          TEST ARM (after deletion)
  three-axes table   PRESENT          three-axes table   ABSENT
  exclusion control  PRESENT          exclusion control  ABSENT
  the break (b)      PRESENT          the break (b)      ABSENT
  the one move       PRESENT          the one move       ABSENT
                                      ...with 8 references to them still in the text
```

**Verdict: the acceptance condition is MET.** The material is load-bearing. Subject does not need
revisiting, and Subject 3 (the recursion) does not become competitive again.

### 3.1 The control arm earned its place twice, and this is the `Fired: live`

The two-sided design was not ceremony. On its first run the **control arm reported anchors ABSENT
from the intact paper** — twice, for two different reasons:

1. **A capitalisation mismatch.** The pattern `must correctly EXCLUDE` was copied from v0.6; this
   paper writes it lowercase. Read on the test arm alone, that ABSENT is indistinguishable from the
   deletion having worked.
2. **The wrap trap.** `At the instrument stratum it does not` straddles a hard line-break in the
   source, so a line-based grep returns 0 on a phrase that is plainly there. Fixed with a newline
   normaliser — which is the documented fix for *storage-form* false absences, and explicitly **not**
   a fix for the paraphrase species (KNOWN_STATE 2026-08-20).

**Both defects biased the test toward the verdict I wanted.** That is the whole argument for the arm:
an author testing their own paper's load-bearingness has a direction of error, and only an arm that
must come back PRESENT constrains it.

## 4. Scope bounds held on §IV

All four of `EXTRACTION_PROMPT.md` §6's bounds are carried into the paper's own text rather than
observed silently by the author:

1. **Share of the paper.** §IV opens with the bound stated *before* the material, so a later reader
   can see the constraint that governed it.
2. **Floor, not measurement.** *"At least twelve, self-observed, undenominated"* — never a rate, and
   **never compared against §7.4's eleven**, with the reason (different population, different route,
   different counting) written into the paper.
3. **The narrow claim.** Declaring a framing produces a new artifact with a new framing; the remedy
   is not self-terminating. Explicitly **not** "verification is futile."
4. **Population written into the sentence.** *"Of the twelve: not one was caught by a gate."* The
   unscoped form is now false of §7.4's eleven, whose instance 11 *was* gate-caught — so the paper
   states the population every time, and then reports the exception as **more informative than the
   streak**, with the narrow generalisation about structural invariants.

## 4.1 A defect this pass introduced, found by asking what the checkers actually name

**Nothing named the practice paper.** `amnesiac_carriage_check` asserts 15 invariants over v0.6;
`checks.sh` asserts structure over v0.6 and concealment; **no checker named `practice_paper_v0_1.md`
at all.** Its only coverage was `claim_cite_check`, which picks it up **by construction** — that
checker scans the whole repository — so the paper was covered by an accident of someone else's design
decision rather than by anyone enrolling it.

**The consequence was a live dangling pointer.** v0.6 §2.8/§2.9 name `../practice/practice_paper_v0_1.md`
§III and §V as their canonical destinations. `checks.sh` row 3 asserted **v0.6's side** — that the
markers name a forward pointer, that the reversion trigger survives — and **nothing touched the far
end.** Delete or renumber §III and both markers dangle **with the gate green**.

That is **Pattern 1 on the pointer substrate, committed by the pass that built the pointers**: a
producer wired with no check that its consumer resolves. It is the same shape as the `$norm` collapse
recorded in the Limb 2 writeup — *a check scoped to what was convenient rather than to what the claim
needed* — and it survived for the mundane reason that the pointers were written **after** the check
was.

**Fixed 2026-08-20**, five arms in row 3: the destination file exists; §III is addressable; §V is
addressable; the directory carries its canonicity marker; and both markers name that exact path.
They deliberately assert **addressability, not content** — asserting content would make a closed
audit a live checker for a document it does not own.

**Discrimination, shown four ways rather than asserted.** Renumbering §III fires exactly the §III arm
(row 3 FAIL count 1); renumbering §V fires exactly the §V arm; removing the README fires exactly the
README arm; removing the file fires the existence arm and correctly suppresses its sub-arms. Baseline
and restored are 0, and the destination file is byte-identical afterwards.

> **The control's own readout was wrong first, and this is the third instance of that shape today.**
> The first pass filtered output on a pattern the *failure* text does not contain, so a fired arm
> **vanished from the view instead of showing FAIL** — and absent-from-a-filtered-view is
> indistinguishable from never-fired. Re-run printing the FAIL lines and a per-row FAIL count. Same
> family as the capitalisation and wrap-trap defects in §3.1: **each time, the instrument was sound
> and the reading of it was not.**

Arms **G/H/I** are wired into `checks.sh selftest` rather than run by hand, because a control that
ran once witnesses the arm and not the wiring. G and H are the free git pair — the destination is
**absent at `c3667f75`** and **present at `HEAD`**, neither state authored to be found.

## 5. Residue and routed-back rulings

- **ROUTED BACK, NOT REPAIRED — `docs/concealment/concealment_without_a_concealer_v0_4.md:34`.**
  That line describes v0.6 §2.8/§2.9 as declared-temporary ***pending the practice paper***. **This
  landing makes it false**, and it sits in an externally-destined document the extraction prompt
  forbids editing. Per the prompt's §10 item 6 and its licence-to-refuse: **this is an operator
  ruling** — amend the line, or accept a knowingly-stale sentence in a published artifact. It has
  **not** been silently repaired, because that would leave the defect in the design where it fires on
  the next receiver.
- **`§7.8`/`§9.4` routing is INHERITED, not ruled.** Placed under "draws on" by inheritance from
  their parents §7 and §9, and flagged as such in `EXTRACTION_PROMPT.md` §3b so the operator can
  override rather than have the call hidden.
- **Aptness is unverifiable and stays that way.** `APTNESS.md` makes it reviewable. No Appendix A row
  was minted to make any citation checkable — that would be the instrument reshaping the substrate to
  fit itself.
- **Watch-item, deliberately not a new gate.** §3's split table has no test but a receiver reading it
  cold. If a future receiver reads it and still tries to relocate §9, the repair did not take. Free
  downstream evidence; no apparatus added to collect it.
- **The paper is v0.1 and unreviewed.** It passes its acceptance test; it has had no external review,
  unlike the concealment paper's six. Its `[UNWITNESSED]`-grade content is confined to §V.5, which
  states its own kill condition.
