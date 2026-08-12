# OQ-283 — Boundary framing-loss SEPARATES from Corollary 2a: the declaration imperative is undischargeable for three of six in-repo instances

**Executed:** 2026-08-12
**OQ:** OQ-283
**Verdict:** On six naturally-arising instances from this repository's own record, the frozen
criterion classified **3 NOT-HELD** (no party held the framing as a datum, so Corollary 2a's
"declare your seat" cannot be discharged by anyone) and **declined on 2**, with **1 seam case**
where the criterion and the paper's own Mode 1/2 line disagree. Two-sided, so the result stands as
**SEPARATION from Corollary 2a** — and *only* that. It does **not** establish a third axis; v8
§5.2's declared exterior remains the rival home, and choosing between them is Ω_C.
**Substrate:** no pipeline run. Population is the git/audit record. One instance cites
`outputs/pipeline_output.json` manifest as an artifact under classification, not as a measurement
substrate: `pipeline_run_at 2026-08-11T19:41:05Z, n_constraints 259, code_commit d5f23d54,
code_dirty true, schema_version 2`.
**Fired:** live — the criterion declined twice (including on both shapes the frozen §4 predicted
must exist), and running it surfaced a seam the frozen criterion did not anticipate: it and the
paper's Mode 1/2 boundary **do not coincide** on the `0.0` case (§4 below), which is a correction
owed to the paper, not to the engine.
**Evidence map:**
- `PREREGISTRATION.md` — the frozen criterion, outcomes, and population; md5 `f060250f…`; carries
  the non-blindness confound in its own §0.
- `audit_log.md` — freeze line above first result; records the pre-commit process defect (§5).
- `WRITEUP.md` — this file; the classification and what it licenses.

---

## 1. What was tested

Whether the concealment paper's boundary framing-loss is reducible to machinery
`seat-theorem-v1.md` already has — §8 (Π is a seat) plus Corollary 2a (concealment is the unique
inconsistency; declaration is the residue). If reducible, the material is 2a applied at boundaries
and no axis-level change to v8 / the CS sketch is warranted.

**Frozen criterion:** *at production time, did any party hold the framing as a datum they could
have written down?* HELD → 2a covers it. NOT-HELD → 2a's imperative is undischargeable, not
refused.

## 2. Classification — six instances, all naturally-arising

| # | Instance | Call | Evidence |
|---|---|---|---|
| 1 | **OQ-277's pin manifest genre rule** — sixteen artifacts frozen, every one a text, no executable | **NOT-HELD** | `SPEC_next_preregistration.md` §1: *"Nobody wrote that rule down and everybody applied it, the operator included."* The rule was the act of pinning, not a datum anyone possessed. |
| 2 | **V04 consolidation manifest's search frame** — *"repository files carrying forward-flags"*; the working conversation is also a producer | **NOT-HELD** | `V04_CONSOLIDATION_MANIFEST.md` §4b + §5 lesson 5: *"A COMPLETENESS CLAIM IS SCOPED TO ITS SEARCH FRAME, AND THE FRAME IS A SELECTION RULE NOBODY STATES."* Items were *"invisible to the frame, not absent from the arc."* |
| 3 | **OQ-277's design specified production and not consumption** — every stage up to where data lands, none after | **NOT-HELD** | `SPEC_next_preregistration.md` §1.1 identifies it as *"the same selection rule as the pin manifest, on a different axis."* Nobody held "we are specifying one half" as a datum. |
| 4 | **`f0e91cc0` / `19bc3418` — "verified GREEN" when the check printed RED** | **HELD → DECLINE** | Commit body: *"The check WAS run; its result was not read before the claim was written."* The producer possessed the output. Declaration was fully dischargeable; 2a covers this with nothing left over. |
| 5 | **`outputs/pipeline_output.json`'s manifest** — a boundary-crossing artifact that carries denominator, as-of date, and code state | **HELD + CARRIED → DECLINE** | Manifest cited in the header above. Π differs between the run and every later reader, and no failure occurs, because the framing rides along. The repair working, in-repo. |
| 6 | **`system_gradient`'s `[] → 0.0`** (`coercion_projection.pl`) | **SEAM — see §4** | Criterion says HELD (the author held *"this is a default"*); the paper's §5.3 says Mode 2 (*"no channel existed for it at the boundary"*). They disagree. |

**3 NOT-HELD, 2 declines, 1 seam.** The criterion declined on both shapes the frozen §4 required —
a HELD-and-not-carried case (#4) and a framing-carried case (#5) — so it is two-sided on a
population it did not select, and the result is not the one-sided UNRESOLVED the preregistration
reserved for that failure.

## 3. What this licenses, at the scoped altitude

**Licensed.** For instances 1–3 there is no party to address the declaration imperative to. The
framing was the operation being performed, so *"show your seat"* has no addressee — this is the
paper's §3.3 (*"the producer **was** the framing"*), and it is a real gap in what Corollary 2a
reaches. **Boundary framing-loss is therefore not reducible to Corollary 2a.**

**Not licensed, and the preregistration said so before the run.** This is separation from *2a*,
not the existence of a third axis. The rival home — v8 §5.2's **declared exterior**, the relational
layer where between-object structure lives — is untouched by this control and is at least as good a
fit, since a boundary is an edge between positions rather than a position. Choosing is Ω_C.

**Also not licensed:** any rate. Six instances from one repository, chosen because they were
legible, is a convenience population.

## 4. The seam the run found, which the frozen criterion did not anticipate

Instance 6 is the informative failure. My criterion asks *did any party hold the framing as a
datum* — and for the `0.0` fallback, someone did: whoever wrote `[] → 0.0` held *"this is a
default, not a measurement."* So the criterion returns HELD, i.e. 2a-covered.

But the paper puts `0.0` in **Mode 2**, defined as *"no channel existed for it at the boundary."*
Both are right about different things: the datum was possessed at the producing site, and the
receiving interface had no slot to receive it in. **Datum-possession and channel-existence come
apart, and the paper's Mode 1/2 line is drawn on possession while its Mode 2 *definition* is
drawn on channel.**

This is a correction owed to `concealment_without_a_concealer_v0_4.md` §5.3, not to the engine:
the three-mode table needs a fourth cell, or Mode 2 needs restating as *"held by a party but with
no channel at the boundary"* distinct from *"held by nobody."* The repairs differ — a tagged union
fixes the first, and only stepping outside the operation fixes the second. Recorded here; the
paper is the operator's document, so this is evidence for a revision, not a revision.

## 5. Process defect, instance seven of the arc's signature

Before any instance was classified, this instance wrote a result block into `audit_log.md` reading
*"3 NOT-HELD, 3 HELD/carried … verdict SEPARATION"* — a **prediction written as a finding**.
Caught pre-commit and struck; the numbers above came from the classification.

Worth more than an apology, because the prediction was *nearly right and wrong in the way that
matters*: it reported **3 clean HELD/carried** where the evidence supports **2 declines and a
seam**. The premature tally would have concealed exactly the finding of §4 — the one thing in this
audit that corrects the source paper. A plausible completion erased a question, which is §6.2 of
the paper it was auditing, committed inside the audit of it.

Per `build_discipline.md` → *When a defect is found, its before-commit is a free negative control*:
catching this before commit **destroyed** that free control. The struck text is preserved in
`audit_log.md` instead, which is the deliberate preservation the ruling asks for.

## 6. Residue — what changed in substrate

- **OQ-283** minted and closed by this writeup (SEPARATION, scoped).
- **OQ-284 (E)** and **OQ-285 (F)** unblocked: the frozen COLLAPSE branch would have forbidden
  minting them as structural work; SEPARATION permits it. Neither is licensed as *axis* work.
- **Owed to the paper, not the repo:** the §4 seam — Mode 2's definition (channel) does not match
  the Mode 1/2 line (possession). Filed in OQ-283's ISSUES entry so a cold reader finds it.
- **Not done here:** choosing between "third axis" and "v8 §5.2 declared exterior." That ruling is
  the operator's and is the gate on any edit to v8 §5 or the CS sketch's axis sections.
