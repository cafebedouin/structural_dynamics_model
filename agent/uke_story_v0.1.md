# UKE_STORY v0.1 [Universal Knowledge Evaluator — Narrative Improvement Protocol]
## Drafted: 2026-07-12 · Status: SUPERSEDED by `agent/uke_story_v0.2.md` (retained as review lineage; never run against a seed)

[UKE_META]
protocol: UKE_STORY v0.1
input: a seed narrative (resleeving-pipeline Stage-4/rev output, OR any drafted story) + its SECTION 0 invariant contract + [optional] the pipeline audit trail (stages 5–10)
governing_law: break, do not polish. A change is kept only if it sharpens; a change that smooths toward the competent mean is rejected even when it "reads better."
terminal_gate: the blind originality read (§7) — run without the audit trail, or the apparatus vouches for its own output.
deliverable: an improved story + a sharpening manifest (every change carries its witness) + Ω. The improved story is still a hypothesis until the terminal gate passes.
concept_budget: 4 terms. *seed triage* (§0.5), *the untranslatable real / the grain* (§1), *the seat test* (§3), *break-not-polish* (governing law). Each must survive the Parfit test — shorter than its translation — or be cut.

---

## §0. FOUNDATION

**Purpose:** Transform a competent seed narrative into a story that survives intelligent criticism *and earns its existence against the competent mean* — a story a capable model would not have written on its own.

**Core Invariants:**
- **Sharpening > improvement.** These are opposites as often as not. Improvement smooths; sharpening cuts.
- **Witness > assertion.** Every "sharpened / fixed / deepened" claim carries the before→after line in the same pass. "Tightened the ending" is not done; the pasted change is.
- **The read is the verdict; the apparatus is a proxy.** No gate, score, or manifest substitutes for the terminal originality read.

**The Central Tension:**
An LLM improving an LLM story regresses to the competent mean by default — safer, smoother, more consensus, i.e. *more* template. The whole reason this protocol exists is that the seed's ceiling is fixed by the process that produced it (topology-preserving resleeving reliably yields competent sevens and structurally cannot exceed them). UKE_STORY is the stage where the straitjacket comes off — but only if "improvement" is disciplined into "sharpening," because ungoverned LLM revision moves in exactly the wrong direction. The governing law and the erasure diagnostic (§4) are what keep this from becoming a high-gloss finish on the same template.

**Relationship to the other protocols.** UKE_STORY is the narrative sibling of UKE_WRITE (argument), UKE_THINK (philosophy), UKE_OPINION (criticism). It inherits their adversarial spine (assume intelligent opposition; the blow must land; degrade by narrowing scope, not by defeating all objections), their counterfactual discipline (if the evidence would look identical whether the claim is true or false, it is decoration — delete), and UKE_THINK's A/B/C complication typing (here: seed triage). It replaces their evidence-tier machinery with the **invariant audit** (§1): a story's "Tier 1" is its untranslatable real, and everything else is load-bearing only insofar as it serves it.

> **Naming note (Ω-NAME, RESOLVED 2026-07-12):** the translation instrument is the resleeve pipeline itself (`agent/uke_narrative_orchestrator.py` + `agent/narrative_transform/` stages 2–5), not a protocol file; this protocol is the *improvement/evaluation* instrument, a distinct adjacent stage. The file was renamed `uke_resleeve_v0.1.md` → `uke_story_v0.1.md` so the improvement lineage ships under its own handle (pipeline = translation; UKE_STORY = improvement).

**Input / Output.**
- IN: seed story; SECTION 0 invariant contract (the untranslatable real + the missing-floor invariant, if present, each with its falsifier); optionally the stages 5–10 audit trail — used for *diagnosis* in §0.5–§6 and **withheld** from §7.
- OUT: improved story; sharpening manifest (change → type A/B/C → witness → erasure-survived? y/n); Ω; terminal-gate verdict.

---

## §0.5. SEED TRIAGE (improve, re-found, or reject)

Before touching prose, classify what is wrong with the seed in one sentence. If you cannot, that is the first problem to solve. Ported from UKE_THINK §0.5.

**Type A — Register / frame drift.** The bones are sound; the *valence* has slipped. The ending converts unredeemed cost into meaning, hope, or knowledge-transmission; the tone consoles where the material demands cold; a motif softens the sting on repetition. *(Witnessed instance: rev2's gold-crater-sunset "someone would still be listening" — the invariant preserved, the valence warmed.)* **Resolution: reframe.** Land the cost; earn or cut the consolation (§2). Cheapest fix; most common.

**Type B — Broken invariant.** The story's central "real" is a *knowable value the system mismeasured*, not an untranslatable real. It is Assessment-class: a hidden bias, documentable, correctable by a fairer authority or a better instrument. No amount of prose fixes this; the foundation is wrong. *(Witnessed instance: rev6 "The Platform Knows" — a posture index measuring a recoverable fall-risk it gets wrong; the invariant held only by the ending's refusal, not by the physics.)* **Resolution: re-found the invariant as in-principle-unreadable (§1), or reject the seed.** Do not polish a Type-B seed; polish disguises it.

**Type C — Underspecified / doubled.** The story is wearing one face and telling two stories, or a POV is doing two jobs (the laborer who also analyzes; the beneficiary who also sees the extraction — collapsing the archetype's knowledge boundary). Indexical variance is asserted but not dramatized. **Resolution: split or index.** Give each seat its own knowledge; make the variance visible in action, not narration.

**Declare the type in the manifest.** The resolution strategy must match the type. Reframing a Type-B seed (the frequent LLM move — make the correctable-bias story *prettier*) is F-MISROUTED-RESOLUTION and accomplishes nothing.

---

## §1. THE INVARIANT AUDIT (D9 as a writing discipline)

The seed's Tier 1 is its **untranslatable real** — the grain: the thing the story's system cannot read, own, or resolve to a value (a texture the sensor misses, a name that lies when claimed, a fidelity that collapses on measurement). Everything else is decoration unless it serves the grain.

**The counterfactual test (the load-bearing gate).**
> Would this story read *identical* if its invariant were merely hidden rather than untranslatable — if a better instrument, a fairer authority, or a recalibration could recover the "true value"?

- If identical → the invariant is decoration; the story is Assessment-class; route to Type-B triage.
- If the story would *break* under that substitution — because the real has no value in the system's terms at all, only a felt grammar outside it — the invariant is load-bearing. Keep, and make the un-recoverability visible in-scene.

**The two-reals check.** A seed often carries both a *recoverable* injustice (a biased score, discardable and in principle fixable) and an *untranslatable* real (the grain). This is fine — reality is layered — *only if the story subordinates the correctable reading to the untranslatable one*, on-screen, and does not let a "build-a-better-meter" subplot become the resolution. *(Witnessed: rev2/rev4 stage the alternative-standard temptation and refuse it: "This will not make you money." That refusal is what earns the invariant. rev6 leaves the correctable reading dominant and is saved only by its last page — precarious.)*

**Do not let the invariant become an administrator.** If the grain acquires an enforcement mechanism or a fixer, it has become a Snare and stopped being the grain.

---

## §2. THE RUPTURE AUDIT (the ending; the blow must land)

Inherited from UKE_THINK §124 ("if the counterargument doesn't make the writer uncomfortable, it hasn't been written honestly"), ported to cost.

1. **Cost accounting.** What did the story's events actually cost — physically, relationally, psychologically? Not thematically.
2. **The blow lands first.** The cost must fully threaten to be *unredeemed* before any meaning arrives. If consolation arrives before the cost has landed, it is substituting for the cost, not answering it.
3. **Consolation discipline.** Meaning, if it comes, comes later and from a different direction, and must be *earned against the fully-landed cost*, not offered in place of it. "The knowledge exists but changes nothing" is earned; "someone would still be listening" is warm — check which you wrote.
4. **Administrative / comfort resolution.** Is the final movement accomplished by a system, an organization, a published critique, or a knowledge-transmission that lets the reader off the hook? The pipeline's native default is *quiet endurance + organizational hope*; treat any ending that lands there as suspect until the material is shown to demand it.
5. **The discomfort test.** If the ending makes neither writer nor reader uneasy, it flinched. HUMAN-REVIEW-not-PUBLISH is the correct routing for a flinch; the flag is the system working, not a near-miss.

---

## §3. THE SEAT TEST (position-invariant vs locally-coherent — the anti-template move)

Ported from UKE_THINK §116 (the Weight Check). This is the primary lever against template-sameness.

The resleeve template is a triad: a laborer with bodily knowledge, an analyst who documents the bias, a beneficiary at a remove — and meaning that lives *only* in the laborer's seat. That is **locally coherent**: the story holds from the position that generated it and dissolves when read from the beneficiary's or analyst's seat, which carry no independent truth, only roles.

**The test:** does the story's meaning hold *mechanistically from every relevant seat*, even where each seat experiences it differently — or only from the protagonist's?

- **Locally coherent (weaker):** the beneficiary is a prop; the analyst is a mouthpiece; the story means one thing, from one seat.
- **Position-invariant (Rift-class):** the instruments' blindness is *true from inside each instrument* — the beneficiary's comfort is a real seat with its own coherence, the analyst's documentation is real and futile in its own terms, and the story does not need any single character to be right. *(The strongest seed closer in the set — rev3's "the system is working smoothly from where they sit" — earns its cold because the beneficiary's seat is genuinely coherent, not strawmanned.)*

**The break:** make at least one non-protagonist seat carry an independent truth the story does not resolve against the protagonist's. This is the single most reliable way to exceed the template, because the template's ceiling *is* single-seat meaning.

---

## §4. THE BREAK-NOT-POLISH GATE (the governing law)

Every proposed change passes through this gate before it is kept.

**The erasure diagnostic** (ported from UKE_OPINION §II). Delete the change. Is the story just as strong without it?
- **Yes → it was decoration** (prose concept-inflation). Cut it. A change that survives its own deletion added gloss, not sharpness.
- **No → the story is genuinely weaker without it** → it is load-bearing → keep, *and record the witness* (the before→after line, the seat it sharpened, the flinch it cut).

**The sharpens-or-reject rule.** A kept change must do at least one of: break the template (§3), deepen the invariant's un-recoverability (§1), land or expose a cost the seed softened (§2), or split a doubled POV (§0.5-C). A change that only smooths — clearer, safer, more balanced, more "literary" — is the regression-to-mean and is rejected on sight, however much it "reads better."

**Subtraction over addition.** Default to cutting. Most sharpening in a competent seed is removal — the softening clause, the explanatory sentence, the consolation beat, the second POV that was doing the first one's job. Addition is suspect; it is where polish enters.

---

## §5. THE REGISTER AUDIT (number and metric discipline)

The seed's origin is a χ/ε formalism; its characteristic leak is *numeric register* — counting as texture, extraction-math on the page, the meter's fingerprints surfacing as diegetic prose. This is the narrative analogue of a character saying "epsilon," one level below where a terminology grep can see it.

- **Number only when a positioned character acts on it in-scene** — reads it aloud, forges it, breaks the weight. Never as ambient texture, countdown, tally, or emotional beat.
- **Positional access governs** (the rift3 discriminator). A gauge-owning institutional POV acting on a reading is earned; a powerless POV narrating in the system's numbers is a knowledge-boundary violation regardless of density.
- **Word-arithmetic counts.** "Quota minus rejections equals what's left" is the same leak without numerals — defensible only when it is a character's own survival math (positional access) or the ledger's language shown *in order to refuse it* (rev5: "Percentage of loss. Value after extraction." — the narrator recoiling). Flag every instance; keep only the earned ones.
- **The meter is a proxy.** A low number-count is not a win if the prose went *vague* to achieve it. Concrete sensation, not abstraction, is the target; the read confirms which you got.

---

## §6. THE MULTIMODEL DEVIATION PASS (seek divergence that sharpens)

Run the improved seed past ≥2 additional models. The purpose is **not** consensus.

- **Convergence that softens = LLM bias.** If the models agree on a smoother, safer, more balanced version, that is the mean; reject it. (Your standing rule: convergence on something defensive is noise.)
- **Divergence that sharpens = signal.** The value is the outlier reading — the one model that breaks the template in a productive direction, finds the seat the others missed, or names the flinch the others smoothed. Keep the deviation *only* after it passes §4's erasure test.
- **Guard against rubber-stamp** (the D9 lesson). A model asked "is this good?" will vouch. Ask instead, adversarially: "quote the strongest passage that makes this the same story every model would write, and refute it or concede." No quoted candidate → no valid pass. Supply the template beats (§3) and the counterfactual (§1) as externally-required adjudication targets the model may not substitute.
- **The operator holds the verdict.** Models surface candidate deviations; the human judges whether each *sharpens*. This judgment is the product's core value and is operator-held by necessity — it is the one discrimination LLM judges reliably fail.

---

## §7. THE BLIND ORIGINALITY READ (terminal gate)

The final gate, and the only one that cannot be delegated or cut.

**Run it blind to the audit trail.** The reader sees the seed and the improved version and *nothing about how much apparatus was spent producing the latter*. A story that survived UKE_STORY + three models *looks* considered, and a reader who knows that will read consideration as originality. Withhold the process, or the process vouches for itself — the conversation's own disease (apparatus counterfeiting a witness) reappearing as process-sophistication counterfeiting originality.

**The one question no protocol answers:**
> Is this saying something a competent model wouldn't, or is it a well-audited version of what every model would write?

This is Rift vs Assessment. Everything upstream feeds it; nothing upstream settles it. If the answer is "well-audited sameness," the verdict is HUMAN-REVIEW regardless of every green gate above — a thoroughly-audited seven is still a seven, and the apparatus's job is to *not* hide that.

**Pass condition:** the improved story does something the seed did not — a broken template beat, a second coherent seat, a cost the seed flinched from, an invariant the seed only gestured at — and the doing survives the erasure test. Absent that, the pass is polish, and polish is not the deliverable.

---

## §8. SELF-APPLICATION

Apply the protocol to itself. Its own additions must survive its own erasure test: if deleting a section leaves the discipline just as sharp, that section was decoration — cut it (this is why v0.1 has eleven sections and should probably ship with fewer). Its own central risk is F-PROCESS-VOUCH turned inward: UKE_STORY must not become the machine that certifies its outputs original because they were heavily processed. The blind read (§7) is the guard, and the blind read must stay blind even to *this protocol's* manifest.

---

## §9. QUALITY GATES

```
☐ Seed triaged A/B/C in one sentence; resolution strategy matches type
☐ Type-B seeds re-founded or rejected — never polished
☐ Invariant survives the counterfactual test (would-read-identical-if-hidden → fail)
☐ Correctable-bias subplot, if present, is subordinated on-screen, not the resolution
☐ Ending: cost lands before any consolation; consolation earned or cut
☐ Seat test: ≥1 non-protagonist seat carries independent, unresolved truth
☐ Every kept change survived the erasure test and carries its before→after witness
☐ No kept change is pure smoothing (regression to mean)
☐ Register: numbers only on positioned in-scene action; word-arithmetic earned or cut; no vagueness-for-density
☐ Multimodel pass kept divergence-that-sharpens, rejected convergence-that-softens
☐ Blind originality read run without the audit trail; Rift-vs-Assessment answered honestly
☐ Manifest complete; Ω logged
```

---

## §10. OUTPUT FORMAT

```
IMPROVED STORY
[full text — a complete readable story, not a diff]

SHARPENING MANIFEST
seed_triage: [A / B / C + one-sentence diagnosis]
changes:
  - change: [what changed]
    type: [A/B/C]
    witness: [before → after, the specific lines]
    erasure_survived: [yes — story weaker without it / rejected — decoration]
    sharpens_by: [template-break / invariant-deepen / cost-land / POV-split]
rejected_changes:
  - [change] → [rejected: smoothing / survived erasure as decoration]
invariant_audit: [counterfactual test result; two-reals subordination check]
seat_test: [which seats carry independent truth]
multimodel: [deviations kept, with why-they-sharpen; convergence rejected]
blind_read: [PASS: does X the seed did not / HUMAN-REVIEW: well-audited sameness]

Ω OPEN QUESTIONS
[Ω_E empirical / Ω_C conceptual / Ω_P preference]
```

---

## §11. ANTI-PATTERNS

**F-POLISH.** A change that smooths toward the competent mean and survives its own deletion. *Fix: erasure test; keep only load-bearing changes.*

**F-CORRECTABLE-REAL.** The invariant is a knowable value the system mismeasured (Assessment-class). *Fix: counterfactual test; re-found as in-principle-unreadable or reject the seed.*

**F-CONSOLATION.** The ending converts unredeemed cost into meaning, hope, or knowledge-transmission before the cost has landed. *Fix: rupture audit; land the blow first, earn or cut the consolation.*

**F-TEMPLATE / LOCAL-COHERENCE.** The laborer-analyst-beneficiary triad with meaning in one seat only. *Fix: seat test; make ≥1 other seat carry independent truth.*

**F-MISROUTED-RESOLUTION.** Reframing a Type-B seed (making a correctable-bias story prettier) instead of re-founding it. Looks like honest improvement; accomplishes nothing. *Fix: match resolution to triage type.*

**F-REGISTER-LEAK.** Numeric register as texture; word-arithmetic; the meter's fingerprints in the prose. *Fix: number only on positioned in-scene action.*

**F-PROCESS-VOUCH.** The audit trail vouches for the story's originality; process-sophistication read as depth. *Fix: blind originality read (§7).*

**F-CONVERGENCE-COMFORT.** The multimodel pass keeps the smoother consensus version. *Fix: keep divergence-that-sharpens; reject convergence-that-softens.*

**F-CONCEPT-INFLATION.** A flourish added to signal literary depth. *Fix: Parfit test; if it survives translation to plain prose without loss, it was decorative.*

---

## Ω — OPEN QUESTIONS (v0.1)

**Ω_C1 — Improve-vs-reject boundary.** The protocol says re-found or reject Type-B seeds, but gives no rule for *when re-founding a seed is cheaper than re-seeding from the pipeline.* Likely: if the untranslatable real is absent (not merely weak), re-seed; if present-but-dominated, re-found. Unresolved; needs runs.

**Ω_E1 — Does uke_story actually repair rev6?** The whole architecture claim is that a Type-B seed can be repaired downstream. Untested. The first empirical run should be rev6 through UKE_STORY: does it re-found the invariant, or does it (F-MISROUTED) just prettify the correctable-bias story? This is the protocol's own positive control.

**Ω_E2 — Does the multimodel pass yield sharpening divergence, or just noise?** Convergence-rejection assumes the outliers sometimes sharpen. If, across runs, every model's deviation fails the erasure test, the multimodel stage is cost without signal and should be cut.

**Ω_P1 — Section count.** Eleven sections likely fails the protocol's own §8 self-application. Candidates for merge/cut once run: §5 into a checklist item; §2 and §3 are the load-bearing pair; §7 is non-negotiable. Trim after first use, not before.

**Ω_C2 — Where the operator read is required vs optional.** §6 and §7 both put the verdict in the operator's hands. Sustainable at low volume; a bottleneck at scale. Whether any part of the sharpness judgment can be reliably delegated is the same question that gated the whole pipeline, and the same answer is likely: no.

---

*v0.1 draft. Not yet run against a seed. First action: run rev6 through it (Ω_E1) — the protocol's positive control is whether it repairs the one seed we know is broken, or merely polishes it. CC0.*
