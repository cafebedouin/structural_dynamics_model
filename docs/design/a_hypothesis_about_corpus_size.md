# A Hypothesis About Corpus Size — Tested, and What Survived

**A record of one line of reasoning: the intuition that the corpus stabilizes as it grows,
the measurements that tested it, and the split verdict that came back. Written so the
reasoning carries the weight the evidence licenses and no more — the strong form did not
survive, the weak form did, and the one genuinely structural fact is narrower than either.**

**Status:** Companion finding to `design_discipline.md`. The numbers here are as-reported by
the re-runnable scripts named in §8; **those scripts are the witnesses, this prose is the
summary**, and where the two disagree the scripts win. Re-run on corpus growth; the verdict's
*shape* is expected to hold, the absolute constants are generator-specific and will move.

---

## 0. The verdict, stated first and at its true width

The corpus's structural vocabulary is **bounded** — it does not discover new structure without
limit as it grows — and this is witnessed. But the boundedness is achieved **substantially
through dimensional redundancy and an entropy-inflated combinatorial cap, not through a rich
multi-way structural forbidding of most of the possibility space.** The intuitively striking
reading — "99.93% of the combinatorial space is structurally forbidden" — is *not* witnessed;
that figure is largely what mild dimensional coupling produces mechanically, and citing it as
evidence of structure is circular. The claim that *does* survive at full strength is narrower
and more interesting: the **coupling pattern between structural dimensions is
generator-invariant** — it is a property of the engine's predicates, not of who authored the
corpus.

So: bounded, yes. Closed-by-rich-structure, no. Closed-substantially-by-redundancy, yes. And
the part that is a fact about the engine rather than about the authoring is the coupling
pattern, not the occupancy fraction.

---

## 1. The question, in two forms that must not be conflated

The intuition under test: *thoughts may be infinite, but categories of thought are not* — like
a library classification (Dewey, Colon), the scheme stabilizes. This has a weak form and a
strong form, and conflating them is the first error to avoid.

**The weak form** — "a finite classification over a bounded feature set saturates" — is true
and nearly empty. Any such scheme saturates; five of the six fingerprint dimensions are bounded
small sets by construction (see §3). Worse, "classification systems don't go on forever" is
partly survivorship: the schemes we can name are the ones a committee *closed by fiat*. Dewey
is finite because Melvil Dewey drew a boundary, not necessarily because the world of books is
finite. The weak form tells us almost nothing about *this* corpus.

**The strong form** — the one the intuition actually means — is that the corpus has an
*endogenous* stabilization: a point past which it stops discovering structure **on its own,
without anyone fixing the schedule, because the structural territory it maps is itself
bounded.** This is a claim about the territory, not the map. It is the form worth testing, and
the form most of the early measurements could not actually address.

---

## 2. The instrument problem — why several confident measurements were aimed wrong

The reasoning toward this verdict failed twice before it succeeded, and both failures share one
shape: **a clean, well-fit measurement aimed at the generator's behavior while appearing to
measure the territory.** Recorded because the failure is instructive, not as a should-have.

- *First false start (lexical).* The dangling-edge fraction is flat across scale (~0.63–0.74
  over a 15× size range, no downward trend) and the in-degree distribution shows no hub
  formation (95–98% singleton targets at both scales). This was read as "open vocabulary, flat
  forever." It is true — about *strings*. Each story mints fresh descriptive target names, so
  the string namespace grows with the corpus and never closes. But string-space was the wrong
  object: a dangling string is a surface pointer, and the question is whether the *class* it
  points to is bounded.

- *Second false start (rate-vs-n).* Every "knee" computed against corpus size — 34,000, then
  ~2k–20k — is a property of **how fast this generator coins novelty as it writes**, not of the
  space being filled. The tell was in the data the whole time: cross-generator diversity is
  non-monotone (197→66, 772→161, 1150→300, 3380→199), and v3's generator hit 300 classes at
  1,150 stories while v5's plateaus near 200 at 3,380. A quantity that depends that strongly on
  *which* generator wrote the corpus cannot deliver an endogenous threshold. The knee measured
  drawing speed, not the size of the country.

The lesson generalizes and is the methodological core of this document: **rate-against-stories
cannot distinguish a bounded territory from a fast cartographer.** Only a measurement aimed at
the territory directly can, and there are exactly two such measurements available.

---

## 3. The two territory-witnesses for *bounded*

These are the measurements that do not depend on the generator's emission rate. They carry the
bounded verdict; the occupancy fraction (§4) does not.

**Witness A — shift against its enumerable hard cap (least inferential).** The `shift`
dimension is the constraint's type-response across a *fixed* 4-point observer grid
(`fingerprint_shift` runs `classify_at_power` at four hard-coded contexts). Because the grid is
fixed, shift has a true combinatorial ceiling: #types⁴ = 8⁴ = **4,096** (8 cascade type atoms, witnessed in `drl_core.pl`), and it can be
*counted*, not estimated. Realized: **59** — about **1.4% of its own enumerable cap.** A space
with a known ceiling that closes at ~1.4% of it cannot be explained by "authoring stopped":
the other 99% is provably unvisited, and there is no survivorship dodge when the cap is
enumerable. This is the single most trustworthy number here because the denominator is counted.

**Witness B — discovery rate against S, not against n (the discriminator).** The coupon-collector
form ΔS/Δn = r·(1 − S/S_max) distinguishes the two models that rate-vs-n cannot: an open
log-space has rate ∝ e^(−S) (convex, no finite intercept); a bounded attractor has rate ∝
(S_max − S) (linear, finite intercept). Both spaces return the **linear / finite-intercept**
form — structural 5-dim S_max ≈ 109 (R² = 0.88), shift S_max ≈ 58 (R² = 0.82), both
negative-slope. Discovery bends toward a bounded attractor.

**Caveat carried with Witness B, not deferred below it:** every S_max is generator-specific, and
the estimator is fragile at exactly the precision the verdict leans on — **the current generator
already realized 116 structural classes at 772 stories, exceeding v5's estimated S_max of 109.**
So "S_max ≈ 109" is an estimate one generator has already overrun, not a constant. The *finite
intercept* (the shape) is the robust result; the *value* is not.

---

## 4. Why the strong form died — and the demotion of 0.07%

The occupancy fraction is the number that wants to be the headline, and it is the number to
distrust. At 772 stories, 116 of a 155,848-cell marginal product is realized (0.074%); at v5,
99 of 149,688 (0.066%) — strikingly small, strikingly constant across generators. The temptation
is to read this as "99.93% structurally forbidden." **That reading is circular:** 0.07% of a
*product of marginals* is what you get for free if the marginals are merely correlated, and the
small fraction is therefore what coupling *produces* — it cannot also be the evidence *for* rich
coupling. It is equally consistent with a few dimensions being near-functions of the others
(fingerprint redundancy), which is a weaker and different finding.

Mutual information between the five marginals settles which it is, and it comes back on the
weaker side:

- **Redundant fraction (total correlation / ΣH): 44.8% (current), 55.7% (v5).** About half the
  dimensional information is shared, not independent.
- **Strongest couplings are generator-invariant:** voids↔zone ≈ 0.83, props↔actors ≈ 0.78, in
  *both* corpora.
- The nominal ~150k cap is mostly entropy-inflated tail; the effective independent cap is far
  smaller, and redundancy collapses it further.

So the boundedness is achieved **substantially through redundancy and an inflated cap, not
through rich multi-way forbidding.** The 0.07% is therefore **demoted from proof to
illustration** — it illustrates that the realized set is small; it does not witness *why*, and
it overstates the richness of the structure.

**A number deliberately held as provisional:** the "effective joint ≈ 6–16 cells" decomposition
rests on an entropy estimate from ~99–116 realized points — a sample size comparable to the
support being estimated, the regime where entropy is systematically *under*estimated and
effective-cell counts correspondingly understated. It is recorded as **order-of-magnitude only,
small-sample-biased-low**, and nothing in the verdict rests on it. The two witnesses in §3 carry
"bounded" without reference to it.

---

## 5. The one fact that is about the engine, not the authoring

Stripped of the over-claims, the result that survives at full strength is this: **the coupling
pattern between structural dimensions is generator-invariant.** voids↔zone ≈ 0.83 and
props↔actors ≈ 0.78 hold across two different generators at a 4.4× size gap. The redundancy that
makes the space small lives in the engine's predicates — in how the fingerprint dimensions are
computed and how they constrain one another — not in any one authoring process.

This is the right load-bearing sentence because it separates *what* from *why-it-is-a-fact*:
boundedness is what was found; **generator-invariant coupling is why it is a property of the
engine rather than of who wrote the corpus.** It is the sentence this document exists to
preserve, and the one most likely to be lost under the louder boundedness story if it is not
stated plainly.

The corresponding model is **punctuated equilibrium over a shared structured subspace:** each
generator saturates to its own small bounded region (its own S_max), all of those regions lying
inside the same generator-invariant coupling structure; a more expressive generator fills a
somewhat different or larger sub-region but cannot escape the coupling that constrains the rest.
Generator identity is thus a *state variable* of the corpus's temporal process, not a confound
to wash out.

---

## 6. What this settles downstream, and what it explicitly does not

**Settled — the OQ-58 structural resolver is well-founded.** The target space into which the
~1,710 dangling `affects_constraint` references would resolve is a bounded ~10²-class attractor,
not an open frontier. Resolving a dangling surface string to its existing structural class is
therefore sound in principle, not a guess into an unbounded space. (Gated, per the OQ-58
disposition policy, behind repairing the 9 delimiter typos first and the five-vs-six-dimension
keying decision — neither settled here.)

**Settled — temporal drift is well-posed.** Past per-generator saturation (~a few thousand
stories for the generators measured), the operative signal stops being "what new structure
appears" and becomes "how the distribution over a fixed, small, bounded class set moves over
time" — which is what `fingerprint_drift` / the drift-velocity machinery is built to measure.

**NOT settled — the drift is *not* claimed to be Markov.** A bounded state set gives "drift over
a fixed set"; it does not give *memorylessness*. Whether the next state depends only on the
current one is an open measurement — and the engine was built with velocity *and acceleration*
terms precisely because the process may carry memory (acceleration is a second-order term a
first-order Markov process does not have). The honest status is: **bounded state set; drift-order
(memory) is an open measurement pending the velocity/acceleration read.** Do not let "Markov on
~10² states" enter as a consequence; it is an untested modeling choice.

---

## 7. Caveats, consolidated

- **Numbers are as-reported by the scripts (§8), not independently re-derived in this prose.**
  The scripts are the witnesses; re-run them rather than citing this document as the source.
- **The verdict's shape is robust; its constants are generator-specific.** Bounded (shape) and
  the generator-invariant coupling (mechanism) are the durable claims. Every S_max, every knee,
  every absolute class count is per-generator and will move with the generator.
- **The MI mechanism rests on a sample (~99–116 points) small relative to the spaces estimated.**
  The redundant-fraction direction (≈half shared) is robust to this; the fine decomposition
  (effective-cell count) is not, and is flagged provisional in §4.
- **Two false starts are recorded in §2 as method, not as error to relitigate** — they locate
  *why* rate-vs-n and lexical measurements cannot answer the strong-form question, which is the
  reusable lesson.

---

## 8. The witnesses (re-runnable)

The following scripts produced every number above and are the canonical source:

- `python/dangle_curve.py` — dangle-fraction vs corpus size (the flat lexical curve, §2).
- `python/dangle_indegree.py` — in-degree / hub-formation test (open-vocabulary fingerprint, §2).
- `python/fingerprint_rarefaction.py` — within-generator accumulation curves.
- `python/separate_spaces.py` — disaggregation of structural vs context-response (shift) spaces.
- `python/territory_test.py` — occupancy of the combinatorial cap + coupon-collector estimator
  (Witnesses A and B, §3–§4); generator-independent, re-runnable on any dump.
- `python/mi_structure.py` — mutual information between marginals (the mechanism, §4–§5).

Cross-references: `design_discipline.md` (§5 classification-as-routing, §7 the ε caveat on
generation-conditional statistics); `GAP-07` in `design_gaps.md` (the declared-absence framing
of the network terminus this finding sits beside); `OQ-58` / `OQ-59` in `ISSUES.md` (the
resolver and the semantic-aliasing question this finding bears on).

---

## 9. Speculative tail — beyond the evidence, fenced as such

*Everything in this section is conjecture that takes §0–§6 as its floor. It is not witnessed and
must not be cited as finding. It is recorded so the line of reasoning is complete, not so it is
believed.*

If generator identity is a state variable (§5), the corpus's long-run behavior has **three
timescales**, only the first of which is measured: within-regime drift (continuous, dominant past
per-generator saturation); **generator punctuations** (a more expressive model re-opens
structural discovery — conjectured from the non-monotone cross-generator diversity, not from a
controlled comparison); and **engine-config punctuations** (changing `standard_context_for_power/2`
— adding observer contexts or retuning canonical parameters — would discontinuously raise shift's
4,096 cap and re-open reading-axis discovery). Under this picture, "does the corpus stabilize at
scale?" has the speculative answer: *within a fixed generator and engine config, yes, at tens of
thousands of stories — ~4–5 orders of magnitude below a billion; across generator and config
changes, never, because each punctuation re-opens discovery.* The Dewey analogy then resolves
cleanly: the corpus is Dewey-like (bounded by structure) **within a regime**, and
Colon-like-only-because-someone-stopped **only if** generators and config are frozen by fiat. The
evidence supports the within-regime boundedness (§3); the punctuation structure is conjecture
awaiting a controlled generator-to-generator comparison that holds story count fixed.
