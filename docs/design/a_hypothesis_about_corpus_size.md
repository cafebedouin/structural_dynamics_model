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

## 10. The depth-lineage probe (2026-06-04) — §0's verdict stressed, and what broke

*Added after the OQ-71 experiment. The scripts and artifacts in
`audits/2026-06-04_oq71_depth_lineage/` are the witnesses; this prose is the summary. Full
design, pre-registration, gates, and rulings: ISSUES.md OQ-71.*

**The verdict, stated first and at its true width — the finding is a disjunction, and citing
either branch alone overstates it.** A 438-story arm differing from the breadth control in two
bundled ways — kernel-nesting depth AND seed-authorship — minted distinct structural classes at
~1.5× the control at every matched n, beyond resampling noise. **Either** nesting depth re-opens
structural discovery, **or** a different seed-author constitutes a new generator regime (§9's
punctuation, observed semi-controlled). This run cannot tell them apart; the
authorship-controlled breadth arm in OQ-71 can. What is settled regardless of branch:
*unconditional* boundedness and coupling-invariance are falsified — §0 survives only as
within-regime. And the single strongest item is not the 1.5× (which carries the confound) but
the **pre-registered shape-match**: novelty rises, turns down at the deepest bands, and the arm
saturates toward its own S_max — the punctuated-equilibrium shape §9 conjectured *before* this
data existed, met at a non-obvious shape no branch of the disjunction disturbs.

**The test.** A designed kernel-nesting lineage (social_coordination ⊃ government ⊃
constitutional_government ⊃ ten constitutions ⊃ instruments ⊃ clauses ⊃ doctrines; 449 seeds,
438 generated, 10 levels deep, fan 2–5 throughout) against a 300-story breadth control from the
same frozen generator (Haiku 4.5, identical prompt/schema/example — the 2026-06-03
never-generated batch). Depth-correlated authoring is the hardest test §0's boundedness can
face: breadth samples the territory loosely; a lineage drills one region. Pre-registered:
H1 excess-over-control ≤ 0 within sibling-length strata; H2 within-band saturation with
persisting ε-spread; H3 coupling invariance. The fingerprint probe was validated by **exact
multiset reproduction of this document's own v5 dump** (3,380/3,380 lines) before any depth
number was read.

**What fired.**
- **H1 falsified, beyond noise at every matched n.** Distinct 5-dim structural classes at
  matched n in the powered (length-2+) stratum: depth 71.3/88.0/103.4 vs control
  47.8/58.7/68.7 at n=150/200/250 — 95% resample bands non-overlapping at all three; discovery
  slope also higher (0.271 vs 0.192 at n=294). *(Color, not evidence: the depth arm realized 156
  distinct classes in 438 stories vs the 806-story baseline's 118 — a non-matched-n, cross-regime
  comparison; demoted 2026-06-20. The matched-n resample bands above are the witness.)*
- **Not list-inflation — closed across all 5 dims (A2, 2026-06-20).** The original claim rested on
  2 of 5 dims (mean |props| 3.83 vs 3.77, |voids| 1.98 vs 2.25). Re-run over all five at matched
  n=294, K=2000: the JOINT distinct-class excess is **+38.7**, while every single dim's MARGINAL
  distinct-value excess is tiny — props −2.0, voids −1.7, actors −2.3 (depth uses *fewer* values
  in three dims), drift +2.7, zone +2.8 (largest = +2.8). Joint excess is ~14× any one dim's
  marginal: the excess is new *combinations*, not cardinality proliferation in any dim
  (positive-controlled — an inflated-props synthetic arm was flagged). Witness:
  `audits/2026-06-04_oq71_depth_lineage/a2_richness_alldims_results.json`.
- **H3 falsified — the coupling pattern reshaped, not dissolved.** Matched-n=300 plug-in MI:
  props↔actors 0.48±0.03 vs live 0.71±0.07 (weaker, ~5σ); voids↔zone 1.31±0.03 vs 1.05±0.05
  (stronger). §5's "generator-invariant coupling" is not depth-invariant.
- **H2 mixed.** Within-band novelty at matched n=100: L5 47.9 → L6 57.9 → L7+ 51.0 — no early
  saturation, but the deepest band turns down (new-vs-shallower falls to 5 then 3 at L8/L9),
  consistent with the arm saturating toward its *own* S_max. ε-spread persists at every band
  (sd 0.13–0.18, full range throughout).

**What it means for §0 — read this with §9's frame, not against it.** The probe bundled two
changes by design (named threat, sibling-shape and richness controlled; authorship not):
kernel-nesting *depth* and seed *authorship* (lineage commitments hand-designed by a different
model than the control's SCOPE-derived seeds). These are confounded in this run, and they are
exactly the two arms of §9's conjecture: either depth re-opens the territory, or a new
seed-author is a new generator regime and this is a **generator punctuation observed
semi-controlled** — the first evidence §9 has. Either way, **§0's boundedness survives only as
within-regime**: the realized class set is not closed against a deliberately different way of
writing into the same engine. The one §0 claim that needed revision either way: coupling
invariance, which held across two *breadth* generators, does not hold across this regime
change.

### 10.1 Phase A resolution (2026-06-20) — the disjunction is a trichotomy, and the breadth arm cannot isolate depth

The disjunction above is really a **trichotomy**: the 1.5× excess could be (1) **depth-realization**
(the nesting *reaching the generator*), (2) **author-identity** (Opus vs the control's Haiku/SCOPE
seeds — a §9 punctuation), or (3) **lineage-structured authoring** (designing seeds as a tree
enriches them whether or not the nesting is realized). A zero-spend read-only pass (the feasibility
gate for any breadth arm) settled which of these the instrument can even test.

**A0 — the nesting never reaches the generator, by design (witnessed).** `build_lineage_seeds.py`
forks two structures: the generation `seeds` (→ `lineage_seeds.json`, fed to the model) carry
`constraint_id, kernel_id, reading_id, human_readable, topic_domain, family_id,
sibling_reading_ids, expected_structural_delta, summary` and **no `parent_kernel`/`level`**; the
parent pointer and level live only in a separate `lineage.json` sidecar consumed post-hoc by the
fingerprint join (builder lines 114–134). The generator prompt
(`generate_kernel_corpus.py:430–486`) reads only the flat seed fields — grep confirms it never
touches `parent_kernel` or `lineage.json` (comment at `:104`: "kernel lineage is carried
separately"). This was the **deliberate design, stated not inferred** — the origin plan
`virtual-inventing-allen.md` lists *"Untouched by design: generation prompt/schema/example,
GEN_MODEL"* and *"Generator held fixed… Only seed authoring and output routing differ from
control,"* framing the whole manipulation as *"depth-correlated authoring."* The generation prompt
was a frozen non-variable by design, so nesting could only ever act authoring-side — **not a wiring
defect**, so the OQ closes *mitigated*, not *inconclusive-by-construction*. Consequence:
the proposed breadth arm's reading-(a) ("design the tree, null `parent_kernel`, regenerate") is a
**provable no-op** — nulling a field the generation seed never references cannot change one byte of
generator input, so `depth − breadth ≈ 0` by construction. **Branch 1 (depth realized at the
generator) was never instantiated in the experiment;** no seed manipulation in this instrument can
isolate it.

**Why no_scope is blind to nesting — the two-path architecture, and what the control shares with
it (witnessed).** The deeper reason A0 holds is the **batch pipeline by design**, not where a field
sits. The engine has two generation paths: the **SCOPE path** (`generate_kernel_corpus._scope_user_prompt`
/ orchestrator `_step_decompose`) hands the *model* a raw topic and lets it **construct the kernel**
— decide `is_contested_kernel`, emit the `readings` array; the **no_scope path** renders
*pre-decomposed* readings handed in as flat seeds. Batch generation (the only economical way to
make ~600 stories) forces **decompose-first**: the model cannot SCOPE-construct kernels inline
across a batch, so the kernel decomposition is resolved *before* the batch and the per-reading
prompt is necessarily blind to it. "The generator can't see/construct nesting" is therefore a
structural property of the **no_scope/batch path**, and it applies identically to any breadth arm
(which must also batch) — A0 is robust, not incidental. Crucially, the **control's** kernel/reading
structure was itself *model-SCOPE-constructed* then harvested: `build_never_generated_seeds.py`
pulls `is_contested_kernel` SCOPE manifests and emits their never-generated readings as flat
no_scope seeds. So the depth-vs-control contrast is, at the structure level, **Opus-hand-designed
nested tree vs the SCOPE model's flat decompositions** — both rendered identically at generation.
This *sharpens* "author-identity" (branch 2): it is **who constructed the kernel structure** (Opus
tree-design vs the SCOPE model), not merely who wrote the prose. (A SCOPE-path arm — let the model
construct kernels from the same deep constitutional *topics* — is a different cut that varies the
structure-author while holding the topic, but it conflates topic-domain richness with nesting
depth, so it is not a cleaner substitute for the registered reading-(b) below.)

**Sibling co-channel (witnessed narrowing — and the sign cuts toward mitigated).** One tree-derived
field *does* reach the prompt — `sibling_reading_ids` — and its set-size covaries moderately with
tree level (Pearson **r=−0.366**; per-level means L0≈3.0, L1≈4.0, falling to L7–L9≈2.0). So the
claim is *not* "the generator never saw depth"; it is **"the generator never saw parent-nesting"**
— it saw sibling-set *size* as a weak depth co-channel. **The negative sign disposes the hazard
toward mitigated:** deeper nodes carry *fewer* siblings, so the channel transmits *less*
contest-pressure at depth, not more — it cuts *against* sibling-mediated tree-position driving the
excess. Two scoped bounds beyond the sign: (i) the sibling-*size* effect on novelty is
witnessed-flat for lengths 2–4 by the pre-existing control-stratification (only length-1 slopes) —
that bounds *size*, not *composition*; (ii) reading-(a)'s pointer-strip leaves the sibling block
unchanged regardless. Sibling *composition* is not separately controlled — a residual the sign
argument dispositions, not a control; naming it rather than burying it under "bounded."

**The close, at its true width.** The 1.5× excess is **not attributable to generator-visible
parent-nesting**; it is the **authorship-bundle — Opus identity and/or lineage-structured
authoring, undistinguished** (with sibling-size a weak, bounded co-channel). The trichotomy is
irreducible by the breadth-arm instrument as designed. *unconditional* boundedness and
coupling-invariance remain falsified (within-regime survival of §0 unchanged); this probe still
must **not** be cited as proving depth-specific discovery.

**Construct-validity width — what OQ-71 falsified is NOT what §3 claims (one inferential step).**
§3's bounded-attractor claim is about the **SCOPE construction path** — does the model, growing the
corpus the way the corpus is actually grown (SCOPE constructs each kernel from a topic), exhaust the
structural-class vocabulary (the generator-specific S_max). OQ-71 stressed boundedness with a regime
the SCOPE path never enters: **Opus-hand-designed structure rendered through no_scope.** So the
falsifier that fired falsified **boundedness of the rendering *substrate* under externally-authored
structure** (the substrate can express more classes than the live corpus holds) — it did **not**
test boundedness of the **SCOPE construction path itself**, which is §3's actual claim. The whole
excess is consistent with "SCOPE is still a bounded attractor, and Opus-hand-design simply reaches
richer structures than SCOPE-construction does." So cite the kill at substrate width, not path
width: **§3's claim is left standing as within-regime** (§10's existing verdict), and OQ-71 does not
re-engage it. Re-engaging §3 on its own turf is OQ-171 (the successor), which also inherits the A0
obstruction — depth/accumulated-context is not a clean input the SCOPE path exposes without dragging
a topic-domain or batch-size confound, so the construct-valid experiment may not be cleanly
constructible. See OQ-171 for the context-controlled batch-of-one design.

**Graduation step (what would move mitigated → resolved, deferred — spend declined 2026-06-20).**
Under the design's own operationalization, "kernel-nesting depth" = depth-correlated *authoring*
(branch 3). The origin plan's reading-(b) — **Opus authors ~300 *flat* (non-tree) seeds, same
frozen generator** — holds author-identity constant and removes lineage-structure, cleanly
splitting branch 2 from branch 3 (the only live question once branch 1 is out of scope). It still
requires generating new stories (API spend), and the spend was declined at this juncture. A future
instance may revive it; it is the registered discriminator, not a new OQ. **Drift cross-link:
whoever runs it must key the control on `control_membership.json` (the 300 frozen ids), NOT the
regenerated `never_generated_seeds.json` (drifted to 274 by 2026-06-13) — else the breadth arm
compares against a 274-story control while believing it is 300.**

## 11. First numbers on §9's timescales (2026-08-21) — and the frontier as the operator now reads it

*Added after the within-model legs of 2026-08-21. Witnesses:
`audits/2026-08-21_flash_regime_vs_redraw/` (script + raw output); KNOWN_STATE 2026-08-21
(the four `testsets_flash*` legs). This prose is the summary; the script wins where they differ.*

**What §9 conjectured now has one measured instance per timescale, on one model
(`gemini-2.5-flash`) over one frozen seed pool (1005 readings), all classified at one engine
state.** Same-seed agreement between two legs, on the engine's own outputs:

| §9 timescale | instantiated as | h1_band | verdict | signature | authored ε |
|---|---|---|---|---|---|
| within-regime drift (the floor) | two thinking-off draws, same prompt/schema commit | 85% | 89% | 93% | 81% |
| regime punctuation | thinking-off draw vs thinking-on draw (budget 8192) | 59% | 65% | 72% | 34% |
| — the punctuated regime's own floor | two thinking-on draws | 64% | 71% | 77% | 40% |
| engine/prompt-config punctuation | thinking-off draw at schema `2e9dff2f` (June) vs `685ed7cf` (Aug) | 72–74% | 79–80% | 87% | 55% |

Three things this licenses, at their width:

- **The regime punctuation is mostly a variance event, not a displacement.** The off-vs-on
  contrast (59%) sits only a few points below the thinking-on regime's *own* redraw floor (64%):
  thinking-on draws disagree with each other nearly as much as with a thinking-off draw. The one
  marginal shift that replicates in both thinking-on draws is the red-verdict rate (6.5–6.8% →
  13.4–14.3%). §9's "punctuation re-opens discovery" should be read, at least for this kind of
  punctuation, as *re-opens dispersion over the same class set* — consistent with §6's "drift over
  a fixed, small set," not with new territory. Whether a *generator* punctuation (a different model
  family) behaves the same way is the next measurement; the stealth and nemotron legs in flight
  are that arm, and OQ-71's 1.5× class excess says the answer may differ.
- **The config punctuation is real and smaller than the regime one** (72–74% vs 85% on h1), and it
  is confounded with possible model-snapshot drift over two months — so it is an upper bound on
  "schema change alone."
- **"Track concepts over time via UUIDs" needs one correction from the rulings that postdate the
  original conversation.** Per-story UUIDs are surrogate identity only (CLAUDE.md, *Generation is
  stochastic*; GAP-35): cross-run "same story" identity does not exist, and matched-seed structure
  is a generation-time decision. The trackable unit is therefore **seed × generator × regime**, and
  "divergence over time" is §6's well-posed drift: the distribution over the bounded class set
  moving per seed as the state variables change. That is precisely what the table above measures,
  and why the redraw floor had to come first — a cross-leg delta is uninterpretable until the
  within-regime floor is known.

**Speculative tail, fenced as §9 is (operator, 2026-08-21).** Even confined to what the engine
does now, the interesting program is not scale but *depth*: generate topical constraint stories in
groups — the kernel/reading families, the designed lineages of OQ-71 — and then turn the engine's
own meta-tools on the groups rather than on stories (fixed-point networks, orbits and H¹ over
families, the Boltzmann/MaxEnt fits, trajectory clustering). On this reading the product of a
mapped conceptual space is not the map but what the meta-tools say about its *regions*: which
families are draw-stable (situation-fixed) across generators and regimes, which are
seat-expressive, and where the class-set boundedness of §3 holds or breaks under depth. OQ-71 is
the first probe of exactly that, and its finding — joint-combination excess, not per-dimension
proliferation — is the shape to expect. Not witnessed beyond OQ-71 and the table above; recorded so
the line of reasoning is complete, not so it is believed.
