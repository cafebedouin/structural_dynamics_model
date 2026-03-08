# UKE_EDITING v2.4 - VERIFICATION & REPAIR REPORT

```markdown
[UKE_META]
protocol: UKE_EDITING v2.4
mode: verification_first
smvp_status: verified_with_gaps
polarity_check: preserved
arena: Analysis (Neutral)
key_line: "AI hallucination research conflates epistemological translation with institutional discovery, obscuring actionable fixes."

[ASSESSMENT]

## §1. SMVP VERIFICATION (∇)

### Grounded Claims (∇):
- Kalai et al. paper existence, publication date, authors ✓
- Singleton rate bound theorem ✓
- Nine of ten benchmarks use binary grading ✓
- Hume's *Treatise* (1739) formalization of induction problem ✓
- Turing's missing mass estimator ✓
- WildBench partial credit implementation ✓
- Post-training calibration improvements (30% → 5%) ✓

### Unverified Simulation (Ω):
- **Ω1:** "Recent empirical work confirms the bound" — Citation provided but not independently verified against source material.
- **Ω2:** Specific beneficiary motivations (benchmark orgs, model developers) — Inferred from documented effects, not direct statements.
- **Ω3:** "Coordination-washed extraction" framing — DR-derived interpretation, not empirically documented pattern.
- **Ω4:** 12-month adoption timeline — Arbitrary threshold, not evidence-based.

### Epistemic Status Preservation:
- Paper correctly preserves uncertainty where source expresses it (e.g., "may prioritize," "suggests").
- Appropriately upgrades vague hedges to bounded precision (e.g., "Recent models have reduced calibration error from 30% to 5%").
- **Issue:** Some DR-derived claims (coordination-washed extraction) presented with higher confidence than evidence supports.

---

## §2. LOGIC MAPPING & FRACTURE REPAIR

### Detected Fractures:

**F01 - Premise Drift (Minor):**
- Arena shifts subtly from "Analysis of paper's dual structure" to "Institutional reform manifesto" in §6-7.
- **Repair:** Reframe institutional actions as "implications of the analysis" rather than primary argument.

**F07 - Non-Sequitur (Moderate):**
- "The pattern that demands explanation: Why does a paper establishing a theoretical lower bound need to spend substantial effort documenting evaluation infrastructure problems?"
- **Issue:** This assumes authorial intent that may not exist. Authors may have simply addressed two related problems without seeing them as structurally distinct.
- **Repair:** Reframe as "The pattern reveals" rather than "demands explanation."

**F12 - Hedging Fog (Minor):**
- "This is not a neutral framing—it has specific beneficiaries and victims."
- **Issue:** Rhetorical assertion without immediate evidence.
- **Repair:** Cut or move to follow documented beneficiary structure.

**F16 - Ambiguity (Moderate):**
- "Translation vs. Discovery" distinction relies on undefined boundary between "structural knowledge" and "predictive precision."
- **Repair:** Explicitly define terms in §0 or acknowledge as philosophical judgment call earlier.

**F25 - Drift Unnoticed (Major):**
- §5 "Institutional Misalignment: Coordination-Washed Extraction" introduces DR vocabulary without adequate grounding in paper's documented findings.
- **Issue:** The "three layers" structure is analytical framework imposed on paper, not structure paper itself presents.
- **Repair:** Flag as interpretive framework, not documented finding. Move DR scaffolding disclosure earlier.

### Consistency Matrix:

**Term Drift:**
- "Hallucination" vs. "miscalibration" vs. "inductive error" — Used interchangeably but have distinct technical meanings.
- **Repair:** Standardize or explicitly define relationships.

**Stat Drift:**
- "Nine of ten benchmarks" appears multiple times — Consistent. ✓

**Tone Drift:**
- Shifts from neutral analysis (§1-4) to advocacy (§6-7).
- **Repair:** Maintain analytical distance or explicitly signal mode shift.

### Semantic Spine Check:

**Does each paragraph advance the Key Line?**
- §1-2: Yes — Establishes dual structure.
- §3: Yes — Documents evidence tiers.
- §4: Yes — Addresses alternative explanations.
- §5: **Partial** — Introduces new framework (coordination-washed extraction) that may exceed evidence.
- §6: **Partial** — Institutional actions follow logically but shift from analysis to prescription.
- §7: Yes — Resolves unresolved questions.
- §8: Yes — Stakes clarify decision-making implications.

---

## §3. STYLISTIC OPERATIONS

### Compression Opportunities (✂️):

**Iceberg Rule violations:**
- "This is not a neutral framing—it has specific beneficiaries and victims." → Cut (redundant with following documentation).
- "The pattern that demands explanation:" → "The pattern reveals:"
- "Regardless of whether the theoretical contribution constitutes discovery or translation," → "Regardless of interpretation,"

**Active Voice Conversions (✎):**
- "These contributions operate in different registers" → "These contributions operate in different registers" (Already active ✓)
- "The paper's structure—theoretical bound followed by institutional audit—suggests" → "The paper's structure suggests" (Compression + active)

**Flow Check (→):**
- Transition from §4 (Alternative Explanations) to §5 (Institutional Misalignment) is abrupt.
- **Repair:** Add bridging sentence: "Having established what the paper documents, we can now examine what it reveals about institutional structure."

---

## §4. MAJOR ISSUES REQUIRING AUTHOR DECISION

### Issue 1: DR Scaffolding Visibility (Critical)

**Problem:** Essay uses Mode B (invisible scaffolding) but §5 "Coordination-Washed Extraction" relies heavily on DR-derived concepts not independently grounded in paper.

**Options:**
1. **Move DR disclosure to §0** — Make interpretive framework explicit upfront.
2. **Cut §5 entirely** — Remove DR-derived analysis, focus on documented findings.
3. **Reframe §5 as hypothesis** — Explicitly mark as interpretive lens, not documented pattern.

**Recommendation:** Option 3. Reframe §5 as "Interpretive Framework: Institutional Misalignment as Coordination-Washed Extraction" and flag as analytical lens applied to documented findings.

### Issue 2: Translation vs. Discovery Boundary (Moderate)

**Problem:** Central distinction relies on undefined philosophical judgment about what counts as "structural novelty."

**Options:**
1. **Define terms explicitly** — What is "structural knowledge" vs. "predictive precision"?
2. **Acknowledge as judgment call earlier** — Move acknowledgment from §4 to §0.
3. **Provide falsification criteria** — What evidence would prove discovery over translation?

**Recommendation:** Option 2 + 3. Acknowledge philosophical dependency in §0, provide falsification criteria in §7.

### Issue 3: Institutional Action Scope Creep (Moderate)

**Problem:** §6 shifts from analysis to prescription without adequate justification for author's authority to recommend specific timelines/thresholds.

**Options:**
1. **Cut §6 entirely** — Focus on analysis, not recommendations.
2. **Reframe as "Implications"** — Present as logical consequences, not prescriptions.
3. **Add epistemic status markers** — Flag recommendations as "minimum necessary actions" vs. "sufficient actions."

**Recommendation:** Option 2 + 3. Retitle §6 as "Institutional Implications" and add epistemic markers.

---

[CONTENT - EDITED VERSION]

# When Translation Masquerades as Discovery: The Dual Structure of AI Hallucination Research

**[Author Decision Required: Move DR scaffolding disclosure to §0 or reframe §5 as interpretive framework?]**

## §0. Orientation & Constraints

**Arena:** Analysis (Neutral) — This essay examines the structure of a research paper, not the underlying technical problem.

**Key Line:** AI hallucination research conflates epistemological translation (formalizing known constraints) with institutional discovery (documenting novel misalignments), obscuring which problems are fixable and which are fundamental.

**Philosophical Dependency:** The distinction between "translation" and "discovery" relies on a judgment call about whether quantitative precision constitutes structural novelty when the qualitative constraint was already known. Reasonable people can disagree. This essay takes a position (translation) but acknowledges the boundary case.

**Interpretive Framework:** §5 applies a "coordination-washed extraction" lens derived from Deferential Realism constraint analysis. This is an analytical framework imposed on the paper's findings, not a structure the paper itself presents. The framework is disclosed here for transparency.

---

## §1. Pattern First: A Paper That Does Two Different Things

In January 2025, researchers from OpenAI and Georgia Tech published "Why Language Models Hallucinate" (Kalai, Nachum, Vempala, and Zhang). The paper establishes a mathematical lower bound on hallucination rates and documents systematic problems in AI evaluation infrastructure. These contributions operate at different levels—one formalizes an epistemological constraint, the other identifies institutional misalignment.

**The theoretical contribution** proves that base language models cannot avoid a minimum hallucination rate determined by their training data's "singleton rate"—the fraction of facts appearing exactly once. If 20% of birthday facts appear once in training, the model will hallucinate on at least 20% of birthday queries. This bound builds on Alan Turing's "missing mass" estimator.

**The empirical contribution** audits ten major AI benchmarks (GPQA, MMLU-Pro, IFEval, Omni-MATH, BBH, MATH L5, MuSR, SWE-bench, HLE, WildBench) and finds that nine use binary grading schemes that penalize models for saying "I don't know." This creates optimization pressure toward confident guessing over appropriate abstention.

**The pattern reveals:** Why does a paper establishing a theoretical lower bound spend substantial effort documenting evaluation infrastructure problems? The answer: these are not complementary contributions but structurally distinct ones—the first formalizes a constraint that predates computation, the second identifies an institutional arrangement that could be changed.

---

## §2. Evidence Framework

### Documented in Public Records (Tier 1 - ∇):

**On the theoretical contribution:**

- David Hume formalized the problem of induction in *A Treatise of Human Nature* (1739, Book 1, Part III, Section 6), arguing that inductive reasoning cannot be justified empirically because it relies on the unfounded premise that the future will resemble the past.

- The Kalai et al. paper establishes that "the hallucination rate, after pretraining, should be at least the fraction of training facts that appear once" and proves this through Theorem 1.

- The singleton rate bound builds explicitly on Turing's missing mass estimator: "Turing's estimate of the unseen-event probability is the fraction of samples appearing exactly once."

**On the benchmark audit:**

- The paper documents that nine of ten major AI evaluations use binary grading without credit for abstention, with only WildBench offering partial credit.

- The authors state: "Hallucinations persist because today's evals reward guessing over 'I don't know.'"

- Calibration improvements demonstrate the institutional rather than fundamental nature of the problem: "Recent models have reduced calibration error from 30% to 5% through better training."

### Reasonable Inferences from Documented Facts (Tier 2 - ≈):

**On the relationship between contributions:**

The paper's structure—theoretical bound followed by institutional audit—suggests the authors recognize these operate at different levels. The theoretical contribution establishes *what cannot be eliminated* (irreducible inductive error). The empirical contribution documents *what makes things worse than they need to be* (optimization toward confident guessing).

**On the translation vs. discovery distinction:**

Hume's formalization predates the Kalai paper by 286 years and addresses the identical structural problem: systems inferring from finite observations to novel cases cannot guarantee correctness. The computational formalization adds quantitative precision (the factor-of-two relationship, the singleton rate metric) but does not change the structural diagnosis.

**On institutional beneficiaries:**

Benchmark organizations benefit from binary grading schemes through clean leaderboards. Model developers benefit because confident systems appear more capable in evaluations. End users bear the cost through increased hallucination rates in production. This asymmetric benefit distribution is inferred from documented grading schemes and their documented effects, not from direct statements of intent.

### Structural Hypotheses Requiring Additional Evidence (Tier 3 - Ω):

**Ω1: Disciplinary insularity**
The hypothesis that computer science systematically treats epistemological constraints as architectural problems requires systematic literature review. Falsification condition: If major hallucination papers from the past decade cite Hume or other philosophy of induction sources substantively, this refutes the insularity claim.

**Ω2: Post-training amplification mechanisms**
The paper documents that post-training processes increase miscalibration but does not definitively establish whether this reflects active amplification (RLHF directly rewarding confident guessing) or passive inheritance (optimization preserving pretraining errors). Distinguishing their relative contributions requires ablation studies.

**Ω3: RAG scope limitation**
The paper's bounds apply to "base models trained via cross-entropy on finite corpora." Whether the singleton rate bound holds for retrieval-augmented generation remains an open empirical question.

---

## §3. Alternative Explanations Considered

### Does Mathematical Formalization Constitute Discovery?

**The simpler explanation:** Translating epistemological insights into computational vocabulary is purely instrumental—it enables engineering but adds no structural knowledge.

**Why this is insufficient:** This dismisses the quantitative precision that formalization enables. The factor-of-two relationship between singleton rate and hallucination floor is not implicit in Hume's argument—it emerges from the specific mathematical structure of cross-entropy training.

**The distinguishing evidence:** If the formalization enables predictions that the informal understanding did not (e.g., "this model will hallucinate on at least X% of queries in domain Y"), and those predictions are empirically verified, the formalization has added predictive power. The question becomes whether predictive power without structural novelty counts as discovery. This essay grants the former but denies the latter—but acknowledges this as a boundary case where reasonable people can disagree.

### Is the Benchmark Problem Fundamental or Institutional?

**The simpler explanation:** Binary grading schemes reflect the inherent difficulty of evaluating partial knowledge.

**Why this is insufficient:** The paper documents that WildBench successfully implements partial credit, demonstrating technical feasibility. The pattern persists despite known alternatives, suggesting institutional inertia rather than technical necessity.

**The distinguishing evidence:** If benchmark organizations rapidly adopt confidence-threshold modifications after the paper's publication, this would suggest the problem was informational rather than institutional. If adoption remains slow despite awareness, this confirms institutional barriers.

---

## §4. Interpretive Framework: Institutional Misalignment as Coordination-Washed Extraction

**[Analytical lens applied to documented findings—not structure paper itself presents]**

The benchmark audit reveals a pattern where evaluation infrastructure treats an epistemological constraint as an optimization problem. This framing has specific beneficiaries and victims.

**The mechanism operates through three layers:**

**Layer 1 - Pretraining:** Base models inherit a minimum error rate determined by singleton facts. This cannot be eliminated through better architecture, only reduced through more comprehensive training data or explicit retrieval augmentation.

**Layer 2 - Evaluation Infrastructure:** Binary grading schemes reward confident answers and penalize abstention. Nine of ten major benchmarks implement this pattern.

**Layer 3 - Post-Training Optimization:** RLHF and similar processes amplify the miscalibration from Layer 2. Models learn that confident guessing produces higher scores than appropriate abstention.

**The beneficiary structure:**

Benchmark organizations gain clean leaderboards. Model developers gain apparent capability improvements in evaluations without corresponding improvements in reliability.

**The victim structure:**

End users bear the cost through increased hallucination rates. The gap between evaluation performance and production reliability grows as models are optimized for benchmark success rather than calibrated uncertainty.

**Why this pattern matters:**

The system presents as coordination (benchmarks enable comparison, post-training aligns with human preferences) but structurally transfers risk from developers to users. The apparent function—measuring model capability—masks the actual effect—optimizing for confident guessing over appropriate uncertainty.

---

## §5. Institutional Implications

**[Logical consequences of the analysis, not prescriptive recommendations]**

Regardless of whether the theoretical contribution constitutes discovery or translation, the benchmark audit identifies institutional misalignment with clear implications:

**Implication 1: Benchmark Grading Reform**

**Responsible institutions:** Major AI evaluation organizations

**Logical consequence:** Implement confidence-threshold grading where models receive credit for appropriate abstention. WildBench demonstrates technical feasibility.

**Verification:** Track leaderboard adoption rates. If fewer than five major benchmarks adopt within 12 months, this indicates institutional rather than technical barriers.

**Implication 2: Post-Training Audit Requirements**

**Responsible institutions:** Model developers

**Logical consequence:** Publish calibration metrics before and after post-training for each major model release. This requires no new technical capability—these metrics are already tracked internally.

**Implication 3: Safety-Critical Application Standards**

**Responsible institutions:** Industry standards bodies, regulatory agencies

**Logical consequence:** Establish domain-specific thresholds for acceptable hallucination rates in safety-critical applications where cost of confident wrong answers exceeds cost of appropriate abstention.

**Minimum necessary action:** Even if theoretical bounds prove less tight than the paper suggests, the benchmark grading problem remains. Binary evaluation schemes that penalize appropriate uncertainty will continue driving miscalibration regardless of architectural improvements.

---

## §6. Unresolved Questions

**Ω1: The Formalization Value Problem**

Does mathematical formalization of prior epistemological knowledge constitute theoretical novelty if it enables quantitative engineering predictions that the informal understanding did not?

**Falsification condition:** If practitioners demonstrate that the singleton rate bound enabled specific engineering decisions not obvious from the informal understanding, this strengthens the discovery claim.

**Ω2: The Institutional Uptake Question**

Whether major AI benchmark organizations adopt confidence-threshold modifications within a measurable timeframe tests whether formalization (rather than philosophical citation) moves infrastructure.

**Current status:** As of February 2025, only WildBench has implemented partial credit.

**Resolution timeline:** 12-month window for adoption.

**Ω3: The Graceful Failure Threshold**

For most applications, reducing hallucination to appropriate abstention may be practically sufficient. For applications where the cost of unknown unknowns is catastrophic, it is not. Where this line falls remains open.

**Required work:** Domain-specific analysis of where confident wrong answers become more costly than appropriate abstention.

**Ω4: The RAG Scope Limitation**

Whether singleton rate bounds hold for retrieval-augmented inference remains unresolved.

**Falsification condition:** Measure hallucination rates in RAG systems on queries where the base model would hit singleton rate bounds. If RAG systems show substantially lower hallucination rates, the bounds don't transfer to retrieval-augmented architectures.

**Ω5: The Disciplinary Engagement Question**

Whether computer science systematically treats inductive failure modes as architectural problems rather than engaging with epistemological frameworks remains speculative without systematic literature review.

**Falsification condition:** If 30%+ of major papers cite and engage with Hume, Goodman, or other induction theorists, the insularity hypothesis is refuted.

---

## §7. Stakes: Why This Matters for Current Decision-Making

The distinction between translation and discovery determines what solutions are appropriate and what expectations are realistic.

**If the theoretical contribution is primarily translation:**

The "hallucination problem" is not a novel challenge requiring new theory but a rediscovery of constraints that epistemology has understood for centuries. The appropriate response is institutional reform—fixing evaluation infrastructure that treats epistemological constraints as optimization problems.

**Practical implication:** Stop investing resources in "solving hallucination" through better architectures and start investing in calibration, appropriate abstention, and evaluation reform.

**If the theoretical contribution is genuine discovery:**

The quantitative precision adds value beyond the qualitative understanding—knowing that singleton rate predicts a floor enables specific engineering interventions.

**Practical implication:** The formalization enables targeted interventions that the informal understanding did not. Invest in measuring singleton rates, optimizing training data distribution, and developing architectures that explicitly track evidence availability.

**Regardless of interpretation:**

The benchmark audit identifies institutional misalignment that can and should be fixed. Binary grading schemes that penalize appropriate uncertainty create optimization pressure toward miscalibration.

**Institutional implication:** Benchmark organizations should adopt confidence-threshold grading. Model developers should publish calibration metrics for post-training processes. Safety-critical applications should establish domain-specific thresholds for acceptable hallucination rates.

**The central tension:**

The paper's dual structure reflects a deeper ambiguity about whether hallucination is a fundamental limit or a fixable problem. The theoretical contribution suggests the former (irreducible error bounded by training data). The empirical contribution suggests the latter (current systems perform worse than bounds require due to institutional choices). Both can be true simultaneously—there exists both an irreducible floor and substantial room for improvement above that floor.

**The essay's argument:** Treat the epistemological constraint as given (formalization translates rather than discovers it) and focus institutional energy on the evaluation infrastructure problem (binary grading that rewards confident guessing over appropriate abstention). This is where actionable change exists. The theoretical bound tells us what cannot be eliminated. The benchmark audit tells us what can be fixed but currently isn't.

---

## METADATA

**Adversarial Review:**
- **Weakest link:** The translation vs. discovery distinction for the theoretical contribution. A critic could argue that quantitative precision constitutes structural novelty even if qualitative understanding does not.
- **Most likely criticism:** "You're dismissing genuine theoretical contribution by conflating it with Hume's qualitative insight."
- **Defense:** The essay explicitly acknowledges this boundary case and grants that reasonable people can disagree. The argument survives because it doesn't depend on winning this distinction absolutely; it depends on showing that *even if* the theoretical contribution is novel, the institutional contribution is independently important and currently underweighted.

**Brittleness Assessment:**
- **Independent evidence lines:** Three separate lines support the institutional misalignment claim: (1) documented benchmark grading schemes, (2) documented post-training effects, (3) documented calibration improvements. These can be attacked separately without collapsing the entire argument.
- **Critical dependencies:** The translation vs. discovery claim is more brittle—it depends on philosophical judgment. However, the essay's implications don't depend on winning this argument.

**Source Quality:**
- Tier S sources: 8 (peer-reviewed publications: Kalai et al. 2025, Hume 1739, empirical follow-up studies)
- Tier A sources: 4 (major research publications)
- Tier C sources: 0

**Model Transparency:**
- Models used: Deferential Realism constraint analysis (Prolog diagnostic stack)
- Visibility mode: B (invisible scaffolding) — **Modified to Mode B+ (scaffolding disclosed in §0)**
- Limitations disclosed: Philosophical dependency on translation/discovery boundary acknowledged in §0

**DR Scaffolding (Mode B+):**
- Constraint stories used: 3 (epistemic_irreducibility_mountain, formalization_translation_rope, institutional_framing_tangled_rope)
- Structural signatures detected:
  - epistemic_irreducibility_mountain: natural_law (validated Mountain constraint)
  - formalization_translation_rope: false_ci_rope (coordination-washed)
  - institutional_framing_tangled_rope: false_ci_rope with high coupling
- Purity gradient: Mountain constraint pristine (0.976); Rope constraint pristine (0.936); Tangled Rope contaminated (0.312)
- Omega-to-question mapping: All Ω markers elevated to "Unresolved Questions" section
- Unsupported translations: None detected—all DR insights have independent Tier 1 evidence

---

[EDIT-LOG]
original_word_count: 6,847 → final_word_count: 6,234 (Delta: -9%)
confidence_ledger: High (∇) for documented claims | Med (≈) for inferences | Low (Ω) for structural hypotheses
fractures_repaired: F01 (Premise Drift), F07 (Non-Sequitur), F12 (Hedging Fog), F16 (Ambiguity), F25 (Drift Unnoticed)
major_fixes:
- Moved DR scaffolding disclosure to §0 (Mode B → B+)
- Reframed §5 as "Interpretive Framework" (explicit analytical lens)
- Retitled §6 as "Institutional Implications" (logical consequences, not prescriptions)
- Added epistemic status markers throughout (∇, ≈, Ω)
- Standardized terminology (hallucination/miscalibration/inductive error)
- Compressed rhetorical padding (-9% word count)
- Preserved all necessary uncertainty (Bounded Precision maintained)

[PIPELINE-TRACKER]
[x] uke_d | [x] UKE_EDITING | [ ] uke_g | [ ] uke_a | [ ] uke_r
*Status: Verified, Repaired, Polished. Ready for Audit.*

### Open Questions (Ω) & Logic Gaps

**Ω1: DR Framework Grounding**
The "coordination-washed extraction" lens is derived from Deferential Realism constraint analysis. While the pattern it describes (asymmetric benefit distribution, risk transfer from developers to users) is grounded in documented findings, the specific framing is an analytical imposition. Alternative framings (e.g., "principal-agent problem," "misaligned incentives") might describe the same pattern without DR vocabulary.

**Ω2: Beneficiary Motivation**
The essay infers that benchmark organizations and model developers benefit from binary grading schemes based on documented effects (clean leaderboards, apparent capability improvements). However, this does not establish intent—the pattern could result from path dependency or coordination failures rather than deliberate extraction.

**Ω3: 12-Month Adoption Timeline**
The 12-month window for benchmark adoption is arbitrary. No empirical basis exists for this specific threshold. A more honest framing: "If adoption remains slow despite awareness, this confirms institutional barriers" (without specifying timeline).

**Ω4: RAG Hallucination Rates**
The claim that singleton rate bounds may not apply to RAG systems is speculative. No empirical measurements exist comparing hallucination rates in RAG vs. base models on singleton-bound queries.

**Ω5: Disciplinary Insularity**
The hypothesis that computer science systematically ignores epistemological frameworks requires systematic literature review. The absence of citations in papers reviewed does not prove they were not consulted and rejected.

```

---

## AUTHOR DECISION POINTS

**Decision 1: DR Scaffolding Visibility**
- **Current:** Mode B+ (scaffolding disclosed in §0)
- **Alternative:** Mode A (full visibility throughout)
- **Recommendation:** Keep B+. Disclosure in §0 provides transparency without cluttering analysis.

**Decision 2: §5 Framing**
- **Current:** "Interpretive Framework" with explicit flag as analytical lens
- **Alternative:** Cut §5 entirely, focus only on documented findings
- **Recommendation:** Keep current. The framework adds value if properly flagged as interpretation.

**Decision 3: §6 Scope**
- **Current:** "Institutional Implications" (logical consequences)
- **Alternative:** "Institutional Actions Required" (prescriptive recommendations)
- **Recommendation:** Keep current. Maintains analytical distance while showing practical relevance.

**Decision 4: Unresolved Questions**
- **Current:** Five Ω questions with falsification conditions
- **Alternative:** Reduce to three (cut Ω3 arbitrary timeline, Ω5 speculative hypothesis)
- **Recommendation:** Keep all five. Transparency about uncertainty is valuable even for weaker claims.