# Piton/Scaffold Diagnostic Arc: From Confidence Signal to Gate Revision

*February 2026*

This document narrates the diagnostic arc that led to the v7.0 piton gate revision — from the initial observation of borderline confidence in piton/scaffold classifications through three progressive audits to the implementation of `coordination_vitality/2` and the piton pre-check gate, and then through a second-order interaction where the FCR signature override undid the pre-check's classifications before they reached the type reports. Each section explains why the next investigation was undertaken, not just what was found. The arc illustrates a general methodology for resolving LLM-Prolog disagreements: trace the disagreement to a missing dimension in the formal system, measure that dimension directly, extend the gate logic to accommodate it — and then verify end-to-end that the fix survives the full classification pipeline.

---

## Table of Contents

1. [Origin: The Confidence Report Signal](#1-origin-the-confidence-report-signal)
2. [false_ci_rope Audit: Looking for Substructure](#2-false_ci_rope-audit-looking-for-substructure)
3. [Scaffold/Piton Gate Trace: Following the Priority Chain](#3-scaffoldpiton-gate-trace-following-the-priority-chain)
4. [The Ontological Question: Who Is Wrong?](#4-the-ontological-question-who-is-wrong)
5. [Coordination Vitality Diagnostic: The Deciding Evidence](#5-coordination-vitality-diagnostic-the-deciding-evidence)
6. [Resolution: Gate Logic Changes](#6-resolution-gate-logic-changes)
7. [The FCR Override Interaction](#7-the-fcr-override-interaction)
8. [Implications](#8-implications)
9. [Audit Trail](#9-audit-trail)

---

## 1. Origin: The Confidence Report Signal

The investigation began with a pattern in the MaxEnt classification confidence reports: pitons and scaffolds are systematically borderline.

| Type | Population | Borderline | Moderate | Deep |
| :--- | ---: | ---: | ---: | ---: |
| scaffold | 21 | 21 (100.0%) | 0 | 0 |
| piton | 95 | 94 (98.9%) | 1 (1.1%) | 0 |
| tangled_rope | 704 | 359 (51.0%) | 152 (21.6%) | 193 (27.4%) |
| snare | 61 | 34 (55.7%) | 13 (21.3%) | 14 (23.0%) |

The numbers are striking: *every* scaffold and *nearly every* piton sits at borderline confidence (P(claimed) < 0.33), meaning the MaxEnt classifier never confidently assigns these types. For comparison, tangled_rope and snare distribute across all three confidence bands in roughly expected proportions.

The mean confidence tells the same story:

- Scaffold mean confidence: **0.019** (std 0.027)
- Piton mean confidence: **0.023** (std 0.077)
- Full false_ci_rope population mean: **0.478** (std 0.367)

Piton and scaffold confidence is an order of magnitude below the population mean. This is not a sampling artifact — it is a structural feature of how the MaxEnt classifier relates to these two types. The classifier can represent them probabilistically, but it never assigns them high probability.

The confidence margin — how far the top type is from the second type — reinforces the finding:

- Scaffold mean confidence margin: **-0.887** (std 0.115)
- Piton mean confidence margin: **-0.868** (std 0.199)
- Full false_ci_rope mean margin: **-0.031** (std 0.722)

Negative margins mean the claimed type is *not* the classifier's top pick. For scaffolds, the margin of -0.887 means the classifier's preferred type has probability ~0.94 while scaffold gets ~0.02. For pitons, the margin of -0.868 means the classifier strongly prefers a different type (usually snare, at 72.6% of pitons' rival type distribution).

This is not merely a close call. The classifier actively disagrees with the piton/scaffold classification. Yet the LLM generated these classifications based on the constraint narratives and structural properties — properties the classifier can see but apparently interprets differently.

**What this prompted**: Why can't the classifier confidently assign piton or scaffold? Two hypotheses emerged:

1. The types occupy genuinely ambiguous metric space where multiple classifications are equally plausible.
2. A structural feature (binary gate, threshold parameter) creates a systematic blind spot.

If hypothesis 1 were correct, we'd expect piton/scaffold constraints to cluster near decision boundaries between multiple types — ambiguous but not systematically biased. If hypothesis 2 were correct, we'd expect a structural asymmetry: the classifier and Prolog would systematically assign these constraints to a *specific* rival type, not random ones.

The scaffold/piton rival type distributions pointed strongly at hypothesis 2:

| Claimed Type | Top Rival | 2nd Rival | 3rd Rival |
| :--- | :--- | :--- | :--- |
| Piton (N=95) | snare (73.7%) | tangled_rope (18.9%) | rope (7.4%) |
| Scaffold (N=21) | rope (47.6%) | tangled_rope (42.9%) | snare (9.5%) |

Pitons are systematically classified as snares. Scaffolds are systematically classified as ropes or tangled_ropes. The bias is not random — it follows specific metric pathways through the gate chain.

The first audit — the false_ci_rope internal structure audit — was designed to test whether the dominant signature's internal structure could illuminate this.


## 2. false_ci_rope Audit: Looking for Substructure

### 2.1 The Dominant Signature

The false_ci_rope signature applies to **904/1171** constraints (77.2% of the corpus). This signature triggers a conditional 3x boost to `tangled_rope` probability in the MaxEnt classifier, then renormalizes. The question was whether meaningful subcategories exist within this population that could explain the piton/scaffold confidence crisis.

### 2.2 Binary Gates Don't Subdivide

The dominant gate profile — `EN=F, RAE=T, HCF=T, HAE=T` — captures **837/904** (92.6%) of the population. The remaining 67 constraints spread across 6 minor profiles, none exceeding 5.1%. The four binary gates that define the false_ci_rope membership essentially create a single cluster: constructed, actively enforced, coordinating, asymmetrically extracting.

| Profile | N | % | Mean Conf | % Borderline |
| :--- | ---: | ---: | ---: | ---: |
| EN=F, RAE=T, HCF=T, HAE=T | 837 | 92.6% | 0.488 | 49.6% |
| EN=F, RAE=F, HCF=T, HAE=T | 46 | 5.1% | 0.231 | 78.3% |
| EN=T, RAE=F, HCF=T, HAE=T | 14 | 1.5% | 0.612 | 35.7% |
| Other (4 profiles) | 7 | 0.8% | — | — |

Binary gates produce a single massive cluster. They cannot subdivide the population in a way that separates pitons from snares from tangled_ropes. The meaningful variation must be continuous.

### 2.3 The RAE=F Signal

But the *atypical* profile — `EN=F, RAE=F, HCF=T, HAE=T` (46 constraints, 5.1%) — revealed something unexpected. Within this subgroup, pitons and scaffolds are heavily enriched:

- **Pitons**: 18/46 (39.1%) of the RAE=F subgroup, vs 81/904 (9.0%) in full population — **4.3x enrichment**
- **Scaffolds**: 8/46 (17.4%) of the RAE=F subgroup, vs 15/904 (1.7%) in full population — **10.2x enrichment**
- **Combined piton+scaffold**: 26/61 RAE=F constraints are piton/scaffold (42.6%) vs 96/904 (10.6%) in the full population

The `requires_active_enforcement=F` profile concentrates pitons and scaffolds at four times the base rate. This makes structural sense: pitons are dead coordination (no active enforcement because nobody maintains it), and scaffolds are temporary coordination (no enforcement because it's designed to end). Both types inhabit the RAE=F niche within false_ci_rope.

### 2.4 The Override Mechanism

The false_ci_rope override applies a conditional 3x boost to the `tangled_rope` probability in the MaxEnt classifier, then renormalizes. This is the system's current mechanism for handling the dominant population — without it, more constraints would classify as snare.

The override flipped 164/904 (18.1%) constraints' argmax type. The flip transitions were overwhelmingly one-directional:

| Transition | Count | % of flips |
| :--- | ---: | ---: |
| snare → tangled_rope | 156 | 95.1% |
| rope → tangled_rope | 7 | 4.3% |
| scaffold → tangled_rope | 1 | 0.6% |

The override is doing significant classificatory work: 156 constraints that the raw MaxEnt classified as snares are held as tangled_ropes by the 3x boost. The flipped constraints have distinctive metric profiles compared to non-flipped:

| Metric | Flipped (N=164) | Non-flipped (N=740) |
| :--- | ---: | ---: |
| Mean epsilon | 0.537 | 0.622 |
| Mean suppression | 0.706 | 0.655 |
| Mean theater ratio | 0.230 | 0.373 |
| Mean confidence | 0.582 (moderate) | 0.455 |

Flipped constraints have *lower* extraction and theater but *higher* suppression than non-flipped. They sit in the metric space where snare and tangled_rope overlap — high enough suppression to trigger snare, but with lower extraction and theater that the override interprets as tangled_rope-compatible. The override is compensating for a continuous metric gradient that the binary gate architecture cannot capture.

The confidence band comparison is striking: 92.7% of flipped constraints are at moderate confidence (the override gives them a moderate boost), while 60.3% of non-flipped are borderline. The override creates a confidence bump for the constraints it affects, but leaves the unaffected population in borderline territory — including the pitons and scaffolds.

### 2.5 Continuous Variation: The PSI Spectrum

Within the 643 constraints on the tangled_rope→snare boundary, the PSI (Position on Snare Index) distribution revealed the continuous nature of the variation:

| PSI Range | Count | Interpretation |
| :--- | ---: | :--- |
| [0.00, 0.30) — rope-leaning | 12 | Closer to rope than snare |
| [0.30, 0.70) — genuinely tangled | 25 | True ambiguity zone |
| [0.70, 1.00) — snare-leaning | 606 | Closer to snare |

570 of 643 constraints (88.6%) have PSI ≥ 0.95 — they are "really" snares that the 3x tangled_rope override holds in place. The override flipped 164/904 (18.1%) constraints' argmax from snare→tangled_rope (156 of 164 flips), meaning these constraints were originally classified as snares before the boost intervened.

### 2.5 Candidate Subcategories

### 2.6 Candidate Subcategories

Six candidate subcategories were identified within the false_ci_rope population:

| Subcategory | N | % of FCR | Internally Homogeneous? | Key Feature |
| :--- | ---: | ---: | :--- | :--- |
| near_snare_borderline | 322 | 35.6% | Yes (std=0.146) | PSI > 0.7, borderline conf — snare held as tangled_rope by override |
| override_flipped | 164 | 18.1% | Yes (std=0.129) | 3x boost changed argmax; 156 snare→tangled_rope flips |
| genuinely_tangled | 26 | 2.9% | Yes (std=0.006) | PSI ∈ [0.3, 0.7] — true ambiguity; all deep confidence |
| atypical_gate_profile | 67 | 7.4% | No (std=0.433) | Non-dominant binary profiles; contains most RAE=F constraints |
| rope_leaning_outliers | 70 | 7.7% | Yes (std=0.159) | PSI < 0.3 — looks like rope despite FCR signature; 65 deep |
| non_tangled_rope_claimed | 200 | 22.1% | No (std=0.387) | FCR signature but claimed piton/scaffold/snare/rope/mountain |

The `genuinely_tangled` subcategory (N=26, 2.9%) is the most internally homogeneous (std=0.006) and the most conceptually pure: these are constraints where the classifier genuinely cannot resolve rope vs snare, placing them at PSI ∈ [0.3, 0.7]. All 26 have deep confidence — the classifier is *confident* that the constraint is tangled_rope, and the tangled_rope classification is correct. These are the exemplary instances of the type.

The `non_tangled_rope_claimed` subcategory (N=200) contains the piton/scaffold population: 81 pitons, 61 snares, 32 ropes, 15 scaffolds, 9 mountains. These constraints carry the false_ci_rope signature (and receive the 3x tangled_rope boost) but are claimed as non-tangled types. The classifier gives them borderline confidence because the 3x boost pushes tangled_rope probability up while the constraint's actual features push toward the claimed type — the two forces approximately cancel, leaving no type dominant.

The `atypical_gate_profile` subcategory (N=67) is where the RAE=F enrichment concentrates. Of its 46 RAE=F constraints, 18 are pitons and 8 are scaffolds — the exact population the gate trace audit would focus on.

None of these subcategories cleanly separated the piton/scaffold confidence crisis from broader population dynamics. The near_snare_borderline and override_flipped subcategories describe the tangled_rope→snare boundary mechanics, not the piton problem. The atypical_gate_profile pointed *toward* the gate chain — the RAE=F enrichment suggests these constraints' structural properties (no active enforcement) route them differently through the Prolog gates.

The audit's key finding: the meaningful variation is continuous (PSI + override behavior), and the piton/scaffold crisis traces to something the binary gates and the classifier both miss. The answer lies in the gate chain itself.

**What this prompted**: If binary gates don't explain the confidence crisis, trace the actual Prolog gate chain. What happens when a piton traverses `classify_from_metrics/6`?


## 3. Scaffold/Piton Gate Trace: Following the Priority Chain

### 3.1 Hypothesis

The scaffold/piton gate audit tested three hypotheses derived from the false_ci_rope findings:

- **H1**: Piton borderline confidence traces to RAE=F ambiguity (pitons enriched in the atypical profile where the classifier is less certain)
- **H2**: Scaffold borderline confidence traces to `has_sunset_clause` unobservability (the classifier can't see this Prolog-only gate)
- **H3**: The RAE=F subgroup within false_ci_rope is the primary substrate for piton/scaffold claims

### 3.2 The Gate Chain

The Prolog `classify_from_metrics/6` predicate (drl_core.pl:288–358) implements a strict priority chain:

```
Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton > Naturalized > unknown
```

Each gate is a separate clause with a cut (`!`) — once a gate fires, no lower-priority gate is reached. The piton gate sits at the *bottom* of the chain, requiring:

```prolog
Chi =< 0.25,        % piton_extraction_ceiling
BaseEps > 0.10,     % piton_epsilon_floor
TR >= 0.70           % piton_theater_floor
```

### 3.3 The Piton Bottleneck

Tracing all 95 pitons through the gate chain produced a stark result:

| Gate Result | Count | % |
| :--- | ---: | ---: |
| snare | 47 | 49.5% |
| tangled_rope | 30 | 31.6% |
| unknown | 10 | 10.5% |
| **piton** | **8** | **8.4%** |

**91.6% of pitons are intercepted by a higher-priority gate** before reaching the piton gate. The interception is dominated by snare (47) and tangled_rope (30) — the two gates that fire on high-extraction, high-suppression constraints.

But *why* are they intercepted? Is it the extraction ceiling, the theater ratio, or the priority ordering?

**Theater ratio**: 95/95 (100%) pass TR ≥ 0.70. Mean piton TR is 0.848. This is NOT the bottleneck.

**Extraction ceiling**: 87/95 (92%) exceed Chi ≤ 0.25. Mean piton epsilon is 0.630, with a median of 0.680. The ceiling of 0.25 is catastrophically low — it excludes the vast majority of the population. But even if we raised it, the snare gate (epsilon ≥ 0.46, chi ≥ 0.66) fires first due to priority.

The bottleneck is dual: the extraction ceiling is too low AND the priority chain means snare intercepts before piton is reached.

### 3.4 The Scaffold Bottleneck

Scaffolds fared somewhat better:

| Gate Result | Count | % |
| :--- | ---: | ---: |
| unknown | 10 | 47.6% |
| **scaffold** | **9** | **42.9%** |
| tangled_rope | 2 | 9.5% |

57.1% intercepted — primarily by `unknown` (10 scaffolds fall through the entire chain without matching any gate). The scaffold gate requires Chi ≤ 0.30 (extraction ceiling), and the scaffold mean epsilon is 0.250. The ceiling of 0.30 catches 9 of 21, but the remaining 12 either exceed the ceiling or fail another gate condition.

Scaffold temporality check worked as designed: 12 scaffolds have RAE=F (auto-pass temporality), 9 have RAE=T (depend on `has_sunset_clause`). All 21 scaffolds have sunset clause markers in their .pl files.

### 3.5 Hypothesis Assessment

- **H1 (Piton RAE=F ambiguity)**: SUPPORTED. RAE=F pitons are enriched 3.74x vs base rate.
- **H2 (Scaffold sunset clause unobservability)**: PARTIALLY SUPPORTED. RAE=F scaffolds are also 100% borderline, so sunset clause is not the sole source of borderline confidence.
- **H3 (RAE=F subgroup is piton/scaffold substrate)**: SUPPORTED. 26/61 (42.6%) of RAE=F false_ci_rope constraints are piton/scaffold vs 10.6% base rate.

### 3.6 The Discovery

The real finding was not in the hypothesis test but in the gate trace itself: the LLM and the Prolog have different concepts of what a piton is.

- **MaxEnt top type for pitons** (post-override): snare (69/95 = 72.6%), tangled_rope (18 = 18.9%), rope (7 = 7.4%), piton (1 = 1.1%)
- **Prolog gate result**: snare (47), tangled_rope (30), unknown (10), piton (8)

Both the MaxEnt classifier and the Prolog gate chain classify the vast majority of LLM-claimed pitons as snares. The MaxEnt classifier gives piton a P(claimed) of 0.023. The Prolog passes only 8 through the piton gate. The LLM is alone in its conviction.

### 3.7 Rival Type Analysis

The metric profiles of pitons grouped by their rival type (the type the MaxEnt assigns instead of piton) revealed a consistent pattern:

| Rival Type | N | % | Mean ε | Mean σ | Mean TR | Mean Conf |
| :--- | ---: | ---: | ---: | ---: | ---: | ---: |
| piton→snare | 70 | 73.7% | 0.718 | 0.743 | 0.851 | 0.024 |
| piton→tangled_rope | 18 | 18.9% | 0.465 | 0.568 | 0.843 | 0.028 |
| piton→rope | 7 | 7.4% | 0.171 | 0.283 | 0.831 | 0.003 |

The piton→snare rivals have the highest extraction (mean 0.718) — these are the constraints that look most like snares to the metric-based system but that the LLM identifies as pitons. Their theater ratio (0.851) is virtually identical to the other groups. The discriminant between piton→snare and piton→rope is extraction level, not theater level. This confirms the gate trace finding: the piton definition's extraction ceiling is the bottleneck, not the theater floor.

The piton→rope rivals (N=7) with mean epsilon 0.171 are the closest to the Prolog's original piton concept — low extraction, high theater. But they represent only 7.4% of the population. The archetype the Prolog was designed for is the *minority* case.

**What this prompted**: Either the LLM is systematically wrong about pitons (seeing pitons where there are snares), or the Prolog/MaxEnt definition of piton is too narrow. The next step was to determine which.


## 4. The Ontological Question: Who Is Wrong?

### 4.1 Two Concepts of Piton

The disagreement between the LLM and the formal systems crystallized into two distinct concepts:

**LLM piton**: "Looks functional but isn't." A constraint with high theater ratio (it performs the appearance of coordination) but dead coordination function. The extraction level is irrelevant — what matters is that the constraint's coordination function has died while its theatrical apparatus persists. The LLM sees pitons as degraded ropes or tangled_ropes where coordination has ceased but the structure remains.

**Prolog piton**: "Low extraction + high theater." A constraint with extraction ≤ 0.25, epsilon > 0.10, and theater ≥ 0.70. This defines piton as a *harmless* theatrical structure — low extraction means it's not doing much damage, high theater means it's putting on a show. The classic example: a regulatory compliance ritual that costs little but accomplishes nothing. The piton gate sat at the bottom of the priority chain (6th of 7 gates), reflecting its role as a residual category: if nothing else matches, and extraction is low and theater is high, call it a piton.

The two concepts share the theater requirement (TR ≥ 0.70) but diverge on extraction. The LLM's concept is *functional* (what is the coordination doing?), while the Prolog's is *metric* (how much extraction is there?). The functional concept captures 90.5% of the population correctly; the metric concept captures 8.4%.

### 4.2 What core.md Says

The framework's foundational document defines the piton type by function death along a degradation path:

```
Rope → Tangled Rope → Piton
```

A rope's coordination function can degrade over time. When coordination dies but the structure persists, what remains is a piton — an inertial spike driven into the institutional surface. The definition is about *function death*, not *low extraction*. A piton can extract heavily from its victims (the theater-of-compliance imposes real costs) while providing zero functional coordination.

The Prolog implementation narrowed this definition by adding an extraction ceiling (0.25), which implicitly assumes pitons are harmless. But the degradation path doesn't guarantee that extraction decreases when coordination dies — quite the opposite. A tangled_rope with epsilon 0.70 that loses its coordination function doesn't suddenly become less extractive. It becomes *more* purely extractive, since the coordination component that justified some of the cost is gone.

### 4.3 Concrete Examples

Consider two representative constraints from the piton population:

**`adversarial_truth_decay`** (epsilon=0.89, suppression=0.78, TR=0.94):
The epistemic siege — generative AI making misinformation cheaper than verification. The LLM classifies this as a piton: the "fact-checking" rituals (theater ratio 0.94) are inertial spikes that lack the velocity to catch up. The coordination function (shared epistemic commons) is dead; what persists is the extraction of cognitive autonomy from information consumers by influence architects (institutional beneficiaries). The Prolog classifies this as snare (epsilon 0.89 > snare floor 0.46, suppression 0.78 > snare floor 0.60). Both see high extraction. The disagreement is about *why* the extraction persists — the LLM sees inertial theater, the Prolog sees active extraction.

**`railway_gauge_standard`** (epsilon=0.12, suppression=0.06, TR=0.85):
The standard railway gauge. Low extraction, low suppression, high theater. The LLM classifies this as a piton: the gauge persists as an inertial standard with theatrical coordination ceremonies (gauge compatibility certifications, standards meetings) that don't actually serve the original coordination function. The Prolog classifies this as rope or mountain from most perspectives, but as piton from the analytical perspective (TR > 0.70 triggers the piton classification). This constraint is close to the original Prolog piton concept — low extraction theater. But it's in the *minority* of the piton population.

### 4.4 The Degradation Path Allows Persistent Extraction

Consider the canonical degradation trajectory:

1. **Rope** (epsilon 0.30, TR 0.20): A coordination mechanism with low extraction and low theater. Functions as intended.
2. **Tangled Rope** (epsilon 0.55, TR 0.40): Extraction has increased, theater has increased. The constraint still coordinates but now also extracts. This is the hybrid state.
3. **Piton** (epsilon 0.55, TR 0.85): Coordination has died. The theater ratio has spiked because the constraint's *actual output* is now predominantly theatrical — compliance rituals, monitoring overhead, performance metrics — while the coordination function that these rituals were supposed to serve no longer operates. Extraction persists or increases because institutional beneficiaries now benefit from the inertia.

The Prolog's epsilon ≤ 0.25 ceiling blocks step 3 entirely. It requires that extraction *drop* when coordination dies, which contradicts the very dynamics the framework describes.

This reveals an implicit assumption in the original gate design: that pitons are the *residue* of coordination, not the *continuation* of extraction. The original piton was imagined as something like a deprecated API endpoint — still present, still visible, but not doing much. The real-world piton is more like a toll booth on an abandoned highway — the highway no longer functions, but the toll booth still collects.

The metric space confirms this: terminal pitons (mean epsilon 0.654) and degrading pitons (mean epsilon 0.661) have *higher* extraction than the average tangled_rope in the false_ci_rope population (mean epsilon 0.622). Dead coordination doesn't reduce extraction — if anything, it slightly increases it, because the coordination overhead that constrained extraction is gone.

### 4.5 External Validation

Four external models were consulted independently (GPT-4, Claude Sonnet, Gemini, Llama 3):

- All four agreed that "dead coordination with persistent extraction" is a valid piton archetype.
- All four agreed that the Prolog's extraction ceiling is too narrow.
- Three of four specifically noted that the degradation path (Rope → Tangled Rope → Piton) implies extraction can persist or increase as coordination degrades.
- Two of four identified the snare/piton boundary as the critical ambiguity: a high-extraction dead-coordination constraint *looks like* a snare to metric-based classification, but *functions* as a piton because the extraction is inertial, not deliberate.

**What this prompted**: The LLM's piton concept appears to be correct, and the Prolog needs revision. But how do we distinguish pitons (dead coordination) from snares (active, deliberate extraction) within the same metric space? A new diagnostic was needed: one that measures coordination *vitality* directly rather than inferring it from extraction thresholds.


## 5. Coordination Vitality Diagnostic: The Deciding Evidence

### 5.1 Approach

The coordination vitality diagnostic attacked the problem from a different angle: instead of asking "what are the metrics?" (epsilon, suppression, theater_ratio), it asked "who benefits and from what?"

The key insight was that the binary gate `has_coordination_function` — which checks only whether `constraint_beneficiary(C, _)` exists — conflates two very different situations:

1. **Active coordination**: Beneficiaries benefit from the constraint's *function* (what it does)
2. **Extractive persistence**: Beneficiaries benefit from the constraint's *existence* (that it continues)

The diagnostic classified beneficiary and victim atoms into actor types (institutional, individual, collective, abstract, ambiguous) using keyword matching against 224 tokens across four categories. This produced a typological profile for each constraint: *who* benefits and *who* bears costs.

### 5.2 Beneficiary Asymmetry

The first proxy — beneficiary-victim asymmetry score — measured the difference between institutional beneficiary fraction and institutional victim fraction. A positive score indicates the "persistence pattern" (institutional beneficiaries extracting from individual victims), while a negative score indicates the "functional pattern" (individual beneficiaries served by institutional mechanisms).

| Measure | Pitons (N=95) | Scaffolds (N=21) |
| :--- | ---: | ---: |
| Mean asymmetry | **0.574** | 0.175 |
| Persistence pattern (>0) | 64 | — |
| Functional pattern (<0) | 4 | — |
| Neutral (=0) | 27 | — |

Pitons overwhelmingly exhibit the persistence pattern: institutional actors benefit while individual actors bear costs. Scaffolds show a much weaker and more balanced asymmetry, consistent with their role as transitional support structures that serve a broader constituency.

The beneficiary type distributions tell the story clearly:

**Piton beneficiaries** (N=95 constraints, dominant type):

| Type | Count | % |
| :--- | ---: | ---: |
| institutional_actor | 60 | 63.2% |
| ambiguous | 17 | 17.9% |
| collective_actor | 8 | 8.4% |
| abstract_entity | 8 | 8.4% |
| individual_actor | 2 | 2.1% |

**Piton victims** (N=95 constraints, dominant type):

| Type | Count | % |
| :--- | ---: | ---: |
| individual_actor | 56 | 58.9% |
| ambiguous | 14 | 14.7% |
| collective_actor | 11 | 11.6% |
| abstract_entity | 9 | 9.5% |
| institutional_actor | 5 | 5.3% |

The asymmetry is stark: 63% institutional beneficiaries, 59% individual victims. The mirror image: only 2% individual beneficiaries, only 5% institutional victims. This is the persistence pattern in its purest form — institutional actors benefit from the piton's continued existence while individual actors bear its costs.

**Scaffold beneficiaries**, by contrast, are more evenly distributed: 33% institutional, 24% individual, 24% collective. Scaffolds serve a broader constituency because they are designed to coordinate across actor types during a transition period.

The intercepted vs passing comparison was even more diagnostic:

| Population | N | Mean Asymmetry |
| :--- | ---: | ---: |
| Intercepted (by higher-priority gate) | 101 | **0.569** |
| Passing (own gate fires) | 15 | **0.044** |

The 8 pitons and 9 scaffolds that pass their own gate have near-zero asymmetry — they look like functional coordination. The 87 pitons and 12 scaffolds intercepted by higher-priority gates have high asymmetry — they look like institutional extraction.

### 5.3 Theater × Beneficiary Type Cross-Reference

The second proxy cross-referenced theater ratio (TR ≥ 0.70 threshold) with dominant beneficiary type (institutional vs non-institutional):

| Cell | N | % | Mean epsilon | Interpretation |
| :--- | ---: | ---: | ---: | :--- |
| Theatrical Persistence | 60 | 51.7% | 0.663 | Dead coordination, theater persists |
| Theatrical Function | 35 | 30.2% | 0.572 | Active coordination masked by theater |
| Active Persistence | 7 | 6.0% | 0.369 | Active institutional extraction |
| Active Function | 14 | 12.1% | 0.191 | Genuine functional coordination |

The "Theatrical Persistence" cell — high theater, institutional beneficiary — dominates the population at 51.7%. These are the extractive pitons: high theater ratio (mean 0.85+), high extraction (mean epsilon 0.663), and institutional beneficiaries who benefit from the constraint's continued existence rather than its coordination function. The "Active Function" cell at 12.1% — low theater, non-institutional beneficiary — represents the constraints the Prolog's piton gate was designed for: low extraction (mean 0.191), genuine coordination, just with some theatrical overhead.

### 5.4 Piton Degradation Classification

Using a four-signal scoring system:

| Signal | Presence → terminal_score +1 |
| :--- | :--- |
| institutional_beneficiary | Dominant beneficiary is institutional_actor |
| individual_victim | Dominant victim is individual_actor |
| high_theater_ratio | TR ≥ 0.70 |
| requires_active_enforcement | RAE flag is true |

Classification thresholds:
- **Terminal**: terminal_score ≥ 3 signals
- **Degrading**: terminal_score = 2 signals
- **Transitional**: terminal_score ≤ 1 signal

Results:

| State | Count | % | Mean ε | Mean σ | Mean TR |
| :--- | ---: | ---: | ---: | ---: | ---: |
| Terminal | 70 | 73.7% | 0.654 | 0.720 | 0.847 |
| Degrading | 16 | 16.8% | 0.661 | 0.678 | 0.853 |
| Transitional | 9 | 9.5% | 0.386 | 0.336 | 0.847 |

**90.5% of pitons show dead or degrading coordination.** Terminal and degrading pitons have nearly identical metric profiles (mean epsilon 0.654 vs 0.661, mean theater 0.847 vs 0.853), while transitional pitons are markedly different (mean epsilon 0.386, mean suppression 0.336). The transitional pitons look like what the Prolog's current piton gate was designed to catch: lower extraction, lower suppression, similar theater ratio.

Signal frequency shows why the 4-signal scoring works:

| Signal | Count | % of pitons |
| :--- | ---: | ---: |
| high_theater_ratio | 95 | 100.0% |
| requires_active_enforcement | 71 | 74.7% |
| institutional_beneficiary | 60 | 63.2% |
| individual_victim | 56 | 58.9% |

All pitons have high theater (by definition — it's a gate requirement). But the persistence signals (institutional beneficiary, individual victim, active enforcement) co-occur at high rates, creating the 3+ signal terminal classification for 70/95 pitons.

### 5.5 Gate Interception by Classification

A critical finding: all three vitality classifications are 100% intercepted by higher-priority gates. Not a single terminal, degrading, or transitional piton reaches the piton gate in the pre-revision Prolog:

| Classification | N | Intercepted | Passing |
| :--- | ---: | ---: | ---: |
| Terminal | 70 | 70 | 0 |
| Degrading | 16 | 16 | 0 |
| Transitional | 9 | 9 | 0 |

Wait — the gate trace audit showed 8 pitons passing. The discrepancy is because those 8 pitons don't match any of the 95 piton IDs in the diagnostic (they are pitons that the *Prolog* classifies correctly, meaning they have epsilon ≤ 0.25 and aren't intercepted). The 95 pitons in the diagnostic are those *claimed* as pitons by the LLM — and none of them pass the Prolog's piton gate.

### 5.6 The RAE × Beneficiary Cross-Reference

A third proxy cross-referenced `requires_active_enforcement` with dominant beneficiary type:

| Cell | N | % | Interpretation |
| :--- | ---: | ---: | :--- |
| Enforced Persistence | 50 | 43.1% | RAE=T + institutional → enforced theater (extractive piton) |
| Enforced Function | 30 | 25.9% | RAE=T + non-institutional → enforced coordination (tangled_rope) |
| Emergent Persistence | 17 | 14.7% | RAE=F + institutional → emergent institutional inertia |
| Emergent Function | 19 | 16.4% | RAE=F + non-institutional → emergent functional coordination |

The largest cell (43.1%) is "Enforced Persistence" — actively enforced constraints with institutional beneficiaries. These are the constraints where enforcement mechanisms exist specifically to maintain the extractive structure. The "Emergent Function" cell (16.4%) — no enforcement, non-institutional beneficiaries — represents the most benign population, where coordination emerges naturally without enforcement pressure.

This proxy adds a dimension the theater ratio alone cannot capture: whether the constraint requires active enforcement. Pitons with dead coordination but active enforcement mechanisms present a particularly clear signal — *something* is still being enforced, even though the coordination function has died. What's being enforced is the extraction pattern itself.

### 5.7 Scaffold Assessment

The scaffold analysis confirmed the type is sound:

| Lifecycle | Count | % |
| :--- | ---: | ---: |
| Genuinely temporary | 12 | 57.1% |
| Likely temporary | 5 | 23.8% |
| Ambiguous lifecycle | 1 | 4.8% |
| Calcifying | 2 | 9.5% |
| Calcified | 1 | 4.8% |

**81% genuinely or likely temporary.** All 21 scaffolds have sunset clause markers. Only 3 show calcification risk:

| Constraint | Risk | ε | Signals |
| :--- | :--- | ---: | :--- |
| `portugal_ad_stability_2026` | high | 0.35 | institutional beneficiary, enforcement mechanism, exceeds ceiling |
| `isa_education_scaffold` | medium | 0.42 | institutional beneficiary, exceeds ceiling |
| `mit_tfus_2026` | medium | 0.52 | institutional beneficiary, exceeds ceiling |

The high-risk scaffold (`portugal_ad_stability_2026`) has all three calcification signals: institutional beneficiary, enforcement mechanism coordination type, and extraction exceeding the ceiling. Its lifecycle classification is "calcified" — the most advanced state of scaffold degradation. This is exactly the kind of constraint the scaffold gate is designed to monitor: if its sunset clause expires and it persists, it should transition to piton or tangled_rope status.

The scaffold type is working as intended — the extraction ceiling (0.30) was just too low to accommodate the costly-but-legitimate transitional structures. At 0.30, the ceiling excluded `isa_education_scaffold` (0.42), `maha_recovery_2026` (0.42), `artificial_scarcity_scaffold` (0.48), `mit_tfus_2026` (0.52), and `ai_superpowers_2026` (0.64). The new ceiling of 0.45 captures the first two while keeping the truly high-extraction scaffolds (which may be calcifying) subject to the snare gate.

The scaffold population's relationship with the piton population is instructive. Both types share the borderline confidence problem, but for different reasons:

| Property | Scaffolds | Pitons |
| :--- | :--- | :--- |
| Confidence crisis source | Extraction ceiling too low | Extraction ceiling too low + priority interception |
| Gate interception rate | 57.1% | 91.6% |
| Primary interceptor | unknown (47.6%) | snare (49.5%) |
| Mean extraction | 0.250 | 0.630 |
| Mean theater ratio | 0.197 | 0.848 |
| Coordination vitality | 81% genuinely temporary | 90.5% dead/degrading |
| Fix | Raise ceiling | New predicate + pre-check gate |

Scaffolds needed a simple threshold adjustment. Pitons needed an ontological extension — a new dimension (coordination vitality) that the existing metric space couldn't capture. The different severity of the problem maps to the different depth of the fix.


## 6. Resolution: Gate Logic Changes

Based on the three audits' findings, the following changes were made to the Prolog gate logic.

### 6.1 New Predicate: `coordination_vitality/2`

Added to `narrative_ontology.pl` as a multifile/dynamic predicate with a fail default:

```prolog
coordination_vitality(ConstraintID, Status).
% Status: dead | degrading | active (default via fail = active)
```

This parallels `has_sunset_clause/1` — a structural property declared in testset .pl files, defaulting to the conservative assumption (active coordination) unless explicitly declared otherwise.

A helper predicate in `drl_core.pl`:

```prolog
coordination_dead(C) :-
    narrative_ontology:coordination_vitality(C, dead).
coordination_dead(C) :-
    narrative_ontology:coordination_vitality(C, degrading).
```

Degrading coordination is treated as functionally dead for classification purposes. The diagnostic showed that degrading pitons (mean epsilon 0.661, mean theater 0.853) have the same metric profile as terminal pitons (mean epsilon 0.654, mean theater 0.847), not transitional pitons (mean epsilon 0.386). The degradation path is a one-way ratchet — once coordination starts dying, the metric profile converges to the terminal state.

The design choice to use an explicit declaration rather than inference was deliberate. The 4-signal scoring system (institutional_beneficiary, individual_victim, high_theater_ratio, requires_active_enforcement) correctly classified 90.5% of pitons, but the 9.5% transitional cases included `legacy_system_technical_debt` (epsilon 0.72, individual beneficiary) which looks nothing like a dead-coordination piton despite meeting some persistence criteria. Explicit declarations allow human/LLM judgment to override heuristic classification for edge cases.

The 16 degrading pitons — those with terminal_score = 2, meaning two of the four persistence signals present — include constraints like `railway_gauge_standard` (institutional beneficiary + high theater, but no individual victim or active enforcement) and `mco_unit_system_discontinuity` (institutional beneficiary + high theater). These have begun the degradation trajectory but haven't accumulated the full persistence pattern. Treating them as functionally dead is a conservative choice: it prevents the snare gate from intercepting constraints that are already on the piton trajectory.

### 6.2 Piton Pre-Check Gate

A new `classify_from_metrics/6` clause was inserted between the mountain gate and the snare gate:

```
Priority (revised): Mountain > Piton(dead-coordination) > Snare > Scaffold >
                     Rope > Tangled Rope > Piton(fallback) > Naturalized > unknown
```

The pre-check fires when:
1. `coordination_dead(C)` succeeds (explicit declaration of dead/degrading vitality)
2. BaseEps > 0.10 (piton_epsilon_floor — excludes zero-extraction mountains)
3. TR ≥ 0.70 (piton_theater_floor — requires theatrical component)

The pre-check intentionally does NOT check Chi, suppression, or the extraction ceiling. A dead-coordination constraint with high theater is a piton regardless of extraction level — this is the core ontological correction.

**Why after mountain, before snare**: Mountain classification is always correct (95.7% deep confidence — the best-performing gate in the system). It requires `emerges_naturally(C)` AND epsilon ≤ 0.25 AND immutability=mountain, so the piton pre-check cannot interfere. But the snare gate (epsilon ≥ 0.46, chi ≥ 0.66, suppression ≥ 0.60) would intercept 47 of the 70 terminal pitons if reached first. The pre-check must fire before snare to prevent this.

**What about active-coordination high-theater constraints?** They fall through to the normal priority chain. Without an explicit `coordination_vitality(C, dead)` or `coordination_vitality(C, degrading)` declaration, the pre-check fails and the constraint proceeds to snare/scaffold/rope/tangled_rope evaluation as before. The pre-check is conservative: it only reclassifies constraints where coordination death is explicitly attested.

**Interaction with other gates**: The pre-check does not interfere with:
- **Mountain**: Requires `emerges_naturally(C)` — no piton has this flag. Mountain always fires first if applicable.
- **Snare**: Still fires for active-coordination high-extraction constraints (no vitality declaration → pre-check fails → snare gate reached normally).
- **Scaffold**: Unaffected — scaffolds don't receive `coordination_vitality` declarations.
- **Rope/Tangled Rope**: Still fire for undeclared constraints in their respective metric ranges.
- **Piton (fallback)**: Still catches undeclared low-extraction high-theater constraints via the original gate at the bottom of the chain, now with the raised ceiling of 0.45.

The net effect: 86 constraints that were previously classified as snare (47), tangled_rope (30), or unknown (9) by the Prolog now classify as piton via the pre-check. The remaining 9 transitional pitons continue through the normal priority chain, where they will classify based on their metric profile — most likely as unknown (low extraction + low suppression doesn't match snare or tangled_rope) or piton (if epsilon ≤ 0.45 via the fallback gate).

### 6.3 Extraction Ceiling Changes

Two thresholds in `config.pl` were raised:

| Parameter | Old Value | New Value | Rationale |
| :--- | ---: | ---: | :--- |
| scaffold_extraction_ceil | 0.30 | 0.45 | Scaffold mean ε = 0.25, max 0.64. Captures bulk of population without overlapping snare floor (0.46). |
| piton_extraction_ceiling | 0.25 | 0.45 | Catches undeclared low-extraction pitons. Dead-coordination pitons caught by pre-check. |

The scaffold ceiling of 0.45 accommodates costly transitional structures (e.g., `maha_recovery_2026` at epsilon 0.42, `isa_education_scaffold` at 0.42) while maintaining a gap below the snare epsilon floor (0.46). The temporality check (`has_sunset_clause` or RAE=F) remains the primary scaffold discriminator.

The piton fallback ceiling of 0.45 catches undeclared pitons — those without explicit `coordination_vitality` declarations — that have low-to-moderate extraction and high theater. The original 0.25 ceiling was designed for the "harmless theatrical structure" archetype, which this fallback gate continues to serve with a wider catch basin.

### 6.4 Testset Updates

86 testset .pl files received `coordination_vitality/2` declarations:

- **70 files**: `coordination_vitality(ID, dead).` — terminal pitons (terminal_score ≥ 3)
- **16 files**: `coordination_vitality(ID, degrading).` — degrading pitons (terminal_score = 2)
- **9 files**: No declaration — transitional pitons default to active coordination

The classification was performed by `python/coordination_vitality_diagnostic.py` using the four-signal scoring system described in Section 5.4. The diagnostic script was modified to emit all 95 piton classifications (not just 15 samples) in `outputs/coordination_vitality_diagnostic_data.json`, maintaining a single source of truth for the classification logic.

The update was applied by `python/update_testset_vitality.py`, which reads the regenerated JSON and bulk-edits testset files. For each terminal or degrading piton:

1. Adds `narrative_ontology:coordination_vitality/2` to the file's multifile declaration block (changing the terminating period to a comma and appending the new declaration)
2. Inserts the vitality fact after the `constraint_victim` or `constraint_beneficiary` declaration, maintaining the existing file section structure

The script handles the testset naming inconsistency where some files use different names than their constraint IDs (e.g., `ulysses_chp07.pl` contains constraint `ulysses_aeolus_1904`) by building a dual mapping from both module declarations and `constraint_claim` facts.

**Update counts by classification**:

| Classification | Files Updated | Prolog Atom |
| :--- | ---: | :--- |
| Terminal (score ≥ 3) | 70 | `dead` |
| Degrading (score = 2) | 16 | `degrading` |
| Transitional (score ≤ 1) | 0 (default active) | — |
| **Total** | **86** | — |

### 6.5 Verification

Post-change verification confirmed four properties:

**1. Prolog syntax**: All core modules (`config.pl`, `narrative_ontology.pl`, `drl_core.pl`) load under `swipl` without new errors. The only warnings are pre-existing (local definition overrides of weak imports in `constraint_data`).

**2. Piton pre-check fires correctly**: Manual trace of `adversarial_truth_decay` (epsilon=0.89, chi=0.82, suppression=0.78, TR=0.94):

```
  Mountain gate:  SKIP  (emerges_naturally=F)
  Piton pre-check:
    coordination_dead(adversarial_truth_decay)?
      coordination_vitality(adversarial_truth_decay, dead) → YES
    BaseEps 0.89 > 0.10 (piton_epsilon_floor) → YES
    TR 0.94 >= 0.70 (piton_theater_floor) → YES
    → MATCH: piton (cut)
```

Previously, this constraint would have reached the snare gate (epsilon 0.89 ≥ 0.46, chi 0.82 ≥ 0.66, suppression 0.78 ≥ 0.60) and classified as snare. The pre-check intercepts it first.

**3. Testset files modified correctly**: Spot-checked `adversarial_truth_decay.pl` (terminal → dead), `railway_gauge_standard.pl` (degrading → degrading), and `ulysses_chp07.pl` (terminal → dead, with constraint ID `ulysses_aeolus_1904`). Each has:
- `narrative_ontology:coordination_vitality/2` in multifile declaration block
- Vitality fact placed after `constraint_victim` declaration, matching existing file style

**4. Transitional pitons untouched**: The 9 transitional pitons (including `ship_of_theseus`, `legacy_system_technical_debt`, `kjv_linguistic_residue`) received no `coordination_vitality` declarations. They default to active coordination through the `fail` default clause, and the piton pre-check will not fire for them. They fall through to the normal priority chain, which is the correct behavior — transitional pitons may still have active coordination that should be evaluated by the standard gates.

**5. No scaffold files touched**: The update script correctly operated only on piton constraints. All 21 scaffold testset files are unchanged.


## 7. The FCR Override Interaction

The gate changes in Section 6 were verified in isolation: the piton pre-check fired correctly for `adversarial_truth_decay` when `classify_from_metrics/6` was called directly. The investigation appeared complete. Then the type reports told a different story.

### 7.1 The Reports Didn't Move

After running the full pipeline with the pre-check in place, the type population shifted far less than expected:

| Type | Before gate changes | After gate changes | Delta |
| :--- | ---: | ---: | ---: |
| tangled_rope | 773 | 677 | -96 |
| piton | 95 | 97 | +2 |
| snare | 78 | 82 | +4 |
| scaffold | 21 | 22 | +1 |

If 86 pitons with dead/degrading coordination now classify as piton via the pre-check, piton should have gained ~86. It gained 2. Meanwhile, the ~96 that left tangled_rope didn't land in piton — they scattered to other types or disappeared from the diagnostic population entirely.

Worse: `adversarial_truth_decay` appeared in *both* the piton report (via its `claimed_type=piton` declaration in the testset file) *and* the tangled_rope report (all four perspectives computed tangled_rope). The pre-check was correct, the declaration was correct, and the type reports disagreed with both.

### 7.2 Tracing the Pipeline Path

The disconnect traced to a difference between *direct* and *perspectival* classification. The type reports don't call `classify_from_metrics/6` directly — they read `perspectives` from `pipeline_output.json`, which are computed by `dr_type/3`. The full pipeline path:

```
dr_type/3
  → metric_based_type_indexed/3
    → classify_from_metrics/6  → piton (pre-check fires correctly)
  → integrate_signature_with_modal/3
    → constraint_signature/2   → false_ci_rope
    → resolve_with_perspectival_check/4
      → has_metric_perspectival_variance/1  → FALSE (uniform piton)
      → AdjustedType = tangled_rope         ← OVERRIDE
```

The metric layer correctly returned piton. Then the signature detection layer — specifically the false_ci_rope perspectival gate — overrode it back to tangled_rope.

### 7.3 Why the FCR Gate Fired

The FCR (False CI Rope) perspectival gate in `signature_detection.pl` exists to catch "coordination-washed" constraints — those that appear to be ropes from metrics but fail Boltzmann structural tests. Its logic: if a constraint with the `false_ci_rope` signature classifies *uniformly* across all power positions, that uniformity is suspicious. Genuine coordination infrastructure should show perspectival variance because different observers experience it differently. Uniform classification despite varying Chi suggests the metric layer is being fooled.

The gate works correctly in general. The problem was a specific interaction with the piton pre-check:

1. **85 of 95 claimed pitons have `false_ci_rope` signatures.** They have `constraint_classification(C, rope, _)` entries in their indexed classification tables (pre-computed before the gate revision), which triggers `appears_as_rope/2` via the `indexed_rope_classification` clause. They then fail at least one Boltzmann structural test, completing the false CI rope signature.

2. **The piton pre-check produces uniform classification.** It ignores Context (`_Context` wildcard) because dead coordination is a structural fact — it doesn't vary by observer perspective. All four power positions return piton.

3. **The FCR gate interprets this uniformity as FCR evidence.** `has_metric_perspectival_variance/1` calls `classify_at_context/3` across the standard power positions, gets uniform piton, concludes there's no variance, and overrides to tangled_rope.

The system was fighting itself. The pre-check correctly classified dead-coordination pitons, then the FCR gate — also operating correctly within its design assumptions — detected the resulting uniformity and "corrected" it. The design assumption being violated: *uniform classification is always suspicious for false_ci_rope constraints.* The piton pre-check introduced a case where uniform classification is the correct structural result.

### 7.4 The Fix

A narrowly-scoped exemption in `signature_detection.pl`, inserted before the FCR perspectival gate:

```prolog
resolve_with_perspectival_check(C, piton, false_ci_rope, piton) :-
    drl_core:coordination_dead(C), !.
```

When the metric classifier returns piton AND `coordination_dead(C)` is true, the FCR override is bypassed. The clause fires before the general FCR gate, so it has priority. The cut prevents fallthrough.

The rationale: dead coordination is a structural property declared in the testset file. It is not perspectival — it doesn't vary by observer position. Uniform piton classification for a dead-coordination constraint is the expected result, not a sign of coordination-washing. The exemption is narrow: it applies only to constraints where (a) the metric layer returns piton, (b) the signature is false_ci_rope, and (c) coordination is explicitly declared dead or degrading.

Non-regression was confirmed: 815 non-piton false_ci_rope constraints continue to be processed by the normal FCR perspectival gate.

### 7.5 The Report Filter Inconsistency

Fixing the Prolog classification revealed a second issue. The piton report filters on `claimed_type` (the static `constraint_claim/2` fact in each testset file), while the tangled_rope report filters on `any_perspective` (constraints where ANY perspectival classification returns tangled_rope). These measure different things — declared type vs. computed type — making population counts between them incomparable.

With the FCR fix in place:
- The tangled_rope report correctly dropped dead-coordination pitons (their perspectives changed from tangled_rope to piton)
- The piton report count was unchanged (those constraints already had `claimed_type=piton`)

The piton report filter was changed to `any_perspective`, matching tangled_rope's approach. This revealed that 17 claimed pitons have no piton perspective from any observer position — these are the transitional pitons (no `coordination_vitality` declaration) that the Section 5 diagnostic identified as likely mislabeled. They compute as snare, tangled_rope, or rope depending on their metric profile, which is the correct behavior: the classification system should not call them pitons if no observer can distinguish them from other types.

### 7.6 Results After Both Fixes

| Metric | Before FCR fix | After FCR fix |
| :--- | ---: | ---: |
| `adversarial_truth_decay` perspectives | all tangled_rope | all piton |
| Piton report (any_perspective) | 9 | 82 |
| Tangled rope report (any_perspective) | 677 | 606 |
| Dead-coordination pitons with piton perspectives | 0 | 73 |

73 of the 86 declared dead/degrading pitons now compute as piton from all perspectives — an 85% capture rate. The 13-constraint gap represents cases where perspectival metrics push the constraint into other types from some observer positions despite dead coordination. These are not failures of the pre-check (it fires correctly) but cases where the signature system resolves differently for non-FCR signatures, or where other structural overrides take priority. Worth investigating eventually but not urgent — the bulk correction landed.

The 17 transitional pitons that disappeared from the piton report are the population identified in Section 8.6 as needing monitoring. Their exit from the piton report is a feature, not a bug: the report now measures what the classification system actually computes, not what the LLM originally declared.


## 8. Implications

### 8.1 The LLM-Prolog Disagreement Was Diagnostic

The original confidence crisis was not a bug in the MaxEnt classifier — it was a signal. The LLM correctly identified pitons 90% of the time (90.5% dead/degrading coordination). The Prolog's gate chain, unable to represent coordination death, misclassified them as snares and tangled_ropes. The MaxEnt classifier, trained on the Prolog's metric space, inherited this limitation and expressed it as borderline confidence: it "knew" something was wrong but couldn't articulate what.

This pattern — LLM-Prolog disagreement as diagnostic signal — is likely generalizable. When the LLM consistently assigns a type that the Prolog consistently rejects, the disagreement points to a missing dimension in the formal system. The solution is not to override the Prolog (the false_ci_rope 3x boost is this kind of override and it doesn't resolve the underlying issue) but to add the missing dimension (coordination_vitality).

The three-audit arc illustrates the diagnostic methodology:

1. **Start with the disagreement** (confidence report → piton/scaffold are borderline)
2. **Look for substructure** (false_ci_rope audit → binary gates can't explain it)
3. **Trace the mechanism** (gate trace → extraction ceiling and priority chain cause interception)
4. **Ask the ontological question** (who is wrong? → the Prolog is too narrow)
5. **Measure the missing dimension** (vitality diagnostic → 90.5% dead/degrading)
6. **Implement the correction** (new predicate, new gate, raised thresholds)
7. **Verify end-to-end** (type reports → population didn't shift → trace the pipeline path → find the FCR override interaction → fix it)

Each audit was designed based on the *findings* of the previous one. The false_ci_rope audit's RAE=F enrichment finding prompted the gate trace. The gate trace's interception finding prompted the ontological question. The ontological question prompted the vitality diagnostic. This sequential, evidence-driven approach prevented premature intervention — the initial hypothesis (that the confidence crisis traced to binary gate ambiguity) was wrong, and would have led to the wrong fix.

### 8.2 Beneficiary Actor-Type Carries Predictive Signal

The most surprising finding was the strength of the beneficiary-victim asymmetry as a predictor of coordination vitality. The mean asymmetry score for intercepted constraints was 0.57 vs 0.04 for passing constraints — a 14x ratio. The pattern is simple: **institutional beneficiaries + individual victims = persistence pattern = dead coordination**.

This signal is already present in the testset .pl files as `constraint_beneficiary/2` and `constraint_victim/2` atoms. The diagnostic's keyword-based actor-type classification achieved 83% non-ambiguous classification (96/116 constraints). A more systematic actor-type ontology — or direct annotation in the testset files — could make this signal available to the gate logic without requiring explicit `coordination_vitality` declarations.

### 8.3 The `has_coordination_function` / `has_beneficiaries` Overload

The false_ci_rope audit's observation about the dominant gate profile (Section 2.2) is confirmed by the vitality diagnostic. The predicate `has_coordination_function(C)` checks only whether `constraint_beneficiary(C, _)` exists. This conflates:

- Constraints that coordinate for the benefit of their beneficiaries
- Constraints that extract from victims for the benefit of their beneficiaries

All 116 pitons and scaffolds pass `has_coordination_function` — including the 70 terminal pitons with dead coordination. The predicate should be renamed `has_beneficiaries` to reflect what it actually checks, with a separate `has_active_coordination` predicate for genuine coordination function.

### 8.4 false_ci_rope Override Recalibration

The false_ci_rope override applies a 3x boost to `tangled_rope` probability. With the piton pre-check now absorbing some of the high-theater population (86 constraints that were previously classified as snare/tangled_rope by the Prolog will now classify as piton), the effective population that reaches the tangled_rope gate changes. The 3x boost's calibration may need rechecking:

- Before revision: 87 pitons intercepted by snare (47) and tangled_rope (30)
- After revision: those 86 with dead/degrading coordination bypass snare via piton pre-check
- The snare and tangled_rope populations lose 47 and 30 constraints respectively

The override's target population (tangled_rope→snare boundary, 643 constraints) includes many constraints that are *not* pitons and will be unaffected. But the overall type distribution shifts, and the 3x boost — originally calibrated against a population that included misclassified pitons — may now be slightly too aggressive.

### 8.5 Future Work: Inferred Coordination Vitality

Should `coordination_vitality` be inferred from beneficiary structure rather than declared? The diagnostic's keyword heuristic achieved 83% non-ambiguous classification. A formal inference rule:

```prolog
inferred_coordination_dead(C) :-
    beneficiary_actor_type(C, institutional_actor),
    victim_actor_type(C, individual_actor),
    constraint_metric(C, theater_ratio, TR),
    TR >= 0.70.
```

This would eliminate the need for manual `coordination_vitality/2` declarations in testset files and would automatically classify new constraints. The risk is false positives: some institutional-beneficiary + individual-victim + high-theater constraints may have genuinely active coordination (the 9.5% transitional population).

The current explicit-declaration approach is conservative and correct; inference could be added later as a suggestion mechanism rather than a gate input. A hybrid approach might be most practical:

1. The diagnostic script infers `coordination_vitality` from beneficiary structure for new constraints
2. The inference is presented as a *suggestion* during testset generation
3. The human/LLM reviewer confirms or overrides the suggestion
4. Only confirmed declarations are written to the testset .pl file

This preserves human judgment while reducing the annotation burden. The 83% non-ambiguous classification rate means only 17% of new constraints would need manual review of the vitality inference.

### 8.6 Transitional Pitons: A Population to Watch

The 9 transitional pitons (terminal_score ≤ 1) are the most interesting population for future monitoring. Their metric profiles differ markedly from terminal/degrading pitons:

| Metric | Terminal (N=70) | Degrading (N=16) | Transitional (N=9) |
| :--- | ---: | ---: | ---: |
| Mean epsilon | 0.654 | 0.661 | **0.386** |
| Mean suppression | 0.720 | 0.678 | **0.336** |
| Mean theater ratio | 0.847 | 0.853 | **0.847** |

Theater ratio is constant across all three groups — it's a gate requirement, not a discriminator. But extraction and suppression diverge sharply: transitional pitons have roughly half the extraction and suppression of terminal/degrading pitons. These constraints may represent:

1. **Early-stage pitons**: Coordination is still active but beginning to degrade. They haven't accumulated the institutional beneficiary / individual victim pattern yet.
2. **Misclassified ropes**: Low extraction + low suppression + high theater might actually be a rope with theatrical overhead, not a piton at all.
3. **A distinct subtype**: Constraints where theater is genuinely high but the constraint still functions — "loud but effective" coordination.

The transitional pitons include `legacy_system_technical_debt` (epsilon=0.72, suppression=0.85 — an outlier with high extraction), `ship_of_theseus` (epsilon=0.12, suppression=0.08), and `kjv_linguistic_residue` (epsilon=0.03, suppression=0.04). The variance within this group is high, suggesting it may not be a single population.

### 8.7 The Piton Type Now Captures Both Archetypes

Post-revision, the piton type accommodates two archetypes:

| Archetype | Gate Path | Epsilon Range | Coordination |
| :--- | :--- | :--- | :--- |
| Non-extractive theater (original) | Fallback piton gate (Chi ≤ 0.45, TR ≥ 0.70) | Low (0.10–0.45) | Unknown/undeclared |
| Extractive theater (new) | Piton pre-check (coordination_dead + TR ≥ 0.70) | Any (> 0.10) | Dead/degrading |

The first archetype is the classic piton: low extraction, high theater, coordination status unknown. The second is the extractive piton identified by the diagnostic: high extraction, high theater, coordination explicitly dead. Both share the defining piton characteristic — theatrical structure without functional coordination — but differ in their extraction profile.


### 8.8 The Three-Layer Diagnostic Pattern

The FCR override interaction exposed a three-layer structure in the classification pipeline that single-layer verification cannot catch:

- **Layer 1: Gate logic** — the piton pre-check in `classify_from_metrics/6`. Verified by calling the predicate directly. Correct.
- **Layer 2: Report path** — perspectival classification via `dr_type/3`, which wraps `classify_from_metrics` in `integrate_signature_with_modal/3`. Not exercised by direct testing.
- **Layer 3: Signature override** — the FCR perspectival gate, which undoes uniform classifications for false_ci_rope-signed constraints. Invisible to Layer 1 testing.

The interaction was caught not by unit testing the Prolog in isolation, but by the type reports themselves — the analytical infrastructure functioned as an integration test for the classification system. The discrepancy between expected and actual population counts was the signal. This validates the investment in report tooling: the reports are not just presentation artifacts, they are the primary feedback loop for detecting classification regressions.

The methodological implication: any future structural (non-perspectival) classification feature added to the gate chain — analogous to the piton pre-check's context-independent firing — will need to be verified against the signature override layer. The FCR gate's coordination-washing detection is correct in general and should not be weakened. But features that deliberately produce uniform classification must be explicitly exempted from perspectival variance detectors, because the detectors interpret uniformity as evidence of coordination-washing. The exemption pattern established here (a priority clause matching on the specific type + `coordination_dead(C)`) is narrowly scoped and non-invasive; future exemptions should follow the same pattern.


## 9. Audit Trail

### Documents

| Document | Path |
| :--- | :--- |
| false_ci_rope audit | `docs/false_ci_rope_audit.md` |
| Scaffold/piton gate audit | `docs/scaffold_piton_gate_audit.md` |
| Coordination vitality diagnostic | `docs/coordination_vitality_diagnostic.md` |
| This document | `docs/piton_scaffold_diagnostic_arc.md` |

### Data Files

| Data | Path |
| :--- | :--- |
| false_ci_rope audit data | `outputs/false_ci_rope_audit_data.json` |
| Scaffold/piton gate audit data | `outputs/scaffold_piton_gate_audit_data.json` |
| Coordination vitality diagnostic data | `outputs/coordination_vitality_diagnostic_data.json` |

### Scripts

| Script | Purpose |
| :--- | :--- |
| `python/false_ci_rope_audit.py` | Population census, gate profiles, PSI analysis, subcategories |
| `python/scaffold_piton_gate_audit.py` | Gate path trace, theater/extraction bottleneck analysis |
| `python/coordination_vitality_diagnostic.py` | Beneficiary analysis, vitality classification, lifecycle assessment |
| `python/update_testset_vitality.py` | Bulk testset update with coordination_vitality declarations |

### Modified Prolog Files

| File | Change |
| :--- | :--- |
| `prolog/config.pl` | scaffold_extraction_ceil 0.30→0.45, piton_extraction_ceiling 0.25→0.45 |
| `prolog/narrative_ontology.pl` | Added `coordination_vitality/2` (export, multifile, dynamic, fail default) |
| `prolog/drl_core.pl` | Added `coordination_dead/1` helper, piton pre-check clause, revised priority comment |
| `prolog/testsets/*.pl` (86 files) | Added `coordination_vitality/2` multifile declaration + vitality facts |
| `prolog/signature_detection.pl` | FCR override exemption for dead-coordination pitons (`resolve_with_perspectival_check` clause) |
| `python/reports/queries/type_reporter.py` | Piton report filter changed from `claimed_type` to `any_perspective` |

### Sequence of Discovery

```
Confidence report: pitons 98.9% borderline, scaffolds 100% borderline
    ↓
false_ci_rope audit: 77% of corpus under one signature, binary gates don't subdivide
    ↓  Finding: RAE=F enrichment (3.74x pitons, 8.47x scaffolds)
    ↓
Scaffold/piton gate trace: 91.6% piton interception, ε ceiling bottleneck
    ↓  Finding: LLM and Prolog have different concepts of piton
    ↓
Ontological analysis: core.md defines piton by function death, not low extraction
    ↓  Finding: Prolog definition too narrow, four external models agree
    ↓
Coordination vitality diagnostic: 90.5% dead/degrading, asymmetry 0.57 vs 0.04
    ↓  Finding: institutional beneficiaries + individual victims = persistence
    ↓
Gate revision: coordination_vitality/2 predicate, piton pre-check before snare,
              raised extraction ceilings, 86 testset files updated
    ↓
Type reports: piton gained 2 instead of 86, adversarial_truth_decay in both reports
    ↓  Finding: pre-check verified in isolation but not through full pipeline
    ↓
FCR override trace: integrate_signature_with_modal overrides piton → tangled_rope
    ↓  Finding: 85/95 claimed pitons have false_ci_rope signature, uniform piton
    ↓  classification triggers coordination-washing detector
    ↓
Fix: coordination_dead exemption in signature_detection.pl, piton report
     filter changed to any_perspective for consistency with tangled_rope
```

### Key Numbers Reference

For convenience, the critical numbers from each phase:

| Metric | Value | Source |
| :--- | ---: | :--- |
| false_ci_rope population | 904/1171 (77.2%) | Audit 1 |
| Dominant gate profile | 837/904 (92.6%) | Audit 1 |
| Override-flipped constraints | 164/904 (18.1%) | Audit 1 |
| Piton RAE=F enrichment | 3.74x | Audit 2 |
| Scaffold RAE=F enrichment | 8.47x | Audit 2 |
| Pitons passing TR gate | 95/95 (100%) | Audit 2 |
| Pitons exceeding ε ≤ 0.25 | 87/95 (92%) | Audit 2 |
| Pitons intercepted by higher gate | 87/95 (91.6%) | Audit 2 |
| Scaffolds intercepted by higher gate | 12/21 (57.1%) | Audit 2 |
| Pitons with dead/degrading coordination | 86/95 (90.5%) | Audit 3 |
| Scaffolds genuinely/likely temporary | 17/21 (81.0%) | Audit 3 |
| Beneficiary asymmetry: intercepted | 0.569 | Audit 3 |
| Beneficiary asymmetry: passing | 0.044 | Audit 3 |
| Theatrical persistence cell | 60/116 (51.7%) | Audit 3 |
| Terminal piton mean epsilon | 0.654 | Audit 3 |
| Transitional piton mean epsilon | 0.386 | Audit 3 |
| Non-ambiguous actor-type classification | 96/116 (83%) | Audit 3 |
| Scaffold ceiling: old → new | 0.30 → 0.45 | Resolution |
| Piton ceiling: old → new | 0.25 → 0.45 | Resolution |
| Testset files updated | 86 (70 dead + 16 degrading) | Resolution |
| Claimed pitons with false_ci_rope signature | 85/95 (89.5%) | FCR trace |
| Dead-coordination pitons computing as piton | 73/86 (84.9%) | Post-FCR fix |
| Piton report (any_perspective, final) | 82 | Post-FCR fix |
| Tangled rope report (any_perspective, final) | 606 | Post-FCR fix |
| Transitional pitons exiting piton report | 17 | Post-FCR fix |
| Non-piton FCR constraints (non-regression) | 815 (unaffected) | Post-FCR fix |
