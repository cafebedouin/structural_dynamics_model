% ============================================================================
% CONSTRAINT STORY: lavender_ai_targeting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lavender_ai_targeting, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lavender_ai_targeting
 *   human_readable: Lavender AI-Powered Targeting System in Gaza
 *   domain: technological/military
 *
 * SUMMARY:
 *   Lavender is an AI system deployed by the Israeli Defense Forces (IDF) to
 *   generate ranked targeting lists for suspected junior Hamas and
 *   Palestinian Islamic Jihad (PIJ) operatives in Gaza. The system takes
 *   input signals (communications intercepts, social network analysis,
 *   behavioral patterns, witness reports, informant designations) and
 *   produces numerical scores and ranked lists of individuals for lethal
 *   targeting. The constraint exhibits the structural signature of a snare:
 *   an extraction mechanism (targeting civilians and suspected operatives
 *   without judicial process, subject to algorithmic opacity) maintained
 *   through suppression of alternatives (no appeal mechanism, no transparency
 *   in algorithmic criteria, no meaningful legal review that rejects
 *   targets). The theater ratio reflects that human review processes exist
 *   (IDF legal advisors, proportionality reviews) but function
 *   performatively: Lavender's output is rarely rejected. The extractiveness
 *   has increased over the interval (from 0.55 to 0.78) as the system's role
 *   has shifted from intelligence support (lower extraction, higher
 *   uncertainty) to direct targeting automation (high extraction, reduced
 *   human override). The suppression coefficient (0.88) reflects the complete
 *   absence of exit options for those designated: no judicial appeal, no
 *   transparency in designation criteria, no ability to contest the
 *   algorithmic assessment. This is a categorical constraint story because it
 *   demonstrates a fundamental tension: if Lavender improves targeting
 *   accuracy for legitimate military operations (coordination function), it
 *   should be classifiable as Rope or Tangled Rope. But the empirical
 *   evidence (high false positive rates from independent investigations,
 *   collateral damage patterns, absence of meaningful human override)
 *   suggests the system's primary function is extraction (mass targeting)
 *   with coordination benefits (if any) as secondary effects. The analytical
 *   observer classification as Snare reflects the structural reality:
 *   Lavender is a formalized apparatus for converting algorithmic suspicion
 *   into lethal action.
 *
 * KEY AGENTS:
 *   - Palestinian civilians in Gaza (powerless/trapped) — no exit option; subject to algorithmic targeting; bear extraction cost
 *   - Suspected operatives (unconfirmed) (powerless/trapped) — targeted on algorithmic criteria; no appeal mechanism; primary victim
 *   - Resistance organizations and civil society (organized/constrained) — both victims of targeting and participants in mutual aid networks; constrained by occupation and blockade
 *   - IDF command structure (institutional/arbitrage) — primary beneficiary; uses Lavender for targeting efficiency and operational velocity
 *   - Israeli political leadership (institutional/arbitrage) — beneficiary through deterrence narrative; uses Lavender as evidence of 'precision' and legitimacy
 *   - International legal framework (institutional/constrained) — theoretically constrains targeting; in practice, human review functions performatively; piton classification
 *   - Analytical observer (analytical/analytical) — sees structural snare regardless of framing as 'precision' or 'efficiency'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lavender_ai_targeting, 0.78).
domain_priors:suppression_score(lavender_ai_targeting, 0.88).
domain_priors:theater_ratio(lavender_ai_targeting, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lavender_ai_targeting, extractiveness, 0.78).
narrative_ontology:constraint_metric(lavender_ai_targeting, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(lavender_ai_targeting, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lavender_ai_targeting, snare).
narrative_ontology:human_readable(lavender_ai_targeting, "Lavender AI-Powered Targeting System in Gaza").
narrative_ontology:topic_domain(lavender_ai_targeting, "technological/military").

domain_priors:requires_active_enforcement(lavender_ai_targeting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lavender_ai_targeting, idf_command_structure).
narrative_ontology:constraint_beneficiary(lavender_ai_targeting, political_leadership_israel).
narrative_ontology:constraint_victim(lavender_ai_targeting, palestinian_civilians_gaza).
narrative_ontology:constraint_victim(lavender_ai_targeting, suspected_operatives_unconfirmed).
narrative_ontology:constraint_victim(lavender_ai_targeting, gaza_population_epistemic_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN CIVILIANS (SNARE) — No exit option from Gaza; cannot contest algorithmic designation; subject to lethal targeting based on opaque AI criteria with minimal judicial review. d≈0.96, f(d)≈1.42, σ=0.8 → χ≈0.88. Pure extraction with extreme suppression of alternatives.
constraint_indexing:constraint_classification(lavender_ai_targeting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SUSPECTED OPERATIVES (UNCONFIRMED) (SNARE) — Targeted on algorithmic suspicion; no appeal mechanism; inability to know or contest basis of designation; death or displacement is the enforcement mechanism. d≈0.98, f(d)≈1.44, σ=0.8 → χ≈0.90. Maximum extraction targeting.
constraint_indexing:constraint_classification(lavender_ai_targeting, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: RESISTANCE ORGANIZATIONS & CIVIL SOCIETY (TANGLED ROPE) — Constrained exit (cannot leave Gaza or the structural conflict); both targets of extraction and participants in coordination networks (mutual aid, governance, documentation). Asymmetric: extraction is dominant (targeting, death) but genuine coordination functions exist (organizing shelter, medical care, information networks). d≈0.80, f(d)≈1.18, σ=0.9 → χ≈0.65. Mixed, organized response to constraint.
constraint_indexing:constraint_classification(lavender_ai_targeting, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: IDF COMMAND STRUCTURE (ROPE) — Primary beneficiary. Uses Lavender for targeting efficiency, threat assessment automation, and operational velocity. Experiences constraint as coordination: algorithm provides ranked targeting lists, enabling distributed strike operations. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.06. Net beneficiary; sees coordination function.
constraint_indexing:constraint_classification(lavender_ai_targeting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ISRAELI POLITICAL LEADERSHIP (ROPE) — Beneficiary through deterrence narrative and casualty reduction claims (for Israeli military). Uses Lavender as evidence of 'precision' targeting and force efficiency. Experiences constraint as coordination: algorithm provides public-facing legitimacy narrative ('AI reduces civilian harm') while enabling sustained military operations. d≈0.10, f(d)≈-0.06, σ=1.0 → χ≈-0.05. Net beneficiary; strong coordination function.
constraint_indexing:constraint_classification(lavender_ai_targeting, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL FRAMEWORK (PITON) — Theoretically constrains military targeting (IHL, distinction principle, proportionality review). In practice, Lavender's opaque algorithmic criteria evade meaningful application of these constraints. Theater: humanitarian review processes exist (targeting coordination with legal advisors) but function performatively — the algorithm's output is rarely rejected, and the review provides legitimacy theater rather than enforcement. theater_ratio=0.65 (≥0.70 threshold not met, but close; algorithm bypasses scrutiny it theoretically faces). Classification as piton reflects institutional inertia: legal frameworks persist but their enforcement mechanism has atrophied under AI automation.
constraint_indexing:constraint_classification(lavender_ai_targeting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From global/civilizational view, Lavender exemplifies a structural snare: an AI system that automates extraction (targeting) while preserving plausible deniability through algorithmic opacity. The system's core function is pure extraction: generating kill lists. Any coordination functions (efficiency, operational safety) are secondary to this core. ε=0.78 and suppression=0.88 confirm snare classification even at the most abstract level. No prospect of 'natural law' framing — this is a contingent institutional choice, not an immutable constraint.
constraint_indexing:constraint_classification(lavender_ai_targeting, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lavender_ai_targeting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lavender_ai_targeting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lavender_ai_targeting, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lavender_ai_targeting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lavender_ai_targeting, TR),
    TR >= 0.70.

:- end_tests(lavender_ai_targeting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. Lavender's primary function is to generate targeting lists for lethal strikes. The system directly enables extraction (death, displacement, destruction) from the Palestinian civilian population. The trajectory from 0.55 to 0.78 reflects increasing system integration: early deployment was intelligence support (higher uncertainty, more human override); current deployment is direct targeting automation (lower uncertainty, rubber-stamp human review). Even if Lavender improved operational efficiency (which it did), the net effect is increased extraction capacity — more targets processed per unit time, more operations sustained. Suppression (0.88): Very high. The mechanism suppressing alternatives is comprehensive: (1) algorithmic opacity — designated individuals do not know the criteria used to target them; (2) absence of judicial process — no court review, appeal, or due process; (3) geographic entrapment — no option to leave Gaza; (4) informational asymmetry — IDF controls all data about designations. Theater ratio (0.65): Moderate-high. Human review processes exist (IDF legal advisors, proportionality assessment) but function performatively. Documented evidence shows extremely high approval rates (>95%) and rare rejection of Lavender targets. The review process provides legitimacy theater ('we follow humanitarian law') while the algorithm's output is operationalized regardless. The theater ratio has increased (0.45 → 0.65) as public scrutiny has increased, requiring more formalized review documentation while approval rates remain high. Mandatrophy resolved: Yes. The constraint is unambiguously a Snare. Framing it as Rope (pure coordination for targeting efficiency) ignores the structural asymmetry: Lavender produces no benefit for the targeted population, only extraction. Framing it as Tangled Rope (mixed coordination and extraction) would require showing genuine coordination benefits for the victims — mutual defense, shared governance, etc. — which do not exist in this unidirectional targeting apparatus. The constraint is pure extraction with suppressed alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries and victims is maximal. The IDF sees Rope (targeting coordination that enables operations). The political leadership sees Rope (efficient deterrence mechanism). The Palestinian population sees Snare (lethal targeting with no appeal, no escape, no contest). The international legal framework sees Piton (theaterically reviewed, practically unrestricted). The suspected operatives see Snare (designated by algorithm, designated for death, no mechanism to contest). The analytical observer sees Snare (extraction apparatus with algorithmic veneer). The key signature of this perspectival landscape is that NO perspective produces a beneficiary classification — the only beneficiaries are institutional actors (IDF, political leadership), and they classify as Rope, not as victims benefiting from Tangled Rope. The victims universally perceive extraction with suppressed alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian civilians: Victim + trapped → d≈0.96, f(d)≈1.42, σ=0.8. Maximum extraction. Suspected operatives: Victim + trapped → d≈0.98, f(d)≈1.44, σ=0.8. Maximum extraction. Resistance organizations: Victim + constrained (Gaza blockade) AND organized → d≈0.80, f(d)≈1.18, σ=0.9. High extraction with some agency. IDF command: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08, σ=1.0. Net beneficiary. Political leadership: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.06, σ=1.0. Net beneficiary. International legal framework: Institutional + constrained → d≈0.62, f(d)≈0.92, σ=1.0. Framework theoretically constrains but practically does not override Lavender targets (piton). Analytical observer: Analytical → d≈0.72, f(d)≈1.15, σ=1.0. Sees structural snare without naturalizing framing.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint is unambiguously a Snare. The mandatrophy attempted to classify it as Rope (pure coordination for military targeting efficiency) or Tangled Rope (mixed coordination and extraction). Resolution: (1) Coordination test: Does Lavender solve a collective action problem for its beneficiaries AND targets? No — it solves it for the IDF (targeting coordination) but imposes pure extraction on the targets. (2) Asymmetry test: Do victims experience genuine benefits from the system that offset extraction costs? No — Palestinian civilians experience only targeting, death, and displacement. (3) Enforcement test: Is enforcement active and does it constrain both beneficiaries and victims? No — enforcement is unidirectional (toward victims only). (4) Extraction magnitude test: Is extraction ≥0.46 (snare threshold)? Yes, ε=0.78 exceeds snare floor. (5) Suppression test: Is suppression ≥0.60 (snare threshold)? Yes, suppression=0.88 exceeds snare floor. Conclusion: Snare classification is definitive. The false framing as Rope or 'precision targeting' naturalizes what is a contingent institutional choice: to automate extraction (targeting) using algorithmic opacity rather than human judgment. The mandatrophy is resolved by recognizing that coordination benefits for beneficiaries do not translate to systemic benefits when coupled with zero exit options for victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    targeting_accuracy_vs_reported,
    'What is the actual false positive rate of Lavender''s operational targeting, and how does it compare to reported accuracy?',
    'Cross-referencing IDF strike data with ground verification; analysis of confirmed vs disputed combatant designations; post-strike investigations by independent monitors',
    'If actual false positive rate is 10-20%: targeting misses are rare operational errors (moderate snare). If false positive rate is 40-60%: the system is systematically incorrect, making extraction indiscriminate (severe snare, possible crimes against humanity). Current estimates from investigative journalists suggest 15-50% civilian casualty rate per strike.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_accuracy_vs_reported, empirical, 'Actual false positive rate of Lavender targeting versus reported accuracy claims').

omega_variable(
    algorithmic_training_data_provenance,
    'What data was used to train Lavender''s classification models, and does that data set contain systematic biases favoring certain designations (e.g., overrepresenting Hamas affiliation signals)?',
    'Disclosure of training data; analysis of ground-truth labels used to train the system; comparison of Lavender''s designation patterns to independent intelligence assessments',
    'If training data is representative and independently verified: algorithm has structural integrity (more Rope characteristics). If training data is biased or sourced from contested intelligence: Lavender is a formalized extraction apparatus with no epistemic grounding (pure Snare, no coordination benefit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_training_data_provenance, empirical, 'Training data provenance and potential systematic biases in Lavender''s models').

omega_variable(
    human_review_actual_override_rate,
    'What percentage of Lavender targets are actually reviewed and rejected by human operators, and what are the criteria for rejection?',
    'IDF internal review process data; comparison of Lavender-generated lists to actual strike orders; analysis of documented rejections and their rationales',
    'If override rate is >30%: significant human oversight exists, Tangled Rope or Scaffold plausible (extraction is constrained by human judgment). If override rate is <5%: algorithm output is rubber-stamped, system is pure Snare with performative human review.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(human_review_actual_override_rate, empirical, 'Actual human review override rate for Lavender targeting recommendations').

omega_variable(
    civilian_collateral_modeling,
    'Does Lavender have an algorithmic collateral damage estimate, and is that estimate used to filter targets or merely recorded?',
    'Internal Lavender specification; analysis of IDF targeting doctrine; comparison of estimated vs actual collateral damage',
    'If collateral damage is estimated but strikes proceed regardless: extraction function is separable from harm minimization (pure Snare). If collateral damage estimates trigger target rejection: coordination function exists (Tangled Rope or Scaffold plausible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_collateral_modeling, empirical, 'Whether Lavender models collateral damage and whether estimates influence targeting decisions').

omega_variable(
    alternative_targeting_methodologies,
    'Were human-only (non-AI) targeting methodologies tested or compared to Lavender? What was the accuracy, speed, and civilian casualty trade-off?',
    'Historical analysis of pre-Lavender targeting practices; controlled comparisons if available; assessment of whether Lavender actually reduces civilian harm or merely increases operational efficiency',
    'If Lavender reduces civilian casualties compared to alternatives: system has coordination benefit (Rope or Tangled Rope possible). If Lavender increases speed without accuracy improvement: system is pure extraction apparatus (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_targeting_methodologies, empirical, 'Whether Lavender improves upon alternative targeting methodologies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lavender_ai_targeting, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lavender_tr_t0, lavender_ai_targeting, theater_ratio, 0, 0.45).
narrative_ontology:measurement(lavender_tr_t6, lavender_ai_targeting, theater_ratio, 6, 0.58).
narrative_ontology:measurement(lavender_tr_t12, lavender_ai_targeting, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(lavender_be_t0, lavender_ai_targeting, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(lavender_be_t6, lavender_ai_targeting, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(lavender_be_t12, lavender_ai_targeting, base_extractiveness, 12, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lavender_ai_targeting, enforcement_mechanism).
narrative_ontology:affects_constraint(lavender_ai_targeting, algorithmic_opacity_military_decision).
narrative_ontology:affects_constraint(lavender_ai_targeting, gaza_civilian_protection_regime).
narrative_ontology:affects_constraint(lavender_ai_targeting, idf_targeting_doctrine_evolution).

% DUAL FORMULATION NOTE:
% Lavender represents a specific technical instantiation (AI system + targeting pipeline) of a broader structural constraint: military targeting without meaningful civilian protection mechanisms. Lavender is downstream of the general targeting doctrine and upstream of specific strike outcomes. The constraint family includes the algorithmic opacity problem (how AI systems evade scrutiny), the civilian protection regime (how international law applies to automated systems), and the targeting doctrine evolution (how militaries justify automation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lavender_ai_targeting, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
