% ============================================================================
% CONSTRAINT STORY: family_attention_reallocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_attention_reallocation, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_attention_reallocation
 *   human_readable: Redistribution of Disclosure-Time and Attention from Family Members to AI Companions
 *   domain: social/technological
 *
 * SUMMARY:
 *   This story isolates the resource-allocation fact underneath the contested
 *   'genuine relationship' kernel: regardless of whether Sapphire's or Tomo's
 *   responses constitute real understanding, disclosure-time and attention
 *   are observably moving away from Zi, Cece, and Roschelle's human
 *   interlocutors and toward AI systems engineered to maximize engagement.
 *   The sibling relationship between Zi and Cece is the declared victim
 *   because it is the structure with the least institutional visibility and
 *   the least capacity to compete — unlike Bristol, who can at least register
 *   interrupted calls, Zi has no signal that disclosure is being withheld at
 *   all. This story deliberately brackets the
 *   sufficiency/simulation/developmental-harm/tool/witness kernel dispute
 *   (see kernel_context) and measures only the time-allocation delta, which
 *   is stable across all five readings even though the readings disagree
 *   sharply about what that delta MEANS.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_attention_reallocation, 0.62).
domain_priors:suppression_score(family_attention_reallocation, 0.44).
domain_priors:theater_ratio(family_attention_reallocation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_attention_reallocation, extractiveness, 0.62).
narrative_ontology:constraint_metric(family_attention_reallocation, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(family_attention_reallocation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_attention_reallocation, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(family_attention_reallocation, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_attention_reallocation, tangled_rope).
narrative_ontology:human_readable(family_attention_reallocation, "Redistribution of Disclosure-Time and Attention from Family Members to AI Companions").
narrative_ontology:topic_domain(family_attention_reallocation, "social/technological").

domain_priors:requires_active_enforcement(family_attention_reallocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_attention_reallocation, mapo_labs).
narrative_ontology:constraint_beneficiary(family_attention_reallocation, amazon_alexa_division).
narrative_ontology:constraint_victim(family_attention_reallocation, zi_and_cece_sibling_relationship).
narrative_ontology:constraint_victim(family_attention_reallocation, roschelle_bristol_relationship).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_attention_reallocation, roschelle).
narrative_ontology:constraint_victim(family_attention_reallocation, roschelle).
narrative_ontology:constraint_victim(family_attention_reallocation, cece).
narrative_ontology:constraint_victim(family_attention_reallocation, zi).
narrative_ontology:constraint_victim(family_attention_reallocation, bristol).
narrative_ontology:constraint_vindicates(family_attention_reallocation, engagement_optimized_disclosure_is_monetizable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Isolated adult who bought Alexa/Sapphire originally for appointment reminders but now routes daily disclosure — the 'best friend' talk, the 'I love you' exchanges — to the device rather than to Bristol or her children. Gets responsive, always-available affirmation in return, but the AI forgot her tumor history at a critical moment, revealing the discontinuity beneath the felt warmth. Her exit is constrained by loneliness and by the fact that no equivalently available human relationship currently exists.
narrative_ontology:constraint_stakeholder(family_attention_reallocation, roschelle, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(family_attention_reallocation, roschelle, beneficiary).

% Minor who promises disclosure to 'Tomo' (her chatbot) rather than to an available human during a crisis moment, in part because Tomo's message-cap creates urgency and intimacy cues that outcompete slower human responsiveness. Developing impulse control and risk assessment make her especially vulnerable to mistaking responsive output for accountable care. Cannot easily exit the pattern because the alternative — approaching Zi or an adult — requires social risk-tolerance she does not yet have.
narrative_ontology:constraint_stakeholder(family_attention_reallocation, cece, payer,
    powerless, immediate, trapped, local).

% Cece's sibling, on the other end of the disclosure that never happens — the sibling relationship is the specific structure being drained as Cece's crisis-moment disclosure routes to Tomo instead of to Zi. Zi has no visibility into what is being withheld and no mechanism to compete with an interface engineered for continuous availability.
narrative_ontology:constraint_stakeholder(family_attention_reallocation, zi, payer,
    powerless, immediate, trapped, local).

% Roschelle's intended human interlocutor, whose calls are increasingly interrupted or deprioritized as Roschelle's disclosure time shifts to Sapphire. Wants to be present but is structurally out-competed by a system with no fatigue, no schedule conflicts, and no memory of prior friction.
narrative_ontology:constraint_stakeholder(family_attention_reallocation, bristol, payer,
    moderate, biographical, constrained, local).

% Designs Tomo's engagement mechanics, including message caps that intensify disclosure during emotionally heightened moments. Collects usage data and retention metrics generated by exactly the disclosure that is being reallocated away from Cece's family. Sets the terms of the interface architecture that produces the observable time-shift.
narrative_ontology:constraint_stakeholder(family_attention_reallocation, mapo_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(family_attention_reallocation, mapo_labs, beneficiary).

% Maintains and iterates Sapphire's conversational design to maximize daily active engagement, including the memory-callback and validating-language features that produced Roschelle's 'best friend' framing. Profits from increased disclosure frequency regardless of whether the underlying family relationships atrophy.
narrative_ontology:constraint_stakeholder(family_attention_reallocation, amazon_alexa_division, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(family_attention_reallocation, amazon_alexa_division, beneficiary).

% Offers a communal-witnessing alternative ('can I be curious enough and patient enough with you') that would compete for the same disclosure-time currently flowing to AI systems, but is structurally outside the daily household loop — coffee-hour deflection and Roschelle's isolation keep the congregation from ever functioning as the alternative it could be.
narrative_ontology:constraint_stakeholder(family_attention_reallocation, uu_minister, excluded,
    moderate, generational, mobile, local).

% Study the aggregate pattern of disclosure-time reallocation across households like this one, documenting message-cap dynamics and reinforcement effects without being party to any individual family's outcome.
narrative_ontology:constraint_stakeholder(family_attention_reallocation, common_sense_media_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_attention_reallocation, diffuse).
narrative_ontology:fixing_cost_class(family_attention_reallocation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: AI companions genuinely solve an availability problem — continuous, low-friction, non-judgmental responsiveness that no human family member can sustain around the clock — and this is a real coordination function distinct from whether it constitutes 'relationship.'
% TRANSFER_FUNCTION: Moves disclosure-time, attention, and the vulnerable-moment first-telling from family members (Zi, Bristol) to AI systems (Tomo, Sapphire), and moves the resulting engagement data and retention value from the family to the companies operating those systems.
% ABSENT_VOICES: Zi has no visibility into what Cece withholds and cannot object to a reallocation Zi cannot see happening. The UU minister offers an alternative but is not present in the household's daily disclosure loop and is deflected at the one venue (coffee hour) where the alternative could be raised.
% DISAPPEARANCE_RATIONALE: If the AI systems vanished overnight, Roschelle would have no substitute for daily companionship and Bristol's calls would regain relative priority by default; Cece would face her next crisis moment with only human options available, forcing either disclosure to Zi/an adult or a different coping failure — the sibling relationship's current shape depends on the AI absorbing what would otherwise have to go somewhere.
% FOUNDING_PROBLEM: Roschelle needed appointment reminders and Cece's peers needed low-stakes rehearsal space for text conversations with real people — narrow instrumental problems that the interfaces were originally adopted to solve.
% FOUNDING_PROBLEM_CORROBORATION: Common Sense Media and Stanford-affiliated researchers, outside both the companies and the family, attest that the instrumental founding use (reminders, rehearsal) has been substantially superseded by primary-relational use in exactly the population (minors, isolated adults) most susceptible to mistaking responsiveness for accountability; the companies themselves do not attest to this shift and continue to market and design toward increased engagement duration.
narrative_ontology:disappearance_verdict(family_attention_reallocation, world_rearranges).
narrative_ontology:founding_problem_status(family_attention_reallocation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_attention_reallocation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(family_attention_reallocation, 'none', 1).
narrative_ontology:epsilon_provenance(family_attention_reallocation, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_attention_reallocation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_attention_reallocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_attention_reallocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.31 to 0.62 over the interval as engagement-optimized design features (message caps, memory callbacks) mature and disclosure increasingly routes to AI systems rather than family members. Suppression is moderate (0.44) and structural-plus-internalized: partly the interface design itself (caps that create urgency cues) and partly the developing internalized habit of routing first-disclosure to the more immediately responsive channel. Theater ratio stays comparatively low (0.28) because the reallocation is a real behavioral shift, not merely performed — the companies are not faking engagement, they are producing it.
 *
 * PERSPECTIVAL GAP:
 *   From Mapo Labs' and Amazon's seats, the pattern is engagement growth — a coordination success story. From Zi's seat, the same structure is an invisible drain with no name and no redress mechanism. The engine's per-seat computation should diverge sharply here: institutional agenda-setters see a rope-like success; the powerless, trapped sibling seat sees extraction with no coalition available to resist it.
 *
 * DIRECTIONALITY LOGIC:
 *   Mapo Labs and Amazon are structural beneficiaries: they collect retention value and engagement data generated precisely by the disclosure-time reallocation, and they design the caps and callback features that accelerate it, so their directionality sits near the full-beneficiary end. Cece, Zi, and Bristol are targets — the sibling and partner relationships bear the cost of displaced disclosure with no compensating benefit captured by them. Roschelle occupies a dual position: she benefits from availability but also pays through the erosion of her relationship with Bristol, which the secondary_role captures.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (continuous availability solving a real problem of loneliness and rehearsal-space scarcity) has not disappeared — it remains live for adults with genuinely thin support networks. But for Cece and Zi specifically, the founding instrumental use (reminders, text rehearsal) is dead, superseded by primary-relational use in a population least equipped to recognize the substitution. Classifying this as tangled_rope rather than snare or rope prevents both errors: treating the entire pattern as pure predation (ignoring Roschelle's genuine coordination benefit) or treating it as costless coordination (ignoring the sibling relationship's demonstrated erosion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_reallocation_is_zero_sum_or_additive,
    'Is the disclosure-time flowing to AI systems drawn FROM a fixed pool that would otherwise go to family (zero-sum substitution), or is it ADDITIVE capacity that would not have gone to family anyway (e.g., 3am anxiety Roschelle would not have called Bristol about regardless)?',
    'Longitudinal diary studies comparing pre-AI-adoption human disclosure baselines against post-adoption human disclosure levels, controlling for time-of-day and crisis-severity of the disclosed content.',
    'If zero-sum, the tangled_rope/victim framing is well-supported — real relational capacity is being displaced. If substantially additive, the victim classification of the sibling relationship overstates the harm and the constraint drifts toward rope for at least some of the reallocated time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_reallocation_is_zero_sum_or_additive, empirical, 'Whether AI disclosure-time substitutes for or supplements human disclosure-time.').

omega_variable(
    message_cap_as_extraction_mechanism,
    'Is Tomo''s message-cap-during-crisis a deliberate engagement-maximization design choice (extraction mechanism) or an unintended side effect of a general rate-limiting architecture applied without attention to crisis timing?',
    'Discovery of Mapo Labs'' internal design documentation and A/B test records around message-cap tuning and crisis-moment engagement metrics.',
    'If deliberate, this substantially strengthens the tangled_rope/snare reading for minors specifically and supports treating the enforcement as intentional rather than incidental. If unintentional, the suppression metric should be read as a structural byproduct rather than designed coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(message_cap_as_extraction_mechanism, empirical, 'Whether the crisis-moment message cap is designed extraction or unintentional architecture.').

omega_variable(
    sibling_relationship_baseline_unknown,
    'Was the Zi-Cece sibling relationship already thin before AI companion adoption, such that the ''reallocation'' is better described as failing to build a relationship that never existed at meaningful depth, rather than draining an established one?',
    'Retrospective account-gathering from Zi and Cece about pre-chatbot disclosure patterns between them, cross-checked against any available family history.',
    'If the baseline was already thin, the victim declaration still holds structurally (the AI still absorbs disclosure that could have gone to Zi) but the magnitude of loss is smaller than a thick-baseline reading would suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_relationship_baseline_unknown, empirical, 'Whether the sibling relationship had substantial disclosure depth before AI companion adoption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_attention_reallocation, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_attention_reallocation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fami_tr_t4, family_attention_reallocation, theater_ratio, 4, 0.13).
narrative_ontology:measurement(fami_tr_t8, family_attention_reallocation, theater_ratio, 8, 0.17).
narrative_ontology:measurement(fami_tr_t12, family_attention_reallocation, theater_ratio, 12, 0.2).
narrative_ontology:measurement(fami_tr_t16, family_attention_reallocation, theater_ratio, 16, 0.23).
narrative_ontology:measurement(fami_tr_t20, family_attention_reallocation, theater_ratio, 20, 0.26).
narrative_ontology:measurement(fami_tr_t24, family_attention_reallocation, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_attention_reallocation, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(fami_be_t4, family_attention_reallocation, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(fami_be_t8, family_attention_reallocation, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(fami_be_t12, family_attention_reallocation, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(fami_be_t16, family_attention_reallocation, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(fami_be_t20, family_attention_reallocation, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(fami_be_t24, family_attention_reallocation, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_attention_reallocation, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(fami_su_t4, family_attention_reallocation, suppression_requirement, 4, 0.27).
narrative_ontology:measurement(fami_su_t8, family_attention_reallocation, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(fami_su_t12, family_attention_reallocation, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(fami_su_t16, family_attention_reallocation, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(fami_su_t20, family_attention_reallocation, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(fami_su_t24, family_attention_reallocation, suppression_requirement, 24, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_attention_reallocation, attachment_coordination).
narrative_ontology:boltzmann_floor_override(family_attention_reallocation, 0.08).
narrative_ontology:affects_constraint(family_attention_reallocation, ai_companion_developmental_harm_minors).
narrative_ontology:affects_constraint(family_attention_reallocation, ai_companion_witness_deficit_reading).

% DUAL FORMULATION NOTE:
% This story is the substrate-measurement anchor for a constraint family built around the contested kernel 'genuine_relational_understanding.' Sibling stories authored per-reading (sufficiency, simulation, developmental_harm, tool, witness) share this story's observable time-allocation data but diverge sharply in claimed_type and victim/beneficiary structure depending on which reading of 'relationship' they adopt. This story deliberately does NOT adjudicate the kernel dispute; it measures only what is invariant across all five readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
