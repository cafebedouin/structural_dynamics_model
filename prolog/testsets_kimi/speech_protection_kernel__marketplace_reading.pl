% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__marketplace_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Marketplace of Ideas Reading of Speech Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The marketplace reading of the First Amendment speech clause holds that
 *   speech protection serves collective truth-discovery: false or harmful
 *   speech should be countered by 'more speech,' not by state censorship.
 *   Associated with Holmes and Brandeis, this reading treats content-based
 *   restrictions as epistemically dangerous distortions of a competitive
 *   truth-market. As constitutional doctrine, it coordinates a vast domain of
 *   public discourse by delegating truth-validation to unregulated
 *   competition. However, it also structurally advantages actors with
 *   pre-existing capacity to generate speech and reach audiences, while
 *   imposing costs on those targeted by hate speech, disinformation, or
 *   harassment who lack comparable resources for counterspeech. This story
 *   instantiates the marketplace_reading of the speech_protection_kernel,
 *   distinct from absolutist, democratic-participation, harm-threshold, and
 *   dignity readings.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: agenda_setter (institutional/constrained) â administers and enforces the doctrine through judicial review and precedent
 *   - high_capacity_speakers: beneficiary (powerful/mobile) â dominate discourse under content-neutrality rules and the 'more speech' remedy
 *   - engagement_platforms: beneficiary (institutional/arbitrage) â monetize engagement amplified by liability shields and content-neutrality norms
 *   - structurally_silenced_groups: payer (powerless/constrained) â bear costs of protected harmful speech without effective counterspeech capacity
 *   - state_legislators: payer (institutional/constrained) â lose democratic authority to regulate speech under marketplace-based review
 *   - legal_academics: observer (analytical) â evaluate empirical validity of the truth-discovery premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.62).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.55).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Marketplace of Ideas Reading of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '15b48d81-8922-41b6-9980-0dc95a1c2ffc').
narrative_ontology:cs_kernel_codification('15b48d81-8922-41b6-9980-0dc95a1c2ffc', fixed_text).
narrative_ontology:cs_authority_grounding('15b48d81-8922-41b6-9980-0dc95a1c2ffc', lineage).
narrative_ontology:cs_interpretation_layer_present('15b48d81-8922-41b6-9980-0dc95a1c2ffc').
narrative_ontology:cs_reading_relation('15b48d81-8922-41b6-9980-0dc95a1c2ffc', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('15b48d81-8922-41b6-9980-0dc95a1c2ffc', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('15b48d81-8922-41b6-9980-0dc95a1c2ffc', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('15b48d81-8922-41b6-9980-0dc95a1c2ffc', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_axiom('15b48d81-8922-41b6-9980-0dc95a1c2ffc', foundational, truth_through_adversarial_exchange).
narrative_ontology:cs_axiom_status(truth_through_adversarial_exchange, holdable).
narrative_ontology:cs_axiom_grounding('15b48d81-8922-41b6-9980-0dc95a1c2ffc', truth_through_adversarial_exchange, empirically_contingent).
narrative_ontology:cs_axiom('15b48d81-8922-41b6-9980-0dc95a1c2ffc', foundational, state_content_neutrality_imperative).
narrative_ontology:cs_axiom_status(state_content_neutrality_imperative, holdable).
narrative_ontology:cs_axiom_grounding('15b48d81-8922-41b6-9980-0dc95a1c2ffc', state_content_neutrality_imperative, instrumental).
narrative_ontology:cs_reference_frame('15b48d81-8922-41b6-9980-0dc95a1c2ffc', competitive_epistemic_market).
narrative_ontology:cs_drift_state('15b48d81-8922-41b6-9980-0dc95a1c2ffc', digital_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('15b48d81-8922-41b6-9980-0dc95a1c2ffc', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, high_capacity_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, engagement_platforms).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, structurally_silenced_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, state_legislators).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, truth_through_competition_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, content_neutrality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the First Amendment through the marketplace-of-ideas framework; strikes down content-based speech restrictions; maintains doctrinal stability through precedent while framing speech protection as serving collective truth-discovery.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Media conglomerates, well-funded political operations, and institutional voices that can dominate public discourse through volume, reach, and repetition; benefit directly from rules that prohibit content-based regulation and from the 'more speech' remedy that privileges capacity over accuracy.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, high_capacity_speakers, beneficiary,
    powerful, biographical, mobile, national).

% Algorithmic distribution systems that monetize attention and engagement; benefit from liability shields and content-neutrality norms that prevent targeted regulation of harmful but high-engagement speech, using marketplace doctrine to resist content-based obligations.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, engagement_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Marginalized communities and individuals targeted by hate speech, harassment, and disinformation who lack the resources to generate effective counterspeech; bear the costs of a system that protects harmful expression on the theory that truth emerges from open competition.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, structurally_silenced_groups, payer,
    powerless, biographical, constrained, national).

% Elected officials seeking to regulate disinformation, campaign finance, or targeted harassment who are blocked by marketplace-based judicial review; lose democratic policymaking space to a judiciary-administered epistemic theory.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, state_legislators, payer,
    institutional, generational, constrained, national).

% Scholars who study, critique, and defend the marketplace rationale; produce empirical research on whether unregulated discourse in digital environments actually produces truth or instead amplifies falsehood and asymmetry.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, legal_academics, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In a pluralistic society without an established epistemic authority, how do citizens collectively determine which beliefs are true? The arrangement delegates truth-validation to decentralized, adversarial public discourse by preventing state actors from suppressing disfavored viewpoints.
% TRANSFER_FUNCTION: Transfers epistemic authority from state regulators to the aggregate outcome of unregulated speech competition; transfers the costs of false and harmful speech to targeted groups and the general public while concentrating discourse advantages on actors with pre-existing reach and resources.
% ABSENT_VOICES: Targets of disinformation and harassment who lack the capital to mount effective counterspeech campaigns; comparative constitutionalists operating in dignity-based or harm-threshold frameworks who have been structurally excluded from US doctrinal conversation; state regulators whose democratic mandates are overridden by judicial epistemic theory.
% DISAPPEARANCE_RATIONALE: If the marketplace reading vanished overnight, the doctrinal basis for striking down content-based speech restrictions would collapse; legislatures would gain broader authority to regulate disinformation, hate speech, and campaign finance; platforms would face altered liability and content-moderation incentives; and the architecture of public discourse would reorganize around dignity, harm, or democratic-participation criteria rather than competitive truth-discovery.
% FOUNDING_PROBLEM: In a democratic society without an official church or state ideology, how do citizens distinguish true beliefs from false ones without empowering government to enforce orthodoxy?
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and critical scholars outside the immediate beneficiary set attest that the contemporary threat to collective truth-discovery is less state censorship and more private platform power, algorithmic amplification, and asymmetric speaker capacity; no neutral empirical source attests that unregulated digital discourse currently functions as a truth-discovery mechanism.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) is moderate-high: the doctrine genuinely coordinates epistemic decentralization, but the 'marketplace' is structurally skewed toward actors with greater resources and reach, making the coordination function partially extractive. Suppression (0.55) reflects the active judicial enforcement required to invalidate content-based laws and maintain the doctrinal boundary. Theater ratio (0.40) has risen over the interval: as empirical conditions shifted toward algorithmic amplification and asymmetric capacity, the 'more speech' remedy increasingly functions as performative legitimation rather than an effective countermechanism. Accessibility collapse (0.70) is high because alternatives to marketplace logic (dignity, harm-threshold) are largely foreclosed in US constitutional doctrine once the framework is accepted. Resistance (0.60) reflects sustained scholarly, legislative, and social-movement opposition to the doctrine's distributional consequences.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter judiciary and beneficiary speakers experience the constraint as genuine coordination protecting democratic discourse from state overreach; the payer seats (silenced groups, constrained legislators) experience the identical structure as enforced extraction that externalizes the costs of harmful speech and strips democratic communities of self-regulatory capacity. The engine computes this divergence from the structural data â the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   High-capacity speakers and engagement platforms are structural beneficiaries (low directionality): the constraint subsidizes their capacity to speak by prohibiting content-based leveling and by shielding them from liability. Structurally silenced groups are structural targets (high directionality): they bear the costs of protected harmful speech and lack the resources to exercise the 'more speech' remedy the doctrine promises. The judiciary sits near symmetric: it neither collects rents nor bears the direct costs of harmful speech, but it pays in institutional legitimacy when the doctrine's empirical premises fail.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled rope prevents the false binary of treating the doctrine as either pure coordination (rope) or pure extraction (snare). The marketplace reading does solve a real coordination problem â how to discover truth without state orthodoxy â but it simultaneously extracts by allowing concentrated speech capacity to dominate. Mandatrophy would occur if the truth-discovery function atrophied entirely while the doctrine persisted by inertia; the temporal measurements show rising theater_ratio but not yet piton levels, suggesting partial atrophy rather than full mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_truth_emergence,
    'Does unregulated adversarial speech in contemporary digital environments actually produce collective truth-discovery, or does algorithmic amplification and asymmetric capacity produce systematic falsehood dominance?',
    'Comparative platform studies, social epistemology research, and empirical analysis of information diffusion in high-engagement media ecosystems.',
    'If the empirical premise is false, the coordination function is hollow and the constraint''s extraction dominates, pushing classification toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_truth_emergence, empirical, 'Empirical validity of truth-discovery premise in digital speech environments').

omega_variable(
    speaker_capacity_asymmetry,
    'Does the ''more speech'' remedy function as a real corrective mechanism when speaker capacity is massively asymmetric, or has it become a structural cover for extraction by dominant voices?',
    'Economic and network analysis of reach, attention markets, and the cost-structure of effective counterspeech.',
    'If counterspeech is structurally illusory for resource-poor actors, the doctrine''s core remedy is performative theater, raising theater_ratio and extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(speaker_capacity_asymmetry, empirical, 'Structural viability of counterspeech remedy under capacity asymmetry').

omega_variable(
    kernel_reading_contest,
    'Is the marketplace reading a live competitor to dignity and harm-threshold readings, or has practice drift made it anachronistic within the broader speech_protection_kernel?',
    'Comparative constitutional adoption patterns, doctrinal citation analysis, and jurisprudential tracking of whether newer readings are gaining institutional traction.',
    'If the marketplace reading is losing institutional viability while persisting doctrinally, this would indicate commitment-system drift and potential axiom-overriding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested status of marketplace reading within the speech protection kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t14, speech_protection_kernel__marketplace_reading, theater_ratio, 14, 0.18).
narrative_ontology:measurement(spee_tr_t28, speech_protection_kernel__marketplace_reading, theater_ratio, 28, 0.22).
narrative_ontology:measurement(spee_tr_t42, speech_protection_kernel__marketplace_reading, theater_ratio, 42, 0.28).
narrative_ontology:measurement(spee_tr_t56, speech_protection_kernel__marketplace_reading, theater_ratio, 56, 0.34).
narrative_ontology:measurement(spee_tr_t70, speech_protection_kernel__marketplace_reading, theater_ratio, 70, 0.4).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(spee_be_t14, speech_protection_kernel__marketplace_reading, base_extractiveness, 14, 0.38).
narrative_ontology:measurement(spee_be_t28, speech_protection_kernel__marketplace_reading, base_extractiveness, 28, 0.45).
narrative_ontology:measurement(spee_be_t42, speech_protection_kernel__marketplace_reading, base_extractiveness, 42, 0.52).
narrative_ontology:measurement(spee_be_t56, speech_protection_kernel__marketplace_reading, base_extractiveness, 56, 0.58).
narrative_ontology:measurement(spee_be_t70, speech_protection_kernel__marketplace_reading, base_extractiveness, 70, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(spee_su_t14, speech_protection_kernel__marketplace_reading, suppression_requirement, 14, 0.42).
narrative_ontology:measurement(spee_su_t28, speech_protection_kernel__marketplace_reading, suppression_requirement, 28, 0.45).
narrative_ontology:measurement(spee_su_t42, speech_protection_kernel__marketplace_reading, suppression_requirement, 42, 0.48).
narrative_ontology:measurement(spee_su_t56, speech_protection_kernel__marketplace_reading, suppression_requirement, 56, 0.52).
narrative_ontology:measurement(spee_su_t70, speech_protection_kernel__marketplace_reading, suppression_requirement, 70, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel decomposes into multiple structurally distinct constraints because the same constitutional text and doctrinal label cover divergent normative claims: absolutist (autonomy-based), marketplace (epistemic), democratic-participation (self-governance-based), harm-threshold (consequentialist), and dignity (relational/subordination-based). Each reading has distinct beneficiary/victim structures, empirical premises, and extraction profiles. This story covers only the marketplace reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
