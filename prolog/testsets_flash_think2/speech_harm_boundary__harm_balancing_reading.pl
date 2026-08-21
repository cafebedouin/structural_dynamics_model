% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Harm Proportionality Balancing
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint is the `harm_balancing_reading` of the
 *   `speech_harm_boundary` kernel. It posits that while speech is
 *   presumptively protected, this protection yields to demonstrable harm,
 *   requiring a proportionality balancing test. This reading contrasts with
 *   the `absolutist_reading` (which prioritizes speech above almost all harm)
 *   and the `dignity_reading` (which subordinates speech to human dignity).
 *   The constraint is claimed as a `tangled_rope` because it genuinely
 *   coordinates free expression but also extracts costs from speakers of
 *   harmful speech through active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.55).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.6).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Harm Proportionality Balancing").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, 'fc99eef8-637b-4a4f-9b02-75fffa5130d1').
narrative_ontology:cs_kernel_codification('fc99eef8-637b-4a4f-9b02-75fffa5130d1', formalized).
narrative_ontology:cs_authority_grounding('fc99eef8-637b-4a4f-9b02-75fffa5130d1', lineage).
narrative_ontology:cs_interpretation_layer_present('fc99eef8-637b-4a4f-9b02-75fffa5130d1').
narrative_ontology:cs_reading_relation('fc99eef8-637b-4a4f-9b02-75fffa5130d1', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc99eef8-637b-4a4f-9b02-75fffa5130d1', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('fc99eef8-637b-4a4f-9b02-75fffa5130d1', foundational, speech_is_presumptively_protected).
narrative_ontology:cs_axiom_status(speech_is_presumptively_protected, holdable).
narrative_ontology:cs_axiom_grounding('fc99eef8-637b-4a4f-9b02-75fffa5130d1', speech_is_presumptively_protected, deontological).
narrative_ontology:cs_axiom('fc99eef8-637b-4a4f-9b02-75fffa5130d1', foundational, demonstrable_harm_justifies_proportional_restriction).
narrative_ontology:cs_axiom_status(demonstrable_harm_justifies_proportional_restriction, holdable).
narrative_ontology:cs_axiom_grounding('fc99eef8-637b-4a4f-9b02-75fffa5130d1', demonstrable_harm_justifies_proportional_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('fc99eef8-637b-4a4f-9b02-75fffa5130d1', liberal_democratic_balancing_tradition).
narrative_ontology:cs_drift_state('fc99eef8-637b-4a4f-9b02-75fffa5130d1', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fc99eef8-637b-4a4f-9b02-75fffa5130d1', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, public_discourse).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, vulnerable_groups).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, free_speech_advocates).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, victims_of_harmful_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or groups whose speech is deemed to cause demonstrable harm, leading to legal restrictions, self-censorship, or social penalties. They bear the direct costs of these restrictions.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_speech, payer,
    powerless, immediate, constrained, global).

% Groups or individuals who are frequent targets of harmful speech (e.g., hate speech, harassment). They benefit from the legal and social protections afforded by this balancing framework, reducing their exposure to harm.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, vulnerable_groups, beneficiary,
    powerless, biographical, trapped, local).

% Judicial bodies and regulatory agencies responsible for interpreting and applying the harm balancing test. They define what constitutes demonstrable harm and ensure proportionality in restrictions, actively enforcing the constraint.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% The abstract realm of public communication and idea exchange. It benefits from a framework that allows robust debate while mitigating the most destructive forms of speech, fostering a more inclusive and productive environment.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, public_discourse, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(speech_harm_boundary__harm_balancing_reading, public_discourse).

% Organizations and individuals who champion broad free speech protections. They often challenge restrictions, bearing the costs of litigation and public advocacy, and perceive the balancing act as potentially over-restrictive.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, free_speech_advocates, payer,
    organized, biographical, mobile, national).

% Individuals who directly experience the negative consequences of harmful speech. While the framework aims to protect them, they still bear the initial impact of harm before legal remedies or social consequences can be applied.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, victims_of_harmful_speech, payer,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__harm_balancing_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the fundamental right to free expression with the imperative to protect individuals and groups from demonstrable harm, aiming to foster a public sphere where ideas can be exchanged without undue risk of injury or social fragmentation.
% TRANSFER_FUNCTION: Transfers the cost of speech restriction (e.g., legal defense, self-censorship, social opprobrium) from potential victims of harm to speakers whose expression is deemed harmful, while transferring the benefit of a safer, more inclusive public sphere to vulnerable groups and society at large.
% ABSENT_VOICES: Those who advocate for an absolute right to speech, regardless of harm, or those who believe any potential for offense should trigger restriction. Their perspectives are often debated but are not fully integrated into the current proportionality balancing framework.
% DISAPPEARANCE_RATIONALE: If this balancing framework vanished overnight, society would either descend into unchecked harmful speech (leading to social fragmentation, incitement to violence, and erosion of trust) or over-restrictive censorship (stifling legitimate expression and critical dissent), fundamentally altering the nature of public and private communication and civic life.
% FOUNDING_PROBLEM: How to reconcile the fundamental right to free expression with the equally fundamental need to protect individuals and communities from speech that causes direct, demonstrable harm, preventing both the tyranny of the majority and the tyranny of the minority in the public sphere.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and victims' advocacy groups consistently attest to the ongoing challenge of balancing these rights, citing numerous contemporary cases of speech-related harm (especially in digital contexts) and calls for its regulation, demonstrating the problem remains active and contested.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` (0.55) is moderate because speakers bear real costs when their speech is restricted, but the system also provides a framework for legitimate expression. `Suppression` (0.60) is moderate, reflecting the active enforcement by courts and regulators to restrict harmful speech. `Theater_ratio` (0.15) is low, indicating that the balancing act is a genuine, ongoing effort, not merely performative. The increasing `extractiveness` and `suppression_requirement` over time reflect growing societal awareness of speech-related harms (especially online) and the corresponding expansion of legal and social mechanisms to address them.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of `vulnerable_groups`, the constraint is a vital `rope` or `scaffold` providing necessary protection. From the `speakers_of_harmful_speech` or `free_speech_advocates` seats, it can feel more like a `snare` or `tangled_rope` due to the restrictions and costs imposed. The engine's per-seat classification will capture this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers of harmful speech are the primary targets (`payer`, `powerless`, `constrained` exit), bearing the costs of restriction. Vulnerable groups and public discourse are beneficiaries, gaining protection and a more constructive environment. Courts and regulators act as `agenda_setters`, defining and enforcing the boundaries. Free speech advocates, while often aligned with broad expression, can also be `payers` when they bear the costs of challenging restrictions they deem excessive.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolutist_reading_divergence,
    'How would the classification of speech protection change under an `absolutist_reading` of the `speech_harm_boundary` kernel?',
    'Analysis of legal systems or philosophical frameworks that adopt an absolutist stance, focusing on their treatment of ''harm'' and ''restriction''.',
    'An `absolutist_reading` would likely result in significantly lower `extractiveness` and `suppression` for speakers, potentially classifying the constraint as a `rope` or even `mountain` for speech, but with higher costs for potential victims of harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_reading_divergence, conceptual, 'Structural differences between harm-balancing and absolutist approaches to speech.').

omega_variable(
    dignity_reading_divergence,
    'How would the classification of speech protection change under a `dignity_reading` of the `speech_harm_boundary` kernel?',
    'Analysis of legal systems or philosophical frameworks that prioritize human dignity, focusing on how they define ''dignity-violating speech'' and its categorical exclusion from protection.',
    'A `dignity_reading` would likely result in higher `suppression` and `extractiveness` for certain categories of speech (e.g., hate speech), potentially classifying the constraint as a `snare` for such speech, but with stronger protections for human dignity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_reading_divergence, conceptual, 'Structural differences between harm-balancing and dignity-based approaches to speech.').

omega_variable(
    subjectivity_of_harm_and_proportionality,
    'To what extent is the determination of ''demonstrable harm'' and ''proportionality'' subjective or open to political manipulation, rather than objectively applied?',
    'Empirical studies of judicial consistency in applying harm tests, analysis of political influence on regulatory bodies, and cross-jurisdictional comparisons of speech restriction outcomes.',
    'If highly subjective or manipulable, the `extractiveness` and `suppression` could be effectively higher for disfavored speakers, pushing the constraint closer to a `snare` or `tangled_rope` with a higher `theater_ratio` (if the balancing is merely a cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subjectivity_of_harm_and_proportionality, empirical, 'Ambiguity in applying harm and proportionality tests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1990, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(spee_tr_t1996, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1996, 0.15).
narrative_ontology:measurement(spee_tr_t2002, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2002, 0.15).
narrative_ontology:measurement(spee_tr_t2008, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(spee_tr_t2014, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(spee_tr_t2020, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(spee_be_t1990, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(spee_be_t1996, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1996, 0.48).
narrative_ontology:measurement(spee_be_t2002, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2002, 0.5).
narrative_ontology:measurement(spee_be_t2008, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2008, 0.52).
narrative_ontology:measurement(spee_be_t2014, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2014, 0.54).
narrative_ontology:measurement(spee_be_t2020, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2020, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1990, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(spee_su_t1996, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1996, 0.53).
narrative_ontology:measurement(spee_su_t2002, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2002, 0.56).
narrative_ontology:measurement(spee_su_t2008, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(spee_su_t2014, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2014, 0.59).
narrative_ontology:measurement(spee_su_t2020, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
