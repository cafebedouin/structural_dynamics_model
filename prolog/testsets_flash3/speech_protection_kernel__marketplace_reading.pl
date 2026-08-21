% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Speech Protection: Marketplace of Ideas Reading
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'marketplace of ideas' reading of speech
 *   protection, where the primary justification for free speech is its
 *   instrumental value in discovering truth and advancing knowledge. It
 *   posits that all ideas, even false or harmful ones, should be allowed to
 *   compete in the public sphere, with truth ultimately prevailing through
 *   rational discourse and 'more speech.' Content-based restrictions are
 *   viewed with extreme skepticism as they distort this epistemic process.
 *   This is one reading of the broader 'speech_protection_kernel'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.25).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.1).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Speech Protection: Marketplace of Ideas Reading").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy/communication_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '8f12be02-10de-4002-a6e1-78fe176abc4c').
narrative_ontology:cs_kernel_codification('8f12be02-10de-4002-a6e1-78fe176abc4c', fixed_text).
narrative_ontology:cs_authority_grounding('8f12be02-10de-4002-a6e1-78fe176abc4c', lineage).
narrative_ontology:cs_interpretation_layer_present('8f12be02-10de-4002-a6e1-78fe176abc4c').
narrative_ontology:cs_reading_relation('8f12be02-10de-4002-a6e1-78fe176abc4c', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f12be02-10de-4002-a6e1-78fe176abc4c', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f12be02-10de-4002-a6e1-78fe176abc4c', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f12be02-10de-4002-a6e1-78fe176abc4c', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('8f12be02-10de-4002-a6e1-78fe176abc4c', foundational, truth_emerges_from_free_exchange).
narrative_ontology:cs_axiom_status(truth_emerges_from_free_exchange, holdable).
narrative_ontology:cs_axiom_grounding('8f12be02-10de-4002-a6e1-78fe176abc4c', truth_emerges_from_free_exchange, empirically_contingent).
narrative_ontology:cs_axiom('8f12be02-10de-4002-a6e1-78fe176abc4c', foundational, content_based_restrictions_distort_truth).
narrative_ontology:cs_axiom_status(content_based_restrictions_distort_truth, holdable).
narrative_ontology:cs_axiom_grounding('8f12be02-10de-4002-a6e1-78fe176abc4c', content_based_restrictions_distort_truth, instrumental).
narrative_ontology:cs_reference_frame('8f12be02-10de-4002-a6e1-78fe176abc4c', millian_epistemic_optimism).
narrative_ontology:cs_drift_state('8f12be02-10de-4002-a6e1-78fe176abc4c', contemporary_disinformation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8f12be02-10de-4002-a6e1-78fe176abc4c', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, public_discourse).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, truth_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, listeners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the free exchange of ideas, which is believed to lead to the discovery of truth and the robust testing of propositions. The quality of public discourse is seen as improving through this process.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, public_discourse, beneficiary,
    institutional, generational, constrained, national).

% Individuals and groups who seek to understand complex issues and arrive at accurate conclusions. They benefit from a wide array of viewpoints being available for consideration and critique.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, truth_seekers, beneficiary,
    moderate, biographical, mobile, global).

% Individuals or groups expressing ideas, including those that may be unpopular, false, or even harmful. They are protected from content-based restrictions, allowing their speech to enter the marketplace.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, speakers, beneficiary,
    moderate, immediate, mobile, local).

% Individuals who receive and evaluate speech. They are expected to critically assess information and engage in 'more speech' to counter falsehoods, rather than relying on censorship.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, listeners, beneficiary,
    moderate, immediate, mobile, local).

% Individuals or groups who suffer direct harm (e.g., defamation, incitement to violence, harassment) from speech that is protected under this reading. Their harm is considered a necessary byproduct of the truth-discovery process, to be addressed by counter-speech rather than restriction.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, victims_of_harmful_speech, excluded,
    powerless, biographical, trapped, local).

% Government entities tasked with balancing free speech with other societal interests. Under this reading, their role in regulating speech content is severely limited, primarily to time, place, and manner restrictions, not viewpoint suppression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, state_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective pursuit of truth and robust public deliberation by ensuring a wide range of ideas can be expressed and debated without content-based government interference.
% TRANSFER_FUNCTION: Transfers the burden of discerning truth from government censors to individual citizens, who are expected to engage in critical evaluation and counter-speech. It also transfers the risk of harm from false or offensive speech to individuals and groups, rather than allowing the state to preemptively restrict it.
% ABSENT_VOICES: Victims of harmful speech are structurally excluded from having their harm recognized as a basis for content restriction; their perspective is subordinated to the collective epistemic goal. Those who advocate for a more protective role for the state in safeguarding vulnerable groups are also marginalized.
% DISAPPEARANCE_RATIONALE: If this reading of speech protection vanished, the legal landscape for speech would fundamentally shift. Content-based restrictions would become more permissible, potentially leading to a more regulated and less diverse public discourse, but also potentially reducing certain forms of speech-related harm. The balance of power between speakers, listeners, and the state would be profoundly altered.
% FOUNDING_PROBLEM: The problem of how to ensure a society arrives at truth and makes informed decisions, avoiding the suppression of valuable ideas and the entrenchment of error, particularly in the face of government power.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading, including many legal scholars and civil liberties advocates, argue that the problem of truth-discovery and preventing government overreach in speech remains live. Critics, however, contend that the 'marketplace' metaphor is flawed and that the problem has evolved to include issues of disinformation and structural harm that this reading fails to address, leading to a contested status.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).
:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint primarily aims for collective epistemic benefit, not direct material extraction from specific parties. Any 'cost' is diffuse, borne by society in the form of tolerating potentially harmful speech for the sake of truth-discovery. Suppression is low (0.1) as the core tenet is against state suppression of speech content. Theater ratio is very low (0.05) as the function is genuinely believed to be active, though its efficacy is increasingly debated. Accessibility collapse is high (0.7) because once this principle is accepted, alternatives like content-based regulation are largely foreclosed. Resistance is low (0.15) from within this framework, as the principle is widely accepted, though external critiques are growing.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public discourse and truth-seekers, this reading is a pure Rope, facilitating a vital collective good. From the perspective of victims of harmful speech, it can feel like a Snare, as their suffering is dismissed as a necessary cost of a system that does not adequately protect them. The engine's classification will reflect the aggregate structural properties, but these divergent experiences are critical.
 *
 * DIRECTIONALITY LOGIC:
 *   Public discourse and truth-seekers are the primary beneficiaries, as the system is designed to serve their collective epistemic goals. Speakers and listeners also benefit from the freedom to express and evaluate ideas. Victims of harmful speech are structurally excluded from having their harm recognized as a basis for restriction, making them an 'excluded' party whose interests are subordinated to the marketplace's operation. State regulators are observers, constrained in their ability to intervene.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandate (truth-discovery) is still considered live by its proponents, preventing a Piton classification. However, critics argue that the 'marketplace' has become distorted by disinformation and power imbalances, suggesting a potential drift towards a Tangled Rope or Snare if the epistemic function is no longer genuinely served. The low theater ratio indicates that the core function is still believed to be active, even if its effectiveness is debated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_more_speech,
    'Is ''more speech'' genuinely effective at countering false or harmful speech in contemporary information environments, or does it amplify them?',
    'Empirical studies on the spread of disinformation and the impact of counter-speech in digital platforms, particularly in contexts of echo chambers and algorithmic amplification.',
    'If ''more speech'' is found to be ineffective or counterproductive, the epistemic justification for this reading weakens, potentially shifting its classification towards a Snare (if harms are significant and unmitigated) or a Piton (if the truth-discovery function is purely theatrical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_more_speech, empirical, 'Assesses the real-world effectiveness of the core mechanism of this reading.').

omega_variable(
    marketplace_metaphor_validity,
    'Is the ''marketplace of ideas'' metaphor still a valid conceptual framework for understanding public discourse, given power imbalances, algorithmic biases, and the nature of contemporary media?',
    'Conceptual analysis and critical theory examining the structural conditions of modern communication, comparing them to the idealized conditions assumed by the metaphor.',
    'If the metaphor is deemed fundamentally flawed or outdated, the conceptual grounding of this reading is undermined, potentially leading to a re-evaluation of its normative claims and a shift towards readings that prioritize equity or harm prevention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marketplace_metaphor_validity, conceptual, 'Examines the foundational metaphor underpinning this reading''s justification.').

omega_variable(
    truth_discovery_vs_individual_autonomy,
    'Is the primary justification for speech protection collective truth-discovery (as this reading asserts), or individual autonomy and self-expression (as other readings emphasize)?',
    'Philosophical and legal arguments regarding the foundational values of free speech, examining the historical evolution of these justifications in jurisprudence and political theory.',
    'If individual autonomy is prioritized, this reading''s instrumental justification for broad protection might be seen as secondary, potentially allowing for content-based restrictions when autonomy is not at stake, or when it conflicts with other values like dignity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(truth_discovery_vs_individual_autonomy, preference, 'Clarifies the foundational normative priority for speech protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1919, speech_protection_kernel__marketplace_reading, theater_ratio, 1919, 0.03).
narrative_ontology:measurement(spee_tr_t1950, speech_protection_kernel__marketplace_reading, theater_ratio, 1950, 0.04).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_kernel__marketplace_reading, theater_ratio, 1980, 0.04).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__marketplace_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__marketplace_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(spee_be_t1919, speech_protection_kernel__marketplace_reading, base_extractiveness, 1919, 0.15).
narrative_ontology:measurement(spee_be_t1950, speech_protection_kernel__marketplace_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(spee_be_t1980, speech_protection_kernel__marketplace_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__marketplace_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__marketplace_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1919, speech_protection_kernel__marketplace_reading, suppression_requirement, 1919, 0.08).
narrative_ontology:measurement(spee_su_t1950, speech_protection_kernel__marketplace_reading, suppression_requirement, 1950, 0.09).
narrative_ontology:measurement(spee_su_t1980, speech_protection_kernel__marketplace_reading, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__marketplace_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__marketplace_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
