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
 *   human_readable: Speech Protection (Marketplace of Ideas Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'marketplace of ideas' reading of speech
 *   protection, where the primary justification for broad free speech is its
 *   instrumental value in discovering truth through open competition among
 *   ideas. False or harmful speech is not suppressed, but rather countered by
 *   'more speech.' This reading prioritizes collective epistemic benefit over
 *   individual autonomy or protection from harm. It is one reading of the
 *   broader 'speech_protection_kernel'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.25).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.15).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Speech Protection (Marketplace of Ideas Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy/communication_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, 'a758fa09-62a0-4e96-9f0f-bb2853b2dcff').
narrative_ontology:cs_kernel_codification('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', fixed_text).
narrative_ontology:cs_authority_grounding('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', lineage).
narrative_ontology:cs_interpretation_layer_present('a758fa09-62a0-4e96-9f0f-bb2853b2dcff').
narrative_ontology:cs_reading_relation('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', foundational, truth_emerges_from_free_exchange).
narrative_ontology:cs_axiom_status(truth_emerges_from_free_exchange, holdable).
narrative_ontology:cs_axiom_grounding('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', truth_emerges_from_free_exchange, empirically_contingent).
narrative_ontology:cs_axiom('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', foundational, counter_speech_is_sufficient_remedy).
narrative_ontology:cs_axiom_status(counter_speech_is_sufficient_remedy, holdable).
narrative_ontology:cs_axiom_grounding('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', counter_speech_is_sufficient_remedy, empirically_contingent).
narrative_ontology:cs_reference_frame('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', enlightenment_rationalism_ideal).
narrative_ontology:cs_drift_state('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', contemporary_digital_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a758fa09-62a0-4e96-9f0f-bb2853b2dcff', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, public_discourse).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, truth_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, speakers_of_false_or_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targets_of_false_or_harmful_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the free exchange of ideas, which is believed to lead to the discovery of truth and the robust testing of propositions. The quality of public discourse is enhanced by this process, even if it involves encountering false or harmful speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, public_discourse, beneficiary,
    institutional, generational, constrained, national).

% Individuals or groups who actively engage in the process of evaluating ideas and seeking knowledge. They benefit from the wide array of viewpoints available, believing that exposure to diverse and even erroneous ideas helps refine understanding.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, truth_seekers, beneficiary,
    moderate, biographical, mobile, global).

% Individuals or groups whose speech might be considered false, misleading, or harmful by others. Under this reading, their speech is protected not for its intrinsic value, but because its suppression would distort the truth-discovery process. They are not 'victims' of the constraint, but rather beneficiaries of its broad protection.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, speakers_of_false_or_harmful_speech, beneficiary,
    powerless, immediate, mobile, local).

% Individuals or groups who are subjected to false, misleading, or harmful speech. They bear the direct costs of such speech, with the expectation that 'more speech' will eventually counter the harm. Their recourse is to engage in counter-speech, not to seek suppression of the original speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targets_of_false_or_harmful_speech, payer,
    powerless, immediate, constrained, local).

% Responsible for upholding the principles of free speech, primarily by refraining from content-based restrictions. Their role is to facilitate the 'marketplace' rather than to regulate its content, even when speech is deemed false or harmful. They enforce the procedural aspects of speech protection.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, state_actors, agenda_setter,
    institutional, generational, constrained, national).

% Interpret and apply the principles of free speech, often debating the scope and limits of the marketplace of ideas. They analyze the effectiveness of counter-speech and the potential for speech to cause irreparable harm, influencing the evolution of this reading.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, legal_scholars_and_judges, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the free exchange of diverse ideas, ensuring that all viewpoints, including those considered false or harmful, can be expressed, thereby facilitating a collective process of truth-discovery and robust debate.
% TRANSFER_FUNCTION: Transfers the burden of refuting false or harmful speech from state censorship to individual and collective counter-speech, with the expectation that truth will ultimately prevail in open competition.
% ABSENT_VOICES: Those who advocate for content-based restrictions on speech to prevent immediate harm or protect vulnerable groups are often marginalized in this framework, as their concerns are subordinated to the epistemic goal of truth-discovery through open debate.
% DISAPPEARANCE_RATIONALE: If this reading of speech protection vanished, the legal and philosophical landscape of free expression would fundamentally shift. State actors would likely gain greater power to regulate speech based on content, leading to a more fragmented and potentially less robust public discourse, as the primary justification for broad protection would be gone.
% FOUNDING_PROBLEM: The historical problem of state censorship suppressing dissenting or unpopular ideas, leading to intellectual stagnation and the entrenchment of error, as well as the suppression of political opposition.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historians corroborate the historical problem of censorship. Proponents of this reading argue the problem remains live, citing ongoing attempts by various actors to control information. Critics, however, argue that the nature of 'harmful speech' has evolved, and the marketplace model is insufficient to address new forms of manipulation and abuse, making the founding problem's status 'contested' in practice.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.25) because the constraint primarily facilitates exchange rather than extracting rents, though it imposes costs on targets of harmful speech. Suppression is low (0.15) because the core tenet is non-interference with speech content. Theater ratio is low (0.1) as the commitment to open discourse is largely genuine, though some performative aspects exist in defending clearly false claims. Accessibility collapse is moderate (0.7) as alternatives to open discourse (e.g., regulated speech environments) are seen as less effective for truth-discovery. Resistance is moderate (0.3) from those who argue for greater protection against harm.
 *
 * PERSPECTIVAL GAP:
 *   While proponents see this as a robust mechanism for societal progress, critics (represented by other readings of the kernel) view it as insufficiently protective of vulnerable groups or as a mechanism that allows powerful voices to drown out marginalized ones. The 'payer' seat (targets of harmful speech) experiences the constraint as a burden, while 'beneficiary' seats (public discourse, speakers) experience it as a freedom.
 *
 * DIRECTIONALITY LOGIC:
 *   Public discourse and truth seekers are direct beneficiaries, as the system is designed to serve their epistemic goals. Speakers of false/harmful speech are also beneficiaries, as their right to speak is protected. Targets of such speech are payers, bearing the cost of exposure with the expectation of eventual counter-speech. State actors are agenda-setters, tasked with maintaining the open marketplace. Legal scholars and judges are observers, analyzing and shaping the interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    truth_discovery_efficacy,
    'Does the ''marketplace of ideas'' reliably lead to truth-discovery, especially in an era of information overload and sophisticated disinformation campaigns?',
    'Empirical studies on the long-term societal impact of unrestricted speech, particularly concerning scientific consensus, public health, and democratic processes.',
    'If efficacy is low, the instrumental justification for this reading weakens, potentially shifting support towards harm-based or dignity-based restrictions. If high, it reinforces the current broad protections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(truth_discovery_efficacy, empirical, 'The empirical question of whether the marketplace of ideas actually works as intended.').

omega_variable(
    counter_speech_sufficiency,
    'Is ''more speech'' always a sufficient remedy for false or harmful speech, or are there contexts where the harm is immediate, irreversible, or disproportionately borne by vulnerable groups?',
    'Case studies and sociological analysis of the impact of specific types of harmful speech on targeted communities, assessing the effectiveness and accessibility of counter-speech for those affected.',
    'If counter-speech is often insufficient, this reading''s justification for non-intervention is undermined, potentially leading to reclassification towards a ''tangled_rope'' or ''snare'' for specific contexts where harm is concentrated and unmitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_speech_sufficiency, empirical, 'The effectiveness of counter-speech as a remedy for harmful speech.').

omega_variable(
    epistemic_vs_autonomy_priority,
    'Is the primary value of free speech truly epistemic (truth-discovery) or is it primarily about individual autonomy and self-expression?',
    'Conceptual analysis and philosophical debate regarding the foundational justifications for free speech, examining the implications of prioritizing one over the other for legal doctrine.',
    'If autonomy is prioritized, this reading might shift towards an ''absolutist_reading'' (less concern for truth-discovery outcomes) or a ''democratic_participation_reading'' (focus on political expression). If epistemic value is reaffirmed, this reading''s core remains stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_vs_autonomy_priority, conceptual, 'The foundational philosophical grounding of free speech protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1900, speech_protection_kernel__marketplace_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(spee_tr_t1950, speech_protection_kernel__marketplace_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__marketplace_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__marketplace_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1900, speech_protection_kernel__marketplace_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(spee_be_t1950, speech_protection_kernel__marketplace_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__marketplace_reading, base_extractiveness, 2000, 0.23).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__marketplace_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1900, speech_protection_kernel__marketplace_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(spee_su_t1950, speech_protection_kernel__marketplace_reading, suppression_requirement, 1950, 0.12).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__marketplace_reading, suppression_requirement, 2000, 0.14).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__marketplace_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, information_standard).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, public_health_information_dissemination).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, election_integrity_discourse).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'speech_protection_kernel', each representing a distinct structural claim about the purpose and limits of free speech. This 'marketplace_reading' emphasizes truth-discovery, while others prioritize individual autonomy, democratic participation, dignity, or harm prevention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
