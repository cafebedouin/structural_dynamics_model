% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__democratic_participation_reading, []).

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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Speech Protection for Democratic Participation
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'democratic participation' reading of free
 *   speech, where protection is strongest for political expression deemed
 *   necessary for self-governance. It establishes a hierarchy of speech, with
 *   political speech receiving the highest level of scrutiny before
 *   restriction, while other forms of speech (e.g., commercial, artistic) are
 *   more readily regulated. The constraint is claimed as a Rope due to its
 *   genuine coordination function in democratic societies, but its metrics
 *   reflect a slight, ongoing extractiveness and suppression as non-political
 *   speech is de-prioritized.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.15).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.2).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Speech Protection for Democratic Participation").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '78cdf399-58b9-4f1f-87f6-249e08db47f6').
narrative_ontology:cs_kernel_codification('78cdf399-58b9-4f1f-87f6-249e08db47f6', fixed_text).
narrative_ontology:cs_authority_grounding('78cdf399-58b9-4f1f-87f6-249e08db47f6', lineage).
narrative_ontology:cs_interpretation_layer_present('78cdf399-58b9-4f1f-87f6-249e08db47f6').
narrative_ontology:cs_reading_relation('78cdf399-58b9-4f1f-87f6-249e08db47f6', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('78cdf399-58b9-4f1f-87f6-249e08db47f6', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('78cdf399-58b9-4f1f-87f6-249e08db47f6', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('78cdf399-58b9-4f1f-87f6-249e08db47f6', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('78cdf399-58b9-4f1f-87f6-249e08db47f6', foundational, political_speech_essential_for_democracy).
narrative_ontology:cs_axiom_status(political_speech_essential_for_democracy, holdable).
narrative_ontology:cs_axiom_grounding('78cdf399-58b9-4f1f-87f6-249e08db47f6', political_speech_essential_for_democracy, deontological).
narrative_ontology:cs_axiom('78cdf399-58b9-4f1f-87f6-249e08db47f6', secondary, speech_hierarchy_legitimate_for_governance).
narrative_ontology:cs_axiom_status(speech_hierarchy_legitimate_for_governance, holdable).
narrative_ontology:cs_axiom_grounding('78cdf399-58b9-4f1f-87f6-249e08db47f6', speech_hierarchy_legitimate_for_governance, conventional).
narrative_ontology:cs_reference_frame('78cdf399-58b9-4f1f-87f6-249e08db47f6', informed_self_governance_framework).
narrative_ontology:cs_drift_state('78cdf399-58b9-4f1f-87f6-249e08db47f6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('78cdf399-58b9-4f1f-87f6-249e08db47f6', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, citizens).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_candidates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, non_political_speakers).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, self_governance_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, informed_electorate_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to freely discuss political issues, criticize government, and participate in public discourse, which is seen as essential for a functioning democracy. Their ability to exit this framework is limited by their citizenship.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, citizens, beneficiary,
    organized, generational, constrained, national).

% Benefit from broad latitude to express their views and campaign messages without undue restriction, enabling them to reach voters and compete for office. They can adapt their speech within legal bounds.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_candidates, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from the ability to organize, protest, and disseminate their messages on matters of public concern, influencing policy and public opinion. Their exit options are limited by the legal framework for advocacy.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, advocacy_groups, beneficiary,
    moderate, biographical, constrained, national).

% Are tasked with balancing speech protection with other societal interests, often through judicial interpretation and enforcement. They administer the legal framework for speech, distinguishing political from non-political expression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, government_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Bear the cost of having their speech (e.g., commercial, artistic, or private expression) subjected to greater regulation and more readily restricted than political speech. Their options are to conform to regulations or challenge them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, non_political_speakers, payer,
    moderate, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public discourse to ensure a robust exchange of ideas necessary for informed self-governance, prioritizing speech that contributes to democratic decision-making.
% TRANSFER_FUNCTION: Transfers a higher degree of protection and immunity from restriction to political speech, while allowing greater regulatory burdens on other forms of expression, from the state to citizens and political actors.
% ABSENT_VOICES: Those who advocate for an 'absolutist' view of speech protection, where all speech is equally protected regardless of content, are often marginalized in this framework, as are those whose 'harm threshold' for speech is lower than the democratic participation standard.
% DISAPPEARANCE_RATIONALE: If this hierarchy of speech protection vanished, the ability of citizens to engage in robust political debate would be severely hampered, as political speech would lose its privileged status and become more vulnerable to restriction. The democratic process itself would be fundamentally altered.
% FOUNDING_PROBLEM: To ensure that citizens have the necessary information and freedom to discuss public affairs, criticize government, and make informed decisions in a self-governing society, preventing tyranny and promoting democratic stability.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, political scientists, and historical analyses of democratic societies consistently corroborate the ongoing need for robust political speech protection to maintain self-governance. This is attested by independent academic research and judicial precedent, not solely by the beneficiaries of the constraint.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).
:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary function is coordination for public good, not rent-seeking. However, it's not zero because non-political speakers bear a cost of reduced protection. Suppression is low (0.2) but present, as the state actively enforces the distinction between protected political speech and regulable non-political speech. Theater ratio is low (0.1) because the core function of protecting political discourse is genuinely performed, though some performative aspects exist in judicial balancing acts. Accessibility collapse is moderate (0.7) because while political speech is highly protected, other forms of speech face more barriers. Resistance is low (0.1) because the principle of protecting political speech is widely accepted, though specific applications are contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of political actors, this constraint is a pure Rope, enabling their function. From the perspective of non-political speakers, it can feel more like a Tangled Rope, where they are coordinated into a system that extracts higher regulatory costs from their expression. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizens, political candidates, and advocacy groups are the primary beneficiaries, as their political expression is highly protected. Government regulators act as agenda-setters, interpreting and enforcing the hierarchy. Non-political speakers are payers, as their speech is more easily restricted. There are no direct 'victims' in the sense of pure extraction, but rather differential treatment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_speech_definition_ambiguity,
    'How is ''political speech'' precisely defined, and is this definition consistently applied across different contexts and technologies?',
    'Judicial clarification through landmark cases, or legislative action providing clearer statutory definitions. Empirical analysis of how different courts apply the definition.',
    'If the definition is too narrow, it could suppress legitimate public discourse, shifting the constraint towards a Snare for some speakers. If too broad, it could dilute the protection for core political expression, shifting it towards a Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_speech_definition_ambiguity, conceptual, 'Ambiguity in defining the core protected category of speech.').

omega_variable(
    non_political_speech_suppression_justification,
    'Are the justifications for restricting non-political speech (e.g., commercial, artistic) genuinely distinct from those for political speech, or is the differential treatment a form of content-based discrimination?',
    'Comparative legal analysis of speech regulations across categories, and empirical studies on the actual harms prevented by restricting non-political speech versus the expressive costs incurred.',
    'If the justifications are found to be weak or pretextual, the constraint''s extractiveness and suppression for non-political speakers would be re-evaluated as higher, potentially shifting it towards a Tangled Rope or Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_political_speech_suppression_justification, empirical, 'Justification for differential treatment of non-political speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1900, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(spee_tr_t1950, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1900, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(spee_be_t1950, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1900, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(spee_su_t1950, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2000, 0.19).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
