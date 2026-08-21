% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing Standard
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality_balancing' reading of
 *   Common Article 3 of the Geneva Conventions, which requires states to
 *   balance detainee dignity against legitimate security needs. It rejects
 *   both absolute prohibitions on certain treatments and unlimited discretion
 *   for security forces, instead mandating a case-by-case assessment with
 *   judicial oversight. This reading positions courts as gatekeepers,
 *   ensuring procedural safeguards and limiting interrogators' discretion to
 *   a moderate degree.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.65).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.7).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.65).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing Standard").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, '83968a7a-a84f-4b3f-af5d-523525aacfcb').
narrative_ontology:cs_kernel_codification('83968a7a-a84f-4b3f-af5d-523525aacfcb', formalized).
narrative_ontology:cs_authority_grounding('83968a7a-a84f-4b3f-af5d-523525aacfcb', lineage).
narrative_ontology:cs_interpretation_layer_present('83968a7a-a84f-4b3f-af5d-523525aacfcb').
narrative_ontology:cs_reading_relation('83968a7a-a84f-4b3f-af5d-523525aacfcb', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('83968a7a-a84f-4b3f-af5d-523525aacfcb', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_axiom('83968a7a-a84f-4b3f-af5d-523525aacfcb', foundational, detainee_dignity_and_security_needs_must_be_balanced).
narrative_ontology:cs_axiom_status(detainee_dignity_and_security_needs_must_be_balanced, holdable).
narrative_ontology:cs_axiom_grounding('83968a7a-a84f-4b3f-af5d-523525aacfcb', detainee_dignity_and_security_needs_must_be_balanced, deontological).
narrative_ontology:cs_axiom('83968a7a-a84f-4b3f-af5d-523525aacfcb', foundational, judicial_review_ensures_proportionality).
narrative_ontology:cs_axiom_status(judicial_review_ensures_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('83968a7a-a84f-4b3f-af5d-523525aacfcb', judicial_review_ensures_proportionality, conventional).
narrative_ontology:cs_reference_frame('83968a7a-a84f-4b3f-af5d-523525aacfcb', post_geneva_conventions_era).
narrative_ontology:cs_drift_state('83968a7a-a84f-4b3f-af5d-523525aacfcb', contemporary_counter_terrorism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('83968a7a-a84f-4b3f-af5d-523525aacfcb', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, state_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, judicial_bodies).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for national security and intelligence gathering, they seek maximum flexibility in interrogation and detention practices. They benefit from the discretion allowed by the balancing standard but are constrained by judicial oversight.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, state_security_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Individuals held in detention, often without immediate access to legal counsel or external oversight. They bear the direct costs of any treatment deemed 'proportionate' but infringing on their dignity, and have extremely limited means of resistance or exit.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees, payer,
    powerless, immediate, trapped, local).

% Organizations and individuals dedicated to protecting human rights. They monitor state practices, document abuses, and advocate for stricter interpretations of humane treatment, often challenging the proportionality balancing as insufficient.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Courts and tribunals tasked with interpreting and enforcing international humanitarian law. They act as gatekeepers, reviewing state practices and ensuring proportionality, thereby legitimizing the balancing act while also constraining state power.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, judicial_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Scholars, practitioners, and institutions involved in the development and interpretation of international law. They contribute to the discourse around Common Article 3, influencing its understanding and application, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a legal framework that reconciles the legitimate security needs of states with the fundamental human dignity of detainees, thereby maintaining the viability and legitimacy of international humanitarian law in contexts of armed conflict.
% TRANSFER_FUNCTION: Transfers some discretionary power to state security agencies (within judicially defined limits) and transfers the burden of justifying treatment to them. It transfers the risk of degrading treatment to detainees, mitigated by the possibility of legal challenge.
% ABSENT_VOICES: Victims of past or ongoing abuses, who are often silenced, lack legal standing, or are unable to access justice mechanisms. Their experiences would highlight the practical failures of proportionality balancing and the persistent extractiveness.
% DISAPPEARANCE_RATIONALE: If this proportionality balancing standard vanished, states would likely revert to either an unworkable absolute prohibition (which security agencies would resist) or, more likely, to broad, unchecked discretion under 'contextual necessity,' leading to a significant increase in abuses and a severe erosion of international humanitarian law norms.
% FOUNDING_PROBLEM: The need to establish minimum standards of humane treatment for persons not taking an active part in hostilities, particularly in non-international armed conflicts, while acknowledging the legitimate security concerns of states and avoiding an absolute prohibition that states would reject.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and judicial rulings consistently affirm the ongoing relevance of this balancing act. While state security agencies often push for greater discretion, the international community and legal bodies generally corroborate the continued necessity of a balancing framework to prevent widespread abuses and maintain the integrity of international law.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because detainees still bear the cost of 'balancing' that may permit treatments short of torture but still degrading, and security agencies benefit from the discretion. Suppression (0.70) is high due to the inherent power imbalance in detention and the difficulty of enforcing standards in opaque environments, despite judicial review. The theater ratio (0.25) is moderate, reflecting genuine judicial oversight but also the performative aspect of legal justifications for practices that push the boundaries of humane treatment. The metrics show a slight increase in extractiveness and suppression over time, reflecting ongoing pressure from security imperatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state security agencies, this standard provides necessary flexibility to protect national interests while maintaining a veneer of legality. From the perspective of detainees and human rights advocates, it represents a compromise that still permits significant infringements on dignity, often with insufficient accountability. Judicial bodies see it as a complex but necessary framework for upholding international law.
 *
 * DIRECTIONALITY LOGIC:
 *   State security agencies are beneficiaries (gain discretion, maintain security) and judicial bodies are also beneficiaries (uphold rule of law, exercise authority). Detainees are the primary victims (bear the costs of any permitted infringements on dignity). Human rights advocates and the international legal community act as observers, pushing for stricter interpretations and greater accountability.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (balancing dignity and security) remains live, preventing mandatrophy. However, the ongoing tension and the slight increase in extractiveness suggest a risk of 'mandate creep,' where security needs gradually overshadow dignity, potentially drifting towards a Snare if the balancing becomes purely rhetorical. The judicial gatekeeping function is crucial in preventing this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a reading of the ''humane_treatment_standard'' kernel, specifically the ''proportionality_balancing'' interpretation?',
    'Analysis of legal scholarship and judicial precedent confirming the interpretive framework.',
    'If not, the classification would need to be re-evaluated against a different kernel or as a standalone constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint as a specific reading of a contested kernel.').

omega_variable(
    sibling_reading_absolute_prohibition_delta,
    'How would the ''absolute_prohibition'' sibling reading structurally differ from this ''proportionality_balancing'' reading?',
    'Comparative legal analysis of the two interpretive frameworks and their implications for state practice and judicial review.',
    'The ''absolute_prohibition'' reading would likely result in lower extractiveness from detainees and higher suppression on state security agencies, potentially classifying as a Rope or even a Mountain (if universally accepted as natural law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_absolute_prohibition_delta, conceptual, 'Structural delta if ''absolute_prohibition'' reading were adopted.').

omega_variable(
    sibling_reading_contextual_necessity_delta,
    'How would the ''contextual_necessity'' sibling reading structurally differ from this ''proportionality_balancing'' reading?',
    'Comparative legal analysis of the two interpretive frameworks and their implications for state practice and judicial review.',
    'The ''contextual_necessity'' reading would likely result in significantly higher extractiveness from detainees and lower suppression on state security agencies, potentially classifying as a Snare due to expanded discretion and reduced accountability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_contextual_necessity_delta, conceptual, 'Structural delta if ''contextual_necessity'' reading were adopted.').

omega_variable(
    proportionality_implementation_fidelity,
    'To what extent is the ''proportionality balancing'' genuinely applied in practice versus being a rhetorical cover for security imperatives?',
    'Empirical study of judicial outcomes, interrogation practices, and detainee treatment records across multiple jurisdictions.',
    'If fidelity is low, the effective extractiveness is higher, and the constraint leans more towards a Snare, as the coordination story (balancing) becomes cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_implementation_fidelity, empirical, 'Assesses the gap between the stated principle of proportionality and its actual implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__proportionality_balancing, theater_ratio, 0, 0.2).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__proportionality_balancing, theater_ratio, 10, 0.22).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__proportionality_balancing, theater_ratio, 20, 0.23).
narrative_ontology:measurement(huma_tr_t30, humane_treatment_standard__proportionality_balancing, theater_ratio, 30, 0.24).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__proportionality_balancing, theater_ratio, 40, 0.25).
narrative_ontology:measurement(huma_tr_t50, humane_treatment_standard__proportionality_balancing, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__proportionality_balancing, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__proportionality_balancing, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(huma_be_t30, humane_treatment_standard__proportionality_balancing, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__proportionality_balancing, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(huma_be_t50, humane_treatment_standard__proportionality_balancing, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__proportionality_balancing, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__proportionality_balancing, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(huma_su_t30, humane_treatment_standard__proportionality_balancing, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__proportionality_balancing, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(huma_su_t50, humane_treatment_standard__proportionality_balancing, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
