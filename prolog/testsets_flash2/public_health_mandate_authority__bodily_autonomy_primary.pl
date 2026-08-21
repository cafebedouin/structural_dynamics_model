% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate as Bodily Autonomy Violation
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'bodily_autonomy_primary' reading
 *   of the 'public_health_mandate_authority' kernel. From this perspective,
 *   public health mandates are a categorical violation of individual bodily
 *   sovereignty, and no collective benefit can justify non-consensual medical
 *   intervention. The constraint operates as a Snare, extracting fundamental
 *   rights from individuals through coercion, with active suppression of
 *   dissent. The unvaccinated are victims, and the immunocompromised are
 *   excluded from the victim set, as their vulnerability does not justify the
 *   violation of others' autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.95).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.88).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.95).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate as Bodily Autonomy Violation").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, 'd732981c-4911-4a73-a14a-220d5aad3ee6').
narrative_ontology:cs_kernel_codification('d732981c-4911-4a73-a14a-220d5aad3ee6', formalized).
narrative_ontology:cs_authority_grounding('d732981c-4911-4a73-a14a-220d5aad3ee6', extraction).
narrative_ontology:cs_interpretation_layer_present('d732981c-4911-4a73-a14a-220d5aad3ee6').
narrative_ontology:cs_reading_relation('d732981c-4911-4a73-a14a-220d5aad3ee6', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('d732981c-4911-4a73-a14a-220d5aad3ee6', public_health_mandate_authority__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('d732981c-4911-4a73-a14a-220d5aad3ee6', foundational, bodily_autonomy_is_absolute).
narrative_ontology:cs_axiom_status(bodily_autonomy_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('d732981c-4911-4a73-a14a-220d5aad3ee6', bodily_autonomy_is_absolute, deontological).
narrative_ontology:cs_axiom('d732981c-4911-4a73-a14a-220d5aad3ee6', foundational, collective_benefit_cannot_justify_coercion).
narrative_ontology:cs_axiom_status(collective_benefit_cannot_justify_coercion, holdable).
narrative_ontology:cs_axiom_grounding('d732981c-4911-4a73-a14a-220d5aad3ee6', collective_benefit_cannot_justify_coercion, deontological).
narrative_ontology:cs_reference_frame('d732981c-4911-4a73-a14a-220d5aad3ee6', unconditional_bodily_sovereignty).
narrative_ontology:cs_drift_state('d732981c-4911-4a73-a14a-220d5aad3ee6', contemporary_public_health_crisis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('d732981c-4911-4a73-a14a-220d5aad3ee6', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, individuals_seeking_medical_freedom).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who refuse mandated medical interventions, facing exclusion from public spaces, employment, or education. They experience direct coercion and loss of liberty, with no acceptable exit options that preserve their bodily autonomy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals, payer,
    powerless, immediate, trapped, national).

% Advocates for individual medical choice and bodily sovereignty, who perceive public health mandates as an overreach of state power. They bear the cost of defending their rights through legal challenges and public discourse, often facing social and professional repercussions.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, individuals_seeking_medical_freedom, payer,
    moderate, biographical, constrained, national).

% Entities responsible for implementing public health mandates. From this reading's perspective, they are the enforcers of a coercive system, imposing non-consensual interventions under the guise of collective benefit. They benefit from the expansion of their authority.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Individuals whose health is vulnerable to infectious diseases. In this reading, their vulnerability does not justify the violation of others' bodily autonomy, and they are excluded from the victim set because the mandate itself is the harm, not the disease.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_individuals, excluded,
    powerless, immediate, trapped, local).

% Those who believe in the primacy of collective health and support mandates. From this reading's perspective, they are beneficiaries of a system that imposes their preferred outcome on others, without themselves experiencing coercion.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. From this reading, the mandate does not solve a genuine coordination problem but rather imposes a non-consensual medical intervention, violating fundamental individual rights.
% TRANSFER_FUNCTION: Transfers bodily autonomy and individual liberty from unvaccinated individuals to the state, under the justification of collective health, which this reading rejects as illegitimate.
% ABSENT_VOICES: The voices of those who prioritize individual bodily autonomy above all collective health justifications are actively suppressed or dismissed in public discourse and policy-making, often labeled as 'anti-science' or 'selfish'.
% DISAPPEARANCE_RATIONALE: If public health mandates vanished overnight, individuals would regain full control over their medical decisions. Society would need to fundamentally re-evaluate the balance between individual rights and collective welfare, likely leading to a more decentralized and consent-based approach to public health, with significant shifts in legal and ethical frameworks.
% FOUNDING_PROBLEM: The perceived problem of managing infectious disease outbreaks through collective action, which this reading reframes as a problem of state overreach into individual bodily autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for bodily autonomy and medical freedom attest that the 'founding problem' is a pretext for state control, and that the true problem is the erosion of individual rights. Legal scholars and bioethicists outside the public health establishment often corroborate the existence of a fundamental tension between individual liberty and state power in this domain.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the constraint targets a fundamental right (bodily autonomy) with severe consequences for non-compliance (exclusion, loss of livelihood). Suppression is also very high (0.88) due to the legal and social enforcement mechanisms that compel compliance and marginalize dissent. Theater ratio is low (0.1) as the constraint's function is direct coercion, not performative maintenance. Accessibility collapse is high (0.75) because alternatives to compliance are severely limited or carry prohibitive costs. Resistance is high (0.9) reflecting the strong opposition from those whose autonomy is targeted.
 *
 * PERSPECTIVAL GAP:
 *   The public health authorities, from their own perspective, would classify this as a Rope or Scaffold, a necessary coordination mechanism for collective well-being. However, from the 'bodily_autonomy_primary' reading, it is a Snare, a pure extraction mechanism that violates fundamental rights. This divergence is central to the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals and those seeking medical freedom are full targets (high d) as they bear the direct costs of coercion and loss of autonomy. Public health authorities are agenda-setters and beneficiaries (low d) as they enforce the mandates and expand their institutional power. Immunocompromised individuals are excluded from the victim set in this reading, as their health status does not justify the violation of others' rights. Public health primary advocates are beneficiaries, as the constraint imposes their preferred collective outcome without coercing them.
 *
 * MANDATROPHY ANALYSIS:
 *   From this reading, the mandate's 'mandate' (its justification) is inherently flawed as it violates a primary ethical principle. The classification as a Snare prevents mislabeling this as a coordination mechanism, highlighting the coercive and extractive nature of the constraint when viewed through the lens of bodily autonomy. There is no mandatrophy in the sense of a function atrophying; rather, the mandate's function is seen as illegitimate from its inception.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_collective_benefit,
    'Can any collective benefit, however great, legitimately override individual bodily sovereignty?',
    'Philosophical and legal consensus on the hierarchy of rights, or a constitutional amendment explicitly defining the limits of state power over individual bodies for public health.',
    'If collective benefit can override, this constraint might be reclassified as a Tangled Rope (coordination with extraction). If not, its Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_collective_benefit, conceptual, 'The fundamental conceptual conflict between individual rights and collective good in public health.').

omega_variable(
    mandate_effectiveness_empirical,
    'What is the empirically demonstrated effectiveness of public health mandates in achieving their stated goals (e.g., reducing transmission, protecting healthcare capacity) versus their social and individual costs?',
    'Rigorous, independent epidemiological and social science studies comparing outcomes in mandated vs. non-mandated populations, accounting for confounding factors.',
    'If mandates are shown to be ineffective or to cause disproportionate harm, the ''public_health_primary'' reading''s justification would weaken, potentially strengthening the ''bodily_autonomy_primary'' position. This would not change the Snare classification from this reading, but would undermine the legitimacy claims of its proponents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_effectiveness_empirical, empirical, 'Empirical evidence for the efficacy and cost-benefit of public health mandates.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, exclusion) or internalized (social pressure, fear of ostracization)?',
    'Post-mandate suppression trajectory: if social pressure and self-censorship persist after legal mandates are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the Snare more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for public health mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 2020, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t2020, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(publ_tr_t2021, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2021, 0.1).
narrative_ontology:measurement(publ_tr_t2022, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(publ_tr_t2023, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2023, 0.1).
narrative_ontology:measurement(publ_tr_t2024, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t2020, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2020, 0.85).
narrative_ontology:measurement(publ_be_t2021, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2021, 0.92).
narrative_ontology:measurement(publ_be_t2022, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2022, 0.95).
narrative_ontology:measurement(publ_be_t2023, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2023, 0.93).
narrative_ontology:measurement(publ_be_t2024, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t2020, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(publ_su_t2021, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2021, 0.85).
narrative_ontology:measurement(publ_su_t2022, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2022, 0.88).
narrative_ontology:measurement(publ_su_t2023, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2023, 0.87).
narrative_ontology:measurement(publ_su_t2024, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
