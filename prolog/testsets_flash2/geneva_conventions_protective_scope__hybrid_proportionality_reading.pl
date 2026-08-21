% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Conventions Protective Scope: Hybrid Proportionality Reading
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid proportionality' reading of the
 *   Geneva Conventions' protective scope, where protections scale by conflict
 *   type (AP I for international, AP II/Common Article 3 for
 *   non-international) and proportionality analysis determines application.
 *   This reading attempts to adapt IHL to complex modern conflicts but
 *   introduces significant legal ambiguity, which powerful states can
 *   leverage. The victim set varies by conflict classification, and effective
 *   extraction fluctuates based on the proportionality calculus, often to the
 *   detriment of weaker parties.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.7).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Conventions Protective Scope: Hybrid Proportionality Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'ab2f4957-c8bf-4b79-a772-2988d5d0b6a7').
narrative_ontology:cs_kernel_codification('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7', fixed_text).
narrative_ontology:cs_authority_grounding('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7', lineage).
narrative_ontology:cs_interpretation_layer_present('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7').
narrative_ontology:cs_reading_relation('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7', foundational, proportionality_as_balancing_principle).
narrative_ontology:cs_axiom_status(proportionality_as_balancing_principle, holdable).
narrative_ontology:cs_axiom_grounding('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7', proportionality_as_balancing_principle, conventional).
narrative_ontology:cs_axiom('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7', foundational, differentiated_protection_by_conflict_type).
narrative_ontology:cs_axiom_status(differentiated_protection_by_conflict_type, holdable).
narrative_ontology:cs_axiom_grounding('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7', differentiated_protection_by_conflict_type, conventional).
narrative_ontology:cs_reference_frame('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7', post_additional_protocols_framework).
narrative_ontology:cs_drift_state('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7', contemporary_hybrid_warfare_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ab2f4957-c8bf-4b79-a772-2988d5d0b6a7', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_states).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_commanders).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_non_international_conflict).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_parties_in_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply IHL, often leveraging the proportionality calculus to justify actions while maintaining a degree of legal flexibility. They benefit from the ambiguity in classifying conflicts and applying differentiated standards.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_states, agenda_setter,
    institutional, generational, constrained, global).

% Operate within the framework, using proportionality assessments to guide targeting decisions. They benefit from the legal space provided by the hybrid reading, which allows for adaptation to complex conflict scenarios while maintaining a claim to legality.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_commanders, beneficiary,
    powerful, biographical, constrained, regional).

% Are often subject to AP II/Common Article 3 standards, which offer fewer protections than AP I. Their combatant status is frequently denied, leading to reduced legal safeguards and increased vulnerability to state action.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups, payer,
    powerless, immediate, trapped, local).

% Experience varying levels of protection depending on the conflict classification and the proportionality judgments made by state actors. Their status is often ambiguous, leading to increased risk and reduced accountability for harm.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_non_international_conflict, payer,
    powerless, immediate, trapped, local).

% Bear the brunt of the differentiated application of IHL, as the hybrid reading allows stronger parties to tailor protections based on conflict type and proportionality. This creates an uneven playing field and reduces their ability to claim full protection.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_parties_in_conflict, payer,
    powerless, biographical, trapped, regional).

% Analyze the application and interpretation of the Geneva Conventions, often highlighting the challenges and inconsistencies arising from the hybrid approach. They seek to clarify legal ambiguities and advocate for more consistent protection.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for regulating armed conflict, aiming to balance military necessity with humanitarian concerns by differentiating protections based on conflict type and applying proportionality principles.
% TRANSFER_FUNCTION: Transfers legal flexibility and reduced accountability to powerful states and military commanders, while transferring increased vulnerability and reduced protections to non-state armed groups and civilians in non-international conflicts.
% ABSENT_VOICES: Victims of conflict, particularly those in non-international armed conflicts whose status is ambiguous, are often not directly represented in the interpretive debates. They would advocate for universal application of the highest protective standards.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, the legal landscape of armed conflict would be fundamentally altered. States would either default to a more restrictive (state-centric) or more expansive (universal rights) interpretation, leading to significant shifts in military conduct, accountability, and the protection of individuals.
% FOUNDING_PROBLEM: The need to regulate the conduct of hostilities and protect individuals during armed conflict, recognizing the evolving nature of warfare beyond traditional interstate conflicts.
% FOUNDING_PROBLEM_CORROBORATION: International legal bodies, human rights organizations, and academic scholars corroborate that the problem of regulating armed conflict and protecting individuals remains live, particularly with the rise of non-international armed conflicts and complex hybrid warfare scenarios.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the legal ambiguity that allows powerful states to selectively interpret and apply IHL, reducing protections for certain groups. Suppression (0.7) is high because weaker parties lack the power to challenge these interpretations or enforce higher standards. The theater ratio (0.2) reflects that while genuine humanitarian concerns are addressed, a portion of the legal discourse serves to legitimize actions that might otherwise be seen as violations. The metrics reflect a system that, while providing some coordination, also enables significant asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states perceive this reading as a necessary adaptation of IHL to modern conflict, balancing military necessity with humanitarianism. Weaker parties and human rights advocates, however, experience it as a mechanism that legitimizes disproportionate force and reduces protections, effectively extracting their rights and safety.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states and military commanders are beneficiaries, gaining flexibility and reduced accountability. Non-state armed groups, civilians in non-international conflicts, and weaker parties are victims, facing reduced and ambiguous protections. IHL scholars act as observers, analyzing the system's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_calculus_objectivity,
    'Is the proportionality calculus applied objectively, or is it systematically biased towards the interests of powerful states?',
    'Independent, retrospective analysis of targeting decisions across multiple conflicts by a neutral international body, comparing stated military advantage with civilian harm.',
    'If systematically biased, the measured extractiveness is understated, and the constraint functions more as a snare. If objective, the coordination function is stronger, and the constraint is closer to a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calculus_objectivity, empirical, 'Assesses the objectivity of proportionality assessments in practice.').

omega_variable(
    conflict_classification_ambiguity,
    'Is the distinction between International Armed Conflict (IAC) and Non-International Armed Conflict (NIAC) sufficiently clear and consistently applied, or does it create exploitable ambiguity?',
    'Legal review of state practices in classifying conflicts and applying corresponding IHL standards, identifying patterns of reclassification that reduce protective scope.',
    'If ambiguity is systematically exploited, the constraint''s suppression and extractiveness are higher than measured, as protections are arbitrarily reduced. If consistently applied, the differentiation serves a genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_classification_ambiguity, conceptual, 'Examines the clarity and consistency of conflict classification and its impact on protective scope.').

omega_variable(
    mandate_drift_from_founding_problem,
    'Has the original mandate to protect individuals in armed conflict drifted towards legitimizing state military action, rather than primarily constraining it?',
    'Comparative analysis of IHL interpretations and state practice over time, focusing on shifts in emphasis from protection to military necessity, and the role of legal advisors in shaping these interpretations.',
    'If a significant drift towards legitimization is found, the constraint''s claimed coordination function is largely theatrical, and its true nature is closer to a snare, with the founding problem being ''dead'' in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_drift_from_founding_problem, empirical, 'Assesses whether the constraint''s function has drifted from protection to legitimization of force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(gene_tr_t2000, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1977, 0.5).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(gene_be_t2000, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1977, 0.55).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(gene_su_t2000, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, universal_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Geneva Conventions' protective scope. It influences and coexists with the 'state-centric' and 'universal rights' readings, as interpretations of IHL are constantly in dialogue and competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
