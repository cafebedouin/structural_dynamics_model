% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment (Originalist Civic Virtue Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'originalist civic virtue' reading of the
 *   Second Amendment, which interprets the right to bear arms as primarily
 *   protecting the capacity of the citizenry, understood as a universal
 *   militia, to participate in collective defense and serve as a check on
 *   state power. This reading emphasizes civic duty and the republican ideal
 *   of an armed populace, rather than individual self-defense or purely
 *   state-controlled military forces. It is one reading of the
 *   'second_amendment_text' kernel, distinct from
 *   'collective_security_reading' and 'individual_right_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.18).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.12).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment (Originalist Civic Virtue Reading)").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, 'bd63fcb8-3ff0-40fb-86ba-193105fbb2fe').
narrative_ontology:cs_kernel_codification('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe', fixed_text).
narrative_ontology:cs_authority_grounding('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe', lineage).
narrative_ontology:cs_interpretation_layer_present('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe').
narrative_ontology:cs_reading_relation('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe', foundational, citizen_militia_duty_axiom).
narrative_ontology:cs_axiom_status(citizen_militia_duty_axiom, holdable).
narrative_ontology:cs_axiom_grounding('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe', citizen_militia_duty_axiom, deontological).
narrative_ontology:cs_axiom('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe', foundational, republican_self_governance_axiom).
narrative_ontology:cs_axiom_status(republican_self_governance_axiom, holdable).
narrative_ontology:cs_axiom_grounding('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe', republican_self_governance_axiom, deontological).
narrative_ontology:cs_reference_frame('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe', founding_era_republican_ideal).
narrative_ontology:cs_drift_state('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd63fcb8-3ff0-40fb-86ba-193105fbb2fe', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, citizenry_qua_political_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, state_legislatures).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republicanism_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, popular_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body of citizens whose capacity for self-defense and civic participation in a militia is protected. Their identity as free citizens is tied to this capacity, which serves as a check on state power and a foundation for republican governance.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, citizenry_qua_political_community, beneficiary,
    organized, generational, identity_locked, national).

% Constrained in their ability to disarm the populace or dismantle the militia structure, as the right protects the citizen-soldier capacity. They bear the cost of this limitation on their regulatory authority.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, state_legislatures, payer,
    institutional, biographical, constrained, national).

% Responsible for interpreting and upholding the Second Amendment, ensuring the 'right of the people' to maintain the capacity for a 'well regulated Militia'. This constrains federal power and requires adherence to a civic republican understanding of armed citizenry.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Scholars and legal experts who analyze the historical context and theoretical underpinnings of the Second Amendment, particularly this civic virtue interpretation, without direct participation in its enforcement or benefit.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the capacity of the citizenry to form a militia for collective defense, ensuring a check on potential tyranny and fostering civic responsibility, thereby securing a free state.
% TRANSFER_FUNCTION: Transfers the responsibility for collective security, in part, to the armed citizenry, and grants them the capacity to fulfill this role, from the state to the people. It also transfers a degree of trust and autonomy to the populace.
% ABSENT_VOICES: Those who advocate for a strict state monopoly on force, or those who view widespread private arms ownership as inherently destabilizing, would argue that the civic virtue framing is anachronistic or dangerous in a modern context. They are often excluded from the originalist discourse that centers this reading, which prioritizes historical intent over contemporary policy outcomes.
% DISAPPEARANCE_RATIONALE: If the Second Amendment, particularly this civic virtue understanding, vanished, the relationship between the state and its citizens regarding armed capacity would fundamentally shift. State power would be less constrained by the threat of popular resistance, and the concept of a citizen-soldier as a civic ideal would diminish, leading to a reorganization of both legal frameworks and political theory regarding popular sovereignty and defense.
% FOUNDING_PROBLEM: The need to ensure the security of a free state against both foreign invasion and potential domestic tyranny, by maintaining a well-regulated militia composed of the general citizenry, rather than relying solely on a standing army, which was viewed with suspicion.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading, including many legal scholars and historians, argue the problem of state overreach and the importance of an armed citizenry remain live. Critics, including other scholars and policy advocates, contend that the nature of warfare and the state has changed such that the founding problem, as originally conceived, is largely dead or requires different solutions. Corroboration for the 'live' status comes from historical texts and political theory, while 'dead' status is argued from modern military and political realities, often by non-beneficiary legal experts and social scientists.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.18, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint primarily grants a capacity and imposes a civic duty, rather than extracting resources or imposing burdens without corresponding benefit. Suppression is low (0.12) as it functions as a protection against state overreach, not a mechanism of coercion. Theater ratio is moderate (0.25) because while the ideal of a universal citizen militia is still invoked, its practical manifestation has significantly atrophied in modern times, leading to some performative maintenance of the concept. Accessibility collapse is high (0.85) as a constitutional right is intended to be a fundamental, uncollapsible aspect of citizenship. Resistance is low (0.15) against the core right itself, though its interpretation and application are highly contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the citizenry, this reading is a fundamental protection of their liberty and civic role. From the perspective of state actors, it represents a limitation on their power to regulate arms and organize defense, which they may perceive as a burden or an impediment to public safety. The engine computes these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'citizenry_qua_political_community' is the primary beneficiary, as the right protects their collective capacity and civic role. State legislatures and the federal government act as payers, as their regulatory authority is constrained by this constitutional right. Analytical observers are external to the direct operation of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_clarification,
    'Is this constraint accurately identified as the ''originalist_civic_virtue_reading'' of the ''second_amendment_text'' kernel?',
    'Further historical and legal scholarship comparing this reading''s tenets against primary founding-era documents and contemporary interpretations.',
    'If misidentified, the entire analysis of its structural relationships to sibling readings and its internal axioms would be compromised, requiring reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_clarification, conceptual, 'Confirms the specific reading being analyzed within the kernel.').

omega_variable(
    sibling_structural_delta_collective_security,
    'How would the ''collective_security_reading'' structurally alter the constraint''s beneficiaries and state power?',
    'Analysis of legal precedents and legislative proposals that prioritize state-organized militia over individual armed citizenry, and their impact on regulatory authority.',
    'The ''collective_security_reading'' would likely shift the beneficiary focus more towards the ''state'' or ''organized militia'' as the primary entity whose security is protected, potentially allowing for greater state regulation of arms to ensure militia effectiveness. This would increase the state''s power and constrain individual citizens more.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_structural_delta_collective_security, conceptual, 'Examines the structural changes implied by the collective security reading.').

omega_variable(
    sibling_structural_delta_individual_right,
    'How would the ''individual_right_reading'' structurally alter the constraint''s beneficiaries and the scope of the right?',
    'Analysis of legal precedents (e.g., *Heller*, *McDonald*) that emphasize individual self-defense and their impact on state regulatory power over private arms ownership.',
    'The ''individual_right_reading'' would decouple the right to bear arms from militia service, emphasizing personal self-defense as the core protected activity. This would broaden the beneficiary set to ''all individuals'' and potentially reduce the state''s regulatory power over private arms ownership, shifting the constraint''s focus from civic duty to individual liberty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_structural_delta_individual_right, conceptual, 'Examines the structural changes implied by the individual right reading.').

omega_variable(
    disagreement_locus_militia_clause,
    'What is the precise locus of disagreement regarding the ''well regulated Militia'' clause across different readings?',
    'Detailed textual analysis of the Second Amendment''s grammar and historical usage of its terms, combined with legal and political theory scholarship.',
    'Clarifying whether the militia clause is a condition, a purpose, or merely an explanatory preamble would fundamentally alter the structural relationship between the collective and individual aspects of the right, impacting regulatory authority and individual liberty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_locus_militia_clause, conceptual, 'Identifies the core interpretive ambiguity in the Second Amendment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1990, 0.23).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(seco_tr_t2020, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2020, 0.25).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(seco_be_t1980, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1980, 0.16).
narrative_ontology:measurement(seco_be_t1990, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1990, 0.17).
narrative_ontology:measurement(seco_be_t2000, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2000, 0.17).
narrative_ontology:measurement(seco_be_t2010, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(seco_be_t2020, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2020, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(seco_su_t1980, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1980, 0.11).
narrative_ontology:measurement(seco_su_t1990, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1990, 0.11).
narrative_ontology:measurement(seco_su_t2000, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(seco_su_t2010, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement(seco_su_t2020, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2020, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_text' kernel. Each reading instantiates a different constraint with unique structural properties and ε values, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
