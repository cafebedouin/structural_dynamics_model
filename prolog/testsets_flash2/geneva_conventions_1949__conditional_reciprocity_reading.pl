% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions (1949) - Conditional Reciprocity Reading
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'conditional reciprocity' reading of the
 *   1949 Geneva Conventions, which holds that the full protections of
 *   international humanitarian law apply only when adversaries comply with
 *   conventional rules of warfare. Non-compliance by irregular forces,
 *   particularly regarding Article 4 criteria (organized command, distinctive
 *   insignia, carrying arms openly), permits states to proportionally degrade
 *   protections, especially for POW status. Civilian immunity is maintained
 *   but subject to proportionality calculations that can narrow its practical
 *   scope. This reading is often invoked by states engaged in asymmetric
 *   conflicts against non-state actors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.45).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.6).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions (1949) - Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, 'ded8f2b1-653b-48a6-9950-d1ae32dac5ed').
narrative_ontology:cs_kernel_codification('ded8f2b1-653b-48a6-9950-d1ae32dac5ed', fixed_text).
narrative_ontology:cs_authority_grounding('ded8f2b1-653b-48a6-9950-d1ae32dac5ed', lineage).
narrative_ontology:cs_interpretation_layer_present('ded8f2b1-653b-48a6-9950-d1ae32dac5ed').
narrative_ontology:cs_reading_relation('ded8f2b1-653b-48a6-9950-d1ae32dac5ed', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('ded8f2b1-653b-48a6-9950-d1ae32dac5ed', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('ded8f2b1-653b-48a6-9950-d1ae32dac5ed', foundational, reciprocity_as_foundational_principle).
narrative_ontology:cs_axiom_status(reciprocity_as_foundational_principle, holdable).
narrative_ontology:cs_axiom_grounding('ded8f2b1-653b-48a6-9950-d1ae32dac5ed', reciprocity_as_foundational_principle, conventional).
narrative_ontology:cs_axiom('ded8f2b1-653b-48a6-9950-d1ae32dac5ed', foundational, conventional_combatant_status_as_prerequisite_for_pow).
narrative_ontology:cs_axiom_status(conventional_combatant_status_as_prerequisite_for_pow, holdable).
narrative_ontology:cs_axiom_grounding('ded8f2b1-653b-48a6-9950-d1ae32dac5ed', conventional_combatant_status_as_prerequisite_for_pow, conventional).
narrative_ontology:cs_reference_frame('ded8f2b1-653b-48a6-9950-d1ae32dac5ed', post_wwii_state_centric_warfare).
narrative_ontology:cs_drift_state('ded8f2b1-653b-48a6-9950-d1ae32dac5ed', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ded8f2b1-653b-48a6-9950-d1ae32dac5ed', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_military_forces).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_security_apparatus).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_in_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate under the interpretation that POW protections are conditional on adversary compliance with Article 4 criteria. They benefit from the flexibility to deny full protections to irregular forces, reducing operational constraints and perceived risks. They enforce this interpretation through military doctrine and rules of engagement.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_military_forces, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from the ability to detain and interrogate irregular combatants without full POW status, citing their non-compliance with conventional warfare rules. This enhances national security operations by allowing more aggressive intelligence gathering and counter-insurgency tactics.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_security_apparatus, beneficiary,
    institutional, generational, constrained, national).

% Bear the cost of denied POW status, facing harsher detention conditions, interrogation methods, and potential prosecution as criminals rather than combatants. Their lack of conventional military structure and insignia, while often a tactical necessity, is used to justify their exclusion from full protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants, payer,
    powerless, immediate, trapped, local).

% Experience a narrowed scope of protection due to proportionality calculations that weigh military advantage against civilian harm. While direct targeting is prohibited, the threshold for 'collateral damage' is higher under this reading, leading to increased civilian casualties and displacement.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Advocate for a broader application of humanitarian protections, arguing against the conditional interpretation of POW status and for stricter limits on proportionality. Their voice is often marginalized in state security debates, and their access to conflict zones can be restricted by states operating under this reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_humanitarian_organizations, excluded,
    organized, generational, constrained, global).

% Analyze the legal implications and practical consequences of this reading, often critiquing its erosion of universal humanitarian principles. They provide academic commentary but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for reciprocal restraint in armed conflict, aiming to reduce overall suffering by incentivizing adversaries to adhere to common rules, particularly regarding the treatment of captured personnel and protection of civilians, by making protections conditional on compliance.
% TRANSFER_FUNCTION: Transfers the burden of risk and suffering from conventional state military forces to irregular combatants and, indirectly, to civilian populations, by making protections conditional on adherence to conventional rules of warfare.
% ABSENT_VOICES: Irregular combatants and their political representatives, as well as human rights advocates, are largely excluded from the interpretive process that defines 'compliance' and 'proportionality' in this reading. They would argue for universal protections regardless of combatant status or reciprocity.
% DISAPPEARANCE_RATIONALE: If this conditional reciprocity reading vanished, state military forces would face increased pressure to apply universal humanitarian protections, potentially altering rules of engagement, detention policies, and proportionality calculations in ways that would significantly impact operational flexibility and perceived security interests. The legal and ethical landscape of armed conflict would be fundamentally reshaped.
% FOUNDING_PROBLEM: The original Geneva Conventions sought to humanize warfare by establishing minimum standards for the treatment of combatants and civilians, particularly after the atrocities of World War II, aiming to prevent a descent into total war without rules.
% FOUNDING_PROBLEM_CORROBORATION: State military and security forces attest that the problem of managing conflict and incentivizing reciprocal restraint remains live, especially in asymmetric warfare. International humanitarian organizations and legal scholars, while disagreeing on the interpretation, corroborate the ongoing need for rules of war, albeit with different emphasis on their application.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).
:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because while it provides some framework for restraint, it allows for significant degradation of protections for irregular combatants and civilians under certain conditions. Suppression (0.6) is substantial as it actively denies full legal status and protections to those who do not conform to conventional military structures, effectively suppressing alternative forms of resistance. Theater ratio (0.2) is low, as the legal arguments and operational practices are genuinely applied, though they may be contested. The temporal measurements reflect an increase in extractiveness and suppression post-9/11, as asymmetric warfare became more prevalent, followed by a slight decrease as legal challenges and international pressure mounted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state military forces, this reading provides necessary flexibility to address modern threats while maintaining a framework for humane conduct. From the perspective of irregular combatants and humanitarian organizations, it represents a dangerous erosion of universal protections, creating a two-tiered system of justice in conflict. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   State military forces and security apparatus are beneficiaries, gaining operational flexibility and reduced constraints. Irregular combatants and civilian populations in conflict zones are victims, bearing the costs of reduced protections. International humanitarian organizations are excluded, advocating for universal application of the law but lacking direct power in this interpretation. International legal scholars act as observers, analyzing and critiquing the reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_calculation_ambiguity,
    'How are ''military advantage'' and ''proportionality'' objectively measured in practice, and to what extent do these calculations genuinely minimize civilian harm versus rationalize it?',
    'Independent, real-time monitoring and post-conflict analysis by neutral parties of targeting decisions and their outcomes, with access to military intelligence and operational data.',
    'If calculations are found to consistently prioritize military advantage over civilian protection beyond reasonable thresholds, it would increase the effective extractiveness and suppression of this reading, potentially reclassifying it closer to a Snare for civilian populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calculation_ambiguity, empirical, 'Ambiguity in the practical application of proportionality principles.').

omega_variable(
    article_4_relevance_in_asymmetric_warfare,
    'Are the Article 4 criteria for POW status (organized command, distinctive insignia, carrying arms openly) still relevant and appropriate for classifying combatants in modern asymmetric conflicts, or do they unfairly disadvantage non-state actors?',
    'International legal review and consensus-building among states and non-state actors on updated criteria for combatant status that reflect contemporary conflict realities, or empirical studies on the practical implications of applying current criteria.',
    'If Article 4 is deemed outdated or unfairly applied, the denial of POW status to irregulars would be seen as pure extraction, increasing the effective extractiveness for irregular combatants and potentially shifting the overall classification towards a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_4_relevance_in_asymmetric_warfare, conceptual, 'Relevance of traditional POW criteria in modern conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1970, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(gene_tr_t2005, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(gene_tr_t2015, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement(gene_be_t1970, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(gene_be_t2005, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(gene_be_t2015, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement(gene_su_t1970, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(gene_su_t2005, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(gene_su_t2015, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This is one of three readings of the Geneva Conventions (1949) kernel, alongside 'humanitarian_ceiling_reading' and 'security_maximization_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
