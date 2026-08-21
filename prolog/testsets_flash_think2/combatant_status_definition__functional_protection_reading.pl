% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Universal Common Article 3 Protections for Detainees
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'functional protection' reading of
 *   combatant status definition within International Humanitarian Law (IHL).
 *   It asserts that all persons detained in armed conflict are entitled to
 *   Common Article 3 minimum protections, including humane treatment and fair
 *   trial rights, regardless of their formal combatant status. This reading
 *   emphasizes the universal application of basic human dignity and due
 *   process, aiming to close gaps where states might deny protections by
 *   refusing to classify individuals as Prisoners of War (POWs). The
 *   constraint is claimed as a Rope because it coordinates states towards a
 *   common, beneficial standard for detainee treatment, with minimal
 *   extraction from those it governs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.1).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.1).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Universal Common Article 3 Protections for Detainees").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law/human_rights").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '48a2b273-35be-4e6f-ae8c-260703daf91b').
narrative_ontology:cs_kernel_codification('48a2b273-35be-4e6f-ae8c-260703daf91b', formalized).
narrative_ontology:cs_authority_grounding('48a2b273-35be-4e6f-ae8c-260703daf91b', lineage).
narrative_ontology:cs_interpretation_layer_present('48a2b273-35be-4e6f-ae8c-260703daf91b').
narrative_ontology:cs_reading_relation('48a2b273-35be-4e6f-ae8c-260703daf91b', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('48a2b273-35be-4e6f-ae8c-260703daf91b', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('48a2b273-35be-4e6f-ae8c-260703daf91b', foundational, human_dignity_is_universal).
narrative_ontology:cs_axiom_status(human_dignity_is_universal, holdable).
narrative_ontology:cs_axiom_grounding('48a2b273-35be-4e6f-ae8c-260703daf91b', human_dignity_is_universal, deontological).
narrative_ontology:cs_axiom('48a2b273-35be-4e6f-ae8c-260703daf91b', foundational, common_article_3_is_minimum_floor).
narrative_ontology:cs_axiom_status(common_article_3_is_minimum_floor, holdable).
narrative_ontology:cs_axiom_grounding('48a2b273-35be-4e6f-ae8c-260703daf91b', common_article_3_is_minimum_floor, conventional).
narrative_ontology:cs_reference_frame('48a2b273-35be-4e6f-ae8c-260703daf91b', universal_human_dignity_framework).
narrative_ontology:cs_drift_state('48a2b273-35be-4e6f-ae8c-260703daf91b', contemporary_conflict_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('48a2b273-35be-4e6f-ae8c-260703daf91b', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, all_detained_persons).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, ihl_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, military_commanders).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, detaining_authorities).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, universal_human_dignity).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, rule_of_law_in_armed_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons deprived of liberty during armed conflict, who receive a baseline of humane treatment and fair process regardless of their formal combatant status. Their lives and dignity depend on this principle.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, all_detained_persons, beneficiary,
    powerless, immediate, trapped, universal).

% International legal scholars, human rights organizations, and states that champion the broadest application of IHL, seeing Common Article 3 as a non-derogable minimum standard for all detainees. They benefit from the principle's clarity and protective scope.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, ihl_advocates, beneficiary,
    organized, generational, mobile, global).

% The primary duty-bearers for implementing IHL. While some states may seek to limit its application, the majority uphold the principle as foundational to the international legal order, even if implementation varies. They are responsible for its enforcement and interpretation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, states_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, constrained, global).

% Responsible for ensuring their forces comply with IHL, including Common Article 3. They bear the operational 'cost' of providing humane treatment and due process to all detainees, even those they might prefer to classify as 'unlawful combatants' to deny protections.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, military_commanders, payer,
    powerful, immediate, constrained, regional).

% The specific personnel and institutions directly responsible for the custody and treatment of detainees. They must apply the protections in practice, which can be resource-intensive and operationally challenging, especially in complex conflict environments.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_authorities, payer,
    institutional, immediate, constrained, local).

% States or factions that argue for a strict, status-dependent application of IHL, seeking to deny basic protections to certain categories of detainees by classifying them outside traditional combatant frameworks. This reading structurally excludes their preferred interpretation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, states_seeking_status_dependent_protections, excluded,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-derogable baseline for humane treatment and fair process for all persons deprived of liberty during armed conflict, preventing a race to the bottom in detainee treatment and ensuring a minimum standard of human dignity.
% TRANSFER_FUNCTION: Transfers the obligation to provide humane treatment and fair process from being status-dependent to being universally applicable, placing a consistent burden on detaining powers and granting fundamental rights to all detainees, regardless of their classification.
% ABSENT_VOICES: States or actors who advocate for narrower interpretations of combatant status to deny protections, or those who believe national security concerns should override universal protections. They are structurally excluded from this reading's premise of status-independent minimums.
% DISAPPEARANCE_RATIONALE: If this principle vanished overnight, states would revert to status-dependent protections, leading to widespread denial of humane treatment and fair trial rights for many detainees, particularly in non-international armed conflicts. This would cause significant human suffering, legal chaos, and undermine the foundational principles of IHL.
% FOUNDING_PROBLEM: The historical problem of states denying basic human rights to persons captured in armed conflict by refusing to grant them combatant status or by classifying them outside existing legal frameworks, leading to arbitrary detention, torture, and summary executions.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and reports from UN bodies consistently corroborate that the problem of denying protections based on status remains live, particularly in asymmetric conflicts and in the context of counter-terrorism operations. Independent legal analysis from outside state-centric or national liberation factions supports the ongoing relevance of this problem.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.1) because this constraint primarily prevents harm and ensures a baseline of rights, rather than extracting from parties. Suppression is also low (0.1) as it's a protective norm, though states must actively suppress impulses to deny these rights. Theater ratio is minimal (0.05) as the principle is a core legal obligation, not a performance. Accessibility collapse is high (0.9) because it effectively collapses the alternative of denying basic rights based on status. Resistance is moderate (0.4) due to ongoing challenges from states seeking to limit IHL's application, particularly in asymmetric conflicts or counter-terrorism operations. The slight increase in extractiveness, suppression, and theater around 2001 reflects the post-9/11 period where some states attempted to create 'status-less' categories of detainees, leading to increased pressure on this protective norm.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of detained persons and IHL advocates, this is a vital protective Rope. From the perspective of military commanders and detaining authorities, it can be perceived as a burdensome obligation that limits operational flexibility, though still recognized as a legal duty. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   All detained persons are clear beneficiaries (d=0.0) as the constraint directly grants them fundamental protections. IHL advocates also benefit by seeing their principles upheld. States Parties to the Geneva Conventions act as agenda-setters, responsible for upholding and interpreting the law. Military commanders and detaining authorities are payers (d=1.0) as they bear the operational and resource costs of ensuring compliance, even when it conflicts with strategic or tactical preferences. States seeking status-dependent protections are structurally excluded, as their preferred interpretation is directly contradicted by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_vs_protection_precedence,
    'Does the functional protection reading truly foreclose the state-centric premise that basic protections are status-dependent, or do they merely coexist as competing interpretations?',
    'Analysis of international jurisprudence and state practice: if courts consistently rule that Common Article 3 applies universally regardless of status, it strengthens the foreclosure claim. If states continue to successfully deny basic protections based on status, it suggests coexistence.',
    'If foreclosure is strong, the state-centric reading''s ability to deny basic protections is legally undermined. If they merely coexist, the functional protection reading faces ongoing, legitimate legal challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_vs_protection_precedence, conceptual, 'The extent to which status-independent protections displace status-dependent ones.').

omega_variable(
    dilution_of_pow_status_advocacy,
    'Does the emphasis on universal Common Article 3 protections inadvertently dilute advocacy for broader Prisoner of War (POW) status for non-state actors (as sought by the national liberation reading)?',
    'Empirical study of advocacy trends and legal outcomes: if the focus on Common Article 3 leads to less successful efforts to expand POW status, it suggests a diluting effect. If both tracks advance independently, they coexist without dilution.',
    'If dilution occurs, the functional protection reading, while beneficial for minimums, might unintentionally hinder efforts to secure higher protections for certain groups. If not, both readings can be pursued in parallel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dilution_of_pow_status_advocacy, empirical, 'Whether universal minimums affect the pursuit of broader POW status.').

omega_variable(
    implementation_gap_severity,
    'To what extent is the principle of status-independent Common Article 3 protections honored in practice, versus being merely a theoretical ideal?',
    'Field reports from human rights monitors, ICRC observations, and independent investigations into detainee treatment in various conflict zones. High rates of non-compliance would indicate a severe implementation gap.',
    'A significant implementation gap would suggest the constraint''s effective extractiveness (χ) is higher for detainees than the base extractiveness (ε) suggests, due to the gap between declared protection and lived reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_gap_severity, empirical, 'The gap between declared IHL principles and actual state practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__functional_protection_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(comb_tr_t1970, combatant_status_definition__functional_protection_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__functional_protection_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__functional_protection_reading, theater_ratio, 2001, 0.08).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__functional_protection_reading, theater_ratio, 2010, 0.07).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__functional_protection_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__functional_protection_reading, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement(comb_be_t1970, combatant_status_definition__functional_protection_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__functional_protection_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__functional_protection_reading, base_extractiveness, 2001, 0.12).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__functional_protection_reading, base_extractiveness, 2010, 0.11).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__functional_protection_reading, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__functional_protection_reading, suppression_requirement, 1949, 0.1).
narrative_ontology:measurement(comb_su_t1970, combatant_status_definition__functional_protection_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__functional_protection_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__functional_protection_reading, suppression_requirement, 2001, 0.15).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__functional_protection_reading, suppression_requirement, 2010, 0.13).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__functional_protection_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, prohibition_of_torture).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'combatant_status_definition' kernel, focusing on the universal application of Common Article 3 protections. It is linked to its sibling readings and the broader prohibition of torture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
