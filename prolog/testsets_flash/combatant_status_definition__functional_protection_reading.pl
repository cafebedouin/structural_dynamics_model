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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Universal Protections (Functional Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint represents the 'functional protection' reading of
 *   combatant status in International Humanitarian Law (IHL). It asserts that
 *   Common Article 3 of the Geneva Conventions establishes a non-derogable
 *   minimum standard of humane treatment and fair trial rights for all
 *   persons detained in armed conflict, regardless of their formal combatant
 *   status. This reading emphasizes the universal application of fundamental
 *   protections, removing status determination as a precondition for basic
 *   human dignity and due process. It is a 'rope' because it genuinely
 *   coordinates humanitarian action and benefits all parties by setting
 *   clear, universal standards, with minimal extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.15).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.2).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Universal Protections (Functional Reading)").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '5526a331-0782-46c9-a11d-0c1efedec5bd').
narrative_ontology:cs_kernel_codification('5526a331-0782-46c9-a11d-0c1efedec5bd', fixed_text).
narrative_ontology:cs_authority_grounding('5526a331-0782-46c9-a11d-0c1efedec5bd', lineage).
narrative_ontology:cs_interpretation_layer_present('5526a331-0782-46c9-a11d-0c1efedec5bd').
narrative_ontology:cs_reading_relation('5526a331-0782-46c9-a11d-0c1efedec5bd', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('5526a331-0782-46c9-a11d-0c1efedec5bd', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('5526a331-0782-46c9-a11d-0c1efedec5bd', foundational, universal_human_dignity).
narrative_ontology:cs_axiom_status(universal_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('5526a331-0782-46c9-a11d-0c1efedec5bd', universal_human_dignity, deontological).
narrative_ontology:cs_axiom('5526a331-0782-46c9-a11d-0c1efedec5bd', foundational, status_independent_protections).
narrative_ontology:cs_axiom_status(status_independent_protections, holdable).
narrative_ontology:cs_axiom_grounding('5526a331-0782-46c9-a11d-0c1efedec5bd', status_independent_protections, conventional).
narrative_ontology:cs_reference_frame('5526a331-0782-46c9-a11d-0c1efedec5bd', geneva_conventions_common_article_3_framework).
narrative_ontology:cs_drift_state('5526a331-0782-46c9-a11d-0c1efedec5bd', post_9_11_counter_terrorism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5526a331-0782-46c9-a11d-0c1efedec5bd', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, all_detained_persons).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, international_humanitarian_law_regime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives minimum humane treatment and fair trial rights, regardless of their combatant status or the nature of the conflict. Their protections are not contingent on formal classification.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, all_detained_persons, beneficiary,
    powerless, immediate, trapped, global).

% Obligated to provide Common Article 3 protections to all persons in their custody, irrespective of status. This reading imposes a baseline standard that limits their discretion in treatment and judicial process, even for those not classified as POWs.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_powers, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from the universal application of fundamental protections, reinforcing the humanitarian principles underlying the Geneva Conventions and strengthening the norm of humane treatment in all armed conflicts.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_humanitarian_law_regime, beneficiary,
    institutional, civilizational, analytical, universal).

% Monitor compliance with Common Article 3 and advocate for its universal application, challenging interpretations that seek to limit protections based on status. They use this reading to press for accountability.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, human_rights_advocates, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-derogable baseline of humane treatment and fair trial rights for all persons detained in armed conflict, preventing a 'rights vacuum' for those not covered by full POW status.
% TRANSFER_FUNCTION: Transfers the burden of ensuring humane treatment and due process from a status-dependent determination to a universal obligation on detaining powers, regardless of the detainee's classification.
% ABSENT_VOICES: Those who advocate for maximal state discretion in classifying and treating non-state combatants, particularly in counter-terrorism contexts, would object to this reading's universalizing effect, arguing it unduly constrains state security operations.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape for detainees would revert to a more status-dependent system, potentially leaving many non-state actors without clear protections, leading to increased arbitrary detention, torture, and denial of fair trial rights. The international legal regime would be significantly weakened.
% FOUNDING_PROBLEM: To ensure a minimum standard of humanity and justice for all persons caught in armed conflict, especially those not covered by traditional Prisoner of War status, preventing abuses in 'non-international' conflicts.
% FOUNDING_PROBLEM_CORROBORATION: International courts (e.g., ICTY, ICJ), UN bodies, and major human rights organizations consistently affirm the live status of this problem and the necessity of Common Article 3's universal application. Legal scholars and independent experts widely corroborate this view, often in opposition to state-centric interpretations.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).

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
 *   Extractiveness is low (0.15) because the constraint primarily sets a baseline of protection, not a mechanism for resource transfer. Suppression is low (0.2) as it's a widely accepted norm, though states may resist its application in specific contexts (e.g., counter-terrorism). Theater ratio is low (0.1) because the core function of providing humane treatment is generally upheld, even if its scope is sometimes contested. The slight increase in extractiveness and suppression around 2001 reflects the 'War on Terror' era, where some states attempted to create 'rights-free zones' for certain detainees, leading to increased contestation and enforcement pressure to uphold CA3.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of detained persons, this is a vital, life-saving protection. From the perspective of detaining powers, it is a necessary but sometimes burdensome obligation that limits their operational flexibility. The international legal community views it as a cornerstone of IHL. These perspectives align on the necessity of the constraint, differing mainly on the practical costs of implementation.
 *
 * DIRECTIONALITY LOGIC:
 *   All detained persons are clear beneficiaries, receiving fundamental protections. Detaining powers are agenda-setters, bearing the cost of compliance but also benefiting from a clear, universal standard that reduces legal ambiguity. The IHL regime itself is a beneficiary, as its core principles are reinforced. There are no direct 'victims' of this reading, as its purpose is to prevent harm.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_armed_conflict,
    'What constitutes an ''armed conflict'' for the purposes of triggering Common Article 3, particularly in ambiguous or protracted situations (e.g., counter-terrorism operations, internal disturbances)?',
    'Consistent jurisprudence from international courts (e.g., ICJ, ICC) or a universally adopted authoritative interpretation by states parties.',
    'A narrow definition would limit the application of CA3, effectively increasing extraction for those excluded; a broad definition would expand protections, reducing potential extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_armed_conflict, conceptual, 'Ambiguity in the threshold for Common Article 3 application.').

omega_variable(
    fair_trial_standards_in_non_international_armed_conflict,
    'What specific ''fair trial'' standards are required by Common Article 3 in non-international armed conflicts, given the absence of detailed provisions like those for POWs?',
    'Development of customary international law, authoritative interpretations by international bodies, or state practice converging on specific minimum standards.',
    'Lack of clarity allows detaining powers to apply lower standards, increasing extraction for detainees; clearer standards would reduce this discretion and enhance protections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_trial_standards_in_non_international_armed_conflict, empirical, 'Uncertainty regarding specific fair trial requirements under CA3.').

omega_variable(
    natural_law_vs_conventional_law,
    'Is the principle of humane treatment in Common Article 3 a reflection of natural law, or is its force derived purely from conventional (treaty) law?',
    'Philosophical and legal consensus on the grounding of fundamental human rights, or a definitive ruling by a global judicial body on the non-derogable nature of these principles independent of state consent.',
    'If natural law, its universality and non-derogability are inherent, making any attempt to limit it a clear violation. If purely conventional, its persistence depends more on state consent and interpretation, potentially allowing for greater contestation and erosion of protections.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_conventional_law, conceptual, 'The philosophical grounding of Common Article 3''s humane treatment principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__functional_protection_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(comb_tr_t1970, combatant_status_definition__functional_protection_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__functional_protection_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__functional_protection_reading, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__functional_protection_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__functional_protection_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__functional_protection_reading, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement(comb_be_t1970, combatant_status_definition__functional_protection_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__functional_protection_reading, base_extractiveness, 1990, 0.13).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__functional_protection_reading, base_extractiveness, 2001, 0.18).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__functional_protection_reading, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__functional_protection_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__functional_protection_reading, suppression_requirement, 1949, 0.15).
narrative_ontology:measurement(comb_su_t1970, combatant_status_definition__functional_protection_reading, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__functional_protection_reading, suppression_requirement, 1990, 0.17).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__functional_protection_reading, suppression_requirement, 2001, 0.25).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__functional_protection_reading, suppression_requirement, 2010, 0.22).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__functional_protection_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'functional protection' reading of the 'combatant_status_definition' kernel. It focuses on universal minimum protections, influencing but not foreclosing other readings that define combatant status more narrowly or broadly for specific groups.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
