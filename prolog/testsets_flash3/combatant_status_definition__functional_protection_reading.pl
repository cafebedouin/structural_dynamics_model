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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Common Article 3 Universal Protections (Functional Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint represents the 'functional protection' reading of
 *   combatant status in International Humanitarian Law (IHL), asserting that
 *   all detained persons are entitled to Common Article 3 minimum
 *   protections, regardless of their formal combatant status. This reading
 *   emphasizes humane treatment and fair trial rights as status-independent,
 *   aiming to prevent legal vacuums and ensure a baseline of dignity in armed
 *   conflict. It is a 'rope' because it provides essential coordination for
 *   humanitarian action with low inherent extraction, though its enforcement
 *   requires active effort.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.15).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.25).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Universal Protections (Functional Reading)").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '5267568c-2ef7-4291-8858-97b70beaf516').
narrative_ontology:cs_kernel_codification('5267568c-2ef7-4291-8858-97b70beaf516', fixed_text).
narrative_ontology:cs_authority_grounding('5267568c-2ef7-4291-8858-97b70beaf516', lineage).
narrative_ontology:cs_interpretation_layer_present('5267568c-2ef7-4291-8858-97b70beaf516').
narrative_ontology:cs_reading_relation('5267568c-2ef7-4291-8858-97b70beaf516', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('5267568c-2ef7-4291-8858-97b70beaf516', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('5267568c-2ef7-4291-8858-97b70beaf516', foundational, universal_humanitarian_floor).
narrative_ontology:cs_axiom_status(universal_humanitarian_floor, holdable).
narrative_ontology:cs_axiom_grounding('5267568c-2ef7-4291-8858-97b70beaf516', universal_humanitarian_floor, deontological).
narrative_ontology:cs_axiom('5267568c-2ef7-4291-8858-97b70beaf516', foundational, status_independent_dignity).
narrative_ontology:cs_axiom_status(status_independent_dignity, holdable).
narrative_ontology:cs_axiom_grounding('5267568c-2ef7-4291-8858-97b70beaf516', status_independent_dignity, deontological).
narrative_ontology:cs_reference_frame('5267568c-2ef7-4291-8858-97b70beaf516', geneva_conventions_common_article_3_framework).
narrative_ontology:cs_drift_state('5267568c-2ef7-4291-8858-97b70beaf516', post_9_11_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5267568c-2ef7-4291-8858-97b70beaf516', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, all_detained_persons).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, international_humanitarian_law_regime).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, humanitarian_imperative).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, rule_of_law_in_armed_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives minimum humane treatment and fair trial rights regardless of their combatant status, as a baseline protection. Their situation is one of extreme vulnerability, making these protections critical.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, all_detained_persons, beneficiary,
    powerless, immediate, trapped, global).

% Are bound by Common Article 3 to provide minimum protections. They are responsible for implementing and enforcing these standards, often balancing this with national security concerns. Their compliance is subject to international scrutiny.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from the strengthening of universal humanitarian principles, enhancing its legitimacy and effectiveness. The functional protection reading reinforces the regime's core values.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_humanitarian_law_regime, beneficiary,
    institutional, civilizational, analytical, universal).

% Monitor compliance with Common Article 3 and advocate for its universal application. They provide critical oversight and pressure for states to adhere to these protections.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, human_rights_advocates, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline of humane treatment and fair trial rights for all persons detained in armed conflict, preventing a legal vacuum based on status determination and ensuring a minimum standard of conduct.
% TRANSFER_FUNCTION: Transfers the burden of proof and the responsibility for humane treatment from the detainee's status to the detaining power's universal obligations, ensuring a floor of protection for all.
% ABSENT_VOICES: Those who advocate for absolute state sovereignty or who seek to deny protections to certain categories of combatants (e.g., 'unlawful combatants') are often marginalized in discussions centered on universal humanitarian principles. They would argue for greater discretion for detaining powers.
% DISAPPEARANCE_RATIONALE: If this functional reading of Common Article 3 vanished, the legal landscape for detainees would become highly fragmented and dependent on arbitrary status determinations, leading to widespread abuses and a collapse of minimum humanitarian standards. The international legal order would be significantly undermined.
% FOUNDING_PROBLEM: The need to establish a minimum standard of humane treatment for all persons caught in armed conflict, particularly in non-international armed conflicts, where traditional combatant status definitions were inadequate.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and the International Committee of the Red Cross consistently attest to the ongoing necessity and live status of this problem, citing contemporary conflicts and the evolving nature of warfare. This corroboration comes from outside the direct state parties who might benefit from narrower interpretations.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.15) because the constraint primarily establishes a floor of protection, benefiting all detainees without imposing significant costs beyond basic humane treatment. Suppression is moderate (0.25) as states may resist universal application, particularly for 'unlawful combatants,' requiring active enforcement and advocacy. Theater ratio is low (0.1) as the core function of providing protection is genuine, not performative. Accessibility collapse is high (0.7) because once this principle is accepted, alternatives that deny basic rights become largely untenable. Resistance is moderate (0.3) due to ongoing state resistance to universalizing protections, especially in contexts of counter-terrorism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of detained persons and humanitarian organizations, this is a vital protective 'rope.' From the perspective of states seeking maximum discretion in wartime, it can be seen as a 'tangled rope' or even a 'snare' that limits their operational flexibility, though this reading aims to minimize that perception by focusing on universal minimums.
 *
 * DIRECTIONALITY LOGIC:
 *   All detained persons are direct beneficiaries (d=0.0). State parties are agenda-setters, bearing the cost of compliance but also benefiting from a more stable and legitimate international order (d=0.5). The IHL regime itself is a beneficiary, as its principles are upheld. There are no direct 'victims' in this reading, as its purpose is to prevent victimhood.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling essential humanitarian coordination as extraction. By establishing a universal floor, it ensures that the mandate of humane treatment remains live and relevant, even as definitions of combatant status are contested. It guards against the 'mandatrophy' of humanitarian principles by making them status-independent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_fair_trial_rights,
    'What constitutes ''fair trial rights'' under Common Article 3, and how does this apply to non-state actors or in contexts of military commissions?',
    'Further clarification through international jurisprudence, state practice, and expert interpretation, particularly regarding due process standards in non-traditional judicial settings.',
    'A narrow interpretation could increase effective extraction by limiting detainee protections, potentially shifting the classification towards a ''tangled rope'' for detainees. A broad interpretation would reinforce the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_fair_trial_rights, conceptual, 'Ambiguity regarding the specific content and application of fair trial rights for all detainees.').

omega_variable(
    state_resistance_to_universalism,
    'To what extent do states actively resist the universal application of Common Article 3, particularly in counter-terrorism operations, and how does this affect de facto protections?',
    'Empirical studies of state practice, judicial review of detention policies, and reports from international monitoring bodies on specific conflicts.',
    'High and sustained state resistance could indicate that the ''suppression'' metric is underestimated, and that the constraint functions more as a ''tangled rope'' or ''snare'' for certain categories of detainees, despite its stated intent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_resistance_to_universalism, empirical, 'The gap between the normative claim of universal protection and actual state practice.').

omega_variable(
    kernel_reading_divergence,
    'Is this functional protection reading genuinely compatible with the ''state_centric_reading'' or ''national_liberation_reading'' within a single coherent IHL framework, or do they represent fundamentally irreconcilable approaches?',
    'Analysis of international court decisions, state treaty reservations, and scholarly debate on the hierarchy and interpretation of IHL principles.',
    'If irreconcilable, the ''coexists_with'' relation might be too weak, suggesting a ''forecloses'' relationship where one reading''s adoption fundamentally undermines the other''s legitimacy, leading to deeper fragmentation of the IHL regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'The degree of conceptual compatibility between this reading and its sibling interpretations of combatant status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__functional_protection_reading, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement(comb_be_t1970, combatant_status_definition__functional_protection_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__functional_protection_reading, base_extractiveness, 1990, 0.13).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__functional_protection_reading, base_extractiveness, 2001, 0.2).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__functional_protection_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__functional_protection_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__functional_protection_reading, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement(comb_su_t1970, combatant_status_definition__functional_protection_reading, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__functional_protection_reading, suppression_requirement, 1990, 0.23).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__functional_protection_reading, suppression_requirement, 2001, 0.35).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__functional_protection_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__functional_protection_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__functional_protection_reading, 0.1).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, prohibition_of_torture).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, right_to_due_process_in_detention).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'combatant_status_definition' kernel, focusing on universal protections. It influences and is influenced by the other readings, as they all interpret the same core IHL principles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
