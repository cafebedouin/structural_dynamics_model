% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Normative Status of Borders: Freedom of Movement Primary Reading
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'freedom of movement primary' reading of
 *   the normative status of borders. It asserts that freedom of movement is a
 *   fundamental human right, and therefore, borders, by restricting this
 *   right, are impermissible unless justified by extraordinary circumstances.
 *   From this perspective, border enforcement is a highly extractive and
 *   suppressive mechanism that creates victims (excluded migrants and
 *   displaced domestic workers in origin states) and unjustly benefits others
 *   (domestic workers in destination states). The constraint is classified as
 *   a Snare because its primary function is seen as extraction and
 *   suppression, with any coordination benefits being secondary or
 *   illegitimate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.95).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.98).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.95).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Normative Status of Borders: Freedom of Movement Primary Reading").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, 'd5d1b7eb-cf1b-4a5a-8653-9144e310b1a0').
narrative_ontology:cs_kernel_codification('d5d1b7eb-cf1b-4a5a-8653-9144e310b1a0', distributed).
narrative_ontology:cs_authority_grounding('d5d1b7eb-cf1b-4a5a-8653-9144e310b1a0', distributed).
narrative_ontology:cs_reading_relation('d5d1b7eb-cf1b-4a5a-8653-9144e310b1a0', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('d5d1b7eb-cf1b-4a5a-8653-9144e310b1a0', border_normative_status__qualified_sovereignty, forecloses).
narrative_ontology:cs_axiom('d5d1b7eb-cf1b-4a5a-8653-9144e310b1a0', foundational, freedom_of_movement_is_a_fundamental_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_a_fundamental_human_right, holdable).
narrative_ontology:cs_axiom_grounding('d5d1b7eb-cf1b-4a5a-8653-9144e310b1a0', freedom_of_movement_is_a_fundamental_human_right, deontological).
narrative_ontology:cs_axiom('d5d1b7eb-cf1b-4a5a-8653-9144e310b1a0', foundational, state_sovereignty_is_subordinate_to_individual_rights).
narrative_ontology:cs_axiom_status(state_sovereignty_is_subordinate_to_individual_rights, holdable).
narrative_ontology:cs_axiom_grounding('d5d1b7eb-cf1b-4a5a-8653-9144e310b1a0', state_sovereignty_is_subordinate_to_individual_rights, deontological).
narrative_ontology:cs_reference_frame('d5d1b7eb-cf1b-4a5a-8653-9144e310b1a0', universal_rights_framework).
narrative_ontology:cs_drift_state('d5d1b7eb-cf1b-4a5a-8653-9144e310b1a0', contemporary_global_politics, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d5d1b7eb-cf1b-4a5a-8653-9144e310b1a0', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, domestic_workers_in_destination_states).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers_in_origin_states).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, universal_human_rights_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, cosmopolitanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Denied entry to desired territories, facing economic hardship, family separation, and often dangerous conditions in transit or origin states. Their fundamental right to movement is violated by border regimes.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Benefit from reduced competition for jobs in destination states due to border restrictions, which can lead to higher wages or better working conditions. This benefit is seen as an unjust externality of rights violations.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, domestic_workers_in_destination_states, beneficiary,
    moderate, biographical, constrained, national).

% Suffer from lack of economic opportunity in their home countries, often exacerbated by global economic inequalities and the inability to seek work abroad. Their inability to move freely contributes to their economic precarity.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers_in_origin_states, payer,
    powerless, biographical, constrained, national).

% Actively enforce border restrictions, deploying resources to prevent unauthorized entry. From this reading's perspective, their actions constitute a violation of fundamental human rights, requiring extraordinary justification that is rarely met.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Claim a right to control their borders as an expression of national sovereignty and self-determination. This reading views such claims as secondary to individual human rights and requiring robust justification for any restriction on movement.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, states_asserting_sovereignty, agenda_setter,
    institutional, civilizational, constrained, national).

% Champion the universal right to freedom of movement and challenge state practices that restrict it. They provide legal and ethical arguments against current border regimes and support migrants' rights.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint primarily coordinates the exclusion of non-citizens, rather than solving a legitimate collective action problem. Any 'coordination' is for the benefit of a subset of humanity at the expense of others' fundamental rights.
% TRANSFER_FUNCTION: Transfers the right to reside and work in desirable territories from excluded migrants to existing citizens and domestic workers, along with the associated economic and social benefits. It also transfers the burden of economic precarity to those denied movement.
% ABSENT_VOICES: The voices of those denied entry, those suffering in transit, and those whose economic opportunities are curtailed by borders are systematically marginalized in national policy debates. Their perspectives are often represented only by advocates.
% DISAPPEARANCE_RATIONALE: If borders ceased to restrict movement overnight, there would be significant global migration flows, leading to a redistribution of labor, capital, and cultural exchange. Destination states would experience increased competition for jobs and resources, while origin states might see a 'brain drain' or reduced pressure on local labor markets. The global economic and social order would fundamentally reorganize.
% FOUNDING_PROBLEM: The problem of managing population flows, protecting national labor markets, and maintaining cultural cohesion within defined territories.
% FOUNDING_PROBLEM_CORROBORATION: States and their citizens often attest that these problems are live and require border controls. However, human rights advocates and some economists argue that the 'problem' is often a pretext for maintaining unjust privileges and that alternative, rights-respecting solutions exist. The framing of the 'problem' itself is part of the contest.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.95) because the constraint denies a fundamental right, imposing severe costs on excluded individuals. Suppression is also very high (0.98) as states employ significant coercive force to maintain borders, with few legitimate alternatives for those seeking entry. Theater ratio is low (0.05) because the enforcement is genuinely functional in preventing movement, not merely performative. Resistance is high (0.85) due to ongoing efforts by migrants and advocates to challenge border regimes. Accessibility collapse is low (0.1) because the 'alternatives' (unrestricted movement) are conceptually clear, even if practically suppressed.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between those who view freedom of movement as a fundamental right (this reading) and those who prioritize state sovereignty. For excluded migrants, the border is a pure snare. For states, it is a legitimate exercise of authority. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded migrants and displaced domestic workers are clear targets (high d) as they bear the direct costs of restricted movement. Domestic workers in destination states are beneficiaries (low d) as they benefit from reduced labor competition. Border enforcement agencies and states asserting sovereignty are agenda-setters (d near symmetric to slightly beneficiary, as they maintain the system from which they derive authority and some benefits). Human rights advocates are observers (analytical d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    justification_threshold_ambiguity,
    'What constitutes ''extraordinary justification'' for restricting freedom of movement, and who adjudicates it?',
    'Development of international legal precedents and a robust, independent international tribunal with enforcement powers to review state justifications for border controls.',
    'If the threshold for ''extraordinary justification'' is set very high and rigorously enforced, the constraint would be reclassified closer to a Mountain (if truly natural limits) or a Rope (if legitimate coordination is found). If the threshold remains vague or state-determined, the Snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justification_threshold_ambiguity, conceptual, 'Ambiguity in the criteria and authority for justifying border restrictions.').

omega_variable(
    displaced_domestic_workers_causality,
    'To what extent are the economic disadvantages of domestic workers in origin states directly caused by border restrictions, versus other factors like global economic inequality or domestic policy failures?',
    'Empirical studies comparing economic outcomes in origin states with varying degrees of migration access, controlling for other economic variables.',
    'If a strong causal link is established, it reinforces the victim status of these workers and the extractive nature of the border constraint. If the link is weak, their victim status might be re-evaluated, potentially lowering the overall extractiveness from this seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_domestic_workers_causality, empirical, 'Causal link between border restrictions and economic displacement in origin states.').

omega_variable(
    internalized_suppression_of_movement,
    'Is the suppression of movement purely structural (physical borders, legal penalties) or is there an internalized component (e.g., fear, cultural norms against leaving home) that persists even if structural barriers were removed?',
    'Post-liberalization studies: if significant numbers of people still do not move after structural barriers are removed, it suggests an internalized component.',
    'If internalized suppression is significant, the effective suppression of the constraint is higher than the structural measure suggests, as individuals carry the suppression with them even after exit options theoretically improve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_movement, empirical, 'Structural vs. internalized suppression mechanism for freedom of movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__freedom_primary, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bord_tr_t10, border_normative_status__freedom_primary, theater_ratio, 10, 0.05).
narrative_ontology:measurement(bord_tr_t20, border_normative_status__freedom_primary, theater_ratio, 20, 0.05).
narrative_ontology:measurement(bord_tr_t30, border_normative_status__freedom_primary, theater_ratio, 30, 0.05).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__freedom_primary, theater_ratio, 40, 0.05).
narrative_ontology:measurement(bord_tr_t50, border_normative_status__freedom_primary, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__freedom_primary, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(bord_be_t10, border_normative_status__freedom_primary, base_extractiveness, 10, 0.92).
narrative_ontology:measurement(bord_be_t20, border_normative_status__freedom_primary, base_extractiveness, 20, 0.93).
narrative_ontology:measurement(bord_be_t30, border_normative_status__freedom_primary, base_extractiveness, 30, 0.94).
narrative_ontology:measurement(bord_be_t40, border_normative_status__freedom_primary, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(bord_be_t50, border_normative_status__freedom_primary, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__freedom_primary, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(bord_su_t10, border_normative_status__freedom_primary, suppression_requirement, 10, 0.92).
narrative_ontology:measurement(bord_su_t20, border_normative_status__freedom_primary, suppression_requirement, 20, 0.94).
narrative_ontology:measurement(bord_su_t30, border_normative_status__freedom_primary, suppression_requirement, 30, 0.96).
narrative_ontology:measurement(bord_su_t40, border_normative_status__freedom_primary, suppression_requirement, 40, 0.97).
narrative_ontology:measurement(bord_su_t50, border_normative_status__freedom_primary, suppression_requirement, 50, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, qualified_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('freedom_primary') of the 'border_normative_status' kernel. It is linked to sibling readings 'sovereignty_primary' and 'qualified_sovereignty', which offer alternative justifications for border control. The ε values differ significantly across these readings, reflecting their distinct structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
