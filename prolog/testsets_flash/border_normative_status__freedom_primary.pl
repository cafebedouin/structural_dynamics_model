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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Freedom of Movement as Primary Right (Border Normative Status)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'freedom primary' reading of the border
 *   normative status kernel, asserting that freedom of movement is a
 *   fundamental human right that borders impermissibly restrict, and
 *   exclusion requires extraordinary justification. From this perspective,
 *   existing border regimes are highly extractive and suppressive, creating
 *   victims among those denied entry and those negatively impacted by
 *   restricted labor mobility. The constraint is classified as a Snare due to
 *   its high extraction, active enforcement, and identifiable victims, with
 *   the coordination story (national security, economic protection) serving
 *   as cover for rights violations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.85).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.95).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Freedom of Movement as Primary Right (Border Normative Status)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '875a380b-3c43-4af3-a32a-323f385bb6f0').
narrative_ontology:cs_kernel_codification('875a380b-3c43-4af3-a32a-323f385bb6f0', distributed).
narrative_ontology:cs_authority_grounding('875a380b-3c43-4af3-a32a-323f385bb6f0', distributed).
narrative_ontology:cs_reading_relation('875a380b-3c43-4af3-a32a-323f385bb6f0', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('875a380b-3c43-4af3-a32a-323f385bb6f0', border_normative_status__qualified_sovereignty, forecloses).
narrative_ontology:cs_axiom('875a380b-3c43-4af3-a32a-323f385bb6f0', foundational, freedom_of_movement_is_a_fundamental_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_a_fundamental_human_right, holdable).
narrative_ontology:cs_axiom_grounding('875a380b-3c43-4af3-a32a-323f385bb6f0', freedom_of_movement_is_a_fundamental_human_right, deontological).
narrative_ontology:cs_axiom('875a380b-3c43-4af3-a32a-323f385bb6f0', foundational, borders_are_presumptively_illegitimate_restrictions).
narrative_ontology:cs_axiom_status(borders_are_presumptively_illegitimate_restrictions, holdable).
narrative_ontology:cs_axiom_grounding('875a380b-3c43-4af3-a32a-323f385bb6f0', borders_are_presumptively_illegitimate_restrictions, deontological).
narrative_ontology:cs_reference_frame('875a380b-3c43-4af3-a32a-323f385bb6f0', universal_human_rights_framework).
narrative_ontology:cs_drift_state('875a380b-3c43-4af3-a32a-323f385bb6f0', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('875a380b-3c43-4af3-a32a-323f385bb6f0', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, domestic_workers_in_high_wage_countries).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, global_capital_flows).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers_in_low_wage_countries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Denied entry to desired territories, often facing economic hardship, violence, or death in transit or in countries of origin. Their fundamental right to movement is violated by border enforcement.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Benefit from reduced competition for jobs and higher wages due to restricted labor supply from abroad. Their economic position is protected by border controls.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, domestic_workers_in_high_wage_countries, beneficiary,
    moderate, biographical, constrained, national).

% Suffer from brain drain and reduced economic opportunities in their home countries as skilled labor migrates, and from the inability to seek better opportunities abroad due to border restrictions.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers_in_low_wage_countries, payer,
    powerless, biographical, trapped, national).

% Actively enforce border controls, deploying resources and personnel to prevent unauthorized entry. They claim to protect national sovereignty and the welfare of their citizens, but under this reading, their actions are rights violations requiring extraordinary justification.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, states_enforcing_borders, agenda_setter,
    institutional, generational, constrained, national).

% Champion the universal right to freedom of movement and challenge state practices that restrict it. They provide legal and moral arguments against restrictive border regimes.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% Capital moves freely across borders, seeking the highest returns, while labor is restricted. This asymmetry benefits capital by suppressing labor costs in high-wage countries and exploiting labor surpluses in low-wage countries.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, global_capital_flows, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(border_normative_status__freedom_primary, global_capital_flows).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading denies a legitimate coordination function for borders in their current form, viewing them primarily as instruments of exclusion and rights violation. Any 'coordination' is for the benefit of those already inside, at the expense of those outside.
% TRANSFER_FUNCTION: Transfers economic opportunity, safety, and the exercise of a fundamental right from excluded migrants to citizens of wealthy states and global capital. It also transfers the burden of economic stagnation and brain drain to sending countries.
% ABSENT_VOICES: The voices of those denied entry, those trapped in precarious situations, and those whose economic prospects are curtailed by restrictive borders are systematically marginalized in national policy debates. Their claims are often dismissed as external to the 'national interest'.
% DISAPPEARANCE_RATIONALE: If borders ceased to function as instruments of exclusion overnight, there would be significant global migration flows, labor markets would rebalance, and the distribution of wealth and opportunity would fundamentally shift. The current nation-state system would be profoundly altered.
% FOUNDING_PROBLEM: The problem of managing human mobility and ensuring equitable distribution of resources and opportunities across populations, while respecting individual rights.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law, philosophical arguments for universal human rights, and the lived experiences of migrants and displaced persons corroborate the ongoing nature of this problem. The UN Declaration of Human Rights and subsequent covenants affirm freedom of movement as a right, providing external corroboration for the problem's existence and status.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).

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
 *   Extractiveness is high (0.85) because the constraint fundamentally denies a basic human right and creates significant economic and social costs for a large population. Suppression is very high (0.95) due to the coercive force of state border enforcement, including physical barriers, surveillance, and legal penalties, with very few legitimate alternatives for those seeking entry. Theater ratio is low (0.1) because the enforcement is genuinely aimed at exclusion, not merely performance; the 'coordination' claims are largely cover. Accessibility collapse is high (0.9) as legal and practical alternatives to state-controlled entry are almost non-existent. Resistance is substantial (0.7) from migrants themselves, human rights organizations, and some political movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states enforcing borders, the constraint might be framed as a necessary Rope or even a Mountain (natural right of self-determination). However, from the perspective of excluded migrants and human rights advocates, it is a clear Snare, actively extracting rights and opportunities. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded migrants and displaced domestic workers are clear targets (high d), bearing the direct costs of denied rights and economic opportunity. Domestic workers in high-wage countries and global capital flows are beneficiaries (low d), gaining from reduced labor competition and asymmetric mobility. States enforcing borders are agenda-setters, actively maintaining the constraint, and while they claim to act for their citizens, from this reading, their actions are fundamentally extractive and suppressive of human rights.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    justification_threshold_ambiguity,
    'What constitutes ''extraordinary justification'' for border exclusion under this reading, and who adjudicates it?',
    'Development of international legal precedents and a robust, independent international tribunal with enforcement powers over state border policies.',
    'If the threshold for justification is high and strictly enforced, the constraint''s effective extractiveness would decrease as fewer exclusions would be deemed legitimate. If the threshold remains vague or self-adjudicated by states, the current extractive nature persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justification_threshold_ambiguity, conceptual, 'Ambiguity in the standard for legitimate border exclusion.').

omega_variable(
    displaced_domestic_workers_victim_status,
    'Is the displacement of domestic workers in low-wage countries a direct consequence of border restrictions, or primarily due to internal economic factors?',
    'Empirical economic studies analyzing the causal link between border policies, brain drain, and domestic labor market conditions in sending countries.',
    'If a strong causal link is established, it reinforces the victim status of this group and the overall extractiveness of the constraint. If the link is weak, their victim status under this specific constraint would be re-evaluated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_domestic_workers_victim_status, empirical, 'Causal link between border restrictions and domestic labor displacement.').

omega_variable(
    border_as_rights_violation_vs_policy_choice,
    'Is the restriction of movement an inherent violation of a fundamental right, or a policy choice that can be justified by other legitimate state interests?',
    'Philosophical and legal consensus on the hierarchy of rights and state prerogatives in international law, potentially through a landmark international court ruling.',
    'If affirmed as an inherent rights violation, the constraint''s classification as a Snare is strongly reinforced. If re-framed as a justifiable policy choice, it might shift towards a Tangled Rope or even a Rope, depending on the balance of interests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(border_as_rights_violation_vs_policy_choice, conceptual, 'Fundamental nature of freedom of movement as a right versus a policy domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_normative_status__freedom_primary, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(bord_tr_t1970, border_normative_status__freedom_primary, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__freedom_primary, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(bord_tr_t2010, border_normative_status__freedom_primary, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__freedom_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_normative_status__freedom_primary, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(bord_be_t1970, border_normative_status__freedom_primary, base_extractiveness, 1970, 0.75).
narrative_ontology:measurement(bord_be_t1990, border_normative_status__freedom_primary, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(bord_be_t2010, border_normative_status__freedom_primary, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__freedom_primary, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_normative_status__freedom_primary, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(bord_su_t1970, border_normative_status__freedom_primary, suppression_requirement, 1970, 0.8).
narrative_ontology:measurement(bord_su_t1990, border_normative_status__freedom_primary, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(bord_su_t2010, border_normative_status__freedom_primary, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__freedom_primary, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, qualified_sovereignty).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, global_labor_market_segmentation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'border_normative_status' kernel. It asserts freedom of movement as primary, in contrast to readings prioritizing state sovereignty or qualified state authority. Each reading constitutes a distinct constraint with different beneficiaries, victims, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
