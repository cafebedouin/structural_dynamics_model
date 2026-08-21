% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Welfare Coordination for Free Movement
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the EU's approach to free movement as operating
 *   through the coordination of national welfare systems, rather than
 *   supranational harmonization. It focuses on the enforcement of
 *   anti-social-dumping rules to protect labor markets, while preserving
 *   member state autonomy in welfare design. This reading highlights the
 *   asymmetric extraction from posted workers and sending states, and the
 *   pressures on receiving state labor markets, despite the coordination
 *   function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.75).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Welfare Coordination for Free Movement").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, '9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e').
narrative_ontology:cs_kernel_codification('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e', formalized).
narrative_ontology:cs_authority_grounding('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e', lineage).
narrative_ontology:cs_interpretation_layer_present('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e').
narrative_ontology:cs_reading_relation('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e', foundational, national_welfare_autonomy_is_foundational).
narrative_ontology:cs_axiom_status(national_welfare_autonomy_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e', national_welfare_autonomy_is_foundational, conventional).
narrative_ontology:cs_axiom('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e', foundational, anti_social_dumping_is_necessary_for_single_market).
narrative_ontology:cs_axiom_status(anti_social_dumping_is_necessary_for_single_market, holdable).
narrative_ontology:cs_axiom_grounding('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e', anti_social_dumping_is_necessary_for_single_market, empirically_contingent).
narrative_ontology:cs_reference_frame('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e', coordinated_national_welfare_systems).
narrative_ontology:cs_drift_state('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e', contemporary_eu_migration_debates, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9bfc7ebb-a2ef-4aa0-88cd-0d4c4476157e', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, receiving_member_states_fiscal_autonomy).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_member_state_labor_markets).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_member_states_fiscal_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_citizens_at_large).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces anti-social-dumping rules, aiming to prevent unfair competition while upholding the principle of free movement. Benefits from maintaining a balance that preserves the integrity of the single market and member state autonomy.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Retains significant control over its national welfare system design, avoiding supranational harmonization. Benefits from the flexibility to adapt social policies to national contexts, even if it creates internal labor market pressures.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_member_states_fiscal_autonomy, beneficiary,
    institutional, biographical, constrained, national).

% Often face cost-competition due to social levy exemptions (up to 2 years) and cabotage wage undercutting. They are a primary target of the 'anti-social-dumping' rules, which aim to protect local labor markets but can also limit their bargaining power and social protections.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, immediate, constrained, regional).

% Experience dual pressure from posted workers undercutting wages and permanent migrants potentially displacing local workers. While anti-social-dumping rules offer some protection, the overall effect can be downward pressure on wages and working conditions for certain sectors.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_member_state_labor_markets, payer,
    organized, biographical, constrained, national).

% Lose workers to other EU states without direct fiscal compensation for their education and social investment. This can strain their welfare systems and long-term economic planning, as they bear the cost of human capital development without fully reaping the benefits.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_member_states_fiscal_capacity, payer,
    institutional, generational, constrained, national).

% Benefit from the general principle of free movement, allowing them to seek employment and residence across the EU. However, the coordination approach means that welfare benefits and social rights are not fully harmonized, leading to potential disparities.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_citizens_at_large, beneficiary,
    moderate, generational, mobile, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the operation of free movement across diverse national welfare systems, preventing a race to the bottom in social standards (social dumping) while respecting member states' autonomy in welfare design.
% TRANSFER_FUNCTION: Transfers the burden of welfare system harmonization from the supranational level to national systems, while simultaneously transferring some labor market costs (via wage undercutting and fiscal drain) from benefiting states/employers to posted workers and sending states.
% ABSENT_VOICES: Advocates for full supranational welfare harmonization and a 'European social union' are largely absent from the core decision-making, as their proposals are seen as infringing on national sovereignty. Their presence would push for more uniform social rights and benefits across the EU.
% DISAPPEARANCE_RATIONALE: If this coordination framework vanished, free movement would either collapse due to unchecked social dumping and national protectionism, or it would force a rapid, potentially destabilizing, supranational harmonization of welfare systems. The current equilibrium would be unsustainable.
% FOUNDING_PROBLEM: The original problem was how to enable free movement of persons within the EU without undermining national welfare states or creating unfair competition through 'social dumping'.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and member states consistently attest to the ongoing challenge of balancing free movement with national welfare integrity. Independent labor economists and social policy researchers also corroborate the persistence of these tensions, particularly concerning posted workers and labor market impacts.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates free movement across diverse welfare systems (beneficiaries: EU institutions, receiving member states' fiscal autonomy) but simultaneously involves significant asymmetric extraction (victims: posted workers, receiving state labor markets, sending member states' fiscal capacity). The extractiveness (0.68) is driven by the structural advantages gained by employers and receiving states through wage undercutting and fiscal imbalances. Suppression (0.75) is high due to the active enforcement of anti-social-dumping rules and the limited exit options for workers and states within the EU framework. Theater ratio (0.25) is moderate, reflecting that while anti-social-dumping rules have a genuine function, their application can also serve to legitimize existing imbalances.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU institutions and receiving member states, this is a necessary coordination mechanism to manage free movement. From the perspective of posted workers and sending member states, it can feel like a system that facilitates exploitation and fiscal drain. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions and receiving member states are beneficiaries (low directionality) as they maintain control and benefit from the coordinated system. Posted workers, receiving state labor markets, and sending member states are targets (high directionality) as they bear the costs of wage undercutting, labor market pressure, and fiscal drain, respectively. EU citizens at large are beneficiaries of free movement, but also indirectly bear some costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to balance free movement with welfare state integrity is still live, preventing a clear mandatrophy resolution. However, the increasing extractiveness and suppression over time suggest that the 'coordination' function is increasingly serving to manage the negative externalities of the 'extraction' rather than genuinely solving the underlying coordination problem. The classification as Tangled Rope captures this hybrid nature, preventing it from being mislabeled as a pure Rope (ignoring extraction) or a pure Snare (ignoring coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_coordination_vs_harmonization,
    'Is the current ''coordination'' approach genuinely preventing social dumping and ensuring fair competition, or is it a less effective substitute for full supranational welfare harmonization?',
    'Comparative analysis of labor market outcomes and social protection levels in the EU versus fully harmonized federal systems (e.g., US states, Canadian provinces).',
    'If coordination is found to be significantly less effective, it would strengthen arguments for supranational harmonization, potentially reclassifying the constraint towards a Snare (if coordination is merely cover) or a Scaffold (if it''s a temporary, insufficient measure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_coordination_vs_harmonization, empirical, 'Assesses the effectiveness of welfare coordination versus harmonization.').

omega_variable(
    fiscal_compensation_for_sending_states,
    'Should sending member states receive fiscal compensation for the loss of human capital due to free movement, and would such compensation alter the constraint''s extractive nature?',
    'Economic modeling of inter-state fiscal transfers and their impact on welfare state sustainability in sending countries, combined with political feasibility analysis.',
    'If fiscal compensation were implemented, it would reduce the extractiveness from sending member states, potentially shifting the overall constraint towards a more balanced Tangled Rope or even a Rope, by addressing a key asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_compensation_for_sending_states, preference, 'Examines the role of fiscal compensation in mitigating extraction from sending states.').

omega_variable(
    reading_divergence_on_free_movement_scope,
    'This ''welfare_coordination_reading'' emphasizes anti-social-dumping rules and national autonomy. How does this diverge from the ''integration_reading'' (expansive rights) and ''member_sovereignty_reading'' (national protection)?',
    'Analysis of ECJ jurisprudence, national legislative acts, and EU policy documents to map the practical implications of each reading on labor mobility, social rights, and national welfare capacity.',
    'The ''integration_reading'' would likely classify the constraint as a Snare (if national limits are seen as illegitimate extraction) or a Tangled Rope (if some coordination is acknowledged but extraction is primary). The ''member_sovereignty_reading'' might classify it as a Rope (if national protection is seen as primary coordination) or even a Mountain (if national autonomy is framed as an irreducible limit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_divergence_on_free_movement_scope, conceptual, 'Documents the structural differences between the ''welfare_coordination_reading'' and its sibling readings of the federation membership kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(fede_tr_t1998, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(fede_tr_t2003, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(fede_tr_t2013, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2013, 0.24).
narrative_ontology:measurement(fede_tr_t2018, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(fede_tr_t2023, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2023, 0.25).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(fede_be_t1998, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1998, 0.6).
narrative_ontology:measurement(fede_be_t2003, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2003, 0.63).
narrative_ontology:measurement(fede_be_t2008, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(fede_be_t2013, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2013, 0.67).
narrative_ontology:measurement(fede_be_t2018, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2018, 0.69).
narrative_ontology:measurement(fede_be_t2023, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2023, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(fede_su_t1998, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1998, 0.65).
narrative_ontology:measurement(fede_su_t2003, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2003, 0.7).
narrative_ontology:measurement(fede_su_t2008, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2008, 0.72).
narrative_ontology:measurement(fede_su_t2013, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2013, 0.74).
narrative_ontology:measurement(fede_su_t2018, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2018, 0.76).
narrative_ontology:measurement(fede_su_t2023, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
