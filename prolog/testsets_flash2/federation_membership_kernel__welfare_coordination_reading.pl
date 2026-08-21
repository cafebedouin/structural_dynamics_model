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
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.65).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.7).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Welfare Coordination for Free Movement").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, 'f0b24970-0627-49c1-a6e8-f25e1206131b').
narrative_ontology:cs_kernel_codification('f0b24970-0627-49c1-a6e8-f25e1206131b', formalized).
narrative_ontology:cs_authority_grounding('f0b24970-0627-49c1-a6e8-f25e1206131b', lineage).
narrative_ontology:cs_interpretation_layer_present('f0b24970-0627-49c1-a6e8-f25e1206131b').
narrative_ontology:cs_reading_relation('f0b24970-0627-49c1-a6e8-f25e1206131b', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0b24970-0627-49c1-a6e8-f25e1206131b', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('f0b24970-0627-49c1-a6e8-f25e1206131b', foundational, national_welfare_autonomy_is_foundational).
narrative_ontology:cs_axiom_status(national_welfare_autonomy_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('f0b24970-0627-49c1-a6e8-f25e1206131b', national_welfare_autonomy_is_foundational, conventional).
narrative_ontology:cs_axiom('f0b24970-0627-49c1-a6e8-f25e1206131b', foundational, anti_social_dumping_rules_are_necessary_coordination).
narrative_ontology:cs_axiom_status(anti_social_dumping_rules_are_necessary_coordination, holdable).
narrative_ontology:cs_axiom_grounding('f0b24970-0627-49c1-a6e8-f25e1206131b', anti_social_dumping_rules_are_necessary_coordination, instrumental).
narrative_ontology:cs_reference_frame('f0b24970-0627-49c1-a6e8-f25e1206131b', coordinated_national_welfare_systems).
narrative_ontology:cs_drift_state('f0b24970-0627-49c1-a6e8-f25e1206131b', contemporary_eu_enlargement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f0b24970-0627-49c1-a6e8-f25e1206131b', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, sending_member_states_via_remittances).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_member_state_labor_markets).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_member_states_via_brain_drain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces anti-social-dumping rules (e.g., Posted Workers Directive) to prevent extreme wage undercutting, while respecting national welfare system autonomy. Benefits from perceived stability and legitimacy of the single market.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Are temporarily sent to another member state, often under conditions that allow for lower social security contributions or wages than local workers, leading to wage undercutting and precarious employment. Their mobility is constrained by employment contracts and the temporary nature of their posting.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, immediate, constrained, regional).

% Experience dual pressure: wage undercutting from posted workers (due to social levy exemptions and cabotage rules) and potential displacement from permanent migrants. This creates downward pressure on wages and strains social services, leading to political resistance.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_member_state_labor_markets, payer,
    organized, biographical, constrained, national).

% Benefit from remittances sent by their citizens working abroad, which can be a significant boost to their national economies. However, they also face brain drain and loss of skilled labor.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_member_states_via_remittances, beneficiary,
    institutional, generational, constrained, national).

% Lose skilled workers and human capital to wealthier member states, impacting their long-term economic development and tax base. This cost is often offset by remittances but represents a structural disadvantage.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_member_states_via_brain_drain, payer,
    institutional, generational, constrained, national).

% Manage the national welfare systems, attempting to balance fiscal sustainability with social solidarity amidst free movement. They observe the impacts of migration on their systems and advocate for policy adjustments within the EU framework.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, national_welfare_administrators, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the free movement of persons across national borders by allowing individuals to access labor markets and social benefits in other member states, while attempting to prevent 'social dumping' and preserve national welfare system integrity.
% TRANSFER_FUNCTION: Transfers labor and human capital from sending to receiving states, and potentially transfers social costs (e.g., unemployment benefits, social services) to receiving states, while remittances flow back to sending states. It also transfers regulatory burden to EU institutions for anti-social-dumping enforcement.
% ABSENT_VOICES: Unemployed or underemployed local workers in receiving states, who experience direct competition and wage depression, often lack organized representation at the EU level. Their concerns are filtered through national political systems, which may not fully capture the transnational nature of the problem.
% DISAPPEARANCE_RATIONALE: If this coordination framework vanished, free movement would likely collapse or become highly fragmented, leading to severe economic disruption, border controls, and a breakdown of the single market. National welfare systems would face immediate pressure to re-establish full sovereignty over migration and social benefits, leading to a complex and costly reorganization.
% FOUNDING_PROBLEM: The problem of enabling economic integration and labor mobility across diverse national welfare systems without either forcing full harmonization or allowing destructive 'race to the bottom' social dumping.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and many member states attest that the problem remains live, citing ongoing challenges with posted workers and social security coordination. However, critics (including some labor unions and national politicians) argue that the current framework is insufficient and that the 'coordination' often masks continued social dumping, making the status contested in practice.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_dumping_severity,
    'To what extent do anti-social-dumping rules genuinely mitigate wage undercutting and social security arbitrage, versus merely formalizing a lower standard for posted workers?',
    'Empirical studies comparing wage and social security outcomes for posted workers versus local workers in various sectors and member states, before and after directive implementations.',
    'If mitigation is minimal, the extractiveness of the constraint is higher than currently estimated, pushing it closer to a ''snare'' for posted workers. If mitigation is substantial, the coordination function is stronger, supporting the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_dumping_severity, empirical, 'Empirical effectiveness of anti-social-dumping rules.').

omega_variable(
    welfare_state_autonomy_vs_harmonization,
    'Is the preservation of member state welfare design autonomy a genuine coordination goal, or a political compromise that enables continued social dumping by preventing full harmonization?',
    'Conceptual analysis of the legal and political history of EU social policy, combined with comparative analysis of federal systems that have achieved greater social policy harmonization.',
    'If autonomy is primarily a cover for social dumping, the constraint''s coordination function is weaker, and its extractive nature is more pronounced. If it''s a genuinely valued principle, the ''tangled_rope'' classification is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_state_autonomy_vs_harmonization, conceptual, 'The true nature of ''welfare design autonomy'' within the EU context.').

omega_variable(
    framing_of_migration_costs,
    'Are the costs borne by receiving state labor markets (wage undercutting, social strain) an unavoidable consequence of free movement, or a policy choice enabled by the current coordination framework?',
    'Policy analysis comparing different models of labor mobility and welfare integration (e.g., Nordic model, full federal welfare union) and their respective outcomes for labor markets and social cohesion.',
    'If costs are unavoidable, the constraint is a more robust ''tangled_rope'' with inherent trade-offs. If they are policy-enabled, the ''extraction'' component is higher, and the ''coordination'' narrative is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_migration_costs, preference, 'Whether migration costs are inherent or policy-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
