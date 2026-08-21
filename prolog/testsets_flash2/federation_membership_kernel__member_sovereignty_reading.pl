% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member State Sovereignty over Free Movement and Welfare
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'member sovereignty' reading of free
 *   movement rights within a federal system (e.g., the EU). It asserts that
 *   national welfare state capacity and labor market protection are
 *   legitimate bounds on free movement, allowing member states to exclude
 *   economically inactive migrants. This reading is often championed by
 *   national governments concerned about fiscal burdens and social cohesion.
 *   The constraint is classified as a Tangled Rope because it attempts to
 *   coordinate national sovereignty with free movement but does so with
 *   significant asymmetric extraction from migrants and sending states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.65).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.7).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member State Sovereignty over Free Movement and Welfare").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, '7b5c164f-11f0-446b-89f1-868a4c1fa86f').
narrative_ontology:cs_kernel_codification('7b5c164f-11f0-446b-89f1-868a4c1fa86f', formalized).
narrative_ontology:cs_authority_grounding('7b5c164f-11f0-446b-89f1-868a4c1fa86f', lineage).
narrative_ontology:cs_interpretation_layer_present('7b5c164f-11f0-446b-89f1-868a4c1fa86f').
narrative_ontology:cs_reading_relation('7b5c164f-11f0-446b-89f1-868a4c1fa86f', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b5c164f-11f0-446b-89f1-868a4c1fa86f', federation_membership_kernel__welfare_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('7b5c164f-11f0-446b-89f1-868a4c1fa86f', foundational, national_welfare_sovereignty).
narrative_ontology:cs_axiom_status(national_welfare_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('7b5c164f-11f0-446b-89f1-868a4c1fa86f', national_welfare_sovereignty, conventional).
narrative_ontology:cs_axiom('7b5c164f-11f0-446b-89f1-868a4c1fa86f', foundational, economic_contribution_as_membership_condition).
narrative_ontology:cs_axiom_status(economic_contribution_as_membership_condition, holdable).
narrative_ontology:cs_axiom_grounding('7b5c164f-11f0-446b-89f1-868a4c1fa86f', economic_contribution_as_membership_condition, instrumental).
narrative_ontology:cs_reference_frame('7b5c164f-11f0-446b-89f1-868a4c1fa86f', post_maastricht_national_control).
narrative_ontology:cs_drift_state('7b5c164f-11f0-446b-89f1-868a4c1fa86f', contemporary_ecj_rulings_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7b5c164f-11f0-446b-89f1-868a4c1fa86f', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, national_welfare_recipients).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_member_states).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, mobile_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert their right to control access to their welfare systems and labor markets, viewing unrestricted free movement as a threat to national solidarity and fiscal stability. They actively enforce policies to exclude economically inactive migrants.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_member_states, agenda_setter,
    institutional, generational, constrained, national).

% Individuals who, under this reading, are deemed not to contribute sufficiently to the host economy and are therefore subject to exclusion or restricted access to social benefits. Their mobility is severely curtailed, and their welfare claims are denied.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, regional).

% These states experience 'brain drain' as their skilled workers leave, while their less economically active citizens face barriers to movement and settlement in other member states. They bear the social and economic costs of restricted mobility for their citizens.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_member_states, payer,
    institutional, biographical, constrained, national).

% Citizens of receiving states who benefit from the perceived protection of their national welfare systems from 'welfare tourism' or increased demand from non-contributing migrants. They see their social solidarity institutions as preserved.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, national_welfare_recipients, beneficiary,
    organized, biographical, constrained, national).

% While economically active, these workers face increased scrutiny and potential restrictions if their employment status changes or if they are perceived as a burden on the welfare state. Their free movement is implicitly conditional on continuous economic contribution.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, mobile_workers, payer,
    moderate, biographical, constrained, continental).

% The Commission, typically advocating for expansive free movement, finds its authority challenged by this reading. Its interpretations are often sidelined or actively resisted by member states asserting national sovereignty in this domain.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, european_commission, excluded,
    institutional, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows member states to coordinate their national welfare and labor market policies with the principle of free movement, ensuring that national social solidarity is maintained while still permitting some degree of cross-border mobility for economically active individuals.
% TRANSFER_FUNCTION: Transfers the burden of welfare provision and labor market protection from national systems to individual migrants (who must prove economic activity) and to sending states (who retain responsibility for their inactive citizens).
% ABSENT_VOICES: Supranational bodies like the European Commission and the European Court of Justice, which typically advocate for a more expansive interpretation of free movement, are sidelined. Migrant advocacy groups and human rights organizations would also object, arguing for universal rights and non-discrimination.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, member states would lose a key justification for restricting migrant access to welfare and labor markets. This would likely lead to a rapid increase in economically inactive migrants seeking residence and benefits, forcing a fundamental renegotiation of welfare state funding and free movement principles across the federation.
% FOUNDING_PROBLEM: The tension between national welfare state sovereignty and the principle of free movement within a federal or quasi-federal system, particularly concerning the fiscal and social impact of economically inactive migrants.
% FOUNDING_PROBLEM_CORROBORATION: National governments and public opinion in receiving states consistently attest to the live nature of this problem, citing ongoing debates about welfare tourism and social integration. Academic literature on welfare state sustainability and migration also corroborates the persistent tension.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because it imposes significant costs on economically inactive migrants and sending states, limiting their access to benefits and opportunities. Suppression (0.70) is also high, as it requires active enforcement by member states to identify and exclude migrants deemed 'inactive.' The theater ratio (0.20) is relatively low, as the enforcement actions are generally genuine attempts to protect national welfare systems, rather than purely performative. Accessibility collapse (0.45) is moderate, as alternatives (e.g., seeking employment) exist but are often difficult to achieve, and resistance (0.55) is present from migrant groups and some supranational bodies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of receiving member states, this constraint is a necessary coordination mechanism to preserve national welfare and social solidarity. From the perspective of migrants and sending states, it is an extractive mechanism that undermines the principle of free movement and creates significant social and economic burdens. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving member states and their national welfare recipients are beneficiaries, as they perceive their social systems as protected (low directionality). Economically inactive migrants, mobile workers facing conditional access, and sending member states bear the costs of restricted mobility and welfare access (high directionality). Supranational bodies like the European Commission are excluded, as their expansive interpretation of free movement is actively resisted.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_inactivity_definition,
    'How is ''economically inactive'' precisely defined and applied across different member states, and does this definition disproportionately affect certain migrant groups?',
    'Comparative legal analysis of national implementation laws and empirical studies on the demographic impact of these definitions.',
    'If definitions are arbitrary or discriminatory, the constraint''s suppression and extractiveness are higher than measured, as it targets specific groups under a neutral guise. If definitions are consistent and objectively applied, the measured values are more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_inactivity_definition, empirical, 'Ambiguity in defining ''economically inactive'' and its impact on migrant groups.').

omega_variable(
    fiscal_burden_vs_economic_contribution,
    'What is the actual fiscal burden of economically inactive migrants on national welfare states, compared to their indirect economic contributions (e.g., consumption, remittances)?',
    'Comprehensive economic modeling and longitudinal studies tracking migrant fiscal impacts, including both direct costs and indirect benefits.',
    'If the net fiscal burden is low or negative, the justification for exclusion weakens, reclassifying the constraint closer to a Snare. If the burden is substantial, it reinforces the coordination aspect of protecting national welfare systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_burden_vs_economic_contribution, empirical, 'Uncertainty regarding the true fiscal impact of economically inactive migrants.').

omega_variable(
    social_solidarity_definition,
    'Is ''social solidarity'' a genuine coordination problem requiring migrant exclusion, or is it a rhetorical cover for xenophobia or protectionism?',
    'Sociological studies on national identity and public attitudes towards migration, combined with policy analysis of alternative integration strategies.',
    'If ''social solidarity'' is primarily a rhetorical device, the constraint''s coordination function is weaker, and its extractiveness is higher. If it reflects a genuine, widely held concern, the coordination function is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_solidarity_definition, conceptual, 'Ambiguity in the concept of ''social solidarity'' and its role in justifying migrant exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_kernel,' focusing on member state sovereignty. It directly influences and is influenced by the 'integration_reading' and 'welfare_coordination_reading' as part of an ongoing policy and legal contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
