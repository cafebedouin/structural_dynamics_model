% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Member State Welfare Sovereignty in Federations
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint, 'Member State Welfare Sovereignty in Federations,' is
 *   one reading of the broader 'federation_membership_obligations' kernel. It
 *   asserts that national welfare states within a federation (like the EU)
 *   retain primary authority to control access to their welfare systems,
 *   making free movement conditional on protecting national labor markets and
 *   ensuring welfare system sustainability. This reading prioritizes national
 *   sovereignty over federal integration principles, leading to the exclusion
 *   of mobile workers from full welfare benefits and the protection of
 *   domestic labor forces.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.65).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.75).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Member State Welfare Sovereignty in Federations").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, '6cdddebb-10e6-428a-a2fd-d32048c45cd7').
narrative_ontology:cs_kernel_codification('6cdddebb-10e6-428a-a2fd-d32048c45cd7', formalized).
narrative_ontology:cs_authority_grounding('6cdddebb-10e6-428a-a2fd-d32048c45cd7', lineage).
narrative_ontology:cs_interpretation_layer_present('6cdddebb-10e6-428a-a2fd-d32048c45cd7').
narrative_ontology:cs_reading_relation('6cdddebb-10e6-428a-a2fd-d32048c45cd7', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('6cdddebb-10e6-428a-a2fd-d32048c45cd7', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('6cdddebb-10e6-428a-a2fd-d32048c45cd7', foundational, national_welfare_sovereignty_paramount).
narrative_ontology:cs_axiom_status(national_welfare_sovereignty_paramount, holdable).
narrative_ontology:cs_axiom_grounding('6cdddebb-10e6-428a-a2fd-d32048c45cd7', national_welfare_sovereignty_paramount, conventional).
narrative_ontology:cs_axiom('6cdddebb-10e6-428a-a2fd-d32048c45cd7', foundational, free_movement_conditional_on_sustainability).
narrative_ontology:cs_axiom_status(free_movement_conditional_on_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('6cdddebb-10e6-428a-a2fd-d32048c45cd7', free_movement_conditional_on_sustainability, empirically_contingent).
narrative_ontology:cs_reference_frame('6cdddebb-10e6-428a-a2fd-d32048c45cd7', westphalian_welfare_state_model).
narrative_ontology:cs_drift_state('6cdddebb-10e6-428a-a2fd-d32048c45cd7', contemporary_federal_integration_pressure, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6cdddebb-10e6-428a-a2fd-d32048c45cd7', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, national_welfare_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, domestic_labor_forces).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_workers_from_other_member_states).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, federation_level_institutions_advocating_integration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert their right to control access to national welfare systems, prioritizing the sustainability of their social contracts and the protection of their domestic labor markets. They actively enforce policies that limit welfare access for mobile workers from other member states.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_welfare_states, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from policies that restrict competition from mobile workers and ensure the perceived sustainability of national welfare systems, which they fund through taxes. They exert political pressure on national governments to maintain these protections.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, domestic_labor_forces, beneficiary,
    organized, biographical, mobile, national).

% Experience limited access to welfare benefits in receiving member states, despite contributing taxes. They face barriers to full integration and often bear the costs of maintaining national welfare systems without receiving commensurate benefits, leading to precarity.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_workers_from_other_member_states, payer,
    powerless, immediate, constrained, regional).

% Their mandate for deeper integration and free movement is undermined by national welfare state closure. They expend political capital and legal resources to challenge national restrictions, often facing resistance and limited enforcement power.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, federation_level_institutions_advocating_integration, payer,
    institutional, generational, constrained, continental).

% Retain ultimate veto authority over welfare access policies, reflecting the democratic accountability to their national electorates. They are the primary site for enacting and defending the closure authority of the national welfare state.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, national_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the fiscal sustainability of national welfare systems and protects domestic labor markets within a larger federal or confederal structure, by allowing member states to set conditions on welfare access for non-nationals.
% TRANSFER_FUNCTION: Transfers the burden of welfare system sustainability and labor market protection from national taxpayers and domestic workers to mobile workers from other member states, who contribute but receive limited benefits.
% ABSENT_VOICES: Advocates for universal social rights and full mobility within the federation, who would argue for a more integrated, rights-based approach to welfare access, are often marginalized in national policy debates dominated by sovereignty concerns.
% DISAPPEARANCE_RATIONALE: If national welfare states lost their closure authority overnight, there would be immediate and significant fiscal pressure on welfare systems, rapid shifts in labor markets, and a fundamental redefinition of federal citizenship and solidarity, leading to a major reorganization of political and economic structures.
% FOUNDING_PROBLEM: The tension between national sovereignty over social policy and the principle of free movement within a federal or confederal system, particularly concerning the fiscal burden and social cohesion of welfare states.
% FOUNDING_PROBLEM_CORROBORATION: Academic literature on federalism and welfare state theory, as well as ongoing political debates and legal challenges within federations (e.g., the EU), consistently corroborate that this tension remains a live and central problem, attested by independent scholars and supranational legal bodies.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (welfare state sustainability, labor market protection) but achieves this through asymmetric extraction from mobile workers. Extractiveness (0.65) is substantial due to the fiscal burden placed on mobile workers without full reciprocal benefits. Suppression (0.75) is high, as national legal and administrative mechanisms actively enforce these exclusions. Theater ratio (0.20) is low, indicating that the stated justifications (sustainability, protection) are largely genuine concerns, though they also serve to legitimize extraction.
 *
 * PERSPECTIVAL GAP:
 *   National welfare states and domestic labor forces experience this as a necessary coordination mechanism, ensuring stability and fairness. Mobile workers and federation-level institutions, however, experience it as an extractive barrier that undermines the principle of free movement and creates social inequality. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   National welfare states and domestic labor forces are beneficiaries (d near 0.0) as they gain protection and sustainability. Mobile workers and federation-level institutions advocating integration are targets (d near 1.0) as they bear the costs of exclusion or the frustration of their integration mandate. National legislatures, as agenda-setters, are also beneficiaries of this reading's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Snare by acknowledging the genuine coordination problem of welfare state sustainability within a federal context. However, it also prevents mislabeling it as a pure Rope by highlighting the asymmetric extraction and active enforcement required to maintain the national closure authority, especially against the backdrop of federal integration principles. The 'live' status of the founding problem, coupled with rising extractiveness, suggests a dynamic where the coordination function is increasingly intertwined with rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_vs_federal_legitimacy,
    'Is the primary locus of democratic legitimacy for welfare policy at the national or federal level?',
    'Referenda on federal welfare integration, or shifts in public opinion and political mandates towards either national or federal control.',
    'If legitimacy shifts decisively to the federal level, this reading''s closure authority would be undermined, potentially leading to reclassification towards a Rope or Scaffold for integration. If it remains national, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_vs_federal_legitimacy, preference, 'Ambiguity in the locus of legitimate authority for welfare policy.').

omega_variable(
    welfare_sustainability_empirical_basis,
    'To what extent do mobile workers genuinely threaten the fiscal sustainability of national welfare states, versus being a convenient political scapegoat?',
    'Independent, long-term empirical studies on the net fiscal contribution of mobile workers to national welfare systems, disaggregated by income and benefit usage.',
    'If studies show a net positive or neutral contribution, the ''sustainability'' justification for extraction would be weakened, pushing the constraint closer to a Snare. If a significant net negative is confirmed, the coordination function is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_sustainability_empirical_basis, empirical, 'Empirical basis for welfare state sustainability claims regarding mobile workers.').

omega_variable(
    kernel_reading_member_sovereignty_primary,
    'This constraint is the ''member_sovereignty_primary'' reading of the ''federation_membership_obligations'' kernel. What would change if the ''integration_primary'' or ''selective_solidarity'' reading were adopted?',
    'Analysis of legal and policy changes if a different reading gained dominance within the federation''s institutions.',
    'The ''integration_primary'' reading would shift the constraint towards a Rope, emphasizing mobility rights and reducing national closure. The ''selective_solidarity'' reading would introduce tiered access based on contribution, potentially creating new forms of extraction or coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_member_sovereignty_primary, conceptual, 'Impact of alternative readings of federation membership obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1990, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(fede_tr_t2010, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t1990, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(fede_be_t2000, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(fede_be_t2010, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(fede_be_t2024, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1990, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(fede_su_t2000, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(fede_su_t2010, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(fede_su_t2024, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
