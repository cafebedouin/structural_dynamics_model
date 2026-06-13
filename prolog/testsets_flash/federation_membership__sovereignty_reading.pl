% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership (Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty reading' of federation
 *   membership, where national authority over borders and migration policy is
 *   retained, and free movement is a negotiable policy rather than an
 *   inherent right. It views federation membership as a conditional treaty,
 *   allowing national governments to impose restrictions on movement and
 *   labor market access for citizens of other member states. This leads to
 *   higher extractiveness from mobile citizens and migrant workers, and
 *   active enforcement of border legitimacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.65).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.7).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '3bf1a749-aef3-47a1-8332-845487928731').
narrative_ontology:cs_kernel_codification('3bf1a749-aef3-47a1-8332-845487928731', formalized).
narrative_ontology:cs_authority_grounding('3bf1a749-aef3-47a1-8332-845487928731', lineage).
narrative_ontology:cs_interpretation_layer_present('3bf1a749-aef3-47a1-8332-845487928731').
narrative_ontology:cs_reading_relation('3bf1a749-aef3-47a1-8332-845487928731', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('3bf1a749-aef3-47a1-8332-845487928731', foundational, national_sovereignty_is_primary).
narrative_ontology:cs_axiom_status(national_sovereignty_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('3bf1a749-aef3-47a1-8332-845487928731', national_sovereignty_is_primary, conventional).
narrative_ontology:cs_axiom('3bf1a749-aef3-47a1-8332-845487928731', foundational, free_movement_is_negotiable_policy).
narrative_ontology:cs_axiom_status(free_movement_is_negotiable_policy, holdable).
narrative_ontology:cs_axiom_grounding('3bf1a749-aef3-47a1-8332-845487928731', free_movement_is_negotiable_policy, conventional).
narrative_ontology:cs_reference_frame('3bf1a749-aef3-47a1-8332-845487928731', westphalian_state_system).
narrative_ontology:cs_drift_state('3bf1a749-aef3-47a1-8332-845487928731', contemporary_globalization_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3bf1a749-aef3-47a1-8332-845487928731', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, migrant_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary authority over national borders and migration policy, viewing federation membership as a conditional treaty. They negotiate terms of free movement and can impose restrictions to protect national interests or labor markets. They benefit from this retained sovereignty.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% Citizens of member states who wish to live or work in another member state. They face conditional access to labor markets, social benefits, and residency, incurring costs and restrictions due to national border legitimacy and negotiable free movement policies.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, constrained, regional).

% Workers from other member states who are subject to specific national labor market regulations and often face precarious employment conditions. Their ability to move and work is highly conditional, making them particularly vulnerable to extraction.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, migrant_workers, payer,
    powerless, immediate, trapped, local).

% Benefit from the ability of national governments to regulate the influx of labor, protecting domestic wages and employment. They can lobby national governments for policies that restrict free movement to their advantage.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, local_labor_markets, beneficiary,
    organized, biographical, mobile, local).

% Administer the federation's treaties and policies, but under the sovereignty reading, their authority is limited by the retained powers of national governments. They facilitate coordination but have less power to enforce free movement as an absolute right.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Advocate for deeper integration and stronger supranational authority, including unconditional free movement as a fundamental right. Their arguments are often sidelined or outvoted by national interests under the sovereignty reading.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, pro_integration_advocates, excluded,
    organized, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates inter-state relations within a federation, allowing for cooperation on shared challenges (e.g., trade, security) while preserving national sovereignty and the ability to manage domestic affairs, including migration.
% TRANSFER_FUNCTION: Transfers the right to control borders and migration policy from a potential supranational authority back to national governments. It transfers costs (e.g., restricted mobility, conditional access to benefits) to mobile citizens and migrant workers.
% ABSENT_VOICES: Advocates for full integration and unconditional free movement are often marginalized in policy debates dominated by national sovereignty concerns. Mobile citizens and migrant workers, despite being directly affected, often lack collective political power to effectively voice their objections.
% DISAPPEARANCE_RATIONALE: If this reading of federation membership vanished, national governments would lose their perceived legitimate right to control borders and migration policy within the federation. This would lead to a rapid shift towards more open borders, significant changes in labor markets, and a re-evaluation of supranational authority, fundamentally altering the political and economic landscape.
% FOUNDING_PROBLEM: The founding problem was how to achieve inter-state cooperation and stability while preserving the sovereignty and distinct national interests of member states, particularly regarding control over borders and domestic policy.
% FOUNDING_PROBLEM_CORROBORATION: National governments and conservative political parties consistently attest that the problem of balancing national sovereignty with federation membership is live and ongoing, citing concerns over national identity, security, and economic stability. This is corroborated by ongoing political debates and policy adjustments within the federation, even if other parties contest the specific solutions.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high due to the ability of national governments to impose conditions and restrictions on free movement, leading to costs for mobile citizens and migrant workers. Suppression (0.7) is also high, reflecting the active enforcement of national border controls and migration policies. The theater ratio (0.2) is relatively low, as the stated function of national border control is largely aligned with its actual operation under this reading. Accessibility collapse (0.4) is moderate, as alternatives for mobile citizens are constrained but not entirely eliminated, and resistance (0.55) is present from those advocating for greater free movement.
 *
 * PERSPECTIVAL GAP:
 *   National governments and local labor markets experience this as a legitimate exercise of sovereignty and a tool for managing domestic economic conditions. Mobile citizens and migrant workers, however, experience it as a significant barrier to opportunity and a source of extraction, as their mobility is restricted and their labor market access is conditional.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments are primary beneficiaries (d=0.0-0.2) as they retain control over borders and migration policy, allowing them to manage national interests. Local labor markets also benefit (d=0.1-0.3) from the ability to regulate labor supply and demand. Mobile citizens and migrant workers are victims (d=0.7-1.0) as they face restrictions on movement, employment, and social benefits, leading to higher costs and reduced opportunities. Supranational institutions are agenda-setters (d=0.4-0.6) in the sense that they administer the treaty, but their authority is limited by national sovereignty under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Snare by acknowledging the coordination function of federation membership (e.g., shared security, economic cooperation). However, it highlights the asymmetric extraction arising from the national retention of border legitimacy and the conditional nature of free movement, which benefits national governments at the expense of mobile citizens. The 'sovereignty reading' itself is a mechanism for maintaining this asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_integration_ambiguity,
    'Is federation membership primarily a conditional treaty between sovereign states, or an irreversible step towards supranational integration?',
    'Judicial rulings on the supremacy of national vs. supranational law, or a formal treaty amendment clarifying the nature of membership and exit rights.',
    'If resolved towards integration, the constraint''s extractiveness from mobile citizens would be reclassified as illegitimate, and border controls would be seen as a violation of rights. If resolved towards sovereignty, current extractiveness would be seen as a legitimate exercise of national power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_ambiguity, conceptual, 'Ambiguity over the fundamental nature of federation membership.').

omega_variable(
    border_legitimacy_scope,
    'To what extent does national authority over borders legitimately extend to citizens of other member states within the federation?',
    'A clear legal framework defining the scope of national border control within the federation, or a political consensus on the limits of free movement policy.',
    'If national authority is deemed to legitimately restrict free movement, the suppression metric would be seen as a necessary component of policy. If not, it would be reclassified as excessive coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(border_legitimacy_scope, preference, 'Scope of national border legitimacy within a federation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1990, federation_membership__sovereignty_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(fede_tr_t2000, federation_membership__sovereignty_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(fede_tr_t2010, federation_membership__sovereignty_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(fede_tr_t2024, federation_membership__sovereignty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t1990, federation_membership__sovereignty_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(fede_be_t2000, federation_membership__sovereignty_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(fede_be_t2010, federation_membership__sovereignty_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(fede_be_t2024, federation_membership__sovereignty_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1990, federation_membership__sovereignty_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(fede_su_t2000, federation_membership__sovereignty_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(fede_su_t2010, federation_membership__sovereignty_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(fede_su_t2024, federation_membership__sovereignty_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'federation_membership' kernel. Its sibling, 'federation_membership__integration_reading', presents an alternative interpretation where free movement is a constitutional right and supranational authority is legitimate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
