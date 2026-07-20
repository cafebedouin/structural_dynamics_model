% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty (Sovereignty Reading)
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty reading of the
 *   federation_membership kernel: membership is a conditional treaty among
 *   sovereign states in which national governments retain legitimate
 *   authority to control borders and to make free movement negotiable policy.
 *   Local labor markets and member state governments benefit from the ability
 *   to restrict mobility; mobile citizens and cross-border workers bear the
 *   costs of conditionality. The constraint coordinates collective federation
 *   goods while asymmetrically extracting mobility rights from individuals.
 *
 * KEY AGENTS:
 *   - member_state_executives: Agenda-setter (institutional/constrained) â negotiate and enforce conditional treaty terms while retaining border legitimacy
 *   - static_labor_pools: Primary beneficiary (organized/constrained) â protected from wage competition via restricted mobility
 *   - mobile_citizens: Primary target (moderate/constrained) â face conditional, revocable mobility rights despite federation membership
 *   - cross_border_workers: Secondary target (moderate/constrained) â bear precarity and compliance costs of contingent access
 *   - supranational_commission: Excluded (institutional/trapped) â integration mandate overridden by sovereignty reading
 *   - migration_economists: Analytical observer (analytical/analytical) â measure wage and allocation effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.72).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.68).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '7ea0d602-c3fc-4817-b5bd-75df2265c7fd').
narrative_ontology:cs_kernel_codification('7ea0d602-c3fc-4817-b5bd-75df2265c7fd', formalized).
narrative_ontology:cs_authority_grounding('7ea0d602-c3fc-4817-b5bd-75df2265c7fd', lineage).
narrative_ontology:cs_interpretation_layer_present('7ea0d602-c3fc-4817-b5bd-75df2265c7fd').
narrative_ontology:cs_reading_relation('7ea0d602-c3fc-4817-b5bd-75df2265c7fd', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('7ea0d602-c3fc-4817-b5bd-75df2265c7fd', foundational, national_border_authority_retained).
narrative_ontology:cs_axiom_status(national_border_authority_retained, holdable).
narrative_ontology:cs_axiom_grounding('7ea0d602-c3fc-4817-b5bd-75df2265c7fd', national_border_authority_retained, conventional).
narrative_ontology:cs_axiom('7ea0d602-c3fc-4817-b5bd-75df2265c7fd', foundational, free_movement_negotiable).
narrative_ontology:cs_axiom_status(free_movement_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('7ea0d602-c3fc-4817-b5bd-75df2265c7fd', free_movement_negotiable, conventional).
narrative_ontology:cs_reference_frame('7ea0d602-c3fc-4817-b5bd-75df2265c7fd', national_sovereignty_framework).
narrative_ontology:cs_drift_state('7ea0d602-c3fc-4817-b5bd-75df2265c7fd', supranational_consolidation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7ea0d602-c3fc-4817-b5bd-75df2265c7fd', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, member_state_executives).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, static_labor_pools).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, cross_border_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and enforce conditional treaty terms of federation membership, retaining authority to set national border policy and to make free movement contingent on domestic political and labor market conditions. They administer the extraction of mobility rights from individuals while coordinating collective federation goods among governments.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, member_state_executives, agenda_setter,
    institutional, generational, constrained, continental).

% Receive protection from unrestricted cross-border labor competition through conditional mobility rules; their wage and employment position is stabilized by the constraint's restriction of market access for mobile citizens.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, static_labor_pools, beneficiary,
    organized, biographical, constrained, national).

% Hold federation membership but face conditional and potentially revocable rights to reside and work across federation territory; their mobility is treated as negotiable policy rather than an automatic membership entitlement, exposing them to deportation, permit revocation, and exclusion.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, constrained, national).

% Commute or relocate for employment under temporary or contingent permits; bear compliance costs, precarity, and the risk of status loss that static workers do not face, while contributing to labor markets they cannot fully settle in.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, cross_border_workers, payer,
    moderate, biographical, constrained, regional).

% Claims institutional competence over free movement and integration arbitration, but under the sovereignty reading its authority over border legitimacy is denied; it is structurally excluded from setting the agenda on membership conditionality.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_commission, excluded,
    institutional, generational, trapped, continental).

% Analyze wage effects, labor allocation efficiency, and fiscal impacts of conditional mobility; their research informs political debate but does not determine treaty structure.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, migration_economists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a federation of sovereign states through conditional treaty obligations, allowing collective goods and market access while preserving national authority over membership terms and population movement.
% TRANSFER_FUNCTION: Transfers mobility rights and labor market access from mobile citizens to static labor pools and member state governments, making free movement contingent on negotiated conditionality rather than an automatic right of membership.
% ABSENT_VOICES: Supranational federalist advocates and mobile citizen groups without voting rights in host states are structurally underrepresented in treaty renegotiation; the supranational commission's integration mandate is backgrounded by the sovereignty reading's border legitimacy claims.
% DISAPPEARANCE_RATIONALE: If the conditional treaty structure vanished overnight, member states would lose the coordinated framework for selective market access and border control; static labor pools would face unregulated wage competition from mobile workers; mobile citizens would gain unconditional mobility rights; and the political economy of the federation would shift toward the integration reading.
% FOUNDING_PROBLEM: How to secure the economic and security benefits of cross-border federation without dissolving national sovereignty over population movement and labor market regulation.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments and nationalist parties attest the problem is live. Supranational institutions and federalist scholars attest the problem has evolved beyond the sovereignty framing. Independent comparative federalism studies from outside both camps document the persistent tension.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.72) because mobility rights are withheld from a defined population and transferred to protected labor markets and state authorities; suppression (0.68) reflects the active border enforcement and treaty machinery required to maintain conditionality. Theater ratio (0.45) captures the symbolic assertion of border sovereignty alongside the functional restriction of labor flows. Accessibility collapse (0.60) indicates that while extra-federation alternatives exist, the understood alternative to conditional membership is exit from the federation itself, which is costly. Resistance (0.55) reflects ongoing political contestation by mobile citizens, pro-migration advocates, and supranational bodies. Measurements share one time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The member state executive seat experiences this constraint as a coordination mechanism preserving democratic sovereignty and labor market stability. The mobile citizen seat experiences the same structure as the active withholding of rights that federation membership appeared to promise. The engine computes this divergence from the structural data: same constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state executives and static labor pools are beneficiaries (low d, subsidized by the constraint's protection). Mobile citizens and cross_border_workers are victims (high d, extracted via mobility restriction). The supranational commission is excluded (no directional flow). Executives sit at moderate-to-low d despite constrained exit because they collect sovereignty rents; their constrained exit reflects federation interdependence rather than target status.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by requiring both genuine coordination (the federation treaty solves collective-action problems in trade and security) and identifiable victims (mobile citizens). A pure coordination reading (rope) would fail because victims are structurally present. A pure extraction reading (snare) would miss the genuine treaty coordination among states. The Tangled Rope classification captures the hybrid: states are coordinated, individuals pay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sovereignty_vs_integration,
    'Is federation membership structurally a conditional treaty among sovereigns, or does it entail an irreversible integration dynamic that overrides national border authority?',
    'Historical institutional analysis of treaty revision patterns: if member states retain unilateral or unanimous veto over free movement scope, sovereignty reading holds; if qualified majority voting and supranational jurisprudence progressively lock in mobility rights, integration reading describes the actual structure.',
    'Resolution determines whether the constraint''s high extractiveness is inherent to the sovereignty reading or represents drift from an integration trajectory that would classify differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sovereignty_vs_integration, conceptual, 'Core kernel ambiguity between conditional treaty and irreversible integration').

omega_variable(
    labor_market_protection_vs_rent,
    'Do conditional mobility restrictions protect vulnerable local labor markets from wage depression, or do they capture economic rents for static citizens by restricting labor supply?',
    'Comparative wage and employment analysis across member states with varying mobility conditionality, controlling for sectoral composition and productivity differentials.',
    'If protective, the coordination function is genuine and the extraction is partly welfare-justified; if rent-capturing, the constraint leans toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_protection_vs_rent, empirical, 'Whether mobility restrictions serve protection or extraction').

omega_variable(
    enforcement_cost_burden,
    'Does the active enforcement of border conditionality cost more than the economic benefit it conveys to local labor markets?',
    'Full fiscal accounting of border administration, compliance costs to mobile citizens, and deadweight loss from restricted labor allocation.',
    'If enforcement costs exceed protected wages, the constraint is largely theatrical or inertial; if benefits exceed costs, the extraction is structurally productive for beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_cost_burden, empirical, 'Net balance of enforcement cost versus protected benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fede_tr_t14, federation_membership__sovereignty_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement(fede_tr_t28, federation_membership__sovereignty_reading, theater_ratio, 28, 0.3).
narrative_ontology:measurement(fede_tr_t42, federation_membership__sovereignty_reading, theater_ratio, 42, 0.36).
narrative_ontology:measurement(fede_tr_t56, federation_membership__sovereignty_reading, theater_ratio, 56, 0.41).
narrative_ontology:measurement(fede_tr_t70, federation_membership__sovereignty_reading, theater_ratio, 70, 0.45).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fede_be_t14, federation_membership__sovereignty_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement(fede_be_t28, federation_membership__sovereignty_reading, base_extractiveness, 28, 0.58).
narrative_ontology:measurement(fede_be_t42, federation_membership__sovereignty_reading, base_extractiveness, 42, 0.65).
narrative_ontology:measurement(fede_be_t56, federation_membership__sovereignty_reading, base_extractiveness, 56, 0.69).
narrative_ontology:measurement(fede_be_t70, federation_membership__sovereignty_reading, base_extractiveness, 70, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fede_su_t14, federation_membership__sovereignty_reading, suppression_requirement, 14, 0.54).
narrative_ontology:measurement(fede_su_t28, federation_membership__sovereignty_reading, suppression_requirement, 28, 0.58).
narrative_ontology:measurement(fede_su_t42, federation_membership__sovereignty_reading, suppression_requirement, 42, 0.62).
narrative_ontology:measurement(fede_su_t56, federation_membership__sovereignty_reading, suppression_requirement, 56, 0.65).
narrative_ontology:measurement(fede_su_t70, federation_membership__sovereignty_reading, suppression_requirement, 70, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% This constraint and federation_membership__integration_reading are sibling readings of the federation_membership kernel. They share a formalized treaty kernel but diverge on whether national sovereignty or supranational integration is the authoritative reading of membership conditionality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
