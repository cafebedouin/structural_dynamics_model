% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Federation Membership Treaty: Sovereignty-Primary Reading
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty_primary reading of the
 *   federation_membership_treaty kernel. Under this reading, free movement of
 *   workers within the federation is not an unconditional market freedom but
 *   a privilege contingent on member state consent. Member states retain the
 *   authority to invoke safeguard clauses, impose transitional restrictions,
 *   and exclude non-nationals from welfare entitlements in order to protect
 *   domestic labor markets and social solidarity systems. The constraint
 *   coordinates the federation by preserving member state willingness to
 *   remain in the compact, but it asymmetrically extracts from
 *   intra-federation migrants by making their mobility rights structurally
 *   subordinate to national regulatory autonomy.
 *
 * KEY AGENTS:
 *   - member_state_executives: Primary agenda-setter (institutional/arbitrage) â sets the conditions of movement, invokes treaty reservations, enforces welfare exclusions.
 *   - domestic_workers: Primary beneficiary (organized/constrained) â shielded from cross-border labor competition through national protective measures.
 *   - intra_federation_migrants: Primary target (moderate/constrained) â bear restricted access, labor-market tests, and conditional welfare status.
 *   - federal_judiciary: Analytical observer (institutional/analytical) â adjudicates mobility disputes but defers to state sovereignty claims in this reading.
 *   - cross_border_employers: Excluded voice (organized/constrained) â would expand hiring if movement were unconditional, but are kept out of treaty bargaining.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.62).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.55).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Federation Membership Treaty: Sovereignty-Primary Reading").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, 'f4e8ae20-c469-45b0-899e-a7ce70db2f6c').
narrative_ontology:cs_kernel_codification('f4e8ae20-c469-45b0-899e-a7ce70db2f6c', formalized).
narrative_ontology:cs_authority_grounding('f4e8ae20-c469-45b0-899e-a7ce70db2f6c', lineage).
narrative_ontology:cs_interpretation_layer_present('f4e8ae20-c469-45b0-899e-a7ce70db2f6c').
narrative_ontology:cs_reading_relation('f4e8ae20-c469-45b0-899e-a7ce70db2f6c', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('f4e8ae20-c469-45b0-899e-a7ce70db2f6c', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('f4e8ae20-c469-45b0-899e-a7ce70db2f6c', foundational, state_consent_prerequisite_for_mobility).
narrative_ontology:cs_axiom_status(state_consent_prerequisite_for_mobility, holdable).
narrative_ontology:cs_axiom_grounding('f4e8ae20-c469-45b0-899e-a7ce70db2f6c', state_consent_prerequisite_for_mobility, conventional).
narrative_ontology:cs_axiom('f4e8ae20-c469-45b0-899e-a7ce70db2f6c', foundational, national_welfare_solidarity_priority).
narrative_ontology:cs_axiom_status(national_welfare_solidarity_priority, holdable).
narrative_ontology:cs_axiom_grounding('f4e8ae20-c469-45b0-899e-a7ce70db2f6c', national_welfare_solidarity_priority, deontological).
narrative_ontology:cs_reference_frame('f4e8ae20-c469-45b0-899e-a7ce70db2f6c', treaty_sovereignty_reserved_framework).
narrative_ontology:cs_drift_state('f4e8ae20-c469-45b0-899e-a7ce70db2f6c', post_enlargement_fiscal_stress, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f4e8ae20-c469-45b0-899e-a7ce70db2f6c', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_state_executives).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, domestic_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, intra_federation_migrants).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, national_regulatory_autonomy_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__sovereignty_primary, conditional_mobility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the conditions under which free movement operates within the federation, invoke safeguard clauses and transitional restrictions, and enforce exclusions from welfare systems. They negotiate treaty revisions and opt-outs to preserve national regulatory autonomy over labor markets.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_state_executives, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive shielding from cross-border labor competition through restrictions on intra-federation migrants. Their wages and employment stability are protected by labor-market tests and welfare-access exclusions applied to newcomers.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, domestic_workers, beneficiary,
    organized, biographical, constrained, national).

% Face conditional mobility rights contingent on member state consent. Subject to labor-market tests, waiting periods for welfare entitlements, and transitional restrictions that make their access to jobs and social protection structurally subordinate to domestic populations.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, intra_federation_migrants, payer,
    moderate, biographical, constrained, continental).

% Adjudicates disputes between mobile workers and member states over the scope of free movement. In this reading, it frequently defers to state sovereignty claims and proportionality assessments that limit unconditional mobility.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federal_judiciary, observer,
    institutional, generational, analytical, continental).

% Would expand hiring across the federation if mobility were unconditional. Their preference for open labor markets is structurally under-weighted in treaty negotiations dominated by member state executives.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, cross_border_employers, excluded,
    organized, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__sovereignty_primary, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_treaty__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a federation of sovereign states by making membership politically sustainable: states retain sufficient regulatory autonomy to protect domestic labor markets and welfare systems, preventing race-to-the-bottom dynamics that could destabilize the federal compact.
% TRANSFER_FUNCTION: Restricts labor mobility and welfare access for intra-federation migrants, transferring regulatory authority and fiscal protection from the mobile worker population to member state governments and domestic resident workers.
% ABSENT_VOICES: Pro-integration legal scholars, mobile-worker advocacy organizations, and cross-border employers who would argue for unconditional or less conditional mobility rights are systematically under-weighted in treaty negotiations where member state executives hold the pen.
% DISAPPEARANCE_RATIONALE: If member states lost the authority to restrict movement and protect labor markets, domestic wage structures would face immediate competitive pressure, welfare systems would experience fiscal stress from entitlement claims, and the federal compact would face secession risks from states that joined on explicitly conditional terms.
% FOUNDING_PROBLEM: How to sustain an economic federation among sovereign states with divergent income levels and welfare regimes without triggering destabilizing labor migration, wage dumping, or fiscal free-riding that would erode public support for membership.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and comparative federalism scholars outside the benefiting state executive seats attest to pre-treaty migration crises and welfare tourism fears that shaped the original bargain. Federal-integration advocates and migrant-rights organizations contest that the current restrictions exceed what is necessary to solve the founding problem, citing independent fiscal analyses.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial but not absolute restrictions placed on mobile workers: they can move but face labor-market tests, welfare exclusions, and transitional periods. Suppression (0.55) captures the active enforcement of these conditions by national administrations and the partial suppression of unconditional mobility as an alternative. Accessibility_collapse (0.45) indicates that alternatives exist (mobile workers can seek permits, appeal to federal courts) but are bureaucratically costly and uncertain. Resistance (0.58) reflects sustained legal and political contestation by migrant advocates, pro-integration federal institutions, and sending states. Theater_ratio (0.25) is relatively low: while some sovereignty rhetoric is performative, the labor-market restrictions have material effects. The measurement series tracks a gradual hardening of extraction as enlargement waves and fiscal stress increased reliance on safeguard clauses between T=0 and T=50.
 *
 * PERSPECTIVAL GAP:
 *   The member state executive seat experiences the constraint as a necessary sovereignty reservation that makes federation membership politically sustainable; the intra-federation migrant seat experiences the same legal framework as a conditional and subordinate status. The domestic worker seat sees protection, not extraction. The engine should compute strong divergence between the agenda-setter/beneficiary seats and the payer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state executives and domestic workers are declared beneficiaries: they collect regulatory autonomy and labor-market protection respectively, giving them directionality near the beneficiary end (low d). Intra-federation migrants are declared victims: they bear the costs of restricted mobility and welfare exclusion, giving them directionality near the target end (high d). Federal institutions sit at analytical distance. No override is needed because the structural derivation from beneficiary/victim declarations and exit options correctly captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by requiring both coordination and extraction: if the federation dissolved because states refused to cede sovereignty, the coordination function (federal integrity) would be visible in retrospect. Conversely, if mobility were fully liberalized without federal collapse, the extraction story (protecting insiders) would be exposed as surplus. The tangled_rope classification holds that both elements are genuinely present and structurally coupled through the same treaty provisions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_integration_framing,
    'Is the classification of this constraint as tangled_rope stable across the kernel''s sibling readings, or does the integration_primary reading instantiate a structurally different constraint with a different epsilon?',
    'Compare the full JSON of the sovereignty_primary and integration_primary readings; if the latter assigns victim status to member states and beneficiary status to mobile workers with lower extraction, the kernel supports two distinct constraints linked by network.affects_constraints.',
    'If the sibling reading is structurally distinct, the current classification is reading-stable; if readings collapse to the same constraint, the kernel is under-decomposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_integration_framing, conceptual, 'Whether the kernel decomposes into distinct constraints per reading.').

omega_variable(
    coordination_or_protectionism,
    'Do restrictions on free movement preserve federal integrity by sustaining member state consent, or do they primarily extract from mobile workers to benefit domestic incumbents?',
    'Comparative analysis of federations with stricter versus looser mobility regimes, measuring federal stability and domestic labor-market outcomes.',
    'If restrictions are shown to be unnecessary for federal stability, the coordination story weakens and the constraint shifts toward snare; if federal dissolution follows mobility liberalization, the tangled_rope classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_protectionism, empirical, 'Whether the constraint''s coordination function is genuine or cover.').

omega_variable(
    victim_beneficiary_boundary,
    'Are domestic workers genuine beneficiaries of restricted mobility, or does the extraction ultimately accrue to state treasuries and political incumbents while workers receive only diffuse protection?',
    'Wage and employment data for domestic workers in sectors exposed to cross-border competition, compared against fiscal savings to state budgets from welfare exclusions.',
    'If gains flow primarily to state treasuries rather than workers, the beneficiary set should be reclassified toward institutional actors, altering directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_beneficiary_boundary, empirical, 'Where the extracted surplus actually lands.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__sovereignty_primary, theater_ratio, 10, 0.15).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.2).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__sovereignty_primary, theater_ratio, 30, 0.23).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__sovereignty_primary, theater_ratio, 40, 0.25).
narrative_ontology:measurement(fede_tr_t50, federation_membership_treaty__sovereignty_primary, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__sovereignty_primary, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__sovereignty_primary, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__sovereignty_primary, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(fede_be_t50, federation_membership_treaty__sovereignty_primary, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__sovereignty_primary, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__sovereignty_primary, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__sovereignty_primary, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(fede_su_t50, federation_membership_treaty__sovereignty_primary, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% This constraint is the sovereignty_primary reading of the federation_membership_treaty kernel. Its sibling readings (integration_primary, subsidiarity_balance) instantiate structurally distinct constraints from the same treaty text. The kernel decomposes across readings because the same legal text supports divergent epsilon values and stakeholder directionalities depending on which interpretive premise is adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
