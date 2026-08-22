% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market as Natural Default: Hybrid Amnesia Reading
 *   domain: political_economy/ideology
 *
 * SUMMARY:
 *   This constraint story models the ideology that market allocation is the
 *   natural and default mode of economic organization. Under the hybrid
 *   amnesia reading, this constraint originated in a period of genuine
 *   historical forgetting (1930sâ1970s), when non-market alternatives fell
 *   out of practical memory due to planning failures and wartime coordination
 *   fatigue. In a second stage (1980sâpresent), identifiable
 *   beneficiariesâcorporate incumbents and neoliberal policy
 *   networksâinherited this amnesia and weaponized it through defensive
 *   rationalization, actively suppressing the recovery of alternative
 *   imaginaries. The extractiveness trajectory rises from modest (0.20) to
 *   moderate (0.45) as the constraint shifts from passive background
 *   assumption to actively enforced closure.
 *
 * KEY AGENTS:
 *   - Neoliberal policy network (agenda_setter/beneficiary): administers the ideology and enforces its boundaries through institutional gatekeeping.
 *   - Corporate incumbents (beneficiary): capture rents from deregulation and TINA policy constraints without running the ideological apparatus.
 *   - Constrained policymakers (payer): identity-locked professionals who enforce the constraint while suffering truncated policy imagination.
 *   - Publics denied alternatives (payer): powerless populations who lose access to decommodified goods and services.
 *   - Heterodox economists (excluded): structurally barred from contesting the naturalness of market allocation.
 *   - Economic historians (observer): provide external corroboration of the kernel's historical contingency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.65).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market as Natural Default: Hybrid Amnesia Reading").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '10dba737-75f3-4e4d-92d8-72bb3f0cbae2').
narrative_ontology:cs_kernel_codification('10dba737-75f3-4e4d-92d8-72bb3f0cbae2', distributed).
narrative_ontology:cs_authority_grounding('10dba737-75f3-4e4d-92d8-72bb3f0cbae2', extraction).
narrative_ontology:cs_interpretation_layer_present('10dba737-75f3-4e4d-92d8-72bb3f0cbae2').
narrative_ontology:cs_reading_relation('10dba737-75f3-4e4d-92d8-72bb3f0cbae2', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('10dba737-75f3-4e4d-92d8-72bb3f0cbae2', market_as_natural_default__beneficiary_maintained_reading, influences).
narrative_ontology:cs_axiom('10dba737-75f3-4e4d-92d8-72bb3f0cbae2', foundational, two_stage_amnesia_capture).
narrative_ontology:cs_axiom_status(two_stage_amnesia_capture, holdable).
narrative_ontology:cs_axiom_grounding('10dba737-75f3-4e4d-92d8-72bb3f0cbae2', two_stage_amnesia_capture, empirically_contingent).
narrative_ontology:cs_axiom('10dba737-75f3-4e4d-92d8-72bb3f0cbae2', foundational, market_naturalness_is_historical_closure).
narrative_ontology:cs_axiom_status(market_naturalness_is_historical_closure, holdable).
narrative_ontology:cs_axiom_grounding('10dba737-75f3-4e4d-92d8-72bb3f0cbae2', market_naturalness_is_historical_closure, empirically_contingent).
narrative_ontology:cs_reference_frame('10dba737-75f3-4e4d-92d8-72bb3f0cbae2', market_pragmatism_without_closure).
narrative_ontology:cs_drift_state('10dba737-75f3-4e4d-92d8-72bb3f0cbae2', contemporary_neoliberal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('10dba737-75f3-4e4d-92d8-72bb3f0cbae2', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, corporate_incumbents).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, neoliberal_policy_network).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, constrained_policymakers).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, publics_denied_alternatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the market-naturalization narrative through think tanks, academic economics departments, and international financial institutions. Collects funding, status, and policy influence from maintaining the kernel's stability. Could theoretically pivot to other coordination frames but is identity-locked to market fundamentalism as a professional community.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, neoliberal_policy_network, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, neoliberal_policy_network, beneficiary).

% Benefit from deregulation, privatization, and TINA policy constraints without directly administering the ideological apparatus. Their rent extraction depends on the foreclosure of non-market allocation. They can relocate or restructure but cannot easily exit the political economy the ideology sustains.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, corporate_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the cost of truncated policy imagination. Their professional identity and career paths are constituted through market-friendly competency; proposing alternatives risks exclusion from serious policy forums. They enforce the constraint on publics while being themselves constrained by it.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, constrained_policymakers, payer,
    moderate, biographical, identity_locked, national).

% Experience the constraint as the absence of decommodified goods and services that were historically available or imaginable. Their political demands are filtered through a framework that treats market allocation as the natural baseline, making non-market claims appear utopian or irrational.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, publics_denied_alternatives, payer,
    powerless, immediate, trapped, national).

% Would argue for planned, solidarity, or commons-based allocation but are structurally marginalized in hiring, publishing, and policy access. Their exclusion is constitutive of the constraint's stability.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, heterodox_economists, excluded,
    moderate, generational, constrained, national).

% Document the historical contingency of market dominance and the forgetting of mid-century alternatives. They do not bear the constraint's costs directly but provide the evidentiary basis for contesting its naturalness.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a policy default that coordinates state and corporate action around a shared economic model, reducing deliberation costs and legitimizing governance during periods of ideological uncertainty.
% TRANSFER_FUNCTION: Moves ideological authority and policy autonomy from publics and policymakers to corporate incumbents and neoliberal intellectual networks by foreclosing alternative economic imaginaries and substituting market-based allocation as the tacit baseline.
% ABSENT_VOICES: Heterodox economists, labor organizers, commons advocates, and de-growth planners are structurally excluded from policy forums, mainstream curricula, and media representation; they would contest the naturalness of market allocation if admitted.
% DISAPPEARANCE_RATIONALE: If the naturalized market default vanished as an ideological constraint, policy space would expand to include public planning, cooperative ownership, and decommodified provision; corporate rent-seeking strategies dependent on TINA logic would face political resistance; the neoliberal policy network would lose epistemic authority and funding.
% FOUNDING_PROBLEM: Interwar and postwar economies faced genuine coordination failures in planning and mixed-economy experiments; market mechanisms provided a pragmatic fallback for resource allocation when centralized planning proved cumbersome or authoritarian.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the neoliberal network attest that mid-century planning failures were historically specific and remediable, not inherent to non-market allocation. Heterodox economists corroborate that alternative coordination mechanisms remain viable. The beneficiary network does not corroborate the founding problem as live; instead it asserts the market as a trans-historical natural baseline.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scalar metrics are anchored at the interval end (T=90). Extractiveness (0.45) reflects moderate but significant extraction: the ideology does not extract material tribute directly, but it forecloses policy options worth substantial welfare gains. Suppression (0.65) is higher than extractiveness because the constraint's persistence now depends on active exclusion of heterodoxy from forums, curricula, and media. Theater ratio (0.45) captures the growing share of performative defenseâinvocations of 'market realities' that function more as boundary-policing than empirical description. Accessibility collapse (0.80) is high because once the naturalness frame is internalized, alternatives become cognitively unavailable. Resistance (0.40) is moderate: heterodox movements exist but remain marginal. The measurement series share a single grid and show the second-stage ratchet clearly.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (neoliberal policy network) experiences the constraint as necessary coordination: it stabilizes expectations, reduces political transaction costs, and prevents what they see as destructive policy experimentation. The payer seats (constrained policymakers and denied publics) experience the same structure as foreclosure: their range of actionable options collapses, and the costs of this collapse are borne unevenly by those with the least exit. The engine computes this divergence from the structural dataâidentical constraint, opposing directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (corporate incumbents, neoliberal policy network) sit near the full-beneficiary end: the constraint subsidizes their authority and rents, and their exit options (arbitrage, institutional mobility) are strong. Payers (constrained policymakers, publics denied alternatives) sit near the full-target end: the constraint extracts policy autonomy and material welfare, and their exit options are identity_locked or trapped. The excluded seat (heterodox economists) would sit near the target end if admitted, but their exclusion amplifies effective extraction for those trapped inside.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpragmatic coordination failure in mid-century planningâis dead. The constraint persists not because the problem is live, but because beneficiaries actively maintain the closure. This triggers the R5 mismatch signal (founding_problem_status=dead + disappearance_verdict=world_rearranges), flagging capture or zombie dynamics. However, because concentrated beneficiaries remain and the theater ratio, while rising, has not fully replaced function with performance, the constraint is classified as tangled_rope rather than piton. The mandate has atrophied, but the extraction is beneficiary-driven, not purely inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amnesia_to_capture_boundary,
    'At what historical juncture did genuine ideological forgetting transform into defensive rationalization by beneficiaries?',
    'Archival research tracing corporate and foundation funding of neoliberal think tanks, alongside discourse analysis of policy documents to detect the shift from pragmatic acceptance to naturalized necessity.',
    'Would sharpen the early-stage epsilon estimate and clarify whether the first stage was genuinely low-extraction or already asymmetric; may shift weight between tangled_rope and snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amnesia_to_capture_boundary, empirical, 'Historical boundary between genuine forgetting and beneficiary capture').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative economic imaginaries structural (institutional exclusion of heterodoxy) or internalized (policymakers genuinely believe TINA)?',
    'Post-exit suppression trajectory: study whether policymakers continue to reject non-market alternatives after leaving office or after institutional displacement.',
    'If internalized, effective suppression exceeds the structural measure and may raise the computed extraction for the identity-locked payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    intentionality_of_beneficiaries,
    'Do corporate incumbents and neoliberal networks consciously weaponize amnesia, or does selection favor those who exploit pre-existing closure without centralized intention?',
    'Historical sociology of ideas tracing funding memos, policy entrepreneurship networks, and corporate strategic documents against a counterfactual of diffuse selection pressure.',
    'High intentionality supports snare-like classification; post-hoc selection favors tangled_rope with emergent extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intentionality_of_beneficiaries, conceptual, 'Conscious conspiracy versus emergent capture mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mark_tr_t15, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(mark_tr_t45, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 45, 0.25).
narrative_ontology:measurement(mark_tr_t60, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(mark_tr_t75, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 75, 0.4).
narrative_ontology:measurement(mark_tr_t90, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 90, 0.45).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(mark_be_t15, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(mark_be_t45, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 45, 0.3).
narrative_ontology:measurement(mark_be_t60, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(mark_be_t75, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 75, 0.42).
narrative_ontology:measurement(mark_be_t90, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 90, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mark_su_t15, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 30, 0.32).
narrative_ontology:measurement(mark_su_t45, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 45, 0.48).
narrative_ontology:measurement(mark_su_t60, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(mark_su_t75, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 75, 0.63).
narrative_ontology:measurement(mark_su_t90, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 90, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one component of the market_as_natural_default kernel family. It is decomposed from the colloquial label 'market fundamentalism' because the kernel's epsilon varies by historical mechanism: pure forgetting, pure capture, and hybrid two-stage each produce structurally distinct constraints with different victim sets and enforcement profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
