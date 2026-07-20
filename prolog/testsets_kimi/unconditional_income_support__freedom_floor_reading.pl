% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support: Autonomy-Enabling Freedom Floor
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_floor_reading of the
 *   unconditional_income_support kernel. It treats unconditional cash
 *   transfers not as redistribution alone but as a coordination mechanism
 *   that restores valid consent to labor-market participation by guaranteeing
 *   a material exit option. The reading claims Pareto improvement: autonomy
 *   is expanded for beneficiaries without identifiable victims, because the
 *   fiscal cost is offset by reduced social spending and improved matching
 *   efficiency. The constraint is claimed as ropeâpure coordinationâwhile
 *   the authored metrics acknowledge moderate extractiveness due to the scale
 *   of redistribution required, leaving the engine to measure the divergence.
 *
 * KEY AGENTS:
 *   - precarious_workers: Primary beneficiary (moderate/constrained) â gains outside option in labor market.
 *   - unpaid_caregivers: Primary beneficiary (powerless/identity_locked) â economic recognition of unwaged work.
 *   - creative_workers: Primary beneficiary (moderate/mobile) â reduced precarity in cultural production.
 *   - domestic_abuse_survivors: Primary beneficiary (powerless/trapped) â private income enabling exit from abusive households.
 *   - net_taxpayers: Payer (organized/constrained) â bears fiscal cost; claimed indirect beneficiary via social efficiency.
 *   - state_disbursement_authority: Agenda setter (institutional/arbitrage) â administers and could alter the arrangement.
 *   - labor_market_analysts: Observer (organized/analytical) â evaluates employment and fiscal effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.45).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.22).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support: Autonomy-Enabling Freedom Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, '3e649ad5-cf24-420a-b5bc-4252f2f43695').
narrative_ontology:cs_kernel_codification('3e649ad5-cf24-420a-b5bc-4252f2f43695', formalized).
narrative_ontology:cs_authority_grounding('3e649ad5-cf24-420a-b5bc-4252f2f43695', lineage).
narrative_ontology:cs_interpretation_layer_present('3e649ad5-cf24-420a-b5bc-4252f2f43695').
narrative_ontology:cs_reading_relation('3e649ad5-cf24-420a-b5bc-4252f2f43695', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e649ad5-cf24-420a-b5bc-4252f2f43695', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('3e649ad5-cf24-420a-b5bc-4252f2f43695', foundational, unconditionality_as_status_equality).
narrative_ontology:cs_axiom_status(unconditionality_as_status_equality, holdable).
narrative_ontology:cs_axiom_grounding('3e649ad5-cf24-420a-b5bc-4252f2f43695', unconditionality_as_status_equality, deontological).
narrative_ontology:cs_axiom('3e649ad5-cf24-420a-b5bc-4252f2f43695', foundational, autonomy_enables_valid_consent).
narrative_ontology:cs_axiom_status(autonomy_enables_valid_consent, holdable).
narrative_ontology:cs_axiom_grounding('3e649ad5-cf24-420a-b5bc-4252f2f43695', autonomy_enables_valid_consent, deontological).
narrative_ontology:cs_reference_frame('3e649ad5-cf24-420a-b5bc-4252f2f43695', autonomous_market_participation).
narrative_ontology:cs_drift_state('3e649ad5-cf24-420a-b5bc-4252f2f43695', post_pilot_evidence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3e649ad5-cf24-420a-b5bc-4252f2f43695', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, creative_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, domestic_abuse_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, net_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work irregular hours or gig contracts without security. The unconditional floor gives them a stable baseline to refuse the worst offers and wait for better matches, reducing the coercion of immediate necessity.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, constrained, national).

% Provide care for children, elderly, or disabled family members without wages. The floor recognizes their work with cash income, reducing dependence on a partner or the state and lowering the barrier to exit from exploitative household arrangements.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, biographical, identity_locked, national).

% Pursue artistic or cultural production with volatile earnings. The floor removes the choice between destitution and abandoning their practice, allowing longer search periods for paid creative work.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, creative_workers, beneficiary,
    moderate, biographical, mobile, national).

% Often trapped financially by partners who control household income. The unconditional individual payment creates a private income stream that does not depend on the abuser or on proving eligibility to a caseworker.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, domestic_abuse_survivors, beneficiary,
    powerless, immediate, trapped, local).

% Finance the grant through general taxation. They do not receive net transfers. The freedom-floor narrative holds that they benefit indirectly from lower social costs and better labor matching, but they bear the direct fiscal cost.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, net_taxpayers, payer,
    organized, biographical, constrained, national).

% Legislates benefit levels, funds them through the tax system, and operates the payment infrastructure. Can modify eligibility, amount, or conditionality. In this reading, it is supposed to refrain from means-testing or work requirements.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, state_disbursement_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Study employment effects, matching efficiency, and fiscal incidence. Some document improved voluntary job transitions; others warn of inflation or tax-base erosion. They do not collect or pay the transfer directly.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, labor_market_analysts, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__freedom_floor_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates the coordination failure where individuals are forced into exploitative or mismatched labor because they lack an outside option, enabling voluntary participation in care, creative, and market work without bureaucratic stigma.
% TRANSFER_FUNCTION: Moves purchasing power from net taxpayers to all residents unconditionally, with the largest relative impact on those with zero or unstable market income.
% ABSENT_VOICES: Targeted-welfare advocates who believe conditionality is necessary for legitimacy; fiscal conservatives who would prefer tax cuts over transfers; and employers of low-wage labor who lose bargaining power when workers have an exit option.
% DISAPPEARANCE_RATIONALE: If the unconditional floor vanished overnight, precarious workers would immediately lose their outside option and be forced into mismatched or exploitative employment; unpaid caregivers would face renewed economic dependence and stigma; domestic abuse survivors would lose the financial independence required to exit dangerous situations; and labor markets would revert to asymmetric bargaining where refusal is not a viable option.
% FOUNDING_PROBLEM: Industrial labor markets coupled with residual welfare states leave large populations without a survival floor outside of employment or bureaucratic eligibility, creating coercion in labor contracts and devaluing unpaid care.
% FOUNDING_PROBLEM_CORROBORATION: Labor sociologists and feminist economists attest to the coercion of unwaged caregivers and precarious workers from outside the beneficiary set; longitudinal studies from the Alaska Permanent Fund and Kenya GiveDirectly pilots corroborate that a floor changes bargaining behavior without collapsing labor supply.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.45 (moderate) because the constraint requires substantial sustained redistribution through the tax system; this is descriptively honest even though the reading frames the outcome as coordination. Suppression is low (0.22) because receipt is unconditional and the constraint does not rely on excluding alternativesâtargeted welfare still exists in parallel. Theater ratio is low (0.15) because the mechanism is direct cash transfer with minimal performative overhead. Accessibility collapse is moderate (0.35): alternatives such as means-tested benefits and private charity persist and are debated. Resistance is moderate (0.40) because fiscal conservatives and employer lobbies actively oppose the tax burden and bargaining-power shift. The measurement grid tracks a gradual rise in extractiveness and theater as the policy moves from pilot to mainstream proposal, increasing political contestation.
 *
 * PERSPECTIVAL GAP:
 *   The net_taxpayer seat and the precarious_worker seat should compute different directionalities: the former bears the direct fiscal transfer while the latter receives the subsidy. The state_disbursement_authority sits near the beneficiary end because it controls the mechanism and does not personally bear the cost. The absence of declared victims prevents the engine from classifying this as a snare or tangled rope, but the moderate extractiveness and payer presence will pull the net_taxpayer seat toward a target classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (precarious_workers, unpaid_caregivers, creative_workers, domestic_abuse_survivors) are structurally subsidized by the constraintâlow directionality. Net_taxpayers are the structural source of fundingâhigher directionality. Because the story declares no victims, the automatic derivation will not push any agent to the full-target extreme. The state_disbursement_authority is agenda_setter with arbitrage exit, placing it near the beneficiary end. The engine will compute asymmetric extraction only if the redistribution is strongly regressive or coercively enforced; here it is neither, so effective extraction is concentrated on the payer seat as a coordination cost rather than an extractive surplus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâlack of a survival floor outside labor markets and the devaluation of careâremains live in most jurisdictions. The arrangement has not persisted past the obsolescence of its justification, so mandatrophy is not in play. Should the constraint outlive the live problem (e.g., if automation eliminated labor-market coercion entirely), it would need to be re-evaluated for piton or scaffold drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pareto_improvement_or_redistribution,
    'Is unconditional income support truly a Pareto improvement, or does it impose concentrated net costs on specific taxpayer cohorts that the freedom-floor reading obscures?',
    'Disaggregated lifetime fiscal incidence studies tracking net transfers by income decile and wealth cohort.',
    'If net costs are concentrated on identifiable groups, the no-victim claim fails and directionality for those groups shifts toward the target end, potentially reclassifying the constraint as tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pareto_improvement_or_redistribution, empirical, 'Whether the fiscal structure has hidden victims among net payers.').

omega_variable(
    labor_supply_generalizability,
    'Are the observed small labor-supply effects in Alaska and Kenya generalizable to unconditional grants at full national scale in advanced economies, or do they depend on partial, peripheral, or temporary programs?',
    'Full-scale, long-duration randomized controlled trials in large advanced economies with diverse labor markets.',
    'If labor supply falls substantially at scale, the coordination story weakens, extraction from taxpayers rises, and the rope classification becomes contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_generalizability, empirical, 'Generalizability of pilot labor-supply neutrality to full-scale implementation.').

omega_variable(
    freedom_floor_reading_validity,
    'This constraint is the freedom_floor_reading of the unconditional_income_support kernel. The sibling dependency_trap_reading claims the same fiscal mechanism is incentive-distorting. Is the disagreement located in empirical labor-supply elasticities, or in the normative weight placed on autonomy versus desert?',
    'Cross-referencing empirical labor-supply evidence with normative framing analysis of policy discourse to isolate the locus of dispute.',
    'If the disagreement is purely empirical, the kernel reading with better evidence should dominate; if it is purely normative, both readings remain structurally valid as competing normative constraints on the same fiscal kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_floor_reading_validity, conceptual, 'Kernel reading contest: location of disagreement between freedom_floor and dependency_trap readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__freedom_floor_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(unco_tr_t25, unconditional_income_support__freedom_floor_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(unco_tr_t40, unconditional_income_support__freedom_floor_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(unco_tr_t50, unconditional_income_support__freedom_floor_reading, theater_ratio, 50, 0.14).
narrative_ontology:measurement(unco_tr_t55, unconditional_income_support__freedom_floor_reading, theater_ratio, 55, 0.15).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__freedom_floor_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(unco_be_t25, unconditional_income_support__freedom_floor_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(unco_be_t40, unconditional_income_support__freedom_floor_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(unco_be_t50, unconditional_income_support__freedom_floor_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(unco_be_t55, unconditional_income_support__freedom_floor_reading, base_extractiveness, 55, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__freedom_floor_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__freedom_floor_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(unco_su_t25, unconditional_income_support__freedom_floor_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement(unco_su_t40, unconditional_income_support__freedom_floor_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(unco_su_t50, unconditional_income_support__freedom_floor_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(unco_su_t55, unconditional_income_support__freedom_floor_reading, suppression_requirement, 55, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the unconditional_income_support kernel. The freedom_floor reading treats the kernel as an autonomy-enabling coordination mechanism; the dependency_trap reading treats it as incentive-distorting extraction; the universality_paradox reading treats it as a politically unstable compromise. They share the same fiscal kernel but instantiate different beneficiary/victim structures and epsilon profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
