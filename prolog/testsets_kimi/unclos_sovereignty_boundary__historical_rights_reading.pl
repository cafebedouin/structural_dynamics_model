% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Usage Sovereign Rights Override of UNCLOS EEZ Provisions
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint is the historical_rights_reading of the contested kernel
 *   unclos_sovereignty_boundary. Expansive claimant states assert that
 *   pre-modern usage, cartographic records, and occupation generate sovereign
 *   title that predates and overrides UNCLOS EEZ delimitation. The doctrine
 *   converts historical narrative into a legal constraint that reallocates
 *   maritime control from treaty-defined coastal entitlements to
 *   civilizational claimant states. Sibling readings include
 *   strict_eez_reading (UNCLOS Article 57 limits are exclusive and supreme)
 *   and non_ratifier_enforcement_reading (freedom of navigation is
 *   independently enforceable customary law).
 *
 * KEY AGENTS:
 *   - expansive_claimant_states: Primary agenda-setter and beneficiary (institutional/regional) â constructs and enforces the historical rights doctrine, collects expanded sovereign control over maritime zones and resources.
 *   - eez_holding_coastal_states: Primary payer (moderate/regional) â loses exclusive economic control to historical overlay claims, diplomatically constrained from effective exit.
 *   - navigational_actors: Secondary payer (powerful/global) â faces increased constraint on freedom of navigation, must conduct costly enforcement patrols or accept restricted passage regimes.
 *   - affected_fishing_communities: Tertiary payer (powerless/local) â trapped by expanded state claims that absorb traditional fishing grounds without legal recourse.
 *   - international_arbitration_bodies: Analytical observer (institutional/global) â reviews claims against UNCLOS, issues rulings that claimant states structurally resist.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.82).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.85).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Usage Sovereign Rights Override of UNCLOS EEZ Provisions").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, 'bc418626-8f49-4443-ac07-82a1d64b821e').
narrative_ontology:cs_kernel_codification('bc418626-8f49-4443-ac07-82a1d64b821e', fixed_text).
narrative_ontology:cs_authority_grounding('bc418626-8f49-4443-ac07-82a1d64b821e', lineage).
narrative_ontology:cs_interpretation_layer_present('bc418626-8f49-4443-ac07-82a1d64b821e').
narrative_ontology:cs_reading_relation('bc418626-8f49-4443-ac07-82a1d64b821e', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('bc418626-8f49-4443-ac07-82a1d64b821e', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('bc418626-8f49-4443-ac07-82a1d64b821e', foundational, historical_usage_generates_sovereign_title).
narrative_ontology:cs_axiom_status(historical_usage_generates_sovereign_title, holdable).
narrative_ontology:cs_axiom_grounding('bc418626-8f49-4443-ac07-82a1d64b821e', historical_usage_generates_sovereign_title, empirically_contingent).
narrative_ontology:cs_axiom('bc418626-8f49-4443-ac07-82a1d64b821e', foundational, unclos_derivative_status).
narrative_ontology:cs_axiom_status(unclos_derivative_status, holdable).
narrative_ontology:cs_axiom_grounding('bc418626-8f49-4443-ac07-82a1d64b821e', unclos_derivative_status, conventional).
narrative_ontology:cs_reference_frame('bc418626-8f49-4443-ac07-82a1d64b821e', historical_sovereignty_continuity).
narrative_ontology:cs_drift_state('bc418626-8f49-4443-ac07-82a1d64b821e', contemporary_unclos_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bc418626-8f49-4443-ac07-82a1d64b821e', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, navigational_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, affected_fishing_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert that centuries of maps, fishing activity, and naval patrols generate sovereign title predating UNCLOS. Enforce the claim through coast guard and navy patrols, artificial island construction, and diplomatic refusal to accept adverse tribunal rulings. Collect expanded maritime jurisdiction and exclusive resource rights.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, beneficiary).

% Hold UNCLOS-granted EEZ entitlements that are overlapped or overridden by historical rights assertions. Lose exclusive control over fisheries, hydrocarbons, and seabed rights within the overlapped zone. Diplomatic protest risks retaliation from the claimant; litigation risks non-compliance by the claimant.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    moderate, generational, constrained, regional).

% Commercial shipping fleets and naval forces face increased constraint in waters claimed under historical rights doctrine. Must either accept prior-notification regimes, conduct costly freedom-of-navigation operations, or reroute around contested corridors. The constraint raises insurance premiums and operational uncertainty.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, navigational_actors, payer,
    powerful, biographical, constrained, global).

% Traditional fishing grounds fall inside expanded historical claims and are no longer accessible under local licensing or are patrolled by claimant-state maritime forces. Lack legal standing to challenge state-level claims and depend on subsistence harvests from increasingly restricted waters.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, affected_fishing_communities, payer,
    powerless, immediate, trapped, local).

% Receive cases brought by affected coastal states, apply UNCLOS and customary law, and issue binding or advisory rulings on the validity of historical rights overlay claims. Their authority is structurally resisted by expansive claimant states that decline to participate or recognize outcomes.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_arbitration_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__historical_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a bloc of claimant states around a shared legal-narrative framework that legitimizes maritime expansion without requiring contemporaneous, consent-based boundary delimitation under UNCLOS.
% TRANSFER_FUNCTION: Moves exclusive economic control and navigational priority over broad maritime zones from proximate coastal states and the international shipping community to expansive historical-claimant states.
% ABSENT_VOICES: Small-scale fishing communities whose traditional grounds are absorbed into expanded claims; smaller coastal states lacking great-power alliance backing; commercial marine insurers who internalize risk but are excluded from the legal determination.
% DISAPPEARANCE_RATIONALE: If the historical rights constraint vanished, EEZ boundaries would revert to UNCLOS Article 57 measurements, coastal states would resume exclusive resource control, navigational routes would clear of unilateral prior-notification demands, and the maritime order would reorganize around treaty-defined entitlements rather than historical-overlay assertions.
% FOUNDING_PROBLEM: How to legitimize maritime control in regions where modern treaty-based boundary delimitation either produces unsatisfying outcomes for major civilizational claimants or where pre-modern usage patterns predate the Westphalian and UNCLOS legal order.
% FOUNDING_PROBLEM_CORROBORATION: Expansive claimant states attest the problem as live, citing civilizational continuity. The Permanent Court of Arbitration (South China Sea ruling, 2016), third-party maritime law scholars, and affected coastal states attest the problem was manufactured to circumvent treaty limits; the tribunal ruling corroborated the dead or overridden status from outside the beneficiary set.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint transfers substantial sovereign and economic control from treaty-defined holders to claimant states. Suppression is slightly higher (0.85) because the arrangement requires active naval, coast guard, and diplomatic enforcement to override the UNCLOS order and suppress adverse tribunal rulings. Theater ratio is moderate (0.48) and rising: material enforcement is real, but an increasing share of activity is performative diplomacy and historical-map display aimed at legitimizing expansion. Accessibility collapse is moderate-high (0.68): UNCLOS alternatives remain formally available but are operationally blocked by non-compliance and power asymmetry. Resistance is high (0.72) due to persistent legal challenges, freedom-of-navigation operations, and diplomatic coalitions opposing the overlay claims.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (expansive claimant states) experiences the constraint as restorative coordination â restoring civilizational territorial continuity. The payer seats (coastal states, navigational actors, fishing communities) experience the same structure as unilateral treaty override and extraction. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive claimant states are beneficiaries with constrained but state-backed exit, placing their directionality near the beneficiary end. EEZ-holding coastal states and navigational actors are declared victims with constrained or trapped exit, placing their directionality near the target end and amplifying effective extraction. Fishing communities are powerless and trapped, experiencing the highest effective extraction. International arbitration bodies sit at the analytical pole with negligible extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents mislabeling it as pure coordination (it is not a neutral legal order but an asymmetric transfer) and prevents mislabeling it as pure snare (there is a genuine coordination function among the claimant-state bloc around a shared historical-legal narrative). The temporal measurements show extraction and suppression rising together, indicating that enforcement has intensified to maintain the constraint against mounting resistance rather than decaying into piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_ambiguity,
    'Is the historical_rights_reading a genuine expression of pre-existing customary international law, or a post-hoc constructed constraint that benefits expansive claimant states by overriding UNCLOS?',
    'Comparative structural analysis against the strict_eez_reading and non_ratifier_enforcement_reading siblings: if strict EEZ compliance produces coherent legal outcomes without historical-overlay exceptions across the full kernel domain, the historical reading is likely constructed extraction; if independent customary-law analysis affirms historical title as a separate source of obligation, the reading gains legitimacy.',
    'If constructed, the constraint trends toward snare; if genuine customary law, it remains a contested tangled_rope with real coordination value for the claimant bloc.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_ambiguity, conceptual, 'Whether this kernel reading is constructed cover or genuine customary law.').

omega_variable(
    historical_evidence_veracity,
    'Do archival, cartographic, and archaeological records substantiate the claimed historical usage and occupation to the degree asserted by claimant states?',
    'Independent third-party historical review; declassification of state archives; comparative cartographic analysis.',
    'If historical records are weak or fabricated, the foundational axiom historical_usage_generates_sovereign_title loses empirical grounding and the reading drifts toward extraction; if records are robust, the reading''s extraction metric is moderated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_veracity, empirical, 'Empirical basis of historical usage claims.').

omega_variable(
    enforcement_sustainability,
    'Can expansive claimant states sustain the naval, coast guard, and diplomatic enforcement required to override UNCLOS without triggering major-power armed conflict?',
    'Observation of enforcement trends, militarization rates, and conflict-escalation indicators over the next decade.',
    'If enforcement is unsustainable, the constraint collapses toward piton or dissolves; if sustainable, extraction hardens and theater_ratio declines as enforcement normalizes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Long-term viability of coercive override of treaty order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(uncl_tr_t7, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 7, 0.35).
narrative_ontology:measurement(uncl_tr_t14, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 14, 0.4).
narrative_ontology:measurement(uncl_tr_t21, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 21, 0.38).
narrative_ontology:measurement(uncl_tr_t28, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 28, 0.42).
narrative_ontology:measurement(uncl_tr_t35, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 35, 0.45).
narrative_ontology:measurement(uncl_tr_t42, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 42, 0.48).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uncl_be_t7, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(uncl_be_t14, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(uncl_be_t21, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 21, 0.65).
narrative_ontology:measurement(uncl_be_t28, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 28, 0.72).
narrative_ontology:measurement(uncl_be_t35, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement(uncl_be_t42, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 42, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(uncl_su_t7, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 7, 0.55).
narrative_ontology:measurement(uncl_su_t14, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 14, 0.62).
narrative_ontology:measurement(uncl_su_t21, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 21, 0.7).
narrative_ontology:measurement(uncl_su_t28, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 28, 0.76).
narrative_ontology:measurement(uncl_su_t35, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 35, 0.82).
narrative_ontology:measurement(uncl_su_t42, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 42, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the unclos_sovereignty_boundary kernel. The label 'UNCLOS sovereignty boundary' conflates three structurally distinct claims: strict EEZ supremacy (low extraction, treaty-based), historical rights override (high extraction, lineage-based), and non-ratifier enforcement (moderate extraction, customary-law based). Each reading carries different beneficiary/victim structures and different epsilon values. They are modeled as separate stories linked via affects_constraints, not as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
