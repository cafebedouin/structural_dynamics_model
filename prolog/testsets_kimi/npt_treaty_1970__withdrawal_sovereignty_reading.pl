% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal as Sovereignty Prerogative (Sovereignty Reading)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty (NPT) is contested through multiple
 *   readings. This constraint instantiates the sovereignty reading, which
 *   treats Article X not as a narrow emergency clause but as a legitimate
 *   exercise of state sovereignty that makes all treaty obligations
 *   contingent on the security environment. Under this reading, threshold
 *   states gain option value from credible withdrawal threats, while the
 *   regime stability norm and compliant non-nuclear weapon states bear the
 *   cost of degraded bindingness. The kernel (npt_treaty_1970) also supports
 *   an oligopoly enforcement reading (Articles I-II as primary binding
 *   obligations) and a reciprocal disarmament reading (Article VI as binding
 *   bargain). This JSON instantiates ONLY the sovereignty reading as a clean,
 *   epsilon-invariant constraint.
 *
 * KEY AGENTS:
 *   - threshold_states: Primary beneficiary (powerful/mobile) â extracts option value from Article X threat credibility
 *   - nuclear_weapon_states: Agenda setter (institutional/arbitrage) â administers regime but faces erosion of bindingness under this reading
 *   - non_nuclear_weapon_states: Primary payer (organized/constrained) â relies on binding obligations that this reading renders contingent
 *   - nonproliferation_advocacy_network: Excluded voice (organized/constrained) â structurally absent from sovereignty discourse
 *   - independent_legal_scholars: Analytical observer (analytical/analytical) â evaluates textual and practice evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.62).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.48).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal as Sovereignty Prerogative (Sovereignty Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, 'a8fd66cc-8576-4633-b032-a335cc005f14').
narrative_ontology:cs_kernel_codification('a8fd66cc-8576-4633-b032-a335cc005f14', formalized).
narrative_ontology:cs_authority_grounding('a8fd66cc-8576-4633-b032-a335cc005f14', lineage).
narrative_ontology:cs_interpretation_layer_present('a8fd66cc-8576-4633-b032-a335cc005f14').
narrative_ontology:cs_reading_relation('a8fd66cc-8576-4633-b032-a335cc005f14', npt_treaty_1970__oligopoly_enforcement_reading, influences).
narrative_ontology:cs_reading_relation('a8fd66cc-8576-4633-b032-a335cc005f14', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('a8fd66cc-8576-4633-b032-a335cc005f14', foundational, unilateral_withdrawal_as_sovereign_prerogative).
narrative_ontology:cs_axiom_status(unilateral_withdrawal_as_sovereign_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('a8fd66cc-8576-4633-b032-a335cc005f14', unilateral_withdrawal_as_sovereign_prerogative, deontological).
narrative_ontology:cs_axiom('a8fd66cc-8576-4633-b032-a335cc005f14', foundational, security_environment_modulates_obligation_strength).
narrative_ontology:cs_axiom_status(security_environment_modulates_obligation_strength, holdable).
narrative_ontology:cs_axiom_grounding('a8fd66cc-8576-4633-b032-a335cc005f14', security_environment_modulates_obligation_strength, conventional).
narrative_ontology:cs_reference_frame('a8fd66cc-8576-4633-b032-a335cc005f14', westphalian_sovereignty_conditional).
narrative_ontology:cs_drift_state('a8fd66cc-8576-4633-b032-a335cc005f14', contemporary_proliferation_challenges, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('a8fd66cc-8576-4633-b032-a335cc005f14', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with advanced nuclear fuel-cycle capability that can credibly invoke Article X. They gain bargaining option value and legitimacy for withdrawal threats under the sovereignty reading, extracting leverage from the nonproliferation regime without yet proliferating.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    powerful, generational, mobile, national).

% The P5 that drafted the NPT and administer its review architecture. They seek to maintain nonproliferation bindingness while managing the sovereignty costs of enforcement. This reading erodes the absolute character of the obligations they wished to institutionalize.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% States that renounced nuclear weapons in exchange for binding obligations on others and disarmament promises. Under the sovereignty reading, the security value of those obligations is degraded because they become contingent on the security perceptions of others, while NNWS lack equivalent withdrawal leverage.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, national).

% Civil society and expert organizations that argue for absolute bindingness and strengthened safeguards. They are structurally excluded from Article X withdrawal discourse, which is framed as an inter-state sovereignty prerogative with no formal role for non-state actors.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nonproliferation_advocacy_network, excluded,
    organized, generational, constrained, global).

% Analyze treaty text, travaux preparatoires, and state practice to assess whether Article X is a narrow emergency clause or a broad affirmation of sovereign contingency. Their findings are cited by all sides but decide nothing.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, independent_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__withdrawal_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates nonproliferation by allowing sovereign states to commit to forgoing nuclear weapons while retaining a legally recognized exit valve, accommodating security anxiety without forcing immediate withdrawal or proliferation.
% TRANSFER_FUNCTION: Transfers option value and coercive leverage from the nonproliferation regime and compliant states to threshold states that can credibly threaten Article X withdrawal; transfers the character of treaty obligations from binding commitments to revocable, security-contingent arrangements.
% ABSENT_VOICES: Non-state actors, affected communities, and future generations are excluded from the inter-state sovereignty discourse around Article X; smaller NNWS without threshold capability have limited voice in defining whether obligations are contingent.
% DISAPPEARANCE_RATIONALE: If the sovereignty reading disappeared and Article X were interpreted as a narrow, extraordinary remedy rather than a broad prerogative, threshold states would lose their exit-threat leverage, compliance incentives would shift toward absolute verification, and the regime would reorganize around enforceable bindingness rather than conditional consent.
% FOUNDING_PROBLEM: How to secure indefinite nonproliferation commitments from sovereign states without requiring them to permanently renounce the ultimate security option of nuclear deterrence.
% FOUNDING_PROBLEM_CORROBORATION: Threshold states corroborate the problem as live, citing changing security environments. Non-nuclear weapon states and nonproliferation advocates attest the problem was solved by the 1968 bargain and only re-emerges as cover for proliferation; independent legal scholars and IAEA officials provide mixed corroboration from outside the beneficiary set.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderately high because the sovereignty reading converts binding obligations into contingent ones, transferring real option value to threshold states. Suppression (0.48) is moderate: the reading suppresses the alternative framing that treaties are unconditionally binding by wrapping withdrawal in sovereignty rhetoric. Theater ratio (0.28) is low-moderate â the legal argument is substantive, though sovereignty claims perform statehood. Accessibility collapse (0.45) reflects that absolute-bindingness alternatives remain debated but are marginalized in state practice. Resistance (0.52) is significant from NNWS and advocates who oppose contingent obligations. Temporal measurements trace the reading's instrumentalization from 1970 through the 2003 North Korean withdrawal and contemporary Iran crises.
 *
 * PERSPECTIVAL GAP:
 *   The threshold-state seat experiences this constraint as a beneficial option preserving sovereign security choice; the NNWS seat experiences it as an asymmetric degradation of the bargain they entered. The NWS agenda-setter seat experiences regime-management friction. The engine computes this divergence from beneficiary/victim declarations and differentiated exit options (mobile vs constrained).
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold_states are declared beneficiaries â they collect option value and bargaining leverage, placing their directionality near the beneficiary end. Non_nuclear_weapon_states are declared victims â they bear the cost of revocable obligations without equivalent exit leverage, placing their directionality near the target end. Nuclear_weapon_states are agenda_setters but not beneficiaries under this specific reading; they suffer authority erosion from the contingency it introduces.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling as pure coordination (rope) because it has identifiable victims (NNWS whose security assurances are degraded) and asymmetric extraction (option value to threshold states). It prevents mislabeling as pure extraction (snare) because the NPT still performs genuine nonproliferation coordination â most states remain members, IAEA safeguards function, and horizontal proliferation has been slower than counterfactuals predict. The active enforcement requirement (diplomatic pressure, sanctions, review conference bargaining) is what holds the coordination together despite the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_sovereignty_kernel_ambiguity,
    'Is this constraint a genuine reading of the NPT kernel, or a constructed post-hoc justification for proliferation leverage?',
    'Comparative analysis of pre-1968 negotiating history versus post-1995 state practice to determine whether the sovereignty reading was structurally intended or retroactively instrumentalized.',
    'If retroactively instrumentalized, the constraint''s extraction is higher than its coordination function warrants, pushing classification toward snare; if structurally intended, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_sovereignty_kernel_ambiguity, conceptual, 'Whether the sovereignty reading is an authentic treaty interpretation or a post-hoc cover story.').

omega_variable(
    regime_stability_vs_threshold_option,
    'Does the Article X option value stabilize the regime by accommodating security concerns, or destabilize it by undermining compliance incentives?',
    'Counterfactual regime stability modeling: compare NNWS compliance rates and withdrawal threat frequency under strict no-withdrawal interpretation versus the sovereignty reading.',
    'If stabilizing, the coordination function is stronger than the metrics suggest and extractiveness is lower; if destabilizing, the victim set is larger and the rope is more tangled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_stability_vs_threshold_option, empirical, 'Whether the withdrawal option is a safety valve or an extraction mechanism.').

omega_variable(
    sibling_reading_structural_pressure,
    'Does the sovereignty reading structurally foreclose the reciprocal disarmament reading by making Article VI obligations contingent, or merely influence its political feasibility?',
    'Legal analysis of whether a single state can simultaneously hold that obligations are security-contingent AND that Article VI creates a binding, temporally urgent disarmament obligation.',
    'If foreclosed, the sibling reading becomes logically impossible within the same framework, altering the kernel''s classification network; if influenced, both remain live and the kernel stays contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_structural_pressure, conceptual, 'Structural relationship between sovereignty and reciprocal disarmament readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_wdrl_sov_tr_t0, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(npt_wdrl_sov_tr_t10, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(npt_wdrl_sov_tr_t20, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(npt_wdrl_sov_tr_t33, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 33, 0.22).
narrative_ontology:measurement(npt_wdrl_sov_tr_t42, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 42, 0.25).
narrative_ontology:measurement(npt_wdrl_sov_tr_t55, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 55, 0.28).

% Extraction over time
narrative_ontology:measurement(npt_wdrl_sov_be_t0, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(npt_wdrl_sov_be_t10, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(npt_wdrl_sov_be_t20, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(npt_wdrl_sov_be_t33, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 33, 0.48).
narrative_ontology:measurement(npt_wdrl_sov_be_t42, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 42, 0.55).
narrative_ontology:measurement(npt_wdrl_sov_be_t55, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 55, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(npt_wdrl_sov_su_t0, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(npt_wdrl_sov_su_t10, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(npt_wdrl_sov_su_t20, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(npt_wdrl_sov_su_t33, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 33, 0.42).
narrative_ontology:measurement(npt_wdrl_sov_su_t42, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 42, 0.45).
narrative_ontology:measurement(npt_wdrl_sov_su_t55, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 55, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% The NPT kernel decomposes into three structurally distinct constraints. This sovereignty reading (high extractiveness, contingent obligations) is downstream of the basic treaty text but competes with the oligopoly and reciprocal readings over the bindingness of obligations. Each reading has a different epsilon, victim set, and directionality profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
