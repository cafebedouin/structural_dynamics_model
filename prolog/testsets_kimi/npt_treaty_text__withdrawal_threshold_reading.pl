% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__withdrawal_threshold_reading, []).

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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold Ambiguity
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint instantiates the withdrawal_threshold_reading of the
 *   contested npt_treaty_text kernel. While sibling readings center on
 *   Articles II and VI (non-proliferation bindingness and disarmament
 *   obligation), this reading focuses on Article X's 'extraordinary events'
 *   threshold for withdrawal. The text is structurally ambiguous between a
 *   high threshold (regime stability priority) and a low threshold
 *   (sovereignty preservation priority). The North Korean withdrawal
 *   precedent (2003) and subsequent Iranian nuclear diplomacy have turned
 *   this ambiguity into a live coordination-extraction mechanism: threshold
 *   states retain credible exit options, while the broader non-proliferation
 *   constituency bears the uncertainty. The constraint is claimed as
 *   tangled_rope because the same ambiguous clause that coordinates regime
 *   participation also asymmetrically transfers strategic leverage to
 *   threshold states.
 *
 * KEY AGENTS:
 *   - threshold_states (Iran, Japan, South Korea): Beneficiaries with moderate power and constrained exit â they gain bargaining leverage from the ambiguous exit option.
 *   - non_proliferation_constituency: Payers with organized power but constrained exit â their security depends on regime stability, which the ambiguity undermines.
 *   - depositary_states (P5/NWS): Agenda-setters with institutional power and arbitrage exit â they maintain the ambiguity through diplomatic non-decision.
 *   - international_legal_community: Analytical observers documenting the indeterminacy without authority to resolve it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.55).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.5).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold Ambiguity").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '5d1158b4-d13c-4424-8613-166d07f46abc').
narrative_ontology:cs_kernel_codification('5d1158b4-d13c-4424-8613-166d07f46abc', fixed_text).
narrative_ontology:cs_authority_grounding('5d1158b4-d13c-4424-8613-166d07f46abc', lineage).
narrative_ontology:cs_interpretation_layer_present('5d1158b4-d13c-4424-8613-166d07f46abc').
narrative_ontology:cs_reading_relation('5d1158b4-d13c-4424-8613-166d07f46abc', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d1158b4-d13c-4424-8613-166d07f46abc', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_axiom('5d1158b4-d13c-4424-8613-166d07f46abc', foundational, state_sovereignty_preserves_exit_option).
narrative_ontology:cs_axiom_status(state_sovereignty_preserves_exit_option, holdable).
narrative_ontology:cs_axiom_grounding('5d1158b4-d13c-4424-8613-166d07f46abc', state_sovereignty_preserves_exit_option, conventional).
narrative_ontology:cs_axiom('5d1158b4-d13c-4424-8613-166d07f46abc', secondary, ambiguous_threshold_sustains_regime_equilibrium).
narrative_ontology:cs_axiom_status(ambiguous_threshold_sustains_regime_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('5d1158b4-d13c-4424-8613-166d07f46abc', ambiguous_threshold_sustains_regime_equilibrium, instrumental).
narrative_ontology:cs_reference_frame('5d1158b4-d13c-4424-8613-166d07f46abc', npt_original_compromise_framework).
narrative_ontology:cs_drift_state('5d1158b4-d13c-4424-8613-166d07f46abc', post_dprk_withdrawal_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5d1158b4-d13c-4424-8613-166d07f46abc', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_constituency).
narrative_ontology:constraint_vindicates(npt_treaty_text__withdrawal_threshold_reading, state_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with advanced nuclear latency (e.g., Iran, Japan, South Korea) that remain NPT parties in part because Article X preserves a credible, if ambiguous, exit option. They benefit from the interpretive ambiguity because it allows them to signal potential withdrawal without triggering immediate regime collapse or military pre-emption, maintaining leverage within the treaty framework.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    moderate, generational, constrained, global).

% The majority of NNWS and NWS that depend on the NPT's permanence for strategic stability. They bear the cost of the ambiguous withdrawal pathway: uncertainty about whether threshold states will remain constrained, potential cascade withdrawal scenarios, and the erosion of the non-proliferation norm when exit appears readily available. They cannot easily exit the non-proliferation order because their security is constituted by it.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_constituency, payer,
    organized, civilizational, constrained, global).

% The five nuclear-weapon states and depositary governments that steward the NPT regime. They actively maintain the interpretive ambiguity around Article X because adjudicating a definitive threshold would risk either deterring threshold states from joining or remaining, or fatally weakening the regime by legitimizing easy exit. Their enforcement takes the form of diplomatic non-decision and procedural containment at Review Conferences.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, depositary_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Academic and juridical experts analyzing Article X who note that the text's 'extraordinary events' language lacks a defined evidentiary standard, and that state practice post-DPRK has not clarified whether a subjective determination suffices. They document the ambiguity but lack authority to resolve it.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, international_legal_community, observer,
    analytical, generational, analytical, global).

% Civil society organizations that would argue for either a strictly prohibitive withdrawal regime or for sovereign exit rights, but are structurally excluded from NPT Review Conference decision-making, which is state-centric.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, anti_nuclear_advocacy_networks, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__withdrawal_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the continued participation of advanced-technology NNWS in the non-proliferation regime by preserving a theoretical exit pathway, preventing immediate defection to overt nuclear acquisition while keeping diplomatic channels open.
% TRANSFER_FUNCTION: Moves strategic flexibility and bargaining leverage from the non-proliferation community to threshold states, and moves diplomatic labor and regime-stability costs from threshold states to the depositary states and compliant parties.
% ABSENT_VOICES: Anti-nuclear advocacy networks and affected non-state populations are excluded from treaty interpretation; states with no nuclear latency have limited voice in Article X debates because the issue is dominated by threshold-state and depositary-state bargaining.
% DISAPPEARANCE_RATIONALE: If the Article X ambiguity were resolved definitively â either by a binding high threshold that eliminated exit credibility or a binding low threshold that normalized withdrawal â the strategic calculus of threshold states would shift immediately. A high threshold would likely accelerate covert breakout programs; a low threshold would trigger cascade withdrawal concerns. The current arrangement persists because the ambiguity itself is the coordination mechanism.
% FOUNDING_PROBLEM: The original NPT negotiators needed to secure universal adherence from sovereign states who would not permanently surrender the right to self-defense; Article X was drafted as a safety valve to prevent immediate non-participation by states reserving ultimate sovereignty over national security.
% FOUNDING_PROBLEM_CORROBORATION: Depositary states and early negotiators attest the clause was a necessary compromise. However, non-proliferation legal scholars and NNWS diplomats outside the threshold-state beneficiary set argue the founding problem has mutated: the clause now serves latent proliferation leverage rather than sovereign insurance, and the original compromise has drifted into structural instability.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.55) because the ambiguous pathway does not extract material rents but does transfer strategic option value from the regime-stability constituency to threshold states. Suppression is moderate (0.5) because the ambiguity is actively maintained by suppressing definitive interpretation at Review Conferences and avoiding binding adjudication. Theater ratio rises to 0.38 because an increasing share of diplomatic activity at Review Conferences performs regime maintenance while deliberately avoiding the Article X threshold question. Accessibility collapse is 0.4 because treaty amendment or authoritative interpretation are technically available alternatives but politically inaccessible. Resistance is 0.5 because the non-proliferation constituency actively pushes for stricter interpretive standards but is blocked by depositary-state procedural control.
 *
 * PERSPECTIVAL GAP:
 *   Threshold states experience the ambiguity as a necessary sovereign guarantee and would compute a low or negative effective extraction (they are subsidized by the option value). The non-proliferation constituency experiences the same clause as a dangerous loophole extracting regime certainty. Depositary states sit asymmetrically: they do not directly collect the extraction, but their arbitrage-grade exit (they control the interpretive agenda) positions them closer to the beneficiary end than the target end, even though they are not declared beneficiaries. The engine computes these divergences from structural position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map directly to structural relationships: threshold_states are named beneficiaries because the ambiguous clause subsidizes their strategic flexibility. non_proliferation_constituency is named victim because it bears the regime-stability costs of that same ambiguity. depositary_states are agenda_setters with arbitrage exit; the derivation chain will place them at low d because they control the constraint's administration, though they are not direct rent-collectors. The engine will compute low effective extraction for threshold states and depositary states, and high effective extraction for the non-proliferation constituency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â sovereign insurance to secure universal adherence â is contested. If the problem were definitively dead, the clause would risk classification as piton (theatrical maintenance of a safety valve no longer needed) or snare (pure extraction of regime stability). However, the ambiguity continues to serve a genuine coordination function: without it, at least one threshold state would likely exit or openly break out. Because coordination and extraction are structurally coupled in the same clause, the correct classification is tangled_rope. This prevents mislabeling the arrangement as pure coordination (rope) â the extraction is real and asymmetric â and as pure extraction (snare) â the coordination function is load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_threshold_intentionality,
    'Does Article X encode an intentional ambiguity as a safety valve, or is the threshold contest an artifact of inartful drafting subsequently exploited by threshold states?',
    'Historical travaux prÃ©paratoires analysis combined with state practice survey to determine original intent versus emergent function.',
    'If intentional, the constraint is closer to a designed scaffold or rope; if emergent, it is a drift-induced tangled rope with stronger extraction dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_threshold_intentionality, conceptual, 'Intentionality of Article X ambiguity').

omega_variable(
    dprk_precedent_legitimacy,
    'Does the Democratic People''s Republic of Korea''s 2003 withdrawal constitute state practice that legitimates a low-threshold interpretation, or was it a breach that should be treated as an outlier?',
    'Subsequent state conduct and opinio juris analysis; Review Conference consensus documents referencing or ignoring the DPRK precedent.',
    'If the precedent is legitimating, the effective suppression of low-threshold withdrawal is lower than measured and the constraint''s extraction may be higher; if it is an outlier, the ambiguity is shallower and the regime more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dprk_precedent_legitimacy, empirical, 'Status of DPRK withdrawal as precedent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(npt__tr_t20, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(npt__tr_t30, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(npt__tr_t35, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(npt__tr_t45, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 45, 0.33).
narrative_ontology:measurement(npt__tr_t55, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 55, 0.38).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(npt__be_t10, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(npt__be_t20, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(npt__be_t30, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(npt__be_t35, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 35, 0.4).
narrative_ontology:measurement(npt__be_t45, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 45, 0.48).
narrative_ontology:measurement(npt__be_t55, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 55, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(npt__su_t10, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(npt__su_t20, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(npt__su_t30, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(npt__su_t35, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 35, 0.42).
narrative_ontology:measurement(npt__su_t45, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 45, 0.48).
narrative_ontology:measurement(npt__su_t55, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 55, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, nnws_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty text kernel. The kernel decomposes into at least three structurally distinct constraints: the NWS reading (non-proliferation as binding), the NNWS reading (disarmament as binding), and this withdrawal threshold reading (Article X ambiguity). Each has a distinct epsilon, beneficiary structure, and classification. They coexist as live positions in diplomatic discourse and influence each other's legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
