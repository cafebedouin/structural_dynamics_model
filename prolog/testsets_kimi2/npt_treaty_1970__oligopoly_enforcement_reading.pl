% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__oligopoly_enforcement_reading, []).

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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Oligopoly Enforcement Reading
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty of 1970 is read here as an oligopoly
 *   enforcement mechanism: Articles I-II impose a strict, binding horizontal
 *   nonproliferation regime on non-nuclear weapon states (NNWS) and threshold
 *   states, while Article VI's disarmament commitments are treated as
 *   contingent and aspirational. Under this reading, the five recognized
 *   nuclear weapon states (P5) function as beneficiaries of a permanent
 *   status hierarchy, enjoying legitimate possession and modernizing their
 *   arsenals while avoiding reciprocal accountability. The IAEA enforces the
 *   non-nuclear side of the bargain intensively, but lacks mandate or
 *   political backing to enforce Article VI against the P5. Threshold states
 *   seeking deterrence are cast as violators rather than security-seekers.
 *   The coordination function (preventing widespread proliferation) is
 *   genuine, but the extraction is asymmetric: the costs of the regime fall
 *   on the have-nots, while the haves capture the security and status rents.
 *   The metrics and claim are authored independently: the claim is
 *   tangled_rope, while the metrics describe a highly extractive, actively
 *   enforced structure with significant theater in the disarmament track.
 *
 * KEY AGENTS:
 *   - P5 nuclear states: Primary agenda-setters and beneficiaries (institutional/arbitrage/global) â maintain nuclear hierarchy and set enforcement priorities.
 *   - Non-nuclear weapon states: Primary payers (organized/constrained/global) â bear inspection burdens and forgo weapons without reciprocal disarmament enforcement.
 *   - Threshold states: Secondary payers (moderate/constrained/national) â denied deterrent pathways, subject to coercion and sanctions.
 *   - IAEA Secretariat: Enforcement administrator (institutional/constrained/global) â implements safeguards but cannot enforce Article VI on P5.
 *   - International legal scholars: Analytical observers (analytical/analytical/global) â document the asymmetry and legal tensions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.72).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.68).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Oligopoly Enforcement Reading").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'ef814bc9-226a-4051-9f5f-3fee6d525971').
narrative_ontology:cs_kernel_codification('ef814bc9-226a-4051-9f5f-3fee6d525971', formalized).
narrative_ontology:cs_authority_grounding('ef814bc9-226a-4051-9f5f-3fee6d525971', lineage).
narrative_ontology:cs_interpretation_layer_present('ef814bc9-226a-4051-9f5f-3fee6d525971').
narrative_ontology:cs_reading_relation('ef814bc9-226a-4051-9f5f-3fee6d525971', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef814bc9-226a-4051-9f5f-3fee6d525971', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('ef814bc9-226a-4051-9f5f-3fee6d525971', foundational, p5_nuclear_hierarchy_legitimate).
narrative_ontology:cs_axiom_status(p5_nuclear_hierarchy_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('ef814bc9-226a-4051-9f5f-3fee6d525971', p5_nuclear_hierarchy_legitimate, conventional).
narrative_ontology:cs_axiom('ef814bc9-226a-4051-9f5f-3fee6d525971', foundational, article_vi_aspirational_non_binding).
narrative_ontology:cs_axiom_status(article_vi_aspirational_non_binding, holdable).
narrative_ontology:cs_axiom_grounding('ef814bc9-226a-4051-9f5f-3fee6d525971', article_vi_aspirational_non_binding, conventional).
narrative_ontology:cs_reference_frame('ef814bc9-226a-4051-9f5f-3fee6d525971', p5_nuclear_hierarchy_legitimate).
narrative_ontology:cs_drift_state('ef814bc9-226a-4051-9f5f-3fee6d525971', contemporary_tpnw_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ef814bc9-226a-4051-9f5f-3fee6d525971', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, vertical_nuclear_monopoly_legitimacy).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, horizontal_nonproliferation_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Permanent members of the UN Security Council recognized as legitimate nuclear weapon states under NPT Articles I-II. They set the nonproliferation enforcement agenda through the IAEA and Security Council while avoiding binding disarmament timelines under Article VI. They benefit from status hierarchy, veto power over enforcement decisions, and freedom to modernize arsenals.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_states, beneficiary).

% States party to the NPT that have renounced nuclear weapons. They accept comprehensive IAEA safeguards, forgo nuclear hedging, and bear the sovereignty costs of inspections and technology denial. They receive security assurances of varying credibility but lack reciprocal enforcement of P5 disarmament.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% States with advanced nuclear infrastructure seeking security guarantees or regional deterrence. Denied legitimate pathway to nuclear status under the NPT, they face sanctions, isolation, and potential military intervention if pursuing weapons programs. Their security dilemmas are managed through coercion rather than integration.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, payer,
    moderate, biographical, constrained, national).

% Administers safeguards verification under the NPT and Additional Protocols. Structurally dependent on state consent and funding, it enforces horizontal nonproliferation rigorously but lacks mandate or political backing to evaluate P5 disarmament compliance under Article VI.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Analyzes treaty asymmetries and documents the legal tension between binding nonproliferation obligations and contested disarmament language. Provides independent assessment of whether the regime operates as coordination or hierarchy.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__oligopoly_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal spread of nuclear weapons to non-nuclear states through international safeguards, verification, and technology controls, reducing the risk of nuclear war and regional arms races.
% TRANSFER_FUNCTION: Moves compliance burdens, sovereignty costs, and technology denial from non-nuclear weapon states and threshold states to the P5-led hierarchy; transfers security dependence from self-help deterrence to great-power guarantees.
% ABSENT_VOICES: Threshold states seeking recognized deterrent sovereignty; non-aligned states demanding binding P5 disarmament timelines; alternative security architectures that do not depend on P5 hegemony â present at Review Conferences but structurally marginalized in enforcement design.
% DISAPPEARANCE_RATIONALE: If the NPT constraint vanished, NNWS would face immediate security dilemma pressures, threshold states would accelerate weaponization programs, the P5 would lose legal legitimacy for nonproliferation enforcement, and the global nuclear order would fragment into regional deterrence races or renewed hegemonic competition.
% FOUNDING_PROBLEM: Prevent unchecked horizontal proliferation of nuclear weapons in the early Cold War, particularly to West Germany, Japan, and other allied states, while preserving superpower nuclear monopoly and alliance stability.
% FOUNDING_PROBLEM_CORROBORATION: Non-nuclear weapon states and the Non-Aligned Movement attest the bargain was sold as reciprocal and time-bound. Cold War historians and international legal scholars outside the P5 note the treaty was designed primarily for alliance management and superpower stability rather than equal security. The International Court of Justice 1996 advisory opinion corroborates that Article VI contains a legal obligation of conduct, contradicting the purely aspirational framing.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the P5 capture permanent status and security benefits while NNWS accept permanent obligation and sovereignty costs with no enforceable quid pro quo. Suppression (0.68) reflects the active enforcement needed to sustain this asymmetry: IAEA inspections, technology denial, sanctions, and Security Council coercion against proliferators. Theater ratio (0.45) captures the performative disarmament discourse at NPT Review Conferences, where ritual commitment substitutes for measurable vertical disarmament. Accessibility collapse (0.60) indicates that while formal alternatives exist (withdrawal under Article X, regional treaties), they are politically and economically prohibitive for most states. Resistance (0.55) reflects persistent NNWS and NAM demands for disarmament timelines, resisted by P5.
 *
 * PERSPECTIVAL GAP:
 *   The P5 seat experiences the constraint as legitimate great-power management of a dangerous technology; the NNWS seat experiences it as a discriminatory regime that locks in hierarchy. The threshold state seat experiences active coercion. The engine will compute these divergent classifications from the same structural data: P5 as near-beneficiary (low d), NNWS as target (high d), threshold states as trapped targets (high d, constrained exit). The IAEA seat sits near symmetric, administering without capturing the rent.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map directly to the NPT hierarchy: p5_nuclear_states are declared beneficiaries because they collect status, security, and freedom from reciprocal enforcement. non_nuclear_weapon_states and threshold_states are declared victims (payers) because they bear the sovereignty and security costs of a regime that denies them the deterrent option while the declared possessors avoid disarmament. The IAEA is not a beneficiary (it collects no rent) and not a victim (it is not extracted from), but an administrative agent whose directionality is structurally tied to the P5 agenda.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the NPT as pure extraction (snare) or pure coordination (rope). The genuine coordination function â preventing horizontal nuclear proliferation and reducing the probability of nuclear war â is acknowledged through the coordination_type declaration and the nonzero but bounded theater ratio. However, the asymmetric extraction is structurally necessary to the constraint's operation: without P5 acquiescence, the regime collapses, and P5 acquiescence is purchased through status immunity. This hybridity is the core of the tangled_rope diagnosis. Mandatrophy would manifest if the coordination function (nonproliferation) were ever fully achieved, leaving only the extraction (hierarchy); the persistence of threshold-state challenges and regional proliferation risks indicates the coordination function remains live, preventing piton decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_character,
    'Is Article VI a legally binding obligation of conduct with temporal urgency, or an aspirational commitment contingent on strategic context?',
    'ICJ case law review, state practice analysis on disarmament negotiations, and textual interpretation of ''good faith'' obligation under the Vienna Convention on the Law of Treaties.',
    'If Article VI is binding and urgent, the oligopoly reading mischaracterizes the treaty structure and the extraction asymmetry is less structurally entrenched; if aspirational, the asymmetry is legally codified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_character, conceptual, 'Ambiguity in Article VI legal status drives reading divergence').

omega_variable(
    threshold_state_sovereignty,
    'Are threshold states victims of a discriminatory regime, or security-seeking actors attempting to free-ride on a public good?',
    'Counterfactual regional security analysis: assess proliferation cascades if NPT obligations were absent for threshold states.',
    'Reframes victimhood versus responsibility; if threshold states would generate destabilizing arms races, the coordination function is stronger than the extraction reading suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_sovereignty, empirical, 'Threshold state status as victim or free-rider').

omega_variable(
    kernel_reading_decomposition,
    'This constraint instantiates the oligopoly_enforcement_reading of kernel npt_treaty_1970. How does the structural classification change if the reciprocal_disarmament_reading or withdrawal_sovereignty_reading is adopted instead?',
    'Comparative classification of sibling constraint stories in the same kernel family.',
    'If the reciprocal reading is adopted, the constraint reclassifies toward rope (genuine reciprocal coordination); if the withdrawal reading dominates, the constraint fragments into contested sovereignty claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer frame ambiguity across NPT kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_oligopoly_tr_t0, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt_oligopoly_tr_t10, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(npt_oligopoly_tr_t20, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(npt_oligopoly_tr_t30, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(npt_oligopoly_tr_t40, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(npt_oligopoly_tr_t50, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(npt_oligopoly_be_t0, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(npt_oligopoly_be_t10, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(npt_oligopoly_be_t20, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(npt_oligopoly_be_t30, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(npt_oligopoly_be_t40, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(npt_oligopoly_be_t50, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(npt_oligopoly_su_t0, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(npt_oligopoly_su_t10, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(npt_oligopoly_su_t20, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(npt_oligopoly_su_t30, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(npt_oligopoly_su_t40, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(npt_oligopoly_su_t50, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
