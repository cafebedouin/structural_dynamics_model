% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Treaty Text â NNWS Reading (Binding Disarmament / Conditional Restraint)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint story instantiates the Non-Nuclear Weapon State (NNWS)
 *   reading of the NPT treaty text kernel. Under this reading, Article VI
 *   imposes a binding legal obligation on Nuclear Weapon States (NWS) to
 *   pursue and conclude nuclear disarmament negotiations, while the
 *   non-proliferation obligations of NNWS under Article II constitute
 *   conditional restraint that purchases NWS compliance. The reading competes
 *   with the NWS reading (non-proliferation binding, disarmament
 *   aspirational) and the withdrawal-threshold reading (regime stability vs.
 *   sovereignty preservation). The NNWS reading attempts to impose
 *   constraints on NWS through Review Conference pressure, consensus
 *   diplomacy, and TPNW regime competition, but its enforcement mechanisms
 *   remain weak, producing moderate extraction from NNWS who continue to bear
 *   restraint costs without reciprocal disarmament.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary beneficiary/agenda-setter (institutional/constrained) â controls review conferences, benefits from NNWS restraint
 *   - non_nuclear_weapon_states: Primary payer (organized/constrained) â bears sovereignty cost of non-proliferation, receives incomplete disarmament
 *   - tpnw_advocacy_coalition: Excluded alternative (organized/mobile) â constructed parallel regime after NPT deadlock
 *   - international_court_of_justice: Analytical observer (institutional/analytical) â affirms legal obligation without enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.52).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.45).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Treaty Text â NNWS Reading (Binding Disarmament / Conditional Restraint)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control/treaty_interpretation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, '53ceb816-f95f-47ef-88e4-c377b174b05c').
narrative_ontology:cs_kernel_codification('53ceb816-f95f-47ef-88e4-c377b174b05c', fixed_text).
narrative_ontology:cs_authority_grounding('53ceb816-f95f-47ef-88e4-c377b174b05c', lineage).
narrative_ontology:cs_interpretation_layer_present('53ceb816-f95f-47ef-88e4-c377b174b05c').
narrative_ontology:cs_reading_relation('53ceb816-f95f-47ef-88e4-c377b174b05c', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('53ceb816-f95f-47ef-88e4-c377b174b05c', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('53ceb816-f95f-47ef-88e4-c377b174b05c', foundational, article_vi_binding_disarmament_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_disarmament_obligation, holdable).
narrative_ontology:cs_axiom_grounding('53ceb816-f95f-47ef-88e4-c377b174b05c', article_vi_binding_disarmament_obligation, conventional).
narrative_ontology:cs_axiom('53ceb816-f95f-47ef-88e4-c377b174b05c', foundational, nnws_restraint_conditional_on_nws_compliance).
narrative_ontology:cs_axiom_status(nnws_restraint_conditional_on_nws_compliance, holdable).
narrative_ontology:cs_axiom_grounding('53ceb816-f95f-47ef-88e4-c377b174b05c', nnws_restraint_conditional_on_nws_compliance, conventional).
narrative_ontology:cs_reference_frame('53ceb816-f95f-47ef-88e4-c377b174b05c', npt_reciprocal_grand_bargain).
narrative_ontology:cs_drift_state('53ceb816-f95f-47ef-88e4-c377b174b05c', post_tpnw_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53ceb816-f95f-47ef-88e4-c377b174b05c', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recognized nuclear-weapon states under NPT Article IX that control the Review Conference agenda and consensus rules. They benefit from the non-proliferation restraint of NNWS while resisting the adoption of binding disarmament timetables under Article VI, asserting instead that disarmament is an aspirational long-term goal.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, nuclear_weapon_states, beneficiary).

% States party to the NPT that have forgone nuclear weapons under Article II, exercising conditional restraint in exchange for promised NWS compliance with Article VI. They bear the sovereignty cost of permanent non-nuclear status and participate in IAEA safeguards, while receiving incomplete disarmament performance from NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, beneficiary).

% Coalition of states and civil society actors that advanced the 2017 Treaty on the Prohibition of Nuclear Weapons after NPT disarmament progress stalled. Structurally marginalized in NPT consensus processes where nuclear umbrella allies and NWS block critical language, but able to construct an alternative legal exit through parallel regime creation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, tpnw_advocacy_coalition, excluded,
    organized, generational, mobile, global).

% Issued the 1996 advisory opinion affirming that Article VI creates an obligation to pursue good-faith negotiations toward nuclear disarmament. Lacks enforcement capacity and observes compliance gaps without coercive authority, providing interpretive guidance that structurally supports the NNWS reading but cannot compel NWS action.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nnws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unchecked nuclear proliferation by creating a legal framework in which NNWS forgo nuclear weapons in exchange for NWS progress toward disarmament, solving the collective-action problem of horizontal nuclear arms racing while committing vertical arsenals to reduction.
% TRANSFER_FUNCTION: NNWS transfer sovereign nuclear option and accept safeguards burdens to the non-proliferation regime; NWS transfer (or promise to transfer) nuclear arsenal reductions and security assurances in exchange. Under the NNWS reading, the flow is conditional: NNWS restraint purchases NWS compliance.
% ABSENT_VOICES: States that never joined the NPT (India, Pakistan, Israel) or withdrew (North Korea) are absent from compliance discourse. The TPNW advocacy coalition is formally excluded from NPT consensus mechanisms. Neutral disarmament-focused civil society voices are procedurally heard but structurally unable to alter consensus outcomes dominated by NWS and umbrella allies.
% DISAPPEARANCE_RATIONALE: If the NPT obligation structure under this reading vanished overnight, the legal and political basis for NNWS permanent non-nuclear status would fracture. Alliances built on extended deterrence would face renegotiation pressure, horizontal proliferation risks would rise, and the TPNW coalition would likely accelerate alternative regime formation.
% FOUNDING_PROBLEM: Prevent unchecked nuclear proliferation while preserving peaceful uses of nuclear energy; create a stable grand bargain between nuclear 'haves' and 'have-nots' that trades NNWS restraint for NWS disarmament.
% FOUNDING_PROBLEM_CORROBORATION: Academic arms-control scholars and the 1996 ICJ advisory opinion attest to the original reciprocal bargain. NWS depositories corroborate the non-proliferation founding problem but dispute that disarmament non-compliance constitutes a breach. Independent nuclear policy institutes (e.g., SIPRI, Chatham House) outside the NWS beneficiary set attest that the disarmament side of the bargain remains substantially unfulfilled.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate because the constraint extracts sovereign nuclear option from NNWS while NWS avoid the disarmament costs the reading assigns to them. Suppression (0.45) is moderate because the constraint relies on institutional enforcement (IAEA safeguards, export control regimes, review conference pressure) rather than direct coercion, though the suppression of NNWS nuclear alternatives is real. Theater_ratio (0.42) has risen over the interval as NPT Review Conferences have become increasingly performative, producing lengthy consensus documents with minimal disarmament substance. Accessibility_collapse (0.50) reflects that alternatives exist (TPNW, Article X withdrawal) but remain politically and economically inaccessible for most NNWS. Resistance (0.55) is significant: NWS actively resist binding disarmament timelines, while a subset of NNWS resist through TPNW and hardening review conference language.
 *
 * PERSPECTIVAL GAP:
 *   The NWS seat experiences the NPT as a successful non-proliferation regime with aspirational disarmament language that preserves strategic stability; from this seat the constraint is coordination it maintains. The NNWS seat experiences the same treaty as a breached reciprocal bargain in which its sovereign restraint is not matched by NWS action; from this seat the constraint extracts unrewarded compliance. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are structural beneficiaries: they collect non-proliferation restraint from NNWS and control the enforcement agenda without incurring the disarmament costs the treaty assigns to them (d near beneficiary end). NNWS are structural payers: they bear the sovereignty cost of permanent non-nuclear status and safeguards compliance while receiving incomplete counter-performance (d near target end). The TPNW coalition is excluded rather than coordinated â their marginalization is the flip side of NWS agenda control. The ICJ sits at the analytical pole, providing interpretive support without altering power flows.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing nuclear proliferation through a reciprocal bargain â remains partially live: non-proliferation has held, but the disarmament quid pro quo has atrophied. The mandatrophy risk is mislabeling the constraint as pure coordination (rope) when NWS have captured the benefits of NNWS restraint without fulfilling their end of the bargain. The moderate epsilon metric (0.52) and rising theater ratio capture this atrophy without collapsing the type into snare, because the coordination function (non-proliferation) is genuine and not merely cover. The classification as rope with moderate extraction, rather than tangled rope, reflects the absence of active coercive enforcement maintaining the asymmetry â the extraction persists through institutional inertia and NWS agenda control, not through a hardened enforcement mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_kernel_ambiguity,
    'Does the NPT treaty text structurally support the NNWS reciprocal-bargain reading, the NWS unilateral-nonproliferation reading, or is it genuinely indeterminate between them?',
    'Authoritative ICJ contentious case or universal treaty amendment clarifying Article VI; or historical travaux analysis establishing original intent.',
    'If the text is indeterminate, the constraint is a distributed commitment system with no single valid reading and the engine''s per-seat divergence is irreducible. If the text supports the NNWS reading, NWS are in material breach and the effective extraction shifts toward the NWS payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_kernel_ambiguity, conceptual, 'Whether the NPT text determinately supports one reading over its siblings.').

omega_variable(
    article_vi_enforcement_gap,
    'Is the absence of Article VI enforcement mechanisms a designed feature of sovereign equality and great-power consent (coordination cost), or an extraction-enabling defect that allows NWS to free-ride on NNWS restraint?',
    'Comparative analysis of enforcement provisions in other arms control treaties; historical negotiating record of the 1968 treaty and 1995 indefinite extension.',
    'If designed, the moderate epsilon reflects necessary coordination cost in a sovereignty-heavy regime. If defect, the asymmetry is extractive and the constraint trends toward tangled rope as the enforcement gap hardens into institutionalized free-riding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_enforcement_gap, empirical, 'Whether the Article VI enforcement gap is design or defect.').

omega_variable(
    nnws_collective_action_capacity,
    'Can NNWS actually impose costs on NWS through Review Conference pressure and TPNW regime competition, or are these mechanisms symbolic without material leverage?',
    'Track NWS behavior change following specific Review Conference outcomes or TPNW entry into force; measure alliance defection rates from extended deterrence.',
    'If the mechanisms are symbolic, the NNWS reading''s attempt to constrain NWS is theater and the constraint''s effective extraction is higher than its coordination value. If material, the rope classification is strengthened by genuine reciprocal accountability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nnws_collective_action_capacity, empirical, 'Whether NNWS institutional mechanisms exert real constraint on NWS.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_nnws_tr_t0, npt_treaty_text__nnws_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(npt_nnws_tr_t10, npt_treaty_text__nnws_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(npt_nnws_tr_t20, npt_treaty_text__nnws_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(npt_nnws_tr_t30, npt_treaty_text__nnws_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(npt_nnws_tr_t40, npt_treaty_text__nnws_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(npt_nnws_tr_t50, npt_treaty_text__nnws_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(npt_nnws_tr_t55, npt_treaty_text__nnws_reading, theater_ratio, 55, 0.42).

% Extraction over time
narrative_ontology:measurement(npt_nnws_be_t0, npt_treaty_text__nnws_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(npt_nnws_be_t10, npt_treaty_text__nnws_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(npt_nnws_be_t20, npt_treaty_text__nnws_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(npt_nnws_be_t30, npt_treaty_text__nnws_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(npt_nnws_be_t40, npt_treaty_text__nnws_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(npt_nnws_be_t50, npt_treaty_text__nnws_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(npt_nnws_be_t55, npt_treaty_text__nnws_reading, base_extractiveness, 55, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(npt_nnws_su_t0, npt_treaty_text__nnws_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(npt_nnws_su_t10, npt_treaty_text__nnws_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(npt_nnws_su_t20, npt_treaty_text__nnws_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(npt_nnws_su_t30, npt_treaty_text__nnws_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement(npt_nnws_su_t40, npt_treaty_text__nnws_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(npt_nnws_su_t50, npt_treaty_text__nnws_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement(npt_nnws_su_t55, npt_treaty_text__nnws_reading, suppression_requirement, 55, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% The npt_treaty_text kernel decomposes into three structurally distinct readings: the NNWS reading (binding disarmament / conditional restraint), the NWS reading (binding non-proliferation / aspirational disarmament), and the withdrawal_threshold_reading (regime stability vs sovereignty preservation). Each reading instantiates a different constraint with different beneficiary/victim structures and epsilon values. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
