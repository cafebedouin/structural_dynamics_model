% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Mutual Assured Deterrence as Standing War-Fighting Calculation
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates the deterrence-equilibrium reading of the
 *   total-war-possibility-space kernel: total war remains strategically
 *   reachable — a live option in the planning space of nuclear-armed states —
 *   but is deterred by the cost-benefit calculation imposed by mutual
 *   vulnerability (assured retaliation). Under this reading, war has not left
 *   the thinkable (contra the space-contraction reading) nor been normatively
 *   banished (contra the nuclear-taboo reading); it has been priced so high
 *   through continuously demonstrated retaliatory capacity that initiation is
 *   irrational for a rational actor. This reading therefore predicts, and the
 *   metrics reflect, persistent doctrine development, counterforce targeting
 *   refinement, and escalation-ladder theorizing as the deterrent signal's
 *   necessary maintenance activities, rather than as residue of an
 *   already-solved problem.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states: agenda_setter/beneficiary (institutional/arbitrage) — sets doctrine, captures credibility
 *   - defense_industrial_base: beneficiary (organized/arbitrage) — captures modernization procurement flow
 *   - strategic_planning_establishments: beneficiary/agenda_setter (institutional/identity_locked) — professional existence constituted by the framing
 *   - domestic_taxpayers_of_nuclear_states: payer (powerless/trapped) — funds without doctrinal voice
 *   - non_nuclear_frontline_populations: payer (powerless/trapped) — bears geographic strike risk without consent
 *   - future_generations_bearing_arsenal_risk: payer (powerless/trapped, civilizational horizon) — inherits stockpile and miscalculation risk
 *   - arms_control_treaty_bodies: excluded (organized/constrained) — sidelined whenever modernization is framed as deterrence maintenance
 *   - strategic_studies_scholars: observer (analytical) — contests which kernel reading is operative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.61).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.58).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Mutual Assured Deterrence as Standing War-Fighting Calculation").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '95d91507-bece-45d7-9352-96a5fe53e6a9').
narrative_ontology:cs_kernel_codification('95d91507-bece-45d7-9352-96a5fe53e6a9', distributed).
narrative_ontology:cs_authority_grounding('95d91507-bece-45d7-9352-96a5fe53e6a9', practice).
narrative_ontology:cs_interpretation_layer_present('95d91507-bece-45d7-9352-96a5fe53e6a9').
narrative_ontology:cs_reading_relation('95d91507-bece-45d7-9352-96a5fe53e6a9', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('95d91507-bece-45d7-9352-96a5fe53e6a9', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('95d91507-bece-45d7-9352-96a5fe53e6a9', foundational, restraint_is_calculated_not_foreclosed).
narrative_ontology:cs_axiom_status(restraint_is_calculated_not_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('95d91507-bece-45d7-9352-96a5fe53e6a9', restraint_is_calculated_not_foreclosed, empirically_contingent).
narrative_ontology:cs_axiom('95d91507-bece-45d7-9352-96a5fe53e6a9', foundational, material_capability_not_norm_is_restraining_mechanism).
narrative_ontology:cs_axiom_status(material_capability_not_norm_is_restraining_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('95d91507-bece-45d7-9352-96a5fe53e6a9', material_capability_not_norm_is_restraining_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('95d91507-bece-45d7-9352-96a5fe53e6a9', cold_war_mutual_vulnerability_equilibrium).
narrative_ontology:cs_drift_state('95d91507-bece-45d7-9352-96a5fe53e6a9', multipolar_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('95d91507-bece-45d7-9352-96a5fe53e6a9', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_base).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, strategic_planning_establishments).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, domestic_taxpayers_of_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_frontline_populations).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, future_generations_bearing_arsenal_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and continuously modernize war-fighting arsenals and escalation doctrines, justifying each increment as necessary to preserve the deterrent signal. They set the terms of the equilibrium — targeting doctrine, alert postures, arms control negotiating positions — and can adjust the arrangement unilaterally while claiming no other state can safely disarm first.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states, beneficiary).

% Captures sustained procurement contracts for delivery systems, warhead modernization, and command-control infrastructure that exist specifically because the deterrence-equilibrium framing requires continuous credibility investment. Has no structural interest in the equilibrium ever being declared solved.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_base, beneficiary,
    organized, generational, arbitrage, national).

% War colleges, think tanks, and planning staffs whose professional existence is built on theorizing escalation ladders, counterforce targeting, and crisis stability. Their expertise and institutional authority are constituted by treating total war as a live, calculable possibility rather than a foreclosed or normatively banished one.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_planning_establishments, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, strategic_planning_establishments, agenda_setter).

% Fund the continuous modernization cycles through taxation with no direct voice in targeting doctrine or arsenal sizing. Cannot meaningfully exit the arrangement — withholding funding is not a live political option, and the costs are diffused across decades of budget lines they cannot trace to any single decision.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, domestic_taxpayers_of_nuclear_states, payer,
    powerless, biographical, trapped, national).

% Live in states allied to or adjacent to nuclear powers whose escalation ladders and counterforce targeting maps place them within likely strike or fallout zones without their consent or participation in the doctrine that produces those maps. They bear the geographic risk the deterrence calculation treats as an acceptable variable.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_frontline_populations, payer,
    powerless, biographical, trapped, regional).

% Inherit stockpiled warheads, accident risk, waste, and the standing possibility that a miscalculation inside the equilibrium produces total war on their watch. They have no representation in any decision that sustains the arrangement and cannot retroactively withdraw consent.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, future_generations_bearing_arsenal_risk, payer,
    powerless, civilizational, trapped, global).

% Multilateral verification and reduction regimes that would prefer the possibility space narrowed toward irreversible disarmament. They are structurally sidelined whenever nuclear states reframe modernization as deterrence maintenance rather than arms-race escalation, since the equilibrium reading treats their preferred end-state as strategically naive.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, arms_control_treaty_bodies, excluded,
    organized, generational, constrained, global).

% Analyze the equilibrium's internal logic, contest its empirical grounding against rival readings (normative taboo, space contraction), and can shift academic and policy consensus about which reading of the kernel is operative without themselves bearing arsenal risk directly.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__deterrence_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_possibility_space__deterrence_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides mutual second-strike survivability guarantees that (on this reading) genuinely stabilize crisis behavior between nuclear-armed adversaries by making first use strategically irrational — a real coordination problem (avoiding miscalculated escalation) solved by credible, continuously demonstrated retaliatory capacity.
% TRANSFER_FUNCTION: Moves enormous continuous fiscal resources from general taxation and opportunity cost of foregone civilian investment toward weapons modernization, delivery system procurement, and planning infrastructure; moves involuntary strategic risk from decision-makers who set doctrine onto frontline and future populations who never consented to the targeting calculus that concerns them.
% ABSENT_VOICES: Non-nuclear frontline states and future generations have no seat in doctrine formulation; arms control bodies are structurally treated as aspirational rather than operative within this reading, since the reading's own premise is that disarmament below deterrence-sufficiency is strategically irrational, not merely undesirable.
% DISAPPEARANCE_RATIONALE: If the deterrence-equilibrium framing vanished overnight — if planners stopped treating total war as a live, calculable option requiring continuous cost-benefit signaling — modernization budgets, counterforce targeting doctrine, escalation-ladder theorizing, and the career structures of strategic planning establishments would all lose their organizing premise; procurement cycles tied to credibility maintenance would have no rationale to continue unchanged.
% FOUNDING_PROBLEM: Following the advent of thermonuclear weapons and the Cuban Missile Crisis, states needed a framework to avoid both unilateral disarmament (perceived as inviting attack) and uncontrolled arms racing (perceived as raising accidental-war risk) — a stable calculable equilibrium in which mutual vulnerability itself performed the restraining function.
% FOUNDING_PROBLEM_CORROBORATION: Strategic planning establishments and nuclear weapons states attest the founding problem remains live (multipolar nuclear proliferation, emerging hypersonic and cyber threats to second-strike assurance). Independent arms control scholars and several former defense officials testifying outside current procurement interests argue the original crisis-stability problem was substantially resolved by the 1990s and that continued doctrine elaboration now serves institutional and industrial continuity more than the founding stabilization function; no source entirely outside both the beneficiary states and their scholarly establishments has weighed in with primary-source authority, which is itself notable.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.61 (moderate-high, rising over the interval) because the equilibrium genuinely performs a coordination function — crisis stability between adversaries with survivable second-strike capacity is a real public good on this reading — but the same structure channels enormous continuous resource transfers to a narrow set of institutional and industrial beneficiaries whose interest in declaring the problem solved is structurally weak. Suppression (0.58) reflects that alternative postures (unilateral reduction, no-first-use commitments, disarmament below deterrence-sufficiency) are treated within the doctrine itself as strategically irrational rather than merely unchosen — a form of foreclosure built into the reading's own logic, not merely external coercion. Theater ratio rises modestly (0.22 to 0.42) as escalation-ladder theorizing and counterforce refinement increasingly outpace any observable change in crisis outcomes, consistent with a maturing signaling apparatus whose marginal doctrinal elaboration serves institutional continuity as much as strategic function. All three tracked metrics share the same six-point time grid (1962, 1975, 1991, 2005, 2016, 2024) so no metric is defaulted at any examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the arrangement reads as prudent, continuously validated coordination against an irreducible security dilemma. From the payer seats — taxpayers, frontline populations, future generations — the same structure reads as an open-ended extraction of resources and risk justified by a calculation they never participated in and cannot audit. The engine should compute divergent per-seat types from this same structural data without either seat's reading being authored as the correct one.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapons states and their planning/industrial apparatus sit near the beneficiary end: they set the doctrine, capture the procurement flow, and hold arbitrage-grade exit (they can adjust posture unilaterally). Domestic taxpayers, frontline populations, and future generations sit near the full-target end: trapped exit, no doctrinal voice, and the costs (fiscal, geographic-risk, inherited-arsenal-risk) flow structurally toward them through the same mechanism that produces the deterrent signal. Strategic planning establishments carry a secondary agenda_setter role because their professional authority is partly constitutive of the doctrine they administer, distinguishing them from the states that hold ultimate launch authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding both unilateral disarmament risk and uncontrolled arms-race risk after 1962) is authored as contested rather than flatly dead: multipolar proliferation and emerging counter-space/cyber threats to second-strike assurance are live arguments that the coordination function persists. But the corroboration trail shows the loudest attestation of continued necessity comes from the parties who also capture the procurement and doctrinal-authority benefits, while independent arms-control voices place the founding problem's acute phase in the past. This is exactly the tangled_rope signature: a real coordination function (crisis stability) persists alongside asymmetric extraction (procurement capture, doctrinal-authority capture) that active enforcement (targeting doctrine, alliance commitments, suppression of below-sufficiency postures) sustains regardless of whether the founding acuity remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the actual mechanism restraining total war material cost-benefit calculation (this reading), constructed normative taboo (nuclear_taboo_reading), or genuine removal from the strategically thinkable (space_contraction_reading)?',
    'Comparative historical analysis of crisis decision-making transcripts (Cuban Missile Crisis, Able Archer 83, Kargil) for whether decision-makers reasoned in cost-benefit terms, invoked taboo-language, or treated total war as literally off the option table; convergent evidence across multiple crises would favor one reading over the others.',
    'If the taboo or space-contraction readings are empirically dominant, this reading''s implied justification for continuous war-fighting capability investment (as necessary deterrent signaling) loses its evidentiary support, reclassifying much of the modernization apparatus as unnecessary extraction rather than functional coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which of the three kernel readings best describes the actual restraining mechanism on total war.').

omega_variable(
    doctrine_elaboration_necessity,
    'Does continued counterforce targeting refinement and escalation-ladder theorizing add real marginal deterrent credibility, or has it become self-sustaining institutional activity decoupled from strategic necessity?',
    'Track whether adversary crisis behavior changes measurably in response to doctrinal announcements versus remaining constant regardless of doctrinal elaboration; a null correlation over multiple crisis cycles would support the decoupling hypothesis.',
    'If decoupled, the rising theater_ratio reflects genuine institutional capture rather than functional signaling, strengthening the tangled_rope classification over a pure rope reading of the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_elaboration_necessity, empirical, 'Whether doctrinal elaboration still serves deterrent function or has become self-perpetuating institutional activity.').

omega_variable(
    consent_structure_of_frontline_risk,
    'Can populations in frontline or allied states be said to have consented to the risk allocation embedded in counterforce targeting doctrine, given alliance treaty ratification processes?',
    'Legal and political analysis of whether alliance treaty ratification constitutes meaningful consent to specific targeting doctrines developed unilaterally and classified after ratification.',
    'If ratification does not constitute meaningful consent to subsequently developed classified doctrine, the victim classification of frontline populations is strengthened rather than merely inferred from geography.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_structure_of_frontline_risk, conceptual, 'Whether frontline population risk-bearing has a genuine consent basis or is purely structural imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1962, 0.22).
narrative_ontology:measurement(tota_tr_t1975, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1991, 0.35).
narrative_ontology:measurement(tota_tr_t2005, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(tota_tr_t2016, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2016, 0.4).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1962, 0.42).
narrative_ontology:measurement(tota_be_t1975, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1991, 0.38).
narrative_ontology:measurement(tota_be_t2005, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(tota_be_t2016, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2016, 0.58).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2024, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1962, 0.65).
narrative_ontology:measurement(tota_su_t1975, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(tota_su_t1991, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1991, 0.45).
narrative_ontology:measurement(tota_su_t2005, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(tota_su_t2016, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__deterrence_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, space_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint, nuclear_taboo_reading, and space_contraction_reading form a three-member constraint family decomposing the colloquial 'why hasn't total war happened since 1945' claim into three structurally distinct kernels sharing the total_war_possibility_space kernel_id. Each authors its own ε: this reading (deterrence_equilibrium) authors moderate-high ε (0.61) reflecting genuine but asymmetrically-captured coordination; the taboo reading would author a different beneficiary structure (normative entrepreneurs, disarmament NGOs) and likely lower material extraction; the space_contraction reading would treat the entire cost-benefit apparatus as epistemically moot since the option is not merely deterred but foreclosed from consideration, implying near-zero ε for the deterrence-maintenance activity itself. Per the ε-invariance principle these are not the same constraint measured three ways; they are three constraints linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
