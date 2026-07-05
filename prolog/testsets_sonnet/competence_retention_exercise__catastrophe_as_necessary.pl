% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe-as-Necessary Doctrine of Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the 'catastrophe_as_necessary' reading of a
 *   contested kernel about how organizations retain safety competence over
 *   time. The doctrine holds that only lived-through catastrophic events
 *   supply the organizational learning and visceral stakes necessary for
 *   genuine competence retention, and that simulation, however sophisticated,
 *   is rehearsal rather than the real thing — meaning competence decays
 *   invisibly during incident-free periods and organizations are most
 *   vulnerable exactly when they appear safest. This reading treats real
 *   disasters as necessary system resets. It is one of three competing
 *   readings of the same kernel (the others hold simulation alone is
 *   sufficient, or that near-misses provide an adequate bridge without
 *   requiring full catastrophe); each reading is authored as its own
 *   constraint per the ε-invariance principle, and this file does not average
 *   or hedge across them.
 *
 * KEY AGENTS:
 *   - veteran_operators_with_disaster_experience: primary beneficiary (moderate/constrained) — status derives from disaster-survivor authority
 *   - incident_investigation_specialists: beneficiary (organized/mobile) — professional field expands after catastrophe
 *   - regulatory_bodies_post_disaster: beneficiary/agenda_setter (institutional/arbitrage) — gains rulemaking leverage from disasters
 *   - frontline_operators_between_incidents: primary target (powerless/trapped) — bears the doctrine's implied fatalism during quiet periods
 *   - communities_near_high_risk_facilities: target (powerless/trapped) — pays in health and property for the 'necessary reset'
 *   - workers_injured_or_killed_in_qualifying_events: target (powerless/trapped) — literally embodies the 'visceral stakes' the doctrine requires
 *   - high_reliability_organization_researchers: analytical observer — assesses comparative evidence across training regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.58).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.42).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe-as-Necessary Doctrine of Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, '6f412f64-8d52-4ff6-985c-51ce5fee24e0').
narrative_ontology:cs_kernel_codification('6f412f64-8d52-4ff6-985c-51ce5fee24e0', distributed).
narrative_ontology:cs_authority_grounding('6f412f64-8d52-4ff6-985c-51ce5fee24e0', practice).
narrative_ontology:cs_interpretation_layer_present('6f412f64-8d52-4ff6-985c-51ce5fee24e0').
narrative_ontology:cs_reading_relation('6f412f64-8d52-4ff6-985c-51ce5fee24e0', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('6f412f64-8d52-4ff6-985c-51ce5fee24e0', competence_retention_exercise__near_miss_as_bridge, coexists_with).
narrative_ontology:cs_axiom('6f412f64-8d52-4ff6-985c-51ce5fee24e0', foundational, visceral_stakes_irreplaceable_by_simulation).
narrative_ontology:cs_axiom_status(visceral_stakes_irreplaceable_by_simulation, holdable).
narrative_ontology:cs_axiom_grounding('6f412f64-8d52-4ff6-985c-51ce5fee24e0', visceral_stakes_irreplaceable_by_simulation, empirically_contingent).
narrative_ontology:cs_axiom('6f412f64-8d52-4ff6-985c-51ce5fee24e0', secondary, competence_decays_invisibly_absent_real_stakes).
narrative_ontology:cs_axiom_status(competence_decays_invisibly_absent_real_stakes, holdable).
narrative_ontology:cs_axiom_grounding('6f412f64-8d52-4ff6-985c-51ce5fee24e0', competence_decays_invisibly_absent_real_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('6f412f64-8d52-4ff6-985c-51ce5fee24e0', post_disaster_institutional_memory_doctrine).
narrative_ontology:cs_drift_state('6f412f64-8d52-4ff6-985c-51ce5fee24e0', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6f412f64-8d52-4ff6-985c-51ce5fee24e0', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, incident_investigation_specialists).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, veteran_operators_with_disaster_experience).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, regulatory_bodies_post_disaster).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, frontline_operators_between_incidents).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, communities_near_high_risk_facilities).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, workers_injured_or_killed_in_qualifying_events).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, normalization_of_deviance_thesis).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, high_reliability_organization_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their professional standing, hazard-pay differentials, and internal authority derive substantially from having 'been there' during a real event. They train juniors by invoking what happened, and their expertise is treated by management as irreplaceable precisely because it cannot be simulated. Displacing this doctrine would flatten a status hierarchy they occupy the top of.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, veteran_operators_with_disaster_experience, beneficiary,
    moderate, biographical, constrained, national).

% A professional field (root-cause analysts, post-mortem consultancies, NTSB-style bodies) whose institutional relevance and funding expand after real disasters. Each catastrophe generates contracts, hearings, and citations; a world where simulation were treated as fully sufficient would shrink the demand for their specific expertise.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, incident_investigation_specialists, beneficiary,
    organized, generational, mobile, national).

% Gain expanded rulemaking authority, budget, and public legitimacy in the aftermath of catastrophic events; the doctrine that only real disasters yield real learning gives regulators standing to impose sweeping post-incident mandates that simulation-based near-miss review would not justify. They administer which events count as 'real enough' to trigger reform.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, regulatory_bodies_post_disaster, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__catastrophe_as_necessary, regulatory_bodies_post_disaster, agenda_setter).

% Work the incident-free stretches during which the doctrine claims their competence is quietly decaying, yet have no mechanism to prove or disprove this about themselves short of living through a disaster. They bear the anxiety of being told they cannot really be prepared, and bear the literal physical risk if the doctrine's implied prescription (wait for the real thing) plays out on their shift.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, frontline_operators_between_incidents, payer,
    powerless, biographical, trapped, local).

% Live downwind, downstream, or downrange of the facilities whose 'necessary system reset' is, from the community's side, an actual disaster with casualties and contamination. They pay the doctrine's tuition in health and property with no seat in deciding whether simulation might have sufficed instead.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, communities_near_high_risk_facilities, payer,
    powerless, generational, trapped, regional).

% Are the literal instantiation of the 'visceral stakes' the doctrine claims are pedagogically necessary. Their injuries and deaths are retrospectively narrated as the source of the organization's improved competence, converting their harm into the institution's curriculum.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, workers_injured_or_killed_in_qualifying_events, payer,
    powerless, immediate, trapped, local).

% Sell high-fidelity simulators and tabletop exercises and would argue their product is structurally sufficient exercise of the relevant competence. Their commercial interest is treated as disqualifying testimony rather than evidence, and they are rarely invited into the post-disaster reform conversation that the catastrophe-as-necessary doctrine dominates.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_simulation_vendors, excluded,
    organized, biographical, constrained, national).

% Study nuclear plants, carrier flight decks, and air traffic control to determine empirically whether simulated stress, near-misses, or actual catastrophe drive genuine competence retention. They can compile comparative organizational data but are not the ones deciding which training regime a given facility funds.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, high_reliability_organization_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, diffuse).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The doctrine coordinates institutional attention and resources toward preserving hard-won operational knowledge that arose from real disasters — ensuring that lessons paid for in casualties are not lost to organizational amnesia during quiet periods.
% TRANSFER_FUNCTION: Moves authority, funding, and professional status toward those positioned as custodians of disaster-derived knowledge (veteran operators, investigators, regulators) and moves risk and injury toward frontline workers and nearby communities who supply the 'necessary' catastrophic events the doctrine treats as pedagogically irreplaceable.
% ABSENT_VOICES: Simulation vendors and near-miss analysts who would argue real catastrophe is not required are structurally discounted as self-interested or as offering an inferior substitute; injured workers and affected communities, whose bodies and property constitute the doctrine's 'visceral stakes,' are rarely consulted on whether the tradeoff was worth making.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, veteran operators would lose a distinctive status claim, investigation specialists and post-disaster regulators would lose some leverage for sweeping reform mandates, and organizations might shift real resources toward simulation and near-miss analysis instead of implicitly awaiting the next real event. Whether operational competence would actually degrade is exactly the empirical question the doctrine forecloses by definition — proponents say the world rearranges dangerously, critics say almost nothing changes except who gets credit for learning.
% FOUNDING_PROBLEM: Organizations that had lived through catastrophic failures (nuclear near-meltdowns, industrial explosions, aviation disasters) needed a way to explain why their post-disaster procedures were more rigorous than pre-disaster ones, and to argue that this rigor could not simply be legislated or simulated into existence beforehand.
% FOUNDING_PROBLEM_CORROBORATION: High-reliability-organization researchers outside the beneficiary set (academics studying naval aviation and nuclear operations) find mixed evidence: some document genuine post-disaster competence gains, others document simulation and near-miss review producing comparable gains without requiring casualties. No source entirely outside the professional communities that benefit from disaster-derived authority has corroborated that catastrophe is strictly necessary rather than merely sufficient or convenient.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, contested).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is authored as moderate-high: the doctrine transfers status, funding, and regulatory leverage toward disaster-experienced actors while normalizing harm to frontline workers and communities as pedagogically necessary. Suppression (0.42) is moderate rather than severe — the doctrine is enforced less by coercion than by professional culture and institutional memory that discounts simulation-based dissent; there is no single enforcer, but veteran-operator hierarchies and post-disaster regulatory reform cycles make dissenting voices structurally hard to credit. Theater ratio (0.31) reflects that a real coordination function exists (genuine organizational learning does occur post-disaster) alongside performative invocation of disaster-survivor authority that exceeds what the learning function requires. Accessibility collapse (0.4) is moderate: alternative training paradigms (simulation, near-miss review) are visible and practiced in parallel, they are simply subordinated in status and funding, not eliminated. Resistance (0.55) is substantial because simulation vendors, some safety researchers, and reform-minded regulators actively contest the doctrine's premise.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (regulators, veteran operators, investigators), the doctrine reads as a hard-won epistemic truth about how organizations actually learn — an unglamorous acknowledgment of human and institutional limits. From the payer seats (frontline workers, communities), the same doctrine reads as a structure that pre-authorizes catastrophe as an acceptable, even necessary, cost of maintaining competence, converting their bodies and neighborhoods into the tuition the institution pays to itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Veteran operators, investigators, and post-disaster regulators are declared beneficiaries because the doctrine's truth-claim directly underwrites their authority, funding, and mandate expansion — their directionality sits toward the beneficiary end. Frontline operators between incidents and nearby communities are declared victims: they bear the anxiety and physical risk the doctrine treats as instrumentally necessary, with trapped/powerless positioning driving directionality toward the full-target end. Injured or killed workers are the starkest case — the doctrine's own logic requires their harm as the substrate of 'genuine' learning, which is why they are listed separately from the general frontline population rather than folded into it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (explaining why post-disaster procedures outperform pre-disaster ones) may remain partly live — organizational amnesia during quiet periods is a real phenomenon documented in HRO literature — but the doctrine's strong claim (that ONLY real catastrophe suffices, that simulation is categorically insufficient) outruns the evidence needed to solve that narrower problem. Treating the founding problem as fully live licenses continued deference to disaster-survivor authority and continued subordination of simulation investment, which is precisely the divergence the classification is designed to surface: a genuine coordination function (real learning happens) has hardened into a structure that also extracts status and legitimizes future harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_catastrophe_vs_siblings,
    'Is real catastrophic experience actually necessary for competence retention (this reading), or do near-misses (near_miss_as_bridge) or high-fidelity simulation (simulation_as_sufficient) suffice?',
    'Comparative longitudinal study across high-reliability organizations with varying incident histories and varying investment in simulation/near-miss review, controlling for baseline hazard rate, to see whether measured competence retention tracks disaster exposure specifically or tracks total structured learning investment regardless of source.',
    'If simulation or near-miss review demonstrably produces equivalent competence retention, the catastrophe_as_necessary reading loses its foundational premise and the doctrine''s extraction of status/funding toward disaster-experienced actors becomes harder to justify as coordination rather than rent-seeking on past harm. If catastrophe genuinely proves necessary, the doctrine''s harsher implications (that some harm is functionally required) become a harder ethical problem rather than a false claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_catastrophe_vs_siblings, empirical, 'The core contested claim distinguishing this reading from its siblings within the same kernel.').

omega_variable(
    disaster_derived_authority_incentive,
    'Do the professional and institutional beneficiaries of this doctrine (veteran operators, investigators, regulators) have an incentive to overstate catastrophe''s necessity relative to cheaper alternatives, independent of the doctrine''s actual truth?',
    'Track whether organizations that experience a real disaster subsequently increase (rather than decrease) investment in simulation and near-miss review, which would suggest disaster experience is treated internally as complementary evidence rather than as the exclusively necessary source of learning claimed by the doctrine.',
    'If disaster-experienced organizations pivot toward more simulation investment post-event, that undercuts the strong ''only catastrophe teaches'' claim and supports reading the doctrine partly as status-protective narrative for those whose authority derives from having survived a real event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disaster_derived_authority_incentive, conceptual, 'Whether beneficiary incentive structures bias the doctrine''s truth-claim.').

omega_variable(
    harm_conversion_ethics,
    'Is it defensible, even if empirically true that catastrophe teaches more than simulation, to treat that catastrophe as institutionally ''necessary'' rather than as tragic and to be minimized regardless of pedagogical value?',
    'This is not resolvable by further data alone; it depends on whether organizational learning value can ever justify treating human injury as an acceptable input to a training pipeline, which is a values question about acceptable tradeoffs.',
    'Affects whether the doctrine, even if empirically well-supported, should be treated as an acceptable operating premise or as a normatively unacceptable framing that ought to be actively resisted regardless of its empirical learning value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_conversion_ethics, preference, 'Normative question about whether pedagogical value can justify treating real harm as necessary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 8, 0.21).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 16, 0.24).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 24, 0.27).
narrative_ontology:measurement(comp_tr_t32, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 32, 0.29).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(comp_be_t32, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(comp_su_t32, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__catastrophe_as_necessary, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the competence_retention_exercise kernel, each authored as an independent, ε-invariant constraint. catastrophe_as_necessary (this file) claims real catastrophic experience is strictly required and logically forecloses simulation_as_sufficient's claim that high-fidelity simulation is structurally equivalent to catastrophic experience — the two premises cannot both hold within a single organizational training framework, since one asserts categorical insufficiency of simulation and the other asserts structural equivalence. This reading coexists with near_miss_as_bridge, since near-misses are a form of partial real-world exposure that this reading's proponents can accommodate as evidence of decay-risk without abandoning the core claim that full catastrophe is what ultimately resets competence. Each sibling should link back to this constraint_id in its own network.affects_constraints array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
