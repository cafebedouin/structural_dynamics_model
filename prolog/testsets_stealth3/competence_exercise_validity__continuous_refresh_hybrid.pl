% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__continuous_refresh_hybrid, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Continuous Drill-Cycle Mandate for Safety-Critical Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Across nuclear, aviation, chemical, and healthcare operations, licenses
 *   and coverage terms condition continued operation on a recurring cycle of
 *   evaluated drills: crews must repeatedly rehearse rare, high-consequence
 *   scenarios rather than validate competence once and stand down. This story
 *   instantiates ONE reading of the contested kernel
 *   competence_exercise_validity — the continuous_refresh_hybrid reading,
 *   which holds that simulation is necessary but not sufficient and that
 *   retention is process-dependent, requiring continuous drill cycles rather
 *   than one-time state validation. The sibling readings
 *   (simulation_as_proxy, real_catastrophe_only) are separate constraints in
 *   separate files and are not averaged into this one. Epsilon's referent is
 *   the standing arrangement under contest — the mandated continuous-exercise
 *   regime as it actually operates — assessed by this reading's own lights:
 *   the reading endorses the cycle's necessity while acknowledging that
 *   mandated volume exceeds the evidence-based minimum and that a compliance
 *   industry rides the requirement. KEY AGENTS (by structural relationship):
 *   - safety_regulators: agenda-setting seat (institutional/constrained) —
 *   writes drill requirements into licenses, audits records; oversight
 *   workload justifies its budget - safety_critical_operators: primary paying
 *   seat (powerful/constrained) — funds and staffs the cycle; retains the
 *   competence and avoids the catastrophe - frontline_crews:
 *   paying-and-protected seat (organized/constrained) — supplies rehearsal
 *   hours and evaluation exposure; owns the competence that keeps them alive
 *   - training_simulation_vendors: collecting seat (organized/arbitrage) —
 *   sells scenario design and drill delivery into guaranteed mandated demand
 *   - reinsurers: co-enforcing beneficiary seat (institutional/mobile) —
 *   prices catastrophe risk and writes drill records into coverage terms -
 *   general_public_affected_communities: protected beneficiary seat
 *   (powerless/trapped) — carries residual local risk and indirect drill
 *   overhead in prices - small_operator_representatives: excluded seat
 *   (moderate/constrained) — would press for size-tiered requirements; absent
 *   from standard-setting tables - human_factors_research_community:
 *   analytical seat (moderate/analytical) — produces the skill-decay evidence
 *   the mandate's legitimacy rests on
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.45).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.51).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.45).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Continuous Drill-Cycle Mandate for Safety-Critical Competence").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, '1bc03741-682f-415c-a31b-91e125495309').
narrative_ontology:cs_kernel_codification('1bc03741-682f-415c-a31b-91e125495309', formalized).
narrative_ontology:cs_authority_grounding('1bc03741-682f-415c-a31b-91e125495309', expertise).
narrative_ontology:cs_interpretation_layer_present('1bc03741-682f-415c-a31b-91e125495309').
narrative_ontology:cs_reading_relation('1bc03741-682f-415c-a31b-91e125495309', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('1bc03741-682f-415c-a31b-91e125495309', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('1bc03741-682f-415c-a31b-91e125495309', foundational, competence_retention_requires_continuous_cycles).
narrative_ontology:cs_axiom_status(competence_retention_requires_continuous_cycles, holdable).
narrative_ontology:cs_axiom_grounding('1bc03741-682f-415c-a31b-91e125495309', competence_retention_requires_continuous_cycles, empirically_contingent).
narrative_ontology:cs_axiom('1bc03741-682f-415c-a31b-91e125495309', secondary, one_time_state_validation_insufficient_for_readiness).
narrative_ontology:cs_axiom_status(one_time_state_validation_insufficient_for_readiness, holdable).
narrative_ontology:cs_axiom_grounding('1bc03741-682f-415c-a31b-91e125495309', one_time_state_validation_insufficient_for_readiness, empirically_contingent).
narrative_ontology:cs_reference_frame('1bc03741-682f-415c-a31b-91e125495309', continuously_exercised_competence_baseline).
narrative_ontology:cs_drift_state('1bc03741-682f-415c-a31b-91e125495309', contemporary_sim_credit_expansion, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('1bc03741-682f-415c-a31b-91e125495309', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, general_public_affected_communities).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, training_simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, reinsurers).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, safety_critical_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, frontline_crews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, safety_critical_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, frontline_crews).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, skill_decay_without_practice).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, team_coordination_degrades_between_exercises).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, process_dependent_competence_retention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes minimum drill frequencies, scenario fidelity requirements, and pass criteria into operating licenses; audits drill records and can issue findings that suspend authority to operate. Its inspection staffing and technical budget scale with the volume of mandated exercise it must oversee.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators, beneficiary).

% Runs plants, fleets, and facilities whose licenses condition on demonstrated drill performance. It schedules crews out of production for rehearsals, buys simulator capacity, and remediates failed objectives. Skipping the cycle risks license action and uninsured liability; running it consumes a permanent operating budget line.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_critical_operators, payer,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, safety_critical_operators, beneficiary).

% Spends recurring duty hours in scenario rehearsal and evaluated drills, including no-notice events that interrupt rest patterns. Drill results gate individual qualification and advancement. The same rehearsal builds the team coordination they would depend on in a real event.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, frontline_crews, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, frontline_crews, beneficiary).

% Designs scenarios, builds and operates simulators, and delivers evaluated drill services under multi-year contracts. Demand is set by mandated drill hours rather than discretionary purchases, so revenue tracks the requirement's volume and fidelity thresholds.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, training_simulation_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Prices coverage for low-frequency, high-severity events and conditions terms on documented drill performance. It commissions its own audits of operator drill records and adjusts premiums or deductibles when cycles lapse.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, reinsurers, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, reinsurers, agenda_setter).

% Lives and works near plants, transport corridors, and facilities whose worst-case events would land locally. It bears drill program costs indirectly through rates and fares and holds no seat in setting the requirements that protect it.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, general_public_affected_communities, beneficiary,
    powerless, generational, trapped, local).

% Represents operators for whom full-fidelity drill cycles are a fixed cost that scales poorly with fleet or plant size. It petitions for tiered requirements and expanded simulator credit and is routinely outvoted in the working groups where frequency standards are drafted.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, small_operator_representatives, excluded,
    moderate, biographical, constrained, national).

% Studies skill decay, team performance under stress, and transfer from simulator to field. Its findings are cited by every other seat, but it holds no vote in standard setting and receives no mandate revenue.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, human_factors_research_community, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__continuous_refresh_hybrid, training_simulation_vendors).
narrative_ontology:fixing_cost_class(competence_exercise_validity__continuous_refresh_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts individually certified skill into continuously rehearsed team capability across a distributed population of safety-critical operations. It solves the shared problem that competence decays silently between rare events and that no single operator captures the full system-level benefit of everyone staying practiced.
% TRANSFER_FUNCTION: Moves operating budgets and crew duty time into recurring scenario design, simulator capacity, and evaluated drills; moves pass/fail assurance data upward to regulators and insurers; moves contracted drill-delivery revenue to training vendors.
% ABSENT_VOICES: Small operators facing fixed-cost drill burdens and crews who experience scheduled drills as evaluation ritual are outside the working groups where drill frequency and fidelity standards are set; those tables are populated by large operators, regulators, and vendors whose interests align on high mandated volume.
% DISAPPEARANCE_RATIONALE: Training calendars, simulator procurement, vendor contracts, insurance premium structures, and regulatory audit programs are all built around mandated drill cycles. Overnight removal would strand simulator assets, void coverage terms keyed to drill records, and leave competence to decay on the skill-fade timescale until the next severe event forced reorganization.
% FOUNDING_PROBLEM: Post-accident investigations in the late 1970s and 1980s found crews and control rooms that were fully certified on paper yet failed basic responses under stress. The gap was traced to skill fade between infrequent validations, and the founding fix was to make exercise continuous rather than episodic.
% FOUNDING_PROBLEM_CORROBORATION: Peer-reviewed skill-decay research and accident-board findings documenting degraded crew response after long drill gaps attest the problem from outside the benefiting parties. Vendor and regulator attestations that the problem is live are discounted as self-interested; the academic and investigative sources carry the corroboration.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__continuous_refresh_hybrid, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).
:- end_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.45: the cycle imposes a permanent recurring cost line on operators and crews, partially reciprocated by retained capability — by this reading's lights the core is justified but mandated volume runs above the evidence-based minimum, and that surplus is the extractive share. Suppression 0.51: persistence depends on active license-condition enforcement, ratcheted hard after the TMI- and Challenger-era findings and roughly stable since; suppression is authored as a raw structural property — the engine scales only extractiveness, by directionality and scope. Theater_ratio 0.32: scheduled, pre-briefed drills increasingly measure compliance rather than exercise degraded conditions, but no-notice audits retain real function. Accessibility_collapse 0.55: once skill-fade evidence is understood, validate-once alternatives lose standing, but the sibling readings keep alternatives live as argued positions. Resistance 0.55: operators push back on volume and scheduling, some crews on evaluation burden, while insurers and publics push for more. The measurement series runs on one shared nine-point grid (1979–2019) so every tracked metric is authored at every examined time point: suppression_requirement traces the enforcement ratchet (0.30 to 0.51), base_extractiveness traces compliance-volume growth (0.24 to 0.45), theater_ratio traces scripted-compliance creep (0.12 to 0.32). Claim/metric independence: claimed_type tangled_rope is asserted from structure — a genuine coordination function plus identifiable payers and collectors through the same enforced arrangement — while the metrics are authored independently of that claim.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and vendor seats the cycle is functioning assurance infrastructure worth defending and extending; from the operator and crew seats the same cycle is a recurring tax of time and budget whose volume is set by parties that do not bear it. The public seat experiences only the protected side. Crews hold coalition capacity through unions and professional bodies — the reason resistance registers at all despite dispersed individual stakes. Small operators meet a fixed-cost wall that the large-operator-dominated standards table does not price, which is why their representatives sit outside the room.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations plus exit atoms reproduce the relationships without overrides: vendors and reinsurers collect from mandated volume with mobile or arbitrage exits — nearest the beneficiary end; regulators administer the arrangement and their budgets scale with oversight load — beneficiary-side despite the agenda-setter role; operators and crews bear the recurring cost under constrained exit — target-side, with crews landing mid-range because their own competence and survival sit inside the arrangement (dual payer/beneficiary positioning); affected publics are subsidized beneficiaries with trapped exit — the strongest damping. Scope amplification applies modestly at the regime's national-to-continental reach.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — certified-but-unpracticed crews failing under stress — is corroborated as live by skill-decay research and accident-board findings from outside the benefiting parties, so the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and raises no zombie flag. The mandatrophy risk here is not a dead mandate but dose inflation: the temporal series is positioned to catch the case where theater passes 0.5 and extraction keeps climbing while the founding problem stays live — the signature of an exercise function drifting into performance maintenance. If dose-response evidence later showed mandated volume far exceeding the retention minimum, the piton-direction hypothesis would activate for investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexical_delta,
    'This constraint is the continuous_refresh_hybrid reading of the competence_exercise_validity kernel; what structural differences would the sibling readings introduce if adopted as the operative standard?',
    'Adoption events: a regulator granting unlimited simulator credit would instantiate simulation_as_proxy (mandated live-drill hours collapse toward the coordination floor and epsilon falls); a post-accident shift to event-driven-only validation would instantiate real_catastrophe_only (the preventive regimen is condemned as insufficient and regulation reorganizes around incident learning).',
    'Sibling adoption changes the victim set (who must drill), the beneficiary set (vendor revenue mix), and epsilon materially. This file''s epsilon is indexed to the hybrid reading only; averaging across readings would violate epsilon invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexical_delta, conceptual, 'Kernel-reading indexicality: epsilon and structure are reading-relative over the shared kernel.').

omega_variable(
    mandated_dose_vs_evidence_based_minimum,
    'How far does mandated drill volume exceed the dose-response minimum that skill-decay research identifies as sufficient for retention?',
    'Meta-analyses of skill-decay dose-response curves and controlled comparison of jurisdictions operating reduced-schedule regimes with equivalent outcome metrics.',
    'The gap between mandated and minimal dose is the extractive share riding the coordination function. If mandated volume approximates the minimum, the arrangement trends toward pure coordination; if it runs well above, the extraction share grows and the payer seats'' effective burden rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandated_dose_vs_evidence_based_minimum, empirical, 'Whether the mandate''s volume tracks retention science or compliance politics.').

omega_variable(
    drill_theater_fraction,
    'What share of evaluated drill activity measures compliance rather than exercising crews under degraded, ambiguous, or no-notice conditions?',
    'Comparative studies of no-notice audit drills versus scheduled announced drills, measuring performance deltas and objective-completion rates.',
    'A theater share above roughly 0.5 would signal that the exercise function is drifting into performance maintenance, activating the piton-direction hypothesis for the exercise regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_theater_fraction, empirical, 'Functional-versus-performative composition of mandated drill activity.').

omega_variable(
    safety_record_attribution,
    'Does the improving safety record attribute to continuous exercise cycles, or to concurrently deployed engineering redundancy, process-safety management, and design changes?',
    'Multivariate accident-rate analyses conditioning on engineering and design upgrades, isolating the marginal contribution of drill-cycle intensity.',
    'Weak drill attribution undermines this reading''s vindication claim and the mandate''s legitimacy basis; strong attribution supports both the reading and the current mandate intensity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_record_attribution, empirical, 'Causal weight of drill cycles in the recorded safety improvement.').

omega_variable(
    standard_setting_authority_basis,
    'Does standard-setting authority rest on demonstrated predictive validity of drill regimens, or on institutional self-perpetuation of the compliance apparatus?',
    'Track whether frequency and fidelity standards actually revise when dose-response evidence contradicts mandated volumes; revision responsiveness distinguishes expertise-grounded from self-perpetuating authority.',
    'If authority is self-perpetuating, the assurance layer''s role reclassifies toward collection and the operator seats'' effective burden rises; if evidence-responsive, the expertise grounding holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standard_setting_authority_basis, conceptual, 'Whether the commitment system''s authority is evidence-wielding or self-justifying.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 1979, 2019).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1979, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 1979, 0.12).
narrative_ontology:measurement_basis(comp_tr_t1979, observed).
narrative_ontology:measurement(comp_tr_t1984, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 1984, 0.16).
narrative_ontology:measurement_basis(comp_tr_t1984, observed).
narrative_ontology:measurement(comp_tr_t1989, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 1989, 0.2).
narrative_ontology:measurement_basis(comp_tr_t1989, observed).
narrative_ontology:measurement(comp_tr_t1994, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 1994, 0.23).
narrative_ontology:measurement_basis(comp_tr_t1994, observed).
narrative_ontology:measurement(comp_tr_t1999, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 1999, 0.26).
narrative_ontology:measurement_basis(comp_tr_t1999, observed).
narrative_ontology:measurement(comp_tr_t2004, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 2004, 0.28).
narrative_ontology:measurement_basis(comp_tr_t2004, observed).
narrative_ontology:measurement(comp_tr_t2009, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 2009, 0.3).
narrative_ontology:measurement_basis(comp_tr_t2009, observed).
narrative_ontology:measurement(comp_tr_t2014, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 2014, 0.31).
narrative_ontology:measurement_basis(comp_tr_t2014, observed).
narrative_ontology:measurement(comp_tr_t2019, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 2019, 0.32).
narrative_ontology:measurement_basis(comp_tr_t2019, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t1979, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 1979, 0.24).
narrative_ontology:measurement_basis(comp_be_t1979, observed).
narrative_ontology:measurement(comp_be_t1984, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 1984, 0.29).
narrative_ontology:measurement_basis(comp_be_t1984, observed).
narrative_ontology:measurement(comp_be_t1989, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 1989, 0.33).
narrative_ontology:measurement_basis(comp_be_t1989, observed).
narrative_ontology:measurement(comp_be_t1994, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 1994, 0.36).
narrative_ontology:measurement_basis(comp_be_t1994, observed).
narrative_ontology:measurement(comp_be_t1999, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 1999, 0.38).
narrative_ontology:measurement_basis(comp_be_t1999, observed).
narrative_ontology:measurement(comp_be_t2004, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 2004, 0.4).
narrative_ontology:measurement_basis(comp_be_t2004, observed).
narrative_ontology:measurement(comp_be_t2009, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 2009, 0.42).
narrative_ontology:measurement_basis(comp_be_t2009, observed).
narrative_ontology:measurement(comp_be_t2014, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 2014, 0.44).
narrative_ontology:measurement_basis(comp_be_t2014, observed).
narrative_ontology:measurement(comp_be_t2019, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 2019, 0.45).
narrative_ontology:measurement_basis(comp_be_t2019, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1979, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 1979, 0.3).
narrative_ontology:measurement_basis(comp_su_t1979, observed).
narrative_ontology:measurement(comp_su_t1984, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 1984, 0.38).
narrative_ontology:measurement_basis(comp_su_t1984, observed).
narrative_ontology:measurement(comp_su_t1989, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 1989, 0.44).
narrative_ontology:measurement_basis(comp_su_t1989, observed).
narrative_ontology:measurement(comp_su_t1994, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 1994, 0.47).
narrative_ontology:measurement_basis(comp_su_t1994, observed).
narrative_ontology:measurement(comp_su_t1999, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 1999, 0.48).
narrative_ontology:measurement_basis(comp_su_t1999, observed).
narrative_ontology:measurement(comp_su_t2004, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 2004, 0.49).
narrative_ontology:measurement_basis(comp_su_t2004, observed).
narrative_ontology:measurement(comp_su_t2009, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 2009, 0.5).
narrative_ontology:measurement_basis(comp_su_t2009, observed).
narrative_ontology:measurement(comp_su_t2014, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 2014, 0.5).
narrative_ontology:measurement_basis(comp_su_t2014, observed).
narrative_ontology:measurement(comp_su_t2019, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 2019, 0.51).
narrative_ontology:measurement_basis(comp_su_t2019, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity__real_catastrophe_only).

% DUAL FORMULATION NOTE:
% The colloquial label 'exercise keeps us safe' conflates three structurally distinct claims about what validates competence: continuous-cycle sufficiency (this file), simulation sufficiency (competence_exercise_validity__simulation_as_proxy), and catastrophe-only validity (competence_exercise_validity__real_catastrophe_only). Each carries its own epsilon, victim set, and enforcement economics; they form a constraint family linked through network edges rather than one observable-dependent story. The upstream empirical claim (skill decay without practice) is cited as evidence by all three readings, which is why the family links run through it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
