% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__hybrid_dependency, []).

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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Hybrid Simulation-Plus-Line-Exposure Competence Standard
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In commercial aviation and other high-reliability domains, competence
 *   maintenance has settled into a hybrid regime: simulator training provides
 *   the repeatable, dangerous, and rare-failure-mode practice that cannot
 *   ethically or economically be induced in real operations, while periodic
 *   line operations, non-jeopardy audits, and actual aircraft time provide
 *   the real-world anchoring that prevents skill and judgment from drifting
 *   into a simulator-only equilibrium detached from operational reality. This
 *   story instantiates the hybrid_dependency reading of the
 *   competence_exercise_requirement kernel: neither pure simulation nor
 *   catastrophe-as-training is adequate on its own. The sibling readings —
 *   simulation_as_adequate_exercise (which holds simulator fidelity plus
 *   debriefing suffices) and catastrophe_as_necessary_anchor (which holds
 *   only real catastrophic events provide irreducible exercise) — are
 *   separate constraints with their own ε values and stakeholder structures;
 *   this file does not average across them or describe their contest
 *   internally.
 *
 * KEY AGENTS:
 *   - flight_crews: primary payers of the hybrid training burden, also primary beneficiaries of genuine competence (moderate power, constrained exit)
 *   - junior_first_officers and reserve_crew_pools: bear disproportionate scheduling and career-leverage costs of real-world anchoring (powerless, trapped)
 *   - airline_operators: administer and fund the standard, balancing direct cost against tail-risk (institutional, constrained)
 *   - regulators: set minimum hybrid ratios from accident-investigation evidence (institutional, analytical)
 *   - passengers_and_public: ultimate beneficiaries of anchored competence, with no visibility into the training design (powerless, trapped)
 *   - simulator_training_vendors: commercially interested in the simulation side of the hybrid balance (organized, mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.28).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.35).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.28).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Simulation-Plus-Line-Exposure Competence Standard").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '5e11eb09-e13b-46ba-9449-9303f757ca38').
narrative_ontology:cs_kernel_codification('5e11eb09-e13b-46ba-9449-9303f757ca38', distributed).
narrative_ontology:cs_authority_grounding('5e11eb09-e13b-46ba-9449-9303f757ca38', expertise).
narrative_ontology:cs_interpretation_layer_present('5e11eb09-e13b-46ba-9449-9303f757ca38').
narrative_ontology:cs_reading_relation('5e11eb09-e13b-46ba-9449-9303f757ca38', competence_exercise_requirement__simulation_as_adequate_exercise, coexists_with).
narrative_ontology:cs_reading_relation('5e11eb09-e13b-46ba-9449-9303f757ca38', competence_exercise_requirement__catastrophe_as_necessary_anchor, influences).
narrative_ontology:cs_axiom('5e11eb09-e13b-46ba-9449-9303f757ca38', foundational, simulation_alone_produces_fragile_equilibrium).
narrative_ontology:cs_axiom_status(simulation_alone_produces_fragile_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('5e11eb09-e13b-46ba-9449-9303f757ca38', simulation_alone_produces_fragile_equilibrium, empirically_contingent).
narrative_ontology:cs_axiom('5e11eb09-e13b-46ba-9449-9303f757ca38', foundational, non_jeopardy_anchoring_can_substitute_for_catastrophe).
narrative_ontology:cs_axiom_status(non_jeopardy_anchoring_can_substitute_for_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('5e11eb09-e13b-46ba-9449-9303f757ca38', non_jeopardy_anchoring_can_substitute_for_catastrophe, instrumental).
narrative_ontology:cs_reference_frame('5e11eb09-e13b-46ba-9449-9303f757ca38', crm_era_hybrid_training_consensus).
narrative_ontology:cs_drift_state('5e11eb09-e13b-46ba-9449-9303f757ca38', contemporary_high_fidelity_simulation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('5e11eb09-e13b-46ba-9449-9303f757ca38', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, passengers_and_public).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, flight_crews).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, airline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, regulators).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, junior_first_officers).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, reserve_crew_pools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, simulator_training_vendors).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, flight_crews).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, airline_operators).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, high_reliability_organization_theory).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, simulation_insufficiency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must accumulate simulator hours AND scheduled line legs AND periodic non-jeopardy audits to keep currency. The hybrid burden costs personal time, rest, and scheduling flexibility, but the same requirement is what keeps their skills genuinely current rather than performatively current — they are both the ones who bear the training load and the ones whose survival depends on the anchoring working.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, flight_crews, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, flight_crews, beneficiary).

% Have the least accumulated real-aircraft time and the weakest bargaining position to refuse additional line-exposure requirements or protest scheduling that concentrates real-world anchoring hours on them disproportionately. Career advancement depends on completing the hybrid regime without complaint; leaving the industry means abandoning years of sunk training investment.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, junior_first_officers, payer,
    powerless, biographical, trapped, national).

% Absorb the unpredictable scheduling churn created by rotating crews through real aircraft time and non-jeopardy audits — they are called up on short notice specifically to fill the anchoring requirement's operational gaps, with little control over when or how often.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, reserve_crew_pools, payer,
    powerless, immediate, trapped, national).

% Design and fund the hybrid training regime, balancing the direct cost of real aircraft time and audit programs against the catastrophic tail-risk cost of undertrained crews. They set audit schedules, line-exposure minimums, and simulator curricula, and could in principle cut real-world anchoring to save cost — the same seat both administers the constraint and bears its budget.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, airline_operators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, airline_operators, payer).

% Mandate minimum ratios of simulator-to-line-hours and non-jeopardy audit frequency, informed by accident investigation history. They do not fly the aircraft or pay the direct training cost, but their credibility depends on the standard actually preventing the failure modes it claims to address.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regulators, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, regulators, observer).

% Receive the safety benefit of crews whose competence has been anchored against real-world drift, without visibility into how the training regime is actually structured or whether it is being maintained honestly. They cannot audit the standard themselves and depend entirely on the regulator-operator relationship functioning.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, passengers_and_public, beneficiary,
    powerless, immediate, trapped, national).

% Sell simulator hours and fidelity upgrades to airlines; benefit whenever the hybrid standard weights toward more simulation, and have a commercial interest in arguments that simulation fidelity can substitute for real aircraft time — a pressure this reading explicitly resists by requiring the real-world anchor regardless of simulator quality.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulator_training_vendors, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__hybrid_dependency, diffuse).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__hybrid_dependency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that simulator training alone drifts from real-world conditions over time — automation surprises, degraded sensory cues, atypical failure combinations, and the psychological weight of real consequence are difficult to fully replicate — while pure real-world exercise of rare catastrophic failure modes is neither ethical nor practical to induce deliberately. The hybrid regime coordinates a training curriculum that uses simulation for repeatable, dangerous, and rare-failure practice, and periodic line operations plus non-jeopardy audits to anchor that practice against real operational drift.
% TRANSFER_FUNCTION: Moves training time, scheduling flexibility, and audit-compliance burden from airline operators and regulators onto flight crews (especially junior and reserve crews), in exchange for a safety margin that flows primarily to passengers and the public. Airlines also transfer some direct cost to simulator vendors in exchange for repeatable, low-risk practice capacity.
% ABSENT_VOICES: Junior first officers and reserve pools bear a disproportionate share of the scheduling churn created by real-world anchoring requirements but have limited voice in setting audit frequency or line-hour minimums, which are negotiated primarily between regulators, unions, and senior operations management.
% DISAPPEARANCE_RATIONALE: If the real-world anchoring requirement (line operations, non-jeopardy audits, actual aircraft time) disappeared and only simulation remained, crew competence would drift silently — automation complacency, atrophied manual flying skill under real sensory load, and untested judgment under authentic operational pressure would accumulate undetected until an incident surfaced the gap. Training budgets would shift heavily toward simulator infrastructure, scheduling would simplify, and the accident-investigation record that currently corroborates the hybrid standard's necessity would eventually re-generate the same requirement from a fresh catastrophe.
% FOUNDING_PROBLEM: Early flight training and later CRM-era safety analysis found that simulator-only currency produced crews who performed well on tested simulator scenarios but exhibited skill and judgment gaps in real operations that simulators did not or could not replicate — automation surprise, degraded real-world cueing, and the psychological difference between simulated and real consequence.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards (independent of airline operators) have repeatedly identified simulator-real-world skill gaps as contributing factors in specific incidents, and academic human-factors researchers outside the airline industry have documented the phenomenon in peer-reviewed studies of manual flying skill decay under high-automation regimes. This corroboration comes from investigative and academic bodies structurally separate from the airlines and regulators who administer the training standard.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__hybrid_dependency_tests).
:- end_tests(competence_exercise_requirement__hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is modest (0.28) because the hybrid regime's costs — scheduling burden, training time, audit compliance — are real but proportionate to a genuine, well-corroborated safety function; the constraint is not primarily transferring value to an unaccountable party but funding a coordination good that flows substantially back to the crews and public who pay for it. Suppression is present but moderate (0.35): junior crews and reserve pools have limited ability to renegotiate anchoring requirements, and the regime is actively enforced through regulatory audit rather than voluntary compliance, but genuine alternatives (advocacy through unions, regulatory comment processes) are not fully suppressed. Theater ratio is low-to-moderate and rises slowly (0.10 to 0.22) reflecting a realistic concern: as simulator fidelity improves, there is chronic pressure to let real-world anchoring requirements become checkbox exercises (a token line leg, a scheduled non-jeopardy audit that has lost its non-jeopardy character in practice) rather than genuine skill-anchoring events — this is the drift this reading exists to resist, not a claim that it has already won.
 *
 * DIRECTIONALITY LOGIC:
 *   Flight crews sit near symmetric: they pay the direct time and scheduling cost of the hybrid regime but are also the direct beneficiaries of the competence it maintains, since their own survival depends on it. Junior first officers and reserve crew pools are pushed further toward the target end because their trapped exit options and weak bargaining position mean they absorb a disproportionate share of the anchoring burden without proportionate say in how it is structured. Airline operators and regulators are agenda-setters — they design and fund the standard rather than being extracted from by it, though operators also pay real direct costs. Passengers are beneficiaries with essentially total dependence and zero visibility, which is why the constraint's legitimacy rests on regulator-operator good faith rather than passenger-side verification. Simulator vendors are a minor beneficiary group whose commercial interest actually pulls against this reading's core claim (that simulation is insufficient alone) — that tension is real and worth flagging rather than smoothing over.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (simulator-only training produces undetected real-world skill drift) remains live, corroborated by accident investigation boards and academic human-factors research structurally independent of the airlines and regulators who administer the standard. This is not mandatrophy: the mandate has not outlived its function. The chronic risk this reading names explicitly is a slow degradation of the real-world anchoring component into theater (rising theater_ratio) as simulator fidelity improves and cost pressure pushes operators toward simulation substitution — but the current measured state (0.22 at t=40) shows meaningful but not yet dominant performative drift, distinguishing this from a constraint that has already collapsed into pure ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anchoring_dose_sufficiency,
    'What minimum quantity and character of real-world anchoring (line hours, audit frequency, non-jeopardy exposure) is actually sufficient to prevent simulator-only skill drift, versus what quantity is currently mandated for institutional-legitimacy reasons rather than demonstrated necessity?',
    'Longitudinal skill-decay studies comparing crews under varying real-world anchoring doses against incident and near-miss rates, controlled for simulator fidelity and route complexity.',
    'If current anchoring requirements substantially exceed the empirically necessary dose, the excess burden on junior crews and reserve pools would be reclassified as extraction cover rather than genuine coordination cost, pushing this constraint toward tangled_rope. If anchoring requirements are already near the minimum necessary dose, the rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anchoring_dose_sufficiency, empirical, 'Whether the mandated real-world anchoring dose matches the empirically necessary dose or exceeds it for institutional-legitimacy reasons.').

omega_variable(
    hybrid_kernel_reading_selection,
    'Is the hybrid_dependency reading the structurally correct account of the competence_exercise_requirement kernel, or do the sibling readings (simulation_as_adequate_exercise, catastrophe_as_necessary_anchor) better describe how competence is actually maintained in some high-reliability domains?',
    'Cross-domain comparison: examine whether domains that have moved toward pure simulation (e.g., certain military simulation-heavy training pipelines) or that structurally rely on catastrophe/near-miss review (e.g., nuclear near-miss analysis) show different competence-drift outcomes than aviation''s hybrid regime.',
    'If cross-domain evidence shows simulation-only regimes maintain competence as well as hybrid regimes, this reading''s core premise weakens and the sibling simulation_as_adequate_exercise reading gains support, with implications for which constraint should be treated as the dominant structural account in aviation specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_kernel_reading_selection, conceptual, 'Whether hybrid_dependency, versus its sibling readings, is the structurally correct account of how this competence kernel is actually maintained across high-reliability domains.').

omega_variable(
    junior_crew_burden_distribution,
    'Is the disproportionate anchoring burden placed on junior first officers and reserve crew pools a necessary feature of the hybrid regime (they need more anchoring because they have less banked experience) or an artifact of their weak bargaining position being used to absorb scheduling costs that could be more evenly distributed?',
    'Compare anchoring-hour distribution across seniority tiers against actual skill-decay risk profiles by seniority; if junior crews receive anchoring hours proportionate to elevated decay risk, the distribution is functional; if the excess tracks bargaining power rather than risk, it is extractive.',
    'A finding of bargaining-power-driven distribution would support reclassifying the burden on junior_first_officers and reserve_crew_pools specifically as a victim relationship within an otherwise legitimate coordination structure, consistent with a tangled_rope reading localized to those seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(junior_crew_burden_distribution, empirical, 'Whether disproportionate anchoring burden on junior and reserve crews tracks genuine skill-decay risk or exploitable weak bargaining position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__hybrid_dependency, theater_ratio, 8, 0.13).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__hybrid_dependency, theater_ratio, 16, 0.16).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__hybrid_dependency, theater_ratio, 24, 0.18).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__hybrid_dependency, theater_ratio, 32, 0.2).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__hybrid_dependency, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 16, 0.23).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 32, 0.34).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__hybrid_dependency, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, catastrophe_as_necessary_anchor).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the competence_exercise_requirement kernel. hybrid_dependency (this file) claims neither simulation alone nor catastrophe-as-anchor alone is sufficient; simulation_as_adequate_exercise claims high-fidelity simulation plus debriefing suffices on its own; catastrophe_as_necessary_anchor claims only real catastrophic events provide irreducible exercise. Each reading has its own ε, beneficiary/victim structure, and classification — they are not the same constraint measured differently, but three structurally distinct claims about what maintains competence, linked here for contamination-propagation analysis (a purity shift in one reading's supporting evidence base plausibly affects confidence in the others).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
