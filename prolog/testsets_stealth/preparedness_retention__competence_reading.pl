% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading of the Exercise-and-Inspection Regime)
 *   domain: governance/institutional-memory/disaster-preparedness
 *
 * SUMMARY:
 *   A national flood-preparedness regime — mandated multi-agency exercises,
 *   scheduled dike and pumping-station inspections, after-action review —
 *   assessed here from the seat that claims it works as designed: drills and
 *   inspections are competence-preserving practices that maintain live
 *   operational capacity. The epsilon referent is the standing
 *   exercise-and-inspection arrangement itself, assessed by this reading's
 *   own lights: the regime's costs are real but are converted into maintained
 *   capacity rather than collected by any seat. This file is one reading of
 *   the preparedness_retention kernel and authors only that reading's
 *   constraint; the sibling readings are separate constraint files linked
 *   through network.affects_constraints, and the family decomposition is
 *   recorded in the dual-formulation note. The claim and the metrics are
 *   authored independently: claimed_type states what this reading believes is
 *   structurally true; the metrics state what it believes is descriptively
 *   true of the regime's operation, including the mild ceremony creep it
 *   openly acknowledges. KEY AGENTS (by structural relationship): -
 *   rijkswaterstaat_engineers: agenda-setter (institutional/identity_locked)
 *   — administers the national exercise calendar and inspection program -
 *   water_board_administrators: secondary agenda-setter
 *   (institutional/identity_locked) — runs the regional drill and inspection
 *   tier - emergency_response_agencies: primary practitioner-beneficiary
 *   (organized/constrained) — their exercised skills are the maintained asset
 *   - flood_plain_residents: primary beneficiary (moderate/constrained) —
 *   receives maintained protective capacity - national_taxpayers: net
 *   beneficiary and funder (organized/mobile) — bears the fiscal coordination
 *   cost - uncovered_region_residents: excluded seat (powerless/constrained)
 *   — outside the exercise regime's priority coverage -
 *   preparedness_researchers: analytical observer (analytical/analytical) —
 *   measures whether competence is actually retained
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.14).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.09).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.09).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.46).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading of the Exercise-and-Inspection Regime)").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "governance/institutional-memory/disaster-preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, '8324de72-d949-46c9-9068-6606b15ffc08').
narrative_ontology:cs_kernel_codification('8324de72-d949-46c9-9068-6606b15ffc08', distributed).
narrative_ontology:cs_authority_grounding('8324de72-d949-46c9-9068-6606b15ffc08', expertise).
narrative_ontology:cs_interpretation_layer_present('8324de72-d949-46c9-9068-6606b15ffc08').
narrative_ontology:cs_reading_relation('8324de72-d949-46c9-9068-6606b15ffc08', preparedness_retention__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('8324de72-d949-46c9-9068-6606b15ffc08', preparedness_retention__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('8324de72-d949-46c9-9068-6606b15ffc08', foundational, exercised_practice_preserves_operational_competence).
narrative_ontology:cs_axiom_status(exercised_practice_preserves_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('8324de72-d949-46c9-9068-6606b15ffc08', exercised_practice_preserves_operational_competence, empirically_contingent).
narrative_ontology:cs_axiom('8324de72-d949-46c9-9068-6606b15ffc08', secondary, mandated_exercise_solves_preparedness_underprovision).
narrative_ontology:cs_axiom_status(mandated_exercise_solves_preparedness_underprovision, holdable).
narrative_ontology:cs_axiom_grounding('8324de72-d949-46c9-9068-6606b15ffc08', mandated_exercise_solves_preparedness_underprovision, instrumental).
narrative_ontology:cs_reference_frame('8324de72-d949-46c9-9068-6606b15ffc08', exercised_competence_baseline).
narrative_ontology:cs_drift_state('8324de72-d949-46c9-9068-6606b15ffc08', contemporary_exercise_audit_cycle, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('8324de72-d949-46c9-9068-6606b15ffc08', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, flood_plain_residents).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_response_agencies).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, national_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, water_board_administrators).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, national_taxpayers).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, skill_decay_hypothesis).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, spaced_practice_retention_effect).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and runs the national exercise calendar: multi-agency flood drills, dike inspection cycles, and after-action review. Its budget is consumed delivering exercises and feeding findings into maintenance schedules; it collects no surplus from the regime it administers. Its staff identity is fused with stewardship of the delta defenses — the organization has become its function — so leaving would mean dissolving the profession, not changing employers.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, rijkswaterstaat_engineers, agenda_setter,
    institutional, generational, identity_locked, national).

% Run the regional tier of the same regime: local drill days, sluice and pumping-station inspections, and volunteer musters. They are among the oldest continuously operating water-governance bodies; administering the exercise cycle is constitutive of what they are, and their regional scope means inspection findings feed directly into their own maintenance crews.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, water_board_administrators, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, water_board_administrators, beneficiary).

% Fire brigades, medical response teams, and technical rescue units whose rare-event skills — dike breach sealing, mass evacuation, high-water pumping — decay without practice. The mandate gives them protected exercise time and cross-agency integration they could not buy individually; their cost is schedule disruption and the exposure of gaps during scored drills.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_response_agencies, beneficiary,
    organized, biographical, constrained, national).

% Live behind the dike rings the regime protects. They receive maintained protective capacity without running any part of it; their contact with the regime is evacuation drills in schools and workplaces and public exercise announcements. Moving away from the protected zone is possible but costly, and most have generational roots in the area.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, flood_plain_residents, beneficiary,
    moderate, generational, constrained, national).

% Fund the exercise regime through general revenue and water-board levies. They carry the fiscal cost directly and receive the safety good only probabilistically — the regime's value is invisible in any year without a flood. They can vote on the levies and, at greater cost, emigrate from the taxed jurisdiction.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, national_taxpayers, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, national_taxpayers, payer).

% Live outside the priority dike-ring coverage where inspection cycles and major exercises concentrate. Their areas receive thinner drill investment and less frequent inspection; they would argue the maintained competence is distributed by administrative priority rather than by risk alone, and they have no seat in the exercise-planning coalition that sets the annual calendar.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, uncovered_region_residents, excluded,
    powerless, generational, constrained, regional).

% Study whether exercised skills transfer to real events: human-factors skill-decay studies, after-action analyses across jurisdictions, and comparative audits of exercise design. They hold no stake in the regime's budget and can redirect their attention to any country's system; their output is the outside check on whether the drills preserve anything.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, preparedness_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_retention__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the under-provision problem for rare-event competence: individual agencies will not spend scarce hours practicing flood-response skills whose need is probabilistic, and skills decay measurably between events. The mandated exercise calendar synchronizes training across agencies, keeps rare-event skills current, and uses inspection findings to route maintenance before failures; it also coordinates information about who holds which verified capability.
% TRANSFER_FUNCTION: Moves staff hours, budget, and schedule disruption from response agencies, water boards, and taxpayers into maintained response capacity, verified dike condition, and exercised inter-agency coordination; during exercises it also moves senior-official attention toward failure scenarios that ordinary administration never surfaces.
% ABSENT_VOICES: Residents of areas outside the priority dike-ring coverage would object that exercise investment follows administrative boundaries rather than risk alone; smaller municipalities that cannot host major exercises would object that the regime's verification benefits concentrate where the machinery already sits. Neither seat is present in the exercise-planning coalition that sets the annual calendar.
% DISAPPEARANCE_RATIONALE: If the exercise-and-inspection regime vanished overnight, equipment and rosters would remain but exercised integration would decay silently: inspection findings would stop routing maintenance, rare-event skills would erode on the skill-decay curve, and inter-agency coordination would exist only on paper. The rearrangement would be invisible until a major high-water event exposed the gap — the exact failure mode the regime exists to prevent.
% FOUNDING_PROBLEM: After long flood-free intervals, response capacity decays: skills go unpracticed, inspection backlogs hide, inter-agency coordination frays, and the system that exists on paper fails to exist in operation when the water comes. The 1953 delta flood is the founding demonstration that a competent-on-paper system without live practice fails catastrophically.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the human-factors skill-decay literature (independent laboratory and field replication), post-flood inquiry reports from comparable delta jurisdictions finding that unpracticed systems underperformed their paper capability, and insurer risk models that price maintained response capacity differently from unmaintained. No seat inside the exercise-planning coalition is needed to establish that the decay problem exists.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.14 at interval end) because the regime's costs — staff hours, exercise budgets, schedule disruption — are consumed in delivery and return as maintained capacity; no seat collects a surplus, and exercise spending scales with delivered exercises rather than decoupling from service cost. Suppression is low (0.09): the statutory mandate solves a real free-rider problem — no agency would unilaterally drill at the required intensity — but it compels participation without suppressing rival training designs; agencies retain latitude over scenario content and format, so the mandate is minimal coercive overhead rather than an exit-blocker. theater_ratio is low (0.18) but creeping: anniversary exercises with official observers and predictable scenario rotations add ceremony, while unannounced, scenario-varied drills — the majority of exercise hours — find and fix real gaps. accessibility_collapse (0.46) is moderate: once skill decay is understood, the no-practice alternative collapses, but rival training modalities (tabletop formats, simulation, cross-border exchanges) remain live, so alternatives are narrowed, not eliminated. resistance (0.22) is drill fatigue: scheduling friction and scored-drill anxiety, accepted as the price of readiness. The temporal series share one grid and show mild oscillation around a gentle rise: theater peaks at anniversary exercise cycles (t=3, 15, 27) and recedes between them, and extractiveness tracks the same calendar through opportunity cost. The oscillation is a side effect of the exercise calendar and post-cycle scenario refresh, not an extraction mechanism — participation is mandated, so there is no intermittent-reinforcement dynamic to harvest. This reading reads the creep as contained drift that scenario-refresh policy can hold. Receipt surface: gains diffuse across residents, agencies, and taxpayers with no capturing seat — the administering agencies consume their budgets in delivery; fixing (scenario refresh, coverage rebalancing) is cheap for the agenda-setters relative to its benefit.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. The two agenda-setter seats (rijkswaterstaat_engineers, water_board_administrators) hold identity-locked positions through institutional identity fusion: the organizations have become their stewardship function, so from those seats the regime is identity-work, and the computed type will sit at maximum function and minimum cost — a scandal revealing hollow drills would break the identity frame and shift their directionality sharply. The funder seat (national_taxpayers) experiences the regime as a budget line whose value is invisible in any flood-free year, so its computed extraction runs higher than the other beneficiary seats' even though it is a net beneficiary. The excluded seat (uncovered_region_residents) experiences the regime primarily as a coverage boundary: the same drills that maintain capacity elsewhere are, from that seat, evidence of investment that stops at an administrative line. The observer seat sees the full structure and adjudicates none of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared (flood_plain_residents, emergency_response_agencies, national_taxpayers) and no victims are declared, because this reading's structural claim is that no seat is extracted from: costs are coordination costs borne by net beneficiaries. emergency_response_agencies sit nearest the full-beneficiary end — the regime subsidizes precisely the capacity they would under-provide alone. flood_plain_residents are near-pure beneficiaries with constrained exit: they receive capacity without running anything. national_taxpayers carry the largest direct cost share and hold mobile exit (voting, migration), which tilts their derived directionality slightly toward the target end while leaving them net beneficiaries — the derivation should place them modestly above the other beneficiary seats. The agenda-setters' low directionality is reinforced by identity lock: exit would cost them their institutional selves. Enforcement exists (the mandate and inspection authority) but is overhead, not extraction: it compels practice whose product returns to the compelled. No directionality overrides are needed because the beneficiary declarations plus exit options already produce this ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — capacity decay over flood-free intervals — is live, so there is no mandatrophy to resolve and no drift toward inertial persistence from this seat. The classification discipline cuts both ways here. Declaring no victims keeps the mandate from being misread as a transfer: the regime's enforcement compels practice whose product returns to the compelled, which is coordination, not extraction. Conversely, the low theater_ratio is the load-bearing measurement separating this reading from its siblings: if ceremony hours were to exceed functional hours (theater_ratio sustained above 0.5), the same structural data would support a degraded reading, and the temporal series exist precisely to date such a transition if it comes. The gentle theater creep in the series is the early signature that would have to accelerate before this file's profile could be honestly revised.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_reading_kernel_commitment,
    'This file instantiates competence_reading of the preparedness_retention kernel; is that the reading the standing exercise-and-inspection regime actually satisfies, and what would the sibling readings change structurally if adopted?',
    'Blind, unannounced exercise audits scoring task completion under degraded conditions, combined with the sibling files'' own metric authorship over the identical arrangement: adoption of husk_reading would re-author theater_ratio near or above 0.5 and extractiveness substantially higher over the same referent, and adoption of hybrid_reading would split this constraint into a live specialized stratum and a ceremonial broad stratum.',
    'The disagreement between readings is located in two measurable places: the ceremony-to-competence ratio of observed exercise hours, and whether drill performance transfers to real-event performance. If the husk reading is right, this file''s epsilon is understated by a wide margin and the regime operates as enforced ceremony; if the hybrid reading is right, the no-victim structure breaks at the broad societal layer. The competence and husk readings are direct negations and cannot be revised toward each other without collapsing into the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_reading_kernel_commitment, conceptual, 'Committer structure: one reading of the preparedness_retention kernel; sibling readings would re-author the same arrangement with different epsilon and victim structure.').

omega_variable(
    training_transfer_validity,
    'Does competence exercised under drill conditions transfer to performance in real high-water events, or does drill skill stay partly bound to the drill context?',
    'After-action comparison of real-event performance against preceding exercise scores across multiple events and jurisdictions, controlling for event severity.',
    'If transfer is weak, this file''s epsilon is understated: the regime''s costs buy less preserved capacity than claimed, and the beneficiary structure weakens toward the taxpayer seat. If transfer is strong, the low-extraction profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_transfer_validity, empirical, 'Whether exercised drill competence transfers to real-event performance — the empirical hinge of the reading.').

omega_variable(
    overinvestment_threshold,
    'At what exercise intensity does marginal drill spending exceed marginal capacity value, creating the fiscal-efficiency loss this reading concedes as its characteristic failure mode?',
    'Marginal-cost analysis of additional exercise hours against probabilistic risk reduction, using insurer pricing and historical event frequencies.',
    'Below the threshold, taxpayers remain net beneficiaries and the no-victim structure holds; above it, national_taxpayers become a mild victim class and the constraint tilts toward a coordination-extraction profile running on the same enforcement machinery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overinvestment_threshold, empirical, 'The over-investment boundary at which a victim class (fiscal efficiency borne by taxpayers) would emerge.').

omega_variable(
    theater_creep_trajectory,
    'Will the mild ceremony creep in the temporal series stay contained by scenario-refresh policy, or accelerate toward a majority-ceremonial regime?',
    'Longitudinal theater-ratio measurement across successive exercise cycles, tracking the ratio of unannounced scenario-varied drills to scheduled observational exercises.',
    'A sustained theater_ratio above 0.5 would date a transition out of this reading''s profile and require re-authoring the constraint; the current series shows contained oscillation around a gentle rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_creep_trajectory, empirical, 'Whether the observed ceremony creep is contained drift or the leading edge of ceremonial takeover.').

omega_variable(
    coverage_boundary_equity,
    'Do residents outside the priority dike-ring coverage constitute a latent victim class — people who fund the regime through general revenue but receive thinner exercise investment near their own defenses?',
    'Coverage mapping of exercise investment and inspection frequency against population and residual-risk data by area.',
    'If uncovered residents bear net cost without commensurate capacity, the no-victim declaration weakens, a victim group enters the structure, and the constraint moves toward a coordination-extraction hybrid; if coverage tracks residual risk, the excluded seat is an equity concern rather than a structural victim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coverage_boundary_equity, empirical, 'Whether the regime''s coverage boundary creates a latent victim class outside the priority dike rings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(preparedness_competence_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(preparedness_competence_tr_t0, observed).
narrative_ontology:measurement(preparedness_competence_tr_t3, preparedness_retention__competence_reading, theater_ratio, 3, 0.16).
narrative_ontology:measurement_basis(preparedness_competence_tr_t3, observed).
narrative_ontology:measurement(preparedness_competence_tr_t6, preparedness_retention__competence_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement_basis(preparedness_competence_tr_t6, observed).
narrative_ontology:measurement(preparedness_competence_tr_t9, preparedness_retention__competence_reading, theater_ratio, 9, 0.15).
narrative_ontology:measurement_basis(preparedness_competence_tr_t9, observed).
narrative_ontology:measurement(preparedness_competence_tr_t12, preparedness_retention__competence_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement_basis(preparedness_competence_tr_t12, observed).
narrative_ontology:measurement(preparedness_competence_tr_t15, preparedness_retention__competence_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(preparedness_competence_tr_t15, observed).
narrative_ontology:measurement(preparedness_competence_tr_t18, preparedness_retention__competence_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement_basis(preparedness_competence_tr_t18, observed).
narrative_ontology:measurement(preparedness_competence_tr_t21, preparedness_retention__competence_reading, theater_ratio, 21, 0.16).
narrative_ontology:measurement_basis(preparedness_competence_tr_t21, observed).
narrative_ontology:measurement(preparedness_competence_tr_t24, preparedness_retention__competence_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement_basis(preparedness_competence_tr_t24, observed).
narrative_ontology:measurement(preparedness_competence_tr_t27, preparedness_retention__competence_reading, theater_ratio, 27, 0.18).
narrative_ontology:measurement_basis(preparedness_competence_tr_t27, observed).
narrative_ontology:measurement(preparedness_competence_tr_t30, preparedness_retention__competence_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(preparedness_competence_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(preparedness_competence_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(preparedness_competence_be_t0, observed).
narrative_ontology:measurement(preparedness_competence_be_t3, preparedness_retention__competence_reading, base_extractiveness, 3, 0.12).
narrative_ontology:measurement_basis(preparedness_competence_be_t3, observed).
narrative_ontology:measurement(preparedness_competence_be_t6, preparedness_retention__competence_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement_basis(preparedness_competence_be_t6, observed).
narrative_ontology:measurement(preparedness_competence_be_t9, preparedness_retention__competence_reading, base_extractiveness, 9, 0.12).
narrative_ontology:measurement_basis(preparedness_competence_be_t9, observed).
narrative_ontology:measurement(preparedness_competence_be_t12, preparedness_retention__competence_reading, base_extractiveness, 12, 0.11).
narrative_ontology:measurement_basis(preparedness_competence_be_t12, observed).
narrative_ontology:measurement(preparedness_competence_be_t15, preparedness_retention__competence_reading, base_extractiveness, 15, 0.13).
narrative_ontology:measurement_basis(preparedness_competence_be_t15, observed).
narrative_ontology:measurement(preparedness_competence_be_t18, preparedness_retention__competence_reading, base_extractiveness, 18, 0.12).
narrative_ontology:measurement_basis(preparedness_competence_be_t18, observed).
narrative_ontology:measurement(preparedness_competence_be_t21, preparedness_retention__competence_reading, base_extractiveness, 21, 0.13).
narrative_ontology:measurement_basis(preparedness_competence_be_t21, observed).
narrative_ontology:measurement(preparedness_competence_be_t24, preparedness_retention__competence_reading, base_extractiveness, 24, 0.12).
narrative_ontology:measurement_basis(preparedness_competence_be_t24, observed).
narrative_ontology:measurement(preparedness_competence_be_t27, preparedness_retention__competence_reading, base_extractiveness, 27, 0.14).
narrative_ontology:measurement_basis(preparedness_competence_be_t27, observed).
narrative_ontology:measurement(preparedness_competence_be_t30, preparedness_retention__competence_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement_basis(preparedness_competence_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'preparedness retention' decomposes into three structurally distinct constraint stories per the epsilon-invariance principle: competence_reading (this file — the regime preserves live capacity; low epsilon, no victims), husk_reading (the same regime is memorial performance; high theater, extraction of attention and budget without competence return), and hybrid_reading (competence is stratified; epsilon differs by stratum and the constraint decomposes further into a live specialized stratum and a ceremonial broad stratum). The siblings are separate files, each linking the others here. The upstream/downstream structure: the competence reading is the regime's self-description and the baseline against which the other two measure divergence; the husk and hybrid readings are downstream critiques that inherit the competence reading's referent — the standing exercise-and-inspection arrangement — while re-assessing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
