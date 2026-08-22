% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Lived Catastrophe Necessity for Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint embodies the lived-catastrophe-necessity reading of the
 *   exercise-as-competence-maintenance kernel: the claim that only actual
 *   catastrophe exercises the full competence kernel because judgment under
 *   real stakes cannot be simulated. The reading holds that competence decays
 *   covertly in systems that rely only on simulation and exercise. This makes
 *   the constraint extractive and asymmetrically enforced—crisis planners
 *   benefit from the institutional authority to declare readiness via
 *   lower-cost simulation, while operators and exposed populations carry the
 *   risk of undetected competence decay. The victim set includes all who
 *   depend on operators whose competence has never been tested under real
 *   stakes. The constraint is actively enforced through institutional
 *   insistence that simulation is sufficient, which suppresses alternative
 *   readings and the demand for real-stakes testing.
 *
 * KEY AGENTS:
 *   - Crisis planners and institutional risk administrators — agenda-setters, institutional power, benefit from simulation-sufficiency authority
 *   - Operators trained in simulation only — moderate power, constrained exit, carry competence debt they do not detect
 *   - Exposed populations (civilian, medical, industrial) — powerless, trapped exit, catastrophic risk from covert decay
 *   - Insurance and regulatory underwriters — institutional power, constrained exit, carry massive tail risk on simulation sufficiency assumption
 *   - Simulation designers — organized power, mobile exit, benefit from exercise legitimacy
 *   - Post-incident investigators — analytical observers, can attest whether real-stakes judgment failures match the reading's prediction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.68).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.72).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Lived Catastrophe Necessity for Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9').
narrative_ontology:cs_kernel_codification('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9', implicit).
narrative_ontology:cs_authority_grounding('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9', extraction).
narrative_ontology:cs_interpretation_layer_present('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9').
narrative_ontology:cs_reading_relation('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9', exercise_as_competence_maintenance__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9', foundational, real_stakes_uniquely_exercise_judgment_kernel).
narrative_ontology:cs_axiom_status(real_stakes_uniquely_exercise_judgment_kernel, holdable).
narrative_ontology:cs_axiom_grounding('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9', real_stakes_uniquely_exercise_judgment_kernel, empirically_contingent).
narrative_ontology:cs_axiom('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9', foundational, competence_decay_covert_without_real_stakes_testing).
narrative_ontology:cs_axiom_status(competence_decay_covert_without_real_stakes_testing, holdable).
narrative_ontology:cs_axiom_grounding('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9', competence_decay_covert_without_real_stakes_testing, empirically_contingent).
narrative_ontology:cs_reference_frame('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9', simulation_as_competence_maintenance_standard).
narrative_ontology:cs_drift_state('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9', contemporary_post_incident_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e2b1cdd8-fc7a-4b2f-b5c0-4bebf4854fc9', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, crisis_planners_and_institutional_risk_administrators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exposed_populations_with_untested_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, operators_subject_to_competence_decay_pathways).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, organizations_underwriting_simulation_sufficiency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_operators_and_exercise_designers).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, operators_trained_and_exercised_in_simulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, mandate, and oversee exercises and protocols. Justify exercise investments as preparation and competence maintenance. Benefit institutionally from the authority to declare readiness and from budget allocation that favors simulation. Their interest in declaring simulation sufficient (low cost, low disruption, politically expedient) runs counter to the lived-catastrophe reading's claim that only real stakes test competence.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, crisis_planners_and_institutional_risk_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Participate in exercises, internalize procedural knowledge, and carry (under this reading) a competence debt they do not know they carry—covert decay that simulation cannot detect. When actual catastrophe arrives, their judgment under real stakes is untested. They bear the cost of the competence maintenance illusion when execution matters.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, operators_trained_and_exercised_in_simulation, payer,
    moderate, biographical, constrained, national).

% Depend for safety on operator competence that has never been tested under real stakes. They carry catastrophic risk from covert competence decay undetected by simulation. In actual crisis, their lives and property are at stake; the operator's judgment-under-real-stakes is their exposure surface.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exposed_populations_with_untested_operators, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exposed_populations_with_untested_operators, payer).

% Insurance carriers, regulatory bodies, and institutional custodians bet their underwriting on the claim that simulation maintains competence. Under this reading, they carry massive tail risk: they have not priced in the competence-decay possibility because the reading claims decay is covert and undetected by available exercises. If actual catastrophe reveals the decay, their reserve calculations were wrong.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, organizations_underwriting_simulation_sufficiency, payer,
    institutional, generational, constrained, national).

% Conduct exercises, argue for their value, and receive institutional recognition and resources for exercise design and execution. They benefit from the assertion that simulation matters—their continued employment and institutional standing rides on exercises being taken seriously. Pressure toward simulation sufficiency (rather than lived-catastrophe necessity) benefits them.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_operators_and_exercise_designers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_operators_and_exercise_designers, agenda_setter).

% After actual catastrophe, conduct inquiries into whether operators' competence failures were technical ignorance or judgment-under-stakes decay. Their findings either support the lived-catastrophe reading (operators knew procedures but judgment failed under real stakes, covert decay) or the hybrid/simulation-sufficiency readings (procedures themselves were insufficient, or simulation was adequate but fidelity low).
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, post_catastrophe_investigators, observer,
    analytical, immediate, analytical, national).

% Jurisdictions that invest in frequent, high-cost, real-stakes testing (combat readiness, medical crisis rotations, disaster response deployments) or that explicitly reject simulation sufficiency are structurally excluded from the domestic institutional consensus. Their alternative authority grounding (lived practice over simulation) is not in the room when readiness standards are set.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, competitor_jurisdictions_and_alternative_frameworks, excluded,
    institutional, generational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, crisis_planners_and_institutional_risk_administrators).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes institutional knowledge about what it takes to maintain competence under crisis: establishes procedures, allocates training resources, creates a framework for rehearsal and skill maintenance. Solves the genuine coordination problem of how to preserve complex adaptive capacity across time and transitions.
% TRANSFER_FUNCTION: Moves resources (training budget, institutional attention, operator time) from general operations into exercise infrastructure, and moves authority over readiness declarations to crisis planners. Under this reading, also transfers risk: the declaration that simulation maintains competence transfers the cost of undetected decay to exposed populations while crisis planners benefit from lower-cost, lower-disruption preparation narratives.
% ABSENT_VOICES: Operators who have only exercised, not experienced real catastrophe, cannot attest whether their exercised competence actually holds under real stakes—they lack standing to judge their own covert decay. Populations whose safety depends on those operators have no say in whether simulation is sufficient. Competing jurisdictions that have chosen high-cost real-stakes testing are not in the institutional conversation about readiness standards.
% DISAPPEARANCE_RATIONALE: If the assumption that simulation maintains competence disappeared and institutions adopted the lived-catastrophe reading, resource allocation would shift dramatically toward real-stakes testing (medical residencies shifting to more live procedures, emergency response agencies conducting live deployments, nuclear operators undergoing real-world scenarios). The underwriting of safety would reorganize around a different epistemic claim about what constitutes evidence of competence.
% FOUNDING_PROBLEM: Complex adaptive systems (aviation, medicine, nuclear operations, emergency response) face the problem that competence erodes without use, but actual catastrophe is rare, costly, and ethically fraught as a training ground. Exercise emerged as a way to maintain competence without requiring constant real catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: The lived-catastrophe reading holds the founding problem is still live but the proposed solution (simulation as sufficient) has become decoupled from its fitness. Post-incident investigations (aviation accidents where exercised procedures failed under real stress, medical crises where trained operators made different judgment calls than in simulation) provide corroboration from outside the crisis-planning institutional seat. Operations research and cognitive science literatures on judgment under real stakes vs. simulation fidelity provide independent technical corroboration. Crisis planners attest the problem is solved by exercise alone.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.48 → 0.68) as institutional reliance on simulation without real-stakes testing deepens and the covert competence-decay risk accumulates. Theater ratio rises sharply (0.38 → 0.58): exercises become more elaborate and frequent as institutions substitute for real-stakes testing, and the ratio of ritualized drill to functional judgment testing increases. Suppression is high and rising (0.55 → 0.72) because institutional consensus around simulation sufficiency actively suppresses alternative frameworks (frequent real-stakes testing, explicit decay-monitoring) and disqualifies operators from attesting their own undetected decay. The reading is CLAIMED as tangled rope because it coordinates genuine competence-maintenance function while extracting risk to exposed populations and operators; the metrics show substantial extractiveness and active suppression characteristic of rope → snare drift. The measurement grid shares one time axis: every metric is authored at every examined point (t=0,5,10,15,20,25).
 *
 * PERSPECTIVAL GAP:
 *   Crisis planners compute a rope reading from their seat—they coordinate competence maintenance, allocate resources efficiently, produce observable exercise activity—and the cost is borne by populations and operators who carry undetectable decay risk. Operators and exposed populations compute a snare reading: they are locked into a system that claims to maintain competence via simulation but carries hidden risk they cannot assess. Insurance underwriters compute something close to the catastrophe-necessity reading once they run loss estimates under the covert-decay assumption—their divergence from the planners' rope reading is sharp and late. The engine will compute these divergences per seat from the structural data: beneficiary/victim declarations, exit options, and power atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Crisis planners are near full beneficiary (d ~ 0.1-0.2): they control the readiness narrative, allocate resources, and bear low personal risk. Operators are near symmetric-to-target (d ~ 0.45-0.65): they benefit from exercise as genuine skill-building but pay the hidden cost of covert decay and possible judgment failure under real stakes. Exposed populations are near full target (d ~ 0.85-0.95): they have no say in readiness standards, no way to assess operator competence, and face catastrophic risk from decay they cannot measure. Insurance underwriters shift from beneficiary to target as they internalize the tail-risk cost (d shifts from 0.3 to 0.7 when the decay assumption is priced in). Simulation designers are beneficiaries (d ~ 0.15) as long as exercises are institutionally valued.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the false-positive snare misclassification by including genuine coordination function (competence maintenance is real) while capturing the extractive structure (risk transfer to powerless agents). The tangled-rope classification is correct because the constraint solves a genuine coordination problem (how to maintain adaptive capacity without constant catastrophe) while asymmetrically extracting from operators and exposed populations (who bear covert decay risk while crisis planners benefit from low-cost, low-disruption preparation narratives). If the lived-catastrophe necessity were universally accepted, the constraint would degrade to snare (pure risk extraction disguised as competence maintenance); the kernel contest makes it tangled rope from the lived-catastrophe reading's seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covert_decay_detectability,
    'Is competence decay from simulation-only training actually covert and undetectable by available measurement methods, or can it be detected through careful post-exercise analysis?',
    'Prospective studies comparing post-exercise performance metrics between operators trained with simulation alone vs. those with recent real-stakes experience; analysis of incident reports for judgment-failure patterns that would indicate undetected decay.',
    'If decay is detectable via available methods, the reading shifts to simulation-sufficiency or hybrid-decay (exercises can be instrumented to measure judgment performance). If decay remains covert despite analysis, the reading strengthens to snare (institutions have incentive to not look).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_decay_detectability, empirical, 'Whether competence decay from simulation-only training is actually undetectable by available methods.').

omega_variable(
    judgment_under_stakes_intra_individual_stability,
    'Does judgment-under-real-stakes constitute a qualitatively different competence from procedure-under-simulation, or are they points on a continuum that high-fidelity simulation can span?',
    'Cognitive science literature on stress, uncertainty, and decision-making under time pressure; comparative incident analysis (simulation failures vs. real-catastrophe performance for same operators); neuroimaging studies of judgment networks during simulated vs. real-stakes tasks.',
    'If judgment-under-stakes is qualitatively different and high-fidelity simulation cannot span it, the lived-catastrophe necessity reading is strengthened. If judgment is a continuum and high enough fidelity can replicate stress patterns, simulation-sufficiency or hybrid readings become more plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judgment_under_stakes_intra_individual_stability, conceptual, 'Whether judgment under real stakes is qualitatively distinct from simulated judgment or points on a continuum.').

omega_variable(
    institutional_incentive_for_simulation_sufficiency,
    'How much of the institutional consensus around simulation sufficiency is driven by genuine belief in the evidence vs. incentive to avoid the high cost and disruption of real-stakes testing?',
    'Behavioral analysis of crisis-planning institutions: do institutions that have access to low-cost real-stakes testing venues adopt them? Do jurisdictions with different funding constraints adopt different positions on simulation vs. real-stakes necessity?',
    'If the consensus is driven by incentive rather than evidence, the extraction component of the constraint becomes active enforcement (suppressing alternative frameworks) rather than emergent from structural necessity, strengthening the tangled-rope-to-snare drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentive_for_simulation_sufficiency, empirical, 'The degree to which institutional consensus on simulation sufficiency is evidence-driven vs. incentive-driven.').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the lived-catastrophe-necessity reading logically foreclose the simulation-sufficiency reading, or do they coexist as live positions held by different institutional factions?',
    'Examination of the core premises: if lived-catastrophe necessity claims decay is covert-and-undetectable while simulation-sufficiency claims simulation can be instrumented to measure all competence dimensions, then a single framework would have to choose which empirical claim is true; but if institutional factions genuinely dispute the empirical facts without logical contradiction, they coexist.',
    'This omega routes the reading_relations field in cs_structure: the relation is coexists_with if both readings remain live empirical disputes; forecloses if one reading''s core premise directly contradicts the other''s within any possible unified framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether the lived-catastrophe and simulation-sufficiency readings foreclose each other or coexist as live positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 5, 0.43).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 10, 0.49).
narrative_ontology:measurement(exer_tr_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(exer_tr_t25, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(exer_be_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(exer_be_t25, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(exer_su_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(exer_su_t25, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.14).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the exercise-as-competence-maintenance kernel. The lived-catastrophe-necessity reading claims only real stakes test competence and competence decays covertly without them. The simulation-sufficiency reading claims high-fidelity simulation constitutes genuine exercise. The hybrid-decay reading claims simulation exercises procedure but not judgment-under-real-stakes. All three are separate constraint stories sharing the same kernel. This story (lived-catastrophe) influences the other two by establishing the competence-decay possibility that the others must account for; they coexist as live positions held by different institutional factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
